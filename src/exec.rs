use std::collections::HashMap;
use std::fs::{File, OpenOptions};
use std::io::Write;
use std::path::{Path, PathBuf};

use std::sync::Arc;

use crate::parser::RunscriptSource;
use crate::process::{CommandExecError, Pipe, PipeInput, StdioRepr, WaitableProcess};

use crate::parser::ast::{
    AndOr, AndOrList, AtomicTopLevelCommand, ComplexWord, CompoundCommand, CompoundCommandKind,
    GuardBodyPair, ListableCommand, Parameter, ParameterSubstitution, PatternBodyPair,
    PipeableCommand, Redirect, RedirectOrCmdWord, RedirectOrEnvVar, SimpleCommand, SimpleWord,
    Word,
};
use itertools::Itertools;

use crate::out;
use glob::{glob_with, MatchOptions, Pattern};

#[derive(Clone)]
pub struct ExecConfig<'a> {
    /// The output stream to output to, or `None` to produce no output
    pub output_stream: Option<Arc<termcolor::StandardStream>>,
    /// The colour choice for the output stream
    pub colour_choice: termcolor::ColorChoice,
    /// The working directory to execute the script's commands in
    pub working_directory: &'a Path,
    /// The path to the script file being executed
    pub script_path: Option<PathBuf>,
    /// The name of the target being executed
    pub target_name: Option<&'a str>,
    /// Positional arguments to pass to the script.
    ///
    ///The first argument replaces `$1`, the second replaces `$2`, etc.
    pub positional_args: Vec<String>,
}

pub struct BaseExecContext {
    pub current_file: Option<PathBuf>,
    pub current_target: Option<String>,
    pub args: Vec<String>,
    pub colour_choice: termcolor::ColorChoice,
}

pub enum EvaluatedRedirect {
    Read(Option<u16>, Vec<String>),
    Write(Option<u16>, Vec<String>),
    ReadWrite(Option<u16>, Vec<String>),
    Append(Option<u16>, Vec<String>),
    Clobber(Option<u16>, Vec<String>),
    Heredoc(Option<u16>, Vec<String>),
    DupRead(Option<u16>, Vec<String>),
    DupWrite(Option<u16>, Vec<String>),
}

#[derive(Debug, Clone)]
pub struct ShellContext {
    /// The current working directory
    working_directory: PathBuf,
    /// The current shell variables
    vars: HashMap<String, String>,
    /// Exported varables
    env: HashMap<String, String>,
    /// Function definitions
    functions: HashMap<String, Arc<CompoundCommand>>,
    /// PID of most recent job
    pid: i32,
    /// Exit code of most recent top-level command
    exit_code: i32, //TODO: This will always be 0. Perhaps an option to have the script keep going even after errors?
}

impl ShellContext {
    pub fn new(config: &ExecConfig) -> ShellContext {
        ShellContext {
            working_directory: config.working_directory.to_owned(),
            vars: HashMap::new(),
            env: HashMap::new(),
            functions: HashMap::new(),
            pid: -1,
            exit_code: 0,
        }
    }

    pub fn exec_command_group(
        &mut self,
        script: &[AtomicTopLevelCommand],
        config: &ExecConfig,
    ) -> Result<WaitableProcess, CommandExecError> {
        use crate::parser::ast::Command;

        let mut jobs = vec![];

        if script.len() > 1 {
            for command in &script[..script.len() - 1] {
                let (command, is_job) = match &command {
                    Command::Job(list) => (list, true),
                    Command::List(list) => (list, false),
                };

                let proc = self.exec_andor_list(command, config, is_job)?;
                if is_job {
                    self.pid = proc.pid();
                    jobs.push(proc);
                } else {
                    let finished = proc.wait();
                    if !finished.status.success() {
                        return Ok(WaitableProcess::finished(finished));
                    }
                }
            }
        }

        if !script.is_empty() {
            match &script.last().unwrap() {
                Command::Job(list) => {
                    let proc = self.exec_andor_list(list, config, true)?;
                    let mut last_proc = WaitableProcess::empty_success();
                    jobs.push(proc);
                    last_proc.set_jobs(jobs);
                    Ok(last_proc)
                }
                Command::List(list) => {
                    let mut last_proc = self.exec_andor_list(list, config, false)?;
                    last_proc.set_jobs(jobs);
                    Ok(last_proc)
                }
            }
        } else {
            Ok(WaitableProcess::empty_success())
        }
    }

    fn exec_andor_list(
        &mut self,
        command: &AndOrList,
        config: &ExecConfig,
        is_job: bool,
    ) -> Result<WaitableProcess, CommandExecError> {
        let mut previous_output = self.exec_listable_command(&command.first, config, is_job)?;
        for chain in &command.rest {
            match chain {
                AndOr::And(command) => {
                    let finished = previous_output.wait();
                    if !is_job {
                        out::process_finish(&finished.status);
                    }
                    if finished.status.success() {
                        previous_output = self.exec_listable_command(command, config, is_job)?;
                    } else {
                        previous_output = WaitableProcess::finished(finished);
                        break;
                    }
                }
                AndOr::Or(command) => {
                    let finished = previous_output.wait();
                    if !is_job {
                        out::process_finish(&finished.status);
                    }
                    if !finished.status.success() {
                        previous_output = self.exec_listable_command(command, config, is_job)?;
                    } else {
                        previous_output = WaitableProcess::finished(finished);
                        break;
                    }
                }
            }
        }
        Ok(previous_output)
    }

    fn exec_listable_command(
        &mut self,
        command: &ListableCommand,
        config: &ExecConfig,
        is_job: bool,
    ) -> Result<WaitableProcess, CommandExecError> {
        if let Some(ref out) = config.output_stream {
            write!(out.lock(), "{}> ", if is_job { "&" } else { "" }).expect("Failed to write");
        }
        let rt = match command {
            ListableCommand::Pipe(_negate, commands) => {
                let mut proc = self.exec_pipeable_command(
                    commands.first().unwrap(),
                    if is_job {
                        Pipe::null_pipe_out()
                    } else {
                        Pipe::inherit_pipe_out()
                    },
                    config,
                )?;
                if let Some(ref out) = config.output_stream {
                    //TODO: Do stuff with cursor position?
                    write!(out.lock(), "| ").expect("Failed to write");
                }
                for command in &commands[1..commands.len() - 1] {
                    let next_proc = self.exec_pipeable_command(
                        command,
                        Pipe::pipe_in_out(proc.pipe_out()),
                        config,
                    )?;
                    proc = next_proc;
                    if let Some(ref out) = config.output_stream {
                        write!(out.lock(), "| ").expect("Failed to write");
                    }
                }
                let last_proc = self.exec_pipeable_command(
                    commands.last().unwrap(),
                    Pipe::pipe_in(proc.pipe_out()),
                    config,
                )?;
                Ok(last_proc) //TODO: Negate?
            }
            ListableCommand::Single(command) => {
                self.exec_pipeable_command(command, Pipe::no_pipe(), config)
            }
        };
        if let Some(ref out) = config.output_stream {
            writeln!(out.lock()).expect("Failed to write");
        }
        rt
    }

    fn exec_pipeable_command(
        &mut self,
        command: &PipeableCommand,
        pipe: Pipe,
        config: &ExecConfig,
    ) -> Result<WaitableProcess, CommandExecError> {
        match command {
            PipeableCommand::Simple(command) => self.exec_simple_command(command, pipe, config),
            PipeableCommand::Compound(command) => match &command.kind {
                CompoundCommandKind::Brace(commands) => self.exec_command_group(commands, config),
                CompoundCommandKind::Subshell(commands) => {
                    let mut context = self.clone();
                    context.exec_command_group(commands, config)
                }
                CompoundCommandKind::While(GuardBodyPair { guard, body }) => {
                    while self
                        .exec_command_group(guard, config)?
                        .wait()
                        .status
                        .success()
                    {
                        self.exec_command_group(body, config)?.wait();
                    }

                    Ok(WaitableProcess::empty_success())
                }
                CompoundCommandKind::Until(GuardBodyPair { guard, body }) => {
                    while !self
                        .exec_command_group(guard, config)?
                        .wait()
                        .status
                        .success()
                    {
                        self.exec_command_group(body, config)?.wait();
                    }

                    Ok(WaitableProcess::empty_success())
                }
                CompoundCommandKind::If {
                    conditionals,
                    else_branch,
                } => {
                    let mut last_proc = WaitableProcess::empty_success();
                    for GuardBodyPair { guard, body } in conditionals {
                        if self
                            .exec_command_group(guard, config)?
                            .wait()
                            .status
                            .success()
                        {
                            last_proc = self.exec_command_group(body, config)?;
                        }
                    }

                    if let Some(else_branch) = else_branch {
                        last_proc = self.exec_command_group(else_branch, config)?;
                    }

                    Ok(last_proc)
                }
                CompoundCommandKind::For { var, words, body } => {
                    let mut last_proc = WaitableProcess::empty_success();
                    for word in words
                        .as_ref()
                        .map(|words| -> Result<_, _> {
                            words
                                .iter()
                                .map(|word| self.evaluate_tl_word(word, config))
                                .flatten_ok()
                                .collect::<Result<Vec<_>, _>>()
                        })
                        .transpose()?
                        .unwrap_or_else(|| config.positional_args.clone())
                    {
                        self.vars.insert(var.clone(), word.clone());
                        last_proc = self.exec_command_group(body, config)?;
                    }

                    Ok(last_proc)
                }
                CompoundCommandKind::Case { word, arms } => {
                    let mut last_proc = WaitableProcess::empty_success();
                    let word = self.evaluate_tl_word(word, config)?;
                    for PatternBodyPair { patterns, body } in arms {
                        let mut pattern_matches = false;
                        for pattern in patterns {
                            let pattern = self.evaluate_tl_word(pattern, config)?;
                            if pattern == word {
                                pattern_matches = true;
                            }
                        }

                        if pattern_matches {
                            last_proc = self.exec_command_group(body, config)?;
                            break;
                        }
                    }

                    Ok(last_proc)
                }
            },
            PipeableCommand::FunctionDef(name, body) => {
                self.functions.insert(name.clone(), body.clone());
                Ok(WaitableProcess::empty_success())
            }
        }
    }

    fn exec_simple_command(
        &mut self,
        command: &SimpleCommand,
        pipe: Pipe,
        config: &ExecConfig,
    ) -> Result<WaitableProcess, CommandExecError> {
        use std::process::Command;

        let mut redirects = vec![];

        let env_remaps = command
            .redirects_or_env_vars
            .iter()
            .filter_map(|r| match r {
                RedirectOrEnvVar::EnvVar(key, word) => Some(
                    word.as_ref()
                        .map(|word| self.evaluate_tl_word(word, config))
                        .transpose()
                        .map(|word| {
                            (
                                key.clone(),
                                word.map(|words| words.join(" ")).unwrap_or_default(),
                            )
                        }),
                ),
                RedirectOrEnvVar::Redirect(redirect) => {
                    redirects.push(redirect);
                    None
                }
            })
            .collect::<Result<Vec<_>, _>>()?;

        let command_words = command
            .redirects_or_cmd_words
            .iter()
            .filter_map(|r| match r {
                RedirectOrCmdWord::CmdWord(w) => Some(self.evaluate_tl_word(w, config)),
                RedirectOrCmdWord::Redirect(redirect) => {
                    redirects.push(redirect);
                    None
                }
            })
            .collect::<Result<Vec<_>, _>>()?;

        if !command_words.is_empty() {
            let redirects = redirects
                .into_iter()
                .map(|redirect| {
                    macro_rules! map_redirect {
                        ($($t:ident),*) => {
                            match redirect {
                                $(Redirect::$t(fd, word) => EvaluatedRedirect::$t(*fd, self.evaluate_tl_word(word, config)?),)*
                            }
                        }
                    }
                    Ok(map_redirect!(Read, Write, ReadWrite, Append, Clobber, Heredoc, DupRead, DupWrite))
                })
                .collect::<Result<Vec<_>, _>>()?;

            if let Some(ref output_stream) = config.output_stream {
                out::command_prompt(
                    output_stream,
                    command,
                    &env_remaps,
                    &redirects,
                    &command_words,
                );
            }

            let command_words = command_words.into_iter().flatten().collect::<Vec<_>>();

            let mut stdin_buffer = None;
            let mut stdin = match pipe.stdin {
                PipeInput::None => StdioRepr::Null,
                PipeInput::Inherit => StdioRepr::Inherit,
                PipeInput::Pipe(stdio) => stdio,
                PipeInput::Buffer(v) => {
                    stdin_buffer = Some(v);
                    StdioRepr::MakePipe
                }
            };
            let mut stdout = if pipe.stdout {
                StdioRepr::MakePipe
            } else {
                StdioRepr::Inherit
            };
            let mut stderr = StdioRepr::Inherit;

            for redirect in redirects {
                match redirect {
                    EvaluatedRedirect::Read(fd, word) => {
                        let file = OpenOptions::new()
                            .read(true)
                            .open(self.working_directory.clone().join(word.join(" ")))
                            .map_err(|e| CommandExecError::BadRedirect { err: e })?; //TODO: Handle errors
                        match fd {
                            None | Some(0) => stdin = StdioRepr::from(file),
                            Some(1) => stdout = StdioRepr::from(file),
                            Some(2) => stderr = StdioRepr::from(file),
                            Some(_fd) => todo!(), // Might have to use nix::dup2?
                        }
                    }
                    EvaluatedRedirect::Write(fd, word) => {
                        let file = OpenOptions::new()
                            .write(true)
                            .create(true)
                            .open(self.working_directory.clone().join(word.join(" ")))
                            .map_err(|e| CommandExecError::BadRedirect { err: e })?; //TODO: Handle errors
                        match fd {
                            Some(0) => stdin = StdioRepr::from(file),
                            None | Some(1) => stdout = StdioRepr::from(file),
                            Some(2) => stderr = StdioRepr::from(file),
                            Some(_fd) => todo!(), // Might have to use nix::dup2?
                        }
                    }
                    EvaluatedRedirect::ReadWrite(fd, word) => {
                        let file = OpenOptions::new()
                            .read(true)
                            .write(true)
                            .create(true)
                            .open(self.working_directory.clone().join(word.join(" ")))
                            .map_err(|e| CommandExecError::BadRedirect { err: e })?; //TODO: Handle errors
                        match fd {
                            None | Some(0) => stdin = StdioRepr::from(file),
                            Some(1) => stdout = StdioRepr::from(file),
                            Some(2) => stderr = StdioRepr::from(file),
                            Some(_fd) => todo!(), // Might have to use nix::dup2?
                        }
                    }
                    EvaluatedRedirect::Append(fd, word) => {
                        let file = OpenOptions::new()
                            .write(true)
                            .append(true)
                            .create(true)
                            .open(self.working_directory.clone().join(word.join(" ")))
                            .map_err(|e| CommandExecError::BadRedirect { err: e })?; //TODO: Handle errors
                        match fd {
                            Some(0) => stdin = StdioRepr::from(file),
                            None | Some(1) => stdout = StdioRepr::from(file),
                            Some(2) => stderr = StdioRepr::from(file),
                            Some(_fd) => todo!(), // Might have to use nix::dup2?
                        }
                    }
                    EvaluatedRedirect::Clobber(fd, word) => {
                        let file = OpenOptions::new()
                            .write(true)
                            .create(true)
                            .open(self.working_directory.clone().join(word.join(" ")))
                            .map_err(|e| CommandExecError::BadRedirect { err: e })?; //TODO: Handle errors
                        match fd {
                            None | Some(0) => stdin = StdioRepr::from(file),
                            Some(1) => stdout = StdioRepr::from(file),
                            Some(2) => stderr = StdioRepr::from(file),
                            Some(_fd) => todo!(), // Might have to use nix::dup2?
                        }
                    }
                    EvaluatedRedirect::Heredoc(fd, word) => {
                        let mut file = tempfile::tempfile()
                            .map_err(|e| CommandExecError::BadRedirect { err: e })?; //TODO: Handle errors
                        for word in word {
                            file.write_all(word.as_bytes())
                                .map_err(|e| CommandExecError::BadRedirect { err: e })?;
                            //TODO: Handle errors
                        }
                        match fd {
                            None | Some(0) => stdin = StdioRepr::from(file),
                            Some(1) => stdout = StdioRepr::from(file),
                            Some(2) => stderr = StdioRepr::from(file),
                            Some(_fd) => todo!(), // Might have to use nix::dup2?
                        }
                    }
                    EvaluatedRedirect::DupRead(_fd, _) => {
                        return Err(CommandExecError::UnsupportedRedirect)
                    }
                    EvaluatedRedirect::DupWrite(_fd, _) => {
                        return Err(CommandExecError::UnsupportedRedirect)
                    }
                }
            }

            match command_words[0].as_str() {
                ":" => Ok(WaitableProcess::empty_success()),
                "__run_panic" => {
                    panic!("Explicit command panic")
                }
                "cd" => {
                    //TODO: Extended cd options
                    let dir = &command_words[1];
                    self.working_directory = self.working_directory.join(dir);
                    Ok(WaitableProcess::empty_success())
                }
                "export" => {
                    let name = &command_words[1];
                    if let Some((name, value)) = name.split_once('=') {
                        self.env.insert(name.to_string(), value.to_string());
                        Ok(WaitableProcess::empty_success())
                    } else {
                        self.env.insert(name.to_string(), self.vars[name].clone());
                        Ok(WaitableProcess::empty_success())
                    }
                }
                "source" => {
                    let file = config.working_directory.join(&command_words[1]);
                    let source = RunscriptSource {
                        source: std::fs::read_to_string(&file)
                            .map_err(|e| CommandExecError::CommandFailed { err: e })?,
                        path: file,
                        dir: config.working_directory.to_owned(),
                    };

                    let shell_file = crate::parser::parse_shell(source).unwrap_or_else(|_| todo!());
                    if let Some(ref output_stream) = config.output_stream {
                        writeln!(output_stream.lock()).unwrap();
                    }
                    self.exec_command_group(&shell_file, config)
                }
                #[cfg(unix)]
                "run" => {
                    let mut child_stdin = None;
                    let stdin_fd = match stdin {
                        StdioRepr::Inherit => Some(0),
                        StdioRepr::Null => None,
                        StdioRepr::MakePipe => {
                            let (read, write) = nix::unistd::pipe()
                                .map_err(|e| CommandExecError::CommandFailed { err: e.into() })?;
                            child_stdin = Some(write);
                            Some(read)
                        }
                        StdioRepr::Fd(n) => Some(n),
                    };
                    let mut child_stdout = None;
                    let stdout_fd = match stdout {
                        StdioRepr::Inherit => Some(1),
                        StdioRepr::Null => None,
                        StdioRepr::MakePipe => {
                            let (read, write) = nix::unistd::pipe()
                                .map_err(|e| CommandExecError::CommandFailed { err: e.into() })?;
                            child_stdout = Some(read);
                            Some(write)
                        }
                        StdioRepr::Fd(n) => Some(n),
                    };
                    let mut child_stderr = None;
                    let stderr_fd = match stderr {
                        StdioRepr::Inherit => Some(2),
                        StdioRepr::Null => None,
                        StdioRepr::MakePipe => {
                            let (read, write) = nix::unistd::pipe()
                                .map_err(|e| CommandExecError::CommandFailed { err: e.into() })?;
                            child_stderr = Some(read);
                            Some(write)
                        }
                        StdioRepr::Fd(n) => Some(n),
                    };

                    use nix::unistd::ForkResult;
                    match unsafe { nix::unistd::fork() } {
                        Ok(ForkResult::Parent { child }) => {
                            if let Some(buffer) = stdin_buffer {
                                if let Some(write_fd) = child_stdin.take() {
                                    use std::os::unix::prelude::FromRawFd;

                                    // This approach can cause a deadlock if the following conditions are true:
                                    // - The child is using a piped stdout
                                    // - The stdin buffer is larger than the pipe buffer
                                    // - The child writes more than one pipe buffer of data without reading enough of stdin
                                    //? Could run this on separate thread to mitigate this, if necessary.
                                    let _ = unsafe {
                                        // SAFETY: child_stdin has been take()n, so no other code might assume it exists.
                                        File::from_raw_fd(write_fd)
                                    }
                                    .write_all(&buffer); //TODO: Do I need to worry about an error here?
                                }
                            }

                            Ok(WaitableProcess::reentrant_nightmare(
                                child,
                                child_stdin,
                                child_stdout,
                                child_stderr,
                            ))
                        }
                        Ok(ForkResult::Child) => {
                            // If any of this setup fails, exit immediately with the OSERR exitcode.
                            fn bail() -> ! {
                                std::process::exit(exitcode::OSERR);
                            }
                            fn bail_a<T, U>(_: T) -> U {
                                bail()
                            }

                            // Set up standard I/O streams
                            (nix::unistd::dup2(stdin_fd.unwrap(), 0).unwrap_or_else(bail_a) == -1)
                                .then(bail);
                            (nix::unistd::dup2(stdout_fd.unwrap(), 1).unwrap_or_else(bail_a) == -1)
                                .then(bail);
                            (nix::unistd::dup2(stderr_fd.unwrap(), 2).unwrap_or_else(bail_a) == -1)
                                .then(bail);

                            // Set up remainder of environment
                            std::env::set_current_dir(&self.working_directory)
                                .unwrap_or_else(bail_a);
                            for (k, v) in &self.env {
                                std::env::set_var(k, v);
                            }

                            let mut args = command_words;
                            args.remove(0);
                            //TODO: This recusrive context only makes sense if we haven't changed directory.
                            // If we have, the script might be assuming `run` will run the script in that directory.
                            let recursive_context = BaseExecContext {
                                args,
                                current_file: config.script_path.to_owned(),
                                current_target: config.target_name.map(ToOwned::to_owned),
                                colour_choice: config.colour_choice,
                            };
                            // resume_unwind so as to not invoke the panic hook.
                            std::panic::resume_unwind(Box::new(recursive_context));
                        }
                        Err(e) => Err(CommandExecError::CommandFailed {
                            err: std::io::Error::from_raw_os_error(e as i32),
                        }),
                    }
                }
                _ => {
                    let mut child = Command::new(&command_words[0])
                        .args(&command_words[1..])
                        .envs(&self.env)
                        .envs(env_remaps)
                        .stdin(stdin)
                        .stdout(stdout)
                        .stderr(stderr)
                        .current_dir(self.working_directory.clone())
                        .spawn()
                        .map_err(|e| CommandExecError::CommandFailed { err: e })?;

                    if let Some(stdin) = stdin_buffer {
                        // This approach can cause a deadlock if the following conditions are true:
                        // - The child is using a piped stdout (the next process hasn't be spawned yet to consume it)
                        // - The stdin buffer is larger than the pipe buffer
                        // - The child writes more than one pipe buffer of data without reading enough of stdin
                        //? Could run this on separate thread to mitigate this, if necessary.
                        //? Could compare buffer size to libc::PIPE_BUF (4KiB), and spawn a thread if it is larger.
                        let _ = child.stdin.take().unwrap().write_all(&stdin); //TODO: Do I need to worry about an error here?
                    }

                    Ok(child.into())
                }
            }
        } else {
            if let Some(ref output_stream) = config.output_stream {
                out::env_remaps(output_stream, &env_remaps);
            }

            self.vars.reserve(env_remaps.len());
            for (key, value) in env_remaps {
                self.vars.insert(key, value);
            }

            Ok(WaitableProcess::empty_success())
        }
    }

    fn evaluate_tl_word(
        &mut self,
        word: &ComplexWord,
        config: &ExecConfig,
    ) -> Result<Vec<String>, CommandExecError> {
        match word {
            ComplexWord::Concat(words) => {
                let words = words
                    .iter()
                    .map(|w| self.evaluate_word(w, config))
                    .collect::<Result<Vec<_>, _>>()?;
                let is_glob = words.iter().any(|w| !matches!(w, GlobPart::Words(_)));
                if is_glob {
                    let working_dir_path = {
                        let mut path = Pattern::escape(&config.working_directory.to_string_lossy());
                        path.push('/');
                        path
                    };
                    let stringified_glob = std::iter::once(working_dir_path.clone())
                        .chain(words.iter().map(|w| match w {
                            GlobPart::Words(words) => {
                                words.iter().map(|s| Pattern::escape(s)).join(" ")
                            }
                            GlobPart::Star => "*".to_owned(),
                            GlobPart::Question => "?".to_owned(),
                            GlobPart::SquareOpen => "[".to_owned(),
                            GlobPart::SquareClose => "]".to_owned(),
                        }))
                        .join("");
                    let matches = glob_with(&stringified_glob, MatchOptions::new())
                        .map_err(|e| CommandExecError::InvalidGlob {
                            glob: stringified_glob.clone(),
                            err: e,
                        })?
                        .filter_map(|path| {
                            if let Ok(path) = path {
                                //TODO: This is literally the worst way to handle paths.
                                let owned = path.to_string_lossy().into_owned();
                                if let Some(stripped) = owned.strip_prefix(&working_dir_path) {
                                    Some(stripped.to_owned())
                                } else {
                                    Some(owned)
                                }
                            } else {
                                None
                            }
                        })
                        .collect::<Vec<_>>();
                    if matches.is_empty() {
                        Err(CommandExecError::NoGlobMatches {
                            glob: stringified_glob,
                        })
                    } else {
                        Ok(matches)
                    }
                } else {
                    Ok(vec![words
                        .into_iter()
                        .flat_map(GlobPart::into_string)
                        .join("")])
                }
            }
            ComplexWord::Single(word) => {
                self.evaluate_word(word, config).map(GlobPart::into_string)
            }
        }
    }

    fn evaluate_word(
        &mut self,
        word: &Word,
        config: &ExecConfig,
    ) -> Result<GlobPart, CommandExecError> {
        match word {
            Word::SingleQuoted(literal) => Ok(vec![literal.clone()].into()),
            Word::DoubleQuoted(words) => Ok(vec![words
                .iter()
                .map(|w| {
                    self.evaluate_simple_word(w, config)
                        .map(GlobPart::into_string)
                })
                .flatten_ok()
                .collect::<Result<String, _>>()?]
            .into()),
            Word::Simple(word) => self.evaluate_simple_word(word, config),
        }
    }

    fn evaluate_simple_word(
        &mut self,
        word: &SimpleWord,
        config: &ExecConfig,
    ) -> Result<GlobPart, CommandExecError> {
        match word {
            SimpleWord::Literal(s) => Ok(vec![s.clone()].into()),
            SimpleWord::Escaped(s) => Ok(vec![s.clone()].into()),
            SimpleWord::Param(p) => Ok(self.evaluate_parameter(p, config).into()),
            SimpleWord::Subst(p) => Ok(self.evaluate_param_subst(p, config)?.into()),
            SimpleWord::Star => Ok(GlobPart::Star),
            SimpleWord::Question => Ok(GlobPart::Question),
            SimpleWord::SquareOpen => Ok(GlobPart::SquareOpen),
            SimpleWord::SquareClose => Ok(GlobPart::SquareClose),
            SimpleWord::Tilde => todo!(),
            SimpleWord::Colon => Ok(vec![":".to_owned()].into()),
        }
    }

    fn evaluate_param_subst(
        &mut self,
        param: &ParameterSubstitution,
        config: &ExecConfig,
    ) -> Result<Vec<String>, CommandExecError> {
        Ok(match param {
            ParameterSubstitution::Command(commands) => self
                .exec_command_group(commands, config)
                .map(|output| {
                    Ok(vec![String::from_utf8(output.wait().stdout).map_err(
                        |e| CommandExecError::UnhandledOsString {
                            err: e.utf8_error(),
                        },
                    )?])
                })
                .and_then(std::convert::identity)?,
            ParameterSubstitution::Len(p) => vec![format!(
                "{}",
                match p {
                    Parameter::At | Parameter::Star => config.positional_args.len(),
                    p => self
                        .evaluate_parameter(p, config)
                        .into_iter()
                        .map(|s| s.len())
                        .reduce(|acc, s| acc + s + 1)
                        .unwrap_or(0),
                }
            )],
            ParameterSubstitution::Arith(_) => todo!(),
            ParameterSubstitution::Default(null_is_unset, parameter, default) => {
                let parameter = self.evaluate_parameter(parameter, config);
                if parameter.is_empty()
                    || (*null_is_unset && parameter.len() == 1 && parameter[0].is_empty())
                {
                    if let Some(word) = default {
                        self.evaluate_tl_word(word, config)?
                    } else {
                        vec![]
                    }
                } else {
                    parameter
                }
            }
            ParameterSubstitution::Assign(_, _, _) => todo!(),
            ParameterSubstitution::Error(_, _, _) => todo!(),
            ParameterSubstitution::Alternative(_, _, _) => todo!(),
            ParameterSubstitution::RemoveSmallestSuffix(_, _) => todo!(),
            ParameterSubstitution::RemoveLargestSuffix(_, _) => todo!(),
            ParameterSubstitution::RemoveSmallestPrefix(_, _) => todo!(),
            ParameterSubstitution::RemoveLargestPrefix(_, _) => todo!(),
        })
    }

    fn evaluate_parameter(&mut self, parameter: &Parameter, config: &ExecConfig) -> Vec<String> {
        match parameter {
            Parameter::Positional(n) => config
                .positional_args
                .get(*n as usize)
                .cloned()
                .into_iter()
                .collect(),
            Parameter::Var(name) => self
                .vars
                .get(name)
                .cloned()
                .or_else(|| std::env::var(name).ok())
                .into_iter()
                .collect(),
            Parameter::At => config.positional_args.clone(),
            Parameter::Pound => vec![format!("{}", config.positional_args.len())],
            Parameter::Dollar => vec![format!("{}", std::process::id())],
            Parameter::Question => vec![format!("{}", self.exit_code)],
            Parameter::Bang => vec![format!("{}", self.pid)],

            Parameter::Star => todo!(), // Like @ but runs evaluate_word on each word
            Parameter::Dash => todo!(), // Options of current run invocation. Perhaps could be useful?
        }
    }
}

enum GlobPart {
    Words(Vec<String>),
    Star,
    Question,
    SquareOpen,
    SquareClose,
}

impl GlobPart {
    fn into_string(self) -> Vec<String> {
        match self {
            GlobPart::Words(words) => words,
            GlobPart::Star => vec!["*".to_owned()],
            GlobPart::Question => vec!["?".to_owned()],
            GlobPart::SquareOpen => vec!["[".to_owned()],
            GlobPart::SquareClose => vec!["]".to_owned()],
        }
    }
}

impl From<Vec<String>> for GlobPart {
    fn from(v: Vec<String>) -> Self {
        Self::Words(v)
    }
}
