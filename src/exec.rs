use std::collections::HashMap;
use std::fs::OpenOptions;
use std::io::{Read, Write};
use std::path::{Path, PathBuf};
use std::process::{Child, ExitStatus, Output, Stdio};
use std::str::Utf8Error;
use std::sync::Arc;

use conch_parser::ast::{
    AndOr, Arithmetic, AtomicCommandList, AtomicShellPipeableCommand, AtomicTopLevelCommand,
    AtomicTopLevelWord, ComplexWord, CompoundCommandKind, GuardBodyPair, ListableCommand,
    Parameter, ParameterSubstitution, PatternBodyPair, PipeableCommand, Redirect,
    RedirectOrCmdWord, RedirectOrEnvVar, SimpleCommand, SimpleWord, Word,
};
use glob::{glob_with, MatchOptions, Pattern, PatternError};
use itertools::Itertools;

use crate::Script;

#[derive(Clone)]
pub struct ExecConfig<'a> {
    /// The output stream to output to, or `None` to produce no output
    pub output_stream: Option<Arc<termcolor::StandardStream>>,
    /// The working directory to execute the script's commands in
    pub working_directory: &'a Path,
    /// Positional arguments to pass to the script.
    ///
    ///The first argument replaces `$1`, the second replaces `$2`, etc.
    pub positional_args: Vec<String>,
    /// Whether to store the text printed to stdout by the executed programs
    pub capture_stdout: bool,
    /// A map of environment variables to remap
    pub env_remap: &'a HashMap<String, String>,
}

#[derive(Debug)]
pub enum ProcessExit {
    Bool(bool),
    Status(ExitStatus),
}

#[derive(Debug)]
pub struct FinishedProcess {
    pub status: ProcessExit,
    pub stdout: Vec<u8>,
    pub stderr: Vec<u8>,
}

#[derive(Debug)]
pub enum CommandExecError {
    InvalidGlob {
        glob: String,
        err: PatternError,
        // loc: RunscriptLocation,
    },
    NoGlobMatches {
        glob: String,
        // loc: RunscriptLocation,
    },
    CommandFailed {
        err: std::io::Error,
        // loc: RunscriptLocation,
    },
    UnhandledOsString {
        err: Utf8Error,
        // loc: RunscriptLocation,
    },
    BadRedirect {
        err: std::io::Error,
        // loc: RunscriptLocation,
    },
}

impl ProcessExit {
    /// Whether the process was successful or not.
    ///
    /// Returns the value of a `Bool` variant, or calls the `success` function on a `Status` variant
    pub fn success(&self) -> bool {
        match self {
            ProcessExit::Bool(b) => *b,
            ProcessExit::Status(s) => s.success(),
        }
    }

    /// The exit code the process exited with, if any.
    ///
    /// Coerces the `Bool` variant into `true = 0`, `false = 1`
    pub fn code(&self) -> Option<i32> {
        match self {
            ProcessExit::Bool(b) => Some(!b as i32),
            ProcessExit::Status(s) => s.code(),
        }
    }
}

enum OngoingProcess {
    Concurrent(Child),
    Finished(FinishedProcess),
}

impl OngoingProcess {
    fn pipe_out(&mut self) -> PipeInput {
        match self {
            OngoingProcess::Concurrent(p) => match p.stdout.take() {
                Some(stdout) => PipeInput::Pipe(stdout.into()),
                None => PipeInput::None,
            },
            OngoingProcess::Finished(p) => PipeInput::Buffer(p.stdout.clone()),
        }
    }
}

struct WaitableProcess {
    process: OngoingProcess,
    associated_jobs: Vec<WaitableProcess>,
}

impl WaitableProcess {
    fn new(process: Child) -> WaitableProcess {
        WaitableProcess {
            process: OngoingProcess::Concurrent(process),
            associated_jobs: vec![],
        }
    }

    fn finished(process: FinishedProcess) -> WaitableProcess {
        WaitableProcess {
            process: OngoingProcess::Finished(process),
            associated_jobs: vec![],
        }
    }

    fn empty_success() -> WaitableProcess {
        WaitableProcess {
            process: OngoingProcess::Finished(FinishedProcess {
                status: ProcessExit::Bool(true),
                stdout: vec![],
                stderr: vec![],
            }),
            associated_jobs: vec![],
        }
    }

    #[cfg(unix)]
    fn hup(self) -> FinishedProcess {
        use std::os::unix::prelude::ExitStatusExt;

        use nix::sys::signal::{kill, Signal};
        use nix::unistd::Pid;

        for job in self.associated_jobs {
            job.hup();
        }

        match self.process {
            OngoingProcess::Concurrent(process) => {
                kill(Pid::from_raw(process.id() as i32), Signal::SIGHUP).unwrap();
                FinishedProcess {
                    status: ProcessExit::Status(ExitStatus::from_raw(129)),
                    stdout: vec![],
                    stderr: vec![],
                }
            }
            OngoingProcess::Finished(proc) => proc,
        }
    }

    #[cfg(windows)]
    fn hup(mut self) -> FinishedProcess {
        todo!()
    }

    fn wait(self) -> FinishedProcess {
        match self.process {
            OngoingProcess::Concurrent(mut process) => {
                let status = process.wait().unwrap();
                let stdout = if let Some(mut stdout) = process.stdout {
                    let mut v = Vec::new();
                    let _ = stdout.read_to_end(&mut v); //TODO: should I handle an error here?
                    v
                } else {
                    Vec::new()
                };
                let stderr = if let Some(mut stderr) = process.stderr {
                    let mut v = Vec::new();
                    let _ = stderr.read_to_end(&mut v); //TODO: should I handle an error here?
                    v
                } else {
                    Vec::new()
                };

                for job in self.associated_jobs.into_iter() {
                    job.hup();
                }

                FinishedProcess {
                    status: ProcessExit::Status(status),
                    stdout,
                    stderr,
                }
            }
            OngoingProcess::Finished(proc) => {
                for job in self.associated_jobs.into_iter() {
                    job.hup();
                }
                proc
            }
        }
    }
}

impl From<Child> for WaitableProcess {
    fn from(process: Child) -> WaitableProcess {
        WaitableProcess::new(process)
    }
}

struct Pipe {
    stdin: PipeInput,
    stdout: bool,
}

impl Pipe {
    fn no_pipe() -> Pipe {
        Pipe {
            stdin: PipeInput::Inherit,
            stdout: false,
        }
    }

    fn pipe_out() -> Pipe {
        Pipe {
            stdin: PipeInput::Inherit,
            stdout: true,
        }
    }

    fn pipe_in(stdin: impl Into<PipeInput>) -> Pipe {
        Pipe {
            stdin: stdin.into(),
            stdout: false,
        }
    }

    fn pipe_in_out(stdin: impl Into<PipeInput>) -> Pipe {
        Pipe {
            stdin: stdin.into(),
            stdout: true,
        }
    }
}

enum PipeInput {
    None,
    Inherit,
    Pipe(Stdio),
    Buffer(Vec<u8>),
}

impl From<Stdio> for PipeInput {
    fn from(stdin: Stdio) -> PipeInput {
        PipeInput::Pipe(stdin)
    }
}

impl From<Vec<u8>> for PipeInput {
    fn from(stdin: Vec<u8>) -> PipeInput {
        PipeInput::Buffer(stdin)
    }
}

impl From<Output> for FinishedProcess {
    fn from(o: Output) -> Self {
        FinishedProcess {
            status: ProcessExit::Status(o.status),
            stdout: o.stdout,
            stderr: o.stderr,
        }
    }
}

pub fn exec_script(
    script: &Script,
    config: &ExecConfig,
) -> Result<FinishedProcess, CommandExecError> {
    Ok(ShellContext {
        working_directory: config.working_directory.to_owned(),
        vars: HashMap::new(),
        env: HashMap::new(),
    }
    .exec_script_entries(&script.commands, config)?
    .wait())
}

#[derive(Debug, Clone)]
struct ShellContext {
    /// The current working directory
    working_directory: PathBuf,
    /// The current shell variables
    vars: HashMap<String, String>,
    /// Exported varables
    env: HashMap<String, String>,
}

impl ShellContext {
    fn exec_script_entries(
        &mut self,
        commands: &[AtomicTopLevelCommand<String>],
        config: &ExecConfig,
    ) -> Result<WaitableProcess, CommandExecError> {
        use conch_parser::ast::Command;

        let mut jobs = vec![];

        if commands.len() > 1 {
            for AtomicTopLevelCommand(command) in &commands[..commands.len() - 1] {
                let (command, is_job) = match command {
                    Command::Job(list) => (list, true),
                    Command::List(list) => (list, false),
                };

                let proc = self.exec_andor_list(command, config)?;
                if is_job {
                    jobs.push(proc);
                } else {
                    proc.wait();
                }
            }
        }

        let mut last_proc = match &commands.last().unwrap().0 {
            Command::Job(list) => self.exec_andor_list(list, config)?,
            Command::List(list) => self.exec_andor_list(list, config)?,
        };

        last_proc.associated_jobs = jobs;

        Ok(last_proc)
    }

    fn exec_andor_list(
        &mut self,
        command: &AtomicCommandList<
            String,
            AtomicTopLevelWord<String>,
            AtomicTopLevelCommand<String>,
        >,
        config: &ExecConfig,
    ) -> Result<WaitableProcess, CommandExecError> {
        let mut previous_output = self.exec_listable_command(&command.first, config)?;
        for chain in &command.rest {
            match chain {
                AndOr::And(command) => {
                    let finished = previous_output.wait();
                    if finished.status.success() {
                        previous_output = self.exec_listable_command(command, config)?;
                    } else {
                        previous_output = WaitableProcess::finished(finished);
                        break;
                    }
                }
                AndOr::Or(command) => {
                    let finished = previous_output.wait();
                    if !finished.status.success() {
                        previous_output = self.exec_listable_command(command, config)?;
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
        command: &ListableCommand<
            AtomicShellPipeableCommand<
                String,
                AtomicTopLevelWord<String>,
                AtomicTopLevelCommand<String>,
            >,
        >,
        config: &ExecConfig,
    ) -> Result<WaitableProcess, CommandExecError> {
        match command {
            ListableCommand::Pipe(_negate, commands) => {
                let mut proc = self.exec_pipeable_command(
                    commands.first().unwrap(),
                    Pipe::pipe_out(),
                    config,
                )?;
                for command in &commands[1..commands.len() - 1] {
                    let next_proc = self.exec_pipeable_command(
                        command,
                        Pipe::pipe_in_out(proc.process.pipe_out()),
                        config,
                    )?;
                    proc = next_proc;
                }
                let last_proc = self.exec_pipeable_command(
                    commands.last().unwrap(),
                    Pipe::pipe_in(proc.process.pipe_out()),
                    config,
                )?;
                Ok(last_proc) //TODO: Negate?
            }
            ListableCommand::Single(command) => {
                self.exec_pipeable_command(command, Pipe::no_pipe(), config)
            }
        }
    }

    fn exec_pipeable_command(
        &mut self,
        command: &AtomicShellPipeableCommand<
            String,
            AtomicTopLevelWord<String>,
            AtomicTopLevelCommand<String>,
        >,
        pipe: Pipe,
        config: &ExecConfig,
    ) -> Result<WaitableProcess, CommandExecError> {
        match command {
            PipeableCommand::Simple(command) => self.exec_simple_command(command, pipe, config),
            PipeableCommand::Compound(command) => match &command.kind {
                CompoundCommandKind::Brace(commands) => self.exec_script_entries(commands, config),
                CompoundCommandKind::Subshell(commands) => {
                    let mut context = self.clone();
                    context.exec_script_entries(commands, config)
                }
                CompoundCommandKind::While(GuardBodyPair { guard, body }) => {
                    while self
                        .exec_script_entries(guard, config)?
                        .wait()
                        .status
                        .success()
                    {
                        self.exec_script_entries(body, config)?.wait();
                    }

                    Ok(WaitableProcess::empty_success())
                }
                CompoundCommandKind::Until(GuardBodyPair { guard, body }) => {
                    while !self
                        .exec_script_entries(guard, config)?
                        .wait()
                        .status
                        .success()
                    {
                        self.exec_script_entries(body, config)?.wait();
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
                            .exec_script_entries(guard, config)?
                            .wait()
                            .status
                            .success()
                        {
                            last_proc = self.exec_script_entries(body, config)?;
                        }
                    }

                    if let Some(else_branch) = else_branch {
                        last_proc = self.exec_script_entries(else_branch, config)?;
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
                        last_proc = self.exec_script_entries(body, config)?;
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
                            last_proc = self.exec_script_entries(body, config)?;
                            break;
                        }
                    }

                    Ok(last_proc)
                }
            },
            PipeableCommand::FunctionDef(_name, _body) => {
                todo!() //TODO: What to do here?? Store it in a hashmap?
                        // Should I even support functions at all?
                        // Probably not, since that's kinda what I'm doing anyway.
            }
        }
    }

    fn exec_simple_command(
        &mut self,
        command: &SimpleCommand<
            String,
            AtomicTopLevelWord<String>,
            Redirect<AtomicTopLevelWord<String>>,
        >,
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
            .flatten_ok()
            .collect::<Result<Vec<_>, _>>()?;

        if !command_words.is_empty() {
            // TODO: Print pre-evaluated command words
            eprintln!("> {}", command_words.join(" "));

            let mut stdin_buffer = None;
            let mut stdin = match pipe.stdin {
                PipeInput::None => Stdio::null(), //TODO: This by default if command is job
                PipeInput::Inherit => Stdio::inherit(),
                PipeInput::Pipe(stdio) => stdio,
                PipeInput::Buffer(v) => {
                    stdin_buffer = Some(v);
                    Stdio::piped()
                }
            };
            let mut stdout = if pipe.stdout {
                Stdio::piped()
            } else {
                Stdio::inherit()
            };
            let mut stderr = Stdio::inherit();

            for redirect in redirects {
                match redirect {
                    Redirect::Read(fd, word) => {
                        let file = OpenOptions::new()
                            .read(true)
                            .open(
                                self.working_directory.clone().join(
                                    self.evaluate_tl_word(word, config)?
                                        .last()
                                        .cloned()
                                        .unwrap_or_else(|| "".to_owned()),
                                ),
                            )
                            .map_err(|e| CommandExecError::BadRedirect { err: e })?; //TODO: Handle errors
                        match fd {
                            None | Some(0) => stdin = Stdio::from(file),
                            Some(1) => stdout = Stdio::from(file),
                            Some(2) => stderr = Stdio::from(file),
                            Some(_fd) => todo!(), // Might have to use nix::dup2?
                        }
                    }
                    Redirect::Write(fd, word) => {
                        let file = OpenOptions::new()
                            .write(true)
                            .create(true)
                            .open(
                                self.working_directory.clone().join(
                                    self.evaluate_tl_word(word, config)?
                                        .last()
                                        .cloned()
                                        .unwrap_or_else(|| "".to_owned()),
                                ),
                            )
                            .map_err(|e| CommandExecError::BadRedirect { err: e })?; //TODO: Handle errors
                        match fd {
                            Some(0) => stdin = Stdio::from(file),
                            None | Some(1) => stdout = Stdio::from(file),
                            Some(2) => stderr = Stdio::from(file),
                            Some(_fd) => todo!(), // Might have to use nix::dup2?
                        }
                    }
                    Redirect::ReadWrite(fd, word) => {
                        let file = OpenOptions::new()
                            .read(true)
                            .write(true)
                            .create(true)
                            .open(
                                self.working_directory.clone().join(
                                    self.evaluate_tl_word(word, config)?
                                        .last()
                                        .cloned()
                                        .unwrap_or_else(|| "".to_owned()),
                                ),
                            )
                            .map_err(|e| CommandExecError::BadRedirect { err: e })?; //TODO: Handle errors
                        match fd {
                            None | Some(0) => stdin = Stdio::from(file),
                            Some(1) => stdout = Stdio::from(file),
                            Some(2) => stderr = Stdio::from(file),
                            Some(_fd) => todo!(), // Might have to use nix::dup2?
                        }
                    }
                    Redirect::Append(fd, word) => {
                        let file = OpenOptions::new()
                            .write(true)
                            .append(true)
                            .create(true)
                            .open(
                                self.working_directory.clone().join(
                                    self.evaluate_tl_word(word, config)?
                                        .last()
                                        .cloned()
                                        .unwrap_or_else(|| "".to_owned()),
                                ),
                            )
                            .map_err(|e| CommandExecError::BadRedirect { err: e })?; //TODO: Handle errors
                        match fd {
                            None | Some(0) => stdin = Stdio::from(file),
                            Some(1) => stdout = Stdio::from(file),
                            Some(2) => stderr = Stdio::from(file),
                            Some(_fd) => todo!(), // Might have to use nix::dup2?
                        }
                    }
                    Redirect::Clobber(fd, word) => {
                        let file = OpenOptions::new()
                            .write(true)
                            .create(true)
                            .open(
                                self.working_directory.clone().join(
                                    self.evaluate_tl_word(word, config)?
                                        .last()
                                        .cloned()
                                        .unwrap_or_else(|| "".to_owned()),
                                ),
                            )
                            .map_err(|e| CommandExecError::BadRedirect { err: e })?; //TODO: Handle errors
                        match fd {
                            None | Some(0) => stdin = Stdio::from(file),
                            Some(1) => stdout = Stdio::from(file),
                            Some(2) => stderr = Stdio::from(file),
                            Some(_fd) => todo!(), // Might have to use nix::dup2?
                        }
                    }
                    Redirect::Heredoc(_fd, _) => todo!(),
                    Redirect::DupRead(_fd, _) => todo!(),
                    Redirect::DupWrite(_fd, _) => todo!(),
                }
            }

            match command_words[0].as_str() {
                ":" => Ok(WaitableProcess::empty_success()),
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
                        // - The child is using a piped stdout
                        // - The stdin buffer is larger than the pipe buffer
                        // - The child writes more than one pipe buffer of data without reading enough of stdin
                        //? Could run this on separate thread to mitigate this, if necessary.
                        let _ = child.stdin.take().unwrap().write_all(&stdin); //TODO: Do I need to worry about an error here?
                    }

                    Ok(child.into())
                }
            }
        } else {
            self.vars.reserve(env_remaps.len());
            for (key, value) in env_remaps {
                self.vars.insert(key, value);
            }
            Ok(WaitableProcess::empty_success())
        }
    }

    fn evaluate_tl_word(
        &mut self,
        AtomicTopLevelWord(word): &AtomicTopLevelWord<String>,
        config: &ExecConfig,
    ) -> Result<Vec<String>, CommandExecError> {
        match word {
            ComplexWord::Concat(words) => {
                let words = words
                    .iter()
                    .map(|w| self.evaluate_word(w, config))
                    .collect::<Result<Vec<_>, _>>()?;
                let is_glob = !words.iter().all(|w| matches!(w, GlobPart::Words(_)));
                if is_glob {
                    let working_dir_path = {
                        let mut path = Pattern::escape(
                            &config.working_directory.to_string_lossy().into_owned(),
                        );
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
                    Ok(words.into_iter().flat_map(GlobPart::into_string).collect())
                }
            }
            ComplexWord::Single(word) => {
                self.evaluate_word(word, config).map(GlobPart::into_string)
            }
        }
    }

    fn evaluate_word(
        &mut self,
        word: &Word<
            String,
            SimpleWord<
                String,
                Parameter<String>,
                Box<
                    ParameterSubstitution<
                        Parameter<String>,
                        AtomicTopLevelWord<String>,
                        AtomicTopLevelCommand<String>,
                        Arithmetic<String>,
                    >,
                >,
            >,
        >,
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
        word: &SimpleWord<
            String,
            Parameter<String>,
            Box<
                ParameterSubstitution<
                    Parameter<String>,
                    AtomicTopLevelWord<String>,
                    AtomicTopLevelCommand<String>,
                    Arithmetic<String>,
                >,
            >,
        >,
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
        param: &ParameterSubstitution<
            Parameter<String>,
            AtomicTopLevelWord<String>,
            AtomicTopLevelCommand<String>,
            Arithmetic<String>,
        >,
        config: &ExecConfig,
    ) -> Result<Vec<String>, CommandExecError> {
        Ok(match param {
            ParameterSubstitution::Command(commands) => self
                .exec_script_entries(commands, config)
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
            ParameterSubstitution::Default(_, _, _) => todo!(),
            ParameterSubstitution::Assign(_, _, _) => todo!(),
            ParameterSubstitution::Error(_, _, _) => todo!(),
            ParameterSubstitution::Alternative(_, _, _) => todo!(),
            ParameterSubstitution::RemoveSmallestSuffix(_, _) => todo!(),
            ParameterSubstitution::RemoveLargestSuffix(_, _) => todo!(),
            ParameterSubstitution::RemoveSmallestPrefix(_, _) => todo!(),
            ParameterSubstitution::RemoveLargestPrefix(_, _) => todo!(),
        })
    }

    fn evaluate_parameter(
        &mut self,
        parameter: &Parameter<String>,
        config: &ExecConfig,
    ) -> Vec<String> {
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

            Parameter::Star => todo!(), // Like @ but runs evaluate_word on each word
            Parameter::Question => todo!(), // Exit code of previous top-level command
            Parameter::Dash => todo!(), // Options of current run invocation. Perhaps could be useful?
            Parameter::Bang => todo!(), // PID of most recent job
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

#[cfg(unix)]
use std::os::raw::{c_char, c_int};

#[cfg(unix)]
extern "C" {
    fn strsignal(sig: c_int) -> *const c_char;
}

#[cfg(unix)]
fn signal(status: &ExitStatus) -> String {
    use std::ffi::CStr;
    use std::os::unix::process::ExitStatusExt;

    let signal = status.signal().expect("Expected signal");

    // SAFETY: No input is invalid.
    let sigstr_ptr = unsafe { strsignal(signal as c_int) };

    if sigstr_ptr.is_null() {
        format!("signal {}", signal)
    } else {
        // SAFETY: The returned string is valid until the next call to strsignal, and has been verified to be non-null.
        let sigstr = unsafe { CStr::from_ptr(sigstr_ptr) };
        format!("signal {} ({})", signal, sigstr.to_string_lossy())
    }
}

#[cfg(not(unix))]
fn signal(_: &ExitStatus) -> String {
    panic!("Non-unix program terminated with signal");
}
