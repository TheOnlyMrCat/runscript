use std::collections::HashMap;
use std::fs::OpenOptions;
use std::io::Write;
use std::path::{Path, PathBuf};

use std::sync::Arc;

use crate::process::{
    BuiltinCommand, CommandExecError, ProcessExit, SpawnContext, SpawnableProcess, StdinRedirect,
    StdoutRedirect, WaitableProcess,
};

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
    ///The first argument replaces `$0`, the second replaces `$1`, etc.
    pub positional_args: Vec<String>,
}

impl std::fmt::Debug for ExecConfig<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "ExecConfig {{ colour_choice: {:?}, working_directory: {:?}, script_path: {:?}, target_name: {:?}, positional_args: {:?} }}",
            self.colour_choice, self.working_directory, self.script_path, self.target_name, self.positional_args
        )
    }
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
pub struct ShellContext<'a, 'b> {
    /// The current working directory
    pub working_directory: PathBuf,
    /// The current shell variables
    pub vars: HashMap<String, String>,
    /// Exported varables
    pub env: HashMap<String, String>,
    /// Function definitions
    functions: HashMap<String, Arc<CompoundCommand>>,
    /// Stack of function arguments
    function_args: Vec<Vec<String>>,
    /// PID of most recent job
    pid: i32,
    /// Exit code of most recent top-level command
    exit_code: i32, //TODO: This will always be 0. Perhaps an option to have the script keep going even after errors?
    /// Immutable config
    config: &'a ExecConfig<'b>,
}

impl<'a, 'b> ShellContext<'a, 'b> {
    pub fn new(config: &'a ExecConfig<'b>) -> Self {
        Self {
            working_directory: config.working_directory.to_owned(),
            vars: HashMap::new(),
            env: HashMap::new(),
            functions: HashMap::new(),
            function_args: Vec::new(),
            pid: -1,
            exit_code: 0,
            config,
        }
    }
}

impl ShellContext<'_, '_> {
    pub fn exec_command_group(&mut self, script: &[AtomicTopLevelCommand]) -> WaitableProcess {
        use crate::parser::ast::Command;

        let mut jobs = vec![];

        if script.len() > 1 {
            for command in &script[..script.len() - 1] {
                let (command, is_job) = match &command {
                    Command::Job(list) => (list, true),
                    Command::List(list) => (list, false),
                };

                let proc = self.exec_andor_list(command, is_job);
                if is_job {
                    self.pid = proc.pid().unwrap_or(-1);
                    jobs.push(proc);
                } else {
                    let finished = proc.wait();
                    if !finished.status.success() {
                        return WaitableProcess::finished(finished);
                    }
                }
            }
        }

        if !script.is_empty() {
            match &script.last().unwrap() {
                Command::Job(list) => {
                    let proc = self.exec_andor_list(list, true);
                    let mut last_proc = WaitableProcess::empty_success();
                    jobs.push(proc);
                    last_proc.set_jobs(jobs);
                    last_proc
                }
                Command::List(list) => {
                    let mut last_proc = self.exec_andor_list(list, false);
                    last_proc.set_jobs(jobs);
                    last_proc
                }
            }
        } else {
            WaitableProcess::empty_success()
        }
    }

    pub fn exec_as_function(
        &mut self,
        commands: Arc<CompoundCommand>,
        args: Vec<String>,
    ) -> WaitableProcess {
        self.function_args.push(args);
        //TODO: input redirection
        let process = self
            .eval_compound_command(&commands)
            .spawn(SpawnContext::PipelineEnd {
                context: self,
                input: StdinRedirect::Inherit,
            });
        self.function_args.pop();
        process
    }

    fn exec_andor_list(&mut self, command: &AndOrList, is_job: bool) -> WaitableProcess {
        let mut previous_output = self.exec_listable_command(&command.first, is_job);
        for chain in &command.rest {
            match chain {
                AndOr::And(command) => {
                    let finished = previous_output.wait();
                    if !is_job {
                        if let Some(output_stream) = &self.config.output_stream {
                            out::process_finish(output_stream, &finished.status);
                        }
                    }
                    if finished.status.success() {
                        previous_output = self.exec_listable_command(command, is_job);
                    } else {
                        previous_output = WaitableProcess::finished(finished);
                        break;
                    }
                }
                AndOr::Or(command) => {
                    let finished = previous_output.wait();
                    if !is_job {
                        if let Some(output_stream) = &self.config.output_stream {
                            out::process_finish(output_stream, &finished.status);
                        }
                    }
                    if !finished.status.success() {
                        previous_output = self.exec_listable_command(command, is_job);
                    } else {
                        previous_output = WaitableProcess::finished(finished);
                        break;
                    }
                }
            }
        }
        previous_output
    }

    fn exec_listable_command(
        &mut self,
        command: &ListableCommand,
        is_job: bool,
    ) -> WaitableProcess {
        let commands = match command {
            ListableCommand::Pipe(_negate, commands) => {
                commands
                    .iter()
                    .map(|command| self.eval_pipeable_command(command, is_job))
                    .collect() //TODO: negate?
            }
            ListableCommand::Single(command) => {
                vec![self.eval_pipeable_command(command, is_job)]
            }
        };
        let last_command = commands.len() - 1;
        let mut next_input = if is_job {
            StdinRedirect::None
        } else {
            StdinRedirect::Inherit
        };
        for (i, command) in commands.into_iter().enumerate() {
            if i == last_command {
                return command.spawn(SpawnContext::PipelineEnd {
                    context: self,
                    input: next_input,
                });
            } else {
                let input;
                let output;
                #[cfg(unix)]
                {
                    //TODO: O_NONBLOCK, if necessary
                    let (read, write) = nix::unistd::pipe().unwrap();
                    input = std::mem::replace(&mut next_input, StdinRedirect::Fd(read));
                    output = write;
                }
                command.spawn(SpawnContext::PipelineIntermediate {
                    context: &*self,
                    input,
                    output: Some(output),
                });
            }
        }
        unreachable!()
    }

    fn eval_pipeable_command<'a>(
        &mut self,
        command: &'a PipeableCommand,
        is_job: bool,
    ) -> SpawnableProcess<'a> {
        match command {
            PipeableCommand::Simple(command) => self.eval_simple_command(command, is_job),
            PipeableCommand::Compound(command) => self.eval_compound_command(command),
            PipeableCommand::FunctionDef(name, body) => {
                //TODO: Delay registration with context
                self.functions.insert(name.clone(), body.clone());
                SpawnableProcess::empty_success()
            }
        }
    }

    fn eval_compound_command<'a>(&mut self, command: &'a CompoundCommand) -> SpawnableProcess<'a> {
        match &command.kind {
            CompoundCommandKind::Brace(commands) => SpawnableProcess::group(commands),
            CompoundCommandKind::Subshell(commands) => SpawnableProcess::subshell(commands),
            //TODO: Evaluate lazily!
            CompoundCommandKind::While(GuardBodyPair { guard, body }) => {
                while self.exec_command_group(guard).wait().status.success() {
                    self.exec_command_group(body).wait();
                }

                SpawnableProcess::empty_success()
            }
            CompoundCommandKind::Until(GuardBodyPair { guard, body }) => {
                while !self.exec_command_group(guard).wait().status.success() {
                    self.exec_command_group(body).wait();
                }

                SpawnableProcess::empty_success()
            }
            CompoundCommandKind::If {
                conditionals,
                else_branch,
            } => {
                let mut branch = None;

                for GuardBodyPair { guard, body } in conditionals {
                    if self.exec_command_group(guard).wait().status.success() {
                        branch = Some(SpawnableProcess::group(body));
                        break;
                    }
                }

                branch.unwrap_or_else(|| {
                    else_branch
                        .as_deref()
                        .map(SpawnableProcess::group)
                        .unwrap_or_else(SpawnableProcess::empty_success)
                })
            }
            CompoundCommandKind::For { var, words, body } => {
                let mut last_proc = WaitableProcess::empty_success();
                for word in match words
                    .as_ref()
                    .map(|words| -> Result<_, _> {
                        words
                            .iter()
                            .map(|word| self.evaluate_tl_word(word))
                            .flatten_ok()
                            .collect::<Result<Vec<_>, _>>()
                    })
                    .transpose()
                {
                    Ok(words) => words.unwrap_or_else(|| self.config.positional_args.clone()),
                    Err(status) => {
                        return SpawnableProcess::empty_status(status);
                    }
                } {
                    self.vars.insert(var.clone(), word.clone());
                    last_proc = self.exec_command_group(body);
                }

                todo!()
            }
            CompoundCommandKind::Case { word, arms } => {
                let mut last_proc = WaitableProcess::empty_success();
                let word = match self.evaluate_tl_word(word) {
                    Ok(word) => word,
                    Err(status) => {
                        return SpawnableProcess::empty_status(status);
                    }
                };
                for PatternBodyPair { patterns, body } in arms {
                    let mut pattern_matches = false;
                    for pattern in patterns {
                        let pattern = match self.evaluate_tl_word(pattern) {
                            Ok(pattern) => pattern,
                            Err(status) => {
                                return SpawnableProcess::empty_status(status);
                            }
                        };
                        if pattern == word {
                            pattern_matches = true;
                        }
                    }

                    if pattern_matches {
                        last_proc = self.exec_command_group(body);
                        break;
                    }
                }

                todo!()
            }
        }
    }

    fn eval_simple_command<'a>(
        &mut self,
        command: &'a SimpleCommand,
        is_job: bool,
    ) -> SpawnableProcess<'a> {
        use std::process::Command;

        let mut redirects = vec![];

        let env_remaps = match command
            .redirects_or_env_vars
            .iter()
            .filter_map(|r| match r {
                RedirectOrEnvVar::EnvVar(key, word) => Some(
                    word.as_ref()
                        .map(|word| self.evaluate_tl_word(word))
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
            .collect::<Result<Vec<_>, _>>()
        {
            Ok(remaps) => remaps,
            Err(status) => return SpawnableProcess::empty_status(status),
        };

        let command_words = match command
            .redirects_or_cmd_words
            .iter()
            .filter_map(|r| match r {
                RedirectOrCmdWord::CmdWord(w) => Some(self.evaluate_tl_word(w)),
                RedirectOrCmdWord::Redirect(redirect) => {
                    redirects.push(redirect);
                    None
                }
            })
            .collect::<Result<Vec<_>, _>>()
        {
            Ok(words) => words,
            Err(status) => return SpawnableProcess::empty_status(status),
        };

        if !command_words.is_empty() {
            let redirects = match redirects
                .into_iter()
                .map(|redirect| {
                    macro_rules! map_redirect {
                        ($($t:ident),*) => {
                            match redirect {
                                $(Redirect::$t(fd, word) => EvaluatedRedirect::$t(*fd, self.evaluate_tl_word(word)?),)*
                            }
                        }
                    }
                    Ok(map_redirect!(Read, Write, ReadWrite, Append, Clobber, Heredoc, DupRead, DupWrite))
                })
                .collect::<Result<Vec<_>, _>>()
            {
                Ok(redirects) => redirects,
                Err(status) => return SpawnableProcess::empty_status(status),
            };

            if let Some(ref output_stream) = self.config.output_stream {
                out::command_prompt(
                    output_stream,
                    command,
                    &env_remaps,
                    &redirects,
                    &command_words,
                );
            }

            //TODO: I don't think flatten()ing is correct. The internal Vec<String>s should instead be joined.
            let command_words = command_words.into_iter().flatten().collect::<Vec<_>>();

            let mut stdin = if is_job {
                StdinRedirect::None
            } else {
                StdinRedirect::Inherit
            };
            let mut stdout = if is_job {
                StdoutRedirect::None
            } else {
                StdoutRedirect::Inherit
            };
            let mut stderr = if is_job {
                StdoutRedirect::None
            } else {
                StdoutRedirect::Inherit
            };

            for redirect in redirects {
                match redirect {
                    EvaluatedRedirect::Read(fd, word) => {
                        let file = match OpenOptions::new()
                            .read(true)
                            .open(self.working_directory.clone().join(word.join(" ")))
                        {
                            Ok(file) => file,
                            Err(err) => {
                                return SpawnableProcess::empty_error(
                                    CommandExecError::BadRedirect { err },
                                );
                            }
                        };
                        match fd {
                            None | Some(0) => stdin = StdinRedirect::from(file),
                            Some(1) => stdout = StdoutRedirect::from(file),
                            Some(2) => stderr = StdoutRedirect::from(file),
                            Some(_fd) => todo!(), // Might have to use nix::dup2?
                        }
                    }
                    EvaluatedRedirect::Write(fd, word) => {
                        let file = match OpenOptions::new()
                            .write(true)
                            .create(true)
                            .open(self.working_directory.clone().join(word.join(" ")))
                        {
                            Ok(file) => file,
                            Err(err) => {
                                return SpawnableProcess::empty_error(
                                    CommandExecError::BadRedirect { err },
                                );
                            }
                        };
                        match fd {
                            Some(0) => stdin = StdinRedirect::from(file),
                            None | Some(1) => stdout = StdoutRedirect::from(file),
                            Some(2) => stderr = StdoutRedirect::from(file),
                            Some(_fd) => todo!(), // Might have to use nix::dup2?
                        }
                    }
                    EvaluatedRedirect::ReadWrite(fd, word) => {
                        let file = match OpenOptions::new()
                            .read(true)
                            .write(true)
                            .create(true)
                            .open(self.working_directory.clone().join(word.join(" ")))
                        {
                            Ok(file) => file,
                            Err(err) => {
                                return SpawnableProcess::empty_error(
                                    CommandExecError::BadRedirect { err },
                                );
                            }
                        };
                        match fd {
                            None | Some(0) => stdin = StdinRedirect::from(file),
                            Some(1) => stdout = StdoutRedirect::from(file),
                            Some(2) => stderr = StdoutRedirect::from(file),
                            Some(_fd) => todo!(), // Might have to use nix::dup2?
                        }
                    }
                    EvaluatedRedirect::Append(fd, word) => {
                        let file = match OpenOptions::new()
                            .write(true)
                            .append(true)
                            .create(true)
                            .open(self.working_directory.clone().join(word.join(" ")))
                        {
                            Ok(file) => file,
                            Err(err) => {
                                return SpawnableProcess::empty_error(
                                    CommandExecError::BadRedirect { err },
                                );
                            }
                        };
                        match fd {
                            Some(0) => stdin = StdinRedirect::from(file),
                            None | Some(1) => stdout = StdoutRedirect::from(file),
                            Some(2) => stderr = StdoutRedirect::from(file),
                            Some(_fd) => todo!(), // Might have to use nix::dup2?
                        }
                    }
                    EvaluatedRedirect::Clobber(fd, word) => {
                        let file = match OpenOptions::new()
                            .write(true)
                            .create(true)
                            .open(self.working_directory.clone().join(word.join(" ")))
                        {
                            Ok(file) => file,
                            Err(err) => {
                                return SpawnableProcess::empty_error(
                                    CommandExecError::BadRedirect { err },
                                );
                            }
                        };
                        match fd {
                            None | Some(0) => stdin = StdinRedirect::from(file),
                            Some(1) => stdout = StdoutRedirect::from(file),
                            Some(2) => stderr = StdoutRedirect::from(file),
                            Some(_fd) => todo!(), // Might have to use nix::dup2?
                        }
                    }
                    EvaluatedRedirect::Heredoc(fd, word) => {
                        let mut file = match tempfile::tempfile() {
                            Ok(file) => file,
                            Err(err) => {
                                return SpawnableProcess::empty_error(
                                    CommandExecError::BadRedirect { err },
                                );
                            }
                        };
                        for word in word {
                            match file.write_all(word.as_bytes()) {
                                Ok(_) => (),
                                Err(err) => {
                                    return SpawnableProcess::empty_error(
                                        CommandExecError::BadRedirect { err },
                                    );
                                }
                            }
                        }
                        match fd {
                            None | Some(0) => stdin = StdinRedirect::from(file),
                            Some(1) => stdout = StdoutRedirect::from(file),
                            Some(2) => stderr = StdoutRedirect::from(file),
                            Some(_fd) => todo!(), // Might have to use nix::dup2?
                        }
                    }
                    EvaluatedRedirect::DupRead(_fd, _) => {
                        return SpawnableProcess::empty_error(CommandExecError::UnsupportedRedirect)
                    }
                    EvaluatedRedirect::DupWrite(_fd, _) => {
                        return SpawnableProcess::empty_error(CommandExecError::UnsupportedRedirect)
                    }
                }
            }

            let mut process = match command_words[0].as_str() {
                ":" | "true" => SpawnableProcess::builtin(BuiltinCommand::True),
                "false" => SpawnableProcess::builtin(BuiltinCommand::False),
                #[cfg(feature = "dev-panic")]
                "__run_panic" => SpawnableProcess::builtin(BuiltinCommand::Panic),
                "cd" => {
                    //TODO: Extended cd options
                    SpawnableProcess::builtin(BuiltinCommand::Cd {
                        path: self.working_directory.join(&command_words[1]),
                    })
                }
                "export" => {
                    let name = &command_words[1];
                    if let Some((name, value)) = name.split_once('=') {
                        SpawnableProcess::builtin(BuiltinCommand::Export {
                            key: name.to_string(),
                            value: value.to_string(),
                        })
                    } else {
                        SpawnableProcess::builtin(BuiltinCommand::Export {
                            key: name.to_string(),
                            value: self.vars[name].clone(),
                        })
                    }
                }
                "unset" => {
                    let name = &command_words[1];
                    SpawnableProcess::builtin(BuiltinCommand::Unset {
                        key: name.to_string(),
                    })
                }
                "source" => SpawnableProcess::builtin(BuiltinCommand::Source {
                    path: self.working_directory.join(&command_words[1]),
                    cwd: self.working_directory.clone(),
                }),
                #[cfg(unix)]
                "run" => SpawnableProcess::reentrant(
                    self.env
                        .iter()
                        .map(|(k, v)| (k.clone(), v.clone()))
                        .collect(),
                    Some(self.working_directory.clone()),
                    BaseExecContext {
                        current_file: self.config.script_path.clone(),
                        current_target: self.config.target_name.map(ToOwned::to_owned),
                        args: {
                            let mut args = command_words;
                            args.remove(0);
                            args
                        },
                        colour_choice: self.config.colour_choice,
                    },
                ),
                _ => {
                    if let Some(commands) = self.functions.get(&command_words[0]).cloned() {
                        SpawnableProcess::function(commands, command_words)
                    } else {
                        let mut command = Command::new(&command_words[0]);
                        command
                            .args(&command_words[1..])
                            .envs(&self.env)
                            .envs(env_remaps)
                            .current_dir(self.working_directory.clone());
                        SpawnableProcess::process(command)
                    }
                }
            };
            process.stdin = stdin;
            process.stdout = stdout;
            process.stderr = stderr;
            process
        } else {
            //TODO: Evaluate lazily, to allow this method to take &self, and to restore correct behaviour.
            if let Some(ref output_stream) = self.config.output_stream {
                out::env_remaps(output_stream, &env_remaps);
            }

            self.vars.reserve(env_remaps.len());
            for (key, value) in env_remaps {
                self.vars.insert(key, value);
            }

            SpawnableProcess::empty_success()
        }
    }

    fn evaluate_tl_word(&self, word: &ComplexWord) -> Result<Vec<String>, ProcessExit> {
        match word {
            ComplexWord::Concat(words) => {
                let words = words
                    .iter()
                    .map(|w| self.evaluate_word(w))
                    .collect::<Result<Vec<_>, _>>()?;
                let is_glob = words.iter().any(|w| !matches!(w, GlobPart::Words(_)));
                if is_glob {
                    let working_dir_path = {
                        let mut path = Pattern::escape(&self.working_directory.to_string_lossy());
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
                        .map_err(|e| {
                            ProcessExit::ExecError(CommandExecError::InvalidGlob {
                                glob: stringified_glob.clone(),
                                err: e,
                            })
                        })?
                        .filter_map(|path| {
                            if let Ok(path) = path {
                                //TODO: This is not a good way to handle paths.
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
                        Err(ProcessExit::ExecError(CommandExecError::NoGlobMatches {
                            glob: stringified_glob,
                        }))
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
            ComplexWord::Single(word) => self.evaluate_word(word).map(GlobPart::into_string),
        }
    }

    fn evaluate_word(&self, word: &Word) -> Result<GlobPart, ProcessExit> {
        match word {
            Word::SingleQuoted(literal) => Ok(vec![literal.clone()].into()),
            Word::DoubleQuoted(words) => Ok(vec![words
                .iter()
                .map(|w| self.evaluate_simple_word(w).map(GlobPart::into_string))
                .flatten_ok()
                .collect::<Result<String, _>>()?]
            .into()),
            Word::Simple(word) => self.evaluate_simple_word(word),
        }
    }

    fn evaluate_simple_word(&self, word: &SimpleWord) -> Result<GlobPart, ProcessExit> {
        match word {
            SimpleWord::Literal(s) => Ok(vec![s.clone()].into()),
            SimpleWord::Escaped(s) => Ok(vec![s.clone()].into()),
            SimpleWord::Param(p) => Ok(self.evaluate_parameter(p).into()),
            SimpleWord::Subst(p) => Ok(self.evaluate_param_subst(p)?.into()),
            SimpleWord::Star => Ok(GlobPart::Star),
            SimpleWord::Question => Ok(GlobPart::Question),
            SimpleWord::SquareOpen => Ok(GlobPart::SquareOpen),
            SimpleWord::SquareClose => Ok(GlobPart::SquareClose),
            SimpleWord::Tilde => todo!(),
            SimpleWord::Colon => Ok(vec![":".to_owned()].into()),
        }
    }

    fn evaluate_param_subst(
        &self,
        param: &ParameterSubstitution,
    ) -> Result<Vec<String>, ProcessExit> {
        Ok(match param {
            ParameterSubstitution::Command(commands) => {
                let output = self.clone().exec_command_group(commands).wait();
                if !output.status.success() {
                    return Err(output.status);
                }
                vec![String::from_utf8(output.stdout).map_err(|e| {
                    ProcessExit::ExecError(CommandExecError::UnhandledOsString {
                        err: e.utf8_error(),
                    })
                })?]
            }
            ParameterSubstitution::Len(p) => vec![format!(
                "{}",
                match p {
                    Parameter::At | Parameter::Star => self.config.positional_args.len() - 1,
                    p => self
                        .evaluate_parameter(p)
                        .into_iter()
                        .map(|s| s.len())
                        .reduce(|acc, s| acc + s + 1)
                        .unwrap_or(0),
                }
            )],
            ParameterSubstitution::Arith(_) => todo!(),
            ParameterSubstitution::Default(null_is_unset, parameter, default) => {
                let parameter = self.evaluate_parameter(parameter);
                if parameter.is_empty()
                    || (*null_is_unset && parameter.len() == 1 && parameter[0].is_empty())
                {
                    if let Some(word) = default {
                        self.evaluate_tl_word(word)?
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

    fn evaluate_parameter(&self, parameter: &Parameter) -> Vec<String> {
        match parameter {
            Parameter::Positional(n) => match self.function_args.last() {
                Some(args) => args.get(*n).cloned().into_iter().collect(),
                None => self
                    .config
                    .positional_args
                    .get(*n)
                    .cloned()
                    .into_iter()
                    .collect(),
            },
            Parameter::Var(name) => self
                .vars
                .get(name)
                .cloned()
                .or_else(|| std::env::var(name).ok())
                .into_iter()
                .collect(),
            Parameter::At => match self.function_args.last() {
                Some(args) => args,
                None => &self.config.positional_args,
            }
            .iter()
            .skip(1)
            .cloned()
            .collect(),
            Parameter::Pound => vec![format!("{}", self.config.positional_args.len() - 1)],
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
