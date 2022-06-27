use std::collections::HashMap;
use std::fs::OpenOptions;
use std::io::Write;
use std::path::{Path, PathBuf};

use std::sync::Arc;

use crate::process::{
    BuiltinCommand, CommandExecError, ProcessExit, RedirectConfig, SpawnContext, SpawnableProcess,
    StdinRedirect, StdoutRedirect, WaitableProcess,
};

use crate::parser::ast::{
    AndOr, AndOrList, AtomicTopLevelCommand, ComplexWord, CompoundCommand, CompoundCommandKind,
    GuardBodyPair, ListableCommand, Parameter, ParameterSubstitution, PatternBodyPair,
    PipeableCommand, Redirect, RedirectOrCmdWord, RedirectOrEnvVar, SimpleCommand, SimpleWord,
    Word,
};
use crate::ptr::Unique;
use itertools::Itertools;

use crate::out::{
    Printable, PrintableCommand, PrintableComplexWord, PrintableEnvRemap, PrintableRedirect,
    PrintableSimpleWord, PrintableWord,
};
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
    Read(Option<u16>, String),
    Write(Option<u16>, String),
    ReadWrite(Option<u16>, String),
    Append(Option<u16>, String),
    Clobber(Option<u16>, String),
    Heredoc(Option<u16>, Vec<String>),
    DupRead(Option<u16>, String),
    DupWrite(Option<u16>, String),
}

#[derive(Debug, Clone)]
pub struct ShellContext<'a, 'b> {
    //TODO: These should be encapsulated?
    /// The current working directory
    pub working_directory: PathBuf,
    /// The current shell variables
    pub vars: HashMap<String, String>,
    /// Exported varables
    pub env: HashMap<String, String>,
    /// Function definitions
    pub functions: HashMap<String, Arc<CompoundCommand>>,
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
        self.exec_command_group_with_default_redirects(script, RedirectConfig::default_fg())
    }

    pub fn exec_command_group_with_default_redirects(
        &mut self,
        script: &[AtomicTopLevelCommand],
        redir: RedirectConfig,
    ) -> WaitableProcess {
        use crate::parser::ast::Command;

        let mut jobs = vec![];

        if script.len() > 1 {
            for command in &script[..script.len() - 1] {
                let (command, is_job) = match &command {
                    Command::Job(list) => (list, true),
                    Command::List(list) => (list, false),
                };

                let proc = self.exec_andor_list(command, redir);
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
                    let proc = self.exec_andor_list(list, RedirectConfig::default_bg());
                    let mut last_proc = WaitableProcess::empty_success();
                    jobs.push(proc);
                    last_proc.set_jobs(jobs);
                    last_proc
                }
                Command::List(list) => {
                    let mut last_proc = self.exec_andor_list(list, redir);
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
        redir: RedirectConfig,
    ) -> WaitableProcess {
        self.function_args.push(args);
        let process = self.exec_compound_command(&commands, redir);
        self.function_args.pop();
        process
    }

    pub fn exec_compound_command(
        &mut self,
        command: &CompoundCommand,
        redir: RedirectConfig,
    ) -> WaitableProcess {
        Self::exec_compound_command_unique(Unique::Borrowed(self), command, redir)
    }

    pub fn exec_compound_command_unique(
        mut this: Unique<Self>,
        command: &CompoundCommand,
        redir: RedirectConfig,
    ) -> WaitableProcess {
        match &command.kind {
            CompoundCommandKind::Brace(commands) => {
                this.exec_command_group_with_default_redirects(commands, redir)
            }
            CompoundCommandKind::Subshell(commands) => {
                this.exec_command_group_with_default_redirects(commands, redir)
            }
            CompoundCommandKind::While(GuardBodyPair { guard, body }) => {
                while this.exec_command_group(guard).wait().status.success() {
                    this.exec_command_group(body).wait();
                }

                WaitableProcess::empty_success()
            }
            CompoundCommandKind::Until(GuardBodyPair { guard, body }) => {
                while !this.exec_command_group(guard).wait().status.success() {
                    this.exec_command_group(body).wait();
                }

                WaitableProcess::empty_success()
            }
            CompoundCommandKind::If {
                conditionals,
                else_branch,
            } => {
                let mut branch = None;

                for GuardBodyPair { guard, body } in conditionals {
                    if this.exec_command_group(guard).wait().status.success() {
                        branch = Some(&**body);
                        break;
                    }
                }

                this.exec_command_group(
                    branch.unwrap_or_else(|| else_branch.as_deref().unwrap_or(&[])),
                )
            }
            CompoundCommandKind::For { var, words, body } => {
                let mut last_proc = WaitableProcess::empty_success();
                for word in match words
                    .as_ref()
                    .map(|words| -> Result<_, _> {
                        words
                            .iter()
                            .map(|word| this.evaluate_tl_word(word).map(|(v, _)| v))
                            .flatten_ok()
                            .collect::<Result<Vec<_>, _>>()
                    })
                    .transpose()
                {
                    Ok(words) => words.unwrap_or_else(|| this.config.positional_args.clone()),
                    Err(status) => {
                        return WaitableProcess::empty_status(status);
                    }
                } {
                    this.vars.insert(var.clone(), word.clone());
                    last_proc = this.exec_command_group(body);
                }

                last_proc
            }
            CompoundCommandKind::Case { word, arms } => {
                let mut last_proc = WaitableProcess::empty_success();
                let word = match this.evaluate_tl_word(word) {
                    Ok(word) => word.0,
                    Err(status) => {
                        return WaitableProcess::empty_status(status);
                    }
                };
                for PatternBodyPair { patterns, body } in arms {
                    let mut pattern_matches = false;
                    for pattern in patterns {
                        let pattern = match this.evaluate_tl_word(pattern) {
                            Ok(pattern) => pattern.0,
                            Err(status) => {
                                return WaitableProcess::empty_status(status);
                            }
                        };
                        if pattern == word {
                            pattern_matches = true;
                        }
                    }

                    if pattern_matches {
                        last_proc = this.exec_command_group(body);
                        break;
                    }
                }

                last_proc
            }
        }
    }

    fn exec_andor_list(&mut self, command: &AndOrList, redir: RedirectConfig) -> WaitableProcess {
        let mut previous_output = self.exec_listable_command(&command.first, redir);
        for chain in &command.rest {
            match chain {
                AndOr::And(command) => {
                    let finished = previous_output.wait();
                    if finished.status.success() {
                        previous_output = self.exec_listable_command(command, redir);
                    } else {
                        previous_output = WaitableProcess::finished(finished);
                        break;
                    }
                }
                AndOr::Or(command) => {
                    let finished = previous_output.wait();
                    if !finished.status.success() {
                        previous_output = self.exec_listable_command(command, redir);
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
        redir: RedirectConfig,
    ) -> WaitableProcess {
        let commands = match command {
            ListableCommand::Pipe(_negate, commands) => {
                commands
                    .iter()
                    .map(|command| self.eval_pipeable_command(command, redir))
                    .collect() //TODO: negate?
            }
            ListableCommand::Single(command) => {
                vec![self.eval_pipeable_command(command, redir)]
            }
        };
        let last_command = commands.len() - 1;
        if let Some(output_stream) = &self.config.output_stream {
            let mut lock = output_stream.lock();
            let any_printable = commands.iter().any(|p| p.is_printable());
            if any_printable {
                //TODO: Proper error handling here?
                write!(lock, "> ").unwrap();
                for (i, (printable, command)) in
                    commands.iter().map(|p| (p.is_printable(), p)).enumerate()
                {
                    if i != 0 {
                        write!(lock, " | ").unwrap();
                    }
                    if printable {
                        command.print_to(&mut lock).unwrap();
                    } else {
                        write!(lock, "(unprintable)").unwrap();
                    }
                }
                writeln!(lock).unwrap();
            }
        }
        let mut next_input = redir.stdin;
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
        &self,
        command: &'a PipeableCommand,
        redir: RedirectConfig,
    ) -> SpawnableProcess<'a> {
        match command {
            PipeableCommand::Simple(command) => self.eval_simple_command(command, redir),
            PipeableCommand::Compound(command) => SpawnableProcess::compound(command, redir),
            PipeableCommand::FunctionDef(name, body) => {
                SpawnableProcess::fn_def(name.clone(), body.clone())
            }
        }
    }

    fn eval_simple_command<'a>(
        &self,
        command: &'a SimpleCommand,
        mut redir: RedirectConfig,
    ) -> SpawnableProcess<'a> {
        use std::process::Command;

        let mut redirects = vec![];

        let (env_remaps, printable_remaps): (Vec<_>, Vec<_>) = match command
            .redirects_or_env_vars
            .iter()
            .filter_map(|r| match r {
                RedirectOrEnvVar::EnvVar(key, word) => Some(
                    word.as_ref()
                        .map(|word| self.evaluate_tl_word(word))
                        .transpose()
                        .map(|word| {
                            word.map(|(words, printable)| {
                                (
                                    (key.clone(), words.join(" ")),
                                    PrintableEnvRemap {
                                        key: key.clone(),
                                        value: printable,
                                    },
                                )
                            })
                            .unwrap_or_else(|| {
                                (
                                    (String::new(), String::new()),
                                    PrintableEnvRemap {
                                        key: String::new(),
                                        value: PrintableComplexWord::Empty,
                                    },
                                )
                            })
                        }),
                ),
                RedirectOrEnvVar::Redirect(redirect) => {
                    redirects.push(redirect);
                    None
                }
            })
            .collect::<Result<Vec<_>, _>>()
        {
            Ok(remaps) => remaps.into_iter().unzip(),
            Err(status) => return SpawnableProcess::empty_status(status),
        };

        let (command_words, printable_words): (Vec<_>, Vec<_>) = match command
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
            Ok(words) => words.into_iter().unzip(),
            Err(status) => return SpawnableProcess::empty_status(status),
        };

        if !command_words.is_empty() {
            let redirects = match redirects
                .into_iter()
                .map(|redirect| {
                    macro_rules! map_redirect {
                        ($($t:ident),*) => {
                            match redirect {
                                $(Redirect::$t(fd, word) => EvaluatedRedirect::$t(*fd, self.evaluate_tl_word(word)?.0.join(" ")),)*
                                Redirect::Heredoc(fd, word) => EvaluatedRedirect::Heredoc(*fd, self.evaluate_tl_word(word)?.0)
                            }
                        }
                    }
                    Ok(map_redirect!(Read, Write, ReadWrite, Append, Clobber, DupRead, DupWrite))
                })
                .collect::<Result<Vec<_>, _>>()
            {
                Ok(redirects) => redirects,
                Err(status) => return SpawnableProcess::empty_status(status),
            };

            let command_words = command_words.into_iter().flatten().collect::<Vec<_>>();
            let (command_name, arguments) = {
                let mut printable_words = printable_words;
                printable_words.remove(0);
                // No panicking possible, because !command_words.is_empty()
                (command_words.first().unwrap().clone(), printable_words)
            };
            let printable_command = Printable {
                command: Some(PrintableCommand {
                    name: command_name,
                    arguments,
                }),
                env_remaps: printable_remaps,
                redirects: redirects
                    .iter()
                    .map(|redir| match redir {
                        EvaluatedRedirect::Read(fd, word) => PrintableRedirect {
                            fd: *fd,
                            direction: "<".to_owned(),
                            literal: true,
                            file: word.clone(),
                        },
                        EvaluatedRedirect::Write(fd, word) => PrintableRedirect {
                            fd: *fd,
                            direction: ">".to_owned(),
                            literal: true,
                            file: word.clone(),
                        },
                        EvaluatedRedirect::Append(fd, word) => PrintableRedirect {
                            fd: *fd,
                            direction: ">>".to_owned(),
                            literal: true,
                            file: word.clone(),
                        },
                        EvaluatedRedirect::Clobber(fd, word) => PrintableRedirect {
                            fd: *fd,
                            direction: ">|".to_owned(),
                            literal: true,
                            file: word.clone(),
                        },
                        EvaluatedRedirect::ReadWrite(fd, word) => PrintableRedirect {
                            fd: *fd,
                            direction: "<>".to_owned(),
                            literal: true,
                            file: word.clone(),
                        },
                        EvaluatedRedirect::Heredoc(fd, _) => PrintableRedirect {
                            fd: *fd,
                            direction: "<<".to_owned(),
                            literal: true,
                            file: "(heredoc)".to_owned(),
                        },
                        EvaluatedRedirect::DupRead(_fd, _word) => todo!(),
                        EvaluatedRedirect::DupWrite(_fd, _word) => todo!(),
                    })
                    .collect(),
            };

            for redirect in redirects {
                match redirect {
                    EvaluatedRedirect::Read(fd, word) => {
                        let file = match OpenOptions::new()
                            .read(true)
                            .open(self.working_directory.clone().join(word))
                        {
                            Ok(file) => file,
                            Err(err) => {
                                return SpawnableProcess::empty_error(
                                    CommandExecError::BadRedirect { err },
                                );
                            }
                        };
                        match fd {
                            None | Some(0) => redir.stdin = StdinRedirect::from(file),
                            Some(1) => redir.stdout = StdoutRedirect::from(file),
                            Some(2) => redir.stderr = StdoutRedirect::from(file),
                            Some(_fd) => todo!(), // Might have to use nix::dup2?
                        }
                    }
                    EvaluatedRedirect::Write(fd, word) => {
                        let file = match OpenOptions::new()
                            .write(true)
                            .create(true)
                            .open(self.working_directory.clone().join(word))
                        {
                            Ok(file) => file,
                            Err(err) => {
                                return SpawnableProcess::empty_error(
                                    CommandExecError::BadRedirect { err },
                                );
                            }
                        };
                        match fd {
                            Some(0) => redir.stdin = StdinRedirect::from(file),
                            None | Some(1) => redir.stdout = StdoutRedirect::from(file),
                            Some(2) => redir.stderr = StdoutRedirect::from(file),
                            Some(_fd) => todo!(), // Might have to use nix::dup2?
                        }
                    }
                    EvaluatedRedirect::ReadWrite(fd, word) => {
                        let file = match OpenOptions::new()
                            .read(true)
                            .write(true)
                            .create(true)
                            .open(self.working_directory.clone().join(word))
                        {
                            Ok(file) => file,
                            Err(err) => {
                                return SpawnableProcess::empty_error(
                                    CommandExecError::BadRedirect { err },
                                );
                            }
                        };
                        match fd {
                            None | Some(0) => redir.stdin = StdinRedirect::from(file),
                            Some(1) => redir.stdout = StdoutRedirect::from(file),
                            Some(2) => redir.stderr = StdoutRedirect::from(file),
                            Some(_fd) => todo!(), // Might have to use nix::dup2?
                        }
                    }
                    EvaluatedRedirect::Append(fd, word) => {
                        let file = match OpenOptions::new()
                            .write(true)
                            .append(true)
                            .create(true)
                            .open(self.working_directory.clone().join(word))
                        {
                            Ok(file) => file,
                            Err(err) => {
                                return SpawnableProcess::empty_error(
                                    CommandExecError::BadRedirect { err },
                                );
                            }
                        };
                        match fd {
                            Some(0) => redir.stdin = StdinRedirect::from(file),
                            None | Some(1) => redir.stdout = StdoutRedirect::from(file),
                            Some(2) => redir.stderr = StdoutRedirect::from(file),
                            Some(_fd) => todo!(), // Might have to use nix::dup2?
                        }
                    }
                    EvaluatedRedirect::Clobber(fd, word) => {
                        let file = match OpenOptions::new()
                            .write(true)
                            .create(true)
                            .open(self.working_directory.clone().join(word))
                        {
                            Ok(file) => file,
                            Err(err) => {
                                return SpawnableProcess::empty_error(
                                    CommandExecError::BadRedirect { err },
                                );
                            }
                        };
                        match fd {
                            None | Some(0) => redir.stdin = StdinRedirect::from(file),
                            Some(1) => redir.stdout = StdoutRedirect::from(file),
                            Some(2) => redir.stderr = StdoutRedirect::from(file),
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
                            None | Some(0) => redir.stdin = StdinRedirect::from(file),
                            Some(1) => redir.stdout = StdoutRedirect::from(file),
                            Some(2) => redir.stderr = StdoutRedirect::from(file),
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

            match command_words[0].as_str() {
                ":" | "true" => {
                    SpawnableProcess::builtin(BuiltinCommand::True, redir, printable_command)
                }
                "false" => {
                    SpawnableProcess::builtin(BuiltinCommand::False, redir, printable_command)
                }
                #[cfg(feature = "dev-panic")]
                "__run_panic" => {
                    SpawnableProcess::builtin(BuiltinCommand::Panic, redir, printable_command)
                }
                "cd" => {
                    //TODO: Extended cd options
                    SpawnableProcess::builtin(
                        BuiltinCommand::Cd {
                            path: self.working_directory.join(&command_words[1]),
                        },
                        redir,
                        printable_command,
                    )
                }
                "export" => {
                    let name = &command_words[1];
                    if let Some((name, value)) = name.split_once('=') {
                        SpawnableProcess::builtin(
                            BuiltinCommand::Export {
                                key: name.to_string(),
                                value: value.to_string(),
                            },
                            redir,
                            printable_command,
                        )
                    } else {
                        SpawnableProcess::builtin(
                            BuiltinCommand::Export {
                                key: name.to_string(),
                                value: self.vars[name].clone(),
                            },
                            redir,
                            printable_command,
                        )
                    }
                }
                "unset" => {
                    let name = &command_words[1];
                    SpawnableProcess::builtin(
                        BuiltinCommand::Unset {
                            key: name.to_string(),
                        },
                        redir,
                        printable_command,
                    )
                }
                "source" => SpawnableProcess::builtin(
                    BuiltinCommand::Source {
                        path: self.working_directory.join(&command_words[1]),
                    },
                    redir,
                    printable_command,
                ),
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
                    redir,
                    printable_command,
                ),
                _ => {
                    if let Some(commands) = self.functions.get(&command_words[0]).cloned() {
                        SpawnableProcess::function(
                            commands,
                            command_words,
                            redir,
                            printable_command,
                        )
                    } else {
                        let mut command = Command::new(&command_words[0]);
                        command
                            .args(&command_words[1..])
                            .envs(&self.env)
                            .envs(env_remaps)
                            .current_dir(self.working_directory.clone());
                        SpawnableProcess::process(command, redir, printable_command)
                    }
                }
            }
        } else {
            SpawnableProcess::remaps(env_remaps, printable_remaps)
        }
    }

    fn evaluate_tl_word(
        &self,
        word: &ComplexWord,
    ) -> Result<(Vec<String>, PrintableComplexWord), ProcessExit> {
        match word {
            ComplexWord::Concat(words) => {
                let (words, printable_words): (Vec<_>, Vec<_>) = words
                    .iter()
                    .map(|w| self.evaluate_word(w))
                    .collect::<Result<Vec<_>, _>>()?
                    .into_iter()
                    .unzip();
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
                        Ok((
                            matches.clone(),
                            PrintableComplexWord::GlobPattern {
                                glob_string: printable_words,
                                matches,
                            },
                        ))
                    }
                } else {
                    Ok((
                        vec![words.into_iter().flat_map(GlobPart::into_string).join("")],
                        PrintableComplexWord::Words(printable_words),
                    ))
                }
            }
            ComplexWord::Single(word) => self.evaluate_word(word).map(|(glob, printable)| {
                (
                    glob.into_string(),
                    PrintableComplexWord::Words(vec![printable]),
                )
            }),
        }
    }

    fn evaluate_word(&self, word: &Word) -> Result<(GlobPart, PrintableWord), ProcessExit> {
        match word {
            Word::SingleQuoted(literal) => Ok((
                vec![literal.clone()].into(),
                PrintableWord::SingleQuoted(literal.clone()),
            )),
            Word::DoubleQuoted(words) => {
                let (words, printables): (Vec<_>, Vec<_>) = words
                    .iter()
                    .map(|w| {
                        self.evaluate_simple_word(w)
                            .map(|(part, printable)| (part.into_string(), printable))
                    })
                    .collect::<Result<Vec<_>, _>>()?
                    .into_iter()
                    .unzip();
                Ok((
                    vec![words.into_iter().flatten().collect::<String>()].into(),
                    PrintableWord::DoubleQuoted(printables),
                ))
            }
            Word::Simple(word) => {
                let (glob_part, printable) = self.evaluate_simple_word(word)?;
                Ok((glob_part, PrintableWord::Unquoted(printable)))
            }
        }
    }

    fn evaluate_simple_word(
        &self,
        word: &SimpleWord,
    ) -> Result<(GlobPart, PrintableSimpleWord), ProcessExit> {
        match word {
            SimpleWord::Literal(s) => Ok((
                vec![s.clone()].into(),
                PrintableSimpleWord::Literal(s.clone()),
            )),
            SimpleWord::Escaped(s) => Ok((
                vec![s.clone()].into(),
                PrintableSimpleWord::Escaped(s.clone()),
            )),
            SimpleWord::Param(p) => Ok({
                let (evaluated, original) = self.evaluate_parameter(p);
                (
                    evaluated.clone().into(),
                    PrintableSimpleWord::Substitution {
                        original,
                        evaluated: evaluated.join(" "),
                    },
                )
            }),
            SimpleWord::Subst(p) => self.evaluate_param_subst(p),
            SimpleWord::Star => Ok((GlobPart::Star, PrintableSimpleWord::Literal("*".to_owned()))),
            SimpleWord::Question => Ok((
                GlobPart::Question,
                PrintableSimpleWord::Literal("?".to_owned()),
            )),
            SimpleWord::SquareOpen => Ok((
                GlobPart::SquareOpen,
                PrintableSimpleWord::Literal("[".to_owned()),
            )),
            SimpleWord::SquareClose => Ok((
                GlobPart::SquareClose,
                PrintableSimpleWord::Literal("]".to_owned()),
            )),
            SimpleWord::Tilde => Ok((
                vec!["~".to_owned()].into(),
                PrintableSimpleWord::Literal("~".to_owned()),
            )),
            SimpleWord::Colon => Ok((
                vec![":".to_owned()].into(),
                PrintableSimpleWord::Literal(":".to_owned()),
            )),
        }
    }

    fn evaluate_param_subst(
        &self,
        param: &ParameterSubstitution,
    ) -> Result<(GlobPart, PrintableSimpleWord), ProcessExit> {
        Ok(match param {
            ParameterSubstitution::Command(commands) => {
                let output = self.clone().exec_command_group(commands).wait();
                if !output.status.success() {
                    return Err(output.status);
                }
                let output = String::from_utf8(output.stdout).map_err(|e| {
                    ProcessExit::ExecError(CommandExecError::UnhandledOsString {
                        err: e.utf8_error(),
                    })
                })?;
                (
                    vec![output.clone()].into(),
                    PrintableSimpleWord::Substitution {
                        original: "()".to_owned(),
                        evaluated: output,
                    },
                )
            }
            ParameterSubstitution::Len(p) => match p {
                Parameter::At | Parameter::Star => {
                    let len = self.config.positional_args.len();
                    (
                        vec![format!("{}", len)].into(),
                        PrintableSimpleWord::Substitution {
                            original: "{#@}".to_owned(),
                            evaluated: format!("{}", len),
                        },
                    )
                }
                p => {
                    let (var, original) = self.evaluate_parameter(p);
                    let len = var
                        .into_iter()
                        .map(|s| s.len())
                        .reduce(|acc, s| acc + s + 1)
                        .unwrap_or(0);
                    (
                        vec![format!("{}", len)].into(),
                        PrintableSimpleWord::Substitution {
                            original: format!("{{#{}}}", original),
                            evaluated: format!("{}", len),
                        },
                    )
                }
            },
            ParameterSubstitution::Arith(_) => todo!(),
            ParameterSubstitution::Default(null_is_unset, parameter, default) => {
                let (parameter, original) = self.evaluate_parameter(parameter);
                let default = default
                    .as_ref()
                    .map(|word| self.evaluate_tl_word(word).map(|(v, _)| v))
                    .unwrap_or_else(|| Ok(Default::default()));
                //TODO: Show parameter expansions within `default`?
                let original = format!(
                    "{{{}{}-{}}}",
                    original,
                    if *null_is_unset { ":" } else { "" },
                    default.as_deref().unwrap_or(&[]).join("")
                );
                if parameter.is_empty()
                    || (*null_is_unset && parameter.len() == 1 && parameter[0].is_empty())
                {
                    let default = default?;
                    (
                        default.clone().into(),
                        PrintableSimpleWord::Substitution {
                            original,
                            evaluated: default.join(""),
                        },
                    )
                } else {
                    (
                        parameter.clone().into(),
                        PrintableSimpleWord::Substitution {
                            original,
                            evaluated: parameter.join(""),
                        },
                    )
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

    fn evaluate_parameter(&self, parameter: &Parameter) -> (Vec<String>, String) {
        match parameter {
            Parameter::Positional(n) => match self.function_args.last() {
                Some(args) => (
                    args.get(*n).cloned().into_iter().collect(),
                    if *n > 9 {
                        format!("{{{}}}", n)
                    } else {
                        format!("{}", n)
                    },
                ),
                None => (
                    self.config
                        .positional_args
                        .get(*n)
                        .cloned()
                        .into_iter()
                        .collect(),
                    if *n > 9 {
                        format!("{{{}}}", n)
                    } else {
                        format!("{}", n)
                    },
                ),
            },
            Parameter::Var(name) => (
                self.vars
                    .get(name)
                    .cloned()
                    .or_else(|| std::env::var(name).ok())
                    .into_iter()
                    .collect(),
                name.clone(),
            ),
            Parameter::At => (
                match self.function_args.last() {
                    Some(args) => args,
                    None => &self.config.positional_args,
                }
                .iter()
                .skip(1)
                .cloned()
                .collect(),
                "@".to_owned(),
            ),
            Parameter::Star => (
                match self.function_args.last() {
                    Some(args) => args,
                    None => &self.config.positional_args,
                }
                .iter()
                .skip(1)
                .cloned()
                .collect(),
                "*".to_owned(),
            ),
            Parameter::Pound => (
                vec![format!(
                    "{}",
                    match self.function_args.last() {
                        Some(args) => args,
                        None => &self.config.positional_args,
                    }
                    .len()
                        - 1
                )],
                "#".to_owned(),
            ),
            Parameter::Dollar => (vec![format!("{}", std::process::id())], "$".to_owned()),
            Parameter::Question => (vec![format!("{}", self.exit_code)], "?".to_owned()),
            Parameter::Bang => (vec![format!("{}", self.pid)], "!".to_owned()),

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
