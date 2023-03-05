use std::collections::HashMap;
use std::fs::OpenOptions;
use std::io::Write;
use std::path::PathBuf;

use std::sync::Arc;

use crate::config::Theme;
use crate::process::{
    BuiltinCommand, CommandExecError, CompoundCommand, ProcessExit, RedirectConfig, SpawnContext,
    SpawnableProcess, StdinRedirect, StdoutRedirect, WaitableProcess,
};

use crate::ptr::Unique;
use itertools::Itertools;
use runscript_parse::{
    AndOr, CommandChain, CommandGroup, GuardBodyPair, Parameter, ParameterSubstitution,
    PatternBodyPair, Pipeline, Redirect, SimpleCommand, SimpleWord, Word,
};

use crate::out::{
    self, OutputState, Printable, PrintableCommand, PrintableComplexWord, PrintableEnvRemap,
    PrintableRedirect, PrintableSimpleWord, PrintableWord,
};
use glob::{glob_with, MatchOptions, Pattern};

#[derive(Clone)]
pub struct ExecConfig {
    /// The output stream to output to, or `None` to produce no output
    pub output_state: Option<Arc<OutputState>>,
    /// The colour choice for the output stream
    pub colour_choice: termcolor::ColorChoice,
    /// The working directory to execute the script's commands in
    pub working_directory: PathBuf,
    /// The path to the script file being executed
    pub script_path: Option<PathBuf>,
    /// The name of the target being executed
    pub target_name: Option<String>,
    /// Positional arguments to pass to the script.
    ///
    ///The first argument replaces `$0`, the second replaces `$1`, etc.
    pub positional_args: Vec<String>,
}

impl std::fmt::Debug for ExecConfig {
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
    pub theme: Option<Theme>,
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
pub struct ShellContext {
    //TODO: These should be encapsulated?
    /// The current working directory
    pub working_directory: PathBuf,
    /// The current shell variables
    pub vars: HashMap<String, String>,
    /// Exported varables
    pub env: HashMap<String, String>,
    /// Function definitions
    pub functions: HashMap<String, CommandGroup>,
    /// Stack of function arguments
    function_args: Vec<Vec<String>>,
    /// PID of most recent job
    pid: i32,
    /// Exit code of most recent top-level command
    exit_code: i32, //TODO: This will always be 0. Perhaps an option to have the script keep going even after errors?
    /// Immutable config
    config: ExecConfig,
}

impl ShellContext {
    pub fn new(config: ExecConfig) -> Self {
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

impl ShellContext {
    pub fn exec_commands(&mut self, script: &[CommandChain]) -> WaitableProcess {
        self.exec_command_group_with_default_redirects(script, RedirectConfig::default_fg())
    }

    pub fn exec_command_group_with_default_redirects(
        &mut self,
        script: &[CommandChain],
        redir: RedirectConfig,
    ) -> WaitableProcess {
        let mut jobs = vec![];

        for (i, command) in script.iter().enumerate() {
            let mut proc = self.exec_pipeline(&command.first, redir);
            for chain in &command.rest {
                match chain {
                    AndOr::And(pipeline) => {
                        let finished = proc.wait();
                        if finished.status.success() {
                            proc = self.exec_pipeline(pipeline, redir);
                        } else {
                            proc = WaitableProcess::finished(finished);
                            break;
                        }
                    }
                    AndOr::Or(pipeline) => {
                        let finished = proc.wait();
                        if !finished.status.success() {
                            proc = self.exec_pipeline(pipeline, redir);
                        } else {
                            proc = WaitableProcess::finished(finished);
                            break;
                        }
                    }
                }
            }

            if i == script.len() - 1 {
                if command.job {
                    jobs.push(proc);
                    proc = WaitableProcess::empty_success();
                }
                proc.set_jobs(jobs);
                return proc;
            } else {
                if command.job {
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

        debug_assert!(script.is_empty());
        WaitableProcess::empty_success()
    }

    pub fn exec_as_function(
        &mut self,
        command: CommandGroup,
        args: Vec<String>,
        redir: RedirectConfig,
    ) -> WaitableProcess {
        todo!()
        // self.function_args.push(args);
        // let process = self.exec_compound_command(&commands, redir);
        // self.function_args.pop();
        // process
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
        match command {
            CompoundCommand::Brace(commands) => {
                this.exec_command_group_with_default_redirects(&commands, redir)
            }
            CompoundCommand::Subshell(commands) => {
                this.exec_command_group_with_default_redirects(&commands, redir)
            }
            CompoundCommand::While(GuardBodyPair { guard, body }) => {
                while this.exec_commands(&guard).wait().status.success() {
                    this.exec_commands(&body).wait();
                }

                WaitableProcess::empty_success()
            }
            CompoundCommand::Until(GuardBodyPair { guard, body }) => {
                while !this.exec_commands(&guard).wait().status.success() {
                    this.exec_commands(&body).wait();
                }

                WaitableProcess::empty_success()
            }
            CompoundCommand::If {
                conditionals,
                else_branch,
            } => {
                let mut branch = None;

                for GuardBodyPair { guard, body } in conditionals {
                    if this.exec_commands(&guard).wait().status.success() {
                        branch = Some(body.as_slice());
                        break;
                    }
                }

                this.exec_commands(branch.unwrap_or_else(|| else_branch.as_deref().unwrap_or(&[])))
            }
            CompoundCommand::For { var, words, body } => {
                let mut last_proc = WaitableProcess::empty_success();
                for word in match words
                    .as_ref()
                    .map(|words| -> Result<_, _> {
                        words
                            .iter()
                            .map(|word| this.evaluate_tl_word(word, redir).map(|(v, _)| v))
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
                    last_proc = this.exec_commands(&body);
                }

                last_proc
            }
            CompoundCommand::Case { word, arms } => {
                let mut last_proc = WaitableProcess::empty_success();
                let word = match this.evaluate_tl_word(&word, redir) {
                    Ok(word) => word.0,
                    Err(status) => {
                        return WaitableProcess::empty_status(status);
                    }
                };
                for PatternBodyPair { patterns, body } in arms {
                    let mut pattern_matches = false;
                    for pattern in patterns {
                        let pattern = match this.evaluate_tl_word(pattern, redir) {
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
                        last_proc = this.exec_commands(body);
                        break;
                    }
                }

                last_proc
            }
        }
    }

    fn exec_pipeline(&mut self, command: &Pipeline, redir: RedirectConfig) -> WaitableProcess {
        let ready_commands = command
            .command_groups
            .iter()
            .map(|command_group| self.eval_command_group(command_group, redir))
            .collect::<Vec<_>>();
        let last_command = ready_commands.len() - 1;
        if let Some(output_state) = &self.config.output_state {
            let mut lock = output_state.lock();
            let any_printable = ready_commands.iter().any(|p| p.is_printable());
            if any_printable {
                //TODO: Proper error handling here?
                out::command_start(&mut lock).unwrap();
                for (i, (printable, command)) in ready_commands
                    .iter()
                    .map(|p| (p.is_printable(), p))
                    .enumerate()
                {
                    if i != 0 {
                        out::command_pipe(&mut lock).unwrap();
                    }
                    if printable {
                        command.print_to(&mut lock).unwrap();
                    } else {
                        out::unprintable_command(&mut lock).unwrap();
                    }
                }
                out::command_end(&mut lock).unwrap();
            }
        }
        let mut next_input = redir.stdin;
        for (i, command) in ready_commands.into_iter().enumerate() {
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

    fn eval_command_group<'a>(
        &self,
        command: &'a CommandGroup,
        redir: RedirectConfig,
    ) -> SpawnableProcess {
        match command {
            CommandGroup::Simple(command) => self.eval_simple_command(command, redir),
            CommandGroup::Brace {
                commands,
                redirects,
            } => SpawnableProcess::compound(
                CompoundCommand::Brace(commands.clone()),
                RedirectConfig::default_bg(),
            ),
            CommandGroup::Subshell {
                commands,
                redirects,
            } => SpawnableProcess::compound(
                CompoundCommand::Subshell(commands.clone()),
                RedirectConfig::default_bg(),
            ),
            CommandGroup::While { body, io } => SpawnableProcess::compound(
                CompoundCommand::While(body.clone()),
                RedirectConfig::default_bg(),
            ),
            CommandGroup::Until { body, io } => SpawnableProcess::compound(
                CompoundCommand::Until(body.clone()),
                RedirectConfig::default_bg(),
            ),
            CommandGroup::If {
                conditionals,
                else_branch,
                io,
            } => SpawnableProcess::compound(
                CompoundCommand::If {
                    conditionals: conditionals.clone(),
                    else_branch: else_branch.clone(),
                },
                RedirectConfig::default_bg(),
            ),
            CommandGroup::For {
                var,
                words,
                body,
                io,
            } => SpawnableProcess::compound(
                CompoundCommand::For {
                    var: var.clone(),
                    words: words.clone(),
                    body: body.clone(),
                },
                RedirectConfig::default_bg(),
            ),
            CommandGroup::Case { word, arms, io } => SpawnableProcess::compound(
                CompoundCommand::Case {
                    word: word.clone(),
                    arms: arms.clone(),
                },
                RedirectConfig::default_bg(),
            ),
            CommandGroup::FunctionDef(name, body) => {
                todo!();
                // SpawnableProcess::fn_def(name.clone(), body.clone())
            }
        }
    }

    fn eval_simple_command(
        &self,
        command: &SimpleCommand,
        mut redir: RedirectConfig,
    ) -> SpawnableProcess {
        use std::process::Command;

        let (words, printable_words): (Vec<_>, Vec<_>) = match command
            .words
            .iter()
            .map(|word| self.evaluate_tl_word(&word, redir))
            .collect::<Result<Vec<_>, _>>()
        {
            Ok(words) => words.into_iter().unzip(),
            Err(status) => return SpawnableProcess::empty_status(status),
        };
        let redirects = match command.redirects.iter().map(|redirect| {
                macro_rules! map_redirect {
                    ($($t:ident),*) => {
                        match redirect {
                            $(Redirect::$t(fd, word) => EvaluatedRedirect::$t(*fd, self.evaluate_tl_word(&word, redir)?.0.join(" ")),)*
                            Redirect::Heredoc(fd, word) => EvaluatedRedirect::Heredoc(*fd, self.evaluate_tl_word(&word, redir)?.0)
                        }
                    }
                }
                Ok(map_redirect!(Read, Write, ReadWrite, Append, Clobber, DupRead, DupWrite))
            }).collect::<Result<Vec<_>, _>>() {
            Ok(redirects) => redirects,
            Err(status) => return SpawnableProcess::empty_status(status),
        };
        let (assignments, printable_assignments) = match command
            .assignments
            .iter()
            .map(|(key, word)| {
                word.as_ref()
                    .map(|word| self.evaluate_tl_word(word, redir))
                    .transpose()
                    .map(|word| {
                        word.map(|(words, printable)| {
                            (
                                (String::from_utf8(key.clone()).unwrap(), words.join(" ")),
                                PrintableEnvRemap {
                                    key: String::from_utf8_lossy(&key).into_owned(),
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
                    })
            })
            .collect::<Result<Vec<_>, _>>()
        {
            Ok(assignments) => assignments.into_iter().unzip(),
            Err(status) => return SpawnableProcess::empty_status(status),
        };

        if !words.is_empty() {
            let command_words = words.into_iter().flatten().collect::<Vec<_>>();
            let (command_name, arguments) = {
                let mut printable_words = printable_words;
                printable_words.remove(0);
                // No panicking possible, because !words.is_empty()
                (command_words.first().unwrap().clone(), printable_words)
            };
            let printable_command = Printable {
                command: Some(PrintableCommand {
                    name: command_name,
                    arguments,
                }),
                env_remaps: printable_assignments,
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
                                value: self.vars[name.as_str()].clone(),
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
                        current_target: self.config.target_name.clone(),
                        args: {
                            let mut args = command_words;
                            args.remove(0);
                            args
                        },
                        colour_choice: self.config.colour_choice,
                        theme: self
                            .config
                            .output_state
                            .as_ref()
                            .map(|state| state.theme.clone()),
                    },
                    redir,
                    printable_command,
                ),
                _ => {
                    if let Some(commands) = self.functions.get(&command_words[0]).cloned() {
                        // SpawnableProcess::function(
                        //     commands,
                        //     command_words,
                        //     redir,
                        //     printable_command,
                        // )
                        todo!()
                    } else {
                        let mut command = Command::new(&command_words[0]);
                        command
                            .args(&command_words[1..])
                            .envs(&self.env)
                            .envs(assignments)
                            .current_dir(self.working_directory.clone());
                        SpawnableProcess::process(command, redir, printable_command)
                    }
                }
            }
        } else {
            SpawnableProcess::remaps(assignments, printable_assignments)
        }
    }

    fn evaluate_tl_word(
        &self,
        words: &[Word],
        redir: RedirectConfig,
    ) -> Result<(Vec<String>, PrintableComplexWord), ProcessExit> {
        let (words, printable_words): (Vec<_>, Vec<_>) = words
            .iter()
            .map(|w| self.evaluate_word(w, redir))
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
                    GlobPart::Words(words) => words.iter().map(|s| Pattern::escape(s)).join(" "),
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

    fn evaluate_word(
        &self,
        word: &Word,
        redir: RedirectConfig,
    ) -> Result<(GlobPart, PrintableWord), ProcessExit> {
        match word {
            Word::SingleQuoted(literal) => Ok((
                vec![String::from_utf8(literal.clone()).unwrap()].into(),
                PrintableWord::SingleQuoted(String::from_utf8(literal.clone()).unwrap()),
            )),
            Word::DoubleQuoted(words) => {
                let (words, printables): (Vec<_>, Vec<_>) = words
                    .iter()
                    .map(|w| {
                        self.evaluate_simple_word(w, redir)
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
                let (glob_part, printable) = self.evaluate_simple_word(word, redir)?;
                Ok((glob_part, PrintableWord::Unquoted(printable)))
            }
        }
    }

    fn evaluate_simple_word(
        &self,
        word: &SimpleWord,
        redir: RedirectConfig,
    ) -> Result<(GlobPart, PrintableSimpleWord), ProcessExit> {
        match word {
            SimpleWord::Literal(s) => Ok((
                vec![String::from_utf8(s.clone()).unwrap()].into(),
                PrintableSimpleWord::Literal(String::from_utf8(s.clone()).unwrap()),
            )),
            SimpleWord::Escaped(s) => Ok((
                vec![String::from_utf8(vec![*s]).unwrap()].into(),
                PrintableSimpleWord::Escaped(String::from_utf8(vec![*s]).unwrap()),
            )),
            SimpleWord::Subst(p) => self.evaluate_param_subst(p, redir),
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
            SimpleWord::BraceOpen => Ok((
                vec!["{".to_owned()].into(),
                PrintableSimpleWord::Literal("{".to_owned()),
            )),
            SimpleWord::BraceClose => Ok((
                vec!["}".to_owned()].into(),
                PrintableSimpleWord::Literal("}".to_owned()),
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
        redir: RedirectConfig,
    ) -> Result<(GlobPart, PrintableSimpleWord), ProcessExit> {
        Ok(match param {
            ParameterSubstitution::Parameter(parameter) => {
                let (evaluated, original) = self.evaluate_parameter(parameter);
                (
                    evaluated.clone().into(),
                    PrintableSimpleWord::Substitution {
                        original,
                        evaluated: evaluated.join(" "),
                    },
                )
            }
            ParameterSubstitution::Command(commands) => {
                let mut context = self.clone();
                context.config.output_state = None;
                let output = context
                    .exec_command_group_with_default_redirects(
                        commands,
                        RedirectConfig {
                            stdout: StdoutRedirect::Capture,
                            ..redir
                        },
                    )
                    .wait();
                if !output.status.success() {
                    return Err(output.status);
                }
                let mut output =
                    String::from_utf8(output.captured_stdout.unwrap()).map_err(|e| {
                        ProcessExit::ExecError(CommandExecError::UnhandledOsString {
                            err: e.utf8_error(),
                        })
                    })?;
                output.truncate(output.trim_end_matches('\n').len());
                (
                    vec![output.clone()].into(),
                    PrintableSimpleWord::Substitution {
                        original: "(...)".to_owned(),
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
                    .map(|word| self.evaluate_tl_word(word, redir).map(|(v, _)| v))
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
                    .get(std::str::from_utf8(name).unwrap())
                    .cloned()
                    .or_else(|| std::env::var(std::str::from_utf8(name).unwrap()).ok())
                    .into_iter()
                    .collect(),
                String::from_utf8(name.clone()).unwrap(),
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
            Parameter::Hash => (
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
