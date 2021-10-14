use std::collections::HashMap;
use std::io::{Read, Write};
use std::path::{Path, PathBuf};
use std::process::{Child, ExitStatus, Output, Stdio};
use std::sync::atomic::AtomicBool;
use std::sync::mpsc::{Receiver, Sender};
use std::sync::{mpsc, Arc};

use conch_parser::ast::{Arithmetic, AtomicCommandList, AtomicShellPipeableCommand, AtomicTopLevelCommand, AtomicTopLevelWord, ComplexWord, CompoundCommand, CompoundCommandKind, GuardBodyPair, ListableCommand, Parameter, ParameterSubstitution, PatternBodyPair, PipeableCommand, Redirect, RedirectOrCmdWord, SimpleCommand, SimpleWord, Word};
use glob::{MatchOptions, Pattern, glob_with};
use itertools::Itertools;

use crate::parser::RunscriptLocation;
use crate::{run, Script};

#[derive(Clone)]
pub struct ExecConfig<'a> {
    /// The verbosity of output to the supplied output stream
    pub verbosity: Verbosity,
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

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Verbosity {
    Normal,
    Quiet,
    Silent,
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
        loc: RunscriptLocation,
    },
    NoGlobMatches {
        glob: String,
        loc: RunscriptLocation,
    },
    BadCommand {
        err: std::io::Error,
        loc: RunscriptLocation,
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

    /// The `ExitStatus` the process exited with.
    ///
    /// Panics on the `Bool` variant if on neither unix nor windows
    pub fn status(self) -> ExitStatus {
        match self {
            ProcessExit::Status(s) => s,
            ProcessExit::Bool(b) => convert_bool(b),
        }
    }
}

enum OngoingProcess {
    Process(Child),
    Builtin(FinishedProcess),
}

impl OngoingProcess {
    fn pipe_out(&mut self) -> PipeInput {
        match self {
            OngoingProcess::Process(p) => match p.stdout.take() {
                Some(stdout) => PipeInput::Pipe(stdout.into()),
                None => PipeInput::None,
            },
            OngoingProcess::Builtin(p) => PipeInput::Buffer(p.stdout.clone()),
        }
    }
}

struct WaitableProcess {
    process: OngoingProcess,
    associated_jobs: Vec<WaitableProcess>,
}

impl WaitableProcess {
    fn new(process: Child) -> WaitableProcess {
        WaitableProcess { process: OngoingProcess::Process(process), associated_jobs: vec![] }
    }

    fn empty_success() -> WaitableProcess {
        WaitableProcess { process: OngoingProcess::Builtin(FinishedProcess { status: ProcessExit::Bool(true), stdout: vec![], stderr: vec![] }), associated_jobs: vec![] }
    }

    #[cfg(unix)]
    fn hup(mut self) -> FinishedProcess {
        use std::os::unix::prelude::ExitStatusExt;

        use nix::sys::signal::{kill, Signal};
        use nix::unistd::Pid;

        for job in self.associated_jobs {
            job.hup();
        }

        match self.process {
            OngoingProcess::Process(process) => {
                kill(Pid::from_raw(process.id() as i32), Signal::SIGHUP).unwrap();
                FinishedProcess {
                    status: ProcessExit::Status(ExitStatus::from_raw(129)),
                    stdout: vec![],
                    stderr: vec![],
                }
            },
            OngoingProcess::Builtin(proc) => proc,
        }
        
    }

    #[cfg(windows)]
    fn hup(mut self) -> FinishedProcess {
        todo!()
    }
    
    fn wait(self) -> FinishedProcess {
        match self.process {
            OngoingProcess::Process(mut process) => {
                let status = process.wait().unwrap();
                let stdout = if let Some(mut stdout) = process.stdout {
                    let mut v = Vec::new();
                    stdout.read_to_end(&mut v);
                    v
                } else {
                    Vec::new()
                };
                let stderr = if let Some(mut stderr) = process.stderr {
                    let mut v = Vec::new();
                    stderr.read_to_end(&mut v);
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
            },
            OngoingProcess::Builtin(proc) => {
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

impl From<Stdio> for PipeInput
{
    fn from(stdin: Stdio) -> PipeInput {
        PipeInput::Pipe(stdin.into())
    }
}

impl From<Vec<u8>> for PipeInput {
    fn from(stdin: Vec<u8>) -> PipeInput {
        PipeInput::Buffer(stdin)
    }
}

#[cfg(unix)]
fn convert_bool(b: bool) -> ExitStatus {
    use std::os::unix::process::ExitStatusExt;

    ExitStatus::from_raw(!b as i32)
}

#[cfg(windows)]
fn convert_bool(b: bool) -> ExitStatus {
    use std::os::windows::process::ExitStatusExt;

    ExitStatus::from_raw(!b as u32)
}

#[cfg(not(any(unix, windows)))]
fn convert_bool(_: bool) -> ExitStatus {
    panic!("Expected ExitStatus")
}

impl FinishedProcess {
    pub fn new(success: bool) -> FinishedProcess {
        FinishedProcess {
            status: ProcessExit::Bool(success),
            stdout: vec![],
            stderr: vec![],
        }
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
    Ok(ShellContext { working_directory: config.working_directory.to_owned(), env: HashMap::new() }.exec_script_entries(&script.commands, config, &HashMap::new())?.wait())
}

#[derive(Debug, Clone)]
struct ShellContext {
    /// The current working directory
    working_directory: PathBuf,
    /// The current environment
    env: HashMap<String, String>,
}

impl ShellContext {
    fn exec_script_entries(
        &mut self,
        commands: &[AtomicTopLevelCommand<String>],
        config: &ExecConfig,
        env: &HashMap<String, String>,
    ) -> Result<WaitableProcess, CommandExecError> {
        use conch_parser::ast::Command;

        let mut jobs = vec![];

        if commands.len() > 1 {
            for AtomicTopLevelCommand(command) in commands {
                let (command, is_job) = match command {
                    Command::Job(list) => (list, true),
                    Command::List(list) => (list, false),
                };

                let proc = self.exec_andor_list(command, config, env)?;
                if is_job {
                    jobs.push(proc);
                } else {
                    proc.wait();
                }
            }
        }

        let mut last_proc = match &commands.last().unwrap().0 {
            Command::Job(list) => self.exec_andor_list(list, config, env)?,
            Command::List(list) => self.exec_andor_list(list, config, env)?,
        };

        last_proc.associated_jobs = jobs;

        Ok(last_proc)
    }

    fn exec_andor_list(
        &mut self,
        command: &AtomicCommandList<String, AtomicTopLevelWord<String>, AtomicTopLevelCommand<String>>,
        config: &ExecConfig,
        env: &HashMap<String, String>,
    ) -> Result<WaitableProcess, CommandExecError> {
        let mut previous_output = self.exec_listable_command(&command.first, config, env)?;
        //TODO: Chain
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
        env: &HashMap<String, String>,
    ) -> Result<WaitableProcess, CommandExecError> {
        match command {
            ListableCommand::Pipe(negate, commands) => {
                let mut proc = self.exec_pipeable_command(commands.first().unwrap(), Pipe::pipe_out(), config, env)?;
                if commands.len() > 2 {
                    for command in &commands[1..commands.len() - 2] {
                        let next_proc = self.exec_pipeable_command(command, Pipe::pipe_in_out(proc.process.pipe_out()), config, env)?;
                        proc = next_proc;
                    }
                }
                let last_proc = self.exec_pipeable_command(commands.last().unwrap(), Pipe::pipe_in(proc.process.pipe_out()), config, env)?;
                Ok(last_proc) //TODO: Negate?
            },
            ListableCommand::Single(command) => {
                self.exec_pipeable_command(&command, Pipe::no_pipe(), config, env)
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
        env: &HashMap<String, String>,
    ) -> Result<WaitableProcess, CommandExecError> {
        match command {
            PipeableCommand::Simple(command) => self.exec_simple_command(&command, pipe, config, env),
            PipeableCommand::Compound(command) => {
                match &command.kind {
                    CompoundCommandKind::Brace(commands) => {
                        self.exec_script_entries(&commands, config, env)
                    }
                    CompoundCommandKind::Subshell(commands) => {
                        let mut context = self.clone();
                        context.exec_script_entries(&commands, config, env)
                    },
                    CompoundCommandKind::While(GuardBodyPair { guard, body }) => {
                        while self.exec_script_entries(guard, config, env)?.wait().status.success() {
                            self.exec_script_entries(body, config, env)?.wait();
                        }

                        Ok(WaitableProcess::empty_success())
                    },
                    CompoundCommandKind::Until(GuardBodyPair { guard, body }) => {
                        while !self.exec_script_entries(guard, config, env)?.wait().status.success() {
                            self.exec_script_entries(body, config, env)?.wait();
                        }

                        Ok(WaitableProcess::empty_success())
                    },
                    CompoundCommandKind::If { conditionals, else_branch } => {
                        let mut last_proc = WaitableProcess::empty_success();
                        for GuardBodyPair { guard, body } in conditionals {
                            if self.exec_script_entries(guard, config, env)?.wait().status.success() {
                                last_proc = self.exec_script_entries(body, config, env)?;
                            }
                        }

                        if let Some(else_branch) = else_branch {
                            last_proc = self.exec_script_entries(else_branch, config, env)?;
                        }

                        Ok(last_proc)
                    },
                    CompoundCommandKind::For { var, words, body } => {
                        let mut last_proc = WaitableProcess::empty_success();
                        for word in words.as_ref().map(|words| -> Result<_, _> { words.iter().map(|word| self.evaluate_tl_word(word, config, env)).flatten_ok().collect::<Result<Vec<_>, _>>() }).transpose()?.unwrap_or(config.positional_args.clone()) {
                            self.env.insert(var.clone(), word.clone());
                            last_proc = self.exec_script_entries(body, config, env)?;
                        }

                        Ok(last_proc)
                    },
                    CompoundCommandKind::Case { word, arms } => {
                        let mut last_proc = WaitableProcess::empty_success();
                        let word = self.evaluate_tl_word(word, config, env)?;
                        for PatternBodyPair { patterns, body } in arms {
                            let mut pattern_matches = false;
                            for pattern in patterns {
                                let pattern = self.evaluate_tl_word(pattern, config, env)?;
                                if pattern == word {
                                    pattern_matches = true;
                                }
                            }

                            if pattern_matches {
                                last_proc = self.exec_script_entries(body, config, env)?;
                                break;
                            }
                        }

                        Ok(last_proc)
                    },
                }
            }
            PipeableCommand::FunctionDef(name, body) => {
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
        env: &HashMap<String, String>,
    ) -> Result<WaitableProcess, CommandExecError> {
        use std::process::Command;

        // TODO: Env variables
        // TODO: Redirects

        let command_words = command
            .redirects_or_cmd_words
            .iter()
            .filter_map(|r| {
                if let RedirectOrCmdWord::CmdWord(w) = r {
                    Some(self.evaluate_tl_word(w, config, env))
                } else {
                    None
                }
            })
            .flatten_ok()
            .collect::<Result<Vec<_>, _>>()?;

        // TODO: Print pre-evaluated command words
        eprintln!(
            "> {}",
            command_words.join(" ")
        );

        // TODO: "Builtin" commands
        // TODO: Interruption

        let mut stdin_buffer = None;
        let mut child = Command::new(&command_words[0])
            .args(&command_words[1..])
            .stdin(match pipe.stdin {
                PipeInput::None => Stdio::null(),
                PipeInput::Inherit => Stdio::inherit(),
                PipeInput::Pipe(stdio) => stdio,
                PipeInput::Buffer(v) => {
                    stdin_buffer = Some(v);
                    Stdio::piped()
                },
            })
            .stdout(if config.capture_stdout || pipe.stdout {
                Stdio::piped()
            } else {
                Stdio::inherit()
            })
            .stderr(if config.capture_stdout {
                Stdio::piped()
            } else {
                Stdio::inherit()
            })
            .spawn()
            .unwrap();
        
        if let Some(stdin) = stdin_buffer {
            //TODO: Can cause deadlock if the child writes too much to stdout before reading stdin.
            //? Could run this on separate thread to mitigate this, if necessary.
            child.stdin.take().unwrap().write_all(&stdin);
        }

        Ok(child.into())
    }

    fn evaluate_tl_word(
        &mut self,
        AtomicTopLevelWord(word): &AtomicTopLevelWord<String>,
        config: &ExecConfig,
        env: &HashMap<String, String>,
    ) -> Result<Vec<String>, CommandExecError> {
        match word {
            ComplexWord::Concat(words) => {
                let words = words
                    .iter()
                    .map(|w| self.evaluate_word(w, config, env))
                    .collect::<Result<Vec<_>, _>>()?;
                let is_glob = !words.iter().all(|w| matches!(w, GlobPart::Words(_)));
                if is_glob {
                    let working_dir_path = {
                        let mut path = Pattern::escape(&config.working_directory.to_string_lossy().into_owned());
                        path.push('/');
                        path
                    };
                    let stringified_glob = std::iter::once(working_dir_path.clone()).chain(words.iter().map(|w| match w {
                        GlobPart::Words(words) => words.iter().map(|s| Pattern::escape(s)).join(" "),
                        GlobPart::Star => "*".to_owned(),
                        GlobPart::Question => "?".to_owned(),
                        GlobPart::SquareOpen => "[".to_owned(),
                        GlobPart::SquareClose => "]".to_owned(),
                    })).join("");
                    Ok(glob_with(&stringified_glob, MatchOptions::new()).unwrap().map(|path| path.unwrap().to_string_lossy().into_owned().strip_prefix(&working_dir_path).unwrap().to_owned()).collect::<Vec<_>>())
                } else {
                    Ok(words.into_iter().flat_map(GlobPart::into_string).collect())
                }
            },
            ComplexWord::Single(word) => self.evaluate_word(word, config, env).map(GlobPart::into_string),
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
        env: &HashMap<String, String>,
    ) -> Result<GlobPart, CommandExecError> {
        match word {
            Word::SingleQuoted(literal) => Ok(vec![literal.clone()].into()),
            Word::DoubleQuoted(words) => Ok(vec![words
                .iter()
                .map(|w| self.evaluate_simple_word(w, config, env).map(GlobPart::into_string))
                .flatten_ok()
                .collect::<Result<String, _>>()?].into()),
            Word::Simple(word) => self.evaluate_simple_word(word, config, env),
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
        env: &HashMap<String, String>,
    ) -> Result<GlobPart, CommandExecError> {
        match word {
            SimpleWord::Literal(s) => Ok(vec![s.clone()].into()),
            SimpleWord::Escaped(s) => Ok(vec![s.clone()].into()),
            SimpleWord::Param(p) => Ok(self.evaluate_parameter(p, config, env).into()),
            SimpleWord::Subst(p) => Ok(self.evaluate_param_subst(p, config, env)?.into()),
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
        env: &HashMap<String, String>,
    ) -> Result<Vec<String>, CommandExecError> {
        match param {
            ParameterSubstitution::Command(commands) => self.exec_script_entries(commands, config, env).map(|output| vec![String::from_utf8(output.wait().stdout).unwrap()]),
            ParameterSubstitution::Len(p) => Ok(vec![format!("{}", match p {
                Parameter::At | Parameter::Star => config.positional_args.len(),
                p => self.evaluate_parameter(p, config, env).into_iter().map(|s| s.len()).reduce(|acc, s| acc + s + 1).unwrap_or(0),
            })]),
            ParameterSubstitution::Arith(_) => todo!(),
            ParameterSubstitution::Default(_, _, _) => todo!(),
            ParameterSubstitution::Assign(_, _, _) => todo!(),
            ParameterSubstitution::Error(_, _, _) => todo!(),
            ParameterSubstitution::Alternative(_, _, _) => todo!(),
            ParameterSubstitution::RemoveSmallestSuffix(_, _) => todo!(),
            ParameterSubstitution::RemoveLargestSuffix(_, _) => todo!(),
            ParameterSubstitution::RemoveSmallestPrefix(_, _) => todo!(),
            ParameterSubstitution::RemoveLargestPrefix(_, _) => todo!(),
        }
    }

    fn evaluate_parameter(
        &mut self,
        parameter: &Parameter<String>,
        config: &ExecConfig,
        env: &HashMap<String, String>,
    ) -> Vec<String> {
        match parameter {
            Parameter::Positional(n) => config
                .positional_args
                .get(*n as usize)
                .cloned()
                .into_iter()
                .collect(),
            Parameter::Var(name) => env
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

/*

fn exec_script_entries(entries: &[ScriptEntry], config: &ExecConfig, env_remap: &HashMap<String, String>) -> Result<ProcessOutput, (CommandExecError, ScriptEntry)> {
    let mut env = config.env_remap.clone();
    let mut stdout_acc = Vec::new();
    let mut stderr_acc = Vec::new();
    for entry in entries {
        match entry {
            ScriptEntry::Command(command) => {
                let output = exec_tl_cmd(&command, config, &env, config.capture_stdout)
                    .map_err(|e| e.either(|e| (e, entry.clone()), |e| e))?;
                if config.capture_stdout {
                    stdout_acc.extend(output.stdout.into_iter());
                    stderr_acc.extend(output.stderr.into_iter());
                }
                if !output.status.success() {
                    if config.verbosity < Verbosity::Silent {
                        match output.status.code() {
                            Some(i) => eprintln!("=> exit {}", i),
                            None => eprintln!("=> {}", signal(&output.status.status())),
                        }
                    }
                    return Ok(ProcessOutput {
                        status: ProcessExit::Bool(false),
                        stdout: stdout_acc,
                        stderr: stderr_acc,
                    });
                }
            },
            ScriptEntry::Env { var, val, loc } => {
                if config.verbosity < Verbosity::Silent {
                    eprintln!("> {}", entry);
                }
                env.insert(
                    var.clone(),
                    evaluate_arg(val, loc.clone(), config, &env, false).map_err(|e| (e, entry.clone()))?.join(" ")
                );
            }
        }
    }
    Ok(ProcessOutput {
        status: ProcessExit::Bool(true),
        stdout: stdout_acc,
        stderr: stderr_acc,
    })
}

fn exec_tl_cmd(command: &script::TopLevelCommand, config: &ExecConfig, env_remap: &HashMap<String, String>, piped: bool) -> Result<ProcessOutput, Either<CommandExecError, (CommandExecError, ScriptEntry)>> {
    match command {
        script::TopLevelCommand::Command(c) => exec_cmd(c, config, env_remap, piped),
        script::TopLevelCommand::BlockCommand(entries) => exec_script_entries(&entries, config, env_remap).map_err(|e| Right(e)),
    }
}

fn exec_cmd(command: &script::Command, config: &ExecConfig, env_remap: &HashMap<String, String>, piped: bool) -> Result<ProcessOutput, Either<CommandExecError, (CommandExecError, ScriptEntry)>> {
    let target = &evaluate_arg(&command.target, command.loc.clone(), config, env_remap, false).map_err(|e| Left(e))?[0];
    if target == "run" {
        if config.verbosity < Verbosity::Silent {
            eprintln!("> {}", command);
        }
        let args = command.args.iter()
            .map(|x| evaluate_arg(x, command.loc.clone(), config, env_remap, true))
            .collect::<Result<Vec<Vec<String>>, CommandExecError>>().map_err(|e| Left(e))?
            .into_iter()
            .flatten()
            .collect::<Vec<_>>();
        //TODO: Pass env
        return run(
            &args.iter().map(|s| &**s).collect::<Vec<_>>(),
            config.working_directory,
            config.verbosity,
            piped,
            env_remap,
        ).map_err(|err| Left(CommandExecError::BadCommand { err, loc: command.loc.clone() }));
    }

    let mut stdin: Option<Vec<u8>> = None;

    let chained_output = match &*command.chained {
        ChainedCommand::Pipe(c) => {
            let h = exec_cmd(&c, config, env_remap, true)?;
            stdin = Some(h.stdout);
            None
        },
        ChainedCommand::And(c) => {
            let h = exec_tl_cmd(&c, config, env_remap, piped)?;
            if !h.status.success() {
                return Ok(h);
            }
            Some(h.stdout)
        },
        ChainedCommand::Or(c) => {
            let h = exec_tl_cmd(&c, config, env_remap, piped)?;
            if h.status.success() {
                return Ok(h);
            }
            Some(h.stdout)
        },
        ChainedCommand::None => None
    };

    let mut args = Vec::with_capacity(command.args.len());
    let mut args_did_evaluate = false;
    for arg in &command.args {
        match arg {
            Argument::Unquoted(ArgPart::Str(_)) | Argument::Single(_) => {}
            Argument::Double(v) if v.iter().all(|p| matches!(p, ArgPart::Str(_))) => {}
            _ => args_did_evaluate = true,
        }
        match evaluate_arg(arg, command.loc.clone(), config, env_remap, true) {
            Ok(v) => args.extend(v),
            Err(e) => {
                eprintln!("> {}", command);
                return Err(Left(e));
            }
        }
    }

    if config.verbosity < Verbosity::Silent {
        if let Some(mut lock) = config.output_stream.as_deref().map(termcolor::StandardStream::lock) {
            write!(lock, "> {}", command).expect("Failed to write");
            if args_did_evaluate {
                use termcolor::WriteColor;

                lock.set_color(ColorSpec::new().set_italic(true)).expect("Failed to set italic");
                write!(
                    lock,
                    " = {}{}",
                    target,
                    args.iter()
                        .map(|arg| if arg.chars().all(|c| c.is_ascii_alphanumeric() || c == '_' || c == '-') {
                            arg.clone()
                        } else {
                            format!("'{}'", arg)
                        })
                        .fold("".to_owned(), |mut acc, s| { acc.push(' '); acc.push_str(&s); acc })
                ).expect("Failed to write");
                lock.reset().expect("Failed to reset colour");
            }
            writeln!(lock).expect("Failed to write");
        }
    }

    let mut child = match Command::new(target)
        .args(args)
        .current_dir(config.working_directory)
        .envs(env_remap)
        .stdin(match stdin { Some(_) => Stdio::piped(), None => if config.verbosity >= Verbosity::Quiet { Stdio::null() } else { Stdio::inherit() } })
        .stdout(if piped { Stdio::piped() } else if config.verbosity >= Verbosity::Quiet { Stdio::null() } else { Stdio::inherit() })
        .stderr(if piped { Stdio::piped() } else if config.verbosity >= Verbosity::Quiet { Stdio::null() } else { Stdio::inherit() })
        .spawn() {
        Ok(c) => c,
        Err(e) => {
            return Err(Left(CommandExecError::BadCommand{
                err: e,
                loc: command.loc.clone(),
            }))
        }
    };

    if let Some(v) = stdin {
        let buffer = &v;
        child.stdin.as_mut().unwrap().write_all(buffer).expect("Failed to write stdin");
    }

    let mut output = ProcessOutput::from(child.wait_with_output().expect("Command was never started"));
    if let Some(mut o) = chained_output {
        o.append(&mut output.stdout);
        output.stdout = o;
    }

    Ok(output)
}

fn evaluate_arg(arg: &Argument, command_loc: RunscriptLocation, config: &ExecConfig, env_override: &HashMap<String, String>, match_globs: bool) -> Result<Vec<String>, CommandExecError> {
    match arg {
        Argument::Unquoted(p) => match p {
            ArgPart::Str(s) => {
                if match_globs && s.chars().any(|c| c == '*' || c == '(' || c == '|' || c == '<' || c == '[' || c == '?') {
                    match Glob::new(&s) {
                        Ok(walker) => {
                            let cwd = config.working_directory;
                            let strings = walker.walk(cwd).into_iter()
                                .map(|k| k.to_string_lossy().into_owned())
                                .collect::<Vec<String>>();
                            if strings.is_empty() {
                                Err(CommandExecError::NoGlobMatches { glob: s.clone(), loc: command_loc })
                            } else {
                                Ok(strings)
                            }
                        },
                        Err(err) => Err(CommandExecError::InvalidGlob { glob: s.clone(), err, loc: command_loc })
                    }
                } else {
                    Ok(vec![s.clone()])
                }
            },
            _ => evaluate_part(p, env_override, config),
        },
        Argument::Single(s) => Ok(vec![s.clone()]),
        Argument::Double(p) => {
            let args = p.iter().map(|x| evaluate_part(x, env_override, config)).collect::<Result<Vec<Vec<String>>, CommandExecError>>()?.iter().fold(vec![], |mut acc, x| { acc.append(&mut x.clone()); acc });
            Ok(vec![args.iter().fold(String::new(), |acc, s| acc + &s)])
        }
    }
}

fn evaluate_part(part: &ArgPart, env_override: &HashMap<String, String>, config: &ExecConfig) -> Result<Vec<String>, CommandExecError> {
    use std::env::VarError::*;
    match part {
        ArgPart::Str(s) => Ok(vec![s.clone()]),
        ArgPart::Arg(n) => Ok(vec![config.positional_args.get(*n - 1).cloned().unwrap_or_else(|| "".to_owned())]),
        ArgPart::Var(v) => Ok(vec![env_override.get(v).cloned().unwrap_or_else(|| std::env::var(v).unwrap_or_else(|err| match err { NotPresent => "".to_owned(), NotUnicode(s) => s.to_string_lossy().into_owned() }))]),
        ArgPart::AllArgs => Ok(config.positional_args.clone()),
        ArgPart::Cmd(c) => {
            Ok(vec![String::from_utf8_lossy(
                &match Command::new(&evaluate_arg(&c.target, c.loc.clone(), config, env_override, false)?[0])
                    .args(c.args.iter().map(|x| evaluate_arg(x, c.loc.clone(), config, env_override, true)).collect::<Result<Vec<Vec<String>>, CommandExecError>>()?.iter().fold(vec![], |mut acc, x| { acc.append(&mut x.clone()); acc }))
                    .current_dir(config.working_directory)
                    .output() {
                        Ok(o) => o.stdout,
                        Err(e) => return Err(CommandExecError::BadCommand {
                            err: e,
                            loc: c.loc.clone(),
                        })
                    }
                ).into_owned()])
        }
    }
}

*/

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

    // SAFETY: Function is guaranteed by POSIX to exist.
    let sigstr = unsafe { CStr::from_ptr(strsignal(signal as c_int)) };
    format!(
        "signal {} ({})",
        signal,
        sigstr.to_string_lossy().to_owned()
    )
}

#[cfg(not(unix))]
fn signal(_: &ExitStatus) -> String {
    panic!("Non-unix program terminated with signal");
}
