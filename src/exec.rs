use std::collections::HashMap;
use std::io::Read;
use std::path::{Path, PathBuf};
use std::process::{Child, ExitStatus, Output, Stdio};
use std::sync::atomic::AtomicBool;
use std::sync::mpsc::{Receiver, Sender};
use std::sync::{mpsc, Arc};

use conch_parser::ast::{
    Arithmetic, AtomicCommandList, AtomicShellPipeableCommand, AtomicTopLevelCommand,
    AtomicTopLevelWord, ComplexWord, ListableCommand,
    Parameter, ParameterSubstitution, PipeableCommand, Redirect, RedirectOrCmdWord, SimpleCommand,
    SimpleWord, Word,
};
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

struct OngoingProcess {
    pub process: Child,
}

impl OngoingProcess {
    fn new(process: Child) -> OngoingProcess {
        OngoingProcess { process }
    }
    
    fn wait(mut self) -> FinishedProcess {
        let status = self.process.wait().unwrap();
        let stdout = if let Some(mut stdout) = self.process.stdout {
            let mut v = Vec::new();
            stdout.read_to_end(&mut v);
            v
        } else {
            Vec::new()
        };
        let stderr = if let Some(mut stderr) = self.process.stderr {
            let mut v = Vec::new();
            stderr.read_to_end(&mut v);
            v
        } else {
            Vec::new()
        };
        FinishedProcess {
            status: ProcessExit::Status(status),
            stdout,
            stderr,
        }
    }
}

impl From<Child> for OngoingProcess {
    fn from(process: Child) -> OngoingProcess {
        OngoingProcess::new(process)
    }
}

struct Pipe {
    stdin: Option<Stdio>,
    stdout: bool,
}

impl Pipe {
    fn no_pipe() -> Pipe {
        Pipe {
            stdin: None,
            stdout: false,
        }
    }

    fn pipe_out() -> Pipe {
        Pipe {
            stdin: None,
            stdout: true,
        }
    }

    fn pipe_in(stdin: Stdio) -> Pipe {
        Pipe {
            stdin: Some(stdin),
            stdout: false,
        }
    }

    fn pipe_in_out(stdin: Stdio) -> Pipe {
        Pipe {
            stdin: Some(stdin),
            stdout: true,
        }
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

struct Interrupt;

struct Interruptable(Receiver<Interrupt>, AtomicBool);
struct Interrupter(Sender<Interrupt>);

impl Interruptable {
    fn was_interrupted(&self) -> bool {
        self.1.fetch_or(
            self.0.try_recv().is_ok(),
            std::sync::atomic::Ordering::AcqRel,
        )
    }
}

impl Interrupter {
    fn new() -> (Interrupter, Interruptable) {
        let (sender, receiver) = mpsc::channel();
        (
            Interrupter(sender),
            Interruptable(receiver, AtomicBool::new(false)),
        )
    }

    fn interrupt(&self) {
        self.0.send(Interrupt);
    }
}

pub fn exec_script(
    script: &Script,
    config: &ExecConfig,
) -> Result<FinishedProcess, CommandExecError> {
    ShellContext { working_directory: config.working_directory.to_owned(), env: HashMap::new() }.exec_script_entries(&script.commands, config, &HashMap::new())
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
    ) -> Result<FinishedProcess, CommandExecError> {
        // let mut stdout = vec![];
        // let mut stderr = vec![];
        crossbeam_utils::thread::scope(|s| {
            let mut jobs = vec![];

            for AtomicTopLevelCommand(command) in commands {
                use conch_parser::ast::Command;

                let (command, is_job) = match command {
                    Command::Job(job) => (job, true),
                    Command::List(list) => (list, false),
                };

                let (interrupter, interruptable) = Interrupter::new();

                if is_job {
                    let mut frozen_context = self.clone();
                    s.spawn(move |_| frozen_context.exec_andor_list(command, config, env, interruptable));
                    jobs.push(interrupter);
                } else {
                    self.exec_andor_list(command, config, env, interruptable);
                }
            }

            for job in jobs {
                job.interrupt();
            }
        })
        .unwrap();

        Ok(FinishedProcess::new(true))
    }

    fn exec_andor_list(
        &mut self,
        command: &AtomicCommandList<String, AtomicTopLevelWord<String>, AtomicTopLevelCommand<String>>,
        config: &ExecConfig,
        env: &HashMap<String, String>,
        interrupter: Interruptable,
    ) -> Result<FinishedProcess, CommandExecError> {
        let mut previous_output = self.exec_listable_command(&command.first, config, env, &interrupter);
        //TODO: Chain
        Ok(FinishedProcess::new(true))
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
        interrupter: &Interruptable,
    ) -> Result<FinishedProcess, CommandExecError> {
        match command {
            ListableCommand::Pipe(negate, commands) => {
                let mut proc = self.exec_pipeable_command(commands.first().unwrap(), Pipe::pipe_out(), config, env, interrupter)?;
                if commands.len() > 2 {
                    for command in &commands[1..commands.len() - 2] {
                        let next_proc = self.exec_pipeable_command(command, Pipe::pipe_in_out(proc.process.stdout.unwrap().into()), config, env, interrupter)?;
                        proc = next_proc;
                    }
                }
                let last_proc = self.exec_pipeable_command(commands.last().unwrap(), Pipe::pipe_in(proc.process.stdout.unwrap().into()), config, env, interrupter)?;
                Ok(last_proc.wait()) //TODO: Negate?
            },
            ListableCommand::Single(command) => {
                self.exec_pipeable_command(&command, Pipe::no_pipe(), config, env, interrupter).map(OngoingProcess::wait)
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
        interrupter: &Interruptable,
    ) -> Result<OngoingProcess, CommandExecError> {
        match command {
            PipeableCommand::Simple(command) => self.exec_simple_command(&command, pipe, config, env, interrupter),
            PipeableCommand::Compound(command) => {
                // Stuff like if, while, until, etc.
                todo!();
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
        interrupter: &Interruptable,
    ) -> Result<OngoingProcess, CommandExecError> {
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

        let output = Command::new(&command_words[0])
            .args(&command_words[1..])
            .stdin(match pipe.stdin { Some(pipe) => pipe, None => Stdio::inherit() })
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

        Ok(output.into())
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
            ParameterSubstitution::Command(commands) => self.exec_script_entries(commands, config, env).map(|output| vec![String::from_utf8(output.stdout).unwrap()]),
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
