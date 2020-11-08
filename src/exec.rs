use std::io::Write;
use std::path::Path;
use std::process::{Command, Stdio, Output, ExitStatus};

use filenamegen::Glob;

use crate::out::{bad_command_err, CommandExecErr};
use crate::parser::RunscriptLocation;
use crate::run;
use crate::script::{self, ArgPart, ChainedCommand, Argument};

pub struct ExecConfig<'a> {
	pub verbosity: Verbosity,
	pub output_stream: termcolor::StandardStream,
	pub working_directory: &'a Path,
	pub positional_args: Vec<String>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd)]
pub enum Verbosity {
	Normal,
	Quiet,
	Silent,
}

#[derive(Debug)]
enum ProcessExit {
    Bool(bool),
    Status(ExitStatus),
}

#[derive(Debug)]
struct ProcessOutput {
    status: ProcessExit,
    stdout: Vec<u8>
}

impl ProcessExit {
    fn success(&self) -> bool {
        match self {
            ProcessExit::Bool(b) => *b,
            ProcessExit::Status(s) => s.success(),
        }
    }

    fn code(&self) -> Option<i32> {
        match self {
            ProcessExit::Bool(b) => Some(if *b { 0 } else { 1 }),
            ProcessExit::Status(s) => s.code(),
        }
    }

    fn status(self) -> ExitStatus {
        match self {
            ProcessExit::Status(s) => s,
            ProcessExit::Bool(_) => panic!("Expected ExitStatus")
        }
    }
}

impl From<Output> for ProcessOutput {
    fn from(o: Output) -> Self {
        ProcessOutput {
            status: ProcessExit::Status(o.status),
            stdout: o.stdout
        }
    }
}

pub fn shell(commands: &Vec<script::Command>, config: &ExecConfig, capture_stdout: bool) -> (bool, Vec<u8>) {
    let mut output_acc = Vec::new();
    for command in commands {
        if config.verbosity < Verbosity::Silent {
            eprintln!("> {}", command);
        }
        let mut output = match exec(&command, config, capture_stdout) {
            Ok(k) => k,
            Err(err) => {
                bad_command_err(config, &command, err);
                return (false, output_acc)
            },
		};
		if capture_stdout {
			output_acc.extend(output.stdout.into_iter());
		}
        if !output.status.success() {
            if config.verbosity < Verbosity::Silent {
                match output.status.code() {
                    Some(i) => eprintln!("=> exit {}", i),
                    None => eprintln!("=> {}", signal(&output.status.status())),
                }
            }
            return (false, output_acc);
        }
    }
    (true, output_acc)
}

fn exec(command: &script::Command, config: &ExecConfig, piped: bool) -> Result<ProcessOutput, CommandExecErr> {
    if command.target == "run" {
        let (success, output) = run(
			command.args.iter()
				.map(|x| evaluate_arg(x, command.loc, config))
				.collect::<Result<Vec<Vec<String>>, CommandExecErr>>()?
				.iter()
				.fold(vec![], |mut acc, x| { acc.append(&mut x.clone()); acc }),
			config.working_directory,
			config.verbosity, piped);
        return Ok(ProcessOutput {
            status: ProcessExit::Bool(success),
            stdout: output,
        })
    }

    let mut stdin: Option<Vec<u8>> = None;

    let chained_output = match &*command.chained {
        ChainedCommand::Pipe(c) => {
            let h = exec(&c, config, true)?;
            stdin = Some(h.stdout);
            None
        },
        ChainedCommand::And(c) => {
            let h = exec(&c, config, piped)?;
            if !h.status.success() {
                return Ok(h);
            }
            Some(h.stdout)
        },
        ChainedCommand::Or(c) => {
            let h = exec(&c, config, piped)?;
            if h.status.success() {
                return Ok(h);
            }
            Some(h.stdout)
        },
        ChainedCommand::None => None
    };

    let mut child = match Command::new(command.target.clone())
        .args(command.args.iter().map(|x| evaluate_arg(x, command.loc, config)).collect::<Result<Vec<Vec<String>>, CommandExecErr>>()?.iter().fold(vec![], |mut acc, x| { acc.append(&mut x.clone()); acc }))
        .current_dir(config.working_directory)
        .stdin(match stdin { Some(_) => Stdio::piped(), None => if config.verbosity >= Verbosity::Quiet { Stdio::null() } else { Stdio::inherit() } })
        .stdout(if piped { Stdio::piped() } else if config.verbosity >= Verbosity::Quiet { Stdio::null() } else { Stdio::inherit() })
        .stderr(if config.verbosity >= Verbosity::Quiet { Stdio::null() } else { Stdio::inherit() })
        .spawn() {
        Ok(c) => c,
        Err(e) => {
            return Err(CommandExecErr::BadCommand{
                err: e,
                loc: command.loc.clone(),
            })
        }
    };

    match stdin {
        Some(v) => {
            let buffer = &v;
            child.stdin.as_mut().unwrap().write(buffer).expect("Failed to write stdin");
        },
        _ => {}
    }

    let mut output = ProcessOutput::from(child.wait_with_output().expect("Command was never started"));
    if let Some(mut o) = chained_output {
        o.append(&mut output.stdout);
        output.stdout = o;
    }

    Ok(output)
}

fn evaluate_arg(arg: &Argument, command_loc: RunscriptLocation, config: &ExecConfig) -> Result<Vec<String>, CommandExecErr> {
    match arg {
        Argument::Unquoted(p) => match p {
            ArgPart::Str(s) => {
                if s.chars().any(|c| c == '*' || c == '(' || c == '|' || c == '<' || c == '[' || c == '?') {
                    match Glob::new(&s) {
                        Ok(walker) => {
                            let cwd = config.working_directory;
                            let strings = walker.walk(cwd).into_iter()
                                .map(|k| k.to_string_lossy().into_owned().to_owned())
                                .collect::<Vec<String>>();
                            if strings.len() == 0 {
                                Err(CommandExecErr::NoGlobMatches { glob: s.clone(), loc: command_loc.clone() })
                            } else {
                                Ok(strings)
                            }
                        },
                        Err(err) => Err(CommandExecErr::InvalidGlob { glob: s.clone(), err, loc: command_loc.clone() })
                    }
                } else {
                    Ok(vec![s.clone()])
                }
            },
            _ => evaluate_part(p, config),
        },
        Argument::Single(s) => Ok(vec![s.clone()]),
        Argument::Double(p) => {
            let args = p.iter().map(|x| evaluate_part(x, config)).collect::<Result<Vec<Vec<String>>, CommandExecErr>>()?.iter().fold(vec![], |mut acc, x| { acc.append(&mut x.clone()); acc });
            Ok(vec![args.iter().fold(String::new(), |acc, s| acc + &s)])
        }
    }
}

fn evaluate_part(part: &ArgPart, config: &ExecConfig) -> Result<Vec<String>, CommandExecErr> {
	use std::env::VarError::*;
    match part {
        ArgPart::Str(s) => Ok(vec![s.clone()]),
        ArgPart::Arg(n) => Ok(vec![config.positional_args.get(*n - 1).map(|s| s.clone()).unwrap_or_else(|| "".to_owned())]),
        ArgPart::Var(v) => Ok(vec![std::env::var(v).unwrap_or_else(|err| match err { NotPresent => "".to_owned(), NotUnicode(s) => s.to_string_lossy().into_owned() })]),
        ArgPart::Cmd(c) => {
            Ok(vec![String::from_utf8_lossy(
                &match Command::new(c.target.clone())
                    .args(c.args.iter().map(|x| evaluate_arg(x, c.loc, config)).collect::<Result<Vec<Vec<String>>, CommandExecErr>>()?.iter().fold(vec![], |mut acc, x| { acc.append(&mut x.clone()); acc }))
                    .current_dir(config.working_directory)
                    .output() {
                        Ok(o) => o.stdout,
                        Err(e) => return Err(CommandExecErr::BadCommand {
                            err: e,
                            loc: c.loc.clone(),
                        })
                    }
                ).into_owned().to_owned()]) //TODO: Propagate error correctly
        }
    }
}

use std::os::raw::{c_int, c_char};

#[cfg(unix)]
extern {
    fn strsignal(sig: c_int) -> *const c_char;
}

#[cfg(unix)]
fn signal(status: &ExitStatus) -> String {
    use std::os::unix::process::ExitStatusExt;
    use std::ffi::CStr;
    
    let signal = status.signal().expect("Expected signal");

    let sigstr = unsafe { CStr::from_ptr(strsignal(signal as c_int)) };
    format!("signal {}", sigstr.to_str().expect("Expected returned string to be valid UTF-8"))
}

#[cfg(not(unix))]
fn signal(_status: &ExitStatus) -> String {
    panic!("Process had no exit code")
}