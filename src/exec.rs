use std::collections::HashMap;
use std::io::Write;
use std::path::Path;
use std::process::{Command, Stdio, Output, ExitStatus};

use filenamegen::Glob;
use termcolor::ColorSpec;

use crate::out;
use crate::parser::RunscriptLocation;
use crate::run;
use crate::script::{self, Runscript, ArgPart, ChainedCommand, Argument, ScriptEntry};

#[derive(Clone)]
pub struct ExecConfig<'a> {
	pub verbosity: Verbosity,
	pub output_stream: &'a std::rc::Rc<termcolor::StandardStream>,
	pub working_directory: &'a Path,
	pub positional_args: Vec<String>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
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

#[derive(Debug)]
pub enum CommandExecErr {
	InvalidGlob {
		glob: String,
		err: anyhow::Error,
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

pub fn shell(entries: &[ScriptEntry], script: &Runscript, config: &ExecConfig, capture_stdout: bool, env_remap: &HashMap<String, String>) -> (bool, Vec<u8>) {
	let mut env = env_remap.clone();
	let mut output_acc = Vec::new();
	for entry in entries {
		match entry {
			ScriptEntry::Command(command) => {
				let output = match exec(&command, config, &env, capture_stdout) {
					Ok(k) => k,
					Err(err) => {
						out::bad_command_err(config.output_stream, &command, script, err);
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
			},
			ScriptEntry::Env { var, val, loc } => {
				if config.verbosity < Verbosity::Silent {
					eprintln!("> {}", entry);
				}
				match evaluate_arg(val, loc.clone(), config, &env, false) {
					Ok(arg) => { env.insert(var.clone(), arg.join(" ")); },
					Err(_) => {
						eprintln!("Failed to evaluate arg"); //TODO: Use out module
						return (false, output_acc);
					}
				}
			}
		}
	}
	(true, output_acc)
}

fn exec(command: &script::Command, config: &ExecConfig, env_remap: &HashMap<String, String>, piped: bool) -> Result<ProcessOutput, CommandExecErr> {
	let target = &evaluate_arg(&command.target, command.loc.clone(), config, env_remap, false)?[0];
	if target == "run" {
		if config.verbosity < Verbosity::Silent {
			eprintln!("> {}", command);
		}
		let args = command.args.iter()
			.map(|x| evaluate_arg(x, command.loc.clone(), config, env_remap, true))
			.collect::<Result<Vec<Vec<String>>, CommandExecErr>>()?
			.into_iter()
			.flatten()
			.collect::<Vec<_>>();
		//TODO: Pass env
		let (success, output) = run(
			&args.iter().map(|s| &**s).collect::<Vec<_>>(),
			config.working_directory,
			config.verbosity,
			piped,
			env_remap,
		);
		return Ok(ProcessOutput {
			status: ProcessExit::Bool(success),
			stdout: output,
		})
	}

	let mut stdin: Option<Vec<u8>> = None;

	let chained_output = match &*command.chained {
		ChainedCommand::Pipe(c) => {
			let h = exec(&c, config, env_remap, true)?;
			stdin = Some(h.stdout);
			None
		},
		ChainedCommand::And(c) => {
			let h = exec(&c, config, env_remap, piped)?;
			if !h.status.success() {
				return Ok(h);
			}
			Some(h.stdout)
		},
		ChainedCommand::Or(c) => {
			let h = exec(&c, config, env_remap, piped)?;
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
				return Err(e);
			}
		}
	}

	if config.verbosity < Verbosity::Silent {
		let mut lock = config.output_stream.lock();
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

	let mut child = match Command::new(target)
		.args(args)
		.current_dir(config.working_directory)
		.envs(env_remap)
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

fn evaluate_arg(arg: &Argument, command_loc: RunscriptLocation, config: &ExecConfig, env_override: &HashMap<String, String>, match_globs: bool) -> Result<Vec<String>, CommandExecErr> {
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
								Err(CommandExecErr::NoGlobMatches { glob: s.clone(), loc: command_loc })
							} else {
								Ok(strings)
							}
						},
						Err(err) => Err(CommandExecErr::InvalidGlob { glob: s.clone(), err, loc: command_loc })
					}
				} else {
					Ok(vec![s.clone()])
				}
			},
			_ => evaluate_part(p, env_override, config),
		},
		Argument::Single(s) => Ok(vec![s.clone()]),
		Argument::Double(p) => {
			let args = p.iter().map(|x| evaluate_part(x, env_override, config)).collect::<Result<Vec<Vec<String>>, CommandExecErr>>()?.iter().fold(vec![], |mut acc, x| { acc.append(&mut x.clone()); acc });
			Ok(vec![args.iter().fold(String::new(), |acc, s| acc + &s)])
		}
	}
}

fn evaluate_part(part: &ArgPart, env_override: &HashMap<String, String>, config: &ExecConfig) -> Result<Vec<String>, CommandExecErr> {
	use std::env::VarError::*;
	match part {
		ArgPart::Str(s) => Ok(vec![s.clone()]),
		ArgPart::Arg(n) => Ok(vec![config.positional_args.get(*n - 1).cloned().unwrap_or_else(|| "".to_owned())]),
		ArgPart::Var(v) => Ok(vec![env_override.get(v).cloned().unwrap_or_else(|| std::env::var(v).unwrap_or_else(|err| match err { NotPresent => "".to_owned(), NotUnicode(s) => s.to_string_lossy().into_owned() }))]),
		ArgPart::Cmd(c) => {
			Ok(vec![String::from_utf8_lossy(
				&match Command::new(&evaluate_arg(&c.target, c.loc.clone(), config, env_override, false)?[0])
					.args(c.args.iter().map(|x| evaluate_arg(x, c.loc.clone(), config, env_override, true)).collect::<Result<Vec<Vec<String>>, CommandExecErr>>()?.iter().fold(vec![], |mut acc, x| { acc.append(&mut x.clone()); acc }))
					.current_dir(config.working_directory)
					.output() {
						Ok(o) => o.stdout,
						Err(e) => return Err(CommandExecErr::BadCommand {
							err: e,
							loc: c.loc.clone(),
						})
					}
				).into_owned()])
		}
	}
}

#[cfg(unix)]
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
	panic!("Non-unix program terminated with signal");
}