use std::io::Write;
use std::process::{Command, Stdio, Output, ExitStatus};

use crate::runfile::{self, ArgPart, ChainedCommand, Argument};
use crate::Config;
use crate::run;
use crate::out::bad_command_err;

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
            ProcessExit::Bool(b) => return Some(*b as i32),
            ProcessExit::Status(s) => return s.code(),
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

pub fn shell(commands: &Vec<runfile::Command>, config: &Config, piped: bool) -> (bool, Vec<u8>) {
    let mut output_acc = Vec::new();
    for command in commands {
        if !config.silent {
            eprintln!("> {}", command);
        }
        let mut output = match exec(&command, config, piped) {
            Ok(k) => k,
            Err(_) => return (false, vec![]),
        };
        output_acc.append(&mut output.stdout);
        if !output.status.success() {
            if !config.silent {
                match output.status.code() {
                    Some(i) => eprintln!("=> exit {}", i),
                    None => eprintln!("=> {}", signal(&output.status.status())),
                }
            }
            return (false, output_acc);
        }
    }
    return (true, output_acc);
}

fn exec(command: &runfile::Command, config: &Config, piped: bool) -> Result<ProcessOutput, ()> {
    if command.target == "run" {
        let (success, output) = run(command.args.iter().map(|x| evaluate_arg(x, config)), config.file.parent().expect("Runfile should have at least one parent"), config.quiet as i32 + config.silent as i32, piped);
        return Ok(ProcessOutput {
            status: ProcessExit::Bool(success),
            stdout: output,
        })
    }

    let mut stdin: Option<Vec<u8>> = None;

    let mut chained_output = match &*command.chained {
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
        .args(command.args.iter().map(|x| evaluate_arg(x, config)))
        .current_dir(config.file.parent().expect("Runfile should have at least one parent"))
        .stdin(match stdin { Some(_) => Stdio::piped(), None => if config.quiet { Stdio::null() } else { Stdio::inherit() } })
        .stdout(if piped { Stdio::piped() } else if config.quiet { Stdio::null() } else { Stdio::inherit() })
        .stderr(if config.quiet { Stdio::null() } else { Stdio::inherit() })
        .spawn() {
        Ok(c) => c,
        Err(e) => {
            bad_command_err(&config, command, e.kind());
            return Err(())
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

fn evaluate_arg(arg: &Argument, config: &Config) -> String {
    match arg {
        Argument::Unquoted(p) => evaluate_part(p, config),
        Argument::Single(s) => s.clone(),
        Argument::Double(p) => p.iter().map(|x| evaluate_part(x, config)).fold(String::new(), |acc, s| acc + &s)
    }
}

fn evaluate_part(part: &ArgPart, config: &Config) -> String {
    match part {
        ArgPart::Str(s) => s.clone(),
        ArgPart::Arg(n) => config.args[*n - 1].clone(),
        ArgPart::Var(v) => std::env::var(v).unwrap_or("".to_owned()),
        ArgPart::Cmd(c) => String::from_utf8_lossy(
            &Command::new(c.target.clone())
                .args(c.args.iter().map(|x| evaluate_arg(x, config)))
                .current_dir(config.file.parent().expect("Runfile should have at least one parent"))
                .output()
                .expect("Failed to execute command").stdout
            ).into_owned().to_owned() //TODO: Propagate error correctly
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
    format!("signal {}", sigstr.to_str().expect("Expected returned string to be valud UTF-8"))
}

#[cfg(not(unix))]
fn signal(_status: &ExitStatus) -> String {
    panic!("Process had no exit code")
}