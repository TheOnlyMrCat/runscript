use std::io::Write;
use std::process::{Command, Stdio, Output, ExitStatus};

use crate::runfile::{self, ArgPart, ChainedCommand, Argument};
use crate::Config;
use crate::run;
use crate::out::{bad_command_err, bad_chain};

pub fn shell(commands: &Vec<runfile::Command>, config: &Config) -> bool {
    for command in commands {
        if !config.silent {
            eprintln!("> {}", command);
        }
        if command.target == "run" {
            if let ChainedCommand::None = *command.chained {
                if !run(command.args.iter().map(|x| evaluate_arg(x, config)), config.file.parent().expect("Runfile should have at least one parent")) {
                    eprintln!("=> exit 1");
                    return false;
                }
            } else {
                bad_chain(&config, &command);
                return false;
            }
        } else {
            let status = match exec(&command, config, false) {
                Ok(k) => k.status,
                Err(_) => return false,
            };
            if !status.success() {
                if !config.silent {
                    match status.code() {
                        Some(i) => eprintln!("=> exit {}", i),
                        None => eprintln!("=> {}", signal(&status)),
                    }
                }
                return false;
            }
        }
    }
    return true;
}

fn exec(command: &runfile::Command, config: &Config, piped: bool) -> Result<Output, ()> {
    if command.target == "run" {
        bad_chain(&config, command);
        return Err(())
    }

    let mut stdin: Option<Vec<u8>> = None;

    match &*command.chained {
        ChainedCommand::Pipe(c) => {
            let h = exec(&c, config, true)?;
            stdin = Some(h.stdout.clone());
        },
        ChainedCommand::And(c) => {
            let h = exec(&c, config, false)?;
            if !h.status.success() {
                return Ok(h);
            }
        },
        ChainedCommand::Or(c) => {
            let h = exec(&c, config, false)?;
            if h.status.success() {
                return Ok(h);
            }
        },
        ChainedCommand::None => {}
    }

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

    Ok(child.wait_with_output().expect("Command was never started"))
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
    "error".to_owned()
}