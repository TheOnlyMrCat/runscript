use std::io::Write;
use std::process::{Command, Stdio, Output, ExitStatus};

use crate::runfile::{self, ArgPart, ChainedCommand};
use crate::Config;
use crate::run;

pub fn shell(commands: &Vec<runfile::Command>, config: &Config) -> bool {
    for command in commands {
        if !config.silent {
            eprintln!("> {}", command);
        }
        if command.target == "run" {
            run(command.args.iter().map(|x| x.parts.iter().fold("".to_owned(), fold_arg(config.clone()))), config.file.parent().expect("Runfile should have at least one parent"));
        } else {
            let status = exec(&command, config, false).status;
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

fn exec(command: &runfile::Command, config: &Config, piped: bool) -> Output {
    if command.target == "run" {
        panic!("Cannot chain recursive run calls");
    }

    let mut stdin: Option<Vec<u8>> = None;

    match &*command.chained {
        ChainedCommand::Pipe(c) => {
            let h = exec(&c, config, true);
            stdin = Some(h.stdout.clone());
        },
        ChainedCommand::And(c) => {
            let h = exec(&c, config, false);
            if !h.status.success() {
                return h;
            }
        },
        ChainedCommand::Or(c) => {
            let h = exec(&c, config, false);
            if h.status.success() {
                return h;
            }
        },
        ChainedCommand::None => {}
    }

    let mut child = Command::new(command.target.clone())
        .args(command.args.iter().map(|x| x.parts.iter().fold("".to_owned(), fold_arg(config.clone()))))
        .current_dir(config.file.parent().expect("Runfile should have at least one parent"))
        .stdin(match stdin { Some(_) => Stdio::piped(), None => Stdio::null() })
        .stdout(if piped { Stdio::piped() } else if config.quiet { Stdio::null() } else { Stdio::inherit() })
        .stderr(if config.quiet { Stdio::null() } else { Stdio::inherit() })
        .spawn()
        .expect("Failed to execute command"); //TODO: What if it fails?

    match stdin {
        Some(v) => {
            let buffer = &v;
            child.stdin.as_mut().unwrap().write(buffer).expect("Failed to write stdin");
        },
        _ => {}
    }

    child.wait_with_output().expect("Command was never started")
}

fn fold_arg(config: Config) -> Box<dyn Fn(String, &ArgPart) -> String> {
    Box::new(move |acc, p| match p {
        ArgPart::Str(s) => acc + s,
        ArgPart::Arg(n) => acc + &config.args[*n - 1],
        ArgPart::Var(v) => acc + &std::env::var(v).unwrap_or("".to_owned()),
        ArgPart::Cmd(c) => acc + &String::from_utf8_lossy(&Command::new(c.target.clone())
            .args(c.args.iter().map(|x| x.parts.iter().fold("".to_owned(), fold_arg(config.clone()))))
            .current_dir(config.file.parent().expect("Runfile should have at least one parent"))
            .output()
            .expect("Failed to execute command").stdout),
    })
}

#[cfg(unix)]
fn signal(status: &ExitStatus) -> String {
    use std::os::unix::process::ExitStatusExt;

    format!("signal {}", status.signal().expect("Expected signal"))
}

#[cfg(not(unix))]
fn signal(_status: &ExitStatus) -> String {
    "error".to_owned()
}