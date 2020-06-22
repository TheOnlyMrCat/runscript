// use std::io::{BufReader, BufRead};
use std::process::{Command, Stdio, ExitStatus};

use crate::runfile;
use crate::Config;

pub fn shell(commands: &Vec<runfile::Command>, config: &Config) -> bool {
    for command in commands {
        if !config.silent {
            println!("> {}", command);
        }
        let mut child = Command::new(command.target.clone())
            .args(
                command.args.iter()
                    .map(|x| {
                        if let runfile::ArgPart::Str(s) = &x.parts[0] {
                            s
                        } else {
                            unimplemented!();
                        }
                    }
                )
            )
            .current_dir(config.file.parent().expect("Runfile should have at least one parent"))
            .stdin(Stdio::null())
            .stdout(if config.quiet { Stdio::null() } else { Stdio::inherit() })
            .stderr(if config.quiet { Stdio::null() } else { Stdio::inherit() })
            .spawn()
            .expect("Failed to execute command"); //TODO: What if it fails?
        
        /*
        if !config.quiet {
            //TODO: Might make time-continuous? Also there're some unwraps here
            println!("  Stdout:");
            for line in BufReader::new(child.stdout.as_mut().unwrap()).lines() {
                println!("    {}", line.unwrap());
            }
            println!("  Stderr:");
            for line in BufReader::new(child.stderr.as_mut().unwrap()).lines() {
                println!("    {}", line.unwrap());
            }
        }
        */
        let status = child.wait().expect("Command wasn't running properly");

        if !status.success() {
            if !config.silent {
                match status.code() {
                    Some(i) => println!(">> exit {}", i),
                    None => println!(">> {}", signal(&status)),
                }
            }
            return false;
        }
    }
    return true;
}

#[allow(dead_code)]
#[cfg(unix)]
fn signal(status: &ExitStatus) -> String {
    use std::os::unix::process::ExitStatusExt;

    format!("signal {}", status.signal().expect("Expected signal"))
}

#[allow(dead_code)]
#[cfg(not(unix))]
fn signal(status: &ExitStatus) -> String {
    "error".to_owned()
}