use std::io::{BufReader, BufRead};
use std::process::{Command, Stdio};

use crate::runfile;
use crate::Config;

pub fn shell(commands: &Vec<runfile::Command>, config: &Config) {
    for command in commands {
        if !config.silent {
            println!("  {}", command);
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
            .stdout(if config.quiet { Stdio::null() } else { Stdio::piped() })
            .stderr(if config.quiet { Stdio::null() } else { Stdio::piped() })
            .spawn()
            .expect("Failed to execute command"); //TODO: What if it fails?

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
        let status = child.wait().expect("Command wasn't running properly");

        if !config.silent {
            match status.code() {
                Some(i) => println!("  `{}` exited with code {}", command, i),
                None => println!("  `{}` terminated by signal", command), //TODO figure out which signal
            }
        }
        if !status.success() {
            break;
        }
    }
}