use std::process::{Command, Stdio};

use crate::runfile;
use crate::Config;

pub fn shell(commands: &Vec<runfile::Command>, config: &Config) {
    for command in commands {
        if !config.silent {
            println!("\t{:?}", command); //TODO: Implement Display
        }
        let status = Command::new(command.target.clone())
            .args(
                command.args.iter()
                    .map(|x| {
                        if let runfile::ArgPart::Str(s) = &x[0] {
                            s
                        } else {
                            unimplemented!();
                        }
                    }
                )
            )
            .stdin(Stdio::null())
            .stdout(if config.quiet { Stdio::null() } else { Stdio::inherit() })
            .stderr(if config.quiet { Stdio::null() } else { Stdio::inherit() })
            .status()
            .expect("Failed to execute command"); //TODO
        if !config.silent {
            match status.code() {
                Some(i) => println!("\t`{:?}` exited with code {}", command, i),
                None => println!("\t`{:?}` terminated by signal", command), //TODO figure out which signal
            }
        }
        if !status.success() {
            break;
        }
    }
}