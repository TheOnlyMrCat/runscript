#[macro_use] extern crate lalrpop_util;

use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::process::{Command, Stdio};

use getopts::Options;

mod runfile;
mod lexer;
use lexer::Lexer;
lalrpop_mod!(pub parser);

fn main() {
    let mut options = Options::new();

    options.optflag("h", "help", "Show this very helpful text");
    options.optflagmulti("q", "quiet", "Passed once: Do not show output of run commands. Twice: Do not print commands as they are being run"); //TODO

    let matches = match options.parse(env::args().skip(1)) {
        Ok(m) => m,
        Err(x) => panic!(x),
    };

    if matches.opt_present("help") {
        print!("{}", options.usage("Usage: run [options] target"));
        return;
    }

    let quiet = matches.opt_present("quiet");
    let silent = matches.opt_count("quiet") > 1;

    let mut runfile_path = env::current_dir().expect("Couldn't get current working directory");
    let run_target: String;
    if let Some(target) = matches.free.get(0) {
        let v = target.split(':').collect::<Vec<&str>>();
        
        if v.len() == 1 {
            runfile_path.push("run");
            run_target = v[0].to_owned();
        } else if v.len() == 2 {
            let mut s = v[0].to_owned();
            s.push_str(".run");
            runfile_path.push(s);
            run_target = v[1].to_owned();
        } else {
            panic!("Invalid target");
        }
    } else {
        runfile_path.push("run");
        run_target = String::new();
    }

    let mut file = String::new();
    File::open(runfile_path).expect("Failed to open file").read_to_string(&mut file).expect("Failed to read file");

    match parser::RunFileParser::new().parse(Lexer::new(&mut file.char_indices())) {
        Ok(rf) => {
            for command in rf.global_target.unwrap().commands {
                if !silent {
                    println!("{:?}", command); //TODO: Implement Display
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
                    .stdout(if quiet { Stdio::null() } else { Stdio::inherit() })
                    .stderr(if quiet { Stdio::null() } else { Stdio::inherit() })
                    .status()
                    .expect("Failed to execute command"); //TODO
                if !silent {
                    match status.code() {
                        Some(i) => println!("`{:?}` exited with code {}", command, i),
                        None => println!("`{:?}` terminated by signal", command), //TODO figure out which signal
                    }
                }
            }
        },
        Err(e) => {
            eprintln!("{:#?}", e);
        }
    }
}
