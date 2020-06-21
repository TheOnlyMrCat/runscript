#[macro_use] extern crate lalrpop_util;

use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::process::Command;

use getopts::Options;

mod runfile;
mod lexer;
use lexer::Lexer;
lalrpop_mod!(pub parser);

fn main() {
    let mut options = Options::new();

    options.optflag("h", "help", "Show this very helpful text");
    options.optflag("q", "quiet", "Do not output executed commands to stdout");

    let matches = match options.parse(env::args().skip(1)) {
        Ok(m) => m,
        Err(x) => panic!(x),
    };

    if matches.opt_present("help") {
        print!("{}", options.usage("Usage: run [options] target"));
    }

    let file = String::from_utf8(File::open("test/basic.run").unwrap().bytes().map(|x| x.unwrap()).collect()).unwrap();
    match parser::RunFileParser::new().parse(Lexer::new(&mut file.chars().enumerate())) {
        Ok(rf) => {
            for command in rf.global_target.unwrap().commands {
                println!("{:?}", command); //TODO: Implement Display
                Command::new(command.target).args(command.args.iter().map(|x| {
                    if let runfile::ArgPart::Str(s) = &x[0] {
                        s
                    } else {
                        unimplemented!();
                    }
                })).status().expect("Failed to execute command");
            }
        },
        Err(e) => {
            eprintln!("{:#?}", e);
        }
    }
}
