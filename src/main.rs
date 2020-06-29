#[macro_use] extern crate lalrpop_util;

use std::env;

use std::path::PathBuf;

mod exec;
mod runfile;
use exec::run;

lalrpop_mod!(pub parser);
lalrpop_mod!(pub doubled);

fn main() {
    run(env::args().skip(1), &env::current_dir().expect("Working environment is not sane"))
}

#[derive(Clone)]
pub struct Config {
    quiet: bool,
    silent: bool,
    file: PathBuf,
    args: Vec<String>,
}