#[macro_use] extern crate lalrpop_util;

use std::env;

use std::path::Path;

mod exec;
mod runfile;
use exec::run;

lalrpop_mod!(pub parser);

fn main() {
    run(env::args().skip(1), &env::current_dir().expect("Working environment is not sane"))
}

pub struct Config<'a> {
    quiet: bool,
    silent: bool,
    file: &'a Path,
}