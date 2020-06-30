#[macro_use] extern crate lalrpop_util;

use std::env;
use std::path::{Path, PathBuf};
use std::ffi::OsStr;
use std::fs::File;
use std::io::prelude::*;

use getopts::Options;

mod exec;
mod runfile;

lalrpop_mod!(pub parser);
lalrpop_mod!(pub doubled);

use exec::shell;
use runfile::{TargetMeta, ScriptType};

fn main() {
    if !run(env::args().skip(1), &env::current_dir().expect("Working environment is not sane")) {
        std::process::exit(1);
    }
}

const PHASES_B: [ScriptType; 2] = [ScriptType::BuildOnly, ScriptType::Build];
const PHASES_T: [ScriptType; 3] = [ScriptType::Build, ScriptType::BuildAndRun, ScriptType::Run];
const PHASES_R: [ScriptType; 2] = [ScriptType::Run, ScriptType::RunOnly];

pub fn run<T: Iterator>(args: T, cwd: &Path) -> bool
    where T::Item: AsRef<OsStr>
{
    let mut options = Options::new();

    options.optflag("h", "help", "Show this very helpful text");
    options.optflagmulti("q", "quiet", "Passed once: Do not show output of run commands. Twice: Do not print commands as they are being run");
    options.optflag("b", "build-only", "Only execute `b!` and `b` scripts");
    options.optflag("", "build-and-run", "Execute `b`, `br`, and `r` scripts (default)");
    options.optflag("r", "run-only", "Only execute `r` and `r!` scripts");
    options.optflag("x", "expect-fail", "Invert exit code");

    let matches = match options.parse(args) {
        Ok(m) => m,
        Err(x) => panic!(x),
    };

    if matches.opt_present("help") {
        print!("{}", options.usage("Usage: run [options] target"));
        return false;
    }

    let mut runfile_path = PathBuf::from(cwd);
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

    let b_pos = matches.opt_positions("build-only")   .iter().fold(-1, |acc, &x| if x as i32 > acc { x as i32 } else { acc });
    let t_pos = matches.opt_positions("build-and-run").iter().fold(-1, |acc, &x| if x as i32 > acc { x as i32 } else { acc });
    let r_pos = matches.opt_positions("run-only")     .iter().fold(-1, |acc, &x| if x as i32 > acc { x as i32 } else { acc });

    let phases: &[ScriptType];
    if b_pos + t_pos + r_pos == -3
    || t_pos > b_pos && t_pos > r_pos {
        phases = &PHASES_T;
    } else if b_pos > t_pos && b_pos > r_pos {
        phases = &PHASES_B;
    } else if r_pos > t_pos && r_pos > b_pos {
        phases = &PHASES_R;
    } else {
        panic!("Failed to identify script phases to run")
    }

    let config = Config {
        quiet: matches.opt_present("quiet"),
        silent: matches.opt_count("quiet") > 1,
        expect_fail: matches.opt_present("expect-fail"),
        file: runfile_path,
        args: if matches.free.len() > 1 { matches.free[1..].to_vec() } else { vec![] }
    };

    let mut file = String::new();
    File::open(&config.file).expect("Failed to open file").read_to_string(&mut file).expect("Failed to read file");

    match parser::RunFileParser::new().parse(&file) {
        Ok(rf) => {
            for &phase in phases {
                if run_target == "" {
                    match &rf.default_target {
                        Some(target) => {
                            match target.commands.get(&TargetMeta { script: phase }).and_then(|c| {
                                eprintln!("{} default", phase);
                                if shell(&c, &config) {
                                    None
                                } else {
                                    Some(())
                                }
                            }) {
                                Some(_) => return config.expect_fail, // Some: The command was executed, and errored.
                                None => {}
                            }
                        },
                        None => {}
                    }
                } else {
                    match rf.targets.get(&run_target) {
                        Some(target) => {
                            match target.commands.get(&TargetMeta { script: phase }).and_then(|c| {
                                eprintln!("{} {}", phase, run_target);
                                if shell(&c, &config) {
                                    None
                                } else {
                                    Some(())
                                }
                            }) {
                                Some(_) => return config.expect_fail,
                                None => {}
                            }
                        },
                        None => panic!("No target with name '{}'", run_target)
                    }
                }
                match &rf.global_target {
                    Some(target) => {
                        match target.commands.get(&TargetMeta { script: phase }).and_then(|c| {
                            eprintln!("{} global", phase);
                            if shell(&c, &config) {
                                None
                            } else {
                                Some(())
                            }
                        }) {
                            Some(_) => return config.expect_fail,
                            None => {}
                        }
                    },
                    None => {}
                }
            }
            return !config.expect_fail;
        },
        Err(e) => {
            eprintln!("{:#?}", e);
            return false;
        }
    }
}

#[derive(Clone)]
pub struct Config {
    quiet: bool,
    silent: bool,
    expect_fail: bool,
    file: PathBuf,
    args: Vec<String>,
}