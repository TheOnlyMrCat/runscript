#[macro_use] extern crate lalrpop_util;

use std::env;
use std::path::{Path, PathBuf};
use std::ffi::OsStr;
use std::fs::File;
use std::io::prelude::*;
use std::rc::Rc;

use getopts::Options;

use codespan_reporting::files::SimpleFile;

use termcolor::{StandardStream, ColorChoice};

mod out;
mod exec;
mod runfile;

lalrpop_mod!(pub parser);
lalrpop_mod!(pub doubled);

use out::*;
use exec::shell;
use runfile::{TargetMeta, ScriptType, Target};

const VERSION: &'static str = env!("CARGO_PKG_VERSION");

const PHASES_B: [ScriptType; 2] = [ScriptType::BuildOnly, ScriptType::Build];
const PHASES_T: [ScriptType; 3] = [ScriptType::Build, ScriptType::BuildAndRun, ScriptType::Run];
const PHASES_R: [ScriptType; 2] = [ScriptType::Run, ScriptType::RunOnly];

fn main() -> Result<(), ()> {
    if run(env::args().skip(1), &env::current_dir().expect("Working environment is not sane"), 0, false).0 {
        Ok(())
    } else {
        Err(())
    }
}

pub fn run<'a, T: Iterator>(args: T, cwd: &Path, inherit_quiet: i32, piped: bool) -> (bool, Vec<u8>)
    where T::Item: AsRef<OsStr>
{
    let mut options = Options::new();

    options.optflag("h", "help", "Show this very helpful text");
    options.optflag("", "version", "Print version information");
    options.optflagmulti("q", "quiet", "Passed once: Do not show output of run commands. Twice: Do not print commands as they are being run");
    options.optflag("b", "build-only", "Only execute `b!` and `b` scripts");
    options.optflag("", "build-and-run", "Execute `b`, `br`, and `r` scripts (default)");
    options.optflag("r", "run-only", "Only execute `r` and `r!` scripts");
    options.optflag("x", "expect-fail", "Invert exit code");

    let matches = match options.parse(args) {
        Ok(m) => m,
        Err(x) => {
            option_parse_err(x);
            return (false, vec![]);
        },
    };

    if matches.opt_present("help") {
        print!("{}", options.usage("Usage: run [options] target"));
        return (false, vec![]);
    }

    if matches.opt_present("version") {
        println!("Runscript version {}", VERSION);
        println!("Author: TheOnlyMrCat");
        println!("Repository: https://github.com/TheOnlyMrCat/runscript");
        return (true, vec![]);
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
            run_target = "".to_owned();
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

    let mut config = Config {
        quiet: matches.opt_present("quiet") || inherit_quiet > 0 || piped,
        silent: matches.opt_count("quiet") > 1 || inherit_quiet > 1 || piped,
        expect_fail: matches.opt_present("expect-fail"),
        file: runfile_path,
        args: if matches.free.len() > 1 { matches.free[1..].to_vec() } else { vec![] },
        codespan_file: SimpleFile::new("".to_owned(), "".to_owned()),
        output_stream: Rc::new(StandardStream::stderr(ColorChoice::Auto))
    };

    let mut file = String::new();
    match File::open(&config.file) {
        Ok(mut f) => match f.read_to_string(&mut file) {
            Ok(_) => {}
            Err(e) => {
                file_read_err(&config, e.kind());
                return (false, vec![]);
            }
        },
        Err(e) => {
            file_read_err(&config, e.kind());
            return (false, vec![]);
        }
    }

    config.codespan_file = SimpleFile::new(String::from(config.file.as_os_str().to_string_lossy()), file.clone());

    match parser::RunFileParser::new().parse(&file) {
        Ok(rf) => {
            let mut output_acc = Vec::new();
            for &phase in phases {
                if run_target == "" {
                    match do_run_target(rf.default_target.as_ref(), "default", phase, &config, piped) {
                        Ok(mut v) => output_acc.append(&mut v),
                        Err(_) => return (config.expect_fail, output_acc)
                    }
                } else {
                    let target = rf.targets.get(&run_target);
                    if let None = target {
                        bad_target(&config, run_target);
                        return (config.expect_fail, output_acc);
                    }
                    match do_run_target(rf.targets.get(&run_target), &run_target, phase, &config, piped) {
                        Ok(mut v) => output_acc.append(&mut v),
                        Err(_) => return (config.expect_fail, output_acc)
                    }
                }
                match do_run_target(rf.global_target.as_ref(), "global", phase, &config, piped) {
                    Ok(mut v) => output_acc.append(&mut v),
                    Err(_) => return (config.expect_fail, output_acc)
                }
            }
            return (!config.expect_fail, output_acc);
        },
        Err(e) => {
            file_parse_err(&config, e);
            return (false, vec![]);
        }
    }
}

fn do_run_target(target: Option<&Target>, name: &str, phase: ScriptType, config: &Config, piped: bool) -> Result<Vec<u8>, Vec<u8>> {
    match target {
        Some(target) => {
            match target.commands.get(&TargetMeta { script: phase }) {
                Some(c) => {
                    phase_message(&config, phase, name);
                    let (status, output) = shell(&c.commands, &config, piped);
                    if status {
                        Ok(output)
                    } else {
                        Err(output)
                    }
                }
                None => Ok(vec![])
            }
        },
        None => Ok(vec![])
    }
}

#[derive(Clone)]
pub struct Config {
    quiet: bool,
    silent: bool,
    expect_fail: bool,
    file: PathBuf,
    args: Vec<String>,
    codespan_file: SimpleFile<String, String>,
    output_stream: Rc<StandardStream>,
}