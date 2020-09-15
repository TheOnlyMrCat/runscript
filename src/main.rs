#[macro_use] extern crate lalrpop_util;

use std::env;
use std::path::{Path, PathBuf};
use std::ffi::OsStr;
use std::rc::Rc;

use getopts::Options;

use codespan_reporting::files::Files;

use termcolor::{StandardStream, ColorChoice};

mod out;
mod exec;
mod runfile;

lalrpop_mod!(pub parser);
lalrpop_mod!(pub doubled);

use out::*;
use exec::shell;
use runfile::{TargetMeta, ScriptType, Target, RunFiles, RunFileRef};

const VERSION: &'static str = env!("CARGO_PKG_VERSION");

const PHASES_B: [ScriptType; 2] = [ScriptType::BuildOnly, ScriptType::Build];
const PHASES_T: [ScriptType; 3] = [ScriptType::Build, ScriptType::BuildAndRun, ScriptType::Run];
const PHASES_R: [ScriptType; 2] = [ScriptType::Run, ScriptType::RunOnly];

fn main() {
    if !run(env::args().skip(1), &env::current_dir().expect("Working environment is not sane"), 0, false).0 {
		std::process::exit(1);
    }
}

pub fn run<'a, T: IntoIterator>(args: T, cwd: &Path, inherit_quiet: i32, piped: bool) -> (bool, Vec<u8>)
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
        println!("Runscript {}", VERSION);
		println!("Written by TheOnlyMrCat");
        println!("Source code available at https://github.com/TheOnlyMrCat/runscript");
        return (true, vec![]);
    }

    let path_branch_file: String;
    let path_branch_dir: String;
    let run_target: String;
    if let Some(target) = matches.free.get(0) {
        let v = target.split(':').collect::<Vec<&str>>();
        
        if v.len() == 1 {
            path_branch_file = "".to_owned();
            path_branch_dir = "run".to_owned();
            run_target = v[0].to_owned();
        } else if v.len() == 2 {
            let path_branch = v[0].to_owned();

            let mut pbf = path_branch.clone();
            pbf.push_str(".run");
            path_branch_file = pbf;

            let mut pbd = path_branch.clone();
            pbd.push_str("/run");
            path_branch_dir = pbd;

            run_target = v[1].to_owned();
        } else {
            path_branch_file = "".to_owned();
            path_branch_dir = "run".to_owned();
            run_target = "".to_owned();
        }
    } else {
        path_branch_file = "".to_owned();
        path_branch_dir = "run".to_owned();
        run_target = "".to_owned();
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
        panic!("Failed to identify script phases to run. This is a bug.")
    }

    let output_stream = Rc::new(StandardStream::stderr(ColorChoice::Auto));

    let mut runfile_data: Option<(String, PathBuf)> = None;
    for path in cwd.ancestors() {
        let file_path = path.join(&path_branch_file);
        if let Ok(s) = std::fs::read_to_string(&file_path) {
            runfile_data = Some((s, file_path));
            break;
        }

        let dir_path = path.join(&path_branch_dir);
        if let Ok(s) = std::fs::read_to_string(&dir_path) {
            runfile_data = Some((s, dir_path));
            break;
        }
    }

    let (runfile, runfile_path) = match runfile_data {
        Some(r) => r,
        None => {
            file_read_err(output_stream);
            return (false, vec![])
        }
	};

    let config = Config {
        quiet: matches.opt_present("quiet") || inherit_quiet > 0 || piped,
        silent: matches.opt_count("quiet") > 1 || inherit_quiet > 1 || piped,
		codespan_file: RunFiles {
			root: RunFileRef {
				file: None,
				name: runfile_path.file_name().unwrap().to_string_lossy().to_owned().to_string(),
				starts: codespan_reporting::files::line_starts(&runfile).collect(),
				source: runfile.into_boxed_str().into(),
			},
		},
        expect_fail: matches.opt_present("expect-fail"),
        file: runfile_path,
        args: if matches.free.len() > 1 { matches.free[1..].to_vec() } else { vec![] },
		output_stream: output_stream,
    };

    match parser::RunFileParser::new().parse(&[], &config.file, &config.codespan_file.source(&[]).unwrap()) {
        Ok(rf) => {
            let mut output_acc = Vec::new();
            for &phase in phases {
                if run_target == "" {
                    match do_run_target(rf.get_default_target(), "default", phase, &config, piped) {
                        Ok(mut v) => output_acc.append(&mut v),
                        Err(_) => return (config.expect_fail, output_acc)
                    }
                } else {
                    let target = rf.get_target(&run_target);
                    if let None = target {
                        bad_target(&config, run_target);
                        return (config.expect_fail, output_acc);
                    }
                    match do_run_target(target, &run_target, phase, &config, piped) {
                        Ok(mut v) => output_acc.append(&mut v),
                        Err(_) => return (config.expect_fail, output_acc)
                    }
                }
                match do_run_target(rf.get_global_target(), "global", phase, &config, piped) {
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

pub struct Config {
    quiet: bool,
    silent: bool,
    expect_fail: bool,
    file: PathBuf,
    args: Vec<String>,
    codespan_file: RunFiles,
    output_stream: Rc<StandardStream>,
}