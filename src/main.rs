#[macro_use] extern crate enum_map;

#[cfg(feature="trace")]
#[macro_use]
extern crate trace;

use std::env;
use std::path::{Path, PathBuf};
use std::ffi::OsStr;
use std::rc::Rc;

use getopts::Options;

use termcolor::{StandardStream, ColorChoice};

mod out;
mod exec;
mod parser;
mod script;

use exec::Verbosity;
use script::ScriptPhase;

#[cfg(feature="trace")]
trace::init_depth_var!();

const VERSION: &str = env!("CARGO_PKG_VERSION");

const PHASES_B: [ScriptPhase; 2] = [ScriptPhase::BuildOnly, ScriptPhase::Build];
const PHASES_T: [ScriptPhase; 3] = [ScriptPhase::Build, ScriptPhase::BuildAndRun, ScriptPhase::Run];
const PHASES_R: [ScriptPhase; 2] = [ScriptPhase::Run, ScriptPhase::RunOnly];

fn main() {
    if let (false, _) = run(env::args().skip(1), &env::current_dir().expect("Working environment is not sane"), Verbosity::Normal, false) {
		std::process::exit(1);
    }
}

pub fn run<T: IntoIterator>(args: T, cwd: &Path, inherit_verbosity: Verbosity, capture_stdout: bool) -> (bool, Vec<u8>)
    where T::Item: AsRef<OsStr>
{
	let output_stream = Rc::new(StandardStream::stderr(ColorChoice::Auto));
	
    let mut options = Options::new();

    options.optflag("h", "help", "Show this very helpful text");
    options.optflag("", "version", "Print version information");
    options.optflagmulti("q", "quiet", "Passed once: Do not show output of run commands. Twice: Produce no output");
    options.optflag("b", "build-only", "Only execute `b!` and `b` scripts");
    options.optflag("", "build-and-run", "Execute `b`, `br`, and `r` scripts (default)");
    options.optflag("r", "run-only", "Only execute `r` and `r!` scripts");
    options.optflag("x", "expect-fail", "Invert exit code");

    let matches = match options.parse(args) {
        Ok(m) => m,
        Err(x) => {
            out::option_parse_err(&output_stream, x);
            return (false, vec![]);
        },
    };

    if matches.opt_present("help") {
        print!("{}", options.usage("Usage: run [options] target"));
        return (true, vec![]);
    }

    if matches.opt_present("version") {
        println!("Runscript {}", VERSION);
		println!("Written by TheOnlyMrCat");
        println!("Source code available at https://github.com/TheOnlyMrCat/runscript");
        return (true, vec![]);
	}
	
	let expect_fail = matches.opt_present("expect-fail");

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

            let mut pbd = path_branch;
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

    let phases: &[ScriptPhase];
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
            out::file_read_err(&output_stream);
            return (false, vec![])
        }
	};

    match parser::parse_runfile(&runfile_path) {
        Ok(rf) => {
			use crate::exec::ExecConfig;
			let exec_config = ExecConfig {
				output_stream: &output_stream,
				verbosity: match matches.opt_count("quiet") {
					0 => Verbosity::Normal,
					1 => Verbosity::Quiet,
					_ => Verbosity::Silent,
				},
				working_directory: runfile_path.parent().expect("Expected file to have parent"),
				positional_args: matches.free.get(1..).unwrap_or(&[]).to_owned(),
			};

			let mut output_acc = Vec::new();
			//TODO: Instead, find all scripts that would run given the target and phases?
            for &phase in phases {
                if run_target == "" {
					match rf.get_default_script(phase) {
						Some(script) => {
							out::phase_message(&output_stream, phase, "default");
							let (success, output) = exec::shell(&script.commands, &rf, &exec_config, capture_stdout);
							if capture_stdout {
								output_acc.extend(output.into_iter());
							}
							if !success {
								return (expect_fail, output_acc);
							}
						},
						None => {}
					}
				} else {
					match rf.get_target(&run_target) {
						Some(target) => match &target[phase] {
							Some(script) => {
								out::phase_message(&output_stream, phase, &run_target);
								let (success, output) = exec::shell(&script.commands, &rf, &exec_config, capture_stdout);
								if capture_stdout {
									output_acc.extend(output.into_iter());
								}
								if !success {
									return (expect_fail, output_acc);
								}
							},
							None => {}
						},
						None => {
							//TODO: Possibly pass arguments to default target
							out::bad_target(&output_stream, run_target);
							return (expect_fail, output_acc);
						}
					}
				}
				match &rf.get_global_script(phase) {
					Some(script) => {
						out::phase_message(&output_stream, phase, "global");
						let (success, output) = exec::shell(&script.commands, &rf, &exec_config, capture_stdout);
						if capture_stdout {
							output_acc.extend(output.into_iter());
						}
						if !success {
							return (expect_fail, output_acc);
						}
					},
					None => {}
				}
            }
            (!expect_fail, output_acc)
        },
        Err(parser::ParseOrIOError::ParseError(e)) => {
            out::file_parse_err(&output_stream, e);
            (false, vec![])
		},
		Err(parser::ParseOrIOError::IOError(_)) => {
			todo!();
		}
    }
}