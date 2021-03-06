#[cfg(feature="trace")]
#[macro_use]
extern crate trace;

use std::collections::HashMap;
use std::env;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::rc::Rc;

use getopts::Options;

use termcolor::{StandardStream, ColorChoice, WriteColor, ColorSpec};

mod out;
mod exec;
mod parser;
mod script;

use exec::{ProcessExit, ProcessOutput, Verbosity};
use script::{Runscript, Script, ScriptPhase};

#[cfg(feature="trace")]
trace::init_depth_var!();

const HELP_TEXT: &str = "\
Usage: run [options] target

Runscript looks for a file named `run` in the current directory
(or otherwise specified) and interprets that as a runscript.
Syntax documentation is available on the github page:
https://github.com/TheOnlyMrCat/runscript

PHASE SELECTION:
    -b, --build-only: Execute `b!` and `b` scripts
     --build-and-run: Execute `b`, `br`, and `r` scripts (default)
       -r --run-only: Execute `r` and `r!` scripts

OUTPUT:
     -l: List targets and scripts in the target runfile in a colourful fashion
     -q: Do not show output of executed commands
    -qq: Produce no output to stderr either
";

const VERSION_TEXT: &str = "\
Written by TheOnlyMrCat
Source code available at https://github.com/TheOnlyMrCat/runscript
";

const ERROR_TEXT: &str = "\
An unexpected error occurred in runscript.
THIS IS A BUG! Please report this to the developer
(https://github.com/TheOnlyMrCat/runscript/issues)
along with the following payload:
";

const VERSION: &str = env!("CARGO_PKG_VERSION");

const PHASES_B: [ScriptPhase; 2] = [ScriptPhase::BuildOnly, ScriptPhase::Build];
const PHASES_T: [ScriptPhase; 3] = [ScriptPhase::Build, ScriptPhase::BuildAndRun, ScriptPhase::Run];
const PHASES_R: [ScriptPhase; 2] = [ScriptPhase::Run, ScriptPhase::RunOnly];

fn panic_hook(info: &std::panic::PanicInfo) {
	eprint!("{}", ERROR_TEXT); // Trailing newline already in literal
	eprintln!("{}", info);
}

fn main() {
	std::panic::set_hook(Box::new(panic_hook));
	let args = env::args().skip(1).collect::<Vec<String>>();
    if !run(
		&args.iter().map(|s| &**s).collect::<Vec<_>>(),
		&env::current_dir().expect("Working environment is not sane"),
		Verbosity::Normal,
		false,
		&HashMap::new()
	).expect("Definition of run in this file doesn't error").status.success() {
		std::process::exit(1);
    }
}

pub fn run(args: &[&str], cwd: &Path, inherit_verbosity: Verbosity, capture_stdout: bool, env_remap: &HashMap<String, String>) -> Result<ProcessOutput, std::io::Error> {
	let output_stream = Rc::new(StandardStream::stderr(ColorChoice::Auto));
	
    let mut options = Options::new();

    options.optflag("h", "help", "");
    options.optflag("", "version", "");
	options.optflagmulti("q", "quiet", "");
	options.optflag("l", "list", "");
    options.optflag("b", "build-only", "");
    options.optflag("", "build-and-run", "");
    options.optflag("r", "run-only", "");
    options.optflag("x", "expect-fail", "");

    let matches = match options.parse(args) {
        Ok(m) => m,
        Err(x) => {
            out::option_parse_err(&output_stream, x);
            return Ok(ProcessOutput::new(false));
        },
    };

    if matches.opt_present("help") {
		print!("{}", HELP_TEXT); // There's a trailing newline in the string anyway
        return Ok(ProcessOutput::new(true));
    }

    if matches.opt_present("version") {
        println!("Runscript {}", VERSION);
		print!("{}", VERSION_TEXT);
        return Ok(ProcessOutput::new(true));
	}
	
	let expect_fail = matches.opt_present("expect-fail");

    let path_branch_file: String;
    let path_branch_dir: String;
    let run_target: Option<String>;
	let run_phase: Option<String>;
    if let Some(target) = matches.free.get(0) {
        let v = target.split(':').collect::<Vec<&str>>();
        
        if v.len() == 1 {
            path_branch_file = "".to_owned();
            path_branch_dir = "run".to_owned();
            run_target = Some(v[0].to_owned());
			run_phase = None;
        } else if v.len() >= 2 {
            let path_branch = v[0].to_owned();
			if path_branch.is_empty() {
				path_branch_file = "".to_owned();
				path_branch_dir = "run".to_owned();
			} else {
				let mut pbf = path_branch.clone();
				pbf.push_str(".run");
				path_branch_file = pbf;

				let mut pbd = path_branch;
				pbd.push_str("/run");
				path_branch_dir = pbd;
			}

			if v[1].is_empty() {
				run_target = None
			} else {
				run_target = Some(v[1].to_owned());
			}

			if v.len() >= 3 {
				run_phase = Some(v[2].to_owned());
			} else {
				run_phase = None;
			}
		} else {
            path_branch_file = "".to_owned();
            path_branch_dir = "run".to_owned();
            run_target = None;
			run_phase = None;
        }
    } else {
        path_branch_file = "".to_owned();
        path_branch_dir = "run".to_owned();
        run_target = None;
		run_phase = None;
    }

    let b_pos = matches.opt_positions("build-only")   .iter().fold(-1, |acc, &x| if x as i32 > acc { x as i32 } else { acc });
    let t_pos = matches.opt_positions("build-and-run").iter().fold(-1, |acc, &x| if x as i32 > acc { x as i32 } else { acc });
    let r_pos = matches.opt_positions("run-only")     .iter().fold(-1, |acc, &x| if x as i32 > acc { x as i32 } else { acc });

    let phases: &[ScriptPhase];
	if let Some(run_phase) = run_phase {
		phases = match &*run_phase {
			"b!" => &[ScriptPhase::BuildOnly],
			"b" => &[ScriptPhase::Build],
			"br" => &[ScriptPhase::BuildAndRun],
			"r" => &[ScriptPhase::Run],
			"r!" => &[ScriptPhase::RunOnly],
			_ => {
				out::bad_phase_err(&output_stream, &run_phase);
				return Ok(ProcessOutput::new(false));
			}
		}
	} else if b_pos + t_pos + r_pos == -3
    || t_pos > b_pos && t_pos > r_pos {
        phases = &PHASES_T;
    } else if b_pos > t_pos && b_pos > r_pos {
        phases = &PHASES_B;
    } else if r_pos > t_pos && r_pos > b_pos {
        phases = &PHASES_R;
    } else {
        panic!("Failed to identify script phases to run; b_pos={},t_pos={},r_pos={}", b_pos, t_pos, r_pos)
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

    let (runfile_source, runfile_path) = match runfile_data {
        Some(r) => r,
        None => {
            out::file_read_err(&output_stream);
            return Ok(ProcessOutput::new(false))
        }
	};

	let runfile_cwd = runfile_path.parent().expect("Expected runfile to have parent");

    match parser::parse_runscript(parser::RunscriptSource { file: runfile_path.clone(), base: runfile_cwd.to_owned(), index: vec![], source: runfile_source }) {
        Ok(rf) => {
			if matches.opt_present("list") {
				fn list_scripts_for(lock: &mut termcolor::StandardStreamLock, name_length: usize, runscript: &Runscript) {
					fn print_phase_list(lock: &mut termcolor::StandardStreamLock, name: &str, name_length: usize, target: &enum_map::EnumMap<ScriptPhase, Option<Script>>) {
						write!(lock, "{0:1$} ", name, name_length).expect("Failed to write");
						for (phase, opt) in target.iter() {
							lock.set_color(ColorSpec::new().set_bold(opt.is_some()).set_intense(opt.is_some()).set_fg(Some(out::phase_color(phase)))).expect("Failed to set colour");
							if opt.is_some() {
								write!(lock, "{}", match phase {
									ScriptPhase::BuildOnly => "B",
									ScriptPhase::Build => "b",
									ScriptPhase::BuildAndRun => "&",
									ScriptPhase::Run => "r",
									ScriptPhase::RunOnly => "R",
								}).expect("Failed to write");
							} else {
								write!(lock, ".").expect("Failed to write");
							}
						}
						lock.reset().expect("Failed to reset colour");
						writeln!(lock).expect("Failed to write");
					}
					
					if runscript.scripts.default_target.values().any(|opt| opt.is_some()) {
						print_phase_list(lock, "default", name_length, &runscript.scripts.default_target);
					}
					
					if runscript.scripts.global_target.values().any(|opt| opt.is_some()) {
						print_phase_list(lock, "global", name_length, &runscript.scripts.global_target);
					}
					
					for (target, map) in &runscript.scripts.targets {
						print_phase_list(lock, &target, name_length, &map);
					}
				}
				
				let mut longest_target = rf.scripts.targets.keys().map(|s| s.len()).max().unwrap_or(7);
				for include in &rf.includes {
					longest_target = std::cmp::max(longest_target, include.runscript.scripts.targets.keys().map(|s| s.len()).max().unwrap_or(7));
				}

				let mut lock = output_stream.lock();
				
				list_scripts_for(&mut lock, longest_target, &rf);

				fn recursive_list_includes(lock: &mut termcolor::StandardStreamLock, name_length: usize, runscript: &Runscript) {
					for include in &runscript.includes {
						writeln!(lock).expect("Failed to write");
						writeln!(lock, "From {}:", include.runscript.name).expect("Failed to write");
						list_scripts_for(lock, name_length, &include.runscript);
						recursive_list_includes(lock, name_length, &include.runscript);
					}
				}
				
				recursive_list_includes(&mut lock, longest_target, &rf);

				return Ok(ProcessOutput::new(true));
			}

			let passthrough = rf.options.iter().any(|x| x == "default_positionals");

			use crate::exec::ExecConfig;
			let exec_config = ExecConfig {
				verbosity: match matches.opt_count("quiet") {
					0 => Verbosity::Normal,
					1 => Verbosity::Quiet,
					_ => Verbosity::Silent,
				}.max(inherit_verbosity),
				output_stream: Some(output_stream.clone()),
				working_directory: runfile_cwd,
				positional_args: matches.free.get(1..).unwrap_or(&[]).to_owned(),
			    capture_stdout,
			    env_remap,
			};
			//TODO: Don't bother initialising this if passthrough isn't enabled
			let exec_config_passthrough = ExecConfig {
				positional_args: matches.free,
				..exec_config.clone()
			};

			let mut stdout_acc = Vec::new();
			let mut stderr_acc = Vec::new();
			//TODO: Instead, find all scripts that would run given the target and phases?
            for &phase in phases {
				match &rf.get_pre_global_script(phase) {
					Some(script) => {
						out::phase_message(&output_stream, phase, "pre-global");
						match exec::exec_script(&script, &exec_config) {
						    Ok(output) => {
								if capture_stdout {
									stdout_acc.extend(output.stdout.into_iter());
									stderr_acc.extend(output.stderr.into_iter());
								}
								if !output.status.success() {
									return Ok(ProcessOutput { status: ProcessExit::Bool(expect_fail), stdout: stdout_acc, stderr: stderr_acc, });
								}
							}
						    Err((err, entry)) => {
								out::bad_command_err(&output_stream, &entry, &rf, err);
								return Ok(ProcessOutput { status: ProcessExit::Bool(expect_fail), stdout: stdout_acc, stderr: stderr_acc, });
							}
						}
					},
					None => {}
				}
                if let Some(ref run_target) = run_target {
					match rf.get_target(&run_target) {
						Some(target) => match &target[phase] {
							Some(script) => {
								out::phase_message(&output_stream, phase, &run_target);
								match exec::exec_script(&script, &exec_config) {
									Ok(output) => {
										if capture_stdout {
											stdout_acc.extend(output.stdout.into_iter());
											stderr_acc.extend(output.stderr.into_iter());
										}
										if !output.status.success() {
											return Ok(ProcessOutput { status: ProcessExit::Bool(expect_fail), stdout: stdout_acc, stderr: stderr_acc, });
										}
									}
									Err((err, entry)) => {
										out::bad_command_err(&output_stream, &entry, &rf, err);
										return Ok(ProcessOutput { status: ProcessExit::Bool(expect_fail), stdout: stdout_acc, stderr: stderr_acc, });
									}
								}
							},
							None => {}
						},
						None => {
							if passthrough {
								if let Some(script) = rf.get_default_script(phase) {
									out::phase_message(&output_stream, phase, "default");
									match exec::exec_script(&script, &exec_config_passthrough) {
										Ok(output) => {
											if capture_stdout {
												stdout_acc.extend(output.stdout.into_iter());
												stderr_acc.extend(output.stderr.into_iter());
											}
											if !output.status.success() {
												return Ok(ProcessOutput { status: ProcessExit::Bool(expect_fail), stdout: stdout_acc, stderr: stderr_acc, });
											}
										}
										Err((err, entry)) => {
											out::bad_command_err(&output_stream, &entry, &rf, err);
											return Ok(ProcessOutput { status: ProcessExit::Bool(expect_fail), stdout: stdout_acc, stderr: stderr_acc, });
										}
									}
								}
							} else {
								out::bad_target(&output_stream, &run_target);
								return Ok(ProcessOutput { status: ProcessExit::Bool(expect_fail), stdout: stdout_acc, stderr: stderr_acc, });
							}
						}
					}
				} else {
					if let Some(script) = rf.get_default_script(phase) {
						out::phase_message(&output_stream, phase, "default");
						match exec::exec_script(&script, &exec_config) {
						    Ok(output) => {
								if capture_stdout {
									stdout_acc.extend(output.stdout.into_iter());
									stderr_acc.extend(output.stderr.into_iter());
								}
								if !output.status.success() {
									return Ok(ProcessOutput { status: ProcessExit::Bool(expect_fail), stdout: stdout_acc, stderr: stderr_acc, });
								}
							}
						    Err((err, entry)) => {
								out::bad_command_err(&output_stream, &entry, &rf, err);
								return Ok(ProcessOutput { status: ProcessExit::Bool(expect_fail), stdout: stdout_acc, stderr: stderr_acc, });
							}
						}
					}
				}
				match &rf.get_global_script(phase) {
					Some(script) => {
						out::phase_message(&output_stream, phase, "global");
						match exec::exec_script(&script, &exec_config) {
						    Ok(output) => {
								if capture_stdout {
									stdout_acc.extend(output.stdout.into_iter());
									stderr_acc.extend(output.stderr.into_iter());
								}
								if !output.status.success() {
									return Ok(ProcessOutput { status: ProcessExit::Bool(expect_fail), stdout: stdout_acc, stderr: stderr_acc, });
								}
							}
						    Err((err, entry)) => {
								out::bad_command_err(&output_stream, &entry, &rf, err);
								return Ok(ProcessOutput { status: ProcessExit::Bool(expect_fail), stdout: stdout_acc, stderr: stderr_acc, });
							}
						}
					},
					None => {}
				}
            }
			Ok(ProcessOutput { status: ProcessExit::Bool(!expect_fail), stdout: stdout_acc, stderr: stderr_acc, })
        },
        Err(e) => {
            out::file_parse_err(&output_stream, e);
            Ok(ProcessOutput::new(false))
		},
    }
}