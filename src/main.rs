#[macro_use] extern crate enum_map;

#[cfg(feature="trace")]
#[macro_use]
extern crate trace;

use std::env;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::rc::Rc;

use getopts::Options;

use termcolor::{StandardStream, ColorChoice, WriteColor, ColorSpec, Color};

mod out;
mod exec;
mod parser;
mod script;

use exec::Verbosity;
use script::{Runscript, Script, ScriptPhase};

#[cfg(feature="trace")]
trace::init_depth_var!();

const VERSION: &str = env!("CARGO_PKG_VERSION");

const PHASES_B: [ScriptPhase; 2] = [ScriptPhase::BuildOnly, ScriptPhase::Build];
const PHASES_T: [ScriptPhase; 3] = [ScriptPhase::Build, ScriptPhase::BuildAndRun, ScriptPhase::Run];
const PHASES_R: [ScriptPhase; 2] = [ScriptPhase::Run, ScriptPhase::RunOnly];

fn main() {
	let args = env::args().skip(1).collect::<Vec<String>>();
    if let (false, _) = run(&args.iter().map(|s| &**s).collect::<Vec<_>>(), &env::current_dir().expect("Working environment is not sane"), Verbosity::Normal, false) {
		std::process::exit(1);
    }
}

pub fn run(args: &[&str], cwd: &Path, inherit_verbosity: Verbosity, capture_stdout: bool) -> (bool, Vec<u8>) {
	let output_stream = Rc::new(StandardStream::stderr(ColorChoice::Auto));
	
    let mut options = Options::new();

    options.optflag("h", "help", "Show this very helpful text");
    options.optflag("", "version", "Print version information");
	options.optflagmulti("q", "quiet", "Passed once: Do not show output of run commands. Twice: Produce no output");
	options.optflag("l", "list", "Lists targets and scripts in the target runfile");
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

    let (runfile_source, runfile_path) = match runfile_data {
        Some(r) => r,
        None => {
            out::file_read_err(&output_stream);
            return (false, vec![])
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
							lock.set_color(ColorSpec::new().set_bold(opt.is_some()).set_intense(opt.is_some()).set_fg(Some(match phase {
								ScriptPhase::BuildOnly => Color::Red,
								ScriptPhase::Build => Color::Yellow,
								ScriptPhase::BuildAndRun => Color::Green,
								ScriptPhase::Run => Color::Blue,
								ScriptPhase::RunOnly => Color::Magenta,
							}))).expect("Failed to set colour");
							if opt.is_some() {
								write!(lock, "{}", match phase {
									ScriptPhase::BuildOnly => "B",
									ScriptPhase::Build => "b",
									ScriptPhase::BuildAndRun => "&",
									ScriptPhase::Run => "r",
									ScriptPhase::RunOnly => "R",
								}).expect("Failed to write");
							} else {
								write!(lock, ".");
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

				return (true, vec![]);
			}

			use crate::exec::ExecConfig;
			let exec_config = ExecConfig {
				output_stream: &output_stream,
				verbosity: match matches.opt_count("quiet") {
					0 => Verbosity::Normal,
					1 => Verbosity::Quiet,
					_ => Verbosity::Silent,
				}.max(inherit_verbosity),
				working_directory: runfile_cwd,
				positional_args: matches.free.get(1..).unwrap_or(&[]).to_owned(),
			};

			let mut output_acc = Vec::new();
			//TODO: Instead, find all scripts that would run given the target and phases?
            for &phase in phases {
                if run_target == "" {
					if let Some(script) = rf.get_default_script(phase) {
						out::phase_message(&output_stream, phase, "default");
						let (success, output) = exec::shell(&script.commands, &rf, &exec_config, capture_stdout);
						if capture_stdout {
							output_acc.extend(output.into_iter());
						}
						if !success {
							return (expect_fail, output_acc);
						}
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
        Err(e) => {
            out::file_parse_err(&output_stream, e);
            (false, vec![])
		},
    }
}