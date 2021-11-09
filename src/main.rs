#[cfg(feature = "trace")]
#[macro_use]
extern crate trace;

use std::collections::HashMap;
use std::env;
use std::fs::File;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use exitcode::ExitCode;
use getopts::Options;

use parser::RunscriptSource;
use termcolor::{ColorChoice, ColorSpec, StandardStream, WriteColor};

mod exec;
mod out;
mod parser;
mod script;

use script::{Runscript, Script, ScriptPhase};

use crate::exec::{exec_script, ExecConfig};
use crate::parser::{ParsingContext, RunscriptLocation};

#[cfg(feature = "trace")]
trace::init_depth_var!();

const HELP_TEXT: &str = "\
Usage: run [options] [[file]:[target][:phase] arguments]

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

const VERSION: &str = env!("CARGO_PKG_VERSION");

const PHASES_B: [ScriptPhase; 2] = [ScriptPhase::BuildOnly, ScriptPhase::Build];
const PHASES_T: [ScriptPhase; 3] = [
    ScriptPhase::Build,
    ScriptPhase::BuildAndRun,
    ScriptPhase::Run,
];
const PHASES_R: [ScriptPhase; 2] = [ScriptPhase::Run, ScriptPhase::RunOnly];

fn panic_hook(info: &std::panic::PanicInfo) {
    let mut tries = 0;
    let report_file = loop {
        let uuid = uuid::Uuid::new_v4().to_hyphenated().to_string();
        let file = {
            let mut file = std::env::temp_dir();
            file.push(format!("runscript-report-{}.txt", uuid));
            file
        };
        match File::create(&file) {
            Ok(mut writer) => {
                let _ = write!(
                    writer,
                    "\
Runscript panicked.
Crate version: {}
Operating System: {}

Panic message:
{}
",
                    VERSION,
                    os_info::get(),
                    info,
                );
                break Some(file);
            }
            Err(_) => {
                tries += 1;
                if tries > 5 {
                    break None;
                }
                continue;
            }
        }
    };

    let mut stderr = StandardStream::stderr(ColorChoice::Auto);
    let _ = stderr.set_color(ColorSpec::new().set_fg(Some(termcolor::Color::Red)));
    let _ = writeln!(
        stderr,
        "\
Runscript had a problem and panicked (crashed).
This is a bug, please report it at https://github.com/TheOnlyMrCat/runscript/issues/new{}.

Please include the arguments with which you invoked runscript, and, if possible, the script
you ran it on.
",
        if let Some(file_path) = report_file {
            format!(
                "\nand include the generated report file at `{}`",
                file_path.display()
            )
        } else {
            String::new()
        }
    );
    let _ = stderr.reset();
}

fn main() {
    std::panic::set_hook(Box::new(panic_hook));

    let args = env::args().skip(1).collect::<Vec<String>>();
    if let Ok(current_dir) = env::current_dir() {
        let output = run(
            &args.iter().map(|s| &**s).collect::<Vec<_>>(),
            &current_dir,
            false,
            &HashMap::new(),
        );
        std::process::exit(output);
    } else {
        eprintln!("Current directory doesn't exist!");
        std::process::exit(exitcode::NOINPUT);
    }
}

pub fn run(
    args: &[&str],
    cwd: &Path,
    capture_stdout: bool,
    env_remap: &HashMap<String, String>,
) -> ExitCode {
    let output_stream = Arc::new(StandardStream::stderr(ColorChoice::Auto));

    let mut options = Options::new();

    options.optflag("h", "help", "");
    options.optflag("", "version", "");
    options.optflag("l", "list", "");
    options.optopt("s", "script", "", "");
    options.optflag("b", "build-only", "");
    options.optflag("", "build-and-run", "");
    options.optflag("r", "run-only", "");

    let matches = match options.parse(args) {
        Ok(m) => m,
        Err(x) => {
            out::option_parse_err(&output_stream, x);
            return exitcode::USAGE;
        }
    };

    if matches.opt_present("help") {
        print!("{}", HELP_TEXT); // There's a trailing newline in the string anyway
        return exitcode::OK;
    }

    if matches.opt_present("version") {
        println!("Runscript {}", VERSION);
        print!("{}", VERSION_TEXT);
        return exitcode::OK;
    }

    if let Some(path) = matches.opt_str("script") {
        let path = match Path::new(&path).canonicalize() {
            Ok(path) => path,
            Err(e) => {
                out::file_read_err(&output_stream, e);
                return exitcode::NOINPUT;
            }
        };
        let source = RunscriptSource {
            source: match std::fs::read_to_string(&path) {
                Ok(s) => s,
                Err(e) => {
                    out::file_read_err(&output_stream, e);
                    return exitcode::IOERR;
                }
            },
            base: path.parent().expect("Runfile has no parent!").to_owned(),
            file: path,
            index: Vec::new(),
        };
        let mut context = ParsingContext::new(&source);
        let commands = match parser::parse_commands(&mut context) {
            Ok(commands) => commands,
            Err(e) => {
                //TODO: print error from out:: module
                eprintln!("run: Error parsing command: {}", e);
                return exitcode::DATAERR;
            }
        };

        let exec_cfg = ExecConfig {
            output_stream: Some(output_stream),
            working_directory: &source.base,
            positional_args: matches.free.iter().skip(1).cloned().collect(),
            capture_stdout,
            env_remap,
        };

        exec::exec_script(
            &Script {
                commands,
                location: RunscriptLocation::default(),
            },
            &exec_cfg,
        )
        .unwrap()
        .status
        .code()
        .unwrap()
    } else {
        exec_runscript(matches, output_stream, cwd, capture_stdout, env_remap)
    }
}

fn exec_runscript(
    matches: getopts::Matches,
    output_stream: Arc<StandardStream>,
    cwd: &Path,
    capture_stdout: bool,
    env_remap: &HashMap<String, String>,
) -> ExitCode {
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
    let b_pos = matches
        .opt_positions("build-only")
        .iter()
        .fold(-1, |acc, &x| if x as i32 > acc { x as i32 } else { acc });
    let t_pos = matches
        .opt_positions("build-and-run")
        .iter()
        .fold(-1, |acc, &x| if x as i32 > acc { x as i32 } else { acc });
    let r_pos = matches
        .opt_positions("run-only")
        .iter()
        .fold(-1, |acc, &x| if x as i32 > acc { x as i32 } else { acc });
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
                return exitcode::USAGE;
            }
        }
    } else if b_pos + t_pos + r_pos == -3 || t_pos > b_pos && t_pos > r_pos {
        phases = &PHASES_T;
    } else if b_pos > t_pos && b_pos > r_pos {
        phases = &PHASES_B;
    } else if r_pos > t_pos && r_pos > b_pos {
        phases = &PHASES_R;
    } else {
        panic!(
            "Failed to identify script phases to run; b_pos={},t_pos={},r_pos={}",
            b_pos, t_pos, r_pos
        )
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
            out::no_runfile_err(&output_stream);
            return exitcode::NOINPUT;
        }
    };
    let runfile_cwd = runfile_path
        .parent()
        .expect("Expected runfile to have parent");
    match parser::parse_runscript(parser::RunscriptSource {
        file: runfile_path.clone(),
        base: runfile_cwd.to_owned(),
        index: vec![],
        source: runfile_source,
    }) {
        Ok(rf) => {
            if matches.opt_present("list") {
                fn list_scripts_for(
                    lock: &mut termcolor::StandardStreamLock,
                    name_length: usize,
                    runscript: &Runscript,
                ) {
                    fn print_phase_list(
                        lock: &mut termcolor::StandardStreamLock,
                        name: &str,
                        name_length: usize,
                        target: &enum_map::EnumMap<ScriptPhase, Option<Script>>,
                    ) {
                        write!(
                            lock,
                            "{0:1$} ",
                            if name.is_empty() { "default" } else { name },
                            name_length
                        )
                        .expect("Failed to write");
                        for (phase, opt) in target.iter() {
                            lock.set_color(
                                ColorSpec::new()
                                    .set_bold(opt.is_some())
                                    .set_intense(opt.is_some())
                                    .set_fg(Some(out::phase_color(phase))),
                            )
                            .expect("Failed to set colour");
                            if opt.is_some() {
                                write!(
                                    lock,
                                    "{}",
                                    match phase {
                                        ScriptPhase::BuildOnly => "B",
                                        ScriptPhase::Build => "b",
                                        ScriptPhase::BuildAndRun => "&",
                                        ScriptPhase::Run => "r",
                                        ScriptPhase::RunOnly => "R",
                                    }
                                )
                                .expect("Failed to write");
                            } else {
                                write!(lock, ".").expect("Failed to write");
                            }
                        }
                        lock.reset().expect("Failed to reset colour");
                        writeln!(lock).expect("Failed to write");
                    }

                    if let Some(default_script) = runscript.scripts.targets.get("") {
                        print_phase_list(lock, "default", name_length, default_script);
                    }

                    for (target, map) in runscript
                        .scripts
                        .targets
                        .iter()
                        .filter(|(target, _)| !target.is_empty())
                    {
                        print_phase_list(lock, target, name_length, map);
                    }
                }

                let mut longest_target = rf
                    .scripts
                    .targets
                    .keys()
                    .map(|s| {
                        if s.is_empty() {
                            "default".len()
                        } else {
                            s.len()
                        }
                    })
                    .max()
                    .unwrap_or(0);
                for include in &rf.includes {
                    longest_target = std::cmp::max(
                        longest_target,
                        include
                            .runscript
                            .scripts
                            .targets
                            .keys()
                            .map(|s| {
                                if s.is_empty() {
                                    "default".len()
                                } else {
                                    s.len()
                                }
                            })
                            .max()
                            .unwrap_or(0),
                    );
                }

                let mut lock = output_stream.lock();

                list_scripts_for(&mut lock, longest_target, &rf);

                fn recursive_list_includes(
                    lock: &mut termcolor::StandardStreamLock,
                    name_length: usize,
                    runscript: &Runscript,
                ) {
                    for include in &runscript.includes {
                        writeln!(lock).expect("Failed to write");
                        writeln!(lock, "From {}:", include.runscript.name)
                            .expect("Failed to write");
                        list_scripts_for(lock, name_length, &include.runscript);
                        recursive_list_includes(lock, name_length, &include.runscript);
                    }
                }

                recursive_list_includes(&mut lock, longest_target, &rf);

                return exitcode::OK;
            }

            let exec_cfg = ExecConfig {
                output_stream: Some(output_stream.clone()),
                working_directory: &runfile_cwd.to_owned(),
                positional_args: matches.free.iter().skip(1).cloned().collect(),
                capture_stdout,
                env_remap,
            };

            match rf.get_target(run_target.as_deref().unwrap_or("")) {
                Some(target) => {
                    let scripts = phases
                        .iter()
                        .filter_map(|&phase| target[phase].as_ref().map(|target| (target, phase)))
                        .collect::<Vec<_>>();
                    if scripts.is_empty() {
                        out::bad_script_phase(&output_stream);
                    }

                    for (script, phase) in scripts {
                        out::phase_message(
                            &output_stream,
                            phase,
                            run_target.as_deref().unwrap_or("default"),
                        );
                        exec_script(script, &exec_cfg).unwrap();
                    }

                    exitcode::OK
                }
                None => {
                    match run_target {
                        Some(name) => out::bad_target(&output_stream, &name),
                        None => out::bad_default(&output_stream),
                    }
                    exitcode::NOINPUT
                }
            }
        }
        Err(e) => {
            out::file_parse_err(&output_stream, e);
            exitcode::DATAERR
        }
    }
}
