#[cfg(feature = "trace")]
#[macro_use]
extern crate trace;

use std::env;
use std::fs::File;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use config::Config;
use exitcode::ExitCode;
use getopts::Options;

use termcolor::{ColorChoice, ColorSpec, StandardStream, WriteColor};

mod exec;
mod out;
mod parser;
mod script;

use script::{Runscript, Script, ScriptPhase};

use crate::exec::{exec_script, ExecConfig};
use crate::parser::RunscriptLocation;

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

    let mut args = env::args().skip(1).collect::<Vec<String>>();
    loop {
        match std::panic::catch_unwind(|| {
            let output = run(&args);
            std::process::exit(output);
        }) {
            Ok(_) => unreachable!(),
            Err(panic) => match panic.downcast::<Vec<String>>() {
                Ok(new_args) => {
                    args = *new_args;
                }
                Err(payload) => {
                    std::panic::resume_unwind(payload);
                }
            },
        }
    }
}

pub fn run(args: &[String]) -> ExitCode {
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

    let config = {
        let mut config = Config::new();
        config.set_default("file.names", vec!["run", "run.local"]).unwrap();

        // Runscript config file is at $RUNSCRIPT_CONFIG_DIR/config.toml, $XDG_CONFIG_HOME/runscript/config.toml,
        // or $HOME/.config/runscript/config.toml on unix.
        // It is at {FOLDERID_LocalAppData}\runscript\config.toml on Windows.
        let config_dir: PathBuf;
        if let Some(dir) = env::var_os("RUNSCRIPT_CONFIG_DIR") {
            config_dir = dir.into();
        } else if let Some(mut dir) = env::var_os("XDG_CONFIG_HOME") {
            dir.push("runscript");
            config_dir = dir.into();
        } else if let Some(mut dir) = env::var_os("HOME") {
            dir.push(".config/runscript");
            config_dir = dir.into();
        } else {
            #[cfg(unix)]
            {
                config_dir = PathBuf::from("~/.config/runscript");
            }
            #[cfg(windows)]
            unsafe {
                use winapi::shared::winerror;
                use winapi::um::{combaseapi, knownfolders, shlobj, shtypes, winbase, winnt};

                let mut path_ptr: winnt::PWSTR = ptr::null_mut();
                let result = shlobj::SHGetKnownFolderPath(&knownfolders::FOLDERID_LocalAppData, 0, ptr::null_mut(), &mut path_ptr);
                if result == winerror::S_OK {
                    let len = winbase::lstrlenW(path_ptr) as usize;
                    let path = slice::from_raw_parts(path_ptr, len);
                    let ostr: OsString = OsStringExt::from_wide(path);
                    combaseapi::CoTaskMemFree(path_ptr as *mut winapi::ctypes::c_void);
                    Some(PathBuf::from(ostr))
                } else {
                    combaseapi::CoTaskMemFree(path_ptr as *mut winapi::ctypes::c_void);
                    None
                }
            }
        }
        // Ignore errors for this merge; the default config will suffice.
        let _ = config.merge(config::File::from(config_dir.join("config.toml")));
        let _ = config.merge(config::Environment::with_prefix("RUNSCRIPT").separator("_"));
        config
    };

    if let Some(path) = matches.opt_str("script") {
        let path = match Path::new(&path).canonicalize() {
            Ok(path) => path,
            Err(e) => {
                out::file_read_err(&output_stream, e);
                return exitcode::NOINPUT;
            }
        };
        let source = match std::fs::read_to_string(&path) {
            Ok(s) => s,
            Err(e) => {
                out::file_read_err(&output_stream, e);
                return exitcode::IOERR;
            }
        };
        let lexer = conch_parser::lexer::Lexer::new(source.chars());
        let mut parser = conch_parser::parse::Parser::<
            _,
            conch_parser::ast::builder::AtomicDefaultBuilder<String>,
        >::new(lexer);
        let commands = match parser.command_group(Default::default()).map(|x| x.commands) {
            Ok(commands) => commands,
            Err(e) => {
                //TODO: print error from out:: module
                eprintln!("run: Error parsing command: {}", e);
                return exitcode::DATAERR;
            }
        };

        let exec_cfg = ExecConfig {
            output_stream: Some(output_stream),
            working_directory: path.parent().expect("Working environment is not sane!"),
            positional_args: matches.free.iter().skip(1).cloned().collect(),
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
        exec_runscript(matches, output_stream, &config)
    }
}

fn exec_runscript(matches: getopts::Matches, output_stream: Arc<StandardStream>, config: &Config) -> ExitCode {
    let cwd = match env::current_dir() {
        Ok(cwd) => cwd,
        Err(e) => {
            out::dir_read_err(&output_stream, e);
            return exitcode::NOINPUT;
        }
    };

    let run_target = matches.free.get(0).map(|x| x.as_str());
    let phases = &[ScriptPhase::BuildAndRun];

    let file_names: Vec<String> = config.get("file.names").unwrap();
    let mut runfile_data: Option<(String, PathBuf)> = None;
    for path in cwd.ancestors() {
        for file in &file_names {
            let runfile_path = path.join(file);
            if let Ok(s) = std::fs::read_to_string(&runfile_path) {
                runfile_data = Some((s, runfile_path));
                break;
            }
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
                                        ScriptPhase::Build => "b",
                                        ScriptPhase::BuildAndRun => "&",
                                        ScriptPhase::Run => "r",
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

                    if let Some(default_script) =
                        runscript.scripts.targets.get("")
                    {
                        print_phase_list(lock, "default", name_length, default_script);
                    }

                    for (target, map) in
                        runscript
                            .scripts
                            .targets
                            .iter()
                            .filter_map(|(target, map)| if target.is_empty() {
                                None
                            } else {
                                Some((target, map))
                            })
                    {
                        print_phase_list(lock, target, name_length, map);
                    }
                }

                let mut longest_target = rf
                    .scripts
                    .targets
                    .keys()
                    .map(|s| match s.as_str() {
                        "" => "default".len(),
                        s => s.len(),
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
                            .map(|s| match s.as_str() {
                                "" => "default".len(),
                                s => s.len(),
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
            };

            match match &run_target {
                Some(target) => rf.get_target(&target),
                None => rf.get_target(""),
            } {
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
