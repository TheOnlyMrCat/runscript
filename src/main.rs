#[cfg(feature = "trace")]
#[macro_use]
extern crate trace;

use std::collections::HashMap;
use std::env;
use std::fs::File;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use clap::{arg, ArgMatches, ValueHint};
use config::Config;
use exitcode::ExitCode;

use parser::RunscriptSource;
use termcolor::{ColorChoice, ColorSpec, StandardStream, WriteColor};

mod exec;
mod out;
mod parser;
mod script;

use script::{Runscript, Script};

use crate::exec::{exec_script, ExecConfig};

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

    let mut app = clap::App::new("run")
        .about("Project script manager and executor")
        .version(VERSION)
        .author("TheOnlyMrCat")
        .setting(clap::AppSettings::NoBinaryName)
        .setting(clap::AppSettings::TrailingVarArg)
        .setting(clap::AppSettings::DeriveDisplayOrder)
        .override_usage("run [OPTIONS] [TARGET] [ARGS]")
        .arg(arg!([target] "Target to run in the script").hide(true))
        .arg(arg!([args] ... "Arguments to pass to the script").hide(true))
        .arg(arg!(-f --file <FILE> "Explicitly specify a script file to run").required(false).value_hint(ValueHint::FilePath))
        .arg(arg!(-l --list "List targets in the script file"))
        .help_heading("PHASE SELECTION")
        .arg(
            arg!(-p --phase <PHASE> "Run a specific phase of the script")
                .required(false)
                .use_delimiter(true)
                .default_value_if("build", None, Some("build"))
                .default_value_if("run", None, Some("run"))
                .conflicts_with("list")
        )
        .arg(arg!(-b --build "Shorthand for `--phase build`"))
        .arg(arg!(-r --run "Shorthand for `--phase run`"));

    let options = match app.try_get_matches_from_mut(args) {
        Ok(m) => m,
        Err(clap::Error { kind: clap::ErrorKind::DisplayHelp, .. }) => {
            app.print_help().expect("Failed to write clap help");
            return exitcode::OK;
        }
        Err(clap::Error { kind: clap::ErrorKind::DisplayVersion, .. }) => {
            println!("Runscript {}", VERSION);
            print!("{}", VERSION_TEXT); // VERSION_TEXT contains a newline already
            return exitcode::OK;
        }
        Err(x) => {
            x.print().expect("Failed to write clap error");
            return exitcode::USAGE;
        }
    };

    let config = {
        let mut config = Config::new();
        config.set_default("file.names", vec!["run", ".run"]).unwrap();
        config.set_default("colors.commands.enabled", true).unwrap();
        config.set_default("colors.phases.enabled", true).unwrap();
        if std::env::var_os("\x52\x55\x4E\x53\x43\x52\x49\x50\x54\x5F\x54\x52\x41\x4E\x53").is_some() {
            config.set_default("colors.phases.build", 6).unwrap();
            config.set_default("colors.phases.exec", 7).unwrap();
            config.set_default("colors.phases.run", 13).unwrap();
        } else {
            config.set_default("colors.phases.build", 1).unwrap();
            config.set_default("colors.phases.exec", 2).unwrap();
            config.set_default("colors.phases.run", 4).unwrap();
        }
        config.set_default("dev.panic", false).unwrap();

        // Runscript config file is at $RUNSCRIPT_CONFIG_DIR/config.toml, $XDG_CONFIG_HOME/runscript/config.toml,
        // or $HOME/.config/runscript/config.toml on unix.
        // It is at {FOLDERID_LocalAppData}\runscript\config.toml on Windows.
        let config_dir: Option<PathBuf>;
        if let Some(dir) = env::var_os("RUNSCRIPT_CONFIG_DIR") {
            config_dir = Some(dir.into());
        } else if let Some(mut dir) = env::var_os("XDG_CONFIG_HOME") {
            dir.push("/runscript");
            config_dir = Some(dir.into());
        } else if let Some(mut dir) = env::var_os("HOME") {
            dir.push("/.config/runscript");
            config_dir = Some(dir.into());
        } else {
            #[cfg(unix)]
            {
                config_dir = Some(PathBuf::from("~/.config/runscript"));
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
        if let Some(config_dir) = config_dir {
            let config_file = config_dir.join("config.toml");
            if config_file.exists() {
                let _ = config.merge(config::File::from(config_dir.join("config.toml")));
            }
        }
        let _ = config.merge(config::Environment::with_prefix("RUNSCRIPT").separator("_"));
        config
    };

    if config.get_bool("dev.panic").unwrap_or(false) {
        let _ = std::panic::take_hook();
    }
    
    let cwd = match env::current_dir() {
        Ok(cwd) => cwd,
        Err(e) => {
            out::dir_read_err(&output_stream, e);
            return exitcode::NOINPUT;
        }
    };

    let runfile = match select_file(&options, &config, &cwd, &output_stream) {
        Ok(runfile) => runfile,
        Err(code) => return code,
    };

    match parser::parse_runscript(runfile.clone()) {
        Ok(rf) => {
            if options.is_present("list") {
                fn list_scripts_for(
                    lock: &mut termcolor::StandardStreamLock,
                    config: &Config,
                    name_length: usize,
                    runscript: &Runscript,
                ) {
                    fn print_phase_list(
                        lock: &mut termcolor::StandardStreamLock,
                        config: &Config,
                        name: &str,
                        name_length: usize,
                        target: &HashMap<String, Script>,
                    ) {
                        write!(
                            lock,
                            "{0:1$} ",
                            if name.is_empty() { "default" } else { name },
                            name_length
                        )
                        .expect("Failed to write");
                        if target.contains_key("build") {
                            lock.set_color(
                                ColorSpec::new()
                                    .set_bold(true)
                                    .set_intense(true)
                                    .set_fg(Some(out::phase_color(config, "build"))),
                            )
                            .expect("Failed to set colour");
                            write!(lock, "B");
                        } else {
                            lock.set_color(
                                ColorSpec::new()
                                    .set_bold(false)
                                    .set_intense(false)
                                    .set_fg(Some(out::phase_color(config, "build"))),
                            )
                            .expect("Failed to set colour");
                            write!(lock, ".");
                        }
                        if target.contains_key("exec") {
                            lock.set_color(
                                ColorSpec::new()
                                    .set_bold(true)
                                    .set_intense(true)
                                    .set_fg(Some(out::phase_color(config, "exec"))),
                            )
                            .expect("Failed to set colour");
                            write!(lock, "&");
                        } else {
                            lock.set_color(
                                ColorSpec::new()
                                    .set_bold(false)
                                    .set_intense(false)
                                    .set_fg(Some(out::phase_color(config, "exec"))),
                            )
                            .expect("Failed to set colour");
                            write!(lock, ".");
                        }
                        if target.contains_key("run") {
                            lock.set_color(
                                ColorSpec::new()
                                    .set_bold(true)
                                    .set_intense(true)
                                    .set_fg(Some(out::phase_color(config, "run"))),
                            )
                            .expect("Failed to set colour");
                            write!(lock, "R");
                        } else {
                            lock.set_color(
                                ColorSpec::new()
                                    .set_bold(false)
                                    .set_intense(false)
                                    .set_fg(Some(out::phase_color(config, "run"))),
                            )
                            .expect("Failed to set colour");
                            write!(lock, ".");
                        }
                        
                        for phase in target.keys() {
                            if phase == "exec" || phase == "build" || phase == "run" {
                                continue;
                            }
                            lock.set_color(
                                ColorSpec::new()
                                    .set_bold(true)
                                    .set_intense(true)
                                    .set_fg(Some(out::phase_color(config, phase))),
                            )
                            .expect("Failed to set colour");
                            write!(
                                lock,
                                "{}",
                                phase
                            )
                            .expect("Failed to write");
                        }
                        lock.reset().expect("Failed to reset colour");
                        writeln!(lock).expect("Failed to write");
                    }

                    if let Some(default_script) =
                        runscript.scripts.targets.get("")
                    {
                        print_phase_list(lock, config, "default", name_length, default_script);
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
                        print_phase_list(lock, config, target, name_length, map);
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

                list_scripts_for(&mut lock, &config, longest_target, &rf);

                fn recursive_list_includes(
                    lock: &mut termcolor::StandardStreamLock,
                    config: &Config,
                    name_length: usize,
                    runscript: &Runscript,
                ) {
                    for include in &runscript.includes {
                        writeln!(lock).expect("Failed to write");
                        writeln!(lock, "From {}:", include.runscript.name)
                            .expect("Failed to write");
                        list_scripts_for(lock, config, name_length, &include.runscript);
                        recursive_list_includes(lock, config, name_length, &include.runscript);
                    }
                }

                recursive_list_includes(&mut lock, &config, longest_target, &rf);

                return exitcode::OK;
            }

            let exec_cfg = ExecConfig {
                output_stream: Some(output_stream.clone()),
                working_directory: &runfile.dir,
                positional_args: options.values_of("args").into_iter().flatten().map(ToOwned::to_owned).collect(),
            };

            let target = options.value_of("target");

            match match &target {
                Some(target) => rf.get_target(target),
                None => rf.get_target(""),
            } {
                Some(target_scripts) => {
                    let phase = options.value_of("phase").unwrap_or("exec");
                    if let Some(script) = target_scripts.get(phase) {
                        out::phase_message(
                            &output_stream,
                            &config,
                            phase,
                            target.unwrap_or("default"),
                        );
                        exec_script(script, &exec_cfg).unwrap();
                        exitcode::OK
                    } else if phase == "exec" {
                        let build_script = target_scripts.get("build");
                        let run_script = target_scripts.get("run");
                        if build_script.is_none() && run_script.is_none() {
                            out::bad_script_phase(&output_stream);
                            exitcode::NOINPUT
                        } else {
                            if let Some(script) = build_script {
                                out::phase_message(
                                    &output_stream,
                                    &config,
                                    "build",
                                    target.unwrap_or("default"),
                                );
                                exec_script(script, &exec_cfg).unwrap();
                            }
                            if let Some(script) = run_script {
                                out::phase_message(
                                    &output_stream,
                                    &config,
                                    "run",
                                    target.unwrap_or("default"),
                                );
                                exec_script(script, &exec_cfg).unwrap();
                            }
                            exitcode::OK
                        }
                    } else {
                        out::bad_script_phase(&output_stream);
                        exitcode::NOINPUT
                    }
                }
                None => {
                    match target {
                        Some(name) => out::bad_target(&output_stream, name),
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

fn select_file(options: &ArgMatches, config: &Config, cwd: &Path, output_stream: &StandardStream) -> Result<RunscriptSource, ExitCode> {
    match options.value_of("file") {
        Some(file) => {
            let path = std::fs::canonicalize(file).map_err(|err| {
                out::file_read_err(output_stream, err);
                exitcode::NOINPUT
            })?;
            let source = std::fs::read_to_string(&path).map_err(|err| {
                out::file_read_err(output_stream, err);
                exitcode::NOINPUT
            })?;
            let dir = path.parent().unwrap_or_else(|| Path::new("")).to_owned();
            Ok(RunscriptSource { path, source, dir })
        },
        None => {
            let file_names: Vec<String> = config.get("file.names").unwrap();
        
            for path in cwd.ancestors() {
                for file in &file_names {
                    let runfile_path = path.join(file);
                    if let Ok(s) = std::fs::read_to_string(&runfile_path) {
                        return Ok(RunscriptSource {
                            path: runfile_path,
                            source: s,
                            dir: path.to_owned(),
                        });
                    }
                }
            }
            out::no_runfile_err(output_stream);
            Err(exitcode::NOINPUT)
        }
    }
}