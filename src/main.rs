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
use exec::BaseExecContext;
use exitcode::ExitCode;

use parser::RunscriptSource;
use termcolor::{ColorChoice, ColorSpec, StandardStream, WriteColor};

mod exec;
mod out;
mod parser;
mod old_parser;
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
    let _ = writeln!(stderr, "Runscript had a problem and panicked (crashed).");
    let _ = stderr.reset();
    let _ = writeln!(
        stderr,
        "\
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
    let mut context = BaseExecContext {
        old_format: false,
        args: env::args().skip(1).collect::<Vec<String>>(),
        current_file: None,
    };

    loop {
        match std::panic::catch_unwind(|| {
            let output = run(context);
            std::process::exit(output);
        }) {
            Ok(_) => unreachable!(),
            Err(panic) => match panic.downcast::<BaseExecContext>() {
                Ok(new_context) => {
                    context = *new_context;
                }
                Err(payload) => {
                    std::panic::resume_unwind(payload);
                }
            },
        }
    }
}

pub fn run(context: BaseExecContext) -> ExitCode {
    let mut app = clap::App::new("run")
        .about("Project script manager and executor")
        .version(VERSION)
        .author("TheOnlyMrCat")
        .setting(clap::AppSettings::NoBinaryName)
        .setting(clap::AppSettings::TrailingVarArg)
        .setting(clap::AppSettings::DeriveDisplayOrder)
        .override_usage("run [OPTIONS] [TARGET:PHASE] [--] [ARGS]")
        .arg(arg!([target] "Target to run in the script").hide(true))
        .arg(arg!([args] ... "Arguments to pass to the script").hide(true))
        .arg(
            arg!(-c --command <COMMAND> "Execute a command")
                .required(false)
                .value_hint(ValueHint::CommandString)
                .conflicts_with_all(&["file", "old-format", "list", "target", "args", "build", "run", "test"])
        )
        .arg(arg!(-f --file <FILE> "Explicitly specify a script file to run").required(false).value_hint(ValueHint::FilePath))
        .arg(arg!(-'1' --"old-format" "Use the old format to parse the script file"))
        .arg(arg!(-l --list "List targets in the script file").conflicts_with_all(&["build", "run", "test"]))
        .arg(arg!(--color <WHEN>).possible_values(&["auto", "always", "ansi", "never"]).required(false).default_value("auto"))
        .arg(arg!(-b --build "Shorthand for `--phase build`").conflicts_with_all(&["run", "test"]))
        .arg(arg!(-r --run "Shorthand for `--phase run`").conflicts_with_all(&["test"]))
        .arg(arg!(-t --test "Shorthand for `--phase test`"));

    let options = match app.try_get_matches_from_mut(context.args) {
        Ok(m) => m,
        Err(clap::Error { kind: clap::ErrorKind::DisplayHelp, .. }) => {
            app.print_help().expect("Failed to print clap help");
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
            config.set_default("colors.phases.run", 13).unwrap();
            config.set_default("colors.phases.test", 7).unwrap();
        } else {
            config.set_default("colors.phases.build", 1).unwrap();
            config.set_default("colors.phases.run", 4).unwrap();
            config.set_default("colors.phases.test", 2).unwrap();
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
                config_dir = None;
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

    let output_stream = Arc::new(StandardStream::stderr(ColorChoice::Auto));

    if !config.get_bool("dev.panic").unwrap_or(false) {
        std::panic::set_hook(Box::new(panic_hook));
    }
    
    let cwd = match env::current_dir() {
        Ok(cwd) => cwd,
        Err(e) => {
            out::dir_read_err(&output_stream, e);
            return exitcode::NOINPUT;
        }
    };

    if let Some(command) = options.value_of("command") {
        match parser::parse_command(command) {
            Ok(command) => {
                let script = Script {
                    commands: vec![command],
                    location: parser::RunscriptLocation { line: 0, column: 0 },
                };
                let exec_cfg = ExecConfig {
                    output_stream: Some(output_stream),
                    working_directory: &cwd,
                    script_path: None,
                    positional_args: options.values_of("args").into_iter().flatten().map(ToOwned::to_owned).collect(),
                    old_format: false,
                };

                exec_script(&script, &exec_cfg).unwrap();
                exitcode::OK
            }
            Err(e) => {
                let mut lock = output_stream.lock();
                writeln!(lock, "{}", e).unwrap();
                exitcode::DATAERR
            }
        }
    } else {
        let runfile = match select_file(&options, &config, &context.current_file, &cwd, &output_stream) {
            Ok(runfile) => runfile,
            Err(code) => return code,
        };

        match if options.is_present("old-format") || context.old_format { 
            old_parser::parse_runscript(runfile.clone()).map_err(|e| {
                parser::RunscriptParseError {
                    script: e.script,
                    data: e.data.into(),
                }
            })
        } else {
            parser::parse_runscript(runfile.clone())
        } {
            Ok(rf) => {
                if options.is_present("list") {
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
                    script_path: Some(runfile.path),
                    positional_args: options.values_of("args").into_iter().flatten().map(ToOwned::to_owned).collect(),
                    old_format: options.is_present("old-format") || context.old_format,
                };

                let target = options.value_of("target").unwrap_or("");
                let (target, phase) = target.split_once(':').unwrap_or_else(|| {
                    if options.is_present("build") {
                        (target, "build")
                    } else if options.is_present("test") {
                        (target, "test")
                    } else {
                        (target, "run")
                    }
                });

                match if target.is_empty() {
                    rf.get_default_target()
                } else {
                    rf.get_target(target)
                } {
                    Some((name, scripts)) => {
                        if let Some(script) = scripts.get(phase) {
                            out::phase_message(
                                &output_stream,
                                &config,
                                phase,
                                name,
                            );
                            exec_script(script, &exec_cfg).unwrap();
                            exitcode::OK
                        } else {
                            out::bad_script_phase(&output_stream);
                            exitcode::NOINPUT
                        }
                    }
                    None => {
                        if target.is_empty() {
                            out::bad_default(&output_stream);
                        } else {
                            out::bad_target(&output_stream, target);
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
}

fn select_file(options: &ArgMatches, config: &Config, context_file: &Option<PathBuf>, cwd: &Path, output_stream: &StandardStream) -> Result<RunscriptSource, ExitCode> {
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
            match context_file {
                Some(file) => {
                    let source = std::fs::read_to_string(&file).map_err(|err| {
                        out::file_read_err(output_stream, err);
                        exitcode::NOINPUT
                    })?;
                    let dir = file.parent().unwrap_or_else(|| Path::new("")).to_owned();
                    Ok(RunscriptSource { path: file.clone(), source, dir })
                }
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
    }
}

fn list_scripts_for(
    lock: &mut termcolor::StandardStreamLock,
    config: &Config,
    name_length: usize,
    runscript: &Runscript,
) {
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
    //TODO: This could be configured
    if target.contains_key("build") {
        lock.set_color(
            ColorSpec::new()
                .set_bold(true)
                .set_intense(true)
                .set_fg(Some(out::phase_color(config, "build"))),
        )
        .expect("Failed to set colour");
        write!(lock, "B").unwrap();
    } else {
        lock.set_color(
            ColorSpec::new()
                .set_bold(false)
                .set_intense(false)
                .set_fg(Some(out::phase_color(config, "build"))),
        )
        .expect("Failed to set colour");
        write!(lock, ".").unwrap();
    }
    if target.contains_key("run") {
        lock.set_color(
            ColorSpec::new()
                .set_bold(true)
                .set_intense(true)
                .set_fg(Some(out::phase_color(config, "run"))),
        )
        .expect("Failed to set colour");
        write!(lock, "R").unwrap();
    } else {
        lock.set_color(
            ColorSpec::new()
                .set_bold(false)
                .set_intense(false)
                .set_fg(Some(out::phase_color(config, "run"))),
        )
        .expect("Failed to set colour");
        write!(lock, ".").unwrap();
    }
    if target.contains_key("test") {
        lock.set_color(
            ColorSpec::new()
                .set_bold(true)
                .set_intense(true)
                .set_fg(Some(out::phase_color(config, "test"))),
        )
        .expect("Failed to set colour");
        write!(lock, "T").unwrap();
    } else {
        lock.set_color(
            ColorSpec::new()
                .set_bold(false)
                .set_intense(false)
                .set_fg(Some(out::phase_color(config, "test"))),
        )
        .expect("Failed to set colour");
        write!(lock, ".").unwrap();
    }
    
    for phase in target.keys() {
        if phase == "test" || phase == "build" || phase == "run" {
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