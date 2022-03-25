#![warn(clippy::print_stdout)]
#![warn(clippy::print_stderr)]

use std::env;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use clap::{arg, ArgMatches, ValueHint};
use config::Config;
use exec::BaseExecContext;
use exitcode::ExitCode;

use parser::RunscriptSource;
use termcolor::{ColorChoice, ColorSpec, StandardStream, WriteColor};

mod config;
mod exec;
mod out;
mod parser;
mod process;
mod script;

use script::{Overrideable, Runscript, Target};

use crate::exec::{ExecConfig, ShellContext};

const VERSION_TEXT: &str = "\
Written by TheOnlyMrCat
Source code available at https://github.com/TheOnlyMrCat/runscript
";

const VERSION: &str = env!("CARGO_PKG_VERSION");

#[cfg(not(feature = "dev-panic"))]
fn panic_hook(info: &std::panic::PanicInfo) {
    use std::fs::File;

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
    #[cfg(not(feature = "dev-panic"))]
    {
        std::panic::set_hook(Box::new(panic_hook));
    }

    let mut context = BaseExecContext {
        args: env::args().skip(1).collect::<Vec<String>>(),
        current_file: None,
        current_target: None,
        colour_choice: ColorChoice::Auto,
    };

    loop {
        // This flagrant abuse of unwinding is the best way I can think of to have
        // the process `fork(2)` itself and get back to `main()` as cleanly as possible.
        // Speaking of which, it's probably wildly unsafe to do so. I might switch to using
        // `clone(2)` on Linux, and simply `Command::spawn` on everything else.
        match std::panic::catch_unwind(|| run(context)) {
            Ok(output) => std::process::exit(output),
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
    let mut app = clap::Command::new("run")
        .about("Project script manager and executor")
        .version(VERSION)
        .author("TheOnlyMrCat")
        .no_binary_name(true)
        .trailing_var_arg(true)
        .setting(clap::AppSettings::DeriveDisplayOrder)
        .override_usage("run [OPTIONS] [TARGET:PHASE] [--] [ARGS]")
        .arg(arg!([target] "Target to run in the script").hide(true))
        .arg(arg!([args] ... "Arguments to pass to the script").hide(true))
        .arg(
            arg!(-c --command <COMMAND> "Execute a command")
                .required(false)
                .value_hint(ValueHint::CommandString)
                .conflicts_with_all(&["script", "file", "list", "target", "build", "run", "test"]),
        )
        .arg(
            arg!(-s --script <FILE> "Run a shell script")
                .required(false)
                .value_hint(ValueHint::FilePath)
                .conflicts_with_all(&["file", "list", "target", "build", "run", "test"]),
        )
        .arg(
            arg!(-f --file <FILE> "Run a runscript file")
                .required(false)
                .value_hint(ValueHint::FilePath),
        )
        .arg(
            arg!(-l --list "List targets in the script file")
                .conflicts_with_all(&["build", "run", "test"]),
        )
        .arg(
            arg!(--color <WHEN>)
                .possible_values(&["auto", "always", "ansi", "never"])
                .required(false)
                .default_value("auto"),
        )
        .arg(arg!(-b --build "Shorthand for `:build`").conflicts_with_all(&["run", "test"]))
        .arg(arg!(-r --run "Shorthand for `:run`").conflicts_with_all(&["test"]))
        .arg(arg!(-t --test "Shorthand for `:test`"));

    let options = match app.try_get_matches_from_mut(context.args) {
        Ok(m) => m,
        Err(err) => {
            match err.kind() {
                clap::ErrorKind::DisplayHelp => {
                    app.print_help().expect("Failed to print clap help");
                    return exitcode::OK;
                }
                #[allow(clippy::print_stdout)]
                clap::ErrorKind::DisplayVersion => {
                    print!("Runscript {}", VERSION);
                    for feature in [
                        (cfg!(feature = "old-parser"), "old-parser"),
                        (cfg!(feature = "dev-panic"), "dev-panic"),
                    ]
                    .into_iter()
                    .filter_map(|(enabled, feature)| enabled.then(|| feature))
                    {
                        print!(" +{}", feature);
                    }
                    println!();
                    print!("{}", VERSION_TEXT); // VERSION_TEXT contains a newline already
                    return exitcode::OK;
                }
                _ => {
                    err.print().expect("Failed to write clap error");
                    return exitcode::USAGE;
                }
            }
        }
    };

    let colour_choice = match options.value_of("color") {
        Some("always") => ColorChoice::Always,
        Some("never") => ColorChoice::Never,
        Some("ansi") => ColorChoice::AlwaysAnsi,
        Some("auto") => context.colour_choice,
        _ => unreachable!(),
    };
    let output_stream = Arc::new(StandardStream::stderr(colour_choice));

    let config = {
        // Runscript config file is at $RUNSCRIPT_CONFIG_DIR/config.toml, $XDG_CONFIG_HOME/runscript/config.toml,
        // or $HOME/.config/runscript/config.toml on unix.
        // It is at $RUNSCRIPT_CONFIG_DIR\config.toml or {FOLDERID_LocalAppData}\runscript\config.toml on Windows.
        let config_dir: Option<PathBuf>;
        if let Some(dir) = env::var_os("RUNSCRIPT_CONFIG_DIR") {
            config_dir = Some(dir.into());
        } else {
            #[cfg(unix)]
            {
                if let Some(mut dir) = env::var_os("XDG_CONFIG_HOME") {
                    dir.push("/runscript");
                    config_dir = Some(dir.into());
                } else if let Some(mut dir) = env::var_os("HOME") {
                    dir.push("/.config/runscript");
                    config_dir = Some(dir.into());
                } else {
                    config_dir = None;
                }
            }
            #[cfg(windows)]
            unsafe {
                use std::ffi::OsString;
                use std::os::windows::ffi::OsStringExt;

                use winapi::shared::winerror;
                use winapi::um::{combaseapi, knownfolders, shlobj, shtypes, winbase, winnt};

                let mut path_ptr: winnt::PWSTR = std::ptr::null_mut();
                let result = shlobj::SHGetKnownFolderPath(
                    &knownfolders::FOLDERID_LocalAppData,
                    0,
                    std::ptr::null_mut(),
                    &mut path_ptr,
                );
                if result == winerror::S_OK {
                    let len = winbase::lstrlenW(path_ptr) as usize;
                    let path = std::slice::from_raw_parts(path_ptr, len);
                    let ostr: OsString = OsStringExt::from_wide(path);
                    combaseapi::CoTaskMemFree(path_ptr as *mut winapi::ctypes::c_void);
                    config_dir = Some(Path::new(&ostr).join("runscript"));
                } else {
                    combaseapi::CoTaskMemFree(path_ptr as *mut winapi::ctypes::c_void);
                    config_dir = None;
                }
            }
        }

        if let Some(config_dir) = config_dir {
            let config_file = config_dir.join("config.toml");
            if let Ok(contents) = std::fs::read_to_string(&config_file) {
                match toml::from_str(&contents) {
                    Ok(config) => config,
                    Err(e) => {
                        //TODO: Move "Warning message" into `out.rs`
                        let mut lock = output_stream.lock();
                        let _ =
                            lock.set_color(ColorSpec::new().set_fg(Some(termcolor::Color::Yellow)));
                        let _ = write!(lock, "Warning:");
                        let _ = lock.reset();
                        let _ = writeln!(
                            lock,
                            " Failed to parse config file at `{}`",
                            config_file.display(),
                        );
                        let _ = writeln!(lock, "{}", e);
                        Config::default()
                    }
                }
            } else {
                Config::default()
            }
        } else {
            Config::default()
        }
    };

    #[cfg(not(feature = "dev-panic"))]
    if config.dev.panic {
        let _ = std::panic::take_hook();
    } else {
        // Capture a short backtrace by default, unless RUST_BACKTRACE is already set
        if env::var_os("RUST_BACKTRACE").is_none() {
            env::set_var("RUST_BACKTRACE", "1");
        }
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
                let exec_cfg = ExecConfig {
                    output_stream: None,
                    colour_choice,
                    working_directory: &cwd,
                    script_path: None,
                    target_name: None,
                    positional_args: options
                        .values_of("args")
                        .into_iter()
                        .flatten()
                        .map(ToOwned::to_owned)
                        .collect(),
                };
                let mut shell_context = ShellContext::new(&exec_cfg);
                shell_context
                    .exec_command_group(&[command], &exec_cfg)
                    .unwrap() //TODO: Handle errors here
                    .wait()
                    .status
                    .coerced_code()
            }
            Err(e) => {
                let mut lock = output_stream.lock();
                writeln!(lock, "{}", e).unwrap();
                exitcode::DATAERR
            }
        }
    } else if let Some(script) = options.value_of("script") {
        let script_path = PathBuf::from(script);

        let script_source = match std::fs::read_to_string(&script_path) {
            Ok(script_source) => script_source,
            Err(e) => {
                out::file_read_err(&output_stream, e);
                return exitcode::NOINPUT;
            }
        };

        let script = match parser::parse_shell(RunscriptSource {
            dir: script_path.parent().unwrap_or(&cwd).to_path_buf(),
            path: script_path.clone(),
            source: script_source,
        }) {
            Ok(script) => script,
            Err(e) => {
                out::file_parse_err(&output_stream, e);
                return exitcode::DATAERR;
            }
        };

        let exec_cfg = ExecConfig {
            output_stream: Some(output_stream),
            colour_choice,
            working_directory: &cwd,
            script_path: Some(script_path),
            target_name: options.value_of("target"),
            positional_args: options
                .values_of("args")
                .into_iter()
                .flatten()
                .map(ToOwned::to_owned)
                .collect(),
        };

        let mut shell_context = ShellContext::new(&exec_cfg);
        let status = shell_context
            .exec_command_group(&script, &exec_cfg)
            .unwrap() //TODO: Handle errors here
            .wait()
            .status;
        out::process_finish(&status);
        status.coerced_code()
    } else {
        let runfile = match select_file(
            &options,
            &config,
            &context.current_file,
            &cwd,
            &output_stream,
        ) {
            Ok(runfile) => runfile,
            Err(code) => return code,
        };

        match {
            #[cfg(feature = "old-parser")]
            {
                match parser::parse_runscript(runfile.clone()) {
                    Ok(rf) => Ok(rf),
                    Err(e) => parser::old::parse_runscript(runfile.clone())
                        .map(|rf| {
                            let mut lock = output_stream.lock();
                            lock.set_color(
                                ColorSpec::new()
                                    .set_fg(Some(termcolor::Color::Yellow))
                                    .set_bold(true),
                            )
                            .unwrap();
                            write!(lock, "Warning:").unwrap();
                            lock.reset().unwrap();
                            writeln!(
                                lock,
                                " Using old parser to parse `{}`",
                                runfile
                                    .path
                                    .strip_prefix(&cwd)
                                    .unwrap_or(&runfile.path)
                                    .display()
                            )
                            .unwrap();
                            rf
                        })
                        .map_err(|()| e),
                }
            }
            #[cfg(not(feature = "old-parser"))]
            {
                parser::parse_runscript(runfile.clone())
            }
        } {
            Ok(rf) => {
                if options.is_present("list") {
                    let longest_target = rf
                        .scripts
                        .keys()
                        .map(|s| match s.as_str() {
                            "" => "(blank)".len(),
                            s => s.len(),
                        })
                        .max()
                        .unwrap_or(0);

                    let mut lock = output_stream.lock();

                    list_scripts_for(&mut lock, &config, longest_target, &rf);

                    return exitcode::OK;
                }

                let target = options.value_of("target").unwrap_or("");
                let (target, phase) = target
                    .split_once(':')
                    .map(|(target, phase)| (target, Some(phase)))
                    .unwrap_or_else(|| {
                        if options.is_present("build") {
                            (target, Some("build"))
                        } else if options.is_present("run") {
                            (target, Some("run"))
                        } else if options.is_present("test") {
                            (target, Some("test"))
                        } else {
                            (target, None)
                        }
                    });

                match if target.is_empty() {
                    match context.current_target {
                        Some(ref target) => rf.get_target(target),
                        None => rf.get_default_target(),
                    }
                } else {
                    rf.get_target(target)
                } {
                    Some((name, target)) => {
                        let exec_cfg = ExecConfig {
                            output_stream: Some(output_stream.clone()),
                            colour_choice,
                            working_directory: &runfile.dir,
                            script_path: Some(runfile.path),
                            target_name: Some(name),
                            positional_args: options
                                .values_of("args")
                                .into_iter()
                                .flatten()
                                .map(ToOwned::to_owned)
                                .collect(),
                        };

                        let phases = match phase {
                            Some(phase) => vec![phase.to_string()],
                            None => match target.options.default_phase {
                                Overrideable::Set(ref phases) => phases.clone(),
                                Overrideable::Unset => vec!["run".to_string()],
                                Overrideable::SetNone => {
                                    out::bad_script_phase(&output_stream);
                                    return exitcode::NOINPUT;
                                }
                            },
                        };

                        let mut exit_code = exitcode::OK;
                        for phase in phases {
                            if let Some(script) = target.scripts.get(&phase) {
                                out::phase_message(&output_stream, &config, &phase, name);
                                let mut shell_context = ShellContext::new(&exec_cfg);
                                let status = shell_context
                                    .exec_command_group(
                                        &script
                                            .commands
                                            .clone() //TODO: Don't clone
                                            .into_iter()
                                            .map(|sc| sc.command)
                                            .collect::<Vec<_>>(),
                                        &exec_cfg,
                                    )
                                    .unwrap() //TODO: Handle errors here
                                    .wait()
                                    .status;
                                out::process_finish(&status);
                                exit_code = status.coerced_code();
                                if exitcode::is_error(exit_code) {
                                    break;
                                }
                            } else {
                                //TODO: Ensure all phases exist prior to running any of them
                                out::bad_script_phase(&output_stream);
                                exit_code = exitcode::NOINPUT;
                                break;
                            }
                        }
                        exit_code
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

fn select_file(
    options: &ArgMatches,
    config: &Config,
    context_file: &Option<PathBuf>,
    cwd: &Path,
    output_stream: &StandardStream,
) -> Result<RunscriptSource, ExitCode> {
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
        }
        None => match context_file {
            Some(file) => {
                let source = std::fs::read_to_string(&file).map_err(|err| {
                    out::file_read_err(output_stream, err);
                    exitcode::NOINPUT
                })?;
                let dir = file.parent().unwrap_or_else(|| Path::new("")).to_owned();
                Ok(RunscriptSource {
                    path: file.clone(),
                    source,
                    dir,
                })
            }
            None => {
                for path in cwd.ancestors() {
                    for file in &config.file.names {
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
        },
    }
}

fn list_scripts_for(
    lock: &mut termcolor::StandardStreamLock,
    config: &Config,
    name_length: usize,
    runscript: &Runscript,
) {
    for (target, map) in runscript.scripts.iter().map(|(target, map)| {
        if target.is_empty() {
            ("(blank)", map)
        } else {
            (target.as_ref(), map)
        }
    }) {
        print_phase_list(lock, config, target, name_length, map);
    }
}

fn print_phase_list(
    lock: &mut termcolor::StandardStreamLock,
    config: &Config,
    name: &str,
    name_length: usize,
    target: &Target,
) {
    write!(
        lock,
        "{0:1$} ",
        if name.is_empty() { "default" } else { name },
        name_length
    )
    .expect("Failed to write");
    //TODO: This could be configured
    if target.scripts.contains_key("build") {
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
    if target.scripts.contains_key("run") {
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
    if target.scripts.contains_key("test") {
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

    for phase in target.scripts.keys() {
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
        write!(lock, " {}", phase).expect("Failed to write");
    }
    lock.reset().expect("Failed to reset colour");
    writeln!(lock).expect("Failed to write");
}
