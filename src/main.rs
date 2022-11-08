#![warn(clippy::print_stdout)]

mod config;
mod exec;
mod out;
mod parser;
mod process;
mod ptr;
mod script;

use std::env;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use clap::{Args, CommandFactory, FromArgMatches, Parser, Subcommand, ValueEnum, ValueHint};
use config::Config;
use exec::BaseExecContext;
use exitcode::ExitCode;

use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};

use self::exec::{ExecConfig, ShellContext};
use self::parser::SourceFile;
use self::script::{Overrideable, Runscript, ScriptExecution, Target};

const HELP_TEXT: &str = "\
Project script manager and executor

Usage: run [OPTIONS] [TARGET:PHASE] [-- ARGS ...]
       run -c <COMMAND> [-- ARGS ...]
       run -s <FILE> [-- ARGS ...]

Global Options:
      --color <WHEN>  [default: auto] [possible values: always, ansi, auto, never]
  -h, --help          Print help information
  -V, --version       Print version information

Options:
  -l, --list         List targets in all runscripts found
  -f, --file <FILE>  Execute from <FILE> instead of finding all applicable runscripts
  -b, --build        
  -r, --run          
  -t, --test         
";

const VERSION_TEXT: &str = "\
Written by TheOnlyMrCat
Source code available at https://github.com/TheOnlyMrCat/runscript
";

const VERSION: &str = env!("CARGO_PKG_VERSION");
const FEATURES: &[(&str, bool)] = &[("old-parser", cfg!(feature = "old-parser")), ("panic-hook", cfg!(feature = "panic-hook"))];

#[cfg(feature = "panic-hook")]
fn panic_hook(info: &std::panic::PanicInfo) {
    use std::fs::File;

    let mut tries = 0;
    let report_file = loop {
        let uuid = uuid::Uuid::new_v4();
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
",
                    VERSION,
                    os_info::get(),
                );
                let _ = writeln!(writer, "Enabled features:");
                for feature in FEATURES.iter().filter_map(|(feature, enabled)| enabled.then_some(feature)) {
                    let _ = write!(writer, " {}", feature);
                }
                let _ = writeln!(writer);
                let _ = writeln!(writer, "Panic info:");
                let _ = writeln!(writer, "{}", info);
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
        "This is a bug, please report it to <~theonlymrcat/public-inbox@lists.sr.ht>."
    );    
    let _ = writeln!(stderr);
    if let Some(file_path) = report_file {
        let _ = writeln!(
        stderr,
            "A report file was generated at `{}`.",
            file_path.display()
        );
    }
    let _ = writeln!(
        stderr,
        "Please include the arguments with which you invoked runscript, and, if possible, the script\n\
        you ran it on.",
    );
    let _ = stderr.reset();
}

fn main() {
    #[cfg(feature = "panic-hook")]
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

#[derive(Parser)]
#[clap(
    about,
    version,
    disable_help_subcommand = true,
    args_conflicts_with_subcommands = true,
    override_usage = "run [options] [target:phase] [-- args]\n       \
        run -c <command> [-- args]\n       \
        run -s <file> [-- args]"
)]
struct Cli {
    #[clap(subcommand)]
    subcommand: Option<CliSubcommand>,
    #[clap(value_enum, long = "color", global = true, default_value_t)]
    colour_choice: CliColourChoice,

    #[clap(short, long, value_hint = ValueHint::FilePath)]
    file: Option<PathBuf>,
    #[clap(short, long)]
    build: bool,
    #[clap(short, long)]
    run: bool,
    #[clap(short, long)]
    test: bool,

    #[clap(hide = true)]
    target: Option<String>,
    #[clap(hide = true, last = true, global = true)]
    args: Vec<String>,
}

#[derive(ValueEnum, Clone, Default)]
enum CliColourChoice {
    Always,
    #[clap(name = "ansi")]
    AlwaysAnsi,
    #[default]
    Auto,
    Never,
}

#[derive(Subcommand)]
enum CliSubcommand {
    #[clap(name = "ArbitrarilyLongStringWhichOnlyServesToDelayTheInevitableCommand")]
    #[clap(override_usage = "run -c <command> [-- args]")]
    #[clap(short_flag = 'c', long_flag = "command", hide = true)]
    Command(CommandSubcommand),
    #[clap(name = "ArbitrarilyLongStringWhichOnlyServesToDelayTheInevitableScript")]
    #[clap(override_usage = "run -s <script> [-- args]")]
    #[clap(short_flag = 's', long_flag = "script", hide = true)]
    Script(ScriptSubcommand),
    #[clap(name = "ArbitrarilyLongStringWhichOnlyServesToDelayTheInevitableList")]
    #[clap(override_usage = "run -l [-f <FILE>]")]
    #[clap(short_flag = 'l', long_flag = "list", hide = true)]
    List(ListSubcommand),
}

#[derive(Args)]
struct CommandSubcommand {
    #[clap(value_hint = ValueHint::CommandString)]
    command: String,
}

#[derive(Args)]
struct ScriptSubcommand {
    #[clap(value_hint = ValueHint::FilePath)]
    path: PathBuf,
}

#[derive(Args)]
struct ListSubcommand {
    #[clap(short, long, value_hint = ValueHint::FilePath)]
    file: Option<PathBuf>,
}

pub fn run(context: BaseExecContext) -> ExitCode {
    let mut app = Cli::command().no_binary_name(true);

    let options = match app.try_get_matches_from_mut(context.args) {
        Ok(m) => Cli::from_arg_matches(&m).unwrap(),
        Err(err) => {
            #[allow(clippy::print_stdout)]
            match err.kind() {
                clap::error::ErrorKind::DisplayHelp => {
                    print!("{}", HELP_TEXT); // HELP_TEXT contains a trailing newline
                    return exitcode::OK;
                }
                clap::error::ErrorKind::DisplayVersion => {
                    print!("runscript {}", VERSION);
                    for feature in FEATURES.iter().filter_map(|(feature, enabled)| enabled.then_some(feature)) {
                        print!(" +{}", feature);
                    }
                    println!();
                    print!("{}", VERSION_TEXT); // VERSION_TEXT contains a trailing newline
                    return exitcode::OK;
                }
                _ => {
                    err.print().expect("Failed to write clap error");
                    return exitcode::USAGE;
                }
            }
        }
    };

    let colour_choice = match options.colour_choice {
        CliColourChoice::Always => ColorChoice::Always,
        CliColourChoice::Never => ColorChoice::Never,
        CliColourChoice::AlwaysAnsi => ColorChoice::AlwaysAnsi,
        CliColourChoice::Auto => context.colour_choice,
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
                        out::warning(
                            &output_stream,
                            format_args!(
                                "Failed to parse config file at `{}`\n{}",
                                config_file.display(),
                                e
                            ),
                        );
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

    #[cfg(feature = "panic-hook")]
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

    match options.subcommand {
        Some(CliSubcommand::Command(command)) => match parser::parse_command(&command.command) {
            Ok(command) => {
                let exec_cfg = ExecConfig {
                    output_stream: None,
                    colour_choice,
                    working_directory: cwd,
                    script_path: None,
                    target_name: None,
                    positional_args: std::iter::once("".to_string())
                        .chain(options.args)
                        .collect(),
                };
                let mut shell_context = ShellContext::new(exec_cfg);
                shell_context
                    .exec_command_group(&[command])
                    .wait()
                    .status
                    .coerced_code()
            }
            Err(e) => {
                let mut lock = output_stream.lock();
                writeln!(lock, "{}", e).unwrap();
                exitcode::DATAERR
            }
        },
        Some(CliSubcommand::Script(script)) => {
            let script_source = match std::fs::read_to_string(&script.path) {
                Ok(script_source) => script_source,
                Err(e) => {
                    out::file_read_err(&output_stream, e);
                    return exitcode::NOINPUT;
                }
            };

            let parsed_script = match parser::parse_shell(SourceFile {
                working_dir: cwd.clone(),
                path: script.path.clone(),
                source: script_source,
            }) {
                Ok(script) => script,
                Err(e) => {
                    out::file_parse_err(&output_stream, &script.path.display().to_string(), e);
                    return exitcode::DATAERR;
                }
            };

            let script_path_argument = script
                .path
                .canonicalize()
                .map(|path| path.to_string_lossy().into_owned())
                .unwrap_or_else(|_| script.path.to_string_lossy().into_owned());

            let exec_cfg = ExecConfig {
                output_stream: Some(output_stream.clone()),
                colour_choice,
                working_directory: cwd,
                script_path: Some(script.path),
                target_name: options.target,
                positional_args: std::iter::once(script_path_argument)
                    .chain(options.args)
                    .collect(),
            };

            let mut shell_context = ShellContext::new(exec_cfg);
            let status = shell_context
                .exec_command_group(&parsed_script)
                .wait()
                .status;
            out::process_finish(&output_stream, &status);
            status.coerced_code()
        }
        Some(CliSubcommand::List(list)) => {
            let files = match select_files(
                list.file.as_deref(),
                &config,
                context.current_file.as_deref(),
                &cwd,
                &output_stream,
            ) {
                Ok(runfile) => runfile,
                Err(code) => return code,
            };

            let runscripts = parse_files(&output_stream, &cwd, files);

            if runscripts.is_empty() {
                exitcode::DATAERR
            } else {
                let mut lock = output_stream.lock();
                let mut first = true;
                for rf in runscripts {
                    if !first {
                        writeln!(lock).unwrap();
                    }
                    first = false;
                    lock.set_color(ColorSpec::new().set_bold(true).set_intense(true))
                        .unwrap();
                    write!(lock, "Targets from `").unwrap();
                    lock.set_color(
                        ColorSpec::new()
                            .set_bold(true)
                            .set_intense(true)
                            .set_fg(Some(Color::Cyan)),
                    )
                    .unwrap();
                    write!(lock, "{}", rf.display_path).unwrap();
                    lock.set_color(ColorSpec::new().set_bold(true).set_intense(true))
                        .unwrap();
                    writeln!(lock, "`:").unwrap();
                    lock.reset().unwrap();
                    let longest_target = rf
                        .scripts
                        .keys()
                        .map(|s| match s.as_str() {
                            "" => "(blank)".len(),
                            s => s.len(),
                        })
                        .max()
                        .unwrap_or(0);

                    list_scripts_for(&mut lock, &config, longest_target, &rf);
                }
                exitcode::OK
            }
        }
        None => {
            let files = match select_files(
                options.file.as_deref(),
                &config,
                context.current_file.as_deref(),
                &cwd,
                &output_stream,
            ) {
                Ok(runfile) => runfile,
                Err(code) => return code,
            };

            let runscripts = parse_files(&output_stream, &cwd, files);

            if runscripts.is_empty() {
                exitcode::DATAERR
            } else {
                let target = options.target.as_deref().unwrap_or("");
                let (target, phase) = target
                    .split_once(':')
                    .map(|(target, phase)| (target, Some(phase)))
                    .unwrap_or_else(|| {
                        if options.build {
                            (target, Some("build"))
                        } else if options.run {
                            (target, Some("run"))
                        } else if options.test {
                            (target, Some("test"))
                        } else {
                            (target, None)
                        }
                    });
                let target = if target.is_empty() {
                    context.current_target.as_deref()
                } else {
                    Some(target)
                };

                match runscripts.iter().find_map(|script| {
                    match target {
                        Some(target) => script.get_target(target),
                        None => script.get_default_target(),
                    }
                    .map(|target| (target, script))
                }) {
                    Some(((name, target), script)) => {
                        let exec_cfg = ExecConfig {
                            output_stream: Some(output_stream.clone()),
                            colour_choice,
                            working_directory: script.working_dir.clone(),
                            script_path: Some(script.canonical_path.clone()),
                            target_name: Some(name.clone()),
                            positional_args: std::iter::once(
                                script.canonical_path.display().to_string(),
                            )
                            .chain(options.args)
                            .collect(),
                        };

                        let phases = match phase {
                            Some(phase) => vec![phase.to_string()],
                            None => match target.options.default_phase {
                                Overrideable::Set(ref phases) => phases.clone(),
                                Overrideable::Unset => vec!["run".to_string()],
                                Overrideable::SetNone => {
                                    out::no_default_phase(&output_stream, name);
                                    return exitcode::NOINPUT;
                                }
                            },
                        };

                        let mut exit_code = exitcode::OK;
                        for phase in phases {
                            if let Some(script) = target.scripts.get(&phase) {
                                out::phase_message(&output_stream, &config, &phase, name);
                                let status = match &script.commands {
                                    ScriptExecution::Internal { commands, .. } => {
                                        let mut shell_context = ShellContext::new(exec_cfg.clone());
                                        shell_context
                                            .exec_command_group(
                                                &commands
                                                    .clone() //TODO: Don't clone?
                                                    .into_iter()
                                                    .map(|sc| sc.command)
                                                    .collect::<Vec<_>>(),
                                            )
                                            .wait()
                                            .status
                                    }
                                    ScriptExecution::ExternalPosix { command, script } => {
                                        let mut file = tempfile::NamedTempFile::new().unwrap();
                                        file.write_all(script.as_bytes()).unwrap();

                                        match std::process::Command::new(&command[0])
                                            .args(&command[1..])
                                            .arg(file.path())
                                            .args(&exec_cfg.positional_args[1..])
                                            .current_dir(&exec_cfg.working_directory)
                                            .status()
                                        {
                                            Ok(status) => process::ProcessExit::StdStatus(status),
                                            Err(err) => process::ProcessExit::ExecError(
                                                process::CommandExecError::CommandFailed { err },
                                            ),
                                        }
                                    }
                                };
                                out::process_finish(&output_stream, &status);
                                exit_code = status.coerced_code();
                                if exitcode::is_error(exit_code) {
                                    break;
                                }
                            } else {
                                //TODO: Ensure all phases exist prior to running any of them
                                out::bad_script_phase(&output_stream, name, &phase);
                                exit_code = exitcode::NOINPUT;
                                break;
                            }
                        }
                        exit_code
                    }
                    None => {
                        match target {
                            Some(target) => out::bad_target(&output_stream, target),
                            None => out::bad_default(&output_stream),
                        }
                        exitcode::NOINPUT
                    }
                }
            }
        }
    }
}

fn select_files(
    cli_file: Option<&Path>,
    config: &Config,
    context_file: Option<&Path>,
    cwd: &Path,
    output_stream: &StandardStream,
) -> Result<Vec<SourceFile>, ExitCode> {
    match cli_file {
        Some(file) => {
            let path = std::fs::canonicalize(file).map_err(|err| {
                out::file_read_err(output_stream, err);
                exitcode::NOINPUT
            })?;
            let source = std::fs::read_to_string(&path).map_err(|err| {
                out::file_read_err(output_stream, err);
                exitcode::NOINPUT
            })?;
            let working_dir = path.parent().unwrap_or_else(|| Path::new("")).to_owned();
            Ok(vec![SourceFile {
                path,
                working_dir,
                source,
            }])
        }
        None => match context_file {
            Some(file) => {
                let source = std::fs::read_to_string(&file).map_err(|err| {
                    out::file_read_err(output_stream, err);
                    exitcode::NOINPUT
                })?;
                let working_dir = file.parent().unwrap_or_else(|| Path::new("")).to_owned();
                Ok(vec![SourceFile {
                    path: file.to_owned(),
                    working_dir,
                    source,
                }])
            }
            None => {
                let files = cwd
                    .ancestors()
                    .flat_map(|path| {
                        config.file.names.iter().filter_map(|file| {
                            let runfile_path = path.join(file);
                            std::fs::read_to_string(&runfile_path)
                                .ok()
                                .map(|source| SourceFile {
                                    path: runfile_path,
                                    working_dir: path.to_owned(),
                                    source,
                                })
                        })
                    })
                    .collect::<Vec<_>>();
                if files.is_empty() {
                    out::no_runfile_err(output_stream);
                    Err(exitcode::NOINPUT)
                } else {
                    Ok(files)
                }
            }
        },
    }
}

fn parse_files(
    output_stream: &termcolor::StandardStream,
    cwd: &Path,
    files: Vec<SourceFile>,
) -> Vec<Runscript> {
    files
        .into_iter()
        .filter_map(|source_file| {
            match {
                #[cfg(feature = "old-parser")]
                {
                    match parser::parse_runscript(source_file.clone()) {
                        Ok(rf) => Ok(rf),
                        Err(e) => parser::old::parse_runscript(source_file.clone())
                            .map(|rf| {
                                out::warning(
                                    output_stream,
                                    format_args!(
                                        "Using old parser to parse `{}`",
                                        source_file
                                            .path
                                            .strip_prefix(&cwd)
                                            .unwrap_or(&source_file.path)
                                            .display()
                                    ),
                                );
                                rf
                            })
                            .map_err(|()| e),
                    }
                }
                #[cfg(not(feature = "old-parser"))]
                {
                    parser::parse_runscript(source_file.clone())
                }
            } {
                Ok(rf) => Some(rf),
                Err(e) => {
                    out::file_parse_err(
                        output_stream,
                        &source_file
                            .path
                            .strip_prefix(cwd)
                            .unwrap_or(&source_file.path)
                            .display()
                            .to_string(),
                        e,
                    );
                    None
                }
            }
        })
        .collect::<Vec<_>>()
}

fn list_scripts_for(
    lock: &mut termcolor::StandardStreamLock,
    config: &Config,
    name_length: usize,
    runscript: &Runscript,
) {
    let default = runscript.get_default_target().map(|(default, _)| default);
    for (is_default, target, map) in runscript.scripts.iter().map(|(target, map)| {
        if target.is_empty() {
            (default == Some(target), "(blank)", map)
        } else {
            (default == Some(target), target.as_ref(), map)
        }
    }) {
        print_phase_list(lock, config, target, name_length, is_default, map);
    }
}

fn print_phase_list(
    lock: &mut termcolor::StandardStreamLock,
    config: &Config,
    name: &str,
    name_length: usize,
    is_default: bool,
    target: &Target,
) {
    if is_default {
        lock.set_color(ColorSpec::new().set_fg(Some(Color::Yellow)))
            .unwrap();
    }
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
