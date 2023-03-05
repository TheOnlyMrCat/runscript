#![warn(clippy::print_stdout, clippy::print_stderr)]

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
use config::{Config, OutputConfig, Theme};
use exec::BaseExecContext;
use exitcode::ExitCode;

use is_terminal::IsTerminal;
use itertools::Itertools;
use out::{OutputState, OutputStateLock};
use script::CollatedTargets;
use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};

use crate::script::Script;

use self::exec::{ExecConfig, ShellContext};
use self::parser::SourceFile;
use self::script::{Overrideable, Runscript, Target};

const HELP_TEXT: &str = "\
Project script manager and executor

Usage: run [OPTIONS] [TARGET:PHASE ...] [-- ARGS ...]
       run -c <COMMAND> [-- ARGS ...]
       run -s <FILE> [-- ARGS ...]

Global Options:
      --color <WHEN>  [default: auto] [possible values: always, ansi, auto, never]
  -u, --theme <NAME>  Change colour theme for this invocation
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
const FEATURES: &[(&str, bool)] = &[
    ("old-parser", cfg!(feature = "old-parser")),
    ("panic-hook", cfg!(feature = "panic-hook")),
];

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
                for feature in FEATURES
                    .iter()
                    .filter_map(|(feature, enabled)| enabled.then_some(feature))
                {
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
        theme: None,
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
    args_conflicts_with_subcommands = true
)]
struct Cli {
    #[clap(subcommand)]
    subcommand: Option<CliSubcommand>,
    #[clap(value_enum, long = "color", global = true, default_value_t)]
    colour_choice: CliColourChoice,
    #[clap(short = 'u', global = true)]
    theme: Option<String>,

    #[clap(short, long, conflicts_with_all = ["build", "run", "test", "targets", "args"])]
    list: bool,
    #[clap(short, long, value_hint = ValueHint::FilePath)]
    file: Option<PathBuf>,
    #[clap(short, long)]
    build: bool,
    #[clap(short, long)]
    run: bool,
    #[clap(short, long)]
    test: bool,

    targets: Vec<String>,
    #[clap(last = true, global = true)]
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
    #[clap(short_flag = 'c', long_flag = "command")]
    Command(CommandSubcommand),
    #[clap(name = "ArbitrarilyLongStringWhichOnlyServesToDelayTheInevitableScript")]
    #[clap(short_flag = 's', long_flag = "script")]
    Script(ScriptSubcommand),
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
                    for feature in FEATURES
                        .iter()
                        .filter_map(|(feature, enabled)| enabled.then_some(feature))
                    {
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
        CliColourChoice::Auto => match context.colour_choice {
            ColorChoice::Auto => {
                if std::io::stderr().is_terminal() {
                    ColorChoice::Auto
                } else {
                    ColorChoice::Never
                }
            }
            choice => choice,
        },
    };
    let output_stream = StandardStream::stderr(colour_choice);

    let mut output_state = OutputState {
        output_stream,
        theme: context.theme.unwrap_or_else(Theme::default),
        config: OutputConfig::default(),
    };

    let config_dir = {
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
        config_dir
    };
    let config = if let Some(config_dir) = config_dir {
        let config_file = config_dir.join("config.toml");
        let config = if let Ok(contents) = std::fs::read_to_string(&config_file) {
            match toml::from_str(&contents) {
                Ok(config) => config,
                Err(e) => {
                    out::warning(
                        &output_state,
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
        };
        match options.theme.as_ref().or(config.output.theme.as_ref()) {
            Some(name) if name != "default" => {
                let mut name = name;
                let themes_dir = config_dir.join("themes");
                let mut resolved_theme = Theme::empty();
                while name != "default" {
                    let theme_file = {
                        if name.contains(['.', '/']) {
                            out::warning(
                                &output_state,
                                format_args!("Theme name contains illegal characters `.` or `/`"),
                            );
                            break;
                        }
                        let mut theme_file = themes_dir.join(name);
                        theme_file.set_extension("toml");
                        theme_file
                    };
                    match std::fs::read_to_string(&theme_file) {
                        Ok(contents) => match toml::from_str(&contents) {
                            Ok(theme) => resolved_theme = resolved_theme.merged_with(theme),
                            Err(e) => out::warning(
                                &output_state,
                                format_args!(
                                    "Failed to parse theme at `{}`\n{}",
                                    theme_file.display(),
                                    e
                                ),
                            ),
                        },
                        Err(e) => out::warning(
                            &output_state,
                            format_args!(
                                "Failed to read theme file at `{}`\n{}",
                                theme_file.display(),
                                e
                            ),
                        ),
                    }

                    name = &resolved_theme.based_on;
                }

                output_state.theme = resolved_theme
                    .merged_with(output_state.theme)
                    .canonicalised();
            }
            _ => {}
        }

        config
    } else {
        Config::default()
    };

    let output_state = Arc::new(output_state);

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
            out::dir_read_err(&output_state, e);
            return exitcode::NOINPUT;
        }
    };

    match options.subcommand {
        Some(CliSubcommand::Command(command)) => {
            match parser::parse_command(command.command.as_bytes()) {
                Ok(command) => {
                    let exec_cfg = ExecConfig {
                        output_state: None,
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
                        .exec_commands(&command)
                        .wait()
                        .status
                        .coerced_code()
                }
                Err(errors) => {
                    //TODO: Pass this through `out` module
                    let mut state = output_state.lock();
                    for e in errors {
                        writeln!(state.output_stream, "{}", e).unwrap();
                    }
                    exitcode::DATAERR
                }
            }
        }
        Some(CliSubcommand::Script(script)) => {
            let script_source = match std::fs::read(&script.path) {
                Ok(script_source) => script_source,
                Err(e) => {
                    out::file_read_err(&output_state, e);
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
                    // out::file_parse_err(&output_state, &script.path.display().to_string(), e);
                    return exitcode::DATAERR;
                }
            };

            let script_path_argument = script
                .path
                .canonicalize()
                .map(|path| path.to_string_lossy().into_owned())
                .unwrap_or_else(|_| script.path.to_string_lossy().into_owned());

            let exec_cfg = ExecConfig {
                output_state: Some(output_state.clone()),
                colour_choice,
                working_directory: cwd,
                script_path: Some(script.path),
                target_name: None,
                positional_args: std::iter::once(script_path_argument)
                    .chain(options.args)
                    .collect(),
            };

            let mut shell_context = ShellContext::new(exec_cfg);
            let status = shell_context.exec_commands(&parsed_script).wait().status;
            out::process_finish(&output_state, &status);
            status.coerced_code()
        }
        None if options.list => {
            let files = match select_files(
                options.file.as_deref(),
                &config,
                context.current_file.as_deref(),
                &cwd,
                &output_state,
            ) {
                Ok(runfile) => runfile,
                Err(code) => return code,
            };

            if files.is_empty() {
                return exitcode::NOINPUT;
            }

            let runscripts = parse_files(&output_state, &cwd, files);

            let mut state = output_state.lock();
            let mut first = true;
            for rf in runscripts {
                let lock = &mut state.output_stream;
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

                list_scripts_for(&mut state, longest_target, &rf);
            }
            exitcode::OK
        }
        None => {
            let files = match select_files(
                options.file.as_deref(),
                &config,
                context.current_file.as_deref(),
                &cwd,
                &output_state,
            ) {
                Ok(runfile) => runfile,
                Err(code) => return code,
            };

            let runscript = parse_files(&output_state, &cwd, files).collect::<CollatedTargets>();

            struct ResolvedTarget<'a> {
                target: String,
                phase: String,
                exec_cfg: ExecConfig,
                script: &'a Script,
            }

            enum TargetResolutionError {
                NonexistentTarget(String),
                NonexistentScript { target: String, phase: String },
                NoDefault,
                NoDefaultPhase(String),
            }
            let targets = options
                .targets
                .into_iter()
                .map(|target| {
                    let (target, phase) = target
                        .split_once(':')
                        .map(|(target, phase)| (target, Some(phase)))
                        .unwrap_or((&target, None));
                    let (target_name, target_def) = if target.is_empty() {
                        context.current_target.as_deref()
                    } else {
                        Some(target)
                    }
                    .map(|target| {
                        runscript.get_target(target).ok_or_else(|| {
                            TargetResolutionError::NonexistentTarget(target.to_owned())
                        })
                    })
                    .unwrap_or_else(|| {
                        runscript
                            .get_default_target()
                            .ok_or(TargetResolutionError::NoDefault)
                    })?;
                    Ok((target_name, target_def, phase.map(ToOwned::to_owned)))
                })
                .collect::<Result<Vec<_>, _>>()
                .and_then(|targets| {
                    let targets = if targets.is_empty() {
                        match runscript.get_default_target() {
                            Some((target_name, target_def)) => {
                                vec![(target_name, target_def, None)]
                            }
                            None => return Err(TargetResolutionError::NoDefault),
                        }
                    } else {
                        targets
                    };
                    targets
                        .into_iter()
                        .map(|(target_name, target_def, phase)| {
                            let phases = match phase {
                                Some(phase) => vec![phase],
                                None => match target_def.options.default_phase {
                                    _ if options.build || options.run || options.test => options
                                        .build
                                        .then_some("build".to_string())
                                        .into_iter()
                                        .chain(options.run.then_some("run".to_string()))
                                        .chain(options.test.then_some("test".to_string()))
                                        .collect::<Vec<_>>(),
                                    Overrideable::Set(ref phases) => phases.clone(),
                                    Overrideable::Unset => vec!["run".to_string()],
                                    Overrideable::SetNone => {
                                        return Err(TargetResolutionError::NoDefaultPhase(
                                            target_name.clone(),
                                        ))
                                    }
                                },
                            };
                            match phases
                                .into_iter()
                                .map(|phase| {
                                    target_def
                                        .scripts
                                        .get(&phase)
                                        .map(|script| (script, phase.clone()))
                                        .ok_or_else(|| TargetResolutionError::NonexistentScript {
                                            target: target_name.clone(),
                                            phase: phase.clone(),
                                        })
                                })
                                .collect::<Result<Vec<_>, _>>()
                            {
                                Ok(scripts) => {
                                    Ok(scripts.into_iter().map(|(script, phase)| ResolvedTarget {
                                        target: target_name.clone(),
                                        phase,
                                        exec_cfg: ExecConfig {
                                            output_state: Some(output_state.clone()),
                                            colour_choice,
                                            working_directory: script.working_dir.clone(),
                                            script_path: Some(script.canonical_path.clone()),
                                            target_name: Some(target_name.clone()),
                                            positional_args: std::iter::once(
                                                script.canonical_path.display().to_string(),
                                            )
                                            .chain(options.args.iter().cloned())
                                            .collect(),
                                        },
                                        script,
                                    }))
                                }
                                Err(e) => Err(e),
                            }
                        })
                        .flatten_ok()
                        .collect::<Result<Vec<_>, _>>()
                });

            match targets {
                Ok(scripts) => {
                    let mut exit_code = exitcode::OK;
                    for ResolvedTarget {
                        target,
                        phase,
                        exec_cfg,
                        script,
                    } in scripts
                    {
                        out::phase_message(&output_state, &phase, &target);
                        let status = match &script.options.executor {
                            Overrideable::Unset | Overrideable::SetNone => {
                                let commands = match parser::parse_command(&script.body) {
                                    Ok(commands) => commands,
                                    Err(e) => {
                                        todo!()
                                    }
                                };
                                let mut shell_context = ShellContext::new(exec_cfg.clone());
                                shell_context.exec_commands(&commands).wait().status
                            }
                            Overrideable::Set(command) => {
                                let mut file = tempfile::NamedTempFile::new().unwrap();
                                file.write_all(&script.body).unwrap();

                                //FIXME: The file needs to actually be passed as an argument
                                let commands = parser::parse_command(&command).unwrap();
                                let mut shell_context = ShellContext::new(exec_cfg.clone());
                                shell_context.exec_commands(&commands).wait().status
                            }
                        };
                        out::process_finish(&output_state, &status);
                        exit_code = status.coerced_code();
                        if exitcode::is_error(exit_code) {
                            break;
                        }
                    }
                    exit_code
                }
                Err(TargetResolutionError::NoDefault) => {
                    out::bad_default(&output_state);
                    exitcode::NOINPUT
                }
                Err(TargetResolutionError::NoDefaultPhase(target)) => {
                    out::no_default_phase(&output_state, &target);
                    exitcode::NOINPUT
                }
                Err(TargetResolutionError::NonexistentTarget(target)) => {
                    out::bad_target(&output_state, &target);
                    exitcode::NOINPUT
                }
                Err(TargetResolutionError::NonexistentScript { target, phase }) => {
                    out::bad_script_phase(&output_state, &target, &phase);
                    exitcode::NOINPUT
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
    output_state: &OutputState,
) -> Result<Vec<SourceFile>, ExitCode> {
    match cli_file {
        Some(file) => {
            let path = std::fs::canonicalize(file).map_err(|err| {
                out::file_read_err(output_state, err);
                exitcode::NOINPUT
            })?;
            let source = std::fs::read(&path).map_err(|err| {
                out::file_read_err(output_state, err);
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
                let source = std::fs::read(file).map_err(|err| {
                    out::file_read_err(output_state, err);
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
                            std::fs::read(&runfile_path).ok().map(|source| SourceFile {
                                path: runfile_path,
                                working_dir: path.to_owned(),
                                source,
                            })
                        })
                    })
                    .collect::<Vec<_>>();
                if files.is_empty() {
                    out::no_runfile_err(output_state);
                    Err(exitcode::NOINPUT)
                } else {
                    Ok(files)
                }
            }
        },
    }
}

fn parse_files<'a>(
    output_state: &'a OutputState,
    cwd: &'a Path,
    files: Vec<SourceFile>,
) -> impl Iterator<Item = Runscript> + 'a {
    files.into_iter().filter_map(move |source_file| {
        match {
            #[cfg(feature = "old-parser")]
            {
                match parser::parse_runscript(source_file.clone()) {
                    Ok(rf) => Ok(rf),
                    Err(e) => parser::old::parse_runscript(source_file.clone())
                        .map(|rf| {
                            out::warning(
                                output_state,
                                format_args!(
                                    "Using old parser to parse `{}`",
                                    source_file
                                        .path
                                        .strip_prefix(cwd)
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
                // out::file_parse_err(
                //     output_state,
                //     &source_file
                //         .path
                //         .strip_prefix(cwd)
                //         .unwrap_or(&source_file.path)
                //         .display()
                //         .to_string(),
                //     e,
                // );
                None
            }
        }
    })
}

fn list_scripts_for(lock: &mut OutputStateLock, name_length: usize, runscript: &Runscript) {
    let default = runscript.get_default_target().map(|(default, _)| default);
    for (is_default, target, map) in runscript.scripts.iter().map(|(target, map)| {
        if target.is_empty() {
            (default == Some(target), "(blank)", map)
        } else {
            (default == Some(target), target.as_ref(), map)
        }
    }) {
        print_phase_list(lock, target, name_length, is_default, map);
    }
}

fn print_phase_list(
    state: &mut OutputStateLock,
    name: &str,
    name_length: usize,
    is_default: bool,
    target: &Target,
) {
    let lock = &mut state.output_stream;
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
                .set_fg(Some(out::phase_color(state.theme, "build"))),
        )
        .expect("Failed to set colour");
        write!(lock, "B").unwrap();
    } else {
        lock.set_color(
            ColorSpec::new()
                .set_bold(false)
                .set_intense(false)
                .set_fg(Some(out::phase_color(state.theme, "build"))),
        )
        .expect("Failed to set colour");
        write!(lock, ".").unwrap();
    }
    if target.scripts.contains_key("run") {
        lock.set_color(
            ColorSpec::new()
                .set_bold(true)
                .set_intense(true)
                .set_fg(Some(out::phase_color(state.theme, "run"))),
        )
        .expect("Failed to set colour");
        write!(lock, "R").unwrap();
    } else {
        lock.set_color(
            ColorSpec::new()
                .set_bold(false)
                .set_intense(false)
                .set_fg(Some(out::phase_color(state.theme, "run"))),
        )
        .expect("Failed to set colour");
        write!(lock, ".").unwrap();
    }
    if target.scripts.contains_key("test") {
        lock.set_color(
            ColorSpec::new()
                .set_bold(true)
                .set_intense(true)
                .set_fg(Some(out::phase_color(state.theme, "test"))),
        )
        .expect("Failed to set colour");
        write!(lock, "T").unwrap();
    } else {
        lock.set_color(
            ColorSpec::new()
                .set_bold(false)
                .set_intense(false)
                .set_fg(Some(out::phase_color(state.theme, "test"))),
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
                .set_fg(Some(out::phase_color(state.theme, phase))),
        )
        .expect("Failed to set colour");
        write!(lock, " {}", phase).expect("Failed to write");
    }
    lock.reset().expect("Failed to reset colour");
    writeln!(lock).expect("Failed to write");
}
