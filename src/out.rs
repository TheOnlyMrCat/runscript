use std::io::Write;

use crate::process::{CommandExecError, ProcessExit};
use termcolor::{Color, ColorSpec, StandardStream, StandardStreamLock, WriteColor};

use crate::config::Config;
use crate::parser::RunscriptParseError;

pub fn warning(output_stream: &StandardStream, message: std::fmt::Arguments) {
    let mut lock = output_stream.lock();
    lock.set_color(ColorSpec::new().set_bold(true).set_fg(Some(Color::Yellow)))
        .unwrap();
    write!(lock, "warning:").unwrap();
    lock.reset().unwrap();
    writeln!(lock, " {message}").unwrap();
}

pub fn error(output_stream: &StandardStream, message: std::fmt::Arguments) {
    let mut lock = output_stream.lock();
    lock.set_color(ColorSpec::new().set_bold(true).set_fg(Some(Color::Red)))
        .unwrap();
    write!(lock, "error:").unwrap();
    lock.reset().unwrap();
    writeln!(lock, " {message}").unwrap()
}

pub fn no_runfile_err(output_stream: &StandardStream) {
    error(
        output_stream,
        format_args!("Could not find runfile to execute"),
    );
}

pub fn dir_read_err(output_stream: &StandardStream, err: std::io::Error) {
    error(
        output_stream,
        format_args!("Failed to access working directory: {err}"),
    );
}

pub fn file_read_err(output_stream: &StandardStream, err: std::io::Error) {
    error(output_stream, format_args!("Failed to read script: {err}"));
}

pub fn file_parse_err(output_stream: &StandardStream, file: &str, err: RunscriptParseError) {
    match &err {
        RunscriptParseError::DuplicateScript {
            new_line: line,
            prev_line: prev,
            target_name: t,
        } => {
            error(
                output_stream,
                format_args!(
                    "In file {file}: Duplicate script: `{t}` on line {line} (previously defined at {prev})"
                ),
            );
        }
        RunscriptParseError::CommandParseError { line, error: err } => {
            error(
                output_stream,
                format_args!("In file {file}: Parse error: {err} on line {line}"),
            );
        }
        RunscriptParseError::IllegalCommandLocation { line } => {
            error(
                output_stream,
                format_args!("In file {file}: Command outside of target on line {line}"),
            );
        }
        RunscriptParseError::NonexistentOption { line, option } => {
            error(
                output_stream,
                format_args!("In file {file}: Nonexistent option: `{option}` on line {line}"),
            );
        }
    }
}

pub fn bad_target(output_stream: &StandardStream, target: &str) {
    error(
        output_stream,
        format_args!("No target with name `{target}`"),
    );
}

pub fn bad_default(output_stream: &StandardStream) {
    error(output_stream, format_args!("No default target"));
}

pub fn no_default_phase(output_stream: &StandardStream, target: &str) {
    error(
        output_stream,
        format_args!("Target `{target}` has no default phase"),
    );
}

pub fn bad_script_phase(output_stream: &StandardStream, target: &str, phase: &str) {
    error(
        output_stream,
        format_args!("No scripts to execute for `{target}:{phase}`"),
    );
}

pub fn phase_color(config: &Config, phase: &str) -> Color {
    if config.colours.enabled {
        config
            .colours
            .phases
            .get(phase)
            .copied()
            .unwrap_or(Color::White)
    } else {
        Color::White
    }
}

pub fn phase_message(output_stream: &StandardStream, config: &Config, phase: &str, name: &str) {
    let mut lock = output_stream.lock();
    lock.set_color(
        ColorSpec::new()
            .set_bold(true)
            .set_intense(true)
            .set_fg(Some(phase_color(config, phase))),
    )
    .unwrap();
    write!(lock, "{}", { phase[0..1].to_uppercase() + &phase[1..] }).unwrap();
    lock.reset().unwrap();
    writeln!(lock, " {}", name).unwrap();
}

pub struct Printable {
    pub command: Option<PrintableCommand>,
    pub env_remaps: Vec<PrintableEnvRemap>,
    pub redirects: Vec<PrintableRedirect>,
}

pub struct PrintableCommand {
    pub name: String,
    pub arguments: Vec<PrintableComplexWord>,
}

pub struct PrintableEnvRemap {
    pub key: String,
    pub value: PrintableComplexWord,
}

impl Default for PrintableEnvRemap {
    fn default() -> Self {
        Self {
            key: Default::default(),
            value: PrintableComplexWord::Empty,
        }
    }
}

pub struct PrintableRedirect {
    pub fd: Option<u16>,
    pub direction: String,
    pub literal: bool,
    pub file: String,
}

pub enum PrintableComplexWord {
    GlobPattern {
        glob_string: Vec<PrintableWord>,
        matches: Vec<String>,
    },
    Words(Vec<PrintableWord>),
    Empty,
}

pub enum PrintableWord {
    SingleQuoted(String),
    DoubleQuoted(Vec<PrintableSimpleWord>),
    Unquoted(PrintableSimpleWord),
}

pub enum PrintableSimpleWord {
    Literal(String),
    Escaped(String),
    Substitution { original: String, evaluated: String },
}

pub fn print_command(
    lock: &mut StandardStreamLock,
    printable: &Printable,
) -> Result<(), std::io::Error> {
    //TODO: Make colour config customisable!!
    for remap in &printable.env_remaps {
        write!(lock, "{}=", remap.key)?;
        print_complex_word(lock, &remap.value)?;
    }

    if let Some(command) = &printable.command {
        if !printable.env_remaps.is_empty() {
            // Separate remaps from command name
            write!(lock, " ")?;
        }
        // Print command name in bold
        lock.set_color(ColorSpec::new().set_bold(true).set_intense(true))?;
        write!(lock, "{}", command.name)?;
        lock.reset()?;

        for complex_word in &command.arguments {
            write!(lock, " ")?;
            print_complex_word(lock, complex_word)?;
        }
    }
    lock.reset()?;

    for redirect in &printable.redirects {
        write!(
            lock,
            " {}{}",
            if let Some(fd) = redirect.fd {
                format!("{}", fd)
            } else {
                "".to_owned()
            },
            redirect.direction
        )?;
        if !redirect.literal {
            lock.set_color(
                ColorSpec::new()
                    .set_fg(Some(Color::Cyan))
                    .set_bold(true)
                    .set_intense(true),
            )?
        }
        write!(lock, "{}", redirect.file)?;
        lock.reset()?;
    }

    Ok(())
}

enum FormattingContext {
    GlobPattern,
    Quoted,
    Freestanding,
}

fn print_complex_word(
    lock: &mut StandardStreamLock,
    complex_word: &PrintableComplexWord,
) -> Result<(), std::io::Error> {
    match complex_word {
        PrintableComplexWord::GlobPattern {
            glob_string,
            matches,
        } => {
            //TODO: Allow printing glob matches instead
            lock.set_color(ColorSpec::new().set_fg(Some(Color::Blue)))?;
            for word in glob_string {
                print_word(lock, word, FormattingContext::GlobPattern)?;
            }
            //TODO: Don't print this on windows console, because it doesn't have dimmed?
            lock.set_color(ColorSpec::new().set_fg(Some(Color::White)).set_dimmed(true))?;
            write!(lock, " ({} matches)", matches.len())?;
            lock.reset()?;
        }
        PrintableComplexWord::Words(words) => {
            //TODO: If this complex word was evaluated empty in evaluations-only output, print an explicit ''
            for word in words {
                print_word(lock, word, FormattingContext::Freestanding)?;
            }
        }
        PrintableComplexWord::Empty => {
            write!(lock, "''")?;
        }
    }
    Ok(())
}

fn print_word(
    lock: &mut StandardStreamLock,
    word: &PrintableWord,
    context: FormattingContext,
) -> Result<(), std::io::Error> {
    match word {
        PrintableWord::SingleQuoted(literal) => {
            // Highlight strings in yellow
            if !matches!(context, FormattingContext::GlobPattern) {
                lock.set_color(
                    ColorSpec::new()
                        .set_fg(Some(Color::Yellow))
                        .set_intense(true),
                )?;
            }
            write!(lock, "'{}'", literal)?;
        }
        PrintableWord::DoubleQuoted(simple_words) => {
            // Highlight strings in yellow
            if !matches!(context, FormattingContext::GlobPattern) {
                lock.set_color(
                    ColorSpec::new()
                        .set_fg(Some(Color::Yellow))
                        .set_intense(true),
                )?;
            }
            write!(lock, "\"")?;
            for simple_word in simple_words {
                print_simple_word(lock, simple_word, FormattingContext::Quoted)?;
            }
            write!(lock, "\"")?;
        }
        PrintableWord::Unquoted(simple_word) => print_simple_word(lock, simple_word, context)?,
    }
    Ok(())
}

fn print_simple_word(
    lock: &mut StandardStreamLock,
    simple_word: &PrintableSimpleWord,
    context: FormattingContext,
) -> Result<(), std::io::Error> {
    match simple_word {
        PrintableSimpleWord::Literal(literal) => write!(lock, "{}", literal)?,
        PrintableSimpleWord::Escaped(literal) => write!(lock, "\\{}", literal)?,
        PrintableSimpleWord::Substitution {
            original: _,
            evaluated,
        } => {
            if evaluated.is_empty() && matches!(context, FormattingContext::Freestanding) {
                // lock.set_color(ColorSpec::new().set_fg(Some(Color::Yellow)).set_intense(true))?;
                lock.set_color(ColorSpec::new().set_fg(Some(Color::White)).set_dimmed(true))?;
                write!(lock, "''")?;
                // write!(lock, "(${})", original)?;
            } else {
                lock.set_color(
                    ColorSpec::new()
                        .set_fg(Some(Color::Cyan))
                        .set_bold(true)
                        .set_intense(true),
                )?;
                write!(lock, "{}", evaluated)?;
            }
            match context {
                FormattingContext::GlobPattern => {
                    lock.set_color(ColorSpec::new().set_fg(Some(Color::Blue)))?
                }
                FormattingContext::Quoted => lock.set_color(
                    ColorSpec::new()
                        .set_fg(Some(Color::Yellow))
                        .set_intense(true),
                )?,
                FormattingContext::Freestanding => lock.reset()?,
            }
        }
    }
    Ok(())
}

pub fn process_finish(output_stream: &StandardStream, status: &ProcessExit) {
    extern "C" {
        fn strsignal(sig: std::os::raw::c_int) -> *const std::os::raw::c_char;
    }

    fn code(code: i32) -> String {
        match code {
            64 => "exit 64 (EX_USAGE)".to_owned(),
            65 => "exit 65 (EX_DATAERR)".to_owned(),
            66 => "exit 66 (EX_NOINPUT)".to_owned(),
            67 => "exit 67 (EX_NOUSER)".to_owned(),
            68 => "exit 68 (EX_NOHOST)".to_owned(),
            69 => "exit 69 (EX_UNAVAILABLE)".to_owned(),
            70 => "exit 70 (EX_SOFTWARE)".to_owned(),
            71 => "exit 71 (EX_OSERR)".to_owned(),
            72 => "exit 72 (EX_OSFILE)".to_owned(),
            73 => "exit 73 (EX_CANTCREAT)".to_owned(),
            74 => "exit 74 (EX_IOERR)".to_owned(),
            75 => "exit 75 (EX_TEMPFAIL)".to_owned(),
            76 => "exit 76 (EX_PROTOCOL)".to_owned(),
            77 => "exit 77 (EX_NOPERM)".to_owned(),
            78 => "exit 78 (EX_CONFIG)".to_owned(),
            101 => "exit 101 (Process panicked)".to_owned(),
            _ => format!("exit {code}"),
        }
    }

    fn signal(signal: i32, core: bool) -> String {
        use std::ffi::CStr;

        // SAFETY: No input to `strsignal` is invalid.
        let sigstr_ptr = unsafe { strsignal(signal as std::os::raw::c_int) };

        if sigstr_ptr.is_null() {
            format!(
                "signal {}{}",
                signal,
                if core { " - core dumped" } else { "" }
            )
        } else {
            // SAFETY: The string returned from `strsignal` is valid until our next call to strsignal
            // and has been verified to be non-null. The string returned by `strsignal` is null-terminated.
            let sigstr = unsafe { CStr::from_ptr(sigstr_ptr) };
            format!(
                "signal {} ({}){}",
                signal,
                sigstr.to_string_lossy(),
                if core { " - core dumped" } else { "" }
            )
        }
    }

    fn exec_error(error: &CommandExecError) -> String {
        match error {
            CommandExecError::CommandFailed { err } => format!("{}", err),
            CommandExecError::InvalidGlob { glob, err } => {
                format!("Invalid glob pattern: {}: {}", glob, err)
            }
            CommandExecError::NoGlobMatches { glob } => {
                format!("No matches for glob pattern: {}", glob)
            }
            CommandExecError::UnhandledOsString { .. } => {
                "Command substitution outputted non-UTF-8 data".to_owned()
            } //TODO: Which parameter in particular?
            CommandExecError::BadRedirect { err } => {
                format!("Failed to open redirect file: {}", err)
            }
            CommandExecError::UnsupportedRedirect => {
                "Unsupported operation for redirect".to_owned()
            }
        }
    }

    let mut lock = output_stream.lock();
    match status {
        ProcessExit::Bool(b) => {
            if !*b {
                writeln!(lock, "=> exit 1").unwrap();
            }
        }
        ProcessExit::StdStatus(status) => {
            if !status.success() {
                if let Some(c) = status.code() {
                    writeln!(lock, "=> {}", code(c)).unwrap();
                } else {
                    #[cfg(unix)]
                    {
                        use std::os::unix::process::ExitStatusExt;

                        writeln!(
                            lock,
                            "=> {}",
                            signal(status.signal().unwrap(), status.core_dumped())
                        )
                        .unwrap();
                    }
                    #[cfg(not(unix))]
                    {
                        // Probably unreachable, but just in case. The documentation doesn't guarantee that
                        // it will always return `Some` on non-unix platforms.
                        writeln!(lock, "=> exit ?").unwrap();
                    }
                }
            }
        }
        ProcessExit::ExecError(error) => {
            writeln!(lock, "=> run: {}", exec_error(error)).unwrap();
        }
        #[cfg(unix)]
        ProcessExit::NixStatus(status) => match status {
            nix::sys::wait::WaitStatus::Exited(_, c) => {
                if *c != 0 {
                    writeln!(lock, "=> {}", code(*c)).unwrap();
                }
            }
            nix::sys::wait::WaitStatus::Signaled(_, sig, core) => {
                writeln!(lock, "=> {}", signal((*sig) as i32, *core)).unwrap();
            }
            _ => writeln!(lock, "=> exit ?").unwrap(),
        },
    }
}
