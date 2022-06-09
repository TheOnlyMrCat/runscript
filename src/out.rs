use std::io::Write;

use crate::parser::ast::{
    AtomicTopLevelWord, ComplexWord, Parameter, ParameterSubstitution, RedirectOrCmdWord,
    SimpleCommand, SimpleWord, Word,
};
use crate::process::{CommandExecError, ProcessExit};
use termcolor::{Color, ColorSpec, StandardStream, StandardStreamLock, WriteColor};

use crate::config::Config;
use crate::exec::EvaluatedRedirect;
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

pub fn file_parse_err(output_stream: &StandardStream, err: RunscriptParseError) {
    match &err {
        RunscriptParseError::DuplicateScript {
            new_line: line,
            prev_line: prev,
            target_name: t,
        } => {
            error(
                output_stream,
                format_args!(
                    "Duplicate script: `{t}` on line {line} (previously defined at {prev})"
                ),
            );
        }
        RunscriptParseError::CommandParseError { line, error: err } => {
            error(
                output_stream,
                format_args!("Parse error: {err} on line {line}"),
            );
        }
        RunscriptParseError::IllegalCommandLocation { line } => {
            error(
                output_stream,
                format_args!("Command outside of target on line {line}"),
            );
        }
        RunscriptParseError::NonexistentOption { line, option } => {
            error(
                output_stream,
                format_args!("Nonexistent option: `{option}` on line {line}"),
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

pub fn command_prompt(
    output_stream: &StandardStream,
    command: &SimpleCommand,
    env_remaps: &[(String, String)],
    redirects: &[EvaluatedRedirect],
    evaluated: &[Vec<String>],
) {
    let mut lock = output_stream.lock();
    let words = command
        .redirects_or_cmd_words
        .iter()
        .filter_map(|w| match w {
            RedirectOrCmdWord::Redirect(_) => None,
            RedirectOrCmdWord::CmdWord(w) => Some(w),
        });
    lock.set_color(ColorSpec::new().set_bold(true).set_intense(true))
        .unwrap();
    for (word, evaluated) in words.zip(evaluated.iter()) {
        let word_contents = print_tl_word(&mut lock, word);
        let evaluated_contents = evaluated.join(" ");
        //? Should perhaps compare in the other direction: split the word contents
        if word_contents != evaluated_contents {
            lock.set_color(
                ColorSpec::new()
                    .set_bold(true)
                    .set_intense(true)
                    .set_fg(Some(Color::Cyan)),
            )
            .unwrap();
            write!(lock, "={}", evaluated_contents).unwrap();
        }
        lock.reset().unwrap();
        write!(lock, " ").unwrap();
    }
    if !redirects.is_empty() || !env_remaps.is_empty() {
        writeln!(lock).unwrap();
        write!(lock, "-- ").unwrap();
        for (key, value) in env_remaps {
            write!(lock, "{}={} ", key, value).unwrap();
        }
        for redirect in redirects {
            match redirect {
                EvaluatedRedirect::Read(fd, file) => {
                    write!(lock, "{}<{} ", fd.unwrap_or(0), file.join(" "))
                }
                EvaluatedRedirect::Write(fd, file) => {
                    write!(lock, "{}>{} ", fd.unwrap_or(1), file.join(""))
                }
                EvaluatedRedirect::ReadWrite(fd, file) => {
                    write!(lock, "{}<>{} ", fd.unwrap_or(0), file.join(""))
                }
                EvaluatedRedirect::Append(fd, file) => {
                    write!(lock, "{}>>{} ", fd.unwrap_or(1), file.join(""))
                }
                EvaluatedRedirect::Clobber(fd, file) => {
                    write!(lock, "{}>|{} ", fd.unwrap_or(1), file.join(""))
                }
                EvaluatedRedirect::Heredoc(fd, _) => {
                    write!(lock, "{}<", fd.unwrap_or(0)).unwrap();
                    lock.set_color(
                        ColorSpec::new()
                            .set_intense(true)
                            .set_fg(Some(Color::Black)),
                    )
                    .unwrap();
                    write!(lock, "(heredoc) ").unwrap();
                    lock.reset()
                }
                EvaluatedRedirect::DupRead(_fd, _) => todo!(),
                EvaluatedRedirect::DupWrite(_fd, _) => todo!(),
            }
            .unwrap();
        }
    }
}

//TODO: Don't print from these functions
fn print_tl_word(lock: &mut StandardStreamLock, word: &AtomicTopLevelWord) -> String {
    match word {
        ComplexWord::Concat(words) => {
            let mut concat = String::new();
            for word in words.iter() {
                concat.push_str(&print_word(lock, word));
            }
            concat
        }
        ComplexWord::Single(word) => print_word(lock, word),
    }
}

fn print_word(lock: &mut StandardStreamLock, word: &Word) -> String {
    match word {
        Word::Simple(w) => print_simple_word(lock, w),
        Word::DoubleQuoted(w) => {
            write!(lock, "\"").unwrap();
            let mut concat = String::new();
            for word in w.iter() {
                concat.push_str(&print_simple_word(lock, word));
            }
            write!(lock, "\"").unwrap();
            concat
        }
        Word::SingleQuoted(lit) => {
            write!(lock, "'{}'", lit).unwrap();
            lit.clone()
        }
    }
}

fn print_simple_word(lock: &mut StandardStreamLock, word: &SimpleWord) -> String {
    match word {
        SimpleWord::Literal(lit) => {
            write!(lock, "{}", lit).unwrap();
            lit.clone()
        }
        SimpleWord::Escaped(tk) => {
            write!(lock, "\\{}", tk).unwrap();
            tk.clone()
        }
        SimpleWord::Param(p) => {
            write!(lock, "$").unwrap();
            print_parameter(lock, p)
        }
        SimpleWord::Subst(s) => {
            match *s {
                ParameterSubstitution::Command(_) => write!(lock, "$(...)").unwrap(),
                ParameterSubstitution::Default(colon, ref param, ref default) => {
                    write!(lock, "${{").unwrap();
                    print_parameter(lock, param);
                    write!(lock, "{}-", if colon { ":" } else { "" }).unwrap();
                    if let Some(word) = default {
                        print_tl_word(lock, word);
                    }
                    write!(lock, "}}").unwrap();
                }
                _ => todo!(),
            }
            "$\u{0}".to_owned()
        }
        SimpleWord::Star => {
            write!(lock, "*").unwrap();
            "*".to_owned()
        }
        SimpleWord::Question => {
            write!(lock, "?").unwrap();
            "?".to_owned()
        }
        SimpleWord::SquareOpen => {
            write!(lock, "[").unwrap();
            "[".to_owned()
        }
        SimpleWord::SquareClose => {
            write!(lock, "]").unwrap();
            "]".to_owned()
        }
        SimpleWord::Tilde => {
            write!(lock, "~").unwrap();
            "~".to_owned()
        }
        SimpleWord::Colon => {
            write!(lock, ":").unwrap();
            ":".to_owned()
        }
    }
}

fn print_parameter(lock: &mut StandardStreamLock, parameter: &Parameter) -> String {
    match parameter {
        Parameter::At => write!(lock, "@").unwrap(),
        Parameter::Star => write!(lock, "*").unwrap(),
        Parameter::Pound => write!(lock, "#").unwrap(),
        Parameter::Question => write!(lock, "?").unwrap(),
        Parameter::Dash => write!(lock, "-").unwrap(),
        Parameter::Dollar => write!(lock, "$").unwrap(),
        Parameter::Bang => write!(lock, "!").unwrap(),
        Parameter::Positional(n) if *n < 10 => write!(lock, "{}", n).unwrap(),
        Parameter::Positional(n) => write!(lock, "{{{}}}", n).unwrap(),
        Parameter::Var(v) => write!(lock, "{}", v).unwrap(),
    }
    "$\u{0}".to_owned()
}

pub fn env_remaps(output_stream: &StandardStream, remaps: &[(String, String)]) {
    let mut lock = output_stream.lock();
    for (k, v) in remaps {
        write!(lock, "{k}={v} ").unwrap();
    }
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
            CommandExecError::BadStarPositional { err } => {
                format!("Could not parse positional parameter: {}", err)
            } //TODO: Which parameter in particular?
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
