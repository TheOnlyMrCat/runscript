use std::io::Write;

use crate::parser::ast::{
    AtomicTopLevelWord, ComplexWord, Parameter, ParameterSubstitution, RedirectOrCmdWord,
    SimpleCommand, SimpleWord, Word,
};
use crate::process::ProcessExit;
use termcolor::{Color, ColorSpec, StandardStream, StandardStreamLock, WriteColor};

use crate::config::Config;
use crate::exec::EvaluatedRedirect;
use crate::parser::RunscriptParseError;

pub fn no_runfile_err(output_stream: &StandardStream) {
    let mut lock = output_stream.lock();
    writeln!(lock, "run: Could not find runfile to execute").expect("Failed to write");
}

pub fn dir_read_err(output_stream: &StandardStream, err: std::io::Error) {
    let mut lock = output_stream.lock();
    writeln!(lock, "run: Failed to access working directory: {}", err).expect("Failed to write");
}

pub fn file_read_err(output_stream: &StandardStream, err: std::io::Error) {
    let mut lock = output_stream.lock();
    writeln!(lock, "run: Failed to read script: {}", err).expect("Failed to write");
}

pub fn file_parse_err(output_stream: &StandardStream, error: RunscriptParseError) {
    let mut lock = output_stream.lock();
    match &error {
        RunscriptParseError::InvalidValue {
            line,
            found,
            expected,
        } => {
            writeln!(
                lock,
                "run({line}): Invalid value: {found} (expected {expected})"
            )
            .unwrap();
        }
        RunscriptParseError::DuplicateScript {
            new_line: line,
            prev_line: prev,
            target_name: t,
        } => {
            writeln!(
                lock,
                "run({line}): Duplicate script: {t} (previously defined at {prev})"
            )
            .unwrap();
        }
        RunscriptParseError::CommandParseError { line, error } => {
            writeln!(lock, "run({line}): {error}").unwrap();
        }
        RunscriptParseError::IllegalCommandLocation { line } => {
            writeln!(lock, "run({line}): Illegal command location").unwrap();
        }
        RunscriptParseError::NonexistentOption { line, option } => {
            writeln!(lock, "run({line}): Nonexistent option: {option}").unwrap();
        }
    }
}

// pub fn bad_command_err(output_stream: &StandardStream, cmd: &ScriptEntry, script: &Runscript, error: CommandExecError) {
// 	match &error {
// 		CommandExecError::BadCommand { err, loc } => match cmd {
// 		    ScriptEntry::Command(TopLevelCommand::Command(cmd)) => emit_error(&output_stream, loc, script, match err.kind() {
// 				NotFound => format!("Couldn't find executable for `{}`", cmd.target),
// 				PermissionDenied => format!("Insufficient permission to execute `{}`", cmd.target),
// 				_ => format!("Failed to execute `{}`", cmd.target),
// 			}),
// 		    _ => unreachable!()
// 		},
// 		CommandExecError::InvalidGlob { glob, loc, .. } => emit_error(&output_stream, loc, script, format!("Failed to parse `{}`", glob)),
// 		CommandExecError::NoGlobMatches { glob, loc, .. } => emit_error(&output_stream, loc, script, format!("No matches found for `{}`", glob)),
// 	}
// 	//TODO Verbose output option
// }

pub fn bad_target(output_stream: &StandardStream, target: &str) {
    let mut lock = output_stream.lock();
    writeln!(lock, "run: No target with name {target}").expect("Failed to write");
}

pub fn bad_default(output_stream: &StandardStream) {
    let mut lock = output_stream.lock();
    writeln!(lock, "run: No default target").expect("Failed to write");
}

pub fn bad_script_phase(output_stream: &StandardStream) {
    let mut lock = output_stream.lock();
    writeln!(lock, "run: No scripts to execute for specified phase").expect("Failed to write");
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
    .expect("Failed to set colour");
    write!(lock, "{}", { phase[0..1].to_uppercase() + &phase[1..] }).expect("Failed to write");
    lock.reset().expect("Failed to reset colour");
    writeln!(lock, " {}", name).expect("Failed to write");
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
        .expect("Failed to set colour");
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
            .expect("Failed to set colour");
            write!(lock, "={}", evaluated_contents).expect("Failed to write");
        }
        lock.reset().expect("Failed to reset colour");
        write!(lock, " ").expect("Failed to write");
    }
    if !redirects.is_empty() || !env_remaps.is_empty() {
        writeln!(lock).expect("Failed to write");
        write!(lock, "-- ").expect("Failed to write");
        for (key, value) in env_remaps {
            write!(lock, "{}={} ", key, value).expect("Failed to write");
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
                    write!(lock, "{}<", fd.unwrap_or(0)).expect("Failed to write");
                    lock.set_color(
                        ColorSpec::new()
                            .set_intense(true)
                            .set_fg(Some(Color::Black)),
                    )
                    .expect("Failed to set colour");
                    write!(lock, "(heredoc) ").expect("Failed to write");
                    lock.reset()
                }
                EvaluatedRedirect::DupRead(_fd, _) => todo!(),
                EvaluatedRedirect::DupWrite(_fd, _) => todo!(),
            }
            .expect("Failed to write");
        }
    }
}

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
            write!(lock, "\"").expect("Failed to write");
            let mut concat = String::new();
            for word in w.iter() {
                concat.push_str(&print_simple_word(lock, word));
            }
            write!(lock, "\"").expect("Failed to write");
            concat
        }
        Word::SingleQuoted(lit) => {
            write!(lock, "'{}'", lit).expect("Failed to write");
            lit.clone()
        }
    }
}

fn print_simple_word(lock: &mut StandardStreamLock, word: &SimpleWord) -> String {
    match word {
        SimpleWord::Literal(lit) => {
            write!(lock, "{}", lit).expect("Failed to write");
            lit.clone()
        }
        SimpleWord::Escaped(tk) => {
            write!(lock, "\\{}", tk).expect("Failed to write");
            tk.clone()
        }
        SimpleWord::Param(p) => {
            write!(lock, "$").expect("Failed to write");
            print_parameter(lock, p)
        }
        SimpleWord::Subst(s) => {
            match *s {
                ParameterSubstitution::Command(_) => {
                    write!(lock, "$(...)").expect("Failed to write")
                }
                ParameterSubstitution::Default(colon, ref param, ref default) => {
                    write!(lock, "${{").expect("Failed to write");
                    print_parameter(lock, param);
                    write!(lock, "{}-", if colon { ":" } else { "" }).expect("Failed to write");
                    if let Some(word) = default {
                        print_tl_word(lock, word);
                    }
                    write!(lock, "}}").expect("Failed to write");
                }
                _ => todo!(),
            }
            "$\u{0}".to_owned()
        }
        SimpleWord::Star => {
            write!(lock, "*").expect("Failed to write");
            "*".to_owned()
        }
        SimpleWord::Question => {
            write!(lock, "?").expect("Failed to write");
            "?".to_owned()
        }
        SimpleWord::SquareOpen => {
            write!(lock, "[").expect("Failed to write");
            "[".to_owned()
        }
        SimpleWord::SquareClose => {
            write!(lock, "]").expect("Failed to write");
            "]".to_owned()
        }
        SimpleWord::Tilde => {
            write!(lock, "~").expect("Failed to write");
            "~".to_owned()
        }
        SimpleWord::Colon => {
            write!(lock, ":").expect("Failed to write");
            ":".to_owned()
        }
    }
}

fn print_parameter(lock: &mut StandardStreamLock, parameter: &Parameter) -> String {
    match parameter {
        Parameter::At => write!(lock, "@").expect("Failed to write"),
        Parameter::Star => write!(lock, "*").expect("Failed to write"),
        Parameter::Pound => write!(lock, "#").expect("Failed to write"),
        Parameter::Question => write!(lock, "?").expect("Failed to write"),
        Parameter::Dash => write!(lock, "-").expect("Failed to write"),
        Parameter::Dollar => write!(lock, "$").expect("Failed to write"),
        Parameter::Bang => write!(lock, "!").expect("Failed to write"),
        Parameter::Positional(n) if *n < 10 => write!(lock, "{}", n).expect("Failed to write"),
        Parameter::Positional(n) => write!(lock, "{{{}}}", n).expect("Failed to write"),
        Parameter::Var(v) => write!(lock, "{}", v).expect("Failed to write"),
    }
    "$\u{0}".to_owned()
}

pub fn env_remaps(output_stream: &StandardStream, remaps: &[(String, String)]) {
    let mut lock = output_stream.lock();
    for (k, v) in remaps {
        write!(lock, "{k}={v} ").expect("Failed to write");
    }
}

pub fn process_finish(status: &ProcessExit) {
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

        // SAFETY: No input is invalid.
        let sigstr_ptr = unsafe { strsignal(signal as std::os::raw::c_int) };

        if sigstr_ptr.is_null() {
            format!("signal {}", signal)
        } else {
            // SAFETY: The returned string is valid until the next call to strsignal, and has been verified to be non-null.
            let sigstr = unsafe { CStr::from_ptr(sigstr_ptr) };
            format!(
                "signal {signal} ({}){}",
                sigstr.to_string_lossy(),
                if core { " - core dumped" } else { "" }
            )
        }
    }

    match status {
        ProcessExit::Bool(b) => {
            if !*b {
                println!("=> exit 1");
            }
        }
        ProcessExit::StdStatus(status) => {
            if !status.success() {
                if let Some(c) = status.code() {
                    println!("=> {}", code(c));
                } else {
                    #[cfg(unix)]
                    {
                        use std::os::unix::process::ExitStatusExt;

                        println!(
                            "=> {}",
                            signal(status.signal().unwrap(), status.core_dumped())
                        );
                    }
                    #[cfg(not(unix))]
                    {
                        println!("=> exit ?");
                    }
                }
            }
        }
        #[cfg(unix)]
        ProcessExit::NixStatus(status) => match status {
            nix::sys::wait::WaitStatus::Exited(_, c) => {
                if *c != 0 {
                    println!("=> {}", code(*c));
                }
            }
            nix::sys::wait::WaitStatus::Signaled(_, sig, core) => {
                signal((*sig) as i32, *core);
            }
            _ => println!("=> exit ?"),
        },
    }
}
