use std::io::Write;
use std::process::ExitStatus;
use std::sync::Arc;

use conch_parser::ast::{Arithmetic, AtomicTopLevelCommand, AtomicTopLevelWord, ComplexWord, Parameter, ParameterSubstitution, Redirect, RedirectOrCmdWord, RedirectOrEnvVar, SimpleCommand, SimpleWord, Word};
use termcolor::{Color, ColorSpec, StandardStream, StandardStreamLock, WriteColor};

// use crate::exec::CommandExecError;
use crate::parser::{RunscriptLocation, RunscriptParseError, RunscriptParseErrorData};
use crate::script::{Runscript, ScriptPhase};

pub fn no_runfile_err(output_stream: &Arc<StandardStream>) {
    let mut lock = output_stream.lock();
    writeln!(lock, "Could not find runfile to execute").expect("Failed to write");
}

pub fn dir_read_err(output_stream: &Arc<StandardStream>, err: std::io::Error) {
    let mut lock = output_stream.lock();
    writeln!(lock, "Failed to access working directory: {}", err).expect("Failed to write");
}

pub fn file_read_err(output_stream: &Arc<StandardStream>, err: std::io::Error) {
    let mut lock = output_stream.lock();
    writeln!(lock, "Failed to read script: {}", err).expect("Failed to write");
}

pub fn bad_phase_err(output_stream: &Arc<StandardStream>, phase: &str) {
    let mut lock = output_stream.lock();
    writeln!(
        lock,
        "`{}` is not a valid phase identifier; expected [`b!`, `b`, `br`, `r`, `r!`]",
        phase
    )
    .expect("Failed to write");
}

pub fn option_parse_err(output_stream: &Arc<StandardStream>, err: getopts::Fail) {
    let mut lock = output_stream.lock();
    writeln!(lock, "{}", err).expect("Failed to write");
}

pub fn file_parse_err(
    output_stream: &Arc<StandardStream>,
    RunscriptParseError { script, data }: RunscriptParseError,
) {
    match &data {
        RunscriptParseErrorData::UnexpectedToken {
            location: loc,
            found,
            expected,
        } => {
            emit_error(
                output_stream,
                loc,
                &script,
                format!(
                    "Unexpected token; found `{}` but expected {}",
                    found, expected
                ),
            );
        }
        RunscriptParseErrorData::UnexpectedEOF {
            location: loc,
            expected,
        } => {
            emit_error(
                output_stream,
                loc,
                &script,
                format!("Unexpected end of file; expected {}", expected),
            );
        }
        RunscriptParseErrorData::InvalidValue {
            location: loc,
            found,
            expected,
        } => {
            emit_error(output_stream, loc, &script, format!("Invalid identifier; found `{}` but expected `{}`", found, expected));
        }
        RunscriptParseErrorData::DuplicateScript {
            new_location: loc,
            previous_location: prev,
            target_name: t,
        } => {
            emit_error(
                output_stream,
                loc,
                &script,
                format!(
                    "Multiple definitions of `{}`",
                    match &**t {
                        "#" => "global target".to_owned(),
                        "-" => "default target".to_owned(),
                        s => format!("`{}`", s),
                    }
                ),
            );
            emit_error(
                output_stream,
                prev,
                &script,
                "Previous definition is here".to_owned(),
            );
        }
        RunscriptParseErrorData::BadInclude { location: loc, .. } => {
            emit_error(
                output_stream,
                loc,
                &script,
                "Could not include referenced runfile".to_owned(),
            );
            //TODO: I/O error notes
        }
        RunscriptParseErrorData::NestedError {
            include_location: loc,
            ..
        } => {
            emit_error(
                output_stream,
                loc,
                &script,
                "Parse error in included file".to_owned(),
            );
        }
        RunscriptParseErrorData::CommandParseError { location, error } => {
            emit_error(
                output_stream,
                location,
                &script,
                format!("Error parsing command: {}", error),
            );
        }
    }
}

// pub fn bad_command_err(output_stream: &Arc<StandardStream>, cmd: &ScriptEntry, script: &Runscript, error: CommandExecError) {
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

pub fn bad_target(output_stream: &Arc<StandardStream>, target: &str) {
    let mut lock = output_stream.lock();
    writeln!(lock, "No target with name {}", target).expect("Failed to write");
}

pub fn bad_default(output_stream: &Arc<StandardStream>) {
    let mut lock = output_stream.lock();
    writeln!(lock, "No default target").expect("Failed to write");
}

pub fn bad_script_phase(output_stream: &Arc<StandardStream>) {
    let mut lock = output_stream.lock();
    writeln!(lock, "No scripts to execute for specified phase").expect("Failed to write");
}

pub fn phase_color(phase: ScriptPhase) -> Color {
    if std::env::var_os("RUNSCRIPT_TRANS").is_some() {
        match phase {
            ScriptPhase::BuildOnly | ScriptPhase::RunOnly => Color::Cyan,
            ScriptPhase::Build | ScriptPhase::Run => Color::Magenta,
            ScriptPhase::BuildAndRun => Color::White,
        }
    } else {
        match phase {
            ScriptPhase::BuildOnly => Color::Red,
            ScriptPhase::Build => Color::Yellow,
            ScriptPhase::BuildAndRun => Color::Green,
            ScriptPhase::Run => Color::Blue,
            ScriptPhase::RunOnly => Color::Magenta,
        }
    }
}

pub fn phase_message(output_stream: &Arc<StandardStream>, phase: ScriptPhase, name: &str) {
    let mut lock = output_stream.lock();
    lock.set_color(
        ColorSpec::new()
            .set_bold(true)
            .set_intense(true)
            .set_fg(Some(phase_color(phase))),
    )
    .expect("Failed to set colour");
    write!(lock, "{}", phase).expect("Failed to write");
    lock.reset().expect("Failed to reset colour");
    writeln!(lock, " {}", name).expect("Failed to write");
}

pub fn command_prompt(
    output_stream: &Arc<StandardStream>,
    command: &SimpleCommand<
        String,
        AtomicTopLevelWord<String>,
        Redirect<AtomicTopLevelWord<String>>,
    >,
    redirects: &[Redirect<Vec<String>>],
    evaluated: &[Vec<String>],
) {
    let mut lock = output_stream.lock();
    let words = command.redirects_or_cmd_words.iter().filter_map(|w| match w {
        RedirectOrCmdWord::Redirect(_) => None,
        RedirectOrCmdWord::CmdWord(w) => Some(w),
    });
    lock.set_color(
        ColorSpec::new()
            .set_bold(true)
            .set_intense(true),
    ).expect("Failed to set colour");
    for (word, evaluated) in words.zip(evaluated.iter()) {
        let word_contents = print_tl_word(&mut lock, word);
        lock.reset().expect("Failed to reset colour");
        let evaluated_contents = evaluated.join(" ");
        //? Should perhaps compare in the other direction: split the word contents
        if word_contents != evaluated_contents {
            lock.set_color(
                ColorSpec::new()
                    .set_bold(true)
                    .set_intense(true)
                    .set_fg(Some(Color::Cyan)),
            ).expect("Failed to set colour");
            write!(lock, "={}", evaluated_contents).expect("Failed to write");
            lock.reset().expect("Failed to reset colour");
        }
        write!(lock, " ").expect("Failed to write");
    }
    if !redirects.is_empty() {
        writeln!(lock).expect("Failed to write");
        write!(lock, "-- ").expect("Failed to write");
        for redirect in redirects {
            match redirect {
                Redirect::Read(fd, file) => write!(lock, "{}<{}", fd.unwrap_or(0), file.join(" ")),
                Redirect::Write(fd, file) => write!(lock, "{}>{}", fd.unwrap_or(1), file.join(" ")),
                Redirect::ReadWrite(fd, file) => write!(lock, "{}<>{}", fd.unwrap_or(0), file.join(" ")),
                Redirect::Append(fd, file) => write!(lock, "{}>>{}", fd.unwrap_or(1), file.join(" ")),
                Redirect::Clobber(fd, file) => write!(lock, "{}>|{}", fd.unwrap_or(1), file.join(" ")),
                Redirect::Heredoc(fd, _) => {
                    write!(lock, "{}<", fd.unwrap_or(0)).expect("Failed to write");
                    lock.set_color(
                        ColorSpec::new()
                            .set_intense(true)
                            .set_fg(Some(Color::Black)),
                    ).expect("Failed to set colour");
                    write!(lock, "heredoc").expect("Failed to write");
                    lock.reset()
                },
                Redirect::DupRead(fd, _) => todo!(),
                Redirect::DupWrite(fd, _) => todo!(),
            }.expect("Failed to write");
        }
    }
}

fn print_tl_word(
    lock: &mut StandardStreamLock,
    AtomicTopLevelWord(word): &AtomicTopLevelWord<String>,
) -> String {
    match word {
        ComplexWord::Concat(words) => {
            let mut concat = String::new();
            for word in words.iter() {
                concat.push_str(&print_word(lock, word));
            }
            concat
        }
        ComplexWord::Single(word) => {
            print_word(lock, word)
        }
    }
}

fn print_word(
    lock: &mut StandardStreamLock,
    word: &Word<
        String,
        SimpleWord<
            String,
            Parameter<String>,
            Box<
                ParameterSubstitution<
                    Parameter<String>,
                    AtomicTopLevelWord<String>,
                    AtomicTopLevelCommand<String>,
                    Arithmetic<String>,
                >,
            >,
        >,
    >,
) -> String {
    match word {
        Word::Simple(w) => {
            print_simple_word(lock, w)
        },
        Word::DoubleQuoted(w) => {
            write!(lock, "\"").expect("Failed to write");
            let mut concat = String::new();
            for word in w.iter() {
                concat.push_str(&print_simple_word(lock, word));
            }
            write!(lock, "\"").expect("Failed to write");
            concat
        },
        Word::SingleQuoted(lit) => {
            write!(lock, "'{}'", lit).expect("Failed to write");
            lit.clone()
        },
    }
}

fn print_simple_word(
    lock: &mut StandardStreamLock,
    word: &SimpleWord<
        String,
        Parameter<String>,
        Box<
            ParameterSubstitution<
                Parameter<String>,
                AtomicTopLevelWord<String>,
                AtomicTopLevelCommand<String>,
                Arithmetic<String>,
            >,
        >,
    >,
) -> String {
    match word {
        SimpleWord::Literal(lit) => {
            write!(lock, "{}", lit).expect("Failed to write");
            lit.clone()
        },
        SimpleWord::Escaped(tk) => {
            write!(lock, "\\{}", tk).expect("Failed to write");
            tk.clone()
        },
        SimpleWord::Param(p) => {
            write!(lock, "$").expect("Failed to write");
            match p {
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
        },
        SimpleWord::Subst(s) => {
            match **s {
                ParameterSubstitution::Command(_) => write!(lock, "$(...)").expect("Failed to write"),
                _ => todo!(),
            }
            "$\u{0}".to_owned()
        },
        SimpleWord::Star => {
            write!(lock, "*").expect("Failed to write");
            "*".to_owned()
        },
        SimpleWord::Question => {
            write!(lock, "?").expect("Failed to write");
            "?".to_owned()
        },
        SimpleWord::SquareOpen => {
            write!(lock, "[").expect("Failed to write");
            "[".to_owned()
        },
        SimpleWord::SquareClose => {
            write!(lock, "]").expect("Failed to write");
            "]".to_owned()
        },
        SimpleWord::Tilde => {
            write!(lock, "~").expect("Failed to write");
            "~".to_owned()
        },
        SimpleWord::Colon => {
            write!(lock, ":").expect("Failed to write");
            ":".to_owned()
        },
        
    }
}

fn emit_error(
    output_stream: &Arc<StandardStream>,
    location: &RunscriptLocation,
    script: &Runscript,
    error_msg: String,
) {
    let mut lock = output_stream.lock();
    match script.unwind_fileid(&location.index) {
        Some(file) => {
            writeln!(
                lock,
                "{}({}:{}): {}",
                file.name, location.line, location.column, error_msg
            )
            .expect("Failed to write");
            fn recursive_include_unwind(
                lock: &mut termcolor::StandardStreamLock,
                script: &Runscript,
                fileid: &[usize],
            ) {
                let (first, rest) = match fileid.split_first() {
                    Some(x) => x,
                    None => return,
                };
                let mut name = &script.name;
                let mut loc_ref = &script.includes[*first];
                for index in rest {
                    name = &loc_ref.runscript.name;
                    loc_ref = &loc_ref.runscript.includes[*index];
                }
                let line = loc_ref.location.line;
                writeln!(lock, "-> In file included from {}:{}", name, line)
                    .expect("Failed to write");
                recursive_include_unwind(lock, script, fileid.split_last().unwrap().1);
            }
            recursive_include_unwind(&mut lock, script, &location.index);
        }
        None => {
            writeln!(lock, "run: Failed to build include tree for error").expect("Failed to write");
            writeln!(
                lock,
                "-> Error at {}:{}: {}",
                location.line, location.column, error_msg
            )
            .expect("Failed to write");
        }
    }
}

pub fn process_finish(status: &crate::exec::ProcessExit) {
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
            _ => format!("exit {}", code),
        }
    }

    fn signal(signal: i32, core: Option<bool>) -> String {
        use std::ffi::CStr;

        // SAFETY: No input is invalid.
        let sigstr_ptr = unsafe { strsignal(signal as std::os::raw::c_int) };

        if sigstr_ptr.is_null() {
            format!("signal {}", signal)
        } else {
            // SAFETY: The returned string is valid until the next call to strsignal, and has been verified to be non-null.
            let sigstr = unsafe { CStr::from_ptr(sigstr_ptr) };
            format!("signal {} ({}{})", signal, sigstr.to_string_lossy(), match core {
                Some(true) => " - core dumped",
                Some(false) => "",
                None => {
                    if signal & 0x80 != 0 {
                        " - core dumped?"
                    } else {
                        ""
                    }
                }
            })
        }
    }

    #[cfg(not(unix))]
    fn signal(_: &ExitStatus) -> String {
        panic!("Non-unix program terminated with signal");
    }

    match status {
        crate::exec::ProcessExit::Bool(b) => {
            if !*b {
                println!("=> exit 1");
            }
        },
        crate::exec::ProcessExit::StdStatus(status) => {
            if !status.success() {
                if let Some(c) = status.code() {
                    println!("=> {}", code(c));
                } else {
                    use std::os::unix::process::ExitStatusExt;

                    println!("=> {}", signal(status.signal().unwrap(), None));
                }
            }
        },
        crate::exec::ProcessExit::NixStatus(status) => {
            match status {
                nix::sys::wait::WaitStatus::Exited(_, c) => {
                    if *c != 0 {
                        println!("=> {}", code(*c));
                    }
                },
                nix::sys::wait::WaitStatus::Signaled(_, sig, core) => {
                    signal((*sig) as i32, Some(*core));
                },
                _ => println!("=> exit ?"),
            }
        },
    }
}