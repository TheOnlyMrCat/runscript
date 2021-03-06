use std::io::ErrorKind::*;
use std::io::Write;
use std::rc::Rc;

use termcolor::{StandardStream, WriteColor, ColorSpec, Color};

use crate::exec::CommandExecError;
use crate::parser::{RunscriptLocation, RunscriptParseError, RunscriptParseErrorData};
use crate::script::TopLevelCommand;
use crate::script::{Runscript, ScriptEntry, ScriptPhase};

pub fn file_read_err(output_stream: &Rc<StandardStream>) {
	let mut lock = output_stream.lock();
	writeln!(lock, "Could not find runfile to execute").expect("Failed to write");
}

pub fn bad_phase_err(output_stream: &Rc<StandardStream>, phase: &str) {
	let mut lock = output_stream.lock();
	writeln!(lock, "`{}` is not a valid phase identifier; expected [`b!`, `b`, `br`, `r`, `r!`]", phase).expect("Failed to write");
}

pub fn option_parse_err(output_stream: &Rc<StandardStream>, err: getopts::Fail) {
	let mut lock = output_stream.lock();
	writeln!(lock, "{}", err).expect("Failed to write");
}

pub fn file_parse_err(output_stream: &Rc<StandardStream>, RunscriptParseError { script, data }: RunscriptParseError) {
	match &data {
		RunscriptParseErrorData::UnexpectedToken { location: loc, found, expected } => {
			emit_error(output_stream, loc, &script, format!("Unexpected token; found `{}` but expected {}", found, expected));
		},
		RunscriptParseErrorData::UnexpectedEOF { location: loc, expected } => {
			emit_error(output_stream, loc, &script, format!("Unexpected end of file; expected {}", expected));
		},
		RunscriptParseErrorData::ReservedToken { location: loc, token, reason } => {
			emit_error(output_stream, loc, &script, format!("Token `{}` reserved. {}", token, reason));
		}
		RunscriptParseErrorData::InvalidID { location: loc, found } => {
			emit_error(output_stream, loc, &script, format!("Invalid content of identifier; found `{}` but identifiers can only have alphanumeric characters and `_`", found));
		},
		RunscriptParseErrorData::MultipleDefinition { new_location: loc, previous_location: prev, target_name: t } => {
			emit_error(output_stream, loc, &script, format!("Multiple definitions of `{}`", match &**t { "#" => "global target".to_owned(), "-" => "default target".to_owned(), s => format!("`{}`", s)}));
			emit_error(output_stream, prev, &script, "Previous definition is here".to_owned());
		},
		RunscriptParseErrorData::BadInclude { location: loc, .. } => {
			emit_error(output_stream, loc, &script, "Could not include referenced runfile".to_owned());
			//TODO: I/O error notes
		},
		RunscriptParseErrorData::NestedError { include_location: loc, .. } => {
			emit_error(output_stream, loc, &script, "Parse error in included file".to_owned());
		},
		RunscriptParseErrorData::IllegalEnv { location: loc, msg } => {
			emit_error(output_stream, loc, &script, format!("Environment variables illegal {}", msg));
		}
	}
}

pub fn bad_command_err(output_stream: &Rc<StandardStream>, cmd: &ScriptEntry, script: &Runscript, error: CommandExecError) {
	match &error {
		CommandExecError::BadCommand { err, loc } => match cmd {
		    ScriptEntry::Command(TopLevelCommand::Command(cmd)) => emit_error(&output_stream, loc, script, match err.kind() {
				NotFound => format!("Couldn't find executable for `{}`", cmd.target),
				PermissionDenied => format!("Insufficient permission to execute `{}`", cmd.target),
				_ => format!("Failed to execute `{}`", cmd.target),
			}),
		    _ => unreachable!()
		},
		CommandExecError::InvalidGlob { glob, loc, .. } => emit_error(&output_stream, loc, script, format!("Failed to parse `{}`", glob)),
		CommandExecError::NoGlobMatches { glob, loc, .. } => emit_error(&output_stream, loc, script, format!("No matches found for `{}`", glob)),
	}
	//TODO Verbose output option
}

pub fn bad_target(output_stream: &Rc<StandardStream>, target: &str) {
	let mut lock = output_stream.lock();
	writeln!(lock, "No target with name {}", target).expect("Failed to write");
	if atty::is(atty::Stream::Stderr) {
		write!(lock, "-> ").expect("Failed to write");
		lock.set_color(ColorSpec::new().set_italic(true)).expect("Failed to set italic");
		writeln!(lock, "(If `{}` was intended as a positional argument, add `$opt default_positionals` to your runscript)", target).expect("Failed to write");
		lock.reset().expect("Failed to reset colour");
	}
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

pub fn phase_message(output_stream: &Rc<StandardStream>, phase: ScriptPhase, name: &str) {
	let mut lock = output_stream.lock();
	lock.set_color(ColorSpec::new().set_bold(true).set_intense(true).set_fg(Some(phase_color(phase)))).expect("Failed to set colour");
	write!(lock, "{}", phase).expect("Failed to write");
	lock.reset().expect("Failed to reset colour");
	writeln!(lock, " {}", name).expect("Failed to write");
}

fn emit_error(output_stream: &Rc<StandardStream>, location: &RunscriptLocation, script: &Runscript, error_msg: String) {
	let mut lock = output_stream.lock();
	match script.unwind_fileid(&location.index) {
		Some(file) => {
			writeln!(lock, "{}({}:{}): {}", file.name, location.line, location.column, error_msg).expect("Failed to write");
			fn recursive_include_unwind(lock: &mut termcolor::StandardStreamLock, script: &Runscript, fileid: &[usize]) {
				let (first, rest) = match fileid.split_first() { Some(x) => x, None => return };
				let mut name = &script.name;
				let mut loc_ref = &script.includes[*first];
				for index in rest {
					name = &loc_ref.runscript.name;
					loc_ref = &loc_ref.runscript.includes[*index];
				}
				let line = loc_ref.location.line;
				writeln!(lock, "-> In file included from {}:{}", name, line).expect("Failed to write");
				recursive_include_unwind(lock, script, fileid.split_last().unwrap().1);
			}
			recursive_include_unwind(&mut lock, script, &location.index);
		},
		None => {
			writeln!(lock, "run: Failed to build include tree for error").expect("Failed to write");
			writeln!(lock, "-> Error at {}:{}: {}", location.line, location.column, error_msg).expect("Failed to write");
		}
	}
}