use std::error::Error;
use std::io::ErrorKind::*;
use std::io::Write;
use std::rc::Rc;

use termcolor::{StandardStream, ColorChoice, WriteColor, ColorSpec, Color};

use lalrpop_util::lexer::Token;
use lalrpop_util::ParseError::{self, *};

use crate::runfile::{Command, ScriptType};

pub fn file_read_err(output_stream: Rc<StandardStream>) {
	let mut lock = output_stream.lock();
	writeln!(lock, "Could not find runfile to execute");
}

pub fn option_parse_err(output_stream: Rc<StandardStream>, err: getopts::Fail) {
	let mut lock = output_stream.lock();
	writeln!(lock, "{}", err);
}

#[derive(Debug)]
pub enum RunscriptError {
	MultipleDefinition {
		target: String,
		location: Location,
		previous_def: Location,
	},
	InvalidInclude {
		include_str: String,
		file_err: std::io::Error,
		dir_err: std::io::Error,
		location: Location,
	}
}

#[derive(Debug)]
pub enum CommandExecErr {
	InvalidGlob {
		glob: String,
		err: anyhow::Error,
		loc: Location,
	},
	NoGlobMatches {
		glob: String,
		loc: Location,
	},
	BadCommand {
		err: std::io::Error,
		loc: Location,
	},
}
use CommandExecErr::*;

#[derive(Debug, Clone)]
pub struct Location {
	include_path: Box<[usize]>,
	start_index: usize,
	end_index: usize,
}

pub fn file_parse_err(config: &crate::Config, err: ParseError<usize, lalrpop_util::lexer::Token, RunscriptError>) {
	//TODO use proper file locations, instead of just indices
	match &err {
		InvalidToken { location: loc } => {
			let mut lock = config.output_stream.lock();
			let (line, col) = config.codespan_file.line_ends.iter().enumerate().find_map(|(line, &index)|
				if index < *loc {
					Some((line, *loc - index))
				} else {
					None
				}
			).unwrap();
			writeln!(lock, "{}({}:{}): Invalid token", config.codespan_file.name, line, col);
		},
		UnrecognizedEOF { location: loc, expected } => {
			let mut lock = config.output_stream.lock();
			let (line, col) = config.codespan_file.line_ends.iter().enumerate().find_map(|(line, &index)|
				if index < *loc {
					Some((line, *loc - index))
				} else {
					None
				}
			).unwrap();
			writeln!(lock, "{}({}:{}): Unexpected end of file", config.codespan_file.name, line, col);
		},
		UnrecognizedToken { token: (start, Token(id, name), end), expected } => {
			let mut lock = config.output_stream.lock();
			let (line, col) = config.codespan_file.line_ends.iter().enumerate().find_map(|(line, &index)|
				if index < *start {
					Some((line, *start - index))
				} else {
					None
				}
			).unwrap();
			writeln!(lock, "{}({}:{}): Unexpected `{}`", config.codespan_file.name, line, col, name);
		},
		ExtraToken { token: (start, Token(_id, name), end) } => {
			let mut lock = config.output_stream.lock();
			let (line, col) = config.codespan_file.line_ends.iter().enumerate().find_map(|(line, &index)|
				if index < *start {
					Some((line, *start - index))
				} else {
					None
				}
			).unwrap();
			writeln!(lock, "{}({}:{}): Unexpected `{}`, expected end of file", config.codespan_file.name, line, col, name);
		},
		User { error } => match error {
			RunscriptError::MultipleDefinition { target: t, location, previous_def } => {
				location.emit_error(config, format!("Multiple definitions of `{}`", match &**t { "#" => "global target".to_owned(), "-" => "default target".to_owned(), s => format!("target `{}`", s)}));
			},
			RunscriptError::InvalidInclude { include_str, file_err, dir_err, location } => {
				location.emit_error(config, format!("Could not include `{}`", include_str));
				//TODO: I/O error notes
			}
		}
	}
}

pub fn bad_command_err(config: &crate::Config, cmd: &Command, error: CommandExecErr) {
	let mut lock = config.output_stream.lock();
	match &error {
		BadCommand { err, loc } => loc.emit_error(config, match err.kind() {
			NotFound => format!("Couldn't find executable for `{}`", cmd.target),
			PermissionDenied => format!("Insufficient permission to execute `{}`", cmd.target),
			_ => format!("Failed to execute `{}`", cmd.target),
		}),
		InvalidGlob { glob, loc, .. } => loc.emit_error(config, format!("Failed to parse `{}`", glob)),
		NoGlobMatches { glob, loc, .. } => loc.emit_error(config, format!("No matches found for `{}`", glob)),
	}
	//TODO Verbose output option
}

pub fn bad_target(config: &crate::Config, target: String) {
	let mut lock = config.output_stream.lock();
	writeln!(lock, "No target with name {}", target);
}

pub fn phase_message(config: &crate::Config, phase: ScriptType, name: &str) {
	let mut lock = config.output_stream.lock();
	lock.set_color(ColorSpec::new().set_bold(true).set_intense(true).set_fg(Some(match phase {
		ScriptType::BuildOnly => Color::Red,
		ScriptType::Build => Color::Yellow,
		ScriptType::BuildAndRun => Color::Green,
		ScriptType::Run => Color::Blue,
		ScriptType::RunOnly => Color::Magenta,
	}))).expect("Failed to set colour");
	write!(lock, "{}", phase).expect("Failed to write");
	lock.reset().expect("Failed to reset colour");
	writeln!(lock, " {}", name).expect("Failed to write");
}

impl Location {
	pub fn new(include_path: Box<[usize]>, start: usize, end: usize) -> Location {
		Location {
			include_path,
			start_index: start,
			end_index: end,
		}
	}

	fn get_file_coordinates(&self, line_ends: &Vec<usize>) -> Option<((usize, usize), (usize, usize))> {
		Some((
			line_ends.iter()
				.enumerate()
				.find_map(|(line, &index)|
					if index >= self.start_index {
						Some((line + 1, if line > 0 {
							// Array access of line_ends here because we want the end index of the previous line
							self.start_index - line_ends[line - 1]
						} else {
							self.start_index
						}))
					} else {
						None
					}
				)?,
			// The same as above, but with self.end_index instead
			line_ends.iter().enumerate().find_map(|(line, &index)| if index >= self.end_index { Some((line + 1, if line > 0 { self.end_index - line_ends[line - 1] } else { self.end_index })) } else { None })?,
		))
	}

	fn emit_error(&self, config: &crate::Config, error_msg: String) {
		let mut lock = config.output_stream.lock();
		let file = config.codespan_file.unwind_fileid(&self.include_path).unwrap();
		let coordinates = self.get_file_coordinates(&file.line_ends).unwrap();
		writeln!(lock, "{}({}:{}): {}", file.name, coordinates.0.0, coordinates.0.1, error_msg);
	}
}