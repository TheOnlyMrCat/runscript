use std::collections::HashMap;
use std::path::PathBuf;

use conch_parser::ast::AtomicTopLevelCommand;
use conch_parser::ast::builder::{AtomicDefaultBuilder, Newline};
use conch_parser::lexer::Lexer;
use conch_parser::parse::{Parser, ParseError};
use conch_parser::token::Token;
use indexmap::IndexMap;
use void::Void;

use crate::parser::{RunscriptLocation, RunscriptSource};
use crate::script::*;

/// An error from a parse function which involves an I/O operation
/// 
/// This enum exists only to propagate [`std::io::Error`](https://doc.rust-lang.org/std/io/struct.Error.html)s to the
/// calling function without including it in the main [`RunscriptParseError`](struct.RunscriptParseError)
#[derive(Debug)]
#[allow(clippy::large_enum_variant)]
pub enum ParseOrIOError {
	IOError(std::io::Error),
	ParseError(RunscriptParseError),
}

/// An error which occurred during parsing of a runscript
#[derive(Debug)]
#[non_exhaustive]
pub struct RunscriptParseError {
	/// The semi-complete runscript which failed to parse
	pub script: Runscript,
	/// The type of error and data related to that type of error
	pub data: RunscriptParseErrorData,
}

/// Data related to a `RunscriptParseError`
#[derive(Debug)]
#[non_exhaustive]
pub enum RunscriptParseErrorData {
	/// End-of-file in an unexpected location
	UnexpectedEOF {
		location: RunscriptLocation,
		/// The token or token type which was expected to come before an end-of-file.
		/// 
		/// Specific tokens would be in backticks (e.g. `\`#/\``), and token groups without backticks (e.g. `an environment variable`)
		expected: String,
	},
	/// Found a token in a place which does not match the context
	UnexpectedToken {
		location: RunscriptLocation,
		/// The text of the token which triggered the error
		found: String,
		/// The token or token type which was expected to come before or instead of the `found` token.
		/// 
		/// Specific tokens would be in backticks (e.g. `\`#/\``), and token groups without backticks (e.g. `an environment variable`)
		expected: String,
	},
	/// Found a token which is reserved for later use
	ReservedToken {
		location: RunscriptLocation,
		/// The text of the token which triggered the error
		token: String,
		/// The reason the token is reserved
		reason: String,
	},
	/// Found a name of a script which contains characters outside of `[A-Za-z_-]`
	InvalidID {
		location: RunscriptLocation,
		/// The text of the identifier that triggered the error
		found: String,
	},
	/// Attempted to define the same script twice
	MultipleDefinition {
		previous_location: RunscriptLocation,
		new_location: RunscriptLocation,
		target_name: String,
	},
	/// An environment variable declaration was found in an illegal location
	IllegalEnv {
		location: RunscriptLocation,
		/// The message to continue the string `"Environment variables illegal "`
		msg: String,
	},
    UnsupportedFeature {
        location: RunscriptLocation,
        msg: String,
    },
    CommandParseError(ParseError<Void>),
}

impl From<ParseError<Void>> for RunscriptParseErrorData {
    fn from(err: ParseError<Void>) -> Self {
        RunscriptParseErrorData::CommandParseError(err)
    }
}

pub fn parse_runfile(path: impl Into<PathBuf>) -> Result<Runscript, ParseOrIOError> {
	let path = path.into();
	parse_runscript(RunscriptSource {
		source: std::fs::read_to_string(&path).map_err(ParseOrIOError::IOError)?,
		dir: path.parent().expect("Runfile has no parent!").to_owned(),
		path,
	}).map_err(ParseOrIOError::ParseError)
}

#[derive(Debug)]
struct ParsingContext<T: Iterator<Item = (usize, char)> + std::fmt::Debug> {
	iterator: std::iter::Peekable<T>,
	runfile: Runscript,
	line_indices: Vec<usize>,
}

impl<T: Iterator<Item = (usize, char)> + std::fmt::Debug> ParsingContext<T> {

	#[cfg_attr(feature="trace", trace)]
	fn get_loc(&self, index: usize) -> RunscriptLocation {
		let (line, column) = self.line_indices.iter()
			.enumerate()
			.find_map(|(line, &end)| if end > index {
				Some((line + 1, index - if line == 0 { 0 } else { self.line_indices[line - 1] }))
			} else {
				None
			})
			.unwrap_or_else(|| (self.line_indices.len(), index - self.line_indices.last().copied().unwrap_or(0)));
		RunscriptLocation {
			line,
			column,
		}
	}
}

#[cfg_attr(feature="trace", trace)]
pub fn parse_runscript(source: RunscriptSource) -> Result<Runscript, RunscriptParseError> {
	let mut context = ParsingContext {
		iterator: source.source.char_indices().peekable(),
		line_indices: source.source.char_indices().filter_map(|(index, ch)| if ch == '\n' { Some(index) } else { None }).collect(),
		runfile: Runscript {
			name: (&source.path).strip_prefix(&source.dir).unwrap() //TODO: Something other than unwrap
					.to_string_lossy().into_owned(), 
			source: source.source.clone(),
			includes: Vec::new(),
			scripts: Scripts {
				targets: IndexMap::new(),
			},
			options: Vec::new(),
		},
	};
	match parse_root(&mut context, &source) {
		Ok(()) => Ok(context.runfile),
		Err(data) => Err(RunscriptParseError { script: context.runfile, data })
	}
}

#[cfg_attr(feature="trace", trace)]
fn parse_root<T: Iterator<Item = (usize, char)> + std::fmt::Debug>(context: &mut ParsingContext<T>, source: &RunscriptSource) -> Result<(), RunscriptParseErrorData> {
	while let Some(tk) = context.iterator.next() { match tk {
		// Comment
		(_, '!') => {
			consume_line(&mut context.iterator);
		},
		// Annotation (Better name?)
		(i, '$') => {
			let (special, bk) = consume_word(&mut context.iterator);

			if matches!(bk, BreakCondition::EOF) {
				return Err(RunscriptParseErrorData::UnexpectedEOF { location: context.get_loc(i + 1), expected: "include".to_owned() });
			}

			match &*special {
				"include" => {
					return Err(RunscriptParseErrorData::UnsupportedFeature { location: context.get_loc(i + 1), msg: "Includes are not supported".to_owned() });
				},
				"opt" => {
					context.runfile.options.push(consume_line(&mut context.iterator).0);
				}
				_ => return Err(RunscriptParseErrorData::UnexpectedToken { location: context.get_loc(i + 1), found: special, expected: "include".to_owned() })
			}
		},
		// Script
		(i, '#') => {
			let (name, bk) = consume_word(&mut context.iterator);

			// Currently, b!, b => "build", br, r, r! => "run"
			let (phase, bk) = match bk {
				BreakCondition::Newline(i) => ("run", BreakCondition::Newline(i)),
				_ => match context.iterator.next() {
					Some((i, '\n')) => ("run", BreakCondition::Newline(i)),
					Some((_, 'b')) => match context.iterator.next() {
						Some((_, '!')) => ("build", BreakCondition::Parse),
						Some((_, 'r')) => ("run", BreakCondition::Parse),
						Some((i, '\n')) => ("build", BreakCondition::Newline(i)),
						_ => ("build", BreakCondition::Parse),
					},
					Some((_, 'r')) => match context.iterator.next() {
						Some((_, '!')) => ("run", BreakCondition::Parse),
						Some((i, '\n')) => ("run", BreakCondition::Newline(i)),
						_ => ("run", BreakCondition::Parse),
					},
					Some(_) => todo!(),
					None => todo!(),
				},
			};

			if !matches!(bk, BreakCondition::Newline(_)) {
				consume_line(&mut context.iterator);
			}

            let location = context.get_loc(i);
			let script = Script {
				location,
				commands: parse_commands(context)?,
			};

			if name == "-" {
				if let Some(prev_script) = &context.runfile.scripts.targets.entry("default".to_owned()).or_default().insert(phase.to_owned(), script) {
					return Err(RunscriptParseErrorData::MultipleDefinition {
						previous_location: prev_script.location,
						new_location: location,
						target_name: name,
					});
				}
			} else if name == "#" || name == "<" {
				return Err(RunscriptParseErrorData::UnsupportedFeature { location: script.location, msg: "Global scripts are not supported".to_owned() });
			} else {
				if name.chars().any(|c| !(c.is_ascii_alphanumeric() || c == '_' || c == '-')) {
					return Err(RunscriptParseErrorData::InvalidID { location: context.get_loc(i + 1), found: name });
				}
				let target = context.runfile.scripts.targets.entry(name.clone()).or_default();
				if let Some(prev_script) = target.insert(phase.to_owned(), script) {
					return Err(RunscriptParseErrorData::MultipleDefinition {
						previous_location: prev_script.location,
						new_location: location,
						target_name: name,
					});
				}
			}
		},
		(_, ' ') | (_, '\n') | (_, '\r') | (_, '\t') => continue,
		_ => todo!(),
	}}
	
	Ok(())
}

#[cfg_attr(feature="trace", trace)]
fn parse_commands<T: Iterator<Item = (usize, char)> + std::fmt::Debug>(context: &mut ParsingContext<T>) -> Result<Vec<AtomicTopLevelCommand<String>>, RunscriptParseErrorData> {
	let mut cmds = Vec::new();
    let eof_loc = context.get_loc(context.runfile.source.len());
	
    let lexer = Lexer::new((&mut context.iterator).map(|(_, ch)| ch));
    let mut parser = Parser::<_, AtomicDefaultBuilder<String>>::new(lexer);
    loop {
        if let Some(Newline(Some(comment))) = parser.newline() {
            if comment == "#/" {
                break;
            }
        }
        cmds.push(parser.complete_command()?.ok_or(RunscriptParseErrorData::UnexpectedEOF { location: eof_loc, expected: "#/".to_owned() })?);
    }
    
	Ok(cmds)
}


#[derive(Debug)]
enum BreakCondition {
	Newline(usize),
	EOF,
	Parse,
	Punctuation(char),
}

#[cfg_attr(feature="trace", trace)]
fn consume_word(iterator: &mut (impl Iterator<Item = (usize, char)> + std::fmt::Debug)) -> (String, BreakCondition) {
	let mut buf = String::new();
	let nl = loop {
		match iterator.next() {
			Some((i, '\n')) => break BreakCondition::Newline(i),
			Some((_, ' ')) | Some((_, '\t')) => break BreakCondition::Parse,
			Some((_, '\r')) => continue,
			Some((_, c)) => buf.push(c),
			None => break BreakCondition::EOF,
		}
	};
	(buf, nl)
}

#[cfg_attr(feature="trace", trace)]
fn consume_word_break_punctuation(iterator: &mut (impl Iterator<Item = (usize, char)> + std::fmt::Debug), brk: &[char]) -> (String, BreakCondition) {
	let mut buf = String::new();
	let nl = loop {
		match iterator.next() {
			Some((i, '\n')) => break BreakCondition::Newline(i),
			Some((_, ' ')) | Some((_, '\t')) => break BreakCondition::Parse,
			Some((_, '\r')) => continue,
			Some((_, c)) if brk.contains(&c) => break BreakCondition::Punctuation(c),
			Some((_, c)) => buf.push(c),
			None => break BreakCondition::EOF,
		}
	};
	(buf, nl)
}

#[cfg_attr(feature="trace", trace)]
fn consume_line(iterator: &mut (impl Iterator<Item = (usize, char)> + std::fmt::Debug)) -> (String, BreakCondition) {
	let mut buf = String::new();
	let bk = loop {
		match iterator.next() {
			Some((i, '\n')) => break BreakCondition::Newline(i),
			Some((_, '\r')) => continue,
			Some((_, c)) => buf.push(c),
			None => break BreakCondition::EOF, //TODO: Get an index for this
		}
	};
	(buf, bk)
}