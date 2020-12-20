use std::collections::HashMap;
use std::path::PathBuf;

use enum_map::EnumMap;

#[cfg(feature="trace")]
use crate::DEPTH;
use crate::script::*;

/// Source code of a runscript which can be consumed by [`parse_runscript`](fn.parse_runscript.html)
#[derive(Debug)]
pub struct RunscriptSource {
	/// The name of the runfile this runscript belongs to, for location-tracking purposes.
	pub file: PathBuf,
	/// The directory the runfile's name should be considered relative to
	pub base: PathBuf,
	/// The include-nesting index of this runscript
	pub index: Vec<usize>,
	/// The runscript code to be parsed. Generally the contents of a runfile.
	pub source: String,
}

/// A location in a runscript
/// 
/// Runscript locations include the line and column number, as well as an index used to track include-nesting
#[derive(Clone, Debug)]
pub struct RunscriptLocation {
	/// The include-nesting index of this runscript
	pub index: Vec<usize>,
	/// The line of the referenced code
	pub line: usize,
	/// The column of the referenced code
	pub column: usize,
}

/// An error from a parse function which involves an I/O operation
/// 
/// This enum exists only to propagate [`std::io::Error`](https://doc.rust-lang.org/std/io/struct.Error.html)s to the
/// calling function without including it in the main [`RunscriptParseError`](struct.RunscriptParseError)
#[derive(Debug)]
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
	/// Tried to include a runfile but couldn't access the file
	BadInclude {
		location: RunscriptLocation,
		/// The I/O error which occurred when trying to access `path/to/file.run`
		file_err: std::io::Error,
		/// The I/O error which occurred when trying to access `path/to/dir/run`
		dir_err: std::io::Error,
	},
	/// An error occurred in a nested runscript
	NestedError {
		include_location: RunscriptLocation,
		error: Box<RunscriptParseError>,
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
	}
}

pub fn parse_runfile(path: impl Into<PathBuf>) -> Result<Runscript, ParseOrIOError> {
	let path = path.into();
	parse_runscript(RunscriptSource {
		source: std::fs::read_to_string(&path).map_err(ParseOrIOError::IOError)?,
		base: path.parent().expect("Runfile has no parent!").to_owned(),
		file: path,
		index: Vec::new(),
	}).map_err(ParseOrIOError::ParseError)
}

pub fn parse_runfile_nested(path: impl Into<PathBuf>, base: impl Into<PathBuf>, index: Vec<usize>) -> Result<Runscript, ParseOrIOError> {
	let path = path.into();
	parse_runscript(RunscriptSource {
		source: std::fs::read_to_string(&path).map_err(ParseOrIOError::IOError)?,
		file: path,
		base: base.into(),
		index,
	}).map_err(ParseOrIOError::ParseError)
}

#[derive(Debug)]
struct ParsingContext<'a, T: Iterator<Item = (usize, char)> + std::fmt::Debug> {
	iterator: std::iter::Peekable<T>,
	runfile: Runscript,
	index: &'a Vec<usize>,
	line_indices: Vec<usize>,
}

impl<T: Iterator<Item = (usize, char)> + std::fmt::Debug> ParsingContext<'_, T> {

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
			index: self.index.clone(),
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
			name: (&source.file).strip_prefix(&source.base).unwrap() //TODO: Something other than unwrap
					.to_string_lossy().into_owned(), 
			source: source.source.clone(),
			includes: Vec::new(),
			scripts: Scripts {
				global_target: EnumMap::new(),
				default_target: EnumMap::new(),
				targets: HashMap::new(),
			}
		},
		index: &source.index,
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
					let (included, bk) = consume_word(&mut context.iterator);
					let index = {
						let mut v = source.index.clone();
						v.push(context.runfile.includes.len());
						v
					};

					if !matches!(bk, BreakCondition::Newline(_)) {
						consume_line(&mut context.iterator);
					}

					let file_branch = {
						let mut st = included.clone();
						st.push_str(".run");
						st
					};
					let file_path = source.base.join(file_branch);
					match parse_runfile_nested(file_path, source.base.clone(), index.clone()) {
						Ok(runscript) => {
							context.runfile.includes.push(RunscriptInclude {
								runscript,
								location: context.get_loc(i),
							});
						}
						Err(ParseOrIOError::IOError(fe)) => {
							let dir_path = {
								let mut p = source.base.join(&included);
								p.push("run");
								p
							};
							match parse_runfile_nested(dir_path, source.base.clone(), index) {
								Ok(runscript) => {
									context.runfile.includes.push(RunscriptInclude {
										runscript,
										location: context.get_loc(i),
									});
								},
								Err(ParseOrIOError::IOError(de)) => {
									return Err(RunscriptParseErrorData::BadInclude {
										location: context.get_loc(i),
										file_err: fe,
										dir_err: de,
									})
								},
								Err(ParseOrIOError::ParseError(e)) => {
									return Err(RunscriptParseErrorData::NestedError {
										include_location: context.get_loc(i),
										error: Box::new(e),
									})
								}
							}
						},
						Err(ParseOrIOError::ParseError(e)) => {
							return Err(RunscriptParseErrorData::NestedError {
								include_location: context.get_loc(i),
								error: Box::new(e),
							})
						}
					}
				},
				_ => return Err(RunscriptParseErrorData::UnexpectedToken { location: context.get_loc(i + 1), found: special, expected: "include".to_owned() })
			}
		},
		// Script
		(i, '#') => {
			let (name, bk) = consume_word(&mut context.iterator);

			// Don't ask...
			let (phase, bk) = match bk {
				BreakCondition::Newline(i) => (ScriptPhase::BuildAndRun, BreakCondition::Newline(i)),
				_ => match context.iterator.next() {
					Some((i, '\n')) => (ScriptPhase::BuildAndRun, BreakCondition::Newline(i)),
					Some((_, 'b')) => match context.iterator.next() {
						Some((_, '!')) => (ScriptPhase::BuildOnly, BreakCondition::Parse),
						Some((_, 'r')) => (ScriptPhase::BuildAndRun, BreakCondition::Parse),
						Some((i, '\n')) => (ScriptPhase::Build, BreakCondition::Newline(i)),
						_ => (ScriptPhase::Build, BreakCondition::Parse),
					},
					Some((_, 'r')) => match context.iterator.next() {
						Some((_, '!')) => (ScriptPhase::RunOnly, BreakCondition::Parse),
						Some((i, '\n')) => (ScriptPhase::Run, BreakCondition::Newline(i)),
						_ => (ScriptPhase::Run, BreakCondition::Parse),
					},
					Some(_) => todo!(),
					None => todo!(),
				},
			};

			if !matches!(bk, BreakCondition::Newline(_)) {
				consume_line(&mut context.iterator);
			}

			let script = Script {
				location: context.get_loc(i),
				commands: parse_commands(context)?,
			};

			if name == "-" {
				if let Some(prev_script) = &context.runfile.scripts.default_target[phase] {
					return Err(RunscriptParseErrorData::MultipleDefinition {
						previous_location: prev_script.location.clone(),
						new_location: script.location,
						target_name: name,
					});
				}
				context.runfile.scripts.default_target[phase] = Some(script);
			} else if name == "#" {
				if let Some(prev_script) = &context.runfile.scripts.global_target[phase] {
					return Err(RunscriptParseErrorData::MultipleDefinition {
						previous_location: prev_script.location.clone(),
						new_location: script.location,
						target_name: name,
					});
				}
				context.runfile.scripts.global_target[phase] = Some(script);
			} else {
				if name.chars().any(|c| !(c.is_ascii_alphanumeric() || c == '_' || c == '-')) {
					return Err(RunscriptParseErrorData::InvalidID { location: context.get_loc(i + 1), found: name });
				}
				let target = context.runfile.scripts.targets.entry(name.clone()).or_insert_with(EnumMap::new);
				if let Some(prev_script) = &target[phase] {
					return Err(RunscriptParseErrorData::MultipleDefinition {
						previous_location: prev_script.location.clone(),
						new_location: script.location,
						target_name: name,
					});
				}
				target[phase] = Some(script);
			}
		},
		(_, ' ') | (_, '\n') | (_, '\r') | (_, '\t') => continue,
		_ => todo!(),
	}}
	
	Ok(())
}

#[cfg_attr(feature="trace", trace)]
fn parse_commands<T: Iterator<Item = (usize, char)> + std::fmt::Debug>(context: &mut ParsingContext<T>) -> Result<Vec<ScriptEntry>, RunscriptParseErrorData> {
	let mut cmds = Vec::new();
	while let Some(cmd) = parse_command(context, ChainedCommand::None, None)? {
		// parse_command returns None when the first 'word' it encounters is `#/`
		cmds.push(cmd);
	}
	Ok(cmds)
}

#[cfg_attr(feature="trace", trace)]
fn parse_command<T: Iterator<Item = (usize, char)> + std::fmt::Debug>(context: &mut ParsingContext<T>, chained: ChainedCommand, nest_terminate: Option<char>) -> Result<Option<ScriptEntry>, RunscriptParseErrorData> {
	let end_loc = context.get_loc(context.runfile.source.len());
	let start_loc = loop {
		match context.iterator.peek() {
			Some((_, ' ')) | Some((_, '\t')) | Some((_, '\n')) => {
				context.iterator.next();
			},
			Some((_, '#')) => {
				context.iterator.next();
				if let Some((_, '/')) = context.iterator.next() {
					return Ok(None);
				} else {
					consume_line(&mut context.iterator);
				}
			},
			Some((i, _)) => {
				break *i;
			},
			None => {
				return Err(RunscriptParseErrorData::UnexpectedEOF { location: end_loc, expected: "#/".to_string() })
			}
		}
	};
	let (target, bk) = match context.iterator.peek() {
		Some((_, '\'')) => {
			context.iterator.next();
			let mut buf = String::new();
			loop {
				match context.iterator.next() {
					Some((_, '\'')) => break,
					Some((_, c)) => buf.push(c),
					None => return Err(RunscriptParseErrorData::UnexpectedEOF { location: end_loc, expected: "`'`".to_owned() }),
				}
			}
			(Argument::Single(buf), BreakCondition::Parse)
		},
		Some((_, '"')) => {
			context.iterator.next();
			let mut acc = Vec::new();
			let mut buf = String::new();
			loop {
				match context.iterator.next() {
					Some((_, '"')) => break,
					Some((_, '$')) => {
						if !buf.is_empty() {
							acc.push(ArgPart::Str(std::mem::replace(&mut buf, String::new())))
						}
						acc.push(parse_interpolate(context)?);
					}
					Some((_, c)) => buf.push(c),
					None => return Err(RunscriptParseErrorData::UnexpectedEOF { location: end_loc, expected: "`\"`".to_owned() }),
				}
			}
			if !buf.is_empty() {
				acc.push(ArgPart::Str(buf))
			}
			(Argument::Double(acc), BreakCondition::Parse)
		},
		Some((_, '$')) => {
			context.iterator.next();
			(Argument::Unquoted(parse_interpolate(context)?), BreakCondition::Parse)
		},
		Some(_) => {
			let (s, bk) = match nest_terminate {
				Some(c) => consume_word_break_punctuation(&mut context.iterator, &[c, '=']),
				None => consume_word_break_punctuation(&mut context.iterator, &['=']),
			};
			(Argument::Unquoted(ArgPart::Str(s)), bk)
		},
		None => return Err(RunscriptParseErrorData::UnexpectedEOF { location: end_loc, expected: "#/".to_owned() })
	};

	if matches!(bk, BreakCondition::Punctuation('=')) {
		let (var_argument, bk) = match context.iterator.peek() {
			Some((_, '\'')) => {
				context.iterator.next();
				let mut buf = String::new();
				loop {
					match context.iterator.next() {
						Some((_, '\'')) => break,
						Some((_, c)) => buf.push(c),
						None => return Err(RunscriptParseErrorData::UnexpectedEOF { location: end_loc, expected: "`'`".to_owned() }),
					}
				}
				(Argument::Single(buf), BreakCondition::Parse)
			},
			Some((_, '"')) => {
				context.iterator.next();
				let mut acc = Vec::new();
				let mut buf = String::new();
				loop {
					match context.iterator.next() {
						Some((_, '"')) => break,
						Some((_, '$')) => {
							if !buf.is_empty() {
								acc.push(ArgPart::Str(std::mem::replace(&mut buf, String::new())))
							}
							acc.push(parse_interpolate(context)?);
						}
						Some((_, c)) => buf.push(c),
						None => return Err(RunscriptParseErrorData::UnexpectedEOF { location: end_loc, expected: "`\"`".to_owned() }),
					}
				}
				if !buf.is_empty() {
					acc.push(ArgPart::Str(buf))
				}
				(Argument::Double(acc), BreakCondition::Parse)
			},
			Some((_, '$')) => {
				context.iterator.next();
				(Argument::Unquoted(parse_interpolate(context)?), BreakCondition::Parse)
			},
			Some(_) => { // This also captures newlines, which causes it to return an empty string. This is expected behaviour.
				let (s, bk) = match nest_terminate {
					Some(c) => consume_word_break_punctuation(&mut context.iterator, &[c]),
					None => consume_word(&mut context.iterator),
				};
				(Argument::Unquoted(ArgPart::Str(s)), bk)
			},
			None => return Err(RunscriptParseErrorData::UnexpectedEOF { location: end_loc, expected: "#/".to_owned() })
		};
		if !matches!(bk, BreakCondition::Newline(_)) {
			consume_line(&mut context.iterator);
		}
		return Ok(Some(ScriptEntry::Env {
			var: match target { Argument::Unquoted(ArgPart::Str(s)) => s, a => panic!("Expected env var to be a string but was {}", a) },
			val: var_argument,
			loc: context.get_loc(start_loc),
		}));
	}

	let mut command = Command {
		target: Box::new(target),
		args: Vec::new(),
		chained: Box::new(chained),
		loc: context.get_loc(start_loc),
	};

	if matches!(bk, BreakCondition::Newline(_)) {
		return Ok(Some(ScriptEntry::Command(command)));
	}

	loop {
		match context.iterator.peek() {
			Some((_, '\'')) => {
				context.iterator.next();
				let mut buf = String::new();
				loop {
					match context.iterator.next() {
						Some((_, '\'')) => break,
						Some((_, c)) => buf.push(c),
						None => return Err(RunscriptParseErrorData::UnexpectedEOF { location: end_loc, expected: "`'`".to_owned() }),
					}
				}
				command.args.push(Argument::Single(buf));
			},
			Some((_, '"')) => {
				context.iterator.next();
				let mut acc = Vec::new();
				let mut buf = String::new();
				loop {
					match context.iterator.next() {
						Some((_, '"')) => break,
						Some((_, '$')) => {
							if !buf.is_empty() {
								acc.push(ArgPart::Str(std::mem::replace(&mut buf, String::new())))
							}
							acc.push(parse_interpolate(context)?);
						}
						Some((_, c)) => buf.push(c),
						None => return Err(RunscriptParseErrorData::UnexpectedEOF { location: end_loc, expected: "`\"`".to_owned() }),
					}
				}
				if !buf.is_empty() {
					acc.push(ArgPart::Str(buf))
				}
				command.args.push(Argument::Double(acc));
			},
			Some((_, ' ')) | Some((_, '\t')) => { context.iterator.next(); }
			Some((_, '$')) => {
				context.iterator.next();
				command.args.push(Argument::Unquoted(parse_interpolate(context)?));
			},
			Some((_, '|')) => {
				context.iterator.next();
				match context.iterator.peek() {
					Some((i, '|')) => {
						let i = *i;
						context.iterator.next();
						return Ok(Some(parse_command(context, ChainedCommand::Or(command), nest_terminate)?.ok_or(RunscriptParseErrorData::UnexpectedToken { location: context.get_loc(i + 1), found: "#/".to_owned(), expected: "command".to_owned()})?));
					},
					Some((i, _)) => {
						let i = *i;
						return Ok(Some(parse_command(context, ChainedCommand::Pipe(command), nest_terminate)?.ok_or(RunscriptParseErrorData::UnexpectedToken { location: context.get_loc(i + 1), found: "#/".to_owned(), expected: "command".to_owned()})?));
					},
					None => {
						return Err(RunscriptParseErrorData::UnexpectedEOF { location: end_loc, expected: "command".to_owned() });
					}
				}
			},
			Some((_, '&')) => {
				context.iterator.next();
				match context.iterator.next() {
					Some((i, '&')) => {
						return Ok(Some(parse_command(context, ChainedCommand::And(command), nest_terminate)?.ok_or(RunscriptParseErrorData::UnexpectedToken { location: context.get_loc(i + 1), found: "#/".to_owned(), expected: "command".to_owned()})?));
					},
					Some((i, _)) => {
						return Err(RunscriptParseErrorData::ReservedToken { location: context.get_loc(i - 1), token: "&".to_owned(), reason: "Single `&` signifies background execution, which is unsupported.".to_owned()})
					},
					None => {
						return Err(RunscriptParseErrorData::UnexpectedEOF { location: end_loc, expected: "command".to_owned()});
					}
				}
			},
			Some((_, '\n')) => {
				context.iterator.next();
				break;
			}
			Some(_) => {
				let (s, bk) = match nest_terminate {
					Some(c) => consume_word_break_punctuation(&mut context.iterator, &[c]),
					None => consume_word(&mut context.iterator),
				};
				command.args.push(Argument::Unquoted(ArgPart::Str(s)));
				if matches!(bk, BreakCondition::Newline(_) | BreakCondition::Punctuation(_)) {
					break;
				}
			},
			None => {
				return Err(RunscriptParseErrorData::UnexpectedEOF { location: end_loc, expected: "#/".to_owned() });
			}
		}
	}

	Ok(Some(ScriptEntry::Command(command)))
}

#[cfg_attr(feature="trace", trace)]
fn parse_interpolate<T: Iterator<Item = (usize, char)> + std::fmt::Debug>(context: &mut ParsingContext<T>) -> Result<ArgPart, RunscriptParseErrorData> {
	let end_loc = context.get_loc(context.runfile.source.len());
	match context.iterator.peek() {
		Some((i, '(')) => {
			let i = *i;
			context.iterator.next();
			Ok(ArgPart::Cmd(
				parse_command(context, ChainedCommand::None, Some(')'))?
					.ok_or(RunscriptParseErrorData::UnexpectedToken { location: context.get_loc(i + 1), found: "#/".to_owned(), expected: "command".to_owned()})?
					.expect_command().ok_or(RunscriptParseErrorData::IllegalEnv { location: context.get_loc(i + 1), msg: "in interpolated command arguments".to_owned()})?
			))
		},
		Some((_, c)) if c.is_ascii_digit() => {
			let mut acc = *c as usize - '0' as usize;
			context.iterator.next();
			loop {
				let (_, c) = context.iterator.peek().ok_or(RunscriptParseErrorData::UnexpectedEOF { location: end_loc.clone(), expected: "#/".to_owned() })?;
				match c.to_digit(10) {
					Some(i) => {
						context.iterator.next();
						acc = acc * 10 + i as usize;
					},
					None => break,
				}
			}
			Ok(ArgPart::Arg(acc))
		},
		Some((_, c)) if c.is_ascii_alphabetic() || *c == '_' => {
			let mut buf = String::from(*c);
			context.iterator.next();
			loop {
				let (_, c) = context.iterator.peek().ok_or(RunscriptParseErrorData::UnexpectedEOF { location: end_loc.clone(), expected: "#/".to_owned() })?;
				if c.is_ascii_alphanumeric() || *c == '_' {
					buf.push(*c);
					context.iterator.next();
				} else {
					break;
				}
			}
			Ok(ArgPart::Var(buf))
		},
		Some((i, c)) => {
			let i = *i; let c = *c;
			Err(RunscriptParseErrorData::UnexpectedToken { location: context.get_loc(i), found: c.to_string(), expected: "environment variable".to_owned() })
		},
		None => Err(RunscriptParseErrorData::UnexpectedEOF { location: end_loc, expected: "environment variable".to_owned() })
	}
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
fn consume_line(iterator: &mut (impl Iterator<Item = (usize, char)> + std::fmt::Debug)) -> BreakCondition {
	loop {
		match iterator.next() {
			Some((i, '\n')) => break BreakCondition::Newline(i),
			None => break BreakCondition::EOF, //TODO: Get an index for this
			_ => continue,
		}
	}
}