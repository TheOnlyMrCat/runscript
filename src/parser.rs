use std::collections::HashMap;
use std::path::PathBuf;

use enum_map::EnumMap;

use crate::script::*;

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

#[derive(Clone, Debug)]
pub struct RunscriptLocation {
	/// The include-nesting index of this runscript
	pub index: Vec<usize>,
	/// The line of the referenced code
	pub line: usize,
	/// The column of the referenced code
	pub column: usize,
}

pub enum ParseOrIOError {
	IOError(std::io::Error),
	ParseError(RunscriptParseError),
}

pub struct RunscriptParseError {
	script: Runscript,
	data: RunscriptParseErrorData,
}

pub enum RunscriptParseErrorData {
	UnexpectedEOF {
		location: RunscriptLocation,
		expected: String,
	},
	UnexpectedToken {
		location: RunscriptLocation,
		found: String,
		expected: String,
	},
	InvalidID {
		location: RunscriptLocation,
		found: String,
	},
	BadInclude {
		location: RunscriptLocation,
		file_err: std::io::Error,
		dir_err: std::io::Error,
	},
	NestedError {
		include_location: RunscriptLocation,
		error: Box<RunscriptParseError>,
	},
	MultipleDefinition {
		previous_location: RunscriptLocation,
		new_location: RunscriptLocation,
		target_name: String,
	}
}

pub fn parse_runfile(path: impl Into<PathBuf>) -> Result<Runscript, ParseOrIOError> {
	let path = path.into();
	parse_runscript(RunscriptSource {
		source: std::fs::read_to_string(&path).map_err(|e| ParseOrIOError::IOError(e))?,
		base: path.parent().expect("Runfile has no parent!").to_owned(),
		file: path,
		index: Vec::new(),
	}).map_err(|e| ParseOrIOError::ParseError(e))
}

pub fn parse_runfile_nested<'a>(path: impl Into<PathBuf>, base: impl Into<PathBuf>, index: Vec<usize>) -> Result<Runscript, ParseOrIOError> {
	let path = path.into();
	parse_runscript(RunscriptSource {
		source: std::fs::read_to_string(&path).map_err(|e| ParseOrIOError::IOError(e))?,
		file: path.into(),
		base: base.into(),
		index: Vec::new(),
	}).map_err(|e| ParseOrIOError::ParseError(e))
}

struct ParsingContext<'a, T: Iterator<Item = (usize, char)>> {
	iterator: std::iter::Peekable<T>,
	runfile: Runscript,
	index: &'a Vec<usize>,
	line_indices: Vec<usize>,
}

impl<T: Iterator<Item = (usize, char)>> ParsingContext<'_, T> {
	fn get_loc(&self, index: usize) -> RunscriptLocation {
		let (line, column) = self.line_indices.iter().enumerate().find_map(|(line, &end)| if end > index { Some((line, index - end)) } else { None }).unwrap_or((self.line_indices.len(), index - self.line_indices.last().copied().unwrap_or(0)));
		RunscriptLocation {
			index: self.index.clone(),
			line,
			column,
		}
	}
}

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

pub fn parse_root<T: Iterator<Item = (usize, char)>>(context: &mut ParsingContext<T>, source: &RunscriptSource) -> Result<(), RunscriptParseErrorData> {
	while let Some(tk) = context.iterator.next() { match tk {
		// Comment
		(_, '!') => {
			consume_line(&context.iterator);
		},
		// Annotation (Better name?)
		(i, '$') => {
			let (special, bk) = consume_word(&context.iterator);

			if matches!(bk, BreakCondition::EOF) {
				return Err(RunscriptParseErrorData::UnexpectedEOF { location: context.get_loc(i + 1), expected: "include".to_owned() });
			}

			match &*special {
				"include" => {
					let (included, bk) = consume_word(&context.iterator);
					let index = {
						let v = source.index.clone();
						v.push(context.runfile.includes.len());
						v
					};

					if !matches!(bk, BreakCondition::Newline(_)) {
						consume_line(&context.iterator);
					}

					let file_branch = {
						let st = included.clone();
						st.push_str(".run");
						st
					};
					let file_path = source.base.join(file_branch);
					match parse_runfile_nested(file_path, source.base, index.clone()) {
						Ok(runscript) => {
							context.runfile.includes.push(RunscriptInclude {
								runscript,
								location: context.get_loc(i),
							});
						}
						Err(ParseOrIOError::IOError(fe)) => {
							let dir_path = {
								let p = source.base.join(&included);
								p.push("run");
								p
							};
							match parse_runfile_nested(dir_path, source.base, index) {
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
			let (name, bk) = consume_word(&context.iterator);

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
					}
					Some((_, 'r')) => match context.iterator.next() {
						Some((_, '!')) => (ScriptPhase::RunOnly, BreakCondition::Parse),
						Some((i, '\n')) => (ScriptPhase::Run, BreakCondition::Newline(i)),
						_ => (ScriptPhase::Run, BreakCondition::Parse),
					}
				},
			};

			if !matches!(bk, BreakCondition::Newline(_)) {
				consume_line(&context.iterator);
			}

			let script = Script {
				location: context.get_loc(i),
				commands: parse_commands(&context)?,
			};

			if name == "-" {
				if let Some(prev_script) = context.runfile.scripts.default_target[phase] {
					return Err(RunscriptParseErrorData::MultipleDefinition {
						previous_location: prev_script.location,
						new_location: script.location,
						target_name: name,
					});
				}
				context.runfile.scripts.default_target[phase] = Some(script);
			} else if name == "#" {
				if let Some(prev_script) = context.runfile.scripts.default_target[phase] {
					return Err(RunscriptParseErrorData::MultipleDefinition {
						previous_location: prev_script.location,
						new_location: script.location,
						target_name: name,
					});
				}
				context.runfile.scripts.global_target[phase] = Some(script);
			} else {
				if name.chars().any(|c| !(c.is_ascii_alphanumeric() || c == '_')) {
					return Err(RunscriptParseErrorData::InvalidID { location: context.get_loc(i + 1), found: name });
				}
				let target = context.runfile.scripts.targets.entry(name).or_insert_with(EnumMap::new);
				if let Some(prev_script) = target[phase] {
					return Err(RunscriptParseErrorData::MultipleDefinition {
						previous_location: prev_script.location,
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

fn parse_commands<T: Iterator<Item = (usize, char)>>(context: &ParsingContext<T>) -> Result<Vec<Command>, RunscriptParseErrorData> {
	let mut cmds = Vec::new();
	while let Some(cmd) = parse_command(context, ChainedCommand::None)? {
		// parse_command returns None when the first 'word' it encounters is `#/`
		cmds.push(cmd);
	}
	Ok(cmds)
}

fn parse_command<T: Iterator<Item = (usize, char)>>(context: &ParsingContext<T>, chained: ChainedCommand) -> Result<Option<Command>, RunscriptParseErrorData> {
	let (start_loc, _) = context.iterator.peek().ok_or_else(|| RunscriptParseErrorData::UnexpectedEOF { location: context.get_loc(context.runfile.source.len()), expected: "#/".to_string() })?;
	let (target, bk) = consume_word(&context.iterator);
	if target == "#/" {
		if !matches!(bk, BreakCondition::Newline(_)) {
			consume_line(&context.iterator);
		}
		return Ok(None);
	}

	let args = Vec::new();
	loop {
		match context.iterator.peek() {
			Some((_, '\'')) => {
				context.iterator.next();
				let mut buf = String::new();
				loop {
					match context.iterator.peek() {
						Some((_, '\'')) => break,
						Some((_, c)) => buf.push(*c),
						None => return Err(RunscriptParseErrorData::UnexpectedEOF { location: context.get_loc(context.runfile.source.len()), expected: "`'`".to_owned() }),
					}
				}
				args.push(Argument::Single(buf));
			},
			Some((_, '"')) => {
				//TODO
				context.iterator.next();
				let mut buf = String::new();
				loop {
					match context.iterator.peek() {
						Some((_, '\'')) => break,
						Some((_, c)) => buf.push(*c),
						None => return Err(RunscriptParseErrorData::UnexpectedEOF { location: context.get_loc(context.runfile.source.len()), expected: "`'`".to_owned() }),
					}
				}
				args.push(Argument::Single(buf));
			},
			Some((_, ' ')) | Some((_, '\t')) => { context.iterator.next(); }
			Some((_, '$')) => {
				context.iterator.next();
				match context.iterator.peek() {
					Some((_, '(')) => todo!(),
					Some((_, c)) if c.is_digit(10) => {
						let mut acc = *c as usize - '0' as usize;
						loop {
							let (_, c) = context.iterator.peek().ok_or_else(|| RunscriptParseErrorData::UnexpectedEOF { location: context.get_loc(context.runfile.source.len()), expected: "#/".to_owned() })?;
							match c.to_digit(10) {
								Some(i) => {
									context.iterator.next();
									acc = acc * 10 + i as usize;
								},
								None => break,
							}
						}
						args.push(Argument::Unquoted(ArgPart::Arg(acc)))
					}
				}
			},
			Some(_) => {
				let (s, bk) = consume_word(&context.iterator);
				args.push(Argument::Unquoted(ArgPart::Str(s)));
				if matches!(bk, BreakCondition::Newline(_)) {
					break;
				}
			},
			None => {
				return Err(RunscriptParseErrorData::UnexpectedEOF { location: context.get_loc(context.runfile.source.len()), expected: "#/".to_owned() });
			}
		}
	}

	Ok(Some(Command {
		target,
		args,
		chained: Box::new(chained),
		loc: context.get_loc(*start_loc),
	}))
}

enum BreakCondition {
	Newline(usize),
	EOF,
	Parse,
	CloseParen,
}

fn consume_word(iterator: &impl Iterator<Item = (usize, char)>) -> (String, BreakCondition) {
	let mut buf = String::new();
	let nl = loop {
		match iterator.next() {
			Some((i, '\n')) => break BreakCondition::Newline(i),
			Some((_, ' ')) | Some((_, '\t')) => break BreakCondition::Parse,
			Some((_, ')')) => break BreakCondition::CloseParen,
			Some((_, '\r')) => continue,
			Some((_, c)) => buf.push(c),
			None => break BreakCondition::EOF,
		}
	};
	(buf, nl)
}

fn consume_line(iterator: &impl Iterator<Item = (usize, char)>) -> BreakCondition {
	loop {
		match iterator.next() {
			Some((i, '\n')) => break BreakCondition::Newline(i),
			None => break BreakCondition::EOF, //TODO: Get an index for this
			_ => continue,
		}
	}
}