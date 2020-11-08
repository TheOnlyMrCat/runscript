use std::collections::HashMap;
use std::path::Path;

use enum_map::EnumMap;

use crate::script::*;

pub struct RunscriptSource<'a> {
	/// The name of the runfile this runscript belongs to, for location-tracking purposes.
	file: &'a Path,
	/// The directory the runfile's name should be considered relative to
	base: &'a Path,
	/// The include-nesting index of this runscript
	index: Vec<usize>,
	/// The runscript code to be parsed. Generally the contents of a runfile.
	source: String,
}

#[derive(Clone, Debug)]
pub struct RunscriptLocation {
	/// The include-nesting index of this runscript
	index: Vec<usize>,
	/// The line of the referenced code
	line: usize,
}

pub enum RunscriptParseError {
	IOError(std::io::Error),
	UnexpectedEOF {
		expected: String,
	},
	UnexpectedToken {
		found: String,
		expected: String,
	},
	InvalidID {
		found: String,
	},
	BadInclude {
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
	}
}

pub fn parse_runfile(path: impl AsRef<Path>) -> Result<Runscript, RunscriptParseError> {
	parse_runscript(RunscriptSource {
		file: path.as_ref(),
		base: path.as_ref().parent().expect("Runfile has no parent!"),
		index: Vec::new(),
		source: std::fs::read_to_string(path).map_err(|e| RunscriptParseError::IOError(e))?,
	})
}

pub fn parse_runfile_nested(path: impl AsRef<Path>, base: impl AsRef<Path>, index: Vec<usize>) -> Result<Runscript, RunscriptParseError> {
	parse_runscript(RunscriptSource {
		file: path.as_ref(),
		base: base.as_ref(),
		index: Vec::new(),
		source: std::fs::read_to_string(path).map_err(|e| RunscriptParseError::IOError(e))?,
	})
}

struct ParsingContext<'a, T: Iterator<Item = (usize, char)>> {
	iterator: std::iter::Peekable<T>,
	runfile: Runscript,
	index: &'a Vec<usize>,
	line_indices: Vec<usize>,
}

impl<T: Iterator<Item = (usize, char)>> ParsingContext<'_, T> {
	fn get_line(&self, index: usize) -> usize {
		self.line_indices.iter().enumerate().find_map(|(line, &end)| if end > index { Some(line) } else { None }).unwrap_or(self.line_indices.len())
	}
}

pub fn parse_runscript(source: RunscriptSource) -> Result<Runscript, RunscriptParseError> {
	let mut context = ParsingContext {
		iterator: source.source.char_indices().peekable(),
		runfile: Runscript {
			name: source.file.strip_prefix(source.base).unwrap() //TODO: Something other than unwrap
					.to_string_lossy().into_owned(), 
			source: source.source,
			includes: Vec::new(),
			scripts: Scripts {
				global_target: EnumMap::new(),
				default_target: EnumMap::new(),
				targets: HashMap::new(),
			}
		},
		index: &source.index,
		line_indices: source.source.char_indices().filter_map(|(index, ch)| if ch == '\n' { Some(index) } else { None }).collect(),
	};

	while let Some(tk) = context.iterator.next() { match tk {
		// Comment
		(_, '!') => {
			consume_line(&context.iterator);
		},
		// Annotation (Better name?)
		(i, '$') => {
			let (special, nl) = consume_word(&context.iterator);

			match &*special {
				"include" => {
					let (included, bk) = consume_word(&context.iterator);
					let index = {
						let v = source.index.clone();
						v.push(context.runfile.includes.len());
						v
					};

					let nl = match bk {
						BreakCondition::Newline(i) => i,
						_ => consume_line(&context.iterator).get_newline_loc("include".to_owned())?,
					};

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
								location: RunscriptLocation {
									index: source.index,
									line: context.get_line(i),
								}
							});
						}
						Err(RunscriptParseError::IOError(fe)) => {
							let dir_path = {
								let p = source.base.join(&included);
								p.push("run");
								p
							};
							match parse_runfile_nested(dir_path, source.base, index) {
								Ok(runscript) => {
									context.runfile.includes.push(RunscriptInclude {
										runscript,
										location: RunscriptLocation {
											index: source.index,
											line: context.get_line(i),
										}
									});
								},
								Err(RunscriptParseError::IOError(de)) => {
									return Err(RunscriptParseError::BadInclude {
										file_err: fe,
										dir_err: de,
									})
								},
								Err(e) => {
									return Err(RunscriptParseError::NestedError {
										include_location: RunscriptLocation {
											index: source.index,
											line: context.get_line(i),
										},
										error: Box::new(e),
									})
								}
							}
						},
						Err(e) => {
							return Err(RunscriptParseError::NestedError {
								include_location: RunscriptLocation {
									index: source.index,
									line: context.get_line(i),
								},
								error: Box::new(e),
							})
						}
					}
				}
				"" => return Err(RunscriptParseError::UnexpectedEOF { expected: "include".to_owned() }),
				_ => return Err(RunscriptParseError::UnexpectedToken { found: special, expected: "include".to_owned() })
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

			let newline_loc = match bk {
				BreakCondition::Newline(i) => i,
				_ => consume_line(&context.iterator).get_newline_loc("script".to_owned())?,
			};

			let script = Script {
				location: RunscriptLocation {
					index: source.index,
					line: context.get_line(i),
				},
				commands: parse_commands(&context)?,
			};

			if name == "-" {
				if let Some(prev_script) = context.runfile.scripts.default_target[phase] {
					return Err(RunscriptParseError::MultipleDefinition {
						previous_location: prev_script.location,
						new_location: script.location,
					});
				}
				context.runfile.scripts.default_target[phase] = Some(script);
			} else if name == "#" {
				if let Some(prev_script) = context.runfile.scripts.default_target[phase] {
					return Err(RunscriptParseError::MultipleDefinition {
						previous_location: prev_script.location,
						new_location: script.location,
					});
				}
				context.runfile.scripts.global_target[phase] = Some(script);
			} else {
				if name.chars().any(|c| !(c.is_ascii_alphanumeric() || c == '_')) {
					return Err(RunscriptParseError::InvalidID { found: name });
				}
				let target = context.runfile.scripts.targets.entry(name).or_insert_with(EnumMap::new);
				if let Some(prev_script) = target[phase] {
					return Err(RunscriptParseError::MultipleDefinition {
						previous_location: prev_script.location,
						new_location: script.location,
					});
				}
				target[phase] = Some(script);
			}
		},
		(_, ' ') | (_, '\n') | (_, '\r') | (_, '\t') => continue,
	}}
	
	Ok(context.runfile)
}

fn parse_commands<T: Iterator<Item = (usize, char)>>(context: &ParsingContext<T>) -> Result<Vec<Command>, RunscriptParseError> {
	let mut cmds = Vec::new();
	while let Some(cmd) = parse_command(context, ChainedCommand::None)? {
		// parse_command returns None when the first 'word' it encounters is `#/`
		cmds.push(cmd);
	}
	Ok(cmds)
}

fn parse_command<T: Iterator<Item = (usize, char)>>(context: &ParsingContext<T>, chained: ChainedCommand) -> Result<Option<Command>, RunscriptParseError> {
	let (start_loc, _) = context.iterator.peek().ok_or_else(|| RunscriptParseError::UnexpectedEOF { expected: "#/".to_string() })?;
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
						None => return Err(RunscriptParseError::UnexpectedEOF { expected: "`'`".to_owned() }),
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
						None => return Err(RunscriptParseError::UnexpectedEOF { expected: "`'`".to_owned() }),
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
							let (_, c) = context.iterator.peek().ok_or_else(|| RunscriptParseError::UnexpectedEOF { expected: "#/".to_owned() })?;
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
				return Err(RunscriptParseError::UnexpectedEOF { expected: "#/".to_owned() });
			}
		}
	}

	Ok(Some(Command {
		target,
		args,
		chained: Box::new(chained),
		loc: RunscriptLocation {
			index: context.index.clone(),
			line: context.get_line(*start_loc),
		}
	}))
}

enum BreakCondition {
	Newline(usize),
	EOF,
	Parse,
	CloseParen,
}

impl BreakCondition {
	fn get_newline_loc(&self, expected: String) -> Result<usize, RunscriptParseError> {
		match self {
			BreakCondition::Newline(i) => Ok(*i),
			BreakCondition::EOF => Err(RunscriptParseError::UnexpectedEOF { expected }),
			_ => panic!("Expected to be called with Newline or EOF"),
		}
	}
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