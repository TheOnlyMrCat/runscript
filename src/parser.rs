use std::collections::HashMap;
use std::ops::Range;
use std::path::Path;

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
	/// The location of the referenced code
	range: Range<usize>,
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

struct ParsingContext<T: Iterator<Item = (usize, char)>> {
	iterator: std::iter::Peekable<T>,
	runfile: Runscript,
}

pub fn parse_runscript(source: RunscriptSource) -> Result<Runscript, RunscriptParseError> {
	let mut context = ParsingContext {
		iterator: source.source.char_indices().peekable(),
		runfile: Runscript {
			name: source.file.strip_prefix(source.base).unwrap() //TODO: Something other than unwrap
					.to_string_lossy().into_owned(), 
			line_ends: source.source.char_indices().filter_map(|(index, ch)| if ch == '\n' { Some(index) } else { None }).collect(),
			source: source.source,
			includes: Vec::new(),
			scripts: Scripts {
				global_target: HashMap::new(),
				default_target: HashMap::new(),
				targets: HashMap::new(),
			}
		}
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
					let (included, nl) = consume_word(&context.iterator);
					let index = {
						let v = source.index.clone();
						v.push(context.runfile.includes.len());
						v
					};

					let nl = if nl == 0 {
						consume_line(&context.iterator)
					} else {
						nl
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
									range: i..nl,
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
											range: i..nl,
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
											range: i..nl,
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
									range: i..nl,
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
			let (name, nl) = consume_word(&context.iterator);

			// Don't ask...
			let (phase, nl) = match nl {
				0 => match context.iterator.next() {
					Some((i, '\n')) => (ScriptPhase::BuildAndRun, i),
					Some((_, 'b')) => match context.iterator.next() {
						Some((_, '!')) => (ScriptPhase::BuildOnly, 0),
						Some((_, 'r')) => (ScriptPhase::BuildAndRun, 0),
						Some((i, '\n')) => (ScriptPhase::Build, i),
						_ => (ScriptPhase::Build, 0),
					}
					Some((_, 'r')) => match context.iterator.next() {
						Some((_, '!')) => (ScriptPhase::RunOnly, 0),
						Some((i, '\n')) => (ScriptPhase::Run, i),
						_ => (ScriptPhase::Run, 0),
					}
				},
				i => (ScriptPhase::BuildAndRun, i),
			};

			if nl == 0 {
				consume_line(&context.iterator);
			}

			let script = Script {
				location: RunscriptLocation {
					index: source.index,
					range: i..nl
				},
				commands: parse_commands(&context),
			};

			if name == "-" {
				context.runfile.scripts.default_target.insert(phase, script);
			} else if name == "#" {
				context.runfile.scripts.global_target.insert(phase, script);
			} else {
				if name.chars().any(|c| !(c.is_ascii_alphanumeric() || c == '_')) {
					return Err(RunscriptParseError::InvalidID { found: name });
				}
				context.runfile.scripts.targets.insert(Target { name, phase }, script);
			}
		},
		(_, ' ') | (_, '\n') | (_, '\r') | (_, '\t') => continue,
	}}
	
	Ok(context.runfile)
}

fn parse_commands<T: Iterator<Item = (usize, char)>>(context: &ParsingContext<T>) -> Vec<Command> {
	//TODO
	vec![]
}

fn consume_word(iterator: &impl Iterator<Item = (usize, char)>) -> (String, usize) {
	let mut special = String::new();
	let nl = loop {
		match iterator.next() {
			Some((i, '\n')) => break i,
			Some((_, ' ')) | Some((_, '\t')) => break 0,
			Some((_, '\r')) => continue,
			Some((_, c)) => special.push(c),
			None => break 0,
		}
	};
	(special, nl)
}

fn consume_line(iterator: &impl Iterator<Item = (usize, char)>) -> usize {
	loop {
		match iterator.next() {
			Some((i, '\n')) => break i,
			None => break 0, //TODO: Get an index for this
			_ => continue,
		}
	}
}