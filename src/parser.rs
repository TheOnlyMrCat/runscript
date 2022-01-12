use std::collections::HashMap;
use std::path::PathBuf;
use std::str::CharIndices;

use conch_parser::ast::builder::{AtomicDefaultBuilder, Builder, DefaultBuilder};
use conch_parser::ast::AtomicTopLevelCommand;
use conch_parser::lexer::Lexer;
use conch_parser::parse::{ParseError, Parser};
use indexmap::IndexMap;

use crate::script::*;

/// Source code of a runscript which can be consumed by [`parse_runscript`](fn.parse_runscript.html)
#[derive(Debug, Clone)]
pub struct RunscriptSource {
    /// The name of the runfile this runscript belongs to, for location-tracking purposes.
    pub path: PathBuf,
    /// The directory the runfile's name should be considered relative to
    pub dir: PathBuf,
    /// The runscript code to be parsed. Generally the contents of a runfile.
    pub source: String,
}

/// A location in a runscript
///
/// Runscript locations include the line and column number, as well as an index used to track include-nesting
#[derive(Clone, Debug, Default)]
pub struct RunscriptLocation {
    /// The include-nesting index of this runscript
    pub index: Vec<usize>,
    /// The line of the referenced code
    pub line: usize,
    /// The column of the referenced code
    pub column: usize,
}

/// An error which occurred during parsing of a runscript
#[derive(Debug)]
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
    /// Found a name of a script which contains characters outside of `[A-Za-z_-]`
    InvalidValue {
        location: RunscriptLocation,
        /// The text of the identifier that triggered the error
        found: String,
        /// A description of what was expected instead of the `found` identifier
        expected: String,
    },
    /// Attempted to define the same script twice
    DuplicateScript {
        previous_location: RunscriptLocation,
        new_location: RunscriptLocation,
        target_name: String,
    },
    CommandParseError {
        location: RunscriptLocation,
        /// Detail of the error
        error: ParseError<<DefaultBuilder<String> as Builder>::Error>,
    },
}

#[derive(Debug)]
pub struct ParsingContext<T: Iterator<Item = (usize, char)> + std::fmt::Debug> {
    pub iterator: std::iter::Peekable<T>,
    pub runfile: Runscript,
    pub line_indices: Vec<usize>,
}

impl ParsingContext<CharIndices<'_>> {
    pub fn new(source: &RunscriptSource) -> ParsingContext<CharIndices<'_>> {
        ParsingContext {
            iterator: source.source.char_indices().peekable(),
            line_indices: source
                .source
                .char_indices()
                .filter_map(|(index, ch)| if ch == '\n' { Some(index) } else { None })
                .collect(),
            runfile: Runscript {
                name: (&source.path)
                    .strip_prefix(&source.dir)
                    .unwrap() //TODO: Something other than unwrap
                    .to_string_lossy()
                    .into_owned(),
                source: source.source.clone(),
                includes: Vec::new(),
                scripts: Scripts {
                    targets: IndexMap::new(),
                },
                options: Vec::new(),
            },
        }
    }
}

impl<T: Iterator<Item = (usize, char)> + std::fmt::Debug> ParsingContext<T> {
    pub fn get_loc(&self, index: usize) -> RunscriptLocation {
        let (line, column) = self
            .line_indices
            .iter()
            .enumerate()
            .find_map(|(line, &end)| {
                if end > index {
                    Some((
                        line + 1,
                        index
                            - if line == 0 {
                                0
                            } else {
                                self.line_indices[line - 1]
                            },
                    ))
                } else {
                    None
                }
            })
            .unwrap_or_else(|| {
                (
                    self.line_indices.len(),
                    index - self.line_indices.last().copied().unwrap_or(0),
                )
            });
        RunscriptLocation {
            index: vec![],
            line,
            column,
        }
    }
}

pub fn parse_runscript(source: RunscriptSource) -> Result<Runscript, RunscriptParseError> {
    let mut context = ParsingContext::new(&source);
    match parse_root(&mut context) {
        Ok(()) => Ok(context.runfile),
        Err(data) => Err(RunscriptParseError {
            script: context.runfile,
            data,
        }),
    }
}

fn parse_root<T: Iterator<Item = (usize, char)> + std::fmt::Debug>(
    context: &mut ParsingContext<T>,
) -> Result<(), RunscriptParseErrorData> {
    let mut current_script = Script {
        commands: Vec::new(),
        location: context.get_loc(0),
    };
    let mut current_script_target = "".to_owned();
    let mut current_script_phase = "exec".to_owned();
    while let Some(tk) = context.iterator.peek().copied() {
        match tk {
            // Comment
            (_, '#') => {
                consume_line(&mut context.iterator);
            }
            // Script
            (i, '[') => {
                let location = context.get_loc(i + 1);

                let (name, phase) = {
                    context.iterator.next();
                    let mut name = (&mut context.iterator)
                        .map_while(|(_, ch)| if ch == ']' { None } else { Some(ch) })
                        .collect::<String>();
                    let phase = name
                        .rfind(':')
                        .map(|idx| name.split_off(idx + 1))
                        .unwrap_or_else(|| "exec".to_owned());
                    name.pop();
                    (name, phase)
                };

                if name
                    .chars()
                    .any(|c| !(c.is_ascii_alphanumeric() || c == '_' || c == '-'))
                {
                    return Err(RunscriptParseErrorData::InvalidValue {
                        location,
                        found: name,
                        expected: "alphanumeric".to_owned(),
                    });
                }

                let target = context
                    .runfile
                    .scripts
                    .targets
                    .entry(current_script_target)
                    .or_insert_with(HashMap::new);
                target.insert(current_script_phase, current_script);

                let new_target = context
                    .runfile
                    .scripts
                    .targets
                    .entry(name.clone())
                    .or_insert_with(HashMap::new);
                if let Some(script) = new_target.get(&phase) {
                    return Err(RunscriptParseErrorData::DuplicateScript {
                        previous_location: script.location.clone(),
                        new_location: script.location.clone(),
                        target_name: name,
                    });
                }

                current_script = Script {
                    location: context.get_loc(i),
                    commands: Vec::new(),
                };
                current_script_target = name;
                current_script_phase = phase;
            }
            (_, ' ') | (_, '\n') | (_, '\r') | (_, '\t') => {
                context.iterator.next();
            }
            (i, _) => {
                let command_pos = context.get_loc(i);

                let lexer = Lexer::new((&mut context.iterator).map(|(_, ch)| ch));
                let mut parser = Parser::<_, AtomicDefaultBuilder<String>>::new(lexer);
                let command = parser
                    .complete_command()
                    .map_err(|e| RunscriptParseErrorData::CommandParseError {
                        location: command_pos,
                        error: e,
                    })?
                    .unwrap(); // The only error that can occur is EOF, but we already skip whitespace

                current_script.commands.push(command);
            }
        }
    }

    let target = context
        .runfile
        .scripts
        .targets
        .entry(current_script_target)
        .or_insert_with(HashMap::new);
    target.insert(current_script_phase, current_script);

    Ok(())
}

pub fn parse_command(
    command: &str,
) -> Result<AtomicTopLevelCommand<String>, ParseError<<DefaultBuilder<String> as Builder>::Error>> {
    let lexer = Lexer::new(command.chars());
    let mut parser = Parser::<_, AtomicDefaultBuilder<String>>::new(lexer);
    parser.complete_command().map(Option::unwrap)
}

#[derive(Debug)]
pub enum BreakCondition {
    Newline(usize),
    Eof,
}

pub fn consume_line(
    iterator: &mut (impl Iterator<Item = (usize, char)> + std::fmt::Debug),
) -> (String, BreakCondition) {
    let mut buf = String::new();
    let bk = loop {
        match iterator.next() {
            Some((i, '\n')) => break BreakCondition::Newline(i),
            Some((_, '\r')) => continue,
            Some((_, c)) => buf.push(c),
            None => break BreakCondition::Eof, //TODO: Get an index for this
        }
    };
    (buf, bk)
}
