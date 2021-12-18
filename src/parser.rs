use std::path::PathBuf;
use std::str::CharIndices;

use conch_parser::ast::builder::{AtomicDefaultBuilder, Builder, DefaultBuilder};
use conch_parser::ast::AtomicTopLevelCommand;
use conch_parser::lexer::Lexer;
use conch_parser::parse::{CommandGroupDelimiters, ParseError, Parser};
use conch_parser::token::Token;
use enum_map::EnumMap;
use linked_hash_map::LinkedHashMap;

use crate::script::*;
#[cfg(feature = "trace")]
use crate::DEPTH;

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
#[derive(Clone, Debug, Default)]
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
#[allow(clippy::large_enum_variant)]
pub enum ParseOrIOError {
    IOError(std::io::Error),
    ParseError(RunscriptParseError),
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
    /// Found a name of a script which contains characters outside of `[A-Za-z_-]`
    InvalidValue {
        location: RunscriptLocation,
        /// The text of the identifier that triggered the error
        found: String,
        /// A description of what was expected instead of the `found` identifier
        expected: String,
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

pub fn parse_runfile(path: impl Into<PathBuf>) -> Result<Runscript, ParseOrIOError> {
    let path = path.into();
    parse_runscript(RunscriptSource {
        source: std::fs::read_to_string(&path).map_err(ParseOrIOError::IOError)?,
        base: path.parent().expect("Runfile has no parent!").to_owned(),
        file: path,
        index: Vec::new(),
    })
    .map_err(ParseOrIOError::ParseError)
}

pub fn parse_runfile_nested(
    path: impl Into<PathBuf>,
    base: impl Into<PathBuf>,
    index: Vec<usize>,
) -> Result<Runscript, ParseOrIOError> {
    let path = path.into();
    parse_runscript(RunscriptSource {
        source: std::fs::read_to_string(&path).map_err(ParseOrIOError::IOError)?,
        file: path,
        base: base.into(),
        index,
    })
    .map_err(ParseOrIOError::ParseError)
}

#[derive(Debug)]
pub struct ParsingContext<'a, T: Iterator<Item = (usize, char)> + std::fmt::Debug> {
    pub iterator: std::iter::Peekable<T>,
    pub runfile: Runscript,
    pub index: &'a Vec<usize>,
    pub line_indices: Vec<usize>,
}

impl ParsingContext<'_, CharIndices<'_>> {
    pub fn new(source: &RunscriptSource) -> ParsingContext<'_, CharIndices<'_>> {
        ParsingContext {
            iterator: source.source.char_indices().peekable(),
            line_indices: source
                .source
                .char_indices()
                .filter_map(|(index, ch)| if ch == '\n' { Some(index) } else { None })
                .collect(),
            runfile: Runscript {
                name: (&source.file)
                    .strip_prefix(&source.base)
                    .unwrap() //TODO: Something other than unwrap
                    .to_string_lossy()
                    .into_owned(),
                source: source.source.clone(),
                includes: Vec::new(),
                scripts: Scripts {
                    targets: LinkedHashMap::new(),
                },
                options: Vec::new(),
            },
            index: &source.index,
        }
    }
}

impl<T: Iterator<Item = (usize, char)> + std::fmt::Debug> ParsingContext<'_, T> {
    #[cfg_attr(feature = "trace", trace)]
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
            index: self.index.clone(),
            line,
            column,
        }
    }
}

#[cfg_attr(feature = "trace", trace)]
pub fn parse_runscript(source: RunscriptSource) -> Result<Runscript, RunscriptParseError> {
    let mut context = ParsingContext::new(&source);
    match parse_root(&mut context, &source) {
        Ok(()) => Ok(context.runfile),
        Err(data) => Err(RunscriptParseError {
            script: context.runfile,
            data,
        }),
    }
}

#[cfg_attr(feature = "trace", trace)]
fn parse_root<T: Iterator<Item = (usize, char)> + std::fmt::Debug>(
    context: &mut ParsingContext<T>,
    source: &RunscriptSource,
) -> Result<(), RunscriptParseErrorData> {
    let mut current_script = Script {
        commands: Vec::new(),
        location: context.get_loc(0),
    };
    let mut current_script_type = ScriptType::Default;
    let mut current_script_phase = ScriptPhase::BuildAndRun;
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
                        .map(|idx| {
                            let phase = name.split_off(idx + 1);
                            Ok(match phase.as_str() {
                                "b!" => ScriptPhase::BuildOnly,
                                "b" => ScriptPhase::Build,
                                "br" => ScriptPhase::BuildAndRun,
                                "r" => ScriptPhase::Run,
                                "r!" => ScriptPhase::RunOnly,
                                phase => {
                                    return Err(RunscriptParseErrorData::InvalidValue {
                                        location: location.clone(),
                                        found: phase.to_owned(),
                                        expected: "b!|b|br|r|r!".to_owned(),
                                    })
                                }
                            })
                        })
                        .transpose()?
                        .unwrap_or(ScriptPhase::BuildAndRun);
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
                    .entry(current_script_type)
                    .or_insert_with(EnumMap::new);
                target[current_script_phase] = Some(current_script);

                let new_target = context
                    .runfile
                    .scripts
                    .targets
                    .entry(ScriptType::Named(name.clone()))
                    .or_insert_with(EnumMap::new);
                if let Some(script) = &new_target[phase] {
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
                current_script_type = ScriptType::Named(name);
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
        .entry(current_script_type)
        .or_insert_with(EnumMap::new);
    target[current_script_phase] = Some(current_script);

    Ok(())
}

/// Parses a block of commands for the given `ParsingContext`, terminating when it reaches a `$|`
#[cfg_attr(feature = "trace", trace)]
pub fn parse_commands<T: Iterator<Item = (usize, char)> + std::fmt::Debug>(
    context: &mut ParsingContext<T>,
) -> Result<
    Vec<AtomicTopLevelCommand<String>>,
    ParseError<<DefaultBuilder<String> as Builder>::Error>,
> {
    let lexer = Lexer::new((&mut context.iterator).map(|(_, c)| c));
    let mut parser = Parser::<_, AtomicDefaultBuilder<String>>::new(lexer);
    let mut commands = vec![];
    loop {
        //TODO: Parser keeps track of position... if commands are parsed manually, line numbers can be derived from that
        commands.extend(
            parser
                .command_group(CommandGroupDelimiters {
                    reserved_tokens: &[Token::Dollar],
                    ..Default::default()
                })?
                .commands,
        );
        parser.reserved_token(&[Token::Dollar])?;
        if let Some(&Token::Pipe) = parser.peek_reserved_token(&[Token::Pipe]) {
            break;
        }
    }
    context.iterator.next();
    Ok(commands)
}

#[derive(Debug)]
pub enum BreakCondition {
    Newline(usize),
    Eof,
    Parse,
}

#[cfg_attr(feature = "trace", trace)]
pub fn consume_word(
    iterator: &mut (impl Iterator<Item = (usize, char)> + std::fmt::Debug),
) -> (String, BreakCondition) {
    let mut buf = String::new();
    let nl = loop {
        match iterator.next() {
            Some((i, '\n')) => break BreakCondition::Newline(i),
            Some((_, ' ')) | Some((_, '\t')) => break BreakCondition::Parse,
            Some((_, '\r')) => continue,
            Some((_, c)) => buf.push(c),
            None => break BreakCondition::Eof,
        }
    };
    (buf, nl)
}

#[cfg_attr(feature = "trace", trace)]
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
