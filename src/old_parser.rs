use crate::shell::lexer::Lexer;
use crate::shell::parse::{ParseError, Parser};
use indexmap::IndexMap;

use crate::parser::RunscriptSource;
use crate::script::*;
use Overrideable::*;

/// Data related to a `RunscriptParseError`
#[derive(Debug)]
#[non_exhaustive]
pub enum OldParseError {
    /// End-of-file in an unexpected location
    UnexpectedEOF {
        line: usize,
        /// The token or token type which was expected to come before an end-of-file.
        ///
        /// Specific tokens would be in backticks (e.g. `\`#/\``), and token groups without backticks (e.g. `an environment variable`)
        expected: String,
    },
    /// Found a token in a place which does not match the context
    UnexpectedToken {
        line: usize,
        /// The text of the token which triggered the error
        found: String,
        /// The token or token type which was expected to come before or instead of the `found` token.
        ///
        /// Specific tokens would be in backticks (e.g. `\`#/\``), and token groups without backticks (e.g. `an environment variable`)
        expected: String,
    },
    /// Found a name of a script which contains characters outside of `[A-Za-z_-]`
    InvalidID {
        line: usize,
        /// The text of the identifier that triggered the error
        found: String,
    },
    /// Attempted to define the same script twice
    MultipleDefinition {
        prev_line: usize,
        new_line: usize,
        target_name: String,
    },
    UnsupportedFeature {
        line: usize,
        msg: String,
    },
    CommandParseError {
        line: usize,
        error: ParseError,
    },
}

type NewParseError = crate::parser::RunscriptParseError;

impl From<OldParseError> for NewParseError {
    fn from(err: OldParseError) -> Self {
        match err {
            OldParseError::UnexpectedEOF { line, expected } => NewParseError::OldParseError {
                line,
                data: format!("Unexpected EOF, expected `{expected}`"),
            },
            OldParseError::UnexpectedToken {
                line,
                found,
                expected,
            } => NewParseError::OldParseError {
                line,
                data: format!("Unexpected token `{found}`, expected `{expected}`"),
            },
            OldParseError::InvalidID { line, found } => NewParseError::InvalidValue {
                line,
                expected: "alphanumeric".to_owned(),
                found,
            },
            OldParseError::MultipleDefinition {
                prev_line,
                new_line,
                target_name,
            } => NewParseError::DuplicateScript {
                prev_line,
                new_line,
                target_name,
            },
            OldParseError::UnsupportedFeature { line, msg } => {
                NewParseError::OldParseError { line, data: msg }
            }
            OldParseError::CommandParseError { line, error } => {
                NewParseError::CommandParseError { line, error }
            }
        }
    }
}

#[derive(Debug)]
struct ParsingContext<T: Iterator<Item = (usize, char)> + std::fmt::Debug> {
    iterator: std::iter::Peekable<T>,
    runfile: Runscript,
    line_indices: Vec<usize>,
}

impl<T: Iterator<Item = (usize, char)> + std::fmt::Debug> ParsingContext<T> {
    #[cfg_attr(feature = "trace", trace)]
    fn get_line(&self, index: usize) -> usize {
        self.line_indices
            .iter()
            .enumerate()
            .find_map(
                |(line, &end)| {
                    if end > index {
                        Some(line + 1)
                    } else {
                        None
                    }
                },
            )
            .unwrap_or_else(|| self.line_indices.len())
    }
}

pub fn parse_runscript(source: RunscriptSource) -> Result<Runscript, OldParseError> {
    let mut context = ParsingContext {
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
            source_text: source.source.clone(),
            scripts: IndexMap::new(),
            options: GlobalOptions::default(),
        },
    };
    match parse_root(&mut context) {
        Ok(()) => Ok(context.runfile),
        Err(data) => Err(data),
    }
}

fn parse_root<T: Iterator<Item = (usize, char)> + std::fmt::Debug>(
    context: &mut ParsingContext<T>,
) -> Result<(), OldParseError> {
    while let Some(tk) = context.iterator.next() {
        match tk {
            // Comment
            (_, '!') => {
                consume_line(&mut context.iterator);
            }
            // Annotation (Better name?)
            (i, '$') => {
                let (special, bk) = consume_word(&mut context.iterator);

                if matches!(bk, BreakCondition::Eof) {
                    return Err(OldParseError::UnexpectedEOF {
                        line: context.get_line(i + 1),
                        expected: "include".to_owned(),
                    });
                }

                match &*special {
                    "include" => {
                        return Err(OldParseError::UnsupportedFeature {
                            line: context.get_line(i + 1),
                            msg: "Includes are not supported".to_owned(),
                        });
                    }
                    "opt" => {
                        // The only option that existed was `default_positionals`, which does not change the
                        // behaviour of the script or script parsing in any way. It's not even unsupported,
                        // because there's nothing to support.
                    }
                    _ => {
                        return Err(OldParseError::UnexpectedToken {
                            line: context.get_line(i + 1),
                            found: special,
                            expected: "include` or `opt".to_owned(),
                        })
                    }
                }
            }
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

                let line = context.get_line(i);
                let script = Script {
                    line,
                    commands: parse_commands(context)?,
                    options: ScriptOptions::default(),
                };

                if name == "-" {
                    if let Some(prev_script) = &context
                        .runfile
                        .scripts
                        .entry("default".to_owned())
                        .or_default()
                        .scripts
                        .insert(phase.to_owned(), script)
                    {
                        return Err(OldParseError::MultipleDefinition {
                            prev_line: prev_script.line,
                            new_line: line,
                            target_name: name,
                        });
                    }
                } else if name == "#" || name == "<" {
                    return Err(OldParseError::UnsupportedFeature {
                        line: script.line,
                        msg: "Global scripts are not supported".to_owned(),
                    });
                } else {
                    if name
                        .chars()
                        .any(|c| !(c.is_ascii_alphanumeric() || c == '_' || c == '-'))
                    {
                        return Err(OldParseError::InvalidID {
                            line: context.get_line(i + 1),
                            found: name,
                        });
                    }
                    let target = context.runfile.scripts.entry(name.clone()).or_default();
                    if let Some(prev_script) = target.scripts.insert(phase.to_owned(), script) {
                        return Err(OldParseError::MultipleDefinition {
                            prev_line: prev_script.line,
                            new_line: line,
                            target_name: name,
                        });
                    }
                }
            }
            (_, ' ') | (_, '\n') | (_, '\r') | (_, '\t') => continue,
            _ => unimplemented!(),
        }
    }

    Ok(())
}

fn parse_commands<T: Iterator<Item = (usize, char)> + std::fmt::Debug>(
    context: &mut ParsingContext<T>,
) -> Result<Vec<ScriptCommand>, OldParseError> {
    let mut cmds = Vec::new();
    let i = context.iterator.peek().unwrap().0;
    let eof_loc = context.get_line(context.runfile.source_text.len());

    let lexer = Lexer::new((&mut context.iterator).map(|(_, ch)| ch));
    let mut parser = Parser::<_>::new(lexer);
    loop {
        if parser.peek_end_delimiter() {
            break;
        }
        cmds.push(ScriptCommand {
            command: parser
                .complete_command()
                .map_err(|e| OldParseError::CommandParseError { line: 0, error: e })?
                .ok_or(OldParseError::UnexpectedEOF {
                    line: eof_loc,
                    expected: "#/".to_owned(),
                })?,
            options: CommandOptions::default(),
        });
    }

    Ok(cmds)
}

#[derive(Debug)]
enum BreakCondition {
    Newline(usize),
    Eof,
    Parse,
}

fn consume_word(
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

fn consume_line(
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
