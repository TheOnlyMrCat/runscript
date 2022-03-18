use std::collections::HashMap;
use std::path::PathBuf;

use crate::shell::ast::AtomicTopLevelCommand;
use crate::shell::lexer::Lexer;
use crate::shell::parse::{CommandGroupDelimiters, ParseError, Parser};
use indexmap::IndexMap;

use crate::script::*;

#[derive(Debug, Clone)]
pub struct RunscriptSource {
    pub path: PathBuf,
    /// The directory the file's name should be considered relative to
    pub dir: PathBuf,
    pub source: String,
}

#[derive(Debug)]
pub enum RunscriptParseError {
    InvalidValue {
        line: usize,
        found: String,
        expected: String,
    },
    DuplicateScript {
        prev_line: usize,
        new_line: usize,
        target_name: String,
    },
    CommandParseError {
        line: usize,
        error: ParseError,
    },
    IllegalCommandLocation {
        line: usize,
    },
    OldParseError {
        line: usize,
        data: String,
    },
}

pub fn parse_shell(
    source: RunscriptSource,
) -> Result<Vec<AtomicTopLevelCommand>, RunscriptParseError> {
    let lexer = Lexer::new(source.source.chars());
    let mut parser = Parser::<_>::new(lexer);

    parser
        .command_group(CommandGroupDelimiters::default())
        .map(|group| group.commands)
        .map_err(|err| RunscriptParseError::CommandParseError {
            line: 0, //TODO: Extract command parse error type
            error: err,
        })
}

pub fn parse_command(command: &str) -> Result<AtomicTopLevelCommand, ParseError> {
    let lexer = Lexer::new(command.chars());
    let mut parser = Parser::<_>::new(lexer);
    parser.complete_command().map(Option::unwrap)
}

pub fn parse_runscript(source: RunscriptSource) -> Result<Runscript, RunscriptParseError> {
    let line_indices = source
        .source
        .char_indices()
        .filter_map(|(i, c)| (c == '\n').then(|| i))
        .collect::<Vec<_>>();
    let mut iterator = source.source.char_indices().peekable();

    let mut runscript = Runscript {
        name: source
            .path
            .file_name()
            .unwrap()
            .to_string_lossy()
            .into_owned(),
        source: source.source.clone(),
        scripts: Scripts {
            targets: IndexMap::new(),
        },
        options: Vec::new(),
    };

    //TODO: Replace with into_ok_or_err (https://github.com/rust-lang/rust/issues/82223)
    macro_rules! line_index {
        ($index:expr) => {
            match line_indices.binary_search(&$index) {
                Ok(i) => i + 1,
                Err(i) => i + 1,
            }
        };
    }

    struct CurrentScript {
        script: Script,
        target: String,
        phase: String,
    }

    let mut current_script: Option<CurrentScript> = None;
    while let Some(tk) = iterator.peek().copied() {
        match tk {
            // Comment
            (_, '#') => {
                consume_line(&mut iterator);
            }
            // Script
            (i, '[') => {
                let line = line_index!(i);

                let (name, phase) = {
                    iterator.next();
                    let mut name = (&mut iterator)
                        .map_while(|(_, ch)| if ch == ']' { None } else { Some(ch) })
                        .collect::<String>();
                    let phase = name
                        .rfind(':')
                        .map(|idx| {
                            let phase = name.split_off(idx + 1);
                            name.pop(); // To remove the `:` as well
                            phase
                        })
                        .unwrap_or_else(|| "run".to_owned());
                    (name, phase)
                };

                if name
                    .chars()
                    .any(|c| !(c.is_ascii_alphanumeric() || c == '_' || c == '-'))
                {
                    return Err(RunscriptParseError::InvalidValue {
                        line,
                        found: name,
                        expected: "alphanumeric".to_owned(),
                    });
                }

                if let Some(current_script) = current_script {
                    let target = runscript
                        .scripts
                        .targets
                        .entry(current_script.target)
                        .or_insert_with(HashMap::new);
                    target.insert(current_script.phase, current_script.script);
                }

                let new_target = runscript
                    .scripts
                    .targets
                    .entry(name.clone())
                    .or_insert_with(HashMap::new);
                if let Some(script) = new_target.get(&phase) {
                    return Err(RunscriptParseError::DuplicateScript {
                        prev_line: script.line,
                        new_line: script.line,
                        target_name: name,
                    });
                }

                current_script = Some(CurrentScript {
                    script: Script {
                        line: line_index!(i),
                        commands: Vec::new(),
                    },
                    target: name,
                    phase,
                });
            }
            (_, ' ') | (_, '\n') | (_, '\r') | (_, '\t') => {
                iterator.next();
            }
            (i, _) => {
                if let Some(ref mut current_script) = current_script {
                    let command_pos = line_index!(i);

                    let lexer = Lexer::new((&mut iterator).map(|(_, ch)| ch));
                    let mut parser = Parser::<_>::new(lexer);
                    let command = parser
                        .complete_command()
                        .map_err(|e| RunscriptParseError::CommandParseError {
                            line: command_pos,
                            error: e,
                        })?
                        .unwrap(); // The only error that can occur is EOF, but we already skip whitespace

                    current_script.script.commands.push(command);
                } else {
                    return Err(RunscriptParseError::IllegalCommandLocation {
                        line: line_index!(i),
                    });
                }
            }
        }
    }

    if let Some(current_script) = current_script {
        let target = runscript
            .scripts
            .targets
            .entry(current_script.target)
            .or_insert_with(HashMap::new);
        target.insert(current_script.phase, current_script.script);
    }

    Ok(runscript)
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
