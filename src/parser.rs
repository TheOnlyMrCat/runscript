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
    NonexistentOption {
        line: usize,
        option: String,
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
    #[cfg(feature = "old-parser")]
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
        scripts: IndexMap::new(),
        options: GlobalOptions::default(),
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
        command_options: CommandOptions,
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
                        .entry(current_script.target)
                        .or_insert_with(HashMap::new);
                    target.insert(current_script.phase, current_script.script);
                }

                let new_target = runscript
                    .scripts
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
                        commands: Vec::new(),
                        line: line_index!(i),
                        options: ScriptOptions::default(),
                    },
                    target: name,
                    phase,
                    command_options: CommandOptions::default(),
                });
            }
            // Option
            (i, ':') => {
                iterator.next();
                if let Some(ref mut current_script) = current_script {
                    if let Some((_, ':')) = iterator.peek() {
                        // Script-wide option
                        iterator.next();
                        let option_name = (&mut iterator)
                            .map_while(|(_, ch)| if ch.is_whitespace() { None } else { Some(ch) })
                            .collect::<String>();
                    } else {
                        // Command-specific option
                        let option_name = (&mut iterator)
                            .map_while(|(_, ch)| if ch.is_whitespace() { None } else { Some(ch) })
                            .collect::<String>();

                        match option_name.as_str() {
                            "ignore-exit" => {
                                current_script.command_options.ignore_exit_code = true;
                            }
                            _ => {
                                return Err(RunscriptParseError::NonexistentOption {
                                    line: line_index!(i),
                                    option: option_name,
                                });
                            }
                        }
                    }
                } else if let Some((_, ':')) = iterator.peek() {
                    // File-wide option
                    iterator.next();
                    let option_name = (&mut iterator)
                        .map_while(|(_, ch)| if ch.is_whitespace() { None } else { Some(ch) })
                        .collect::<String>();

                    match option_name.as_str() {
                        "default-script" => {
                            let default_name = (&mut iterator)
                                .map_while(
                                    |(_, ch)| if ch.is_whitespace() { None } else { Some(ch) },
                                )
                                .collect::<String>();
                            runscript.options.default_target = Some(Some(default_name));
                        }
                        "no-default-script" => {
                            runscript.options.default_target = Some(None);
                        }
                        _ => {
                            return Err(RunscriptParseError::NonexistentOption {
                                line: line_index!(i),
                                option: option_name,
                            });
                        }
                    }
                } else {
                    return Err(RunscriptParseError::IllegalCommandLocation {
                        line: line_index!(iterator.peek().unwrap().0),
                    });
                }
            }
            // Whitespace
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

                    current_script.script.commands.push(ScriptCommand {
                        command,
                        options: std::mem::take(&mut current_script.command_options),
                    });
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
            .entry(current_script.target)
            .or_insert_with(HashMap::new);
        target.insert(current_script.phase, current_script.script);
    }

    Ok(runscript)
}

#[derive(Debug)]
pub enum BreakCondition {
    Newline(usize),
    Space,
    Eof,
}

pub fn consume_line(
    iterator: &mut impl Iterator<Item = (usize, char)>,
) -> (String, BreakCondition) {
    let mut buf = String::new();
    let bk = loop {
        match iterator.next() {
            Some((i, '\n')) => break BreakCondition::Newline(i),
            Some((_, '\r')) => continue,
            Some((_, c)) => buf.push(c),
            None => break BreakCondition::Eof,
        }
    };
    (buf, bk)
}

pub fn consume_word(
    iterator: &mut impl Iterator<Item = (usize, char)>,
) -> (String, BreakCondition) {
    let mut buf = String::new();
    let bk = loop {
        match iterator.next() {
            Some((i, '\n')) => break BreakCondition::Newline(i),
            Some((_, ' ')) | Some((_, '\r')) | Some((_, '\t')) => {
                break BreakCondition::Space;
            }
            Some((_, c)) => buf.push(c),
            None => break BreakCondition::Eof,
        }
    };
    (buf, bk)
}
