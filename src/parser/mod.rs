use std::path::PathBuf;

use indexmap::IndexMap;
use parse::{CommandGroupDelimiters, ParseError, Parser};

use crate::script::Overrideable::*;
use crate::script::*;

pub mod ast;
pub mod lexer;
#[cfg(feature = "old-parser")]
pub mod old;
pub mod parse;
pub mod token;

use ast::AtomicTopLevelCommand;
use lexer::Lexer;

#[derive(Debug, Clone)]
pub struct RunscriptSource {
    pub path: PathBuf,
    /// The directory the file's name should be considered relative to
    pub dir: PathBuf,
    pub source: String,
}

#[derive(Debug)]
pub enum RunscriptParseError {
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
}

//TODO: It might be convenient to fold this and conch_parser together even more

pub fn parse_shell(
    source: RunscriptSource,
) -> Result<Vec<AtomicTopLevelCommand>, RunscriptParseError> {
    let lexer = Lexer::new(source.source.chars());
    let mut parser = Parser::<_>::new(lexer);

    parser
        .command_group(CommandGroupDelimiters::default())
        .map(|group| group.commands)
        .map_err(|err| RunscriptParseError::CommandParseError {
            line: 0,
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
        path: source
            .path
            .canonicalize()
            .unwrap_or(source.path)
            .to_string_lossy()
            .into_owned(),
        source_text: source.source.clone(),
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
        target_options: TargetOptions,
        command_option: bool,
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
                // Get script name, phase, and the (optional) executing shell.
                let (name, phase) = {
                    iterator.next();
                    let mut name = (&mut iterator)
                        .map_while(|(_, ch)| if ch == ']' { None } else { Some(ch) })
                        .collect::<String>();
                    let phase = name
                        .rfind(':')
                        .map(|idx| {
                            let phase = name.split_off(idx + 1);
                            name.pop(); // To remove the trailing `:`
                            phase
                        })
                        .unwrap_or_else(|| "run".to_owned());
                    (name, phase)
                };
                let execution_type = {
                    let (mut execution_type, _) = consume_line(&mut iterator);
                    execution_type.retain(|c| !c.is_whitespace());
                    execution_type
                };

                // Clean up the previous script if there was one
                if let Some(current_script) = current_script {
                    let target = runscript.scripts.entry(current_script.target).or_default();
                    target
                        .scripts
                        .insert(current_script.phase, current_script.script);
                    target.options.merge(current_script.target_options).unwrap();
                }

                // Ensure the new script is unique
                let new_target = runscript.scripts.entry(name.clone()).or_default();
                if let Some(script) = new_target.scripts.get(&phase) {
                    return Err(RunscriptParseError::DuplicateScript {
                        prev_line: script.line,
                        new_line: line_index!(i),
                        target_name: name,
                    });
                }

                // Set up parsing context for new script
                current_script = Some(CurrentScript {
                    script: Script {
                        commands: if execution_type.is_empty() {
                            ScriptExecution::Internal {
                                commands: Vec::new(),
                                options: ScriptOptions::default(),
                            }
                        } else {
                            ScriptExecution::ExternalPosix {
                                path: execution_type,
                                commands: String::new(),
                            }
                        },
                        line: line_index!(i),
                    },
                    target: name,
                    phase,
                    target_options: TargetOptions::default(),
                    command_option: false,
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

                        match option_name.as_str() {
                            "default-phase" => {
                                let mut phases = vec![];
                                loop {
                                    let (phase, bk) = consume_word(&mut iterator);
                                    phases.push(phase);
                                    if !matches!(bk, BreakCondition::Space) {
                                        break;
                                    }
                                }
                                current_script
                                    .target_options
                                    .default_phase
                                    .merge(Set(phases))
                                    .unwrap();
                            }
                            "no-default-phase" => {
                                current_script
                                    .target_options
                                    .default_phase
                                    .merge(SetNone)
                                    .unwrap();
                            }
                            _ => {
                                return Err(RunscriptParseError::NonexistentOption {
                                    line: line_index!(i),
                                    option: option_name,
                                })
                            }
                        }
                    } else if let ScriptExecution::ExternalPosix { commands, .. } =
                        &mut current_script.script.commands
                    {
                        // Target-specific options can still exist in externally-executed scripts,
                        // but we still need to give the `:` back if we're not using it
                        commands.push(':');
                    } else {
                        // Command-specific option. This must be given to the command parser, but
                        // we've already consumed the `:`, so we must pass it through with a state
                        // change. If >1 lookahead is needed elsewhere, I might generalise
                        // conch_parser's multipeek adapter.
                        current_script.command_option = true;
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
                            runscript
                                .options
                                .default_target
                                .merge(Set(default_name))
                                .unwrap();
                        }
                        "no-default-script" => {
                            runscript.options.default_target.merge(SetNone).unwrap();
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
            (_, c @ (' ' | '\n' | '\r' | '\t')) => {
                if let Some(CurrentScript {
                    script:
                        Script {
                            commands: ScriptExecution::ExternalPosix { commands, .. },
                            ..
                        },
                    ..
                }) = &mut current_script
                {
                    commands.push(c);
                }
                iterator.next();
            }
            // Everything else is probably a command
            (i, c) => {
                if let Some(current_script) = &mut current_script {
                    match &mut current_script.script.commands {
                        ScriptExecution::ExternalPosix { commands, .. } => {
                            commands.push(c);
                        }
                        ScriptExecution::Internal { commands, .. } => {
                            let command_pos = line_index!(i);

                            current_script.command_option = false;

                            let lexer = Lexer::new((&mut iterator).map(|(_, ch)| ch));
                            let mut parser = Parser::<_>::new(lexer);
                            let command = parser
                                .complete_command()
                                .map_err(|e| RunscriptParseError::CommandParseError {
                                    line: command_pos,
                                    error: e,
                                })?
                                .unwrap(); // The only error that can occur is EOF before any word, but we already skip whitespace

                            commands.push(ScriptCommand {
                                command,
                                options: CommandOptions::default(),
                            });
                        }
                    }
                } else {
                    return Err(RunscriptParseError::IllegalCommandLocation {
                        line: line_index!(i),
                    });
                }
            }
        }
    }

    if let Some(current_script) = current_script {
        let target = runscript.scripts.entry(current_script.target).or_default();
        target
            .scripts
            .insert(current_script.phase, current_script.script);
        target.options.merge(current_script.target_options).unwrap();
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
