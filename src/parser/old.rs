//! Old-format parser, from the previous "1.0.0" version of runscript. Used by very few people,
//! and only accessible using the `old-format` feature. No error-checking is done, since new
//! (and edited) runscripts should use the new format.

use crate::parser::lexer::Lexer;
use crate::parser::parse::Parser;
use indexmap::IndexMap;

use crate::script::*;

use super::SourceFile;

#[derive(Debug)]
struct ParsingContext<T: Iterator<Item = (usize, char)> + std::fmt::Debug> {
    iterator: std::iter::Peekable<T>,
    runfile: Runscript,
    line_indices: Vec<usize>,
}

pub fn parse_runscript(source: SourceFile) -> Result<Runscript, ()> {
    let mut context = ParsingContext {
        iterator: source.source.char_indices().peekable(),
        line_indices: source
            .source
            .char_indices()
            .filter_map(|(index, ch)| if ch == '\n' { Some(index) } else { None })
            .collect(),
        runfile: Runscript {
            display_path: source
                .path
                .file_name()
                .unwrap()
                .to_string_lossy()
                .into_owned(),
            canonical_path: source.path.canonicalize().unwrap_or(source.path),
            source_text: source.source.clone(),
            scripts: IndexMap::new(),
            options: GlobalOptions::default(),
        },
    };
    match parse_root(&mut context) {
        Ok(()) => Ok(context.runfile),
        Err(()) => Err(()),
    }
}

fn parse_root<T: Iterator<Item = (usize, char)> + std::fmt::Debug>(
    context: &mut ParsingContext<T>,
) -> Result<(), ()> {
    while let Some(tk) = context.iterator.next() {
        match tk {
            // Comment
            (_, '!') => {
                consume_line(&mut context.iterator);
            }
            // Annotation
            (_, '$') => {
                // The only thing that it would be is `$opt default_positionals`. In practice, `$include`s weren't used.
                consume_line(&mut context.iterator);
            }
            // Script
            (i, '#') => {
                let (name, bk) = consume_word(&mut context.iterator);

                // b!, b => "build", br, r, r! => "run"
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
                        _ => return Err(()),
                    },
                };

                if !matches!(bk, BreakCondition::Newline(_)) {
                    consume_line(&mut context.iterator);
                }

                let line = context
                    .line_indices
                    .iter()
                    .enumerate()
                    .find_map(|(line, &end)| if end > i { Some(line + 1) } else { None })
                    .unwrap_or(context.line_indices.len());
                let script = Script {
                    line,
                    commands: ScriptExecution::Internal {
                        commands: parse_commands(context)?,
                        options: ScriptOptions::default(),
                    },
                };

                if name == "-" {
                    context
                        .runfile
                        .scripts
                        .entry("default".to_owned())
                        .or_default()
                        .scripts
                        .insert(phase.to_owned(), script);
                } else {
                    context
                        .runfile
                        .scripts
                        .entry(name.clone())
                        .or_default()
                        .scripts
                        .insert(phase.to_owned(), script);
                }
            }
            (_, ' ') | (_, '\n') | (_, '\r') | (_, '\t') => continue,
            _ => return Err(()),
        }
    }

    Ok(())
}

fn parse_commands<T: Iterator<Item = (usize, char)> + std::fmt::Debug>(
    context: &mut ParsingContext<T>,
) -> Result<Vec<ScriptCommand>, ()> {
    let mut cmds = Vec::new();

    let lexer = Lexer::new((&mut context.iterator).map(|(_, ch)| ch));
    let mut parser = Parser::<_>::new(lexer);
    loop {
        if parser.peek_end_delimiter() {
            break;
        }
        cmds.push(ScriptCommand {
            command: parser.complete_command().map_err(|_| ())?.ok_or(())?,
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
            None => break BreakCondition::Eof,
        }
    };
    (buf, bk)
}
