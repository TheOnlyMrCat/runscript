//! Old-format parser, from the previous "1.0.0" version of runscript. Used by very few people,
//! and only accessible using the `old-format` feature. No error-checking is done, since new
//! (and edited) runscripts should use the new format.

use std::path::PathBuf;

use indexmap::IndexMap;

use crate::script::*;

use super::SourceFile;

#[derive(Debug)]
struct ParsingContext<T: Iterator<Item = (usize, u8)> + std::fmt::Debug> {
    iterator: std::iter::Peekable<T>,
    runfile: Runscript,
    line_indices: Vec<usize>,
    canonical_path: PathBuf,
    working_dir: PathBuf,
}

pub fn parse_runscript(source: SourceFile) -> Result<Runscript, ()> {
    let mut context = ParsingContext {
        iterator: source.source.iter().copied().enumerate().peekable(),
        line_indices: source
            .source
            .iter()
            .copied()
            .enumerate()
            .filter_map(|(index, ch)| if ch == b'\n' { Some(index) } else { None })
            .collect(),
        runfile: Runscript {
            display_path: source
                .path
                .file_name()
                .unwrap()
                .to_string_lossy()
                .into_owned(),
            scripts: IndexMap::new(),
            options: GlobalOptions::default(),
        },
        canonical_path: source.path.canonicalize().unwrap_or(source.path),
        working_dir: source.working_dir,
    };
    match parse_root(&mut context) {
        Ok(()) => Ok(context.runfile),
        Err(()) => Err(()),
    }
}

fn parse_root<T: Iterator<Item = (usize, u8)> + std::fmt::Debug>(
    context: &mut ParsingContext<T>,
) -> Result<(), ()> {
    while let Some(tk) = context.iterator.next() {
        match tk {
            // Comment
            (_, b'!') => {
                consume_line(&mut context.iterator);
            }
            // Annotation
            (_, b'$') => {
                // The only thing that it would be is `$opt default_positionals`. In practice, `$include`s weren't used.
                consume_line(&mut context.iterator);
            }
            // Script
            (i, b'#') => {
                let (name, bk) = consume_word(&mut context.iterator);

                // b!, b => "build", br, r, r! => "run"
                let (phase, bk) = match bk {
                    BreakCondition::Newline(i) => ("run", BreakCondition::Newline(i)),
                    _ => match context.iterator.next() {
                        Some((i, b'\n')) => ("run", BreakCondition::Newline(i)),
                        Some((_, b'b')) => match context.iterator.next() {
                            Some((_, b'!')) => ("build", BreakCondition::Parse),
                            Some((_, b'r')) => ("run", BreakCondition::Parse),
                            Some((i, b'\n')) => ("build", BreakCondition::Newline(i)),
                            _ => ("build", BreakCondition::Parse),
                        },
                        Some((_, b'r')) => match context.iterator.next() {
                            Some((_, b'!')) => ("run", BreakCondition::Parse),
                            Some((i, b'\n')) => ("run", BreakCondition::Newline(i)),
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
                    options: Default::default(),
                    body: parse_commands(context)?.to_vec(),
                    canonical_path: context.canonical_path.clone(),
                    working_dir: context.working_dir.clone(),
                    line,
                };

                if name == b"-" {
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
                        .entry(String::from_utf8(name.clone()).unwrap())
                        .or_default()
                        .scripts
                        .insert(phase.to_owned(), script);
                }
            }
            (_, b' ') | (_, b'\n') | (_, b'\r') | (_, b'\t') => continue,
            _ => return Err(()),
        }
    }

    Ok(())
}

fn parse_commands<T: Iterator<Item = (usize, u8)> + std::fmt::Debug>(
    context: &mut ParsingContext<T>,
) -> Result<Vec<u8>, ()> {
    let mut commands = Vec::new();

    loop {
        let (line, bk) = consume_line(&mut context.iterator);
        if matches!(bk, BreakCondition::Eof) {
            return Err(());
        }
        if line == b"#/" {
            return Ok(commands);
        } else {
            commands.extend(line.into_iter());
        }
    }
}

#[derive(Debug)]
enum BreakCondition {
    Newline(usize),
    Eof,
    Parse,
}

fn consume_word(
    iterator: &mut (impl Iterator<Item = (usize, u8)> + std::fmt::Debug),
) -> (Vec<u8>, BreakCondition) {
    let mut buf = Vec::new();
    let nl = loop {
        match iterator.next() {
            Some((i, b'\n')) => break BreakCondition::Newline(i),
            Some((_, b' ')) | Some((_, b'\t')) => break BreakCondition::Parse,
            Some((_, b'\r')) => continue,
            Some((_, c)) => buf.push(c),
            None => break BreakCondition::Eof,
        }
    };
    (buf, nl)
}

fn consume_line(
    iterator: &mut (impl Iterator<Item = (usize, u8)> + std::fmt::Debug),
) -> (Vec<u8>, BreakCondition) {
    let mut buf = Vec::new();
    let bk = loop {
        match iterator.next() {
            Some((i, b'\n')) => break BreakCondition::Newline(i),
            Some((_, b'\r')) => continue,
            Some((_, c)) => buf.push(c),
            None => break BreakCondition::Eof,
        }
    };
    (buf, bk)
}
