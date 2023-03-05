#[cfg(feature = "old-parser")]
pub mod old;

use std::ops::Range;
use std::path::PathBuf;

use chumsky::prelude::*;
use indexmap::IndexMap;
use runscript_parse::{pad_intercommand, whitespace, CommandChain};

use crate::script::Overrideable::*;
use crate::script::*;

#[derive(Debug, Clone)]
pub struct SourceFile {
    pub path: PathBuf,
    pub working_dir: PathBuf,
    pub source: Vec<u8>,
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
    IllegalCommandLocation {
        line: usize,
    },
}

impl chumsky::Error<u8> for RunscriptParseError {
    type Span = Range<usize>;
    type Label = ();

    fn expected_input_found<Iter: IntoIterator<Item = Option<u8>>>(
        span: Self::Span,
        expected: Iter,
        found: Option<u8>,
    ) -> Self {
        todo!()
    }

    fn with_label(self, label: Self::Label) -> Self {
        todo!()
    }

    fn merge(self, other: Self) -> Self {
        todo!()
    }
}

//TODO: It might be convenient to fold this and conch_parser together even more

pub fn parse_shell(source: SourceFile) -> Result<Vec<CommandChain>, Vec<Simple<u8>>> {
    pad_intercommand(runscript_parse::command_chain())
        .repeated()
        .parse(source.source)
}

pub fn parse_command(command: &[u8]) -> Result<Vec<CommandChain>, Vec<Simple<u8>>> {
    pad_intercommand(runscript_parse::command_chain())
        .repeated()
        .parse(command)
}

pub fn parse_runscript(source: SourceFile) -> Result<Runscript, Vec<Simple<u8>>> {
    let display_path = source
        .path
        .file_name()
        .unwrap()
        .to_string_lossy()
        .into_owned();
    let canonical_path = source.path.canonicalize().unwrap_or(source.path);
    let working_dir = source.working_dir.clone();

    let ident = one_of(b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")
        .then(
            one_of(b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_-").repeated(),
        )
        .map(|(first, rest)| {
            std::iter::once(first)
                .chain(rest.into_iter())
                .collect::<Vec<_>>()
        });
    let option = just(b"::")
        .ignore_then(ident.clone())
        .then_ignore(whitespace().repeated().at_least(1))
        .then(none_of(b"\n").repeated())
        .then_ignore(just(b'\n'));
    let options = option
        .repeated()
        .separated_by(just(b'\n').repeated().at_least(1))
        .allow_leading()
        .allow_trailing()
        .flatten();

    let header = just(b'[')
        .ignore_then(ident.clone())
        .then(just(b':').ignore_then(ident).or_not())
        .then_ignore(just(b']'))
        .then_ignore(just(b'\n'));

    let body = none_of(b"[\n")
        .then(none_of(b"\n").repeated())
        .or_not()
        .then(just(b'\n'))
        .map(|(line, lf)| {
            let mut line = line
                .map(|(first, rest)| {
                    std::iter::once(first)
                        .chain(rest.into_iter())
                        .collect::<Vec<_>>()
                })
                .unwrap_or(vec![]);
            line.push(lf);
            line
        })
        .repeated()
        .flatten();

    let parser = options
        .clone()
        .then(
            header
                .then(options)
                .then(body)
                .map(move |(((target_name, phase), options), body)| {
                    let mut target = Target::default();
                    let mut script_options = ScriptOptions::default();
                    for (key, value) in options {
                        match key.as_slice() {
                            b"default-phase" => {
                                target.options.default_phase = Set(value
                                    .split(|&b| b == b' ')
                                    .map(|value| String::from_utf8(value.to_owned()).unwrap())
                                    .collect())
                            }
                            b"no-default-phase" => target.options.default_phase = SetNone,
                            b"shell" => script_options.executor = Set(value),
                            _ => todo!(),
                        }
                    }
                    target.scripts.insert(
                        phase
                            .map(|phase| String::from_utf8(phase).ok())
                            .flatten()
                            .unwrap_or("run".to_owned()),
                        Script {
                            options: script_options,
                            body,
                            canonical_path: canonical_path.clone(),
                            working_dir: working_dir.clone(),
                            line: 0,
                        },
                    );
                    (String::from_utf8(target_name).unwrap(), target)
                })
                .repeated(),
        )
        .map(move |(options, targets)| {
            let mut collated_targets = IndexMap::<String, Target>::new();
            let mut global_options = GlobalOptions::default();
            for (key, value) in options {
                match key.as_slice() {
                    b"default-target" | b"default-script" => {
                        global_options.default_target = Set(value)
                    }
                    b"no-default-target" | b"no-default-script" => {
                        global_options.default_target = SetNone
                    }
                    b"default-shell" => global_options.default_executor = Set(value),
                    _ => todo!(),
                }
            }
            for (target_name, target) in targets {
                collated_targets
                    .entry(target_name)
                    .or_default()
                    .merge(target);
            }
            Runscript {
                display_path: display_path.clone(),
                scripts: collated_targets,
                options: global_options,
            }
        });

    parser.parse(source.source)
}
