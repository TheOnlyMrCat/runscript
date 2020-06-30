use std::io::ErrorKind::{self, *};
use std::path::PathBuf;

use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFile;
use codespan_reporting::term::{emit, Config};
use termcolor::{StandardStream, ColorChoice};

use lalrpop_util::lexer::Token;
use lalrpop_util::ParseError::{self, *};

pub fn file_read_err(filename: PathBuf, kind: ErrorKind) {
    let d: Diagnostic<()> = Diagnostic::error()
        .with_message("Error reading input file")
        .with_notes(match kind {
            NotFound => vec![format!("File {} does not exist", filename.as_os_str().to_string_lossy())],
            PermissionDenied => vec![format!("No permission to read file {}", filename.as_os_str().to_string_lossy())],
            InvalidData => vec![format!("Contents of {} are not valid UTF-8", filename.as_os_str().to_string_lossy())],
            WouldBlock => vec!["Reading file would block".to_owned()],
            Interrupted => vec!["Interrupted while reading file".to_owned()],
            _ => vec![]
        });
    
    let w = StandardStream::stderr(ColorChoice::Auto);
    let c = Config::default();

    emit(&mut w.lock(), &c, &SimpleFile::new(filename.as_os_str().to_string_lossy(), ""), &d).expect("Couldn't print error");
}

pub fn option_parse_err(err: getopts::Fail) {
    let d: Diagnostic<()> = Diagnostic::error()
        .with_message(format!("{}", err));
    
    let w = StandardStream::stderr(ColorChoice::Auto);
    let c = Config::default();

    emit(&mut w.lock(), &c, &SimpleFile::new("", ""), &d).expect("Couldn't print error");
}

pub enum RunscriptError {
    MultipleDefinition {
        target: String,
        location: (usize, usize),
        previous_def: (usize, usize),
    },
}

pub fn file_parse_err(file: &SimpleFile<String, String>, err: ParseError<usize, lalrpop_util::lexer::Token, RunscriptError>) {
    let d: Diagnostic<()> = match err {
        InvalidToken { location: loc } => Diagnostic::error()
                .with_message("Invalid token in input")
                .with_labels(vec![
                    Label::primary((), loc..loc + 1).with_message("Invalid token")
                ]),
        UnrecognizedEOF { location: loc, expected } => Diagnostic::error()
                .with_message("Unexpected end of file")
                .with_labels(vec![
                    Label::primary((), loc..loc + 1).with_message("Unexpected end of file")
                ])
                .with_notes(vec![
                    format!("Expected one of the following:{}", expected.iter().fold("".to_owned(), |acc, x| acc + "\n    " + x))
                ]),
        UnrecognizedToken { token: (start, Token(_id, name), end), expected } => Diagnostic::error()
                .with_message(format!("Unexpected {}", name))
                .with_labels(vec![
                    Label::primary((), start..end).with_message(format!("Unexpected {}", name))
                ])
                .with_notes(vec![
                    format!("Expected one of the following:{}", expected.iter().fold("".to_owned(), |acc, x| acc + "\n    " + x))
                ]),
        ExtraToken { token: (start, Token(_id, name), end) } => Diagnostic::error()
                .with_message(format!("Unexpected {}", name))
                .with_labels(vec![
                    Label::primary((), start..end).with_message(format!("Unexpected {}", name))
                ])
                .with_notes(vec!["Expected end of file".to_owned()]),
        User { error } => match error {
            RunscriptError::MultipleDefinition { target: t, location: (nl, nr), previous_def: (pl, pr) } => Diagnostic::error()
                .with_message(format!("Multiple definitions of {}", match &*t { "#" => "global target".to_owned(), "-" => "default target".to_owned(), s => format!("target `{}`", s)}))
                .with_labels(vec![
                    Label::primary((), nl..nr).with_message(format!("Multiple definitions of {}", t)),
                    Label::secondary((), pl..pr).with_message("Previous definition is here")
                ]),
        }
    };

    let w = StandardStream::stderr(ColorChoice::Auto);
    let c = Config::default();

    emit(&mut w.lock(), &c, file, &d).expect("Couldn't print error");
}