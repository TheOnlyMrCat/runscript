use std::io::ErrorKind::*;
use std::io::Write;
use std::rc::Rc;

use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFile;
use codespan_reporting::term::{emit, Config};
use termcolor::{StandardStream, ColorChoice, WriteColor, ColorSpec, Color};

use lalrpop_util::lexer::Token;
use lalrpop_util::ParseError::{self, *};

use crate::runfile::{Command, ScriptType};
use crate::exec::CommandExecErr::{self, *};

pub fn file_read_err(output_stream: Rc<StandardStream>) {
    let d: Diagnostic<()> = Diagnostic::error()
		.with_message("Error finding runscript")
		.with_notes(vec!["Additional diagnostics planned".to_owned()]);
    
    let w = output_stream;
    let c = Config::default();

    emit(&mut w.lock(), &c, &SimpleFile::new("", ""), &d).expect("Couldn't print error");
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
	InvalidInclude {
		include_str: String,
		file_err: std::io::Error,
		dir_err: std::io::Error,
		location: (usize, usize),
	}
}

pub fn file_parse_err(config: &crate::Config, err: ParseError<usize, lalrpop_util::lexer::Token, RunscriptError>) {
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
        UnrecognizedToken { token: (start, Token(id, name), end), expected } => Diagnostic::error()
                .with_message(format!("Unexpected `{}`", if name != "\n" { name } else { "newline" }))
                .with_labels(vec![
                    Label::primary((), start..end).with_message(format!("Unexpected `{}`", if name != "\n" { name } else { "newline" }))
                ])
                .with_notes(vec![
					format!("Token matched as id `{}`", id),
                    format!("Expected one of the following:{}", expected.iter().fold("".to_owned(), |acc, x| acc + "\n    " + x)),
                ]),
        ExtraToken { token: (start, Token(_id, name), end) } => Diagnostic::error()
                .with_message(format!("Unexpected `{}`", name))
                .with_labels(vec![
                    Label::primary((), start..end).with_message(format!("Unexpected `{}`", if name != "\n" { name } else { "newline" })),
                ])
                .with_notes(vec!["Expected end of file".to_owned()]),
        User { error } => match error {
            RunscriptError::MultipleDefinition { target: t, location: (nl, nr), previous_def: (pl, pr) } => Diagnostic::error()
                .with_message(format!("Multiple definitions of `{}`", match &*t { "#" => "global target".to_owned(), "-" => "default target".to_owned(), s => format!("target `{}`", s)}))
                .with_labels(vec![
                    Label::primary((), nl..nr).with_message(format!("Multiple definitions of `{}`", t)),
                    Label::secondary((), pl..pr).with_message("Previous definition is here"),
				]),
			RunscriptError::InvalidInclude { include_str, file_err, dir_err, location: (l, r) } => Diagnostic::error()
					.with_message(format!("Could not include `{}`", include_str))
					.with_labels(vec![
						Label::primary((), l..r).with_message(format!("Could not include `{}`", include_str)),
					])
					.with_notes(vec![
						match file_err.kind() {
							NotFound => format!("File {}.run not found", include_str),
							PermissionDenied => format!("Read permission denied for {}.run", include_str),
							InvalidData => format!("File {}.run did not contain valid UTF-8", include_str),
							Interrupted => format!("Interrupted while reading file {}.run", include_str),
							_ => format!("An I/O error occurred while reading {}.run", include_str),
						},
						match dir_err.kind() {
							NotFound => format!("File {}/run not found", include_str),
							PermissionDenied => format!("Read permission denied for {}/run", include_str),
							InvalidData => format!("File {}/run did not contain valid UTF-8", include_str),
							Interrupted => format!("Interrupted while reading file {}./run", include_str),
							_ => format!("An I/O error occurred while reading {}/run", include_str),
						}
					])
        }
    };

    let w = &config.output_stream;
    let c = Config::default();

    emit(&mut w.lock(), &c, &config.codespan_file, &d).expect("Couldn't print error");
}

pub fn bad_command_err(config: &crate::Config, cmd: &Command, error: CommandExecErr) {
    let d: Diagnostic<()> = Diagnostic::error()
        .with_message(match &error {
            BadCommand { .. } => format!("Failed to execute `{}`", cmd.target),
            InvalidGlob { glob, .. } => format!("Failed to parse `{}`", glob),
            NoGlobMatches { glob, .. } => format!("No matches found for `{}`", glob),
        })
        .with_labels(vec![
            match &error {
                BadCommand { err, loc } => Label::primary((), loc.0..loc.1).with_message(match err.kind() {
                    NotFound => "Couldn't find executable",
                    PermissionDenied => "Insufficient permission to execute command",
                    _ => "Failed to execute command",
                }),
                InvalidGlob { err, loc, .. } => Label::primary((), loc.0..loc.1).with_message(format!("{:#}", err)),
                NoGlobMatches { loc, .. } => Label::primary((), loc.0..loc.1).with_message("No matches found for glob"),
            }
        ])
        .with_notes(match &error {
            BadCommand { err, .. } => match err.kind() {
                NotFound => vec![
                    "You can add the executable to your $PATH".to_owned(),
                    "You can specify the full path to the executable".to_owned()
                ],
                _ => vec![]
            },
            _ => vec![]
        });
    
    let w = &config.output_stream;
    let c = Config::default();

    emit(&mut w.lock(), &c, &config.codespan_file, &d).expect("Couldn't print error");
}

pub fn bad_target(config: &crate::Config, target: String) {
    let d: Diagnostic<()> = Diagnostic::error()
        .with_message(format!("No target with name `{}`", target));

    let w = &config.output_stream;
    let c = Config::default();

    emit(&mut w.lock(), &c, &config.codespan_file, &d).expect("Couldn't print error");
}

pub fn phase_message(config: &crate::Config, phase: ScriptType, name: &str) {
    let mut lock = config.output_stream.lock();
    lock.set_color(ColorSpec::new().set_bold(true).set_intense(true).set_fg(Some(match phase {
        ScriptType::BuildOnly => Color::Red,
        ScriptType::Build => Color::Yellow,
        ScriptType::BuildAndRun => Color::Green,
        ScriptType::Run => Color::Blue,
        ScriptType::RunOnly => Color::Magenta,
    }))).expect("Failed to set colour");
    write!(lock, "{}", phase).expect("Failed to write");
    lock.reset().expect("Failed to reset colour");
    writeln!(lock, " {}", name).expect("Failed to write");
}