use std::collections::HashMap;
use std::ops::Range;
use std::path::Path;

use crate::script::{Runscript, Scripts};

pub struct RunscriptSource {
	/// The name of the runfile this runscript belongs to, for location-tracking purposes.
	name: String,
	/// The include-nesting index of this runscript
	index: Vec<usize>,
	/// The runscript code to be parsed. Generally the contents of a runfile.
	source: String,
}

pub struct RunscriptLocation {
	/// The include-nesting index of this runscript
	index: Box<[usize]>,
	/// The location of the referenced code
	range: Range<usize>,
}

pub enum RunscriptParseError {
	InvalidPath,
	IOError(std::io::Error),
}

pub fn parse_runfile(path: impl AsRef<Path>) -> Result<Runscript, RunscriptParseError> {
	parse_runscript(RunscriptSource {
		name: path.as_ref().file_name()
			.ok_or(RunscriptParseError::InvalidPath)?
			.to_string_lossy()
			.into_owned(),
		index: Vec::new(),
		source: std::fs::read_to_string(path).map_err(|e| RunscriptParseError::IOError(e))?,
	})
}

pub fn parse_runfile_nested(path: impl AsRef<Path>, index: Vec<usize>) -> Result<Runscript, RunscriptParseError> {
	parse_runscript(RunscriptSource {
		name: path.as_ref().file_name()
			.ok_or(RunscriptParseError::InvalidPath)?
			.to_string_lossy()
			.into_owned(),
		index: Vec::new(),
		source: std::fs::read_to_string(path).map_err(|e| RunscriptParseError::IOError(e))?,
	})
}

struct ParsingContext<T: Iterator<Item = (usize, char)>> {
	iterator: std::iter::Peekable<T>,
	runfile: Runscript,
}

pub fn parse_runscript(source: RunscriptSource) -> Result<Runscript, RunscriptParseError> {
	let mut context = ParsingContext {
		iterator: source.source.char_indices().peekable(),
		runfile: Runscript {
			name: source.name,
			line_ends: source.source.char_indices().filter_map(|(index, ch)| if ch == '\n' { Some(index) } else { None }).collect(),
			source: source.source,
			includes: Vec::new(),
			scripts: Scripts {
				global_target: HashMap::new(),
				default_target: HashMap::new(),
				targets: HashMap::new(),
			}
		}
	};
	todo!();
}