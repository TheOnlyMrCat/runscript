use std::ops::Range;
use std::path::Path;

use crate::runfile::RunFileRef;

pub struct RunscriptSource<'a> {
	/// The name of the runfile this runscript belongs to, for location-tracking purposes.
	name: String,
	/// The include-nesting index of this runscript
	index: Vec<usize>,
	/// The runscript code to be parsed. Generally the contents of a runfile.
	source: &'a str,
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

pub fn parse_runfile(path: impl AsRef<Path>) -> Result<RunFileRef, RunscriptParseError> {
	parse_runscript(RunscriptSource {
		name: path.as_ref().file_name()
			.ok_or(RunscriptParseError::InvalidPath)?
			.to_string_lossy()
			.into_owned(),
		index: Vec::new(),
		source: &std::fs::read_to_string(path).map_err(|e| RunscriptParseError::IOError(e))?,
	})
}

pub fn parse_runfile_nested(path: impl AsRef<Path>, index: Vec<usize>) -> Result<RunFileRef, RunscriptParseError> {
	parse_runscript(RunscriptSource {
		name: path.as_ref().file_name()
			.ok_or(RunscriptParseError::InvalidPath)?
			.to_string_lossy()
			.into_owned(),
		index: Vec::new(),
		source: &std::fs::read_to_string(path).map_err(|e| RunscriptParseError::IOError(e))?,
	})
}

struct ParsingContext {
	
}

pub fn parse_runscript(source: RunscriptSource) -> Result<RunFileRef, RunscriptParseError> {
	todo!();
}