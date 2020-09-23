use std::collections::HashMap;
use std::fmt::{self, Display, Formatter};

use crate::out::Location;

#[derive(Debug)]
pub struct RunFileRef {
	pub file: Option<RunFile>,
	pub name: String,
	pub source: Box<str>,
	pub line_ends: Vec<usize>,
}

#[derive(Debug)]
pub struct RunFile {
    pub global_target: Option<Target>,
    pub default_target: Option<Target>,
	pub targets: HashMap<String, Target>,
	pub includes: Vec<RunFileRef>,
}

#[derive(Debug)]
pub struct Target {
    pub commands: HashMap<TargetMeta, TargetInfo>,
}

#[derive(Debug,PartialEq,Hash,Eq)]
pub struct TargetMeta {
    pub script: ScriptType,
}

#[derive(Debug)]
pub struct TargetInfo {
	pub commands: Vec<Command>,
    pub loc: Location,
}

#[derive(Debug,PartialEq,Hash,Eq,Clone,Copy)]
pub enum ScriptType {
    BuildOnly,   // b!
    Build,       // b
    BuildAndRun, // br (default)
    Run,         // r
    RunOnly,     // r!
}

#[derive(Debug, Clone)]
pub struct Command {
    pub target: String,
    pub args: Vec<Argument>,
    pub chained: Box<ChainedCommand>,
    pub loc: Location,
}

#[derive(Debug, Clone)]
pub enum Argument {
    Unquoted(ArgPart, Location),
    Single(String),
    Double(Vec<ArgPart>),
}

#[derive(Debug, Clone)]
pub enum ChainedCommand {
    And(Command),
    Or(Command),
    Pipe(Command),
    None,
}

#[derive(Debug, Clone)]
pub enum ArgPart {
    Str(String),
    Arg(usize),
    Var(String),
    Cmd(Command),
}

impl RunFile {
	pub fn get_default_target(&self) -> Option<&Target> {
		if let Some(t) = &self.default_target {
			return Some(t);
		}
		for included in &self.includes {
			if let Some(t) = &included.file.as_ref()?.get_default_target() {
				return Some(t);
			}
		}
		None
	}

	pub fn get_global_target(&self) -> Option<&Target> {
		if let Some(t) = &self.global_target {
			return Some(t);
		}
		for included in &self.includes {
			if let Some(t) = &included.file.as_ref()?.get_global_target() {
				return Some(t);
			}
		}
		None
	}

	pub fn get_target(&self, name: &String) -> Option<&Target> {
		if let Some(t) = &self.targets.get(name) {
			return Some(t);
		}
		for included in &self.includes {
			if let Some(t) = &included.file.as_ref()?.get_target(name) {
				return Some(t);
			}
		}
		None
	}
}

impl RunFileRef {
	pub fn unwind_fileid(&self, id: &[usize]) -> Option<&RunFileRef> {
		if id.len() == 0 {
			Some(&self)
		} else {
			let mut file_ref = self;
			for index in id {
				file_ref = file_ref.file.as_ref()?.includes.get(*index)?
			}
			Some(file_ref)
		}
	}
}

impl Default for Target {
    fn default() -> Self {
        Target {
            commands: HashMap::default(),
        }
    }
}

impl Display for Command {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}{}{}", match &*self.chained {
            ChainedCommand::None => "".to_owned(),
            ChainedCommand::And(c) => format!("{} && ", c),
            ChainedCommand::Or(c) => format!("{} || ", c),
            ChainedCommand::Pipe(c) => format!("{} | ", c),
        }, self.target, self.args.iter().fold("".to_owned(), |mut acc, x| {
            acc.push(' ');
            acc + &format!("{}", x)
        }))
    }
}

impl Display for Argument {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            Argument::Unquoted(p, _) => match p {
                ArgPart::Str(s) => s.clone(),
                ArgPart::Arg(n) => format!("${}", n),
                ArgPart::Var(v) => format!("${}", v),
                ArgPart::Cmd(c) => format!("$({})", c),
            },
            Argument::Single(s) => format!("'{}'", s),
            Argument::Double(p) =>
                format!(
                    "\"{}\"",
                    p.iter().fold(
                        "".to_owned(),
                        |acc, part| match part {
                            ArgPart::Str(s) => acc + s,
                            ArgPart::Arg(n) => { acc + &format!("${}", n) },
                            ArgPart::Var(v) => { acc + &format!("${}", v) },
                            ArgPart::Cmd(c) => { acc + &format!("$({})", c) },
                        }
                    )
                )
        })
    }
}

impl Display for ScriptType {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            ScriptType::BuildOnly =>   "Build!",
            ScriptType::Build =>       "Build",
            ScriptType::BuildAndRun => "Build & Run",
            ScriptType::Run =>         "Run",
            ScriptType::RunOnly =>     "Run!"
        })
    }
}