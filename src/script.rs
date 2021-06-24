use std::collections::HashMap;
use std::fmt::{self, Display, Formatter};

use enum_map::EnumMap;

use crate::parser::RunscriptLocation;

/// A parsed runscript
#[derive(Clone, Debug)]
pub struct Runscript {
	/// The name of the runscript for the location tracker
	pub name: String,
	/// The source of the runscript for emitting errors
	pub source: String,
	/// The runscripts this runscript includes.
	pub includes: Vec<RunscriptInclude>,
	/// The scripts this runscript declares
	pub scripts: Scripts,
	/// Runtime options to change the behaviour of the interpreter
	pub options: Vec<String>,
}

/// The script and source location of an included runscript
#[derive(Clone, Debug)]
pub struct RunscriptInclude {
	pub runscript: Runscript,
	/// Where the include statement is in the runscript which is including the other
	pub location: RunscriptLocation,
}

/// The exectable scripts defined by a [`Runscript`](struct.Runscript.html)
#[derive(Clone, Debug)]
pub struct Scripts {
	/// The scripts defined under `#<`
	/// 
	/// These scripts are executed in their respective `ScriptPhase` before the targeted target.
	pub pre_global_target: EnumMap<ScriptPhase, Option<Script>>,
	/// The scripts defined under `##`
	/// 
	/// These scripts are executed in their respective `ScriptPhase` after the targeted target.
	pub global_target: EnumMap<ScriptPhase, Option<Script>>,
	/// The scripts defined under `#-`
	/// 
	/// These scripts are executed in their respective `ScriptPhase` as the targeted target if no target is specified
	pub default_target: EnumMap<ScriptPhase, Option<Script>>,
	/// The scripts defined under `#name`
	/// 
	/// These scripts are executed in their respective `ScriptPhase` if they were chosen as the target
	pub targets: HashMap<String, EnumMap<ScriptPhase, Option<Script>>>,
}

#[derive(Clone, Debug)]
pub struct Script {
	pub commands: Vec<ScriptEntry>,
    pub location: RunscriptLocation,
}

#[derive(Debug,PartialEq,Hash,Eq,Clone,Copy,enum_map::Enum)]
pub enum ScriptPhase {
    BuildOnly,   // b!
    Build,       // b
    BuildAndRun, // br (default)
    Run,         // r
    RunOnly,     // r!
}

#[derive(Debug, Clone)]
pub enum ScriptEntry {
	Command(TopLevelCommand),
	Env {
		var: String,
		val: Argument,
		loc: RunscriptLocation,
	},
}

impl ScriptEntry {
	pub fn expect_command(self) -> Option<Command> {
		match self {
			ScriptEntry::Command(TopLevelCommand::Command(c)) => Some(c),
			_ => None,
		}
	}
}

#[derive(Debug, Clone)]
pub enum TopLevelCommand {
	Command(Command),
	BlockCommand(Vec<ScriptEntry>),
}

#[derive(Debug, Clone)]
pub struct Command {
    pub target: Box<Argument>,
    pub args: Vec<Argument>,
    pub chained: Box<ChainedCommand>,
    pub loc: RunscriptLocation,
}

#[derive(Debug, Clone)]
pub enum Argument {
    Unquoted(ArgPart),
    Single(String),
    Double(Vec<ArgPart>),
}

#[derive(Debug, Clone)]
pub enum ChainedCommand {
    And(TopLevelCommand),
    Or(TopLevelCommand),
    Pipe(Command),
    None,
}

#[derive(Debug, Clone)]
pub enum ArgPart {
    Str(String),
	Arg(usize),
	AllArgs,
    Var(String),
    Cmd(Command),
}

impl Runscript {
	pub fn unwind_fileid(&self, id: &[usize]) -> Option<&Runscript> {
		if id.is_empty() {
			Some(&self)
		} else {
			let mut file_ref = self;
			for index in id {
				file_ref = &file_ref.includes.get(*index)?.runscript;
			}
			Some(file_ref)
		}
	}

	pub fn get_target(&self, target: &str) -> Option<&EnumMap<ScriptPhase, Option<Script>>> {
		match self.scripts.targets.get(target).as_ref() {
			Some(map) if map.values().any(Option::is_some) => {
				Some(map)
			},
			_ => {
				for include in &self.includes {
					match include.runscript.scripts.targets.get(target) {
						Some(map) if map.values().any(Option::is_some) => {
							return Some(map);
						},
						_ => {}
					}
				}
				None
			}
		}
	}

	pub fn get_default_script(&self, phase: ScriptPhase) -> Option<&Script> {
		match self.scripts.default_target[phase].as_ref() {
			Some(script) => {
				Some(&script)
			},
			_ => {
				for include in &self.includes {
					if let Some(script) = include.runscript.scripts.default_target[phase].as_ref() {
						return Some(&script);
					}
				}
				None
			}
		}
	}

	pub fn get_global_script(&self, phase: ScriptPhase) -> Option<&Script> {
		match self.scripts.global_target[phase].as_ref() {
			Some(script) => {
				Some(&script)
			},
			_ => {
				for include in &self.includes {
					if let Some(script) = include.runscript.scripts.global_target[phase].as_ref() {
						return Some(&script);
					}
				}
				None
			}
		}
	}

	pub fn get_pre_global_script(&self, phase: ScriptPhase) -> Option<&Script> {
		match self.scripts.pre_global_target[phase].as_ref() {
			Some(script) => {
				Some(&script)
			},
			_ => {
				for include in &self.includes {
					if let Some(script) = include.runscript.scripts.pre_global_target[phase].as_ref() {
						return Some(&script);
					}
				}
				None
			}
		}
	}
}

impl Display for ScriptEntry {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		match self {
			ScriptEntry::Command(c) => c.fmt(f),
			ScriptEntry::Env { var, val, .. } => write!(f, "{}={}", var, val),
		}
	}
}

impl Display for TopLevelCommand {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		match self {
			TopLevelCommand::Command(c) => c.fmt(f),
			TopLevelCommand::BlockCommand(_) => write!(f, "{{ ... }}"),
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
            Argument::Unquoted(p) => match p {
                ArgPart::Str(s) => s.clone(),
                ArgPart::Arg(n) => format!("${}", n),
                ArgPart::Var(v) => format!("${}", v),
				ArgPart::Cmd(c) => format!("$({})", c),
				ArgPart::AllArgs => "$@".to_owned(),
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
							ArgPart::AllArgs => { acc + "$@" },
                        }
                    )
                )
        })
    }
}

impl Display for ScriptPhase {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            ScriptPhase::BuildOnly =>   "Build!",
            ScriptPhase::Build =>       "Build",
            ScriptPhase::BuildAndRun => "Build & Run",
            ScriptPhase::Run =>         "Run",
            ScriptPhase::RunOnly =>     "Run!"
        })
    }
}