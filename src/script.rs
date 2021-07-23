use std::fmt::{self, Display, Formatter};

use conch_parser::ast::TopLevelCommand;
use enum_map::EnumMap;
use linked_hash_map::LinkedHashMap;

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
    /// The scripts defined under `$#name`
    ///
    /// These scripts are executed in their respective `ScriptPhase` if they were chosen as the target
    pub targets: LinkedHashMap<String, EnumMap<ScriptPhase, Option<Script>>>,
}

#[derive(Debug, PartialEq, Hash, Eq, Clone, Copy, enum_map::Enum)]
pub enum ScriptPhase {
    BuildOnly,   // b!
    Build,       // b
    BuildAndRun, // br (default)
    Run,         // r
    RunOnly,     // r!
}

#[derive(Clone, Debug)]
pub struct Script {
    pub commands: Vec<TopLevelCommand<String>>,
    pub location: RunscriptLocation,
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
            Some(map) if map.values().any(Option::is_some) => Some(map),
            _ => {
                for include in &self.includes {
                    match include.runscript.scripts.targets.get(target) {
                        Some(map) if map.values().any(Option::is_some) => {
                            return Some(map);
                        }
                        _ => {}
                    }
                }
                None
            }
        }
    }

    pub fn get_default_script(&self, phase: ScriptPhase) -> Option<&Script> {
        match self
            .scripts
            .targets
            .get("")
            .map(|o| o[phase].as_ref())
            .flatten()
        {
            Some(script) => Some(&script),
            _ => {
                for include in &self.includes {
                    if let Some(script) = include
                        .runscript
                        .scripts
                        .targets
                        .get("")
                        .map(|o| o[phase].as_ref())
                        .flatten()
                    {
                        return Some(&script);
                    }
                }
                None
            }
        }
    }
}

impl Display for ScriptPhase {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                ScriptPhase::BuildOnly => "Build!",
                ScriptPhase::Build => "Build",
                ScriptPhase::BuildAndRun => "Build & Run",
                ScriptPhase::Run => "Run",
                ScriptPhase::RunOnly => "Run!",
            }
        )
    }
}
