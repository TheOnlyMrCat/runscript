use std::collections::HashMap;

use conch_parser::ast::AtomicTopLevelCommand;
use indexmap::IndexMap;

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
    pub targets: IndexMap<String, HashMap<String, Script>>,
}

#[derive(Clone, Debug)]
pub struct Script {
    pub commands: Vec<AtomicTopLevelCommand<String>>,
    pub location: RunscriptLocation,
}

impl Runscript {
    pub fn unwind_fileid(&self, id: &[usize]) -> Option<&Runscript> {
        if id.is_empty() {
            Some(self)
        } else {
            let mut file_ref = self;
            for index in id {
                file_ref = &file_ref.includes.get(*index)?.runscript;
            }
            Some(file_ref)
        }
    }

    pub fn get_default_target(&self) -> Option<(&String, &HashMap<String, Script>)> {
        self
            .scripts
            .targets
            .get_index(0)
    }

    pub fn get_target(&self, target: &str) -> Option<(&String, &HashMap<String, Script>)> {
        self
            .scripts
            .targets
            .get_key_value(target)
    }
}
