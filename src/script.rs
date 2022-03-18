use std::collections::HashMap;

use crate::shell::ast::AtomicTopLevelCommand;
use indexmap::IndexMap;

/// A parsed runscript
#[derive(Clone, Debug)]
pub struct Runscript {
    /// The name of the runscript for the location tracker
    pub name: String,
    /// The source of the runscript for emitting errors
    pub source: String,
    /// The scripts this runscript declares
    pub scripts: Scripts,
    /// Runtime options to change the behaviour of the interpreter
    pub options: Vec<String>,
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
    pub commands: Vec<AtomicTopLevelCommand>,
    pub line: usize,
}

impl Runscript {
    pub fn get_default_target(&self) -> Option<(&String, &HashMap<String, Script>)> {
        self.scripts.targets.get_index(0)
    }

    pub fn get_target(&self, target: &str) -> Option<(&String, &HashMap<String, Script>)> {
        self.scripts.targets.get_key_value(target)
    }
}
