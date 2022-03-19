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
    pub scripts: IndexMap<String, HashMap<String, Script>>,
    /// Runtime options to change the behaviour of the interpreter
    pub options: GlobalOptions,
}

#[derive(Clone, Debug)]
pub struct Script {
    pub commands: Vec<ScriptCommand>,
    pub line: usize,
    pub options: ScriptOptions,
}

#[derive(Clone, Debug)]
pub struct ScriptCommand {
    pub command: AtomicTopLevelCommand,
    pub options: CommandOptions,
}

#[derive(Clone, Debug, Default)]
pub struct GlobalOptions {
    pub default_target: Option<Option<String>>,
}

#[derive(Clone, Debug, Default)]
pub struct ScriptOptions {
    pub default_phase: Vec<String>,
}

#[derive(Clone, Debug, Default)]
pub struct CommandOptions {
    pub ignore_exit_code: bool,
}

impl Runscript {
    pub fn get_default_target(&self) -> Option<(&String, &HashMap<String, Script>)> {
        match self.options.default_target {
            Some(Some(ref name)) => self.scripts.get_key_value(name),
            Some(None) => None,
            None => self.scripts.get_index(0),
        }
    }

    pub fn get_target(&self, target: &str) -> Option<(&String, &HashMap<String, Script>)> {
        self.scripts.get_key_value(target)
    }
}
