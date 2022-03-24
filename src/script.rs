use std::collections::HashMap;

use crate::shell::ast::AtomicTopLevelCommand;
use indexmap::IndexMap;

/// A parsed runscript
#[derive(Clone, Debug)]
pub struct Runscript {
    pub name: String,
    pub source_text: String,
    pub scripts: IndexMap<String, Target>,
    pub options: GlobalOptions,
}

#[derive(Clone, Debug, Default)]
pub struct Target {
    pub scripts: HashMap<String, Script>,
    pub options: TargetOptions,
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
    pub default_target: Overrideable<String>,
}

#[derive(Clone, Debug, Default)]
pub struct TargetOptions {
    pub default_phase: Overrideable<Vec<String>>,
}

#[derive(Clone, Debug, Default)]
pub struct ScriptOptions {}

#[derive(Clone, Debug, Default)]
pub struct CommandOptions {}

#[derive(Clone, Debug)]
pub enum Overrideable<T> {
    Set(T),
    SetNone,
    Unset,
}

use Overrideable::*;

impl<T> Default for Overrideable<T> {
    fn default() -> Self {
        Self::Unset
    }
}

impl Runscript {
    pub fn get_default_target(&self) -> Option<(&String, &Target)> {
        match self.options.default_target {
            Set(ref name) => self.scripts.get_key_value(name),
            SetNone => None,
            Unset => self.scripts.get_index(0),
        }
    }

    pub fn get_target(&self, target: &str) -> Option<(&String, &Target)> {
        self.scripts.get_key_value(target)
    }
}

impl TargetOptions {
    pub fn merge(&mut self, other: TargetOptions) -> Result<(), ()> {
        self.default_phase.merge(other.default_phase)?;
        Ok(())
    }
}

impl<T> Overrideable<T> {
    pub fn merge(&mut self, other: Overrideable<T>) -> Result<(), ()> {
        match (&*self, other) {
            (Unset, Set(phases)) => {
                *self = Set(phases);
            }
            (Unset, SetNone) => {
                *self = SetNone;
            }
            (_, Unset) => {}
            _ => {
                return Err(());
            }
        }

        Ok(())
    }
}
