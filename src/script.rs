use std::path::PathBuf;

use indexmap::IndexMap;

use crate::parser::ast::AtomicTopLevelCommand;

#[derive(Clone, Debug)]
pub struct Runscript {
    pub display_path: String,
    pub scripts: IndexMap<String, Target>,
    pub options: GlobalOptions,
}

#[derive(Clone, Debug, Default)]
pub struct CollatedTargets {
    pub scripts: IndexMap<String, Target>,
    pub options: GlobalOptions,
}

impl FromIterator<Runscript> for CollatedTargets {
    fn from_iter<T: IntoIterator<Item = Runscript>>(iter: T) -> Self {
        iter.into_iter().fold(Self::default(), |mut a, b| {
            a.scripts.reserve(b.scripts.len());
            for (name, target) in b.scripts {
                match a.scripts.entry(name) {
                    indexmap::map::Entry::Occupied(mut entry) => entry.get_mut().merge(target),
                    indexmap::map::Entry::Vacant(entry) => {
                        entry.insert(target);
                    }
                }
            }
            a.options.merge(b.options);
            a
        })
    }
}

#[derive(Clone, Debug, Default)]
pub struct Target {
    pub scripts: IndexMap<String, Script>,
    pub options: TargetOptions,
}

#[derive(Clone, Debug)]
pub struct Script {
    pub commands: ScriptExecution,
    pub canonical_path: PathBuf,
    pub working_dir: PathBuf,
    pub line: usize,
}

#[derive(Clone, Debug)]
pub enum ScriptExecution {
    Internal {
        commands: Vec<ScriptCommand>,
        options: ScriptOptions,
    },
    ExternalPosix {
        command: Vec<String>,
        script: String,
    },
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
}

impl CollatedTargets {
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

impl Target {
    pub fn merge(&mut self, mut other: Target) {
        other
            .scripts
            .retain(|key, _| !self.scripts.contains_key(key));
        self.scripts.extend(other.scripts);
        self.options.merge(other.options);
    }
}

impl GlobalOptions {
    pub fn merge(&mut self, other: GlobalOptions) {
        self.default_target.merge(other.default_target);
    }
}

impl TargetOptions {
    pub fn merge(&mut self, other: TargetOptions) {
        self.default_phase.merge(other.default_phase);
    }

    pub fn merge_unique(&mut self, other: TargetOptions) -> Result<(), ()> {
        self.default_phase.merge_unique(other.default_phase)?;
        Ok(())
    }
}

impl<T> Overrideable<T> {
    pub fn merge(&mut self, other: Overrideable<T>) {
        match (&*self, other) {
            (Unset, Set(phases)) => {
                *self = Set(phases);
            }
            (Unset, SetNone) => {
                *self = SetNone;
            }
            _ => {}
        }
    }

    pub fn merge_unique(&mut self, other: Overrideable<T>) -> Result<(), ()> {
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
