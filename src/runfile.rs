use std::collections::HashMap;

#[derive(Debug)]
pub struct RunFile {
    pub global_target: Option<Target>,
    pub default_target: Option<Target>,
    pub targets: HashMap<String, Target>,
}

#[derive(Debug)]
pub struct Target {
    pub commands: HashMap<TargetMeta, Vec<Command>>,
}

#[derive(Debug,PartialEq,Hash,Eq)]
pub struct TargetMeta {
    pub script: ScriptType,
}

#[derive(Debug,PartialEq,Hash,Eq,Clone)]
pub enum ScriptType {
    BuildOnly,   // b!
    Build,       // b
    BuildAndRun, // br (default)
    Run,         // r
    RunOnly,     // r!
}

#[derive(Debug)]
pub struct Command {
    pub target: String,
    pub args: Vec<Argument>,
}

pub type Argument = Vec<ArgPart>;

#[derive(Debug)]
pub enum ArgPart {
    Str(String),
    Var(String),
    Cmd(Command),
}

impl Default for Target {
    fn default() -> Self {
        Target {
            commands: HashMap::default(),
        }
    }
}