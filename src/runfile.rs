use std::collections::HashMap;

#[derive(Debug)]
pub struct RunFile {
    pub global_target: Option<Target>,
    pub default_target: Option<Target>,
    pub targets: HashMap<String, Target>,
}

#[derive(Debug)]
pub struct Target {
    pub commands: Vec<Command>,
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