#[derive(Debug)]
pub struct RunFile {
    pub global_target: Option<Target>,
    pub targets: Vec<Target>,
}

#[derive(Debug)]
pub struct Target {
    pub name: String,
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