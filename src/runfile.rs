use std::collections::HashMap;
use std::fmt::{self, Display, Formatter};
use std::mem::discriminant;

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

#[derive(Debug)]
pub struct Argument {
    pub parts: Vec<ArgPart>
}

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

impl ArgPart {
    fn arg_str(&self) -> String {
        if let ArgPart::Str(s) = self {
            s.clone()
        } else {
            panic!("Expected Str argument");
        }
    }
}

impl Display for Command {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}{}", self.target, self.args.iter().fold("".to_owned(), |mut acc, x| {
            acc.push(' ');
            acc + &format!("{}", x)
        }))
    }
}

impl Display for Argument {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        if self.parts.iter().any(|x| discriminant(x) != discriminant(&ArgPart::Str("".to_owned()))) {
            write!(f, "\"{}\"", self.parts.iter().fold("".to_owned(), |acc, part| match part {
                ArgPart::Str(s) => acc + s,
                ArgPart::Var(v) => { acc + &format!("${}", v) },
                ArgPart::Cmd(c) => { acc + &format!("$({})", c) },
            }))
        } else if self.parts.iter().any(|x| x.arg_str().contains(' ')) {
            write!(f, "'{}'", self.parts.iter().fold("".to_owned(), |acc, part| acc + &part.arg_str()))
        } else {
            write!(f, "{}", self.parts.iter().fold("".to_owned(), |acc, part| acc + &part.arg_str()))
        }
    }
}