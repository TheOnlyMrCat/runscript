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
    pub commands: HashMap<TargetMeta, TargetInfo>,
}

#[derive(Debug,PartialEq,Hash,Eq)]
pub struct TargetMeta {
    pub script: ScriptType,
}

#[derive(Debug)]
pub struct TargetInfo {
    pub commands: Vec<Command>,
    pub loc: (usize, usize),
}

#[derive(Debug,PartialEq,Hash,Eq,Clone,Copy)]
pub enum ScriptType {
    BuildOnly,   // b!
    Build,       // b
    BuildAndRun, // br (default)
    Run,         // r
    RunOnly,     // r!
}

#[derive(Debug, Clone)]
pub struct Command {
    pub target: String,
    pub args: Vec<Argument>,
    pub chained: Box<ChainedCommand>,
    pub loc: (usize, usize),
}

#[derive(Debug, Clone)]
pub struct Argument {
    pub parts: Vec<ArgPart>
}

#[derive(Debug, Clone)]
pub enum ChainedCommand {
    And(Command),
    Or(Command),
    Pipe(Command),
    None,
}

#[derive(Debug, Clone)]
pub enum ArgPart {
    Str(String),
    Arg(usize),
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
        write!(f, "{}{}{}", match &*self.chained {
            ChainedCommand::None => "".to_owned(),
            ChainedCommand::And(c) => format!("{} && ", c),
            ChainedCommand::Or(c) => format!("{} || ", c),
            ChainedCommand::Pipe(c) => format!("{} | ", c),
        }, self.target, self.args.iter().fold("".to_owned(), |mut acc, x| {
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
                ArgPart::Arg(n) => { acc + &format!("${}", n) },
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

impl Display for ScriptType {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            ScriptType::BuildOnly =>   "\x1b[91;1mBuild!\x1b[0m",
            ScriptType::Build =>       "\x1b[93;1mBuild\x1b[0m",
            ScriptType::BuildAndRun => "\x1b[92;1mBuild & Run\x1b[0m",
            ScriptType::Run =>         "\x1b[94;1mRun\x1b[0m",
            ScriptType::RunOnly =>     "\x1b[95;1mRun!\x1b[0m"
        })
    }
}