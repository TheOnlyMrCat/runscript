use std::collections::HashMap;
use std::fmt::{self, Display, Formatter};

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
pub enum Argument {
    Unquoted(ArgPart, (usize, usize)),
    Single(String),
    Double(Vec<ArgPart>),
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
        write!(f, "{}", match self {
            Argument::Unquoted(p, _) => match p {
                ArgPart::Str(s) => s.clone(),
                ArgPart::Arg(n) => format!("${}", n),
                ArgPart::Var(v) => format!("${}", v),
                ArgPart::Cmd(c) => format!("$({})", c),
            },
            Argument::Single(s) => format!("'{}'", s),
            Argument::Double(p) =>
                format!(
                    "\"{}\"",
                    p.iter().fold(
                        "".to_owned(),
                        |acc, part| match part {
                            ArgPart::Str(s) => acc + s,
                            ArgPart::Arg(n) => { acc + &format!("${}", n) },
                            ArgPart::Var(v) => { acc + &format!("${}", v) },
                            ArgPart::Cmd(c) => { acc + &format!("$({})", c) },
                        }
                    )
                )
        })
    }
}

impl Display for ScriptType {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            ScriptType::BuildOnly =>   "Build!",
            ScriptType::Build =>       "Build",
            ScriptType::BuildAndRun => "Build & Run",
            ScriptType::Run =>         "Run",
            ScriptType::RunOnly =>     "Run!"
        })
    }
}