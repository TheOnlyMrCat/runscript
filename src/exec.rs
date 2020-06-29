use std::ffi::OsStr;
use std::fs::File;
use std::io::prelude::*;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio, ExitStatus};

use getopts::Options;

use crate::runfile::{self, TargetMeta, ScriptType, ArgPart};
use crate::Config;

use crate::parser;

const PHASES_B: [ScriptType; 2] = [ScriptType::BuildOnly, ScriptType::Build];
const PHASES_T: [ScriptType; 3] = [ScriptType::Build, ScriptType::BuildAndRun, ScriptType::Run];
const PHASES_R: [ScriptType; 2] = [ScriptType::Run, ScriptType::RunOnly];

pub fn run<T: Iterator>(args: T, cwd: &Path)
    where T::Item: AsRef<OsStr>
{
    let mut options = Options::new();

    options.optflag("h", "help", "Show this very helpful text");
    options.optflagmulti("q", "quiet", "Passed once: Do not show output of run commands. Twice: Do not print commands as they are being run");
    options.optflag("b", "build-only", "Only execute `b!` and `b` scripts");
    options.optflag("", "build-and-run", "Execute `b`, `br`, and `r` scripts (default)");
    options.optflag("r", "run-only", "Only execute `r` and `r!` scripts");

    let matches = match options.parse(args) {
        Ok(m) => m,
        Err(x) => panic!(x),
    };

    if matches.opt_present("help") {
        print!("{}", options.usage("Usage: run [options] target"));
        return;
    }

    let mut runfile_path = PathBuf::from(cwd);
    let run_target: String;
    if let Some(target) = matches.free.get(0) {
        let v = target.split(':').collect::<Vec<&str>>();
        
        if v.len() == 1 {
            runfile_path.push("run");
            run_target = v[0].to_owned();
        } else if v.len() == 2 {
            let mut s = v[0].to_owned();
            s.push_str(".run");
            runfile_path.push(s);
            run_target = v[1].to_owned();
        } else {
            panic!("Invalid target");
        }
    } else {
        runfile_path.push("run");
        run_target = String::new();
    }

    let b_pos = matches.opt_positions("build-only")   .iter().fold(-1, |acc, &x| if x as i32 > acc { x as i32 } else { acc });
    let t_pos = matches.opt_positions("build-and-run").iter().fold(-1, |acc, &x| if x as i32 > acc { x as i32 } else { acc });
    let r_pos = matches.opt_positions("run-only")     .iter().fold(-1, |acc, &x| if x as i32 > acc { x as i32 } else { acc });

    let phases: &[ScriptType];
    if b_pos + t_pos + r_pos == -3
    || t_pos > b_pos && t_pos > r_pos {
        phases = &PHASES_T;
    } else if b_pos > t_pos && b_pos > r_pos {
        phases = &PHASES_B;
    } else if r_pos > t_pos && r_pos > b_pos {
        phases = &PHASES_R;
    } else {
        panic!("Failed to identify script phases to run")
    }

    let config = Config {
        quiet: matches.opt_present("quiet"),
        silent: matches.opt_count("quiet") > 1,
        file: runfile_path,
        args: matches.free[1..].to_vec()
    };

    let mut file = String::new();
    File::open(&config.file).expect("Failed to open file").read_to_string(&mut file).expect("Failed to read file");

    match parser::RunFileParser::new().parse(&file) {
        Ok(rf) => {
            for &phase in phases {
                if run_target == "" {
                    match &rf.default_target {
                        Some(target) => {
                            match target.commands.get(&TargetMeta { script: phase }).and_then(|c| {
                                println!("{} default", phase);
                                if shell(&c, &config) {
                                    None
                                } else {
                                    Some(())
                                }
                            }) {
                                Some(_) => break,
                                None => {}
                            }
                        },
                        None => {}
                    }
                } else {
                    match rf.targets.get(&run_target) {
                        Some(target) => {
                            match target.commands.get(&TargetMeta { script: phase }).and_then(|c| {
                                println!("{} {}", phase, run_target);
                                if shell(&c, &config) {
                                    None
                                } else {
                                    Some(())
                                }
                            }) {
                                Some(_) => break,
                                None => {}
                            }
                        },
                        None => panic!("No target with name '{}'", run_target)
                    }
                }
                match &rf.global_target {
                    Some(target) => {
                        match target.commands.get(&TargetMeta { script: phase }).and_then(|c| {
                            println!("{} global", phase);
                            if shell(&c, &config) {
                                None
                            } else {
                                Some(())
                            }
                        }) {
                            Some(_) => break,
                            None => {}
                        }
                    },
                    None => {}
                }
            }
        },
        Err(e) => {
            eprintln!("{:#?}", e);
        }
    }
}

fn shell(commands: &Vec<runfile::Command>, config: &Config) -> bool {
    for command in commands {
        if !exec(command, config) {
            return false;
        }
    }
    return true;
}

fn exec(command: &runfile::Command, config: &Config) -> bool {
    if !config.silent {
        println!("> {}", command);
    }
    if command.target == "run" {
        run(command.args.iter().map(|x| x.parts.iter().fold("".to_owned(), fold_arg(config.clone()))), config.file.parent().expect("Runfile should have at least one parent"));
        return true;
    } else {
        let mut child = Command::new(command.target.clone())
            .args(command.args.iter().map(|x| x.parts.iter().fold("".to_owned(), fold_arg(config.clone()))))
            .current_dir(config.file.parent().expect("Runfile should have at least one parent"))
            .stdin(Stdio::null())
            .stdout(if config.quiet { Stdio::null() } else { Stdio::inherit() })
            .stderr(if config.quiet { Stdio::null() } else { Stdio::inherit() })
            .spawn()
            .expect("Failed to execute command"); //TODO: What if it fails?
        
        /*
        if !config.quiet {
            //TODO: Might make time-continuous? Also there're some unwraps here
            println!("  Stdout:");
            for line in BufReader::new(child.stdout.as_mut().unwrap()).lines() {
                println!("    {}", line.unwrap());
            }
            println!("  Stderr:");
            for line in BufReader::new(child.stderr.as_mut().unwrap()).lines() {
                println!("    {}", line.unwrap());
            }
        }
        */
        let status = child.wait().expect("Command wasn't running properly");

        if !status.success() {
            if !config.silent {
                match status.code() {
                    Some(i) => println!(">> exit {}", i),
                    None => println!(">> {}", signal(&status)),
                }
            }
            return false;
        }
        return true;
    }
}

fn fold_arg(config: Config) -> Box<dyn Fn(String, &ArgPart) -> String> {
    Box::new(move |acc, p| match p {
        ArgPart::Str(s) => acc + s,
        ArgPart::Arg(n) => acc + &config.args[*n - 1],
        ArgPart::Var(v) => acc + &std::env::var(v).unwrap_or("".to_owned()),
        ArgPart::Cmd(c) => acc + &String::from_utf8_lossy(&Command::new(c.target.clone())
            .args(c.args.iter().map(|x| x.parts.iter().fold("".to_owned(), fold_arg(config.clone()))))
            .current_dir(config.file.parent().expect("Runfile should have at least one parent"))
            .output()
            .expect("Failed to execute command").stdout),
    })
}

#[allow(dead_code)]
#[cfg(unix)]
fn signal(status: &ExitStatus) -> String {
    use std::os::unix::process::ExitStatusExt;

    format!("signal {}", status.signal().expect("Expected signal"))
}

#[allow(dead_code)]
#[cfg(not(unix))]
fn signal(_status: &ExitStatus) -> String {
    "error".to_owned()
}