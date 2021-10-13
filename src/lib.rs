pub mod parser;
mod script;

#[doc(inline)]
pub use script::*;

pub mod exec;

use exec::{FinishedProcess, Verbosity};
use std::collections::HashMap;
use std::path::Path;
use std::process::{Command, Stdio};

/// Alternate definition of `run` for `exec` when it makes a recursive call
pub(crate) fn run(
    args: &[&str],
    cwd: &Path,
    inherit_verbosity: Verbosity,
    capture_stdout: bool,
    env_remap: &HashMap<String, String>,
) -> Result<FinishedProcess, std::io::Error> {
    let mut command = Command::new("run");
    command
        .args(args)
        .envs(env_remap)
        .current_dir(cwd)
        .stdin(Stdio::inherit())
        .stdout(if capture_stdout {
            Stdio::piped()
        } else {
            Stdio::inherit()
        })
        .stderr(if capture_stdout {
            Stdio::piped()
        } else {
            Stdio::inherit()
        });
    match inherit_verbosity {
        Verbosity::Normal => &mut command,
        Verbosity::Quiet => command.arg("-q"),
        Verbosity::Silent => command.arg("-qq"),
    };
    let output = command.output()?;
    Ok(FinishedProcess::from(output))
}
