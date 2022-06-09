use std::fs::File;
use std::io::Read;
use std::path::PathBuf;
use std::process::{Child, ExitStatus, Output, Stdio};
use std::str::Utf8Error;
use std::sync::Arc;

use glob::PatternError;

use crate::exec::{BaseExecContext, ShellContext};
use crate::parser::ast::{self, CompoundCommand};
use crate::parser::RunscriptSource;

#[derive(Debug)]
pub enum CommandExecError {
    InvalidGlob { glob: String, err: PatternError },
    NoGlobMatches { glob: String },
    CommandFailed { err: std::io::Error },
    UnhandledOsString { err: Utf8Error },
    BadRedirect { err: std::io::Error },
    UnsupportedRedirect,
    UnsupportedIntermediatePipelineCommand,
}

#[derive(Debug)]
pub enum ProcessExit {
    Bool(bool),
    StdStatus(ExitStatus),
    ExecError(CommandExecError),
    #[cfg(unix)]
    NixStatus(nix::sys::wait::WaitStatus),
}

impl ProcessExit {
    /// Whether the process was successful or not.
    ///
    /// Returns the value of a `Bool` variant, or calls the `success` function on a `Status` variant
    pub fn success(&self) -> bool {
        match self {
            ProcessExit::Bool(b) => *b,
            ProcessExit::StdStatus(s) => s.success(),
            ProcessExit::ExecError(_) => false,
            #[cfg(unix)]
            ProcessExit::NixStatus(s) => matches!(s, nix::sys::wait::WaitStatus::Exited(_, 0)),
        }
    }

    /// The coerced exit code of the process.
    ///
    /// Coerces the `Bool` variant into `true = 0`, `false = 1`
    /// Signals are converted to an exit code of `128 + signal`
    /// Execution errors are converted to an exit code of `127`
    pub fn coerced_code(&self) -> i32 {
        match self {
            ProcessExit::Bool(b) => !b as i32,
            ProcessExit::StdStatus(s) => s.code().unwrap_or_else(|| {
                #[cfg(unix)]
                {
                    use std::os::unix::prelude::ExitStatusExt;

                    s.signal().map(|s| 128 + s as i32).unwrap()
                }
                #[cfg(not(unix))]
                {
                    unreachable!()
                }
            }),
            ProcessExit::ExecError(_) => 127,
            #[cfg(unix)]
            ProcessExit::NixStatus(s) => match s {
                nix::sys::wait::WaitStatus::Exited(_, code) => *code,
                nix::sys::wait::WaitStatus::Signaled(_, sig, _) => 128 + *sig as i32,
                _ => unreachable!(),
            },
        }
    }
}

#[derive(Debug)]
pub struct FinishedProcess {
    pub status: ProcessExit,
    pub stdout: Vec<u8>,
    pub stderr: Vec<u8>,
}

enum OngoingProcess {
    Concurrent(Child),
    #[cfg(unix)]
    Reentrant {
        pid: nix::unistd::Pid,
        stdin: Option<std::os::unix::prelude::RawFd>,
        stdout: Option<std::os::unix::prelude::RawFd>,
        stderr: Option<std::os::unix::prelude::RawFd>,
    },
    Finished(FinishedProcess),
}

pub struct WaitableProcess {
    process: OngoingProcess,
    associated_jobs: Vec<WaitableProcess>,
}

impl WaitableProcess {
    pub fn new(process: Child) -> WaitableProcess {
        WaitableProcess {
            process: OngoingProcess::Concurrent(process),
            associated_jobs: vec![],
        }
    }

    #[cfg(unix)]
    pub fn reentrant_nightmare(
        pid: nix::unistd::Pid,
        stdin: Option<std::os::unix::prelude::RawFd>,
        stdout: Option<std::os::unix::prelude::RawFd>,
        stderr: Option<std::os::unix::prelude::RawFd>,
    ) -> WaitableProcess {
        WaitableProcess {
            process: OngoingProcess::Reentrant {
                pid,
                stdin,
                stdout,
                stderr,
            },
            associated_jobs: vec![],
        }
    }

    pub fn finished(process: FinishedProcess) -> WaitableProcess {
        WaitableProcess {
            process: OngoingProcess::Finished(process),
            associated_jobs: vec![],
        }
    }

    pub fn empty_success() -> WaitableProcess {
        WaitableProcess {
            process: OngoingProcess::Finished(FinishedProcess {
                status: ProcessExit::Bool(true),
                stdout: vec![],
                stderr: vec![],
            }),
            associated_jobs: vec![],
        }
    }

    pub fn empty_error(error: CommandExecError) -> WaitableProcess {
        WaitableProcess {
            process: OngoingProcess::Finished(FinishedProcess {
                status: ProcessExit::ExecError(error),
                stdout: vec![],
                stderr: vec![],
            }),
            associated_jobs: vec![],
        }
    }

    pub fn empty_status(status: ProcessExit) -> WaitableProcess {
        WaitableProcess {
            process: OngoingProcess::Finished(FinishedProcess {
                status,
                stdout: vec![],
                stderr: vec![],
            }),
            associated_jobs: vec![],
        }
    }

    pub fn pid(&self) -> Option<i32> {
        match &self.process {
            OngoingProcess::Concurrent(p) => Some(p.id() as i32),
            #[cfg(unix)]
            OngoingProcess::Reentrant { pid, .. } => Some(pid.as_raw()),
            OngoingProcess::Finished(_) => None,
        }
    }

    pub fn set_jobs(&mut self, jobs: Vec<WaitableProcess>) {
        self.associated_jobs = jobs;
    }

    #[cfg(unix)]
    pub fn hup(self) -> FinishedProcess {
        use nix::sys::signal::{kill, Signal};
        use nix::sys::wait::WaitStatus;
        use nix::unistd::Pid;

        // I shouldn't have to do this manually; there's something I can do with process groups
        // so the OS does this. I just don't know how.
        for job in self.associated_jobs {
            job.hup();
        }

        match self.process {
            OngoingProcess::Concurrent(process) => {
                let pid = Pid::from_raw(process.id() as i32);
                kill(pid, Signal::SIGHUP).unwrap();
                FinishedProcess {
                    status: ProcessExit::NixStatus(WaitStatus::Signaled(
                        pid,
                        Signal::SIGHUP,
                        false,
                    )),
                    stdout: vec![],
                    stderr: vec![],
                }
            }
            OngoingProcess::Reentrant { pid, .. } => {
                kill(pid, Signal::SIGHUP).unwrap();
                FinishedProcess {
                    status: ProcessExit::NixStatus(WaitStatus::Signaled(
                        pid,
                        Signal::SIGHUP,
                        false,
                    )),
                    stdout: vec![],
                    stderr: vec![],
                }
            }
            OngoingProcess::Finished(proc) => proc,
        }
    }

    #[cfg(not(unix))]
    pub fn hup(mut self) -> FinishedProcess {
        for job in self.associated_jobs {
            job.hup();
        }

        match self.process {
            OngoingProcess::Concurrent(process) => {
                let pid = process.id() as i32;
                let _ = process.kill();
                let status = process.wait().unwrap();
                FinishedProcess {
                    status: ProcessExit::StdStatus(status),
                    stdout: vec![],
                    stderr: vec![],
                }
            }
            OngoingProcess::Finished(proc) => proc,
        }
    }

    pub fn wait(self) -> FinishedProcess {
        match self.process {
            OngoingProcess::Concurrent(mut process) => {
                let status = process.wait().unwrap();
                let stdout = if let Some(mut stdout) = process.stdout {
                    let mut v = Vec::new();
                    let _ = stdout.read_to_end(&mut v); //TODO: should I handle an error here?
                    v
                } else {
                    Vec::new()
                };
                let stderr = if let Some(mut stderr) = process.stderr {
                    let mut v = Vec::new();
                    let _ = stderr.read_to_end(&mut v); //TODO: should I handle an error here?
                    v
                } else {
                    Vec::new()
                };

                for job in self.associated_jobs.into_iter() {
                    job.hup();
                }

                FinishedProcess {
                    status: ProcessExit::StdStatus(status),
                    stdout,
                    stderr,
                }
            }
            #[cfg(unix)]
            OngoingProcess::Reentrant {
                pid,
                stdin,
                stdout,
                stderr,
            } => {
                use std::os::unix::io::FromRawFd;

                if let Some(stdin) = stdin {
                    nix::unistd::close(stdin).unwrap();
                }
                let status = nix::sys::wait::waitpid(pid, None).unwrap();
                let stdout = if let Some(stdout) = stdout {
                    let mut stdout_file = unsafe { File::from_raw_fd(stdout) };
                    let mut v = Vec::new();
                    let _ = stdout_file.read_to_end(&mut v); //TODO: should I handle an error here?
                    v
                } else {
                    Vec::new()
                };
                let stderr = if let Some(stderr) = stderr {
                    let mut stderr_file = unsafe { File::from_raw_fd(stderr) };
                    let mut v = Vec::new();
                    let _ = stderr_file.read_to_end(&mut v); //TODO: should I handle an error here?
                    v
                } else {
                    Vec::new()
                };

                for job in self.associated_jobs.into_iter() {
                    job.hup();
                }

                FinishedProcess {
                    status: ProcessExit::NixStatus(status),
                    stdout,
                    stderr,
                }
            }
            OngoingProcess::Finished(proc) => {
                for job in self.associated_jobs.into_iter() {
                    job.hup();
                }
                proc
            }
        }
    }
}

impl From<Output> for FinishedProcess {
    fn from(o: Output) -> Self {
        FinishedProcess {
            status: ProcessExit::StdStatus(o.status),
            stdout: o.stdout,
            stderr: o.stderr,
        }
    }
}

impl From<Child> for WaitableProcess {
    fn from(process: Child) -> WaitableProcess {
        WaitableProcess::new(process)
    }
}

pub struct SpawnableProcess<'a> {
    ty: SpawnableProcessType<'a>,
    pub stdin: StdinRedirect,
    pub stdout: StdoutRedirect,
    pub stderr: StdoutRedirect,
}

pub enum SpawnableProcessType<'a> {
    StdProcess(std::process::Command),
    Builtin(BuiltinCommand),
    Group(&'a [ast::Command]),
    SubshellGroup(&'a [ast::Command]),
    Function(Arc<CompoundCommand>, Vec<String>),
    Finished(FinishedProcess),
    #[cfg(unix)]
    Reentrant {
        env: Vec<(String, String)>,
        cwd: Option<PathBuf>,
        recursive_context: BaseExecContext,
    },
}

impl SpawnableProcess<'_> {
    pub fn new(ty: SpawnableProcessType<'_>) -> SpawnableProcess<'_> {
        SpawnableProcess {
            ty,
            stdin: StdinRedirect::None,
            stdout: StdoutRedirect::None,
            stderr: StdoutRedirect::None,
        }
    }

    pub fn empty_success() -> Self {
        Self::new(SpawnableProcessType::Finished(FinishedProcess {
            status: ProcessExit::Bool(true),
            stdout: vec![],
            stderr: vec![],
        }))
    }

    pub fn empty_error(error: CommandExecError) -> Self {
        Self::new(SpawnableProcessType::Finished(FinishedProcess {
            status: ProcessExit::ExecError(error),
            stdout: vec![],
            stderr: vec![],
        }))
    }

    pub fn empty_status(status: ProcessExit) -> Self {
        Self::new(SpawnableProcessType::Finished(FinishedProcess {
            status,
            stdout: vec![],
            stderr: vec![],
        }))
    }

    pub fn process(process: std::process::Command) -> Self {
        Self::new(SpawnableProcessType::StdProcess(process))
    }

    pub fn builtin(builtin: BuiltinCommand) -> Self {
        Self::new(SpawnableProcessType::Builtin(builtin))
    }

    pub fn group(group: &[ast::Command]) -> SpawnableProcess<'_> {
        Self::new(SpawnableProcessType::Group(group))
    }

    pub fn subshell(group: &[ast::Command]) -> SpawnableProcess<'_> {
        Self::new(SpawnableProcessType::SubshellGroup(group))
    }

    pub fn function(function: Arc<CompoundCommand>, args: Vec<String>) -> Self {
        Self::new(SpawnableProcessType::Function(function, args))
    }

    pub fn reentrant(
        env: Vec<(String, String)>,
        cwd: Option<PathBuf>,
        recursive_context: BaseExecContext,
    ) -> Self {
        Self::new(SpawnableProcessType::Reentrant {
            env,
            cwd,
            recursive_context,
        })
    }

    pub fn spawn(self, context: SpawnContext) -> WaitableProcess {
        match self.ty {
            SpawnableProcessType::StdProcess(mut cmd) => {
                let resolved_stdin = match self.stdin {
                    StdinRedirect::None | StdinRedirect::Inherit => context.stdin(),
                    stdin => stdin,
                };
                let resolved_stdout = match self.stdout {
                    stdout @ (StdoutRedirect::None | StdoutRedirect::Inherit) => {
                        context.stdout().map(StdoutRedirect::Fd).unwrap_or(stdout)
                    }
                    stdout => stdout,
                };
                cmd.stdin(resolved_stdin)
                    .stdout(resolved_stdout)
                    .stderr(self.stderr);
                let process = cmd.spawn().unwrap();
                // if let StdinRedirect::Buffer(stdin_buffer) = self.stdin {
                //     if let Some(mut child_stdin) = process.stdin.as_ref() {
                //         use std::io::Write;
                //         // This approach can cause a deadlock if the following conditions are true:
                //         // - The child is using a piped stdout (the next process hasn't be spawned yet to consume it)
                //         // - The stdin buffer is larger than the pipe buffer
                //         // - The child writes more than one pipe buffer of data without reading enough of stdin
                //         //? Could run this on separate thread to mitigate this, if necessary.
                //         //? Could compare buffer size to libc::PIPE_BUF (4KiB), and spawn a thread if it is larger.
                //         let _ = child_stdin.write_all(&stdin_buffer);
                //         //TODO: Do I need to worry about an error here?
                //     }
                // }
                WaitableProcess::new(process)
            }
            SpawnableProcessType::Function(command, args) => match context {
                SpawnContext::PipelineEnd { context, .. } => {
                    context.exec_as_function(command, args)
                }
                SpawnContext::PipelineIntermediate { context, .. } => {
                    let mut context = context.clone();
                    context.exec_as_function(command, args)
                }
            },
            SpawnableProcessType::Group(commands) => match context {
                SpawnContext::PipelineEnd { context, .. } => context.exec_command_group(commands),
                SpawnContext::PipelineIntermediate { context, .. } => {
                    let mut context = context.clone();
                    context.exec_command_group(commands)
                }
            },
            SpawnableProcessType::SubshellGroup(commands) => {
                let mut context = match context {
                    SpawnContext::PipelineEnd { context, .. } => context.clone(),
                    SpawnContext::PipelineIntermediate { context, .. } => context.clone(),
                };
                context.exec_command_group(commands)
            }
            SpawnableProcessType::Builtin(builtin) => builtin.spawn(context),
            SpawnableProcessType::Finished(process) => WaitableProcess::finished(process),
            #[cfg(unix)]
            SpawnableProcessType::Reentrant {
                env,
                cwd,
                recursive_context,
            } => {
                let mut child_stdin = None;
                let stdin_fd = match self.stdin {
                    StdinRedirect::None => None,
                    StdinRedirect::Inherit => Some(0),
                    StdinRedirect::Dup(fd) | StdinRedirect::Fd(fd) => Some(fd),
                };
                let mut child_stdout = None;
                let stdout_fd = match self.stdout {
                    StdoutRedirect::None => None,
                    StdoutRedirect::Inherit => Some(1),
                    StdoutRedirect::Dup(fd) | StdoutRedirect::Fd(fd) => Some(fd),
                };
                let mut child_stderr = None;
                let stderr_fd = match self.stderr {
                    StdoutRedirect::None => None,
                    StdoutRedirect::Inherit => Some(2),
                    StdoutRedirect::Dup(fd) | StdoutRedirect::Fd(fd) => Some(fd),
                };

                use nix::unistd::ForkResult;
                match unsafe { nix::unistd::fork() } {
                    Ok(ForkResult::Parent { child }) => WaitableProcess::reentrant_nightmare(
                        child,
                        child_stdin,
                        child_stdout,
                        child_stderr,
                    ),
                    Ok(ForkResult::Child) => {
                        // If any of this setup fails, exit immediately with the OSERR exitcode.
                        fn bail() -> ! {
                            std::process::exit(exitcode::OSERR);
                        }
                        fn bail_a<T, U>(_: T) -> U {
                            bail()
                        }

                        // Set up standard I/O streams
                        // (nix::unistd::dup2(stdin_fd.unwrap(), 0).unwrap_or_else(bail_a) == -1)
                        //     .then(bail);
                        // (nix::unistd::dup2(stdout_fd.unwrap(), 1).unwrap_or_else(bail_a) == -1)
                        //     .then(bail);
                        // (nix::unistd::dup2(stderr_fd.unwrap(), 2).unwrap_or_else(bail_a) == -1)
                        //     .then(bail);

                        // Set up remainder of environment
                        if let Some(cwd) = cwd {
                            std::env::set_current_dir(&cwd).unwrap_or_else(bail_a);
                        }
                        for (k, v) in &env {
                            std::env::set_var(k, v);
                        }
                        // resume_unwind so as to not invoke the panic hook.
                        std::panic::resume_unwind(Box::new(recursive_context));
                    }
                    Err(e) => WaitableProcess::empty_error(CommandExecError::CommandFailed {
                        err: e.into(),
                    }),
                }
            }
        }
    }
}

pub enum BuiltinCommand {
    True,
    False,
    #[cfg(feature = "dev-panic")]
    Panic,
    Cd {
        /// Path should be `join`ed with `context.working_directory` already
        path: PathBuf,
    },
    Export {
        key: String,
        value: String,
    },
    Unset {
        key: String,
    },
    Source {
        /// Path should be `join`ed with `context.working_directory` already
        path: PathBuf,
        cwd: PathBuf,
    },
}

impl BuiltinCommand {
    fn spawn(self, context: SpawnContext) -> WaitableProcess {
        match self {
            BuiltinCommand::True => WaitableProcess::empty_success(),
            BuiltinCommand::False => WaitableProcess::empty_status(ProcessExit::Bool(false)),
            #[cfg(feature = "dev-panic")]
            BuiltinCommand::Panic => panic!("Explicit command panic"),
            BuiltinCommand::Cd { path } => {
                if let SpawnContext::PipelineEnd { context, .. } = context {
                    context.working_directory = path;
                }
                WaitableProcess::empty_success()
            }
            BuiltinCommand::Export { key, value } => {
                if let SpawnContext::PipelineEnd { context, .. } = context {
                    context.env.insert(key, value);
                }
                WaitableProcess::empty_success()
            }
            BuiltinCommand::Unset { key } => {
                if let SpawnContext::PipelineEnd { context, .. } = context {
                    context.vars.remove(&key);
                    context.env.remove(&key);
                }
                WaitableProcess::empty_success()
            }
            BuiltinCommand::Source { path, cwd } => {
                let source = RunscriptSource {
                    source: match std::fs::read_to_string(&path) {
                        Ok(source) => source,
                        Err(err) => {
                            return WaitableProcess::empty_error(CommandExecError::CommandFailed {
                                err,
                            });
                        }
                    },
                    path,
                    dir: cwd,
                };

                let shell_file = crate::parser::parse_shell(source).unwrap_or_else(|_| todo!());
                match context {
                    SpawnContext::PipelineEnd { context, .. } => {
                        context.exec_command_group(&shell_file)
                    }
                    SpawnContext::PipelineIntermediate { context, .. } => {
                        context.clone().exec_command_group(&shell_file)
                    }
                }
            }
        }
    }
}

pub enum SpawnContext<'a, 'ctx_a, 'ctx_b> {
    PipelineIntermediate {
        context: &'a ShellContext<'ctx_a, 'ctx_b>,
        input: StdinRedirect,
        #[cfg(unix)]
        output: Option<std::os::unix::io::RawFd>,
        #[cfg(windows)]
        output: Option<std::os::windows::prelude::RawHandle>,
    },
    PipelineEnd {
        context: &'a mut ShellContext<'ctx_a, 'ctx_b>,
        input: StdinRedirect,
    },
}

impl SpawnContext<'_, '_, '_> {
    fn stdin(&self) -> StdinRedirect {
        match self {
            SpawnContext::PipelineIntermediate { input, .. } => *input,
            SpawnContext::PipelineEnd { input, .. } => *input,
        }
    }

    #[cfg(unix)]
    fn stdout(&self) -> Option<std::os::unix::io::RawFd> {
        match self {
            SpawnContext::PipelineIntermediate { output, .. } => *output,
            SpawnContext::PipelineEnd { .. } => None,
        }
    }
}

#[derive(Clone, Copy)]
pub enum StdinRedirect {
    None,
    Inherit,
    #[cfg(unix)]
    Dup(std::os::unix::io::RawFd),
    #[cfg(unix)]
    Fd(std::os::unix::io::RawFd),
    #[cfg(windows)]
    Fd(std::os::windows::prelude::RawHandle),
}

impl From<StdinRedirect> for Stdio {
    fn from(redir: StdinRedirect) -> Self {
        match redir {
            StdinRedirect::None => Stdio::null(),
            StdinRedirect::Inherit => Stdio::inherit(),
            #[cfg(unix)]
            StdinRedirect::Dup(_) => todo!(),
            #[cfg(unix)]
            StdinRedirect::Fd(fd) => {
                use std::os::unix::io::FromRawFd;
                // SAFETY: Not currently enforced...
                unsafe { Stdio::from_raw_fd(fd) }
            }
        }
    }
}

#[cfg(unix)]
impl<T> From<T> for StdinRedirect
where
    T: std::os::unix::io::IntoRawFd,
{
    fn from(file: T) -> StdinRedirect {
        StdinRedirect::Fd(file.into_raw_fd())
    }
}

#[cfg(windows)]
impl<T> From<T> for StdinRedirect
where
    T: std::os::windows::io::IntoRawHandle,
{
    fn from(file: T) -> StdinRedirect {
        StdinRedirect::Fd(file.into_raw_handle())
    }
}

pub enum StdoutRedirect {
    None,
    Inherit,
    #[cfg(unix)]
    Dup(std::os::unix::io::RawFd),
    #[cfg(unix)]
    Fd(std::os::unix::io::RawFd),
    #[cfg(windows)]
    Fd(std::os::windows::prelude::RawHandle),
}

impl From<StdoutRedirect> for Stdio {
    fn from(redir: StdoutRedirect) -> Self {
        match redir {
            StdoutRedirect::None => Stdio::null(),
            StdoutRedirect::Inherit => Stdio::inherit(),
            #[cfg(unix)]
            StdoutRedirect::Dup(_) => todo!(),
            #[cfg(unix)]
            StdoutRedirect::Fd(fd) => {
                use std::os::unix::io::FromRawFd;
                // SAFETY: Not currently enforced...
                unsafe { Stdio::from_raw_fd(fd) }
            }
        }
    }
}

#[cfg(unix)]
impl<T> From<T> for StdoutRedirect
where
    T: std::os::unix::io::IntoRawFd,
{
    fn from(file: T) -> StdoutRedirect {
        StdoutRedirect::Fd(file.into_raw_fd())
    }
}

#[cfg(windows)]
impl<T> From<T> for StdoutRedirect
where
    T: std::os::windows::io::IntoRawHandle,
{
    fn from(file: T) -> StdoutRedirect {
        StdoutRedirect::Fd(file.into_raw_handle())
    }
}
