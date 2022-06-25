use std::fs::File;
use std::io::Read;
use std::path::PathBuf;
use std::process::{Child, ExitStatus, Output, Stdio};
use std::str::Utf8Error;
use std::sync::Arc;

use glob::PatternError;

use crate::exec::{BaseExecContext, ShellContext};
use crate::out::{self, Printable, PrintableEnvRemap};
use crate::parser::ast::CompoundCommand;
use crate::parser::RunscriptSource;
use crate::ptr::Ref;

#[derive(Debug)]
pub enum CommandExecError {
    InvalidGlob { glob: String, err: PatternError },
    NoGlobMatches { glob: String },
    CommandFailed { err: std::io::Error },
    UnhandledOsString { err: Utf8Error },
    BadRedirect { err: std::io::Error },
    UnsupportedRedirect,
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
    //TODO: Get rid of these?
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
    redir: RedirectConfig,
    printable: Option<Printable>,
}

pub enum SpawnableProcessType<'a> {
    StdProcess(std::process::Command),
    Builtin(BuiltinCommand),
    Compound(&'a CompoundCommand),
    FnDef(String, Arc<CompoundCommand>),
    Function(Arc<CompoundCommand>, Vec<String>),
    Remaps(Vec<(String, String)>),
    Finished(FinishedProcess),
    #[cfg(unix)]
    Reentrant {
        env: Vec<(String, String)>,
        cwd: Option<PathBuf>,
        recursive_context: BaseExecContext,
    },
}

impl SpawnableProcess<'_> {
    pub fn new(
        ty: SpawnableProcessType<'_>,
        redir: RedirectConfig,
        printable: Option<Printable>,
    ) -> SpawnableProcess<'_> {
        SpawnableProcess {
            ty,
            redir,
            printable,
        }
    }

    pub fn empty_success() -> Self {
        Self::new(
            SpawnableProcessType::Finished(FinishedProcess {
                status: ProcessExit::Bool(true),
                stdout: vec![],
                stderr: vec![],
            }),
            RedirectConfig::default_bg(),
            None,
        )
    }

    pub fn empty_error(error: CommandExecError) -> Self {
        Self::new(
            SpawnableProcessType::Finished(FinishedProcess {
                status: ProcessExit::ExecError(error),
                stdout: vec![],
                stderr: vec![],
            }),
            RedirectConfig::default_bg(),
            None,
        )
    }

    pub fn empty_status(status: ProcessExit) -> Self {
        Self::new(
            SpawnableProcessType::Finished(FinishedProcess {
                status,
                stdout: vec![],
                stderr: vec![],
            }),
            RedirectConfig::default_bg(),
            None,
        )
    }

    pub fn process(
        process: std::process::Command,
        redir: RedirectConfig,
        printable: Printable,
    ) -> Self {
        Self::new(
            SpawnableProcessType::StdProcess(process),
            redir,
            Some(printable),
        )
    }

    pub fn builtin(builtin: BuiltinCommand, redir: RedirectConfig, printable: Printable) -> Self {
        Self::new(
            SpawnableProcessType::Builtin(builtin),
            redir,
            Some(printable),
        )
    }

    pub fn compound(compound: &CompoundCommand, redir: RedirectConfig) -> SpawnableProcess<'_> {
        Self::new(SpawnableProcessType::Compound(compound), redir, None)
    }

    pub fn function(
        function: Arc<CompoundCommand>,
        args: Vec<String>,
        redir: RedirectConfig,
        printable: Printable,
    ) -> Self {
        Self::new(
            SpawnableProcessType::Function(function, args),
            redir,
            Some(printable),
        )
    }

    pub fn remaps(env: Vec<(String, String)>, printables: Vec<PrintableEnvRemap>) -> Self {
        Self::new(
            SpawnableProcessType::Remaps(env),
            RedirectConfig::default_bg(),
            Some(Printable {
                command: None,
                env_remaps: printables,
                redirects: Vec::new(),
            }),
        )
    }

    pub fn fn_def(name: String, body: Arc<CompoundCommand>) -> Self {
        Self::new(
            SpawnableProcessType::FnDef(name, body),
            RedirectConfig::default_bg(),
            None,
        )
    }

    pub fn reentrant(
        env: Vec<(String, String)>,
        cwd: Option<PathBuf>,
        recursive_context: BaseExecContext,
        redir: RedirectConfig,
        printable: Printable,
    ) -> Self {
        Self::new(
            SpawnableProcessType::Reentrant {
                env,
                cwd,
                recursive_context,
            },
            redir,
            Some(printable),
        )
    }

    pub fn is_printable(&self) -> bool {
        self.printable.is_some()
    }

    pub fn print_to(
        &self,
        output_stream: &mut termcolor::StandardStreamLock,
    ) -> std::io::Result<()> {
        if let Some(printable) = &self.printable {
            out::print_command(output_stream, printable)
        } else {
            Ok(())
        }
    }

    pub fn spawn(mut self, mut context: SpawnContext) -> WaitableProcess {
        self.redir.apply_context(&context);
        match self.ty {
            SpawnableProcessType::StdProcess(mut cmd) => {
                cmd.stdin(self.redir.stdin)
                    .stdout(self.redir.stdout)
                    .stderr(self.redir.stderr);
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
            SpawnableProcessType::FnDef(name, body) => {
                Ref::into_unique(context.shell_context())
                    .functions
                    .insert(name, body);
                WaitableProcess::empty_success()
            }
            SpawnableProcessType::Function(command, args) => {
                Ref::into_unique(context.shell_context())
                    .exec_as_function(command, args, self.redir)
            }
            SpawnableProcessType::Compound(command) => ShellContext::exec_compound_command_unique(
                Ref::into_unique(context.shell_context()),
                command,
                self.redir,
            ),
            SpawnableProcessType::Builtin(builtin) => builtin.spawn(context),
            SpawnableProcessType::Finished(process) => WaitableProcess::finished(process),
            #[cfg(unix)]
            SpawnableProcessType::Reentrant {
                env,
                cwd,
                recursive_context,
            } => {
                use nix::unistd::ForkResult;
                match unsafe { nix::unistd::fork() } {
                    Ok(ForkResult::Parent { child }) => {
                        WaitableProcess::reentrant_nightmare(child, None, None, None)
                    }
                    Ok(ForkResult::Child) => {
                        // If any of this setup fails, exit immediately with the OSERR exitcode.
                        fn bail() -> ! {
                            std::process::exit(exitcode::OSERR);
                        }
                        fn bail_a<T, U>(_: T) -> U {
                            bail()
                        }

                        cfg_if::cfg_if! {
                            if #[cfg(target_os = "fuchsia")] {
                                // fuchsia doesn't have /dev/null
                            } else if #[cfg(target_os = "redox")] {
                                const DEV_NULL: &str = "null:\0";
                            } else if #[cfg(target_os = "vxworks")] {
                                const DEV_NULL: &str = "/null\0";
                            } else {
                                const DEV_NULL: &str = "/dev/null\0";
                            }
                        }

                        // Set up standard I/O streams
                        match self.redir.stdin {
                            StdinRedirect::Null => {
                                (nix::unistd::dup2(
                                    nix::fcntl::open(
                                        DEV_NULL,
                                        nix::fcntl::OFlag::O_WRONLY | nix::fcntl::OFlag::O_CLOEXEC,
                                        nix::sys::stat::Mode::from_bits_truncate(0o666),
                                    )
                                    .unwrap_or_else(bail_a),
                                    0,
                                )
                                .unwrap_or_else(bail_a)
                                    == -1)
                                    .then(bail);
                            }
                            StdinRedirect::Inherit => {} // Nothing to do
                            StdinRedirect::Fd(fd) => {
                                (nix::unistd::dup2(fd, 0).unwrap_or_else(bail_a) == -1).then(bail);
                            }
                            StdinRedirect::Dup(_) => std::process::exit(exitcode::UNAVAILABLE),
                        }
                        match self.redir.stdout {
                            StdoutRedirect::Null => {
                                (nix::unistd::dup2(
                                    nix::fcntl::open(
                                        DEV_NULL,
                                        nix::fcntl::OFlag::O_WRONLY | nix::fcntl::OFlag::O_CLOEXEC,
                                        nix::sys::stat::Mode::from_bits_truncate(0o666),
                                    )
                                    .unwrap_or_else(bail_a),
                                    1,
                                )
                                .unwrap_or_else(bail_a)
                                    == -1)
                                    .then(bail);
                            }
                            StdoutRedirect::Inherit => {} // Nothing to do
                            StdoutRedirect::Fd(fd) => {
                                (nix::unistd::dup2(fd, 1).unwrap_or_else(bail_a) == -1).then(bail);
                            }
                            StdoutRedirect::Dup(_) => std::process::exit(exitcode::UNAVAILABLE),
                        }
                        match self.redir.stderr {
                            StdoutRedirect::Null => {
                                (nix::unistd::dup2(
                                    nix::fcntl::open(
                                        DEV_NULL,
                                        nix::fcntl::OFlag::O_WRONLY | nix::fcntl::OFlag::O_CLOEXEC,
                                        nix::sys::stat::Mode::from_bits_truncate(0o666),
                                    )
                                    .unwrap_or_else(bail_a),
                                    2,
                                )
                                .unwrap_or_else(bail_a)
                                    == -1)
                                    .then(bail);
                            }
                            StdoutRedirect::Inherit => {} // Nothing to do
                            StdoutRedirect::Fd(fd) => {
                                (nix::unistd::dup2(fd, 2).unwrap_or_else(bail_a) == -1).then(bail);
                            }
                            StdoutRedirect::Dup(_) => std::process::exit(exitcode::UNAVAILABLE),
                        }

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
            SpawnableProcessType::Remaps(env) => {
                let mut context = Ref::into_unique(context.shell_context());
                context.vars.extend(env.into_iter());
                WaitableProcess::empty_success()
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
    fn spawn(self, mut context: SpawnContext) -> WaitableProcess {
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
                Ref::into_unique(context.shell_context()).exec_command_group(&shell_file)
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

impl<'a, 'ctx_a, 'ctx_b> SpawnContext<'a, 'ctx_a, 'ctx_b> {
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

    fn shell_context<'b>(&'b mut self) -> Ref<'b, ShellContext<'ctx_a, 'ctx_b>> {
        match self {
            SpawnContext::PipelineIntermediate { context, .. } => (*context).into(),
            SpawnContext::PipelineEnd { context, .. } => Ref::Unique(*context),
        }
    }
}

#[derive(Clone, Copy)]
pub struct RedirectConfig {
    pub stdin: StdinRedirect,
    pub stdout: StdoutRedirect,
    pub stderr: StdoutRedirect,
}

impl RedirectConfig {
    pub fn default_fg() -> RedirectConfig {
        RedirectConfig {
            stdin: StdinRedirect::Inherit,
            stdout: StdoutRedirect::Inherit,
            stderr: StdoutRedirect::Inherit,
        }
    }

    pub fn default_bg() -> RedirectConfig {
        RedirectConfig {
            stdin: StdinRedirect::Null,
            stdout: StdoutRedirect::Null,
            stderr: StdoutRedirect::Null,
        }
    }

    fn apply_context(&mut self, context: &SpawnContext) {
        self.stdin = match self.stdin {
            StdinRedirect::Null | StdinRedirect::Inherit => context.stdin(),
            stdin => stdin,
        };
        self.stdout = match self.stdout {
            stdout @ (StdoutRedirect::Null | StdoutRedirect::Inherit) => {
                context.stdout().map(StdoutRedirect::Fd).unwrap_or(stdout)
            }
            stdout => stdout,
        };
    }
}

#[derive(Clone, Copy)]
pub enum StdinRedirect {
    Null,
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
            StdinRedirect::Null => Stdio::null(),
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

#[derive(Clone, Copy)]
pub enum StdoutRedirect {
    Null,
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
            StdoutRedirect::Null => Stdio::null(),
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
