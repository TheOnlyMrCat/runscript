use std::fs::File;
use std::io::Read;
use std::process::{Child, ExitStatus, Output, Stdio};
use std::str::Utf8Error;

use glob::PatternError;

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

#[derive(Debug)]
pub struct FinishedProcess {
    pub status: ProcessExit,
    pub stdout: Vec<u8>,
    pub stderr: Vec<u8>,
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

    pub fn pid(&self) -> i32 {
        match &self.process {
            OngoingProcess::Concurrent(p) => p.id() as i32,
            #[cfg(unix)]
            OngoingProcess::Reentrant { pid, .. } => pid.as_raw(),
            OngoingProcess::Finished(_) => 0,
        }
    }

    pub fn set_jobs(&mut self, jobs: Vec<WaitableProcess>) {
        self.associated_jobs = jobs;
    }

    pub fn pipe_out(&mut self) -> PipeInput {
        match self.process {
            OngoingProcess::Concurrent(ref mut p) => match p.stdout.take() {
                Some(stdout) => PipeInput::Pipe(stdout.into()),
                None => PipeInput::None,
            },
            #[cfg(unix)]
            OngoingProcess::Reentrant { stdout, .. } => match stdout {
                Some(stdout) => PipeInput::Pipe((stdout).into()),
                None => PipeInput::None,
            },
            OngoingProcess::Finished(ref p) => PipeInput::Buffer(p.stdout.clone()),
        }
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

impl From<Child> for WaitableProcess {
    fn from(process: Child) -> WaitableProcess {
        WaitableProcess::new(process)
    }
}

pub enum StdioRepr {
    Inherit,
    Null,
    MakePipe,
    #[cfg(unix)]
    Fd(std::os::unix::io::RawFd),
    #[cfg(windows)]
    Fd(std::os::windows::prelude::RawHandle),
}

impl From<StdioRepr> for Stdio {
    fn from(repr: StdioRepr) -> Stdio {
        match repr {
            StdioRepr::Inherit => Stdio::inherit(),
            StdioRepr::Null => Stdio::null(),
            StdioRepr::MakePipe => Stdio::piped(),
            #[cfg(unix)]
            StdioRepr::Fd(fd) => {
                use std::os::unix::io::FromRawFd;
                unsafe {
                    // SAFETY: Not currently enforced. Replace with OwnedFd and OwnedHandle when #87074 is stable
                    Stdio::from_raw_fd(fd)
                }
            }
            #[cfg(windows)]
            StdioRepr::Fd(handle) => {
                use std::os::windows::io::FromRawHandle;
                unsafe { Stdio::from_raw_handle(handle) }
            }
        }
    }
}

#[cfg(unix)]
impl<T> From<T> for StdioRepr
where
    T: std::os::unix::io::IntoRawFd,
{
    fn from(file: T) -> StdioRepr {
        StdioRepr::Fd(file.into_raw_fd())
    }
}

#[cfg(windows)]
impl<T> From<T> for StdioRepr
where
    T: std::os::windows::io::IntoRawHandle,
{
    fn from(file: T) -> StdioRepr {
        StdioRepr::Fd(file.into_raw_handle())
    }
}

pub struct Pipe {
    pub stdin: PipeInput,
    pub stdout: bool,
}

impl Pipe {
    pub fn no_pipe() -> Pipe {
        Pipe {
            stdin: PipeInput::Inherit,
            stdout: false,
        }
    }

    pub fn null_pipe_out() -> Pipe {
        Pipe {
            stdin: PipeInput::None,
            stdout: true,
        }
    }

    pub fn inherit_pipe_out() -> Pipe {
        Pipe {
            stdin: PipeInput::Inherit,
            stdout: true,
        }
    }

    pub fn pipe_in(stdin: impl Into<PipeInput>) -> Pipe {
        Pipe {
            stdin: stdin.into(),
            stdout: false,
        }
    }

    pub fn pipe_in_out(stdin: impl Into<PipeInput>) -> Pipe {
        Pipe {
            stdin: stdin.into(),
            stdout: true,
        }
    }
}

pub enum PipeInput {
    None,
    Inherit,
    Pipe(StdioRepr),
    Buffer(Vec<u8>),
}

impl From<StdioRepr> for PipeInput {
    fn from(stdin: StdioRepr) -> PipeInput {
        PipeInput::Pipe(stdin)
    }
}

impl From<Vec<u8>> for PipeInput {
    fn from(stdin: Vec<u8>) -> PipeInput {
        PipeInput::Buffer(stdin)
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
