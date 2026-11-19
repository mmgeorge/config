use tokio::process::Command;

use std::path::{Path, PathBuf};

/// Admit native process creation while retaining the caller's repository ownership.
pub trait ProcessOwner: Send + Sync {
    /// Spawn only while the original operation still permits execution.
    fn spawn(
        &self,
        command: Command,
    ) -> std::io::Result<Box<dyn process_wrap::tokio::ChildWrapper>>;
}

/// Resolve platform launch shims without changing process ownership.
pub fn command(program: &str, args: &[String]) -> Command {
    #[cfg(not(windows))]
    {
        let mut command = Command::new(program);
        command.args(args);
        command
    }

    #[cfg(windows)]
    {
        let resolved = executable(program).unwrap_or_else(|| PathBuf::from(program));
        let mut command = Command::new(resolved);
        command.args(args);
        command
    }
}

/// Resolve an installed executable without starting a shell or modifying its search path.
pub fn executable(program: &str) -> Option<PathBuf> {
    executable_at(
        program,
        &std::env::current_dir().ok()?,
        &std::env::var_os("PATH").unwrap_or_default(),
    )
}

/// Resolve a command against its launch directory and effective PATH without running it.
pub fn executable_at(
    program: &str,
    working_directory: &Path,
    search_path: &std::ffi::OsStr,
) -> Option<PathBuf> {
    if program.is_empty() {
        return None;
    }
    let requested = Path::new(program);
    let qualified = requested.is_absolute() || requested.components().count() > 1;
    let directory: Vec<PathBuf> = if qualified {
        vec![working_directory.to_path_buf()]
    } else {
        let directory =
            std::env::split_paths(search_path).map(|directory| working_directory.join(directory));
        #[cfg(windows)]
        let directory = std::iter::once(working_directory.to_path_buf()).chain(directory);
        directory.collect()
    };
    for directory in directory {
        #[cfg(windows)]
        let extensions: &[&str] = if requested
            .extension()
            .and_then(|extension| extension.to_str())
            .is_some_and(|extension| {
                ["exe", "com", "cmd", "bat"]
                    .iter()
                    .any(|supported| extension.eq_ignore_ascii_case(supported))
            }) {
            &[""]
        } else {
            &["exe", "com", "cmd", "bat"]
        };
        #[cfg(not(windows))]
        let extensions = [""];
        for extension in extensions {
            let candidate = if extension.is_empty() {
                directory.join(requested)
            } else {
                directory.join(format!("{program}.{extension}"))
            };
            if candidate.is_file() {
                #[cfg(unix)]
                {
                    use std::os::unix::fs::PermissionsExt;
                    let Ok(metadata) = candidate.metadata() else {
                        continue;
                    };
                    if metadata.permissions().mode() & 0o111 == 0 {
                        continue;
                    }
                }
                return Some(candidate);
            }
        }
    }
    None
}

/// Spawn a command with descendant containment retained through collection.
pub fn spawn_owned(
    command: Command,
) -> std::io::Result<Box<dyn process_wrap::tokio::ChildWrapper>> {
    #[cfg(windows)]
    {
        spawn_contained(command, None)
    }
    #[cfg(not(windows))]
    {
        use process_wrap::tokio::{CommandWrap, KillOnDrop};
        let mut command = CommandWrap::from(command);
        command.wrap(KillOnDrop);
        #[cfg(unix)]
        command.wrap(process_wrap::tokio::ProcessGroup::leader());
        command.spawn()
    }
}

/// Launch into a kill-on-close job, nested in `parent` when the caller registered one.
///
/// The job is attached at process creation. Windows refuses that creation attribute with
/// `ERROR_ACCESS_DENIED` for a packaged app's execution alias, such as the Store build of
/// `pwsh`, while another launch of that app is in flight, so a refused launch is repeated
/// suspended and assigned to `parent` and then to its own job before its first instruction runs,
/// which gives the same containment.
#[cfg(windows)]
pub(crate) fn spawn_contained(
    command: Command,
    parent: Option<std::os::windows::io::BorrowedHandle<'_>>,
) -> std::io::Result<Box<dyn process_wrap::tokio::ChildWrapper>> {
    use process_wrap::tokio::{CommandWrap, CreationFlags, JobObject, KillOnDrop};
    use std::os::windows::io::AsRawHandle;
    use windows::Win32::System::Threading::CREATE_NO_WINDOW;
    let mut command = command;
    command.creation_flags(CREATE_NO_WINDOW.0);
    let launched = match parent {
        Some(parent) => JobObject::spawn_in(&mut command, parent),
        None => JobObject::spawn(&mut command),
    };
    match launched {
        Err(error) if job_attribute_refused(&error) => {
            let mut suspended = CommandWrap::from(command);
            suspended.wrap(CreationFlags(CREATE_NO_WINDOW));
            if let Some(parent) = parent {
                suspended.wrap(ParentJob(parent.as_raw_handle() as isize));
            }
            suspended.wrap(JobObject).wrap(KillOnDrop);
            suspended.spawn()
        }
        launched => launched,
    }
}

/// Report whether Windows refused the job-list creation attribute rather than the launch.
#[cfg(windows)]
fn job_attribute_refused(error: &std::io::Error) -> bool {
    error.raw_os_error() == i32::try_from(windows::Win32::Foundation::ERROR_ACCESS_DENIED.0).ok()
}

/// Assign a suspended child to a registered parent job before `JobObject` nests its own job.
///
/// Holds the raw handle value because wrappers must be `'static`; the caller's borrowed handle
/// outlives the spawn call that uses it.
#[cfg(windows)]
#[derive(Debug)]
struct ParentJob(isize);

#[cfg(windows)]
impl process_wrap::tokio::CommandWrapper for ParentJob {
    fn post_spawn(
        &mut self,
        _command: &mut Command,
        child: &mut tokio::process::Child,
        _core: &process_wrap::tokio::CommandWrap,
    ) -> std::io::Result<()> {
        use windows::Win32::{Foundation::HANDLE, System::JobObjects::AssignProcessToJobObject};
        let process = child
            .raw_handle()
            .ok_or_else(|| std::io::Error::other("suspended child has no process handle"))?;
        // SAFETY: the parent job handle is live for the spawn call, and the child is suspended
        // until `JobObject` resumes it after both assignments.
        unsafe { AssignProcessToJobObject(HANDLE(self.0 as _), HANDLE(process)) }
            .map_err(std::io::Error::other)
    }
}

#[cfg(all(test, windows))]
mod tests {
    use super::job_attribute_refused;

    /// Only an access-denied refusal repeats the launch suspended; other failures stand.
    #[test]
    fn only_access_denied_falls_back_to_a_suspended_launch() {
        assert!(job_attribute_refused(&std::io::Error::from_raw_os_error(5)));
        assert!(!job_attribute_refused(&std::io::Error::from_raw_os_error(
            2
        )));
        assert!(!job_attribute_refused(&std::io::Error::other(
            "access denied"
        )));
    }
}
