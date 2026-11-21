use tokio::process::Command;

use std::path::{Path, PathBuf};

/// Choose whether an unqualified executable may be found outside PATH.
enum ExecutableSearch {
    PlatformDefault,
    PathOnly,
}

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

/// Resolve a bare executable through absolute PATH directories.
pub fn executable_on_path(program: &str) -> Option<PathBuf> {
    resolve_executable(
        program,
        &std::env::current_dir().ok()?,
        &std::env::var_os("PATH")?,
        ExecutableSearch::PathOnly,
    )
}

/// Resolve a command against its launch directory and effective PATH without running it.
pub fn executable_at(
    program: &str,
    working_directory: &Path,
    search_path: &std::ffi::OsStr,
) -> Option<PathBuf> {
    resolve_executable(
        program,
        working_directory,
        search_path,
        ExecutableSearch::PlatformDefault,
    )
}

/// Apply the search policy before checking platform extensions and file permissions.
fn resolve_executable(
    program: &str,
    working_directory: &Path,
    search_path: &std::ffi::OsStr,
    search: ExecutableSearch,
) -> Option<PathBuf> {
    if program.is_empty() {
        return None;
    }
    let requested = Path::new(program);
    let qualified = requested.is_absolute() || requested.components().count() > 1;
    let platform_default = matches!(search, ExecutableSearch::PlatformDefault);
    if qualified && !platform_default {
        return None;
    }
    let launch_directory = (qualified || (cfg!(windows) && platform_default))
        .then(|| working_directory.to_path_buf());
    // Relative PATH entries can resolve inside the checkout that started this process.
    let path_directory = std::env::split_paths(search_path)
        .filter(|directory| !qualified && (platform_default || directory.is_absolute()))
        .map(|directory| working_directory.join(directory));
    for directory in launch_directory.into_iter().chain(path_directory) {
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
    use super::{ExecutableSearch, executable_at, job_attribute_refused, resolve_executable};
    use std::path::Path;

    /// Keep checkout executables outside implicit search even with relative PATH entries.
    #[test]
    fn path_only_lookup_skips_windows_checkout_relative_entries() -> std::io::Result<()> {
        let fixture = tempfile::tempdir()?;
        let workspace = fixture.path().join("workspace");
        let workspace_bin = workspace.join("bin");
        let installation = fixture.path().join("installation");
        std::fs::create_dir_all(&workspace_bin)?;
        std::fs::create_dir(&installation)?;
        let local_executable = workspace.join("agy.exe");
        let installed_executable = installation.join("agy.exe");
        std::fs::write(&local_executable, [])?;
        std::fs::write(workspace_bin.join("agy.exe"), [])?;
        std::fs::write(&installed_executable, [])?;
        let relative_entries = [Path::new(""), Path::new("."), Path::new("bin")];
        let checkout_path = std::env::join_paths(relative_entries).map_err(std::io::Error::other)?;
        let search_path =
            std::env::join_paths(relative_entries.into_iter().chain([installation.as_path()]))
                .map_err(std::io::Error::other)?;

        assert_eq!(
            executable_at("agy", &workspace, &search_path),
            Some(local_executable)
        );
        assert_eq!(
            resolve_executable("agy", &workspace, &search_path, ExecutableSearch::PathOnly),
            Some(installed_executable)
        );
        assert_eq!(
            resolve_executable(
                "agy",
                &workspace,
                &checkout_path,
                ExecutableSearch::PathOnly
            ),
            None
        );
        Ok(())
    }

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
