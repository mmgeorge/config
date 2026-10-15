use tokio::process::Command;

use std::path::{Path, PathBuf};

/// Resolve platform launch shims without changing process ownership.
pub(crate) fn command(program: &str, args: &[String]) -> Command {
    #[cfg(not(windows))]
    {
        let mut command = Command::new(program);
        command.args(args);
        command
    }

    #[cfg(windows)]
    {
        let resolved = executable(program).unwrap_or_else(|| PathBuf::from(program));
        let extension = resolved
            .extension()
            .and_then(|value| value.to_str())
            .unwrap_or_default();

        if extension.eq_ignore_ascii_case("cmd") || extension.eq_ignore_ascii_case("bat") {
            let mut command = Command::new("cmd.exe");
            command
                .arg("/d")
                .arg("/s")
                .arg("/c")
                .arg(resolved)
                .args(args);
            return command;
        }

        let mut command = Command::new(resolved);
        command.args(args);
        command
    }
}



/// Resolve an installed executable without starting a shell or modifying its search path.
pub(crate) fn executable(program: &str) -> Option<PathBuf> {
    executable_at(program, &std::env::current_dir().ok()?, &std::env::var_os("PATH").unwrap_or_default())
}

/// Resolve a command against its launch directory and effective PATH without running it.
pub(crate) fn executable_at(program: &str, working_directory: &Path, search_path: &std::ffi::OsStr) -> Option<PathBuf> {
    if program.is_empty() { return None; }
    let requested = Path::new(program);
    let qualified = requested.is_absolute() || requested.components().count() > 1;
    let directory: Vec<PathBuf> = if qualified {
        vec![working_directory.to_path_buf()]
    } else {
        let directory = std::env::split_paths(search_path).map(|directory| working_directory.join(directory));
        #[cfg(windows)]
        let directory = std::iter::once(working_directory.to_path_buf()).chain(directory);
        directory.collect()
    };
    for directory in directory {
        #[cfg(windows)]
        let extensions = ["", "exe", "com", "cmd", "bat"];
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
                    let Ok(metadata) = candidate.metadata() else { continue; };
                    if metadata.permissions().mode() & 0o111 == 0 { continue; }
                }
                return Some(candidate);
            }
        }
    }
    None
}


pub(crate) fn spawn_owned(
    command: Command,
) -> std::io::Result<Box<dyn process_wrap::tokio::ChildWrapper>> {
    use process_wrap::tokio::{CommandWrap, KillOnDrop};
    let mut command = CommandWrap::from(command);
    command.wrap(KillOnDrop);
    #[cfg(windows)]
    {
        use process_wrap::tokio::{CreationFlags, JobObject};
        command
            .wrap(CreationFlags(
                windows::Win32::System::Threading::CREATE_NO_WINDOW,
            ))
            .wrap(JobObject);
    }
    #[cfg(unix)]
    command.wrap(process_wrap::tokio::ProcessGroup::leader());
    command.spawn()
}
