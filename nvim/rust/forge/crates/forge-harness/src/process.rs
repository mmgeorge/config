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
        let mut command = Command::new(resolved);
        command.args(args);
        command
    }
}

/// Resolve an installed executable without starting a shell or modifying its search path.
pub(crate) fn executable(program: &str) -> Option<PathBuf> {
    executable_at(
        program,
        &std::env::current_dir().ok()?,
        &std::env::var_os("PATH").unwrap_or_default(),
    )
}

/// Resolve a command against its launch directory and effective PATH without running it.
pub(crate) fn executable_at(
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

#[cfg(all(test, windows))]
mod tests {
    use super::*;

    #[test]
    fn npm_shell_script_does_not_shadow_windows_launcher() {
        let directory = tempfile::tempdir().unwrap();
        let installation = directory.path().join("npm bin");
        std::fs::create_dir(&installation).unwrap();
        std::fs::write(installation.join("codex"), "#!/bin/sh\n").unwrap();
        std::fs::write(installation.join("codex.cmd"), "@echo off\r\n").unwrap();

        assert_eq!(
            executable_at("codex", directory.path(), installation.as_os_str()),
            Some(installation.join("codex.cmd"))
        );
        assert_eq!(
            executable_at("codex", &installation, std::ffi::OsStr::new("")),
            Some(installation.join("codex.cmd"))
        );
        std::fs::remove_file(installation.join("codex.cmd")).unwrap();
        assert_eq!(
            executable_at("codex", directory.path(), installation.as_os_str()),
            None
        );
    }

    #[test]
    fn explicit_windows_launcher_does_not_resolve_a_different_suffix() {
        let directory = tempfile::tempdir().unwrap();
        let launcher = directory.path().join("codex.CMD");
        std::fs::write(&launcher, "@echo off\r\n").unwrap();
        std::fs::write(directory.path().join("codex.exe.cmd"), "@echo off\r\n").unwrap();

        assert_eq!(
            executable_at(
                launcher.to_str().unwrap(),
                directory.path(),
                std::ffi::OsStr::new("")
            ),
            Some(launcher)
        );
        assert_eq!(
            executable_at("codex.exe", directory.path(), std::ffi::OsStr::new("")),
            None
        );
    }

    #[tokio::test]
    async fn resolved_batch_launcher_runs_with_arguments() {
        let directory = tempfile::tempdir().unwrap();
        let installation = directory.path().join("npm bin");
        std::fs::create_dir(&installation).unwrap();
        std::fs::write(installation.join("codex"), "#!/bin/sh\n").unwrap();
        std::fs::write(
            installation.join("codex.cmd"),
            "@echo off\r\necho %~1 %~2\r\n",
        )
        .unwrap();
        let launcher = executable_at("codex", directory.path(), installation.as_os_str()).unwrap();
        let mut child = command(
            launcher.to_str().unwrap(),
            &["app-server".into(), "two words".into()],
        );
        let output = tokio::time::timeout(std::time::Duration::from_secs(5), child.output())
            .await
            .unwrap()
            .unwrap();

        assert!(output.status.success(), "{:?}", output);
        assert_eq!(
            String::from_utf8(output.stdout).unwrap().trim(),
            "app-server two words"
        );
    }
}
