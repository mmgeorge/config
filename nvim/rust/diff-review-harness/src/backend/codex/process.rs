use tokio::process::Command;

#[cfg(windows)]
use std::path::{Path, PathBuf};

/// Build a Codex command while resolving Windows command shims.
pub(crate) fn command(program: &str, args: &[String]) -> Command {
    #[cfg(not(windows))]
    {
        let mut command = Command::new(program);
        command.args(args);
        command
    }

    #[cfg(windows)]
    {
        let resolved = resolve_windows_program(program).unwrap_or_else(|| PathBuf::from(program));
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

#[cfg(windows)]
fn resolve_windows_program(program: &str) -> Option<PathBuf> {
    let requested = Path::new(program);
    if requested.is_absolute() || requested.components().count() > 1 {
        return requested.exists().then(|| requested.to_path_buf());
    }

    let search_path = std::env::var_os("PATH")?;
    for directory in std::env::split_paths(&search_path) {
        for extension in ["exe", "com", "cmd", "bat", ""] {
            let candidate = if extension.is_empty() {
                directory.join(program)
            } else {
                directory.join(format!("{program}.{extension}"))
            };
            if candidate.is_file() {
                return Some(candidate);
            }
        }
    }
    None
}
