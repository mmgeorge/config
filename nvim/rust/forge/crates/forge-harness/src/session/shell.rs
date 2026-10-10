use anyhow::{Result, ensure};
use serde::{Deserialize, Serialize};
use std::path::PathBuf;

#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
/// Selects the interpreter for accepted local checks independently of provider tools.
pub enum CheckShell {
    #[default]
    System,
    Nushell,
}

impl CheckShell {
    /// Resolve the selected interpreter without substituting another shell.
    pub fn executable(self) -> Result<PathBuf> {
        let program = match self {
            Self::Nushell => "nu".to_owned(),
            Self::System => {
                #[cfg(windows)]
                {
                    "pwsh".to_owned()
                }
                #[cfg(not(windows))]
                {
                    std::env::var("SHELL").unwrap_or_else(|_| "/bin/zsh".into())
                }
            }
        };
        let path = PathBuf::from(&program);
        if path.is_absolute() {
            ensure!(path.is_file(), "selected shell is unavailable: {program}");
            return Ok(path);
        }
        for directory in std::env::split_paths(&std::env::var_os("PATH").unwrap_or_default()) {
            for suffix in if cfg!(windows) {
                &[".exe", ""][..]
            } else {
                &[""][..]
            } {
                let candidate = directory.join(format!("{program}{suffix}"));
                if candidate.is_file() {
                    return Ok(candidate);
                }
            }
        }
        anyhow::bail!("selected shell {program} is unavailable. Change Shell in /config")
    }

    /// Construct a noninteractive invocation that propagates native command failures.
    pub fn command(
        self,
        executable: &std::path::Path,
        source: &str,
    ) -> Result<tokio::process::Command> {
        ensure!(!source.trim().is_empty(), "check command is empty");
        let mut command = tokio::process::Command::new(executable);
        match self {
            Self::Nushell => {
                command.args(["--no-config-file", "--no-history", "-c", source]);
            }
            Self::System => {
                #[cfg(windows)]
                {
                    command.args(["-NoLogo", "-NoProfile", "-NonInteractive", "-Command"]);
                    command.arg(format!("$ErrorActionPreference='Stop'; & {{ {source} }}; $checkSucceeded=$?; if ($null -ne $LASTEXITCODE -and $LASTEXITCODE -ne 0) {{ exit $LASTEXITCODE }}; if (-not $checkSucceeded) {{ exit 1 }}"));
                }
                #[cfg(not(windows))]
                {
                    command.args(["-c", source]);
                }
            }
        }
        command.stdin(std::process::Stdio::null());
        Ok(command)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[cfg(windows)]
    #[tokio::test]
    async fn system_shell_preserves_native_exit_and_reports_powershell_errors() -> Result<()> {
        let shell = CheckShell::System;
        let executable = shell.executable()?;
        let mut native = shell.command(&executable, "& pwsh -NoProfile -Command 'exit 13'")?;
        let result =
            tokio::time::timeout(std::time::Duration::from_secs(10), native.output()).await??;
        assert_eq!(result.status.code(), Some(13));
        let mut invalid = shell.command(
            &executable,
            "Get-Content -LiteralPath 'missing-check-fixture-file'",
        )?;
        let result =
            tokio::time::timeout(std::time::Duration::from_secs(10), invalid.output()).await??;
        assert!(!result.status.success());
        Ok(())
    }

    #[tokio::test]
    async fn nushell_selection_uses_nushell_and_preserves_failed_status() -> Result<()> {
        let Ok(executable) = CheckShell::Nushell.executable() else {
            return Ok(());
        };
        let mut command =
            CheckShell::Nushell.command(&executable, "print 'nushell output'; exit 9")?;
        let result =
            tokio::time::timeout(std::time::Duration::from_secs(10), command.output()).await??;
        assert_eq!(result.status.code(), Some(9));
        assert!(String::from_utf8_lossy(&result.stdout).contains("nushell output"));
        Ok(())
    }
}
