use crate::command::{normalize_command_in, shell_token_list};
use crate::shell::{CommandAnalysisFailure, CommandShell};
use std::collections::BTreeSet;
use std::path::{Path, PathBuf};
use std::process::Stdio;
use std::time::Duration;
use tokio::io::AsyncReadExt;

// Names travel as JSON data, never as executable PowerShell source. Disable module
// autoloading: discovery must not execute module initialization before approval.
const DISCOVER: &str = r#"
$ErrorActionPreference = 'Stop'
[Console]::OutputEncoding = [System.Text.UTF8Encoding]::new($false)
$names = @(Microsoft.PowerShell.Utility\ConvertFrom-Json -InputObject $env:FORGE_COMMAND_NAMES)
$PSModuleAutoLoadingPreference = 'None'
$missing = @($names | Microsoft.PowerShell.Core\ForEach-Object {
    if (-not (Microsoft.PowerShell.Core\Get-Command -Name $_ -ListImported -ErrorAction SilentlyContinue)) { $_ }
})
if ($missing.Count -gt 0) {
    $modules = @(Microsoft.PowerShell.Core\Get-Module -ListAvailable)
    $missing = @($missing | Microsoft.PowerShell.Core\Where-Object {
        $name = $_
        -not ($modules | Microsoft.PowerShell.Core\Where-Object {
            $prefix = $_.Name + '\'
            $_.ExportedCommands.ContainsKey($name) -or (
                $name.StartsWith($prefix, [System.StringComparison]::OrdinalIgnoreCase) -and
                $_.ExportedCommands.ContainsKey($name.Substring($prefix.Length))
            )
        })
    })
}
Microsoft.PowerShell.Utility\ConvertTo-Json -InputObject $missing -Compress
"#;

/// Resolve literal PowerShell commands without executing the submitted source or loading profiles.
///
/// Discover built-in aliases, imported commands, installed module exports, and executables in
/// the host environment. Other shells retain syntax-only analysis. Profile-only definitions
/// are unavailable; callers must not treat this preflight as proof that execution will succeed.
pub async fn validate_command_resolution(
    command: &str,
    shell: CommandShell,
    working_directory: &Path,
) -> Result<(), CommandAnalysisFailure> {
    let normalized = normalize_command_in(command, shell);
    if let Some(failure) = normalized.failure {
        return Err(failure);
    }
    let mut names = BTreeSet::new();
    for invocation in normalized.invocation_list {
        if invocation.shell != CommandShell::PowerShell {
            continue;
        }
        let tokens = shell_token_list(&invocation.source, invocation.shell)
            .map_err(|error| CommandAnalysisFailure::Incomplete(error.to_string()))?;
        let mut tokens = tokens.into_iter();
        let name = tokens
            .next()
            .filter(|name| name != "&")
            .or_else(|| tokens.next());
        if let Some(name) = name {
            names.insert(name);
        }
    }
    if names.is_empty() {
        return Ok(());
    }
    let executable = powershell_executable().ok_or_else(|| {
        CommandAnalysisFailure::Incomplete("PowerShell command discovery is unavailable".into())
    })?;
    let mut probe = tokio::process::Command::new(executable);
    probe
        .args([
            "-NoLogo",
            "-NoProfile",
            "-NonInteractive",
            "-Command",
            DISCOVER,
        ])
        .env(
            "FORGE_COMMAND_NAMES",
            serde_json::to_string(&names)
                .map_err(|error| CommandAnalysisFailure::Incomplete(error.to_string()))?,
        )
        .current_dir(working_directory)
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::null());
    let mut child = crate::process::spawn_owned(probe).map_err(|error| {
        CommandAnalysisFailure::Incomplete(format!("PowerShell discovery launch failed: {error}"))
    })?;
    let output = tokio::time::timeout(Duration::from_secs(5), async {
        let mut output = Vec::new();
        child
            .stdout()
            .take()
            .expect("discovery stdout is piped")
            .take(1_048_576)
            .read_to_end(&mut output)
            .await?;
        let status = child.wait().await?;
        Ok::<_, std::io::Error>((status, output))
    })
    .await
    .map_err(|_| {
        CommandAnalysisFailure::Incomplete("PowerShell command discovery timed out".into())
    })?
    .map_err(|error| {
        CommandAnalysisFailure::Incomplete(format!("PowerShell command discovery failed: {error}"))
    })?;
    if !output.0.success() {
        return Err(CommandAnalysisFailure::Incomplete(
            "PowerShell command discovery exited unsuccessfully".into(),
        ));
    }
    let missing: Vec<String> = serde_json::from_slice(&output.1).map_err(|error| {
        CommandAnalysisFailure::Incomplete(format!(
            "invalid PowerShell discovery response: {error}"
        ))
    })?;
    if missing.is_empty() {
        return Ok(());
    }
    Err(CommandAnalysisFailure::UnresolvedCommands(missing))
}

/// Find the discovery interpreter on the host PATH without searching the working directory.
fn powershell_executable() -> Option<PathBuf> {
    let search_path = std::env::var_os("PATH")?;
    for program in ["pwsh", "powershell"] {
        for directory in std::env::split_paths(&search_path).filter(|path| path.is_absolute()) {
            let candidate = directory.join(program);
            if let Some(executable) = candidate.to_str().and_then(crate::process::executable) {
                return Some(executable);
            }
        }
    }
    None
}

#[cfg(all(test, windows))]
mod tests {
    use super::*;

    /// Reject unquoted Git placeholders without executing any pipeline stage.
    #[tokio::test]
    async fn rejects_unresolved_pipeline_commands() {
        let workspace = tempfile::tempdir().unwrap();
        let failure = validate_command_resolution(
            "git log -n 25 --format=%h|%ad|%s --date=short",
            CommandShell::PowerShell,
            workspace.path(),
        )
        .await
        .unwrap_err();
        assert_eq!(
            failure,
            CommandAnalysisFailure::UnresolvedCommands(vec!["%ad".into(), "%s".into()])
        );
    }

    /// Preserve literal format arguments and the built-in percent alias without executing them.
    #[tokio::test]
    async fn accepts_percent_alias_and_quoted_format_without_execution() {
        let workspace = tempfile::tempdir().unwrap();
        validate_command_resolution(
            r"git log --format='%h|%ad|%s'; Write-Output 1 | % { Write-Output $_ }; Microsoft.PowerShell.Management\Get-Item .",
            CommandShell::PowerShell,
            workspace.path(),
        )
        .await
        .unwrap();
    }
}
