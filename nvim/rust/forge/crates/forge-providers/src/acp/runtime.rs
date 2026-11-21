use super::AcpKind as ProviderKind;
use anyhow::{Result, bail, ensure};
use std::collections::BTreeMap;
use std::path::Path;

/// Reports the resolved adapter launch independently of provider session state.
pub struct RuntimeStatus {
    pub available: bool,
    pub command: Vec<String>,
    pub environment: BTreeMap<String, String>,
}

/// Resolve the installed bundled ACP command.
pub struct AcpRuntime {
    kind: ProviderKind,
    command: Vec<String>,
}

impl AcpRuntime {
    /// Configure the bundled command or an explicit launcher.
    pub fn new(kind: ProviderKind, command: Vec<String>) -> Self {
        Self { kind, command }
    }

    /// Get the adapter identity whose native launch this resolver owns.
    pub(super) fn kind(&self) -> ProviderKind {
        self.kind
    }

    /// Resolve the configured program from the current process search path.
    pub fn status(&self) -> RuntimeStatus {
        let mut command = self.command.clone();
        // A bare Antigravity launcher selects the installed CLI regardless of the launch directory.
        let program = command.first().and_then(|program| {
            if uses_installed_cli(program) {
                forge_shell::process::executable_on_path(program)
            } else {
                forge_shell::process::executable(program)
            }
        });
        if let Some(program) = &program {
            command[0] = program.to_string_lossy().into_owned();
        }
        let environment = BTreeMap::new();
        RuntimeStatus {
            available: program.is_some(),
            command,
            environment,
        }
    }

    /// Resolve a launch command without implicitly downloading software.
    pub fn resolve(&self) -> Result<RuntimeStatus> {
        ensure!(
            !self.command.first().is_some_and(|program| {
                Path::new(program)
                    .file_stem()
                    .and_then(|name| name.to_str())
                    .is_some_and(|name| name.eq_ignore_ascii_case("agy_acp_server"))
            }),
            "The standalone ACP server is retired. Remove the Antigravity command override to use agy acp."
        );
        let status = self.status();
        if !status.available {
            if self.command.first().is_some_and(|program| program == "agy") {
                bail!("Antigravity CLI is unavailable. Install agy and make it available on PATH.");
            }
            bail!(
                "Antigravity command override is unavailable. Correct the executable path or remove the override to use agy acp."
            );
        }
        Ok(status)
    }
}

/// Recognize unqualified CLI names while preserving explicit paths to launchers.
fn uses_installed_cli(program: &str) -> bool {
    if cfg!(windows) {
        return ["agy", "agy.exe", "agy.com", "agy.cmd", "agy.bat"]
            .iter()
            .any(|name| program.eq_ignore_ascii_case(name));
    }
    program == "agy"
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Treat bare spellings consistently and keep an explicit path outside implicit lookup.
    #[test]
    #[cfg(windows)]
    fn windows_bare_agy_spellings_use_installed_lookup() {
        for program in [
            "agy", "AGY", "agy.exe", "AGY.EXE", "AgY.CmD", "agy.bat", "agy.com",
        ] {
            assert!(uses_installed_cli(program), "{program}");
        }
        for program in [
            ".\\agy.exe",
            "bin/agy.exe",
            "C:\\Tools\\agy.exe",
            "launcher.exe",
        ] {
            assert!(!uses_installed_cli(program), "{program}");
        }
    }

    /// Resolve an absolute agy override even when the installation is outside PATH.
    #[test]
    #[cfg(windows)]
    fn absolute_override_resolves_without_searching_path() -> Result<()> {
        let directory = tempfile::tempdir()?;
        let program = directory.path().join("agy.exe");
        std::fs::write(&program, [])?;
        let command = vec![
            program.to_str().expect("temporary paths are Unicode").into(),
            "acp".into(),
        ];
        let runtime = AcpRuntime::new(ProviderKind::Antigravity, command.clone());

        assert_eq!(runtime.resolve()?.command, command);
        Ok(())
    }

    /// Direct users with stale absolute overrides to the setting that determines the executable.
    #[test]
    fn missing_absolute_override_reports_configuration() -> Result<()> {
        let directory = tempfile::tempdir()?;
        let program = directory.path().join("agy.exe");
        let command = vec![program.to_str().expect("temporary paths are Unicode").into()];
        let runtime = AcpRuntime::new(ProviderKind::Antigravity, command);

        assert!(matches!(runtime.resolve(), Err(error)
            if error.to_string().contains("Antigravity command override is unavailable")));
        Ok(())
    }

    /// Reject a configured standalone executable even when it is missing from PATH.
    #[test]
    fn standalone_override_reports_the_retired_runtime() {
        let runtime = AcpRuntime::new(ProviderKind::Antigravity, vec!["agy_acp_server.exe".into()]);
        let result = runtime.resolve();
        assert!(
            matches!(result, Err(error) if error.to_string().contains("standalone ACP server is retired"))
        );
    }
}
