use super::AcpKind as ProviderKind;
use anyhow::{Result, ensure};
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

/// Reports the resolved adapter launch independently of provider session state.
pub struct RuntimeStatus {
    pub available: bool,
    pub command: Vec<String>,
    pub environment: BTreeMap<String, String>,
}

/// Resolve configured ACP adapters or the supported managed installation.
pub struct AcpRuntime {
    kind: ProviderKind,
    command: Vec<String>,
    root: PathBuf,
}

impl AcpRuntime {
    /// Locate existing managed copies beneath the Harness data directory.
    pub fn new(kind: ProviderKind, command: Vec<String>, root: &Path) -> Self {
        Self {
            kind,
            command,
            root: root.join("provider-runtime").join("antigravity"),
        }
    }

    /// Get the adapter identity whose native launch this resolver owns.
    pub(super) fn kind(&self) -> ProviderKind {
        self.kind
    }

    fn default_command(&self) -> bool {
        self.command.len() == 1 && self.command[0] == "agy_acp_server"
    }

    fn managed_command(&self, root: &Path) -> Option<Vec<String>> {
        let program = root.join(if cfg!(windows) {
            "agy_acp_server.exe"
        } else {
            "agy_acp_server.par"
        });
        program.is_file().then(|| {
            let mut command = vec![program.to_string_lossy().into_owned()];
            if cfg!(target_os = "linux") {
                command.push("--uid=".into());
            }
            command
        })
    }

    /// Select the newest installed copy; native initialization owns version validation.
    fn latest_managed_command(&self) -> Option<Vec<String>> {
        std::fs::read_dir(&self.root)
            .ok()?
            .filter_map(Result::ok)
            .filter_map(|entry| {
                let version = semver::Version::parse(entry.file_name().to_str()?).ok()?;
                let command = self.managed_command(&entry.path())?;
                Some((version, command))
            })
            .max_by(|left, right| left.0.cmp(&right.0))
            .map(|(_, command)| command)
    }

    /// Resolve the configured adapter before falling back to an existing managed copy.
    pub fn status(&self) -> RuntimeStatus {
        let mut command = self.command.clone();
        let program = command
            .first()
            .and_then(|program| forge_shell::process::executable(program));
        if let Some(program) = &program {
            command[0] = program.to_string_lossy().into_owned();
        }
        let environment = BTreeMap::new();
        if program.is_none()
            && self.default_command()
            && let Some(command) = self.latest_managed_command()
        {
            return RuntimeStatus {
                available: true,
                command,
                environment,
            };
        }
        RuntimeStatus {
            available: program.is_some(),
            command,
            environment,
        }
    }

    /// Resolve a launch command without implicitly downloading software.
    pub fn resolve(&self) -> Result<RuntimeStatus> {
        let status = self.status();
        ensure!(
            status.available,
            "ACP runtime is not installed. Configure an installed ACP adapter in harness.providers"
        );
        Ok(status)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Prefer the newest installed runtime without an exact-version installation pin.
    #[test]
    fn managed_runtime_selects_newest_installed_version() -> Result<()> {
        let directory = tempfile::tempdir()?;
        let runtime = AcpRuntime::new(
            ProviderKind::Antigravity,
            vec!["agy_acp_server".into()],
            directory.path(),
        );
        assert!(runtime.latest_managed_command().is_none());
        let executable = if cfg!(windows) {
            "agy_acp_server.exe"
        } else {
            "agy_acp_server.par"
        };
        for version in ["1.4.5", "1.5.0", "unversioned"] {
            let root = runtime.root.join(version);
            std::fs::create_dir_all(&root)?;
            std::fs::write(root.join(executable), [])?;
        }
        std::fs::create_dir_all(runtime.root.join("99.0.0"))?;
        let command = runtime
            .latest_managed_command()
            .expect("an installed version has an executable");
        assert_eq!(
            Path::new(&command[0]),
            runtime.root.join("1.5.0").join(executable)
        );
        Ok(())
    }
}
