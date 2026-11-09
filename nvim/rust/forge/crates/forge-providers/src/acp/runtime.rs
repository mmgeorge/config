use super::AcpKind as ProviderKind;
use anyhow::{Result, ensure};
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

const MANAGED_ANTIGRAVITY_VERSION: &str = "1.4.5";

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
            && let Some(command) =
                self.managed_command(&self.root.join(MANAGED_ANTIGRAVITY_VERSION))
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
