use crate::backend::BackendKind;
use anyhow::{Context, Result, ensure};
use serde::Serialize;
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

const LEGACY_CLAUDE_VERSION: &str = "0.89.1";
const LEGACY_ANTIGRAVITY_VERSION: &str = "1.3.0";

#[derive(Serialize)]
/// Reports installation independently of authentication and provider session state.
pub(super) struct RuntimeStatus {
    pub(super) available: bool,
    pub(super) managed: bool,
    pub(super) adapter_version: Option<String>,
    pub(super) command: Vec<String>,
    pub(super) environment: BTreeMap<String, String>,
}

/// Resolves installed ACP adapters and retains compatibility with existing managed copies.
pub(super) struct AcpRuntime {
    kind: BackendKind,
    command: Vec<String>,
    root: PathBuf,
}

impl AcpRuntime {
    /// Locate existing managed copies beneath the Harness data directory.
    pub(super) fn new(kind: BackendKind, command: Vec<String>, root: &Path) -> Self {
        let name = if kind == BackendKind::Claude {
            "claude"
        } else {
            "antigravity"
        };
        Self {
            kind,
            command,
            root: root.join("provider-runtime").join(name),
        }
    }

    fn legacy_version(&self) -> &'static str {
        if self.kind == BackendKind::Claude {
            LEGACY_CLAUDE_VERSION
        } else {
            LEGACY_ANTIGRAVITY_VERSION
        }
    }

    fn default_command(&self) -> bool {
        self.command.len() == 1
            && self.command[0]
                == if self.kind == BackendKind::Claude {
                    "claude-agent-acp"
                } else {
                    "agy_acp_server"
                }
    }

    fn managed_command(&self, root: &Path) -> Option<Vec<String>> {
        if self.kind == BackendKind::Claude {
            let script =
                root.join("node_modules/@agentclientprotocol/claude-agent-acp/dist/index.js");
            let node = crate::process::executable("node")?;
            script.is_file().then(|| {
                vec![
                    node.to_string_lossy().into_owned(),
                    script.to_string_lossy().into_owned(),
                ]
            })
        } else {
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
    }

    /// Resolve the configured adapter before falling back to an existing managed copy.
    pub(super) fn status(&self) -> RuntimeStatus {
        let mut command = self.command.clone();
        let program = command
            .first()
            .and_then(|program| crate::process::executable(program));
        if let Some(program) = &program {
            command[0] = program.to_string_lossy().into_owned();
        }
        let mut environment = BTreeMap::new();
        if self.kind == BackendKind::Claude {
            let executable = std::env::var_os("CLAUDE_CODE_EXECUTABLE")
                .filter(|executable| !executable.is_empty())
                .map(PathBuf::from)
                .or_else(|| crate::process::executable("claude"));
            if let Some(executable) = executable {
                environment.insert(
                    "CLAUDE_CODE_EXECUTABLE".into(),
                    executable.to_string_lossy().into_owned(),
                );
            }
        }
        if program.is_none()
            && self.default_command()
            && let Some(command) = self.managed_command(&self.root.join(self.legacy_version()))
        {
            return RuntimeStatus {
                available: true,
                managed: true,
                adapter_version: Some(self.legacy_version().into()),
                command,
                environment,
            };
        }
        RuntimeStatus {
            available: program.is_some(),
            managed: false,
            adapter_version: None,
            command,
            environment,
        }
    }

    /// Resolve a launch command without implicitly downloading software.
    pub(super) fn resolve(&self) -> Result<RuntimeStatus> {
        let status = self.status();
        ensure!(
            status.available,
            "ACP runtime is not installed. Configure an installed ACP adapter in harness.backends"
        );
        Ok(status)
    }
}

impl RuntimeStatus {
    /// Report the native executable selected for ACP and terminal authentication.
    pub(super) async fn provider_version(&self) -> Result<Option<String>> {
        let Some(program) = self.environment.get("CLAUDE_CODE_EXECUTABLE") else {
            return Ok(None);
        };
        let mut command = crate::process::command(program, &["--version".into()]);
        command
            .stdin(std::process::Stdio::null())
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::piped());
        let child =
            crate::process::spawn_owned(command).context("launch Claude Code version check")?;
        let output = Box::into_pin(child.wait_with_output()).await?;
        ensure!(
            output.status.success(),
            "Claude Code version check failed: {}",
            String::from_utf8_lossy(&output.stderr).trim()
        );
        parse_claude_version(&output.stdout).map(Some)
    }
}

fn parse_claude_version(output: &[u8]) -> Result<String> {
    let output = std::str::from_utf8(output).context("Claude Code version is not UTF-8")?;
    let version = output
        .split_whitespace()
        .next()
        .context("Claude Code version is empty")?;
    semver::Version::parse(version).context("Claude Code returned an invalid version")?;
    Ok(version.to_owned())
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn claude_version_is_independent_of_adapter_version() -> Result<()> {
        assert_eq!(
            parse_claude_version(b"2.1.302 (Claude Code)\r\n")?,
            "2.1.302"
        );
        assert_eq!(
            parse_claude_version(b"2.2.0-beta.1 (Claude Code)\n")?,
            "2.2.0-beta.1"
        );
        assert!(parse_claude_version(b"").is_err());
        assert!(parse_claude_version(b"unexpected version output").is_err());
        assert!(parse_claude_version(&[0xff]).is_err());
        Ok(())
    }

    #[tokio::test]
    async fn launch_preserves_selected_claude_executable() -> Result<()> {
        use super::super::session::AcpSessionState;
        use super::super::transport::AcpTransport;
        use crate::backend::approval::PermissionCoordinator;
        use serde_json::json;
        use std::sync::Arc;

        let directory = tempfile::tempdir()?;
        let peer = directory.path().join("peer.cjs");
        std::fs::write(
            &peer,
            r#"
const readline = require('node:readline');
readline.createInterface({input: process.stdin}).on('line', line => {
  const request = JSON.parse(line);
  if (request.id === undefined) return;
  process.stdout.write(JSON.stringify({jsonrpc: '2.0', id: request.id, result: {
    protocolVersion: 1, executable: process.env.CLAUDE_CODE_EXECUTABLE,
    agentInfo: {version: 'adapter-fixture'}
  }}) + '\n');
});
"#,
        )?;
        let executable = directory
            .path()
            .join("selected native CLI.exe")
            .to_string_lossy()
            .into_owned();
        let runtime = RuntimeStatus {
            available: true,
            managed: false,
            adapter_version: None,
            command: vec!["node".into(), peer.to_string_lossy().into_owned()],
            environment: BTreeMap::from([("CLAUDE_CODE_EXECUTABLE".into(), executable.clone())]),
        };
        let handler = Arc::new(AcpSessionState::new(
            BackendKind::Claude,
            PermissionCoordinator::transient(directory.path())?,
        ));
        let transport =
            AcpTransport::launch(&runtime, &directory.path().to_string_lossy(), handler, None)
                .await?;
        let initialized = transport
            .request("initialize", json!({"protocolVersion": 1}))
            .await?;
        transport.shutdown().await?;
        assert_eq!(initialized["executable"], executable);
        assert_eq!(initialized["agentInfo"]["version"], "adapter-fixture");
        assert!(
            runtime.provider_version().await.is_err(),
            "missing native executable must report a version failure"
        );
        Ok(())
    }

    #[test]
    fn missing_runtime_does_not_create_installation() -> Result<()> {
        let root = tempfile::tempdir()?;
        let runtime = AcpRuntime::new(
            BackendKind::Claude,
            vec![
                root.path()
                    .join("missing-adapter")
                    .to_string_lossy()
                    .into_owned(),
            ],
            root.path(),
        );
        assert!(!runtime.status().available);
        let error = runtime
            .resolve()
            .err()
            .context("missing adapter resolved")?;
        assert!(
            error
                .to_string()
                .contains("Configure an installed ACP adapter")
        );
        assert!(!root.path().join("provider-runtime").exists());
        Ok(())
    }
}
