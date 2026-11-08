use super::AcpKind as ProviderKind;
use anyhow::{Result, ensure};
use serde::Serialize;
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

const MANAGED_CLAUDE_VERSION: &str = "0.99.1";
const MANAGED_ANTIGRAVITY_VERSION: &str = "1.4.5";

#[derive(Serialize)]
/// Reports installation independently of authentication and provider session state.
pub struct RuntimeStatus {
    pub available: bool,
    pub managed: bool,
    pub adapter_version: Option<String>,
    pub command: Vec<String>,
    pub environment: BTreeMap<String, String>,
    #[serde(skip)]
    native_command: Option<Vec<String>>,
}

/// Resolve configured ACP adapters or the supported managed installation.
pub struct AcpRuntime {
    kind: ProviderKind,
    command: Vec<String>,
    root: PathBuf,
    native_command: Option<Vec<String>>,
}

impl AcpRuntime {
    /// Locate existing managed copies beneath the Harness data directory.
    pub fn new(kind: ProviderKind, command: Vec<String>, root: &Path) -> Self {
        let name = if kind == ProviderKind::Claude {
            "claude"
        } else {
            "antigravity"
        };
        let mut runtime = Self {
            kind,
            command,
            root: root.join("provider-runtime").join(name),
            native_command: None,
        };
        if kind == ProviderKind::Claude {
            let executable = std::env::var_os("CLAUDE_CODE_EXECUTABLE")
                .filter(|value| !value.is_empty())
                .map(PathBuf::from)
                .or_else(|| forge_shell::process::executable("claude"));
            if let Some(executable) = executable {
                runtime.select_native_command(vec![executable.to_string_lossy().into_owned()]);
            }
        }
        runtime
    }

    /// Get the adapter identity whose native launch this resolver owns.
    pub(super) fn kind(&self) -> ProviderKind {
        self.kind
    }

    /// Select an explicit native executable and its fixed launcher arguments before resolution.
    pub(super) fn select_native_command(&mut self, mut command: Vec<String>) {
        if let Some(program) = command.first_mut()
            && let Some(resolved) = forge_shell::process::executable(program)
        {
            *program = resolved.to_string_lossy().into_owned();
        }
        self.native_command = Some(command);
    }

    fn managed_version(&self) -> &'static str {
        if self.kind == ProviderKind::Claude {
            MANAGED_CLAUDE_VERSION
        } else {
            MANAGED_ANTIGRAVITY_VERSION
        }
    }

    fn default_command(&self) -> bool {
        self.command.len() == 1
            && self.command[0]
                == if self.kind == ProviderKind::Claude {
                    "claude-agent-acp"
                } else {
                    "agy_acp_server"
                }
    }

    fn managed_command(&self, root: &Path) -> Option<Vec<String>> {
        if self.kind == ProviderKind::Claude {
            let script =
                root.join("node_modules/@agentclientprotocol/claude-agent-acp/dist/index.js");
            let node = forge_shell::process::executable("node")?;
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
    pub fn status(&self) -> RuntimeStatus {
        let mut command = self.command.clone();
        let program = command
            .first()
            .and_then(|program| forge_shell::process::executable(program));
        if let Some(program) = &program {
            command[0] = program.to_string_lossy().into_owned();
        }
        let mut environment = BTreeMap::new();
        if self.kind == ProviderKind::Claude {
            let executable = std::env::var_os("CLAUDE_CODE_EXECUTABLE")
                .filter(|executable| !executable.is_empty())
                .map(PathBuf::from)
                .or_else(|| forge_shell::process::executable("claude"));
            if let Some(executable) = executable {
                environment.insert(
                    "CLAUDE_CODE_EXECUTABLE".into(),
                    executable.to_string_lossy().into_owned(),
                );
            }
        }
        let native_command = self.native_command.clone().or_else(|| {
            environment
                .get("CLAUDE_CODE_EXECUTABLE")
                .map(|program| vec![program.clone()])
        });
        if let Some(program) = native_command.as_ref().and_then(|command| command.first()) {
            environment.insert("CLAUDE_CODE_EXECUTABLE".into(), program.clone());
        }
        if program.is_none()
            && self.default_command()
            && let Some(command) = self.managed_command(&self.root.join(self.managed_version()))
        {
            return RuntimeStatus {
                available: true,
                managed: true,
                adapter_version: Some(self.managed_version().into()),
                command,
                environment,
                native_command,
            };
        }
        RuntimeStatus {
            available: program.is_some(),
            managed: false,
            adapter_version: None,
            command,
            environment,
            native_command,
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

impl RuntimeStatus {
    /// Validate slash-command suppression against the exact selected native executable before opening history.
    pub(super) async fn validate_native_claude(
        &self,
        context: &forge_provider_api::OperationContext,
    ) -> Result<forge_provider_api::version::ValidatedRuntime, forge_provider_api::ProviderError>
    {
        // Pinned contract: @anthropic-ai/claude-agent-sdk@0.3.312 sdk.d.ts, verbatimPrompts.
        // Older native CLIs ignore client_composed and can dispatch /goal from ordinary input.
        let requirement = forge_provider_api::version::RuntimeRequirement::new(
            ProviderKind::Claude.id(),
            self.native_command
                .as_ref()
                .and_then(|command| command.first())
                .map(PathBuf::from)
                .unwrap_or_else(|| PathBuf::from("claude")),
            semver::Version::new(2, 1, 248),
        );
        let reported = self.provider_version(context).await.map_err(|_| {
            forge_provider_api::ProviderError::from(
                requirement
                    .probe_failed(forge_provider_api::version::RuntimeProbeFailure::Unavailable),
            )
        })?;
        Ok(requirement.validate(reported.as_deref())?)
    }

    /// Report the native executable selected for ACP and terminal authentication.
    pub async fn provider_version(
        &self,
        context: &forge_provider_api::OperationContext,
    ) -> Result<Option<String>, forge_provider_api::ProviderError> {
        let Some((program, arguments)) = self
            .native_command
            .as_ref()
            .and_then(|command| command.split_first())
        else {
            return Ok(None);
        };
        context.check_admission(forge_provider_api::ProviderOperation::Inspect)?;
        #[cfg(test)]
        if let Some(version) = super::in_memory_peer::version_for(program) {
            return parse_claude_version(version.as_bytes()).map(Some);
        }
        use tokio::io::AsyncReadExt;
        let mut command = forge_shell::process::command(program, arguments);
        command.arg("--version");
        command
            .stdin(std::process::Stdio::null())
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::null());
        let mut child = forge_shell::process::spawn_owned(command)
            .map_err(|_| version_failure(forge_provider_api::ProviderFailureKind::Runtime))?;
        // SAFETY: Taking stdout preserves the wrapped child's identity and collection ownership.
        let mut stdout = unsafe { child.try_inner_child_mut() }
            .and_then(|child| child.stdout.take())
            .ok_or_else(|| version_failure(forge_provider_api::ProviderFailureKind::Runtime))?;
        let mut reader = tokio::spawn(async move {
            let mut output = Vec::new();
            stdout.read_to_end(&mut output).await.map(|_| output)
        });
        // Keep process collection in the retained worker after the original budget expires.
        let result = tokio::select! {
            biased;
            result = async {
                let status = child.wait().await;
                let output = (&mut reader).await;
                (status, output)
            } => Some(result),
            _ = context.interrupted() => None,
        };
        let (status, output) = match result {
            Some(result) => result,
            None => {
                let collected = Box::into_pin(child.kill()).await;
                let _ = reader.await;
                collected.map_err(|_| {
                    version_failure(forge_provider_api::ProviderFailureKind::Runtime)
                })?;
                return Err(context.interrupted().await.failure(
                    forge_provider_api::ProviderOperation::Inspect,
                    forge_provider_api::ProviderPhase::Observation,
                    forge_provider_api::FailureDisposition::Unchanged,
                ));
            }
        };
        let output = output
            .map_err(|_| version_failure(forge_provider_api::ProviderFailureKind::Runtime))?
            .map_err(|_| version_failure(forge_provider_api::ProviderFailureKind::Runtime))?;
        if !status
            .map_err(|_| version_failure(forge_provider_api::ProviderFailureKind::Runtime))?
            .success()
        {
            return Err(version_failure(
                forge_provider_api::ProviderFailureKind::Runtime,
            ));
        }
        parse_claude_version(&output).map(Some)
    }
}

fn parse_claude_version(output: &[u8]) -> Result<String, forge_provider_api::ProviderError> {
    let malformed = || version_failure(forge_provider_api::ProviderFailureKind::Protocol);
    let output = std::str::from_utf8(output).map_err(|_| malformed())?;
    let version = output.split_whitespace().next().ok_or_else(malformed)?;
    semver::Version::parse(version).map_err(|_| malformed())?;
    Ok(version.to_owned())
}

fn version_failure(
    kind: forge_provider_api::ProviderFailureKind,
) -> forge_provider_api::ProviderError {
    forge_provider_api::ProviderFailure::new(forge_provider_api::ProviderOperation::Inspect, kind)
        .into()
}
