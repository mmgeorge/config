//! Claude Code executable resolution, command line, environment, and version floor.

use super::transport::ProcessIo;
use async_trait::async_trait;
use forge_provider_api::{
    FailureDisposition, ProviderError, ProviderFailure, ProviderFailureKind, ProviderOperation,
    ProviderPhase,
};
use std::path::PathBuf;

/// Oldest CLI release whose stream-json behavior this adapter was verified against.
pub(crate) const MINIMUM_VERSION: semver::Version = semver::Version::new(2, 1, 317);

/// Select how a process opens its conversation history.
#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) enum SessionStart {
    /// Create a new persisted conversation under an adapter-minted id.
    Fresh { session_id: String },
    /// Resume a persisted conversation, optionally truncated after one transcript entry.
    Resume {
        session_id: String,
        resume_at: Option<String>,
        drops_turn: Option<String>,
    },
    /// Copy a persisted conversation into a new id, optionally truncated after one entry.
    Fork {
        source_id: String,
        target_id: String,
        resume_at: Option<String>,
    },
    /// Run without writing a transcript.
    Ephemeral,
}

/// Every launch-time choice of one CLI process.
#[derive(Clone, Debug)]
pub(crate) struct LaunchSpec {
    pub workspace: String,
    pub start: SessionStart,
    pub model: Option<String>,
    pub effort: Option<String>,
    pub planning: bool,
    pub mcp_config: McpConfig,
    pub plugin_dir: Option<PathBuf>,
}

/// Where the CLI reads its MCP server configuration.
///
/// Server entries can carry credentials such as bearer headers, and any local process can read
/// another process's command line, so a session's configuration lives in its private profile.
#[derive(Clone, Debug)]
pub(crate) enum McpConfig {
    /// A configuration that holds no credentials, passed as JSON text.
    Inline(serde_json::Value),
    /// A configuration file inside the session's private profile directory.
    File(PathBuf),
}

/// Native tools whose effects bypass Harness ownership of agents, goals, schedules, shells,
/// worktrees, and questions. Harness supplies each of these through its own control tools.
pub(crate) const DISALLOWED_TOOLS: &[&str] = &[
    "Agent",
    "Task",
    "ProposeGoal",
    "CronCreate",
    "CronDelete",
    "CronList",
    "RemoteTrigger",
    "ScheduleWakeup",
    "EnterWorktree",
    "ExitWorktree",
    "Workflow",
    "PushNotification",
    "SendMessage",
    "Monitor",
    "TaskStop",
    "TaskOutput",
    "AskUserQuestion",
];

impl LaunchSpec {
    /// Build the CLI arguments that follow the executable and its fixed launcher arguments.
    pub(crate) fn arguments(&self) -> Vec<String> {
        let mut arguments = vec![
            "-p".to_owned(),
            "--input-format=stream-json".to_owned(),
            "--output-format=stream-json".to_owned(),
            "--verbose".to_owned(),
            "--include-partial-messages".to_owned(),
            "--permission-prompt-tool=stdio".to_owned(),
            "--thinking-display=summarized".to_owned(),
            // User-level hooks and permission rules must not run inside Harness sessions.
            "--setting-sources=project,local".to_owned(),
            format!(
                "--settings={}",
                serde_json::json!({"permissions": {"ask": ["*"]}})
            ),
            "--strict-mcp-config".to_owned(),
            "--mcp-config".to_owned(),
            match &self.mcp_config {
                McpConfig::Inline(config) => config.to_string(),
                McpConfig::File(path) => path.display().to_string(),
            },
            format!("--disallowedTools={}", DISALLOWED_TOOLS.join(",")),
            format!(
                "--permission-mode={}",
                if self.planning { "plan" } else { "default" }
            ),
        ];
        if let Some(model) = &self.model {
            arguments.push(format!("--model={model}"));
        }
        if let Some(effort) = &self.effort {
            arguments.push(format!("--effort={effort}"));
        }
        if let Some(plugin_dir) = &self.plugin_dir {
            arguments.push(format!("--plugin-dir={}", plugin_dir.display()));
        }
        match &self.start {
            SessionStart::Fresh { session_id } => {
                arguments.push(format!("--session-id={session_id}"));
            }
            SessionStart::Resume {
                session_id,
                resume_at,
                drops_turn,
            } => {
                arguments.push(format!("--resume={session_id}"));
                if let Some(entry) = resume_at {
                    arguments.push(format!("--resume-session-at={entry}"));
                }
                if let Some(prompt) = drops_turn {
                    arguments.push(format!("--resume-drops-turn={prompt}"));
                }
            }
            SessionStart::Fork {
                source_id,
                target_id,
                resume_at,
            } => {
                arguments.push(format!("--resume={source_id}"));
                arguments.push("--fork-session".to_owned());
                arguments.push(format!("--session-id={target_id}"));
                if let Some(entry) = resume_at {
                    arguments.push(format!("--resume-session-at={entry}"));
                }
            }
            SessionStart::Ephemeral => arguments.push("--no-session-persistence".to_owned()),
        }
        arguments
    }
}

/// Start CLI processes and report the installed CLI version.
#[async_trait]
pub(crate) trait ClaudeLauncher: Send + Sync {
    /// Report whether a CLI executable is installed.
    fn available(&self) -> bool;
    /// Name the executable for version diagnostics.
    fn executable(&self) -> PathBuf;
    /// Report the installed CLI version.
    async fn version(
        &self,
        context: &forge_provider_api::OperationContext,
    ) -> Result<String, ProviderError>;
    /// Start one CLI process.
    fn launch(&self, spec: &LaunchSpec) -> Result<ProcessIo, ProviderError>;
}

/// Launch the installed `claude` executable.
pub(crate) struct ProcessLauncher {
    command: Vec<String>,
}

impl ProcessLauncher {
    /// Resolve `CLAUDE_CODE_EXECUTABLE` first, then the configured command.
    pub(crate) fn new(mut command: Vec<String>) -> Self {
        if let Some(executable) =
            std::env::var_os("CLAUDE_CODE_EXECUTABLE").filter(|value| !value.is_empty())
        {
            command = vec![executable.to_string_lossy().into_owned()];
        }
        if let Some(program) = command.first_mut()
            && let Some(resolved) = forge_shell::process::executable(program)
        {
            *program = resolved.to_string_lossy().into_owned();
        }
        Self { command }
    }

    fn program(&self) -> Option<(&String, &[String])> {
        self.command.split_first()
    }
}

#[async_trait]
impl ClaudeLauncher for ProcessLauncher {
    fn available(&self) -> bool {
        self.program()
            .is_some_and(|(program, _)| std::path::Path::new(program).is_file())
    }

    fn executable(&self) -> PathBuf {
        self.program()
            .map(|(program, _)| PathBuf::from(program))
            .unwrap_or_else(|| PathBuf::from("claude"))
    }

    async fn version(
        &self,
        context: &forge_provider_api::OperationContext,
    ) -> Result<String, ProviderError> {
        use tokio::io::AsyncReadExt;
        let (program, fixed) = self
            .program()
            .ok_or_else(|| version_failure(ProviderFailureKind::Runtime))?;
        context.check_admission(ProviderOperation::Inspect)?;
        let mut command = forge_shell::process::command(program, fixed);
        command.arg("--version");
        command
            .stdin(std::process::Stdio::null())
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::null());
        let mut child = forge_shell::process::spawn_owned(command)
            .map_err(|_| version_failure(ProviderFailureKind::Runtime))?;
        // SAFETY: Taking stdout preserves the wrapped child's identity and collection ownership.
        let mut stdout = unsafe { child.try_inner_child_mut() }
            .and_then(|child| child.stdout.take())
            .ok_or_else(|| version_failure(ProviderFailureKind::Runtime))?;
        let mut reader = tokio::spawn(async move {
            let mut output = Vec::new();
            stdout.read_to_end(&mut output).await.map(|_| output)
        });
        let result = tokio::select! {
            biased;
            result = async {
                let status = child.wait().await;
                let output = (&mut reader).await;
                (status, output)
            } => Some(result),
            _ = context.interrupted() => None,
        };
        let Some((status, output)) = result else {
            let collected = Box::into_pin(child.kill()).await;
            let _ = reader.await;
            collected.map_err(|_| version_failure(ProviderFailureKind::Runtime))?;
            return Err(context.interrupted().await.failure(
                ProviderOperation::Inspect,
                ProviderPhase::Observation,
                FailureDisposition::Unchanged,
            ));
        };
        let output = output
            .map_err(|_| version_failure(ProviderFailureKind::Runtime))?
            .map_err(|_| version_failure(ProviderFailureKind::Runtime))?;
        if !status
            .map_err(|_| version_failure(ProviderFailureKind::Runtime))?
            .success()
        {
            return Err(version_failure(ProviderFailureKind::Runtime));
        }
        parse_version(&output)
    }

    fn launch(&self, spec: &LaunchSpec) -> Result<ProcessIo, ProviderError> {
        let (program, fixed) = self
            .program()
            .ok_or_else(|| launch_failure(ProviderFailureKind::Runtime))?;
        let mut arguments = fixed.to_vec();
        arguments.extend(spec.arguments());
        let mut command = forge_shell::process::command(program, &arguments);
        // Mark the session as programmatic so interactive session pickers can exclude it.
        command.env("CLAUDE_CODE_ENTRYPOINT", "sdk-rust");
        command.env("CLAUDE_CODE_SDK_READS_SESSION_STATE", "1");
        // The CLI owns no background work, so no notification can start a turn on its own.
        command.env("CLAUDE_CODE_DISABLE_BACKGROUND_TASKS", "1");
        command.env(
            "CLAUDE_AGENT_SDK_CLIENT_APP",
            format!("forge/{}", env!("CARGO_PKG_VERSION")),
        );
        command.env_remove("NODE_OPTIONS");
        command
            .current_dir(&spec.workspace)
            .stdin(std::process::Stdio::piped())
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::null());
        let mut process = forge_shell::process::spawn_owned(command)
            .map_err(|_| launch_failure(ProviderFailureKind::Runtime))?;
        let stdin = process
            .stdin()
            .take()
            .ok_or_else(|| launch_failure(ProviderFailureKind::Runtime))?;
        let stdout = process
            .stdout()
            .take()
            .ok_or_else(|| launch_failure(ProviderFailureKind::Runtime))?;
        Ok(ProcessIo {
            stdout: Box::new(stdout),
            stdin: Box::new(stdin),
            process: Some(process),
        })
    }
}

/// Parse `claude --version` output such as `2.1.317 (Claude Code)`.
pub(crate) fn parse_version(output: &[u8]) -> Result<String, ProviderError> {
    let malformed = || version_failure(ProviderFailureKind::Protocol);
    let output = std::str::from_utf8(output).map_err(|_| malformed())?;
    let version = output.split_whitespace().next().ok_or_else(malformed)?;
    semver::Version::parse(version).map_err(|_| malformed())?;
    Ok(version.to_owned())
}

fn version_failure(kind: ProviderFailureKind) -> ProviderError {
    ProviderFailure::new(ProviderOperation::Inspect, kind).into()
}

fn launch_failure(kind: ProviderFailureKind) -> ProviderError {
    ProviderFailure::new(ProviderOperation::ThreadOpen, kind)
        .at_phase(ProviderPhase::Preparation, FailureDisposition::Unchanged)
        .into()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn spec(start: SessionStart) -> LaunchSpec {
        LaunchSpec {
            workspace: ".".into(),
            start,
            model: Some("haiku".into()),
            effort: Some("low".into()),
            planning: false,
            mcp_config: McpConfig::Inline(serde_json::json!({"mcpServers": {}})),
            plugin_dir: None,
        }
    }

    #[test]
    fn arguments_select_history_and_keep_mcp_config_as_one_argument() {
        let fresh = spec(SessionStart::Fresh {
            session_id: "fresh-id".into(),
        })
        .arguments();
        assert!(fresh.contains(&"--session-id=fresh-id".to_owned()));
        assert!(fresh.contains(&"--model=haiku".to_owned()));
        let mcp = fresh.iter().position(|argument| argument == "--mcp-config");
        assert_eq!(
            fresh[mcp.unwrap() + 1],
            serde_json::json!({"mcpServers": {}}).to_string()
        );
        let rewound = spec(SessionStart::Resume {
            session_id: "kept".into(),
            resume_at: Some("entry".into()),
            drops_turn: Some("prompt".into()),
        })
        .arguments();
        assert!(rewound.contains(&"--resume=kept".to_owned()));
        assert!(rewound.contains(&"--resume-session-at=entry".to_owned()));
        assert!(rewound.contains(&"--resume-drops-turn=prompt".to_owned()));
        let forked = spec(SessionStart::Fork {
            source_id: "source".into(),
            target_id: "target".into(),
            resume_at: None,
        })
        .arguments();
        assert!(forked.contains(&"--fork-session".to_owned()));
        assert!(forked.contains(&"--session-id=target".to_owned()));
        assert!(
            spec(SessionStart::Ephemeral)
                .arguments()
                .contains(&"--no-session-persistence".to_owned())
        );
    }

    #[test]
    fn versions_parse_from_the_cli_banner() {
        assert_eq!(
            parse_version(b"2.1.317 (Claude Code)\n").unwrap(),
            "2.1.317"
        );
        assert!(parse_version(b"not a version").is_err());
    }
}
