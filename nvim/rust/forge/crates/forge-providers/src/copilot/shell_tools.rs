//! Replace model-facing native shells without changing their argument vocabulary.

use std::sync::Arc;

use async_trait::async_trait;
use github_copilot_sdk::{Tool, ToolInvocation, ToolResult, tool::ToolHandler};
use serde::Deserialize;
use serde_json::{Value, json};

use super::shell_session::ShellSession;

/// Identify a supported native shell vocabulary without passing open tool names internally.
#[derive(Clone, Copy)]
pub(super) enum ShellTool {
    PowerShell,
    ReadPowerShell,
    StopPowerShell,
    ListPowerShell,
    Bash,
    ReadBash,
    StopBash,
    ListBash,
}

#[derive(Deserialize)]
#[serde(rename_all = "lowercase")]
enum LaunchMode {
    Sync,
    Async,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct LaunchArguments {
    command: String,
    description: String,
    #[serde(rename = "shellId")]
    shell_id: Option<String>,
    mode: Option<LaunchMode>,
    detach: Option<bool>,
    initial_wait: Option<f64>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct ReadArguments {
    #[serde(rename = "shellId")]
    shell_id: String,
    delay: f64,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct StopArguments {
    #[serde(rename = "shellId")]
    shell_id: String,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct ListArguments {}

impl ShellTool {
    /// Parse the SDK tool name once at the external callback boundary.
    fn parse(name: &str) -> Option<Self> {
        Some(match name {
            "powershell" => Self::PowerShell,
            "read_powershell" => Self::ReadPowerShell,
            "stop_powershell" => Self::StopPowerShell,
            "list_powershell" => Self::ListPowerShell,
            "bash" => Self::Bash,
            "read_bash" => Self::ReadBash,
            "stop_bash" => Self::StopBash,
            "list_bash" => Self::ListBash,
            _ => return None,
        })
    }

    /// Get the native wire name for this supported operation.
    pub(super) fn name(self) -> &'static str {
        match self {
            Self::PowerShell => "powershell",
            Self::ReadPowerShell => "read_powershell",
            Self::StopPowerShell => "stop_powershell",
            Self::ListPowerShell => "list_powershell",
            Self::Bash => "bash",
            Self::ReadBash => "read_bash",
            Self::StopBash => "stop_bash",
            Self::ListBash => "list_bash",
        }
    }

    /// Identify launches whose soft initial wait belongs to the main invocation.
    pub(super) fn is_command(self) -> bool {
        matches!(self, Self::PowerShell | Self::Bash)
    }

    /// Identify explicit output reads whose delay must remain interruptible by the owner.
    pub(super) fn is_read(self) -> bool {
        matches!(self, Self::ReadPowerShell | Self::ReadBash)
    }

    /// Reject malformed native arguments before allocating resources or entering permissions.
    pub(super) fn validate(
        self,
        arguments: &Value,
    ) -> Result<(), forge_provider_api::ProviderError> {
        let invalid = || {
            forge_provider_api::ProviderError::from(forge_provider_api::ProviderFailure::new(
                forge_provider_api::ProviderOperation::InputSubmit,
                forge_provider_api::ProviderFailureKind::Rejected,
            ))
        };
        match self {
            Self::PowerShell | Self::Bash => {
                let args: LaunchArguments =
                    serde_json::from_value(arguments.clone()).map_err(|_| invalid())?;
                if args.command.trim().is_empty()
                    || args.description.trim().is_empty()
                    || args
                        .shell_id
                        .as_ref()
                        .is_some_and(|id| id.trim().is_empty())
                    || args
                        .initial_wait
                        .is_some_and(|delay| !delay.is_finite() || delay < 0.0)
                    || (args.detach == Some(true) && !matches!(args.mode, Some(LaunchMode::Async)))
                {
                    return Err(invalid());
                }
            }
            Self::ReadPowerShell | Self::ReadBash => {
                let args: ReadArguments =
                    serde_json::from_value(arguments.clone()).map_err(|_| invalid())?;
                if args.shell_id.trim().is_empty() || !args.delay.is_finite() || args.delay < 0.0 {
                    return Err(invalid());
                }
            }
            Self::StopPowerShell | Self::StopBash => {
                let args: StopArguments =
                    serde_json::from_value(arguments.clone()).map_err(|_| invalid())?;
                if args.shell_id.trim().is_empty() {
                    return Err(invalid());
                }
            }
            Self::ListPowerShell | Self::ListBash => {
                let _: ListArguments =
                    serde_json::from_value(arguments.clone()).map_err(|_| invalid())?;
            }
        }
        Ok(())
    }

    /// Get the matching nonblocking native reader after a launch's soft wait.
    pub(super) fn read_name(self) -> &'static str {
        match self {
            Self::PowerShell
            | Self::ReadPowerShell
            | Self::StopPowerShell
            | Self::ListPowerShell => "read_powershell",
            Self::Bash | Self::ReadBash | Self::StopBash | Self::ListBash => "read_bash",
        }
    }
}

/// Advertise the platform shell tools using an implementation that cannot wake the model.
pub(super) fn tools(shell: Arc<ShellSession>) -> Vec<Tool> {
    let handler: Arc<dyn ToolHandler> = Arc::new(ShellHandler { shell });
    let name = if cfg!(windows) { "powershell" } else { "bash" };
    let command = json!({"type":"object","properties":{
        "command":{"type":"string"},"description":{"type":"string"},
        "shellId":{"type":"string","description":"Optional fresh unique label. A label already issued by this conversation cannot be reused for another command."},"mode":{"type":"string","enum":["sync","async"]},
        "detach":{"type":"boolean"},"initial_wait":{"type":"number","minimum":0}
    },"required":["command","description"]});
    let read = json!({"type":"object","properties":{"shellId":{"type":"string"},"delay":{"type":"number","minimum":0}},"required":["shellId","delay"]});
    let stop =
        json!({"type":"object","properties":{"shellId":{"type":"string"}},"required":["shellId"]});
    [
        (name.to_owned(), "Run a shell command. Sync waits up to initial_wait seconds (default 30) for initial output; this soft wait never terminates the command. Async returns a shellId immediately. Running commands remain available across turns. Use read to retrieve output and stop to terminate a command. Completion updates the terminal inventory without starting a new model turn.", command),
        (format!("read_{name}"), "Read output from a shellId returned by the shell command tool; delay controls how long to wait for new output.", read),
        (format!("stop_{name}"), "Stop the command identified by shellId.", stop),
        (format!("list_{name}"), "List shell commands owned by this conversation.", json!({"type":"object","properties":{}})),
    ].into_iter().map(|(name, description, schema)| {
        Tool::new(name).with_description(description).with_parameters(schema)
            .with_overrides_built_in_tool(true).with_skip_permission(true).with_handler(handler.clone())
    }).collect()
}

struct ShellHandler {
    shell: Arc<ShellSession>,
}

#[async_trait]
impl ToolHandler for ShellHandler {
    /// Preserve native tool output while keeping native failures behind the adapter boundary.
    async fn call(
        &self,
        invocation: ToolInvocation,
    ) -> Result<ToolResult, github_copilot_sdk::Error> {
        let Some(tool) = ShellTool::parse(&invocation.tool_name) else {
            return Ok(ToolResult::Expanded(
                github_copilot_sdk::ToolResultExpanded::new(
                    "Unsupported shell operation",
                    "failure",
                ),
            ));
        };
        let result = self
            .shell
            .execute(&invocation.tool_call_id, tool, invocation.arguments)
            .await;
        Ok(match result {
            Ok(output) => output.into_sdk(),
            Err(error) => {
                let message = match error {
                    forge_provider_api::ProviderError::Cancelled => "Shell operation was cancelled",
                    forge_provider_api::ProviderError::Failure(failure)
                        if matches!(
                            failure.kind(),
                            forge_provider_api::ProviderFailureKind::Authorization
                                | forge_provider_api::ProviderFailureKind::Rejected
                                | forge_provider_api::ProviderFailureKind::OperationConflict
                        ) =>
                    {
                        "Shell operation was not authorized or its identifier is already in use"
                    }
                    _ => "Shell operation failed",
                };
                ToolResult::Expanded(
                    github_copilot_sdk::ToolResultExpanded::new(message, "failure")
                        .with_error(message),
                )
            }
        })
    }
}
