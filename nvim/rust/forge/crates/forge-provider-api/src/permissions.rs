use crate::ProviderError;
use async_trait::async_trait;
use forge_shell::shell::CommandShell;
use serde::{Deserialize, Serialize};

/// Represent the result of host authorization without a native SDK response type.
#[derive(Clone, Debug)]
pub enum PermissionResponse {
    /// Authorize this request once.
    Allow,
    /// Reject this request and optionally explain the decision to the model.
    Deny { reason: Option<String> },
    /// The user cancelled the pending permission decision.
    Cancelled,
}

/// Authorize a native tool whose event identifies its root or child invocation.
#[async_trait]
pub trait ScopedPermissions: Send + Sync {
    async fn authorize(
        &self,
        address: crate::TurnAddress,
        request: PermissionRequest,
    ) -> Result<PermissionResponse, ProviderError>;
}

/// Enforce host task, checkpoint, and permission policy for provider callbacks.
#[async_trait]
pub trait ProviderPermissions: Send + Sync {
    /// Resolve one normalized permission request for its native session.
    async fn authorize(
        &self,
        scope: &crate::InvocationScope,
        request: PermissionRequest,
    ) -> PermissionResponse;

    /// Authorize an explicitly registered native child under its original parent authority.
    /// Implementations must validate binding, ancestry, child turn ownership, and independent revocation.
    async fn authorize_observed_child(
        &self,
        parent: &crate::InvocationScope,
        child: &crate::TurnAddress,
        request: PermissionRequest,
    ) -> PermissionResponse {
        let _ = (parent, child, request);
        PermissionResponse::Deny {
            reason: Some("Native child permission authority is unavailable".into()),
        }
    }

    /// Wait for the original child's host barrier after validating its registered parent relationship.
    async fn before_observed_child(
        &self,
        parent: &crate::InvocationScope,
        child: &crate::TurnAddress,
    ) -> Result<(), ProviderError> {
        let _ = (parent, child);
        Err(ProviderError::Cancelled)
    }

    /// Authorize verified private session storage while preserving host policy and revocation.
    /// The adapter must first prove every target is a read/write beneath this binding's own
    /// canonical storage directory, including symlink resolution. Ordinary work never uses this.
    async fn authorize_session_storage(
        &self,
        scope: &crate::InvocationScope,
        request: PermissionRequest,
    ) -> PermissionResponse {
        self.authorize(scope, request).await
    }

    /// Wait for the host's pre-execution barrier before native tool execution.
    async fn before_tool(&self, scope: &crate::InvocationScope) -> Result<(), ProviderError>;
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum PermissionTarget {
    /// Reject a request whose operation cannot be safely normalized.
    Unknown,
    Command {
        command: String,
        #[serde(default)]
        shell: Option<CommandShell>,
    },
    Read {
        path: String,
    },
    Write {
        path: String,
    },
    UnknownFile {
        operation: crate::access::DirectoryOperation,
    },
    Network {
        target: String,
    },
    Mcp {
        target: String,
    },
    Elevate {
        target: String,
    },
    Tool {
        target: String,
    },
    /// Identify a host-owned canonical tool independently of its advertised native alias.
    HostTool {
        name: String,
    },
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct PermissionRequest {
    pub id: String,
    pub provider: crate::ProviderId,
    pub reason: Option<String>,
    pub target_list: Vec<PermissionTarget>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
/// Rejects command execution when Harness cannot determine every shell invocation.
pub struct CommandAnalysisError {
    /// Shell selected at the provider or nested-launcher boundary.
    pub shell: CommandShell,
    /// Cause that prevents authorization of any partial result.
    pub failure: forge_shell::shell::CommandAnalysisFailure,
}

impl std::fmt::Display for CommandAnalysisError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.failure {
            forge_shell::shell::CommandAnalysisFailure::MalformedTransport => write!(
                formatter,
                "shell_command_parse_error: Harness could not decode the {:?} command transport. No command was executed.",
                self.shell
            ),
            forge_shell::shell::CommandAnalysisFailure::Syntax => write!(
                formatter,
                "shell_command_parse_error: Harness could not parse the {:?} command syntax. Simplify it or split independent operations into separate tool calls, keeping pipelines intact. No command was executed.",
                self.shell
            ),
            forge_shell::shell::CommandAnalysisFailure::Unsupported(construct) => write!(
                formatter,
                "shell_command_unsupported: Harness does not support {construct} in {:?} command analysis. Rewrite the unsupported construct while keeping pipelines intact. No command was executed.",
                self.shell
            ),
            forge_shell::shell::CommandAnalysisFailure::UnresolvedCommands(names) => write!(
                formatter,
                "shell_command_not_found: PowerShell could not resolve command(s): {}. Check command names and quote literal pipe characters in arguments (for example, --format='%h|%ad|%s'). Profile-only definitions are not loaded during validation. No command was executed.",
                names.join(", ")
            ),
            forge_shell::shell::CommandAnalysisFailure::Incomplete(reason) => write!(
                formatter,
                "shell_command_analysis_failed: Harness could not finish {:?} command analysis: {reason}. No command was executed.",
                self.shell
            ),
        }
    }
}

impl std::error::Error for CommandAnalysisError {}
