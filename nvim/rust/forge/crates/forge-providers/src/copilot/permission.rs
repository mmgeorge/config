use crate::permission_input::{normalize_permission_payload, provider_path, provider_shell};
use forge_provider_api::access::DirectoryOperation;
use forge_provider_api::permissions::{PermissionRequest, PermissionTarget};
use serde_json::Value;

/// Normalize one Copilot SDK permission callback into the Harness permission model.
pub fn permission_from_copilot(
    id: String,
    kind: &str,
    params: Value,
    _workspace: &str,
) -> PermissionRequest {
    // Notifications wrap the same request that older RPC calls sent directly.
    let params = params.get("permissionRequest").cloned().unwrap_or(params);
    let kind = params.get("kind").and_then(Value::as_str).unwrap_or(kind);
    let encoded = params.to_string();
    let target_list = match kind {
        "shell" => vec![PermissionTarget::Command {
            command: params
                .get("fullCommandText")
                .or_else(|| params.get("command"))
                .or_else(|| params.get("fullCommand"))
                .or_else(|| params.get("full_command"))
                .and_then(Value::as_str)
                .unwrap_or(&encoded)
                .to_owned(),
            shell: provider_shell(&params),
        }],
        "write" => vec![provider_path(&params).map_or(
            PermissionTarget::UnknownFile {
                operation: DirectoryOperation::Write,
            },
            |path| PermissionTarget::Write { path },
        )],
        "read" => vec![provider_path(&params).map_or(
            PermissionTarget::UnknownFile {
                operation: DirectoryOperation::Read,
            },
            |path| PermissionTarget::Read { path },
        )],
        "url" => vec![PermissionTarget::Network {
            target: params
                .get("url")
                .or_else(|| params.get("target"))
                .and_then(Value::as_str)
                .unwrap_or("*")
                .to_owned(),
        }],
        "mcp" => vec![PermissionTarget::Mcp {
            target: {
                let tool = params
                    .get("toolName")
                    .or_else(|| params.get("tool_name"))
                    .or_else(|| params.get("name"))
                    .and_then(Value::as_str)
                    .unwrap_or("*");
                params
                    .get("serverName")
                    .and_then(Value::as_str)
                    .map_or_else(|| tool.to_owned(), |server| format!("{server}/{tool}"))
            },
        }],
        "custom-tool" | "hook" => vec![PermissionTarget::Tool {
            target: params
                .get("toolName")
                .and_then(Value::as_str)
                .unwrap_or(kind)
                .to_owned(),
        }],
        _ => normalize_permission_payload(&params, &encoded),
    };
    PermissionRequest {
        id,
        provider: "Copilot CLI".into(),
        reason: params
            .get("reason")
            .or_else(|| params.get("intention"))
            .or_else(|| params.get("warning"))
            .or_else(|| params.get("hookMessage"))
            .or_else(|| params.get("toolDescription"))
            .and_then(Value::as_str)
            .map(str::to_owned),
        target_list,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn callback_paths_and_operation_are_preserved_without_title_inference() {
        for operation in ["read", "write"] {
            let request = permission_from_copilot(
                "request".into(),
                operation,
                json!({"permissionRequest":{"kind":operation,"fileName":"folder/file.rs"}}),
                ".",
            );
            let expected = if operation == "write" {
                PermissionTarget::Write {
                    path: "folder/file.rs".into(),
                }
            } else {
                PermissionTarget::Read {
                    path: "folder/file.rs".into(),
                }
            };
            assert_eq!(request.target_list, vec![expected]);
        }
        let request = permission_from_copilot(
            "request".into(),
            "write",
            json!({"title":"edit apparently-allowed.rs"}),
            ".",
        );
        assert!(matches!(
            request.target_list[0],
            PermissionTarget::UnknownFile { .. }
        ));
    }
}

use async_trait::async_trait;
use forge_provider_api::permissions::{PermissionResponse, ProviderPermissions};
use github_copilot_sdk::handler::{PermissionHandler, PermissionResult};
use github_copilot_sdk::hooks::{HookContext, PreToolUseInput, PreToolUseOutput, SessionHooks};
use github_copilot_sdk::{PermissionRequestData, PermissionRequestKind, RequestId, SessionId};
use std::sync::Arc;

/// Bind native permission callbacks to the host's authorization policy.
pub fn handler(context: Arc<dyn ProviderPermissions>) -> Arc<dyn PermissionHandler> {
    Arc::new(Callback { context })
}

/// Bind native pre-tool hooks to the host's checkpoint barrier.
pub fn checkpoint_hooks(context: Arc<dyn ProviderPermissions>) -> Arc<dyn SessionHooks> {
    Arc::new(Callback { context })
}

struct Callback {
    context: Arc<dyn ProviderPermissions>,
}

#[async_trait]
impl SessionHooks for Callback {
    async fn on_pre_tool_use(
        &self,
        _input: PreToolUseInput,
        _context: HookContext,
    ) -> Option<PreToolUseOutput> {
        self.context
            .before_tool()
            .await
            .err()
            .map(|error| PreToolUseOutput {
                permission_decision: Some("deny".into()),
                permission_decision_reason: Some(format!("{error:#}")),
                ..Default::default()
            })
    }
}

#[async_trait]
impl PermissionHandler for Callback {
    async fn handle(
        &self,
        session_id: SessionId,
        request_id: RequestId,
        data: PermissionRequestData,
    ) -> PermissionResult {
        let kind = permission_kind(data.kind);
        let params = serde_json::to_value(data).unwrap_or(Value::Null);
        let request = permission_from_copilot(request_id.to_string(), kind, params, "");
        match self.context.authorize(session_id.as_ref(), request).await {
            PermissionResponse::Allow => PermissionResult::approve_once(),
            PermissionResponse::Deny { reason } => PermissionResult::reject(reason),
            PermissionResponse::Cancelled => PermissionResult::reject(None),
        }
    }
}
fn permission_kind(kind: Option<PermissionRequestKind>) -> &'static str {
    match kind {
        Some(PermissionRequestKind::Shell) => "shell",
        Some(PermissionRequestKind::Write) => "write",
        Some(PermissionRequestKind::Read) => "read",
        Some(PermissionRequestKind::Url) => "url",
        Some(PermissionRequestKind::Mcp) => "mcp",
        Some(PermissionRequestKind::CustomTool) => "custom-tool",
        Some(PermissionRequestKind::Memory) => "memory",
        Some(PermissionRequestKind::Hook) => "hook",
        Some(PermissionRequestKind::Unknown) | None => "unknown",
        _ => "unknown",
    }
}
