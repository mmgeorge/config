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
    let target_list = match kind {
        "shell" => vec![
            params
                .get("fullCommandText")
                .or_else(|| params.get("command"))
                .or_else(|| params.get("fullCommand"))
                .or_else(|| params.get("full_command"))
                .and_then(Value::as_str)
                .filter(|command| !command.trim().is_empty())
                .map_or(PermissionTarget::Unknown, |command| {
                    PermissionTarget::Command {
                        command: command.to_owned(),
                        shell: provider_shell(&params),
                    }
                }),
        ],
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
        "url" => vec![
            params
                .get("url")
                .or_else(|| params.get("target"))
                .and_then(Value::as_str)
                .filter(|target| !target.trim().is_empty())
                .map_or(PermissionTarget::Unknown, |target| {
                    PermissionTarget::Network {
                        target: target.to_owned(),
                    }
                }),
        ],
        "mcp" => vec![match (
            params.get("serverName").and_then(Value::as_str),
            params
                .get("toolName")
                .or_else(|| params.get("tool_name"))
                .or_else(|| params.get("name"))
                .and_then(Value::as_str),
        ) {
            (Some(server), Some(tool)) if !server.trim().is_empty() && !tool.trim().is_empty() => {
                PermissionTarget::Mcp {
                    target: format!("{server}/{tool}"),
                }
            }
            _ => PermissionTarget::Unknown,
        }],
        "custom-tool" | "hook" => vec![
            params
                .get("toolName")
                .and_then(Value::as_str)
                .filter(|name| !name.trim().is_empty())
                .map_or(PermissionTarget::Unknown, |name| PermissionTarget::Tool {
                    target: name.to_owned(),
                }),
        ],
        _ => normalize_permission_payload(&params),
    };
    PermissionRequest {
        id,
        provider: forge_provider_api::ProviderId::new("copilot")
            .expect("registered Copilot identity"),
        reason: None,
        target_list,
    }
}

use async_trait::async_trait;
use forge_provider_api::permissions::PermissionResponse;
use github_copilot_sdk::handler::{PermissionHandler, PermissionResult};
use github_copilot_sdk::hooks::{HookContext, PreToolUseInput, PreToolUseOutput, SessionHooks};
use github_copilot_sdk::{PermissionRequestData, PermissionRequestKind, RequestId, SessionId};
use std::sync::Arc;

/// Bind native permission callbacks to the host's authorization policy.
pub fn handler(
    context: Arc<dyn super::provider::NativeCallbackRouter>,
) -> Arc<dyn PermissionHandler> {
    Arc::new(Callback { context })
}

/// Keep a helper permission wait interruptible after its main invocation is cancelled or closed.
pub(super) fn shell_handler(
    context: Arc<dyn super::provider::NativeCallbackRouter>,
    cancellation: tokio::sync::watch::Receiver<()>,
) -> Arc<dyn PermissionHandler> {
    Arc::new(ShellPermission {
        callback: Callback { context },
        cancellation,
    })
}

struct ShellPermission {
    callback: Callback,
    cancellation: tokio::sync::watch::Receiver<()>,
}

#[async_trait]
impl PermissionHandler for ShellPermission {
    /// Reject cancellation while retaining the native request's exact tool identity.
    async fn handle(
        &self,
        session_id: SessionId,
        request_id: RequestId,
        data: PermissionRequestData,
    ) -> PermissionResult {
        let mut cancellation = self.cancellation.clone();
        cancellation.borrow_and_update();
        tokio::select! {
            biased;
            _ = cancellation.changed() => PermissionResult::reject(None),
            result = self.callback.handle(session_id, request_id, data) => result,
        }
    }
}

/// Bind native pre-tool hooks to the host's checkpoint barrier.
pub fn checkpoint_hooks(
    context: Arc<dyn super::provider::NativeCallbackRouter>,
) -> Arc<dyn SessionHooks> {
    Arc::new(Callback { context })
}

struct Callback {
    context: Arc<dyn super::provider::NativeCallbackRouter>,
}

#[async_trait]
impl SessionHooks for Callback {
    async fn on_pre_tool_use(
        &self,
        _input: PreToolUseInput,
        _context: HookContext,
    ) -> Option<PreToolUseOutput> {
        match self.context.before_tool_native(None).await {
            Ok(()) => None,
            Err(_) => Some(PreToolUseOutput {
                permission_decision: Some("deny".into()),
                permission_decision_reason: Some(
                    "Host pre-execution barrier rejected this operation".into(),
                ),
                modified_args: None,
                additional_context: None,
                suppress_output: None,
            }),
        }
    }
}

#[async_trait]
impl PermissionHandler for Callback {
    async fn handle(
        &self,
        _session_id: SessionId,
        request_id: RequestId,
        data: PermissionRequestData,
    ) -> PermissionResult {
        let kind = permission_kind(data.kind);
        let tool_call_id = data.tool_call_id.clone();
        let params = serde_json::to_value(data).unwrap_or(Value::Null);
        let request = permission_from_copilot(request_id.to_string(), kind, params, "");
        let Some(tool_call_id) = tool_call_id.as_deref() else {
            return PermissionResult::reject(None);
        };
        if self
            .context
            .before_tool_native(Some(tool_call_id))
            .await
            .is_err()
        {
            return PermissionResult::reject(None);
        }
        let response = self.context.authorize_native(tool_call_id, request).await;
        if self
            .context
            .before_shell_native(tool_call_id)
            .await
            .is_err()
        {
            return PermissionResult::reject(None);
        }
        match response {
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

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    struct PermissionPolicy {
        response: PermissionResponse,
        barrier_open: bool,
    }

    #[async_trait]
    impl super::super::provider::NativeCallbackRouter for PermissionPolicy {
        /// Verify the native callback's identity and normalized request at the host boundary.
        async fn authorize_native(
            &self,
            _call_id: &str,
            request: PermissionRequest,
        ) -> PermissionResponse {
            assert!(
                self.barrier_open,
                "closed barrier must prevent authorization"
            );
            assert_eq!(request.id, "request");
            assert_eq!(
                request.target_list,
                vec![PermissionTarget::Write {
                    path: "file.rs".into()
                }]
            );
            self.response.clone()
        }

        async fn call_native(
            &self,
            _: &str,
            _: forge_provider_api::control::ToolCall,
        ) -> Result<String, forge_provider_api::ProviderError> {
            Err(forge_provider_api::ProviderError::Cancelled)
        }

        async fn before_tool_native(
            &self,
            _: Option<&str>,
        ) -> Result<(), forge_provider_api::ProviderError> {
            if self.barrier_open {
                Ok(())
            } else {
                Err(forge_provider_api::ProviderError::Cancelled)
            }
        }

        async fn bind_native_call(
            &self,
            _: &str,
            _: &forge_provider_api::InvocationScope,
        ) -> Result<(), forge_provider_api::ProviderError> {
            Ok(())
        }

        async fn release_native_call(&self, _: &str) {}
    }

    /// Preserve host authorization and denial feedback in the SDK response.
    #[tokio::test]
    async fn callback_maps_host_decisions_without_losing_feedback() {
        for (response, expected) in [
            (PermissionResponse::Allow, json!({"kind":"approve-once"})),
            (
                PermissionResponse::Deny {
                    reason: Some("Policy denied this write.".into()),
                },
                json!({"kind":"reject","feedback":"Policy denied this write."}),
            ),
            (
                PermissionResponse::Deny { reason: None },
                json!({"kind":"reject"}),
            ),
            (PermissionResponse::Cancelled, json!({"kind":"reject"})),
        ] {
            let callback = handler(Arc::new(PermissionPolicy {
                response,
                barrier_open: true,
            }));
            let result = callback
                .handle(
                    SessionId::new("native"),
                    RequestId::new("request"),
                    serde_json::from_value(
                        json!({"kind":"write","path":"file.rs","toolCallId":"tool"}),
                    )
                    .unwrap(),
                )
                .await;
            let PermissionResult::Decision { decision, .. } = result else {
                panic!("native callback omitted its decision");
            };
            assert_eq!(serde_json::to_value(decision).unwrap(), expected);
        }
    }

    /// Enforce the host barrier for tools that do not require an authorization prompt.
    #[tokio::test]
    async fn pre_tool_hook_enforces_host_barrier() {
        for barrier_open in [true, false] {
            let callback = checkpoint_hooks(Arc::new(PermissionPolicy {
                response: PermissionResponse::Allow,
                barrier_open,
            }));
            let result = callback
                .on_pre_tool_use(
                    serde_json::from_value(json!({
                        "sessionId":"native", "timestamp":0.0, "cwd":".",
                        "toolName":"read", "toolArgs":{}
                    }))
                    .unwrap(),
                    HookContext {
                        session_id: SessionId::new("native"),
                    },
                )
                .await;
            if barrier_open {
                assert!(result.is_none());
            } else {
                assert_eq!(result.unwrap().permission_decision.as_deref(), Some("deny"));
            }
        }
    }

    /// Do not approve a permission callback after its invocation barrier has closed.
    #[tokio::test]
    async fn permission_callback_rejects_closed_barrier() {
        let callback = handler(Arc::new(PermissionPolicy {
            response: PermissionResponse::Allow,
            barrier_open: false,
        }));
        let result = callback
            .handle(
                SessionId::new("native"),
                RequestId::new("request"),
                serde_json::from_value(
                    json!({"kind":"write","path":"file.rs","toolCallId":"tool"}),
                )
                .unwrap(),
            )
            .await;
        let PermissionResult::Decision { decision, .. } = result else {
            panic!("native callback omitted its decision");
        };
        assert_eq!(
            serde_json::to_value(decision).unwrap(),
            json!({"kind":"reject"})
        );
    }

    /// Reject incomplete native requests without manufacturing broad permission targets.
    #[test]
    fn incomplete_permission_targets_are_unknown() {
        for (kind, payload) in [
            ("url", json!({})),
            ("url", json!({"url":" "})),
            ("mcp", json!({"toolName":"read"})),
            ("mcp", json!({"serverName":"server"})),
            ("mcp", json!({"serverName":" ","toolName":"read"})),
        ] {
            let request = permission_from_copilot("request".into(), kind, payload, ".");
            assert_eq!(request.target_list, vec![PermissionTarget::Unknown]);
        }
        for kind in ["read", "write"] {
            let request = permission_from_copilot("request".into(), kind, json!({"path":" "}), ".");
            assert!(matches!(
                request.target_list[0],
                PermissionTarget::UnknownFile { .. }
            ));
        }
    }

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
