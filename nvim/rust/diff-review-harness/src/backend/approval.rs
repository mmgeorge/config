use crate::backend::{BackendEvent, BackendEventSink};
use crate::permissions::command::{
    broad_command_pattern, exact_command_pattern, normalize_command,
};
use crate::permissions::document::{
    CATEGORY_BASH, CATEGORY_EDIT, CATEGORY_ELEVATE, CATEGORY_MCP, CATEGORY_READ, CATEGORY_TOOL,
    CATEGORY_WEBFETCH, PermissionDecision,
};
use crate::permissions::matcher::{PermissionRequest, PermissionTarget};
use crate::permissions::store::PermissionStore;
use crate::session::ExecutionMode;
use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use serde_json::{Value, json};
use std::collections::BTreeMap;
use std::path::Path;
use std::sync::{Arc, Mutex, RwLock};
use tokio::sync::oneshot;
use uuid::Uuid;

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum ApprovalResolution {
    AllowOnce,
    AllowExact,
    AllowBroad,
    DenyOnce,
    DenyExact,
    DenyBroad,
    Cancel,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ApprovalChoice {
    pub id: String,
    pub label: String,
    pub resolution: ApprovalResolution,
    #[serde(default)]
    pub rule_list: Vec<(String, String)>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ApprovalRequestView {
    pub id: String,
    pub provider: String,
    pub title: String,
    pub detail: String,
    pub reason: Option<String>,
    pub choice_list: Vec<ApprovalChoice>,
}

struct PendingApproval {
    view: ApprovalRequestView,
    response: oneshot::Sender<ApprovalResolution>,
}

pub struct PermissionCoordinator {
    pending_map: Mutex<BTreeMap<String, PendingApproval>>,
    store: Arc<RwLock<PermissionStore>>,
}

impl PermissionCoordinator {
    pub fn new(store: Arc<RwLock<PermissionStore>>) -> Self {
        Self {
            pending_map: Mutex::new(BTreeMap::new()),
            store,
        }
    }

    pub fn store(&self) -> Arc<RwLock<PermissionStore>> {
        Arc::clone(&self.store)
    }

    pub fn transient(workspace: impl AsRef<Path>) -> Result<Arc<Self>> {
        let directory = std::env::temp_dir()
            .join("diff-review-harness")
            .join(Uuid::new_v4().to_string());
        let store = PermissionStore::load(directory.join("permissions.json"), workspace.as_ref())?;
        Ok(Arc::new(Self::new(Arc::new(RwLock::new(store)))))
    }

    pub async fn authorize(
        &self,
        execution_mode: ExecutionMode,
        request: PermissionRequest,
        event_sink: Option<&BackendEventSink>,
    ) -> Result<ApprovalResolution> {
        if execution_mode == ExecutionMode::Yolo {
            return Ok(ApprovalResolution::AllowOnce);
        }
        let evaluation = self
            .store
            .read()
            .map_err(|_| anyhow::anyhow!("permission store lock poisoned"))?
            .compiled()
            .evaluate(execution_mode, &request);
        match evaluation.decision {
            PermissionDecision::Allow => return Ok(ApprovalResolution::AllowOnce),
            PermissionDecision::Deny => return Ok(ApprovalResolution::DenyOnce),
            PermissionDecision::Ask => {}
        }
        let id = request.id.clone();
        let view = approval_view(&request);
        let (sender, receiver) = oneshot::channel();
        self.pending_map
            .lock()
            .map_err(|_| anyhow::anyhow!("approval registry lock poisoned"))?
            .insert(
                id.clone(),
                PendingApproval {
                    view: view.clone(),
                    response: sender,
                },
            );
        if let Some(event_sink) = event_sink {
            let _ = event_sink.send(BackendEvent {
                kind: "approval_requested".into(),
                text: None,
                data: serde_json::to_value(&view)?,
                activity: None,
                summary: None,
                task_update: None,
            });
        }
        receiver
            .await
            .with_context(|| format!("approval request {id} was cancelled"))
    }

    pub fn resolve(
        &self,
        approval_id: &str,
        choice_id: &str,
        event_sink: Option<&BackendEventSink>,
    ) -> Result<ApprovalRequestView> {
        let mut pending_map = self
            .pending_map
            .lock()
            .map_err(|_| anyhow::anyhow!("approval registry lock poisoned"))?;
        let pending = pending_map
            .get(approval_id)
            .with_context(|| format!("approval request is no longer pending: {approval_id}"))?;
        let choice = pending
            .view
            .choice_list
            .iter()
            .find(|choice| choice.id == choice_id)
            .cloned()
            .with_context(|| format!("unknown approval choice: {choice_id}"))?;
        anyhow::ensure!(
            !pending.response.is_closed(),
            "provider no longer waits for approval {approval_id}"
        );
        let pending = pending_map
            .remove(approval_id)
            .expect("validated approval must remain pending while locked");
        drop(pending_map);
        let persistent_decision = match choice.resolution {
            ApprovalResolution::AllowExact | ApprovalResolution::AllowBroad => {
                Some(PermissionDecision::Allow)
            }
            ApprovalResolution::DenyExact | ApprovalResolution::DenyBroad => {
                Some(PermissionDecision::Deny)
            }
            _ => None,
        };
        if let Some(decision) = persistent_decision {
            if let Err(error) = self
                .store
                .write()
                .map_err(|_| anyhow::anyhow!("permission store lock poisoned"))?
                .set_rule_list(&choice.rule_list, decision)
            {
                Self::emit_lifecycle(event_sink, "approval_cancelled", &pending.view)?;
                return Err(error);
            }
        }
        if pending.response.send(choice.resolution).is_err() {
            Self::emit_lifecycle(event_sink, "approval_cancelled", &pending.view)?;
            anyhow::bail!("provider no longer waits for approval {approval_id}");
        }
        Self::emit_lifecycle(event_sink, "approval_resolved", &pending.view)?;
        Ok(pending.view)
    }

    pub fn pending_list(&self) -> Result<Vec<ApprovalRequestView>> {
        Ok(self
            .pending_map
            .lock()
            .map_err(|_| anyhow::anyhow!("approval registry lock poisoned"))?
            .values()
            .map(|pending| pending.view.clone())
            .collect())
    }

    pub fn cancel_all(&self, event_sink: Option<&BackendEventSink>) -> Result<()> {
        let pending_list = {
            let mut pending_map = self
                .pending_map
                .lock()
                .map_err(|_| anyhow::anyhow!("approval registry lock poisoned"))?;
            std::mem::take(&mut *pending_map)
                .into_values()
                .collect::<Vec<_>>()
        };
        for pending in pending_list {
            let _ = pending.response.send(ApprovalResolution::Cancel);
            Self::emit_lifecycle(event_sink, "approval_cancelled", &pending.view)?;
        }
        Ok(())
    }

    fn emit_lifecycle(
        event_sink: Option<&BackendEventSink>,
        kind: &str,
        view: &ApprovalRequestView,
    ) -> Result<()> {
        if let Some(event_sink) = event_sink {
            let _ = event_sink.send(BackendEvent {
                kind: kind.into(),
                text: None,
                data: serde_json::to_value(view)?,
                activity: None,
                summary: None,
                task_update: None,
            });
        }
        Ok(())
    }
}

fn target_rule_list(request: &PermissionRequest, broad: bool) -> Vec<(String, String)> {
    let mut rule_list = Vec::new();
    for target in &request.target_list {
        match target {
            PermissionTarget::Command { command } => {
                let normalization = normalize_command(command);
                for invocation in &normalization.invocation_list {
                    let pattern = if broad {
                        broad_command_pattern(invocation)
                    } else {
                        exact_command_pattern(invocation)
                    };
                    if let Some(pattern) = pattern {
                        rule_list.push((CATEGORY_BASH.into(), pattern));
                    }
                }
            }
            PermissionTarget::Read { path } => rule_list.push((
                CATEGORY_READ.into(),
                if broad { ".".into() } else { path.clone() },
            )),
            PermissionTarget::Write { path } => rule_list.push((
                CATEGORY_EDIT.into(),
                if broad { ".".into() } else { path.clone() },
            )),
            PermissionTarget::Network { target } => rule_list.push((
                CATEGORY_WEBFETCH.into(),
                if broad { "*".into() } else { target.clone() },
            )),
            PermissionTarget::Mcp { target } => rule_list.push((
                CATEGORY_MCP.into(),
                if broad {
                    target
                        .split('/')
                        .next()
                        .map_or_else(|| "*".into(), |server| format!("{server}/*"))
                } else {
                    target.clone()
                },
            )),
            PermissionTarget::Elevate { target } => rule_list.push((
                CATEGORY_ELEVATE.into(),
                if broad { "*".into() } else { target.clone() },
            )),
            PermissionTarget::Tool { target } => rule_list.push((
                CATEGORY_TOOL.into(),
                if broad { "*".into() } else { target.clone() },
            )),
        }
    }
    rule_list.sort();
    rule_list.dedup();
    rule_list
}

fn display_rule_list(rule_list: &[(String, String)]) -> String {
    rule_list
        .iter()
        .map(|(category, pattern)| format!("{category} {pattern}"))
        .collect::<Vec<_>>()
        .join(", ")
}

fn approval_view(request: &PermissionRequest) -> ApprovalRequestView {
    let (title, detail) = request.target_list.first().map_or_else(
        || ("Provider permission".into(), String::new()),
        |target| match target {
            PermissionTarget::Command { command } => ("Run command".into(), command.clone()),
            PermissionTarget::Read { path } => ("Read path".into(), path.clone()),
            PermissionTarget::Write { path } => ("Write path".into(), path.clone()),
            PermissionTarget::Network { target } => ("Access network".into(), target.clone()),
            PermissionTarget::Mcp { target } => ("Call MCP tool".into(), target.clone()),
            PermissionTarget::Elevate { target } => ("Elevate process".into(), target.clone()),
            PermissionTarget::Tool { target } => ("Call provider tool".into(), target.clone()),
        },
    );
    let exact_rule_list = target_rule_list(request, false);
    let broad_rule_list = target_rule_list(request, true);
    ApprovalRequestView {
        id: request.id.clone(),
        provider: request.provider.clone(),
        title,
        detail,
        reason: request.reason.clone(),
        choice_list: vec![
            ApprovalChoice {
                id: "allow_once".into(),
                label: "Allow once".into(),
                resolution: ApprovalResolution::AllowOnce,
                rule_list: Vec::new(),
            },
            ApprovalChoice {
                id: "allow_exact".into(),
                label: format!(
                    "Always allow exact: {}",
                    display_rule_list(&exact_rule_list)
                ),
                resolution: ApprovalResolution::AllowExact,
                rule_list: exact_rule_list.clone(),
            },
            ApprovalChoice {
                id: "allow_broad".into(),
                label: format!(
                    "Always allow broad: {}",
                    display_rule_list(&broad_rule_list)
                ),
                resolution: ApprovalResolution::AllowBroad,
                rule_list: broad_rule_list.clone(),
            },
            ApprovalChoice {
                id: "deny_once".into(),
                label: "Deny once".into(),
                resolution: ApprovalResolution::DenyOnce,
                rule_list: Vec::new(),
            },
            ApprovalChoice {
                id: "deny_exact".into(),
                label: format!("Always deny exact: {}", display_rule_list(&exact_rule_list)),
                resolution: ApprovalResolution::DenyExact,
                rule_list: exact_rule_list,
            },
            ApprovalChoice {
                id: "deny_broad".into(),
                label: format!("Always deny broad: {}", display_rule_list(&broad_rule_list)),
                resolution: ApprovalResolution::DenyBroad,
                rule_list: broad_rule_list,
            },
            ApprovalChoice {
                id: "cancel".into(),
                label: "Cancel request".into(),
                resolution: ApprovalResolution::Cancel,
                rule_list: Vec::new(),
            },
        ],
    }
}

pub fn permission_from_provider(
    method: &str,
    message: &Value,
    workspace: &str,
) -> PermissionRequest {
    let params = message.get("params").unwrap_or(&Value::Null);
    let id = format!("approval-{}", Uuid::new_v4());
    let provider = if method.starts_with("session/")
        || method.starts_with("fs/")
        || method.starts_with("terminal/")
    {
        "ACP"
    } else {
        "Codex CLI"
    };
    let reason = params
        .get("reason")
        .and_then(Value::as_str)
        .map(str::to_owned);
    let encoded = params.to_string();
    let target_list = if method == "mcpServer/elicitation/request" {
        let server = params
            .get("serverName")
            .and_then(Value::as_str)
            .unwrap_or("unknown");
        let tool = params
            .pointer("/meta/toolName")
            .or_else(|| params.pointer("/meta/tool_name"))
            .and_then(Value::as_str)
            .unwrap_or("*");
        vec![PermissionTarget::Mcp {
            target: format!("{server}/{tool}"),
        }]
    } else if method.contains("commandExecution") || method == "terminal/create" {
        let command = params
            .get("command")
            .and_then(Value::as_str)
            .map(str::to_owned)
            .or_else(|| {
                params
                    .get("args")
                    .and_then(Value::as_array)
                    .map(|argument_list| {
                        argument_list
                            .iter()
                            .filter_map(Value::as_str)
                            .collect::<Vec<_>>()
                            .join(" ")
                    })
            })
            .unwrap_or_else(|| encoded.clone());
        vec![PermissionTarget::Command { command }]
    } else if method.contains("fileChange") || method == "fs/write_text_file" {
        let path = provider_path(params).unwrap_or_else(|| workspace.into());
        vec![PermissionTarget::Write { path }]
    } else if method == "fs/read_text_file" {
        let path = provider_path(params).unwrap_or_else(|| workspace.into());
        vec![PermissionTarget::Read { path }]
    } else if method.contains("permission") || method.contains("permissions") {
        normalize_permission_payload(params, &encoded)
    } else {
        vec![PermissionTarget::Tool {
            target: method.into(),
        }]
    };
    PermissionRequest {
        id,
        provider: provider.into(),
        reason,
        target_list,
    }
}

fn provider_path(params: &Value) -> Option<String> {
    params
        .get("path")
        .and_then(Value::as_str)
        .or_else(|| params.get("grantRoot").and_then(Value::as_str))
        .or_else(|| params.pointer("/file/path").and_then(Value::as_str))
        .map(str::to_owned)
}

fn normalize_permission_payload(params: &Value, encoded: &str) -> Vec<PermissionTarget> {
    if let Some(tool_call) = params.get("toolCall") {
        let kind = tool_call
            .get("kind")
            .and_then(Value::as_str)
            .unwrap_or("tool");
        let title = tool_call
            .get("title")
            .and_then(Value::as_str)
            .unwrap_or(kind);
        return match kind {
            "read" | "search" => vec![PermissionTarget::Read { path: title.into() }],
            "edit" | "delete" | "move" => vec![PermissionTarget::Write { path: title.into() }],
            "execute" => vec![PermissionTarget::Command {
                command: tool_call
                    .get("rawInput")
                    .map(Value::to_string)
                    .unwrap_or_else(|| title.into()),
            }],
            "fetch" => vec![PermissionTarget::Network {
                target: title.into(),
            }],
            "mcp" => vec![PermissionTarget::Mcp {
                target: title.into(),
            }],
            _ => vec![PermissionTarget::Tool {
                target: format!("{kind}/{title}"),
            }],
        };
    }
    let mut target_list = Vec::new();
    for (namespace, pointer) in [
        ("read", "/permissions/fileSystem/read"),
        ("write", "/permissions/fileSystem/write"),
    ] {
        if let Some(path_list) = params.pointer(pointer).and_then(Value::as_array) {
            for path in path_list.iter().filter_map(Value::as_str) {
                target_list.push(if namespace == "read" {
                    PermissionTarget::Read { path: path.into() }
                } else {
                    PermissionTarget::Write { path: path.into() }
                });
            }
        }
    }
    let network = params.pointer("/permissions/network");
    if network.is_some_and(|value| !value.is_null())
        || encoded.to_ascii_lowercase().contains("network")
    {
        target_list.push(PermissionTarget::Network {
            target: network
                .and_then(|value| value.get("host"))
                .and_then(Value::as_str)
                .unwrap_or("*")
                .into(),
        });
    }
    if target_list.is_empty() {
        target_list.push(PermissionTarget::Tool {
            target: "provider/permission".into(),
        });
    }
    target_list
}

pub fn acp_response(message: &Value, resolution: ApprovalResolution) -> Value {
    if resolution == ApprovalResolution::Cancel {
        return json!({ "outcome": { "outcome": "cancelled" } });
    }
    let allow = matches!(
        resolution,
        ApprovalResolution::AllowOnce
            | ApprovalResolution::AllowExact
            | ApprovalResolution::AllowBroad
    );
    let desired = if allow { "allow_once" } else { "reject_once" };
    let option_id = message
        .pointer("/params/options")
        .and_then(Value::as_array)
        .and_then(|option_list| {
            option_list
                .iter()
                .find(|option| option.get("kind").and_then(Value::as_str) == Some(desired))
                .and_then(|option| option.get("optionId").or_else(|| option.get("option_id")))
                .cloned()
        });
    option_id.map_or_else(
        || json!({ "outcome": { "outcome": "cancelled" } }),
        |option_id| json!({ "outcome": { "outcome": "selected", "optionId": option_id } }),
    )
}

pub fn codex_response(message: &Value, resolution: ApprovalResolution) -> Value {
    let allow = matches!(
        resolution,
        ApprovalResolution::AllowOnce
            | ApprovalResolution::AllowExact
            | ApprovalResolution::AllowBroad
    );
    if message.get("method").and_then(Value::as_str) == Some("mcpServer/elicitation/request") {
        return json!({
            "action": if allow { "accept" } else if resolution == ApprovalResolution::Cancel { "cancel" } else { "decline" },
            "content": if allow { json!({}) } else { Value::Null },
        });
    }
    if message.get("method").and_then(Value::as_str) == Some("item/permissions/requestApproval") {
        return if allow {
            json!({ "permissions": message.pointer("/params/permissions").cloned().unwrap_or(Value::Null), "scope": "turn" })
        } else {
            json!({ "permissions": {}, "scope": "turn" })
        };
    }
    json!({ "decision": match resolution {
        ApprovalResolution::AllowOnce | ApprovalResolution::AllowExact | ApprovalResolution::AllowBroad => "accept",
        ApprovalResolution::DenyOnce | ApprovalResolution::DenyExact | ApprovalResolution::DenyBroad => "decline",
        ApprovalResolution::Cancel => "cancel",
    }})
}

pub fn protected_target(request: &PermissionRequest, store: &PermissionStore) -> bool {
    request.target_list.iter().any(|target| match target {
        PermissionTarget::Write { path } => store.protects(Path::new(path)),
        PermissionTarget::Command { command } => store.protects_command(command),
        _ => false,
    })
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::permissions::store::PermissionStore;

    #[test]
    fn builds_exact_and_broad_command_choices() {
        let request = PermissionRequest {
            id: "one".into(),
            provider: "test".into(),
            reason: None,
            target_list: vec![PermissionTarget::Command {
                command: "git status --short".into(),
            }],
        };
        let view = approval_view(&request);
        assert!(
            view.choice_list[1]
                .rule_list
                .contains(&("bash".into(), "git status --short".into()))
        );
        assert!(
            view.choice_list[2]
                .rule_list
                .contains(&("bash".into(), "git *".into()))
        );
    }

    #[tokio::test]
    async fn pauses_asked_permissions_and_persists_the_selected_rule() {
        let directory = tempfile::tempdir().unwrap();
        let store = Arc::new(RwLock::new(
            PermissionStore::load(directory.path().join("permissions.json"), "D:/repo").unwrap(),
        ));
        let coordinator = Arc::new(PermissionCoordinator::new(Arc::clone(&store)));
        let request = PermissionRequest {
            id: "approval-one".into(),
            provider: "test".into(),
            reason: Some("inspect repository".into()),
            target_list: vec![PermissionTarget::Command {
                command: "rg TODO src".into(),
            }],
        };
        let (event_sink, mut event_stream) = tokio::sync::mpsc::unbounded_channel();
        let waiting_event_sink = event_sink.clone();
        let waiting_coordinator = Arc::clone(&coordinator);
        let waiting = tokio::spawn(async move {
            waiting_coordinator
                .authorize(ExecutionMode::Read, request, Some(&waiting_event_sink))
                .await
                .unwrap()
        });
        let event = event_stream.recv().await.unwrap();
        assert_eq!(event.kind, "approval_requested");
        coordinator
            .resolve("approval-one", "allow_broad", Some(&event_sink))
            .unwrap();
        assert_eq!(waiting.await.unwrap(), ApprovalResolution::AllowBroad);
        let resolved = event_stream.recv().await.unwrap();
        assert_eq!(resolved.kind, "approval_resolved");
        let source = store.read().unwrap().open().1;
        assert!(source.contains("\"rg *\": \"allow\""));
    }

    #[tokio::test]
    async fn persists_an_always_deny_choice_in_the_shared_document() {
        let directory = tempfile::tempdir().unwrap();
        let store = Arc::new(RwLock::new(
            PermissionStore::load(directory.path().join("permissions.json"), "D:/repo").unwrap(),
        ));
        let coordinator = Arc::new(PermissionCoordinator::new(Arc::clone(&store)));
        let (event_sink, mut event_stream) = tokio::sync::mpsc::unbounded_channel();
        let waiting_event_sink = event_sink.clone();
        let waiting_coordinator = Arc::clone(&coordinator);
        let waiting = tokio::spawn(async move {
            waiting_coordinator
                .authorize(
                    ExecutionMode::Full,
                    PermissionRequest {
                        id: "approval-deny".into(),
                        provider: "test".into(),
                        reason: None,
                        target_list: vec![PermissionTarget::Command {
                            command: "rg TODO src".into(),
                        }],
                    },
                    Some(&waiting_event_sink),
                )
                .await
                .unwrap()
        });
        assert_eq!(
            event_stream.recv().await.unwrap().kind,
            "approval_requested"
        );
        coordinator
            .resolve("approval-deny", "deny_broad", Some(&event_sink))
            .unwrap();
        assert_eq!(waiting.await.unwrap(), ApprovalResolution::DenyBroad);
        assert_eq!(
            store.read().unwrap().compiled().document.permission["bash"]["rg *"],
            PermissionDecision::Deny
        );
    }

    #[tokio::test]
    async fn cancellation_clears_every_pending_approval_and_emits_lifecycle_events() {
        let coordinator = PermissionCoordinator::transient("D:/repo").unwrap();
        let (event_sink, mut event_stream) = tokio::sync::mpsc::unbounded_channel();
        let waiting_event_sink = event_sink.clone();
        let waiting_coordinator = Arc::clone(&coordinator);
        let waiting = tokio::spawn(async move {
            waiting_coordinator
                .authorize(
                    ExecutionMode::Read,
                    PermissionRequest {
                        id: "approval-cancel".into(),
                        provider: "test".into(),
                        reason: None,
                        target_list: vec![PermissionTarget::Command {
                            command: "rg TODO".into(),
                        }],
                    },
                    Some(&waiting_event_sink),
                )
                .await
                .unwrap()
        });
        assert_eq!(
            event_stream.recv().await.unwrap().kind,
            "approval_requested"
        );
        coordinator.cancel_all(Some(&event_sink)).unwrap();
        assert_eq!(
            event_stream.recv().await.unwrap().kind,
            "approval_cancelled"
        );
        assert_eq!(waiting.await.unwrap(), ApprovalResolution::Cancel);
        assert!(coordinator.pending_list().unwrap().is_empty());
    }
}
