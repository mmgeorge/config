use crate::backend::json_rpc::JsonRpcProcess;
use crate::backend::{
    Backend, BackendCapability, BackendEventSink, BackendModel, BackendOutput, BackendRequest,
    PromptMode,
};
use crate::session::WriteMode;
use anyhow::{Context, Result};
use async_trait::async_trait;
use serde_json::{Value, json};
use tokio::sync::Mutex;

/// Owns Codex app-server thread lifecycle, prompting, permission policy, and native fork.
pub struct CodexBackend {
    command: Vec<String>,
    default_model: Mutex<Option<String>>,
}

fn assistant_plan_fallback(output: &BackendOutput) -> Option<String> {
    let markdown = output
        .event
        .iter()
        .filter(|event| event.kind == "assistant_message")
        .filter_map(|event| event.text.as_deref())
        .filter(|text| !text.trim().is_empty())
        .collect::<Vec<_>>()
        .join("\n\n");
    (!markdown.trim().is_empty()).then_some(markdown)
}

impl CodexBackend {
    /// Build a Codex backend from its app-server launch command.
    pub fn new(command: Vec<String>) -> Result<Self> {
        anyhow::ensure!(
            !command.is_empty(),
            "Codex backend requires harness.backends.codex.command"
        );
        Ok(Self {
            command,
            default_model: Mutex::new(None),
        })
    }

    async fn connect(
        &self,
        request: &BackendRequest,
        output: &mut BackendOutput,
        event_sink: Option<BackendEventSink>,
    ) -> Result<JsonRpcProcess> {
        let mut process = JsonRpcProcess::start(
            &self.command,
            request.write_mode == WriteMode::Write,
            &request.workspace,
            request.trust_policy.clone(),
            event_sink,
        )
        .await?;
        process.request("initialize", json!({
            "clientInfo": { "name": "diff-review-harness", "title": "DiffReview Harness", "version": env!("CARGO_PKG_VERSION") },
            "capabilities": { "experimentalApi": true }
        }), output).await?;
        process.notify("initialized", Value::Null).await?;
        Ok(process)
    }

    fn approval_policy(request: &BackendRequest) -> &'static str {
        if request.write_mode == WriteMode::Write {
            "never"
        } else {
            "untrusted"
        }
    }

    fn with_model(mut params: Value, model: &str) -> Value {
        if model != "default" {
            params
                .as_object_mut()
                .expect("Codex request params must be an object")
                .insert("model".into(), Value::String(model.into()));
        }
        params
    }

    async fn model_catalog(
        process: &mut JsonRpcProcess,
        output: &mut BackendOutput,
    ) -> Result<Vec<BackendModel>> {
        let result = process
            .request(
                "model/list",
                json!({ "cursor": null, "includeHidden": false }),
                output,
            )
            .await?;
        Ok(Self::parse_model_catalog(&result))
    }

    fn parse_model_catalog(result: &Value) -> Vec<BackendModel> {
        result
            .get("data")
            .and_then(Value::as_array)
            .into_iter()
            .flatten()
            .filter_map(|model| {
                let id = model
                    .get("model")
                    .or_else(|| model.get("id"))
                    .and_then(Value::as_str)?;
                let effort = model
                    .get("supportedReasoningEfforts")
                    .and_then(Value::as_array)
                    .into_iter()
                    .flatten()
                    .filter_map(|entry| {
                        entry
                            .get("reasoningEffort")
                            .and_then(Value::as_str)
                            .map(str::to_owned)
                    })
                    .collect();
                Some(BackendModel {
                    id: id.to_owned(),
                    label: model
                        .get("displayName")
                        .and_then(Value::as_str)
                        .unwrap_or(id)
                        .to_owned(),
                    effort,
                    is_default: model
                        .get("isDefault")
                        .and_then(Value::as_bool)
                        .unwrap_or(false),
                })
            })
            .collect()
    }

    async fn resolve_model(
        &self,
        request: &BackendRequest,
        process: &mut JsonRpcProcess,
        output: &mut BackendOutput,
    ) -> Result<String> {
        if request.model != "default" {
            return Ok(request.model.clone());
        }
        if let Some(model) = self.default_model.lock().await.clone() {
            return Ok(model);
        }
        let model_list = Self::model_catalog(process, output).await?;
        let model = model_list
            .iter()
            .find(|model| model.is_default)
            .or_else(|| model_list.first())
            .map(|model| model.id.clone())
            .context("Codex model/list returned no visible models")?;
        *self.default_model.lock().await = Some(model.clone());
        Ok(model)
    }
}

#[async_trait]
impl Backend for CodexBackend {
    async fn prompt_stream(
        &self,
        request: BackendRequest,
        event_sink: Option<BackendEventSink>,
    ) -> Result<BackendOutput> {
        let mut output = BackendOutput {
            capability: BackendCapability {
                native_fork: true,
                native_goal: true,
                native_plan: true,
                model_selection: true,
                effort_selection: true,
                permission_control: true,
            },
            ..BackendOutput::default()
        };
        let mut process = self.connect(&request, &mut output, event_sink).await?;
        let resolved_model = self
            .resolve_model(&request, &mut process, &mut output)
            .await?;
        output.runtime.provider = "Codex CLI".into();
        output.runtime.model = Some(resolved_model);
        let thread = match &request.backend_session_id {
            Some(thread_id) => process.request("thread/resume", json!({
                "threadId": thread_id,
                "cwd": request.workspace,
                "approvalPolicy": Self::approval_policy(&request),
                "sandbox": if request.write_mode == WriteMode::Write { "workspace-write" } else { "read-only" }
            }), &mut output).await?,
            None => process.request("thread/start", Self::with_model(json!({
                "cwd": request.workspace,
                "approvalPolicy": Self::approval_policy(&request),
                "sandbox": if request.write_mode == WriteMode::Write { "workspace-write" } else { "read-only" },
                "experimentalRawEvents": false,
                "historyMode": "legacy",
                "developerInstructions": "You run inside DiffReview Harness. For planning, call harness_plan_submit with the complete Markdown plan. For a terminal goal state, call harness_goal_complete or harness_goal_blocked. Never claim completion through ordinary prose alone.",
                "dynamicTools": [
                    { "type": "function", "name": "harness_plan_submit", "description": "Submit the complete Markdown plan for mandatory user review.", "inputSchema": { "type": "object", "properties": { "markdown": { "type": "string" } }, "required": ["markdown"] } },
                    { "type": "function", "name": "harness_goal_complete", "description": "Mark the active Harness goal complete.", "inputSchema": { "type": "object", "properties": { "summary": { "type": "string" } }, "required": ["summary"] } },
                    { "type": "function", "name": "harness_goal_blocked", "description": "Mark the active Harness goal blocked.", "inputSchema": { "type": "object", "properties": { "reason": { "type": "string" } }, "required": ["reason"] } },
                    { "type": "function", "name": "harness_goal_status", "description": "Report nonterminal progress toward the active Harness goal.", "inputSchema": { "type": "object", "properties": { "status": { "type": "string" } }, "required": ["status"] } }
                ]
            }), &request.model), &mut output).await?,
        };
        let thread_id = thread
            .pointer("/thread/id")
            .or_else(|| thread.get("threadId"))
            .or_else(|| thread.get("thread_id"))
            .and_then(Value::as_str)
            .map(str::to_owned)
            .or(request.backend_session_id.clone())
            .context("Codex thread response omitted thread id")?;
        output.backend_session_id = Some(thread_id.clone());
        let mut prompt = request.text.clone();
        if request.mode == PromptMode::GoalContinuation {
            let objective = request
                .text
                .strip_prefix("/goal ")
                .and_then(|value| value.lines().next())
                .filter(|value| *value != "resume")
                .map(str::to_owned);
            if let Some(objective) = objective {
                process
                    .request(
                        "thread/goal/set",
                        json!({
                            "threadId": thread_id,
                            "objective": objective,
                            "status": "active"
                        }),
                        &mut output,
                    )
                    .await?;
                prompt = format!("Work toward the active goal: {objective}");
            } else {
                prompt = "Continue working toward the active goal.".into();
            }
        } else if request.mode == PromptMode::ExecutePlan {
            process
                .request(
                    "thread/goal/set",
                    json!({
                        "threadId": thread_id,
                        "objective": "Complete the plan",
                        "status": "active"
                    }),
                    &mut output,
                )
                .await?;
        }
        let collaboration_mode = (request.mode == PromptMode::Plan).then(|| {
            let settings = Self::with_model(json!({
                "reasoning_effort": request.effort,
                "developer_instructions": "Produce a reviewer-readable implementation plan, then call harness_plan_submit with the complete Markdown artifact. Do not modify files."
            }), &request.model);
            json!({ "mode": "plan", "settings": settings })
        });
        process.request("turn/start", Self::with_model(json!({
            "threadId": thread_id,
            "input": [{ "type": "text", "text": prompt }],
            "cwd": request.workspace,
            "effort": request.effort,
            "serviceTier": if request.fast_mode { Value::String("fast".into()) } else { Value::Null },
            "collaborationMode": collaboration_mode,
            "approvalPolicy": Self::approval_policy(&request),
            "sandboxPolicy": {
                "type": if request.write_mode == WriteMode::Write { "workspaceWrite" } else { "readOnly" }
            }
        }), &request.model), &mut output).await?;
        let completed = process.read_until("turn/completed", &mut output).await?;
        let status = completed
            .pointer("/turn/status")
            .or_else(|| completed.get("status"))
            .and_then(Value::as_str)
            .unwrap_or_default();
        anyhow::ensure!(
            !status.eq_ignore_ascii_case("failed"),
            "Codex turn failed: {completed}"
        );
        if request.mode == PromptMode::Plan && output.plan_markdown.is_none() {
            output.plan_markdown = assistant_plan_fallback(&output);
        }
        Ok(output)
    }

    async fn fork(&self, request: BackendRequest) -> Result<String> {
        let source = request
            .backend_session_id
            .clone()
            .context("Codex thread has not started")?;
        let mut output = BackendOutput::default();
        let mut process = self.connect(&request, &mut output, None).await?;
        let result = process.request("thread/fork", json!({
            "threadId": source,
            "cwd": request.workspace,
            "approvalPolicy": Self::approval_policy(&request),
            "sandbox": if request.write_mode == WriteMode::Write { "workspace-write" } else { "read-only" }
        }), &mut output).await?;
        result
            .pointer("/thread/id")
            .or_else(|| result.get("threadId"))
            .or_else(|| result.get("thread_id"))
            .and_then(Value::as_str)
            .map(str::to_owned)
            .context("Codex fork response omitted thread id")
    }

    async fn model_list(&self, request: BackendRequest) -> Result<Vec<BackendModel>> {
        let mut output = BackendOutput::default();
        let mut process = self.connect(&request, &mut output, None).await?;
        let model_list = Self::model_catalog(&mut process, &mut output).await?;
        if let Some(model) = model_list.iter().find(|model| model.is_default) {
            *self.default_model.lock().await = Some(model.id.clone());
        }
        Ok(model_list)
    }

    async fn goal_status(
        &self,
        request: BackendRequest,
        objective: Option<String>,
        status: &str,
    ) -> Result<()> {
        let thread_id = request
            .backend_session_id
            .clone()
            .context("Codex thread has not started")?;
        let mut output = BackendOutput::default();
        let mut process = self.connect(&request, &mut output, None).await?;
        if status == "cleared" {
            process
                .request(
                    "thread/goal/clear",
                    json!({ "threadId": thread_id }),
                    &mut output,
                )
                .await?;
        } else {
            process
                .request(
                    "thread/goal/set",
                    json!({ "threadId": thread_id, "objective": objective, "status": status }),
                    &mut output,
                )
                .await?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::backend::BackendEvent;

    #[test]
    fn uses_codex_plan_mode_message_when_no_structured_plan_event_arrives() {
        let output = BackendOutput {
            event: vec![BackendEvent {
                kind: "assistant_message".into(),
                text: Some("# Plan\n\n1. Inspect the change.".into()),
                data: Value::Null,
                activity: None,
                summary: None,
                plan_progress: None,
            }],
            ..BackendOutput::default()
        };
        assert_eq!(
            assistant_plan_fallback(&output).as_deref(),
            Some("# Plan\n\n1. Inspect the change.")
        );
    }

    #[test]
    fn omits_the_default_model_sentinel_from_codex_requests() {
        let default = CodexBackend::with_model(json!({ "cwd": "workspace" }), "default");
        assert!(default.get("model").is_none());
        let selected = CodexBackend::with_model(json!({ "cwd": "workspace" }), "gpt-5.4-mini");
        assert_eq!(
            selected.get("model").and_then(Value::as_str),
            Some("gpt-5.4-mini")
        );
        let catalog = CodexBackend::parse_model_catalog(&json!({
            "data": [
                {
                    "model": "gpt-default",
                    "displayName": "Default model",
                    "isDefault": true,
                    "supportedReasoningEfforts": [{ "reasoningEffort": "medium" }]
                }
            ]
        }));
        assert_eq!(catalog[0].id, "gpt-default");
        assert!(catalog[0].is_default);
    }
}
