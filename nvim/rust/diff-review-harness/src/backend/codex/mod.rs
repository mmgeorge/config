use crate::backend::json_rpc::JsonRpcProcess;
use crate::backend::steering::{ActiveSteering, SteerCommand, SteeringLane};
use crate::backend::{
    Backend, BackendCapability, BackendEventSink, BackendModel, BackendOutput, BackendRequest,
    PromptMode,
};
use crate::session::WriteMode;
use anyhow::{Context, Result};
use async_trait::async_trait;
use serde_json::{Value, json};
use std::collections::HashMap;
use tokio::sync::Mutex;

/// Owns Codex app-server thread lifecycle, prompting, permission policy, and native fork.
pub struct CodexBackend {
    command: Vec<String>,
    default_model: Mutex<Option<String>>,
    steering: SteeringLane,
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
            steering: SteeringLane::default(),
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

    async fn run_active_turn(
        process: &mut JsonRpcProcess,
        output: &mut BackendOutput,
        steering: &mut ActiveSteering,
        thread_id: &str,
        turn_id: &str,
        mut provider_turn_started: bool,
    ) -> Result<Value> {
        let mut pending_command = HashMap::<u64, SteerCommand>::new();
        loop {
            tokio::select! {
                Some(command) = steering.receive(), if provider_turn_started => {
                    let request_id = process
                        .send_request(
                            "turn/steer",
                            json!({
                                "threadId": thread_id,
                                "input": [{ "type": "text", "text": command.text.clone() }],
                                "expectedTurnId": turn_id,
                            }),
                        )
                        .await;
                    match request_id {
                        Ok(request_id) => {
                            pending_command.insert(request_id, command);
                        }
                        Err(error) => command.complete(Err(error)),
                    }
                }
                message = process.read_message(output) => {
                    let message = message?;
                    if Self::notification_turn_id(&message, "turn/started") == Some(turn_id) {
                        provider_turn_started = true;
                    }
                    if let Some(request_id) = message.get("id").and_then(Value::as_u64)
                        && let Some(command) = pending_command.remove(&request_id)
                    {
                        let result = JsonRpcProcess::request_result(
                            &message,
                            request_id,
                            "turn/steer",
                        )
                        .context("Codex steering response omitted its request result")?
                        .map(drop);
                        command.complete(result);
                    }
                    if message.get("method").and_then(Value::as_str) == Some("turn/completed") {
                        drop(pending_command);
                        return Ok(message.get("params").cloned().unwrap_or(Value::Null));
                    }
                }
            }
        }
    }

    fn notification_turn_id<'a>(message: &'a Value, method: &str) -> Option<&'a str> {
        (message.get("method").and_then(Value::as_str) == Some(method))
            .then(|| message.pointer("/params/turn/id").and_then(Value::as_str))
            .flatten()
    }

    async fn start_turn(
        process: &mut JsonRpcProcess,
        output: &mut BackendOutput,
        params: Value,
    ) -> Result<(Value, bool)> {
        let request_id = process.send_request("turn/start", params).await?;
        let mut started_turn_id = None;
        loop {
            let message = process.read_message(output).await?;
            if let Some(turn_id) = Self::notification_turn_id(&message, "turn/started") {
                started_turn_id = Some(turn_id.to_owned());
            }
            if let Some(response) =
                JsonRpcProcess::request_result(&message, request_id, "turn/start")
            {
                let turn = response?;
                let turn_id = turn
                    .pointer("/turn/id")
                    .or_else(|| turn.get("turnId"))
                    .or_else(|| turn.get("turn_id"))
                    .and_then(Value::as_str)
                    .context("Codex turn/start response omitted turn id")?;
                let provider_turn_started = started_turn_id.as_deref() == Some(turn_id);
                return Ok((turn, provider_turn_started));
            }
        }
    }
}

#[async_trait]
impl Backend for CodexBackend {
    async fn prompt_stream(
        &self,
        request: BackendRequest,
        event_sink: Option<BackendEventSink>,
    ) -> Result<BackendOutput> {
        let mut active_steering = self.steering.activate()?;
        let mut output = BackendOutput {
            capability: BackendCapability {
                native_fork: true,
                native_compact: true,
                native_steer: true,
                native_goal: true,
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
        let plan_question_schema = crate::control_tools::plan_question_input_schema();
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
                "developerInstructions": "You run inside DiffReview Harness. Call harness_plan_question whenever the user asks for interactive or multiple-choice questions, and during planning when a material user decision is required. The question tool works in every mode. Call harness_plan_submit only with a complete Markdown plan. End the turn after a Harness question or plan control call. For a terminal goal state, call harness_goal_complete or harness_goal_blocked. Never claim a control action through ordinary prose alone.",
                "dynamicTools": [
                    { "type": "function", "name": "harness_plan_submit", "description": "Submit the complete Markdown plan for mandatory user review.", "inputSchema": { "type": "object", "properties": { "markdown": { "type": "string" } }, "required": ["markdown"] } },
                    { "type": "function", "name": "harness_plan_question", "description": "Pause any Harness turn and present one to three interactive user questions. Use this for explicit requests for multiple-choice questions as well as planning decisions.", "inputSchema": plan_question_schema },
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
        let mut prompt = format!(
            "Harness interaction contract: when the user explicitly asks for interactive or multiple-choice questions, call harness_plan_question with the complete question set. The tool works outside planning. Do not claim questions were sent through prose.\n\n{}",
            request.text
        );
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
        let (turn, provider_turn_started) = Self::start_turn(&mut process, &mut output, Self::with_model(json!({
            "threadId": thread_id,
            "input": [{ "type": "text", "text": prompt }],
            "cwd": request.workspace,
            "effort": request.effort,
            "serviceTier": if request.fast_mode { Value::String("fast".into()) } else { Value::Null },
            "approvalPolicy": Self::approval_policy(&request),
            "sandboxPolicy": {
                "type": if request.write_mode == WriteMode::Write { "workspaceWrite" } else { "readOnly" }
            }
        }), &request.model)).await?;
        let turn_id = turn
            .pointer("/turn/id")
            .or_else(|| turn.get("turnId"))
            .or_else(|| turn.get("turn_id"))
            .and_then(Value::as_str)
            .context("Codex turn/start response omitted turn id")?;
        let completed = Self::run_active_turn(
            &mut process,
            &mut output,
            &mut active_steering,
            &thread_id,
            turn_id,
            provider_turn_started,
        )
        .await?;
        let status = completed
            .pointer("/turn/status")
            .or_else(|| completed.get("status"))
            .and_then(Value::as_str)
            .unwrap_or_default();
        anyhow::ensure!(
            !status.eq_ignore_ascii_case("failed"),
            "Codex turn failed: {completed}"
        );
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

    async fn steer(&self, text: String) -> Result<()> {
        self.steering.steer(text).await
    }

    async fn compact(&self, request: BackendRequest) -> Result<BackendOutput> {
        let thread_id = request
            .backend_session_id
            .clone()
            .context("Codex thread has not started")?;
        let mut output = BackendOutput {
            backend_session_id: Some(thread_id.clone()),
            capability: BackendCapability {
                native_fork: true,
                native_compact: true,
                native_steer: true,
                native_goal: true,
                model_selection: true,
                effort_selection: true,
                permission_control: true,
            },
            ..BackendOutput::default()
        };
        let mut process = self.connect(&request, &mut output, None).await?;
        process
            .request(
                "thread/resume",
                json!({
                    "threadId": thread_id,
                    "cwd": request.workspace,
                    "approvalPolicy": Self::approval_policy(&request),
                    "sandbox": if request.write_mode == WriteMode::Write { "workspace-write" } else { "read-only" }
                }),
                &mut output,
            )
            .await?;
        process
            .request(
                "thread/compact/start",
                json!({ "threadId": thread_id }),
                &mut output,
            )
            .await?;
        let completed = process.read_until("turn/completed", &mut output).await?;
        let status = completed
            .pointer("/turn/status")
            .or_else(|| completed.get("status"))
            .and_then(Value::as_str)
            .unwrap_or_default();
        anyhow::ensure!(
            !status.eq_ignore_ascii_case("failed"),
            "Codex compaction failed: {completed}"
        );
        output.runtime.provider = "Codex CLI".into();
        Ok(output)
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

    #[test]
    fn recognizes_only_the_matching_turn_started_notification() {
        let started = json!({
            "method": "turn/started",
            "params": { "turn": { "id": "turn-42", "status": "inProgress" } }
        });
        assert_eq!(
            CodexBackend::notification_turn_id(&started, "turn/started"),
            Some("turn-42")
        );
        assert_eq!(
            CodexBackend::notification_turn_id(&started, "turn/completed"),
            None
        );
    }
}
