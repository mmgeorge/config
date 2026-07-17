use crate::backend::approval::PermissionCoordinator;
use crate::backend::steering::SteeringLane;
use crate::backend::{
    Backend, BackendCapability, BackendDescriptor, BackendEventSink, BackendKind, BackendModel,
    BackendOutput, BackendRequest, PromptMode,
};
use crate::control_tools::ControlToolRegistry;
use crate::session::ExecutionMode;
use anyhow::{Context, Result};
use async_trait::async_trait;
use serde_json::{Value, json};
use std::sync::Arc;
use tokio::sync::Mutex;

mod json_rpc;
mod process;
mod security;
mod turn_coordinator;

use json_rpc::CodexJsonRpc;
use security::CodexSecurity;

/// Owns Codex app-server thread lifecycle, prompting, permission policy, and native fork.
pub struct CodexBackend {
    command: Vec<String>,
    default_model: Mutex<Option<String>>,
    steering: SteeringLane,
    active_turn: Mutex<CodexTurnState>,
    permission_coordinator: Arc<PermissionCoordinator>,
}

/// Tracks whether the current prompt reached Codex conversation history.
#[derive(Clone, Debug, Default)]
enum CodexTurnState {
    #[default]
    Idle,
    Pending,
    Submitted {
        request: BackendRequest,
        thread_id: String,
    },
    Completed,
}

fn capability() -> BackendCapability {
    BackendCapability {
        native_fork: true,
        native_compact: true,
        native_steer: true,
        native_turn_rollback: true,
        native_goal: true,
        model_selection: true,
        effort_selection: true,
        fast_mode: true,
        permission_control: true,
        execution_mode_list: vec![
            ExecutionMode::Read,
            ExecutionMode::Write,
            ExecutionMode::Full,
            ExecutionMode::Yolo,
        ],
        agent: crate::agent::AgentCapability::codex(),
    }
}

impl CodexBackend {
    /// Build a Codex backend with an isolated conservative permission registry.
    pub fn new(command: Vec<String>) -> Result<Self> {
        let permission_coordinator = PermissionCoordinator::transient(".")?;
        Self::new_with_permission_coordinator(command, permission_coordinator)
    }

    /// Build a Codex backend with the shared Harness permission coordinator.
    pub fn new_with_permission_coordinator(
        command: Vec<String>,
        permission_coordinator: Arc<PermissionCoordinator>,
    ) -> Result<Self> {
        anyhow::ensure!(
            !command.is_empty(),
            "Codex backend requires harness.backends.codex.command"
        );
        Ok(Self {
            command,
            default_model: Mutex::new(None),
            steering: SteeringLane::default(),
            active_turn: Mutex::new(CodexTurnState::Idle),
            permission_coordinator,
        })
    }

    async fn connect(
        &self,
        request: &BackendRequest,
        output: &mut BackendOutput,
        event_sink: Option<BackendEventSink>,
    ) -> Result<CodexJsonRpc> {
        let security = CodexSecurity::new(request.execution_mode);
        let command = security.launch_command(&self.command);
        let mut process = CodexJsonRpc::start(
            &command,
            &request.workspace,
            request.execution_mode,
            Arc::clone(&self.permission_coordinator),
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

    fn apply_security(params: &mut Value, request: &BackendRequest) {
        CodexSecurity::new(request.execution_mode).apply(params, &request.workspace);
    }

    fn secure(mut params: Value, request: &BackendRequest) -> Value {
        Self::apply_security(&mut params, request);
        params
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
        process: &mut CodexJsonRpc,
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
        process: &mut CodexJsonRpc,
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

    fn notification_turn_id<'a>(message: &'a Value, method: &str) -> Option<&'a str> {
        (message.get("method").and_then(Value::as_str) == Some(method))
            .then(|| message.pointer("/params/turn/id").and_then(Value::as_str))
            .flatten()
    }

    fn notification_matches_turn(
        message: &Value,
        method: &str,
        thread_id: &str,
        turn_id: &str,
    ) -> bool {
        message.get("method").and_then(Value::as_str) == Some(method)
            && message.pointer("/params/threadId").and_then(Value::as_str) == Some(thread_id)
            && Self::notification_turn_id(message, method) == Some(turn_id)
    }

    async fn start_turn(
        &self,
        process: &mut CodexJsonRpc,
        output: &mut BackendOutput,
        request: &BackendRequest,
        thread_id: &str,
        params: Value,
    ) -> Result<(Value, bool, Vec<Value>)> {
        let request_id = process.send_request("turn/start", params).await?;
        *self.active_turn.lock().await = CodexTurnState::Submitted {
            request: request.clone(),
            thread_id: thread_id.to_owned(),
        };
        let mut started_turn_id = None;
        let mut observed_message_list = Vec::new();
        loop {
            let message = process.read_message(output).await?;
            if let Some(turn_id) = Self::notification_turn_id(&message, "turn/started") {
                started_turn_id = Some(turn_id.to_owned());
            }
            if let Some(response) = CodexJsonRpc::request_result(&message, request_id, "turn/start")
            {
                let turn = response?;
                let turn_id = turn
                    .pointer("/turn/id")
                    .or_else(|| turn.get("turnId"))
                    .or_else(|| turn.get("turn_id"))
                    .and_then(Value::as_str)
                    .context("Codex turn/start response omitted turn id")?;
                let provider_turn_started = started_turn_id.as_deref() == Some(turn_id);
                observed_message_list.push(message);
                return Ok((turn, provider_turn_started, observed_message_list));
            }
            observed_message_list.push(message);
        }
    }
}

#[async_trait]
impl Backend for CodexBackend {
    fn descriptor(&self) -> BackendDescriptor {
        BackendDescriptor {
            kind: BackendKind::Codex,
            label: "Codex CLI".into(),
            capability: capability(),
        }
    }

    async fn prompt_stream(
        &self,
        request: BackendRequest,
        event_sink: Option<BackendEventSink>,
    ) -> Result<BackendOutput> {
        *self.active_turn.lock().await = CodexTurnState::Pending;
        let mut active_steering = self.steering.activate(event_sink.clone())?;
        let mut output = BackendOutput {
            capability: capability(),
            ..BackendOutput::default()
        };
        let coordinator_event_sink = event_sink.clone();
        let mut process = self.connect(&request, &mut output, event_sink).await?;
        let resolved_model = self
            .resolve_model(&request, &mut process, &mut output)
            .await?;
        output.runtime.provider = "Codex CLI".into();
        output.runtime.model = Some(resolved_model);
        let dynamic_tool_list = ControlToolRegistry
            .definition_list()
            .into_iter()
            .map(|definition| {
                json!({
                    "type": "function",
                    "name": definition.name,
                    "description": definition.description,
                    "inputSchema": definition.input_schema,
                })
            })
            .collect::<Vec<_>>();
        let thread = match &request.backend_session_id {
            Some(thread_id) => process.request("thread/resume", Self::secure(json!({
                "threadId": thread_id,
                "cwd": request.workspace
            }), &request), &mut output).await?,
            None => process.request("thread/start", Self::with_model(Self::secure(json!({
                "cwd": request.workspace,
                "experimentalRawEvents": false,
                "historyMode": "legacy",
                "developerInstructions": "You run inside DiffReview Harness. Call harness_plan_question whenever the user asks for interactive or multiple-choice questions, and during planning when a material user decision is required. While questions remain pending, use harness_question_answer only for an explicit user answer and harness_question_withdraw only when no material user decision remains. The question tools work in every mode. Call harness_plan_submit only with a complete Markdown plan. End the turn after a Harness question or plan control call. For a terminal goal state, call harness_goal_complete or harness_goal_blocked. Never claim a control action through ordinary prose alone.",
                "dynamicTools": dynamic_tool_list
            }), &request), &request.model), &mut output).await?,
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
            "Harness interaction contract: when the user explicitly asks for interactive or multiple-choice questions, call harness_plan_question with the complete question set. While questions remain pending, use harness_question_answer only for an explicit user answer and harness_question_withdraw only when no material user decision remains. The question tools work outside planning. Do not claim control actions through prose.\n\n{}",
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
        let (turn, provider_turn_started, observed_message_list) = self.start_turn(&mut process, &mut output, &request, &thread_id, Self::with_model(Self::secure(json!({
            "threadId": thread_id,
            "input": [{ "type": "text", "text": prompt }],
            "cwd": request.workspace,
            "effort": request.effort,
            "serviceTier": if request.fast_mode { Value::String("fast".into()) } else { Value::Null }
        }), &request), &request.model)).await?;
        let turn_id = turn
            .pointer("/turn/id")
            .or_else(|| turn.get("turnId"))
            .or_else(|| turn.get("turn_id"))
            .and_then(Value::as_str)
            .context("Codex turn/start response omitted turn id")?;
        let completed = turn_coordinator::CodexTurnCoordinator::new(
            self,
            &mut process,
            &mut output,
            &mut active_steering,
            coordinator_event_sink,
            &request,
            &thread_id,
        )
        .run(
            turn_id.to_owned(),
            provider_turn_started,
            observed_message_list,
        )
        .await?;
        *self.active_turn.lock().await = CodexTurnState::Completed;
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
        let result = process
            .request(
                "thread/fork",
                Self::secure(
                    json!({
                        "threadId": source,
                        "cwd": request.workspace
                    }),
                    &request,
                ),
                &mut output,
            )
            .await?;
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

    async fn steer_target(&self, text: String, target: crate::backend::SteerTarget) -> Result<()> {
        self.steering.steer_target(text, target).await
    }

    async fn interrupt_target(&self, target: crate::backend::SteerTarget) -> Result<()> {
        self.steering.interrupt_target(target).await
    }

    async fn compact(&self, request: BackendRequest) -> Result<BackendOutput> {
        let thread_id = request
            .backend_session_id
            .clone()
            .context("Codex thread has not started")?;
        let mut output = BackendOutput {
            backend_session_id: Some(thread_id.clone()),
            capability: capability(),
            ..BackendOutput::default()
        };
        let mut process = self.connect(&request, &mut output, None).await?;
        process
            .request(
                "thread/resume",
                Self::secure(
                    json!({
                        "threadId": thread_id,
                        "cwd": request.workspace
                    }),
                    &request,
                ),
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

    async fn rollback_cancelled_turn(&self) -> Result<bool> {
        let state = std::mem::take(&mut *self.active_turn.lock().await);
        match state {
            CodexTurnState::Pending => Ok(true),
            CodexTurnState::Submitted { request, thread_id } => {
                let mut output = BackendOutput::default();
                let mut process = self.connect(&request, &mut output, None).await?;
                process
                    .request(
                        "thread/rollback",
                        json!({ "threadId": thread_id, "numTurns": 1 }),
                        &mut output,
                    )
                    .await
                    .context("roll back output-free Codex turn")?;
                Ok(true)
            }
            CodexTurnState::Idle | CodexTurnState::Completed => Ok(false),
        }
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

    #[test]
    fn ignores_a_child_completion_while_waiting_for_the_parent_turn() {
        let child = json!({
            "method": "turn/completed",
            "params": {
                "threadId": "child-thread",
                "turn": { "id": "child-turn", "status": "completed" }
            }
        });
        let parent = json!({
            "method": "turn/completed",
            "params": {
                "threadId": "parent-thread",
                "turn": { "id": "parent-turn", "status": "completed" }
            }
        });

        assert!(!CodexBackend::notification_matches_turn(
            &child,
            "turn/completed",
            "parent-thread",
            "parent-turn"
        ));
        assert!(CodexBackend::notification_matches_turn(
            &parent,
            "turn/completed",
            "parent-thread",
            "parent-turn"
        ));
    }

    #[tokio::test]
    async fn distinguishes_an_unsubmitted_cancel_from_a_completed_turn() {
        let backend = CodexBackend::new(vec!["codex".into(), "app-server".into()]).unwrap();
        *backend.active_turn.lock().await = CodexTurnState::Pending;
        assert!(backend.rollback_cancelled_turn().await.unwrap());
        *backend.active_turn.lock().await = CodexTurnState::Completed;
        assert!(!backend.rollback_cancelled_turn().await.unwrap());
    }
}
