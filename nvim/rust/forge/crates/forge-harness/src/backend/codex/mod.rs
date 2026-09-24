use crate::backend::approval::PermissionCoordinator;
use crate::backend::steering::{ActiveSteering, SteeringLane};
use crate::backend::{
    Backend, BackendCapability, BackendCatalogRequest, BackendDescriptor, BackendEventSink,
    BackendForkRequest, BackendForkResult, BackendInput, BackendKind, BackendModel, BackendOutput,
    BackendRequest, BackendTimingRecord, CatalogCapability, CatalogMutation, McpDefinition,
    McpStatus, McpToolDefinition, PromptMode, SkillDefinition,
};
use crate::control_tools::ControlToolRegistry;
use crate::session::ExecutionMode;
use crate::trace::TraceStore;
use anyhow::{Context, Result};
use async_trait::async_trait;
use serde_json::{Value, json};
use std::collections::{HashMap, HashSet};
use std::path::Path;
use std::sync::Arc;
use std::time::Instant;
use tokio::sync::{Mutex, Notify};

mod json_rpc;
mod process;
mod recap;
mod runtime;
mod security;
mod turn_coordinator;

use json_rpc::CodexJsonRpc;
use runtime::CodexRuntime;
use security::CodexSecurity;

struct CodexConnection {
    process: CodexJsonRpc,
    timing: Vec<BackendTimingRecord>,
}

/// Owns Codex app-server thread lifecycle, prompting, permission policy, and native fork.
pub struct CodexBackend {
    runtime: CodexRuntime,
    default_model: Mutex<Option<String>>,
    steering_by_session: Mutex<HashMap<String, SteeringLane>>,
    active_turn_by_session: Mutex<HashMap<String, CodexTurnState>>,
    turn_started: Notify,
    connection_by_session: Mutex<HashMap<String, Arc<Mutex<CodexConnection>>>>,
    completed_turn_by_session: Mutex<HashMap<String, String>>,
    permission_coordinator: Arc<PermissionCoordinator>,
    trace: Arc<TraceStore>,
}

/// Tracks whether the current prompt reached Codex conversation history.
#[derive(Clone, Debug, Default)]
enum CodexTurnState {
    #[default]
    Idle,
    Pending,
    Submitted {
        request: Box<BackendRequest>,
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
        catalog: CatalogCapability {
            skill: true,
            mcp: true,
            live_mcp_mutation: false,
        },
    }
}

fn question_contract(mode: PromptMode, request_text: &str) -> &'static str {
    if mode == PromptMode::Plan && request_text.contains("Planning feedback:") {
        "The planning feedback in this turn has already been recorded and consumed. Do not call harness_question_answer or harness_question_withdraw for it."
    } else {
        "While questions remain pending, use harness_question_answer only for an explicit user answer and harness_question_withdraw only when no material user decision remains."
    }
}

impl CodexBackend {
    /// Build a Codex backend with an isolated conservative permission registry.
    pub fn new(command: Vec<String>) -> Result<Self> {
        let permission_coordinator = PermissionCoordinator::transient(".")?;
        let trace = Arc::new(TraceStore::open(Path::new("."))?);
        Self::new_with_permission_coordinator(command, permission_coordinator, trace)
    }

    /// Return how many Codex app-server process generations this backend launched.
    pub fn app_server_start_count(&self) -> u64 {
        self.runtime.start_count()
    }

    /// Return the operating-system process identifier for the shared app-server.
    pub async fn app_server_process_id(&self) -> Option<u32> {
        self.runtime.process_id().await
    }

    /// Build a Codex backend with the shared Harness permission coordinator.
    pub fn new_with_permission_coordinator(
        command: Vec<String>,
        permission_coordinator: Arc<PermissionCoordinator>,
        trace: Arc<TraceStore>,
    ) -> Result<Self> {
        anyhow::ensure!(
            !command.is_empty(),
            "Codex backend requires harness.backends.codex.command"
        );
        Ok(Self {
            runtime: CodexRuntime::new(
                CodexSecurity::new(ExecutionMode::Read).launch_command(&command),
            ),
            default_model: Mutex::new(None),
            steering_by_session: Mutex::new(HashMap::new()),
            active_turn_by_session: Mutex::new(HashMap::new()),
            turn_started: Notify::new(),
            connection_by_session: Mutex::new(HashMap::new()),
            completed_turn_by_session: Mutex::new(HashMap::new()),
            permission_coordinator,
            trace,
        })
    }

    /// Publish turn readiness only after its control receiver is registered.
    async fn activate_turn_control(
        &self,
        session_id: &str,
        event_sink: Option<BackendEventSink>,
    ) -> Result<ActiveSteering> {
        let active = self.steering_lane(session_id).await.activate(event_sink)?;
        self.active_turn_by_session
            .lock()
            .await
            .insert(session_id.to_owned(), CodexTurnState::Pending);
        self.turn_started.notify_waiters();
        Ok(active)
    }

    async fn connect(
        &self,
        request: &BackendRequest,
        output: &mut BackendOutput,
        event_sink: Option<BackendEventSink>,
    ) -> Result<CodexConnection> {
        let process_started = Instant::now();
        let mut process = self
            .runtime
            .connect(
                &request.workspace,
                request.execution_mode,
                Arc::clone(&self.permission_coordinator),
                event_sink,
                Arc::clone(&self.trace),
                request.harness_session_id.clone(),
            )
            .await?;
        let process_duration_ms = process_started.elapsed().as_secs_f64() * 1000.0;
        let initialize_started = Instant::now();
        process.request("initialize", json!({
            "clientInfo": { "name": "forge-harness", "title": "Forge Harness", "version": env!("CARGO_PKG_VERSION") },
            "capabilities": { "experimentalApi": true }
        }), output).await?;
        process.notify("initialized", Value::Null).await?;
        Ok(CodexConnection {
            process,
            timing: vec![
                BackendTimingRecord {
                    phase: "codex.process_start".into(),
                    duration_ms: process_duration_ms,
                },
                BackendTimingRecord {
                    phase: "codex.initialize".into(),
                    duration_ms: initialize_started.elapsed().as_secs_f64() * 1000.0,
                },
            ],
        })
    }

    async fn persistent_connection(
        &self,
        request: &BackendRequest,
        output: &mut BackendOutput,
        event_sink: Option<BackendEventSink>,
    ) -> Result<Arc<Mutex<CodexConnection>>> {
        if let Some(connection) = self
            .connection_by_session
            .lock()
            .await
            .get(&request.harness_session_id)
            .cloned()
        {
            connection.lock().await.process.set_request_context(
                &request.workspace,
                request.execution_mode,
                event_sink,
            );
            return Ok(connection);
        }
        let connection = Arc::new(Mutex::new(self.connect(request, output, event_sink).await?));
        self.connection_by_session
            .lock()
            .await
            .insert(request.harness_session_id.clone(), Arc::clone(&connection));
        Ok(connection)
    }

    async fn steering_lane(&self, session_id: &str) -> SteeringLane {
        let mut steering_by_session = self.steering_by_session.lock().await;
        steering_by_session
            .entry(session_id.to_owned())
            .or_default()
            .clone()
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

    fn fork_parameters(
        source_thread_id: String,
        last_turn_id: String,
        request: &BackendRequest,
    ) -> Value {
        Self::secure(
            json!({
                "threadId": source_thread_id,
                "cwd": request.workspace,
                "lastTurnId": last_turn_id,
                "excludeTurns": true,
            }),
            request,
        )
    }

    fn catalog_backend_request(request: &BackendCatalogRequest) -> BackendRequest {
        BackendRequest {
            harness_session_id: request.harness_session_id.clone(),
            workspace: request.workspace.clone(),
            input: BackendInput::from_text(""),
            mode: PromptMode::Chat,
            model: "default".into(),
            effort: "medium".into(),
            context_window: None,
            fast_mode: false,
            execution_mode: request.execution_mode,
            backend_session_id: request.backend_session_id.clone(),
            control_context: None,
        }
    }

    async fn catalog_process(
        &self,
        request: &BackendCatalogRequest,
    ) -> Result<(CodexJsonRpc, BackendOutput)> {
        let backend_request = Self::catalog_backend_request(request);
        let mut output = BackendOutput::default();
        let process = self
            .connect(&backend_request, &mut output, None)
            .await?
            .process;
        Ok((process, output))
    }

    /// Open an independent connection and load the thread that owns terminal state.
    async fn terminal_connection(
        &self,
        mut request: BackendCatalogRequest,
    ) -> Result<Option<(CodexJsonRpc, BackendOutput, String)>> {
        request.backend_session_id = self
            .active_session_id_for(&request.harness_session_id)
            .await
            .or(request.backend_session_id);
        let Some(thread_id) = request.backend_session_id.clone() else {
            return Ok(None);
        };
        let (mut process, mut output) = self.catalog_process(&request).await?;
        Self::load_terminal_thread(&mut process, &mut output, &thread_id, &request.workspace)
            .await?;
        Ok(Some((process, output, thread_id)))
    }

    async fn load_terminal_thread(
        process: &mut CodexJsonRpc,
        output: &mut BackendOutput,
        thread_id: &str,
        workspace: &str,
    ) -> Result<()> {
        process
            .request(
                "thread/resume",
                json!({ "threadId": thread_id, "cwd": workspace }),
                output,
            )
            .await
            .context("load Codex thread for background terminal request")?;
        Ok(())
    }

    async fn skill_catalog(
        process: &mut CodexJsonRpc,
        output: &mut BackendOutput,
        workspace: &str,
        force_reload: bool,
    ) -> Result<Vec<SkillDefinition>> {
        let result = process
            .request(
                "skills/list",
                json!({ "cwds": [workspace], "forceReload": force_reload }),
                output,
            )
            .await?;
        Ok(Self::parse_skill_catalog(&result))
    }

    fn parse_skill_catalog(result: &Value) -> Vec<SkillDefinition> {
        result
            .get("data")
            .and_then(Value::as_array)
            .into_iter()
            .flatten()
            .flat_map(|workspace| {
                workspace
                    .get("skills")
                    .and_then(Value::as_array)
                    .into_iter()
                    .flatten()
            })
            .filter_map(|skill| {
                Some(SkillDefinition {
                    name: skill.get("name")?.as_str()?.to_owned(),
                    description: skill
                        .get("description")
                        .and_then(Value::as_str)
                        .unwrap_or_default()
                        .to_owned(),
                    enabled: skill
                        .get("enabled")
                        .and_then(Value::as_bool)
                        .unwrap_or(true),
                    user_invocable: skill
                        .get("userInvocable")
                        .and_then(Value::as_bool)
                        .unwrap_or(true),
                    path: skill.get("path").and_then(Value::as_str).map(str::to_owned),
                    source: skill
                        .get("scope")
                        .and_then(Value::as_str)
                        .map(str::to_owned),
                    argument_hint: skill
                        .get("argumentHint")
                        .and_then(Value::as_str)
                        .map(str::to_owned),
                })
            })
            .collect()
    }

    fn parse_mcp_catalog(result: &Value, config_result: &Value) -> Vec<McpDefinition> {
        let mut server_list = result
            .get("data")
            .or_else(|| result.get("servers"))
            .and_then(Value::as_array)
            .cloned()
            .unwrap_or_default()
            .into_iter()
            .map(|server| (server, true))
            .collect::<Vec<_>>();
        let config_map = config_result
            .pointer("/config/mcp_servers")
            .or_else(|| config_result.pointer("/config/mcpServers"))
            .and_then(Value::as_object);
        let listed_name_set = server_list
            .iter()
            .filter_map(|(server, _)| server.get("name").and_then(Value::as_str))
            .map(str::to_owned)
            .collect::<HashSet<_>>();
        if let Some(config_map) = config_map {
            for name in config_map.keys() {
                if !listed_name_set.contains(name.as_str()) {
                    server_list.push((json!({ "name": name }), false));
                }
            }
        }

        let mut definition_list = server_list
            .iter()
            .filter_map(|(server, reported_status)| {
                let name = server
                    .get("name")
                    .or_else(|| server.get("server"))
                    .and_then(Value::as_str)?
                    .to_owned();
                let config = config_map.and_then(|config_map| config_map.get(&name));
                let status_text = server
                    .get("runtimeStatus")
                    .filter(|status| !status.is_null())
                    .or_else(|| server.get("status"))
                    .and_then(|status| status.as_str().or_else(|| status.get("status")?.as_str()))
                    .unwrap_or_default()
                    .to_ascii_lowercase();
                let auth_text = server
                    .get("authStatus")
                    .and_then(Value::as_str)
                    .unwrap_or_default()
                    .to_ascii_lowercase();
                let enabled = config
                    .and_then(|config| config.get("enabled"))
                    .or_else(|| server.get("enabled"))
                    .and_then(Value::as_bool)
                    .unwrap_or(!status_text.contains("disabled"));
                let tool_error = server
                    .get("toolsError")
                    .or_else(|| server.get("toolError"))
                    .and_then(Value::as_str)
                    .map(str::to_owned);
                let error = server
                    .get("error")
                    .and_then(|error| {
                        error
                            .as_str()
                            .or_else(|| error.get("message").and_then(Value::as_str))
                    })
                    .map(str::to_owned)
                    .or_else(|| tool_error.clone());
                let tool_value = server.get("tools");
                let status = if !enabled || status_text.contains("disabled") {
                    McpStatus::Disabled
                } else if auth_text == "notloggedin"
                    || auth_text.contains("needs")
                    || status_text.contains("auth")
                {
                    McpStatus::NeedsAuthentication
                } else if error.is_some()
                    || status_text.contains("fail")
                    || status_text.contains("error")
                {
                    McpStatus::Failed
                } else if status_text == "notstarted" || status_text == "cancelled" {
                    McpStatus::Unavailable
                } else if status_text.contains("start")
                    || status_text.contains("load")
                    || status_text.contains("pending")
                {
                    McpStatus::Loading
                } else if status_text.contains("connect")
                    || status_text.contains("ready")
                    || (*reported_status && tool_value.is_some())
                {
                    McpStatus::Connected
                } else {
                    McpStatus::Unavailable
                };
                let mut tools = Vec::new();
                if let Some(tool_map) = tool_value.and_then(Value::as_object) {
                    for (name, tool) in tool_map {
                        tools.push(McpToolDefinition {
                            name: tool
                                .get("name")
                                .and_then(Value::as_str)
                                .unwrap_or(name)
                                .to_owned(),
                            description: tool
                                .get("description")
                                .and_then(Value::as_str)
                                .map(str::to_owned),
                        });
                    }
                } else if let Some(tool_list) = tool_value.and_then(Value::as_array) {
                    tools.extend(tool_list.iter().filter_map(|tool| {
                        if let Some(name) = tool.as_str() {
                            return Some(McpToolDefinition {
                                name: name.to_owned(),
                                description: None,
                            });
                        }
                        Some(McpToolDefinition {
                            name: tool.get("name")?.as_str()?.to_owned(),
                            description: tool
                                .get("description")
                                .and_then(Value::as_str)
                                .map(str::to_owned),
                        })
                    }));
                }
                let token_count = tool_value.map(|tool_value| {
                    serde_json::to_vec(tool_value)
                        .map(|serialized| serialized.len().div_ceil(4) as u64)
                        .unwrap_or_default()
                });
                let transport = config
                    .and_then(|config| {
                        if config.get("command").is_some() {
                            return Some("stdio");
                        }
                        if config.get("url").is_some() {
                            return Some("http");
                        }
                        config.get("transport").and_then(|transport| {
                            transport
                                .as_str()
                                .or_else(|| transport.get("type").and_then(Value::as_str))
                        })
                    })
                    .or_else(|| {
                        server.get("transport").and_then(|transport| {
                            transport
                                .as_str()
                                .or_else(|| transport.get("type").and_then(Value::as_str))
                        })
                    })
                    .or_else(|| server.get("type").and_then(Value::as_str))
                    .unwrap_or("unknown")
                    .to_owned();
                Some(McpDefinition {
                    name,
                    transport,
                    enabled,
                    status,
                    status_detail: error,
                    token_count,
                    token_estimated: token_count.is_some(),
                    tools,
                    tool_error,
                })
            })
            .collect::<Vec<_>>();
        definition_list.sort_by(|left, right| left.name.cmp(&right.name));
        definition_list
    }

    fn quoted_config_segment(value: &str) -> String {
        format!("\"{}\"", value.replace('\\', "\\\\").replace('"', "\\\""))
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
                    default_reasoning: model
                        .get("defaultReasoningEffort")
                        .and_then(Value::as_str)
                        .map(str::to_owned),
                    reasoning: effort,
                    selected_reasoning: None,
                    context_window: Vec::new(),
                    default_context_window: None,
                    selected_context_window: None,
                    vision: model
                        .get("supportsVision")
                        .and_then(Value::as_bool)
                        .unwrap_or(false),
                    description: model
                        .get("description")
                        .and_then(Value::as_str)
                        .map(str::to_owned),
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

    fn notification_thread_turn_id<'a>(
        message: &'a Value,
        method: &str,
        thread_id: &str,
    ) -> Option<&'a str> {
        (message.pointer("/params/threadId").and_then(Value::as_str) == Some(thread_id))
            .then(|| Self::notification_turn_id(message, method))
            .flatten()
    }

    fn notification_matches_turn(
        message: &Value,
        method: &str,
        thread_id: &str,
        turn_id: &str,
    ) -> bool {
        Self::notification_thread_turn_id(message, method, thread_id) == Some(turn_id)
    }

    /// Resume native continuation on its owning reader and adopt the provider-started turn.
    async fn resume_native_goal_turn(
        &self,
        process: &mut CodexJsonRpc,
        output: &mut BackendOutput,
        request: &BackendRequest,
        thread_id: &str,
    ) -> Result<Option<(Value, bool, Vec<Value>)>> {
        process.set_activity_publication(true);
        let request_id = process
            .send_request(
                "thread/goal/set",
                json!({"threadId":thread_id,"status":"active"}),
            )
            .await?;
        self.active_turn_by_session.lock().await.insert(
            request.harness_session_id.clone(),
            CodexTurnState::Submitted {
                request: Box::new(request.clone()),
                thread_id: thread_id.to_owned(),
            },
        );
        let mut acknowledged = false;
        let mut activated = false;
        let mut turn = None;
        let mut settled = false;
        let mut observed_message_list = Vec::new();
        loop {
            let message = process.receive_message().await?;
            if let Some(result) =
                CodexJsonRpc::request_result(&message, request_id, "thread/goal/set")
            {
                let result = result?;
                if let Some(state) = json_rpc::native_goal_state(
                    &json!({
                        "method":"thread/goal/updated", "params":{"threadId":thread_id,"goal":result.get("goal")}
                    }),
                    thread_id,
                ) {
                    self.trace.record(
                        &request.harness_session_id,
                        "goal.resume.admission",
                        json!({"phase":"acknowledgment","status":state,"enabled":activated}),
                    );
                    activated = true;
                    if !settled {
                        settled = state != crate::goal::GoalState::Active;
                        output.evidence.native_state = Some(state);
                    }
                }
                acknowledged = true;
            }
            if message.pointer("/params/threadId").and_then(Value::as_str) == Some(thread_id)
                && let Some(turn_id) = Self::notification_turn_id(&message, "turn/started")
            {
                activated = true;
                turn.get_or_insert_with(|| json!({"turn":{"id":turn_id}}));
            }
            if let Some(state) = json_rpc::native_goal_state(&message, thread_id) {
                self.trace.record(
                    &request.harness_session_id,
                    "goal.resume.admission",
                    json!({"phase":"notification","status":state,"enabled":activated}),
                );
                if state == crate::goal::GoalState::Active {
                    activated = true;
                } else if !activated {
                    continue;
                }
                settled = state != crate::goal::GoalState::Active;
            }
            if !activated {
                continue;
            }
            process.publish_message(message.clone(), output).await?;
            observed_message_list.push(message);
            if acknowledged {
                if let Some(turn) = turn {
                    return Ok(Some((turn, true, observed_message_list)));
                }
                if settled {
                    return Ok(None);
                }
            }
        }
    }

    async fn start_turn(
        &self,
        process: &mut CodexJsonRpc,
        output: &mut BackendOutput,
        request: &BackendRequest,
        thread_id: &str,
        continuing_exchange: bool,
        params: Value,
    ) -> Result<(Value, bool, Vec<Value>)> {
        let request_id = process.send_request("turn/start", params).await?;
        self.active_turn_by_session.lock().await.insert(
            request.harness_session_id.clone(),
            CodexTurnState::Submitted {
                request: Box::new(request.clone()),
                thread_id: thread_id.to_owned(),
            },
        );
        let mut observed_message_list = Vec::new();
        loop {
            let message = process.receive_message().await?;
            if let Some(response) = CodexJsonRpc::request_result(&message, request_id, "turn/start")
            {
                let turn = response?;
                let turn_id = turn
                    .pointer("/turn/id")
                    .or_else(|| turn.get("turnId"))
                    .or_else(|| turn.get("turn_id"))
                    .and_then(Value::as_str)
                    .context("Codex turn/start response omitted turn id")?;
                let provider_turn_started = observed_message_list.iter().any(|message| {
                    Self::notification_matches_turn(message, "turn/started", thread_id, turn_id)
                });
                process.set_activity_publication(true);
                let mut admitted_message_list = Vec::new();
                let mut admitted = continuing_exchange;
                for message in observed_message_list {
                    let message_turn = message
                        .pointer("/params/turnId")
                        .or_else(|| message.pointer("/params/turn/id"))
                        .and_then(Value::as_str);
                    let message_thread =
                        message.pointer("/params/threadId").and_then(Value::as_str);
                    if message_thread == Some(thread_id) && message_turn == Some(turn_id) {
                        admitted = true;
                    } else if !admitted
                        || (message_thread == Some(thread_id) && message_turn.is_some())
                    {
                        process.set_activity_publication(false);
                        process.publish_message(message, output).await?;
                        process.set_activity_publication(true);
                        continue;
                    }
                    process.publish_message(message.clone(), output).await?;
                    admitted_message_list.push(message);
                }
                return Ok((turn, provider_turn_started, admitted_message_list));
            }
            observed_message_list.push(message);
        }
    }
}

#[async_trait]
impl Backend for CodexBackend {
    async fn recap(
        &self,
        request: BackendCatalogRequest,
        model: &str,
        history: &str,
    ) -> Result<String> {
        let (mut process, mut output) = self.catalog_process(&request).await?;
        recap::generate(
            &mut process,
            &mut output,
            &request.workspace,
            model,
            history,
        )
        .await
    }
    async fn terminate_terminal(&self, request: BackendCatalogRequest, id: &str) -> Result<()> {
        let (mut process, mut output, thread_id) = self
            .terminal_connection(request)
            .await?
            .context("Codex session has no active thread")?;
        let result = process
            .request(
                "thread/backgroundTerminals/terminate",
                json!({"threadId": thread_id, "processId": id}),
                &mut output,
            )
            .await?;
        anyhow::ensure!(
            result.get("terminated").and_then(Value::as_bool) == Some(true),
            "Background terminal is no longer running or could not be terminated"
        );
        Ok(())
    }

    async fn background_terminals(
        &self,
        request: BackendCatalogRequest,
    ) -> Result<super::terminal::TerminalSnapshot> {
        let Some((mut process, mut output, thread_id)) = self.terminal_connection(request).await?
        else {
            return Ok(super::terminal::TerminalSnapshot {
                supported: true,
                terminal: Vec::new(),
            });
        };
        let mut snapshot = super::terminal::TerminalSnapshot {
            supported: true,
            terminal: Vec::new(),
        };
        let mut cursor: Option<String> = None;
        let mut page_count = 0;
        loop {
            page_count += 1;
            anyhow::ensure!(
                page_count <= 16,
                "background terminal pagination exceeds 16 pages"
            );
            let result = process
                .request(
                    "thread/backgroundTerminals/list",
                    json!({"threadId": thread_id, "limit": 100, "cursor": cursor}),
                    &mut output,
                )
                .await?;
            snapshot
                .terminal
                .extend(super::terminal::TerminalSnapshot::parse(&result, false)?.terminal);
            anyhow::ensure!(
                snapshot.terminal.len() <= 1024,
                "background terminal inventory exceeds 1024 entries"
            );
            let next = result
                .get("nextCursor")
                .and_then(Value::as_str)
                .map(str::to_owned);
            if next.is_none() {
                break;
            }
            anyhow::ensure!(
                next != cursor,
                "background terminal pagination did not advance"
            );
            cursor = next;
        }
        Ok(snapshot)
    }

    async fn shutdown(&self) -> Result<()> {
        self.runtime.shutdown().await?;
        self.connection_by_session.lock().await.clear();
        self.steering_by_session.lock().await.clear();
        self.active_turn_by_session.lock().await.clear();
        self.completed_turn_by_session.lock().await.clear();
        Ok(())
    }

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
        let mut active_steering = self
            .activate_turn_control(&request.harness_session_id, event_sink.clone())
            .await?;
        let mut output = BackendOutput {
            capability: capability(),
            ..BackendOutput::default()
        };
        let coordinator_event_sink = event_sink.clone();
        let connection = self
            .persistent_connection(&request, &mut output, event_sink)
            .await?;
        let mut connection = connection.lock().await;
        let process = &mut connection.process;
        process.set_activity_publication(false);
        let request_text = request.input.text();
        let mut control_context = request
            .control_context
            .clone()
            .unwrap_or_else(|| crate::control_tools::ControlTurnContext::inactive(request.mode));
        control_context.planning_feedback =
            request.mode == PromptMode::Plan && request_text.contains("Planning feedback:");
        if control_context.plan_document.is_none() {
            control_context.plan_document = (request.mode == PromptMode::Plan)
                .then(|| super::mock_plan_document_from_prompt(&request_text))
                .flatten();
        }
        process.set_control_context(control_context);
        let resolved_model = self.resolve_model(&request, process, &mut output).await?;
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
            Some(thread_id) => {
                process
                    .request(
                        "thread/resume",
                        Self::with_model(Self::secure(
                            json!({
                                "threadId": thread_id,
                                "cwd": request.workspace,
                                "config": { "model_reasoning_effort": request.effort },
                                "serviceTier": if request.fast_mode { Value::String("fast".into()) } else { Value::Null }
                            }),
                            &request,
                        ), &request.model),
                        &mut output,
                    )
                    .await?
            }
            None => {
                process
                    .request(
                        "thread/start",
                        Self::with_model(
                            Self::secure(
                                json!({
                                    "cwd": request.workspace,
                                    "experimentalRawEvents": false,
                                    "historyMode": "legacy",
                                    "developerInstructions": super::HARNESS_SYSTEM_MESSAGE,
                                    "dynamicTools": dynamic_tool_list
                                }),
                                &request,
                            ),
                            &request.model,
                        ),
                        &mut output,
                    )
                    .await?
            }
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
        let question_contract = question_contract(request.mode, &request_text);
        let mut prompt = format!(
            "Harness interaction contract: when the user explicitly asks for interactive or multiple-choice questions, call harness_question_ask with the complete question set. {question_contract} The question tools work outside planning. Do not claim control actions through prose.\n\n{}",
            request_text
        );
        if request.mode == PromptMode::GoalContinuation {
            let objective = request
                .input
                .text()
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
        }
        let mut input = vec![json!({ "type": "text", "text": prompt })];
        if let BackendInput::Skill { name, .. } = &request.input {
            let skill = Self::skill_catalog(process, &mut output, &request.workspace, true)
                .await?
                .into_iter()
                .find(|skill| skill.name == *name && skill.enabled && skill.user_invocable)
                .with_context(|| format!("skill ${name} is unavailable or disabled"))?;
            let path = skill
                .path
                .with_context(|| format!("skill ${name} omitted its provider path"))?;
            input.push(json!({ "type": "skill", "name": name, "path": path }));
        }
        let native_resume =
            request.mode == PromptMode::GoalContinuation && request_text.trim() == "/goal resume";
        let admission = if native_resume {
            self.resume_native_goal_turn(process, &mut output, &request, &thread_id)
                .await?
        } else {
            Some(self
                .start_turn(
                    process,
                    &mut output,
                    &request,
                        &thread_id,
                        false,
                        Self::with_model(
                        Self::secure(
                            json!({
                                "threadId": thread_id,
                                "input": input,
                                "cwd": request.workspace,
                                "effort": request.effort,
                                "serviceTier": if request.fast_mode { Value::String("fast".into()) } else { Value::Null }
                            }),
                            &request,
                        ),
                        &request.model,
                    ),
                )
                .await?)
        };
        let turn_id = if let Some((turn, provider_turn_started, observed_message_list)) = admission
        {
            let current_turn_id = turn
                .pointer("/turn/id")
                .or_else(|| turn.get("turnId"))
                .or_else(|| turn.get("turn_id"))
                .and_then(Value::as_str)
                .context("Codex turn/start response omitted turn id")?
                .to_owned();
            let completed = turn_coordinator::CodexTurnCoordinator::new(
                self,
                process,
                &mut output,
                &mut active_steering,
                coordinator_event_sink.clone(),
                &request,
                &thread_id,
            )
            .run(
                current_turn_id.clone(),
                provider_turn_started,
                observed_message_list,
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
            Some(
                completed
                    .pointer("/turn/id")
                    .or_else(|| completed.get("turnId"))
                    .and_then(Value::as_str)
                    .map(str::to_owned)
                    .unwrap_or(current_turn_id),
            )
        } else {
            None
        };
        self.active_turn_by_session.lock().await.insert(
            request.harness_session_id.clone(),
            CodexTurnState::Completed,
        );
        if let Some(turn_id) = turn_id {
            self.completed_turn_by_session
                .lock()
                .await
                .insert(request.harness_session_id.clone(), turn_id.clone());
            output.provider_checkpoint_id = Some(turn_id);
        }
        Ok(output)
    }

    async fn fork(&self, request: BackendForkRequest) -> Result<BackendForkResult> {
        let source_request = request.source;
        let mut output = BackendOutput::default();
        let mut connection = self.connect(&source_request, &mut output, None).await?;
        let fork_started = Instant::now();
        let result = match (
            source_request.backend_session_id.clone(),
            request.checkpoint_id.clone(),
        ) {
            (Some(source), Some(last_turn_id)) => {
                connection
                    .process
                    .request(
                        "thread/fork",
                        Self::fork_parameters(source, last_turn_id, &source_request),
                        &mut output,
                    )
                    .await?
            }
            _ => {
                connection
                    .process
                    .request(
                        "thread/start",
                        Self::with_model(
                            Self::secure(
                                json!({
                                    "cwd": source_request.workspace,
                                    "historyMode": "legacy",
                                }),
                                &source_request,
                            ),
                            &source_request.model,
                        ),
                        &mut output,
                    )
                    .await?
            }
        };
        connection.timing.push(BackendTimingRecord {
            phase: "codex.thread_fork".into(),
            duration_ms: fork_started.elapsed().as_secs_f64() * 1000.0,
        });
        let backend_session_id = result
            .pointer("/thread/id")
            .or_else(|| result.get("threadId"))
            .or_else(|| result.get("thread_id"))
            .and_then(Value::as_str)
            .map(str::to_owned)
            .context("Codex fork response omitted thread id")?;
        let timing = connection.timing.clone();
        self.connection_by_session.lock().await.insert(
            request.target_harness_session_id,
            Arc::new(Mutex::new(connection)),
        );
        Ok(BackendForkResult {
            backend_session_id,
            timing,
        })
    }

    async fn steer(&self, _text: String) -> Result<()> {
        anyhow::bail!("Codex steering requires a Harness session target")
    }

    async fn steer_session(&self, session_id: &str, text: String) -> Result<()> {
        self.steering_lane(session_id).await.steer(text).await
    }

    async fn steer_target(
        &self,
        session_id: &str,
        text: String,
        target: crate::backend::SteerTarget,
    ) -> Result<()> {
        self.steering_lane(session_id)
            .await
            .steer_target(text, target.clone())
            .await
            .with_context(|| {
                format!(
                    "Codex could not steer target {}/{}",
                    target.thread_id, target.turn_id
                )
            })
    }

    async fn interrupt_target(&self, target: crate::backend::SteerTarget) -> Result<()> {
        let steering_list = self
            .steering_by_session
            .lock()
            .await
            .values()
            .cloned()
            .collect::<Vec<_>>();
        let mut failure = Vec::new();
        for steering in steering_list {
            match steering.interrupt_target(target.clone()).await {
                Ok(()) => return Ok(()),
                Err(error) => failure.push(format!("{error:#}")),
            }
        }
        anyhow::ensure!(
            !failure.is_empty(),
            "Codex has no active turn for the requested target"
        );
        anyhow::bail!(
            "Codex could not settle target {}/{}: {}",
            target.thread_id,
            target.turn_id,
            failure.join(" | ")
        )
    }

    async fn cleanup_execution(&self, session_id: &str) -> Result<()> {
        self.steering_lane(session_id)
            .await
            .cleanup_execution()
            .await
    }

    async fn active_session_id(&self) -> Option<String> {
        self.active_session_id_for("").await
    }

    async fn active_session_id_for(&self, session_id: &str) -> Option<String> {
        match self.active_turn_by_session.lock().await.get(session_id) {
            Some(CodexTurnState::Submitted { thread_id, .. }) => Some(thread_id.clone()),
            Some(CodexTurnState::Idle | CodexTurnState::Pending | CodexTurnState::Completed)
            | None => None,
        }
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
        let mut process = self.connect(&request, &mut output, None).await?.process;
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
        let mut process = self.connect(&request, &mut output, None).await?.process;
        let model_list = Self::model_catalog(&mut process, &mut output).await?;
        if let Some(model) = model_list.iter().find(|model| model.is_default) {
            *self.default_model.lock().await = Some(model.id.clone());
        }
        Ok(model_list)
    }

    async fn skill_list(&self, request: BackendCatalogRequest) -> Result<Vec<SkillDefinition>> {
        let (mut process, mut output) = self.catalog_process(&request).await?;
        Self::skill_catalog(&mut process, &mut output, &request.workspace, true).await
    }

    async fn set_skill_enabled(
        &self,
        request: BackendCatalogRequest,
        name: &str,
        enabled: bool,
    ) -> Result<CatalogMutation> {
        let (mut process, mut output) = self.catalog_process(&request).await?;
        let result = process
            .request(
                "skills/config/write",
                json!({ "path": null, "name": name, "enabled": enabled }),
                &mut output,
            )
            .await?;
        let effective_enabled = result
            .get("effectiveEnabled")
            .and_then(Value::as_bool)
            .unwrap_or(enabled);
        anyhow::ensure!(
            effective_enabled == enabled,
            "Codex skill ${name} remains {} because a higher-precedence configuration overrides the change",
            if effective_enabled {
                "enabled"
            } else {
                "disabled"
            }
        );
        self.connection_by_session
            .lock()
            .await
            .remove(&request.harness_session_id);
        Ok(CatalogMutation {
            name: name.to_owned(),
            enabled,
            restart_required: false,
        })
    }

    async fn mcp_configuration(&self, request: BackendCatalogRequest) -> Result<Vec<McpDefinition>> {
        let (mut process, mut output) = self.catalog_process(&request).await?;
        let config = process.request(
            "config/read",
            json!({ "cwd": request.workspace, "includeLayers": false }),
            &mut output,
        ).await?;
        Ok(Self::parse_mcp_catalog(&Value::Null, &config))
    }

    async fn mcp_list(&self, request: BackendCatalogRequest) -> Result<Vec<McpDefinition>> {
        let (mut process, mut output) = self.catalog_process(&request).await?;
        let mut server_list = Vec::new();
        let mut cursor = None::<String>;
        let mut cursor_set = HashSet::new();
        loop {
            let page = process
                .request(
                    "mcpServerStatus/list",
                    json!({ "threadId": request.backend_session_id,
                        "detail": "toolsAndAuthOnly", "cursor": cursor }),
                    &mut output,
                )
                .await?;
            server_list.extend(
                page.get("data")
                    .and_then(Value::as_array)
                    .cloned()
                    .unwrap_or_default(),
            );
            cursor = page
                .get("nextCursor")
                .and_then(Value::as_str)
                .map(str::to_owned);
            let Some(next_cursor) = &cursor else { break };
            anyhow::ensure!(
                cursor_set.insert(next_cursor.clone()),
                "MCP inventory repeated a pagination cursor"
            );
        }
        let config = process
            .request(
                "config/read",
                json!({ "cwd": request.workspace, "includeLayers": false }),
                &mut output,
            )
            .await?;
        Ok(Self::parse_mcp_catalog(
            &json!({ "data": server_list }),
            &config,
        ))
    }

    async fn set_mcp_enabled(
        &self,
        request: BackendCatalogRequest,
        name: &str,
        enabled: bool,
    ) -> Result<CatalogMutation> {
        let restart_required = self.has_active_turn(&request.harness_session_id).await;
        let config_name = Self::quoted_config_segment(name);
        let (mut process, mut output) = self.catalog_process(&request).await?;
        let result = process
            .request(
                "config/batchWrite",
                json!({
                    "edits": [{
                        "keyPath": format!("mcp_servers.{config_name}.enabled"),
                        "value": enabled,
                        "mergeStrategy": "replace"
                    }],
                    "reloadUserConfig": true
                }),
                &mut output,
            )
            .await?;
        if result.get("status").and_then(Value::as_str) == Some("okOverridden") {
            let effective_enabled = result
                .pointer("/overriddenMetadata/effectiveValue")
                .and_then(Value::as_bool)
                .unwrap_or(enabled);
            let override_message = result
                .pointer("/overriddenMetadata/message")
                .and_then(Value::as_str)
                .unwrap_or("a higher-precedence configuration overrides the change");
            anyhow::ensure!(
                effective_enabled == enabled,
                "Codex MCP {name} remains {} because {override_message}",
                if effective_enabled {
                    "enabled"
                } else {
                    "disabled"
                }
            );
        }
        self.connection_by_session
            .lock()
            .await
            .remove(&request.harness_session_id);
        Ok(CatalogMutation {
            name: name.to_owned(),
            enabled,
            restart_required,
        })
    }

    async fn has_active_turn(&self, session_id: &str) -> bool {
        matches!(
            self.active_turn_by_session.lock().await.get(session_id),
            Some(CodexTurnState::Pending | CodexTurnState::Submitted { .. })
        )
    }

    async fn stop_goal_session(&self, session_id: &str, clear: bool) -> Result<()> {
        loop {
            let started = self.turn_started.notified();
            tokio::pin!(started);
            started.as_mut().enable();
            if matches!(
                self.active_turn_by_session.lock().await.get(session_id),
                Some(CodexTurnState::Pending | CodexTurnState::Submitted { .. })
            ) {
                break;
            }
            started.await;
        }
        let result = self.steering_lane(session_id).await.stop_goal(clear).await;
        if result.is_err()
            && matches!(
                self.active_turn_by_session.lock().await.get(session_id),
                Some(CodexTurnState::Completed)
            )
        {
            return Ok(());
        }
        result
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
        let mut process = self.connect(&request, &mut output, None).await?.process;
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
        anyhow::bail!("Codex rollback requires a Harness session target")
    }

    async fn rollback_cancelled_turn_for(&self, session_id: &str) -> Result<bool> {
        let state = self
            .active_turn_by_session
            .lock()
            .await
            .remove(session_id)
            .unwrap_or_default();
        match state {
            CodexTurnState::Pending => Ok(true),
            CodexTurnState::Submitted { request, thread_id } => {
                let mut output = BackendOutput::default();
                let mut process = self.connect(&request, &mut output, None).await?.process;
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

    #[tokio::test]
    async fn loads_a_thread_before_using_a_secondary_terminal_connection() -> Result<()> {
        use futures_util::{SinkExt, StreamExt};

        let fixture = tempfile::tempdir()?;
        let workspace = fixture.path().to_string_lossy().into_owned();
        let permission = PermissionCoordinator::transient(&workspace)?;
        let trace = Arc::new(TraceStore::open(fixture.path())?);
        let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await?;
        let endpoint = format!("ws://{}", listener.local_addr()?);
        let expected_workspace = workspace.clone();
        let server = tokio::spawn(async move {
            let (stream, _) = listener.accept().await.unwrap();
            let mut socket = tokio_tungstenite::accept_async(stream).await.unwrap();
            let request: Value =
                serde_json::from_str(socket.next().await.unwrap().unwrap().to_text().unwrap())
                    .unwrap();
            assert_eq!(request["method"], "thread/resume");
            assert_eq!(request["params"]["threadId"], "provider-thread");
            assert_eq!(request["params"]["cwd"], expected_workspace);
            socket
                .send(tokio_tungstenite::tungstenite::Message::Text(
                    json!({
                        "id": request["id"],
                        "result": { "thread": { "id": "provider-thread" } }
                    })
                    .to_string()
                    .into(),
                ))
                .await
                .unwrap();
        });
        let mut process = CodexJsonRpc::connect(
            &endpoint,
            &workspace,
            ExecutionMode::Read,
            permission,
            None,
            trace,
            "session".into(),
        )
        .await?;
        CodexBackend::load_terminal_thread(
            &mut process,
            &mut BackendOutput::default(),
            "provider-thread",
            &workspace,
        )
        .await?;
        tokio::time::timeout(std::time::Duration::from_secs(2), server).await??;
        Ok(())
    }

    #[tokio::test]
    #[ignore = "requires the installed authenticated Codex CLI and configured MCP servers"]
    async fn live_mcp_catalog() {
        let backend = CodexBackend::new(vec!["codex".into(), "app-server".into()]).unwrap();
        let result = tokio::time::timeout(
            std::time::Duration::from_secs(30),
            backend.mcp_list(BackendCatalogRequest {
                harness_session_id: "mcp-catalog-test".into(),
                workspace: std::env::current_dir()
                    .unwrap()
                    .to_string_lossy()
                    .into_owned(),
                backend_session_id: None,
                execution_mode: ExecutionMode::Read,
            }),
        )
        .await;
        backend.shutdown().await.unwrap();
        let inventory = result.expect("MCP inventory exceeded 30 seconds").unwrap();
        eprintln!("MCP inventory: {} servers", inventory.len());
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

    #[test]
    fn forks_without_replaying_provider_turns() {
        let request = BackendRequest {
            harness_session_id: "harness-session".into(),
            workspace: "workspace".into(),
            input: BackendInput::from_text(""),
            mode: PromptMode::Chat,
            model: "default".into(),
            effort: "medium".into(),
            context_window: None,
            fast_mode: false,
            execution_mode: ExecutionMode::Read,
            backend_session_id: Some("source-thread".into()),
            control_context: None,
        };
        let params = CodexBackend::fork_parameters(
            "source-thread".into(),
            "completed-turn".into(),
            &request,
        );

        assert_eq!(params.get("threadId"), Some(&json!("source-thread")));
        assert_eq!(params.get("lastTurnId"), Some(&json!("completed-turn")));
        assert_eq!(params.get("excludeTurns"), Some(&json!(true)));
    }

    #[test]
    fn parses_complete_skill_and_mcp_catalog_rows() {
        assert_eq!(
            CodexBackend::quoted_config_segment("github.enterprise"),
            "\"github.enterprise\""
        );
        let skill_list = CodexBackend::parse_skill_catalog(&json!({
            "data": [{
                "cwd": "D:/repo",
                "skills": [{
                    "name": "walkthrough",
                    "description": "Build a review walkthrough",
                    "enabled": false,
                    "path": "D:/skills/walkthrough/SKILL.md"
                }]
            }]
        }));
        assert_eq!(skill_list.len(), 1);
        assert!(!skill_list[0].enabled);
        assert!(skill_list[0].user_invocable);

        let mcp_list = CodexBackend::parse_mcp_catalog(
            &json!({
                "data": [{
                    "name": "github",
                    "authStatus": "unsupported",
                    "tools": {
                        "issue_get": {
                            "name": "issue_get",
                            "description": "Read an issue",
                            "inputSchema": { "type": "object" }
                        },
                        "issue_list": {
                            "name": "issue_list",
                            "inputSchema": { "type": "object" }
                        }
                    }
                }]
            }),
            &json!({
                "config": {
                    "mcp_servers": {
                        "github": { "command": "github-mcp-server", "enabled": true },
                        "legacy": { "url": "https://example.com/mcp", "enabled": false }
                    }
                }
            }),
        );
        assert_eq!(mcp_list.len(), 2);
        assert_eq!(mcp_list[0].name, "github");
        assert_eq!(mcp_list[0].transport, "stdio");
        assert_eq!(mcp_list[0].status, McpStatus::Connected);
        assert_eq!(mcp_list[0].tools.len(), 2);
        assert!(mcp_list[0].token_count.is_some());
        assert!(mcp_list[0].token_estimated);
        assert_eq!(mcp_list[1].name, "legacy");
        assert_eq!(mcp_list[1].transport, "http");
        assert_eq!(mcp_list[1].status, McpStatus::Disabled);
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
    fn mcp_runtime_status_takes_precedence_over_an_empty_tool_catalog() {
        for (runtime_status, expected) in [
            ("notStarted", McpStatus::Unavailable),
            ("starting", McpStatus::Loading),
            ("connected", McpStatus::Connected),
            ("authenticationRequired", McpStatus::NeedsAuthentication),
            ("failed", McpStatus::Failed),
            ("cancelled", McpStatus::Unavailable),
            ("disabled", McpStatus::Disabled),
        ] {
            let catalog = CodexBackend::parse_mcp_catalog(
                &json!({"data":[{"name":"server","runtimeStatus":runtime_status,"tools":{}}]}),
                &json!({}),
            );
            assert_eq!(catalog[0].status, expected, "{runtime_status}");
        }
        let catalog = CodexBackend::parse_mcp_catalog(
            &json!({"data":[{"name":"server","tools":{},"toolsError":"Discovery failed"}]}),
            &json!({}),
        );
        assert_eq!(catalog[0].status, McpStatus::Failed);
        assert_eq!(catalog[0].tool_error.as_deref(), Some("Discovery failed"));
    }

    #[test]
    fn planning_feedback_omits_pending_question_resolution_instructions() {
        let contract = question_contract(
            PromptMode::Plan,
            "Planning feedback:\n- Geometry: Native Arrow",
        );
        assert!(contract.contains("already been recorded and consumed"));
        assert!(contract.contains("Do not call harness_question_answer"));

        let ordinary = question_contract(PromptMode::Chat, "Use Native Arrow");
        assert!(ordinary.contains("While questions remain pending"));
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
    async fn explicit_turn_excludes_stale_activity_before_and_after_admission() -> Result<()> {
        use futures_util::{SinkExt, StreamExt};
        let fixture = tempfile::tempdir()?;
        let workspace = fixture.path().to_string_lossy().into_owned();
        let permission = PermissionCoordinator::transient(&workspace)?;
        let trace = Arc::new(TraceStore::open(fixture.path())?);
        let backend = CodexBackend::new_with_permission_coordinator(
            vec!["unused".into()],
            permission.clone(),
            trace.clone(),
        )?;
        let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await?;
        let endpoint = format!("ws://{}", listener.local_addr()?);
        let server = tokio::spawn(async move {
            let (stream, _) = listener.accept().await.unwrap();
            let mut socket = tokio_tungstenite::accept_async(stream).await.unwrap();
            for method in ["thread/resume", "turn/start"] {
                let request: Value =
                    serde_json::from_str(socket.next().await.unwrap().unwrap().to_text().unwrap())
                        .unwrap();
                assert_eq!(request["method"], method);
                let mut messages = vec![
                    json!({"method":"turn/started","params":{"threadId":"parent","turn":{"id":"old","status":"inProgress"}}}),
                    json!({"method":"item/completed","params":{"threadId":"parent","turnId":"old","item":{"id":"old-answer","type":"agentMessage","phase":"final_answer","text":"STALE"}}}),
                    json!({"method":"turn/completed","params":{"threadId":"parent","turn":{"id":"old","status":"completed"}}}),
                ];
                if method == "thread/resume" {
                    messages.push(
                        json!({"method":"thread/goal/cleared","params":{"threadId":"parent"}}),
                    );
                    messages.push(json!({"id":request["id"],"result":{"thread":{"id":"parent"}}}));
                } else {
                    messages.extend([
                        json!({"method":"turn/started","params":{"threadId":"parent","turn":{"id":"current","status":"inProgress"}}}),
                        json!({"method":"item/completed","params":{"threadId":"parent","turnId":"old","item":{"id":"old-before-ack","type":"agentMessage","phase":"final_answer","text":"STALE"}}}),
                        json!({"id":request["id"],"result":{"turn":{"id":"current"}}}),
                        json!({"method":"item/completed","params":{"threadId":"parent","turnId":"old","item":{"id":"old-after-ack","type":"agentMessage","phase":"final_answer","text":"STALE"}}}),
                        json!({"method":"turn/completed","params":{"threadId":"parent","turn":{"id":"old","status":"completed"}}}),
                        json!({"method":"item/completed","params":{"threadId":"parent","turnId":"current","item":{"id":"new-answer","type":"agentMessage","phase":"final_answer","text":"CURRENT"}}}),
                        json!({"method":"turn/completed","params":{"threadId":"parent","turn":{"id":"current","status":"completed"}}}),
                    ]);
                }
                for message in messages {
                    socket
                        .send(tokio_tungstenite::tungstenite::Message::Text(
                            message.to_string().into(),
                        ))
                        .await
                        .unwrap();
                }
            }
            let _ = socket.next().await;
        });
        let process = CodexJsonRpc::connect(
            &endpoint,
            &workspace,
            ExecutionMode::Read,
            permission,
            None,
            trace,
            "session".into(),
        )
        .await?;
        backend.connection_by_session.lock().await.insert(
            "session".into(),
            Arc::new(Mutex::new(CodexConnection {
                process,
                timing: Vec::new(),
            })),
        );
        let output = tokio::time::timeout(
            std::time::Duration::from_secs(2),
            backend.prompt_stream(
                BackendRequest {
                    harness_session_id: "session".into(),
                    workspace,
                    input: BackendInput::from_text("new prompt"),
                    mode: PromptMode::Chat,
                    model: "gpt-5.6-terra".into(),
                    effort: "medium".into(),
                    context_window: None,
                    fast_mode: false,
                    execution_mode: ExecutionMode::Read,
                    backend_session_id: Some("parent".into()),
                    control_context: None,
                },
                None,
            ),
        )
        .await??;
        assert_eq!(output.provider_checkpoint_id.as_deref(), Some("current"));
        assert_eq!(output.evidence.native_state, None);
        let encoded = serde_json::to_string(&output.event)?;
        assert!(encoded.contains("CURRENT"));
        assert!(!encoded.contains("STALE"));
        assert!(!encoded.contains("\"turn_id\":\"old\""));
        drop(backend);
        tokio::time::timeout(std::time::Duration::from_secs(2), server).await??;
        Ok(())
    }

    #[tokio::test]
    async fn native_resume_adopts_auto_started_turn_without_explicit_start() -> Result<()> {
        use futures_util::{SinkExt, StreamExt};
        for (before_ack, starts_turn) in
            [(true, true), (false, true), (true, false), (false, false)]
        {
            let fixture = tempfile::tempdir()?;
            let workspace = fixture.path().to_string_lossy().into_owned();
            let permission = PermissionCoordinator::transient(&workspace)?;
            let trace = Arc::new(TraceStore::open(fixture.path())?);
            let backend = CodexBackend::new_with_permission_coordinator(
                vec!["unused".into()],
                permission.clone(),
                trace.clone(),
            )?;
            let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await?;
            let endpoint = format!("ws://{}", listener.local_addr()?);
            let server = tokio::spawn(async move {
                let (stream, _) = listener.accept().await.unwrap();
                let mut socket = tokio_tungstenite::accept_async(stream).await.unwrap();
                let request: Value =
                    serde_json::from_str(socket.next().await.unwrap().unwrap().to_text().unwrap())
                        .unwrap();
                assert_eq!(request["method"], "thread/resume");
                assert_eq!(request["params"]["model"], "gpt-5.6-terra");
                assert_eq!(
                    request["params"]["config"]["model_reasoning_effort"],
                    "medium"
                );
                socket
                    .send(tokio_tungstenite::tungstenite::Message::Text(
                        json!({
                            "id":request["id"],"result":{"thread":{"id":"parent"}}
                        })
                        .to_string()
                        .into(),
                    ))
                    .await
                    .unwrap();
                let request: Value =
                    serde_json::from_str(socket.next().await.unwrap().unwrap().to_text().unwrap())
                        .unwrap();
                assert_eq!(
                    request["method"], "thread/goal/set",
                    "resume must not send turn/start"
                );
                assert_eq!(
                    request["params"],
                    json!({"threadId":"parent","status":"active"})
                );
                let acknowledgement =
                    json!({"id":request["id"],"result":{"goal":{"status":"active"}}});
                let mut messages = vec![
                    json!({"method":"thread/goal/updated","params":{"threadId":"parent","goal":{"status":"paused"}}}),
                    json!({"method":"thread/goal/updated","params":{"threadId":"parent","goal":{"status":"active"}}}),
                ];
                if !before_ack {
                    messages.push(acknowledgement.clone());
                }
                if starts_turn {
                    messages.extend([
                        json!({"method":"turn/started","params":{"threadId":"parent","turn":{"id":"resumed","status":"inProgress"}}}),
                        json!({"method":"item/completed","params":{"threadId":"parent","turnId":"resumed","item":{"id":"answer","type":"agentMessage","phase":"final_answer","text":"RESUMED-ONCE"}}}),
                        json!({"method":"turn/completed","params":{"threadId":"parent","turn":{"id":"resumed","status":"completed"}}}),
                    ]);
                }
                messages.push(json!({"method":"thread/goal/updated","params":{"threadId":"parent","goal":{"status":"usageLimited"}}}));
                if before_ack {
                    messages.push(acknowledgement);
                }
                for message in messages {
                    socket
                        .send(tokio_tungstenite::tungstenite::Message::Text(
                            message.to_string().into(),
                        ))
                        .await
                        .unwrap();
                }
                if let Some(Ok(message)) = socket.next().await {
                    assert!(
                        !message.is_text(),
                        "resume sent an additional provider request: {message}"
                    );
                }
            });
            let process = CodexJsonRpc::connect(
                &endpoint,
                &workspace,
                ExecutionMode::Read,
                permission,
                None,
                trace,
                "session".into(),
            )
            .await?;
            backend.connection_by_session.lock().await.insert(
                "session".into(),
                Arc::new(Mutex::new(CodexConnection {
                    process,
                    timing: Vec::new(),
                })),
            );
            let output = tokio::time::timeout(
                std::time::Duration::from_secs(2),
                backend.prompt_stream(
                    BackendRequest {
                        harness_session_id: "session".into(),
                        workspace,
                        input: BackendInput::from_text("/goal resume"),
                        mode: PromptMode::GoalContinuation,
                        model: "gpt-5.6-terra".into(),
                        effort: "medium".into(),
                        context_window: None,
                        fast_mode: false,
                        execution_mode: ExecutionMode::Read,
                        backend_session_id: Some("parent".into()),
                        control_context: None,
                    },
                    None,
                ),
            )
            .await??;
            assert_eq!(
                output.evidence.native_state,
                Some(crate::goal::GoalState::UsageLimited)
            );
            assert_eq!(
                output.provider_checkpoint_id.as_deref(),
                starts_turn.then_some("resumed")
            );
            assert!(!backend.has_active_turn("session").await);
            drop(backend);
            tokio::time::timeout(std::time::Duration::from_secs(2), server).await??;
        }
        Ok(())
    }

    #[tokio::test]
    async fn goal_control_waits_for_reader_registration() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let backend = CodexBackend::new_with_permission_coordinator(
            vec!["codex".into(), "app-server".into()],
            PermissionCoordinator::transient(fixture.path())?,
            Arc::new(TraceStore::open(fixture.path())?),
        )?;
        for clear in [false, true] {
            let mut control = Box::pin(backend.stop_goal_session("session", clear));
            assert!(
                futures_util::poll!(control.as_mut()).is_pending(),
                "goal control must wait for startup instead of reporting delivery"
            );
            let mut reader = backend.activate_turn_control("session", None).await?;
            assert!(futures_util::poll!(control.as_mut()).is_pending());
            let command = reader.receive().await.unwrap();
            assert_eq!(
                command.operation,
                if clear {
                    crate::backend::steering::ActiveTurnOperation::ClearGoal
                } else {
                    crate::backend::steering::ActiveTurnOperation::PauseGoal
                }
            );
            command.complete(Ok(())).await;
            control.await?;
            backend
                .active_turn_by_session
                .lock()
                .await
                .insert("session".into(), CodexTurnState::Completed);
        }
        Ok(())
    }

    #[tokio::test]
    async fn targeted_steering_preserves_provider_rejection() {
        let backend = CodexBackend::new(vec!["codex".into(), "app-server".into()]).unwrap();
        let lane = SteeringLane::default();
        let mut active = lane.activate(None).unwrap();
        backend
            .steering_by_session
            .lock()
            .await
            .insert("session".into(), lane);
        let other_lane = SteeringLane::default();
        let mut other_active = other_lane.activate(None).unwrap();
        backend
            .steering_by_session
            .lock()
            .await
            .insert("other-session".into(), other_lane);
        let target = crate::backend::SteerTarget {
            thread_id: "child".into(),
            turn_id: "turn".into(),
        };
        let (result, ()) = tokio::join!(
            backend.steer_target("session", "hello".into(), target),
            async {
                let command = active.receive().await.unwrap();
                command
                    .complete(Err(anyhow::anyhow!("provider rejected child steering")))
                    .await;
            }
        );
        let error = format!("{:#}", result.unwrap_err());
        assert!(error.contains("child/turn"));
        assert!(error.contains("provider rejected child steering"));
        assert!(futures_util::poll!(Box::pin(other_active.receive()).as_mut()).is_pending());
        let missing = tokio::time::timeout(
            std::time::Duration::from_secs(1),
            backend.steer_target(
                "missing-session",
                "correction".into(),
                crate::backend::SteerTarget {
                    thread_id: "child".into(),
                    turn_id: "turn".into(),
                },
            ),
        )
        .await
        .expect("missing session must not probe another active session");
        assert!(format!("{:#}", missing.unwrap_err()).contains("no active turn"));
    }

    #[tokio::test]
    async fn distinguishes_an_unsubmitted_cancel_from_a_completed_turn() {
        let backend = CodexBackend::new(vec!["codex".into(), "app-server".into()]).unwrap();
        backend
            .active_turn_by_session
            .lock()
            .await
            .insert("session-a".into(), CodexTurnState::Pending);
        assert!(
            backend
                .rollback_cancelled_turn_for("session-a")
                .await
                .unwrap()
        );
        backend
            .active_turn_by_session
            .lock()
            .await
            .insert("session-a".into(), CodexTurnState::Completed);
        assert!(
            !backend
                .rollback_cancelled_turn_for("session-a")
                .await
                .unwrap()
        );
    }
}
