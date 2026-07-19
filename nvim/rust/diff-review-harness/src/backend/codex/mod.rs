use crate::backend::approval::PermissionCoordinator;
use crate::backend::steering::SteeringLane;
use crate::backend::{
    Backend, BackendCapability, BackendCatalogRequest, BackendDescriptor, BackendEventSink,
    BackendForkRequest, BackendForkResult, BackendInput, BackendKind, BackendModel, BackendOutput,
    BackendRequest, BackendTimingRecord, CatalogCapability, CatalogMutation, McpDefinition,
    McpStatus, McpToolDefinition, PromptMode, SkillDefinition,
};
use crate::control_tools::ControlToolRegistry;
use crate::session::ExecutionMode;
use anyhow::{Context, Result};
use async_trait::async_trait;
use serde_json::{Value, json};
use std::collections::{HashMap, HashSet};
use std::sync::Arc;
use std::time::Instant;
use tokio::sync::Mutex;

mod json_rpc;
mod process;
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
    connection_by_session: Mutex<HashMap<String, Arc<Mutex<CodexConnection>>>>,
    completed_turn_by_session: Mutex<HashMap<String, String>>,
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
        catalog: CatalogCapability {
            skill: true,
            mcp: true,
            live_mcp_mutation: false,
        },
    }
}

impl CodexBackend {
    /// Build a Codex backend with an isolated conservative permission registry.
    pub fn new(command: Vec<String>) -> Result<Self> {
        let permission_coordinator = PermissionCoordinator::transient(".")?;
        Self::new_with_permission_coordinator(command, permission_coordinator)
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
            connection_by_session: Mutex::new(HashMap::new()),
            completed_turn_by_session: Mutex::new(HashMap::new()),
            permission_coordinator,
        })
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
            )
            .await?;
        let process_duration_ms = process_started.elapsed().as_secs_f64() * 1000.0;
        let initialize_started = Instant::now();
        process.request("initialize", json!({
            "clientInfo": { "name": "diff-review-harness", "title": "DiffReview Harness", "version": env!("CARGO_PKG_VERSION") },
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
                    .get("status")
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
                let error = server
                    .get("error")
                    .and_then(|error| {
                        error
                            .as_str()
                            .or_else(|| error.get("message").and_then(Value::as_str))
                    })
                    .map(str::to_owned);
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
                    tool_error: server
                        .get("toolError")
                        .and_then(Value::as_str)
                        .map(str::to_owned),
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
        self.active_turn_by_session.lock().await.insert(
            request.harness_session_id.clone(),
            CodexTurnState::Submitted {
                request: request.clone(),
                thread_id: thread_id.to_owned(),
            },
        );
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
        self.active_turn_by_session
            .lock()
            .await
            .insert(request.harness_session_id.clone(), CodexTurnState::Pending);
        let steering = self.steering_lane(&request.harness_session_id).await;
        let mut active_steering = steering.activate(event_sink.clone())?;
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
            Some(thread_id) => process.request("thread/resume", Self::secure(json!({
                "threadId": thread_id,
                "cwd": request.workspace
            }), &request), &mut output).await?,
            None => process.request("thread/start", Self::with_model(Self::secure(json!({
                "cwd": request.workspace,
                "experimentalRawEvents": false,
                "historyMode": "legacy",
                "developerInstructions": "You run inside DiffReview Harness. During planning, build the canonical JSON artifact with harness_plan_create and harness_plan_edit, request its latest version with harness_plan_read, then call harness_plan_submit with its exact ID and version. During execution, call harness_plan_task_report after each whole task and harness_plan_deviation when implementation diverges from accepted intent. Call harness_plan_question whenever a material user decision remains. While questions remain pending, use harness_question_answer only for an explicit user answer and harness_question_withdraw only when no material user decision remains. The question tools work in every mode. End the turn after a Harness question or plan control call. For a terminal goal state, call harness_goal_complete or harness_goal_blocked. Never claim a control action through ordinary prose alone.",
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
        let request_text = request.input.text();
        let mut prompt = format!(
            "Harness interaction contract: when the user explicitly asks for interactive or multiple-choice questions, call harness_plan_question with the complete question set. While questions remain pending, use harness_question_answer only for an explicit user answer and harness_question_withdraw only when no material user decision remains. The question tools work outside planning. Do not claim control actions through prose.\n\n{}",
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
        let (turn, provider_turn_started, observed_message_list) = self.start_turn(process, &mut output, &request, &thread_id, Self::with_model(Self::secure(json!({
            "threadId": thread_id,
            "input": input,
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
            process,
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
        self.active_turn_by_session.lock().await.insert(
            request.harness_session_id.clone(),
            CodexTurnState::Completed,
        );
        self.completed_turn_by_session
            .lock()
            .await
            .insert(request.harness_session_id.clone(), turn_id.to_owned());
        output.provider_checkpoint_id = Some(turn_id.to_owned());
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

    async fn steer_target(&self, text: String, target: crate::backend::SteerTarget) -> Result<()> {
        let steering_list = self
            .steering_by_session
            .lock()
            .await
            .values()
            .cloned()
            .collect::<Vec<_>>();
        for steering in steering_list {
            if steering
                .steer_target(text.clone(), target.clone())
                .await
                .is_ok()
            {
                return Ok(());
            }
        }
        anyhow::bail!("Codex has no active turn for the requested target")
    }

    async fn interrupt_target(&self, target: crate::backend::SteerTarget) -> Result<()> {
        let steering_list = self
            .steering_by_session
            .lock()
            .await
            .values()
            .cloned()
            .collect::<Vec<_>>();
        for steering in steering_list {
            if steering.interrupt_target(target.clone()).await.is_ok() {
                return Ok(());
            }
        }
        anyhow::bail!("Codex has no active turn for the requested target")
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

    async fn mcp_list(&self, request: BackendCatalogRequest) -> Result<Vec<McpDefinition>> {
        let (mut process, mut output) = self.catalog_process(&request).await?;
        let result = process
            .request(
                "mcpServerStatus/list",
                json!({ "threadId": request.backend_session_id, "detail": "full" }),
                &mut output,
            )
            .await?;
        let config = process
            .request(
                "config/read",
                json!({ "cwd": request.workspace, "includeLayers": false }),
                &mut output,
            )
            .await?;
        Ok(Self::parse_mcp_catalog(&result, &config))
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
