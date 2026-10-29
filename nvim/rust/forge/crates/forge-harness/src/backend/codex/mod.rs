pub(crate) mod approval;
use crate::backend::approval::PermissionCoordinator;
use crate::backend::steering::{ActiveSteering, SteeringLane};
use crate::backend::{
    Backend, BackendCapability, BackendCatalogRequest, BackendDescriptor, BackendEventSink,
    BackendForkRequest, BackendForkResult, BackendInput, BackendKind, BackendModel, BackendOutput,
    BackendRequest, BackendTimingRecord, CatalogCapability, CatalogMutation, McpDefinition,
    McpStatus, McpToolDefinition, PromptMode, SkillDefinition,
};
use crate::control_tools::ControlToolRegistry;
use crate::session::ApprovalMode;
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
mod request_execution;
mod resources;
mod retry;
mod runtime;
mod security;
mod text_generation;
mod turn_coordinator;

use json_rpc::CodexJsonRpc;
use resources::CodexResources;
use runtime::CodexRuntime;
use security::CodexSecurity;

struct CodexConnection {
    process: CodexJsonRpc,
    timing: Vec<BackendTimingRecord>,
}

struct TerminalConnection {
    process: CodexJsonRpc,
    output: BackendOutput,
    thread_id: String,
}

/// Owns Codex app-server thread lifecycle, prompting, permission policy, and native fork.
pub struct CodexBackend {
    runtime: CodexRuntime,
    command: Vec<String>,
    settings_root: Option<std::path::PathBuf>,
    resource_by_session: Mutex<HashMap<String, Arc<CodexResources>>>,
    default_model: Mutex<Option<String>>,
    steering_by_session: Mutex<HashMap<String, SteeringLane>>,
    active_turn_by_session: Mutex<HashMap<String, CodexTurnState>>,
    turn_started: Notify,
    connection_by_session: Mutex<HashMap<String, Arc<Mutex<CodexConnection>>>>,
    terminal_connection_by_session: Mutex<HashMap<String, Arc<Mutex<Option<TerminalConnection>>>>>,
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
        os_sandbox: BackendKind::Codex.supports_sandbox(),
        native_fork: true,
        native_compact: true,
        compaction: super::CompactionCapability {
            lifecycle: true,
            background: false,
            summary: false,
            metrics: false,
        },
        native_steer: true,
        native_turn_rollback: true,
        native_goal: true,
        model_selection: true,
        effort_selection: true,
        fast_mode: true,
        ultrafast_mode: true,
        permission_control: true,
        background_terminals: true,
        approval_mode_list: vec![ApprovalMode::Ask, ApprovalMode::Deny, ApprovalMode::Allow],
        agent: crate::agent::AgentCapability::default(),
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
        let trace = Arc::new(TraceStore::open(Path::new("."))?);
        Self::new_with_permission_coordinator(command, permission_coordinator, trace, None)
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
        settings_root: Option<std::path::PathBuf>,
    ) -> Result<Self> {
        anyhow::ensure!(
            !command.is_empty(),
            "Codex backend requires harness.backends.codex.command"
        );
        Ok(Self {
            runtime: CodexRuntime::new(command.clone()),
            command,
            settings_root,
            resource_by_session: Mutex::new(HashMap::new()),
            default_model: Mutex::new(None),
            steering_by_session: Mutex::new(HashMap::new()),
            active_turn_by_session: Mutex::new(HashMap::new()),
            turn_started: Notify::new(),
            connection_by_session: Mutex::new(HashMap::new()),
            terminal_connection_by_session: Mutex::new(HashMap::new()),
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
        let resources = if request.mode == PromptMode::Background {
            None
        } else {
            self.resources(&Self::catalog_from_request(request)).await?
        };
        let runtime = resources
            .as_ref()
            .map(|resources| &resources.runtime)
            .unwrap_or(&self.runtime);
        let mut process = runtime
            .connect(
                &request.workspace,
                request.approval_mode,
                Arc::clone(&self.permission_coordinator),
                event_sink,
                Arc::clone(&self.trace),
                request.harness_session_id.clone(),
            )
            .await?;
        process.set_execution_owner(request.owner()?).await;
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

    fn catalog_from_request(request: &BackendRequest) -> BackendCatalogRequest {
        BackendCatalogRequest {
            execution_owner: request.execution_owner.clone(),
            harness_session_id: request.harness_session_id.clone(),
            workspace: request.workspace.clone(),
            access: request.access.clone(),
            approval_mode: request.approval_mode,
            backend_session_id: request.backend_session_id.clone(),
        }
    }

    async fn resources(
        &self,
        request: &BackendCatalogRequest,
    ) -> Result<Option<Arc<CodexResources>>> {
        let Some(root) = &self.settings_root else {
            return Ok(None);
        };
        let mut resources = self.resource_by_session.lock().await;
        if let Some(resources) = resources.get(&request.runtime_key()?) {
            return Ok(Some(resources.clone()));
        }
        let configured = Arc::new(CodexResources::new(root, request, self.command.clone())?);
        resources.insert(request.runtime_key()?, configured.clone());
        Ok(Some(configured))
    }

    async fn configured_parameters(
        &self,
        mut parameter: Value,
        request: &BackendRequest,
        process: &mut CodexJsonRpc,
        output: &mut BackendOutput,
    ) -> Result<Value> {
        parameter["config"]["model_reasoning_summary"] = json!("concise");
        if request.mode == PromptMode::Background {
            let native = process
                .request(
                    "config/read",
                    json!({"includeLayers":false,"cwd":request.workspace}),
                    output,
                )
                .await?;
            let mut config = text_generation::tool_free_config();
            let servers = native
                .pointer("/config/mcp_servers")
                .and_then(Value::as_object)
                .map(|servers| {
                    servers
                        .keys()
                        .map(|name| (name.clone(), json!({"enabled":false})))
                        .collect::<serde_json::Map<String, Value>>()
                })
                .unwrap_or_default();
            config["mcp_servers"] = Value::Object(servers);
            config["model_reasoning_effort"] = json!(request.effort);
            config["model_reasoning_summary"] = json!("concise");
            config["project_doc_max_bytes"] = json!(0);
            config["hooks"] = json!({});
            parameter["config"] = config;
            parameter["baseInstructions"] = json!(request.system_message());
            parameter["developerInstructions"] = Value::Null;
            parameter["approvalPolicy"] = json!("never");
            parameter["sandbox"] = json!("read-only");
            return Ok(parameter);
        }
        if let Some(resources) = self.resources(&Self::catalog_from_request(request)).await? {
            let mut config = resources
                .configure(process, output, &request.workspace)
                .await?;
            if let Some(existing) = parameter.get("config").and_then(Value::as_object) {
                config
                    .as_object_mut()
                    .expect("resource configuration is an object")
                    .extend(existing.clone());
            }
            parameter["config"] = config;
        }
        if let Some(gate) = request
            .control_context
            .as_ref()
            .and_then(|context| context.checkpoint.as_ref())
        {
            if parameter.pointer("/config/hooks").is_none() {
                let native = process
                    .request(
                        "config/read",
                        json!({"cwd":request.workspace,"includeLayers":false}),
                        output,
                    )
                    .await?;
                parameter["config"]["hooks"] = native
                    .pointer("/config/hooks")
                    .filter(|value| value.is_object())
                    .cloned()
                    .unwrap_or(json!({}));
            }
            let hooks = parameter["config"]["hooks"]
                .as_object_mut()
                .context("Codex hooks configuration is not an object")?;
            if hooks.get("PreToolUse").is_none_or(Value::is_null) {
                hooks.insert("PreToolUse".into(), json!([]));
            }
            let pre_tool = hooks
                .entry("PreToolUse")
                .or_insert_with(|| json!([]))
                .as_array_mut()
                .context("Codex PreToolUse configuration is not an array")?;
            if !gate.ready() {
                pre_tool.push(gate.hook()?);
            }
            parameter["config"]["features.hooks"] = json!(true);
        }
        Ok(parameter)
    }

    async fn load_thread(
        &self,
        process: &mut CodexJsonRpc,
        output: &mut BackendOutput,
        request: &BackendRequest,
        ephemeral: bool,
        dynamic_tool: Vec<Value>,
    ) -> Result<String> {
        let (method, parameter) = match &request.backend_session_id {
            Some(thread) => (
                "thread/resume",
                json!({"threadId":thread,"cwd":request.workspace,
                "config":{"model_reasoning_effort":request.effort},
                "developerInstructions":request.system_message(),"serviceTier":request.service_tier}),
            ),
            None => (
                "thread/start",
                json!({"cwd":request.workspace,"ephemeral":ephemeral,
                "experimentalRawEvents":false,"historyMode":"legacy",
                "developerInstructions":request.system_message(),"dynamicTools":dynamic_tool,
                "serviceTier":request.service_tier}),
            ),
        };
        let parameter = Self::with_model(Self::secure_thread(parameter, request), &request.model);
        let parameter = self
            .configured_parameters(parameter, request, process, output)
            .await?;
        let response = process.request(method, parameter, output).await?;
        let thread = response
            .pointer("/thread/id")
            .or_else(|| response.get("threadId"))
            .or_else(|| response.get("thread_id"))
            .and_then(Value::as_str)
            .map(str::to_owned)
            .or(request.backend_session_id.clone())
            .context("Codex thread response omitted thread id")?;
        if request.mode != PromptMode::Background
            && let Some(resources) = self.resources(&Self::catalog_from_request(request)).await?
        {
            resources.retain_thread(&thread).await;
        }
        Ok(thread)
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
            .get(&request.runtime_key()?)
            .cloned()
        {
            connection
                .lock()
                .await
                .process
                .set_request_context(&request.workspace, request.approval_mode, event_sink)
                .await;
            return Ok(connection);
        }
        let connection = Arc::new(Mutex::new(self.connect(request, output, event_sink).await?));
        self.connection_by_session
            .lock()
            .await
            .insert(request.runtime_key()?, Arc::clone(&connection));
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
        CodexSecurity::new(request.approval_mode, &request.access)
            .apply_turn(params, &request.workspace);
    }

    fn secure(mut params: Value, request: &BackendRequest) -> Value {
        Self::apply_security(&mut params, request);
        params
    }

    fn secure_thread(mut params: Value, request: &BackendRequest) -> Value {
        CodexSecurity::new(request.approval_mode, &request.access)
            .apply_thread(&mut params, &request.workspace);
        params["config"]["agents.enabled"] = json!(false);
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
        Self::secure_thread(
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
            execution_owner: request.execution_owner.clone(),
            harness_session_id: request.harness_session_id.clone(),
            workspace: request.workspace.clone(),
            input: BackendInput::from_text(""),
            mode: PromptMode::Chat,
            model: "default".into(),
            effort: "medium".into(),
            context_window: None,
            service_tier: crate::backend::ServiceTier::Standard,
            access: request.access.clone(),
            approval_mode: request.approval_mode,
            backend_session_id: request.backend_session_id.clone(),
            submission: None,
            recovered_input: Vec::new(),
            control_context: None,
        }
    }

    async fn catalog_process(
        &self,
        request: &BackendCatalogRequest,
    ) -> Result<(CodexJsonRpc, BackendOutput)> {
        let backend_request = Self::catalog_backend_request(request);
        let mut output = BackendOutput::default();
        let mut process = self
            .connect(&backend_request, &mut output, None)
            .await?
            .process;
        process.set_activity_publication(false);
        Ok((process, output))
    }

    /// Open an independent connection and load the thread that owns terminal state.
    async fn terminal_connection(
        &self,
        mut request: BackendCatalogRequest,
    ) -> Result<Option<(CodexJsonRpc, BackendOutput, String)>> {
        request.backend_session_id = self
            .active_session_id_for(&request.runtime_key()?)
            .await
            .or(request.backend_session_id);
        let Some(thread_id) = request.backend_session_id.clone() else {
            return Ok(None);
        };
        let (mut process, mut output) = self.catalog_process(&request).await?;
        let resources = self.resources(&request).await?;
        Self::load_terminal_thread(
            &mut process,
            &mut output,
            &request,
            resources.as_deref(),
            std::time::Duration::from_millis(500),
        )
        .await?;
        Ok(Some((process, output, thread_id)))
    }

    async fn load_terminal_thread(
        process: &mut CodexJsonRpc,
        output: &mut BackendOutput,
        request: &BackendCatalogRequest,
        resources: Option<&CodexResources>,
        retry_delay: std::time::Duration,
    ) -> Result<()> {
        let thread_id = request
            .backend_session_id
            .as_deref()
            .context("terminal thread id missing")?;
        if let Some(resources) = resources
            && resources.thread().await.as_deref() == Some(thread_id)
        {
            return Ok(());
        }
        let mut parameter = json!({"threadId":thread_id});
        if let Some(resources) = resources {
            parameter["config"] = resources
                .configure(process, output, &request.workspace)
                .await?;
        }
        let mut retries_remaining = 4;
        loop {
            let id = process
                .send_request("thread/resume", parameter.clone())
                .await
                .context("load Codex thread for background terminal request")?;
            let (message, result) = loop {
                let message = process
                    .read_message(output)
                    .await
                    .context("load Codex thread for background terminal request")?;
                if let Some(result) = CodexJsonRpc::request_result(&message, id, "thread/resume") {
                    break (message, result);
                }
            };
            let empty_rollout = message.pointer("/error/code").and_then(Value::as_i64)
                == Some(-32603)
                && message
                    .pointer("/error/message")
                    .and_then(Value::as_str)
                    .is_some_and(|message| {
                        message.starts_with("failed to read thread:")
                            && message.contains("failed to read session metadata")
                            && message.contains(&format!("{thread_id}.jsonl"))
                            && message.ends_with(" is empty")
                    });
            if !empty_rollout || retries_remaining == 0 {
                result
                    .map(|_| ())
                    .context("load Codex thread for background terminal request")?;
                if let Some(resources) = resources {
                    resources.retain_thread(thread_id).await;
                }
                return Ok(());
            }
            retries_remaining -= 1;
            tokio::time::sleep(retry_delay).await;
        }
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
                    scope: None,
                    resource_id: None,
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
                    scope: None,
                    source_path: None,
                    resource_id: None,
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
        let mut models = Vec::new();
        let mut cursor: Option<String> = None;
        let mut seen = HashSet::new();
        loop {
            let result = process
                .request(
                    "model/list",
                    json!({ "cursor": cursor, "includeHidden": false }),
                    output,
                )
                .await?;
            models.extend(Self::parse_model_catalog(&result));
            let Some(next) = result.get("nextCursor").and_then(Value::as_str) else {
                return Ok(models);
            };
            anyhow::ensure!(
                seen.insert(next.to_owned()),
                "Codex model catalog repeated a cursor"
            );
            cursor = Some(next.to_owned());
        }
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
                    service_tiers: model
                        .get("serviceTiers")
                        .and_then(Value::as_array)
                        .into_iter()
                        .flatten()
                        .filter_map(|tier| match tier.get("id").and_then(Value::as_str) {
                            Some("priority" | "fast") => Some(crate::backend::ServiceTier::Fast),
                            Some("ultrafast") => Some(crate::backend::ServiceTier::Ultrafast),
                            _ => None,
                        })
                        .collect(),
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
        if request.model != "default"
            && request.service_tier == crate::backend::ServiceTier::Standard
        {
            return Ok(request.model.clone());
        }
        if request.service_tier == crate::backend::ServiceTier::Standard
            && let Some(model) = self.default_model.lock().await.clone()
        {
            return Ok(model);
        }
        let model_list = Self::model_catalog(process, output).await?;
        let model = model_list
            .iter()
            .find(|model| {
                if request.model == "default" {
                    model.is_default
                } else {
                    model.id == request.model
                }
            })
            .or_else(|| {
                (request.model == "default")
                    .then(|| model_list.first())
                    .flatten()
            })
            .context("Selected Codex model is not in the current catalog")?;
        request.service_tier.validate_model(model)?;
        if request.model == "default" {
            *self.default_model.lock().await = Some(model.id.clone());
        }
        Ok(model.id.clone())
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
        let objective = request
            .control_context
            .as_ref()
            .and_then(|context| context.native_goal.as_ref())
            .context("native goal resume requires its committed objective")?;
        let request_id = process
            .send_request(
                "thread/goal/set",
                json!({"threadId":thread_id,"objective":objective,"status":"active"}),
            )
            .await?;
        self.active_turn_by_session.lock().await.insert(
            request.runtime_key()?,
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
            let received = process.receive_message().await?;
            let message = match received {
                json_rpc::ReceivedMessage::Provider(message) => message,
                completed => {
                    process.publish_received(completed, output).await?;
                    continue;
                }
            };
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
        mut params: Value,
    ) -> Result<(Value, bool, Vec<Value>)> {
        let submission = if continuing_exchange
            && request
                .submission
                .as_ref()
                .map(|submission| submission.record())
                .transpose()?
                .is_some_and(|record| {
                    matches!(
                        record.delivery,
                        super::submission::SubmissionState::Accepted { .. }
                    )
                }) {
            None
        } else {
            request.submission.as_ref()
        };
        params["summary"] = json!("concise");
        if let Some(submission) = submission {
            params["clientUserMessageId"] = json!(submission.id());
            submission.sending(super::submission::SubmissionTarget::Codex {
                thread_id: thread_id.to_owned(),
            })?;
        }
        let request_id = process.send_request("turn/start", params).await?;
        self.active_turn_by_session.lock().await.insert(
            request.runtime_key()?,
            CodexTurnState::Submitted {
                request: Box::new(request.clone()),
                thread_id: thread_id.to_owned(),
            },
        );
        let mut observed_message_list = Vec::new();
        loop {
            let received = process.receive_message().await?;
            let message = match received {
                json_rpc::ReceivedMessage::Provider(message) => message,
                completed => {
                    process.publish_received(completed, output).await?;
                    continue;
                }
            };
            if let Some(response) = CodexJsonRpc::request_result(&message, request_id, "turn/start")
            {
                if response.is_err()
                    && let Some(submission) = submission
                    && matches!(
                        message.pointer("/error/code").and_then(Value::as_i64),
                        Some(-32602..=-32600)
                    )
                {
                    submission.rejected(response.as_ref().unwrap_err().to_string())?;
                }
                let turn = response?;
                if let Some(submission) = submission {
                    submission.accepted(format!("turn/start:{turn}"))?;
                }
                let turn_id = turn
                    .pointer("/turn/id")
                    .or_else(|| turn.get("turnId"))
                    .or_else(|| turn.get("turn_id"))
                    .and_then(Value::as_str)
                    .context("Codex turn/start response omitted turn id")?;
                let mut provider_turn_started = observed_message_list.iter().any(|message| {
                    Self::notification_matches_turn(message, "turn/started", thread_id, turn_id)
                });
                process.set_activity_publication(true);
                if !provider_turn_started {
                    // Acknowledged input can join a turn whose start event preceded this request.
                    process
                        .publish_message(
                            json!({
                                "method": "turn/started",
                                "params": { "threadId": thread_id, "turn": { "id": turn_id } }
                            }),
                            output,
                        )
                        .await?;
                }
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
                        provider_turn_started |= message
                            .get("method")
                            .and_then(Value::as_str)
                            .is_some_and(|method| method.starts_with("item/"));
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
    fn supports_task_steering(&self, _kind: crate::task::TaskKind) -> bool {
        false
    }

    async fn reconcile_submission(
        &self,
        submission: &super::submission::SubmissionRecord,
        _known_receipts: &std::collections::HashSet<String>,
    ) -> Result<Option<String>> {
        let super::submission::SubmissionState::Sending {
            target: super::submission::SubmissionTarget::Codex { thread_id },
        } = &submission.delivery
        else {
            return Ok(None);
        };
        let mut request = Self::catalog_from_request(&submission.request);
        request.backend_session_id = Some(thread_id.clone());
        let (mut process, mut output) = self.catalog_process(&request).await?;
        let mut cursor: Option<String> = None;
        let mut seen = HashSet::new();
        loop {
            let page = process
                .request(
                    "thread/items/list",
                    json!({"threadId":thread_id,"cursor":cursor,"sortDirection":"asc","limit":100}),
                    &mut output,
                )
                .await?;
            let rows = page
                .get("data")
                .and_then(Value::as_array)
                .context("Codex submission history omitted items")?;
            for row in rows {
                let item = row.get("item").unwrap_or(row);
                if item.get("type").and_then(Value::as_str) == Some("userMessage")
                    && item.get("clientId").and_then(Value::as_str) == Some(submission.id.as_str())
                {
                    return Ok(Some(format!("history:{thread_id}:{}", item["id"])));
                }
            }
            let Some(next) = page.get("nextCursor").and_then(Value::as_str) else {
                return Ok(None);
            };
            anyhow::ensure!(
                seen.insert(next.to_owned()),
                "Codex submission history repeated a cursor"
            );
            cursor = Some(next.to_owned());
        }
    }
    fn checkpoint_tool_gate(&self) -> bool {
        true
    }

    async fn reconcile_settings(
        &self,
        request: BackendCatalogRequest,
        setting: &crate::settings::PortableSettings,
    ) -> Result<()> {
        if self.settings_root.is_none() {
            anyhow::ensure!(
                setting.skill.is_empty() && setting.mcp.is_empty(),
                "Codex resource configuration requires the Forge settings store"
            );
            return Ok(());
        }
        let next = Arc::new(CodexResources::new(
            self.settings_root.as_ref().expect("settings root checked"),
            &request,
            self.command.clone(),
        )?);
        self.cleanup_execution(&request.runtime_key()?).await?;
        let terminal_slot = self
            .terminal_connection_by_session
            .lock()
            .await
            .entry(request.runtime_key()?)
            .or_insert_with(|| Arc::new(Mutex::new(None)))
            .clone();
        let mut terminal = terminal_slot.lock().await;
        *terminal = None;
        self.connection_by_session
            .lock()
            .await
            .remove(&request.runtime_key()?);
        if let Some(resources) = self
            .resource_by_session
            .lock()
            .await
            .remove(&request.runtime_key()?)
        {
            resources.runtime.shutdown().await?;
        }
        self.resource_by_session
            .lock()
            .await
            .insert(request.runtime_key()?, next);
        let backend_request = Self::catalog_backend_request(&request);
        let (mut process, mut output) = self.catalog_process(&request).await?;
        self.load_thread(
            &mut process,
            &mut output,
            &backend_request,
            true,
            Vec::new(),
        )
        .await?;
        Ok(())
    }

    async fn generate_text(
        &self,
        request: BackendCatalogRequest,
        purpose: super::TextGeneration,
        model: &str,
        reasoning_effort: Option<&str>,
        history: &str,
    ) -> Result<String> {
        self.trace.record(
            &request.harness_session_id,
            "codex.generation.request",
            json!({
                "purpose": purpose.name(),
                "model":model,
                "reasoning_effort":reasoning_effort,
            }),
        );
        let (mut process, mut output) = self.catalog_process(&request).await?;
        text_generation::generate(
            &mut process,
            &mut output,
            &request.workspace,
            purpose,
            model,
            reasoning_effort,
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
        mut request: BackendCatalogRequest,
    ) -> Result<super::terminal::TerminalSnapshot> {
        let runtime_key = request.runtime_key()?;
        request.backend_session_id = self
            .active_session_id_for(&runtime_key)
            .await
            .or(request.backend_session_id);
        let Some(thread_id) = request.backend_session_id.clone() else {
            return Ok(super::terminal::TerminalSnapshot {
                supported: true,
                starting: false,
                terminal: Vec::new(),
            });
        };
        let slot = self
            .terminal_connection_by_session
            .lock()
            .await
            .entry(runtime_key)
            .or_insert_with(|| Arc::new(Mutex::new(None)))
            .clone();
        let mut connection = slot.lock().await;
        if connection
            .as_ref()
            .is_none_or(|connection| connection.thread_id != thread_id)
        {
            *connection = None;
            let Some((process, output, thread_id)) = self.terminal_connection(request).await?
            else {
                anyhow::bail!("Codex thread disappeared while opening terminal inventory");
            };
            *connection = Some(TerminalConnection {
                process,
                output,
                thread_id,
            });
        }
        let TerminalConnection {
            process,
            output,
            thread_id,
        } = connection.as_mut().unwrap();
        let result = async {
            let mut snapshot = super::terminal::TerminalSnapshot {
                supported: true,
                starting: false,
                terminal: Vec::new(),
            };
            let mut cursor: Option<String> = None;
            let mut seen_cursors = std::collections::HashSet::new();
            loop {
                let result = process
                    .request(
                        "thread/backgroundTerminals/list",
                        json!({"threadId": thread_id, "limit": 100, "cursor": cursor}),
                        output,
                    )
                    .await?;
                snapshot
                    .terminal
                    .extend(super::terminal::TerminalSnapshot::parse(&result, false)?.terminal);
                let next = result
                    .get("nextCursor")
                    .and_then(Value::as_str)
                    .map(str::to_owned);
                let Some(next) = next else {
                    break;
                };
                anyhow::ensure!(
                    seen_cursors.insert(next.clone()),
                    "background terminal pagination did not advance"
                );
                cursor = Some(next);
            }
            Ok(snapshot)
        }
        .await;
        if result.is_err() {
            *connection = None;
        }
        result
    }

    async fn shutdown(&self) -> Result<()> {
        for (_, resources) in self.resource_by_session.lock().await.drain() {
            resources.runtime.shutdown().await?;
        }
        self.runtime.shutdown().await?;
        self.connection_by_session.lock().await.clear();
        self.terminal_connection_by_session.lock().await.clear();
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
        mut request: BackendRequest,
        event_sink: Option<BackendEventSink>,
    ) -> Result<BackendOutput> {
        let skills = self.permission_coordinator.skills(
            self.settings_root.as_deref(),
            &request,
            super::BackendKind::Codex,
        )?;
        let _directories = self
            .permission_coordinator
            .directories(self.settings_root.as_deref(), &request)?;
        let event_sink = skills.sink(event_sink);
        let mut active_steering = self
            .activate_turn_control(&request.runtime_key()?, event_sink.clone())
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
        let dynamic_tool_definition = ControlToolRegistry
            .definition_list_for_context(Some(&control_context), request.owner()?.is_root());
        process.set_control_context(control_context).await;
        request.model = self.resolve_model(&request, process, &mut output).await?;
        output.runtime.provider = "Codex CLI".into();
        output.runtime.model = Some(request.model.clone());
        let runtime_event = output.runtime.resolved_event();
        if let Some(sink) = coordinator_event_sink.as_ref() {
            sink.send_wait(runtime_event).await?;
        } else {
            output.event.push(runtime_event);
        }
        let dynamic_tool_list = dynamic_tool_definition
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
        let thread_id = self
            .load_thread(process, &mut output, &request, false, dynamic_tool_list)
            .await?;
        if request.backend_session_id.is_some()
            && let Some(gate) = request
                .control_context
                .as_ref()
                .and_then(|context| context.checkpoint.as_ref())
            && !gate.ready()
        {
            // Existing exec sessions accept stdin without running Codex's PreToolUse hook.
            let terminals = process
                .request(
                    "thread/backgroundTerminals/list",
                    json!({"threadId":thread_id,"limit":1}),
                    &mut output,
                )
                .await
                .and_then(|value| super::terminal::TerminalSnapshot::parse(&value, false));
            if !terminals
                .as_ref()
                .is_ok_and(|snapshot| snapshot.terminal.is_empty())
            {
                gate.wait().await?;
            }
        }
        output.backend_session_id = Some(thread_id.clone());
        let mut prompt = request_text.clone();
        let native_goal = request
            .control_context
            .as_ref()
            .and_then(|context| context.native_goal.as_deref());
        if let Some(objective) = native_goal {
            if request_text.lines().next() != Some("/goal resume") {
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
                if request_text.starts_with("/goal ") {
                    prompt = format!("Work toward the active goal: {objective}");
                }
            } else {
                prompt = "Continue working toward the active goal.".into();
            }
        }
        let recovered = request
            .recovered_input
            .iter()
            .map(BackendInput::text)
            .collect::<Vec<_>>();
        prompt = request.recovered_text(&prompt, &recovered);
        let mut input =
            crate::attachment::codex_input(prompt, &request.input_attachments()).await?;
        for name in request
            .recovered_input
            .iter()
            .chain(std::iter::once(&request.input))
            .filter_map(|input| {
                if let BackendInput::Skill { name, .. } = input {
                    Some(name)
                } else {
                    None
                }
            })
        {
            let catalog = if let Some(resources) = self
                .resources(&Self::catalog_from_request(&request))
                .await?
            {
                resources.catalog.skill.clone()
            } else {
                Self::skill_catalog(process, &mut output, &request.workspace, true).await?
            };
            let skill = catalog
                .into_iter()
                .find(|skill| skill.name == *name && skill.enabled && skill.user_invocable)
                .with_context(|| format!("skill ${name} is unavailable or disabled"))?;
            let path = skill
                .path
                .with_context(|| format!("skill ${name} omitted its provider path"))?;
            input.push(json!({ "type": "skill", "name": name, "path": path }));
        }
        let native_resume = native_goal.is_some()
            && request_text.lines().next() == Some("/goal resume")
            && request.recovered_input.is_empty();
        let resume_message = native_resume
            .then(|| {
                request_text
                    .split_once("\n\nAdditional user instructions:\n")
                    .or_else(|| request_text.split_once("\n\nHarness transition:\n"))
                    .map(|(_, text)| text.to_owned())
            })
            .flatten();
        let admission = if native_resume {
            if let Some(submission) = &request.submission {
                submission.sending(super::submission::SubmissionTarget::Untracked)?;
            }
            let admission = self
                .resume_native_goal_turn(process, &mut output, &request, &thread_id)
                .await?;
            if let Some(submission) = &request.submission {
                submission.accepted(format!("goal/resume:{thread_id}"))?;
            }
            admission
        } else {
            Some(
                self.start_turn(
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
                                "serviceTier": request.service_tier
                            }),
                            &request,
                        ),
                        &request.model,
                    ),
                )
                .await?,
            )
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
            let coordinator = turn_coordinator::CodexTurnCoordinator::new(
                self,
                process,
                &mut output,
                &mut active_steering,
                coordinator_event_sink.clone(),
                &request,
                &thread_id,
            );
            let completion = coordinator.run(
                current_turn_id.clone(),
                provider_turn_started,
                observed_message_list,
            );
            let completed = if let Some(text) = resume_message {
                let lane = self.steering_lane(&request.runtime_key()?).await;
                let (completed, delivered) = tokio::join!(completion, lane.steer(text.into()));
                delivered.context("deliver instructions to resumed native goal")?;
                completed?
            } else {
                completion.await?
            };
            let status = completed
                .pointer("/turn/status")
                .or_else(|| completed.get("status"))
                .and_then(Value::as_str)
                .unwrap_or_default();
            if status.eq_ignore_ascii_case("failed") {
                self.active_turn_by_session
                    .lock()
                    .await
                    .insert(request.runtime_key()?, CodexTurnState::Completed);
                let error = completed
                    .pointer("/turn/error")
                    .or_else(|| completed.get("error"));
                if let Some((error, code)) =
                    error.and_then(|error| retry::transient_code(error).map(|code| (error, code)))
                    && super::retry::replay_safe(&output)
                {
                    return Err(super::retry::RetryableFailure {
                        address: super::ProviderAddress {
                            owner: Some(request.owner()?),
                            thread_id: thread_id.clone(),
                            turn_id: completed
                                .pointer("/turn/id")
                                .or_else(|| completed.get("turnId"))
                                .and_then(Value::as_str)
                                .unwrap_or(&current_turn_id)
                                .to_owned(),
                        },
                        code,
                        detail: error.clone(),
                        message: format!("Codex turn failed: {completed}"),
                    }
                    .into());
                }
                anyhow::bail!("Codex turn failed: {completed}");
            }
            Some(
                completed
                    .pointer("/turn/id")
                    .or_else(|| completed.get("turnId"))
                    .and_then(Value::as_str)
                    .map(str::to_owned)
                    .unwrap_or(current_turn_id),
            )
        } else {
            anyhow::ensure!(
                resume_message.is_none(),
                "native goal settled before receiving the resume message"
            );
            None
        };
        self.active_turn_by_session
            .lock()
            .await
            .insert(request.runtime_key()?, CodexTurnState::Completed);
        if let Some(turn_id) = turn_id {
            self.completed_turn_by_session
                .lock()
                .await
                .insert(request.runtime_key()?, turn_id.clone());
            output.provider_checkpoint_id = Some(turn_id);
        }
        skills.output(&mut output)?;
        Ok(output)
    }

    async fn fork(&self, request: BackendForkRequest) -> Result<BackendForkResult> {
        let mut source_request = request.source;
        source_request.harness_session_id = request.target_harness_session_id.clone();
        let mut output = BackendOutput::default();
        let mut connection = self.connect(&source_request, &mut output, None).await?;
        let fork_started = Instant::now();
        let (method, parameter) = match (
            source_request.backend_session_id.clone(),
            request.checkpoint_id.clone(),
        ) {
            (Some(source), Some(last_turn_id)) => (
                "thread/fork",
                Self::fork_parameters(source, last_turn_id, &source_request),
            ),
            _ => (
                "thread/start",
                Self::with_model(
                    Self::secure_thread(
                        json!({"cwd":source_request.workspace,"historyMode":"legacy"}),
                        &source_request,
                    ),
                    &source_request.model,
                ),
            ),
        };
        let parameter = self
            .configured_parameters(
                parameter,
                &source_request,
                &mut connection.process,
                &mut output,
            )
            .await?;
        let result = connection
            .process
            .request(method, parameter, &mut output)
            .await?;
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
        if let Some(resources) = self
            .resources(&Self::catalog_from_request(&source_request))
            .await?
        {
            resources.retain_thread(&backend_session_id).await;
        }
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

    async fn steer_session(&self, session_id: &str, input: super::SteeringInput) -> Result<()> {
        self.steering_lane(session_id).await.steer(input).await
    }

    async fn cleanup_execution(&self, session_id: &str) -> Result<()> {
        self.steering_lane(session_id)
            .await
            .cleanup_execution()
            .await
    }

    async fn cancel_session(&self, session_id: &str) -> Result<()> {
        self.steering_lane(session_id)
            .await
            .cleanup_execution()
            .await?;
        self.connection_by_session.lock().await.remove(session_id);
        Ok(())
    }

    async fn active_session_id_for(&self, session_id: &str) -> Option<String> {
        match self.active_turn_by_session.lock().await.get(session_id) {
            Some(CodexTurnState::Submitted { thread_id, .. }) => Some(thread_id.clone()),
            Some(CodexTurnState::Idle | CodexTurnState::Pending | CodexTurnState::Completed)
            | None => None,
        }
    }

    async fn compact(&self, request: super::BackendCompactionRequest) -> Result<BackendOutput> {
        let request = request.request;
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
        self.load_thread(&mut process, &mut output, &request, false, Vec::new())
            .await?;
        output.metrics.context_usage = None;
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
            status.eq_ignore_ascii_case("completed"),
            "Codex compaction did not complete: {completed}"
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
        if let Some(resources) = self.resources(&request).await? {
            return Ok(resources.catalog.skill.clone());
        }
        let (mut process, mut output) = self.catalog_process(&request).await?;
        Self::skill_catalog(&mut process, &mut output, &request.workspace, true).await
    }

    async fn set_skill_enabled(
        &self,
        request: BackendCatalogRequest,
        name: &str,
        enabled: bool,
    ) -> Result<CatalogMutation> {
        anyhow::ensure!(
            self.settings_root.is_none(),
            "Managed skill selection must use the shared settings operation"
        );
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
            .remove(&request.runtime_key()?);
        Ok(CatalogMutation {
            name: name.to_owned(),
            enabled,
            restart_required: false,
        })
    }

    async fn mcp_configuration(
        &self,
        request: BackendCatalogRequest,
    ) -> Result<Vec<McpDefinition>> {
        if let Some(resources) = self.resources(&request).await? {
            return Ok(resources.catalog.mcp.clone());
        }
        let (mut process, mut output) = self.catalog_process(&request).await?;
        let config = process
            .request(
                "config/read",
                json!({ "cwd": request.workspace, "includeLayers": false }),
                &mut output,
            )
            .await?;
        Ok(Self::parse_mcp_catalog(&Value::Null, &config))
    }

    async fn mcp_list(&self, request: BackendCatalogRequest) -> Result<Vec<McpDefinition>> {
        let resources = self.resources(&request).await?;
        let thread = if let Some(resources) = resources.as_ref() {
            request
                .backend_session_id
                .clone()
                .or(resources.thread().await)
        } else {
            request.backend_session_id.clone()
        };
        if resources.is_some() && thread.is_none() {
            return self.mcp_configuration(request).await;
        }
        let (mut process, mut output) = self.catalog_process(&request).await?;
        if let Some(resources) = resources.as_ref()
            && resources.thread().await != thread
        {
            let mut backend_request = Self::catalog_backend_request(&request);
            backend_request.backend_session_id = thread.clone();
            self.load_thread(
                &mut process,
                &mut output,
                &backend_request,
                true,
                Vec::new(),
            )
            .await?;
        }
        let mut server_list = Vec::new();
        let mut cursor = None::<String>;
        let mut cursor_set = HashSet::new();
        loop {
            let page = process
                .request(
                    "mcpServerStatus/list",
                    json!({ "threadId": thread,
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
        let config = if let Some(resources) = resources.as_ref() {
            json!({"config":resources.configure(&mut process,&mut output,&request.workspace).await?})
        } else {
            process
                .request(
                    "config/read",
                    json!({ "cwd": request.workspace, "includeLayers": false }),
                    &mut output,
                )
                .await?
        };
        let parsed = Self::parse_mcp_catalog(&json!({ "data": server_list }), &config);
        Ok(resources
            .map(|resources| resources.mcp(parsed.clone()))
            .unwrap_or(parsed))
    }

    async fn set_mcp_enabled(
        &self,
        request: BackendCatalogRequest,
        name: &str,
        enabled: bool,
    ) -> Result<CatalogMutation> {
        anyhow::ensure!(
            self.settings_root.is_none(),
            "Managed MCP selection must use the shared settings operation"
        );
        let restart_required = self.has_active_turn(&request.runtime_key()?).await;
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
            .remove(&request.runtime_key()?);
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
    async fn checkpoint_hook_preserves_user_hooks_and_disappears_when_ready() -> Result<()> {
        use futures_util::{SinkExt, StreamExt};
        let fixture = tempfile::tempdir()?;
        let workspace = fixture.path().to_string_lossy().into_owned();
        let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await?;
        let endpoint = format!("ws://{}", listener.local_addr()?);
        let server = tokio::spawn(async move {
            let (stream, _) = listener.accept().await.unwrap();
            let mut socket = tokio_tungstenite::accept_async(stream).await.unwrap();
            for pending in [true, false] {
                let request: Value =
                    serde_json::from_str(socket.next().await.unwrap().unwrap().to_text().unwrap())
                        .unwrap();
                assert_eq!(request["method"], "config/read");
                let result = json!({"config":{"hooks":{"PreToolUse":[{"matcher":"user", "hooks":[
                    {"type":"command","command":"user-hook"}]}], "PostToolUse":null}}});
                socket
                    .send(tokio_tungstenite::tungstenite::Message::Text(
                        json!({"id":request["id"],"result":result})
                            .to_string()
                            .into(),
                    ))
                    .await
                    .unwrap();
                let request: Value =
                    serde_json::from_str(socket.next().await.unwrap().unwrap().to_text().unwrap())
                        .unwrap();
                assert_eq!(request["method"], "thread/start");
                let hooks = request["params"]["config"]["hooks"]["PreToolUse"]
                    .as_array()
                    .unwrap();
                assert_eq!(hooks.len(), if pending { 2 } else { 1 });
                assert_eq!(hooks[0]["hooks"][0]["command"], "user-hook");
                if pending {
                    assert!(
                        hooks[1]["hooks"][0]["command"]
                            .as_str()
                            .unwrap()
                            .contains(" checkpoint-wait ")
                    );
                }
                assert_eq!(request["params"]["config"]["features.hooks"], true);
                socket
                    .send(tokio_tungstenite::tungstenite::Message::Text(
                        json!({"id":request["id"],"result":{"thread":{"id":"checkpoint-thread"}}})
                            .to_string()
                            .into(),
                    ))
                    .await
                    .unwrap();
            }
        });
        let backend = CodexBackend::new(vec!["unused".into()])?;
        let mut request = CodexBackend::catalog_backend_request(&BackendCatalogRequest {
            execution_owner: None,
            harness_session_id: "checkpoint".into(),
            workspace: workspace.clone(),
            access: Default::default(),
            approval_mode: ApprovalMode::Ask,
            backend_session_id: None,
        });
        let gate = crate::checkpoint::ToolGate::new()?;
        let mut context = crate::control_tools::ControlTurnContext::inactive(PromptMode::Chat);
        context.checkpoint = Some(gate.clone());
        request.control_context = Some(context);
        let mut process = CodexJsonRpc::connect(
            &endpoint,
            &workspace,
            ApprovalMode::Ask,
            PermissionCoordinator::transient(&workspace)?,
            None,
            Arc::new(TraceStore::open(fixture.path())?),
            "checkpoint".into(),
        )
        .await?;
        backend
            .load_thread(
                &mut process,
                &mut BackendOutput::default(),
                &request,
                true,
                Vec::new(),
            )
            .await?;
        gate.complete(Ok(()))?;
        backend
            .load_thread(
                &mut process,
                &mut BackendOutput::default(),
                &request,
                true,
                Vec::new(),
            )
            .await?;
        server.await?;
        Ok(())
    }

    #[tokio::test]
    async fn loads_a_thread_before_using_a_secondary_terminal_connection() -> Result<()> {
        terminal_thread_resume_fixture(vec![json!({
            "result": { "thread": { "id": "provider-thread" } }
        })])
        .await
    }

    #[tokio::test]
    async fn terminal_thread_resume_recovers_empty_rollout_and_preserves_errors() -> Result<()> {
        let empty_rollout = json!({
            "error": {
                "code": -32603,
                "message": "failed to read thread: thread-store internal error: failed to read session metadata /sessions/rollout-provider-thread.jsonl: rollout at /sessions/rollout-provider-thread.jsonl is empty"
            }
        });
        let success = json!({ "result": { "thread": { "id": "provider-thread" } } });
        let mut recovering = vec![empty_rollout.clone(); 4];
        recovering.push(success);
        terminal_thread_resume_fixture(recovering).await?;

        let failure = terminal_thread_resume_fixture(vec![empty_rollout.clone(); 5])
            .await
            .unwrap_err();
        assert!(format!("{failure:#}").contains("rollout-provider-thread.jsonl is empty"));

        for response in [
            json!({ "error": { "code": -32603, "message": "permission denied" } }),
            json!({ "error": { "code": -32603, "message": "thread not found" } }),
            json!({ "error": {
                "code": -32602,
                "message": empty_rollout["error"]["message"]
            } }),
            json!({ "error": {
                "code": -32603,
                "message": "failed to read thread: failed to read session metadata /sessions/rollout-other-thread.jsonl: rollout at /sessions/rollout-other-thread.jsonl is empty"
            } }),
        ] {
            let expected = response["error"].to_string();
            let failure = terminal_thread_resume_fixture(vec![response])
                .await
                .unwrap_err();
            assert!(format!("{failure:#}").contains(&expected));
        }
        let failure = terminal_thread_resume_fixture(Vec::new())
            .await
            .unwrap_err();
        assert!(
            format!("{failure:#}").contains("load Codex thread for background terminal request")
        );
        Ok(())
    }

    async fn terminal_thread_resume_fixture(response: Vec<Value>) -> Result<()> {
        use futures_util::{SinkExt, StreamExt};

        let fixture = tempfile::tempdir()?;
        let workspace = fixture.path().to_string_lossy().into_owned();
        let permission = PermissionCoordinator::transient(&workspace)?;
        let trace = Arc::new(TraceStore::open(fixture.path())?);
        let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await?;
        let endpoint = format!("ws://{}", listener.local_addr()?);
        let server = tokio::spawn(async move {
            let (stream, _) = listener.accept().await.unwrap();
            let mut socket = tokio_tungstenite::accept_async(stream).await.unwrap();
            for mut response in response {
                let request: Value =
                    serde_json::from_str(socket.next().await.unwrap().unwrap().to_text().unwrap())
                        .unwrap();
                assert_eq!(request["method"], "thread/resume");
                assert_eq!(request["params"], json!({"threadId":"provider-thread"}));
                response["id"] = request["id"].clone();
                socket
                    .send(tokio_tungstenite::tungstenite::Message::Text(
                        response.to_string().into(),
                    ))
                    .await
                    .unwrap();
            }
        });
        let mut process = CodexJsonRpc::connect(
            &endpoint,
            &workspace,
            ApprovalMode::Ask,
            permission,
            None,
            trace,
            "session".into(),
        )
        .await?;
        let request = BackendCatalogRequest {
            execution_owner: None,
            harness_session_id: "session".into(),
            workspace,
            access: Default::default(),
            approval_mode: ApprovalMode::Ask,
            backend_session_id: Some("provider-thread".into()),
        };
        let result = CodexBackend::load_terminal_thread(
            &mut process,
            &mut BackendOutput::default(),
            &request,
            None,
            std::time::Duration::ZERO,
        )
        .await;
        tokio::time::timeout(std::time::Duration::from_secs(2), server).await??;
        result
    }

    #[tokio::test]
    async fn terminal_status_preserves_resources_and_does_not_resume_a_loaded_thread() -> Result<()>
    {
        use futures_util::{SinkExt, StreamExt};
        let fixture = tempfile::tempdir()?;
        let workspace = fixture.path().to_string_lossy().into_owned();
        crate::settings::SettingsStore::open(fixture.path())?.configure_catalog(fixture.path())?;
        let request = BackendCatalogRequest {
            execution_owner: None,
            harness_session_id: "session".into(),
            workspace: workspace.clone(),
            access: Default::default(),
            approval_mode: ApprovalMode::Ask,
            backend_session_id: Some("provider-thread".into()),
        };
        let resources = CodexResources::new(fixture.path(), &request, vec!["unused".into()])?;
        let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await?;
        let endpoint = format!("ws://{}", listener.local_addr()?);
        let server = tokio::spawn(async move {
            let (stream, _) = listener.accept().await.unwrap();
            let mut socket = tokio_tungstenite::accept_async(stream).await.unwrap();
            for method in [
                "config/read",
                "skills/extraRoots/set",
                "skills/list",
                "thread/resume",
                "thread/backgroundTerminals/list",
            ] {
                let message: Value =
                    serde_json::from_str(socket.next().await.unwrap().unwrap().to_text().unwrap())
                        .unwrap();
                assert_eq!(message["method"], method);
                let result = match method {
                    "config/read" => {
                        json!({"config":{"mcp_servers":{"native":{"command":"native"}}}})
                    }
                    "skills/list" => json!({"data":[]}),
                    "thread/resume" => {
                        assert_eq!(message["params"]["threadId"], "provider-thread");
                        assert_eq!(
                            message.pointer("/params/config/mcp_servers/native/enabled"),
                            Some(&json!(false))
                        );
                        assert_eq!(
                            message.pointer("/params/config/skills/bundled/enabled"),
                            Some(&json!(false))
                        );
                        assert!(message["params"].get("model").is_none());
                        assert!(message["params"].get("developerInstructions").is_none());
                        json!({"thread":{"id":"provider-thread"}})
                    }
                    _ => json!({}),
                };
                socket
                    .send(tokio_tungstenite::tungstenite::Message::Text(
                        json!({"id":message["id"],"result":result})
                            .to_string()
                            .into(),
                    ))
                    .await
                    .unwrap();
            }
        });
        let mut process = CodexJsonRpc::connect(
            &endpoint,
            &workspace,
            ApprovalMode::Ask,
            PermissionCoordinator::transient(&workspace)?,
            None,
            Arc::new(TraceStore::open(fixture.path())?),
            "session".into(),
        )
        .await?;
        let mut output = BackendOutput::default();
        CodexBackend::load_terminal_thread(
            &mut process,
            &mut output,
            &request,
            Some(&resources),
            std::time::Duration::ZERO,
        )
        .await?;
        CodexBackend::load_terminal_thread(
            &mut process,
            &mut output,
            &request,
            Some(&resources),
            std::time::Duration::ZERO,
        )
        .await?;
        process
            .request(
                "thread/backgroundTerminals/list",
                json!({"threadId":"provider-thread"}),
                &mut output,
            )
            .await?;
        tokio::time::timeout(std::time::Duration::from_secs(2), server).await??;
        Ok(())
    }

    #[tokio::test]
    async fn terminal_polling_reuses_passive_connection_across_pages_and_polls() -> Result<()> {
        use futures_util::{SinkExt, StreamExt};
        let fixture = tempfile::tempdir()?;
        let workspace = fixture.path().to_string_lossy().into_owned();
        let permission = PermissionCoordinator::transient(&workspace)?;
        let trace = Arc::new(TraceStore::open(fixture.path())?);
        let backend = CodexBackend::new_with_permission_coordinator(
            vec!["unused-provider".into()],
            permission.clone(),
            trace.clone(),
            None,
        )?;
        let request = BackendCatalogRequest {
            execution_owner: None,
            harness_session_id: "session".into(),
            workspace: workspace.clone(),
            access: Default::default(),
            approval_mode: ApprovalMode::Ask,
            backend_session_id: Some("provider-thread".into()),
        };
        let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await?;
        let endpoint = format!("ws://{}", listener.local_addr()?);
        let server = tokio::spawn(async move {
            let (stream, _) = listener.accept().await.unwrap();
            let mut socket = tokio_tungstenite::accept_async(stream).await.unwrap();
            for (cursor, next, process_id) in [
                (Value::Null, json!("next"), "one"),
                (json!("next"), Value::Null, "two"),
                (Value::Null, Value::Null, "three"),
            ] {
                let message: Value =
                    serde_json::from_str(socket.next().await.unwrap().unwrap().to_text().unwrap())
                        .unwrap();
                assert_eq!(message["method"], "thread/backgroundTerminals/list");
                assert_eq!(message["params"]["threadId"], "provider-thread");
                assert_eq!(message["params"]["cursor"], cursor);
                socket.send(tokio_tungstenite::tungstenite::Message::Text(json!({
                    "id":message["id"], "result":{"data":[{"processId":process_id,"command":"nu"}],"nextCursor":next}
                }).to_string().into())).await.unwrap();
            }
        });
        let mut process = CodexJsonRpc::connect(
            &endpoint,
            &workspace,
            ApprovalMode::Ask,
            permission,
            None,
            trace,
            "session".into(),
        )
        .await?;
        process.set_activity_publication(false);
        backend.terminal_connection_by_session.lock().await.insert(
            request.runtime_key()?,
            Arc::new(Mutex::new(Some(TerminalConnection {
                process,
                output: BackendOutput::default(),
                thread_id: "provider-thread".into(),
            }))),
        );
        let first = backend.background_terminals(request.clone()).await?;
        assert_eq!(
            first
                .terminal
                .iter()
                .map(|terminal| terminal.id.as_str())
                .collect::<Vec<_>>(),
            ["one", "two"]
        );
        let second = backend.background_terminals(request).await?;
        assert_eq!(second.terminal[0].id, "three");
        tokio::time::timeout(std::time::Duration::from_secs(2), server).await??;
        Ok(())
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
                    "serviceTiers": [{ "id": "priority" }],
                    "supportedReasoningEfforts": [{ "reasoningEffort": "medium" }]
                }
            ]
        }));
        assert_eq!(catalog[0].id, "gpt-default");
        assert!(catalog[0].is_default);
        assert_eq!(
            catalog[0].service_tiers,
            vec![crate::backend::ServiceTier::Fast]
        );
        assert!(
            crate::backend::ServiceTier::Fast
                .validate_model(&catalog[0])
                .is_ok()
        );
        assert!(
            crate::backend::ServiceTier::Ultrafast
                .validate_model(&catalog[0])
                .is_err()
        );
    }

    #[test]
    fn forks_without_replaying_provider_turns() {
        let request = BackendRequest {
            execution_owner: None,
            harness_session_id: "harness-session".into(),
            workspace: "workspace".into(),
            input: BackendInput::from_text(""),
            mode: PromptMode::Chat,
            model: "default".into(),
            effort: "medium".into(),
            context_window: None,
            service_tier: crate::backend::ServiceTier::Standard,
            access: Default::default(),
            approval_mode: ApprovalMode::Ask,
            backend_session_id: Some("source-thread".into()),
            submission: None,
            recovered_input: Vec::new(),
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

    #[tokio::test]
    async fn unsupported_model_speed_is_rejected_before_native_submission() -> Result<()> {
        use futures_util::{SinkExt, StreamExt};
        let fixture = tempfile::tempdir()?;
        let workspace = fixture.path().to_string_lossy().into_owned();
        let permission = PermissionCoordinator::transient(&workspace)?;
        let trace = Arc::new(TraceStore::open(fixture.path())?);
        let backend = CodexBackend::new_with_permission_coordinator(
            vec!["unused".into()],
            permission.clone(),
            trace.clone(),
            None,
        )?;
        let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await?;
        let endpoint = format!("ws://{}", listener.local_addr()?);
        let server = tokio::spawn(async move {
            let (stream, _) = listener.accept().await.unwrap();
            let mut socket = tokio_tungstenite::accept_async(stream).await.unwrap();
            for cursor in [Value::Null, json!("next-page")] {
                let request: Value =
                    serde_json::from_str(socket.next().await.unwrap().unwrap().to_text().unwrap())
                        .unwrap();
                assert_eq!(request["method"], "model/list");
                assert_eq!(request["params"]["cursor"], cursor);
                let result = if cursor.is_null() {
                    json!({"data":[],"nextCursor":"next-page"})
                } else {
                    json!({"data":[{"model":"gpt-6-luna","serviceTiers":[{"id":"priority"}]}]})
                };
                socket
                    .send(tokio_tungstenite::tungstenite::Message::Text(
                        json!({"id":request["id"],"result":result})
                            .to_string()
                            .into(),
                    ))
                    .await
                    .unwrap();
            }
            assert!(
                tokio::time::timeout(std::time::Duration::from_millis(50), socket.next())
                    .await
                    .is_err(),
                "unsupported model speed reached native submission"
            );
        });
        let process = CodexJsonRpc::connect(
            &endpoint,
            &workspace,
            ApprovalMode::Ask,
            permission,
            None,
            trace.clone(),
            "speed-test".into(),
        )
        .await?;
        backend.connection_by_session.lock().await.insert(
            "speed-test".into(),
            Arc::new(Mutex::new(CodexConnection {
                process,
                timing: Vec::new(),
            })),
        );
        let mut request = BackendRequest {
            execution_owner: None,
            harness_session_id: "speed-test".into(),
            workspace,
            input: BackendInput::from_text("must not dispatch"),
            mode: PromptMode::Chat,
            model: "gpt-6-luna".into(),
            effort: "low".into(),
            context_window: None,
            service_tier: crate::backend::ServiceTier::Ultrafast,
            access: Default::default(),
            approval_mode: ApprovalMode::Ask,
            backend_session_id: None,
            submission: None,
            recovered_input: Vec::new(),
            control_context: None,
        };
        let store = super::super::submission::SubmissionStore::open(fixture.path(), trace)?;
        let submission = store.prepare(&request, BackendKind::Codex)?;
        request.submission = Some(submission.clone());
        let error = backend.prompt_stream(request, None).await.unwrap_err();
        assert!(
            error
                .to_string()
                .contains("unavailable for model gpt-6-luna"),
            "{error:#}"
        );
        assert_eq!(
            submission.record()?.delivery,
            super::super::submission::SubmissionState::Prepared
        );
        server.await?;
        Ok(())
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
        for (sends_started, service_tier, resumed, default_model) in [
            (true, crate::backend::ServiceTier::Standard, true, false),
            (false, crate::backend::ServiceTier::Fast, true, false),
            (true, crate::backend::ServiceTier::Ultrafast, true, false),
            (true, crate::backend::ServiceTier::Ultrafast, false, false),
            (true, crate::backend::ServiceTier::Ultrafast, true, true),
            (true, crate::backend::ServiceTier::Ultrafast, false, true),
        ] {
            let fixture = tempfile::tempdir()?;
            let workspace = fixture.path().to_string_lossy().into_owned();
            let permission = PermissionCoordinator::transient(&workspace)?;
            let trace = Arc::new(TraceStore::open(fixture.path())?);
            let backend = CodexBackend::new_with_permission_coordinator(
                vec!["unused".into()],
                permission.clone(),
                trace.clone(),
                None,
            )?;
            *backend.default_model.lock().await = Some("gpt-catalog-default".into());
            let expected_model = if default_model {
                "gpt-catalog-default"
            } else {
                "gpt-5.6-terra"
            };
            let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await?;
            let endpoint = format!("ws://{}", listener.local_addr()?);
            let server = tokio::spawn(async move {
                let (stream, _) = listener.accept().await.unwrap();
                let mut socket = tokio_tungstenite::accept_async(stream).await.unwrap();
                if service_tier != crate::backend::ServiceTier::Standard {
                    let request: Value = serde_json::from_str(
                        socket.next().await.unwrap().unwrap().to_text().unwrap(),
                    )
                    .unwrap();
                    assert_eq!(request["method"], "model/list");
                    socket.send(tokio_tungstenite::tungstenite::Message::Text(json!({
                        "id":request["id"], "result":{"data":[{"model":expected_model,"isDefault":true,
                        "serviceTiers":[{"id":"priority"},{"id":"ultrafast"}]}]}
                    }).to_string().into())).await.unwrap();
                }
                for method in [
                    if resumed {
                        "thread/resume"
                    } else {
                        "thread/start"
                    },
                    "turn/start",
                ] {
                    let request: Value = serde_json::from_str(
                        socket.next().await.unwrap().unwrap().to_text().unwrap(),
                    )
                    .unwrap();
                    assert_eq!(request["method"], method);
                    assert_eq!(request["params"]["model"], expected_model);
                    assert_eq!(
                        request["params"]["serviceTier"],
                        serde_json::to_value(service_tier).unwrap()
                    );
                    if method == "turn/start" {
                        assert_eq!(request["params"]["summary"], "concise");
                        uuid::Uuid::parse_str(
                            request["params"]["clientUserMessageId"].as_str().unwrap(),
                        )
                        .unwrap();
                    } else {
                        assert_eq!(
                            request["params"]["config"]["model_reasoning_summary"],
                            "concise"
                        );
                    }
                    let mut messages = vec![
                        json!({"method":"turn/started","params":{"threadId":"parent","turn":{"id":"old","status":"inProgress"}}}),
                        json!({"method":"item/completed","params":{"threadId":"parent","turnId":"old","item":{"id":"old-answer","type":"agentMessage","phase":"final_answer","text":"STALE"}}}),
                        json!({"method":"turn/completed","params":{"threadId":"parent","turn":{"id":"old","status":"completed"}}}),
                    ];
                    if method != "turn/start" {
                        let instructions =
                            request["params"]["developerInstructions"].as_str().unwrap();
                        assert!(instructions.contains(
                            "Approval mode, filesystem access, and task are independent"
                        ));
                        assert!(instructions.contains("Answer the current user request"));
                        messages.push(
                            json!({"method":"thread/goal/cleared","params":{"threadId":"parent"}}),
                        );
                        messages
                            .push(json!({"id":request["id"],"result":{"thread":{"id":"parent"}}}));
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
                        if !sends_started
                            && message["method"] == "turn/started"
                            && message["params"]["turn"]["id"] == "current"
                        {
                            continue;
                        }
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
                ApprovalMode::Ask,
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
            let mut request = BackendRequest {
                execution_owner: None,
                harness_session_id: "session".into(),
                workspace,
                input: BackendInput::from_text("new prompt"),
                mode: PromptMode::Chat,
                model: if default_model {
                    "default"
                } else {
                    expected_model
                }
                .into(),
                effort: "medium".into(),
                context_window: None,
                service_tier,
                access: Default::default(),
                approval_mode: ApprovalMode::Ask,
                backend_session_id: resumed.then(|| "parent".into()),
                submission: None,
                recovered_input: Vec::new(),
                control_context: None,
            };
            let journal = super::super::submission::SubmissionStore::open(
                fixture.path(),
                backend.trace.clone(),
            )?;
            let submission = journal.prepare(&request, BackendKind::Codex)?;
            request.submission = Some(submission.clone());
            let output = tokio::time::timeout(
                std::time::Duration::from_secs(2),
                backend.prompt_stream(request, None),
            )
            .await??;
            assert!(matches!(
                submission.record()?.delivery,
                super::super::submission::SubmissionState::Accepted { .. }
            ));
            assert_eq!(output.runtime.model.as_deref(), Some(expected_model));
            assert_eq!(output.provider_checkpoint_id.as_deref(), Some("current"));
            assert_eq!(output.evidence.native_state, None);
            let encoded = serde_json::to_string(&output.event)?;
            assert!(encoded.contains("CURRENT"));
            assert!(!encoded.contains("STALE"));
            assert!(!encoded.contains("\"turn_id\":\"old\""));
            let start = output
                .event
                .iter()
                .position(|event| {
                    event.turn_boundary == Some(crate::backend::TurnBoundary::Started)
                })
                .unwrap();
            let answer = output
                .event
                .iter()
                .position(|event| event.text.as_deref() == Some("CURRENT"))
                .unwrap();
            assert!(
                start < answer,
                "acknowledged turns must own their response without another start notification"
            );
            let mut exchange: crate::exchange::Exchange = serde_json::from_value(json!({
                "id":"after-cancel", "session_id":"session", "agent_id":"primary", "ordinal":2,
                "prompt":"what is this repo?", "kind":"chat", "state":"running",
                "created_at_ms":0, "attributed_matches_checkpoint":false, "node_list":[]
            }))?;
            for event in &output.event {
                assert!(exchange.observe_turn(event, 1)?);
            }
            assert_eq!(exchange.turn.len(), 1);
            let saved = serde_json::to_string(&exchange)?;
            assert!(saved.contains("CURRENT"));
            assert!(!saved.contains("STALE"));
            exchange.finish(crate::exchange::ExchangeOutcome::Complete, 7000)?;
            let rendered = crate::buffer::projection::project(
                &crate::timeline::TimelineEntry::Exchange {
                    id: exchange.id.clone(),

                    created_at_ms: 0,
                    exchange: exchange.into(),

                    agent_by_id: std::collections::HashMap::new(),
                },
                &forge_buffer::width::WidthProfile::default(),
                false,
                &std::collections::HashMap::new(),
                &std::collections::HashMap::new(),
                &mut crate::buffer::identity::DisplayRegistry::default(),
            )?;
            assert!(rendered.entry.block.iter().any(|block| {
                block
                    .text
                    .wire_rows()
                    .iter()
                    .any(|text| text.contains("CURRENT"))
            }));
            if !sends_started {
                println!(
                    "ACKNOWLEDGED_TURN_FIXTURE:{}",
                    serde_json::to_string(&rendered.entry.block)?
                );
            }
            backend.cancel_session("another-session").await?;
            assert!(
                backend
                    .connection_by_session
                    .lock()
                    .await
                    .contains_key("session")
            );
            backend.cancel_session("session").await?;
            assert!(
                !backend
                    .connection_by_session
                    .lock()
                    .await
                    .contains_key("session")
            );
            drop(backend);
            tokio::time::timeout(std::time::Duration::from_secs(2), server).await??;
        }
        Ok(())
    }

    #[tokio::test]
    async fn native_resume_adopts_auto_started_turn_without_explicit_start() -> Result<()> {
        use futures_util::{SinkExt, StreamExt};
        for (before_ack, starts_turn, message_header) in [
            (true, true, None),
            (false, true, None),
            (true, false, None),
            (false, false, None),
            (false, true, Some("Additional user instructions")),
            (false, true, Some("Harness transition")),
        ] {
            let with_message = message_header.is_some();
            let fixture = tempfile::tempdir()?;
            let workspace = fixture.path().to_string_lossy().into_owned();
            let permission = PermissionCoordinator::transient(&workspace)?;
            let trace = Arc::new(TraceStore::open(fixture.path())?);
            let backend = CodexBackend::new_with_permission_coordinator(
                vec!["unused".into()],
                permission.clone(),
                trace.clone(),
                None,
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
                let instructions = request["params"]["developerInstructions"].as_str().unwrap();
                assert!(instructions.contains("active Goal task"));
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
                    json!({"threadId":"parent","objective":"Resume test objective","status":"active"})
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
                let deferred = if with_message {
                    messages.split_off(messages.len() - 2)
                } else {
                    Vec::new()
                };
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
                if with_message {
                    let request: Value = serde_json::from_str(
                        socket.next().await.unwrap().unwrap().to_text().unwrap(),
                    )
                    .unwrap();
                    assert_eq!(request["method"], "turn/steer");
                    assert_eq!(request["params"]["expectedTurnId"], "resumed");
                    assert_eq!(
                        request["params"]["input"][0]["text"],
                        "Keep the original objective and add tests"
                    );
                    socket
                        .send(tokio_tungstenite::tungstenite::Message::Text(
                            json!({"id":request["id"],"result":{"turnId":"resumed"}})
                                .to_string()
                                .into(),
                        ))
                        .await
                        .unwrap();
                    for message in deferred {
                        socket
                            .send(tokio_tungstenite::tungstenite::Message::Text(
                                message.to_string().into(),
                            ))
                            .await
                            .unwrap();
                    }
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
                ApprovalMode::Ask,
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
                        execution_owner: None,
                        harness_session_id: "session".into(),
                        workspace,
                        input: BackendInput::from_text(message_header.map_or_else(|| "/goal resume".into(), |header|
                            format!("/goal resume\n\n{header}:\nKeep the original objective and add tests"))),
                        mode: PromptMode::GoalContinuation,
                        model: "gpt-5.6-terra".into(),
                        effort: "medium".into(),
                        context_window: None,
                        service_tier: crate::backend::ServiceTier::Standard,
                        access: Default::default(),
                        approval_mode: ApprovalMode::Ask,
                        backend_session_id: Some("parent".into()),
                        submission: None,
                        recovered_input: Vec::new(),
                        control_context: Some({
                            let mut context = crate::control_tools::ControlTurnContext::inactive(PromptMode::GoalContinuation);
                            context.native_goal = Some("Resume test objective".into());
                            context
                        }),
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
            None,
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
