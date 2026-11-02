#[cfg(test)]
use crate::provider::test_support::ProviderTestExt as _;
pub(crate) mod approval;
pub(crate) mod attachments;
use crate::provider::ProviderAdapter as _;
use crate::provider::approval::PermissionCoordinator;
use crate::provider::steering::{ActiveSteering, SteeringLane};
use crate::provider::{
    CatalogCapability, CatalogMutation, McpDefinition, PromptMode, ProviderCapability,
    ProviderCatalogRequest, ProviderDescriptor, ProviderEventSink, ProviderForkRequest,
    ProviderForkResult, ProviderInput, ProviderKind, ProviderModel, ProviderOutput,
    ProviderRequest, ProviderTimingRecord, SkillDefinition,
};
use crate::session::ApprovalMode;
use crate::tools::ControlToolRegistry;
use crate::trace::TraceStore;
use anyhow::{Context, Result};
use async_trait::async_trait;
use forge_providers::codex::catalog;
use serde_json::{Value, json};
use std::collections::{HashMap, HashSet};
use std::path::Path;
use std::sync::Arc;
use std::time::Instant;
use tokio::sync::{Mutex, Notify};

mod background;
mod json_rpc;
mod normalization;
mod request_execution;
mod resources;
mod retry;
use forge_providers::codex::runtime;
use forge_providers::codex::security;
mod text_generation;
mod turn_coordinator;

use json_rpc::CodexJsonRpc;
use resources::CodexResources;
use runtime::CodexRuntime;
use security::CodexSecurity;

struct CodexConnection {
    process: CodexJsonRpc,
    timing: Vec<ProviderTimingRecord>,
    thread: Option<CodexThread>,
}

/// Distinguish an in-memory conversation from one used for provider submission.
enum CodexThread {
    Prepared(String),
    Submitted(String),
}

impl CodexThread {
    /// Get the identity owned by either lifecycle state.
    fn id(&self) -> &str {
        match self {
            Self::Prepared(id) | Self::Submitted(id) => id,
        }
    }
}

struct TerminalConnection {
    process: CodexJsonRpc,
    output: ProviderOutput,
    thread_id: String,
}

/// Owns Codex app-server thread lifecycle, prompting, permission policy, and native fork.
pub struct CodexProvider {
    session_events: Arc<crate::provider::session_events::SessionEvents>,
    runtime: CodexRuntime,
    background: background::BackgroundDelivery,
    command: Vec<String>,
    settings_root: Option<std::path::PathBuf>,
    resource_by_session: Mutex<HashMap<String, Arc<CodexResources>>>,
    default_model: Mutex<Option<String>>,
    steering_by_session: Mutex<HashMap<String, SteeringLane>>,
    active_turn_by_session: Mutex<HashMap<String, CodexTurnState>>,
    turn_started: Notify,
    connection_by_session: Mutex<HashMap<String, Arc<Mutex<CodexConnection>>>>,
    startup_by_session: Mutex<HashMap<String, Arc<Mutex<()>>>>,
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
        request: Box<ProviderRequest>,
        thread_id: String,
    },
    Completed,
}

pub(super) fn capability() -> ProviderCapability {
    ProviderCapability {
        integration: super::ProviderIntegration {
            background_generation: true, isolated_generation: true, terminal_termination: true,
            native_agent_suppression: true, mcp_runtime_status: true, mcp_tool_catalog: true,
            limitations: vec![forge_provider_api::capability::FeatureLimitation {
                id: "filesystem_access".into(), support: forge_provider_api::capability::Support::Limited,
                reason: "Scopes and write-off apply to reported file operations. Shell commands, MCP internals, and operations without permission callbacks retain provider-native restrictions. The selected read scope is not enforced as OS read confinement.".into(),
            }],
            isolation_modes: vec![
                super::IsolationMode { id: "elevated".into(), label: "Elevated".into(), description: "Dedicated low-privilege users and filesystem ACLs. Setup needs admin approval; sandboxed commands do not run as administrator.".into() },
                super::IsolationMode { id: "unelevated".into(), label: "Unelevated".into(), description: "Restricted token for the current user without admin setup. Weaker network isolation; denied-read paths are unsupported.".into() },
                super::IsolationMode { id: "mxc".into(), label: "MXC".into(), description: "Prefer Microsoft Execution Containers where supported; falls back to Elevated. Child processes stop when the command exits.".into() },
            ],
            tracked_submission: true, resource_target: Some("codexcli".into()),
            ..Default::default()
        },
        os_sandbox: true,
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

impl CodexProvider {
    /// Build a Codex provider with an isolated conservative permission registry.
    pub fn new(command: Vec<String>) -> Result<Self> {
        let permission_coordinator = PermissionCoordinator::transient(".")?;
        let trace = Arc::new(TraceStore::open(Path::new("."))?);
        Self::new_with_permission_coordinator(command, permission_coordinator, trace, None)
    }

    /// Return how many Codex app-server process generations this provider launched.
    pub fn app_server_start_count(&self) -> u64 {
        self.runtime.start_count()
    }

    /// Return the operating-system process identifier for the shared app-server.
    pub async fn app_server_process_id(&self) -> Option<u32> {
        self.runtime.process_id().await
    }

    /// Build a Codex provider with the shared Harness permission coordinator.
    pub fn new_with_permission_coordinator(
        command: Vec<String>,
        permission_coordinator: Arc<PermissionCoordinator>,
        trace: Arc<TraceStore>,
        settings_root: Option<std::path::PathBuf>,
    ) -> Result<Self> {
        anyhow::ensure!(
            !command.is_empty(),
            "Codex provider requires harness.providers.codex.command"
        );
        Ok(Self {
            session_events: Default::default(),
            runtime: CodexRuntime::new(command.clone()),
            background: background::BackgroundDelivery::default(),
            command,
            settings_root,
            resource_by_session: Mutex::new(HashMap::new()),
            default_model: Mutex::new(None),
            steering_by_session: Mutex::new(HashMap::new()),
            active_turn_by_session: Mutex::new(HashMap::new()),
            turn_started: Notify::new(),
            connection_by_session: Mutex::new(HashMap::new()),
            startup_by_session: Mutex::new(HashMap::new()),
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
        event_sink: Option<ProviderEventSink>,
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
        request: &ProviderRequest,
        output: &mut ProviderOutput,
        event_sink: Option<ProviderEventSink>,
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
        let mut process = CodexJsonRpc::from_connection(
            runtime.connect().await?,
            &request.workspace,
            request.approval_mode,
            Arc::clone(&self.permission_coordinator),
            event_sink,
            Arc::clone(&self.trace),
            request.harness_session_id.clone(),
        )?;
        process.set_execution_owner(request.owner()?).await;
        let process_duration_ms = process_started.elapsed().as_secs_f64() * 1000.0;
        let initialize_started = Instant::now();
        let initialized = process.request("initialize", json!({
            "clientInfo": { "name": "forge-harness", "title": "Forge Harness", "version": env!("CARGO_PKG_VERSION") },
            "capabilities": { "experimentalApi": true }
        }), output).await?;
        let reported = initialized
            .get("userAgent")
            .and_then(Value::as_str)
            .and_then(|agent| {
                agent
                    .split(|character: char| {
                        character.is_whitespace() || matches!(character, '/' | '(' | ')')
                    })
                    .find(|part| semver::Version::parse(part).is_ok())
            });
        forge_provider_api::version::RuntimeRequirement::new(
            ProviderKind::Codex,
            self.command
                .first()
                .map(std::path::PathBuf::from)
                .unwrap_or_else(|| "Codex app-server".into()),
            semver::Version::new(0, 164, 0),
        )
        .validate(reported)
        .map_err(super::ProviderError::from)?;
        process.notify("initialized", Value::Null).await?;
        Ok(CodexConnection {
            process,
            thread: None,
            timing: vec![
                ProviderTimingRecord {
                    phase: "codex.process_start".into(),
                    duration_ms: process_duration_ms,
                },
                ProviderTimingRecord {
                    phase: "codex.initialize".into(),
                    duration_ms: initialize_started.elapsed().as_secs_f64() * 1000.0,
                },
            ],
        })
    }

    fn catalog_from_request(request: &ProviderRequest) -> ProviderCatalogRequest {
        ProviderCatalogRequest {
            execution_owner: request.execution_owner.clone(),
            harness_session_id: request.harness_session_id.clone(),
            workspace: request.workspace.clone(),
            access: request.access.clone(),
            approval_mode: request.approval_mode,
            provider_session_id: request.provider_session_id.clone(),
        }
    }

    async fn resources(
        &self,
        request: &ProviderCatalogRequest,
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
        request: &ProviderRequest,
        process: &mut CodexJsonRpc,
        output: &mut ProviderOutput,
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
        output: &mut ProviderOutput,
        request: &ProviderRequest,
        ephemeral: bool,
        dynamic_tool: Vec<Value>,
    ) -> Result<String> {
        let (method, parameter) = match &request.provider_session_id {
            Some(thread) => (
                "thread/resume",
                json!({"threadId":thread,"cwd":request.workspace,"excludeTurns":true,
                "config":{"model_reasoning_effort":request.effort},
                "developerInstructions":request.system_message(),"serviceTier":request.service_tier}),
            ),
            None => (
                "thread/start",
                json!({"cwd":request.workspace,"ephemeral":ephemeral,
                "experimentalRawEvents":false,"historyMode":"paginated",
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
            .or(request.provider_session_id.clone().map(Into::into))
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
        request: &ProviderRequest,
        output: &mut ProviderOutput,
        event_sink: Option<ProviderEventSink>,
    ) -> Result<Arc<Mutex<CodexConnection>>> {
        capability().validate_access(&request.access)?;
        let runtime_key = request.runtime_key()?;
        self.background.stop(&runtime_key).await?;
        let background_sink = self.background.sink(&runtime_key).await;
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
                .await?;
            connection
                .lock()
                .await
                .process
                .set_background_sink(background_sink);
            return Ok(connection);
        }
        let mut connected = self.connect(request, output, event_sink).await?;
        connected.process.set_background_sink(background_sink);
        let connection = Arc::new(Mutex::new(connected));
        self.connection_by_session
            .lock()
            .await
            .insert(request.runtime_key()?, Arc::clone(&connection));
        Ok(connection)
    }

    async fn startup_guard(&self, session_id: String) -> tokio::sync::OwnedMutexGuard<()> {
        self.startup_by_session
            .lock()
            .await
            .entry(session_id)
            .or_default()
            .clone()
            .lock_owned()
            .await
    }

    async fn steering_lane(&self, session_id: &str) -> SteeringLane {
        let mut steering_by_session = self.steering_by_session.lock().await;
        steering_by_session
            .entry(session_id.to_owned())
            .or_default()
            .clone()
    }

    fn apply_security(params: &mut Value, request: &ProviderRequest) {
        CodexSecurity::new(request.approval_mode, &request.access)
            .apply_turn(params, &request.workspace);
    }

    fn secure(mut params: Value, request: &ProviderRequest) -> Value {
        Self::apply_security(&mut params, request);
        params
    }

    fn secure_thread(mut params: Value, request: &ProviderRequest) -> Value {
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
        request: &ProviderRequest,
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

    fn catalog_provider_request(request: &ProviderCatalogRequest) -> ProviderRequest {
        ProviderRequest {
            execution_owner: request.execution_owner.clone(),
            harness_session_id: request.harness_session_id.clone(),
            workspace: request.workspace.clone(),
            input: ProviderInput::from_text(""),
            mode: PromptMode::Chat,
            model: "default".into(),
            effort: "medium".into(),
            context_window: None,
            service_tier: crate::provider::ServiceTier::Standard,
            access: request.access.clone(),
            approval_mode: request.approval_mode,
            provider_session_id: request.provider_session_id.clone(),
            submission: None,
            recovered_input: Vec::new(),
            control_context: None,
        }
    }

    async fn catalog_process(
        &self,
        request: &ProviderCatalogRequest,
    ) -> Result<(CodexJsonRpc, ProviderOutput)> {
        capability().validate_access(&request.access)?;
        let provider_request = Self::catalog_provider_request(request);
        let mut output = ProviderOutput::default();
        let mut process = self
            .connect(&provider_request, &mut output, None)
            .await?
            .process;
        process.set_activity_publication(false);
        Ok((process, output))
    }

    /// Load a thread before sending an observer request on an independent connection.
    async fn thread_connection(
        &self,
        mut request: ProviderCatalogRequest,
    ) -> Result<Option<(CodexJsonRpc, ProviderOutput, String)>> {
        request.provider_session_id = self
            .active_session_id_for(&request.runtime_key()?)
            .await?
            .or(request.provider_session_id);
        let Some(thread_id) = request.provider_session_id.clone().map(String::from) else {
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
        output: &mut ProviderOutput,
        request: &ProviderCatalogRequest,
        resources: Option<&CodexResources>,
        retry_delay: std::time::Duration,
    ) -> Result<()> {
        let thread_id = request
            .provider_session_id
            .as_ref()
            .map(|id| id.as_str())
            .context("terminal thread id missing")?;
        if let Some(resources) = resources
            && resources.thread().await.as_deref() == Some(thread_id)
        {
            return Ok(());
        }
        let mut parameter = json!({"threadId":thread_id,"excludeTurns":true});
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
                if let Some(result) =
                    forge_providers::codex::protocol::request_result(&message, id, "thread/resume")
                {
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
        output: &mut ProviderOutput,
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
        Ok(catalog::parse_skill_catalog(&result))
    }

    async fn model_catalog(
        process: &mut CodexJsonRpc,
        output: &mut ProviderOutput,
    ) -> Result<Vec<ProviderModel>> {
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
            models.extend(catalog::parse_model_catalog(&result));
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

    async fn resolve_model(
        &self,
        request: &ProviderRequest,
        process: &mut CodexJsonRpc,
        output: &mut ProviderOutput,
    ) -> Result<String> {
        if request.model != "default"
            && request.service_tier == crate::provider::ServiceTier::Standard
        {
            return Ok(request.model.clone());
        }
        if request.service_tier == crate::provider::ServiceTier::Standard
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
        output: &mut ProviderOutput,
        request: &ProviderRequest,
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
            if let Some(result) = forge_providers::codex::protocol::request_result(
                &message,
                request_id,
                "thread/goal/set",
            ) {
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
                        output.completion.evidence.native_state = Some(state);
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
        output: &mut ProviderOutput,
        request: &ProviderRequest,
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
            submission.sending(super::submission::SubmissionTarget::native(
                "codex",
                thread_id.to_owned(),
                serde_json::json!({}),
            ))?;
        }
        let acknowledgment_deadline =
            tokio::time::Instant::now() + std::time::Duration::from_secs(30);
        let request_id = tokio::time::timeout_at(
            acknowledgment_deadline,
            process.send_request("turn/start", params),
        )
        .await
        .map_err(|_| super::ProviderError::DeliveryUncertain {
            message: "Codex initial input dispatch exceeded 30 seconds".into(),
        })??;
        self.active_turn_by_session.lock().await.insert(
            request.runtime_key()?,
            CodexTurnState::Submitted {
                request: Box::new(request.clone()),
                thread_id: thread_id.to_owned(),
            },
        );
        let mut observed_message_list = Vec::new();
        loop {
            let received =
                tokio::time::timeout_at(acknowledgment_deadline, process.receive_message())
                    .await
                    .map_err(|_| super::ProviderError::DeliveryUncertain {
                        message: "Codex initial input acknowledgment exceeded 30 seconds".into(),
                    })??;
            let message = match received {
                json_rpc::ReceivedMessage::Provider(message) => message,
                completed => {
                    process.publish_received(completed, output).await?;
                    continue;
                }
            };
            if let Some(response) =
                forge_providers::codex::protocol::request_result(&message, request_id, "turn/start")
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
impl crate::provider::ProviderAdapter for CodexProvider {
    type Host = crate::provider::HarnessProviderHost;
    /// Initialize or reuse a native thread without suspending its retained background delivery.
    async fn initialize_session(
        &self,
        mut request: ProviderRequest,
    ) -> Result<Option<forge_provider_api::ProviderSessionId>> {
        capability().validate_access(&request.access)?;
        let runtime_key = request.runtime_key()?;
        let _startup = self.startup_guard(runtime_key.clone()).await;
        let mut output = ProviderOutput::default();
        let connection = self
            .persistent_connection(&request, &mut output, None)
            .await?;
        let result = async {
            let mut connection = connection.lock().await;
            if let Some(thread) = &connection.thread {
                return Ok(Some(thread.id().to_owned().try_into()?));
            }
            request.model = self
                .resolve_model(&request, &mut connection.process, &mut output)
                .await?;
            let tools = ControlToolRegistry
                .definition_list_for_owner(&request.owner()?, true, true)
                .into_iter()
                .map(|definition| {
                    json!({
                        "type":"function", "name":definition.name, "description":definition.description,
                        "inputSchema":definition.input_schema,
                    })
                })
                .collect();
            let thread = self
                .load_thread(&mut connection.process, &mut output, &request, false, tools)
                .await?;
            connection.thread = Some(if request.provider_session_id.is_some() {
                CodexThread::Submitted(thread.clone())
            } else {
                CodexThread::Prepared(thread.clone())
            });
            Ok(Some(thread.try_into()?))
        }.await;
        self.background.start(runtime_key, connection).await?;
        result
    }

    fn supports_task_steering(&self, _kind: crate::task::TaskKind) -> bool {
        false
    }

    async fn reconcile_submission(
        &self,
        submission: &super::submission::SubmissionRecord,
        _known_receipts: &std::collections::HashSet<String>,
    ) -> Result<Option<String>> {
        let super::submission::SubmissionState::Sending { target } = &submission.delivery else {
            return Ok(None);
        };
        let Some((thread_id, _)) = target.native_checkpoint("codex", 1) else {
            return Ok(None);
        };
        let mut request = Self::catalog_from_request(&submission.request);
        request.provider_session_id = Some(thread_id.to_owned().try_into()?);
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
        request: ProviderCatalogRequest,
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
        self.background.stop(&request.runtime_key()?).await?;
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
        let provider_request = Self::catalog_provider_request(&request);
        let (mut process, mut output) = self.catalog_process(&request).await?;
        self.load_thread(
            &mut process,
            &mut output,
            &provider_request,
            true,
            Vec::new(),
        )
        .await?;
        Ok(())
    }

    async fn generate_text(
        &self,
        request: ProviderCatalogRequest,
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
    async fn terminate_terminal(&self, request: ProviderCatalogRequest, id: &str) -> Result<()> {
        let (mut process, mut output, thread_id) = self
            .thread_connection(request)
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
        mut request: ProviderCatalogRequest,
    ) -> Result<super::terminal::TerminalSnapshot> {
        let runtime_key = request.runtime_key()?;
        request.provider_session_id = self
            .active_session_id_for(&runtime_key)
            .await?
            .or(request.provider_session_id);
        let Some(thread_id) = request.provider_session_id.clone().map(String::from) else {
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
            let Some((process, output, thread_id)) = self.thread_connection(request).await? else {
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
                    .extend(forge_providers::terminal::parse_snapshot(&result, false)?.terminal);
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
        let mut failure = self
            .session_events
            .shutdown()
            .await
            .err()
            .map(anyhow::Error::from);
        if let Err(error) = self.background.stop_readers().await {
            failure.get_or_insert(error);
        }
        let connections: Vec<_> = self
            .connection_by_session
            .lock()
            .await
            .values()
            .cloned()
            .collect();
        for connection in connections {
            let mut connection = connection.lock().await;
            if let Err(error) = connection.process.shutdown_background().await {
                failure.get_or_insert(error);
            }
            if let Err(error) = connection.process.close().await {
                failure.get_or_insert(error);
            }
        }
        for (_, resources) in self.resource_by_session.lock().await.drain() {
            if let Err(error) = resources.runtime.shutdown().await {
                failure.get_or_insert(error);
            }
        }
        if let Err(error) = self.runtime.shutdown().await {
            failure.get_or_insert(error);
        }
        self.connection_by_session.lock().await.clear();
        self.startup_by_session.lock().await.clear();
        self.terminal_connection_by_session.lock().await.clear();
        self.steering_by_session.lock().await.clear();
        self.active_turn_by_session.lock().await.clear();
        self.completed_turn_by_session.lock().await.clear();
        self.background.close().await;
        match failure {
            Some(error) => Err(error),
            None => Ok(()),
        }
    }

    fn descriptor(&self) -> ProviderDescriptor {
        ProviderDescriptor {
            kind: ProviderKind::Codex,
            label: "Codex CLI".into(),
            capability: capability(),
        }
    }

    /// Subscribe to original-owner background updates independently of foreground requests.
    async fn session_events(
        &self,
        owner: crate::provider::ExecutionOwner,
    ) -> Result<crate::provider::events::ProviderEventStream, crate::provider::ProviderError> {
        let stream = self.session_events.subscribe(owner.clone())?;
        self.background
            .attach(&owner, self.session_events.session_sink(&owner)?)
            .await;
        Ok(stream)
    }

    async fn session_steer(
        self: std::sync::Arc<Self>,
        owner: &crate::provider::ExecutionOwner,
        scope: crate::provider::InvocationScope,
        input: crate::provider::SteeringInput,
    ) -> Result<(), crate::provider::ProviderError> {
        if scope.owner != *owner {
            return Err(crate::provider::ProviderError::Rejected {
                message: "steering scope belongs to another owner".into(),
            });
        }
        if !self.session_events.running(&scope).await? {
            let request = self.session_events.continuation(&scope, input)?;
            return self.session_prompt(request, scope).await;
        }
        let events = self.session_events.clone();
        let provider = self.clone();
        let continuation = input.clone();
        let result = events
            .steer(&scope, input, |input| async move {
                provider
                    .steer_native(&owner.runtime_key(), input)
                    .await
                    .map_err(crate::provider::ProviderError::from)
            })
            .await;
        if matches!(
            &result,
            Err(crate::provider::ProviderError::NotConsumed { .. })
        ) {
            events.wait_closed(&scope).await?;
            let request = events.continuation(&scope, continuation)?;
            return self.session_prompt(request, scope).await;
        }
        result
    }

    async fn session_prompt(
        self: Arc<Self>,
        request: ProviderRequest,
        scope: crate::provider::InvocationScope,
    ) -> Result<(), crate::provider::ProviderError> {
        let events = self.session_events.clone();
        events
            .submit(
                request,
                scope,
                crate::provider::TerminalBoundary::AdapterCertificate,
                move |request, sink| async move { self.run_native(request, sink).await },
            )
            .await
    }

    async fn fork(&self, request: ProviderForkRequest) -> Result<ProviderForkResult> {
        let mut source_request = request.source;
        source_request.harness_session_id = request.target_harness_session_id.clone();
        let mut output = ProviderOutput::default();
        let mut connection = self.connect(&source_request, &mut output, None).await?;
        let fork_started = Instant::now();
        let (method, parameter) = match (
            source_request.provider_session_id.clone(),
            request.checkpoint_id.clone(),
        ) {
            (Some(source), Some(last_turn_id)) => (
                "thread/fork",
                Self::fork_parameters(source.into(), last_turn_id, &source_request),
            ),
            _ => (
                "thread/start",
                Self::with_model(
                    Self::secure_thread(
                        json!({"cwd":source_request.workspace,"historyMode":"paginated"}),
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
        connection.timing.push(ProviderTimingRecord {
            phase: "codex.thread_fork".into(),
            duration_ms: fork_started.elapsed().as_secs_f64() * 1000.0,
        });
        let provider_session_id = result
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
            resources.retain_thread(&provider_session_id).await;
        }
        let timing = connection.timing.clone();
        connection.thread = Some(if request.checkpoint_id.is_some() {
            CodexThread::Submitted(provider_session_id.clone())
        } else {
            CodexThread::Prepared(provider_session_id.clone())
        });
        self.connection_by_session.lock().await.insert(
            request.target_harness_session_id,
            Arc::new(Mutex::new(connection)),
        );
        Ok(ProviderForkResult {
            provider_session_id: provider_session_id.try_into()?,
            timing,
        })
    }

    async fn cleanup_execution(&self, session_id: &str) -> Result<()> {
        self.background.stop(session_id).await?;
        self.steering_lane(session_id)
            .await
            .cleanup_execution()
            .await
    }

    async fn cancel_session(&self, session_id: &str) -> Result<()> {
        self.cleanup_execution(session_id).await
    }

    async fn reset_session(&self, owner: &crate::provider::ExecutionOwner) -> Result<()> {
        let key = owner.runtime_key();
        self.background.stop(&key).await?;
        let connection = self.connection_by_session.lock().await.get(&key).cloned();
        if let Some(connection) = connection {
            connection.lock().await.process.close().await?;
            self.connection_by_session.lock().await.remove(&key);
        }
        self.active_turn_by_session.lock().await.remove(&key);
        self.session_events.reset_owner(owner).await?;
        Ok(())
    }

    async fn fence_session(&self, owner: &crate::provider::ExecutionOwner) -> Result<()> {
        let key = owner.runtime_key();
        let interrupt = self.cleanup_execution(&key).await;
        let collection = self.session_events.cancel(owner).await;
        let connection = self.connection_by_session.lock().await.get(&key).cloned();
        let close = if let Some(connection) = connection {
            let result = connection.lock().await.process.close().await;
            if result.is_ok() {
                self.connection_by_session.lock().await.remove(&key);
            }
            result
        } else {
            Ok(())
        };
        match (interrupt, collection, close) {
            (Err(error), collection, close) => {
                let error = if let Err(next) = collection {
                    error.context(format!("collect Codex execution: {next:#}"))
                } else {
                    error
                };
                Err(if let Err(next) = close {
                    error.context(format!("close Codex connection: {next:#}"))
                } else {
                    error
                })
            }
            (Ok(()), Err(error), close) => {
                let error = anyhow::Error::new(error);
                Err(if let Err(next) = close {
                    error.context(format!("close Codex connection: {next:#}"))
                } else {
                    error
                })
            }
            (Ok(()), Ok(()), close) => close,
        }
    }

    async fn active_session_id_for(
        &self,
        session_id: &str,
    ) -> Result<Option<forge_provider_api::ProviderSessionId>> {
        Ok(
            match self.active_turn_by_session.lock().await.get(session_id) {
                Some(CodexTurnState::Submitted { thread_id, .. }) => {
                    Some(thread_id.clone().try_into()?)
                }
                Some(
                    CodexTurnState::Idle | CodexTurnState::Pending | CodexTurnState::Completed,
                )
                | None => None,
            },
        )
    }

    async fn compact(&self, request: super::ProviderCompactionRequest) -> Result<ProviderOutput> {
        let scope = super::EventScope::SessionOperation {
            owner: request.request.owner()?,
            operation_id: request.id.clone(),
        };
        let request = request.request;
        let thread_id = request
            .provider_session_id
            .clone()
            .map(String::from)
            .context("Codex thread has not started")?;
        let mut output = ProviderOutput {
            event: Vec::new(),
            completion: crate::provider::ProviderCompletion {
                native: forge_provider_api::completion::ProviderCompletion {
                    provider_session_id: Some(thread_id.clone().try_into()?),
                    capability: capability(),
                    ..Default::default()
                },
                ..Default::default()
            },
        };
        let mut process = self.connect(&request, &mut output, None).await?.process;
        self.load_thread(&mut process, &mut output, &request, false, Vec::new())
            .await?;
        process.normalization.bind(Some(scope));
        output.completion.metrics.context_usage = None;
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
        output.completion.runtime.provider = "Codex CLI".into();
        Ok(output)
    }

    async fn model_list(&self, request: ProviderRequest) -> Result<Vec<ProviderModel>> {
        let mut output = ProviderOutput::default();
        let mut process = self.connect(&request, &mut output, None).await?.process;
        let model_list = Self::model_catalog(&mut process, &mut output).await?;
        if let Some(model) = model_list.iter().find(|model| model.is_default) {
            *self.default_model.lock().await = Some(model.id.clone());
        }
        Ok(model_list)
    }

    async fn skill_list(&self, request: ProviderCatalogRequest) -> Result<Vec<SkillDefinition>> {
        if let Some(resources) = self.resources(&request).await? {
            return Ok(resources.catalog.skill.clone());
        }
        let (mut process, mut output) = self.catalog_process(&request).await?;
        Self::skill_catalog(&mut process, &mut output, &request.workspace, true).await
    }

    async fn set_skill_enabled(
        &self,
        request: ProviderCatalogRequest,
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
        self.background.stop(&request.runtime_key()?).await?;
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
        request: ProviderCatalogRequest,
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
        Ok(catalog::parse_mcp_catalog(&Value::Null, &config))
    }

    async fn mcp_list(&self, request: ProviderCatalogRequest) -> Result<Vec<McpDefinition>> {
        let resources = self.resources(&request).await?;
        let thread = if let Some(resources) = resources.as_ref() {
            request
                .provider_session_id
                .clone()
                .map(String::from)
                .or(resources.thread().await)
        } else {
            request.provider_session_id.clone().map(String::from)
        };
        if resources.is_some() && thread.is_none() {
            return self.mcp_configuration(request).await;
        }
        let (mut process, mut output) = self.catalog_process(&request).await?;
        if let Some(resources) = resources.as_ref()
            && resources.thread().await != thread
        {
            let mut provider_request = Self::catalog_provider_request(&request);
            provider_request.provider_session_id =
                thread.clone().map(TryInto::try_into).transpose()?;
            self.load_thread(
                &mut process,
                &mut output,
                &provider_request,
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
        let parsed = catalog::parse_mcp_catalog(&json!({ "data": server_list }), &config);
        Ok(resources
            .map(|resources| resources.mcp(parsed.clone()))
            .unwrap_or(parsed))
    }

    async fn set_mcp_enabled(
        &self,
        request: ProviderCatalogRequest,
        name: &str,
        enabled: bool,
    ) -> Result<CatalogMutation> {
        anyhow::ensure!(
            self.settings_root.is_none(),
            "Managed MCP selection must use the shared settings operation"
        );
        let restart_required = self.has_active_turn(&request.runtime_key()?).await;
        let config_name = catalog::quoted_config_segment(name);
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
        self.background.stop(&request.runtime_key()?).await?;
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
        request: ProviderRequest,
        update: crate::provider::NativeGoalUpdate,
    ) -> Result<()> {
        let (mut process, mut output, thread_id) = self
            .thread_connection(Self::catalog_from_request(&request))
            .await?
            .context("Codex thread has not started")?;
        let (method, params) = match update {
            crate::provider::NativeGoalUpdate::Absent => {
                ("thread/goal/clear", json!({ "threadId": thread_id }))
            }
            crate::provider::NativeGoalUpdate::Present { objective, status } => {
                let status = match status {
                    crate::provider::NativeGoalStatus::Active => "active",
                    crate::provider::NativeGoalStatus::Paused => "paused",
                    crate::provider::NativeGoalStatus::Complete => "complete",
                    crate::provider::NativeGoalStatus::Blocked => "blocked",
                    crate::provider::NativeGoalStatus::UsageLimited => "usageLimited",
                    crate::provider::NativeGoalStatus::BudgetLimited => "budgetLimited",
                };
                (
                    "thread/goal/set",
                    json!({ "threadId": thread_id, "objective": objective, "status": status }),
                )
            }
        };
        process.request(method, params, &mut output).await?;
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
                let mut output = ProviderOutput::default();
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

impl CodexProvider {
    async fn steer_native(&self, session_id: &str, input: super::SteeringInput) -> Result<()> {
        self.steering_lane(session_id).await.steer(input).await
    }
}

impl CodexProvider {
    /// Drive admitted native work inside the session-owned task.
    async fn run_native(
        &self,
        mut request: ProviderRequest,
        event_sink: ProviderEventSink,
    ) -> std::result::Result<crate::provider::ProviderCompletion, super::ProviderError> {
        capability().validate_access(&request.access)?;
        if request.owner().map_err(super::ProviderError::from)?
            != event_sink.invocation_scope()?.owner
        {
            return Err(super::ProviderError::Rejected {
                message: "Codex invocation scope does not match the requested owner".into(),
            });
        }
        let runtime_key = request.runtime_key().map_err(super::ProviderError::from)?;
        let output: anyhow::Result<crate::provider::ProviderOutput> = async {
            let event_sink = Some(event_sink);
            let skills = self.permission_coordinator.skills(
                self.settings_root.as_deref(),
                &request,
                super::ProviderKind::Codex,
            )?;
            let _directories = self
                .permission_coordinator
                .directories(self.settings_root.as_deref(), &request)?;
            let event_sink = skills.sink(event_sink);
            let mut active_steering = self
                .activate_turn_control(&request.runtime_key()?, event_sink.clone())
                .await?;
            let mut output = ProviderOutput {
                event: Vec::new(),
                completion: crate::provider::ProviderCompletion {
                    native: forge_provider_api::completion::ProviderCompletion {
                    capability: capability(), ..Default::default() },
                    ..Default::default()
                },
            };
            let coordinator_event_sink = event_sink.clone();
            let startup = self.startup_guard(request.runtime_key()?).await;
            let connection = self
                .persistent_connection(&request, &mut output, event_sink)
                .await?;
            let mut connection = connection.lock().await;
            let prepared = connection.thread.as_ref().is_some_and(|thread| {
                matches!(thread, CodexThread::Prepared(_))
                    && request.provider_session_id.as_ref().is_none_or(|id| id.as_str() == thread.id())
            });
            if request.provider_session_id.is_none() {
                request.provider_session_id = connection.thread.as_ref().map(|thread| thread.id().to_owned().try_into()).transpose()?;
            }
            let CodexConnection {
                process,
                thread: retained_thread,
                ..
            } = &mut *connection;
            process.set_activity_publication(false);
            let request_text = request.input.text();
            let mut control_context = request.control_context.clone().unwrap_or_else(|| {
                crate::tools::ControlTurnContext::inactive(request.mode)
            });
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
            output.completion.runtime.provider = "Codex CLI".into();
            output.completion.runtime.model = Some(request.model.clone());
            if let Some(sink) = coordinator_event_sink.as_ref() {
                let runtime_event = crate::provider::ProviderEvent::Runtime {
                    scope: sink.invocation_scope()?.clone(),
                    update: crate::provider::RuntimeUpdate::Resolved {
                        runtime: output.completion.runtime.clone(),
                    },
                };
                sink.send_wait(runtime_event).await?;
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
            let thread_id = if prepared {
                request.provider_session_id.clone().map(String::from).context("prepared Codex thread is missing")?
            } else {
                self.load_thread(process, &mut output, &request, false, dynamic_tool_list).await?
            };
            drop(startup);
            if prepared {
                // The initial thread has no per-turn checkpoint hook. Admit its first turn
                // only after the checkpoint completes, before native tools can execute.
                if let Some(gate) = request.control_context.as_ref().and_then(|context| context.checkpoint.as_ref()) {
                    gate.wait().await?;
                }
            } else if request.provider_session_id.is_some()
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
                    .and_then(|value| forge_providers::terminal::parse_snapshot(&value, false));
                if !terminals
                    .as_ref()
                    .is_ok_and(|snapshot| snapshot.terminal.is_empty())
                {
                    gate.wait().await?;
                }
            }
            output.completion.provider_session_id = Some(thread_id.clone().try_into()?);
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
                .map(ProviderInput::text)
                .collect::<Vec<_>>();
            prompt = request.recovered_text(&prompt, &recovered);
            let mut input =
                crate::provider::codex::attachments::input(prompt, &request.input_attachments()).await?;
            for name in request
                .recovered_input
                .iter()
                .chain(std::iter::once(&request.input))
                .filter_map(|input| {
                    if let ProviderInput::Skill { name, .. } = input {
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
            *retained_thread = Some(CodexThread::Submitted(thread_id.clone()));
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
                                    "serviceTier": request.service_tier,
                                    "collaborationMode": prepared.then(|| json!({
                                        "mode":"default", "settings":{
                                            "model":request.model, "reasoning_effort":request.effort,
                                            "developer_instructions":request.system_message()
                                        }
                                    }))
                                }),
                                &request,
                            ),
                            &request.model,
                        ),
                    )
                    .await?,
                )
            };
            let turn_id = if let Some((turn, _provider_turn_started, observed_message_list)) =
                admission
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
                    if let Some((error, code)) = error
                        .and_then(|error| retry::transient_code(error).map(|code| (error, code)))
                        && !process.normalization.replay_sensitive
                        && super::retry::replay_safe(&output.completion)
                    {
                        return Err(super::retry::RetryableFailure {
                            address: super::TurnAddress {
                                scope: coordinator_event_sink
                                    .as_ref()
                                    .context("Codex retry has no invocation")?
                                    .invocation_scope()?
                                    .clone(),
                                thread_id: thread_id.clone(),
                                turn_id: completed
                                    .pointer("/turn/id")
                                    .or_else(|| completed.get("turnId"))
                                    .and_then(Value::as_str)
                                    .unwrap_or(&current_turn_id)
                                    .into(),
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
                if let Some(context) = &request.control_context {
                    let covered = request.submission.as_ref().map(|submission| {
                        forge_provider_api::lifecycle::SubmissionId::try_from(submission.id().to_owned())
                    }).transpose()?.into_iter().collect();
                    context.provider_completion.certify(
                        crate::provider::ExecutionCertificate::WithoutExecution { covered },
                    )?;
                }
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
                output.completion.provider_checkpoint_id = Some(turn_id);
            }
            Ok(output)
        }
        .await;
        let connection = self
            .connection_by_session
            .lock()
            .await
            .get(&runtime_key)
            .cloned();
        if let Some(connection) = connection {
            self.background
                .start(runtime_key, connection)
                .await
                .map_err(super::ProviderError::from)?;
        }
        output
            .map(ProviderOutput::into_completion)
            .map_err(super::ProviderError::from)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    /// A certified native interruption must not retire the session's socket owner.
    #[tokio::test]
    async fn normal_cancellation_keeps_reader_for_next_native_turn() -> Result<()> {
        use futures_util::{SinkExt, StreamExt};
        use std::time::Duration;
        use tokio_tungstenite::tungstenite::Message;

        let fixture = tempfile::tempdir()?;
        let workspace = fixture.path().to_string_lossy().into_owned();
        let permission = PermissionCoordinator::transient(&workspace)?;
        let trace = Arc::new(TraceStore::open(fixture.path())?);
        let provider = CodexProvider::new_with_permission_coordinator(
            vec!["unused".into()],
            permission.clone(),
            trace.clone(),
            None,
        )?;
        let provider = crate::provider::test_support::ProviderTestSession::new(provider)?;
        let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await?;
        let endpoint = format!("ws://{}", listener.local_addr()?);
        let (started, ready) = tokio::sync::oneshot::channel();
        let mut server = tokio::spawn(async move {
            let (stream, _) = listener.accept().await?;
            let mut socket = tokio_tungstenite::accept_async(stream).await?;
            for method in ["thread/start", "turn/start"] {
                let request: Value = serde_json::from_str(
                    socket
                        .next()
                        .await
                        .context("native request missing")??
                        .to_text()?,
                )?;
                anyhow::ensure!(
                    request["method"] == method,
                    "unexpected native request: {request}"
                );
                let result = if method == "thread/start" {
                    json!({"thread":{"id":"retained-thread"}})
                } else {
                    json!({"turn":{"id":"interrupted-turn"}})
                };
                socket
                    .send(Message::Text(
                        json!({"id":request["id"],"result":result})
                            .to_string()
                            .into(),
                    ))
                    .await?;
            }
            started
                .send(())
                .map_err(|_| anyhow::anyhow!("cancellation waiter dropped"))?;
            let interrupt: Value = serde_json::from_str(
                socket
                    .next()
                    .await
                    .context("interrupt missing")??
                    .to_text()?,
            )?;
            anyhow::ensure!(
                interrupt["method"] == "turn/interrupt",
                "unexpected interrupt request: {interrupt}"
            );
            for message in [
                json!({"id":interrupt["id"],"result":{}}),
                json!({"method":"turn/completed","params":{"threadId":"retained-thread","turn":{"id":"interrupted-turn","status":"interrupted"}}}),
            ] {
                socket
                    .send(Message::Text(message.to_string().into()))
                    .await?;
            }
            let resume: Value = serde_json::from_str(
                socket
                    .next()
                    .await
                    .context("retained thread resume missing")??
                    .to_text()?,
            )?;
            anyhow::ensure!(
                resume["method"] == "thread/resume"
                    && resume["params"]["threadId"] == "retained-thread",
                "unexpected retained thread resume: {resume}"
            );
            socket
                .send(Message::Text(
                    json!({"id":resume["id"],"result":{"thread":{"id":"retained-thread"}}})
                        .to_string()
                        .into(),
                ))
                .await?;
            let next: Value = serde_json::from_str(
                socket
                    .next()
                    .await
                    .context("next turn missing")??
                    .to_text()?,
            )?;
            anyhow::ensure!(
                next["method"] == "turn/start" && next["params"]["threadId"] == "retained-thread",
                "session reader was not reused: {next}"
            );
            for message in [
                json!({"id":next["id"],"result":{"turn":{"id":"next-turn"}}}),
                json!({"method":"item/completed","params":{"threadId":"retained-thread","turnId":"next-turn","item":{"id":"answer","type":"agentMessage","phase":"final_answer","text":"next turn completed"}}}),
                json!({"method":"turn/completed","params":{"threadId":"retained-thread","turn":{"id":"next-turn","status":"completed"}}}),
            ] {
                socket
                    .send(Message::Text(message.to_string().into()))
                    .await?;
            }
            match socket.next().await {
                None | Some(Ok(Message::Close(_))) => {}
                other => {
                    anyhow::bail!("explicit owner fence did not close native socket: {other:?}")
                }
            }
            Ok::<_, anyhow::Error>(())
        });
        let process = CodexJsonRpc::connect(
            &endpoint,
            &workspace,
            ApprovalMode::Ask,
            permission,
            None,
            trace,
            "cancel-reader".into(),
        )
        .await?;
        provider.connection_by_session.lock().await.insert(
            "cancel-reader".into(),
            Arc::new(Mutex::new(CodexConnection {
                process,
                timing: Vec::new(),
                thread: None,
            })),
        );
        let mut request = CodexProvider::catalog_provider_request(&ProviderCatalogRequest {
            execution_owner: None,
            harness_session_id: "cancel-reader".into(),
            workspace,
            access: Default::default(),
            approval_mode: ApprovalMode::Ask,
            provider_session_id: None,
        });
        request.model = "fixture-model".into();
        request.input = ProviderInput::from_text("first request");
        let first = provider.session_prompt_and_collect(request.clone());
        let cancel = async {
            ready
                .await
                .context("fake peer stopped before first turn/start acknowledgement")?;
            provider
                .cancel_session("cancel-reader")
                .await
                .context("interrupt and collect first native turn")
        };
        let (first, cancelled) = tokio::time::timeout(Duration::from_secs(5), async {
            tokio::select! {
                biased;
                peer = &mut server => {
                    peer.context("fake peer task failed")??;
                    anyhow::bail!("fake peer exited before first cancellation completed")
                }
                pair = async { tokio::join!(first, cancel) } => Ok(pair),
            }
        })
        .await
        .context("first native cancellation timed out")??;
        if let Err(error) = cancelled {
            if server.is_finished() {
                (&mut server).await.context("fake peer task failed")??;
            }
            return Err(error);
        }
        match first {
            Err(crate::provider::ProviderError::Cancelled) => {}
            Err(error) => {
                return Err(anyhow::Error::from(error))
                    .context("first native turn failed before certified cancellation");
            }
            Ok(_) => anyhow::bail!("interrupted turn did not retain its cancelled terminal result"),
        }
        assert!(
            provider
                .connection_by_session
                .lock()
                .await
                .contains_key("cancel-reader")
        );
        request.provider_session_id = Some("retained-thread".to_owned().try_into()?);
        request.input = ProviderInput::from_text("next request");
        let output = tokio::time::timeout(Duration::from_secs(5), async {
            tokio::select! {
                biased;
                peer = &mut server => {
                    peer.context("fake peer task failed")??;
                    anyhow::bail!("fake peer exited before the second native turn completed")
                }
                result = provider.session_prompt_and_collect(request) => result.context("second native turn failed on retained socket"),
            }
        }).await.context("second native turn timed out")??;
        assert!(serde_json::to_string(&output.event)?.contains("next turn completed"));
        tokio::time::timeout(
            Duration::from_secs(5),
            provider.fence_session(&crate::provider::ExecutionOwner::root("cancel-reader")),
        )
        .await
        .context("explicit owner fence timed out")??;
        tokio::time::timeout(Duration::from_secs(5), provider.close()).await??;
        tokio::time::timeout(Duration::from_secs(5), provider.shutdown()).await??;
        tokio::time::timeout(Duration::from_secs(5), server).await???;
        Ok(())
    }

    #[tokio::test]
    async fn eager_initialization_reuses_live_thread_without_persisting_or_resuming() -> Result<()>
    {
        use futures_util::{SinkExt, StreamExt};
        let fixture = tempfile::tempdir()?;
        let workspace = fixture.path().to_string_lossy().into_owned();
        let permission = PermissionCoordinator::transient(&workspace)?;
        let trace = Arc::new(TraceStore::open(fixture.path())?);
        let provider = CodexProvider::new_with_permission_coordinator(
            vec!["unused".into()],
            permission.clone(),
            trace.clone(),
            None,
        )?;
        let provider = crate::provider::test_support::ProviderTestSession::new(provider)?;
        let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await?;
        let endpoint = format!("ws://{}", listener.local_addr()?);
        let server = tokio::spawn(async move {
            let (stream, _) = listener.accept().await.unwrap();
            let mut socket = tokio_tungstenite::accept_async(stream).await.unwrap();
            let mut initial_instructions = Value::Null;
            for (method, thread) in [
                ("thread/start", "prepared-thread"),
                ("turn/start", "prepared-thread"),
            ] {
                let request: Value =
                    serde_json::from_str(socket.next().await.unwrap().unwrap().to_text().unwrap())
                        .unwrap();
                assert_eq!(request["method"], method);
                if method == "thread/start" {
                    initial_instructions = request["params"]["developerInstructions"].clone();
                    assert_eq!(request["params"]["ephemeral"], false);
                    assert_eq!(request["params"]["historyMode"], "paginated");
                    assert!(
                        !request["params"]["dynamicTools"]
                            .as_array()
                            .unwrap()
                            .is_empty()
                    );
                } else {
                    assert_eq!(request["params"]["threadId"], thread);
                }
                if method == "turn/start" {
                    assert_eq!(request["params"]["collaborationMode"]["mode"], "default");
                    assert_eq!(
                        request["params"]["collaborationMode"]["settings"]["model"],
                        "fixture-model"
                    );
                    assert!(request["params"]["collaborationMode"]["settings"]["developer_instructions"].is_string());
                    assert_ne!(
                        request["params"]["collaborationMode"]["settings"]["developer_instructions"],
                        initial_instructions
                    );
                }
                let response = if method == "turn/start" {
                    json!({"turn":{"id":"first-turn"}})
                } else {
                    json!({"thread":{"id":thread}})
                };
                socket
                    .send(tokio_tungstenite::tungstenite::Message::Text(
                        json!({"id":request["id"],"result":response})
                            .to_string()
                            .into(),
                    ))
                    .await
                    .unwrap();
                if method == "turn/start" {
                    for message in [
                        json!({"method":"item/completed","params":{"threadId":thread,"turnId":"first-turn","item":{"id":"answer","type":"agentMessage","phase":"final_answer","text":"ready"}}}),
                        json!({"method":"turn/completed","params":{"threadId":thread,"turn":{"id":"first-turn","status":"completed"}}}),
                    ] {
                        socket
                            .send(tokio_tungstenite::tungstenite::Message::Text(
                                message.to_string().into(),
                            ))
                            .await
                            .unwrap();
                    }
                }
            }
        });
        let process = CodexJsonRpc::connect(
            &endpoint,
            &workspace,
            ApprovalMode::Ask,
            permission,
            None,
            trace,
            "eager".into(),
        )
        .await?;
        provider.connection_by_session.lock().await.insert(
            "eager".into(),
            Arc::new(Mutex::new(CodexConnection {
                process,
                timing: Vec::new(),
                thread: None,
            })),
        );
        let mut request = CodexProvider::catalog_provider_request(&ProviderCatalogRequest {
            execution_owner: None,
            harness_session_id: "eager".into(),
            workspace,
            access: Default::default(),
            approval_mode: ApprovalMode::Ask,
            provider_session_id: None,
        });
        request.model = "fixture-model".into();
        assert_eq!(
            provider.initialize_session(request.clone()).await?,
            Some("prepared-thread".to_owned().try_into()?)
        );
        assert!(provider.active_turn_by_session.lock().await.is_empty());
        assert_eq!(
            provider.initialize_session(request.clone()).await?,
            Some("prepared-thread".to_owned().try_into()?)
        );
        request.input = ProviderInput::from_text("first prompt");
        let gate = crate::checkpoint::ToolGate::new()?;
        gate.complete(Err("fixture checkpoint failed".into()))?;
        let mut blocked_request = request.clone();
        let mut blocked_context = crate::tools::ControlTurnContext::inactive(PromptMode::Chat);
        blocked_context.checkpoint = Some(gate);
        blocked_request.control_context = Some(blocked_context);
        let failure = provider
            .session_prompt_and_collect(blocked_request)
            .await
            .unwrap_err();
        assert!(failure.to_string().contains("fixture checkpoint failed"));
        // The retained idle reader owns the connection until explicitly stopped.
        provider.background.stop("eager").await?;
        assert!(matches!(
            provider.connection_by_session.lock().await["eager"]
                .lock()
                .await
                .thread,
            Some(CodexThread::Prepared(_))
        ));
        request.mode = PromptMode::Plan;
        request.control_context =
            Some(crate::tools::ControlTurnContext::inactive(PromptMode::Plan));
        let output = provider.session_prompt_and_collect(request.clone()).await?;
        assert_eq!(
            output
                .completion
                .provider_session_id
                .as_ref()
                .map(|id| id.as_str()),
            Some("prepared-thread")
        );
        assert!(serde_json::to_string(&output.event)?.contains("ready"));
        assert_eq!(
            provider.initialize_session(request).await?,
            Some("prepared-thread".to_owned().try_into()?)
        );
        server.await?;
        provider.shutdown().await?;
        Ok(())
    }

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
        let provider = CodexProvider::new(vec!["unused".into()])?;
        let provider = crate::provider::test_support::ProviderTestSession::new(provider)?;
        let mut request = CodexProvider::catalog_provider_request(&ProviderCatalogRequest {
            execution_owner: None,
            harness_session_id: "checkpoint".into(),
            workspace: workspace.clone(),
            access: Default::default(),
            approval_mode: ApprovalMode::Ask,
            provider_session_id: None,
        });
        let gate = crate::checkpoint::ToolGate::new()?;
        let mut context = crate::tools::ControlTurnContext::inactive(PromptMode::Chat);
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
        provider
            .load_thread(
                &mut process,
                &mut ProviderOutput::default(),
                &request,
                true,
                Vec::new(),
            )
            .await?;
        gate.complete(Ok(()))?;
        provider
            .load_thread(
                &mut process,
                &mut ProviderOutput::default(),
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
                assert_eq!(
                    request["params"],
                    json!({"threadId":"provider-thread","excludeTurns":true})
                );
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
        let request = ProviderCatalogRequest {
            execution_owner: None,
            harness_session_id: "session".into(),
            workspace,
            access: Default::default(),
            approval_mode: ApprovalMode::Ask,
            provider_session_id: Some("provider-thread".to_owned().try_into().unwrap()),
        };
        let result = CodexProvider::load_terminal_thread(
            &mut process,
            &mut ProviderOutput::default(),
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
        let request = ProviderCatalogRequest {
            execution_owner: None,
            harness_session_id: "session".into(),
            workspace: workspace.clone(),
            access: Default::default(),
            approval_mode: ApprovalMode::Ask,
            provider_session_id: Some("provider-thread".to_owned().try_into().unwrap()),
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
        let mut output = ProviderOutput::default();
        CodexProvider::load_terminal_thread(
            &mut process,
            &mut output,
            &request,
            Some(&resources),
            std::time::Duration::ZERO,
        )
        .await?;
        CodexProvider::load_terminal_thread(
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
        let provider = CodexProvider::new_with_permission_coordinator(
            vec!["unused-provider".into()],
            permission.clone(),
            trace.clone(),
            None,
        )?;
        let provider = crate::provider::test_support::ProviderTestSession::new(provider)?;
        let request = ProviderCatalogRequest {
            execution_owner: None,
            harness_session_id: "session".into(),
            workspace: workspace.clone(),
            access: Default::default(),
            approval_mode: ApprovalMode::Ask,
            provider_session_id: Some("provider-thread".to_owned().try_into().unwrap()),
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
        provider.terminal_connection_by_session.lock().await.insert(
            request.runtime_key()?,
            Arc::new(Mutex::new(Some(TerminalConnection {
                process,
                output: ProviderOutput::default(),
                thread_id: "provider-thread".into(),
            }))),
        );
        let first = provider.background_terminals(request.clone()).await?;
        assert_eq!(
            first
                .terminal
                .iter()
                .map(|terminal| terminal.id.as_str())
                .collect::<Vec<_>>(),
            ["one", "two"]
        );
        let second = provider.background_terminals(request).await?;
        assert_eq!(second.terminal[0].id, "three");
        tokio::time::timeout(std::time::Duration::from_secs(2), server).await??;
        Ok(())
    }

    #[test]
    fn omits_the_default_model_sentinel_from_codex_requests() {
        let default = CodexProvider::with_model(json!({ "cwd": "workspace" }), "default");
        assert!(default.get("model").is_none());
        let selected = CodexProvider::with_model(json!({ "cwd": "workspace" }), "gpt-5.4-mini");
        assert_eq!(
            selected.get("model").and_then(Value::as_str),
            Some("gpt-5.4-mini")
        );
        let catalog = catalog::parse_model_catalog(&json!({
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
            vec![crate::provider::ServiceTier::Fast]
        );
        assert!(
            crate::provider::ServiceTier::Fast
                .validate_model(&catalog[0])
                .is_ok()
        );
        assert!(
            crate::provider::ServiceTier::Ultrafast
                .validate_model(&catalog[0])
                .is_err()
        );
    }

    #[test]
    fn forks_without_replaying_provider_turns() {
        let request = ProviderRequest {
            execution_owner: None,
            harness_session_id: "harness-session".into(),
            workspace: "workspace".into(),
            input: ProviderInput::from_text(""),
            mode: PromptMode::Chat,
            model: "default".into(),
            effort: "medium".into(),
            context_window: None,
            service_tier: crate::provider::ServiceTier::Standard,
            access: Default::default(),
            approval_mode: ApprovalMode::Ask,
            provider_session_id: Some("source-thread".to_owned().try_into().unwrap()),
            submission: None,
            recovered_input: Vec::new(),
            control_context: None,
        };
        let params = CodexProvider::fork_parameters(
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
        let provider = CodexProvider::new_with_permission_coordinator(
            vec!["unused".into()],
            permission.clone(),
            trace.clone(),
            None,
        )?;
        let provider = crate::provider::test_support::ProviderTestSession::new(provider)?;
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
        provider.connection_by_session.lock().await.insert(
            "speed-test".into(),
            Arc::new(Mutex::new(CodexConnection {
                process,
                thread: None,
                timing: Vec::new(),
            })),
        );
        let mut request = ProviderRequest {
            execution_owner: None,
            harness_session_id: "speed-test".into(),
            workspace,
            input: ProviderInput::from_text("must not dispatch"),
            mode: PromptMode::Chat,
            model: "gpt-6-luna".into(),
            effort: "low".into(),
            context_window: None,
            service_tier: crate::provider::ServiceTier::Ultrafast,
            access: Default::default(),
            approval_mode: ApprovalMode::Ask,
            provider_session_id: None,
            submission: None,
            recovered_input: Vec::new(),
            control_context: None,
        };
        let store = super::super::submission::SubmissionStore::open(fixture.path(), trace)?;
        let submission = store.prepare(&request, ProviderKind::Codex)?;
        request.submission = Some(submission.clone());
        let error = provider
            .session_prompt_and_collect(request)
            .await
            .unwrap_err();
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

    /// An unanswered native start remains uncertain and cannot trigger a replay.
    #[tokio::test]
    async fn initial_start_acknowledgment_timeout_preserves_delivery_evidence_without_replay()
    -> Result<()> {
        use futures_util::{SinkExt, StreamExt};
        let fixture = tempfile::tempdir()?;
        let workspace = fixture.path().to_string_lossy().into_owned();
        let permission = PermissionCoordinator::transient(&workspace)?;
        let trace = Arc::new(TraceStore::open(fixture.path())?);
        let provider = CodexProvider::new_with_permission_coordinator(
            vec!["unused".into()],
            permission.clone(),
            trace.clone(),
            None,
        )?;
        let provider = crate::provider::test_support::ProviderTestSession::new(provider)?;
        let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await?;
        let endpoint = format!("ws://{}", listener.local_addr()?);
        let (started_tx, started_rx) = tokio::sync::oneshot::channel();
        let (stop_tx, mut stop_rx) = tokio::sync::oneshot::channel();
        let server = tokio::spawn(async move {
            let (stream, _) = listener.accept().await?;
            let mut socket = tokio_tungstenite::accept_async(stream).await?;
            let resume: Value = serde_json::from_str(
                socket
                    .next()
                    .await
                    .context("missing thread resume")??
                    .to_text()?,
            )?;
            anyhow::ensure!(
                resume["method"] == "thread/resume",
                "expected thread resume"
            );
            socket
                .send(tokio_tungstenite::tungstenite::Message::Text(
                    json!({"id":resume["id"],"result":{"thread":{"id":"parent"}}})
                        .to_string()
                        .into(),
                ))
                .await?;
            let start: Value = serde_json::from_str(
                socket
                    .next()
                    .await
                    .context("missing turn start")??
                    .to_text()?,
            )?;
            anyhow::ensure!(start["method"] == "turn/start", "expected turn start");
            let _ = started_tx.send(
                start["params"]["clientUserMessageId"]
                    .as_str()
                    .unwrap_or_default()
                    .to_owned(),
            );
            let mut starts = 1usize;
            loop {
                tokio::select! {
                    _ = &mut stop_rx => break,
                    message = socket.next() => match message {
                        Some(Ok(message)) if message.is_text() => {
                            let value: Value = serde_json::from_str(message.to_text()?)?;
                            if value["method"] == "turn/start" { starts += 1; }
                        }
                        Some(Ok(_)) => {},
                        Some(Err(_)) | None => break,
                    }
                }
            }
            Ok::<usize, anyhow::Error>(starts)
        });
        let process = CodexJsonRpc::connect(
            &endpoint,
            &workspace,
            ApprovalMode::Ask,
            permission,
            None,
            trace.clone(),
            "ack-timeout".into(),
        )
        .await?;
        provider.connection_by_session.lock().await.insert(
            "ack-timeout".into(),
            Arc::new(Mutex::new(CodexConnection {
                process,
                thread: None,
                timing: Vec::new(),
            })),
        );
        let mut request = ProviderRequest {
            execution_owner: None,
            harness_session_id: "ack-timeout".into(),
            workspace,
            input: ProviderInput::from_text("retain this input"),
            mode: PromptMode::Chat,
            model: "gpt-5.6-terra".into(),
            effort: "medium".into(),
            context_window: None,
            service_tier: crate::provider::ServiceTier::Standard,
            access: Default::default(),
            approval_mode: ApprovalMode::Ask,
            provider_session_id: Some("parent".to_owned().try_into()?),
            submission: None,
            recovered_input: Vec::new(),
            control_context: None,
        };
        let journal =
            super::super::submission::SubmissionStore::open(fixture.path(), trace.clone())?;
        let submission = journal.prepare(&request, ProviderKind::Codex)?;
        let submission_id = submission.id().to_owned();
        request.submission = Some(submission.clone());
        let owner = provider.clone();
        let retained_trace = trace.clone();
        let retained_journal = journal.clone();
        let prompt = tokio::spawn(async move {
            crate::provider::test_support::retry_prompt(
                &owner,
                request,
                None,
                &retained_trace,
                &retained_journal,
            )
            .await
        });
        anyhow::ensure!(
            started_rx.await? == submission_id,
            "native start lost submission identity"
        );
        tokio::time::pause();
        tokio::time::advance(std::time::Duration::from_secs(29)).await;
        tokio::task::yield_now().await;
        anyhow::ensure!(
            !prompt.is_finished(),
            "initial acknowledgment deadline fired early"
        );
        tokio::time::advance(std::time::Duration::from_secs(1)).await;
        let error = prompt
            .await?
            .expect_err("unacknowledged native input must remain uncertain");
        anyhow::ensure!(
            matches!(
                error.downcast_ref::<super::super::ProviderError>(),
                Some(super::super::ProviderError::DeliveryUncertain { .. })
            ),
            "unexpected failure: {error:#}"
        );
        let record = submission.record()?;
        anyhow::ensure!(
            matches!(
                record.delivery,
                super::super::submission::SubmissionState::Sending { .. }
            ),
            "uncertain input lost its dispatch evidence"
        );
        anyhow::ensure!(
            record.id == submission_id && record.request.input.text() == "retain this input",
            "submission journal lost the original input"
        );
        let _ = stop_tx.send(());
        anyhow::ensure!(
            server.await?? == 1,
            "uncertain input was automatically replayed"
        );
        Ok(())
    }

    #[test]
    fn recognizes_only_the_matching_turn_started_notification() {
        let started = json!({
            "method": "turn/started",
            "params": { "turn": { "id": "turn-42", "status": "inProgress" } }
        });
        assert_eq!(
            CodexProvider::notification_turn_id(&started, "turn/started"),
            Some("turn-42")
        );
        assert_eq!(
            CodexProvider::notification_turn_id(&started, "turn/completed"),
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

        assert!(!CodexProvider::notification_matches_turn(
            &child,
            "turn/completed",
            "parent-thread",
            "parent-turn"
        ));
        assert!(CodexProvider::notification_matches_turn(
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
            (true, crate::provider::ServiceTier::Standard, true, false),
            (false, crate::provider::ServiceTier::Fast, true, false),
            (true, crate::provider::ServiceTier::Ultrafast, true, false),
            (true, crate::provider::ServiceTier::Ultrafast, false, false),
            (true, crate::provider::ServiceTier::Ultrafast, true, true),
            (true, crate::provider::ServiceTier::Ultrafast, false, true),
        ] {
            let fixture = tempfile::tempdir()?;
            let workspace = fixture.path().to_string_lossy().into_owned();
            let permission = PermissionCoordinator::transient(&workspace)?;
            let trace = Arc::new(TraceStore::open(fixture.path())?);
            let provider = CodexProvider::new_with_permission_coordinator(
                vec!["unused".into()],
                permission.clone(),
                trace.clone(),
                None,
            )?;
            let provider = crate::provider::test_support::ProviderTestSession::new(provider)?;
            *provider.default_model.lock().await = Some("gpt-catalog-default".into());
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
                if service_tier != crate::provider::ServiceTier::Standard {
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
                        assert!(instructions.contains(super::super::HARNESS_SYSTEM_MESSAGE));
                        assert!(instructions.contains("Answer the current request"));
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
            provider.connection_by_session.lock().await.insert(
                "session".into(),
                Arc::new(Mutex::new(CodexConnection {
                    process,
                    thread: None,
                    timing: Vec::new(),
                })),
            );
            let mut request = ProviderRequest {
                execution_owner: None,
                harness_session_id: "session".into(),
                workspace,
                input: ProviderInput::from_text("new prompt"),
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
                provider_session_id: resumed.then(|| "parent".to_owned().try_into().unwrap()),
                submission: None,
                recovered_input: Vec::new(),
                control_context: None,
            };
            let journal = super::super::submission::SubmissionStore::open(
                fixture.path(),
                provider.trace.clone(),
            )?;
            let submission = journal.prepare(&request, ProviderKind::Codex)?;
            request.submission = Some(submission.clone());
            let output = tokio::time::timeout(
                std::time::Duration::from_secs(2),
                provider.session_prompt_and_collect(request),
            )
            .await??;
            assert!(matches!(
                submission.record()?.delivery,
                super::super::submission::SubmissionState::Accepted { .. }
            ));
            assert_eq!(
                output.completion.runtime.model.as_deref(),
                Some(expected_model)
            );
            assert_eq!(
                output.completion.provider_checkpoint_id.as_deref(),
                Some("current")
            );
            assert_eq!(output.completion.evidence.native_state, None);
            let encoded = serde_json::to_string(&output.event)?;
            assert!(encoded.contains("CURRENT"));
            assert!(!encoded.contains("STALE"));
            assert!(!encoded.contains("\"turn_id\":\"old\""));
            let start = output
                .event
                .iter()
                .position(|event| {
                    matches!(
                        event,
                        crate::provider::ProviderEvent::Turn {
                            update: crate::provider::TurnUpdate::Started { .. },
                            ..
                        }
                    )
                })
                .unwrap();
            let answer = output
                .event
                .iter()
                .position(|event| matches!(event, crate::provider::ProviderEvent::Message { update, .. } if update.text() == Some("CURRENT")))
                .unwrap();
            assert!(
                start < answer,
                "acknowledged turns must own their response without another start notification"
            );
            let scope = output
                .event
                .iter()
                .find_map(|event| event.scope().invocation().cloned())
                .unwrap();
            let mut exchange: crate::exchange::Exchange = serde_json::from_value(json!({
                "id":scope.exchange_id, "session_id":scope.owner.session_id, "agent_id":scope.owner.agent_id, "ordinal":2,
                "prompt":"what is this repo?", "kind":"chat", "state":"running",
                "created_at_ms":0, "attributed_matches_checkpoint":false, "node_list":[]
            }))?;
            for event in &output.event {
                if event.turn_address().is_some() {
                    assert!(exchange.observe_turn(event, 1)?);
                } else {
                    let before = exchange.turn.len();
                    exchange.observe_turn(event, 1)?;
                    assert_eq!(exchange.turn.len(), before);
                }
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
            provider.cancel_session("another-session").await?;
            assert!(
                provider
                    .connection_by_session
                    .lock()
                    .await
                    .contains_key("session")
            );
            provider.cancel_session("session").await?;
            assert!(
                !provider
                    .connection_by_session
                    .lock()
                    .await
                    .contains_key("session")
            );
            drop(provider);
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
            let provider = CodexProvider::new_with_permission_coordinator(
                vec!["unused".into()],
                permission.clone(),
                trace.clone(),
                None,
            )?;
            let provider = crate::provider::test_support::ProviderTestSession::new(provider)?;
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
            provider.connection_by_session.lock().await.insert(
                "session".into(),
                Arc::new(Mutex::new(CodexConnection {
                    process,
                    thread: None,
                    timing: Vec::new(),
                })),
            );
            let output = tokio::time::timeout(
                std::time::Duration::from_secs(2),
                provider.session_prompt_and_collect(
                    ProviderRequest {
                        execution_owner: None,
                        harness_session_id: "session".into(),
                        workspace,
                        input: ProviderInput::from_text(message_header.map_or_else(|| "/goal resume".into(), |header|
                            format!("/goal resume\n\n{header}:\nKeep the original objective and add tests"))),
                        mode: PromptMode::GoalContinuation,
                        model: "gpt-5.6-terra".into(),
                        effort: "medium".into(),
                        context_window: None,
                        service_tier: crate::provider::ServiceTier::Standard,
                        access: Default::default(),
                        approval_mode: ApprovalMode::Ask,
                        provider_session_id: Some("parent".to_owned().try_into().unwrap()),
                        submission: None,
                        recovered_input: Vec::new(),
                        control_context: Some({
                            let mut context = crate::tools::ControlTurnContext::inactive(PromptMode::GoalContinuation);
                            context.native_goal = Some("Resume test objective".into());
                            context
                        }),
                    },
                ),
            )
            .await??;
            assert_eq!(
                output.completion.evidence.native_state,
                Some(crate::goal::GoalState::UsageLimited)
            );
            assert_eq!(
                output.completion.provider_checkpoint_id.as_deref(),
                starts_turn.then_some("resumed")
            );
            assert!(!provider.has_active_turn("session").await);
            drop(provider);
            tokio::time::timeout(std::time::Duration::from_secs(2), server).await??;
        }
        Ok(())
    }

    #[tokio::test]
    async fn goal_control_waits_for_reader_registration() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let provider = CodexProvider::new_with_permission_coordinator(
            vec!["codex".into(), "app-server".into()],
            PermissionCoordinator::transient(fixture.path())?,
            Arc::new(TraceStore::open(fixture.path())?),
            None,
        )?;
        let provider = crate::provider::test_support::ProviderTestSession::new(provider)?;
        for clear in [false, true] {
            let mut control = Box::pin(provider.stop_goal_session("session", clear));
            assert!(
                futures_util::poll!(control.as_mut()).is_pending(),
                "goal control must wait for startup instead of reporting delivery"
            );
            let mut reader = provider.activate_turn_control("session", None).await?;
            assert!(futures_util::poll!(control.as_mut()).is_pending());
            let command = reader.receive().await.unwrap();
            assert_eq!(
                command.operation,
                if clear {
                    crate::provider::steering::ActiveTurnOperation::ClearGoal
                } else {
                    crate::provider::steering::ActiveTurnOperation::PauseGoal
                }
            );
            command.complete(Ok(())).await;
            control.await?;
            provider
                .active_turn_by_session
                .lock()
                .await
                .insert("session".into(), CodexTurnState::Completed);
        }
        Ok(())
    }

    #[tokio::test]
    async fn distinguishes_an_unsubmitted_cancel_from_a_completed_turn() {
        let provider = CodexProvider::new(vec!["codex".into(), "app-server".into()]).unwrap();
        let provider = crate::provider::test_support::ProviderTestSession::new(provider).unwrap();
        provider
            .active_turn_by_session
            .lock()
            .await
            .insert("session-a".into(), CodexTurnState::Pending);
        assert!(
            provider
                .rollback_cancelled_turn_for("session-a")
                .await
                .unwrap()
        );
        provider
            .active_turn_by_session
            .lock()
            .await
            .insert("session-a".into(), CodexTurnState::Completed);
        assert!(
            !provider
                .rollback_cancelled_turn_for("session-a")
                .await
                .unwrap()
        );
    }
}
