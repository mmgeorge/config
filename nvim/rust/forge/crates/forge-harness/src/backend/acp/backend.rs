use super::control_server::ControlServer;
use super::session::AcpSessionState;
use super::transport::AcpTransport;
use super::runtime::AcpRuntime;
use crate::settings::ResourceProfile;
use crate::backend::*;
use crate::backend::approval::PermissionCoordinator;
use crate::session::PermissionMode;
use anyhow::{Context, Result, ensure};
use async_trait::async_trait;
use serde_json::{Value, json};
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::Mutex;

/// Owns ACP root-session processes and adapts provider-specific configuration.
pub(crate) struct AcpBackend {
    kind: BackendKind,
    runtime: AcpRuntime,
    permission: Arc<PermissionCoordinator>,
    session: Mutex<HashMap<String, Arc<AcpSession>>>,
    conversation: Mutex<HashMap<String, String>>,
    settings_root: std::path::PathBuf,
}

struct AcpSession {
    id: String,
    native_ask: bool,
    transport: AcpTransport,
    state: Arc<AcpSessionState>,
    _control: ControlServer,
    turn: Mutex<()>,
    initialized: Value,
    configuration: Value,
    _profile: Option<ResourceProfile>,
}

impl AcpBackend {
    /// Validate the launch descriptor without launching an unaffiliated provider process.
    pub(crate) fn new(kind: BackendKind, command: Vec<String>, permission: Arc<PermissionCoordinator>, settings_root: std::path::PathBuf) -> Result<Self> {
        ensure!(!command.is_empty(), "ACP command is required");
        let runtime = AcpRuntime::new(kind, command, &settings_root);
        Ok(Self { kind, runtime, permission, session: Mutex::new(HashMap::new()), conversation: Mutex::new(HashMap::new()), settings_root })
    }

    async fn runtime_capability(&self, session: &AcpSession) -> BackendCapability {
        let mut capability = self.descriptor().capability;
        capability.native_fork &= session.initialized.pointer("/agentCapabilities/sessionCapabilities/fork").is_some();
        capability.native_steer &= session.initialized.pointer("/_meta/steering/supported").and_then(Value::as_bool) == Some(true);
        capability.agent.observe = session.initialized.pointer("/agentCapabilities/sessionCapabilities/subagents").is_some();
        let catalog = session.state.catalog.read().await;
        capability.effort_selection = find_option(&catalog, "thought_level").or_else(||find_option(&catalog, "effort")).is_some();
        capability
    }

    async fn session(&self, mut request: BackendCatalogRequest, tools: Option<Vec<crate::control_tools::ControlToolDefinition>>) -> Result<Arc<AcpSession>> {
        self.kind.validate_access(&request.access)?;
        let runtime_key = request.runtime_key()?;
        let mut session_map = self.session.lock().await;
        if let Some(session) = session_map.get(&runtime_key).cloned() {
            let changed = if let Some(tools) = &tools {
                let previous = session.state.tool_definition.read().await;
                tools.iter().map(|definition|definition.mcp_value()).collect::<Vec<_>>()
                    != previous.iter().map(|definition|definition.mcp_value()).collect::<Vec<_>>()
            } else { false };
            if !changed { return Ok(session); }
            ensure!(session.state.active.read().await.is_none(), "Tool profile changes require active execution cleanup");
            request.backend_session_id = Some(session.id.clone());
            session.transport.shutdown().await?;
            session_map.remove(&runtime_key);
        }
        if request.backend_session_id.is_none() {
            request.backend_session_id = self.conversation.lock().await.get(&request.runtime_key()?).cloned();
        }
        let has_history = request.backend_session_id.is_some();
        let state = Arc::new(AcpSessionState::new(self.kind, self.permission.clone()));
        let setting = crate::settings::SettingsStore::open(&self.settings_root)?.get(&request.workspace,&request.harness_session_id,
            if self.kind == BackendKind::Claude {"claude"} else {"antigravity"})?;
        *state.tool_definition.write().await = tools.unwrap_or_else(||crate::control_tools::ControlToolRegistry
            .definition_list_for_owner(&request.owner().expect("validated runtime owner"),setting.desired.subagent_tools.unwrap_or(false)));
        let control = ControlServer::start(state.clone()).await?;
        let target = if self.kind == BackendKind::Claude { "claudecode" } else { "antigravity-cli" };
        let resources = crate::settings::SessionResources::load(&self.settings_root, &request,
            if self.kind == BackendKind::Claude { "claude" } else { "antigravity" }, target)?;
        let selected_skill = &resources.selected_skill;
        let profile = if self.kind == BackendKind::Antigravity {
            let excluded: Vec<_> = resources.skill.iter().filter(|skill| !skill.enabled).collect();
            if !excluded.is_empty() {
                let workspace = std::path::Path::new(&request.workspace);
                let native = crate::settings::ResourceCatalog::native_skills(vec![
                    workspace.join(".gemini/skills"),workspace.join(".agents/skills"),
                ])?;
                for skill in excluded {
                    ensure!(!native.iter().any(|candidate|candidate.name == skill.name),
                        "Antigravity cannot exclude project-native skill {} without changing project configuration", skill.name);
                }
            }
            Some(ResourceProfile::antigravity(&self.settings_root, &ResourceProfile::native_home()?, selected_skill)?)
        } else { Some(ResourceProfile::claude(&self.settings_root, selected_skill)?) };
        let environment_profile = (self.kind == BackendKind::Antigravity)
            .then(||profile.as_ref().map(ResourceProfile::path)).flatten();
        let transport = AcpTransport::launch(&self.runtime.resolve()?, &request.workspace, state.clone(), environment_profile).await?;
        let initialized = transport.request("initialize", json!({"protocolVersion":1,
            "clientInfo":{"name":"forge-harness","title":"Forge Harness","version":"0.1.0"},
            "clientCapabilities":{"session":{"compaction":{},"configOptions":{"boolean":{}}},"subagents":{}}})).await?;
        ensure!(initialized["protocolVersion"] == 1, "ACP agent did not negotiate protocol v1");
        ensure!(initialized.pointer("/agentCapabilities/loadSession").and_then(Value::as_bool) == Some(true),
            "ACP agent cannot resume conversations, which Forge requires");
        let mut parameter = json!({"cwd":request.workspace,"mcpServers":[{"type":"http",
            "name":"forge-harness-control","url":control.url,"headers":[]}]});
        parameter["mcpServers"].as_array_mut().context("ACP MCP list missing")?.extend(resources.acp()?);
        let resource_status: Vec<_> = resources.mcp.iter().map(|server|json!({
            "id":server.resource_id,"name":server.name,
            "state":if server.status == crate::backend::McpStatus::Loading { "requested" }
                else if !server.enabled { "disabled" } else { "unavailable" },
            "reason":server.status_detail,
        })).collect();
        state.catalog.write().await["mcpResources"] = json!(resource_status);
        if self.kind == BackendKind::Claude {
            parameter["_meta"] = json!({"claudeCode":{"emitRawSDKMessages":true,"options":{
                "strictMcpConfig":true,"disallowedTools":["Agent","Task"]}}});
            if request.execution_mode != PermissionMode::Yolo {
                parameter["_meta"]["claudeCode"]["options"]["settings"] = json!({"permissions":{"ask":["*"]}});
            }
            parameter["_meta"]["claudeCode"]["options"]["plugins"] = json!([
                {"type":"local","path":profile.as_ref().expect("Claude profile").path()}
            ]);
            parameter["_meta"]["claudeCode"]["options"]["skills"] = json!(selected_skill.iter()
                .map(|skill|format!("forge-managed:{}",skill.name)).collect::<Vec<_>>());
        }
        let response = if let Some(id) = &request.backend_session_id {
            parameter["sessionId"] = json!(id);
            ensure!(initialized.pointer("/agentCapabilities/loadSession").and_then(Value::as_bool) == Some(true),
                "ACP agent cannot load the saved conversation");
            transport.request("session/load", parameter.clone()).await?
        } else { transport.request("session/new", parameter.clone()).await? };
        let id = response["sessionId"].as_str().map(str::to_owned).or(request.backend_session_id)
            .context("ACP session response has no conversation identity")?;
        if let Some(field_map) = response.as_object() {
            let mut catalog = state.catalog.write().await;
            for (field, value) in field_map { catalog[field] = value.clone(); }
        }
        let session = Arc::new(AcpSession { id, native_ask:request.execution_mode != PermissionMode::Yolo,
            transport, state, _control: control, turn: Mutex::new(()), initialized, configuration: parameter, _profile:profile });
        if has_history {
            self.conversation.lock().await.insert(runtime_key.clone(), session.id.clone());
        }
        session_map.insert(runtime_key, session.clone());
        Ok(session)
    }

    async fn configure(&self, session: &AcpSession, request: &BackendRequest) -> Result<()> {
        let catalog = session.state.catalog.read().await.clone();
        if request.model != "default" && !request.model.is_empty() {
            if let Some(option) = find_option(&catalog, "model") {
                let response = session.transport.request("session/set_config_option", json!({"sessionId":session.id,
                    "configId":option["id"],"value":request.model})).await?;
                if let Some(options) = response.get("configOptions") { session.state.catalog.write().await["configOptions"] = options.clone(); }
            } else {
                session.transport.request("session/set_model", json!({"sessionId":session.id,"modelId":request.model})).await?;
            }
        }
        if let Some(option) = find_option(&catalog, "thought_level")
            .or_else(|| find_option(&catalog, "effort"))
            && !request.effort.is_empty() && request.effort != "default" {
            session.transport.request("session/set_config_option", json!({"sessionId":session.id,
                "configId":option["id"],"value":request.effort})).await?;
        }
        let modes = catalog.pointer("/modes/availableModes").and_then(Value::as_array);
        let planning = matches!(request.mode, PromptMode::Plan | PromptMode::PlanDiscussion | PromptMode::PlanQuestion);
        let plan_mode = if planning {
            ["plan", "architect"].into_iter().find(|id|
                modes.is_some_and(|modes| modes.iter().any(|mode| mode["id"] == *id)))
        } else { None };
        let desired = if let Some(mode) = plan_mode { mode } else if request.execution_mode == PermissionMode::Yolo {
            if self.kind == BackendKind::Claude { "bypassPermissions" } else { "yolo" }
        } else { "default" };
        if modes.is_some_and(|modes| modes.iter().any(|mode| mode["id"] == desired)) {
            session.transport.request("session/set_mode", json!({"sessionId":session.id,"modeId":desired})).await?;
        }
        Ok(())
    }
}

#[async_trait]
impl Backend for AcpBackend {
    async fn setup(&self, request: BackendCatalogRequest, install: bool) -> Result<Value> {
        ensure!(!self.has_active_turn(&request.runtime_key()?).await, "Finish active provider work before setup");
        let status = if install { self.runtime.install().await? } else { self.runtime.status() };
        let mut result = serde_json::to_value(&status)?;
        result["auth_methods"] = json!([]);
        if !status.available { return Ok(result); }
        self.cleanup_execution(&request.runtime_key()?).await?;
        let state = Arc::new(AcpSessionState::new(self.kind, self.permission.clone()));
        let profile = if self.kind == BackendKind::Antigravity {
            Some(ResourceProfile::antigravity(&self.settings_root, &ResourceProfile::native_home()?, &[])?)
        } else { None };
        let transport = AcpTransport::launch(&status.command, &request.workspace, state, profile.as_ref().map(ResourceProfile::path)).await?;
        let initialized = transport.request("initialize", json!({"protocolVersion":1,
            "clientInfo":{"name":"forge-harness","version":env!("CARGO_PKG_VERSION")},
            "clientCapabilities":{"auth":{"terminal":true}}})).await;
        transport.shutdown().await?;
        let initialized = initialized?;
        ensure!(initialized["protocolVersion"] == 1, "ACP agent did not negotiate protocol v1");
        result["provider_version"] = initialized.pointer("/agentInfo/version").cloned().unwrap_or(Value::Null);
        result["auth_methods"] = json!(initialized.get("authMethods").and_then(Value::as_array)
            .into_iter().flatten().map(|method| {
                let terminal = method["type"] == "terminal";
                let mut command = status.command.clone();
                if terminal { command.extend(method.get("args").and_then(Value::as_array).into_iter().flatten()
                    .filter_map(Value::as_str).map(str::to_owned)); }
                json!({"id":method["id"],"label":method["name"],"description":method["description"],
                    "terminal":terminal,"command":if terminal { json!(command) } else { Value::Null }})
            }).collect::<Vec<_>>());
        Ok(result)
    }

    async fn reconcile_settings(&self, request: BackendCatalogRequest, _setting: &crate::settings::PortableSettings) -> Result<()> {
        self.cleanup_execution(&request.runtime_key()?).await?;
        self.session(request, None).await?;
        Ok(())
    }

    async fn authenticate(&self, request: BackendCatalogRequest, method: String) -> Result<()> {
        ensure!(!self.has_active_turn(&request.runtime_key()?).await, "Authentication cannot change during active provider work");
        let state = Arc::new(AcpSessionState::new(self.kind, self.permission.clone()));
        let profile = if self.kind == BackendKind::Antigravity {
            Some(ResourceProfile::antigravity(&self.settings_root, &ResourceProfile::native_home()?, &[])?)
        } else { None };
        let transport = AcpTransport::launch(&self.runtime.resolve()?, &request.workspace, state, profile.as_ref().map(ResourceProfile::path)).await?;
        let initialized = transport.request("initialize", json!({"protocolVersion":1,
            "clientInfo":{"name":"forge-harness","version":env!("CARGO_PKG_VERSION")},
            "clientCapabilities":{"auth":{"terminal":true}}})).await?;
        let selected = initialized["authMethods"].as_array().and_then(|method_list|
            method_list.iter().find(|candidate|candidate["id"] == method))
            .with_context(|| format!("ACP agent did not advertise authentication method {method}"))?;
        ensure!(selected["type"] != "terminal", "This sign-in method requires the terminal action in /config → Provider setup");
        let result = transport.request("authenticate", json!({"methodId":method})).await;
        transport.shutdown().await?;
        result?;
        self.cleanup_execution(&request.runtime_key()?).await?;
        Ok(())
    }

    async fn capabilities(&self, request: BackendCatalogRequest) -> Result<CapabilityManifest> {
        let mut manifest = CapabilityManifest::from_descriptor(self.descriptor());
        if let Some(session) = self.session.lock().await.get(&request.runtime_key()?).cloned() {
            let mut descriptor = self.descriptor();
            descriptor.capability = self.runtime_capability(&session).await;
            let native_fork = descriptor.capability.native_fork;
            let native_steer = descriptor.capability.native_steer;
            manifest = CapabilityManifest::from_descriptor(descriptor);
            manifest.provider_version = session.initialized.pointer("/agentInfo/version").and_then(Value::as_str).map(str::to_owned);
            let catalog = session.state.catalog.read().await;
            manifest.notice = catalog["mcpResources"].as_array().into_iter().flatten()
                .filter(|resource|resource["state"] == "unavailable")
                .filter_map(|resource|resource["reason"].as_str().map(str::to_owned)).collect();
            for capability in &mut manifest.capability {
                let supported = match capability.id {
                    "native_fork" => Some(native_fork),
                    "native_steer" => Some(native_steer),
                    "resume" => Some(session.initialized.pointer("/agentCapabilities/loadSession").and_then(Value::as_bool) == Some(true)),
                    "effort_selection" => Some(find_option(&catalog, "thought_level").or_else(||find_option(&catalog, "effort")).is_some()),
                    _ => None,
                };
                if let Some(supported) = supported {
                    capability.support = if supported { super::super::capability::Support::Supported } else { super::super::capability::Support::Unsupported };
                }
            }
        } else {
            manifest.availability = "discovering";
            manifest.availability_reason = Some("Agent startup and authentication have not completed".into());
        }
        if !self.runtime.status().available {
            manifest.availability = "blocked";
            manifest.availability_reason = Some("ACP runtime is not installed. Open /config → Provider setup".into());
        }
        Ok(manifest)
    }
    fn descriptor(&self) -> BackendDescriptor {
        let claude = self.kind == BackendKind::Claude;
        BackendDescriptor { kind: self.kind, label: if claude { "Claude Code" } else { "Antigravity" }.into(),
            capability: BackendCapability { native_fork: claude, native_compact: claude, native_steer: claude,
                model_selection: true, effort_selection: claude, permission_control: true,
                catalog:CatalogCapability { skill:true,mcp:true,live_mcp_mutation:false },
                execution_mode_list: vec![PermissionMode::Read, PermissionMode::Write, PermissionMode::Yolo],
                compaction: CompactionCapability { lifecycle: claude, summary: claude, ..Default::default() },
                ..Default::default() } }
    }

    async fn shutdown(&self) -> Result<()> {
        for session in self.session.lock().await.drain().map(|(_, session)| session) { session.transport.shutdown().await?; }
        Ok(())
    }

    async fn skill_list(&self, request: BackendCatalogRequest) -> Result<Vec<SkillDefinition>> {
        let settings = crate::settings::SettingsStore::open(&self.settings_root)?;
        let catalog = settings.catalog(&request.workspace,&request.harness_session_id,
            if self.kind == BackendKind::Claude { "claude" } else { "antigravity" })?;
        let target = if self.kind == BackendKind::Claude { "claudecode" } else { "antigravity-cli" };
        Ok(catalog.skill
            .into_iter().filter(|skill|crate::settings::ResourceCatalog::supports_target(&skill.targets, target))
            .map(|skill|SkillDefinition {
                scope:skill.scope,
                resource_id:Some(skill.id.clone()),
                enabled:catalog.settings.desired.skill.get(&skill.id) != Some(&false),
                source:Some(if skill.id.starts_with("import:") { "imported" } else { "rulesync" }.into()),
                name:skill.name,description:skill.description,user_invocable:skill.user_invocable,
                path:Some(skill.source),argument_hint:None,
            }).collect())
    }

    async fn mcp_configuration(&self, request: BackendCatalogRequest) -> Result<Vec<McpDefinition>> {
        let settings = crate::settings::SettingsStore::open(&self.settings_root)?;
        let catalog = settings.catalog(&request.workspace,&request.harness_session_id,
            if self.kind == BackendKind::Claude { "claude" } else { "antigravity" })?;
        let target = if self.kind == BackendKind::Claude { "claudecode" } else { "antigravity-cli" };
        Ok(catalog.server
            .into_iter().filter(|server|crate::settings::ResourceCatalog::supports_target(&server.targets, target))
            .map(|server| {
                let enabled = catalog.settings.desired.mcp.get(&server.id) != Some(&false);
                McpDefinition { resource_id:Some(server.id),scope:server.scope,source_path:Some(server.source),name:server.name,transport:server.transport,enabled,
                    status:if enabled { McpStatus::Unavailable } else { McpStatus::Disabled },
                    status_detail:enabled.then(||"Configured selection. ACP does not expose server readiness".into()),
                    token_count:None,token_estimated:false,tools:Vec::new(),tool_error:None }
            }).collect())
    }

    async fn mcp_list(&self, request: BackendCatalogRequest) -> Result<Vec<McpDefinition>> {
        let mut definition = self.mcp_configuration(request.clone()).await?;
        if let Some(session) = self.session.lock().await.get(&request.runtime_key()?).cloned() {
            let catalog = session.state.catalog.read().await;
            for server in &mut definition {
                if let Some(resource) = catalog["mcpResources"].as_array().into_iter().flatten()
                    .find(|resource|resource["name"] == server.name && resource["state"] == "unavailable") {
                    server.status_detail = resource["reason"].as_str().map(str::to_owned);
                }
            }
        }
        Ok(definition)
    }

    async fn prompt_stream(&self, request: BackendRequest, sink: Option<BackendEventSink>) -> Result<BackendOutput> {
        let skills = self.permission.skills(Some(&self.settings_root), &request, self.kind)?;
        let _directories = self.permission.directories(Some(&self.settings_root), &request)?;
        let sink = skills.sink(sink);
        if self.kind == BackendKind::Claude {
            let existing = self.session.lock().await.get(&request.runtime_key()?).cloned();
            if let Some(existing) = existing
                && existing.native_ask != (request.execution_mode != PermissionMode::Yolo) {
                ensure!(existing.state.active.read().await.is_none(), "Permission policy changes require active execution cleanup");
                self.cleanup_execution(&request.runtime_key()?).await?;
            }
        }
        let tools = crate::control_tools::ControlToolRegistry.definition_list_for_context(request.control_context.as_ref(),request.owner()?.is_root());
        let session = self.session(catalog_request(&request), Some(tools)).await?;
        let _turn = session.turn.try_lock().context("ACP session already owns active work")?;
        self.configure(&session, &request).await?;
        session.state.begin(&request, &session.id, sink).await;
        self.conversation.lock().await.insert(request.runtime_key()?, session.id.clone());
        let text = match &request.input {
            BackendInput::Text { text } => text.clone(),
            BackendInput::Skill { name, arguments } => format!("/{name} {arguments}"),
        };
        let prompt = if text == "/compact" { text } else { format!("{}\n\n{}", request.system_message(), text) };
        let response = session.transport.request("session/prompt", json!({"sessionId":session.id,
            "prompt":[{"type":"text","text":prompt}]})).await;
        session.state.finish().await?;
        response.context("ACP prompt failed")?;
        let mut output = std::mem::take(&mut *session.state.output.lock().await);
        output.backend_session_id = Some(session.id.clone());
        output.capability = self.runtime_capability(&session).await;
        let catalog = session.state.catalog.read().await;
        output.runtime = BackendRuntime { provider: self.descriptor().label,
            model: catalog.pointer("/models/currentModelId").and_then(Value::as_str).map(str::to_owned)
                .or_else(|| find_option(&catalog,"model").and_then(|option|option["currentValue"].as_str()).map(str::to_owned)) };
        skills.output(&mut output)?;
        Ok(output)
    }

    async fn model_list(&self, request: BackendRequest) -> Result<Vec<BackendModel>> {
        let session = self.session(catalog_request(&request), None).await?;
        let catalog = session.state.catalog.read().await;
        let model_list = catalog.pointer("/models/availableModels").and_then(Value::as_array)
            .or_else(|| find_option(&catalog, "model").and_then(|option|option["options"].as_array()));
        let mut result = Vec::new();
        for model in model_list.into_iter().flatten() {
            let Some(id) = model["modelId"].as_str().or_else(|| model["value"].as_str()) else { continue };
            let reasoning: Vec<String> = find_option(&catalog,"thought_level").or_else(||find_option(&catalog,"effort"))
                .and_then(|option|option["options"].as_array()).into_iter().flatten()
                .filter_map(|option|option["value"].as_str().map(str::to_owned)).collect();
            result.push(BackendModel { id: id.into(), reasoning, default_reasoning: None,
                selected_reasoning: None, context_window: Vec::new(), default_context_window: None,
                selected_context_window: None, vision: session.initialized.pointer("/agentCapabilities/promptCapabilities/image").and_then(Value::as_bool) == Some(true),
                description: model["description"].as_str().or_else(|| model["name"].as_str()).map(str::to_owned),
                is_default: catalog.pointer("/models/currentModelId").and_then(Value::as_str) == Some(id) });
        }
        Ok(result)
    }

    async fn fork(&self, request: BackendForkRequest) -> Result<BackendForkResult> {
        ensure!(self.kind == BackendKind::Claude, "Antigravity does not implement native session fork");
        let session = self.session(catalog_request(&request.source), None).await?;
        ensure!(session.initialized.pointer("/agentCapabilities/sessionCapabilities/fork").is_some(), "Claude ACP did not advertise fork");
        let mut configuration = session.configuration.clone();
        configuration["sessionId"] = json!(session.id);
        configuration["cwd"] = json!(request.source.workspace);
        let response = session.transport.request("session/fork", configuration).await?;
        let id = response["sessionId"].as_str().context("ACP fork did not create a conversation")?;
        ensure!(id != session.id, "ACP fork returned the source conversation");
        Ok(BackendForkResult::unprofiled(id))
    }

    async fn compact(&self, request: BackendCompactionRequest) -> Result<BackendOutput> {
        ensure!(self.kind == BackendKind::Claude, "Antigravity has no manual compaction operation");
        let mut prompt = request.request;
        prompt.input = BackendInput::from_text("/compact");
        self.prompt_stream(prompt, None).await
    }

    async fn has_active_turn(&self, id: &str) -> bool {
        let session = self.session.lock().await.get(id).cloned();
        match session { Some(session) => session.state.active.read().await.is_some(), None => false }
    }

    async fn cancel_session(&self, id: &str) -> Result<()> {
        if let Some(session) = self.session.lock().await.get(id).cloned() {
            session.transport.notify("session/cancel", json!({"sessionId":session.id}))?;
        }
        Ok(())
    }

    async fn steer_session(&self, id: &str, input: crate::backend::SteeringInput) -> Result<()> {
        let text = input.text();
        let session = self.session.lock().await.get(id).cloned().context("ACP conversation is not active")?;
        ensure!(self.kind == BackendKind::Claude && session.initialized.pointer("/_meta/steering/supported").and_then(Value::as_bool) == Some(true),
            "ACP agent did not advertise steering support");
        let response = session.transport.request("_session/steering", json!({"sessionId":session.id,
            "prompt":[{"type":"text","text":text}],"_meta":{"steering":{"idleBehavior":"promptRequired"}}})).await?;
        ensure!(response["outcome"] != "promptRequired", "Active work finished before steering. Queue this prompt instead");
        Ok(())
    }

    async fn cleanup_execution(&self, id: &str) -> Result<()> {
        if let Some(session) = self.session.lock().await.remove(id) {
            *session.state.active.write().await = None;
            *session.state.control.lock().await = None;
            session.transport.shutdown().await?;
        }
        Ok(())
    }

    async fn active_session_id_for(&self, id: &str) -> Option<String> {
        self.conversation.lock().await.get(id).cloned()
    }
}

fn catalog_request(request: &BackendRequest) -> BackendCatalogRequest {
    BackendCatalogRequest { execution_owner: request.execution_owner.clone(), harness_session_id:request.harness_session_id.clone(),workspace:request.workspace.clone(),
        access:request.access.clone(),execution_mode:request.execution_mode,backend_session_id:request.backend_session_id.clone() }
}

fn find_option<'catalog>(catalog: &'catalog Value, category: &str) -> Option<&'catalog Value> {
    catalog["configOptions"].as_array()?.iter().find(|option|option["category"] == category || option["id"] == category)
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::settings::{ImportKind, SettingScope, SettingsStore};

    #[tokio::test]
    async fn catalog_includes_disabled_imports_and_refreshes_selection_without_provider_startup() -> Result<()> {
        let fixture = tempfile::tempdir()?;
        let workspace = fixture.path().join("workspace");
        std::fs::create_dir(&workspace)?;
        let server_source = fixture.path().join("native.json");
        std::fs::write(&server_source, json!({"mcpServers":{"imported-server":{
            "command":std::env::current_exe()?,"targets":["*"],"env":{"TOKEN":"native-credential-fixture"}
        }}}).to_string())?;
        let skill_source = fixture.path().join("native-skill");
        std::fs::create_dir(&skill_source)?;
        std::fs::write(skill_source.join("SKILL.md"), "---\nname: imported-skill\ndescription: Imported skill\ntargets: ['*']\n---\nPrivate instructions")?;
        let workspace = workspace.to_string_lossy().into_owned();
        let mut settings = SettingsStore::open(fixture.path())?;
        for (revision, kind, source) in [(0, ImportKind::Mcp, &server_source), (1, ImportKind::Skills, &skill_source)] {
            let preview = settings.preview_import(kind, source, SettingScope::Workspace, &workspace, "session", "claude")?;
            let selected = preview.select(&preview.fingerprint, &[preview.candidate[0].id.clone()])?;
            settings.import_operation(revision, SettingScope::Workspace, &workspace, "session", &selected,
                &format!("import-{revision}"), &json!({"fingerprint":preview.fingerprint}))?;
        }
        let request = BackendCatalogRequest { execution_owner: None, harness_session_id:"session".into(), workspace:workspace.clone(),
            access:Default::default(), execution_mode:PermissionMode::Read, backend_session_id:None };
        for kind in [BackendKind::Claude, BackendKind::Antigravity] {
            let backend = AcpBackend::new(kind, vec!["fixture-must-not-launch".into()],
                PermissionCoordinator::transient(fixture.path())?, fixture.path().to_owned())?;
            let server = backend.mcp_configuration(request.clone()).await?;
            let skill = backend.skill_list(request.clone()).await?;
            assert_eq!(server.len(), 1);
            assert_eq!(skill.len(), 1);
            assert!(!server[0].enabled && !skill[0].enabled);
            assert_eq!(server[0].status, McpStatus::Disabled);
            let mut foreign = request.clone();
            foreign.workspace = "another-workspace".into();
            assert!(backend.mcp_configuration(foreign.clone()).await?.is_empty());
            assert!(backend.skill_list(foreign).await?.is_empty());
        }
        let mut desired = settings.get(&workspace, "session", "claude")?.desired;
        for enabled in desired.mcp.values_mut().chain(desired.skill.values_mut()) { *enabled = true; }
        settings.update(2, SettingScope::Workspace, &workspace, "session", &desired)?;
        std::fs::remove_file(server_source)?;
        for kind in [BackendKind::Claude, BackendKind::Antigravity] {
            let backend = AcpBackend::new(kind, vec!["fixture-must-not-launch".into()],
                PermissionCoordinator::transient(fixture.path())?, fixture.path().to_owned())?;
            let server = backend.mcp_list(request.clone()).await?;
            let skill = backend.skill_list(request.clone()).await?;
            assert!(server[0].enabled && skill[0].enabled);
            assert_eq!(server[0].status, McpStatus::Unavailable);
            assert!(server[0].status_detail.as_ref().unwrap().contains("readiness"));
            assert!(!serde_json::to_string(&server)?.contains("native-credential-fixture"));
            assert!(backend.session.lock().await.is_empty());
            assert!(backend.conversation.lock().await.is_empty());
        }
        Ok(())
    }
}
