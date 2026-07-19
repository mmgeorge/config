mod event_decoder;

use crate::agent::{AgentCapability, AgentControlMode};
use crate::backend::approval::{
    ApprovalResolution, PermissionCoordinator, permission_from_copilot, protected_target,
};
use crate::backend::{
    Backend, BackendCapability, BackendCatalogRequest, BackendContextWindow, BackendDescriptor,
    BackendEvent, BackendEventSink, BackendForkRequest, BackendInput, BackendKind, BackendModel,
    BackendOutput, BackendRequest, CatalogCapability, CatalogMutation, McpDefinition, McpStatus,
    McpToolDefinition, PromptMode, SkillDefinition,
};
use crate::control_tools::{ControlToolInvocation, ControlToolRegistry, apply_invocation};
use crate::session::ExecutionMode;
use anyhow::{Context, Result};
use async_trait::async_trait;
use event_decoder::CopilotEventDecoder;
use github_copilot_sdk::handler::{PermissionHandler, PermissionResult};
use github_copilot_sdk::rpc::{
    CommandsInvokeRequest, McpConfigDisableRequest, McpConfigEnableRequest, McpDisableRequest,
    McpDiscoverRequest, McpEnableRequest, McpListToolsRequest, SessionsForkRequest,
    SkillsConfigSetDisabledSkillsRequest, SkillsDisableRequest, SkillsEnableRequest,
    SlashCommandInvocationResult,
};
use github_copilot_sdk::tool::ToolHandler;
use github_copilot_sdk::types::ContextTier;
use github_copilot_sdk::{
    CliProgram, Client, ClientOptions, DeliveryMode, MessageOptions, Model, PermissionRequestData,
    PermissionRequestKind, RequestId, ResumeSessionConfig, SessionConfig, SessionId,
    SetModelOptions, SystemMessageConfig, Tool, ToolInvocation, ToolResult,
};
use serde_json::Value;
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::{
    Arc, Mutex as StandardMutex,
    atomic::{AtomicU64, Ordering},
};
use tokio::sync::{Mutex, mpsc};

const SYSTEM_MESSAGE: &str = "You run inside DiffReview Harness. During planning, build the canonical JSON artifact with harness_plan_create and harness_plan_edit, request its latest version with harness_plan_read, then call harness_plan_submit with its exact ID and version. During execution, call harness_plan_task_report after each whole task and harness_plan_deviation when implementation diverges from accepted intent. Call harness_plan_question whenever a material user decision remains. While questions remain pending, use harness_question_answer only for an explicit user answer and harness_question_withdraw only when no material user decision remains. The question tools work in every mode. End the turn after a Harness question or plan control call. For a terminal goal state, call harness_goal_complete or harness_goal_blocked. Never claim a control action through ordinary prose alone.";

type SharedCopilotSession = Arc<github_copilot_sdk::session::Session>;
type CopilotSessionSlot = Arc<Mutex<Option<SharedCopilotSession>>>;

/// Owns the native Copilot SDK client, session, event decoder, and Harness callbacks.
pub struct CopilotBackend {
    command: Vec<String>,
    client: Mutex<Option<Client>>,
    client_start_count: AtomicU64,
    session_slot_by_harness: Mutex<HashMap<String, CopilotSessionSlot>>,
    turn_event_by_harness: Mutex<HashMap<String, Arc<CopilotTurnEvent>>>,
    control_router_by_harness: Mutex<HashMap<String, Arc<ControlToolRouter>>>,
    permission_context_by_harness: Mutex<HashMap<String, Arc<CopilotPermissionContext>>>,
    completed_event_by_harness: Mutex<HashMap<String, String>>,
    permission_coordinator: Arc<PermissionCoordinator>,
}

impl CopilotBackend {
    /// Build a Copilot backend with an isolated conservative permission registry.
    pub fn new(command: Vec<String>) -> Result<Self> {
        let permission_coordinator = PermissionCoordinator::transient(".")?;
        Self::new_with_permission_coordinator(command, permission_coordinator)
    }

    /// Build a Copilot backend with the shared Harness permission coordinator.
    pub fn new_with_permission_coordinator(
        command: Vec<String>,
        permission_coordinator: Arc<PermissionCoordinator>,
    ) -> Result<Self> {
        Ok(Self {
            command,
            client: Mutex::new(None),
            client_start_count: AtomicU64::new(0),
            session_slot_by_harness: Mutex::new(HashMap::new()),
            turn_event_by_harness: Mutex::new(HashMap::new()),
            control_router_by_harness: Mutex::new(HashMap::new()),
            permission_context_by_harness: Mutex::new(HashMap::new()),
            completed_event_by_harness: Mutex::new(HashMap::new()),
            permission_coordinator,
        })
    }

    /// Return how many Copilot SDK client process generations this backend launched.
    pub fn client_start_count(&self) -> u64 {
        self.client_start_count.load(Ordering::Relaxed)
    }

    async fn client(&self, workspace: &str) -> Result<Client> {
        let mut client = self.client.lock().await;
        if let Some(client) = client.as_ref() {
            return Ok(client.clone());
        }
        let mut options = ClientOptions::new().with_cwd(workspace);
        if let Some(program) = self.command.first() {
            options = options.with_program(CliProgram::Path(PathBuf::from(program)));
            if self.command.len() > 1 {
                options = options.with_prefix_args(self.command.iter().skip(1));
            }
        }
        let started = Client::start(options)
            .await
            .context("start GitHub Copilot SDK client")?;
        self.client_start_count.fetch_add(1, Ordering::Relaxed);
        *client = Some(started.clone());
        Ok(started)
    }

    async fn session(&self, request: &BackendRequest) -> Result<SharedCopilotSession> {
        let session_slot = self.session_slot(&request.harness_session_id).await;
        let mut active_session = session_slot.lock().await;
        if let Some(session) = active_session.as_ref() {
            if request.model != "default" {
                let client = self.client(&request.workspace).await?;
                let reasoning_effort = Self::reasoning_effort(&client, request).await?;
                session
                    .set_model(
                        &request.model,
                        model_options(reasoning_effort, request.context_window.as_deref()),
                    )
                    .await
                    .context("set Copilot session model")?;
            }
            return Ok(Arc::clone(session));
        }
        let client = self.client(&request.workspace).await?;
        let reasoning_effort = Self::reasoning_effort(&client, request).await?;
        let tool_list =
            self.control_tool_list(self.control_router(&request.harness_session_id).await);
        let system_message = SystemMessageConfig::new()
            .with_mode("append")
            .with_content(SYSTEM_MESSAGE);
        let permission_handler: Arc<dyn PermissionHandler> = Arc::new(CopilotPermissionHandler {
            context: self.permission_context(&request.harness_session_id).await,
        });
        let session = if let Some(session_id) = request.backend_session_id.as_deref() {
            let mut config = ResumeSessionConfig::new(SessionId::new(session_id));
            config.model = (request.model != "default").then(|| request.model.clone());
            config.reasoning_effort = reasoning_effort.clone();
            config.context_tier = request.context_window.clone();
            config.streaming = Some(true);
            config.system_message = Some(system_message);
            config.tools = Some(tool_list);
            config.working_directory = Some(PathBuf::from(&request.workspace));
            config.include_sub_agent_streaming_events = Some(true);
            config.enable_config_discovery = Some(true);
            config.enable_skills = Some(true);
            client
                .resume_session(config.with_permission_handler(permission_handler))
                .await
                .context("resume Copilot session")?
        } else {
            let mut config = SessionConfig::default();
            config.model = (request.model != "default").then(|| request.model.clone());
            config.reasoning_effort = reasoning_effort;
            config.context_tier = request.context_window.clone();
            config.streaming = Some(true);
            config.system_message = Some(system_message);
            config.tools = Some(tool_list);
            config.working_directory = Some(PathBuf::from(&request.workspace));
            config.include_sub_agent_streaming_events = Some(true);
            config.enable_config_discovery = Some(true);
            config.enable_skills = Some(true);
            client
                .create_session(config.with_permission_handler(permission_handler))
                .await
                .context("create Copilot session")?
        };
        let session = Arc::new(session);
        *active_session = Some(Arc::clone(&session));
        Ok(session)
    }

    async fn session_slot(&self, harness_session_id: &str) -> CopilotSessionSlot {
        let mut session_slot_by_harness = self.session_slot_by_harness.lock().await;
        Arc::clone(
            session_slot_by_harness
                .entry(harness_session_id.to_owned())
                .or_insert_with(|| Arc::new(Mutex::new(None))),
        )
    }

    async fn active_session(&self, harness_session_id: &str) -> Option<SharedCopilotSession> {
        let session_slot = self
            .session_slot_by_harness
            .lock()
            .await
            .get(harness_session_id)
            .cloned()?;
        session_slot.lock().await.clone()
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

    async fn reasoning_effort(client: &Client, request: &BackendRequest) -> Result<Option<String>> {
        if request.model == "default" {
            return Ok(None);
        }
        let model_list = client
            .list_models()
            .await
            .context("inspect Copilot model reasoning capabilities")?;
        let model_list = model_list
            .into_iter()
            .map(model_descriptor)
            .collect::<Vec<_>>();
        Ok(selected_reasoning_effort(&model_list, request))
    }

    async fn turn_event(&self, session_id: &str) -> Arc<CopilotTurnEvent> {
        let mut event_by_harness = self.turn_event_by_harness.lock().await;
        Arc::clone(event_by_harness.entry(session_id.to_owned()).or_default())
    }

    async fn control_router(&self, session_id: &str) -> Arc<ControlToolRouter> {
        let mut router_by_harness = self.control_router_by_harness.lock().await;
        Arc::clone(router_by_harness.entry(session_id.to_owned()).or_default())
    }

    async fn permission_context(&self, session_id: &str) -> Arc<CopilotPermissionContext> {
        let mut context_by_harness = self.permission_context_by_harness.lock().await;
        Arc::clone(
            context_by_harness
                .entry(session_id.to_owned())
                .or_insert_with(|| {
                    Arc::new(CopilotPermissionContext::new(Arc::clone(
                        &self.permission_coordinator,
                    )))
                }),
        )
    }

    fn control_tool_list(&self, router: Arc<ControlToolRouter>) -> Vec<Tool> {
        let handler: Arc<dyn ToolHandler> = Arc::new(CopilotControlToolHandler { router });
        ControlToolRegistry
            .definition_list()
            .into_iter()
            .map(|definition| {
                Tool::new(definition.name)
                    .with_description(definition.description)
                    .with_parameters(definition.input_schema)
                    .with_skip_permission(true)
                    .with_handler(Arc::clone(&handler))
            })
            .collect()
    }
}

fn capability() -> BackendCapability {
    BackendCapability {
        native_fork: true,
        native_compact: false,
        native_steer: true,
        native_turn_rollback: false,
        native_goal: false,
        model_selection: true,
        effort_selection: true,
        fast_mode: false,
        permission_control: true,
        execution_mode_list: vec![
            ExecutionMode::Read,
            ExecutionMode::Write,
            ExecutionMode::Full,
            ExecutionMode::Yolo,
        ],
        agent: AgentCapability {
            observe: true,
            catalog: false,
            spawn: AgentControlMode::ParentMediated,
            input: AgentControlMode::ParentMediated,
            interrupt: AgentControlMode::Unsupported,
            parallel: true,
        },
        catalog: CatalogCapability {
            skill: true,
            mcp: true,
            live_mcp_mutation: true,
        },
    }
}

#[async_trait]
impl Backend for CopilotBackend {
    fn descriptor(&self) -> BackendDescriptor {
        BackendDescriptor {
            kind: BackendKind::Copilot,
            label: "Copilot CLI".into(),
            capability: capability(),
        }
    }

    async fn prompt_stream(
        &self,
        request: BackendRequest,
        event_sink: Option<BackendEventSink>,
    ) -> Result<BackendOutput> {
        let turn_event = self.turn_event(&request.harness_session_id).await;
        let _turn_event_guard = turn_event.activate(event_sink.clone())?;
        self.permission_context(&request.harness_session_id)
            .await
            .configure(
                request.execution_mode,
                request.workspace.clone(),
                event_sink.clone(),
            )?;
        let session = self.session(&request).await?;
        let mut subscription = session.subscribe();
        let control_router = self.control_router(&request.harness_session_id).await;
        let mut control_stream = control_router.activate()?;
        let mut output = BackendOutput {
            backend_session_id: Some(session.id().to_string()),
            capability: capability(),
            runtime: crate::backend::BackendRuntime {
                provider: "Copilot CLI".into(),
                model: (request.model != "default").then(|| request.model.clone()),
            },
            ..BackendOutput::default()
        };
        let decoder = CopilotEventDecoder;
        let initial_task_snapshot = session
            .rpc()
            .plan()
            .read_sql_todos_with_dependencies()
            .await
            .context("read initial Copilot task snapshot")?;
        let initial_task_data = serde_json::to_value(initial_task_snapshot).unwrap_or(Value::Null);
        if initial_task_data
            .get("rows")
            .and_then(Value::as_array)
            .is_some_and(|row_list| !row_list.is_empty())
        {
            decoder.publish_task_snapshot(
                initial_task_data,
                "copilot-root".into(),
                &mut output,
                event_sink.as_ref(),
            );
        }
        let provider_text = match &request.input {
            BackendInput::Text { text } => text.clone(),
            BackendInput::Skill { name, arguments } => {
                let skill_available = session
                    .rpc()
                    .skills()
                    .list()
                    .await
                    .with_context(|| format!("resolve Copilot skill ${name}"))?
                    .skills
                    .into_iter()
                    .any(|skill| skill.name == *name && skill.enabled && skill.user_invocable);
                anyhow::ensure!(
                    skill_available,
                    "Copilot skill ${name} is unavailable or disabled"
                );
                match session
                    .rpc()
                    .commands()
                    .invoke(CommandsInvokeRequest {
                        name: name.clone(),
                        input: (!arguments.is_empty()).then(|| arguments.clone()),
                    })
                    .await
                    .with_context(|| format!("invoke Copilot skill ${name}"))?
                {
                    SlashCommandInvocationResult::AgentPrompt(result) => result.prompt,
                    _ => anyhow::bail!("Copilot skill ${name} did not produce an agent prompt"),
                }
            }
        };
        let prompt = format!(
            "Harness interaction contract: when the user explicitly asks for interactive or multiple-choice questions, call harness_plan_question with the complete question set. While questions remain pending, use harness_question_answer only for an explicit user answer and harness_question_withdraw only when no material user decision remains. The question tools work outside planning. Do not claim control actions through prose.\n\n{}",
            provider_text
        );
        session
            .send(MessageOptions::new(prompt))
            .await
            .context("send Copilot prompt")?;
        loop {
            tokio::select! {
                provider_event = subscription.recv() => {
                    let provider_event = provider_event.context("receive Copilot session event")?;
                    if provider_event.event_type == "session.idle" {
                        self.completed_event_by_harness
                            .lock()
                            .await
                            .insert(request.harness_session_id.clone(), provider_event.id.clone());
                        output.provider_checkpoint_id = Some(provider_event.id.clone());
                        break;
                    }
                    if provider_event.event_type == "session.todos_changed" {
                        let task_snapshot = session
                            .rpc()
                            .plan()
                            .read_sql_todos_with_dependencies()
                            .await
                            .context("read Copilot task snapshot")?;
                        decoder.publish_task_snapshot(
                            serde_json::to_value(task_snapshot).unwrap_or(Value::Null),
                            provider_event
                                .agent_id
                                .clone()
                                .unwrap_or_else(|| "copilot-root".into()),
                            &mut output,
                            event_sink.as_ref(),
                        );
                        continue;
                    }
                    let terminal_error = provider_event.event_type == "session.error"
                        && !provider_event.is_transient_error();
                    decoder.decode(&provider_event, &mut output, event_sink.as_ref());
                    if terminal_error {
                        let detail = provider_event
                            .data
                            .get("message")
                            .and_then(Value::as_str)
                            .unwrap_or("Copilot session failed");
                        anyhow::bail!("{detail}");
                    }
                }
                invocation = control_stream.receive() => {
                    if let Some(invocation) = invocation
                        && let Err(error) = apply_invocation(&invocation, &mut output)
                    {
                        output.control_error = Some(format!("{error:#}"));
                    }
                }
            }
        }
        while let Ok(invocation) = control_stream.try_receive() {
            if let Err(error) = apply_invocation(&invocation, &mut output) {
                output.control_error = Some(format!("{error:#}"));
            }
        }
        Ok(output)
    }

    async fn fork(&self, request: BackendForkRequest) -> Result<crate::backend::BackendForkResult> {
        let started = std::time::Instant::now();
        let mut target_request = request.source;
        let client = self.client(&target_request.workspace).await?;
        let backend_session_id = match (
            target_request.backend_session_id.clone(),
            request.checkpoint_id,
        ) {
            (Some(source_session_id), Some(to_event_id)) => client
                .rpc()
                .sessions()
                .fork(SessionsForkRequest {
                    name: None,
                    session_id: SessionId::new(source_session_id),
                    to_event_id: Some(to_event_id),
                })
                .await
                .context("fork Copilot session")?
                .session_id
                .to_string(),
            _ => self.session(&target_request).await?.id().to_string(),
        };
        target_request.harness_session_id = request.target_harness_session_id;
        target_request.backend_session_id = Some(backend_session_id.clone());
        self.session(&target_request).await?;
        Ok(crate::backend::BackendForkResult {
            backend_session_id,
            timing: vec![crate::backend::BackendTimingRecord {
                phase: "copilot.session_fork".into(),
                duration_ms: started.elapsed().as_secs_f64() * 1000.0,
            }],
        })
    }

    async fn model_list(&self, request: BackendRequest) -> Result<Vec<BackendModel>> {
        let client = self.client(&request.workspace).await?;
        let model_list = client.list_models().await.context("list Copilot models")?;
        if model_list.is_empty() {
            return Ok(vec![default_model_descriptor()]);
        }
        Ok(model_list.into_iter().map(model_descriptor).collect())
    }

    async fn skill_list(&self, request: BackendCatalogRequest) -> Result<Vec<SkillDefinition>> {
        let backend_request = Self::catalog_backend_request(&request);
        let session = self.session(&backend_request).await?;
        let skill_list = session
            .rpc()
            .skills()
            .list()
            .await
            .context("list Copilot skills")?;
        Ok(skill_list
            .skills
            .into_iter()
            .filter(|skill| skill.user_invocable)
            .map(|skill| SkillDefinition {
                name: skill.name,
                description: skill.description,
                enabled: skill.enabled,
                user_invocable: skill.user_invocable,
                path: skill.path,
                source: serde_json::to_value(skill.source)
                    .ok()
                    .and_then(|source| source.as_str().map(str::to_owned)),
                argument_hint: skill.argument_hint,
            })
            .collect())
    }

    async fn set_skill_enabled(
        &self,
        request: BackendCatalogRequest,
        name: &str,
        enabled: bool,
    ) -> Result<CatalogMutation> {
        let backend_request = Self::catalog_backend_request(&request);
        let client = self.client(&request.workspace).await?;
        let settings = client
            .rpc()
            .user()
            .settings()
            .get()
            .await
            .context("read Copilot disabled skills")?;
        let mut disabled_skill_list = settings
            .settings
            .get("disabledSkills")
            .and_then(|setting| setting.value.as_array())
            .into_iter()
            .flatten()
            .filter_map(|value| value.as_str().map(str::to_owned))
            .collect::<Vec<_>>();
        disabled_skill_list.retain(|disabled_name| disabled_name != name);
        if !enabled {
            disabled_skill_list.push(name.to_owned());
            disabled_skill_list.sort();
            disabled_skill_list.dedup();
        }
        client
            .rpc()
            .skills()
            .config()
            .set_disabled_skills(SkillsConfigSetDisabledSkillsRequest {
                disabled_skills: disabled_skill_list,
            })
            .await
            .context("persist Copilot disabled skills")?;
        let session = self.session(&backend_request).await?;
        if enabled {
            session
                .rpc()
                .skills()
                .enable(SkillsEnableRequest { name: name.into() })
                .await
                .with_context(|| format!("enable Copilot skill ${name}"))?;
        } else {
            session
                .rpc()
                .skills()
                .disable(SkillsDisableRequest { name: name.into() })
                .await
                .with_context(|| format!("disable Copilot skill ${name}"))?;
        }
        Ok(CatalogMutation {
            name: name.to_owned(),
            enabled,
            restart_required: false,
        })
    }

    async fn mcp_list(&self, request: BackendCatalogRequest) -> Result<Vec<McpDefinition>> {
        let backend_request = Self::catalog_backend_request(&request);
        let client = self.client(&request.workspace).await?;
        let session = self.session(&backend_request).await?;
        let discovery = client
            .rpc()
            .mcp()
            .discover(McpDiscoverRequest {
                working_directory: Some(request.workspace.clone()),
            })
            .await
            .context("discover Copilot MCP servers")?;
        let server_list = session
            .rpc()
            .mcp()
            .list()
            .await
            .context("list Copilot MCP status")?;
        let status_by_name = server_list
            .servers
            .into_iter()
            .map(|server| (server.name.clone(), server))
            .collect::<HashMap<_, _>>();
        let token_by_name = session
            .rpc()
            .metadata()
            .get_context_attribution()
            .await
            .ok()
            .and_then(|attribution| attribution.context_attribution)
            .map(|attribution| {
                attribution
                    .entries
                    .into_iter()
                    .filter(|entry| entry.kind == "mcpServer")
                    .map(|entry| {
                        (
                            entry
                                .id
                                .strip_prefix("mcpServer:")
                                .unwrap_or(&entry.label)
                                .to_owned(),
                            entry.tokens.max(0) as u64,
                        )
                    })
                    .collect::<HashMap<_, _>>()
            })
            .unwrap_or_default();
        let mut definition_list = Vec::new();
        for discovered in discovery.servers {
            let status_entry = status_by_name.get(&discovered.name);
            let status_text = status_entry
                .and_then(|server| serde_json::to_value(&server.status).ok())
                .and_then(|status| status.as_str().map(str::to_owned))
                .unwrap_or_else(|| {
                    if discovered.enabled {
                        "unavailable"
                    } else {
                        "disabled"
                    }
                    .into()
                });
            let status = match status_text.as_str() {
                "disabled" | "not_configured" => McpStatus::Disabled,
                "pending" => McpStatus::Loading,
                "connected" => McpStatus::Connected,
                "needs-auth" | "needs_auth" => McpStatus::NeedsAuthentication,
                "failed" => McpStatus::Failed,
                _ => McpStatus::Unavailable,
            };
            let (tools, tool_error) = if status == McpStatus::Connected {
                match session
                    .rpc()
                    .mcp()
                    .list_tools(McpListToolsRequest {
                        server_name: discovered.name.clone(),
                    })
                    .await
                {
                    Ok(result) => (
                        result
                            .tools
                            .into_iter()
                            .map(|tool| McpToolDefinition {
                                name: tool.name,
                                description: tool.description,
                            })
                            .collect(),
                        None,
                    ),
                    Err(error) => (Vec::new(), Some(format!("{error:#}"))),
                }
            } else {
                (Vec::new(), None)
            };
            let transport = discovered
                .r#type
                .and_then(|transport| serde_json::to_value(transport).ok())
                .and_then(|transport| transport.as_str().map(str::to_owned))
                .unwrap_or_else(|| "unknown".into());
            definition_list.push(McpDefinition {
                token_count: token_by_name.get(&discovered.name).copied(),
                token_estimated: false,
                enabled: discovered.enabled,
                status,
                status_detail: status_entry.and_then(|server| server.error.clone()),
                name: discovered.name,
                transport,
                tools,
                tool_error,
            });
        }
        definition_list.sort_by(|left, right| left.name.cmp(&right.name));
        Ok(definition_list)
    }

    async fn set_mcp_enabled(
        &self,
        request: BackendCatalogRequest,
        name: &str,
        enabled: bool,
    ) -> Result<CatalogMutation> {
        let backend_request = Self::catalog_backend_request(&request);
        let client = self.client(&request.workspace).await?;
        if enabled {
            client
                .rpc()
                .mcp()
                .config()
                .enable(McpConfigEnableRequest {
                    names: vec![name.into()],
                })
                .await
                .with_context(|| format!("persist enabled Copilot MCP {name}"))?;
        } else {
            client
                .rpc()
                .mcp()
                .config()
                .disable(McpConfigDisableRequest {
                    names: vec![name.into()],
                })
                .await
                .with_context(|| format!("persist disabled Copilot MCP {name}"))?;
        }
        let session = self.session(&backend_request).await?;
        if enabled {
            session
                .rpc()
                .mcp()
                .enable(McpEnableRequest {
                    server_name: name.into(),
                })
                .await
                .with_context(|| format!("enable Copilot MCP {name}"))?;
        } else {
            session
                .rpc()
                .mcp()
                .disable(McpDisableRequest {
                    server_name: name.into(),
                })
                .await
                .with_context(|| format!("disable Copilot MCP {name}"))?;
        }
        Ok(CatalogMutation {
            name: name.to_owned(),
            enabled,
            restart_required: false,
        })
    }

    async fn has_active_turn(&self, session_id: &str) -> bool {
        self.turn_event(session_id).await.is_active()
    }

    async fn steer(&self, _text: String) -> Result<()> {
        anyhow::bail!("Copilot steering requires a Harness session target")
    }

    async fn steer_session(&self, session_id: &str, text: String) -> Result<()> {
        let session = self
            .active_session(session_id)
            .await
            .context("Copilot session has not started")?;
        session
            .send(MessageOptions::new(text.clone()).with_mode(DeliveryMode::Immediate))
            .await
            .context("steer active Copilot turn")?;
        self.turn_event(session_id).await.publish_steering(text)?;
        Ok(())
    }

    async fn cancel(&self) -> Result<()> {
        Ok(())
    }

    async fn cancel_session(&self, session_id: &str) -> Result<()> {
        if let Some(session) = self.active_session(session_id).await {
            session.abort().await.context("abort active Copilot turn")?;
        }
        Ok(())
    }

    async fn active_session_id(&self) -> Option<String> {
        None
    }

    async fn active_session_id_for(&self, session_id: &str) -> Option<String> {
        self.active_session(session_id)
            .await
            .map(|session| session.id().to_string())
    }
}

/// Coordinates acknowledged steering events with the currently active Copilot turn.
#[derive(Default)]
struct CopilotTurnEvent {
    event_sink: StandardMutex<Option<BackendEventSink>>,
}

impl CopilotTurnEvent {
    /// Attach one event sink for the active turn and clear it when the turn exits.
    fn activate(&self, event_sink: Option<BackendEventSink>) -> Result<CopilotTurnEventGuard<'_>> {
        *self
            .event_sink
            .lock()
            .map_err(|_| anyhow::anyhow!("Copilot turn event lock poisoned"))? = event_sink;
        Ok(CopilotTurnEventGuard { owner: self })
    }

    fn is_active(&self) -> bool {
        self.event_sink
            .lock()
            .expect("Copilot turn event lock poisoned")
            .is_some()
    }

    /// Publish steering only after the Copilot SDK accepts the immediate message.
    fn publish_steering(&self, text: String) -> Result<()> {
        let event_sink = self
            .event_sink
            .lock()
            .map_err(|_| anyhow::anyhow!("Copilot turn event lock poisoned"))?
            .clone();
        if let Some(event_sink) = event_sink {
            event_sink
                .send(BackendEvent::steering_input(text))
                .map_err(|_| anyhow::anyhow!("Copilot turn event receiver closed"))?;
        }
        Ok(())
    }
}

/// Clears the active Copilot event sink across success and failure exits.
struct CopilotTurnEventGuard<'a> {
    owner: &'a CopilotTurnEvent,
}

impl Drop for CopilotTurnEventGuard<'_> {
    fn drop(&mut self) {
        if let Ok(mut event_sink) = self.owner.event_sink.lock() {
            *event_sink = None;
        }
    }
}

fn model_descriptor(model: Model) -> BackendModel {
    let output_limit = model
        .capabilities
        .limits
        .as_ref()
        .and_then(|limit| limit.max_output_tokens)
        .unwrap_or(0);
    let token_price = model
        .billing
        .as_ref()
        .and_then(|billing| billing.token_prices.as_ref());
    let mut context_window = Vec::new();
    if let Some(default_limit) = token_price.and_then(|price| price.max_prompt_tokens) {
        context_window.push(BackendContextWindow {
            id: "default".into(),
            token_limit: u64::try_from(default_limit.saturating_add(output_limit)).ok(),
        });
    } else if let Some(default_limit) = model
        .capabilities
        .limits
        .as_ref()
        .and_then(|limit| limit.max_context_window_tokens)
    {
        context_window.push(BackendContextWindow {
            id: "default".into(),
            token_limit: u64::try_from(default_limit).ok(),
        });
    }
    if let Some(long_limit) = token_price
        .and_then(|price| price.long_context.as_ref())
        .and_then(|long_context| long_context.max_prompt_tokens)
    {
        context_window.push(BackendContextWindow {
            id: "long_context".into(),
            token_limit: u64::try_from(long_limit.saturating_add(output_limit)).ok(),
        });
    }
    let vision = model
        .capabilities
        .supports
        .as_ref()
        .and_then(|supports| supports.vision)
        .unwrap_or_else(|| {
            model
                .capabilities
                .limits
                .as_ref()
                .is_some_and(|limit| limit.vision.is_some())
        });
    BackendModel {
        is_default: model.id == "auto",
        id: model.id,
        reasoning: model.supported_reasoning_efforts.unwrap_or_default(),
        default_reasoning: model.default_reasoning_effort,
        selected_reasoning: None,
        context_window,
        default_context_window: Some("default".into()),
        selected_context_window: None,
        vision,
        description: None,
    }
}

fn default_model_descriptor() -> BackendModel {
    BackendModel {
        is_default: true,
        id: "default".into(),
        reasoning: Vec::new(),
        default_reasoning: None,
        selected_reasoning: None,
        context_window: Vec::new(),
        default_context_window: None,
        selected_context_window: None,
        vision: false,
        description: None,
    }
}

fn selected_reasoning_effort(
    model_list: &[BackendModel],
    request: &BackendRequest,
) -> Option<String> {
    if request.model == "default" {
        return None;
    }
    model_list
        .iter()
        .find(|model| model.id == request.model)
        .filter(|model| {
            model
                .reasoning
                .iter()
                .any(|effort| effort == &request.effort)
        })
        .map(|_| request.effort.clone())
}

fn model_options(
    reasoning_effort: Option<String>,
    context_window: Option<&str>,
) -> Option<SetModelOptions> {
    let mut options = SetModelOptions::default();
    if let Some(reasoning_effort) = reasoning_effort {
        options = options.with_reasoning_effort(reasoning_effort);
    }
    if let Some(context_window) = context_window {
        let context_tier = match context_window {
            "default" => ContextTier::Default,
            "long_context" => ContextTier::LongContext,
            _ => return Some(options),
        };
        options = options.with_context_tier(context_tier);
    }
    (options.reasoning_effort.is_some() || options.context_tier.is_some()).then_some(options)
}

#[derive(Default)]
struct ControlToolRouter {
    sender: StandardMutex<Option<mpsc::UnboundedSender<ControlToolInvocation>>>,
}

impl ControlToolRouter {
    fn activate(self: &Arc<Self>) -> Result<ControlToolStream> {
        let mut sender = self
            .sender
            .lock()
            .map_err(|_| anyhow::anyhow!("Copilot control-tool router lock poisoned"))?;
        anyhow::ensure!(
            sender.is_none(),
            "Copilot control-tool router already active"
        );
        let (active_sender, receiver) = mpsc::unbounded_channel();
        *sender = Some(active_sender);
        Ok(ControlToolStream {
            router: Arc::clone(self),
            receiver,
        })
    }

    fn route(&self, invocation: ControlToolInvocation) -> Result<()> {
        self.sender
            .lock()
            .map_err(|_| anyhow::anyhow!("Copilot control-tool router lock poisoned"))?
            .as_ref()
            .context("Copilot control tool ran without an active turn")?
            .send(invocation)
            .context("route Copilot control tool")
    }
}

struct ControlToolStream {
    router: Arc<ControlToolRouter>,
    receiver: mpsc::UnboundedReceiver<ControlToolInvocation>,
}

impl ControlToolStream {
    async fn receive(&mut self) -> Option<ControlToolInvocation> {
        self.receiver.recv().await
    }

    fn try_receive(&mut self) -> Result<ControlToolInvocation, mpsc::error::TryRecvError> {
        self.receiver.try_recv()
    }
}

impl Drop for ControlToolStream {
    fn drop(&mut self) {
        if let Ok(mut sender) = self.router.sender.lock() {
            *sender = None;
        }
    }
}

struct CopilotControlToolHandler {
    router: Arc<ControlToolRouter>,
}

#[async_trait]
impl ToolHandler for CopilotControlToolHandler {
    async fn call(
        &self,
        invocation: ToolInvocation,
    ) -> std::result::Result<ToolResult, github_copilot_sdk::Error> {
        let name = invocation.tool_name;
        let result = self.router.route(ControlToolInvocation {
            name: name.clone(),
            arguments: invocation.arguments,
        });
        Ok(match result {
            Ok(()) => ToolResult::Text(format!("{name} accepted by DiffReview Harness")),
            Err(error) => ToolResult::Text(format!("{name} failed: {error:#}")),
        })
    }
}

struct CopilotPermissionState {
    execution_mode: ExecutionMode,
    workspace: String,
    event_sink: Option<BackendEventSink>,
}

struct CopilotPermissionContext {
    coordinator: Arc<PermissionCoordinator>,
    state: StandardMutex<CopilotPermissionState>,
}

impl CopilotPermissionContext {
    fn new(coordinator: Arc<PermissionCoordinator>) -> Self {
        Self {
            coordinator,
            state: StandardMutex::new(CopilotPermissionState {
                execution_mode: ExecutionMode::Read,
                workspace: ".".into(),
                event_sink: None,
            }),
        }
    }

    fn configure(
        &self,
        execution_mode: ExecutionMode,
        workspace: String,
        event_sink: Option<BackendEventSink>,
    ) -> Result<()> {
        *self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("Copilot permission context lock poisoned"))? =
            CopilotPermissionState {
                execution_mode,
                workspace,
                event_sink,
            };
        Ok(())
    }
}

struct CopilotPermissionHandler {
    context: Arc<CopilotPermissionContext>,
}

#[async_trait]
impl PermissionHandler for CopilotPermissionHandler {
    async fn handle(
        &self,
        _session_id: SessionId,
        request_id: RequestId,
        data: PermissionRequestData,
    ) -> PermissionResult {
        let (execution_mode, workspace, event_sink) = match self.context.state.lock() {
            Ok(state) => (
                state.execution_mode,
                state.workspace.clone(),
                state.event_sink.clone(),
            ),
            Err(_) => {
                return PermissionResult::reject(Some("permission context unavailable".into()));
            }
        };
        let kind = permission_kind(data.kind);
        let params = serde_json::to_value(data).unwrap_or(Value::Null);
        let request = permission_from_copilot(request_id.to_string(), kind, params, &workspace);
        let protected = match self.context.coordinator.store().read() {
            Ok(store) => protected_target(&request, &store),
            Err(_) => return PermissionResult::reject(Some("permission store unavailable".into())),
        };
        if protected {
            return PermissionResult::reject(Some("target is protected by Harness".into()));
        }
        match self
            .context
            .coordinator
            .authorize(execution_mode, request, event_sink.as_ref())
            .await
        {
            Ok(
                ApprovalResolution::AllowOnce
                | ApprovalResolution::AllowExact
                | ApprovalResolution::AllowBroad,
            ) => PermissionResult::approve_once(),
            Ok(_) => PermissionResult::reject(None),
            Err(error) => PermissionResult::reject(Some(format!("{error:#}"))),
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
mod test {
    use super::*;

    #[test]
    fn descriptor_exposes_only_stable_native_capabilities() {
        let backend = CopilotBackend::new(Vec::new()).unwrap();
        let descriptor = backend.descriptor();
        assert_eq!(descriptor.kind, BackendKind::Copilot);
        assert!(descriptor.capability.native_steer);
        assert!(descriptor.capability.native_fork);
        assert!(!descriptor.capability.native_compact);
        assert!(descriptor.capability.agent.observe);
    }

    #[tokio::test]
    async fn routes_control_tools_without_provider_specific_schemas() {
        let router = Arc::new(ControlToolRouter::default());
        let mut stream = router.activate().unwrap();
        router
            .route(ControlToolInvocation {
                name: "harness_goal_complete".into(),
                arguments: Value::Null,
            })
            .unwrap();
        assert_eq!(
            stream.receive().await.unwrap().name,
            "harness_goal_complete"
        );
    }

    #[test]
    fn omits_reasoning_effort_for_auto_and_unsupported_models() {
        let mut request = BackendRequest {
            harness_session_id: "harness-session".into(),
            workspace: "D:/repo".into(),
            input: BackendInput::from_text("test"),
            mode: crate::backend::PromptMode::Chat,
            model: "default".into(),
            effort: "low".into(),
            context_window: None,
            fast_mode: false,
            execution_mode: ExecutionMode::Read,
            backend_session_id: None,
        };
        let model_list = vec![BackendModel {
            id: "gpt-test".into(),
            reasoning: vec!["medium".into()],
            default_reasoning: Some("medium".into()),
            selected_reasoning: None,
            context_window: Vec::new(),
            default_context_window: None,
            selected_context_window: None,
            vision: false,
            description: None,
            is_default: false,
        }];
        assert_eq!(selected_reasoning_effort(&model_list, &request), None);
        request.model = "gpt-test".into();
        assert_eq!(selected_reasoning_effort(&model_list, &request), None);
        request.effort = "medium".into();
        assert_eq!(
            selected_reasoning_effort(&model_list, &request).as_deref(),
            Some("medium")
        );
    }

    #[test]
    fn provides_an_auto_model_when_native_discovery_is_empty() {
        let model = default_model_descriptor();

        assert!(model.is_default);
        assert_eq!(model.id, "default");
        assert!(model.reasoning.is_empty());
        assert!(model.context_window.is_empty());
    }

    #[test]
    fn maps_provider_reasoning_context_and_vision_metadata() {
        let model: Model = serde_json::from_value(serde_json::json!({
            "id": "gpt-test",
            "name": "Ignored display label",
            "defaultReasoningEffort": "low",
            "supportedReasoningEfforts": ["low", "high"],
            "capabilities": {
                "limits": { "max_output_tokens": 28000 },
                "supports": { "reasoningEffort": true, "vision": true }
            },
            "billing": {
                "tokenPrices": {
                    "maxPromptTokens": 300000,
                    "longContext": { "maxPromptTokens": 1072000 }
                }
            }
        }))
        .unwrap();

        let descriptor = model_descriptor(model);

        assert_eq!(descriptor.id, "gpt-test");
        assert_eq!(descriptor.reasoning, ["low", "high"]);
        assert_eq!(descriptor.default_reasoning.as_deref(), Some("low"));
        assert_eq!(descriptor.context_window[0].token_limit, Some(328_000));
        assert_eq!(descriptor.context_window[1].token_limit, Some(1_100_000));
        assert!(descriptor.vision);
    }

    #[test]
    fn applies_reasoning_and_context_to_a_native_model_switch() {
        let options = model_options(Some("high".into()), Some("long_context")).unwrap();

        assert_eq!(options.reasoning_effort.as_deref(), Some("high"));
        assert!(matches!(
            options.context_tier,
            Some(ContextTier::LongContext)
        ));
    }

    #[test]
    fn publishes_steering_only_while_a_copilot_turn_is_active() {
        let turn_event = CopilotTurnEvent::default();
        let (event_sink, mut event_stream) = mpsc::unbounded_channel();
        let turn_guard = turn_event.activate(Some(event_sink)).unwrap();

        turn_event.publish_steering("accepted".into()).unwrap();
        let event = event_stream.try_recv().unwrap();
        assert_eq!(event.kind, "steering_input");
        assert_eq!(event.text.as_deref(), Some("accepted"));

        drop(turn_guard);
        turn_event.publish_steering("late".into()).unwrap();
        assert!(event_stream.try_recv().is_err());
    }
}
