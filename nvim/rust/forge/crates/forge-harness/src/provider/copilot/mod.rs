pub(crate) mod attachments;
mod profile;
use crate::permissions::{
    provider::ProviderPermissionContext, session_directory::SessionDirectory,
};
#[cfg(test)]
use crate::provider::ProviderAdapter as _;
use crate::tools::router::ControlToolRouter;
#[cfg(test)]
use forge_providers::copilot::mcp::launch_request as mcp_launch_request;
#[cfg(test)]
pub(crate) mod approval;
use forge_providers::copilot::sessions::{
    CopilotSessions, PreparedSession, SessionReuse as CopilotSessionReuse, SessionSelection,
};
#[cfg(test)]
mod event_decoder;
mod host_turn;
#[cfg(test)]
use forge_providers::copilot::prompt::completes_exchange;
#[cfg(test)]
mod file_change_test;

mod mcp;
pub(crate) mod mock_stream;
#[cfg(test)]
use forge_providers::copilot::catalog::model_descriptor;
use forge_providers::copilot::runtime::CopilotRuntime;

use forge_providers::copilot::text_generation;
use forge_providers::copilot::turn;
#[cfg(test)]
mod turn_test;

use crate::agent::AgentCapability;
use crate::provider::approval::PermissionCoordinator;
use crate::provider::{
    CatalogCapability, CatalogMutation, McpDefinition, PromptMode, ProviderCapability,
    ProviderCatalogRequest, ProviderCompletion, ProviderDescriptor, ProviderError, ProviderEvent,
    ProviderEventSink, ProviderForkRequest, ProviderInput, ProviderKind, ProviderModel,
    ProviderOutput, ProviderRequest, SkillDefinition,
};
use crate::session::ApprovalMode;
use crate::tools::{ControlToolRegistry, ControlTurnContext};
use anyhow::{Context, Result};
use async_trait::async_trait;
use forge_providers::copilot::handle::CopilotClient as Client;
#[cfg(test)]
use forge_providers::copilot::handle::{default_model_descriptor, model_options};
#[cfg(test)]
use github_copilot_sdk::handler::PermissionResult;
#[cfg(test)]
use github_copilot_sdk::session_events::ReasoningSummary;
#[cfg(test)]
use github_copilot_sdk::types::ContextTier;
#[cfg(test)]
use github_copilot_sdk::{Model, RequestId, SessionId};
#[cfg(test)]
use serde_json::Value;
use serde_json::json;
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;
use tokio::sync::Mutex;
use turn::{CopilotRetraction, CopilotTurnEvent};

const SYSTEM_MESSAGE: &str = super::HARNESS_SYSTEM_MESSAGE;

fn native_agent_exclusions() -> Vec<String> {
    [
        "task",
        "list_agents",
        "read_agent",
        "write_agent",
        "send_inbox",
    ]
    .into_iter()
    .map(|name| format!("builtin:{name}"))
    .collect()
}

type SharedCopilotSession = Arc<forge_providers::copilot::handle::CopilotSession>;
/// Owns the native Copilot SDK client, session, event decoder, and Harness callbacks.
pub struct CopilotProvider {
    runtime: CopilotRuntime,
    settings_root: Option<PathBuf>,
    trace: Arc<crate::trace::TraceStore>,
    sessions: Arc<CopilotSessions<crate::settings::ResourceProfile>>,
    control_router_by_harness: Mutex<HashMap<String, Arc<ControlToolRouter>>>,
    permission_context_by_harness: Mutex<HashMap<String, Arc<ProviderPermissionContext>>>,
    permission_coordinator: Arc<PermissionCoordinator>,
}

impl CopilotProvider {
    /// Build a Copilot provider with an isolated conservative permission registry.
    pub fn new(command: Vec<String>) -> Result<Self> {
        let permission_coordinator = PermissionCoordinator::transient(".")?;
        Self::new_with_permission_coordinator(
            command,
            permission_coordinator,
            Arc::new(crate::trace::TraceStore::open(std::path::Path::new("."))?),
            None,
        )
    }

    /// Build a Copilot provider with the shared Harness permission coordinator.
    pub fn new_with_permission_coordinator(
        command: Vec<String>,
        permission_coordinator: Arc<PermissionCoordinator>,
        trace: Arc<crate::trace::TraceStore>,
        settings_root: Option<PathBuf>,
    ) -> Result<Self> {
        Ok(Self {
            settings_root,
            sessions: Arc::new(CopilotSessions::new(trace.clone())),
            trace,
            runtime: CopilotRuntime::new(command),

            control_router_by_harness: Mutex::new(HashMap::new()),
            permission_context_by_harness: Mutex::new(HashMap::new()),
            permission_coordinator,
        })
    }

    /// Return how many Copilot SDK client process generations this provider launched.
    pub fn client_start_count(&self) -> u64 {
        self.runtime.start_count()
    }

    async fn client(&self, workspace: &str) -> Result<Client> {
        Ok(self.runtime.client(workspace).await?)
    }

    async fn session(
        &self,
        request: &ProviderRequest,
        reuse: CopilotSessionReuse,
    ) -> Result<SharedCopilotSession> {
        capability().validate_access(&request.access)?;
        let key = request.runtime_key()?;
        let client = async {
            let client = self.client(&request.workspace).await?;
            let reasoning = Self::reasoning_effort(&client, request).await?;
            Ok((client, reasoning))
        };
        let permission = self.permission_context(&key).await;
        let tool_list = self.control_tool_list(request);
        let selected_tools = request.control_context.as_ref().map(|_| tool_list.clone());
        let selection = SessionSelection {
            runtime_key: key.clone(),
            correlation_id: request.harness_session_id.clone(),
            session_id: request.provider_session_id.clone(),
            model: (request.model != "default").then(|| request.model.clone()),
            context_tier: request.context_window.clone(),
            tools: selected_tools,
            reuse,
        };
        self.sessions
            .acquire(
                client,
                selection,
                permission.clone(),
                self.control_router(&key).await,
                || async {
                    let resource_request = ProviderCatalogRequest {
                        execution_owner: request.execution_owner.clone(),
                        harness_session_id: request.harness_session_id.clone(),
                        workspace: request.workspace.clone(),
                        access: request.access.clone(),
                        approval_mode: request.approval_mode,
                        provider_session_id: request.provider_session_id.clone(),
                    };
                    let resources = self
                        .settings_root
                        .as_ref()
                        .filter(|_| request.mode != PromptMode::Background)
                        .map(|root| {
                            crate::settings::SessionResources::load(
                                root,
                                &resource_request,
                                "copilot",
                                "copilotcli",
                            )
                        })
                        .transpose()?;
                    let profile = resources
                        .as_ref()
                        .map(|resources| {
                            crate::settings::ResourceProfile::copilot(
                                self.settings_root
                                    .as_ref()
                                    .expect("configured resource root"),
                                &resources.selected_skill,
                            )
                        })
                        .transpose()?;
                    let skill_directories = profile
                        .as_ref()
                        .map(|profile| vec![profile.path().join("skills")]);

                    let skills = resources.as_ref().map(|resources| {
                        resources
                            .skill
                            .iter()
                            .map(|skill| forge_providers::copilot::handle::SkillSelection {
                                name: skill.name.clone(),
                                enabled: resources
                                    .selected_skill
                                    .iter()
                                    .any(|selected| selected.name == skill.name),
                            })
                            .collect()
                    });
                    let mcp = resources.as_ref().map(|resources| resources.launch.clone());
                    Ok(PreparedSession {
                        options: forge_providers::copilot::session::SessionOptions {
                            session_id: request.provider_session_id.clone(),
                            model: (request.model != "default").then(|| request.model.clone()),
                            reasoning_effort: None,
                            context_tier: request.context_window.clone(),
                            workspace: PathBuf::from(&request.workspace),
                            mode: request.mode,
                            system_message: if request.mode == PromptMode::Background {
                                request.system_message()
                            } else {
                                SYSTEM_MESSAGE.to_owned()
                            },
                            resources: if resources.is_some() {
                                forge_providers::copilot::session::ResourceDiscovery::Managed {
                                    skill_directories: skill_directories.unwrap_or_default(),
                                }
                            } else {
                                forge_providers::copilot::session::ResourceDiscovery::Native
                            },
                            excluded_tools: native_agent_exclusions(),
                            tools: tool_list,
                        },
                        skills,
                        mcp,
                        profile,
                    })
                },
                |session| {
                    permission.set_session_directory(SessionDirectory::new(
                        session.id().to_owned(),
                        session.workspace_path(),
                        session.cwd(),
                    )?)
                },
            )
            .await
    }

    async fn active_session(&self, key: &str) -> Option<SharedCopilotSession> {
        self.sessions.active(key).await
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

    async fn reasoning_effort(
        client: &Client,
        request: &ProviderRequest,
    ) -> Result<Option<String>> {
        if request.model == "default" {
            return Ok(None);
        }
        Ok(selected_reasoning_effort(&client.models().await?, request))
    }

    async fn turn_event(&self, key: &str) -> Arc<CopilotTurnEvent> {
        self.sessions.turn(key).await
    }

    async fn control_router(&self, session_id: &str) -> Arc<ControlToolRouter> {
        let mut router_by_harness = self.control_router_by_harness.lock().await;
        Arc::clone(router_by_harness.entry(session_id.to_owned()).or_default())
    }

    async fn permission_context(&self, session_id: &str) -> Arc<ProviderPermissionContext> {
        let mut context_by_harness = self.permission_context_by_harness.lock().await;
        Arc::clone(
            context_by_harness
                .entry(session_id.to_owned())
                .or_insert_with(|| {
                    Arc::new(ProviderPermissionContext::new(Arc::clone(
                        &self.permission_coordinator,
                    )))
                }),
        )
    }

    fn control_tool_list(
        &self,
        request: &ProviderRequest,
    ) -> Vec<crate::tools::ControlToolDefinition> {
        ControlToolRegistry.definition_list_for_context(
            request.control_context.as_ref(),
            request.owner().is_ok_and(|owner| owner.is_root()),
        )
    }
}

pub(super) fn capability() -> ProviderCapability {
    ProviderCapability {
        integration: super::ProviderIntegration {
            background_generation: true,
            isolated_generation: true,
            terminal_termination: true,
            native_agent_suppression: true,
            mcp_runtime_status: true,
            mcp_tool_catalog: true,
            tracked_submission: true,
            resource_target: Some("copilotcli".into()),
            ..Default::default()
        },
        os_sandbox: false,
        native_fork: true,
        native_compact: true,
        compaction: super::CompactionCapability {
            lifecycle: true,
            background: true,
            summary: true,
            metrics: true,
        },
        native_steer: true,
        native_turn_rollback: true,
        native_goal: false,
        model_selection: true,
        effort_selection: true,
        fast_mode: false,
        ultrafast_mode: false,
        permission_control: true,
        background_terminals: true,
        approval_mode_list: vec![ApprovalMode::Ask, ApprovalMode::Deny, ApprovalMode::Allow],
        agent: AgentCapability::default(),
        catalog: CatalogCapability {
            skill: true,
            mcp: true,
            live_mcp_mutation: false,
        },
    }
}

#[async_trait]
impl crate::provider::ProviderAdapter for CopilotProvider {
    type Host = crate::provider::HarnessProviderHost;
    async fn reconcile_submission(
        &self,
        submission: &super::submission::SubmissionRecord,
        known_receipts: &std::collections::HashSet<String>,
    ) -> Result<Option<String>> {
        let super::submission::SubmissionState::Sending { target } = &submission.delivery else {
            return Ok(None);
        };
        let Some((session_id, checkpoint)) = target.native_checkpoint("copilot", 1) else {
            return Ok(None);
        };
        let cursor = checkpoint["cursor"]
            .as_str()
            .context("Copilot recovery checkpoint omitted cursor")?;
        let prompt = checkpoint["prompt"]
            .as_str()
            .context("Copilot recovery checkpoint omitted prompt")?;
        let mut request = submission.request.clone();
        request.provider_session_id = Some(session_id.to_owned());
        let session = self
            .session(&request, CopilotSessionReuse::Compatible)
            .await?;
        Ok(session
            .reconcile_submission(cursor, prompt, known_receipts)
            .await?)
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
                "shared resource selection requires the Forge settings store"
            );
            return Ok(());
        }
        self.cleanup_execution(&request.runtime_key()?).await?;
        self.session(
            &Self::catalog_provider_request(&request),
            CopilotSessionReuse::Refresh,
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
        self.trace.record(&request.harness_session_id, "copilot.generation.request", json!({
            "purpose": purpose.name(),
            "model":model,"reasoning_effort":reasoning_effort,"instructions":purpose.instructions(),"history":history}));
        let generation = forge_provider_api::generation::TextGenerationRequest {
            model: model.to_owned(),
            reasoning: match reasoning_effort {
                Some(level) => {
                    forge_provider_api::generation::ReasoningSelection::Level(level.into())
                }
                None if purpose == super::TextGeneration::SessionName
                    && model != "default"
                    && model != "auto" =>
                {
                    forge_provider_api::generation::ReasoningSelection::Lowest
                }
                None => forge_provider_api::generation::ReasoningSelection::Default,
            },
            instructions: purpose.instructions().to_owned(),
            input: purpose.input(history),
        };
        let result = text_generation::generate(
            self.runtime.command(),
            &request.workspace,
            &generation,
            &purpose,
        )
        .await;
        self.trace.record(
            &request.harness_session_id,
            "copilot.generation.result",
            match &result {
                Ok(text) => json!({"text":text}),
                Err(error) => json!({"error":format!("{error:#}")}),
            },
        );
        result
    }
    async fn terminate_terminal(&self, request: ProviderCatalogRequest, id: &str) -> Result<()> {
        self.sessions.terminate(&request.runtime_key()?, id).await
    }

    async fn background_terminals(
        &self,
        request: ProviderCatalogRequest,
    ) -> Result<super::terminal::TerminalSnapshot> {
        self.sessions
            .terminals(&request.runtime_key()?, &request.harness_session_id)
            .await
    }

    async fn shutdown(&self) -> Result<()> {
        self.runtime.close_admission();
        self.sessions.close_observers().await;
        self.runtime.shutdown().await?;
        self.sessions.clear().await;
        self.control_router_by_harness.lock().await.clear();
        self.permission_context_by_harness.lock().await.clear();
        Ok(())
    }

    fn descriptor(&self) -> ProviderDescriptor {
        ProviderDescriptor {
            kind: ProviderKind::Copilot,
            label: "Copilot CLI".into(),
            capability: capability(),
        }
    }

    async fn prompt_stream(
        &self,
        request: ProviderRequest,
        event_sink: ProviderEventSink,
    ) -> std::result::Result<ProviderCompletion, ProviderError> {
        let output: std::result::Result<ProviderCompletion, ProviderError> = async {
            let event_sink = Some(event_sink);
            let turn_event = self.turn_event(&request.runtime_key()?).await;
            let skills = self.permission_coordinator.skills(
                self.settings_root.as_deref(),
                &request,
                super::ProviderKind::Copilot,
            )?;
            let _directories = self
                .permission_coordinator
                .directories(self.settings_root.as_deref(), &request)?;
            let event_sink = skills.sink(event_sink);
            let turn_event_guard = turn_event.activate(
                event_sink.clone(),
                request
                    .control_context
                    .as_ref()
                    .and_then(|context| context.submission_cancelled.clone()),
            )?;
            self.permission_context(&request.runtime_key()?)
                .await
                .configure(
                    request.owner()?,
                    request.approval_mode,
                    request.workspace.clone(),
                    event_sink.clone(),
                )?;
            self.permission_context(&request.runtime_key()?)
                .await
                .bind_context(request.control_context.as_ref())?;
            let session = self
                .session(&request, CopilotSessionReuse::Compatible)
                .await?;
            let control_router = self.control_router(&request.runtime_key()?).await;
            let control_context = request
                .control_context
                .clone()
                .unwrap_or_else(|| ControlTurnContext::inactive(request.mode));
            let control_stream = control_router.activate(control_context)?;
            let host = host_turn::HostTurn::new(
                control_router,
                control_stream,
                self.permission_context(&request.runtime_key()?).await,
                self.sessions.clone(),
                request.runtime_key()?,
                session.clone(),
                request.mode == PromptMode::ExecutePlan,
            );
            let repair = self.sessions.repair(&request.runtime_key()?).await;
            let completed = forge_providers::copilot::prompt::run(
                &session,
                &turn_event,
                &turn_event_guard,
                forge_providers::copilot::prompt::TurnOptions {
                    correlation_id: request.harness_session_id.clone(),
                    workspace: request.workspace.clone(),
                    model: (request.model != "default").then(|| request.model.clone()),
                    capability: capability(),
                    input: (&request.input).into(),
                    recovered: request.recovered_input.iter().map(Into::into).collect(),
                    instructions: request.interaction_contract(),
                    repair_caution: repair.map(|repair| repair.caution().to_owned()),
                    attachments: crate::provider::copilot::attachments::input(
                        "",
                        &request.input_attachments(),
                    )?,
                    receipt: request.submission.as_ref().map(|receipt| {
                        receipt.clone() as Arc<dyn forge_provider_api::submission::InputReceipt>
                    }),
                    background_compaction_observed: self
                        .sessions
                        .observes_compaction(&request.runtime_key()?)
                        .await,
                },
                event_sink
                    .as_ref()
                    .expect("active Copilot invocation has an event sink"),
                &host,
                self.trace.clone(),
            )
            .await?;
            if completed.answered_rejection {
                self.permission_coordinator
                    .take_timeout_feedback(&request.owner()?)?;
            }
            let mut output = host.take_output()?;
            output.merge_native(completed.completion);
            Ok(output)
        }
        .await;
        output
    }

    async fn compact(&self, request: super::ProviderCompactionRequest) -> Result<ProviderOutput> {
        let id = request.id;
        let request = request.request;
        let scope = super::EventScope::SessionOperation {
            owner: request.owner()?,
            operation_id: id.clone(),
        };
        let session = self
            .session(&request, CopilotSessionReuse::Compatible)
            .await?;
        let compaction = self.sessions.compact(&request.runtime_key()?, id).await?;
        let context_usage = compaction
            .result
            .context_window
            .as_ref()
            .and_then(|context| {
                context
                    .current_tokens
                    .zip(context.token_limit)
                    .and_then(|(used, size)| crate::session::ContextUsage::reported(used, size))
            });
        Ok(ProviderOutput {
            event: vec![ProviderEvent::Compaction {
                scope,
                update: compaction.into(),
            }],
            completion: crate::provider::ProviderCompletion {
                native: forge_provider_api::completion::ProviderCompletion {
                    provider_session_id: Some(session.id().to_string()),
                    capability: capability(),
                    runtime: crate::provider::ProviderRuntime {
                        provider: "Copilot CLI".into(),
                        model: None,
                    },
                    metrics: crate::provider::TurnMetrics {
                        context_usage,
                        ..Default::default()
                    },
                    ..Default::default()
                },
                ..Default::default()
            },
        })
    }

    /// Subscribe to owner-scoped session observations independently of foreground turns.
    async fn session_event_stream(
        &self,
        owner: super::ExecutionOwner,
    ) -> Result<Option<super::events::ProviderEventStream>> {
        let runtime_key = owner.runtime_key();
        let scope = super::EventScope::SessionOperation {
            owner,
            operation_id: uuid::Uuid::new_v4().to_string(),
        };
        let (sink, stream) = super::events::channel(scope);
        self.sessions.subscribe(runtime_key, sink).await;
        Ok(Some(stream))
    }

    async fn fork(
        &self,
        request: ProviderForkRequest,
    ) -> Result<crate::provider::ProviderForkResult> {
        let started = std::time::Instant::now();
        let mut target_request = request.source;
        let client = self.client(&target_request.workspace).await?;
        let provider_session_id = match (
            target_request.provider_session_id.clone(),
            request.checkpoint_id,
        ) {
            (Some(source_session_id), to_event_id) => {
                client.fork(source_session_id, to_event_id).await?
            }
            _ => {
                target_request
                    .harness_session_id
                    .clone_from(&request.target_harness_session_id);
                self.session(&target_request, CopilotSessionReuse::Compatible)
                    .await?
                    .id()
                    .to_string()
            }
        };
        target_request.harness_session_id = request.target_harness_session_id;
        target_request.provider_session_id = Some(provider_session_id.clone());
        self.session(&target_request, CopilotSessionReuse::Compatible)
            .await?;
        Ok(crate::provider::ProviderForkResult {
            provider_session_id,
            timing: vec![crate::provider::ProviderTimingRecord {
                phase: "copilot.session_fork".into(),
                duration_ms: started.elapsed().as_secs_f64() * 1000.0,
            }],
        })
    }

    async fn model_list(&self, request: ProviderRequest) -> Result<Vec<ProviderModel>> {
        Ok(self.client(&request.workspace).await?.models().await?)
    }

    async fn skill_list(&self, request: ProviderCatalogRequest) -> Result<Vec<SkillDefinition>> {
        if let Some(root) = &self.settings_root {
            return Ok(crate::settings::SessionResources::load(
                root,
                &request,
                "copilot",
                "copilotcli",
            )?
            .skill);
        }
        let provider_request = Self::catalog_provider_request(&request);
        let session = self
            .session(&provider_request, CopilotSessionReuse::Compatible)
            .await?;
        Ok(session.skills().await?)
    }

    async fn set_skill_enabled(
        &self,
        request: ProviderCatalogRequest,
        name: &str,
        enabled: bool,
    ) -> Result<CatalogMutation> {
        let client = self.client(&request.workspace).await?;
        client.set_skill_preference(name, enabled).await?;
        let session = self
            .session(
                &Self::catalog_provider_request(&request),
                CopilotSessionReuse::Compatible,
            )
            .await?;
        Ok(session.set_skill_enabled(name, enabled).await?)
    }

    async fn initialize_session(&self, request: ProviderRequest) -> Result<Option<String>> {
        let session = self
            .session(&request, CopilotSessionReuse::Compatible)
            .await?;
        Ok(Some(session.id().to_string()))
    }

    async fn mcp_configuration(
        &self,
        request: ProviderCatalogRequest,
    ) -> Result<Vec<McpDefinition>> {
        if let Some(root) = &self.settings_root {
            return Ok(crate::settings::SessionResources::load(
                root,
                &request,
                "copilot",
                "copilotcli",
            )?
            .mcp);
        }
        Ok(self
            .client(&request.workspace)
            .await?
            .discover_mcp(&request.workspace)
            .await?)
    }

    async fn mcp_list(&self, request: ProviderCatalogRequest) -> Result<Vec<McpDefinition>> {
        let provider_request = Self::catalog_provider_request(&request);
        let session = self
            .session(&provider_request, CopilotSessionReuse::Compatible)
            .await?;
        let mcp_failure = self.sessions.mcp_failures(&request.runtime_key()?).await;
        let definition = self.mcp_configuration(request).await?;
        Ok(session.mcp_list(definition, &mcp_failure).await?)
    }

    async fn set_mcp_enabled(
        &self,
        request: ProviderCatalogRequest,
        name: &str,
        enabled: bool,
    ) -> Result<CatalogMutation> {
        self.client(&request.workspace)
            .await?
            .set_mcp_preference(name, enabled)
            .await?;
        let session = self
            .session(
                &Self::catalog_provider_request(&request),
                CopilotSessionReuse::Compatible,
            )
            .await?;
        Ok(session.set_mcp_enabled(name, enabled).await?)
    }

    async fn has_active_turn(&self, session_id: &str) -> bool {
        self.turn_event(session_id).await.is_active()
    }

    async fn prepare_turn(&self, session_id: &str) -> Result<()> {
        self.turn_event(session_id).await.reset()
    }

    async fn steer_session(&self, session_id: &str, input: super::SteeringInput) -> Result<()> {
        let text = input.text().to_owned();
        let message = crate::provider::copilot::attachments::input(&text, input.attachments())?;
        self.trace
            .record(session_id, "copilot.session.steer", json!({"text":text}));
        let session = self
            .active_session(session_id)
            .await
            .context("Copilot session has not started")?;
        self.turn_event(session_id)
            .await
            .steer(async {
                let receipt = input.submission().map(|receipt| {
                    receipt as Arc<dyn forge_provider_api::submission::InputReceipt>
                });
                Ok(session.steer(message, receipt).await?)
            })
            .await?;
        if !matches!(input, super::SteeringInput::AgentResult(_)) {
            let input_message_id = input
                .submission()
                .map(|submission| submission.record())
                .transpose()?
                .and_then(|record| record.input_message_id);
            self.turn_event(session_id)
                .await
                .publish_steering(text, input_message_id, input.task_start().cloned())
                .await?;
        }
        Ok(())
    }

    async fn cleanup_execution(&self, session_id: &str) -> Result<()> {
        let controls = self.control_router(session_id).await.cancel()?;
        let turn_event = self.turn_event(session_id).await;
        let stop = turn_event.request_stop()?;
        let result = tokio::time::timeout(std::time::Duration::from_secs(8), async {
            if stop.submitted {
                let session = self
                    .active_session(session_id)
                    .await
                    .context("Submitted Copilot session is missing")?;
                session.abort().await.context("abort active Copilot turn")?;
            }
            if turn_event.stop().await? {
                if stop.sending || !stop.submitted {
                    let session = self
                        .active_session(session_id)
                        .await
                        .context("Submitted Copilot session is missing")?;
                    session
                        .abort()
                        .await
                        .context("abort committed Copilot send after collection")?;
                }
                turn_event.wait_finished().await;
            }
            Ok::<_, anyhow::Error>(())
        })
        .await
        .context("Copilot execution did not stop within 8 seconds")
        .and_then(|result| result);
        if let Some(controls) = controls {
            controls.close().await;
        }
        result
    }

    async fn rollback_cancelled_turn_for(&self, session_id: &str) -> Result<bool> {
        let turn_event = self.turn_event(session_id).await;
        let target = turn_event.retraction_target()?;
        if matches!(target, CopilotRetraction::Unsent) {
            return Ok(true);
        }
        let session = self
            .active_session(session_id)
            .await
            .context("Copilot session is missing for rewind")?;
        session.rewind(target).await?;
        turn_event.finish_retraction()?;
        Ok(true)
    }

    async fn active_session_id_for(&self, session_id: &str) -> Option<String> {
        self.active_session(session_id)
            .await
            .map(|session| session.id().to_string())
    }
}

fn selected_reasoning_effort(
    model_list: &[ProviderModel],
    request: &ProviderRequest,
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

/// Finish the main agent independently from attached background work.

#[cfg(test)]
mod test {
    #[test]
    fn only_root_session_idle_finishes_the_provider_invocation() {
        let mut event: github_copilot_sdk::SessionEvent =
            serde_json::from_value(serde_json::json!({
                "id":"idle", "type":"assistant.idle", "timestamp":"2026-09-18T00:00:00Z", "data":{}
            }))
            .unwrap();
        assert!(!super::completes_exchange(&event));
        event.agent_id = Some("child".into());
        assert!(!super::completes_exchange(&event));
        event.agent_id = None;
        event.event_type = "assistant.turn_end".into();
        assert!(!super::completes_exchange(&event));
        event.event_type = "session.idle".into();
        assert!(super::completes_exchange(&event));
        event.agent_id = Some("child".into());
        assert!(!super::completes_exchange(&event));
    }

    use super::*;

    #[test]
    fn mcp_launch_preserves_empty_arguments_and_explicit_tool_selection() {
        let payload = json!({"type":"stdio","command":"docs-mcp","args":[]});
        let request = serde_json::to_value(mcp_launch_request("docs", &payload)).unwrap();
        assert_eq!(request["serverName"], "docs");
        assert_eq!(request["config"]["args"], json!([]));
        assert_eq!(request["config"]["tools"], json!(["*"]));
        assert!(payload.get("tools").is_none());

        let payload = json!({"type":"http","url":"https://example.invalid/mcp","tools":[]});
        let request = serde_json::to_value(mcp_launch_request("remote", &payload)).unwrap();
        assert_eq!(request["config"], payload);
    }

    #[test]
    fn descriptor_exposes_supported_native_capabilities() {
        let provider = CopilotProvider::new(Vec::new()).unwrap();
        let descriptor = provider.descriptor();
        assert_eq!(descriptor.kind, ProviderKind::Copilot);
        assert!(descriptor.capability.native_steer);
        assert!(descriptor.capability.native_fork);
        assert!(descriptor.capability.native_compact);
        assert!(descriptor.capability.native_turn_rollback);
        assert!(descriptor.capability.agent.observe);
    }

    #[test]
    fn omits_reasoning_effort_for_auto_and_unsupported_models() {
        let mut request = ProviderRequest {
            execution_owner: None,
            harness_session_id: "harness-session".into(),
            workspace: "D:/repo".into(),
            input: ProviderInput::from_text("test"),
            mode: crate::provider::PromptMode::Chat,
            model: "default".into(),
            effort: "low".into(),
            context_window: None,
            service_tier: crate::provider::ServiceTier::Standard,
            access: Default::default(),
            approval_mode: ApprovalMode::Ask,
            provider_session_id: None,
            submission: None,
            recovered_input: Vec::new(),
            control_context: None,
        };
        let model_list = vec![ProviderModel {
            service_tiers: vec![crate::provider::ServiceTier::Standard],
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
                "limits": {
                    "max_context_window_tokens": 272000,
                    "max_output_tokens": 128000
                },
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
        assert_eq!(descriptor.context_window[0].id, "default");
        assert_eq!(descriptor.context_window[0].token_limit, Some(272_000));
        assert_eq!(descriptor.context_window[1].id, "long_context");
        assert_eq!(descriptor.context_window[1].token_limit, None);
        assert!(descriptor.vision);
    }

    #[test]
    fn maps_explicit_context_without_billing_metadata() {
        let model: Model = serde_json::from_value(serde_json::json!({
            "id": "gpt-test",
            "name": "Test model",
            "capabilities": {
                "limits": { "max_context_window_tokens": 272000 }
            }
        }))
        .unwrap();

        let descriptor = model_descriptor(model);

        assert_eq!(descriptor.context_window.len(), 1);
        assert_eq!(descriptor.context_window[0].token_limit, Some(272_000));
    }

    #[test]
    fn does_not_infer_missing_context_limits_from_billing_or_output() {
        for context_limit in [
            serde_json::Value::Null,
            serde_json::json!(0),
            serde_json::json!(-1),
        ] {
            let model: Model = serde_json::from_value(serde_json::json!({
                "id": "gpt-test",
                "name": "Test model",
                "capabilities": {
                    "limits": {
                        "max_context_window_tokens": context_limit,
                        "max_output_tokens": 128000
                    }
                },
                "billing": {
                    "tokenPrices": {
                        "maxPromptTokens": 272000,
                        "longContext": { "maxPromptTokens": 1000000 }
                    }
                }
            }))
            .unwrap();

            let descriptor = model_descriptor(model);

            assert_eq!(descriptor.context_window.len(), 2);
            assert!(
                descriptor
                    .context_window
                    .iter()
                    .all(|context| context.token_limit.is_none())
            );
            assert_eq!(
                descriptor.default_context_window.as_deref(),
                Some("default")
            );
        }
    }

    #[test]
    fn applies_reasoning_and_context_to_a_native_model_switch() {
        let options = model_options(Some("high".into()), Some("long_context"));

        assert_eq!(options.reasoning_effort.as_deref(), Some("high"));
        assert_eq!(options.reasoning_summary, Some(ReasoningSummary::Concise));
        assert!(matches!(
            options.context_tier,
            Some(ContextTier::LongContext)
        ));
        for context in [None, Some("unknown")] {
            assert_eq!(
                model_options(None, context).reasoning_summary,
                Some(ReasoningSummary::Concise)
            );
        }
    }

    #[tokio::test]
    async fn publishes_steering_only_while_a_copilot_turn_is_active() {
        let turn_event = CopilotTurnEvent::default();
        let (event_sink, mut event_stream) =
            crate::provider::events::channel(crate::provider::InvocationScope::new(
                crate::provider::ExecutionOwner::root("session"),
                "exchange",
            ));
        let turn_guard = turn_event.activate(Some(event_sink), None).unwrap();

        turn_event
            .publish_steering("accepted".into(), Some("message-identity".into()), None)
            .await
            .unwrap();
        let event = event_stream.try_recv().unwrap();
        assert!(matches!(event, ProviderEvent::InputAccepted {
            acknowledgement: crate::provider::InputAcknowledgement::Steering { input_id: Some(id), text }, ..
        } if id == "message-identity" && text == "accepted"));

        drop(turn_guard);
        turn_event
            .publish_steering("late".into(), None, None)
            .await
            .unwrap();
        assert!(event_stream.try_recv().is_err());
    }
}
