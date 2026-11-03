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
#[cfg(test)]
mod file_change_test;
mod host_turn;

mod mcp;
pub(crate) mod mock_stream;
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

enum FailedClientRecovery {
    Idle,
    Running {
        failed: forge_providers::copilot::client_generation::FailedGeneration,
        task: tokio::task::JoinHandle<std::result::Result<(), Arc<ProviderError>>>,
    },
}

/// Owns the native Copilot SDK client, session, event decoder, and Harness callbacks.
pub struct CopilotProvider {
    session_events: Arc<crate::provider::session_events::SessionEvents>,
    runtime: Arc<CopilotRuntime>,
    settings_root: Option<PathBuf>,
    trace: Arc<crate::trace::TraceStore>,
    sessions: Arc<CopilotSessions<crate::settings::ResourceProfile>>,
    control_router_by_harness: Arc<Mutex<HashMap<String, Arc<ControlToolRouter>>>>,
    permission_context_by_harness: Arc<Mutex<HashMap<String, Arc<ProviderPermissionContext>>>>,
    permission_coordinator: Arc<PermissionCoordinator>,
    recovery: Mutex<FailedClientRecovery>,
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
            session_events: Default::default(),
            settings_root,
            sessions: Arc::new(CopilotSessions::new(trace.clone())),
            trace,
            runtime: Arc::new(CopilotRuntime::new(command)),

            control_router_by_harness: Default::default(),
            permission_context_by_harness: Default::default(),
            permission_coordinator,
            recovery: Mutex::new(FailedClientRecovery::Idle),
        })
    }

    /// Return how many Copilot SDK client process generations this provider launched.
    pub fn client_start_count(&self) -> u64 {
        self.runtime.start_count()
    }

    async fn client(&self, workspace: &str) -> Result<Client> {
        Ok(self.runtime.client(workspace).await?)
    }

    /// Retain shared-client retirement independently of the reset caller's lifetime.
    async fn recover_failed_client(
        &self,
        recovery: &mut FailedClientRecovery,
        failed: forge_providers::copilot::client_generation::FailedGeneration,
    ) -> Result<()> {
        let runtime = self.runtime.clone();
        let sessions = self.sessions.clone();
        let events = self.session_events.clone();
        let permissions = self.permission_context_by_harness.clone();
        let routers = self.control_router_by_harness.clone();
        let coordinator = self.permission_coordinator.clone();
        let collecting = failed.clone();
        *recovery = FailedClientRecovery::Running {
            failed,
            task: tokio::spawn(async move {
                let collect = async {
                    let owners = sessions.fail_generation(&collecting).await?;
                    let mut controls = Vec::new();
                    let mut fencing = None;
                    for owner in &owners {
                        let key = owner.runtime_key();
                        let permission = permissions.lock().await.get(&key).cloned();
                        if let Some(permission) = permission
                            && let Err(error) = permission.retire()
                        {
                            fencing.get_or_insert_with(|| ProviderError::from(error));
                        }
                        if let Err(error) = coordinator.cancel_owner(owner, None).await {
                            fencing.get_or_insert_with(|| ProviderError::from(error));
                        }
                        let router = routers.lock().await.get(&key).cloned();
                        if let Some(router) = router {
                            if let Err(error) = router.cancel() {
                                fencing.get_or_insert_with(|| ProviderError::from(error));
                            }
                            controls.push(router);
                        }
                    }
                    if let Some(error) = fencing {
                        return Err(error);
                    }
                    let client = runtime.collect_failed_client(&collecting).await?;
                    // Foreground decoders release their locks before native intake collection.
                    for owner in &owners {
                        events.cancel(owner).await?;
                    }
                    for router in controls {
                        router.close().await.map_err(ProviderError::from)?;
                    }
                    let collected = sessions.collect_failed_generation(&client).await?;
                    for owner in &owners {
                        let key = owner.runtime_key();
                        events.reset_owner(owner).await?;
                        routers.lock().await.remove(&key);
                        permissions.lock().await.remove(&key);
                    }
                    runtime.finish_recovery(collected).await
                };
                collect.await.map_err(Arc::new)
            }),
        };
        Self::join_failed_client_recovery(recovery).await
    }

    /// Join the retained recovery worker without detaching it on timeout or waiter cancellation.
    async fn join_failed_client_recovery(recovery: &mut FailedClientRecovery) -> Result<()> {
        let (failed, result) = match recovery {
            FailedClientRecovery::Idle => return Ok(()),
            FailedClientRecovery::Running { failed, task } => {
                let result = tokio::time::timeout(std::time::Duration::from_secs(60), task)
                    .await
                    .context("Copilot shared-client recovery exceeded 60 seconds")?;
                (failed.clone(), result)
            }
        };
        *recovery = FailedClientRecovery::Idle;
        let result = result.map_err(|error| {
            ProviderError::Operation(forge_provider_api::error::ProviderOperationFailure::new(
                forge_provider_api::error::ProviderOperation::CollectNativeRuntime,
                Arc::new(error),
            ))
        })?;
        result.map_err(|cleanup| {
            ProviderError::PreparationCleanup {
                cause: Arc::new(ProviderError::Observation(failed.failure())),
                cleanup,
            }
            .into()
        })
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
            execution_owner: request.owner()?,
            runtime_key: key.clone(),
            correlation_id: request.harness_session_id.clone(),
            session_id: request.provider_session_id.clone().map(Into::into),
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
                            session_id: request.provider_session_id.clone().map(Into::into),
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
                                    selected_mcp: resources
                                        .as_ref()
                                        .map(|resources| {
                                            resources
                                                .launch
                                                .iter()
                                                .map(|(name, _)| name.clone())
                                                .collect()
                                        })
                                        .unwrap_or_default(),
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
        request.provider_session_id = Some(session_id.to_owned().try_into()?);
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
    ) -> std::result::Result<(), crate::provider::ProviderError> {
        let result: anyhow::Result<()> = async move {
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
        .await;
        result.map_err(|source| {
            crate::provider::lifecycle_error::classify(
                forge_provider_api::error::ProviderOperation::ReconcileSettings,
                source,
            )
        })
    }

    async fn generate_text(
        &self,
        request: ProviderCatalogRequest,
        purpose: super::TextGeneration,
        model: &str,
        reasoning_effort: Option<&str>,
        history: &str,
    ) -> std::result::Result<String, crate::provider::ProviderError> {
        let result: anyhow::Result<String> = async move {
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

        }.await;
        result.map_err(|source| {
            crate::provider::lifecycle_error::classify(
                forge_provider_api::error::ProviderOperation::GenerateText,
                source,
            )
        })
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

    async fn shutdown(&self) -> std::result::Result<(), crate::provider::ProviderError> {
        let result: anyhow::Result<()> = async move {
            self.runtime.close_admission();
            let mut recovery = self.recovery.lock().await;
            Self::join_failed_client_recovery(&mut recovery).await?;
            if let Some(failed) = self.runtime.failed_generation().await {
                self.recover_failed_client(&mut recovery, failed).await?;
            }
            let mut failures = Vec::new();
            if let Err(error) = self.session_events.shutdown().await {
                failures.push(error);
            }
            if let Err(error) = self.sessions.close_observers().await {
                failures.push(error);
            }
            let client_stopped = match self.runtime.shutdown().await {
                Ok(()) => forge_providers::copilot::sessions::ClientShutdown::Collected,
                Err(error) => {
                    failures.push(error);
                    forge_providers::copilot::sessions::ClientShutdown::Uncertain
                }
            };
            if let Err(error) = self.sessions.clear(client_stopped).await {
                failures.push(error);
            }
            self.control_router_by_harness.lock().await.clear();
            self.permission_context_by_harness.lock().await.clear();
            crate::provider::lifecycle_error::combine(failures).map_err(anyhow::Error::from)
        }
        .await;
        result.map_err(|source| {
            crate::provider::lifecycle_error::classify(
                forge_provider_api::error::ProviderOperation::Shutdown,
                source,
            )
        })
    }

    fn descriptor(&self) -> ProviderDescriptor {
        ProviderDescriptor {
            kind: ProviderKind::Copilot,
            label: "Copilot CLI".into(),
            capability: capability(),
        }
    }

    async fn session_events(
        &self,
        owner: crate::provider::ExecutionOwner,
    ) -> Result<crate::provider::events::ProviderEventStream, crate::provider::ProviderError> {
        let stream = self.session_events.subscribe(owner.clone())?;
        self.sessions
            .subscribe(
                owner.runtime_key(),
                self.session_events.session_sink(&owner)?,
            )
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
                    provider_session_id: Some(session.id().to_string().try_into()?),
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
                client.fork(source_session_id.into(), to_event_id).await?
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
        target_request.provider_session_id = Some(provider_session_id.clone().try_into()?);
        self.session(&target_request, CopilotSessionReuse::Compatible)
            .await?;
        Ok(crate::provider::ProviderForkResult {
            provider_session_id: provider_session_id.try_into()?,
            timing: vec![crate::provider::ProviderTimingRecord {
                phase: "copilot.session_fork".into(),
                duration_ms: started.elapsed().as_secs_f64() * 1000.0,
            }],
        })
    }

    async fn model_list(&self, request: ProviderRequest) -> Result<Vec<ProviderModel>> {
        let client = || self.client(&request.workspace);
        self.trace.record(
            &request.harness_session_id,
            "future.stack_probe",
            json!({
                "phase":"copilot.model.client", "bytes":crate::trace::future_bytes(&client),
            }),
        );
        self.trace.record(&request.harness_session_id, "future.stack_probe", json!({
            "phase":"copilot.sdk.start", "bytes":forge_providers::copilot::runtime::CopilotRuntime::sdk_start_future_bytes(),
        }));
        let client = client().await?;
        self.trace.record(
            &request.harness_session_id,
            "future.stack_probe",
            json!({"phase":"copilot.model.client.ready","bytes":0}),
        );
        let models = || client.models();
        self.trace.record(
            &request.harness_session_id,
            "future.stack_probe",
            json!({"phase":"copilot.model.list","bytes":crate::trace::future_bytes(&models)}),
        );
        Ok(models().await?)
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

    async fn initialize_session(
        &self,
        request: ProviderRequest,
    ) -> std::result::Result<
        Option<forge_provider_api::ProviderSessionId>,
        crate::provider::ProviderError,
    > {
        let result: anyhow::Result<Option<forge_provider_api::ProviderSessionId>> = async move {
        let start = || self.session(&request, CopilotSessionReuse::Compatible);
        self.trace.record(&request.harness_session_id, "future.stack_probe", json!({"phase":"copilot.initialize.session","bytes":crate::trace::future_bytes(&start)}));
        self.trace.record(&request.harness_session_id, "future.stack_probe", json!({"phase":"copilot.sdk.start","bytes":forge_providers::copilot::runtime::CopilotRuntime::sdk_start_future_bytes()}));
        let session = start().await?;
        self.trace.record(
            &request.harness_session_id,
            "future.stack_probe",
            json!({"phase":"copilot.initialize.ready","bytes":0}),
        );
        Ok(Some(session.id().to_string().try_into()?))

        }.await;
        result.map_err(|source| {
            crate::provider::lifecycle_error::classify(
                forge_provider_api::error::ProviderOperation::InitializeSession,
                source,
            )
        })
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

    async fn cleanup_execution(
        &self,
        session_id: &str,
    ) -> std::result::Result<(), crate::provider::ProviderError> {
        let result: anyhow::Result<()> = async move {
            // Observation loss already fenced this native client. Collect its local
            // ownership without requiring an abort response from the dead transport.
            // The engine retains the original unknown execution outcome separately.
            if let Some(requested) = self.runtime.failed_generation().await {
                let mut recovery = self.recovery.lock().await;
                Self::join_failed_client_recovery(&mut recovery).await?;
                if let Some(current) = self.runtime.failed_generation().await
                    && requested.same(&current)
                {
                    return self.recover_failed_client(&mut recovery, current).await;
                }
                return Ok(());
            }
            let controls = self.control_router(session_id).await.cancel()?;
            let turn_event = self.turn_event(session_id).await;
            let stop = turn_event.request_stop()?;
            let stopped = (stop.submitted || stop.sending)
                && self
                    .active_session(session_id)
                    .await
                    .is_some_and(|session| session.client_stopped());
            if stopped || (!stop.submitted && !stop.sending) {
                self.session_events.cancel_runtime(session_id).await?;
            }
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
        .await;
        result.map_err(|source| {
            crate::provider::lifecycle_error::classify(
                forge_provider_api::error::ProviderOperation::CleanupExecution,
                source,
            )
        })
    }

    async fn fence_session(
        &self,
        owner: &crate::provider::ExecutionOwner,
    ) -> std::result::Result<(), crate::provider::ProviderError> {
        let result: anyhow::Result<()> = async move {
            let key = owner.runtime_key();
            if let Some(permission) = self.permission_context_by_harness.lock().await.get(&key) {
                permission.retire()?;
            }
            self.permission_coordinator
                .cancel_owner(owner, None)
                .await?;
            if let Some(router) = self.control_router_by_harness.lock().await.get(&key) {
                router.cancel()?;
            }
            Ok(())
        }
        .await;
        result.map_err(|source| {
            crate::provider::lifecycle_error::classify(
                forge_provider_api::error::ProviderOperation::FenceSession,
                source,
            )
        })
    }

    async fn reset_session(
        &self,
        owner: &crate::provider::ExecutionOwner,
    ) -> std::result::Result<(), crate::provider::ProviderError> {
        let result: anyhow::Result<()> = async move {
            // Capture failure before waiting: an old queued reset cannot retire a successor.
            let requested = self.runtime.failed_generation().await;
            let mut recovery = self.recovery.lock().await;
            Self::join_failed_client_recovery(&mut recovery).await?;
            let current = self.runtime.failed_generation().await;
            if let Some(requested) = requested {
                match current {
                    Some(current) if requested.same(&current) => {
                        return self.recover_failed_client(&mut recovery, current).await;
                    }
                    Some(_) | None => return Ok(()),
                }
            }
            if let Some(current) = current {
                return self.recover_failed_client(&mut recovery, current).await;
            }
            let key = owner.runtime_key();
            self.fence_session(owner).await?;
            self.sessions.retire(&key).await?;
            self.control_router_by_harness.lock().await.remove(&key);
            self.permission_context_by_harness.lock().await.remove(&key);
            self.session_events.reset_owner(owner).await?;
            Ok(())
        }
        .await;
        result.map_err(|source| {
            crate::provider::lifecycle_error::classify(
                forge_provider_api::error::ProviderOperation::ResetSession,
                source,
            )
        })
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

    async fn active_session_id_for(
        &self,
        session_id: &str,
    ) -> Result<Option<forge_provider_api::ProviderSessionId>> {
        self.active_session(session_id)
            .await
            .map(|session| session.id().to_string().try_into().map_err(Into::into))
            .transpose()
    }
}

impl CopilotProvider {
    async fn steer_native(&self, session_id: &str, input: super::SteeringInput) -> Result<()> {
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
            .steer(
                input
                    .submission()
                    .map(|receipt| {
                        forge_provider_api::lifecycle::SubmissionId::try_from(
                            receipt.id().to_owned(),
                        )
                    })
                    .transpose()?,
                async {
                    let receipt = input.submission().map(|receipt| {
                        receipt as Arc<dyn forge_provider_api::submission::InputReceipt>
                    });
                    Ok(session.steer(message, receipt).await?)
                },
            )
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
}

impl CopilotProvider {
    /// Drive admitted native work inside the session-owned task.
    async fn run_native(
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
                .await
                .map_err(|error| {
                    let cause = ProviderError::from(error);
                    self.trace.record(
                        &request.harness_session_id,
                        "copilot.preparation.failed",
                        json!({"phase":"session", "error":cause.to_string()}),
                    );
                    match cause {
                        error @ ProviderError::PreparationCleanup { .. } => error,
                        error => ProviderError::PreparationFailed(Arc::new(error)),
                    }
                })?;
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
            .await;
            if let (Some(context), Some(sink)) =
                (request.control_context.as_ref(), event_sink.as_ref())
            {
                let certification = (|| -> anyhow::Result<_> {
                    let (_, event_id) = turn_event
                        .foreground_anchor()?
                        .context("Copilot terminal lacks an admitted foreground anchor")?;
                    Ok((event_id, turn_event.certified_executions()?))
                })();
                let (event_id, executions) = match certification {
                    Ok(certification) => certification,
                    Err(error) => {
                        // Preserve an established native failure; successful execution
                        // requires explicit terminal coverage rather than a skipped certificate.
                        completed?;
                        return Err(ProviderError::Observation(
                            forge_provider_api::lifecycle::ObservationFailure::Protocol {
                                message: format!(
                                    "cannot certify Copilot execution completion: {error:#}"
                                ),
                            },
                        ));
                    }
                };
                let scope = sink.invocation_scope()?.clone();
                let thread_id = session.id().to_string();
                let initial_is_first = executions
                    .first()
                    .is_some_and(|(anchor, _, _)| anchor == &event_id);
                let mut initial = None;
                let mut continuations = Vec::new();
                for (native_event, covered, result) in executions {
                    if native_event == event_id {
                        initial = Some((covered, result));
                    } else {
                        continuations.push(crate::provider::NativeExecutionCertificate {
                            anchor: crate::provider::TurnAddress {
                                scope: scope.clone(),
                                thread_id: thread_id.clone(),
                                turn_id: native_event.into(),
                            },
                            covered,
                            result,
                            completed_at_ms: crate::provider::retry::now_ms(),
                        });
                    }
                }
                if let Some((covered, initial_result)) = initial.filter(|_| initial_is_first) {
                    let anchor = crate::provider::TurnAddress {
                        scope,
                        thread_id,
                        turn_id: event_id.into(),
                    };
                    context.provider_completion.certify(
                        crate::provider::ExecutionCertificate::Foreground {
                            anchor,
                            covered,
                            initial_result: Some(Box::new(initial_result)),
                            continuations,
                        },
                    )?;
                } else {
                    completed?;
                    return Err(ProviderError::Observation(
                        forge_provider_api::lifecycle::ObservationFailure::Protocol {
                            message:
                                "Copilot terminal coverage lacks its initial foreground execution"
                                    .into(),
                        },
                    ));
                }
            }
            let completed = completed?;
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

#[cfg(test)]
mod test {
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

    #[tokio::test]
    async fn publishes_steering_through_retained_copilot_context() {
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
        assert!(matches!(
            event_stream.try_recv().unwrap(),
            ProviderEvent::InputAccepted { .. }
        ));
    }
}
