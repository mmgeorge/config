use anyhow::{Context, Result};
use forge_provider_api::catalog::{McpDefinition, McpStatus, McpToolDefinition, SkillDefinition};
use forge_provider_api::input::MessageInput;
use forge_provider_api::submission::InputReceipt;
use forge_provider_api::{Compaction, CompactionState, ProviderError, ProviderModel};
use github_copilot_sdk::rpc::*;
use github_copilot_sdk::session_events::ReasoningSummary;
use github_copilot_sdk::types::ContextTier;
use github_copilot_sdk::{DeliveryMode, SessionId, SetModelOptions};
use serde_json::Value;
use sha2::{Digest, Sha256};
use std::collections::{HashMap, HashSet};
use std::path::Path;
use std::sync::Arc;

#[derive(Clone)]
pub struct CopilotClient {
    pub(crate) inner: github_copilot_sdk::Client,
    pub(crate) generation: super::client_generation::ClientGeneration,
}

pub struct CopilotSession {
    pub(crate) inner: Arc<github_copilot_sdk::session::Session>,
    pub(crate) events: super::intake::SessionIntake,
    pub(crate) generation: super::client_generation::ClientGeneration,
    /// Set while selected MCP servers are still starting, so accepted input must release the load.
    pub(crate) mcp_loading: std::sync::atomic::AtomicBool,
}

pub struct SkillSelection {
    pub name: String,
    pub enabled: bool,
}

#[derive(Debug, Eq, PartialEq)]
pub(crate) enum SubmissionReconciliation {
    Found(String),
    NotFound,
    Ambiguous,
    ContentMismatch,
}

impl CopilotClient {
    /// Bind a native transport to its single shared health generation.
    pub(crate) fn new(inner: github_copilot_sdk::Client) -> Self {
        Self {
            generation: super::client_generation::ClientGeneration::new(inner.clone()),
            inner,
        }
    }

    /// Fence an unexposed transport rejected during runtime initialization.
    pub(crate) fn force_stop(&self) {
        self.inner.force_stop();
    }

    /// Collect the SDK runtime before marking its shared generation unavailable.
    pub(crate) async fn stop(&self) -> Result<(), github_copilot_sdk::StopErrors> {
        self.inner.stop().await?;
        self.generation.mark_stopped();
        Ok(())
    }

    /// Inject fixture transport loss into every session using this client.
    #[cfg(test)]
    pub(crate) fn fail_generation(
        &self,
        failure: forge_provider_api::lifecycle::ObservationFailure,
    ) {
        self.generation.fail(failure);
    }

    /// Reject session reuse after any reader has lost this shared transport.
    pub(crate) fn ensure_healthy(&self) -> Result<(), ProviderError> {
        self.generation.ensure_healthy()
    }

    pub async fn models(&self) -> Result<Vec<ProviderModel>, ProviderError> {
        let result: Result<Vec<ProviderModel>> = async {
            let models = self
                .inner
                .list_models()
                .await
                .context("list Copilot models")?;
            let mut models: Vec<_> = models
                .into_iter()
                .map(super::catalog::model_descriptor)
                .collect();
            super::catalog::order_models(&mut models);
            Ok(models)
        }
        .await;
        result.map_err(|error| {
            tracing::error!(target: "forge::provider", event_name = "provider.catalog.failed",
                provider = "copilot", operation = "model_list", error = %format_args!("{error:#}"));
            native_failure(
                forge_provider_api::error::ProviderOperation::TurnStart,
                forge_provider_api::error::ProviderFailureKind::Transport,
            )
        })
    }
    pub async fn discover_mcp(&self, workspace: &str) -> Result<Vec<McpDefinition>, ProviderError> {
        let result: Result<Vec<McpDefinition>> = async {
            let discovery = self
                .inner
                .rpc()
                .mcp()
                .discover(McpDiscoverRequest {
                    include_effective_source: Some(true),
                    working_directory: Some(workspace.to_owned()),
                })
                .await
                .context("discover configured Copilot MCP servers")?;
            let mut servers = discovery
                .servers
                .into_iter()
                .map(|server| McpDefinition {
                    scope: None,
                    source_path: None,
                    resource_id: None,
                    name: server.name,
                    enabled: server.enabled,
                    transport: server
                        .r#type
                        .and_then(|transport| serde_json::to_value(transport).ok())
                        .and_then(|transport| transport.as_str().map(str::to_owned))
                        .map(|transport| match transport.as_str() {
                            "stdio" => forge_provider_api::catalog::McpTransportKind::Stdio,
                            "http" => forge_provider_api::catalog::McpTransportKind::Http,
                            "sse" => forge_provider_api::catalog::McpTransportKind::Sse,
                            _ => forge_provider_api::catalog::McpTransportKind::Unknown,
                        })
                        .unwrap_or(forge_provider_api::catalog::McpTransportKind::Unknown),
                    status: if server.enabled {
                        McpStatus::Loading
                    } else {
                        McpStatus::Disabled
                    },
                    status_detail: None,
                    token_count: None,
                    token_estimated: false,
                    tools: Vec::new(),
                    tool_error: None,
                })
                .collect::<Vec<_>>();
            servers.sort_by(|left, right| left.name.cmp(&right.name));
            Ok(servers)
        }
        .await;
        result.map_err(|error| {
            tracing::error!(target: "forge::provider", event_name = "provider.catalog.failed",
                provider = "copilot", operation = "mcp_list", error = %format_args!("{error:#}"));
            native_failure(
                forge_provider_api::error::ProviderOperation::TurnStart,
                forge_provider_api::error::ProviderFailureKind::Transport,
            )
        })
    }
    pub async fn fork(
        &self,
        session_id: String,
        checkpoint: Option<String>,
    ) -> Result<String, ProviderError> {
        let result: Result<String> = async {
            super::operation::dispatch_for(forge_provider_api::ProviderOperation::ThreadFork)?;
            Ok(self
                .inner
                .rpc()
                .sessions()
                .fork(SessionsForkRequest {
                    name: None,
                    session_id: SessionId::new(session_id),
                    to_event_id: checkpoint,
                })
                .await
                .context("fork Copilot session")?
                .session_id
                .to_string())
        }
        .await;
        result.map_err(|error| {
            tracing::error!(target: "forge::provider", event_name = "provider.fork.failed",
                provider = "copilot", error = %format_args!("{error:#}"));
            error.downcast::<ProviderError>().unwrap_or_else(|_| {
                native_failure(
                    forge_provider_api::ProviderOperation::ThreadFork,
                    forge_provider_api::ProviderFailureKind::Transport,
                )
            })
        })
    }
}

impl CopilotSession {
    /// Let accepted input proceed while selected MCP servers finish starting.
    ///
    /// Copilot holds a queued turn until its in-flight MCP load settles, which a hung server holds
    /// for the native startup bound. `moveLoadingToBackground` releases only turns already
    /// waiting, so the call follows native acceptance of the input; before any turn waits it is a
    /// no-op. Release is best effort: on failure the turn waits for the load as Copilot defaults to.
    pub async fn release_mcp_loading(&self) {
        if self.mcp_loading.load(std::sync::atomic::Ordering::Acquire) {
            let _ =
                super::operation::wait(self.inner.rpc().mcp().move_loading_to_background()).await;
        }
    }

    /// Check whether a failure applies to this session's exact native client.
    pub fn belongs_to(&self, failed: &super::client_generation::FailedGeneration) -> bool {
        failed.matches(&self.generation)
    }

    /// Reject reuse of a cached binding whose shared client has failed.
    pub fn ensure_healthy(&self) -> Result<(), ProviderError> {
        self.generation.ensure_healthy()
    }

    /// Join local readers after client collection, without a dead transport detach RPC.
    pub async fn local_collect_after_client_stop(
        &self,
        proof: &super::client_generation::CollectedClient,
    ) -> Result<(), ProviderError> {
        if !self.belongs_to(proof.failed_generation()) {
            return Err(native_failure(
                forge_provider_api::error::ProviderOperation::TurnInterrupt,
                forge_provider_api::error::ProviderFailureKind::Rejected,
            ));
        }
        self.events.close().await?;
        self.inner.stop_event_loop().await;
        Ok(())
    }

    pub fn id(&self) -> String {
        self.inner.id().to_string()
    }
    pub fn workspace_path(&self) -> Option<&Path> {
        self.inner.workspace_path()
    }
    pub fn cwd(&self) -> &Path {
        self.inner.cwd()
    }
    pub async fn disconnect(&self) -> Result<(), ProviderError> {
        let intake = self.events.close().await;
        if self.generation.is_collected() {
            self.inner.stop_event_loop().await;
            return intake;
        }
        if self.events.client_stopped() {
            // A stopped transport is not proof that its process and resources were collected.
            // Keep cleanup ownership until the runtime supplies a CollectedClient witness.
            return intake.and_then(|()| self.generation.ensure_healthy());
        }
        let native = self.inner.disconnect().await.map_err(|_| {
            native_failure(
                forge_provider_api::error::ProviderOperation::TurnInterrupt,
                forge_provider_api::error::ProviderFailureKind::Transport,
            )
        });
        match (intake, native) {
            (Err(cause), Err(cleanup)) => Err(ProviderError::PreparationCleanup {
                cause: Box::new(cause),
                cleanup: Box::new(cleanup),
            }),
            (Err(error), _) | (_, Err(error)) => Err(error),
            (Ok(()), Ok(())) => Ok(()),
        }
    }
    /// Interrupt foreground generation while preserving independently owned shell tasks.
    pub async fn abort(&self) -> Result<(), ProviderError> {
        self.ensure_healthy()?;
        let result = self
            .inner
            .rpc()
            .abort(AbortRequest::default())
            .await
            .map_err(|_| {
                ProviderError::Failure(
                    forge_provider_api::ProviderFailure::new(
                        forge_provider_api::ProviderOperation::TurnInterrupt,
                        forge_provider_api::ProviderFailureKind::Transport,
                    )
                    .at_phase(
                        forge_provider_api::ProviderPhase::Interruption,
                        forge_provider_api::FailureDisposition::Uncertain,
                    ),
                )
            })?;
        if !result.success {
            return Err(ProviderError::Failure(
                forge_provider_api::ProviderFailure::new(
                    forge_provider_api::ProviderOperation::TurnInterrupt,
                    forge_provider_api::ProviderFailureKind::Rejected,
                )
                .at_phase(
                    forge_provider_api::ProviderPhase::Interruption,
                    forge_provider_api::FailureDisposition::Uncertain,
                ),
            ));
        }
        Ok(())
    }

    /// Inspect native tasks before allowing a destructive SDK refresh of the binding.
    pub async fn terminal_snapshot(
        &self,
    ) -> Result<forge_provider_api::terminal::TerminalInventory, ProviderError> {
        let tasks = self.native_terminal_tasks().await?;
        let encoded = serde_json::to_value(tasks).map_err(|_| {
            native_failure(
                forge_provider_api::ProviderOperation::TerminalList,
                forge_provider_api::ProviderFailureKind::Protocol,
            )
        })?;
        let inventory = crate::terminal::parse_snapshot(&encoded, true).map_err(|error| {
            forge_provider_api::ProviderFailure::terminal_inventory(
                forge_provider_api::ProviderOperation::TerminalList,
                error,
            )
        })?;
        Ok(inventory)
    }

    /// Refresh native liveness before reading task rows for display or cancellation.
    async fn native_terminal_tasks(&self) -> Result<TaskList, ProviderError> {
        self.ensure_healthy()?;
        self.inner.rpc().tasks().refresh().await.map_err(|_| {
            native_failure(
                forge_provider_api::ProviderOperation::TerminalList,
                forge_provider_api::ProviderFailureKind::Transport,
            )
        })?;
        self.inner.rpc().tasks().list().await.map_err(|_| {
            native_failure(
                forge_provider_api::ProviderOperation::TerminalList,
                forge_provider_api::ProviderFailureKind::Transport,
            )
        })
    }

    /// Stop every detached shell before closing the entire binding.
    pub async fn terminate_all(&self) -> Result<(), ProviderError> {
        let tasks = self.native_terminal_tasks().await?;
        let identities =
            crate::terminal::copilot_task_identities(&tasks.tasks).map_err(|error| {
                forge_provider_api::ProviderFailure::terminal_inventory(
                    forge_provider_api::ProviderOperation::TerminalTerminate,
                    error,
                )
            })?;
        for id in identities {
            self.terminate_terminal(id.as_str()).await?;
        }
        Ok(())
    }

    /// Report whether this binding belongs to a stopped native process.
    pub fn client_stopped(&self) -> bool {
        self.events.client_stopped()
    }
    /// Get positive collection proof for this exact SDK process, without treating failure as stop.
    pub fn client_collected(&self) -> bool {
        self.generation.is_collected()
    }

    pub async fn set_model(
        &self,
        model: &str,
        effort: Option<String>,
        context: Option<&str>,
    ) -> Result<(), ProviderError> {
        self.inner
            .set_model(model, Some(model_options(effort, context)?))
            .await
            .map(|_| ())
            .map_err(|_| {
                native_failure(
                    forge_provider_api::error::ProviderOperation::ThreadConfigure,
                    forge_provider_api::error::ProviderFailureKind::Transport,
                )
            })
    }

    /// Confirm that an idle configuration update applied instead of accepting a queued switch.
    pub async fn set_model_confirmed(
        &self,
        model: &str,
        effort: Option<String>,
        context: Option<&str>,
    ) -> Result<(), ProviderError> {
        let options = model_options(effort, context)?;
        let result = self
            .inner
            .rpc()
            .model()
            .switch_to(ModelSwitchToRequest {
                model_id: model.to_owned(),
                reasoning_effort: options.reasoning_effort,
                reasoning_summary: options.reasoning_summary,
                context_tier: options.context_tier,
                ..Default::default()
            })
            .await
            .map_err(|_| {
                native_failure(
                    forge_provider_api::ProviderOperation::ThreadConfigure,
                    forge_provider_api::ProviderFailureKind::Transport,
                )
            })?;
        confirm_model_switch(model, &result)
    }
    pub async fn skills(&self) -> Result<Vec<SkillDefinition>, ProviderError> {
        let result: Result<Vec<SkillDefinition>> = async {
            let skill_list = self
                .inner
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
                    scope: None,
                    resource_id: None,
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
        .await;
        result.map_err(|_| {
            native_failure(
                forge_provider_api::error::ProviderOperation::TurnStart,
                forge_provider_api::error::ProviderFailureKind::Transport,
            )
        })
    }
    pub async fn mcp_list(
        &self,
        definition: Vec<McpDefinition>,
        mcp_failure: &HashMap<String, String>,
    ) -> Result<Vec<McpDefinition>, ProviderError> {
        let result: Result<Vec<McpDefinition>> = async {
            let server_list = self
                .inner
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
            let token_by_name = self
                .inner
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
            for discovered in definition {
                if !discovered.enabled || discovered.status == McpStatus::Unavailable {
                    definition_list.push(discovered);
                    continue;
                }
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
                let mut status = match status_text.as_str() {
                    "disabled" | "not_configured" => McpStatus::Disabled,
                    "pending" => McpStatus::Loading,
                    "connected" => McpStatus::Connected,
                    "needs-auth" | "needs_auth" => McpStatus::NeedsAuthentication,
                    "failed" => McpStatus::Failed,
                    _ => McpStatus::Unavailable,
                };
                if discovered.transport == forge_provider_api::catalog::McpTransportKind::Unknown {
                    status = McpStatus::Unavailable;
                }
                let startup_error = mcp_failure.get(&discovered.name);
                if startup_error.is_some()
                    && matches!(
                        status,
                        McpStatus::Disabled | McpStatus::Loading | McpStatus::Unavailable
                    )
                {
                    status = McpStatus::Failed;
                }
                let (tools, tool_error) = if status == McpStatus::Connected {
                    match self
                        .inner
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
                        Err(_) => (
                            Vec::new(),
                            forge_provider_api::diagnostic::SanitizedMessage::new(
                                "MCP tool catalog is unavailable",
                            )
                            .ok(),
                        ),
                    }
                } else {
                    (Vec::new(), None)
                };
                definition_list.push(McpDefinition {
                    scope: discovered.scope,
                    source_path: discovered.source_path,
                    resource_id: discovered.resource_id,
                    token_count: token_by_name.get(&discovered.name).copied(),
                    token_estimated: false,
                    enabled: discovered.enabled,
                    status,
                    status_detail: if discovered.transport
                        == forge_provider_api::catalog::McpTransportKind::Unknown
                    {
                        forge_provider_api::diagnostic::SanitizedMessage::new(
                            "MCP transport is not supported by this provider",
                        )
                        .ok()
                    } else if status_entry
                        .and_then(|server| server.error.as_ref())
                        .is_some()
                        || startup_error.is_some()
                    {
                        forge_provider_api::diagnostic::SanitizedMessage::new(
                            "MCP server reported a failure",
                        )
                        .ok()
                    } else {
                        None
                    },
                    name: discovered.name,
                    transport: discovered.transport,
                    tools,
                    tool_error,
                });
            }
            definition_list.sort_by(|left, right| left.name.cmp(&right.name));
            Ok(definition_list)
        }
        .await;
        result.map_err(|_| {
            native_failure(
                forge_provider_api::error::ProviderOperation::TurnStart,
                forge_provider_api::error::ProviderFailureKind::Transport,
            )
        })
    }
    pub async fn terminate_terminal(&self, id: &str) -> Result<(), ProviderError> {
        let result: Result<()> = async {
            let inventory = self.native_terminal_tasks().await?;
            let identities =
                crate::terminal::copilot_task_identities(&inventory.tasks).map_err(|error| {
                    ProviderError::from(forge_provider_api::ProviderFailure::terminal_inventory(
                        forge_provider_api::ProviderOperation::TerminalTerminate,
                        error,
                    ))
                })?;
            anyhow::ensure!(
                identities.iter().any(|terminal| terminal.as_str() == id),
                "Background terminal is no longer running"
            );
            super::operation::dispatch_for(
                forge_provider_api::ProviderOperation::TerminalTerminate,
            )?;
            let result = self
                .inner
                .rpc()
                .tasks()
                .cancel(github_copilot_sdk::rpc::TasksCancelRequest { id: id.into() })
                .await?;
            anyhow::ensure!(
                result.cancelled,
                "Background terminal could not be cancelled"
            );

            Ok(())
        }
        .await;
        result.map_err(|error| {
            error.downcast::<ProviderError>().unwrap_or_else(|_| {
                native_failure(
                    forge_provider_api::ProviderOperation::TerminalTerminate,
                    forge_provider_api::ProviderFailureKind::Transport,
                )
            })
        })
    }
    /// Capture a durable event anchor before sending, independently of process-local cursors.
    pub async fn delivery_boundary(&self) -> Result<(String, Option<String>), ProviderError> {
        let tail = self.inner.rpc().event_log().tail().await.map_err(|_| {
            native_failure(
                forge_provider_api::ProviderOperation::TurnStart,
                forge_provider_api::ProviderFailureKind::Transport,
            )
        })?;
        let history = self
            .inner
            .rpc()
            .event_log()
            .read(EventLogReadRequest {
                agent_ids: None,
                direction: Some(EventsReadDirection::Backward),
                include_ephemeral: Some(false),
                cursor: None,
                agent_scope: Some(EventsAgentScope::Primary),
                types: None,
                max: Some(1),
                wait_ms: Some(0),
            })
            .await
            .map_err(|_| {
                native_failure(
                    forge_provider_api::ProviderOperation::TurnStart,
                    forge_provider_api::ProviderFailureKind::Transport,
                )
            })?;
        if history.cursor_status != EventsCursorStatus::Ok {
            return Err(native_failure(
                forge_provider_api::ProviderOperation::TurnStart,
                forge_provider_api::ProviderFailureKind::Protocol,
            ));
        }
        Ok((
            tail.cursor,
            history.events.last().map(|event| event.id.clone()),
        ))
    }

    /// Read one pinned durable snapshot without creating, resuming, or activating native history.
    pub async fn find_unclaimed_submission_in(
        client: &github_copilot_sdk::Client,
        session_id: &str,
        anchor: Option<&str>,
        known_receipts: &HashSet<String>,
        content_digest: Option<&str>,
    ) -> std::result::Result<SubmissionReconciliation, ProviderError> {
        let mut cursor = None;
        let mut candidates = Vec::new();
        let mut after_boundary = anchor.is_none();
        loop {
            let history = client
                .rpc()
                .sessions()
                .read_persisted_events(SessionsReadPersistedEventsRequest {
                    session_id: SessionId::new(session_id),
                    cursor,
                    direction: Some(EventsReadDirection::Forward),
                    max: Some(100),
                })
                .await
                .map_err(|_| {
                    native_failure(
                        forge_provider_api::ProviderOperation::InputReconcile,
                        forge_provider_api::ProviderFailureKind::Transport,
                    )
                })?;
            if history.cursor_status != EventsCursorStatus::Ok {
                return Ok(SubmissionReconciliation::Ambiguous);
            }
            for event in history.events {
                if !after_boundary {
                    after_boundary = Some(event.id.as_str()) == anchor;
                    continue;
                }
                if event.agent_id.is_some()
                    || event.event_type != "user.message"
                    || event
                        .data
                        .get("isAutopilotContinuation")
                        .and_then(Value::as_bool)
                        == Some(true)
                    || event
                        .data
                        .get("source")
                        .is_some_and(|source| !source.is_null())
                {
                    continue;
                }
                let Some(message_id) = event.data.get("messageId").and_then(Value::as_str) else {
                    continue;
                };
                candidates.push((
                    message_id.to_owned(),
                    event
                        .data
                        .get("content")
                        .and_then(Value::as_str)
                        .map(str::to_owned),
                ));
            }
            if !history.has_more {
                return Ok(if after_boundary {
                    resolve_unclaimed_submissions(&candidates, known_receipts, content_digest)
                } else {
                    SubmissionReconciliation::Ambiguous
                });
            }
            cursor = Some(history.cursor);
        }
    }
    pub async fn rewind(
        &self,
        target: super::turn::CopilotRetraction,
    ) -> Result<(), ProviderError> {
        let result: Result<()> = async {
            let super::turn::CopilotRetraction {
                mut cursor,
                event_id,
                message_id,
                boundary,
            } = target;
            let mut found = false;
            let mut last_event = None;
            let mut seen = HashSet::new();
            loop {
                let history = self
                    .inner
                    .rpc()
                    .event_log()
                    .read(EventLogReadRequest {
                        agent_ids: None,
                        direction: None,
                        include_ephemeral: Some(false),
                        cursor: Some(cursor.clone()),
                        agent_scope: Some(EventsAgentScope::Primary),
                        types: None,
                        max: Some(100),
                        wait_ms: Some(0),
                    })
                    .await
                    .context("check exact cancelled input before native truncation")?;
                anyhow::ensure!(
                    history.cursor_status == EventsCursorStatus::Ok,
                    "Copilot retraction boundary expired"
                );
                for event in history.events {
                    last_event = Some(event.id.clone());
                    if event.agent_id.is_some() || event.event_type != "user.message" {
                        continue;
                    }
                    anyhow::ensure!(
                        !found
                            && event.id == event_id
                            && event.data.get("messageId").and_then(Value::as_str)
                                == Some(message_id.as_str()),
                        "Copilot retraction would remove another input"
                    );
                    found = true;
                }
                if !history.has_more {
                    break;
                }
                anyhow::ensure!(
                    history.cursor != cursor && seen.insert(history.cursor.clone()),
                    "Copilot history cursor did not advance"
                );
                cursor = history.cursor;
            }
            anyhow::ensure!(
                found && last_event.as_deref() == Some(boundary.as_str()),
                "Copilot retraction history boundary changed"
            );
            super::operation::dispatch_for(forge_provider_api::ProviderOperation::InputRetract)?;
            let result = self
                .inner
                .rpc()
                .history()
                .truncate(HistoryTruncateRequest { event_id })
                .await
                .context("rewind output-free Copilot turn")?;
            anyhow::ensure!(
                result.events_removed > 0,
                "Copilot rewind removed no history events"
            );

            Ok(())
        }
        .await;
        result.map_err(|error| {
            error.downcast::<ProviderError>().unwrap_or_else(|_| {
                native_failure(
                    forge_provider_api::ProviderOperation::InputRetract,
                    forge_provider_api::ProviderFailureKind::Transport,
                )
            })
        })
    }
    /// Truncate native history so it ends at the requested turn checkpoint.
    ///
    /// Copilot checkpoints name a turn's terminal event. The first primary `user.message` after
    /// it begins the discarded suffix; when none exists, history already ends there and a repeated
    /// rewind succeeds without mutation.
    pub async fn rewind_after(&self, after: Option<&str>) -> Result<(), ProviderError> {
        const OPERATION: forge_provider_api::ProviderOperation =
            forge_provider_api::ProviderOperation::ThreadRewind;
        let Some(event_id) = self.first_input_after(after).await? else {
            return Ok(());
        };
        super::operation::dispatch_for(OPERATION)?;
        let result = self
            .inner
            .rpc()
            .history()
            .truncate(HistoryTruncateRequest { event_id })
            .await;
        if result.is_err() || self.first_input_after(after).await?.is_some() {
            return Err(forge_provider_api::ProviderFailure::new(
                OPERATION,
                forge_provider_api::ProviderFailureKind::Transport,
            )
            .at_phase(
                forge_provider_api::ProviderPhase::Observation,
                forge_provider_api::FailureDisposition::Uncertain,
            )
            .into());
        }
        Ok(())
    }

    /// Find the first primary input after a checkpoint, rejecting a checkpoint absent from history.
    async fn first_input_after(
        &self,
        after: Option<&str>,
    ) -> Result<Option<String>, ProviderError> {
        const OPERATION: forge_provider_api::ProviderOperation =
            forge_provider_api::ProviderOperation::ThreadRewind;
        let transport = || {
            native_failure(
                OPERATION,
                forge_provider_api::ProviderFailureKind::Transport,
            )
        };
        let mut cursor = None;
        let mut past_checkpoint = after.is_none();
        let mut seen = HashSet::new();
        loop {
            let history = self
                .inner
                .rpc()
                .event_log()
                .read(EventLogReadRequest {
                    agent_ids: None,
                    direction: Some(EventsReadDirection::Forward),
                    include_ephemeral: Some(false),
                    cursor: cursor.clone(),
                    agent_scope: Some(EventsAgentScope::Primary),
                    types: None,
                    max: Some(100),
                    wait_ms: Some(0),
                })
                .await
                .map_err(|_| transport())?;
            if history.cursor_status != EventsCursorStatus::Ok {
                return Err(transport());
            }
            for event in history.events {
                if !past_checkpoint {
                    past_checkpoint = Some(event.id.as_str()) == after;
                    continue;
                }
                if event.agent_id.is_none() && event.event_type == "user.message" {
                    return Ok(Some(event.id));
                }
            }
            if !history.has_more {
                break;
            }
            if !seen.insert(history.cursor.clone()) {
                return Err(transport());
            }
            cursor = Some(history.cursor);
        }
        if past_checkpoint {
            Ok(None)
        } else {
            Err(native_failure(
                OPERATION,
                forge_provider_api::ProviderFailureKind::Rejected,
            ))
        }
    }

    pub async fn compact(&self, id: String) -> Result<Compaction, ProviderError> {
        let result: Result<Compaction> = async {
            super::operation::dispatch_for(forge_provider_api::ProviderOperation::ThreadCompact)?;
            let result = self
                .inner
                .rpc()
                .history()
                .compact()
                .await
                .context("compact Copilot history")?;
            let data = serde_json::to_value(&result)?;
            Ok(Compaction {
                id,
                thread_id: self.id(),
                state: if result.success {
                    CompactionState::Completed
                } else {
                    CompactionState::Failed
                },
                manual: true,
                started_at_ms: None,
                completed_at_ms: None,
                result: serde_json::from_value(data)?,
            })
        }
        .await;
        result.map_err(|error| {
            error.downcast::<ProviderError>().unwrap_or_else(|_| {
                native_failure(
                    forge_provider_api::ProviderOperation::ThreadCompact,
                    forge_provider_api::ProviderFailureKind::Transport,
                )
            })
        })
    }
    pub async fn steer(
        &self,
        input: MessageInput,
        receipt: Option<Arc<dyn InputReceipt>>,
        scope: &forge_provider_api::InvocationScope,
        workspace: &str,
        authority: super::turn::NativeSendAuthority<'_>,
    ) -> Result<String, ProviderError> {
        self.ensure_healthy()?;
        let message = super::input::message(input).with_mode(DeliveryMode::Immediate);
        if let Some(receipt) = &receipt {
            let (cursor, anchor) = self.delivery_boundary().await?;
            self.ensure_healthy()?;
            receipt
                .clone()
                .record_sending(Some(super::prompt::sending_recovery_token(
                    scope.thread_id.clone(),
                    receipt.id().clone(),
                    receipt.attempt_id()?,
                    super::prompt::NativeDeliveryBoundary {
                        session_id: self.id(),
                        cursor,
                        anchor,
                    },
                    workspace.to_owned(),
                    hex::encode(Sha256::digest(message.prompt.as_bytes())),
                )?))
                .await?;
        }
        self.ensure_healthy()?;
        let accepted = match super::input::send(&self.inner, message, None, authority).await {
            Ok(accepted) => accepted,
            Err(error) => {
                if super::operation::current().is_some_and(|operation| !operation.dispatched())
                    && let Some(receipt) = &receipt
                {
                    receipt.clone().record_rejected(error.clone()).await?;
                }
                return Err(error);
            }
        };
        if let Some(receipt) = receipt {
            let receipt_id = receipt.id().clone();
            let attempt_id = receipt.attempt_id()?;
            receipt
                .record_accepted(Some(super::prompt::accepted_recovery_token(
                    receipt_id,
                    attempt_id,
                    scope.thread_id.clone(),
                    self.id(),
                    accepted.clone(),
                )?))
                .await
                .map_err(|_| ProviderError::DeliveryUncertain)?;
        }
        Ok(accepted)
    }
    pub async fn reconcile_skills(&self, selected: &[SkillSelection]) -> Result<(), ProviderError> {
        let result: Result<()> = async {
            let mut stage = "inspect Copilot skill selection";
            super::operation::wait(async {
                let loaded = self
                    .inner
                    .rpc()
                    .skills()
                    .list()
                    .await
                    .context("inspect Copilot skill selection")?;
                for skill in loaded.skills.iter().filter(|skill| {
                    skill.enabled
                        && !selected
                            .iter()
                            .any(|wanted| wanted.name == skill.name && wanted.enabled)
                }) {
                    self.inner
                        .rpc()
                        .skills()
                        .disable(SkillsDisableRequest {
                            name: skill.name.clone(),
                        })
                        .await
                        .with_context(|| {
                            format!("exclude unselected Copilot skill {}", skill.name)
                        })?;
                }
                stage = "verify Copilot skill selection";
                let loaded = self
                    .inner
                    .rpc()
                    .skills()
                    .list()
                    .await
                    .context("verify Copilot skill selection")?;
                for skill in selected {
                    let enabled = loaded
                        .skills
                        .iter()
                        .any(|candidate| candidate.name == skill.name && candidate.enabled);
                    anyhow::ensure!(
                        enabled == skill.enabled,
                        "Copilot did not apply skill selection for {}",
                        skill.name
                    );
                }
                Ok::<_, anyhow::Error>(())
            })
            .await
            .with_context(|| {
                format!("Copilot resource verification timed out while attempting to {stage}")
            })
            .and_then(|result| result)
        }
        .await;
        result.map_err(|_| {
            native_failure(
                forge_provider_api::error::ProviderOperation::TurnStart,
                forge_provider_api::error::ProviderFailureKind::Transport,
            )
        })
    }
}

/// Admit only an authoritative applied acknowledgement for the requested model.
fn confirm_model_switch(model: &str, result: &ModelSwitchToResult) -> Result<(), ProviderError> {
    if result.persistence_error.is_some() {
        return Err(native_failure(
            forge_provider_api::ProviderOperation::ThreadConfigure,
            forge_provider_api::ProviderFailureKind::Runtime,
        ));
    }
    if result.status.as_deref() != Some("applied")
        || result.deferred == Some(true)
        || result.model_id.as_deref() != Some(model)
    {
        return Err(native_failure(
            forge_provider_api::ProviderOperation::ThreadConfigure,
            forge_provider_api::ProviderFailureKind::Protocol,
        ));
    }
    Ok(())
}

pub fn model_options(
    reasoning_effort: Option<String>,
    context_window: Option<&str>,
) -> Result<SetModelOptions, ProviderError> {
    let mut options = SetModelOptions::default().with_reasoning_summary(ReasoningSummary::Concise);
    if let Some(reasoning_effort) = reasoning_effort {
        options = options.with_reasoning_effort(reasoning_effort);
    }
    let context_tier = match context_window {
        None | Some("default") => ContextTier::Default,
        Some("long_context") => ContextTier::LongContext,
        Some(_) => {
            return Err(forge_provider_api::capability::UnsupportedCapability(
                forge_provider_api::capability::ProviderFeature::ContextWindowSelection,
            )
            .into());
        }
    };
    options = options.with_context_tier(context_tier);
    Ok(options)
}

/// Compare a native history message with the private digest in a Sending checkpoint.
pub(super) fn content_matches(expected: Option<&str>, content: &str) -> bool {
    expected.is_none_or(|expected| hex::encode(Sha256::digest(content.as_bytes())) == expected)
}

/// Select exactly one unclaimed history input matching the persisted content evidence.
pub(super) fn resolve_unclaimed_submissions(
    candidates: &[(String, Option<String>)],
    known_receipts: &HashSet<String>,
    content_digest: Option<&str>,
) -> SubmissionReconciliation {
    let unclaimed = candidates
        .iter()
        .filter(|(message_id, _)| !known_receipts.contains(message_id))
        .collect::<Vec<_>>();
    let matching = unclaimed
        .iter()
        .filter(|(_, content)| {
            content_digest.is_none_or(|expected| {
                content
                    .as_deref()
                    .is_some_and(|content| content_matches(Some(expected), content))
            })
        })
        .map(|(message_id, _)| message_id.as_str())
        .collect::<Vec<_>>();
    match matching.as_slice() {
        [message_id] => SubmissionReconciliation::Found((*message_id).to_owned()),
        [] if unclaimed.is_empty() => SubmissionReconciliation::NotFound,
        [] => SubmissionReconciliation::ContentMismatch,
        _ => SubmissionReconciliation::Ambiguous,
    }
}

fn native_failure(
    operation: forge_provider_api::error::ProviderOperation,
    kind: forge_provider_api::error::ProviderFailureKind,
) -> ProviderError {
    ProviderError::Failure(forge_provider_api::ProviderFailure::new(operation, kind))
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Mutex;
    use tracing::{
        field::{Field, Visit},
        instrument::WithSubscriber,
    };
    use tracing_subscriber::{Layer, layer::SubscriberExt};

    struct CapturedError(Arc<Mutex<Vec<String>>>);

    impl Visit for CapturedError {
        /// Capture only the emitted cause, never unrelated SDK fields.
        fn record_debug(&mut self, field: &Field, value: &dyn std::fmt::Debug) {
            if field.name() == "error" {
                self.0.lock().unwrap().push(format!("{value:?}"));
            }
        }
    }

    impl<SubscriberType: tracing::Subscriber> Layer<SubscriberType> for CapturedError {
        /// Observe the real adapter's diagnostic emission before normalization.
        fn on_event(
            &self,
            event: &tracing::Event<'_>,
            _context: tracing_subscriber::layer::Context<'_, SubscriberType>,
        ) {
            event.record(&mut CapturedError(self.0.clone()));
        }
    }

    /// Keep native catalog causes in diagnostics without changing normalized errors or logging RPC data.
    #[tokio::test]
    async fn model_list_failure_logs_native_cause_before_normalization() {
        let captured = Arc::new(Mutex::new(Vec::new()));
        let subscriber =
            tracing_subscriber::Registry::default().with(CapturedError(captured.clone()));
        let (client, peer) = super::super::test_peer::client(|request| {
            assert_eq!(request["method"], "models.list");
            vec![
                serde_json::json!({"jsonrpc":"2.0","id":request["id"],"error":{
                    "code":-32603,"message":"native-catalog-cause","data":{"token":"private-rpc-data"}
                }}),
            ]
        });
        let client = CopilotClient::new(client);
        let error = client
            .models()
            .with_subscriber(subscriber)
            .await
            .unwrap_err();
        assert!(!error.to_string().contains("native-catalog-cause"));
        let recorded = captured.lock().unwrap().join("\n");
        assert!(
            recorded.contains("list Copilot models") && recorded.contains("native-catalog-cause"),
            "{recorded}"
        );
        assert!(!recorded.contains("private-rpc-data"));
        client.stop().await.unwrap();
        peer.collected().await;
    }
}
