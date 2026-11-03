use anyhow::{Context, Result};
use forge_provider_api::catalog::{
    CatalogMutation, McpDefinition, McpStatus, McpToolDefinition, SkillDefinition,
};
use forge_provider_api::input::MessageInput;
use forge_provider_api::submission::InputReceipt;
use forge_provider_api::{Compaction, CompactionState, ProviderError, ProviderModel, ServiceTier};
use github_copilot_sdk::rpc::*;
use github_copilot_sdk::session_events::ReasoningSummary;
use github_copilot_sdk::types::ContextTier;
use github_copilot_sdk::{DeliveryMode, SessionId, SetModelOptions};
use serde_json::{Value, json};
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

    /// Get whether this shared transport is unavailable to retained session handles.
    #[cfg(test)]
    pub(crate) fn is_stopped(&self) -> bool {
        self.generation.is_stopped()
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
            if models.is_empty() {
                Ok(vec![default_model_descriptor()])
            } else {
                let mut models: Vec<_> = models
                    .into_iter()
                    .map(super::catalog::model_descriptor)
                    .collect();
                super::catalog::order_models(&mut models);
                Ok(models)
            }
        }
        .await;
        result.map_err(|_| {
            native_failure(
                forge_provider_api::error::ProviderOperation::NativeTransport,
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
        result.map_err(|_| {
            native_failure(
                forge_provider_api::error::ProviderOperation::NativeTransport,
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
        result.map_err(|_| {
            native_failure(
                forge_provider_api::error::ProviderOperation::NativeTransport,
                forge_provider_api::error::ProviderFailureKind::Transport,
            )
        })
    }
}

impl CopilotSession {
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
                forge_provider_api::error::ProviderOperation::CleanupExecution,
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
        if self.events.client_stopped() {
            // A stopped transport is not proof that its process and resources were collected.
            // Keep cleanup ownership until the runtime supplies a CollectedClient witness.
            return intake.and_then(|()| self.generation.ensure_healthy());
        }
        let native = self.inner.disconnect().await.map_err(|_| {
            native_failure(
                forge_provider_api::error::ProviderOperation::CleanupExecution,
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
    /// Abort foreground generation and stop this session's detached shell tasks.
    pub async fn abort(&self) -> Result<(), ProviderError> {
        if self.events.client_stopped() {
            return Ok(());
        }
        let foreground = self.inner.abort().await.map_err(|_| {
            native_failure(
                forge_provider_api::error::ProviderOperation::CancelSession,
                forge_provider_api::error::ProviderFailureKind::Transport,
            )
        });
        // Native abort acknowledges model interruption without stopping detached shell tasks.
        let shells = async {
            self.inner.rpc().tasks().refresh().await.map_err(|_| {
                native_failure(
                    forge_provider_api::ProviderOperation::CancelSession,
                    forge_provider_api::ProviderFailureKind::Transport,
                )
            })?;
            let tasks = self.inner.rpc().tasks().list().await.map_err(|_| {
                native_failure(
                    forge_provider_api::ProviderOperation::CancelSession,
                    forge_provider_api::ProviderFailureKind::Transport,
                )
            })?;
            let encoded = serde_json::to_value(tasks).map_err(|_| {
                native_failure(
                    forge_provider_api::ProviderOperation::CancelSession,
                    forge_provider_api::ProviderFailureKind::Protocol,
                )
            })?;
            let inventory = crate::terminal::parse_snapshot(&encoded, true).map_err(|_| {
                native_failure(
                    forge_provider_api::ProviderOperation::CancelSession,
                    forge_provider_api::ProviderFailureKind::Protocol,
                )
            })?;
            for terminal in inventory.terminal {
                let result = self
                    .inner
                    .rpc()
                    .tasks()
                    .cancel(TasksCancelRequest { id: terminal.id })
                    .await
                    .map_err(|_| {
                        native_failure(
                            forge_provider_api::ProviderOperation::CancelSession,
                            forge_provider_api::ProviderFailureKind::Transport,
                        )
                    })?;
                if !result.cancelled {
                    return Err(native_failure(
                        forge_provider_api::ProviderOperation::CancelSession,
                        forge_provider_api::ProviderFailureKind::Rejected,
                    ));
                }
            }
            Ok::<(), ProviderError>(())
        }
        .await;
        match (foreground, shells) {
            (Ok(()), result) | (result, Ok(())) => result,
            (Err(primary), Err(secondary)) => Err(ProviderError::Combined {
                primary: Box::new(primary),
                secondary: vec![secondary],
            }),
        }
    }

    /// Report whether this binding belongs to a stopped native process.
    pub fn client_stopped(&self) -> bool {
        self.events.client_stopped()
    }
    pub async fn set_model(
        &self,
        model: &str,
        effort: Option<String>,
        context: Option<&str>,
    ) -> Result<(), ProviderError> {
        self.inner
            .set_model(model, Some(model_options(effort, context)))
            .await
            .map(|_| ())
            .map_err(|_| {
                native_failure(
                    forge_provider_api::error::ProviderOperation::ReconcileSettings,
                    forge_provider_api::error::ProviderFailureKind::Transport,
                )
            })
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
                forge_provider_api::error::ProviderOperation::NativeTransport,
                forge_provider_api::error::ProviderFailureKind::Transport,
            )
        })
    }
    pub async fn set_skill_enabled(
        &self,
        name: &str,
        enabled: bool,
    ) -> Result<CatalogMutation, ProviderError> {
        let result: Result<CatalogMutation> = async {
            if enabled {
                self.inner
                    .rpc()
                    .skills()
                    .enable(SkillsEnableRequest { name: name.into() })
                    .await
                    .with_context(|| format!("enable Copilot skill ${name}"))?;
            } else {
                self.inner
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
        .await;
        result.map_err(|_| {
            native_failure(
                forge_provider_api::error::ProviderOperation::ReconcileSettings,
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
                forge_provider_api::error::ProviderOperation::NativeTransport,
                forge_provider_api::error::ProviderFailureKind::Transport,
            )
        })
    }
    pub async fn set_mcp_enabled(
        &self,
        name: &str,
        enabled: bool,
    ) -> Result<CatalogMutation, ProviderError> {
        let result: Result<CatalogMutation> = async {
            if enabled {
                self.inner
                    .rpc()
                    .mcp()
                    .enable(McpEnableRequest {
                        server_name: name.into(),
                    })
                    .await
                    .with_context(|| format!("enable Copilot MCP {name}"))?;
            } else {
                self.inner
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
        .await;
        result.map_err(|_| {
            native_failure(
                forge_provider_api::error::ProviderOperation::NativeTransport,
                forge_provider_api::error::ProviderFailureKind::Transport,
            )
        })
    }
    pub async fn terminate_terminal(&self, id: &str) -> Result<(), ProviderError> {
        let result: Result<()> = async {
            self.inner.rpc().tasks().refresh().await?;
            let inventory = self.inner.rpc().tasks().list().await?;
            let inventory =
                crate::terminal::parse_snapshot(&serde_json::to_value(inventory)?, true)?;
            anyhow::ensure!(
                inventory.terminal.iter().any(|terminal| terminal.id == id),
                "Background terminal is no longer running"
            );
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
        result.map_err(|_| {
            native_failure(
                forge_provider_api::error::ProviderOperation::NativeTransport,
                forge_provider_api::error::ProviderFailureKind::Transport,
            )
        })
    }
    /// Reconcile a persisted cursor using only root input events and opaque accepted receipts.
    pub async fn find_unclaimed_submission_in(
        session: &github_copilot_sdk::session::Session,
        cursor: &str,
        known_receipts: &HashSet<String>,
        content_digest: Option<&str>,
    ) -> std::result::Result<SubmissionReconciliation, ProviderError> {
        let mut cursor = cursor.to_owned();
        let mut seen = std::collections::HashSet::new();
        let mut candidates = Vec::new();
        loop {
            let history = session
                .rpc()
                .event_log()
                .read(EventLogReadRequest {
                    agent_ids: None,
                    direction: None,
                    include_ephemeral: Some(false),
                    cursor: Some(cursor.clone()),
                    agent_scope: Some(EventsAgentScope::Primary),
                    types: Some(json!(["user.message"])),
                    max: Some(100),
                    wait_ms: Some(0),
                })
                .await
                .map_err(|_| {
                    ProviderError::Failure(forge_provider_api::ProviderFailure::new(
                        forge_provider_api::ProviderOperation::RecoverSession,
                        forge_provider_api::ProviderFailureKind::Transport,
                    ))
                })?;
            if history.cursor_status != EventsCursorStatus::Ok {
                return Err(ProviderError::Failure(
                    forge_provider_api::ProviderFailure::new(
                        forge_provider_api::ProviderOperation::RecoverSession,
                        forge_provider_api::ProviderFailureKind::Protocol,
                    ),
                ));
            }
            for event in history.events {
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
                return Ok(resolve_unclaimed_submissions(
                    &candidates,
                    known_receipts,
                    content_digest,
                ));
            }
            if history.cursor == cursor || !seen.insert(history.cursor.clone()) {
                return Err(ProviderError::Failure(
                    forge_provider_api::ProviderFailure::new(
                        forge_provider_api::ProviderOperation::RecoverSession,
                        forge_provider_api::ProviderFailureKind::Protocol,
                    ),
                ));
            }
            cursor = history.cursor;
        }
    }
    pub async fn rewind(
        &self,
        target: super::turn::CopilotRetraction,
    ) -> Result<(), ProviderError> {
        let result: Result<()> = async {
            let event_id = match target {
                super::turn::CopilotRetraction::Event(event_id) => event_id,
                super::turn::CopilotRetraction::History(mut cursor) => loop {
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
                            types: Some(json!(["user.message"])),
                            max: Some(1),
                            wait_ms: Some(0),
                        })
                        .await
                        .context("verify cancelled Copilot submission in native history")?;
                    anyhow::ensure!(
                        history.cursor_status == EventsCursorStatus::Ok,
                        "Copilot rewind history boundary expired"
                    );
                    if let Some(event) = history.events.into_iter().find(|event| {
                        event.agent_id.is_none() && event.event_type == "user.message"
                    }) {
                        break event.id;
                    }
                    if !history.has_more {
                        return Ok(());
                    }
                    anyhow::ensure!(
                        history.cursor != cursor,
                        "Copilot rewind history cursor did not advance"
                    );
                    cursor = history.cursor;
                },
                super::turn::CopilotRetraction::Unsent => unreachable!(),
            };
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
        result.map_err(|_| {
            native_failure(
                forge_provider_api::error::ProviderOperation::NativeTransport,
                forge_provider_api::error::ProviderFailureKind::Transport,
            )
        })
    }
    pub async fn compact(&self, id: String) -> Result<Compaction, ProviderError> {
        let result: Result<Compaction> = async {
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
        result.map_err(|_| {
            native_failure(
                forge_provider_api::error::ProviderOperation::NativeTransport,
                forge_provider_api::error::ProviderFailureKind::Transport,
            )
        })
    }
    pub async fn steer(
        &self,
        input: MessageInput,
        receipt: Option<Arc<dyn InputReceipt>>,
        scope: &forge_provider_api::InvocationScope,
        workspace: &str,
    ) -> Result<String, ProviderError> {
        self.ensure_healthy()?;
        let message = super::input::message(input).with_mode(DeliveryMode::Immediate);
        if let Some(receipt) = &receipt {
            let history = self.inner.rpc().event_log().tail().await.map_err(|_| {
                ProviderError::Failure(forge_provider_api::ProviderFailure::new(
                    forge_provider_api::ProviderOperation::AdmitInput,
                    forge_provider_api::ProviderFailureKind::Transport,
                ))
            })?;
            self.ensure_healthy()?;
            receipt
                .clone()
                .record_sending(Some(super::prompt::sending_recovery_token(
                    scope.owner.clone(),
                    receipt.id().clone(),
                    workspace.to_owned(),
                    self.id(),
                    history.cursor,
                    hex::encode(Sha256::digest(message.prompt.as_bytes())),
                )?))
                .await?;
        }
        self.ensure_healthy()?;
        let accepted = self
            .inner
            .send(message)
            .await
            .map_err(|_| ProviderError::DeliveryUncertain)?;
        if let Some(receipt) = receipt {
            let receipt_id = receipt.id().clone();
            receipt
                .record_accepted(Some(super::prompt::accepted_recovery_token(
                    receipt_id,
                    scope.owner.clone(),
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
            tokio::time::timeout(std::time::Duration::from_secs(30), async {
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
                forge_provider_api::error::ProviderOperation::NativeTransport,
                forge_provider_api::error::ProviderFailureKind::Transport,
            )
        })
    }
}

pub fn default_model_descriptor() -> ProviderModel {
    ProviderModel {
        service_tiers: vec![ServiceTier::Standard],
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

pub fn model_options(
    reasoning_effort: Option<String>,
    context_window: Option<&str>,
) -> SetModelOptions {
    let mut options = SetModelOptions::default().with_reasoning_summary(ReasoningSummary::Concise);
    if let Some(reasoning_effort) = reasoning_effort {
        options = options.with_reasoning_effort(reasoning_effort);
    }
    if let Some(context_window) = context_window {
        let context_tier = match context_window {
            "default" => ContextTier::Default,
            "long_context" => ContextTier::LongContext,
            _ => return options,
        };
        options = options.with_context_tier(context_tier);
    }
    options
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

    #[test]
    fn provides_an_auto_model_when_native_discovery_is_empty() {
        let model = default_model_descriptor();
        assert!(model.is_default);
        assert_eq!(model.id, "default");
        assert!(model.reasoning.is_empty());
        assert!(model.context_window.is_empty());
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
}
