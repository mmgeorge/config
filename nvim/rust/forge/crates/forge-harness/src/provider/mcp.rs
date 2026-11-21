use super::{McpDefinition, McpStatus, Provider, ProviderError};
use anyhow::{Context, Result};
use forge_provider_api::SanitizedMessage;
use forge_provider_api::catalog::{McpQuery, QueryContext};
use std::sync::Arc;
use std::time::Duration;
use tokio::task::JoinHandle;
use tokio::time::Instant;

/// Owns one session's discovery job independently from short picker requests.
#[derive(Default)]
pub(crate) struct McpDiscovery {
    context: Option<QueryContext>,
    configured: Vec<McpDefinition>,
    servers: Vec<McpDefinition>,
    pending: Option<JoinHandle<Result<Vec<McpDefinition>, ProviderError>>>,
    completed_at: Option<Instant>,
}

impl Drop for McpDiscovery {
    /// Cancel discovery when its session is collected or its configuration changes.
    fn drop(&mut self) {
        if let Some(pending) = self.pending.take() {
            pending.abort();
        }
    }
}

impl McpDiscovery {
    /// Return configured rows while one retained job resolves startup and tool catalogs.
    pub(crate) async fn snapshot(
        &mut self,
        provider: Arc<Provider>,
        context: QueryContext,
        configured: Option<Vec<McpDefinition>>,
    ) -> Result<Vec<McpDefinition>> {
        // A workspace observation cannot establish readiness for an opened binding.
        if self.context.as_ref() != Some(&context) {
            *self = Self::default();
            self.context = Some(context.clone());
        }
        let integration = provider.descriptor().capability.integration;
        if !integration.mcp_runtime_status && !integration.mcp_tool_catalog {
            let servers = match configured {
                Some(servers) => servers,
                None => provider.mcp_list(McpQuery::Configured(context)).await?,
            };
            return Ok(without_observation(servers));
        }
        let shared_configuration = configured.is_some();
        if let Some(configured) = configured {
            self.configure(configured);
        }

        // Merge results into the configuration that admitted the query.
        if self.pending.as_ref().is_some_and(JoinHandle::is_finished) {
            let result = self
                .pending
                .take()
                .unwrap()
                .await
                .context("MCP discovery task failed")?;
            self.completed_at = Some(Instant::now());
            self.servers = match result {
                Ok(observed) => merge_configured(self.configured.clone(), observed),
                Err(ProviderError::Unsupported(_)) => without_observation(self.configured.clone()),
                Err(_) => status_without_observation(
                    self.configured.clone(),
                    McpStatus::Failed,
                    "MCP discovery failed",
                ),
            };
            return Ok(self.servers.clone());
        }

        // Refresh runtime observations without feeding their status back into preparation.
        if self.pending.is_none()
            && self
                .completed_at
                .is_none_or(|completed| completed.elapsed() > Duration::from_secs(5))
        {
            if !shared_configuration {
                let configured = provider
                    .mcp_list(McpQuery::Configured(context.clone()))
                    .await?;
                self.configure(configured);
            }
            if !self.configured.iter().any(can_observe) {
                self.completed_at = Some(Instant::now());
                return Ok(self.servers.clone());
            }
            self.pending = Some(tokio::spawn(
                async move { provider.mcp_list(McpQuery::Observed(context)).await },
            ));
        }
        Ok(self.servers.clone())
    }

    /// Invalidate observations when their configured rows change within the same context.
    fn configure(&mut self, configured: Vec<McpDefinition>) {
        if self.configured == configured {
            return;
        }
        if let Some(pending) = self.pending.take() {
            pending.abort();
        }
        self.completed_at = None;
        self.servers = configured.clone();
        for server in self.servers.iter_mut().filter(|server| can_observe(server)) {
            server.status = McpStatus::Loading;
        }
        self.configured = configured;
    }
}

/// Keep disabled rows and preparation diagnostics outside runtime discovery.
fn can_observe(row: &McpDefinition) -> bool {
    row.enabled
        && match row.status {
            McpStatus::Disabled | McpStatus::Failed | McpStatus::NeedsAuthentication => false,
            McpStatus::Unavailable => row.status_detail.is_none(),
            McpStatus::Connected | McpStatus::Loading => true,
        }
}

/// Describe configured servers when the queried provider cannot observe readiness or tools.
fn without_observation(configured: Vec<McpDefinition>) -> Vec<McpDefinition> {
    status_without_observation(
        configured,
        McpStatus::Unavailable,
        "The provider does not report MCP readiness here",
    )
}

/// Overlay the query result while preserving per-server preparation evidence.
fn status_without_observation(
    mut configured: Vec<McpDefinition>,
    status: McpStatus,
    detail: &str,
) -> Vec<McpDefinition> {
    let detail = SanitizedMessage::new(detail).expect("static diagnostic is nonempty");
    for row in configured.iter_mut().filter(|row| can_observe(row)) {
        row.status = status;
        row.status_detail = Some(detail.clone());
    }
    configured
}

/// Merge runtime observations without replacing shared resource identity, selection, or preparation errors.
fn merge_configured(
    configured: Vec<McpDefinition>,
    observed: Vec<McpDefinition>,
) -> Vec<McpDefinition> {
    let mut observed: std::collections::HashMap<_, _> = observed
        .into_iter()
        .map(|row| (row.name.clone(), row))
        .collect();
    configured
        .into_iter()
        .map(|mut row| {
            if !can_observe(&row) {
                return row;
            }
            match observed.remove(&row.name) {
                Some(live) => {
                    row.status = live.status;
                    row.status_detail = live.status_detail;
                    row.token_count = live.token_count;
                    row.token_estimated = live.token_estimated;
                    row.tools = live.tools;
                    row.tool_error = live.tool_error;
                }
                None => {
                    row.status = McpStatus::Unavailable;
                    row.status_detail = Some(
                        SanitizedMessage::new(
                            "Provider did not report this configured MCP server",
                        )
                        .expect("static diagnostic is nonempty"),
                    );
                }
            }
            row
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::provider::test_support::{EventFixtureProvider, ProviderTestSession};
    use forge_provider_api::catalog::{McpTransportKind, WorkspaceContext};

    /// Keep skipped, disabled, and unobservable servers honest before and after native open.
    #[tokio::test]
    async fn mcp_configured_only_refresh_preserves_current_preparation() {
        let provider_id = forge_providers::registry::select("antigravity").unwrap();
        let descriptor = forge_providers::registry::descriptor(&provider_id).unwrap();
        let native = EventFixtureProvider::new(descriptor, |_, _, _, _| async {
            Err(ProviderError::Cancelled)
        });
        let session = ProviderTestSession::from_native(native).unwrap();
        let provider = session.provider_handle();
        let workspace = QueryContext::Workspace(WorkspaceContext {
            workspace: "workspace".into(),
        });
        let mut discovery = McpDiscovery::default();
        let missing_command = SanitizedMessage::new("Executable missing").unwrap();
        let missing = McpDefinition {
            status_detail: Some(missing_command.clone()),
            ..server("repairable", McpStatus::Unavailable)
        };
        let selected = server("selected", McpStatus::Loading);
        let disabled = McpDefinition {
            enabled: false,
            ..server("disabled", McpStatus::Disabled)
        };

        let snapshot = discovery
            .snapshot(
                provider.clone(),
                workspace.clone(),
                Some(vec![missing, selected.clone(), disabled.clone()]),
            )
            .await
            .unwrap();
        assert_eq!(snapshot[0].status_detail, Some(missing_command));
        assert_eq!(snapshot[1].status, McpStatus::Unavailable);
        assert_eq!(snapshot[2], disabled);
        assert!(discovery.pending.is_none());

        let repaired = server("repairable", McpStatus::Loading);
        let refreshed = discovery
            .snapshot(
                provider.clone(),
                workspace,
                Some(vec![repaired.clone(), selected.clone()]),
            )
            .await
            .unwrap();
        assert_eq!(refreshed[0].status, McpStatus::Unavailable);
        assert_eq!(refreshed[0].status_detail, refreshed[1].status_detail);

        let bound = QueryContext::Binding(forge_provider_api::BindingId::new("binding").unwrap());
        let snapshot = discovery
            .snapshot(provider, bound, Some(vec![repaired, selected]))
            .await
            .unwrap();
        assert_eq!(snapshot, refreshed);
        assert!(discovery.pending.is_none());
    }

    /// Continue to accept readiness updates when a configured row carries no observation.
    #[test]
    fn mcp_configured_unavailable_without_detail_accepts_readiness() {
        let configured = server("selected", McpStatus::Unavailable);
        let observed = server("selected", McpStatus::Connected);
        let snapshot = merge_configured(vec![configured], vec![observed]);
        assert_eq!(snapshot[0].status, McpStatus::Connected);
    }

    /// Make a normalized configured or observed row for the discovery tests.
    fn server(name: &str, status: McpStatus) -> McpDefinition {
        McpDefinition {
            resource_id: Some(name.to_owned()),
            scope: None,
            source_path: None,
            name: name.to_owned(),
            transport: McpTransportKind::Stdio,
            enabled: true,
            status,
            status_detail: None,
            token_count: None,
            token_estimated: false,
            tools: Vec::new(),
            tool_error: None,
        }
    }
}
