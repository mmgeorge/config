use super::{McpDefinition, McpStatus, Provider};
use anyhow::{Context, Result};
use forge_provider_api::catalog::{McpQuery, QueryContext};
use std::sync::Arc;
use std::time::Duration;
use tokio::task::JoinHandle;
use tokio::time::Instant;

/// Owns one session's discovery job independently from short picker requests.
#[derive(Default)]
pub(crate) struct McpDiscovery {
    servers: Vec<McpDefinition>,
    pending: Option<JoinHandle<Result<Vec<McpDefinition>>>>,
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
        if self.pending.as_ref().is_some_and(JoinHandle::is_finished) {
            let result = self
                .pending
                .take()
                .unwrap()
                .await
                .context("MCP discovery task failed")?;
            self.completed_at = Some(Instant::now());
            match result {
                Ok(servers) => self.servers = servers,
                Err(error) => {
                    if self.servers.is_empty() {
                        self.completed_at = None;
                        return Err(error);
                    }
                    for server in &mut self.servers {
                        if server.enabled
                            && !(server.status == McpStatus::Unavailable
                                && server.status_detail.is_some())
                        {
                            server.status = McpStatus::Failed;
                            server.status_detail = Some(
                                forge_provider_api::SanitizedMessage::new("MCP discovery failed")
                                    .expect("static diagnostic is nonempty"),
                            );
                        }
                    }
                }
            }
            return Ok(self.servers.clone());
        }
        if self.pending.is_none()
            && self
                .completed_at
                .is_none_or(|completed| completed.elapsed() > Duration::from_secs(5))
        {
            let configured = match configured {
                Some(configured) => configured,
                None => {
                    provider
                        .mcp_list(McpQuery::Configured(context.clone()))
                        .await?
                }
            };
            let previous = std::mem::replace(&mut self.servers, configured);
            if self.servers.is_empty() {
                self.completed_at = Some(Instant::now());
                return Ok(Vec::new());
            }
            for server in &mut self.servers {
                if server.enabled
                    && !matches!(
                        server.status,
                        McpStatus::Failed | McpStatus::NeedsAuthentication
                    )
                    && !(server.status == McpStatus::Unavailable && server.status_detail.is_some())
                {
                    if let Some(observed) = previous.iter().find(|observed| {
                        observed.resource_id == server.resource_id
                            && observed.name == server.name
                            && observed.enabled == server.enabled
                    }) {
                        server.status = observed.status;
                        server.status_detail.clone_from(&observed.status_detail);
                        server.tools.clone_from(&observed.tools);
                        server.tool_error.clone_from(&observed.tool_error);
                        server.token_count = observed.token_count;
                        server.token_estimated = observed.token_estimated;
                    } else {
                        server.status = McpStatus::Loading;
                    }
                }
            }
            let configured = self.servers.clone();
            self.pending = Some(tokio::spawn(async move {
                let observed = provider.mcp_list(McpQuery::Observed(context)).await?;
                Ok(merge_configured(configured, observed))
            }));
        }
        Ok(self.servers.clone())
    }
}
/// Merge runtime observations without replacing shared resource identity, selection, or preparation errors.
pub(crate) fn merge_configured(
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
            if !row.enabled || row.status == McpStatus::Unavailable {
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
                        forge_provider_api::SanitizedMessage::new(
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
