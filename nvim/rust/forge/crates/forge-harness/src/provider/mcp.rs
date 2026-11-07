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

#[cfg(test)]
mod test {
    use super::*;
    use crate::provider::ProviderDescriptor;
    use forge_provider_api::catalog::{
        McpDefinition as NativeMcpDefinition, McpStatus as NativeMcpStatus, McpTransportKind,
    };
    use std::sync::Arc;
    use std::sync::atomic::{AtomicUsize, Ordering};
    use tokio::sync::Notify;

    fn descriptor() -> ProviderDescriptor {
        ProviderDescriptor {
            kind: forge_providers::registry::nonselectable_id()
                .expect("fixture identity is available"),
            label: "Normalized catalog fixture".into(),
            capability: Default::default(),
        }
    }

    fn native_server(name: &str, status: NativeMcpStatus) -> NativeMcpDefinition {
        NativeMcpDefinition {
            resource_id: None,
            scope: None,
            source_path: None,
            name: name.into(),
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

    fn bridge(
        release: Arc<Notify>,
        calls: Arc<AtomicUsize>,
        unavailable_reason: Option<&'static str>,
    ) -> Arc<Provider> {
        let fixture = crate::provider::test_support::EventFixtureProvider::new(
            descriptor(),
            |request, _sink, start, _cancelled| async move {
                start.observed(1).await?;
                Ok(crate::provider::test_support::native_result(
                    &request,
                    forge_provider_api::turn::TurnOutcome::Completed {
                        stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
                    },
                ))
            },
        )
        .with_mcp_list(move |query| {
            let release = release.clone();
            let calls = calls.clone();
            async move {
                match query {
                    McpQuery::Configured(_) => {
                        let mut working = native_server("working", NativeMcpStatus::Unavailable);
                        working.status_detail = unavailable_reason.map(|reason| {
                            forge_provider_api::diagnostic::SanitizedMessage::new(reason)
                                .expect("fixture message is nonempty")
                        });
                        Ok(vec![
                            working,
                            native_server("unwired", NativeMcpStatus::Unavailable),
                        ])
                    }
                    McpQuery::Observed(_) => {
                        calls.fetch_add(1, Ordering::SeqCst);
                        release.notified().await;
                        let mut failed = native_server("unwired", NativeMcpStatus::Failed);
                        failed.status_detail = Some(
                            forge_provider_api::diagnostic::SanitizedMessage::new(
                                "connection could not be initialized",
                            )
                            .expect("fixture message is nonempty"),
                        );
                        Ok(vec![
                            native_server("working", NativeMcpStatus::Connected),
                            failed,
                        ])
                    }
                }
            }
        });
        let session = crate::provider::test_support::ProviderTestSession::from_native(fixture)
            .expect("normalized fixture bridges successfully");
        session.provider_handle()
    }

    fn request() -> QueryContext {
        QueryContext::Workspace(forge_provider_api::catalog::WorkspaceContext {
            workspace: "D:/work".into(),
        })
    }

    #[tokio::test]
    async fn closing_discovery_cancels_the_retained_provider_request() {
        let provider = bridge(Arc::new(Notify::new()), Arc::new(AtomicUsize::new(0)), None);
        let mut discovery = McpDiscovery::default();
        discovery.snapshot(provider, request(), None).await.unwrap();
        tokio::task::yield_now().await;
        let pending = discovery.pending.as_ref().unwrap().abort_handle();
        drop(discovery);
        tokio::task::yield_now().await;
        assert!(pending.is_finished());
    }

    #[tokio::test]
    async fn known_credential_failure_stays_unavailable_while_other_servers_load() {
        let provider = bridge(
            Arc::new(Notify::new()),
            Arc::new(AtomicUsize::new(0)),
            Some("credential reference is unset"),
        );
        let mut discovery = McpDiscovery::default();
        let rows = discovery.snapshot(provider, request(), None).await.unwrap();
        assert_eq!(rows[0].status, McpStatus::Unavailable);
        assert_eq!(
            rows[0]
                .status_detail
                .as_ref()
                .map(|message| message.as_str()),
            Some("credential reference is unset")
        );
        assert_eq!(rows[1].status, McpStatus::Loading);
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

#[cfg(test)]
mod tests {}
