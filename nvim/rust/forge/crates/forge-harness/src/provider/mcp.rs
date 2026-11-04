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
                None => tokio::time::timeout(
                    Duration::from_secs(10),
                    provider.mcp_list(McpQuery::Configured(context.clone())),
                )
                .await
                .context("MCP configuration discovery timed out")??,
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
                let observed = tokio::time::timeout(
                    Duration::from_secs(120),
                    provider.mcp_list(McpQuery::Observed(context)),
                )
                .await
                .context("MCP startup discovery timed out after 120 seconds")??;
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
    use forge_provider_api::{
        ProviderError,
        catalog::{
            McpDefinition as NativeMcpDefinition, McpStatus as NativeMcpStatus, McpTransportKind,
        },
        events::ProviderEventSink,
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
            |_request, _sink: ProviderEventSink, _cancelled| async { Ok::<(), ProviderError>(()) },
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

    #[tokio::test]
    async fn cold_requests_share_discovery_and_preserve_individual_server_failures() {
        let calls = Arc::new(AtomicUsize::new(0));
        let release = Arc::new(Notify::new());
        let provider = bridge(release.clone(), calls.clone(), None);
        let mut discovery = McpDiscovery::default();
        for _ in 0..3 {
            let rows = discovery
                .snapshot(provider.clone(), request(), None)
                .await
                .unwrap();
            assert_eq!(rows.len(), 2);
            assert!(rows.iter().all(|row| row.status == McpStatus::Loading));
        }
        tokio::task::yield_now().await;
        assert_eq!(calls.load(Ordering::SeqCst), 1);
        release.notify_one();
        tokio::task::yield_now().await;
        let rows = discovery
            .snapshot(provider.clone(), request(), None)
            .await
            .unwrap();
        assert_eq!(rows[0].status, McpStatus::Connected);
        assert_eq!(rows[1].status, McpStatus::Failed);
        assert_eq!(
            rows[1]
                .status_detail
                .as_ref()
                .map(|message| message.as_str()),
            Some("connection could not be initialized")
        );
        assert_eq!(
            discovery
                .snapshot(provider.clone(), request(), None)
                .await
                .unwrap(),
            rows
        );
        assert_eq!(calls.load(Ordering::SeqCst), 1);
        discovery.completed_at = Some(Instant::now() - Duration::from_secs(6));
        assert_eq!(
            discovery
                .snapshot(provider.clone(), request(), None)
                .await
                .unwrap(),
            rows
        );
        tokio::task::yield_now().await;
        assert_eq!(calls.load(Ordering::SeqCst), 2);
        release.notify_one();
        tokio::task::yield_now().await;
        assert_eq!(
            discovery.snapshot(provider, request(), None).await.unwrap(),
            rows
        );
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
mod tests {
    use super::*;

    fn server(name: &str, enabled: bool, status: McpStatus) -> McpDefinition {
        McpDefinition {
            resource_id: Some(format!("shared-{name}")),
            scope: Some(forge_provider_api::catalog::ResourceScope::Workspace),
            source_path: Some("shared-config".into()),
            name: name.into(),
            transport: forge_provider_api::catalog::McpTransportKind::Stdio,
            enabled,
            status,
            status_detail: None,
            token_count: None,
            token_estimated: false,
            tools: Vec::new(),
            tool_error: None,
        }
    }

    /// Keep host selection and credential failures authoritative while importing native readiness.
    #[tokio::test]
    async fn host_configured_rows_bypass_native_definitions_and_preserve_metadata() -> Result<()> {
        let descriptor = forge_providers::registry::descriptor(
            &forge_providers::registry::nonselectable_id().expect("fixture identity"),
        )?;
        let native =
            crate::provider::test_support::EventFixtureProvider::new(descriptor, |_, _, _| async {
                Ok(())
            })
            .with_mcp_list(|query| async move {
                match query {
                    McpQuery::Configured(_) => {
                        panic!("host-owned definitions must not query native configuration")
                    }
                    McpQuery::Observed(_) => Ok(vec![
                        server("ready", true, McpStatus::Connected),
                        server("disabled", true, McpStatus::Connected),
                        server("unselected", true, McpStatus::Connected),
                    ]),
                }
            });
        let fixture = crate::provider::test_support::ProviderTestSession::from_native(native)?;
        let provider = fixture.provider_handle();
        let mut missing = server("missing", true, McpStatus::Unavailable);
        missing.status_detail = Some(forge_provider_api::SanitizedMessage::new(
            "Required credential is missing",
        )?);
        let configured = vec![
            server("ready", true, McpStatus::Loading),
            server("disabled", false, McpStatus::Disabled),
            missing.clone(),
        ];
        let context = QueryContext::Workspace(forge_provider_api::catalog::WorkspaceContext {
            workspace: "D:/work".into(),
        });
        let mut discovery = McpDiscovery::default();
        assert_eq!(
            discovery
                .snapshot(provider.clone(), context.clone(), Some(configured.clone()))
                .await?,
            configured
        );
        while !discovery
            .pending
            .as_ref()
            .expect("observation job is retained")
            .is_finished()
        {
            tokio::task::yield_now().await;
        }
        let observed = discovery
            .snapshot(provider, context, Some(configured))
            .await?;
        assert_eq!(observed.len(), 3);
        assert_eq!(observed[0].status, McpStatus::Connected);
        assert_eq!(observed[0].resource_id.as_deref(), Some("shared-ready"));
        assert_eq!(observed[1].status, McpStatus::Disabled);
        assert_eq!(observed[2], missing);
        Ok(())
    }

    /// Keep configuration authoritative while importing only runtime readiness and tools.
    #[test]
    fn shared_catalog_retains_selection_identity_and_preparation_failures() {
        let mut unavailable = server("missing", true, McpStatus::Unavailable);
        unavailable.status_detail =
            Some(forge_provider_api::SanitizedMessage::new("Executable is missing").unwrap());
        let result = merge_configured(
            vec![
                server("active", true, McpStatus::Loading),
                server("disabled", false, McpStatus::Disabled),
                unavailable,
                server("unreported", true, McpStatus::Loading),
            ],
            vec![
                server("active", true, McpStatus::Connected),
                server("disabled", true, McpStatus::Connected),
                server("missing", true, McpStatus::Connected),
                server("unselected", true, McpStatus::Connected),
            ],
        );
        assert_eq!(result.len(), 4);
        assert_eq!(result[0].status, McpStatus::Connected);
        assert_eq!(result[0].resource_id.as_deref(), Some("shared-active"));
        assert!(!result[1].enabled);
        assert_eq!(result[1].status, McpStatus::Disabled);
        assert_eq!(
            result[2].status_detail.as_ref().unwrap().as_str(),
            "Executable is missing"
        );
        assert_eq!(result[3].status, McpStatus::Unavailable);
    }
}
