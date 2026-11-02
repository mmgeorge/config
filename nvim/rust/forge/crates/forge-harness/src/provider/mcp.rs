use super::{McpDefinition, McpStatus, Provider, ProviderCatalogRequest};
use anyhow::{Context, Result};
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
        request: ProviderCatalogRequest,
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
                        if server.enabled {
                            server.status = McpStatus::Failed;
                            server.status_detail = Some(format!("{error:#}"));
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
            let configured = tokio::time::timeout(
                Duration::from_secs(10),
                provider.mcp_configuration(request.clone()),
            )
            .await
            .context("MCP configuration discovery timed out")??;
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
            self.pending = Some(tokio::spawn(async move {
                tokio::time::timeout(Duration::from_secs(120), provider.mcp_list(request))
                    .await
                    .context("MCP startup discovery timed out after 120 seconds")?
            }));
        }
        Ok(self.servers.clone())
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::provider::{
        ProviderCapability, ProviderDescriptor, ProviderEventSink, ProviderKind, ProviderRequest,
    };
    use std::sync::atomic::{AtomicUsize, Ordering};
    use tokio::sync::Notify;

    struct DiscoveryProvider {
        fixture_events: std::sync::Arc<crate::provider::test_support::SessionEvents>,
        release: Notify,
        calls: AtomicUsize,
        unavailable_reason: Option<&'static str>,
    }

    #[async_trait::async_trait]
    impl crate::provider::ProviderAdapter for DiscoveryProvider {
        type Host = crate::provider::HarnessProviderHost;
        async fn shutdown(&self) -> Result<()> {
            Ok(())
        }

        async fn cleanup_execution(&self, _session_id: &str) -> Result<()> {
            anyhow::bail!("provider cannot confirm execution cleanup")
        }

        async fn cancel_session(&self, _session_id: &str) -> Result<()> {
            Ok(())
        }

        async fn has_active_turn(&self, _session_id: &str) -> bool {
            false
        }

        async fn active_session_id_for(
            &self,
            _session_id: &str,
        ) -> Result<Option<forge_provider_api::ProviderSessionId>> {
            Ok(None)
        }

        fn descriptor(&self) -> ProviderDescriptor {
            ProviderDescriptor {
                kind: ProviderKind::Mock,
                label: "Discovery test".into(),
                capability: ProviderCapability::default(),
            }
        }

        async fn session_events(
            &self,
            owner: crate::provider::ExecutionOwner,
        ) -> std::result::Result<
            crate::provider::events::ProviderEventStream,
            crate::provider::ProviderError,
        > {
            let stream = self.fixture_events.subscribe(owner.clone())?;
            Ok(stream)
        }

        async fn session_prompt(
            self: std::sync::Arc<Self>,
            request: crate::provider::ProviderRequest,
            scope: crate::provider::InvocationScope,
        ) -> std::result::Result<(), crate::provider::ProviderError> {
            let events = self.fixture_events.clone();
            events
                .submit(
                    request,
                    scope,
                    crate::provider::TerminalBoundary::LocalFixture,
                    move |request, sink| async move { self.run_native(request, sink).await },
                )
                .await
        }

        async fn mcp_configuration(&self, _: ProviderCatalogRequest) -> Result<Vec<McpDefinition>> {
            let mut working = server("working", McpStatus::Unavailable);
            working.status_detail = self.unavailable_reason.map(str::to_owned);
            Ok(vec![working, server("unwired", McpStatus::Unavailable)])
        }

        async fn mcp_list(&self, _: ProviderCatalogRequest) -> Result<Vec<McpDefinition>> {
            self.calls.fetch_add(1, Ordering::SeqCst);
            self.release.notified().await;
            let mut failed = server("unwired", McpStatus::Failed);
            failed.status_detail = Some("initialize connection closed".into());
            Ok(vec![server("working", McpStatus::Connected), failed])
        }
    }

    impl DiscoveryProvider {
        async fn run_native(
            &self,
            _: ProviderRequest,
            _: ProviderEventSink,
        ) -> Result<crate::provider::ProviderCompletion, crate::provider::ProviderError> {
            Err(anyhow::anyhow!("catalog discovery must not prompt").into())
        }
    }

    fn server(name: &str, status: McpStatus) -> McpDefinition {
        McpDefinition {
            scope: None,
            source_path: None,
            resource_id: None,
            name: name.into(),
            transport: "stdio".into(),
            enabled: true,
            status,
            status_detail: None,
            token_count: None,
            token_estimated: false,
            tools: Vec::new(),
            tool_error: None,
        }
    }

    fn request() -> ProviderCatalogRequest {
        ProviderCatalogRequest {
            execution_owner: None,
            harness_session_id: "test".into(),
            workspace: ".".into(),
            access: Default::default(),
            approval_mode: crate::session::ApprovalMode::Ask,
            provider_session_id: None,
        }
    }

    #[tokio::test]
    async fn closing_discovery_cancels_the_retained_provider_request() {
        let provider = Arc::new(DiscoveryProvider {
            fixture_events: Default::default(),
            release: Notify::new(),
            calls: AtomicUsize::new(0),
            unavailable_reason: None,
        });
        let mut discovery = McpDiscovery::default();
        discovery.snapshot(provider, request()).await.unwrap();
        tokio::task::yield_now().await;
        let pending = discovery.pending.as_ref().unwrap().abort_handle();
        drop(discovery);
        tokio::task::yield_now().await;
        assert!(pending.is_finished());
    }

    #[tokio::test]
    async fn known_credential_failure_stays_unavailable_while_other_servers_load() {
        let provider = Arc::new(DiscoveryProvider {
            fixture_events: Default::default(),
            release: Notify::new(),
            calls: AtomicUsize::new(0),
            unavailable_reason: Some("credential reference is unset"),
        });
        let mut discovery = McpDiscovery::default();
        let rows = discovery.snapshot(provider, request()).await.unwrap();
        assert_eq!(rows[0].status, McpStatus::Unavailable);
        assert_eq!(
            rows[0].status_detail.as_deref(),
            Some("credential reference is unset")
        );
        assert_eq!(rows[1].status, McpStatus::Loading);
    }

    #[tokio::test]
    async fn cold_requests_share_discovery_and_preserve_individual_server_failures() {
        let provider = Arc::new(DiscoveryProvider {
            fixture_events: Default::default(),
            release: Notify::new(),
            calls: AtomicUsize::new(0),
            unavailable_reason: None,
        });
        let mut discovery = McpDiscovery::default();
        for _ in 0..3 {
            let rows = discovery
                .snapshot(provider.clone(), request())
                .await
                .unwrap();
            assert_eq!(rows.len(), 2);
            assert!(rows.iter().all(|row| row.status == McpStatus::Loading));
        }
        tokio::task::yield_now().await;
        assert_eq!(provider.calls.load(Ordering::SeqCst), 1);
        provider.release.notify_one();
        tokio::task::yield_now().await;
        let rows = discovery
            .snapshot(provider.clone(), request())
            .await
            .unwrap();
        assert_eq!(rows[0].status, McpStatus::Connected);
        assert_eq!(rows[1].status, McpStatus::Failed);
        assert_eq!(
            rows[1].status_detail.as_deref(),
            Some("initialize connection closed")
        );
        assert_eq!(
            discovery
                .snapshot(provider.clone(), request())
                .await
                .unwrap(),
            rows
        );
        assert_eq!(provider.calls.load(Ordering::SeqCst), 1);
        discovery.completed_at = Some(Instant::now() - Duration::from_secs(6));
        assert_eq!(
            discovery
                .snapshot(provider.clone(), request())
                .await
                .unwrap(),
            rows
        );
        tokio::task::yield_now().await;
        assert_eq!(provider.calls.load(Ordering::SeqCst), 2);
        provider.release.notify_one();
        tokio::task::yield_now().await;
        assert_eq!(discovery.snapshot(provider, request()).await.unwrap(), rows);
    }
}
