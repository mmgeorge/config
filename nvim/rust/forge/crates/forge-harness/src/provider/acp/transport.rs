use crate::provider::ProviderError;
use anyhow::Result;
use forge_providers::acp::AcpHandler;
use forge_providers::acp::{AcpConnection, AcpLaunch};
use serde_json::Value;
use std::sync::Arc;

/// Binds native ACP transport to durable Harness submission policy.
pub(super) struct AcpTransport {
    connection: AcpConnection,
}

impl AcpTransport {
    /// Probe and validate the selected adapter before setup reports it as supported.
    pub(super) async fn initialize(
        &self,
        kind: crate::provider::ProviderKind,
        runtime: std::path::PathBuf,
        parameter: Value,
    ) -> std::result::Result<Value, ProviderError> {
        use forge_providers::acp::conversation::{
            initialization_failure, runtime_requirement, validate_initialization,
        };
        let requirement = runtime_requirement(kind, runtime)?;
        let initialized = self
            .connection
            .request("initialize", parameter)
            .await
            .map_err(|source| initialization_failure(&requirement, source))?;
        validate_initialization(&requirement, &initialized)?;
        Ok(initialized)
    }

    /// Launch a provider using its resolved host configuration.
    pub(super) async fn launch(
        runtime: &super::runtime::RuntimeStatus,
        workspace: &str,
        handler: Arc<dyn AcpHandler>,
        profile: Option<&std::path::Path>,
    ) -> Result<Self, ProviderError> {
        let connection = AcpConnection::launch(
            AcpLaunch {
                command: &runtime.command,
                environment: &runtime.environment,
                workspace,
                profile,
            },
            handler,
        )
        .await
        .map_err(ProviderError::from)?;
        Ok(Self { connection })
    }

    /// Send one native request and preserve its failure category for Harness.
    pub(super) async fn request(
        &self,
        method: &str,
        parameter: Value,
    ) -> Result<Value, ProviderError> {
        self.connection
            .request(method, parameter)
            .await
            .map_err(ProviderError::from)
    }

    /// Collect native process ownership before acknowledging shutdown.
    pub(super) async fn shutdown(&self) -> Result<(), ProviderError> {
        self.connection
            .shutdown()
            .await
            .map_err(ProviderError::from)
    }
}
