use crate::ProviderError;
use async_trait::async_trait;

/// Host-owned effects needed to decide whether a native turn may retry or finish.
pub struct HostTurnStatus {
    pub replay_safe: bool,
    pub control_handoff: bool,
    pub cancelled: bool,
    pub error: Option<ProviderError>,
}

/// Keep tool reduction and workflow authority outside the native provider loop.
#[async_trait]
pub trait TurnControl: Send + Sync {
    async fn before_send(&self, scope: &crate::InvocationScope) -> Result<(), ProviderError>;
    async fn receive(&self, scope: &crate::InvocationScope) -> Result<(), ProviderError>;
    async fn close(&self, scope: &crate::InvocationScope) -> Result<(), ProviderError>;
    async fn status(&self, scope: &crate::InvocationScope)
    -> Result<HostTurnStatus, ProviderError>;
    fn tool_finished(&self, scope: &crate::InvocationScope, name: Option<&str>) -> bool;
}
