use crate::ProviderError;
use async_trait::async_trait;

/// Host-owned effects needed to decide whether a native turn may retry or finish.
pub struct HostTurnStatus {
    pub replay_safe: bool,
    pub control_handoff: bool,
    pub cancelled: bool,
    pub error: Option<String>,
}

/// Keep tool reduction and workflow authority outside the native provider loop.
#[async_trait]
pub trait TurnControl: Send + Sync {
    async fn before_send(&self) -> Result<(), ProviderError>;
    async fn receive(&self) -> Result<(), ProviderError>;
    async fn close(&self) -> Result<(), ProviderError>;
    async fn status(&self) -> Result<HostTurnStatus, ProviderError>;
    fn tool_finished(&self, name: Option<&str>) -> bool;
}
