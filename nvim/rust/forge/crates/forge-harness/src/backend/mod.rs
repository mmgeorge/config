mod acp;
pub mod approval;
mod catalog;
mod permission_input;
pub(crate) use catalog::select_model_settings;
mod compaction;
pub use compaction::{
    BackendCompactionRequest, Compaction, CompactionCapability, CompactionContext, CompactionCost,
    CompactionResult, CompactionState, CompactionTokenCost, CompactionUsage,
};
pub mod codex;
pub mod copilot;
pub mod events;
mod execution;
pub(crate) mod mcp;
pub mod mock_response;
mod prompt;
pub(crate) mod retry;
mod service_tier;
mod steering;
pub mod submission;
pub use service_tier::ServiceTier;
pub(crate) mod text_generation;
pub use text_generation::{BACKGROUND_SYSTEM_MESSAGE, TextGeneration};
pub mod terminal;
pub mod usage;
pub use catalog::{
    BackendCatalogRequest, BackendInput, CatalogCapability, CatalogMutation, McpDefinition,
    McpStatus, McpToolDefinition, ResourceScope, SkillDefinition,
};
pub use execution::{
    AgentId, EventScope, ExchangeId, ExecutionOwner, InvocationId, InvocationScope, MessageAddress,
    ProviderTurnId, SessionId, TaskId, ToolAddress, ToolId, TurnAddress, TurnUpdate,
};
pub(crate) use steering::{ActiveSteering, SteerCommand, SteeringLane};
pub use steering::{SteerTarget, SteeringInput};

mod contract;
#[cfg(test)]
pub(crate) mod test_support;
pub use contract::*;
mod message;
pub use message::{MessageClassification, MessageId, MessageRole, MessageUpdate};
mod tool_status;
pub use tool_status::{
    TextUpdate, ToolDiagnostic, ToolInvocation, ToolOutcome, ToolStatus, ToolUpdate,
};
mod capability;
pub use capability::{CapabilityManifest, UnsupportedCapability};
