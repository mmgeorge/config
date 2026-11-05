pub mod approval;
mod catalog;
pub(crate) use catalog::select_model_settings;
pub use compaction::{
    Compaction, CompactionCapability, CompactionContext, CompactionCost, CompactionResult,
    CompactionState, CompactionUsage,
};
pub use forge_provider_api::compaction;
pub type ProviderCompactionRequest =
    forge_provider_api::compaction::ProviderCompactionRequest<ProviderRequest>;
mod bridge;
mod session_opening;
pub(crate) use bridge::{ConfigureReceipt, ConfiguredBindingReceipt};
mod completion;
mod live_host_effect;
pub(crate) use live_host_effect::{LiveHostCommit, LiveHostEffect, LiveHostEffectSender};
pub mod events;
mod execution;
mod host_invocation;
pub(crate) mod lifecycle_error;
pub(crate) mod mcp;
pub mod mock_response;
pub(crate) mod operation;
mod prompt;
pub(crate) mod retry;
pub(crate) use completion::{CompletionSlot, HostEffectBatch};
pub(crate) mod session_execution;
mod steering;
pub mod submission;
pub use forge_provider_api::ServiceTier;
pub use forge_provider_api::submission::{DispatchObserver, DispatchTracker};
pub(crate) mod text_generation;
pub use catalog::{
    CatalogCapability, McpDefinition, McpStatus, McpToolDefinition, ProviderCatalogRequest,
    ProviderInput, ResourceScope, SkillDefinition,
};
pub use execution::{
    AgentId, EventScope, ExchangeId, ExecutionOwner, InvocationId, InvocationScope, MessageAddress,
    ProviderTurnId, SessionId, TaskId, ToolAddress, ToolId, TurnAddress, TurnUpdate,
};
pub use forge_provider_api::terminal;
pub use forge_provider_api::usage;
pub use steering::SteeringInput;
pub(crate) use steering::{ActiveSteering, SteerCommand, SteeringLane, SteeringTarget};
pub use text_generation::{BACKGROUND_SYSTEM_MESSAGE, TextGeneration};

mod contract;
#[cfg(test)]
pub(crate) mod test_support;
pub use contract::*;
pub use forge_provider_api::message;
pub use message::{MessageClassification, MessageId, MessageRole, MessageUpdate};
mod tool_status;
pub(crate) mod tool_timeout;
pub use tool_status::{
    TextUpdate, ToolDiagnostic, ToolInvocation, ToolOutcome, ToolStatus, ToolUpdate,
};
mod capability;
mod provider_trace;
pub use capability::{CapabilityManifest, UnsupportedCapability};

pub use forge_provider_api::execution::TurnSummary;
pub use forge_provider_api::model::*;
pub use forge_provider_api::tool::ToolActivityKind;

pub use forge_provider_api::error::ProviderError;
pub use forge_provider_api::event::*;

pub use forge_provider_api::PromptMode;

pub use forge_provider_api::completion::TurnMetrics;
