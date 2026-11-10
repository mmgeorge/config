pub mod approval;
mod approval_view;
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
mod context_delivery;
mod session_opening;
pub(crate) use bridge::{ConfigureReceipt, ConfiguredBindingReceipt, NativeBindingStatus};
mod completion;
mod live_host_effect;
pub(crate) use live_host_effect::{LiveHostCommit, LiveHostEffect, LiveHostEffectSender};
pub mod events;
pub use events::{HarnessEvent, HarnessEventSink, HarnessEventStream, NativeEventSource};
mod host_event;
pub use host_event::*;
mod ownership;
pub(crate) use ownership::ObservedAgentAssociation;
mod execution;
mod host_invocation;
pub(crate) mod lifecycle_error;
pub(crate) mod mcp;
pub mod mock_response;
pub(crate) mod operation;
mod prompt;
pub(crate) use prompt::TaskInstructions;
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
mod thread_owner;
pub use execution::{
    AgentId, EventScope, ExchangeId, InvocationId, InvocationScope, ScopeOwner, SessionId, TaskId,
    ThreadOwner, ThreadOwnerError, ToolAddress, TurnAddress, TurnEvent,
};
pub use forge_provider_api::terminal;
pub mod terminal_status;
pub use forge_provider_api::usage;
pub use steering::SteeringInput;
pub(crate) use steering::{ActiveSteering, SteerCommand, SteeringLane, SteeringTarget};
pub use text_generation::{BACKGROUND_SYSTEM_MESSAGE, TextGeneration};

mod contract;
#[cfg(test)]
pub(crate) mod test_support;
pub use contract::*;
pub use forge_provider_api::message;
pub use forge_provider_api::testing::TestMessageContent;
pub use message::MessageClassification;
mod tool_status;
pub(crate) mod tool_timeout;
pub use tool_status::{TextUpdate, ToolDiagnostic, ToolOutcome, ToolStatus};
mod capability;
pub mod capability_menu;
mod provider_trace;
pub use capability::{CapabilityManifest, UnsupportedCapability};

pub use forge_provider_api::model::*;

pub use forge_provider_api::error::ProviderError;
pub use forge_provider_api::event::*;

mod prompt_mode;
pub use prompt_mode::PromptMode;

pub use forge_provider_api::completion::TurnMetrics;
