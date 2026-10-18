pub mod approval;
mod acp;
mod catalog;
pub(crate) use catalog::select_model_settings;
mod compaction;
pub use compaction::{BackendCompactionRequest, Compaction, CompactionCapability, CompactionContext, CompactionCost, CompactionResult, CompactionState, CompactionTokenCost, CompactionUsage};
pub(crate) mod mcp;
pub mod codex;
pub mod copilot;
pub mod events;
mod execution;
mod prompt;
mod steering;
mod service_tier;
pub use service_tier::ServiceTier;
pub(crate) mod text_generation;
pub use text_generation::{BACKGROUND_SYSTEM_MESSAGE, TextGeneration};
pub mod terminal;
pub mod usage;
pub use catalog::{
    BackendCatalogRequest, BackendInput, CatalogCapability, CatalogMutation, McpDefinition,
    McpStatus, McpToolDefinition, ResourceScope, SkillDefinition,
};
pub use execution::{ExecutionOwner, ProviderAddress, TurnBoundary};
pub use steering::{SteerTarget, SteeringInput};
pub(crate) use steering::{ActiveSteering, SteerCommand, SteeringLane};

mod backend;
pub use backend::*;
mod capability;
pub use capability::{CapabilityManifest, UnsupportedCapability};
