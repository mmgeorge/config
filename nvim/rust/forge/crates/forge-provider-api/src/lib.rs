//! Shared provider contracts, independent of provider SDKs and Harness workflows.

pub mod access;
pub mod agent;
pub mod approval;
pub mod catalog;
pub mod change;
pub mod checklist;
pub mod compaction;
pub mod context;
pub mod control;
pub mod deadline;
pub mod error;
pub mod event;
pub mod events;
pub mod execution;
pub mod generation;
pub mod input;
pub mod message;
pub mod model;
pub mod permissions;
pub mod progress;
pub mod retry;
pub mod service_tier;
pub mod skill;
pub mod submission;
pub mod terminal;
pub mod tool;
pub mod trace;
pub mod turn;
pub mod turn_control;
pub mod usage;

pub use change::{ProviderChangeKind, ProviderChangeSet, ProviderFileChange};
pub use checklist::{ChecklistStatus, ProviderChecklistEntry, ProviderChecklistUpdate};
pub use compaction::{Compaction, CompactionState};
pub use error::ProviderError;
pub use event::*;
pub use events::{ProviderEventSink, ProviderEventStream};
pub use execution::*;
pub use message::{MessageClassification, MessageId, MessageRole, MessageUpdate};
pub use model::*;
pub use service_tier::ServiceTier;
pub use tool::{
    TextUpdate, ToolActivityKind, ToolDiagnostic, ToolInvocation, ToolOutcome, ToolStatus,
    ToolUpdate,
};

pub mod native_goal;
pub use input::PromptMode;
pub use native_goal::{NativeGoalStatus, NativeGoalUpdate};

pub mod completion;
pub use completion::TurnMetrics;

pub mod capability;
pub mod provider;
