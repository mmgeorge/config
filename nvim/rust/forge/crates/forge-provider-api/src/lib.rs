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
pub mod diagnostic;
pub mod error;
pub mod event;
pub mod events;
pub mod execution;
pub mod generation;
pub mod identity;
pub mod input;
pub mod lifecycle;
pub mod message;
pub mod model;
pub mod operation_context;
pub mod operations;
pub mod permissions;
pub mod progress;
pub mod provider_session_id;
pub mod request_identity;
pub mod retained_operation;
pub mod retained_task;
pub mod retry;
pub mod service_tier;
pub mod session;
pub mod skill;
pub mod submission;
pub mod terminal;
pub mod testing;
pub mod tool;
pub mod trace;
pub mod turn;
pub mod turn_control;
pub mod turn_result;
pub mod turn_runner;
pub mod turn_usage;
pub mod usage;
pub mod version;

pub use agent::{
    AgentActiveState, AgentDelegation, AgentDescription, AgentEvent, AgentLifecycleEvent,
    AgentOutcome,
};
pub use capability::{CapabilityManifest, CapabilityRestriction};
pub use catalog::{McpQuery, QueryContext, WorkspaceContext};
pub use change::{ProviderChangeKind, ProviderChangeSet, ProviderFileChange};
pub use checklist::{ChecklistStatus, ProviderChecklistEntry, ProviderChecklistUpdate};
pub use compaction::{Compaction, CompactionState};
pub use diagnostic::{SanitizedMessage, SanitizedMessageError};
pub use error::{
    FailureDisposition, ProviderError, ProviderFailure, ProviderFailureKind, ProviderOperation,
    ProviderPhase,
};
pub use event::*;
pub use events::{
    AdapterActivityWriter, NativeTurnStart, ProviderEventSink, ProviderEventStream,
    TurnRegistrationReader,
};
pub use execution::*;
pub use generation::{ReasoningSelection, TextGenerationRequest, TextValidator};
pub use identity::{IdentityError, ItemId, ModelCallId, ThreadId, TurnId, UsageReportId};
pub use input::{
    FileAttachment, MessageInput, PromptInput, QuestionReply, QuestionReplyOption,
    QuestionReplyResponse, TaskKind, TaskStart,
};
pub use lifecycle::{CompletionSnapshot, ObservationFailure, SubmissionId};
pub use message::*;
pub use model::*;
pub use operation_context::*;
pub use operations::*;
pub use provider_session_id::{ProviderSessionId, ProviderSessionIdError};
pub use request_identity::{
    PromptRequestIdentity, SessionRequestIdentity, SteeringRequestIdentity,
};
pub use service_tier::ServiceTier;
pub use session::*;
pub use tool::{
    InspectableArguments, TextUpdate, ToolCompletion, ToolDescriptor, ToolDiagnostic,
    ToolInputUpdate, ToolItemEvent, ToolOutcome, ToolStatus,
};
pub use trace::{ProviderTrace, ProviderTraceEvent, ProviderTraceRecord};
pub use turn_result::{CompletedTurn, CoveredInput, TurnResult, TurnResultError};
pub use turn_usage::{TurnUsageReport, UsageReportError};

pub use input::PromptMode;

pub mod completion;
pub use completion::TurnMetrics;

pub mod capability;
pub mod provider;
pub use provider::{
    AuthenticationInteraction, AuthenticationMethod, AuthenticationMethodId,
    AuthenticationMethodIdError, CompactionRequest, ProviderSetup, RuntimeAvailability,
    RuntimeOrigin, RuntimeVersions, TerminalLaunch, VersionObservation,
};

pub use testing::TestMessageContent;
