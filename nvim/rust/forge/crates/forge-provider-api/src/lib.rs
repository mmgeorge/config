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
pub mod diagnostic;
pub mod error;
pub mod event;
pub mod events;
pub mod execution;
pub mod generation;
pub mod input;
pub mod lifecycle;
pub mod message;
pub mod model;
pub mod permissions;
pub mod progress;
pub mod provider_session_id;
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
pub mod usage;
pub mod version;

pub use catalog::ProviderCatalogRequest;
pub use change::{ProviderChangeKind, ProviderChangeSet, ProviderFileChange};
pub use checklist::{ChecklistStatus, ProviderChecklistEntry, ProviderChecklistUpdate};
pub use compaction::{Compaction, CompactionState};
pub use diagnostic::{SanitizedMessage, SanitizedMessageError};
pub use error::{ProviderError, ProviderFailure, ProviderFailureKind, ProviderOperation};
pub use event::*;
pub use events::{ProviderEventSink, ProviderEventStream};
pub use execution::*;
pub use generation::{ReasoningSelection, TextGenerationRequest, TextValidator};
pub use input::{
    FileAttachment, MessageInput, PromptInput, QuestionReply, QuestionReplyOption,
    QuestionReplyResponse,
};
pub use lifecycle::{
    CompletionSnapshot, ExecutionFailure, ExecutionObservation, ExecutionResult, ExecutionUpdate,
    ObservationFailure, SubmissionId,
};
pub use message::{MessageClassification, MessageId, MessageRole, MessageUpdate};
pub use model::*;
pub use provider_session_id::{ProviderSessionId, ProviderSessionIdError};
pub use service_tier::ServiceTier;
pub use session::{
    BindingId, BindingIdError, CheckpointId, CheckpointIdError, ContinuationOutcome,
    ContinuationRequest, ContinuationToken, ConversationForkRequest, ForkTarget, HostEffectFailure,
    HostToolEndpoint, InputAcceptance, InvocationCallbacks, InvocationConfiguration, McpLaunch,
    McpTransport, ModelSelection, PromptRequest, ProviderHostEffects, RecoveredInput,
    RecoveryToken, RecoveryTokenError, ResourceLease, ResourceSelection, SessionBinding,
    SessionMode, SessionRequest, SessionSettings, SteeringAcceptance, SteeringDelivery,
    SteeringRequest, SubmissionEvidence,
};
pub use tool::{
    TextUpdate, ToolActivityKind, ToolDiagnostic, ToolInvocation, ToolOutcome, ToolStatus,
    ToolUpdate,
};
pub use trace::{ProviderTrace, ProviderTraceEvent, ProviderTraceRecord};

pub mod native_goal;
pub use input::PromptMode;
pub use native_goal::{NativeGoalCommand, NativeGoalStatus, NativeGoalUpdate};

pub mod completion;
pub use completion::TurnMetrics;

pub mod capability;
pub mod provider;
pub use provider::{
    AuthenticationInteraction, AuthenticationMethod, AuthenticationMethodId,
    AuthenticationMethodIdError, CompactionRequest, GoalStopMode, ProviderSetup,
    RuntimeAvailability, RuntimeOrigin, RuntimeVersions, TerminalLaunch, VersionObservation,
};
