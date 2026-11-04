use crate::approval;
use crate::checklist::ProviderChecklistUpdate;
use crate::compaction::Compaction;
use crate::context::ContextUsage;
use crate::message::MessageUpdate;
use crate::tool::{TextUpdate, ToolUpdate};
use crate::*;
use serde::{Deserialize, Serialize};

/// Bind every normalized observation to the live session generation that produced it.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct BoundProviderEvent {
    /// Identify the exact live provider binding; retired bindings cannot update replacements.
    pub binding_id: crate::session::BindingId,
    /// Identify the final normalized event position represented by this envelope.
    pub sequence: crate::EventSequence,
    /// Carry the provider-neutral observation.
    pub event: ProviderEvent,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum ProviderEvent {
    /// Prove all native foreground resources have been collected for one invocation.
    Settled {
        settlement: Box<crate::InvocationSettlement>,
    },
    /// Publish scoped collection failure while the adapter retains unfinished resource ownership.
    SettlementFailed {
        scope: InvocationScope,
        failure: crate::ProviderFailure,
    },
    /// Publish effective support on the same ordered stream used for dependent admissions.
    CapabilitiesChanged {
        scope: EventScope,
        capabilities: crate::capability::CapabilityManifest,
    },
    /// Publish a late-resolved durable history identity independently of display-only runtime text.
    SessionIdentity {
        scope: EventScope,
        provider_session_id: crate::ProviderSessionId,
    },
    /// Carry validated execution lifecycle evidence independently of RPC completion.
    Execution {
        observation: crate::lifecycle::ExecutionObservation,
    },
    Message {
        address: MessageAddress,
        update: MessageUpdate,
    },
    Tool {
        address: ToolAddress,
        update: ToolUpdate,
    },
    Turn {
        address: TurnAddress,
        update: TurnUpdate,
    },
    Usage {
        address: TurnAddress,
        update: usage::UsageUpdate,
    },
    Runtime {
        scope: InvocationScope,
        update: RuntimeUpdate,
    },
    Compaction {
        scope: EventScope,
        update: Box<Compaction>,
    },
    Agent {
        scope: InvocationScope,
        update: AgentUpdate,
    },
    Checklist {
        address: TurnAddress,
        update: ProviderChecklistUpdate,
    },
    Approval {
        scope: InvocationScope,
        update: ApprovalUpdate,
    },
    DirectoryAccess {
        scope: InvocationScope,
        grant: Vec<crate::access::DirectoryGrant>,
    },
    InputAccepted {
        scope: InvocationScope,
        acknowledgement: InputAcknowledgement,
    },
    Retry {
        scope: InvocationScope,
        update: Option<retry::ProviderRetry>,
    },
    Diagnostic {
        scope: EventScope,
        diagnostic: ProviderDiagnostic,
    },
    ParentBoundary {
        scope: InvocationScope,
        update: ParentBoundaryUpdate,
    },
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum RuntimeUpdate {
    Resolved { runtime: ProviderRuntime },
    ContextUsage { usage: ContextUsage },
    Progress { received_bytes: u64 },
    ForegroundIdle,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum AgentUpdate {
    Lifecycle {
        event: crate::agent::AgentLifecycleEvent,
    },
    ConversationResolved {
        thread_id: String,
    },
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "kind", content = "request", rename_all = "snake_case")]
pub enum ApprovalUpdate {
    Requested(approval::ApprovalRequestView),
    Resolved(approval::ApprovalRequestView),
    Cancelled(approval::ApprovalRequestView),
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum InputAcknowledgement {
    Steering {
        input_id: Option<String>,
        text: String,
    },
    TaskStarted {
        input_id: Option<String>,
        start: crate::input::TaskStart,
        prompt: String,
    },
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ProviderDiagnostic {
    /// Carry only a normalized failure category.
    pub kind: crate::error::ProviderFailureKind,
    /// Preserve user-actionable text after native details have been removed.
    pub message: crate::diagnostic::SanitizedMessage,
    pub fatal: bool,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum ParentBoundaryUpdate {
    WaitStarted { agent_count: usize },
    WaitUpdated { agent_count: usize },
    WaitFinished,
}

impl ProviderEvent {
    pub fn scope(&self) -> EventScope {
        match self {
            Self::Settled { settlement } => settlement.scope.clone().into(),
            Self::SettlementFailed { scope, .. } => scope.clone().into(),
            Self::CapabilitiesChanged { scope, .. } | Self::SessionIdentity { scope, .. } => {
                scope.clone()
            }
            Self::Execution { observation } => observation.scope().clone().into(),
            Self::Message { address, .. } => address.turn.scope.clone().into(),
            Self::Tool { address, .. } => address.turn.scope.clone().into(),
            Self::Turn { address, .. }
            | Self::Usage { address, .. }
            | Self::Checklist { address, .. } => address.scope.clone().into(),
            Self::Compaction { scope, .. } | Self::Diagnostic { scope, .. } => scope.clone(),
            Self::Runtime { scope, .. }
            | Self::Agent { scope, .. }
            | Self::Approval { scope, .. }
            | Self::DirectoryAccess { scope, .. }
            | Self::InputAccepted { scope, .. }
            | Self::Retry { scope, .. }
            | Self::ParentBoundary { scope, .. } => scope.clone().into(),
        }
    }

    pub fn turn_address(&self) -> Option<&TurnAddress> {
        match self {
            Self::Message { address, .. } => Some(&address.turn),
            Self::Tool { address, .. } => Some(&address.turn),
            Self::Turn { address, .. }
            | Self::Usage { address, .. }
            | Self::Checklist { address, .. } => Some(address),
            _ => None,
        }
    }

    pub fn message_delta(&self) -> Option<&str> {
        match self {
            Self::Message {
                update: MessageUpdate::Delta { text },
                ..
            } => Some(text),
            _ => None,
        }
    }

    pub fn tool_output_delta(&self) -> Option<&str> {
        match self {
            Self::Tool {
                update:
                    ToolUpdate::Output {
                        update: TextUpdate::Append(text),
                    },
                ..
            } => Some(text),
            _ => None,
        }
    }

    pub fn observed_at_ms(&self, fallback: i64) -> i64 {
        match self {
            Self::Turn {
                update: TurnUpdate::Started { started_at_ms },
                ..
            } => *started_at_ms,
            Self::Turn {
                update:
                    TurnUpdate::CallFinished { completed_at_ms }
                    | TurnUpdate::Finished {
                        completed_at_ms, ..
                    },
                ..
            } => *completed_at_ms,
            _ => fallback,
        }
    }
}

/// Represent the provider display label and resolved model shown by Harness.
#[derive(Clone, Debug, Default, Deserialize, Serialize, Eq, PartialEq)]
pub struct ProviderRuntime {
    /// Display text only; use ProviderDescriptor.kind for machine identity.
    pub provider: String,
    pub model: Option<String>,
}
