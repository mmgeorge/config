//! Typed observations from one ordered native provider connection.
use crate::*;
use serde::{Deserialize, Serialize};

/// Bind a normalized observation to its original connection and stream position.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct BoundProviderEvent {
    pub binding_id: BindingId,
    pub sequence: EventSequence,
    pub event: ProviderEvent,
}

/// Carry notifications whose ownership is resolved by the binding registry.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum ProviderEvent {
    Provider {
        event: ProviderConnectionEvent,
    },
    Thread {
        thread_id: ThreadId,
        event: ThreadEvent,
    },
    Turn {
        thread_id: ThreadId,
        turn_id: TurnId,
        event: TurnEvent,
    },
    Item {
        thread_id: ThreadId,
        turn_id: TurnId,
        item_id: ItemId,
        event: crate::message::ItemEvent,
    },
    Agent {
        agent_id: AgentId,
        event: crate::agent::AgentEvent,
    },
}

/// Report effective connection support and availability.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "kind", content = "value", rename_all = "snake_case")]
pub enum ProviderConnectionEvent {
    CapabilitiesChanged(crate::capability::CapabilityManifest),
    Warning(ProviderWarning),
    Unavailable(ProviderUnavailable),
}

/// Observe thread metadata independently of response-cycle completion.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "kind", content = "value", rename_all = "snake_case")]
pub enum ThreadEvent {
    Error { error: ProviderFailure },
    IdentityResolved(ProviderSessionId),
    ContextUsage(crate::context::ContextUsage),
    TerminalsUpdated(Result<crate::terminal::TerminalInventory, ProviderFailure>),
    CompactionUpdated(Box<crate::compaction::Compaction>),
}

/// Permit native activity without completion or host-policy publication authority.
#[derive(Clone, Debug)]
pub enum AdapterActivity {
    Thread {
        thread_id: ThreadId,
        event: ThreadEvent,
    },
    Turn {
        thread_id: ThreadId,
        turn_id: TurnId,
        event: TurnMetadataEvent,
    },
    Item {
        thread_id: ThreadId,
        turn_id: TurnId,
        item_id: ItemId,
        event: crate::message::ItemEvent,
    },
    Agent {
        agent_id: AgentId,
        event: crate::agent::AgentEvent,
    },
    Warning(ProviderWarning),
}

/// Permit turn metadata while reserving start and completion for the retained runner.
#[derive(Clone, Debug)]
pub enum TurnMetadataEvent {
    Error { error: ProviderFailure },
    ModelResolved(crate::model::ResolvedModel),
    Usage(TurnUsageReport),
    ChecklistUpdated(ProviderChecklistUpdate),
}

impl From<AdapterActivity> for ProviderEvent {
    /// Normalize the closed activity vocabulary without creating lifecycle authority.
    fn from(activity: AdapterActivity) -> Self {
        match activity {
            AdapterActivity::Thread { thread_id, event } => Self::Thread { thread_id, event },
            AdapterActivity::Turn {
                thread_id,
                turn_id,
                event,
            } => Self::Turn {
                thread_id,
                turn_id,
                event: match event {
                    TurnMetadataEvent::Error { error } => TurnEvent::Error { error },
                    TurnMetadataEvent::ModelResolved(model) => TurnEvent::ModelResolved(model),
                    TurnMetadataEvent::Usage(usage) => TurnEvent::Usage(usage),
                    TurnMetadataEvent::ChecklistUpdated(checklist) => {
                        TurnEvent::ChecklistUpdated(checklist)
                    }
                },
            },
            AdapterActivity::Item {
                thread_id,
                turn_id,
                item_id,
                event,
            } => Self::Item {
                thread_id,
                turn_id,
                item_id,
                event,
            },
            AdapterActivity::Agent { agent_id, event } => Self::Agent { agent_id, event },
            AdapterActivity::Warning(warning) => Self::Provider {
                event: ProviderConnectionEvent::Warning(warning),
            },
        }
    }
}

/// Identify the exact object affected by a recoverable warning.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum WarningScope {
    Provider,
    Thread {
        thread_id: ThreadId,
    },
    Turn {
        thread_id: ThreadId,
        turn_id: TurnId,
    },
    Item {
        thread_id: ThreadId,
        turn_id: TurnId,
        item_id: ItemId,
    },
    Agent {
        agent_id: AgentId,
    },
    Operation {
        operation_id: OperationId,
    },
}

/// Classify recoverable native limitations without implying turn failure.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum ProviderWarningKind {
    /// Preserve a message whose native phase cannot be mapped to a known display role.
    UnsupportedMessageClassification,
    /// Some optional tool description is missing, but required execution evidence remains valid.
    IncompleteToolMetadata,
    /// Some reported diff metadata is unavailable without implying that no file changed.
    IncompleteFileChangeMetadata,
    /// Optional compaction summary or metrics are missing while its known state remains visible.
    IncompleteCompactionMetadata,
    /// Report a classified native notice that does not itself establish turn failure.
    NativeNotice { category: ProviderFailureKind },
    /// A native notification outside the adapter's mapping was observed and not applied.
    UnsupportedNativeEvent,
}

/// Explain a nonfatal limitation at its original scope.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ProviderWarning {
    pub scope: WarningScope,
    pub kind: ProviderWarningKind,
    pub message: SanitizedMessage,
}

/// Fence an unusable connection without manufacturing a completed turn.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ProviderUnavailable {
    pub failure: ProviderFailure,
    pub message: SanitizedMessage,
}

impl ProviderEvent {
    /// Get a native timestamp when this observation carries one.
    pub fn observed_at_ms(&self, fallback: i64) -> i64 {
        match self {
            Self::Turn {
                event: TurnEvent::Started(started),
                ..
            } => started.started_at_ms,
            Self::Turn {
                event: TurnEvent::Completed(completed),
                ..
            } => completed.result().completed_at_ms(),
            Self::Turn {
                event: TurnEvent::Usage(usage),
                ..
            } => *usage.observed_at_ms(),
            Self::Provider { .. }
            | Self::Thread { .. }
            | Self::Turn { .. }
            | Self::Item { .. }
            | Self::Agent { .. } => fallback,
        }
    }
}

/// Represent the provider label and configured default displayed by the host.
#[derive(Clone, Debug, Default, Deserialize, Serialize, Eq, PartialEq)]
pub struct ProviderRuntime {
    pub provider: String,
    pub model: Option<String>,
}
