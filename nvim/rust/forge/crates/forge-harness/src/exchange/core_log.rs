//! Retain lifecycle ordering evidence without input, tool output, or optional diagnostic text.

use forge_provider_api::{
    BindingId, EventScope, EventSequence, InvocationScope, OperationId, ProviderEvent,
    ProviderFailure, ProviderFailureKind, ProviderOperation,
};
use serde::{Deserialize, Serialize};
use std::{collections::HashMap, path::PathBuf, sync::Mutex};

use super::command::{
    Command, CommandFailure, CommandId, CommandOutcome, CommandResult, ExecuteCommandError,
};
use super::journal::{CommandJournal, CommandJournalError};

/// Preserve safe owner facts independently of optional trace enablement, capacity, and rotation.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "record", rename_all = "snake_case")]
pub(crate) enum CoreRecord {
    /// Record owner receipt, including replay and rejected admission, without serializing its payload.
    CommandReceived {
        command: CommandId,
        kind: CoreCommandKind,
    },
    /// Record new durable FIFO admission in the same transaction as its command record.
    CommandAdmitted {
        command: CommandId,
        kind: CoreCommandKind,
    },
    /// Record effect admission in the same transaction as the active command revision.
    CommandStarted { command: CommandId, revision: u64 },
    /// Record a committed outcome before attempting any response publication.
    CommandFinished {
        command: CommandId,
        revision: u64,
        outcome: CoreOutcome,
    },
    /// Record the response attempt separately from durable command completion.
    CommandResponded {
        command: CommandId,
        outcome: CoreOutcome,
    },
    /// Record the exact operation before its retained work starts.
    OperationStarted { operation: CoreOperation },
    /// Record the operation result without asserting that a response waiter observed it.
    OperationCompleted {
        operation: CoreOperation,
        outcome: CoreOutcome,
    },
    /// Record consumption, rather than adapter emission, of one ordered provider observation.
    ProviderEventConsumed {
        binding: BindingId,
        sequence: EventSequence,
        scope: EventScope,
        kind: CoreProviderEventKind,
        evidence: Option<CoreProviderEvidence>,
    },
    /// Record the owner transition at its commit boundary without introducing another state owner.
    TransitionCommitted {
        command: Option<CommandId>,
        invocation: Option<InvocationScope>,
        kind: CoreTransitionKind,
        outcome: CoreOutcome,
    },
}

impl CoreRecord {
    /// Capture typed native lifecycle evidence without retaining completion text or tool payloads.
    pub(crate) fn provider_event_metadata(
        binding: BindingId,
        sequence: EventSequence,
        scope: EventScope,
        event: &ProviderEvent,
    ) -> Self {
        Self::ProviderEventConsumed {
            binding,
            sequence,
            scope,
            kind: CoreProviderEventKind::from(event),
            evidence: CoreProviderEvidence::from_event(event),
        }
    }

    /// Capture event correlation for durable log fixtures without retaining message content.
    #[cfg(test)]
    pub(crate) fn provider_event(
        scope: EventScope,
        event: &forge_provider_api::BoundProviderEvent,
    ) -> Self {
        Self::provider_event_metadata(
            event.binding_id.clone(),
            event.sequence,
            scope,
            &event.event,
        )
    }
}

/// Preserve native completion and failures independently of successful input acceptance.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "evidence", rename_all = "snake_case")]
pub(crate) enum CoreProviderEvidence {
    /// Retain the original host identities and positive native start timestamp.
    TurnStarted {
        thread_id: forge_provider_api::ThreadId,
        turn_id: forge_provider_api::TurnId,
        started_at_ms: i64,
    },
    /// Retain the exact canonical native reason and provider collection boundary.
    TurnCompleted {
        thread_id: forge_provider_api::ThreadId,
        turn_id: forge_provider_api::TurnId,
        completed: forge_provider_api::CompletedTurn,
    },
    /// Retain native thread failure without inventing turn attribution.
    ThreadError {
        thread_id: forge_provider_api::ThreadId,
        error: ProviderFailure,
    },
    /// Retain positively correlated turn failure without inventing completion.
    TurnError {
        thread_id: forge_provider_api::ThreadId,
        turn_id: forge_provider_api::TurnId,
        error: ProviderFailure,
    },
    /// Retain connection failure independently of any native completion certificate.
    ProviderUnavailable { failure: ProviderFailure },
    /// Retain recoverable warning scope and category without display contents.
    Warning {
        scope: forge_provider_api::WarningScope,
        kind: forge_provider_api::ProviderWarningKind,
    },
    /// Retain feature observation failure without erasing the previous successful snapshot.
    TerminalsFailed {
        thread_id: forge_provider_api::ThreadId,
        error: ProviderFailure,
    },
}

impl CoreProviderEvidence {
    /// Select lifecycle and failure metadata without copying message, tool, or warning text.
    fn from_event(event: &ProviderEvent) -> Option<Self> {
        use forge_provider_api::{ProviderConnectionEvent, ThreadEvent, TurnEvent};
        match event {
            ProviderEvent::Provider { event } => match event {
                ProviderConnectionEvent::Unavailable(unavailable) => {
                    Some(Self::ProviderUnavailable {
                        failure: unavailable.failure,
                    })
                }
                ProviderConnectionEvent::Warning(warning) => Some(Self::Warning {
                    scope: warning.scope.clone(),
                    kind: warning.kind.clone(),
                }),
                ProviderConnectionEvent::CapabilitiesChanged(_) => None,
            },
            ProviderEvent::Thread { thread_id, event } => match event {
                ThreadEvent::Error { error } => Some(Self::ThreadError {
                    thread_id: thread_id.clone(),
                    error: *error,
                }),
                ThreadEvent::TerminalsUpdated(Err(error)) => Some(Self::TerminalsFailed {
                    thread_id: thread_id.clone(),
                    error: *error,
                }),
                ThreadEvent::IdentityResolved(_)
                | ThreadEvent::ContextUsage(_)
                | ThreadEvent::TerminalsUpdated(Ok(_))
                | ThreadEvent::CompactionUpdated(_) => None,
            },
            ProviderEvent::Turn {
                thread_id,
                turn_id,
                event,
            } => match event {
                TurnEvent::Started(started) => Some(Self::TurnStarted {
                    thread_id: thread_id.clone(),
                    turn_id: turn_id.clone(),
                    started_at_ms: started.started_at_ms,
                }),
                TurnEvent::Completed(completed) => Some(Self::TurnCompleted {
                    thread_id: thread_id.clone(),
                    turn_id: turn_id.clone(),
                    completed: completed.clone(),
                }),
                TurnEvent::Error { error } => Some(Self::TurnError {
                    thread_id: thread_id.clone(),
                    turn_id: turn_id.clone(),
                    error: *error,
                }),
                TurnEvent::ModelResolved(_)
                | TurnEvent::Usage(_)
                | TurnEvent::ChecklistUpdated(_) => None,
            },
            ProviderEvent::Item { .. } | ProviderEvent::Agent { .. } => None,
        }
    }
}

/// Identify host operations by their original command and provider operations by their real context.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "owner", rename_all = "snake_case")]
pub(crate) enum CoreOperation {
    /// Correlate host collection and configuration without inventing a provider operation identity.
    Command {
        command: CommandId,
        kind: CoreCommandKind,
    },
    /// Preserve the host-issued native operation identity and exact available target metadata.
    Provider {
        id: OperationId,
        kind: ProviderOperation,
        binding: Option<BindingId>,
        scope: EventScope,
        invocation: Option<Box<InvocationScope>>,
    },
}

/// Classify command payloads without persisting any user-supplied fields.
#[derive(Clone, Copy, Debug, Deserialize, Serialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum CoreCommandKind {
    /// Control child execution.
    Agent,
    /// Continue a captured workflow.
    ContinueWorkflow,
    /// Restore captured host effects.
    Rollback,
    /// Collect selected foreground work.
    CancelExchange,
    /// Commit complete session configuration.
    ConfigureSession,
    /// Compact settled provider history.
    CompactContext,
    /// Accept or mutate journaled input.
    Input,
    /// Change host-owned task policy or lifecycle.
    Task,
}

impl From<&Command> for CoreCommandKind {
    /// Classify every owned command exhaustively without examining its content.
    fn from(command: &Command) -> Self {
        match command {
            Command::Agent(_) => Self::Agent,
            Command::ContinueWorkflow(_) => Self::ContinueWorkflow,
            Command::Rollback(_) => Self::Rollback,
            Command::CancelExchange(_) => Self::CancelExchange,
            Command::ConfigureSession(_) => Self::ConfigureSession,
            Command::CompactContext(_) => Self::CompactContext,
            Command::Input(_) => Self::Input,
            Command::Task(_) => Self::Task,
        }
    }
}

/// Preserve normalized outcomes while excluding recursive domain errors and provider diagnostics.
#[derive(Clone, Copy, Debug, Deserialize, Serialize)]
#[serde(tag = "outcome", rename_all = "snake_case")]
pub(crate) enum CoreOutcome {
    /// The recorded postcondition was established.
    Succeeded,
    /// The selected invocation reached intentional cancellation.
    Cancelled,
    /// An observation deadline elapsed while retained work may remain.
    TimedOut,
    /// Work remains owned and has no final settlement.
    Pending,
    /// A process or owner interruption prevented ordinary completion.
    Interrupted,
    /// A typed host precondition rejected the request.
    Rejected,
    /// A domain operation failed without enough normalized metadata to infer its effect disposition.
    Failed,
    /// Required persistence could not certify an outcome.
    StorageUnavailable,
    /// An observer lost authority to infer the final outcome.
    ObservationLost,
    /// A normalized provider cause retains its exact phase and effect disposition.
    ProviderFailed { failure: ProviderFailure },
    /// A command retained a provider category without retaining a complete phase/disposition receipt.
    ProviderReason { kind: ProviderFailureKind },
}

impl CoreOutcome {
    /// Classify a durable command result without copying its domain payload or error text.
    pub(crate) fn command(result: &Result<CommandOutcome, CommandFailure>) -> Self {
        match result {
            Ok(CommandOutcome::Cancelled(
                super::command::CancellationOutcome::Cancelled { .. }
                | super::command::CancellationOutcome::Retracted { .. },
            )) => Self::Cancelled,
            Ok(
                CommandOutcome::Cancelled(super::command::CancellationOutcome::NoWork)
                | CommandOutcome::Agent(_)
                | CommandOutcome::Rollback(_)
                | CommandOutcome::Task(_)
                | CommandOutcome::Compacted(_)
                | CommandOutcome::Configured(_)
                | CommandOutcome::Input { .. },
            ) => Self::Succeeded,
            Err(CommandFailure::CollectionTimedOut | CommandFailure::ConfigurationTimedOut) => {
                Self::TimedOut
            }
            Err(CommandFailure::Interrupted | CommandFailure::OwnerStopping) => Self::Interrupted,
            Err(
                CommandFailure::DeliveryUncertain
                | CommandFailure::DispatchUnconfirmed
                | CommandFailure::CollectionUncertain,
            ) => Self::ObservationLost,
            Err(CommandFailure::ConfigurationReconcileFailed { kind }) => {
                Self::ProviderReason { kind: *kind }
            }
            Err(
                CommandFailure::Agent(_)
                | CommandFailure::Rollback(_)
                | CommandFailure::Task(_)
                | CommandFailure::TaskSynchronization { .. }
                | CommandFailure::Compaction(_),
            ) => Self::Failed,
            Err(
                CommandFailure::Input(_)
                | CommandFailure::AttachmentUnavailable
                | CommandFailure::LegacyInputRejected
                | CommandFailure::PrerequisiteFailed
                | CommandFailure::OwnershipUnavailable
                | CommandFailure::ExchangeChanged
                | CommandFailure::ConfigurationRejected(_),
            ) => Self::Rejected,
        }
    }

    /// Classify a response attempt, including failures before durable command admission.
    pub(crate) fn response(result: &Result<CommandResult, ExecuteCommandError>) -> Self {
        match result {
            Ok(result) => Self::command(&Ok(result.outcome.clone())),
            Err(ExecuteCommandError::Failed(failure)) => Self::command(&Err(failure.clone())),
            Err(ExecuteCommandError::OutcomeUncommitted) => Self::StorageUnavailable,
            Err(ExecuteCommandError::ObservationLost) => Self::ObservationLost,
            Err(ExecuteCommandError::OwnerClosed) => Self::Interrupted,
            Err(ExecuteCommandError::QueueFull | ExecuteCommandError::IdentityConflict) => {
                Self::Rejected
            }
        }
    }
}

/// Identify the exact native event family without persisting its contents.
#[derive(Clone, Copy, Debug, Deserialize, Serialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum CoreProviderEventKind {
    /// Connection support, warnings, or unavailability changed.
    Provider,
    /// Conversation metadata changed.
    Thread,
    /// Canonical turn lifecycle or metadata changed.
    Turn,
    /// An item was admitted, updated, or completed.
    Item,
    /// A native child relationship or lifecycle changed.
    Agent,
}

impl From<&ProviderEvent> for CoreProviderEventKind {
    /// Classify native event families exhaustively without retaining payload content.
    fn from(event: &ProviderEvent) -> Self {
        match event {
            ProviderEvent::Provider { .. } => Self::Provider,
            ProviderEvent::Thread { .. } => Self::Thread,
            ProviderEvent::Turn { .. } => Self::Turn,
            ProviderEvent::Item { .. } => Self::Item,
            ProviderEvent::Agent { .. } => Self::Agent,
        }
    }
}

/// Identify owner lifecycle milestones whose order must survive diagnostic rotation.
#[derive(Clone, Copy, Debug, Deserialize, Serialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum CoreTransitionKind {
    /// Cancellation crossed its progress-notice boundary.
    CancellationPending,
    /// Cancellation crossed its response deadline with work retained.
    CancellationTimedOut,
    /// Exact foreground collection started.
    CollectionStarted,
    /// Native and host work were collected.
    Collected,
    /// Foreground execution reached settlement.
    ForegroundSettled,
    /// Owner settlement committed.
    SettlementCompleted,
    /// Retained collection completed after its original response.
    CollectionCompleted,
    /// Retained collection failed after its original response.
    CollectionFailed,
    /// Complete configuration committed.
    ConfigurationCommitted,
    /// Input state committed.
    InputCommitted,
    /// Task policy or an active task checkpoint committed.
    TaskCommitted,
}

/// Serialize metadata-only appends from native consumers without owning host lifecycle state.
pub(crate) struct CoreEventLog {
    root: PathBuf,
    sessions: Mutex<HashMap<String, CommandJournal>>,
    operations: std::sync::Arc<crate::provider::operation::OperationRegistry>,
}

impl CoreEventLog {
    /// Create lazy connection ownership without opening storage or starting another writer task.
    pub(crate) fn new(root: PathBuf) -> Self {
        Self {
            root,
            sessions: Mutex::new(HashMap::new()),
            operations: Default::default(),
        }
    }

    /// Get the shared registry of original native operation ownership.
    pub(crate) fn operations(
        &self,
    ) -> std::sync::Arc<crate::provider::operation::OperationRegistry> {
        self.operations.clone()
    }

    /// Append safe metadata using the existing schema and transaction sequence allocator.
    /// Hold no asynchronous guard or lifecycle resource while SQLite commits this evidence.
    pub(crate) fn append(
        &self,
        session: &str,
        record: &CoreRecord,
    ) -> Result<Option<u64>, CommandJournalError> {
        let mut sessions = self
            .sessions
            .lock()
            .map_err(|_| CommandJournalError::CoreUnavailable)?;
        let journal = match sessions.entry(session.into()) {
            std::collections::hash_map::Entry::Occupied(entry) => entry.into_mut(),
            std::collections::hash_map::Entry::Vacant(entry) => {
                // Native owners may start before a caller command opens its journal.
                // Reuse shared bootstrap rather than maintaining a separate metadata schema.
                drop(
                    crate::storage::SqliteStore::open(&self.root)
                        .map_err(|_| CommandJournalError::CoreUnavailable)?,
                );
                entry.insert(CommandJournal::open_metadata(&self.root, session)?)
            }
        };
        // Temporary utilities have no durable host session and must not create one for logging.
        if !journal.has_durable_session()? {
            return Ok(None);
        }
        journal.append_core(record).map(Some)
    }
}

/// Persist a real host session once before exercising owner command or core-evidence storage.
#[cfg(test)]
pub(crate) fn persist_test_session(root: &std::path::Path, id: &str) -> anyhow::Result<()> {
    use crate::session::SessionStore;
    let mut store = crate::storage::SqliteStore::open(root)?;
    if store.load_session(id)?.is_some() {
        return Ok(());
    }
    let session: crate::session::HarnessSession = serde_json::from_value(serde_json::json!({
        "id":id, "primary_agent_id":crate::session::HarnessSession::primary_agent_id(id),
        "name":"Core evidence fixture", "workspace":"workspace", "provider":"mock", "model":"mock",
        "effort":"", "approval_mode":"ask", "created_at_ms":0, "updated_at_ms":0,
        "provider_fork_state":{"state":"ready"}
    }))?;
    store.save_session(&session)
}
