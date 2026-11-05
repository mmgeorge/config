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
        event: &ProviderEvent,
    ) -> Self {
        Self::ProviderEventConsumed {
            binding,
            sequence,
            scope: event.scope(),
            kind: CoreProviderEventKind::from(event),
            evidence: CoreProviderEvidence::from_event(event),
        }
    }

    /// Capture event correlation for durable log fixtures without retaining message content.
    #[cfg(test)]
    pub(crate) fn provider_event(event: &forge_provider_api::BoundProviderEvent) -> Self {
        Self::provider_event_metadata(event.binding_id.clone(), event.sequence, &event.event)
    }
}

/// Preserve asynchronous execution failure independently of successful input acceptance.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "evidence", rename_all = "snake_case")]
pub(crate) enum CoreProviderEvidence {
    /// Native execution began at an exact turn.
    ExecutionStarted {
        address: forge_provider_api::TurnAddress,
    },
    /// Native execution reached terminal evidence.
    ExecutionFinished { outcome: CoreExecutionOutcome },
    /// Required host publication failed after a known native result.
    PublicationFailed {
        failure: forge_provider_api::HostEffectFailure,
        outcome: CoreExecutionOutcome,
    },
    /// Native observation failed independently of delivery.
    ObservationLost {
        failure: forge_provider_api::ObservationFailure,
    },
    /// Collection completed with the original native outcome.
    Settled { outcome: CoreExecutionOutcome },
    /// Collection failed with a complete normalized phase and disposition.
    SettlementFailed { failure: ProviderFailure },
    /// A diagnostic supplied a normalized reason without exposing display text.
    Diagnostic {
        kind: ProviderFailureKind,
        fatal: bool,
    },
}

impl CoreProviderEvidence {
    /// Select lifecycle and failure metadata while excluding arbitrary event payloads.
    fn from_event(event: &ProviderEvent) -> Option<Self> {
        use forge_provider_api::ExecutionUpdate;
        match event {
            ProviderEvent::Execution { observation } => Some(match observation.update() {
                ExecutionUpdate::Started { address } => Self::ExecutionStarted {
                    address: address.clone(),
                },
                ExecutionUpdate::Finished { result, .. }
                | ExecutionUpdate::ResolvedWithoutExecution { result, .. } => {
                    Self::ExecutionFinished {
                        outcome: CoreExecutionOutcome::from(result),
                    }
                }
                ExecutionUpdate::PublicationFailed {
                    failure, result, ..
                }
                | ExecutionUpdate::ResolutionPublicationFailed {
                    failure, result, ..
                } => Self::PublicationFailed {
                    failure: *failure,
                    outcome: CoreExecutionOutcome::from(result),
                },
                ExecutionUpdate::ObservationLost { failure, .. } => Self::ObservationLost {
                    failure: failure.clone(),
                },
            }),
            ProviderEvent::Settled { settlement } => Some(Self::Settled {
                outcome: CoreExecutionOutcome::from(&settlement.result),
            }),
            ProviderEvent::SettlementFailed { failure, .. } => {
                Some(Self::SettlementFailed { failure: *failure })
            }
            ProviderEvent::Diagnostic { diagnostic, .. } => Some(Self::Diagnostic {
                kind: diagnostic.kind,
                fatal: diagnostic.fatal,
            }),
            ProviderEvent::CapabilitiesChanged { .. }
            | ProviderEvent::SessionIdentity { .. }
            | ProviderEvent::Terminals { .. }
            | ProviderEvent::Message { .. }
            | ProviderEvent::Tool { .. }
            | ProviderEvent::Turn { .. }
            | ProviderEvent::Usage { .. }
            | ProviderEvent::Runtime { .. }
            | ProviderEvent::Compaction { .. }
            | ProviderEvent::Agent { .. }
            | ProviderEvent::Checklist { .. }
            | ProviderEvent::Approval { .. }
            | ProviderEvent::DirectoryAccess { .. }
            | ProviderEvent::InputAccepted { .. }
            | ProviderEvent::Retry { .. }
            | ProviderEvent::ParentBoundary { .. } => None,
        }
    }
}

/// Persist terminal execution outcome without assistant contents or completion metrics.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "outcome", rename_all = "snake_case")]
pub(crate) enum CoreExecutionOutcome {
    /// Native execution completed normally.
    Completed,
    /// Native execution confirmed cancellation.
    Cancelled,
    /// Native execution failed after acceptance may already have succeeded.
    Failed { failure: CoreExecutionFailure },
}

impl From<&forge_provider_api::ExecutionResult> for CoreExecutionOutcome {
    /// Exclude completion payloads while preserving the native terminal result.
    fn from(result: &forge_provider_api::ExecutionResult) -> Self {
        match result {
            forge_provider_api::ExecutionResult::Completed { .. } => Self::Completed,
            forge_provider_api::ExecutionResult::Cancelled { .. } => Self::Cancelled,
            forge_provider_api::ExecutionResult::Failed { failure, .. } => Self::Failed {
                failure: CoreExecutionFailure::from(failure),
            },
        }
    }
}

/// Preserve typed terminal and retry metadata without native diagnostic text.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "failure", rename_all = "snake_case")]
pub(crate) enum CoreExecutionFailure {
    /// Native execution was positively rejected.
    Rejected,
    /// A permanent failure has a normalized category.
    Terminal { kind: ProviderFailureKind },
    /// Shared retry policy receives the exact native turn and any provider-directed wait.
    Retryable {
        address: forge_provider_api::TurnAddress,
        category: forge_provider_api::retry::RetryCategory,
        retry_after: Option<std::time::Duration>,
    },
}

impl From<&forge_provider_api::ExecutionFailure> for CoreExecutionFailure {
    /// Preserve terminal classification and provider-directed waits without copying output.
    fn from(failure: &forge_provider_api::ExecutionFailure) -> Self {
        match failure {
            forge_provider_api::ExecutionFailure::Rejected => Self::Rejected,
            forge_provider_api::ExecutionFailure::Terminal { kind } => {
                Self::Terminal { kind: *kind }
            }
            forge_provider_api::ExecutionFailure::Retryable {
                address,
                category,
                retry_after,
            } => Self::Retryable {
                address: address.clone(),
                category: *category,
                retry_after: *retry_after,
            },
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
        invocation: Option<InvocationScope>,
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
            Ok(CommandOutcome::Cancelled(super::command::CancellationOutcome::Cancelled {
                ..
            })) => Self::Cancelled,
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

/// Preserve the closed provider event vocabulary without text, tool arguments, or native payloads.
#[derive(Clone, Copy, Debug, Deserialize, Serialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum CoreProviderEventKind {
    /// Foreground collection completed.
    Settled,
    /// Foreground collection failed.
    SettlementFailed,
    /// Effective capabilities changed.
    CapabilitiesChanged,
    /// Durable native history identity resolved.
    SessionIdentity,
    /// Background terminal inventory changed.
    Terminals,
    /// Execution lifecycle evidence arrived.
    Execution,
    /// Assistant message content changed.
    Message,
    /// Tool output or lifecycle changed.
    Tool,
    /// Native turn lifecycle changed.
    Turn,
    /// Provider usage changed.
    Usage,
    /// Runtime metadata changed.
    Runtime,
    /// Context compaction progressed.
    Compaction,
    /// Native descendant state changed.
    Agent,
    /// Provider checklist changed.
    Checklist,
    /// Approval state changed.
    Approval,
    /// Directory authority changed.
    DirectoryAccess,
    /// Exact input acceptance was observed.
    InputAccepted,
    /// Native retry information changed.
    Retry,
    /// A normalized diagnostic arrived.
    Diagnostic,
    /// Native parent boundary changed.
    ParentBoundary,
}

impl From<&ProviderEvent> for CoreProviderEventKind {
    /// Classify every provider observation exhaustively without retaining its content.
    fn from(event: &ProviderEvent) -> Self {
        match event {
            ProviderEvent::Settled { .. } => Self::Settled,
            ProviderEvent::SettlementFailed { .. } => Self::SettlementFailed,
            ProviderEvent::CapabilitiesChanged { .. } => Self::CapabilitiesChanged,
            ProviderEvent::SessionIdentity { .. } => Self::SessionIdentity,
            ProviderEvent::Terminals { .. } => Self::Terminals,
            ProviderEvent::Execution { .. } => Self::Execution,
            ProviderEvent::Message { .. } => Self::Message,
            ProviderEvent::Tool { .. } => Self::Tool,
            ProviderEvent::Turn { .. } => Self::Turn,
            ProviderEvent::Usage { .. } => Self::Usage,
            ProviderEvent::Runtime { .. } => Self::Runtime,
            ProviderEvent::Compaction { .. } => Self::Compaction,
            ProviderEvent::Agent { .. } => Self::Agent,
            ProviderEvent::Checklist { .. } => Self::Checklist,
            ProviderEvent::Approval { .. } => Self::Approval,
            ProviderEvent::DirectoryAccess { .. } => Self::DirectoryAccess,
            ProviderEvent::InputAccepted { .. } => Self::InputAccepted,
            ProviderEvent::Retry { .. } => Self::Retry,
            ProviderEvent::Diagnostic { .. } => Self::Diagnostic,
            ProviderEvent::ParentBoundary { .. } => Self::ParentBoundary,
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
}

impl CoreEventLog {
    /// Create lazy connection ownership without opening storage or starting another writer task.
    pub(crate) fn new(root: PathBuf) -> Self {
        Self {
            root,
            sessions: Mutex::new(HashMap::new()),
        }
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

#[cfg(test)]
mod tests {
    use super::*;

    /// A successful acceptance operation cannot erase later native execution or collection failure evidence.
    #[test]
    fn asynchronous_failure_evidence_survives_successful_input_acceptance() -> anyhow::Result<()> {
        let root = tempfile::tempdir()?;
        persist_test_session(root.path(), "session")?;
        let database = rusqlite::Connection::open(root.path().join("harness.sqlite3"))?;
        let log = CoreEventLog::new(root.path().to_owned());
        let scope = InvocationScope::new(
            forge_provider_api::ExecutionOwner::root("session"),
            "exchange",
        );
        let binding = BindingId::new("binding")?;
        log.append(
            "session",
            &CoreRecord::OperationCompleted {
                operation: CoreOperation::Provider {
                    id: OperationId::new("accepted-input")?,
                    kind: ProviderOperation::InputSubmit,
                    binding: Some(binding.clone()),
                    scope: scope.clone().into(),
                    invocation: Some(scope.clone()),
                },
                outcome: CoreOutcome::Succeeded,
            },
        )?;
        let mut completion = forge_provider_api::completion::ProviderCompletion::default();
        completion.runtime.provider = "private-runtime-payload".into();
        let observation = forge_provider_api::ExecutionObservation::new(
            scope.clone(),
            forge_provider_api::ExecutionUpdate::ResolvedWithoutExecution {
                covered_submissions: vec![forge_provider_api::SubmissionId::try_from(
                    "input".to_owned(),
                )?],
                completed_at_ms: 1,
                result: forge_provider_api::ExecutionResult::Failed {
                    completion: completion.into(),
                    failure: forge_provider_api::ExecutionFailure::Terminal {
                        kind: ProviderFailureKind::Quota,
                    },
                },
            },
        )?;
        log.append(
            "session",
            &CoreRecord::provider_event_metadata(
                binding.clone(),
                EventSequence::new(1),
                &ProviderEvent::Execution { observation },
            ),
        )?;
        let failure =
            ProviderFailure::new(ProviderOperation::Cancel, ProviderFailureKind::Transport)
                .at_phase(
                    forge_provider_api::ProviderPhase::Collection,
                    forge_provider_api::FailureDisposition::Invalidated,
                );
        log.append(
            "session",
            &CoreRecord::provider_event_metadata(
                binding,
                EventSequence::new(2),
                &ProviderEvent::SettlementFailed { scope, failure },
            ),
        )?;
        let execution: String = database.query_row(
            "SELECT record FROM owner_core_log WHERE sequence=2",
            [],
            |row| row.get(0),
        )?;
        assert!(!execution.contains("private-runtime-payload"));
        assert!(matches!(
            serde_json::from_str::<CoreRecord>(&execution)?,
            CoreRecord::ProviderEventConsumed {
                evidence: Some(CoreProviderEvidence::ExecutionFinished {
                    outcome: CoreExecutionOutcome::Failed {
                        failure: CoreExecutionFailure::Terminal {
                            kind: ProviderFailureKind::Quota
                        }
                    }
                }),
                ..
            }
        ));
        let collection: String = database.query_row(
            "SELECT record FROM owner_core_log WHERE sequence=3",
            [],
            |row| row.get(0),
        )?;
        assert!(
            matches!(serde_json::from_str::<CoreRecord>(&collection)?, CoreRecord::ProviderEventConsumed {
            evidence: Some(CoreProviderEvidence::SettlementFailed { failure: observed }), ..
        } if observed == failure)
        );
        Ok(())
    }

    /// Standalone utilities bootstrap metadata storage without hidden sessions; later durable owners share one sequence.
    #[test]
    fn standalone_metadata_bootstrap_preserves_session_ownership_and_shared_sequence()
    -> anyhow::Result<()> {
        let root = tempfile::tempdir()?;
        let log = CoreEventLog::new(root.path().to_owned());
        let record = CoreRecord::OperationStarted {
            operation: CoreOperation::Command {
                command: CommandId::new("operation")?,
                kind: CoreCommandKind::ConfigureSession,
            },
        };
        assert_eq!(log.append("session", &record)?, None);
        let database = rusqlite::Connection::open(root.path().join("harness.sqlite3"))?;
        assert_eq!(
            database.query_row("SELECT COUNT(*) FROM session_record", [], |row| row
                .get::<_, i64>(0))?,
            0
        );
        assert_eq!(
            database.query_row("SELECT COUNT(*) FROM owner_core_log", [], |row| row
                .get::<_, i64>(0))?,
            0
        );
        persist_test_session(root.path(), "session")?;
        assert_eq!(log.append("session", &record)?, Some(1));
        let mut journal = CommandJournal::open(root.path(), "session")?;
        assert_eq!(journal.append_core(&record)?, 2);
        assert_eq!(log.append("session", &record)?, Some(3));
        database.execute_batch("CREATE TRIGGER reject_native_core BEFORE INSERT ON owner_core_log BEGIN SELECT RAISE(ABORT,'fixture metadata write failure'); END;")?;
        assert!(log.append("session", &record).is_err());
        assert_eq!(
            database.query_row("SELECT MAX(sequence) FROM owner_core_log", [], |row| row
                .get::<_, i64>(0))?,
            3
        );
        Ok(())
    }
}
