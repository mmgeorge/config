//! Share typed operation failure classes across caller commands and internal input work.

use super::{command::ContextCompactionFailure, transition::TransitionFailure};

/// Preserve typed collection/dispatch uncertainty separately from a legacy broker commit cause.
#[derive(Debug, thiserror::Error)]
pub(super) enum OperationFailure {
    /// Native receipt evidence could not be reconciled under its admitted generation.
    #[error("input receipt reconciliation failed")]
    InputReconciliation(#[source] crate::provider::ProviderError),
    #[error("context compaction failed: {failure}")]
    Compaction {
        failure: ContextCompactionFailure,
        #[source]
        cause: crate::provider::ProviderError,
    },
    #[error(transparent)]
    Transition(#[from] TransitionFailure),
    #[error("owner operation state could not be committed")]
    Domain(#[source] anyhow::Error),
    #[error("the provider worker ended before it confirmed the dispatch")]
    DispatchLost,
    #[error("owner operation cannot start another effect after storage certification failed")]
    StorageUncommitted,
    #[error("owner operation exceeded its observation deadline")]
    JobDeadline,
    #[error("owner operation worker observation was lost")]
    JobObservationLost(#[source] tokio::task::JoinError),
    #[error("owner operation was rejected before execution was changed")]
    Validation(#[source] anyhow::Error),
    #[error("execution admission became unavailable after collection")]
    ExecutionUnavailable,
    #[error("provider settings reconciliation failed")]
    Reconciliation(#[source] crate::provider::ProviderError),
}

/// Describe how a command whose work had to be stopped finishes.
pub(super) enum StoppedCommand {
    /// The command finishes with this typed failure.
    Failed(super::command::CommandFailure),
    /// Owner storage could not commit the command's outcome.
    Uncommitted,
}

/// Mark a domain step that could not run because owner storage already failed.
#[derive(Debug, thiserror::Error)]
#[error("owner storage is unavailable")]
pub(super) struct OwnerStorageUnavailable;

/// Report whether a domain error came from owner storage rather than from domain validation.
///
/// Storage failure fences the session for its lifetime, because no later command can certify its
/// outcome. A domain rule that refused the command, such as continuing a task that is not
/// running, leaves storage healthy, so classifying it as storage failure made one rejected
/// command disable the whole session. Only SQLite, file I/O, and the owner's storage error types
/// count as storage; everything else is a rejection of this command.
/// Report whether a failure says the provider may hold an effect the host did not commit.
fn delivery_uncertain(error: &anyhow::Error) -> bool {
    matches!(
        error.downcast_ref::<crate::provider::ProviderError>(),
        Some(crate::provider::ProviderError::DeliveryUncertain)
    )
}

pub(super) fn storage_failure(error: &anyhow::Error) -> bool {
    error.chain().any(|cause| {
        cause.is::<rusqlite::Error>()
            || cause.is::<std::io::Error>()
            || cause.is::<crate::storage::database::DatabaseError>()
            || cause.is::<crate::storage::writer::WriteBehindFailure>()
            || cause.is::<super::journal::CommandJournalError>()
            || cause.is::<super::input_receipts::InputReceiptError>()
            || cause.is::<OwnerStorageUnavailable>()
            || cause
                .downcast_ref::<super::command::TaskCommandFailure>()
                .is_some_and(|failure| {
                    matches!(
                        failure,
                        super::command::TaskCommandFailure::StateUnavailable
                    )
                })
    })
}

impl OperationFailure {
    /// Classify the outcome of a command that failed while its work was stopped.
    ///
    /// A lost deadline reports a timeout, contention that rolled the outcome back reports a
    /// retryable busy failure, other storage failures leave the outcome uncommitted, an
    /// explicit rejection keeps its own failure, a domain rule that refused the command reports
    /// a failed prerequisite, and every other cause means the stop could not prove the provider
    /// idle.
    pub(super) fn into_stopped_command(self) -> StoppedCommand {
        use super::command::CommandFailure;
        match self {
            // Contention rolled the write back, so the journal records a failure the user can
            // retry instead of fencing the whole queue as if its state were unknown. A busy
            // write that left delivery uncertain already committed part of its outcome, so it
            // stays uncommitted below.
            Self::Domain(error) | Self::Transition(TransitionFailure::Domain(error))
                if crate::storage::database::storage_busy(&error)
                    && !delivery_uncertain(&error) =>
            {
                StoppedCommand::Failed(CommandFailure::StorageBusy)
            }
            Self::Domain(error) | Self::Transition(TransitionFailure::Domain(error))
                if !storage_failure(&error) =>
            {
                StoppedCommand::Failed(CommandFailure::PrerequisiteFailed)
            }
            Self::Domain(_)
            | Self::StorageUncommitted
            | Self::Transition(TransitionFailure::Domain(_)) => StoppedCommand::Uncommitted,
            Self::Transition(TransitionFailure::Rejected(failure)) => {
                StoppedCommand::Failed(failure)
            }
            Self::JobDeadline | Self::Transition(TransitionFailure::Overdue) => {
                StoppedCommand::Failed(CommandFailure::StopTimedOut)
            }
            Self::InputReconciliation(_)
            | Self::Compaction { .. }
            | Self::DispatchLost
            | Self::JobObservationLost(_)
            | Self::Validation(_)
            | Self::ExecutionUnavailable
            | Self::Reconciliation(_)
            | Self::Transition(TransitionFailure::Uncertain) => {
                StoppedCommand::Failed(CommandFailure::StopFailed)
            }
        }
    }

    /// Get the SQLite or filesystem cause of a storage failure as a stable code.
    pub(super) fn storage_cause(&self) -> Option<&'static str> {
        match self {
            Self::Domain(error) | Self::Transition(TransitionFailure::Domain(error)) => {
                crate::storage::database::storage_cause(error)
            }
            _ => None,
        }
    }

    /// Get a stable host classification without inspecting provider or diagnostic text.
    pub(super) fn code(&self) -> &'static str {
        match self {
            Self::InputReconciliation(_) => "input_reconciliation_failed",
            Self::Compaction { failure, .. } => failure.code(),
            Self::Transition(TransitionFailure::Rejected(failure)) => failure.code(),
            Self::Transition(TransitionFailure::Overdue) => "execution_collection_timeout",
            Self::Transition(TransitionFailure::Uncertain) => "execution_collection_uncertain",
            Self::Transition(TransitionFailure::Domain(_)) | Self::Domain(_) => {
                "operation_state_commit_failed"
            }
            Self::DispatchLost => "provider_dispatch_unconfirmed",
            Self::StorageUncommitted => "operation_state_uncommitted",
            Self::JobDeadline => "operation_timeout",
            Self::JobObservationLost(_) => "operation_observation_lost",
            Self::Validation(source) => {
                if let Some(reason) =
                    source.downcast_ref::<super::command::ConfigurationRejection>()
                {
                    reason.code()
                } else if let Some(reason) = source.downcast_ref::<super::command::CommandFailure>()
                {
                    reason.code()
                } else {
                    "operation_rejected"
                }
            }
            Self::ExecutionUnavailable => "session_ownership_unavailable",
            Self::Reconciliation(_) => "configuration_reconciliation_failed",
        }
    }

    /// Render a bounded user-facing reason from normalized failure classes at publication.
    pub(super) fn diagnostic(&self) -> String {
        let reason = match self {
            Self::InputReconciliation(error) => {
                let diagnostic = crate::provider::lifecycle_error::diagnostic(
                    forge_provider_api::ProviderOperation::TurnStart,
                    error,
                );
                format!(
                    "Input receipt reconciliation failed ({:?})",
                    diagnostic.kind()
                )
            }
            Self::Validation(source) => {
                if let Some(reason) =
                    source.downcast_ref::<super::command::ConfigurationRejection>()
                {
                    reason.to_string()
                } else if let Some(reason) = source.downcast_ref::<super::command::CommandFailure>()
                {
                    reason.to_string()
                } else {
                    self.to_string()
                }
            }
            Self::Reconciliation(error) => {
                let diagnostic = crate::provider::lifecycle_error::diagnostic(
                    forge_provider_api::ProviderOperation::ThreadConfigure,
                    error,
                );
                format!(
                    "Provider settings application failed ({:?})",
                    diagnostic.kind()
                )
            }
            Self::Transition(TransitionFailure::Domain(_)) => {
                "Owner collection state could not be committed".into()
            }
            Self::Compaction { .. }
            | Self::Transition(
                TransitionFailure::Rejected(_)
                | TransitionFailure::Overdue
                | TransitionFailure::Uncertain,
            )
            | Self::Domain(_)
            | Self::DispatchLost
            | Self::StorageUncommitted
            | Self::JobDeadline
            | Self::JobObservationLost(_)
            | Self::ExecutionUnavailable => self.to_string(),
        };
        format!("{reason} ({})", self.code())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn busy() -> anyhow::Error {
        rusqlite::Error::SqliteFailure(
            rusqlite::ffi::Error::new(rusqlite::ffi::SQLITE_BUSY),
            Some("database is locked".into()),
        )
        .into()
    }

    /// Contention that rolled the outcome back is a retryable failure, not unknown state that
    /// would close the command queue for the session's lifetime.
    #[test]
    fn rolled_back_contention_finishes_the_command_as_busy() {
        assert!(matches!(
            OperationFailure::Domain(busy()).into_stopped_command(),
            StoppedCommand::Failed(super::super::command::CommandFailure::StorageBusy)
        ));
        assert_eq!(
            OperationFailure::Domain(busy()).storage_cause(),
            Some("sqlite_busy")
        );
    }

    /// A busy write after the host half committed leaves the outcome uncommitted for recovery.
    #[test]
    fn contention_with_uncertain_delivery_stays_uncommitted() {
        let error = busy().context(crate::provider::ProviderError::DeliveryUncertain);
        assert!(matches!(
            OperationFailure::Domain(error).into_stopped_command(),
            StoppedCommand::Uncommitted
        ));
        let corrupt: anyhow::Error = rusqlite::Error::SqliteFailure(
            rusqlite::ffi::Error::new(rusqlite::ffi::SQLITE_CORRUPT),
            None,
        )
        .into();
        assert!(matches!(
            OperationFailure::Domain(corrupt).into_stopped_command(),
            StoppedCommand::Uncommitted
        ));
    }
}
