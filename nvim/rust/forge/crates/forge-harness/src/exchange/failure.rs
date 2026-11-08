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

impl OperationFailure {
    /// Classify the outcome of a command that failed while its work was stopped.
    ///
    /// A lost deadline reports a timeout, storage failures leave the outcome uncommitted, an
    /// explicit rejection keeps its own failure, and every other cause means the stop could not
    /// prove the provider idle.
    pub(super) fn into_stopped_command(self) -> StoppedCommand {
        use super::command::CommandFailure;
        match self {
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
