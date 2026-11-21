//! Retain context compaction until its correlated native result is committed by the owner.

use super::command::{CommandFailure, ContextCompactionFailure, ContextCompactionId};
use super::failure::OperationFailure;
use super::transition::{ChildCollectionPolicy, RetainedTransition, TransitionFailure};
use crate::broker::publication::SessionUpdate;
use crate::broker::{
    CompactionEvidenceError, CompactionProvenance, CompactionResult, HarnessBroker,
};

/// Own one manual compaction's preflight, native work, and confirmed terminal history.
pub(super) struct ContextCompactionRun {
    stage: CompactionStage,
}

impl ContextCompactionRun {
    /// Create a read-only preflight stage before exchange collection is authorized.
    pub(super) fn new() -> Self {
        Self {
            stage: CompactionStage::Inspect,
        }
    }

    /// Advance only after native and host collection establish safe compaction admission.
    pub(super) fn collected(&mut self) {
        assert!(matches!(self.stage, CompactionStage::AwaitingCollection));
        self.stage = CompactionStage::Prepare;
    }

    /// Report how a committed attempt ended: its host identity once the provider confirmed the
    /// compaction, or the failure its caller reports when the provider answered without
    /// compacting.
    pub(super) fn outcome(&self) -> Result<&ContextCompactionId, UnsuccessfulCompaction> {
        match &self.stage {
            CompactionStage::Completed(id) => Ok(id),
            CompactionStage::Unsuccessful { failure, cause, .. } => Err(UnsuccessfulCompaction {
                failure: failure.clone(),
                cause: cause.clone(),
            }),
            _ => unreachable!("only a committed attempt can complete its caller command"),
        }
    }

    /// Report whether the attempt has yet to start: the command is checking availability,
    /// collecting the exchange it stops, or preparing its job.
    pub(super) fn awaiting_start(&self) -> bool {
        matches!(
            self.stage,
            CompactionStage::Inspect
                | CompactionStage::AwaitingCollection
                | CompactionStage::Prepare
        )
    }

    /// Get the active host attempt identity for correlated job/event logs.
    pub(super) fn attempt_id(&self) -> Option<&ContextCompactionId> {
        match &self.stage {
            CompactionStage::Running { id, .. }
            | CompactionStage::Observed { id, .. }
            | CompactionStage::Unobserved(id)
            | CompactionStage::Completed(id)
            | CompactionStage::Unsuccessful { id, .. } => Some(id),
            CompactionStage::Inspect
            | CompactionStage::AwaitingCollection
            | CompactionStage::Prepare
            | CompactionStage::Consumed => None,
        }
    }

    /// Await the retained worker without blocking command admission or provider event reduction.
    pub(super) async fn next(&mut self) -> CompactionEvent {
        match &mut self.stage {
            CompactionStage::Inspect
            | CompactionStage::Prepare
            | CompactionStage::Completed(_)
            | CompactionStage::Unsuccessful { .. } => CompactionEvent::Ready,
            CompactionStage::Running { task, deadline, .. } => tokio::select! {
                result = task => CompactionEvent::Completed(result.map(Box::new)),
                _ = tokio::time::sleep_until(*deadline) => CompactionEvent::Deadline,
            },
            CompactionStage::AwaitingCollection
            | CompactionStage::Observed { .. }
            | CompactionStage::Unobserved(_)
            | CompactionStage::Consumed => std::future::pending().await,
        }
    }

    /// Check whether the next fact would start collection or reserve a new native request.
    pub(super) fn can_abandon_before_dispatch(&self, event: &CompactionEvent) -> bool {
        matches!(event, CompactionEvent::Ready)
            && matches!(
                self.stage,
                CompactionStage::Inspect | CompactionStage::Prepare
            )
    }

    /// Commit native facts and select the next compaction stage in the exchange owner.
    ///
    /// `interrupted` is the exchange this command's collection stopped, which records the
    /// attempt and runs again after it.
    pub(super) fn update(
        &mut self,
        event: CompactionEvent,
        broker: &mut HarnessBroker,
        can_commit: bool,
        interrupted: Option<&crate::provider::ExchangeId>,
    ) -> Result<CompactionProgress, OperationFailure> {
        if matches!(event, CompactionEvent::Deadline) {
            return Err(OperationFailure::Compaction {
                failure: ContextCompactionFailure::TimedOut,
                cause: crate::provider::ProviderError::DeliveryUncertain,
            });
        }
        let stage = std::mem::replace(&mut self.stage, CompactionStage::Consumed);
        if !can_commit {
            self.stage = match (stage, event) {
                (CompactionStage::Running { id, .. }, CompactionEvent::Completed(Ok(result))) => {
                    CompactionStage::Observed { id, result }
                }
                (CompactionStage::Running { id, .. }, CompactionEvent::Completed(Err(_))) => {
                    CompactionStage::Unobserved(id)
                }
                (stage, _) => stage,
            };
            return Err(OperationFailure::StorageUncommitted);
        }
        match event {
            CompactionEvent::Ready => match stage {
                CompactionStage::Inspect => {
                    broker.owner_compaction_available().map_err(|failure| {
                        OperationFailure::Transition(TransitionFailure::Rejected(
                            CommandFailure::Compaction(failure),
                        ))
                    })?;
                    self.stage = CompactionStage::AwaitingCollection;
                    Ok(CompactionProgress::Collect(ChildCollectionPolicy::Preserve))
                }
                CompactionStage::Prepare => {
                    let (job, started) = broker
                        .prepare_owned_compaction(CompactionProvenance::Manual, interrupted)
                        .map_err(OperationFailure::Domain)?;
                    let id = ContextCompactionId::new(job.id())
                        .expect("host compaction UUIDs are nonempty");
                    self.stage = CompactionStage::Running {
                        id,
                        task: tokio::spawn(job.run()),
                        deadline: tokio::time::Instant::now() + std::time::Duration::from_secs(120),
                    };
                    Ok(CompactionProgress::Waiting {
                        updates: started.into_updates(),
                    })
                }
                CompactionStage::Completed(id) => {
                    self.stage = CompactionStage::Completed(id);
                    Ok(CompactionProgress::Committed {
                        updates: Vec::new(),
                    })
                }
                // The provider answered and its terminal record is saved, so the native side
                // is idle and the turn the compaction stopped can run again as it does after a
                // confirmed compaction; the caller still learns why nothing was compacted.
                stage @ CompactionStage::Unsuccessful { .. } => {
                    self.stage = stage;
                    Ok(CompactionProgress::Unsuccessful {
                        updates: Vec::new(),
                    })
                }
                CompactionStage::AwaitingCollection
                | CompactionStage::Running { .. }
                | CompactionStage::Observed { .. }
                | CompactionStage::Unobserved(_)
                | CompactionStage::Consumed => {
                    unreachable!("only synchronous compaction stages produce ready facts")
                }
            },
            CompactionEvent::Completed(result) => {
                let CompactionStage::Running {
                    id,
                    task: _,
                    deadline: _,
                } = stage
                else {
                    unreachable!("compaction facts belong to the active retained job")
                };
                let result = match result {
                    Ok(result) => result,
                    Err(error) => {
                        self.stage = CompactionStage::Unobserved(id);
                        return Err(OperationFailure::JobObservationLost(error));
                    }
                };
                let retained = result.clone();
                let (delivery, error, provenance) = match broker.commit_owned_compaction(*result) {
                    Ok(committed) => committed,
                    Err(error) => {
                        self.stage = CompactionStage::Observed {
                            id,
                            result: retained,
                        };
                        return Err(OperationFailure::Domain(error));
                    }
                };
                assert!(matches!(provenance, CompactionProvenance::Manual));
                self.stage = match error {
                    Some(error) => {
                        let failure = match error.downcast_ref::<CompactionEvidenceError>() {
                            Some(
                                CompactionEvidenceError::AuthorityChanged
                                | CompactionEvidenceError::ConversationChanged,
                            ) => ContextCompactionFailure::AuthorityChanged,
                            Some(CompactionEvidenceError::Unconfirmed) => {
                                ContextCompactionFailure::Unconfirmed
                            }
                            Some(CompactionEvidenceError::Declined { explanation }) => {
                                ContextCompactionFailure::Declined {
                                    explanation: explanation.clone(),
                                }
                            }
                            Some(CompactionEvidenceError::Failed { kind }) => {
                                ContextCompactionFailure::Provider { kind: *kind }
                            }
                            None => ContextCompactionFailure::Provider {
                                kind: crate::provider::lifecycle_error::diagnostic(
                                    forge_provider_api::ProviderOperation::ThreadCompact,
                                    error.as_ref(),
                                )
                                .kind(),
                            },
                        };
                        CompactionStage::Unsuccessful {
                            id,
                            failure,
                            cause: crate::provider::lifecycle_error::classify(
                                forge_provider_api::ProviderOperation::ThreadCompact,
                                error,
                            ),
                        }
                    }
                    None => CompactionStage::Completed(id),
                };
                Ok(CompactionProgress::Waiting {
                    updates: delivery.into_updates(),
                })
            }
            CompactionEvent::Deadline => {
                unreachable!("deadline handling retains the actual native task above")
            }
        }
    }

    /// Transfer timed-out work or uncommitted facts without losing durable terminal history.
    pub(super) fn retain(self, transition: &mut RetainedTransition) {
        match self.stage {
            CompactionStage::Running { id, task, .. } => transition.retain_compaction(id, task),
            CompactionStage::Observed { id: _, result } => {
                transition.retain_compaction_result(*result)
            }
            CompactionStage::Unobserved(id) => transition.retain_lost_compaction(id),
            CompactionStage::Inspect
            | CompactionStage::AwaitingCollection
            | CompactionStage::Prepare
            | CompactionStage::Completed(_)
            | CompactionStage::Unsuccessful { .. }
            | CompactionStage::Consumed => {}
        }
    }
}

/// Correlate one compaction worker completion or observation deadline with its owner stage.
pub(super) enum CompactionEvent {
    Ready,
    Completed(Result<Box<CompactionResult>, tokio::task::JoinError>),
    Deadline,
}

impl CompactionEvent {
    /// Get a bounded log label without native payloads or context content.
    pub(super) fn name(&self) -> &'static str {
        match self {
            Self::Ready => "compaction_ready",
            Self::Completed(_) => "compaction_result_received",
            Self::Deadline => "compaction_deadline",
        }
    }
}

/// Distinguish collection admission, committed publications, and terminal compaction history.
pub(super) enum CompactionProgress {
    Collect(ChildCollectionPolicy),
    Waiting {
        updates: Vec<SessionUpdate>,
    },
    /// The provider confirmed the compaction.
    Committed {
        updates: Vec<SessionUpdate>,
    },
    /// The provider answered without compacting, and that answer is saved; the owner hands any
    /// stopped turn back exactly as after `Committed`, then reports `outcome`'s failure.
    Unsuccessful {
        updates: Vec<SessionUpdate>,
    },
}

/// Carry why a committed attempt did not compact: the caller's failure and its classified cause.
pub(super) struct UnsuccessfulCompaction {
    pub(super) failure: ContextCompactionFailure,
    pub(super) cause: crate::provider::ProviderError,
}

impl From<UnsuccessfulCompaction> for OperationFailure {
    fn from(unsuccessful: UnsuccessfulCompaction) -> Self {
        OperationFailure::Compaction {
            failure: unsuccessful.failure,
            cause: unsuccessful.cause,
        }
    }
}

enum CompactionStage {
    Inspect,
    AwaitingCollection,
    Prepare,
    Running {
        id: ContextCompactionId,
        task: tokio::task::JoinHandle<CompactionResult>,
        deadline: tokio::time::Instant,
    },
    Observed {
        id: ContextCompactionId,
        result: Box<CompactionResult>,
    },
    Unobserved(ContextCompactionId),
    Completed(ContextCompactionId),
    /// The provider's terminal answer is saved and says nothing was compacted.
    Unsuccessful {
        id: ContextCompactionId,
        failure: ContextCompactionFailure,
        cause: crate::provider::ProviderError,
    },
    Consumed,
}
