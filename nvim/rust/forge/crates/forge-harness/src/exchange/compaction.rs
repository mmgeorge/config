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

    /// Get the host attempt identity after confirmed compaction has committed.
    pub(super) fn completed_id(&self) -> &ContextCompactionId {
        let CompactionStage::Completed(id) = &self.stage else {
            unreachable!("only confirmed compaction can complete its caller command")
        };
        id
    }

    /// Get the active host attempt identity for correlated job/event logs.
    pub(super) fn attempt_id(&self) -> Option<&ContextCompactionId> {
        match &self.stage {
            CompactionStage::Running { id, .. }
            | CompactionStage::Observed { id, .. }
            | CompactionStage::Unobserved(id)
            | CompactionStage::Completed(id) => Some(id),
            CompactionStage::Inspect
            | CompactionStage::AwaitingCollection
            | CompactionStage::Prepare
            | CompactionStage::Failed(_)
            | CompactionStage::Consumed => None,
        }
    }

    /// Await the retained worker without blocking command admission or provider event reduction.
    pub(super) async fn next(&mut self) -> CompactionEvent {
        match &mut self.stage {
            CompactionStage::Inspect
            | CompactionStage::Prepare
            | CompactionStage::Completed(_)
            | CompactionStage::Failed(_) => CompactionEvent::Ready,
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

    /// Commit native facts and select the next compaction stage in the exchange owner.
    pub(super) fn update(
        &mut self,
        event: CompactionEvent,
        broker: &mut HarnessBroker,
        can_commit: bool,
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
                        .prepare_owned_compaction(CompactionProvenance::Manual)
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
                CompactionStage::Failed(error) => Err(*error),
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
                        CompactionStage::Failed(Box::new(OperationFailure::Compaction {
                            failure,
                            cause: crate::provider::lifecycle_error::classify(
                                forge_provider_api::ProviderOperation::ThreadCompact,
                                error,
                            ),
                        }))
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
            | CompactionStage::Failed(_)
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

/// Distinguish collection admission, committed publications, and confirmed compaction history.
pub(super) enum CompactionProgress {
    Collect(ChildCollectionPolicy),
    Waiting { updates: Vec<SessionUpdate> },
    Committed { updates: Vec<SessionUpdate> },
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
    Failed(Box<OperationFailure>),
    Consumed,
}
