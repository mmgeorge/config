//! Retain automatic check stages inside the exchange owner between detached job completions.

use super::*;
use crate::checks::owner::{AutomaticCheckRequest, CheckCompletionOutcome, CheckOwnerFailure};

/// Identify a detached automatic-check result without allowing workers to mutate broker state.
pub(crate) enum OwnedCheckResult {
    Refresh(Result<Box<check_admission::AutomaticCheckRefreshResult>>),
    Run(Result<Box<check_admission::AutomaticCheckResult>>),
    Published(Result<(Value, Vec<SessionEvent>)>),
}

/// Request the next owner-controlled transition or wait without occupying a caller-command slot.
pub(crate) enum OwnedCheckProgress {
    Pending,
    Revalidate,
    Repair {
        prompt: forge_provider_api::forge::CheckRepair,
        change_owner: String,
    },
    Ready(CheckCompletionOutcome),
    Finished,
}

/// Keep transaction, publication and worker collection ownership together until completion.
pub(crate) struct OwnedCheckCompletion {
    transaction: Option<check_admission::AutomaticCheckTransaction>,
    action: Option<AutomaticCheckAction>,
    worker: Option<tokio::task::JoinHandle<OwnedCheckResult>>,
    reply: Option<
        tokio::sync::oneshot::Sender<
            std::result::Result<CheckCompletionOutcome, CheckOwnerFailure>,
        >,
    >,
    sink: publication::SessionUpdateSink,
    cancellation: Option<check_admission::AutomaticCheckCancellation>,
    stopping: bool,
}

impl OwnedCheckCompletion {
    /// Begin automatic completion only after the owner validates execution and resource availability.
    pub(crate) fn start(
        broker: &mut HarnessBroker,
        request: AutomaticCheckRequest,
        sink: publication::SessionUpdateSink,
    ) -> Result<Self> {
        let (transaction, action) = broker.begin_check_completion(sink.clone())?;
        Ok(Self {
            transaction,
            action: Some(action),
            worker: None,
            reply: Some(request.reply),
            sink,
            cancellation: None,
            stopping: false,
        })
    }

    /// Start one detached stage or yield the exact transition that requires foreground ownership.
    pub(crate) fn advance(&mut self) -> Result<OwnedCheckProgress> {
        if self.worker.is_some() {
            return Ok(OwnedCheckProgress::Pending);
        }
        if self.stopping {
            self.fail(anyhow::anyhow!("automatic checks were interrupted"));
            return Ok(OwnedCheckProgress::Finished);
        }
        let Some(action) = self.action.take() else {
            return Ok(OwnedCheckProgress::Pending);
        };
        match action {
            AutomaticCheckAction::Finish((value, events)) => {
                Ok(OwnedCheckProgress::Ready(CheckCompletionOutcome {
                    value,
                    events,
                }))
            }
            AutomaticCheckAction::Refresh(job) => {
                self.worker = Some(tokio::spawn(async move {
                    OwnedCheckResult::Refresh(job.run().await.map(Box::new))
                }));
                Ok(OwnedCheckProgress::Pending)
            }
            AutomaticCheckAction::Run(job) => {
                self.cancellation = Some(job.cancellation());
                self.worker = Some(tokio::spawn(async move {
                    OwnedCheckResult::Run(job.run().await.map(Box::new))
                }));
                Ok(OwnedCheckProgress::Pending)
            }
            AutomaticCheckAction::Publish { update, result } => {
                let sink = self.sink.clone();
                self.worker = Some(tokio::spawn(async move {
                    OwnedCheckResult::Published(
                        async {
                            sink.send_wait(update).await?;
                            Ok(result)
                        }
                        .await,
                    )
                }));
                Ok(OwnedCheckProgress::Pending)
            }
            AutomaticCheckAction::Revalidate => Ok(OwnedCheckProgress::Revalidate),
            AutomaticCheckAction::Repair {
                prompt,
                change_owner,
            } => Ok(OwnedCheckProgress::Repair {
                prompt,
                change_owner,
            }),
        }
    }

    /// Await only the owned worker; the caller must continue servicing provider and command inputs.
    pub(crate) async fn next(&mut self) -> Option<Result<OwnedCheckResult>> {
        let worker = self.worker.as_mut()?;
        let result = worker.await.map_err(anyhow::Error::from);
        self.worker = None;
        Some(result)
    }

    /// Apply a completed worker result synchronously in the owner's update phase.
    pub(crate) fn apply(
        &mut self,
        broker: &mut HarnessBroker,
        result: OwnedCheckResult,
    ) -> Result<()> {
        if self.stopping {
            return Ok(());
        }
        let transaction = self
            .transaction
            .as_mut()
            .context("automatic check transaction is missing")?;
        self.action = Some(match result {
            OwnedCheckResult::Refresh(result) => {
                broker.apply_automatic_check_refresh(transaction, *result?)?
            }
            OwnedCheckResult::Run(result) => {
                broker.apply_automatic_check_result(transaction, *result?)?
            }
            OwnedCheckResult::Published(result) => AutomaticCheckAction::Finish(result?),
        });
        Ok(())
    }

    /// Revalidate only after the owner has collected reopened execution and committed its settlement.
    pub(crate) fn revalidate(&mut self, broker: &mut HarnessBroker) -> Result<()> {
        self.action = Some(
            broker.revalidate_automatic_checks(
                self.transaction
                    .as_mut()
                    .context("automatic check transaction is missing")?,
            )?,
        );
        Ok(())
    }

    /// Apply the owner's completed repair foreground result without spawning a nested caller command.
    pub(crate) fn repaired(
        &mut self,
        broker: &mut HarnessBroker,
        result: (Value, Vec<SessionEvent>),
    ) -> Result<()> {
        self.action = Some(
            broker.apply_automatic_check_repair(
                self.transaction
                    .as_mut()
                    .context("automatic check transaction is missing")?,
                result,
            )?,
        );
        Ok(())
    }

    /// Request interruption while retaining any running worker until actual collection.
    pub(crate) fn cancel(&mut self) -> Result<()> {
        self.stopping = true;
        if let Some(cancellation) = &self.cancellation {
            cancellation.cancel()?;
        }
        Ok(())
    }

    /// Resolve completion only after the owner retains the ordered terminal publication batch.
    pub(crate) fn complete(&mut self, outcome: CheckCompletionOutcome) {
        if let Some(reply) = self.reply.take() {
            let _ = reply.send(Ok(outcome));
        }
    }

    /// Fail the response after collection; dropping a waiter never cancels the retained operation.
    pub(crate) fn fail(&mut self, error: anyhow::Error) {
        if let Some(reply) = self.reply.take() {
            let _ = reply.send(Err(CheckOwnerFailure::Worker(error)));
        }
    }

    /// Report whether dropping this state would lose an uncollected worker.
    pub(crate) fn has_worker(&self) -> bool {
        self.worker.is_some()
    }
}
