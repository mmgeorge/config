//! Advance task verification only in the exchange owner between detached worker results.
use super::checks::{
    OwnedTaskChecks, TaskCheckAcknowledgment, TaskCheckFinalization, TaskCheckReport,
    TaskCheckScanResult, VerificationGateScan,
};
use super::*;
use crate::checks::owner::CheckOwnerFailure;

/// Preserve whether failure publication handled the task and which acknowledgment remains.
type TaskCheckFailurePublicationResult =
    Result<(bool, Vec<SessionEvent>, Option<TaskCheckAcknowledgment>)>;

/// Carry one immutable worker result back to the owning update step.
pub(crate) enum TaskCheckEvent {
    Gate(Box<Result<(VerificationGateScan, bool)>>),
    Scanned(Box<(OwnedTaskChecks, Result<TaskCheckScanResult>)>),
    Checked(Box<(OwnedTaskChecks, Result<check_admission::TaskCheckResult>)>),
    Postscanned(
        Box<(
            OwnedTaskChecks,
            check_admission::TaskCheckResult,
            Result<TaskCheckScanResult>,
        )>,
    ),
    Reported(Box<(TaskCheckFinalization, Result<TaskCheckReport>)>),
    Published(Box<Result<(Vec<SessionEvent>, TaskCheckAcknowledgment)>>),
    FailurePublished(Box<(anyhow::Error, TaskCheckFailurePublicationResult)>),
    UnpublishedFailure(anyhow::Error),
}

/// Release internal verification ownership only after its publication acknowledgment.
pub(crate) enum TaskCheckProgress {
    Pending,
    Finished {
        events: Vec<SessionEvent>,
        failure: Option<CheckOwnerFailure>,
    },
}

/// Retain task authority and workers independently of service request lifetimes.
pub(crate) struct OwnedTaskCheckRun {
    task: crate::task::Task,
    worker: Option<tokio::task::JoinHandle<TaskCheckEvent>>,
    cancellation: Option<check_admission::AutomaticCheckCancellation>,
    sink: publication::SessionUpdateSink,
    stopping: bool,
}

impl OwnedTaskCheckRun {
    /// Prepare verification after the owner establishes idle execution and input priority.
    pub(crate) fn prepare(
        broker: &mut HarnessBroker,
        sink: publication::SessionUpdateSink,
    ) -> Result<Option<Self>> {
        let Some(task) = broker.task_controller.current()? else {
            return Ok(None);
        };
        let gate = if task.phase() == crate::task::TaskPhase::Verify
            && task.status() == crate::task::TaskStatus::Running
        {
            broker.prepare_verification_gate()?
        } else {
            None
        };
        if gate.is_none() && !Self::eligible(broker, &task)? {
            return Ok(None);
        }
        let mut run = Self {
            task,
            worker: None,
            cancellation: None,
            sink,
            stopping: false,
        };
        if let Some(gate) = gate {
            run.worker = Some(tokio::spawn(async move {
                TaskCheckEvent::Gate(Box::new(gate.run().await))
            }));
        } else {
            run.begin(broker)?;
        }
        Ok(Some(run))
    }

    /// Await one owned worker without preventing provider events or caller admission.
    pub(crate) async fn next(&mut self) -> Option<Result<TaskCheckEvent>> {
        let worker = self.worker.as_mut()?;
        let result = worker.await.map_err(anyhow::Error::from);
        self.worker = None;
        Some(result)
    }

    /// Apply a detached result synchronously and start only its required next stage.
    pub(crate) fn update(
        &mut self,
        broker: &mut HarnessBroker,
        event: TaskCheckEvent,
    ) -> Result<TaskCheckProgress> {
        if self.stopping {
            return Ok(TaskCheckProgress::Finished {
                events: Vec::new(),
                failure: None,
            });
        }
        match event {
            TaskCheckEvent::UnpublishedFailure(error) => {
                return Ok(TaskCheckProgress::Finished {
                    events: Vec::new(),
                    failure: Some(CheckOwnerFailure::Worker(error)),
                });
            }
            TaskCheckEvent::Gate(result) => {
                let (gate, current) = match *result {
                    Ok(result) => result,
                    Err(error) => return self.fail_initial(broker, error),
                };
                broker.commit_verification_gate(gate, current)?;
                let Some(task) = broker.task_controller.current()? else {
                    return Ok(Self::finished());
                };
                if !Self::eligible(broker, &task)? {
                    return Ok(Self::finished());
                }
                self.task = task;
                self.begin(broker)?;
            }
            TaskCheckEvent::Scanned(payload) => {
                let (mut state, result) = *payload;
                let scanned = match result {
                    Ok(result) => result,
                    Err(error) => return self.fail_state(broker, &state, error),
                };
                let prepared = broker.with_publication_sink(&self.sink, |broker| {
                    broker.prepare_owned_task_check_run(&mut state, scanned)
                });
                let run = match prepared {
                    Ok(run) => run,
                    Err(error) => return self.fail_state(broker, &state, error),
                };
                self.cancellation = Some(run.cancellation());
                self.worker = Some(tokio::spawn(async move {
                    TaskCheckEvent::Checked(Box::new((state, run.run().await)))
                }));
            }
            TaskCheckEvent::Checked(payload) => {
                let (state, result) = *payload;
                self.cancellation = None;
                let checked = match result {
                    Ok(result) => result,
                    Err(error) => return self.fail_state(broker, &state, error),
                };
                let scan = match broker.prepare_task_check_postscan(&state) {
                    Ok(scan) => scan,
                    Err(error) => return self.fail_state(broker, &state, error),
                };
                self.worker = Some(tokio::spawn(async move {
                    TaskCheckEvent::Postscanned(Box::new((state, checked, scan.run().await)))
                }));
            }
            TaskCheckEvent::Postscanned(payload) => {
                let (state, checked, result) = *payload;
                let scan = match result {
                    Ok(result) => result,
                    Err(error) => return self.fail_state(broker, &state, error),
                };
                let mut finalization =
                    broker.prepare_task_check_finalization(state, checked, scan)?;
                self.worker = Some(tokio::spawn(async move {
                    let report = finalization.capture_report().await;
                    TaskCheckEvent::Reported(Box::new((finalization, report)))
                }));
            }
            TaskCheckEvent::Reported(payload) => {
                let (finalization, result) = *payload;
                let report = match result {
                    Ok(report) => report,
                    Err(error) => {
                        let prepared = broker.with_publication_sink(&self.sink, |broker| {
                            broker.prepare_task_check_report_failure(&finalization, &error)
                        })?;
                        self.worker = Some(tokio::spawn(async move {
                            TaskCheckEvent::FailurePublished(Box::new((
                                error,
                                prepared.run().await,
                            )))
                        }));
                        return Ok(TaskCheckProgress::Pending);
                    }
                };
                let publication = broker.with_publication_sink(&self.sink, |broker| {
                    broker.commit_task_check_finalization(finalization, report)
                })?;
                self.worker = Some(tokio::spawn(async move {
                    TaskCheckEvent::Published(Box::new(publication.run().await))
                }));
            }
            TaskCheckEvent::Published(result) => {
                let (mut events, acknowledgment) = (*result)?;
                broker.acknowledge_owned_task_check(acknowledgment)?;
                events.push(Self::session_changed(broker)?);
                return Ok(TaskCheckProgress::Finished {
                    events,
                    failure: None,
                });
            }
            TaskCheckEvent::FailurePublished(payload) => {
                let (error, result) = *payload;
                let (handled, mut events, acknowledgment) = result?;
                if let Some(acknowledgment) = acknowledgment {
                    broker.acknowledge_owned_task_check(acknowledgment)?;
                }
                if handled {
                    events.push(Self::session_changed(broker)?);
                }
                return Ok(TaskCheckProgress::Finished {
                    events,
                    failure: (!handled).then_some(CheckOwnerFailure::Worker(error)),
                });
            }
        }
        Ok(TaskCheckProgress::Pending)
    }

    /// Stop scheduling new stages while retaining running scans and check processes.
    pub(crate) fn cancel(&mut self) -> Result<()> {
        self.stopping = true;
        if let Some(cancellation) = &self.cancellation {
            cancellation.cancel()?;
        }
        Ok(())
    }

    /// Report whether this owner still holds an uncollected asynchronous stage.
    pub(crate) fn has_worker(&self) -> bool {
        self.worker.is_some()
    }

    /// Check task continuation authority without consulting a transient service caller.
    fn eligible(broker: &HarnessBroker, task: &crate::task::Task) -> Result<bool> {
        let Some((turn, work)) = task
            .turn
            .as_ref()
            .and_then(|turn| turn.continuation.as_ref().map(|work| (turn, work)))
        else {
            return Ok(false);
        };
        Ok(task.next_action() == crate::task::TaskNextAction::Checks
            && work.work.is_checks()
            && work.delivery == crate::task::TaskInputDelivery::Pending
            && broker
                .store
                .continuation_source_complete(&broker.session.id, &turn.id)?)
    }

    /// Capture initial scan inputs or retain an initial failure publication.
    fn begin(&mut self, broker: &mut HarnessBroker) -> Result<()> {
        match broker.begin_owned_task_checks() {
            Ok((state, scan)) => {
                self.worker = Some(tokio::spawn(async move {
                    TaskCheckEvent::Scanned(Box::new((state, scan.run().await)))
                }));
                Ok(())
            }
            Err(error) => {
                self.fail_initial(broker, error)?;
                Ok(())
            }
        }
    }

    /// Commit initial failure state before dispatching its publication job.
    fn fail_initial(
        &mut self,
        broker: &mut HarnessBroker,
        error: anyhow::Error,
    ) -> Result<TaskCheckProgress> {
        let publication = broker.with_publication_sink(&self.sink, |broker| {
            broker.prepare_task_check_start_failure(&self.task, &error)
        })?;
        let Some(publication) = publication else {
            self.worker = Some(tokio::spawn(async move {
                TaskCheckEvent::UnpublishedFailure(error)
            }));
            return Ok(TaskCheckProgress::Pending);
        };
        self.worker = Some(tokio::spawn(async move {
            TaskCheckEvent::FailurePublished(Box::new((error, publication.run().await)))
        }));
        Ok(TaskCheckProgress::Pending)
    }

    /// Commit admitted verification failure before dispatching its publication job.
    fn fail_state(
        &mut self,
        broker: &mut HarnessBroker,
        state: &OwnedTaskChecks,
        error: anyhow::Error,
    ) -> Result<TaskCheckProgress> {
        let publication = broker.with_publication_sink(&self.sink, |broker| {
            broker.prepare_owned_task_check_failure(state, &error)
        })?;
        self.worker = Some(tokio::spawn(async move {
            TaskCheckEvent::FailurePublished(Box::new((error, publication.run().await)))
        }));
        Ok(TaskCheckProgress::Pending)
    }

    /// Publish the committed task/session snapshot through the owner's retained event stream.
    fn session_changed(broker: &HarnessBroker) -> Result<SessionEvent> {
        Ok(SessionEvent {
            session_id: broker.session.id.clone(),
            event: "session_changed".into(),
            payload: serde_json::to_value(broker.snapshot()?.session)?,
        })
    }

    /// Finish a gate that no longer requires check continuation.
    fn finished() -> TaskCheckProgress {
        TaskCheckProgress::Finished {
            events: Vec::new(),
            failure: None,
        }
    }
}
