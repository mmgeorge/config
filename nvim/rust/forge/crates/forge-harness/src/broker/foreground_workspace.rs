//! Capture admission checkpoints outside broker ownership and commit under task authority.

use super::foreground_dispatch::ForegroundAdmission;
use super::*;

/// Own the initial barrier or the task's first workspace observation.
pub(crate) struct AdmissionWorkspaceJob {
    session_id: String,
    workspace: String,
    exchange_id: String,
    work: WorkspaceWork,
}

enum WorkspaceWork {
    Initial(crate::checkpoint::CheckpointTask),
    Task {
        capture: Box<execution::WorkspaceCaptureJob>,
        permit: crate::task::TaskPermit,
        purpose: WorkspacePurpose,
    },
}

enum WorkspacePurpose {
    ExecutionBaseline { execution_id: String },
    GoalObservation,
}

/// Retain capture identity even when checkpoint acquisition fails.
pub(crate) struct AdmissionWorkspaceResult {
    session_id: String,
    workspace: String,
    exchange_id: String,
    owner: WorkspaceOwner,
    checkpoint: Result<crate::checkpoint::CheckpointRecord>,
}

enum WorkspaceOwner {
    Initial,
    Task {
        permit: crate::task::TaskPermit,
        purpose: WorkspacePurpose,
    },
}

impl AdmissionWorkspaceJob {
    /// Collect the owned capture before returning its identity and outcome to the broker.
    pub(crate) async fn run(self) -> AdmissionWorkspaceResult {
        let Self {
            session_id,
            workspace,
            exchange_id,
            work,
        } = self;
        let (owner, checkpoint) = match work {
            WorkspaceWork::Initial(mut task) => {
                let checkpoint = (&mut task.0)
                    .await
                    .context("initial checkpoint worker failed")
                    .and_then(|result| result.map_err(anyhow::Error::new));
                (WorkspaceOwner::Initial, checkpoint)
            }
            WorkspaceWork::Task {
                capture,
                permit,
                purpose,
            } => (
                WorkspaceOwner::Task { permit, purpose },
                capture.run().await,
            ),
        };
        AdmissionWorkspaceResult {
            session_id,
            workspace,
            exchange_id,
            owner,
            checkpoint,
        }
    }
}

impl HarnessBroker {
    /// Prepare the next required workspace barrier before provider submission.
    pub(crate) fn prepare_foreground_workspace(
        &mut self,
        admission: &mut ForegroundAdmission,
    ) -> Result<Option<ForegroundAction>> {
        if admission.workspace_prepared {
            return Ok(None);
        }
        let staged = admission
            .staged_interaction
            .as_ref()
            .context("workspace preparation has no exchange")?;
        let runtime = self
            .exchange_runtime
            .as_mut()
            .context("workspace preparation has no runtime")?;
        anyhow::ensure!(
            runtime.exchange_id == staged.exchange.id,
            "workspace preparation crosses exchange ownership"
        );
        let work = if staged.mode == PromptMode::ExecutePlan && runtime.checkpoint.is_some() {
            WorkspaceWork::Initial(runtime.checkpoint.take().expect("checkpoint checked above"))
        } else {
            let Some(task) = self.task_controller.current()? else {
                return Ok(None);
            };
            let purpose = match &task.workflow {
                crate::task::WorkflowSnapshot::Execute { execution }
                    if staged.mode == PromptMode::ExecutePlan
                        && execution.record.baseline_checkpoint.is_none() =>
                {
                    WorkspacePurpose::ExecutionBaseline {
                        execution_id: execution.record.id.clone(),
                    }
                }
                crate::task::WorkflowSnapshot::Goal { goal }
                    if goal.observed_checkpoint.is_none() =>
                {
                    WorkspacePurpose::GoalObservation
                }
                crate::task::WorkflowSnapshot::Execute { .. }
                | crate::task::WorkflowSnapshot::Goal { .. }
                | crate::task::WorkflowSnapshot::Plan { .. } => return Ok(None),
            };
            WorkspaceWork::Task {
                capture: Box::new(self.prepare_workspace_capture()),
                permit: task.permit(),
                purpose,
            }
        };
        let job = AdmissionWorkspaceJob {
            session_id: self.session.id.clone(),
            workspace: self.session.workspace.clone(),
            exchange_id: staged.exchange.id.clone(),
            work,
        };
        Ok(Some(ForegroundAction::Effect(
            ForegroundJob::AdmissionWorkspace(job),
        )))
    }

    /// Commit one capture only while its session, exchange, and admitting task remain current.
    pub(crate) fn complete_foreground_workspace(
        &mut self,
        admission: &mut ForegroundAdmission,
        result: AdmissionWorkspaceResult,
    ) -> Result<()> {
        let AdmissionWorkspaceResult {
            session_id,
            workspace,
            exchange_id,
            owner,
            checkpoint,
        } = result;
        let staged = admission
            .staged_interaction
            .as_mut()
            .context("workspace completion has no exchange")?;
        anyhow::ensure!(
            session_id == self.session.id
                && workspace == self.session.workspace
                && exchange_id == staged.exchange.id,
            "workspace capture authority changed before commit"
        );
        match owner {
            WorkspaceOwner::Initial => {
                let mut runtime = self
                    .exchange_runtime
                    .take()
                    .context("initial checkpoint runtime is missing")?;
                let result = if runtime.exchange_id == exchange_id {
                    let checkpoint = (|| {
                        if let Some(permit) = &runtime.task_permit {
                            permit.validate(self.task_controller.current()?.as_ref())?;
                        }
                        checkpoint
                    })();
                    self.finish_initial_checkpoint(&mut runtime, &mut staged.exchange, checkpoint)
                } else {
                    Err(anyhow::anyhow!(
                        "initial checkpoint owner changed before commit"
                    ))
                };
                self.exchange_runtime = Some(runtime);
                result
            }
            WorkspaceOwner::Task { permit, purpose } => {
                let task = self
                    .task_controller
                    .current()?
                    .context("workspace task was removed")?;
                permit.validate(Some(&task))?;
                let event: fn(String) -> crate::task::TaskEvent = match purpose {
                    WorkspacePurpose::ExecutionBaseline { execution_id } => {
                        let crate::task::WorkflowSnapshot::Execute { execution } = &task.workflow
                        else {
                            return Err(crate::task::StaleTask.into());
                        };
                        anyhow::ensure!(
                            execution.record.id == execution_id
                                && execution.record.baseline_checkpoint.is_none(),
                            "execution baseline changed during capture"
                        );
                        crate::task::TaskEvent::ExecutionBaselineCaptured
                    }
                    WorkspacePurpose::GoalObservation => {
                        anyhow::ensure!(
                            matches!(&task.workflow, crate::task::WorkflowSnapshot::Goal { goal }
                            if goal.observed_checkpoint.is_none()),
                            "goal observation changed during capture"
                        );
                        crate::task::TaskEvent::GoalWorkspaceObserved
                    }
                };
                let checkpoint = checkpoint?;
                self.store.save_checkpoint(&checkpoint)?;
                self.task_controller.apply(
                    &mut self.store,
                    &permit,
                    event(checkpoint.id),
                    self.clock.now_ms(),
                )?;
                admission.workspace_prepared = true;
                Ok(())
            }
        }
    }
}
