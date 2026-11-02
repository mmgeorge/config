use super::*;

/// Capture workspace evidence while retaining the exact host-effect reservation.
pub(crate) struct GoalProgressJob {
    pub(super) capture: execution::WorkspaceCaptureJob,
    pub(crate) previous: Option<crate::checkpoint::CheckpointRecord>,
    pub(crate) previous_id: Option<String>,
    pub(crate) permit: crate::task::TaskPermit,
    pub(crate) guard: crate::provider::session_execution::HostEffectGuard,
}

pub(crate) struct GoalProgressResult {
    pub(crate) checkpoint: crate::checkpoint::CheckpointRecord,
    pub(crate) changed: bool,
    pub(crate) previous_id: Option<String>,
    pub(crate) permit: crate::task::TaskPermit,
    pub(crate) guard: crate::provider::session_execution::HostEffectGuard,
}

impl GoalProgressResult {
    /// Commit workspace evidence and its checkpoint under the same captured task authority.
    pub(super) fn commit_workspace(
        self,
        broker: &mut HarnessBroker,
    ) -> Result<(bool, crate::provider::session_execution::HostEffectGuard)> {
        let Self {
            checkpoint,
            changed,
            previous_id,
            permit,
            mut guard,
        } = self;
        let task = broker
            .task_controller
            .current()?
            .context("goal task disappeared")?;
        permit.validate_observation(Some(&task))?;
        let crate::task::WorkflowSnapshot::Goal { goal } = &task.workflow else {
            return Err(crate::task::StaleTask.into());
        };
        anyhow::ensure!(
            goal.observed_checkpoint == previous_id,
            "goal workspace baseline changed during capture"
        );
        let checkpoint_id = checkpoint.id.clone();
        guard.begin_mutation();
        if let Err(error) = broker.task_controller.apply_with_effects(
            &mut broker.store,
            &task.permit(),
            crate::task::TaskEvent::GoalWorkspaceObserved(checkpoint_id),
            crate::storage::TaskEffects {
                checkpoint: vec![checkpoint],
                ..Default::default()
            },
            broker.clock.now_ms(),
        ) {
            if error.is::<crate::task::StaleTask>() {
                guard.cancel_definite()?;
            }
            return Err(error);
        }
        Ok((changed, guard))
    }
}

impl GoalProgressJob {
    pub(crate) async fn run(self) -> Result<GoalProgressResult> {
        let Self {
            capture,
            previous,
            previous_id,
            permit,
            guard,
        } = self;
        let checkpoint = capture.run().await?;
        let changed = previous
            .as_ref()
            .map(|before| before.changed_paths(&checkpoint))
            .transpose()?
            .is_some_and(|paths| !paths.is_empty());
        Ok(GoalProgressResult {
            checkpoint,
            changed,
            previous_id,
            permit,
            guard,
        })
    }
}

/// Retain an Execute-plan evidence reservation during an unlocked progress scan.
pub(crate) struct ExecutionProgressJob {
    pub(crate) progress: execution_progress::PlanProgressJob,
    pub(crate) permit: crate::task::TaskPermit,
    pub(crate) guard: crate::provider::session_execution::HostEffectGuard,
}

pub(crate) struct ExecutionProgressResult {
    pub(crate) progress: execution_progress::PlanProgressResult,
    pub(crate) permit: crate::task::TaskPermit,
    pub(crate) guard: crate::provider::session_execution::HostEffectGuard,
}

impl ExecutionProgressJob {
    pub(crate) async fn run(self) -> Result<ExecutionProgressResult> {
        let Self {
            progress,
            permit,
            guard,
        } = self;
        let progress = progress.run().await?;
        Ok(ExecutionProgressResult {
            progress,
            permit,
            guard,
        })
    }
}
