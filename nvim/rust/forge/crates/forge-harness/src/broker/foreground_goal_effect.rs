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
    pub(crate) workspace: execution::ReservedWorkspace,
    pub(crate) changed: bool,
    pub(crate) previous_id: Option<String>,
    pub(crate) permit: crate::task::TaskPermit,
    pub(crate) guard: crate::provider::session_execution::HostEffectGuard,
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
        let workspace = capture.run_reserved().await?;
        let changed = previous
            .as_ref()
            .map(|before| before.changed_paths(workspace.checkpoint()))
            .transpose()?
            .is_some_and(|paths| !paths.is_empty());
        Ok(GoalProgressResult {
            workspace,
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
