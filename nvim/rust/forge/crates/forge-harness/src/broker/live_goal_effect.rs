//! Validate terminal goal intent without committing it during a native turn.

use super::*;

impl HarnessBroker {
    /// Validate a retained goal request before acknowledging its callback.
    pub(super) fn validate_live_goal_request(
        &mut self,
        transaction: &mut ForegroundTransaction,
        request: &crate::provider::LiveHostEffect,
    ) -> Result<()> {
        let control = request
            .batch
            .completion
            .goal_control
            .as_ref()
            .context("live goal control is missing")?;
        let task = self
            .task_controller
            .current()?
            .context("live goal task disappeared")?;
        transaction
            .dispatched_task_permit
            .as_ref()
            .context("live goal has no dispatched authority")?
            .validate_observation(Some(&task))?;
        let completing = control.termination == crate::goal::GoalTermination::Complete;
        self.task_controller
            .admit_dispatched_goal(&control.permit, completing)?;
        anyhow::ensure!(
            !self.questions.snapshot()?.required_pending(),
            "required questions must be resolved before goal control"
        );
        anyhow::ensure!(
            !control.message.trim().is_empty(),
            "goal completion requires a summary or blocking reason"
        );
        let authority = transaction
            .task_authority
            .as_ref()
            .context("goal authority is missing")?;
        authority.seal_mutations(|| {
            self.store.record_task_terminal_request(
                &self.session.id,
                &control.request_id,
                Some(control.permit.task_id()),
                &(request.scope.clone(), control),
                None,
            )
        })?;
        // Keep the callback batch pending. Only collected native completion may
        // consume GoalEvidence and change the task's terminal state.
        Ok(())
    }
}
