//! Commit explicit logical goal controls independently of native turn settlement.

use super::*;

impl HarnessBroker {
    /// Apply one authenticated goal control without claiming native acceptance or completion.
    pub(super) fn commit_live_goal_effect(
        &mut self,
        transaction: &mut ForegroundTransaction,
        request: &crate::provider::LiveHostEffect,
    ) -> Result<(
        crate::provider::session_execution::HostEffectId,
        Vec<SessionEvent>,
    )> {
        use crate::provider::session_execution::{
            HostEffectId, HostEffectKind, HostEffectOutput, ResultEffectAdmission,
        };
        let id = HostEffectId {
            batch_revision: request.batch.revision,
            kind: HostEffectKind::GoalEvidence,
            ordinal: 0,
        };
        let mut guard = match transaction.settlement.begin_result_effect(id)? {
            ResultEffectAdmission::Applied(output) => return Ok((id, output.events.clone())),
            ResultEffectAdmission::Apply(guard) => guard,
        };
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
        let permit = self
            .task_controller
            .admit_dispatched_goal(&control.permit, completing)?;
        anyhow::ensure!(
            !self.questions.snapshot()?.required_pending(),
            "required questions must be resolved before goal control"
        );
        let mut goal = self.current_goal()?.context("live goal is missing")?;
        guard.begin_mutation();
        if completing {
            self.task_controller.finish(
                &mut self.store,
                &permit,
                crate::task::TaskOutcomeState::Complete,
                control.message.clone(),
                self.clock.now_ms(),
            )?;
            goal.state = crate::goal::GoalState::Complete;
        } else {
            self.task_controller.block_dispatched_goal(
                &mut self.store,
                &permit,
                control.message.clone(),
                self.clock.now_ms(),
            )?;
            goal.state = crate::goal::GoalState::Blocked;
        }
        goal.updated_at_ms = self.clock.now_ms();
        self.session.active_task = self.task_controller.current()?;
        self.trace.record(
            &self.session.id,
            "goal.control_committed",
            json!({
                "task_id":task.id,"termination":if completing { "complete" } else { "blocked" },
                "entry_accepted":task.entry_accepted(),"observed_turn":task.observed_turn,
                "batch_revision":request.batch.revision,
            }),
        );
        let mut payload = serde_json::to_value(goal)?;
        payload
            .as_object_mut()
            .context("goal projection must be an object")?
            .insert("reported_status".into(), control.message.clone().into());
        let events = vec![self.event("goal_changed", payload)?];
        guard.commit_output(HostEffectOutput {
            nodes: Vec::new(),
            events: events.clone(),
            elicitation: None,
            awaiting_input: false,
            publication_drained: false,
            plan_document: None,
            continuing: Some(false),
            plan_record: None,
        })?;
        Ok((id, events))
    }
}
