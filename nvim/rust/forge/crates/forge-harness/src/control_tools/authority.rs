use anyhow::{Context, Result, ensure};
use std::sync::Arc;

use crate::task::{SessionTaskController, TaskPermit};

/// Preserves the task contract that admitted a provider callback across asynchronous waits.
#[derive(Clone)]
pub(crate) struct TaskAuthority {
    controller: Option<Arc<SessionTaskController>>,
    permit: TaskPermit,
}

impl TaskAuthority {
    pub(crate) fn capture(context: &super::ControlTurnContext) -> Option<Self> {
        context.task_permit.as_ref().map(|permit| Self {
            controller: context.task_controller.clone(),
            permit: permit.clone(),
        })
    }

    /// Rejects callbacks whose task was replaced, suspended, cleared, or not acknowledged.
    pub(crate) fn validate(&self) -> Result<()> {
        let current = self
            .controller
            .as_ref()
            .context("task callback has no session controller")?
            .current()?;
        self.permit.validate(current.as_ref())?;
        ensure!(
            current.as_ref().is_some_and(|task| task.entry_accepted()),
            "task entry has not been acknowledged by the provider"
        );
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::task::{ActiveTask, TaskChange};

    #[test]
    fn approval_authority_rejects_replacement_and_unacknowledged_entry() -> Result<()> {
        let (_root, mut store, controller) = SessionTaskController::test_goal();
        let task = controller.current()?.unwrap();
        let mut context = super::super::ControlTurnContext::inactive(
            crate::backend::PromptMode::GoalContinuation,
        );
        context.task_controller = Some(controller.clone());
        context.task_permit = Some(task.permit());
        let authority = TaskAuthority::capture(&context).unwrap();
        authority.validate()?;
        let mut replacement = ActiveTask::goal("Replacement".into(), Default::default(), false, 2);
        replacement.request_entry("New instructions".into(), 2)?;
        let replacement = controller
            .change(
                &mut store,
                Some(&task.revision),
                TaskChange::Install(Box::new(replacement)),
                2,
            )?
            .unwrap();
        assert!(
            authority
                .validate()
                .unwrap_err()
                .is::<crate::task::StaleTask>()
        );
        context.task_permit = Some(replacement.permit());
        let pending = TaskAuthority::capture(&context).unwrap();
        assert!(pending.validate().is_err());
        controller.acknowledge_entry(
            &mut store,
            &replacement.revision,
            &replacement.entry.as_ref().unwrap().id,
            3,
        )?;
        pending.validate()?;
        Ok(())
    }
}
