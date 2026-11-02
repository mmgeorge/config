use crate::tools::error::{ToolContext, ToolResult as Result};
use crate::tools::{ControlToolInvocation, ControlToolResult, runtime::ControlToolRuntime};

/// Resolve and validate the task retained by this question's originating turn.
pub(super) fn current_task(runtime: &ControlToolRuntime) -> Result<Option<crate::task::Task>> {
    if let Some(permit) = runtime.context.task_permit.as_ref() {
        let task = runtime
            .context
            .task_controller
            .as_ref()
            .context("question task owner is missing")?
            .current()?;
        permit.validate(task.as_ref())?;
        return Ok(task);
    }
    Ok(None)
}

/// Apply a question action under the current session and revision.
pub(super) fn apply(
    runtime: &ControlToolRuntime,
    invocation: ControlToolInvocation,
    action: crate::question::QuestionAction,
) -> Result<ControlToolResult> {
    current_task(runtime)?;
    let controller = runtime
        .context
        .questions
        .as_ref()
        .context("question operation requires a session question controller")?;
    let snapshot = controller.apply(crate::question::QuestionCommand {
        operation_id: uuid::Uuid::new_v4().to_string(),
        question_id: invocation.arguments["question_id"]
            .as_str()
            .context("question identity required")?
            .to_owned()
            .try_into()?,
        revision: invocation.arguments["revision"]
            .as_u64()
            .context("question revision required")?,
        action,
    })?;
    Ok(ControlToolResult {
        invocation: None,
        message: serde_json::to_string(&snapshot)?,
    })
}
