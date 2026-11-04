use crate::tools::error::{ToolContext, ToolResult as Result};
use crate::tools::{ControlToolInvocation, ControlToolResult, runtime::ControlToolRuntime};

/// Capture identities supplied by the active host endpoint rather than model tool arguments.
pub(super) fn tool_call(
    runtime: &ControlToolRuntime,
    invocation: &ControlToolInvocation,
) -> Result<Option<crate::exchange::QuestionToolCall>> {
    if runtime.context.question_events.is_none() {
        return Ok(None);
    }
    let scope = runtime
        .context
        .question_invocation
        .clone()
        .context("question tool has no authenticated invocation")?;
    let call_id = invocation
        .call_id
        .clone()
        .context("question tool has no authenticated call identity")?;
    Ok(Some(crate::exchange::QuestionToolCall::new(scope, call_id)))
}
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
pub(super) async fn apply(
    runtime: &ControlToolRuntime,
    invocation: ControlToolInvocation,
    action: crate::question::QuestionAction,
) -> Result<ControlToolResult> {
    current_task(runtime)?;
    let _controller = runtime
        .context
        .questions
        .as_ref()
        .context("question operation requires a session question controller")?;
    let call = tool_call(runtime, &invocation)?;
    let operation_id = call
        .as_ref()
        .map(|call| call.operation_id().as_str().to_owned())
        .unwrap_or_else(|| uuid::Uuid::new_v4().to_string());
    let command = crate::question::QuestionCommand {
        operation_id,
        question_id: invocation.arguments["question_id"]
            .as_str()
            .context("question identity required")?
            .to_owned()
            .try_into()?,
        revision: invocation.arguments["revision"]
            .as_u64()
            .context("question revision required")?,
        action,
    };
    let authority = crate::tools::TaskAuthority::capture(&runtime.context);
    let snapshot = if let Some(events) = &runtime.context.question_events {
        let authority = authority.context("question mutation has no captured tool authority")?;
        events
            .change(crate::exchange::QuestionChange::tool(
                command,
                authority,
                call.context("question mutation has no authenticated tool call")?,
            )?)
            .await?
    } else {
        #[cfg(test)]
        {
            match authority {
                Some(authority) => authority.commit(|| _controller.apply(command))?,
                None => _controller.apply(command)?,
            }
        }
        #[cfg(not(test))]
        {
            return Err(crate::tools::ToolError::MissingState(
                "question mutation owner is missing".into(),
            ));
        }
    };
    Ok(ControlToolResult {
        invocation: None,
        message: serde_json::to_string(&snapshot)?,
    })
}
