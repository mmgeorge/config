use crate::plan::{PlanState, render_plan, validate_workspace_references};
use crate::tools::ControlToolDefinition;
use crate::tools::error::ToolContext;
use crate::tools::schema::strict_object_input_schema;
use crate::tools::schema::string_schema;
use serde_json::json;

pub(super) const NAME: &str = "plan_submit";

/// Describe the plan_submit tool and its accepted inputs.
pub(super) fn definition() -> ControlToolDefinition {
    ControlToolDefinition {
        name: NAME,
        description: "Submit the exact canonical declaration design for user review. Parse every artifact and \
                proposed file, reporting each structure problem by line, and automatically validate \
                Rust and TypeScript imports and signature references against the proposed declarations and \
                available sources. Proven invalid references return actionable errors and remain editable. \
                Unavailable or unsupported evidence remains in saved diagnostics and permits submission. Success \
                confirms submission and the current version. No implementation build or second model review runs. \
                Harness accepts full submissions automatically only when the task's auto-accept policy is \
                enabled; the user controls it with /plan accept on or /plan accept off. Wait for Harness \
                execution instructions rather than treating submission itself as implementation authority. In \
                Plan, tools remain available after submission. Continue on further work or user input, then \
                submit the latest version again. Any later accepted edit invalidates the pending submission until \
                resubmitted. During execution, revisions are allowed only in Resolve. Submit one consolidated \
                contract change with a concrete reason. Internal helpers and Calls/Constructs/Accesses \
                differences do not require a revision. The execution tool call waits for the review decision. \
                Continue the same turn using its returned revision and feedback.",
        input_schema: strict_object_input_schema(
            vec![
                ("plan_id", string_schema()),
                ("reason", string_schema()),
                (
                    "expected_version",
                    json!({ "type": "integer", "minimum": 1 }),
                ),
            ],
            &["plan_id", "expected_version"],
        ),
    }
}

/// Execute plan_submit within the current turn authority.
pub(super) async fn execute(
    runtime: &mut crate::tools::runtime::ControlToolRuntime,
    invocation: crate::tools::ControlToolInvocation,
    output: crate::provider::ProviderCompletion,
) -> crate::tools::ToolResult<crate::tools::ControlToolResult> {
    runtime.require_editable_plan()?;
    let submission = output
        .plan_submit
        .as_ref()
        .context("plan submit did not produce a submission")?;
    let document = runtime
        .plan_document
        .as_ref()
        .context("plan submit has no active canonical document")?;
    crate::tools::error::ensure!(
        submission.plan_id == document.plan_id,
        "submitted plan id does not match the active plan"
    );
    crate::tools::error::ensure!(
        submission.expected_version == document.version,
        "submitted version does not match active version"
    );
    if let Some(design) = &document.design {
        let workspace = runtime
            .context
            .workspace_root
            .as_deref()
            .context("design submission has no workspace root")?;
        let design = if runtime.context.execution.is_some() {
            design
                .validated_revision(workspace, &document.title)
                .await?
        } else {
            design.validated(workspace, &document.title).await?
        };
        let warnings = grouped_warnings(design.warnings());
        let mut submitted = document.clone();
        submitted.design = Some(design.into_design());
        submitted.validate_for_submission()?;
        if let Some(execution) = &runtime.context.execution {
            let reason = invocation
                .arguments
                .get("reason")
                .and_then(serde_json::Value::as_str)
                .filter(|reason| !reason.trim().is_empty())
                .context("execution revision requires a reason")?;
            let mut response = execution
                .send(crate::plan::execution::ExecutionOperation::Submit {
                    baseline: Box::new(document.clone()),
                    document: Box::new(submitted),
                    reason: reason.into(),
                })
                .await?;
            let generation = response["generation"]
                .as_u64()
                .context("revision response has no generation")?;
            let plan_state = serde_json::from_value(response["plan_state"].clone())?;
            let document = serde_json::from_value(
                response
                    .as_object_mut()
                    .context("revision response is not an object")?
                    .remove("document")
                    .context("revision response has no canonical document")?,
            )?;
            runtime.context.execution.as_mut().unwrap().generation = generation;
            runtime.context.plan_state = Some(plan_state);
            runtime.plan_document = Some(document);
            let message =
                super::phase_done::execution_response_text("Revision reviewed.", &response);
            return Ok(crate::tools::ControlToolResult {
                invocation: None,
                message,
            });
        }
        runtime.plan_document = Some(submitted);
        runtime.context.plan_state = Some(PlanState::AwaitingReview);
        let mut message = runtime.plan_version_message("Declaration design submitted for review");
        if !warnings.is_empty() {
            message.push_str(&format!(
                "\nUnverified reference warnings do not block review:\n{}",
                warnings.join("\n")
            ));
        }
        return Ok(crate::tools::ControlToolResult {
            invocation: Some(invocation),
            message,
        });
    }
    document.validate_for_submission()?;
    let workspace_root = runtime
        .context
        .workspace_root
        .as_deref()
        .context("plan submission has no workspace root")?;
    validate_workspace_references(document, workspace_root)?;
    render_plan(document)?;
    runtime.plan_document = Some(document.clone());
    runtime.context.plan_state = Some(PlanState::AwaitingReview);
    Ok(crate::tools::ControlToolResult {
        invocation: Some(invocation),
        message: runtime.plan_version_message("Plan passed canonical submission validation"),
    })
}

/// Print one line per file and warning message, so a file with many unchecked dependency
/// references reports a count instead of one identical line per reference.
fn grouped_warnings(warnings: Vec<crate::plan::PlanViolation>) -> Vec<String> {
    let mut group = Vec::<(String, String, Vec<String>)>::new();
    for warning in warnings {
        let (file, position) = warning
            .path
            .split_once(':')
            .map_or((warning.path.as_str(), ""), |(file, position)| {
                (file, position)
            });
        match group
            .iter_mut()
            .find(|(path, message, _)| path == file && *message == warning.message)
        {
            Some((_, _, positions)) => positions.push(position.into()),
            None => group.push((file.into(), warning.message, vec![position.into()])),
        }
    }
    group
        .into_iter()
        .map(|(file, message, positions)| match positions.as_slice() {
            [position] => format!("{file}:{position}: warning: {message}"),
            _ => format!(
                "{file}: warning: {message} ({} references)",
                positions.len()
            ),
        })
        .collect()
}

/// Decode the tool's replayable completion fields.
pub(super) fn record(
    invocation: &crate::tools::ControlToolInvocation,
    output: &mut crate::provider::ProviderCompletion,
) -> crate::tools::ToolResult<()> {
    output.plan_submit = Some(crate::tools::invocation::decode_arguments::<
        crate::provider::PlanSubmitRequest,
    >(invocation)?);
    output.structured_plan = true;
    Ok(())
}
