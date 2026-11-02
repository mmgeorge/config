use crate::tools::ControlToolDefinition;
use crate::tools::error::ToolContext;
use crate::tools::schema::strict_object_input_schema;
use crate::tools::schema::string_schema;

pub(super) const NAME: &str = "plan_overview_read";

/// Describe the plan_overview_read tool and its accepted inputs.
pub(super) fn definition() -> ControlToolDefinition {
    ControlToolDefinition {
        name: NAME,
        description: "Read the plan overview, shared version, and file inventory as structured data. Supply plan_id. \
                During execution, also returns the phase, findings, and verification_evidence with completed \
                tool-call IDs. Use plan_file_read for file contents.",
        input_schema: strict_object_input_schema(vec![("plan_id", string_schema())], &["plan_id"]),
    }
}

/// Execute plan_overview_read within the current turn authority.
pub(super) async fn execute(
    runtime: &mut crate::tools::runtime::ControlToolRuntime,
    invocation: crate::tools::ControlToolInvocation,
    output: crate::provider::ProviderCompletion,
) -> crate::tools::ToolResult<crate::tools::ControlToolResult> {
    runtime.require_readable_plan()?;
    let document = runtime
        .plan_document
        .as_ref()
        .context("plan read has no active canonical document")?;
    let requested = output
        .plan_read
        .as_deref()
        .context("plan read did not produce a plan ID")?;
    if requested != document.plan_id {
        return Err(crate::tools::ToolError::PlanIdMismatch {
            requested: requested.into(),
            active: document.plan_id.clone(),
        });
    }

    let overview = document.overview()?;
    if let Some(execution) = &runtime.context.execution {
        let state = execution
            .send(crate::plan::execution::ExecutionOperation::Inspect)
            .await?;
        return Ok(crate::tools::ControlToolResult {
            invocation: None,
            message: crate::codec::to_string(&ExecutionOverview {
                overview,
                execution: state,
            })?,
        });
    }
    Ok(crate::tools::ControlToolResult {
        invocation: Some(invocation),
        message: crate::codec::to_string_pretty(&overview)?,
    })
}

/// Attach execution evidence without serializing and reparsing the plan overview.
#[derive(serde::Serialize)]
struct ExecutionOverview<'a> {
    #[serde(flatten)]
    overview: crate::plan::PlanOverview<'a>,
    execution: serde_json::Value,
}

/// Decode the tool's replayable completion fields.
pub(super) fn record(
    invocation: &crate::tools::ControlToolInvocation,
    output: &mut crate::provider::ProviderCompletion,
) -> crate::tools::ToolResult<()> {
    output.plan_read = invocation
        .arguments
        .get("plan_id")
        .and_then(serde_json::Value::as_str)
        .map(str::to_owned);
    output.structured_plan = output.plan_read.is_some();
    Ok(())
}
