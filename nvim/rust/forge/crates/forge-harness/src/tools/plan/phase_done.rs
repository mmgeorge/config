use crate::tools::ControlToolDefinition;
use crate::tools::error::ToolContext;
use crate::tools::schema::strict_object_input_schema;
use crate::tools::schema::string_schema;
use serde_json::json;

pub(super) const NAME: &str = "plan_phase_done";

/// Describe the plan_phase_done tool and its accepted inputs.
pub(super) fn definition() -> ControlToolDefinition {
    ControlToolDefinition {
        name: NAME,
        description: "Finish the current Implement, Verify, or Resolve phase of the active accepted semantic plan. \
                Forge validates the expected phase and revision, commits the transition, and returns the next \
                instructions. Implement and Resolve finish without conformance checks. Verify collects required \
                declaration and public API differences together. Extra internal helpers never \
                gate completion. End the turn after success. Verification \
                findings enter Resolve, where code defects can be fixed and intentional contract changes can be \
                submitted together. Verify submits the report saved in plan:verification.md: write each \
                manual action's outcome, evidence, and summary there with plan_edit before calling this \
                tool. Passed cannot bypass semantic conformance. Failed enters Resolve. Blocked preserves \
                Verify.",
        input_schema: strict_object_input_schema(
            vec![
                (
                    "phase",
                    json!({"type":"string","enum":["implement","verify","resolve"]}),
                ),
                ("revision", json!({"type":"integer","minimum":1})),
                ("summary", string_schema()),
            ],
            &["phase", "revision", "summary"],
        ),
    }
}

/// Execute plan_phase_done within the current turn authority.
pub(super) async fn execute(
    runtime: &mut crate::tools::runtime::ControlToolRuntime,
    invocation: crate::tools::ControlToolInvocation,
    _output: crate::provider::ProviderCompletion,
) -> crate::tools::ToolResult<crate::tools::ControlToolResult> {
    let identity = serde_json::to_string(&invocation)?;

    let execution = runtime
        .context
        .execution
        .as_ref()
        .context("phase completion requires an active execution")?;
    let request = serde_json::from_value(invocation.arguments.clone())?;
    let response = execution
        .send(crate::plan::execution::ExecutionOperation::Phase(request))
        .await?;
    runtime.terminal = true;
    let message = execution_response_text("Phase finished.", &response);
    runtime.completion = Some((identity, message.clone()));
    Ok(crate::tools::ControlToolResult {
        invocation: None,
        message,
    })
}

/// Render an execution control response as plain text: the resulting state, any findings,
/// review feedback, and the next instruction.
pub(super) fn execution_response_text(opening: &str, response: &serde_json::Value) -> String {
    use std::fmt::Write;
    let label = |value: &serde_json::Value| value.as_str().unwrap_or("unknown").to_owned();
    let mut text = format!(
        "{opening} Execution is {} in {} at revision {}.\n",
        label(&response["status"]),
        label(&response["phase"]),
        response["revision"]
    );
    if let Some(state) = response["plan_state"].as_str() {
        let _ = writeln!(text, "The plan is {state}.");
    }
    for (heading, key) in [
        ("Findings", "findings"),
        ("Reviewer changes", "manual_changes"),
        ("Approval comments", "approval_comments"),
    ] {
        let items = response[key].as_array().cloned().unwrap_or_default();
        if items.is_empty() {
            continue;
        }
        let _ = writeln!(text, "\n{heading}:");
        for item in items {
            let item = item
                .as_str()
                .map_or_else(|| item.to_string(), str::to_owned);
            let _ = writeln!(text, "- {}", item.replace('\n', "\n  "));
        }
    }
    if let Some(instructions) = response["instructions"].as_str() {
        let _ = write!(text, "\n{instructions}\n");
    }
    text
}
