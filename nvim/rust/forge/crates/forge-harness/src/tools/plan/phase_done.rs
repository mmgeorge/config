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
                declaration and public API differences together. Extra internal helpers and \
                Calls/Constructs/Accesses never gate completion. End the turn after success. Verification \
                findings enter Resolve, where code defects can be fixed and intentional contract changes can be \
                submitted together. Verify requires an agent assessment and exact completed tool-call IDs in \
                verification.evidence. After running checks, call plan_overview_read with plan_id to obtain \
                execution.verification_evidence IDs and output previews. Passed cannot bypass semantic \
                conformance. Failed enters Resolve. Blocked preserves Verify.",
        input_schema: strict_object_input_schema(
            vec![
                (
                    "phase",
                    json!({"type":"string","enum":["implement","verify","resolve"]}),
                ),
                ("revision", json!({"type":"integer","minimum":1})),
                ("summary", string_schema()),
                (
                    "verification",
                    strict_object_input_schema(
                        vec![
                            (
                                "outcome",
                                json!({"type":"string","enum":["passed","failed","blocked"]}),
                            ),
                            (
                                "checks",
                                json!({"type":"array","description":"One result per accepted manual action, in plan order. Use the exact command or manual check label. Do not repeat these results in findings.","items":strict_object_input_schema(vec![
                            ("category", json!({"type":"string","enum":["manual"]})),
                            ("label", string_schema()),
                            ("outcome", json!({"type":"string","enum":["passed","failed","blocked"]})),
                            ("summary", string_schema()),
                            ("evidence", json!({"type":"array","items":{"type":"string"}}))
                        ], &["category","label","outcome","summary","evidence"])}),
                            ),
                            (
                                "evidence",
                                json!({"type":"array","description":"Exact completed tool-call IDs from plan_overview_read execution.verification_evidence", "items":{"type":"string"}}),
                            ),
                            (
                                "findings",
                                json!({"type":"array","items":{"type":"string"}}),
                            ),
                            ("reason", string_schema()),
                            ("reuse_reason", string_schema()),
                        ],
                        &["outcome", "checks"],
                    ),
                ),
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
    let message = serde_json::to_string(&response)?;
    runtime.completion = Some((identity, message.clone()));
    Ok(crate::tools::ControlToolResult {
        invocation: None,
        message,
    })
}
