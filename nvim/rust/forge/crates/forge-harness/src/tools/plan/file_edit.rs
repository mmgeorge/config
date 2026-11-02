use crate::tools::ControlToolDefinition;
use crate::tools::schema::strict_object_input_schema;
use crate::tools::schema::string_schema;
use serde_json::json;

pub(super) const NAME: &str = "plan_file_edit";

/// Describe the plan_file_edit tool and its accepted inputs.
pub(super) fn definition() -> ControlToolDefinition {
    ControlToolDefinition {
        name: NAME,
        description: "Edit proposed file changes without writing project files. Supply plan_id, expected_version, and \
                an atomic patch using *** Begin Patch, Add File, Update File, Delete File, Move to, @@, and *** \
                End Patch. Paths are project-relative. Source files contain semantic structure: signatures, \
                comments, and Calls/Constructs/Accesses; never executable bodies. New callables require a \
                nonempty Algorithm; use the academic-pseudocode skill. Modified callables use a \
                one-to-two-sentence Change, even when the signature stays the same. Place behavior and reference \
                sections directly below the signature. Comment declarations, without starting comments with \
                Returns. Keep unchanged references; omitted reference sections mean unknown, and empty sections \
                mean none. New internal declarations need a recorded use. Configuration files retain complete \
                contents. Read existing files with plan_file_read first and omit displayed line numbers from \
                patches. The first edit saves the original structure for comparison; later edits change the \
                proposal. Add requires an absent path. Returns the shared version and applied diff; no-ops keep \
                the version. Use plan_overview_edit for the plan overview.",
        input_schema: strict_object_input_schema(
            vec![
                ("plan_id", string_schema()),
                ("expected_version", json!({"type":"integer","minimum":1})),
                ("patch", string_schema()),
                (
                    "source_digests",
                    json!({"type":"object","description":"Optional inspected source hashes for stale-read checks.","additionalProperties":{"type":"string"}}),
                ),
            ],
            &["plan_id", "expected_version", "patch"],
        ),
    }
}

/// Apply the decoded file_edit operation to the active plan.
pub(super) async fn execute(
    runtime: &mut crate::tools::runtime::ControlToolRuntime,
    invocation: crate::tools::ControlToolInvocation,
    output: crate::backend::BackendCompletion,
) -> crate::tools::ToolResult<crate::tools::ControlToolResult> {
    super::state::apply_edit(runtime, invocation, output).await
}

/// Decode the tool's replayable completion fields.
pub(super) fn record(
    invocation: &crate::tools::ControlToolInvocation,
    output: &mut crate::backend::BackendCompletion,
) -> crate::tools::ToolResult<()> {
    let request =
        crate::tools::invocation::decode_arguments::<crate::plan::DesignPatchRequest>(invocation)?;
    crate::plan::ensure_file_patch(&request.patch)?;
    output.design_patch.push(request.into());
    output.plan_submit = None;
    output.structured_plan = true;
    Ok(())
}
