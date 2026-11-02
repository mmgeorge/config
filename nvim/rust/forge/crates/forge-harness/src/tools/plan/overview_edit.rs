use crate::tools::ControlToolDefinition;

pub(super) const NAME: &str = "plan_overview_edit";

/// Describe the plan_overview_edit tool and its accepted inputs.
pub(super) fn definition() -> ControlToolDefinition {
    ControlToolDefinition {
        name: NAME,
        description: "Update the plan overview without editing file proposals or project files. Supply plan_id, \
                expected_version, and the fields to change. Omitted fields stay unchanged; supplied arrays \
                replace the whole array, and usage:null clears the example. The checks field selects additional \
                saved checks; prerequisites are included automatically and [] clears additions. Automatic checks \
                still apply. Returns the shared version and overview diff; no-ops keep the version. Check \
                selection edits also return the check preview. Read current fields with plan_overview_read. \
                Submission requires objective, background, and design; keep the overview consistent with file \
                proposals. Use plan_file_edit for file changes.",
        input_schema: input_schema(),
    }
}

/// Describe overview fields alongside the tool while deriving their shapes from the request types.
fn input_schema() -> serde_json::Value {
    use crate::tools::schema::{clear_descriptions, describe_fields};
    let mut schema =
        serde_json::to_value(schemars::schema_for!(crate::plan::PlanOverviewEditRequest))
            .expect("overview edit schema is serializable");
    clear_descriptions(&mut schema);
    describe_fields(
        &mut schema,
        "/properties",
        &[
            ("plan_id", "The active plan ID."),
            (
                "expected_version",
                "The shared version from the last plan read or edit.",
            ),
            ("title", "The plan's review title."),
            (
                "objective",
                "The requested outcome and scope. Required at submission.",
            ),
            (
                "usage",
                "One caller interaction and its expected result as Markdown. Use fenced blocks for text inputs and outputs. \
            For a CLI, include the full command and expected stdout, exit status, or artifact. Describe non-text results briefly. \
            Omit when caller-facing behavior is unchanged; null removes an existing example.",
            ),
            (
                "requirements",
                "The two to four key acceptance outcomes, or [] when the objective is sufficient.",
            ),
            (
                "background",
                "Current behavior, relevant paths and symbols, and guarantees to preserve. Required at submission.",
            ),
            (
                "decisions",
                "Consequential choices and the reasons for them, or [].",
            ),
            (
                "design",
                "The solution, state ownership, failure behavior, and compatibility. Include unsupported or generated file changes. Required at submission.",
            ),
            (
                "flows",
                "Structured runtime flows, or []. Use one initiating event per flow and separate independent lifecycles. \
            Name participating objects or operations and the values, events, or results passed between them. Harness draws the diagrams.",
            ),
            (
                "verification",
                "Manual actions and expected observations after automated checks pass, or [] when unnecessary.",
            ),
            (
                "tests",
                "Intended coverage grouped by file, including inline and reused tests. Use exact test names and scenarios; this is not a record of executed results.",
            ),
            (
                "checks",
                "Replace additional saved check IDs. Prerequisites are included automatically. Omit to preserve the selection or use [] to clear additions; automatic checks still apply.",
            ),
        ],
    );
    describe_fields(
        &mut schema,
        "/$defs/DesignDecision/properties",
        &[
            ("decision", "The selected approach."),
            (
                "rationale",
                "Why it fits the task's constraints and tradeoffs.",
            ),
        ],
    );
    describe_fields(
        &mut schema,
        "/$defs/DesignFlow/properties",
        &[
            ("title", "A descriptive flow name."),
            ("description", "Its purpose and ordering guarantees."),
            ("trigger", "The event that starts this execution."),
            ("steps", "Actions in execution order."),
        ],
    );
    describe_fields(
        &mut schema,
        "/$defs/DesignFlowStep/properties",
        &[
            ("action", "A function or explicit operation."),
            ("effects", "Terminal state changes caused by the action."),
            (
                "branches",
                "Conditional paths evaluated during this execution.",
            ),
        ],
    );
    describe_fields(
        &mut schema,
        "/$defs/DesignFlowEffect/properties",
        &[
            ("target", "The affected value, object, or state."),
            ("change", "The resulting state change."),
        ],
    );
    describe_fields(
        &mut schema,
        "/$defs/DesignFlowBranch/properties",
        &[
            (
                "condition",
                "The condition for this path during the current execution, not a later event.",
            ),
            ("steps", "Actions in order when the condition holds."),
        ],
    );
    describe_fields(
        &mut schema,
        "/$defs/DesignTestFile/properties",
        &[
            (
                "file",
                "A project-relative file path using forward slashes.",
            ),
            ("cases", "Changed or reused tests in this file."),
        ],
    );
    describe_fields(
        &mut schema,
        "/$defs/DesignTestCase/properties",
        &[
            (
                "name",
                "The exact test name, including module qualification when needed.",
            ),
            (
                "target",
                "The runner target when multiple executables contain this name.",
            ),
            (
                "change",
                "Whether the test is new, modified, removed, or reused.",
            ),
            (
                "description",
                "The scenario and expected result, or removal rationale. For isolated tests, name collaborators to mock; for integration tests, name real boundaries exercised.",
            ),
        ],
    );
    schema
}

/// Apply the decoded overview_edit operation to the active plan.
pub(super) async fn execute(
    runtime: &mut crate::tools::runtime::ControlToolRuntime,
    invocation: crate::tools::ControlToolInvocation,
    output: crate::provider::ProviderCompletion,
) -> crate::tools::ToolResult<crate::tools::ControlToolResult> {
    let editing_checks = invocation
        .arguments
        .get("checks")
        .is_some_and(serde_json::Value::is_array);
    let mut result = super::state::apply_edit(runtime, invocation, output).await?;
    if editing_checks
        && let Some(document) = runtime.plan_document.as_ref()
        && let Some(design) = document.design.as_ref()
    {
        let workspace = runtime
            .context
            .workspace_root
            .as_deref()
            .ok_or_else(|| crate::tools::error::rejected!("plan has no workspace"))?;
        let mut preview = design.clone();
        preview.check_configuration = Some(crate::checks::ConfigSnapshot::load(workspace)?);
        result.message.push_str(&format!(
            "\nCheck preview: {}",
            serde_json::to_string(&crate::checks::preview_plan(workspace, &preview).await?)?
        ));
    }
    Ok(result)
}

/// Decode the tool's replayable completion fields.
pub(super) fn record(
    invocation: &crate::tools::ControlToolInvocation,
    output: &mut crate::provider::ProviderCompletion,
) -> crate::tools::ToolResult<()> {
    output
        .design_patch
        .push(crate::plan::DesignEditRequest::Overview(
            crate::tools::invocation::decode_arguments::<crate::plan::PlanOverviewEditRequest>(
                invocation,
            )?,
        ));
    output.plan_submit = None;
    output.structured_plan = true;
    Ok(())
}
