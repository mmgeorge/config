use crate::tools::ControlToolDefinition;
use crate::tools::schema::strict_object_input_schema;
use crate::tools::schema::string_schema;
use serde_json::json;

pub(super) const NAME: &str = "plan_edit";

/// Describe the plan_edit tool and its accepted inputs.
pub(super) fn definition() -> ControlToolDefinition {
    ControlToolDefinition {
        name: NAME,
        description: "Edit plan artifacts and proposed project files without writing project files. Supply \
                plan_id, expected_version, and an atomic patch using *** Begin Patch, Add File, Update File, \
                Delete File, Revert File, Move to, @@, and *** End Patch. Read a path with plan_read first and \
                omit displayed line numbers from patches. Revert File withdraws every proposal for a project \
                file, or returns an artifact to its last submitted text.\n\
                Artifacts are stored exactly as written; plan_submit reports every problem in them by line. \
                plan:overview.md is `# Title` then `## Objective`, `## Usage`, `## Requirements`, \
                `## Background`, `## Decisions`, `## Design`, and `## Manual checks`; Objective, Background, \
                and Design are required. List sections use `- item` with two-space continuation lines, \
                decisions use `- **Decision.** Rationale.`, and Manual checks lists what the agent must \
                observe in Verify because automated checks cannot establish it. plan:flows.md has one \
                `## Title` per independently triggered flow, then `Trigger: event`, a description paragraph, \
                and `- action` steps; nest `- effect: target -> change` and `- if condition` under a step and \
                the branch's steps under the `if`, two spaces per level. plan:tests.md has one \
                `## path/to/file` per file and items such as - new `exact_test_name` [target: runner]: \
                scenario and expected result, where the change is new, modified, removed, or reused and the \
                target is optional. plan:checks.md lists saved checks as `- [ ] id` lines; mark `[x]` on each \
                check to run in addition to automatic ones, and mark its prerequisites too. Add File on an \
                artifact replaces its whole text.\n\
                Verify: plan_edit accepts only a patch of plan:verification.md. Replace each \
                `N. pending []` row with `N. passed|failed|blocked [IDs] note`, citing IDs from \
                plan:evidence.md, and list defects under `## Findings` as `path:line:column: message [IDs]`. \
                plan_phase_done reports any problem in the report by line.\n\
                Project files: paths are project-relative. Source files contain semantic structure: \
                signatures, comments, and Calls/Constructs/Accesses; never executable bodies. Each callable \
                carries at most one behavior block: a nonempty Algorithm for a new callable, including a \
                renamed or replacing one, or a one-to-two-sentence Change for an existing callable, even when \
                its signature stays the same; never both. Load the academic-pseudocode skill before writing an \
                Algorithm. Algorithm statements are one per line: assignments with ←, calls such as \
                process(item), if/otherwise, for each item in items, while, and return; explanations belong \
                only in # comments, and statements inside a control block are indented under it. Place \
                behavior and reference sections directly below the signature, each under a standalone \
                heading; plan_read shows their content indented, and content written flush with its heading \
                ends at the next blank line. For example:\n\
                pub fn existing();\nChange\n  Preserve the existing result while recording the update.\n\
                pub fn added();\nAlgorithm\n  return none\n\
                Calls, Constructs, and Accesses list bare qualified names such as App::add_systems, never \
                expressions, receivers, or arguments. Document every new declaration, including enum \
                variants and fields, with a doc comment (/// in Rust), without starting comments with \
                Returns. Keep unchanged references; omitted reference sections mean unknown, and empty \
                sections mean none. Every new internal declaration needs a recorded use: list each new field \
                or constant under the Accesses of a callable that reads or writes it. Source file edits are \
                stored as written even when their structure is incomplete, and plan_submit reports each \
                structure problem by line. Configuration files retain complete contents. The first edit saves \
                the original structure for comparison; later edits change the proposal. Add requires an \
                absent project path. Patch the smallest region you change, copying context lines exactly \
                from plan_read; a chunk whose lines all sit at one different indentation still applies, \
                shifted to match. A failed edit changes nothing. Returns the shared version and the changed \
                paths; no-ops keep the version.",
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

/// Apply the decoded edit operation to the active plan.
pub(super) async fn execute(
    runtime: &mut crate::tools::runtime::ControlToolRuntime,
    invocation: crate::tools::ControlToolInvocation,
    output: crate::provider::ProviderCompletion,
) -> crate::tools::ToolResult<crate::tools::ControlToolResult> {
    super::state::apply_edit(runtime, invocation, output).await
}

/// Decode the tool's replayable completion fields.
pub(super) fn record(
    invocation: &crate::tools::ControlToolInvocation,
    output: &mut crate::provider::ProviderCompletion,
) -> crate::tools::ToolResult<()> {
    let request =
        crate::tools::invocation::decode_arguments::<crate::plan::DesignPatchRequest>(invocation)?;
    output.design_patch.push(request);
    output.plan_submit = None;
    output.structured_plan = true;
    Ok(())
}
