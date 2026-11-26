use crate::tools::ControlToolDefinition;
use crate::tools::schema::strict_object_input_schema;
use crate::tools::schema::string_schema;
use serde_json::json;

pub(super) const NAME: &str = "plan_edit";

/// Describe the plan_edit tool and its accepted inputs.
pub(super) fn definition() -> ControlToolDefinition {
    ControlToolDefinition {
        name: NAME,
        description: "During execution, edit plan artifacts and proposed project files without writing project files. \
                During planning, use ordinary file tools in the supplied directory instead. Supply \
                plan_id, expected_version, and an atomic patch using *** Begin Patch, Add File, Update File, \
                Delete File, Revert File, Move to, @@, and *** End Patch. Read a path with plan_read first and \
                omit displayed line numbers from patches. Revert File withdraws every proposal for a project \
                file, or returns an artifact to its last submitted text.\n\
                Artifacts are stored exactly as written; plan_submit reports every problem in them by line. \
                plan:overview.md is free-form Markdown with an optional `# Title` and an optional \
                `## Manual checks` dash list; every Manual checks item is a Verify action, so omit the \
                section rather than listing \"None\". plan:flows.md has one \
                `## Title` per independently triggered flow, then a one-to-two-line description that names \
                its trigger, then one ```mermaid block: sequenceDiagram for a flow that crosses ownership \
                boundaries, flowchart for branching control flow inside one owner, or stateDiagram-v2 for a \
                lifecycle whose transitions come from several triggers. plan:tests.md has one \
                `## path/to/file` per file and items such as - new `exact_test_name`: \
                scenario and expected result, where the change is new, modified, removed, or reused and the \
                name is the test's declared title, optionally preceded by its group titles. Add \
                [target: check] after the name only when the name matches cases in several test checks; \
                the target is a test check's ID or label from check_definition_list. plan:checks.md lists one check ID per line, each a saved check from \
                check_definition_list to run during Checking; a line's presence alone \
                selects its check, and Harness adds prerequisites itself. Add File on an \
                artifact replaces its whole text, and Delete File clears it. Operations on one artifact \
                apply in patch order; a project file appears at most once per patch.\n\
                Verify: plan_edit accepts only a patch of plan:verification.md. Replace each \
                `N. pending []` row with `N. passed|failed|blocked [IDs] note`, citing IDs from \
                plan:evidence.md, and list defects under `## Findings` as `path:line:column: message [IDs]`. \
                plan_phase_done reports any problem in the report by line.\n\
                Project files: paths are project-relative. Source files contain semantic structure: \
                signatures and comments, never executable bodies. Each callable \
                carries at most one behavior block: a nonempty Algorithm classifies new behavior, a \
                Change with an elided Algorithm diff classifies modified behavior, and no block claims unchanged \
                behavior. Harness validates supplied blocks, not their historical classification. Algorithm statements are one per line: assignments \
                with ←, calls such as process(item), comparisons such as `target is none`, if/otherwise, \
                for each item in items, while, and return; explanations belong \
                only in # comments, and statements inside a control block are indented under it. In Change, \
                use `- ` for removals, `+ ` for additions, and two spaces for context, followed by the statement's \
                own indentation. Use `...` at the prefix column between hunks and before the first hunk only \
                if earlier statements are omitted. Use indented `...` after a diff prefix to elide a control body. \
                Use unprefixed # comments as optional anchors. Place \
                the behavior block directly below the signature under a standalone Algorithm or Change \
                heading; plan_read shows its content indented, and content written flush with its heading \
                ends at the next blank line. Write what the callable uses inside the block itself; source \
                files have no Calls, Constructs, or Accesses sections. For example:\n\
                pub fn existing(result: Result);\nChange\n  - return result\n  + return record_update(result)\n\
                pub fn added();\nAlgorithm\n  return none\n\
                Document every new declaration, including enum variants and fields, with a doc comment \
                (/// in Rust), without starting comments with Returns. Source file edits are \
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
