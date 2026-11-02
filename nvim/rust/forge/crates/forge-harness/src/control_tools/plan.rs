use super::{ControlToolDefinition, strict_object_input_schema, string_schema};
use serde_json::json;

/// Defines the separate file and overview surfaces for authoring a semantic plan.
pub(super) fn definitions() -> Vec<ControlToolDefinition> {
    vec![
        ControlToolDefinition {
            name: "plan_file_edit",
            description: "Edit proposed file changes without writing project files. Supply plan_id, expected_version, and an atomic patch using *** Begin Patch, Add File, Update File, Delete File, Move to, @@, and *** End Patch. Paths are project-relative. Source files contain semantic structure: signatures, comments, and Calls/Constructs/Accesses; never executable bodies. New callables require a nonempty Algorithm; use the academic-pseudocode skill. Modified callables use a one-to-two-sentence Change, even when the signature stays the same. Place behavior and reference sections directly below the signature. Comment declarations, without starting comments with Returns. Keep unchanged references; omitted reference sections mean unknown, and empty sections mean none. New internal declarations need a recorded use. Configuration files retain complete contents. Read existing files with plan_file_read first and omit displayed line numbers from patches. The first edit saves the original structure for comparison; later edits change the proposal. Add requires an absent path. Returns the shared version and applied diff; no-ops keep the version. Use plan_overview_edit for the plan overview.",
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
        },
        ControlToolDefinition {
            name: "plan_file_read",
            description: "Read a file's proposed semantic structure or complete configuration text without changing the plan. Supply plan_id and a project-relative path. Unedited files are read from the workspace without being added to the plan. Set baseline to read the saved original of an edited file. Optional start_line/end_line select an inclusive range. Returns numbered text, the shared version, and a source digest for unedited files. Use plan_overview_read for the plan overview and file inventory.",
            input_schema: strict_object_input_schema(
                vec![
                    ("plan_id", string_schema()),
                    (
                        "path",
                        json!({"type":"string","description":"Project-relative .rs, .ts, .tsx, .lua, JSON/JSONC, TOML, YAML, XML, or .gitignore path."}),
                    ),
                    ("baseline", json!({"type":"boolean"})),
                    ("start_line", json!({"type":"integer","minimum":1})),
                    ("end_line", json!({"type":"integer","minimum":1})),
                ],
                &["plan_id", "path"],
            ),
        },
        ControlToolDefinition {
            name: "plan_overview_read",
            description: "Read the plan overview, shared version, and file inventory as structured data. Supply plan_id. During execution, also returns the phase, findings, and verification_evidence with completed tool-call IDs. Use plan_file_read for file contents.",
            input_schema: strict_object_input_schema(
                vec![("plan_id", string_schema())],
                &["plan_id"],
            ),
        },
        ControlToolDefinition {
            name: "plan_overview_edit",
            description: "Update the plan overview without editing file proposals or project files. Supply plan_id, expected_version, and the fields to change. Omitted fields stay unchanged; supplied arrays replace the whole array, and usage:null clears the example. Returns the shared version and overview diff; no-ops keep the version. Read current fields with plan_overview_read. Submission requires objective, background, and design; keep the overview consistent with file proposals. Use plan_file_edit for file changes and plan_checks_add to select saved checks.",
            input_schema: serde_json::to_value(schemars::schema_for!(
                crate::plan::PlanOverviewEditRequest
            ))
            .expect("overview edit schema is serializable"),
        },
    ]
}
