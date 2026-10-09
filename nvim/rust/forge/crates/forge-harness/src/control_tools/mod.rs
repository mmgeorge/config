use crate::backend::{BackendOutput, PlanSubmitRequest};
use crate::plan::{
    PlanQuestionAnswer, PlanQuestionSet, PlanQuestionWithdrawal,
};
use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use serde_json::{Map, Value, json};
use std::collections::HashMap;
use std::sync::OnceLock;
use tokio::io::{AsyncWriteExt, BufReader};

mod failure;
pub mod repository;
pub mod runtime;
pub(crate) use failure::{
    ControlToolArgumentError, control_tool_failure_json, schema_violation_list,
};
pub use runtime::{ControlToolResult, ControlToolRuntime, ControlTurnContext};

/// Defines one Harness control tool independently from any provider transport.
#[derive(Clone, Debug)]
pub struct ControlToolDefinition {
    pub name: &'static str,
    pub description: &'static str,
    pub input_schema: Value,
}

impl ControlToolDefinition {
    /// Convert the provider-neutral definition into the MCP tool-list shape.
    pub fn mcp_value(&self) -> Value {
        json!({
            "name": self.name,
            "description": self.description,
            "inputSchema": self.input_schema,
        })
    }
}

/// Owns the canonical Harness control-tool catalog for every backend adapter.
#[derive(Clone, Debug, Default)]
pub struct ControlToolRegistry;

impl ControlToolRegistry {
    /// Build the complete provider-neutral control-tool definition list.
    pub fn definition_list(&self) -> Vec<ControlToolDefinition> {
        vec![
            ControlToolDefinition {
                name: "harness_repository_status",
                description: "Read bounded Git status for the authenticated active interaction workspace. No Status view is required. Each page is a fresh observation, not a mutation precondition.",
                input_schema: repository_input_schema(false, false),
            },
            ControlToolDefinition {
                name: "harness_repository_changed_paths",
                description: "List changed repository paths with exact path_hex identities for the active interaction. At most 256 paths per fresh page.",
                input_schema: repository_input_schema(false, false),
            },
            ControlToolDefinition {
                name: "harness_repository_diff",
                description: "Inspect staged or unstaged changes in the active interaction repository. At most 8 files per fresh page. Sources above 128 KiB or 4096 lines return an explicit limit state. Read-only exact hunks include source hashes.",
                input_schema: repository_input_schema(true, false),
            },
            ControlToolDefinition {
                name: "harness_repository_file_diff",
                description: "Inspect a changed file selected by exact path_hex from repository status. Inherits active interaction workspace and read policy. Returns bounded read-only staged or unstaged hunks.",
                input_schema: repository_input_schema(true, true),
            },
            ControlToolDefinition {
                name: "harness_design_apply_patch",
                description: "Atomically edit virtual declaration overviews, complete JSON/JSONC, TOML, YAML, XML, and .gitignore proposals, and plan.json metadata. Never writes workspace files. Use *** Begin Patch / Add File / Update File / Delete File / Move to / @@ syntax with project-relative paths. Source proposals contain signatures, explanatory comments, Change summaries, Calls, and Accesses, never executable bodies. Configuration retains complete values. Read affected text through harness_plan_read when exact text or version is unavailable and omit its displayed line numbers from patches. Read an existing workspace file before its first edit. The first successful Update, Delete, or Move captures its immutable baseline. Add requires an absent destination. Later edits use the saved proposal. Optional source_digests maps paths to inspected hashes for stale-read checks across turns. Success returns the new version and an applied diff capped at 16 KiB. Use that confirmation instead of rereading unchanged files. plan.json already exists and accepts Update File only. Its fields are objective, optional usage, requirements, background, decisions, design, verification, and tests. Submission requires nonempty objective, background, and design. Requirements identify the 2-4 key delivered outcomes and acceptance constraints and may be empty for bug fixes. Keep routine validation and implementation safeguards in Design or declarations unless explicitly requested or essential to task-specific acceptance. Background records relevant starting conditions. Decisions is an array of {decision, rationale} objects for consequential choices with task-specific reasons, or [] when none apply. Design explains proposed responsibilities and behavior. Do not duplicate repository instructions, milestones, or execution progress in plan prose. Propose concrete supported file changes, including needed .gitignore rules, and describe generated artifacts or unsupported file changes in Design. Do not create placeholder lockfiles. verification.automated contains one executable command per nonblank line. verification.manual contains Markdown actions and expected results. Both strings can be empty. The required tests array groups {file, cases} by project-relative forward-slash path. Each case has name, change (new, modified, removed, or reused), and description (scenario and expected result or removal rationale). Include inline and separate-file tests and reused coverage, omit unrelated tests, and use [] when none apply. The inventory records intended coverage, not results. Keep all metadata and proposals consistent. Optional title names the design.",
                input_schema: strict_object_input_schema(vec![("plan_id",string_schema()),("expected_version",json!({"type":"integer","minimum":1})),("patch",string_schema()),("title",string_schema()),("source_digests",json!({"type":"object","additionalProperties":{"type":"string"}}))], &["plan_id","expected_version","patch"]),
            },
            ControlToolDefinition {
                name: "harness_plan_read",
                description: "List captured/proposed declaration and configuration paths and plan.json, or read one file as plain text with 1-based line numbers and the active version. Optional start_line and end_line select an inclusive range and require path. Omitted bounds select the beginning or end of the file. Line-number prefixes are presentation only, omit them from patches. plan.json contains objective, optional usage, requirements, background, decisions, design, verification (automated commands and manual checks), and tests (planned coverage) and has no baseline. Omit path for the inventory and, during execution, current phase, findings, and verification_evidence with completed tool-call IDs. Set baseline true to inspect immutable current-code declarations or complete configuration. For an uncaptured path, extracts only that workspace file and returns its source digest without saving a baseline or changing the version. Read existing files before editing them. Does not return implementation bodies.",
                input_schema: strict_object_input_schema(
                    vec![("plan_id", string_schema()),("path",string_schema()),("baseline",json!({"type":"boolean"})),("start_line",json!({"type":"integer","minimum":1})),("end_line",json!({"type":"integer","minimum":1}))],
                    &["plan_id"],
                ),
            },
            ControlToolDefinition {
                name: "harness_plan_submit",
                description: "Format and submit the exact canonical declaration design for user review. Automatically validate Rust and TypeScript imports and signature references against the proposed declarations and available sources. Proven invalid references return actionable errors and remain editable. Unavailable or unsupported evidence remains in saved diagnostics and permits submission. Success confirms submission and the current version. Submission is not implementation approval. No implementation build or second model review runs. During execution, reason is required and explains the necessary deviation. Forge accepts the new revision automatically unless the user disabled Auto-approve plan revisions in /config. End the turn after any successful submission and let Harness manage review and continuation.",
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
            },
            ControlToolDefinition {
                name: "harness_plan_phase_done",
                description: "Finish the current Implement, Verify, or Resolve phase of the active accepted semantic plan. Forge validates the expected phase and revision, runs semantic gates, commits the transition, and returns the next instructions. End the turn after success. Semantic mismatches return errors: prefer correcting the implementation, or submit a justified revision. Verify requires an agent assessment and exact completed tool-call IDs in verification.evidence. After running checks, call harness_plan_read with only plan_id to obtain execution.verification_evidence IDs and output previews. Passed cannot bypass semantic conformance. Failed enters Resolve. Blocked preserves Verify.",
                input_schema: strict_object_input_schema(vec![
                    ("phase", json!({"type":"string","enum":["implement","verify","resolve"]})),
                    ("revision", json!({"type":"integer","minimum":1})),
                    ("summary", string_schema()),
                    ("verification", strict_object_input_schema(vec![
                        ("outcome", json!({"type":"string","enum":["passed","failed","blocked"]})),
                        ("evidence", json!({"type":"array","description":"Exact completed tool-call IDs from harness_plan_read execution.verification_evidence", "items":{"type":"string"}})),
                        ("findings", json!({"type":"array","items":{"type":"string"}})),
                        ("reason", string_schema()), ("reuse_reason", string_schema())
                    ], &["outcome"]))
                ], &["phase","revision","summary"]),
            },
            ControlToolDefinition {
                name: "harness_question_ask",
                description: "Pause any Harness turn and present one to three interactive user questions. Use this for explicit requests for multiple-choice questions as well as planning decisions.",
                input_schema: plan_question_input_schema(),
            },
            ControlToolDefinition {
                name: "harness_question_answer",
                description: "Record an answer only when the user explicitly and unambiguously answers one currently pending Harness question. Never call this while continuing a planning-feedback turn because Harness already consumed those answers.",
                input_schema: question_answer_input_schema(),
            },
            ControlToolDefinition {
                name: "harness_question_withdraw",
                description: "Withdraw currently pending Harness questions only when no material user decision remains. Never call this while continuing a planning-feedback turn because Harness already resolved that question set.",
                input_schema: question_withdraw_input_schema(),
            },
            ControlToolDefinition {
                name: "harness_goal_complete",
                description: "Complete an active Goal task only when its objective and all required work are satisfied. Not for Execute tasks, which use harness_plan_phase_done. End the turn after success.",
                input_schema: strict_object_input_schema(
                    vec![("summary", string_schema())],
                    &["summary"],
                ),
            },
            ControlToolDefinition {
                name: "harness_goal_blocked",
                description: "Report an active Goal task blocked when no authorized progress remains, with a concrete cause and what is needed to resume. Not for Execute tasks. Do not use for ordinary effort or user cancellation. End the turn after success.",
                input_schema: strict_object_input_schema(
                    vec![("reason", string_schema())],
                    &["reason"],
                ),
            },
            ControlToolDefinition {
                name: "harness_goal_status",
                description: "Report observed nonterminal progress toward an active Goal task. This does not complete an Execute phase or change permissions.",
                input_schema: strict_object_input_schema(
                    vec![("status", string_schema())],
                    &["status"],
                ),
            },
        ]
    }

    /// Convert the canonical definitions into the MCP tool-list shape.
    pub fn mcp_tool_list(&self) -> Vec<Value> {
        self.definition_list()
            .iter()
            .map(ControlToolDefinition::mcp_value)
            .collect()
    }
}

/// Represents one provider callback into a Harness control tool.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ControlToolInvocation {
    pub name: String,
    pub arguments: Value,
}

struct ControlToolValidator {
    schema: Value,
    validator: jsonschema::Validator,
}

static CONTROL_TOOL_VALIDATOR_MAP: OnceLock<HashMap<&'static str, ControlToolValidator>> =
    OnceLock::new();

/// Apply one provider-neutral control invocation to the normalized turn result.
pub fn apply_invocation(
    invocation: &ControlToolInvocation,
    output: &mut BackendOutput,
) -> Result<()> {
    validate_arguments(invocation)?;
    match invocation.name.as_str() {
        "harness_design_apply_patch" => {
            output.design_patch.push(decode_arguments::<crate::plan::DesignPatchRequest>(invocation)?);
            output.structured_plan = true;
        }
        "harness_plan_read" => {
            output.plan_read = invocation
                .arguments
                .get("plan_id")
                .and_then(Value::as_str)
                .map(str::to_owned);
            output.structured_plan = output.plan_read.is_some();
        }
        "harness_plan_submit" => {
            output.plan_submit = Some(decode_arguments::<PlanSubmitRequest>(invocation)?);
            output.structured_plan = true;
        }
        "harness_question_ask" => {
            output.plan_question =
                Some(decode_arguments::<PlanQuestionSet>(invocation)?.normalize()?);
        }
        "harness_question_answer" => {
            output.question_answer = Some(decode_arguments::<PlanQuestionAnswer>(invocation)?);
        }
        "harness_question_withdraw" => {
            output.question_withdrawal =
                Some(decode_arguments::<PlanQuestionWithdrawal>(invocation)?);
        }
        "harness_plan_phase_done" => {},
        "harness_goal_complete" => output.evidence.structured_complete = true,
        "harness_goal_blocked" => output.evidence.structured_blocked = true,
        "harness_goal_status" => output.evidence.tool_called = true,
        "harness_repository_status"
        | "harness_repository_changed_paths"
        | "harness_repository_diff"
        | "harness_repository_file_diff" => {}
        name => anyhow::bail!("unknown Harness control tool: {name}"),
    }
    Ok(())
}

fn validate_arguments(invocation: &ControlToolInvocation) -> Result<()> {
    let validator_map = CONTROL_TOOL_VALIDATOR_MAP.get_or_init(|| {
        ControlToolRegistry
            .definition_list()
            .into_iter()
            .map(|definition| {
                let schema = definition.input_schema;
                let validator = jsonschema::validator_for(&schema)
                    .expect("Harness control-tool schemas must compile");
                (definition.name, ControlToolValidator { schema, validator })
            })
            .collect()
    });
    let validation = validator_map
        .get(invocation.name.as_str())
        .with_context(|| format!("unknown Harness control tool: {}", invocation.name))?;
    let mut violation_list = validation
        .validator
        .iter_errors(&invocation.arguments)
        .flat_map(|error| schema_violation_list(&error, &validation.schema))
        .collect::<Vec<_>>();
    violation_list.sort_by(|left, right| {
        (&left.path, &left.code, &left.message).cmp(&(&right.path, &right.code, &right.message))
    });
    violation_list.dedup_by(|left, right| {
        left.path == right.path && left.code == right.code && left.message == right.message
    });
    if violation_list.is_empty() {
        return Ok(());
    }
    Err(ControlToolArgumentError {
        violation: violation_list,
    }
    .into())
}

pub(super) fn json_pointer_to_path(pointer: &str) -> String {
    let mut path = String::new();
    for segment in pointer.split('/').skip(1) {
        let segment = segment.replace("~1", "/").replace("~0", "~");
        if segment.chars().all(|character| character.is_ascii_digit()) {
            path.push('[');
            path.push_str(&segment);
            path.push(']');
        } else {
            if !path.is_empty() {
                path.push('.');
            }
            path.push_str(&segment);
        }
    }
    path
}

fn decode_arguments<T>(invocation: &ControlToolInvocation) -> Result<T>
where
    T: for<'de> Deserialize<'de>,
{
    let encoded = serde_json::to_vec(&invocation.arguments)?;
    let mut deserializer = serde_json::Deserializer::from_slice(&encoded);
    serde_path_to_error::deserialize(&mut deserializer)
        .map_err(|error| anyhow::anyhow!("{} at JSON path {}", error.inner(), error.path()))
        .with_context(|| format!("decode {} arguments", invocation.name))
}

fn repository_input_schema(diff: bool, file: bool) -> Value {
    let mut properties = vec![(
        "offset",
        json!({ "type": "integer", "minimum": 0, "maximum": 65536 }),
    )];
    if diff {
        properties.push((
            "side",
            json!({ "type": "string", "enum": ["staged", "unstaged"] }),
        ));
    }
    if file {
        properties.push((
            "path_hex",
            json!({ "type": "string", "pattern": "^([0-9a-fA-F]{2})+$", "maxLength": 8192 }),
        ));
    }
    strict_object_input_schema(properties, if file { &["path_hex"] } else { &[] })
}

fn string_schema() -> Value {
    json!({ "type": "string" })
}






#[cfg(test)]
fn plan_edit_input_schema() -> Value {
    crate::plan::plan_edit_request_schema()
}



fn strict_object_input_schema(
    property_list: Vec<(&'static str, Value)>,
    required_list: &[&str],
) -> Value {
    let property_map = property_list
        .into_iter()
        .map(|(name, schema)| (name.to_owned(), schema))
        .collect::<Map<_, _>>();
    json!({
        "type": "object",
        "properties": property_map,
        "required": required_list,
        "additionalProperties": false
    })
}

/// Build the shared structured-input contract for planning feedback.
pub fn plan_question_input_schema() -> Value {
    json!({
        "type": "object",
        "properties": {
            "id": { "type": "string" },
            "questions": {
                "type": "array",
                "minItems": 1,
                "maxItems": 3,
                "items": {
                    "type": "object",
                    "properties": {
                        "id": { "type": "string" },
                        "header": { "type": "string" },
                        "question": { "type": "string" },
                        "options": {
                            "type": "array",
                            "minItems": 2,
                            "maxItems": 3,
                            "items": {
                                "type": "object",
                                "properties": {
                                    "label": { "type": "string" },
                                    "description": { "type": "string" }
                                },
                                "required": ["label", "description"],
                                "additionalProperties": false
                            }
                        },
                        "allow_freeform": { "type": "boolean" }
                    },
                    "required": ["header", "question", "options"],
                    "additionalProperties": false
                }
            }
        },
        "required": ["questions"],
        "additionalProperties": false
    })
}

/// Build the structured-input contract for one explicit conversational answer.
pub fn question_answer_input_schema() -> Value {
    json!({
        "type": "object",
        "properties": {
            "question_id": { "type": "string" },
            "response": {
                "type": "object",
                "properties": {
                    "kind": { "type": "string", "enum": ["selected", "other"] },
                    "option": { "type": "string" },
                    "feedback": { "type": "string" },
                    "text": { "type": "string" }
                },
                "required": ["kind"],
                "additionalProperties": false
            }
        },
        "required": ["question_id", "response"],
        "additionalProperties": false
    })
}

/// Build the structured-input contract for removing a resolved decision boundary.
pub fn question_withdraw_input_schema() -> Value {
    strict_object_input_schema(vec![("reason", string_schema())], &["reason"])
}

/// Run the Harness control-tool MCP server over JSONL stdio.
pub async fn run_stdio() -> Result<()> {
    let registry = ControlToolRegistry;
    let mut input = forge_protocol::frame::JsonLineReader::new(BufReader::new(
        forge_protocol::input::ThreadInput::new(std::io::stdin())?,
    ));
    let mut output = tokio::io::stdout();
    while let Some(line) = input.next_frame().await? {
        let request: Value =
            serde_json::from_slice(&line).context("decode Harness control MCP request")?;
        let id = request.get("id").cloned().unwrap_or(Value::Null);
        let method = request
            .get("method")
            .and_then(Value::as_str)
            .unwrap_or_default();
        let result = match method {
            "initialize" => json!({
                "protocolVersion": "2025-03-26",
                "capabilities": { "tools": {} },
                "serverInfo": { "name": "forge-harness-control", "version": env!("CARGO_PKG_VERSION") }
            }),
            "tools/list" => json!({ "tools": registry.mcp_tool_list() }),
            "tools/call" => {
                let name = request
                    .pointer("/params/name")
                    .and_then(Value::as_str)
                    .unwrap_or_default();
                let arguments = request
                    .pointer("/params/arguments")
                    .cloned()
                    .unwrap_or(Value::Null);
                json!({
                    "content": [{ "type": "text", "text": serde_json::to_string(&json!({ "tool": name, "arguments": arguments }))? }],
                    "structuredContent": { "tool": name, "arguments": arguments }
                })
            }
            "notifications/initialized" => continue,
            _ => json!({}),
        };
        let mut response = json!({ "jsonrpc": "2.0", "id": id, "result": result });
        let encoded = match forge_protocol::outbound::encode(
            &response,
            forge_protocol::MAX_FRAME_BYTES,
        ) {
            Ok(encoded) => encoded,
            Err(_) => forge_protocol::outbound::encode(
                &json!({
                    "jsonrpc": "2.0",
                    "id": response["id"].take(),
                    "error": { "code": -32000, "message": "Forge MCP response exceeds its frame limit" },
                }),
                forge_protocol::MAX_FRAME_BYTES,
            )?,
        };
        output.write_all(&encoded).await?;
        output.flush().await?;
    }
    Ok(())
}

#[cfg(test)]
mod test {
    #[test]
    fn repository_tools_reject_caller_authority_and_only_accept_advertised_inputs() {
        for name in [
            "harness_repository_status",
            "harness_repository_changed_paths",
            "harness_repository_diff",
            "harness_repository_file_diff",
        ] {
            let mut arguments = if name == "harness_repository_file_diff" {
                json!({ "path_hex": "612e747874" })
            } else {
                json!({})
            };
            assert!(
                validate_arguments(&ControlToolInvocation {
                    name: name.into(),
                    arguments: arguments.clone()
                })
                .is_ok()
            );
            for field in [
                "workspace",
                "session_id",
                "interaction_id",
                "permission",
                "command",
            ] {
                arguments[field] = json!("forged");
                assert!(
                    validate_arguments(&ControlToolInvocation {
                        name: name.into(),
                        arguments: arguments.clone()
                    })
                    .is_err()
                );
                arguments.as_object_mut().unwrap().remove(field);
            }
        }
        assert!(
            validate_arguments(&ControlToolInvocation {
                name: "harness_repository_file_diff".into(),
                arguments: json!({})
            })
            .is_err()
        );
        assert!(
            validate_arguments(&ControlToolInvocation {
                name: "harness_repository_status".into(),
                arguments: json!({ "side": "staged" })
            })
            .is_err()
        );
        assert!(
            validate_arguments(&ControlToolInvocation {
                name: "harness_repository_diff".into(),
                arguments: json!({ "side": "other" })
            })
            .is_err()
        );
    }
    use super::*;
    #[test]
    fn design_patch_contract_replaces_legacy_authoring_tools() {
        let definition = ControlToolRegistry.definition_list();
        assert!(definition.iter().any(|tool| tool.name == "harness_design_apply_patch"));
        assert!(!definition.iter().any(|tool| matches!(tool.name,"harness_plan_edit" | "harness_plan_deviation" | "harness_plan_task_report")));
        let invocation = ControlToolInvocation { name:"harness_design_apply_patch".into(),arguments:json!({"plan_id":"plan","expected_version":1,"patch":"*** Begin Patch\n*** Add File: lib.rs\n+pub struct Owner;\n*** End Patch"}) };
        let mut output = BackendOutput::default();
        apply_invocation(&invocation,&mut output).unwrap();
        assert_eq!(output.design_patch.len(),1);
        let mut invalid = invocation;
        invalid.arguments["baseline"] = json!({});
        assert!(apply_invocation(&invalid,&mut BackendOutput::default()).is_err());
    }

    #[test]
    #[cfg(any())]
    fn exposes_resource_oriented_plan_tools() {
        let tool_list = ControlToolRegistry.mcp_tool_list();
        let name_list = tool_list
            .iter()
            .filter_map(|tool| tool.get("name").and_then(Value::as_str))
            .collect::<Vec<_>>();
        assert_eq!(
            name_list,
            [
                "harness_repository_status",
                "harness_repository_changed_paths",
                "harness_repository_diff",
                "harness_repository_file_diff",
                "harness_plan_edit",
                "harness_plan_read",
                "harness_plan_submit",
                "harness_plan_deviation",
                "harness_plan_task_report",
                "harness_question_ask",
                "harness_question_answer",
                "harness_question_withdraw",
                "harness_goal_complete",
                "harness_goal_blocked",
                "harness_goal_status"
            ]
        );

        let schema = plan_edit_input_schema();
        assert!(schema.pointer("/properties/operations").is_none());
        assert!(
            schema
                .pointer("/properties/entity_changes/properties/add/items/properties/name")
                .is_some()
        );
        assert!(
            schema
                .pointer("/properties/entity_changes/properties/add/items/properties/action/enum")
                .and_then(Value::as_array)
                .is_some_and(|action_list| action_list.contains(&json!("rename")))
        );
        assert!(
            schema
                .pointer("/properties/entity_changes/properties/add/items/properties/renamed_from")
                .is_some()
        );
        assert!(
            schema
                .pointer(
                    "/properties/entity_changes/properties/add/items/properties/members/items/properties/action/enum"
                )
                .and_then(Value::as_array)
                .is_some_and(|action_list| !action_list.contains(&json!("rename")))
        );
        assert!(
            schema
                .pointer("/properties/entity_changes/properties/add/items/properties/entity_id")
                .is_none()
        );
        assert!(
            schema
                .pointer(
                    "/properties/entity_changes/properties/add/items/properties/exclusive_owner_entity"
                )
                .is_none()
        );
        assert!(
            schema
                .pointer(
                    "/properties/entity_changes/properties/modify/items/properties/exclusive_owner_entity"
                )
                .is_none()
        );
        assert!(
            schema
                .pointer("/properties/dependencies/properties/add/items/properties/version")
                .is_some()
        );
        assert!(
            schema
                .pointer("/properties/entity_changes/properties/modify/items/properties/members/properties/remove")
                .is_some()
        );
        assert!(
            schema
                .pointer("/properties/entity_changes/properties/add/items/properties/variants/items/properties/visibility")
                .is_none()
        );
        assert!(
            !schema
                .pointer(
                    "/properties/entity_changes/properties/add/items/properties/members/items/properties/kind/enum"
                )
                .and_then(Value::as_array)
                .is_some_and(|kind_list| kind_list.contains(&json!("variant")))
        );
        assert!(
            !schema
                .pointer("/properties/entity_changes/properties/add/items/properties/kind/enum")
                .and_then(Value::as_array)
                .is_some_and(|kind_list| kind_list.contains(&json!("test")))
        );
        assert!(
            schema
                .pointer("/properties/entity_changes/properties/add/items/properties/variants/items/properties/fields")
                .is_some()
        );
        assert!(
            schema
                .pointer("/properties/tasks/properties/modify/items/properties/files/properties/modify/items/properties/subtasks")
                .is_some()
        );
        assert_eq!(
            schema.pointer("/$defs/flow_step/properties/target/oneOf/0/properties/kind/const"),
            Some(&json!("planned_entity"))
        );
        assert!(
            schema
                .pointer("/$defs/flow_edge/properties/result")
                .is_some()
        );
        assert_eq!(
            schema.pointer(
                "/$defs/flow_edge/properties/relation/oneOf/1/properties/callable/properties/kind/enum/1"
            ),
            Some(&json!("method"))
        );
        assert_eq!(
            schema.pointer(
                "/$defs/flow_edge/properties/target/oneOf/1/properties/entity_kind/enum/0"
            ),
            Some(&json!("type"))
        );
        assert_eq!(
            schema.pointer("/$defs/flow_edge/properties/target/oneOf/1/properties/kind/const"),
            Some(&json!("workspace_entity"))
        );
        assert_eq!(
            schema.pointer("/$defs/flow_edge/properties/target/oneOf/1/properties/line/minimum"),
            Some(&json!(1))
        );
        assert!(
            schema
                .pointer("/$defs/flow_edge/properties/target/oneOf/2/properties/dependency")
                .is_some()
        );
        assert!(
            schema
                .pointer("/$defs/flow_edge/properties/edge_id")
                .is_none()
        );
        assert!(
            schema
                .pointer("/$defs/flow_branch/properties/steps")
                .is_some()
        );
        assert_eq!(
            schema.pointer("/properties/flows/properties/add/items/properties/steps/items/$ref"),
            Some(&json!("#/$defs/flow_step"))
        );
        assert!(
            schema
                .pointer(
                    "/properties/entity_changes/properties/add/items/properties/path/description"
                )
                .and_then(Value::as_str)
                .is_some_and(|description| description.contains("file, not a module"))
        );
        assert!(
            schema
                .pointer(
                    "/properties/tasks/properties/modify/items/properties/files/properties/modify/items/properties/subtasks/properties/modify/items/oneOf/0/properties/subtask/description"
                )
                .and_then(Value::as_str)
                .is_some_and(|description| description.contains("Required selector"))
        );
        assert!(
            schema
                .pointer(
                    "/properties/tasks/properties/add/items/properties/files/items/oneOf/0/properties/subtasks/items/oneOf/0/properties/entities/description"
                )
                .and_then(Value::as_str)
                .is_some_and(|description| description.contains("Complete replacement"))
        );
        assert!(schema.pointer("/properties/tests").is_none());
        assert_eq!(
            schema.pointer(
                "/properties/tasks/properties/add/items/properties/files/items/oneOf/0/properties/subtasks/items/oneOf/1/properties/operation/const"
            ),
            Some(&json!("test"))
        );
        assert_eq!(
            schema.pointer(
                "/properties/tasks/properties/add/items/properties/files/items/oneOf/3/properties/action/const"
            ),
            Some(&json!("rename"))
        );
    }

    #[test]
    #[cfg(any())]
    fn decodes_the_advertised_plan_edit_shape() {
        let invocation = ControlToolInvocation {
            name: "harness_plan_edit".into(),
            arguments: json!({
                "plan_id": "plan",
                "expected_version": 1,
                "plan": {
                    "modify": {
                        "overview": "Persist drafts.",
                        "usage": {
                            "command": "draft-sync status --document doc-42",
                            "expected_result": "Print one pending draft."
                        }
                    }
                },
                "entity_changes": {
                    "add": [
                        {
                            "action": "add",
                            "kind": "resource",
                            "name": "DraftCache",
                            "description": "Own pending drafts.",
                            "path": "src/draft_sync.rs",
                            "members": [{
                                "action": "add",
                                "kind": "method",
                                "name": "store",
                                "description": "Store one draft."
                            }]
                        },
                        {
                            "action": "add",
                            "kind": "enum",
                            "name": "DraftStatus",
                            "description": "Tracks draft persistence.",
                            "path": "src/draft_sync.rs",
                            "variants": [{
                                "action": "add",
                                "name": "Failed",
                                "description": "Carries a persistence failure.",
                                "fields": [{
                                    "action": "add",
                                    "name": "message",
                                    "type": "String"
                                }]
                            }]
                        }
                    ]
                },
                "dependencies": {
                    "add": [{
                        "action": "add",
                        "name": "tokio",
                        "version": "1",
                        "manifest": "Cargo.toml",
                        "license": "MIT",
                        "justification": "Runs asynchronous draft persistence."
                    }]
                },
                "flows": {
                    "add": [{
                        "title": "Draft persistence",
                        "description": "Persist independent draft observations.",
                        "source": {
                            "kind": "planned_entity",
                            "entity": "DraftCache"
                        },
                        "edges": [
                            {
                                "relation": "call",
                                "callable": {
                                    "kind": "method",
                                    "name": "schedule"
                                },
                                "target": {
                                    "kind": "workspace_entity",
                                    "entity_kind": "type",
                                    "name": "RetryScheduler",
                                    "path": "src/scheduler.rs",
                                    "line": 42
                                },
                                "expansion": [],
                                "branches": []
                            },
                            {
                                "relation": "read",
                                "callable": {
                                    "kind": "method",
                                    "name": "pending"
                                },
                                "target": {
                                    "kind": "planned_entity",
                                    "entity": "DraftCache"
                                },
                                "return_type": {
                                    "value_type": "DraftChange[]"
                                },
                                "expansion": [],
                                "branches": []
                            },
                            {
                                "relation": "write",
                                "callable": {
                                    "kind": "method",
                                    "name": "persist"
                                },
                                "target": {
                                    "kind": "external_entity",
                                    "entity_kind": "type",
                                    "name": "DraftStore",
                                    "dependency": null
                                },
                                "expansion": [],
                                "branches": []
                            }
                        ]
                    }]
                },
                "tasks": {
                    "add": [{
                        "title": "Own pending drafts.",
                        "description": "Keep drafts outside editor buffers.",
                        "files": [
                            {
                                "action": "add",
                                "path": "src/draft_sync.rs",
                                "subtasks": [
                                    {
                                        "operation": "create",
                                        "description": "the durable owner.",
                                        "entities": ["DraftCache", "DraftStatus"]
                                    },
                                    {
                                        "operation": "test",
                                        "action": "add",
                                        "name": "retries_failed_draft_after_backoff",
                                        "category": "unit",
                                        "behavior": "Retry a failed draft only after its backoff expires.",
                                        "covers_entities": ["DraftCache", "DraftStatus"]
                                    }
                                ]
                            },
                            {
                                "path": "tests/draft_recovery.rs",
                                "action": "add",
                                "subtasks": [{
                                    "operation": "test",
                                    "action": "add",
                                    "name": "restores_pending_draft_after_reopen",
                                    "category": "integration",
                                    "behavior": "Restore a pending draft through real editor and cache modules.",
                                    "covers_entities": ["DraftCache"]
                                }]
                            }
                        ]
                    }]
                }
            }),
        };
        let mut output = BackendOutput::default();

        apply_invocation(&invocation, &mut output).unwrap();

        let request = &output.plan_edit[0];
        let entity = &request.mutation.entity_changes.as_ref().unwrap().add[0];
        assert_eq!(entity.name, "DraftCache");
        let enum_entity = &request.mutation.entity_changes.as_ref().unwrap().add[1];
        assert_eq!(enum_entity.variants[0].name, "Failed");
        assert_eq!(enum_entity.variants[0].fields[0].name, "message");
        let dependency = &request.mutation.dependencies.as_ref().unwrap().add[0];
        assert_eq!(dependency.name, "tokio");
        assert_eq!(dependency.version, "1");
        let edge_list = &request.mutation.flows.as_ref().unwrap().add[0].edges;
        assert_eq!(edge_list.len(), 3);
        assert!(matches!(&edge_list[1].relation, PlanFlowRelation::Read));
        assert_eq!(edge_list[1].callable.as_ref().unwrap().name, "pending");
        assert_eq!(
            edge_list[1].return_type.as_ref().unwrap().value_type,
            "DraftChange[]"
        );
        assert!(matches!(
            &edge_list[0].target,
            EntityReference::WorkspaceEntity {
                entity_kind: ReferencedEntityKind::Type,
                name,
                path,
                line: 42,
            } if name == "RetryScheduler" && path == "src/scheduler.rs"
        ));
        assert_eq!(edge_list[2].return_type, None);
        assert_eq!(
            request.mutation.plan.as_ref().unwrap().modify.usage,
            PatchField::Value(PlanUsage {
                command: "draft-sync status --document doc-42".into(),
                expected_result: "Print one pending draft.".into(),
            })
        );
        let task = &request.mutation.tasks.as_ref().unwrap().add[0];
        let PlanSubtask::Work(work) = &task.files[0].subtasks[0] else {
            panic!("first subtask must own implementation entities");
        };
        assert_eq!(work.entities, ["DraftCache", "DraftStatus"]);
        let PlanSubtask::Test(unit_test) = &task.files[0].subtasks[1] else {
            panic!("second subtask must describe a unit test");
        };
        assert_eq!(unit_test.category, TestCategory::Unit);
        assert_eq!(unit_test.name, "retries_failed_draft_after_backoff");
        let PlanSubtask::Test(integration_test) = &task.files[1].subtasks[0] else {
            panic!("test file must contain an integration test subtask");
        };
        assert_eq!(integration_test.category, TestCategory::Integration);
        assert_eq!(integration_test.name, "restores_pending_draft_after_reopen");
    }

    #[test]
    #[cfg(any())]
    fn rejects_internal_identity_fields_from_model_edits() {
        let invocation = ControlToolInvocation {
            name: "harness_plan_edit".into(),
            arguments: json!({
                "plan_id": "plan",
                "expected_version": 1,
                "entity_changes": {
                    "add": [{
                        "entity_id": "draft_cache",
                        "action": "add",
                        "kind": "resource",
                        "name": "DraftCache",
                        "description": "Own pending drafts.",
                        "path": "src/draft_sync.rs"
                    }]
                }
            }),
        };

        let error = apply_invocation(&invocation, &mut BackendOutput::default())
            .unwrap_err()
            .to_string();

        assert!(error.contains("entity_id"));
        assert!(error.contains("Additional properties are not allowed"));
    }

    #[test]
    #[cfg(any())]
    fn rejects_harness_resolved_dependency_versions_from_model_edits() {
        let invocation = ControlToolInvocation {
            name: "harness_plan_edit".into(),
            arguments: json!({
                "plan_id": "plan",
                "expected_version": 1,
                "dependencies": {
                    "add": [{
                        "action": "add",
                        "name": "datafusion",
                        "version": "54",
                        "resolved_version": "54.1.0",
                        "manifest": "Cargo.toml",
                        "license": "Apache-2.0",
                        "justification": "Runs queries. The standard library has no query engine."
                    }]
                }
            }),
        };

        let error = apply_invocation(&invocation, &mut BackendOutput::default())
            .unwrap_err()
            .to_string();

        assert!(error.contains("resolved_version"));
        assert!(error.contains("Additional properties are not allowed"));
    }

    #[test]
    #[cfg(any())]
    fn reports_the_exact_nested_json_path_for_invalid_arguments() {
        let invocation = ControlToolInvocation {
            name: "harness_plan_edit".into(),
            arguments: json!({
                "plan_id": "plan",
                "expected_version": 1,
                "flows": {
                    "add": [{
                        "title": "Reader",
                        "description": "Read input.",
                        "source": {
                            "kind": "planned_entity",
                            "entity": "Reader"
                        },
                        "edges": [{
                            "relation": "read",
                            "callable": {
                                "kind": "method",
                                "name": "load"
                            },
                            "target": { "entity": "reader" }
                        }]
                    }]
                }
            }),
        };
        let error = format!(
            "{:#}",
            apply_invocation(&invocation, &mut BackendOutput::default()).unwrap_err()
        );

        assert!(
            error.contains("flows.add[0].edges[0].target"),
            "unexpected error: {error}"
        );
        assert!(error.contains("kind"));
    }

    #[test]
    #[cfg(any())]
    fn reports_every_independent_structural_violation_in_one_response() {
        let invocation = ControlToolInvocation {
            name: "harness_plan_edit".into(),
            arguments: json!({
                "expected_version": 0,
                "unexpected": true,
                "assumptions": {
                    "add": [42]
                }
            }),
        };
        let error = format!(
            "{:#}",
            apply_invocation(&invocation, &mut BackendOutput::default()).unwrap_err()
        );

        assert!(
            error.contains("4 structural violation(s)"),
            "unexpected error: {error}"
        );
        assert!(error.contains("<arguments>: \"plan_id\" is a required property"));
        assert!(error.contains("<arguments>: Additional properties are not allowed"));
        assert!(error.contains("expected_version"));
        assert!(error.contains("assumptions.add[0]"));
        assert!(error.contains("is not of type \"string\""));
    }

    #[test]
    #[cfg(any())]
    fn rejects_member_properties_from_enum_variants_before_plan_mutation() {
        let invocation = ControlToolInvocation {
            name: "harness_plan_edit".into(),
            arguments: json!({
                "plan_id": "plan",
                "expected_version": 1,
                "entity_changes": {
                    "add": [{
                        "action": "add",
                        "kind": "enum",
                        "name": "DraftStatus",
                        "description": "Tracks draft persistence.",
                        "path": "src/draft_sync.rs",
                        "variants": [{
                            "action": "add",
                            "name": "Failed",
                            "description": "Carries a persistence failure.",
                            "visibility": "public",
                            "fields": []
                        }]
                    }]
                }
            }),
        };

        let error = format!(
            "{:#}",
            apply_invocation(&invocation, &mut BackendOutput::default()).unwrap_err()
        );

        assert!(error.contains("entity_changes.add[0].variants[0]"));
        assert!(error.contains("visibility"));
        assert!(error.contains("Additional properties are not allowed"));
    }

    fn relation_violation_list(relation: Value) -> Vec<failure::ControlToolViolation> {
        let plan_schema = plan_edit_input_schema();
        let schema = json!({
            "$schema": "http://json-schema.org/draft-07/schema#",
            "$ref": "#/definitions/PlanFlowRelation",
            "definitions": plan_schema
                .get("definitions")
                .expect("generated plan schema must expose definitions")
                .clone(),
        });
        let validator =
            jsonschema::validator_for(&schema).expect("flow relation schema must compile");
        validator
            .iter_errors(&relation)
            .flat_map(|error| schema_violation_list(&error, &schema))
            .collect()
    }

    #[test]
    fn accepts_each_typed_relation_string() {
        for relation in [
            "construct",
            "call",
            "read",
            "write",
            "send",
            "emit",
            "return",
        ] {
            assert!(relation_violation_list(json!(relation)).is_empty());
        }
    }

    #[test]
    fn rejects_legacy_relation_objects() {
        assert!(!relation_violation_list(json!({ "kind": "call" })).is_empty());
        assert!(!relation_violation_list(json!("dispatch")).is_empty());
    }









}
