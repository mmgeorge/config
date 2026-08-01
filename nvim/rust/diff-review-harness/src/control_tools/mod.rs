use crate::backend::{BackendOutput, PlanSubmitRequest};
use crate::plan::{
    PlanDeviationRequest, PlanEditRequest, PlanQuestionAnswer, PlanQuestionSet,
    PlanQuestionWithdrawal, PlanTaskReport,
};
use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use serde_json::{Map, Value, json};
use std::collections::HashMap;
use std::sync::OnceLock;
use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader};

mod failure;
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
                name: "harness_plan_edit",
                description: "Atomically edit the broker-created canonical PlanDocument with optimistic version checking. Patch title, overview, usage, and assumptions directly. Set complete top-level Plan Schema resources directly, rename identifying names or titles explicitly, and retract plan entries through delete lists. Action fields inside resources describe future implementation changes, not document editing. Plan nodes have no generated IDs. Model each changed program construct once as an entity_change and each concrete test once as a flat task-file subtask whose operation is test.",
                input_schema: plan_edit_input_schema(),
            },
            ControlToolDefinition {
                name: "harness_plan_read",
                description: "Request the current canonical JSON plan by stable plan ID. The next Harness planning context includes the complete document.",
                input_schema: strict_object_input_schema(
                    vec![("plan_id", string_schema())],
                    &["plan_id"],
                ),
            },
            ControlToolDefinition {
                name: "harness_plan_submit",
                description: "Submit the exact validated canonical plan version for mandatory user review. Invalid plans return actionable validation errors and remain editable.",
                input_schema: strict_object_input_schema(
                    vec![
                        ("plan_id", string_schema()),
                        (
                            "expected_version",
                            json!({ "type": "integer", "minimum": 1 }),
                        ),
                    ],
                    &["plan_id", "expected_version"],
                ),
            },
            ControlToolDefinition {
                name: "harness_plan_deviation",
                description: "Record an execution-time informational or scope deviation. Scope deviations carry the same ordered set/delete proposed_changes shape as harness_plan_edit.",
                input_schema: plan_deviation_input_schema(),
            },
            ControlToolDefinition {
                name: "harness_plan_task_report",
                description: "Complete or block the active whole-plan task with version-scoped JSON pointer evidence for the task, subtasks, entities, and tests. Harness validates the evidence and selects the next task.",
                input_schema: plan_task_report_input_schema(),
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
                description: "Mark the active Harness goal complete only after every required task finishes.",
                input_schema: strict_object_input_schema(
                    vec![("summary", string_schema())],
                    &["summary"],
                ),
            },
            ControlToolDefinition {
                name: "harness_goal_blocked",
                description: "Mark the active Harness goal blocked with concrete evidence.",
                input_schema: strict_object_input_schema(
                    vec![("reason", string_schema())],
                    &["reason"],
                ),
            },
            ControlToolDefinition {
                name: "harness_goal_status",
                description: "Report nonterminal progress toward the active Harness goal.",
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
        "harness_plan_edit" => {
            output
                .plan_edit
                .push(decode_arguments::<PlanEditRequest>(invocation)?);
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
        "harness_plan_deviation" => {
            output
                .plan_deviation
                .push(decode_arguments::<PlanDeviationRequest>(invocation)?);
            output.structured_plan = true;
        }
        "harness_plan_task_report" => {
            output
                .plan_task_report
                .push(decode_arguments::<PlanTaskReport>(invocation)?);
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
        "harness_goal_complete" => output.evidence.structured_complete = true,
        "harness_goal_blocked" => output.evidence.structured_blocked = true,
        "harness_goal_status" => output.evidence.tool_called = true,
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

fn string_schema() -> Value {
    json!({ "type": "string" })
}

fn nullable_string_schema() -> Value {
    json!({ "type": ["string", "null"] })
}

fn string_array_schema() -> Value {
    json!({ "type": "array", "items": { "type": "string" } })
}

fn json_pointer_schema(pattern: &str, description: &str) -> Value {
    json!({
        "type": "string",
        "pattern": pattern,
        "description": description,
    })
}

fn nullable_json_pointer_schema(pattern: &str, description: &str) -> Value {
    json!({
        "type": ["string", "null"],
        "pattern": pattern,
        "description": description,
    })
}

fn json_pointer_array_schema(pattern: &str, description: &str) -> Value {
    json!({
        "type": "array",
        "items": json_pointer_schema(pattern, description),
    })
}

fn plan_edit_input_schema() -> Value {
    crate::plan::plan_edit_request_schema()
}

fn plan_deviation_input_schema() -> Value {
    crate::plan::plan_deviation_request_schema()
}

fn plan_task_report_input_schema() -> Value {
    strict_object_input_schema(
        vec![
            ("execution_id", string_schema()),
            (
                "task_path",
                json_pointer_schema(
                    "^/tasks/[0-9]+$",
                    "JSON Pointer for the active task in the accepted plan revision, for example /tasks/0.",
                ),
            ),
            (
                "state",
                json!({ "type": "string", "enum": ["complete", "blocked"] }),
            ),
            (
                "completed_subtask_paths",
                json_pointer_array_schema(
                    "^/tasks/[0-9]+/files/[0-9]+/subtasks/[0-9]+$",
                    "JSON Pointer for one completed subtask in the accepted plan revision.",
                ),
            ),
            (
                "completed_entity_paths",
                json_pointer_array_schema(
                    "^/entity_changes/[0-9]+$",
                    "JSON Pointer for one completed entity in the accepted plan revision.",
                ),
            ),
            (
                "test_results",
                json!({
                    "type": "array",
                    "items": strict_object_input_schema(
                        vec![
                            (
                                "test_subtask_path",
                                nullable_json_pointer_schema(
                                    "^/tasks/[0-9]+/files/[0-9]+/subtasks/[0-9]+$",
                                    "JSON Pointer for the concrete test subtask in the accepted plan revision.",
                                ),
                            ),
                            (
                                "status",
                                json!({
                                    "type": "string",
                                    "enum": ["passed", "failed", "skipped", "not_run"]
                                }),
                            ),
                            ("command", nullable_string_schema()),
                            ("detail", nullable_string_schema()),
                        ],
                        &["status"],
                    )
                }),
            ),
            ("changed_paths", string_array_schema()),
            ("summary", nullable_string_schema()),
            ("blocking_reason", nullable_string_schema()),
        ],
        &["execution_id", "task_path", "state"],
    )
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
    let mut input = BufReader::new(tokio::io::stdin()).lines();
    let mut output = tokio::io::stdout();
    while let Some(line) = input.next_line().await? {
        let request: Value =
            serde_json::from_str(&line).context("decode Harness control MCP request")?;
        let id = request.get("id").cloned().unwrap_or(Value::Null);
        let method = request
            .get("method")
            .and_then(Value::as_str)
            .unwrap_or_default();
        let result = match method {
            "initialize" => json!({
                "protocolVersion": "2025-03-26",
                "capabilities": { "tools": {} },
                "serverInfo": { "name": "diff-review-harness-control", "version": env!("CARGO_PKG_VERSION") }
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
        let response = json!({ "jsonrpc": "2.0", "id": id, "result": result });
        output
            .write_all(serde_json::to_string(&response)?.as_bytes())
            .await?;
        output.write_all(b"\n").await?;
        output.flush().await?;
    }
    Ok(())
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::plan::{PatchField, PlanUsage};

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
                        "steps": [{
                            "action": "Read draft observations",
                            "target": {
                                "kind": "planned_entity",
                                "entity": "DraftCache"
                            },
                            "edges": [
                                {
                                    "relation": {
                                        "kind": "call",
                                        "callable": {
                                            "kind": "method",
                                            "name": "schedule"
                                        }
                                    },
                                    "target": {
                                        "kind": "workspace_entity",
                                        "entity_kind": "type",
                                        "name": "RetryScheduler",
                                        "path": "src/scheduler.rs",
                                        "line": 42
                                    },
                                    "expansion": [],
                                    "result": null
                                },
                                {
                                    "relation": {
                                        "kind": "read",
                                        "callable": {
                                            "kind": "method",
                                            "name": "pending"
                                        }
                                    },
                                    "target": {
                                        "kind": "planned_entity",
                                        "entity": "DraftCache"
                                    },
                                    "expansion": [],
                                    "result": {
                                        "kind": "type",
                                        "name": "DraftChange[]"
                                    }
                                },
                                {
                                    "relation": {
                                        "kind": "write",
                                        "callable": {
                                            "kind": "method",
                                            "name": "persist"
                                        }
                                    },
                                    "target": {
                                        "kind": "external_entity",
                                        "entity_kind": "type",
                                        "name": "DraftStore",
                                        "dependency": null
                                    },
                                    "expansion": [],
                                    "result": null
                                }
                            ],
                            "branches": []
                        }]
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
        let edge_list = &request.mutation.flows.as_ref().unwrap().add[0].steps[0].edges;
        assert_eq!(edge_list.len(), 3);
        assert!(matches!(
            &edge_list[1].relation,
            PlanFlowRelation::Read { callable }
                if callable.kind == PlanCallableKind::Method && callable.name == "pending"
        ));
        assert_eq!(
            edge_list[1].result,
            Some(PlanFlowValue::Type {
                name: "DraftChange[]".into()
            })
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
        assert_eq!(edge_list[2].result, None);
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
                        "steps": [{
                            "action": "Read",
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
            error.contains("flows.add[0].steps[0].target"),
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
    fn reports_invalid_emit_payloads_with_the_emit_shape_and_result_hint() {
        for unexpected_field in ["event", "effect"] {
            let violation_list = relation_violation_list(json!({
                "kind": "emit",
                (unexpected_field): "diagnostic written"
            }));

            assert_eq!(violation_list.len(), 1);
            let violation = &violation_list[0];
            assert_eq!(violation.path, unexpected_field);
            assert_eq!(violation.code, "unknown_field");
            assert_eq!(
                violation.expected_shape,
                Some(json!({ "allowed_fields": ["kind"] }))
            );
            assert_eq!(
                violation.hint.as_deref(),
                Some(
                    "Produces an observable effect at the target. The relation contains only kind; describe the produced effect in the edge result."
                )
            );
        }
    }

    #[test]
    fn reports_missing_fields_from_the_selected_relation_variant() {
        let send_violation_list = relation_violation_list(json!({ "kind": "send" }));
        assert_eq!(send_violation_list.len(), 1);
        assert_eq!(send_violation_list[0].code, "missing_field");
        assert_eq!(
            send_violation_list[0].expected_shape,
            Some(json!({ "required": ["event"] }))
        );

        let call_violation_list = relation_violation_list(json!({ "kind": "call" }));
        assert_eq!(call_violation_list.len(), 1);
        assert_eq!(call_violation_list[0].code, "missing_field");
        assert_eq!(
            call_violation_list[0].expected_shape,
            Some(json!({ "required": ["callable"] }))
        );
    }

    #[test]
    fn exposes_direct_set_explicit_rename_and_delete_schema() {
        let definition = ControlToolRegistry
            .definition_list()
            .into_iter()
            .find(|definition| definition.name == "harness_plan_edit")
            .unwrap();
        let schema = definition.input_schema;

        assert_eq!(
            schema.pointer("/properties/set/anyOf/0/$ref"),
            Some(&json!("#/definitions/PlanResourceSet"))
        );
        assert!(
            schema
                .pointer("/definitions/PlanResourceSet/properties/flows/items/properties/key")
                .is_none()
        );
        assert_eq!(
            schema.pointer("/definitions/PlanSemanticRename/required"),
            Some(&json!(["from", "to"]))
        );
        assert_eq!(
            schema.pointer("/definitions/PlanResourceSet/properties/flows/items/$ref"),
            Some(&json!("#/definitions/PlanFlow"))
        );
        assert_eq!(
            schema.pointer("/definitions/PlanResourceDelete/properties/flows/items/type"),
            Some(&json!("string"))
        );
        assert_eq!(
            schema.pointer("/definitions/PlanFlow/properties/steps/items/$ref"),
            Some(&json!("#/definitions/PlanFlowStep"))
        );
        assert!(schema.pointer("/properties/entity_changes").is_none());
        assert!(
            schema
                .pointer("/definitions/PlanFlowEdge/properties/edge_id")
                .is_none()
        );
        assert!(schema.pointer("/properties/tests").is_none());

        let deviation_schema = ControlToolRegistry
            .definition_list()
            .into_iter()
            .find(|definition| definition.name == "harness_plan_deviation")
            .unwrap()
            .input_schema;
        assert_eq!(
            deviation_schema.pointer("/properties/proposed_changes/$ref"),
            Some(&json!("#/definitions/PlanMutation"))
        );
        assert_eq!(
            deviation_schema.pointer("/definitions/PlanResourceDelete/properties/flows/items/type"),
            Some(&json!("string"))
        );
    }

    #[test]
    fn generated_plan_schema_keeps_nested_metadata_symmetric_and_complete() {
        let schema = plan_edit_input_schema();

        assert_eq!(
            schema.pointer("/definitions/ProgramEntityMemberChange/required"),
            Some(&json!(["action", "kind", "name"]))
        );
        assert_eq!(
            schema.pointer("/definitions/EnumVariantChange/required"),
            Some(&json!(["action", "name", "fields"]))
        );
        assert_eq!(
            schema.pointer("/definitions/EnumVariantFieldChange/required"),
            Some(&json!(["action", "name", "type"]))
        );
        for property in ["renamed_from", "description"] {
            assert!(
                schema
                    .pointer(&format!(
                        "/definitions/ProgramEntityMemberChange/properties/{property}"
                    ))
                    .is_some()
            );
            assert!(
                schema
                    .pointer(&format!(
                        "/definitions/EnumVariantChange/properties/{property}"
                    ))
                    .is_some()
            );
            assert!(
                schema
                    .pointer(&format!(
                        "/definitions/EnumVariantFieldChange/properties/{property}"
                    ))
                    .is_some()
            );
        }
        assert!(
            schema
                .pointer("/definitions/EnumVariantFieldChange/properties/kind")
                .is_some()
        );
        assert!(
            schema
                .pointer("/definitions/EnumVariantFieldChange/properties/visibility")
                .is_some()
        );
        assert_eq!(
            schema.pointer("/definitions/ProgramEntityChange/required"),
            Some(&json!([
                "action",
                "kind",
                "name",
                "description",
                "path",
                "members",
                "variants",
                "conforms_to"
            ]))
        );
        assert_eq!(
            schema.pointer("/definitions/DependencyChangeAction/enum"),
            Some(&json!(["add", "modify", "remove"]))
        );

        let invocation = ControlToolInvocation {
            name: "harness_plan_edit".into(),
            arguments: json!({
                "plan_id": "plan",
                "expected_version": 1,
                "set": {
                    "entity_changes": [{
                        "action": "add",
                        "kind": "enum",
                        "name": "InspectionState",
                        "description": "Represents inspection progress.",
                        "path": "src/state.rs",
                        "members": [],
                        "variants": [{
                            "action": "add",
                            "name": "Ready",
                            "description": "Carries one ready report.",
                            "fields": [{
                                "action": "add",
                                "kind": "field",
                                "name": "report",
                                "type": "InspectionReport",
                                "visibility": "public",
                                "description": "Carries the completed inspection."
                            }, {
                                "action": "remove",
                                "name": "legacy_report",
                                "type": "InspectionReport",
                                "visibility": "private"
                            }]
                        }],
                        "conforms_to": []
                    }]
                }
            }),
        };
        apply_invocation(&invocation, &mut BackendOutput::default()).unwrap();
    }

    #[test]
    fn task_report_schema_requires_canonical_json_pointers() {
        let schema = plan_task_report_input_schema();
        assert_eq!(
            schema.pointer("/properties/task_path/pattern"),
            Some(&json!("^/tasks/[0-9]+$"))
        );
        assert_eq!(
            schema.pointer("/properties/completed_entity_paths/items/pattern"),
            Some(&json!("^/entity_changes/[0-9]+$"))
        );

        let invalid_invocation = ControlToolInvocation {
            name: "harness_plan_task_report".into(),
            arguments: json!({
                "execution_id": "execution",
                "task_path": "tasks[0]",
                "state": "complete"
            }),
        };
        assert!(
            apply_invocation(&invalid_invocation, &mut BackendOutput::default()).is_err(),
            "dot-and-index diagnostics must not masquerade as canonical JSON Pointers"
        );

        let valid_invocation = ControlToolInvocation {
            name: "harness_plan_task_report".into(),
            arguments: json!({
                "execution_id": "execution",
                "task_path": "/tasks/0",
                "state": "complete"
            }),
        };
        apply_invocation(&valid_invocation, &mut BackendOutput::default()).unwrap();
    }

    #[test]
    fn decodes_ordered_complete_set_resources_without_generated_node_ids() {
        let invocation = ControlToolInvocation {
            name: "harness_plan_edit".into(),
            arguments: json!({
                "plan_id": "plan",
                "expected_version": 1,
                "plan": {
                    "overview": "Persist drafts.",
                    "usage": {
                        "command": "draft-sync status",
                        "expected_result": "Print one pending draft."
                    }
                },
                "set": {
                    "entity_changes": [{
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
                        }],
                        "variants": [],
                        "conforms_to": []
                    }],
                    "flows": [{
                        "title": "Draft persistence",
                        "description": "Persist draft observations.",
                        "steps": [{
                            "action": "Read draft observations",
                            "target": {
                                "kind": "planned_entity",
                                "entity": "DraftCache"
                            },
                            "edges": [{
                                "relation": {
                                    "kind": "read",
                                    "callable": {
                                        "kind": "method",
                                        "name": "pending"
                                    }
                                },
                                "target": {
                                    "kind": "planned_entity",
                                    "entity": "DraftCache"
                                },
                                "expansion": [],
                                "result": {
                                    "kind": "type",
                                    "name": "DraftChange[]"
                                }
                            }],
                            "branches": []
                        }]
                    }]
                },
                "assumptions": []
            }),
        };
        let mut output = BackendOutput::default();

        apply_invocation(&invocation, &mut output).unwrap();

        let request = &output.plan_edit[0];
        let set = request.mutation.set.as_ref().unwrap();
        let entity = &set.entity_changes.as_ref().unwrap()[0];
        assert_eq!(entity.name, "DraftCache");
        assert_eq!(entity.members[0].name, "store");
        let flow = &set.flows.as_ref().unwrap()[0];
        assert_eq!(flow.steps[0].edges.len(), 1);
        assert_eq!(
            request.mutation.plan.as_ref().unwrap().usage,
            PatchField::Value(PlanUsage {
                command: "draft-sync status".into(),
                expected_result: "Print one pending draft.".into(),
            })
        );
        assert_eq!(request.mutation.assumptions, Some(Vec::new()));
    }

    #[test]
    fn rejects_legacy_operation_envelopes_with_the_new_patch_shape() {
        let invocation = ControlToolInvocation {
            name: "harness_plan_edit".into(),
            arguments: json!({
                "plan_id": "plan",
                "expected_version": 1,
                "flows": [{
                    "operation": "create",
                    "value": {
                        "title": "Legacy flow",
                        "description": "Uses the removed operation envelope.",
                        "steps": []
                    }
                }]
            }),
        };

        let error = apply_invocation(&invocation, &mut BackendOutput::default()).unwrap_err();
        let error = error
            .downcast_ref::<ControlToolArgumentError>()
            .expect("legacy operation envelope must fail argument validation");
        let violation = error
            .violation
            .iter()
            .find(|violation| violation.path == "flows")
            .expect("legacy top-level collection must report its exact path");

        assert_eq!(violation.code, "unknown_field");
        assert!(
            violation
                .hint
                .as_deref()
                .is_some_and(|hint| hint.contains("set.flows") && hint.contains("delete.flows"))
        );
    }

    #[test]
    fn rejects_key_value_wrappers_with_direct_set_guidance() {
        let invocation = ControlToolInvocation {
            name: "harness_plan_edit".into(),
            arguments: json!({
                "plan_id": "plan",
                "expected_version": 1,
                "set": {
                    "flows": [{
                        "key": "Draft persistence",
                        "value": {
                            "title": "Draft persistence",
                            "description": "Persist draft observations.",
                            "steps": []
                        }
                    }]
                }
            }),
        };

        let error = apply_invocation(&invocation, &mut BackendOutput::default()).unwrap_err();
        let error = error
            .downcast_ref::<ControlToolArgumentError>()
            .expect("key/value wrapper must fail argument validation");

        assert!(
            error.violation.iter().any(|violation| {
                violation.path == "set.flows[0]"
                    && violation.code == "unknown_field"
                    && violation
                        .hint
                        .as_deref()
                        .is_some_and(|hint| hint.contains("without a key/value wrapper"))
            }),
            "unexpected violations: {:#?}",
            error.violation
        );
    }

    #[test]
    fn reports_missing_implementation_actions_at_the_complete_resource_paths() {
        let invocation = ControlToolInvocation {
            name: "harness_plan_edit".into(),
            arguments: json!({
                "plan_id": "plan",
                "expected_version": 1,
                "set": {
                    "dependencies": [{
                        "name": "durable-cache",
                        "version": "1",
                        "manifest": "Cargo.toml",
                        "license": "MIT",
                        "justification": "Provides durable storage."
                    }],
                    "entity_changes": [{
                        "action": "add",
                        "kind": "enum",
                        "name": "InspectionError",
                        "description": "Classifies inspection failures.",
                        "path": "src/inspection.rs",
                        "members": [],
                        "variants": [{
                            "action": "add",
                            "name": "Read",
                            "description": "Carries one read failure.",
                            "fields": [{
                                "name": "source",
                                "type": "String"
                            }]
                        }],
                        "extends": null,
                        "conforms_to": []
                    }]
                }
            }),
        };

        let error = apply_invocation(&invocation, &mut BackendOutput::default()).unwrap_err();
        let error = error
            .downcast_ref::<ControlToolArgumentError>()
            .expect("missing implementation actions must fail argument validation");
        let path_list = error
            .violation
            .iter()
            .filter(|violation| violation.code == "missing_field")
            .map(|violation| violation.path.as_str())
            .collect::<Vec<_>>();

        assert!(path_list.contains(&"set.dependencies[0]"));
        assert!(path_list.contains(&"set.entity_changes[0].variants[0].fields[0]"));
        assert!(error.violation.iter().all(|violation| {
            violation
                .hint
                .as_deref()
                .is_some_and(|hint| hint.contains("implementation `action`"))
        }));
    }

    #[test]
    fn rejects_recursive_mutations_and_generated_node_id_fields() {
        let recursive = ControlToolInvocation {
            name: "harness_plan_edit".into(),
            arguments: json!({
                "plan_id": "plan",
                "expected_version": 1,
                "set": {
                    "flows": {
                        "modify": []
                    }
                }
            }),
        };
        let recursive_error =
            apply_invocation(&recursive, &mut BackendOutput::default()).unwrap_err();
        let recursive_error = recursive_error
            .downcast_ref::<ControlToolArgumentError>()
            .expect("recursive shape must fail argument validation");
        assert!(recursive_error.violation.iter().any(|violation| {
            violation.path == "set.flows" && violation.code == "type_mismatch"
        }));

        let generated_node_id = ControlToolInvocation {
            name: "harness_plan_edit".into(),
            arguments: json!({
                "plan_id": "plan",
                "expected_version": 1,
                "set": {
                    "entity_changes": [{
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
        let node_id_error =
            apply_invocation(&generated_node_id, &mut BackendOutput::default()).unwrap_err();
        let node_id_error = node_id_error
            .downcast_ref::<ControlToolArgumentError>()
            .expect("generated node IDs must fail argument validation");
        assert!(
            node_id_error.violation.iter().any(|violation| {
                violation.path == "set.entity_changes[0].entity_id"
                    && violation.code == "unknown_field"
                    && violation.message.contains("entity_id")
            }),
            "unexpected violations: {:#?}",
            node_id_error.violation
        );
    }

    #[test]
    fn reports_exact_set_paths_and_all_independent_violations() {
        let invocation = ControlToolInvocation {
            name: "harness_plan_edit".into(),
            arguments: json!({
                "expected_version": 0,
                "unexpected": true,
                "set": {
                    "flows": [{
                        "title": "Reader",
                        "description": "Read input.",
                        "steps": [{
                            "action": "Read",
                            "target": { "entity": "reader" },
                            "edges": [],
                            "branches": []
                        }]
                    }]
                }
            }),
        };
        let error = apply_invocation(&invocation, &mut BackendOutput::default()).unwrap_err();
        let error = error
            .downcast_ref::<ControlToolArgumentError>()
            .expect("invalid request must fail argument validation");
        let path_list = error
            .violation
            .iter()
            .map(|violation| violation.path.as_str())
            .collect::<Vec<_>>();

        assert!(path_list.contains(&"<arguments>"));
        assert!(path_list.contains(&"expected_version"));
        assert!(
            path_list
                .iter()
                .any(|path| path.starts_with("set.flows[0]"))
        );
    }
}
