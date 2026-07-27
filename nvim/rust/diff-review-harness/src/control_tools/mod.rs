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

pub mod runtime;
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
                description: "Atomically edit the broker-created canonical plan with optimistic version checking. Group changes by resource. Every resource uses add, modify, and remove. Nested resources use the same vocabulary inside their owner. Model each changed program construct once as an entity_change. Model each concrete test once as a flat task-file subtask whose operation is test.",
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
                description: "Record an execution-time informational or scope deviation. Scope deviations carry the same resource-oriented proposed_changes shape as harness_plan_edit.",
                input_schema: plan_deviation_input_schema(),
            },
            ControlToolDefinition {
                name: "harness_plan_task_report",
                description: "Complete or block the active whole-plan task with subtask, entity, path, and test evidence. Reference a planned test through its test_subtask_id. Harness validates the evidence and selects the next task.",
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

static CONTROL_TOOL_VALIDATOR_MAP: OnceLock<HashMap<&'static str, jsonschema::Validator>> =
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
                let validator = jsonschema::validator_for(&definition.input_schema)
                    .expect("Harness control-tool schemas must compile");
                (definition.name, validator)
            })
            .collect()
    });
    let validator = validator_map
        .get(invocation.name.as_str())
        .with_context(|| format!("unknown Harness control tool: {}", invocation.name))?;
    let mut violation_list = validator
        .iter_errors(&invocation.arguments)
        .map(|error| {
            let path = json_pointer_to_path(&error.instance_path().to_string());
            let location = if path.is_empty() {
                "<arguments>".to_owned()
            } else {
                path
            };
            format!("- {location}: {error}")
        })
        .collect::<Vec<_>>();
    violation_list.sort();
    violation_list.dedup();
    if violation_list.is_empty() {
        return Ok(());
    }
    let typed_detail = match invocation.name.as_str() {
        "harness_plan_edit" => first_typed_violation::<PlanEditRequest>(invocation),
        _ => None,
    };
    let typed_detail = typed_detail
        .map(|detail| format!("\nTyped decoding detail:\n- {detail}"))
        .unwrap_or_default();
    anyhow::bail!(
        "{} arguments contain {} structural violation(s)\n{}{}",
        invocation.name,
        violation_list.len(),
        violation_list.join("\n"),
        typed_detail
    );
}

fn first_typed_violation<T>(invocation: &ControlToolInvocation) -> Option<String>
where
    T: for<'de> Deserialize<'de>,
{
    let encoded = serde_json::to_vec(&invocation.arguments).ok()?;
    let mut deserializer = serde_json::Deserializer::from_slice(&encoded);
    serde_path_to_error::deserialize::<_, T>(&mut deserializer)
        .err()
        .map(|error| format!("{} at JSON path {}", error.inner(), error.path()))
}

fn json_pointer_to_path(pointer: &str) -> String {
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

fn change_action_schema() -> Value {
    json!({ "type": "string", "enum": ["add", "modify", "remove"] })
}

fn entity_change_action_schema() -> Value {
    json!({ "type": "string", "enum": ["add", "modify", "remove", "rename"] })
}

fn entity_kind_schema() -> Value {
    json!({
        "type": "string",
        "enum": [
            "class", "abstract_class", "struct", "enum", "trait", "interface",
            "app", "config", "function", "fn", "method", "constant", "field",
            "resource", "cache", "adapter"
        ]
    })
}

fn member_kind_schema() -> Value {
    json!({
        "type": "string",
        "enum": ["field", "method", "function", "fn", "constant", "property"]
    })
}

fn visibility_schema() -> Value {
    json!({
        "type": ["string", "null"],
        "enum": ["public", "protected", "internal", "private", null]
    })
}

fn plan_usage_input_schema() -> Value {
    json!({
        "type": ["object", "null"],
        "description": "Use null when caller-facing Usage does not apply.",
        "properties": {
            "command": { "type": "string" },
            "expected_result": { "type": "string" }
        },
        "required": ["command", "expected_result"],
        "additionalProperties": false
    })
}

fn entity_reference_input_schema() -> Value {
    json!({
        "description": "Use planned_entity for one entity in entity_changes, workspace_entity for an unchanged repository construct, and external_entity for a dependency or runtime boundary.",
        "oneOf": [
            strict_object_input_schema(
                vec![
                    ("kind", json!({ "type": "string", "const": "planned_entity" })),
                    ("entity", string_schema()),
                ],
                &["kind", "entity"],
            ),
            strict_object_input_schema(
                vec![
                    ("kind", json!({ "type": "string", "const": "workspace_entity" })),
                    (
                        "entity_kind",
                        json!({ "type": "string", "enum": ["type", "endpoint"] }),
                    ),
                    ("name", string_schema()),
                    ("path", string_schema()),
                    ("line", json!({ "type": "integer", "minimum": 1 })),
                ],
                &["kind", "entity_kind", "name", "path", "line"],
            ),
            strict_object_input_schema(
                vec![
                    ("kind", json!({ "type": "string", "const": "external_entity" })),
                    (
                        "entity_kind",
                        json!({ "type": "string", "enum": ["type", "endpoint"] }),
                    ),
                    ("name", string_schema()),
                    ("dependency", nullable_string_schema()),
                ],
                &["kind", "entity_kind", "name"],
            ),
        ]
    })
}

fn function_parameter_input_schema() -> Value {
    strict_object_input_schema(
        vec![("name", string_schema()), ("type", string_schema())],
        &["name", "type"],
    )
}

fn enum_variant_field_input_schema() -> Value {
    strict_object_input_schema(
        vec![
            ("action", change_action_schema()),
            ("name", string_schema()),
            ("type", string_schema()),
        ],
        &["action", "name", "type"],
    )
}

fn enum_variant_field_patch_input_schema() -> Value {
    strict_object_input_schema(
        vec![
            ("field", string_schema()),
            ("action", change_action_schema()),
            ("name", string_schema()),
            ("type", string_schema()),
        ],
        &["field"],
    )
}

fn enum_variant_input_schema() -> Value {
    strict_object_input_schema(
        vec![
            ("action", change_action_schema()),
            ("name", string_schema()),
            ("description", string_schema()),
            (
                "fields",
                json!({ "type": "array", "items": enum_variant_field_input_schema() }),
            ),
        ],
        &["action", "name", "description"],
    )
}

fn enum_variant_patch_input_schema() -> Value {
    strict_object_input_schema(
        vec![
            ("variant", string_schema()),
            ("action", change_action_schema()),
            ("name", string_schema()),
            ("description", string_schema()),
            (
                "fields",
                collection_mutation_schema(
                    enum_variant_field_input_schema(),
                    enum_variant_field_patch_input_schema(),
                ),
            ),
        ],
        &["variant"],
    )
}

fn entity_member_input_schema() -> Value {
    strict_object_input_schema(
        vec![
            ("action", change_action_schema()),
            ("kind", member_kind_schema()),
            ("name", string_schema()),
            ("description", string_schema()),
            ("visibility", visibility_schema()),
            ("type", nullable_string_schema()),
            (
                "parameters",
                json!({ "type": "array", "items": function_parameter_input_schema() }),
            ),
            ("return_type", nullable_string_schema()),
        ],
        &["action", "kind", "name", "description"],
    )
}

fn entity_member_patch_input_schema() -> Value {
    strict_object_input_schema(
        vec![
            ("member", string_schema()),
            ("action", change_action_schema()),
            ("kind", member_kind_schema()),
            ("name", string_schema()),
            ("description", string_schema()),
            ("visibility", visibility_schema()),
            ("type", nullable_string_schema()),
            (
                "parameters",
                json!({ "type": "array", "items": function_parameter_input_schema() }),
            ),
            ("return_type", nullable_string_schema()),
        ],
        &["member"],
    )
}

fn entity_change_input_schema() -> Value {
    let mut schema = strict_object_input_schema(
        vec![
            ("action", entity_change_action_schema()),
            ("kind", entity_kind_schema()),
            ("renamed_from", string_schema()),
            ("name", string_schema()),
            ("description", string_schema()),
            (
                "path",
                json!({
                    "type": "string",
                    "description": "Repository-relative source path. A path names a file, not a module."
                }),
            ),
            (
                "members",
                json!({ "type": "array", "items": entity_member_input_schema() }),
            ),
            (
                "variants",
                json!({ "type": "array", "items": enum_variant_input_schema() }),
            ),
            ("extends", nullable_reference_schema()),
            (
                "conforms_to",
                json!({ "type": "array", "items": entity_reference_input_schema() }),
            ),
        ],
        &["action", "kind", "name", "description", "path"],
    );
    schema
        .as_object_mut()
        .expect("entity change schema")
        .insert(
            "allOf".into(),
            json!([
                {
                    "if": { "properties": { "action": { "const": "rename" } } },
                    "then": { "required": ["renamed_from"] }
                },
                {
                    "if": {
                        "properties": {
                            "action": { "enum": ["add", "modify", "remove"] }
                        }
                    },
                    "then": { "not": { "required": ["renamed_from"] } }
                }
            ]),
        );
    schema
}

fn entity_change_patch_input_schema() -> Value {
    strict_object_input_schema(
        vec![
            ("entity", string_schema()),
            ("action", entity_change_action_schema()),
            ("kind", entity_kind_schema()),
            ("renamed_from", nullable_string_schema()),
            ("name", string_schema()),
            ("description", string_schema()),
            ("path", string_schema()),
            (
                "members",
                collection_mutation_schema(
                    entity_member_input_schema(),
                    entity_member_patch_input_schema(),
                ),
            ),
            (
                "variants",
                collection_mutation_schema(
                    enum_variant_input_schema(),
                    enum_variant_patch_input_schema(),
                ),
            ),
            ("extends", nullable_reference_schema()),
            (
                "conforms_to",
                json!({ "type": "array", "items": entity_reference_input_schema() }),
            ),
        ],
        &["entity"],
    )
}

fn nullable_reference_schema() -> Value {
    json!({
        "oneOf": [
            entity_reference_input_schema(),
            { "type": "null" }
        ]
    })
}

fn dependency_input_schema() -> Value {
    strict_object_input_schema(
        vec![
            ("action", change_action_schema()),
            ("name", string_schema()),
            ("version", string_schema()),
            ("manifest", string_schema()),
            ("license", nullable_string_schema()),
            ("justification", string_schema()),
        ],
        &[
            "action",
            "name",
            "version",
            "manifest",
            "license",
            "justification",
        ],
    )
}

fn dependency_patch_input_schema() -> Value {
    strict_object_input_schema(
        vec![
            ("dependency", string_schema()),
            ("action", change_action_schema()),
            ("name", string_schema()),
            ("version", string_schema()),
            ("manifest", string_schema()),
            ("license", nullable_string_schema()),
            ("justification", string_schema()),
        ],
        &["dependency"],
    )
}

fn flow_value_input_schema() -> Value {
    json!({
        "description": "Use type for a named type crossing the boundary and text for any other value, event, result, or observable effect.",
        "oneOf": [
            strict_object_input_schema(
                vec![
                    ("kind", json!({ "type": "string", "const": "type" })),
                    ("name", string_schema()),
                ],
                &["kind", "name"],
            ),
            strict_object_input_schema(
                vec![
                    ("kind", json!({ "type": "string", "const": "text" })),
                    ("text", string_schema()),
                ],
                &["kind", "text"],
            ),
        ]
    })
}

fn flow_step_input_schema() -> Value {
    json!({ "$ref": "#/$defs/flow_step" })
}

fn flow_step_definition_schema() -> Value {
    strict_object_input_schema(
        vec![
            ("action", string_schema()),
            ("target", entity_reference_input_schema()),
            (
                "edges",
                json!({ "type": "array", "items": flow_edge_input_schema() }),
            ),
            (
                "branches",
                json!({ "type": "array", "items": flow_branch_input_schema() }),
            ),
        ],
        &["action", "target", "edges", "branches"],
    )
}

fn flow_step_patch_input_schema() -> Value {
    strict_object_input_schema(
        vec![
            ("step", string_schema()),
            ("action", string_schema()),
            ("target", entity_reference_input_schema()),
            (
                "edges",
                json!({ "type": "array", "items": flow_edge_input_schema() }),
            ),
            (
                "branches",
                json!({ "type": "array", "items": flow_branch_input_schema() }),
            ),
        ],
        &["step"],
    )
}

fn flow_relation_input_schema() -> Value {
    let callable_schema = || {
        strict_object_input_schema(
            vec![
                (
                    "kind",
                    json!({ "type": "string", "enum": ["function", "method"] }),
                ),
                ("name", string_schema()),
            ],
            &["kind", "name"],
        )
    };
    json!({
        "description": "Typed runtime relationship from the owning flow step to one receiver or endpoint. Call, read, write, and construct targets must resolve to a type entity.",
        "oneOf": [
            strict_object_input_schema(
                vec![("kind", json!({ "type": "string", "const": "construct" }))],
                &["kind"],
            ),
            strict_object_input_schema(
                vec![
                    ("kind", json!({ "type": "string", "const": "call" })),
                    ("callable", callable_schema()),
                ],
                &["kind", "callable"],
            ),
            strict_object_input_schema(
                vec![
                    ("kind", json!({ "type": "string", "const": "read" })),
                    ("callable", callable_schema()),
                ],
                &["kind", "callable"],
            ),
            strict_object_input_schema(
                vec![
                    ("kind", json!({ "type": "string", "const": "write" })),
                    ("callable", callable_schema()),
                ],
                &["kind", "callable"],
            ),
            strict_object_input_schema(
                vec![
                    ("kind", json!({ "type": "string", "const": "send" })),
                    ("event", string_schema()),
                ],
                &["kind", "event"],
            ),
            strict_object_input_schema(
                vec![("kind", json!({ "type": "string", "const": "emit" }))],
                &["kind"],
            ),
            strict_object_input_schema(
                vec![("kind", json!({ "type": "string", "const": "return" }))],
                &["kind"],
            ),
        ]
    })
}

fn flow_edge_input_schema() -> Value {
    json!({ "$ref": "#/$defs/flow_edge" })
}

fn flow_edge_definition_schema() -> Value {
    strict_object_input_schema(
        vec![
            ("relation", flow_relation_input_schema()),
            ("target", entity_reference_input_schema()),
            (
                "expansion",
                json!({ "type": "array", "items": flow_step_input_schema() }),
            ),
            (
                "result",
                json!({ "oneOf": [flow_value_input_schema(), { "type": "null" }] }),
            ),
        ],
        &["relation", "target", "expansion", "result"],
    )
}

fn flow_branch_input_schema() -> Value {
    json!({ "$ref": "#/$defs/flow_branch" })
}

fn flow_branch_definition_schema() -> Value {
    strict_object_input_schema(
        vec![
            ("condition", string_schema()),
            (
                "steps",
                json!({ "type": "array", "items": flow_step_input_schema() }),
            ),
        ],
        &["condition", "steps"],
    )
}

fn flow_definition_map() -> Value {
    json!({
        "flow_step": flow_step_definition_schema(),
        "flow_edge": flow_edge_definition_schema(),
        "flow_branch": flow_branch_definition_schema()
    })
}

fn attach_flow_definitions(mut schema: Value) -> Value {
    schema["$defs"] = flow_definition_map();
    schema
}

fn flow_input_schema() -> Value {
    strict_object_input_schema(
        vec![
            ("title", string_schema()),
            ("description", string_schema()),
            (
                "steps",
                json!({ "type": "array", "items": flow_step_input_schema() }),
            ),
        ],
        &["title", "description", "steps"],
    )
}

fn flow_patch_input_schema() -> Value {
    strict_object_input_schema(
        vec![
            ("flow", string_schema()),
            ("title", string_schema()),
            ("description", string_schema()),
            (
                "steps",
                collection_mutation_schema(
                    flow_step_input_schema(),
                    flow_step_patch_input_schema(),
                ),
            ),
        ],
        &["flow"],
    )
}

fn work_subtask_input_schema() -> Value {
    strict_object_input_schema(
        vec![
            (
                "operation",
                json!({
                    "type": "string",
                    "enum": [
                        "expose", "encapsulate", "move", "centralize", "distribute",
                        "extract", "inline", "split", "merge", "compose", "embed", "create",
                        "destroy", "register", "unregister", "attach", "detach", "start",
                        "stop", "route", "resolve", "defer", "configure", "relax", "enable",
                        "disable", "reuse", "generalize", "specialize"
                    ]
                }),
            ),
            ("description", string_schema()),
            (
                "entities",
                json!({
                    "type": "array",
                    "items": { "type": "string" },
                    "description": "Complete replacement list of planned program entities owned by this subtask. Use [] for a dependency-only manifest subtask. Never put package dependencies here and never use add, modify, or remove inside this field."
                }),
            ),
        ],
        &["operation", "description"],
    )
}

fn test_subtask_input_schema() -> Value {
    strict_object_input_schema(
        vec![
            (
                "operation",
                json!({
                    "type": "string",
                    "const": "test",
                    "description": "Identifies one flat test-edit subtask. Test fields live directly on this object."
                }),
            ),
            ("action", change_action_schema()),
            ("name", string_schema()),
            (
                "category",
                json!({ "type": "string", "enum": ["unit", "integration"] }),
            ),
            ("behavior", string_schema()),
            (
                "covers_entities",
                json!({
                    "type": "array",
                    "items": { "type": "string" },
                    "description": "Optional production-entity traceability for this test. These references never establish ownership."
                }),
            ),
        ],
        &["operation", "action", "name", "category", "behavior"],
    )
}

fn subtask_input_schema() -> Value {
    json!({
        "oneOf": [
            work_subtask_input_schema(),
            test_subtask_input_schema()
        ]
    })
}

fn work_subtask_patch_input_schema() -> Value {
    let mut schema = work_subtask_input_schema();
    schema["required"] = json!(["subtask"]);
    schema["properties"]["subtask"] = json!({
        "type": "string",
        "description": "Required selector naming the existing implementation subtask to modify."
    });
    schema
}

fn test_subtask_patch_input_schema() -> Value {
    let mut schema = test_subtask_input_schema();
    schema["required"] = json!(["subtask", "operation"]);
    schema["properties"]["subtask"] = json!({
        "type": "string",
        "description": "Required selector naming the existing test subtask to modify."
    });
    schema
}

fn subtask_patch_input_schema() -> Value {
    json!({
        "oneOf": [
            work_subtask_patch_input_schema(),
            test_subtask_patch_input_schema()
        ]
    })
}

fn file_input_schema() -> Value {
    let subtasks = json!({ "type": "array", "items": subtask_input_schema() });
    json!({
        "oneOf": [
            strict_object_input_schema(
                vec![
                    ("action", json!({ "type": "string", "const": "add" })),
                    ("path", string_schema()),
                    ("subtasks", subtasks.clone()),
                ],
                &["action", "path"],
            ),
            strict_object_input_schema(
                vec![
                    ("action", json!({ "type": "string", "const": "modify" })),
                    ("path", string_schema()),
                    ("subtasks", subtasks.clone()),
                ],
                &["action", "path"],
            ),
            strict_object_input_schema(
                vec![
                    ("action", json!({ "type": "string", "const": "remove" })),
                    ("path", string_schema()),
                    ("subtasks", subtasks.clone()),
                ],
                &["action", "path"],
            ),
            strict_object_input_schema(
                vec![
                    ("action", json!({ "type": "string", "const": "rename" })),
                    ("from", string_schema()),
                    ("to", string_schema()),
                    ("subtasks", subtasks),
                ],
                &["action", "from", "to"],
            ),
        ]
    })
}

fn file_change_input_schema() -> Value {
    json!({
        "oneOf": [
            strict_object_input_schema(
                vec![
                    ("action", json!({ "type": "string", "const": "add" })),
                    ("path", string_schema()),
                ],
                &["action", "path"],
            ),
            strict_object_input_schema(
                vec![
                    ("action", json!({ "type": "string", "const": "modify" })),
                    ("path", string_schema()),
                ],
                &["action", "path"],
            ),
            strict_object_input_schema(
                vec![
                    ("action", json!({ "type": "string", "const": "remove" })),
                    ("path", string_schema()),
                ],
                &["action", "path"],
            ),
            strict_object_input_schema(
                vec![
                    ("action", json!({ "type": "string", "const": "rename" })),
                    ("from", string_schema()),
                    ("to", string_schema()),
                ],
                &["action", "from", "to"],
            ),
        ]
    })
}

fn file_patch_input_schema() -> Value {
    strict_object_input_schema(
        vec![
            ("path", string_schema()),
            ("change", file_change_input_schema()),
            (
                "subtasks",
                collection_mutation_schema(subtask_input_schema(), subtask_patch_input_schema()),
            ),
        ],
        &["path"],
    )
}

fn task_input_schema() -> Value {
    strict_object_input_schema(
        vec![
            ("title", string_schema()),
            ("description", string_schema()),
            (
                "files",
                json!({ "type": "array", "items": file_input_schema() }),
            ),
        ],
        &["title", "description"],
    )
}

fn task_patch_input_schema() -> Value {
    strict_object_input_schema(
        vec![
            ("task", string_schema()),
            ("title", string_schema()),
            ("description", string_schema()),
            (
                "files",
                collection_mutation_schema(file_input_schema(), file_patch_input_schema()),
            ),
        ],
        &["task"],
    )
}

fn assumption_input_schema() -> Value {
    string_schema()
}

fn assumption_patch_input_schema() -> Value {
    strict_object_input_schema(
        vec![("assumption", string_schema()), ("text", string_schema())],
        &["assumption", "text"],
    )
}

fn collection_mutation_schema(add_schema: Value, modify_schema: Value) -> Value {
    strict_object_input_schema(
        vec![
            ("add", json!({ "type": "array", "items": add_schema })),
            ("modify", json!({ "type": "array", "items": modify_schema })),
            ("remove", string_array_schema()),
        ],
        &[],
    )
}

fn plan_field_mutation_schema() -> Value {
    strict_object_input_schema(
        vec![(
            "modify",
            strict_object_input_schema(
                vec![
                    ("title", string_schema()),
                    ("overview", string_schema()),
                    ("usage", plan_usage_input_schema()),
                ],
                &[],
            ),
        )],
        &["modify"],
    )
}

fn plan_mutation_property_list() -> Vec<(&'static str, Value)> {
    vec![
        ("plan", plan_field_mutation_schema()),
        (
            "entity_changes",
            collection_mutation_schema(
                entity_change_input_schema(),
                entity_change_patch_input_schema(),
            ),
        ),
        (
            "dependencies",
            collection_mutation_schema(dependency_input_schema(), dependency_patch_input_schema()),
        ),
        (
            "flows",
            collection_mutation_schema(flow_input_schema(), flow_patch_input_schema()),
        ),
        (
            "tasks",
            collection_mutation_schema(task_input_schema(), task_patch_input_schema()),
        ),
        (
            "assumptions",
            collection_mutation_schema(assumption_input_schema(), assumption_patch_input_schema()),
        ),
    ]
}

fn plan_edit_input_schema() -> Value {
    let mut property_list = vec![
        ("plan_id", string_schema()),
        (
            "expected_version",
            json!({ "type": "integer", "minimum": 1 }),
        ),
    ];
    property_list.extend(plan_mutation_property_list());
    let mut schema = strict_object_input_schema(property_list, &["plan_id", "expected_version"]);
    schema["anyOf"] = json!([
        { "required": ["plan"] },
        { "required": ["entity_changes"] },
        { "required": ["dependencies"] },
        { "required": ["flows"] },
        { "required": ["tasks"] },
        { "required": ["assumptions"] }
    ]);
    attach_flow_definitions(schema)
}

fn plan_deviation_input_schema() -> Value {
    let proposed_changes = strict_object_input_schema(plan_mutation_property_list(), &[]);
    attach_flow_definitions(strict_object_input_schema(
        vec![
            ("plan_id", string_schema()),
            (
                "kind",
                json!({ "type": "string", "enum": ["informational", "scope"] }),
            ),
            ("summary", string_schema()),
            ("reason", string_schema()),
            ("task_id", nullable_string_schema()),
            ("subtask_id", nullable_string_schema()),
            ("affected_paths", string_array_schema()),
            ("proposed_changes", proposed_changes),
        ],
        &["plan_id", "kind", "summary", "reason", "proposed_changes"],
    ))
}

fn plan_task_report_input_schema() -> Value {
    strict_object_input_schema(
        vec![
            ("execution_id", string_schema()),
            ("task_id", string_schema()),
            (
                "state",
                json!({ "type": "string", "enum": ["complete", "blocked"] }),
            ),
            ("completed_subtask_ids", string_array_schema()),
            ("completed_entity_ids", string_array_schema()),
            (
                "test_results",
                json!({
                    "type": "array",
                    "items": strict_object_input_schema(
                        vec![
                            ("test_subtask_id", nullable_string_schema()),
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
        &["execution_id", "task_id", "state"],
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
    use crate::plan::{
        EntityReference, PatchField, PlanCallableKind, PlanFlowRelation, PlanFlowValue,
        PlanSubtask, PlanUsage, ReferencedEntityKind, TestCategory,
    };

    #[test]
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
        assert!(entity.entity_id.is_empty());
        assert!(entity.members[0].member_id.is_empty());
        let enum_entity = &request.mutation.entity_changes.as_ref().unwrap().add[1];
        assert_eq!(enum_entity.variants[0].name, "Failed");
        assert_eq!(enum_entity.variants[0].fields[0].name, "message");
        let dependency = &request.mutation.dependencies.as_ref().unwrap().add[0];
        assert_eq!(dependency.name, "tokio");
        assert_eq!(dependency.version, "1");
        assert!(dependency.dependency_id.is_empty());
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
        assert!(edge_list.iter().all(|edge| edge.edge_id.is_empty()));
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
        assert_eq!(work.entity_ids, ["DraftCache", "DraftStatus"]);
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
}
