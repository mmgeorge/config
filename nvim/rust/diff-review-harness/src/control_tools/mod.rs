use crate::backend::{BackendOutput, PlanSubmitRequest};
use crate::plan::{
    PlanDeviationRequest, PlanDocument, PlanEditRequest, PlanQuestionAnswer, PlanQuestionSet,
    PlanQuestionWithdrawal, PlanTaskReport,
};
use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use serde_json::{Value, json};
use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader};

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
                name: "harness_plan_create",
                description: "Create the canonical JSON plan document. Use stable semantic IDs for definitions, flows, tasks, subtasks, code edits, tests, and assumptions.",
                input_schema: json!({
                    "type": "object",
                    "properties": { "document": plan_document_input_schema() },
                    "required": ["document"]
                }),
            },
            ControlToolDefinition {
                name: "harness_plan_edit",
                description: "Atomically edit the active canonical plan with semantic operations and optimistic version checking. Definition updates replace complete member or enum variant lists.",
                input_schema: plan_edit_input_schema(),
            },
            ControlToolDefinition {
                name: "harness_plan_read",
                description: "Request the current canonical JSON plan by stable plan ID. The next Harness planning context includes the complete document.",
                input_schema: json!({
                    "type": "object",
                    "properties": { "plan_id": { "type": "string" } },
                    "required": ["plan_id"]
                }),
            },
            ControlToolDefinition {
                name: "harness_plan_submit",
                description: "Submit the exact validated canonical plan version for mandatory user review.",
                input_schema: json!({
                    "type": "object",
                    "properties": {
                        "plan_id": { "type": "string" },
                        "expected_version": { "type": "integer", "minimum": 1 }
                    },
                    "required": ["plan_id", "expected_version"]
                }),
            },
            ControlToolDefinition {
                name: "harness_plan_deviation",
                description: "Record an execution-time informational or scope deviation. Scope deviations include semantic plan operations and follow the configured review policy.",
                input_schema: plan_deviation_input_schema(),
            },
            ControlToolDefinition {
                name: "harness_plan_task_report",
                description: "Complete or block the active whole-plan task with subtask, code-edit, path, and test evidence. Harness then selects the next task.",
                input_schema: plan_task_report_input_schema(),
            },
            ControlToolDefinition {
                name: "harness_plan_question",
                description: "Pause any Harness turn and present one to three interactive user questions. Use this for explicit requests for multiple-choice questions as well as planning decisions.",
                input_schema: plan_question_input_schema(),
            },
            ControlToolDefinition {
                name: "harness_question_answer",
                description: "Record an answer only when the user explicitly and unambiguously answers one pending Harness question.",
                input_schema: question_answer_input_schema(),
            },
            ControlToolDefinition {
                name: "harness_question_withdraw",
                description: "Withdraw pending Harness questions only when no material user decision remains.",
                input_schema: question_withdraw_input_schema(),
            },
            ControlToolDefinition {
                name: "harness_goal_complete",
                description: "Mark the active Harness goal complete only after every required task finishes.",
                input_schema: json!({ "type": "object", "properties": { "summary": { "type": "string" } }, "required": ["summary"] }),
            },
            ControlToolDefinition {
                name: "harness_goal_blocked",
                description: "Mark the active Harness goal blocked with concrete evidence.",
                input_schema: json!({ "type": "object", "properties": { "reason": { "type": "string" } }, "required": ["reason"] }),
            },
            ControlToolDefinition {
                name: "harness_goal_status",
                description: "Report nonterminal progress toward the active Harness goal.",
                input_schema: json!({ "type": "object", "properties": { "status": { "type": "string" } }, "required": ["status"] }),
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

/// Apply one provider-neutral control invocation to the normalized turn result.
pub fn apply_invocation(
    invocation: &ControlToolInvocation,
    output: &mut BackendOutput,
) -> Result<()> {
    match invocation.name.as_str() {
        "harness_plan_create" => {
            output.plan_document = Some(serde_json::from_value::<PlanDocument>(
                invocation
                    .arguments
                    .get("document")
                    .cloned()
                    .context("harness_plan_create requires document")?,
            )?);
            output.structured_plan = true;
        }
        "harness_plan_edit" => {
            output
                .plan_edit
                .push(serde_json::from_value::<PlanEditRequest>(
                    invocation.arguments.clone(),
                )?);
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
            output.plan_submit = Some(serde_json::from_value::<PlanSubmitRequest>(
                invocation.arguments.clone(),
            )?);
            output.structured_plan = true;
        }
        "harness_plan_deviation" => {
            output
                .plan_deviation
                .push(serde_json::from_value::<PlanDeviationRequest>(
                    invocation.arguments.clone(),
                )?);
            output.structured_plan = true;
        }
        "harness_plan_task_report" => {
            output
                .plan_task_report
                .push(serde_json::from_value::<PlanTaskReport>(
                    invocation.arguments.clone(),
                )?);
            output.structured_plan = true;
        }
        "harness_plan_question" => {
            output.plan_question = Some(
                serde_json::from_value::<PlanQuestionSet>(invocation.arguments.clone())?
                    .normalize()?,
            );
        }
        "harness_question_answer" => {
            output.question_answer = Some(serde_json::from_value::<PlanQuestionAnswer>(
                invocation.arguments.clone(),
            )?);
        }
        "harness_question_withdraw" => {
            output.question_withdrawal = Some(serde_json::from_value::<PlanQuestionWithdrawal>(
                invocation.arguments.clone(),
            )?);
        }
        "harness_goal_complete" => output.evidence.structured_complete = true,
        "harness_goal_blocked" => output.evidence.structured_blocked = true,
        "harness_goal_status" => output.evidence.tool_called = true,
        name => anyhow::bail!("unknown Harness control tool: {name}"),
    }
    Ok(())
}

fn plan_document_input_schema() -> Value {
    let parameter = json!({
        "type": "object",
        "properties": { "name": { "type": "string" }, "type_name": { "type": "string" } },
        "required": ["name", "type_name"]
    });
    let member = json!({
        "type": "object",
        "properties": {
            "id": { "type": "string" },
            "action": { "type": "string", "enum": ["add", "modify", "remove"] },
            "kind": { "type": "string", "enum": ["field", "method", "function", "constant", "property"] },
            "name": { "type": "string" },
            "visibility": { "type": "string", "enum": ["public", "protected", "internal", "private"] },
            "type_name": { "type": ["string", "null"] },
            "parameters": { "type": "array", "items": parameter },
            "return_type": { "type": ["string", "null"] }
        },
        "required": ["id", "action", "kind", "name", "visibility", "parameters"]
    });
    let variant = json!({
        "type": "object",
        "properties": {
            "id": { "type": "string" },
            "name": { "type": "string" },
            "fields": { "type": "array", "items": {
                "type": "object",
                "properties": { "name": { "type": "string" }, "type_name": { "type": "string" } },
                "required": ["name", "type_name"]
            } }
        },
        "required": ["id", "name", "fields"]
    });
    let definition = json!({
        "type": "object",
        "properties": {
            "id": { "type": "string" },
            "action": { "type": "string", "enum": ["add", "modify", "remove"] },
            "kind": { "type": "string", "enum": ["trait", "interface", "abstract_class", "class", "struct", "enum", "config", "resource", "type_alias"] },
            "name": { "type": "string" },
            "path": { "type": "string", "description": "Repository-relative source path" },
            "conforms_to": { "type": "array", "items": { "type": "string" } },
            "extends": { "type": ["string", "null"] },
            "exclusive_parent_id": { "type": ["string", "null"] },
            "members": { "type": "array", "items": member },
            "variants": { "type": "array", "items": variant }
        },
        "required": ["id", "action", "kind", "name", "path", "conforms_to", "members", "variants"]
    });
    let flow_step = json!({
        "type": "object",
        "properties": {
            "id": { "type": "string" }, "action": { "type": "string" },
            "location": { "type": "string" }, "value_to_next": { "type": ["string", "null"] },
            "order": { "type": "integer", "minimum": 0 }
        },
        "required": ["id", "action", "location", "order"]
    });
    let flow = json!({
        "type": "object",
        "properties": {
            "id": { "type": "string" }, "title": { "type": "string" },
            "rationale": { "type": "string" }, "order": { "type": "integer", "minimum": 0 },
            "steps": { "type": "array", "items": flow_step }
        },
        "required": ["id", "title", "rationale", "order", "steps"]
    });
    let code_edit = json!({
        "type": "object",
        "properties": {
            "id": { "type": "string" },
            "action": { "type": "string", "enum": ["add", "modify", "remove"] },
            "kind": { "type": "string", "enum": ["class", "struct", "enum", "trait", "interface", "test", "app", "config", "function", "method", "constant", "field", "resource", "cache", "adapter"] },
            "target": { "type": "string" }, "description": { "type": "string" },
            "definition_id": { "type": ["string", "null"] }, "member_id": { "type": ["string", "null"] }
        },
        "required": ["id", "action", "kind", "target", "description"]
    });
    let subtask = json!({
        "type": "object",
        "properties": {
            "id": { "type": "string" }, "title": { "type": "string" },
            "detail": { "type": "string" }, "order": { "type": "integer", "minimum": 0 },
            "code_edits": { "type": "array", "items": code_edit }
        },
        "required": ["id", "title", "detail", "order", "code_edits"]
    });
    let plan_file = json!({
        "type": "object",
        "properties": {
            "path": { "type": "string", "description": "Repository-relative source path" },
            "subtasks": { "type": "array", "items": subtask }
        },
        "required": ["path", "subtasks"]
    });
    let task = json!({
        "type": "object",
        "properties": {
            "id": { "type": "string" }, "title": { "type": "string" },
            "rationale": { "type": "string" }, "order": { "type": "integer", "minimum": 0 },
            "files": { "type": "array", "items": plan_file }
        },
        "required": ["id", "title", "rationale", "order", "files"]
    });
    let test_case = json!({
        "type": "object",
        "properties": {
            "id": { "type": "string" }, "title": { "type": "string" },
            "behavior": { "type": "string" }, "mocks": { "type": "array", "items": { "type": "string" } },
            "task_ids": { "type": "array", "items": { "type": "string" } },
            "flow_ids": { "type": "array", "items": { "type": "string" } }
        },
        "required": ["id", "title", "behavior", "mocks", "task_ids", "flow_ids"]
    });
    json!({
        "type": "object",
        "properties": {
            "version": { "type": "integer", "minimum": 1 },
            "plan_id": { "type": "string" },
            "title": { "type": "string" },
            "overview": { "type": "string" },
            "usage": { "type": ["object", "null"], "properties": {
                "kind": { "type": "string", "enum": ["command", "code", "interaction", "omitted"] },
                "input": { "type": "string" }, "expected_result": { "type": "string" }
            }, "required": ["kind", "input", "expected_result"] },
            "definitions": { "type": "array", "items": definition },
            "flows": { "type": "array", "items": flow },
            "tasks": { "type": "array", "items": task },
            "test_plan": { "type": "object", "properties": {
                "unit": { "type": "array", "items": test_case },
                "integration": { "type": "array", "items": test_case }
            }, "required": ["unit", "integration"] },
            "assumptions": { "type": "array", "items": {
                "type": "object", "properties": { "id": { "type": "string" }, "text": { "type": "string" } },
                "required": ["id", "text"]
            } }
        },
        "required": ["version", "plan_id", "title", "overview", "definitions", "flows", "tasks", "test_plan", "assumptions"]
    })
}

fn plan_edit_input_schema() -> Value {
    json!({
        "type": "object",
        "properties": {
            "plan_id": { "type": "string" },
            "expected_version": { "type": "integer", "minimum": 1 },
            "operations": {
                "type": "array",
                "minItems": 1,
                "description": "Semantic operations and fields: document.replace(document), overview.update(text), usage.update(usage), definition.add(definition), definition.update(definition_id, changes), definition.remove(definition_id), flow.add(flow), flow.update(flow_id, changes), flow.remove(flow_id), flow_step.add(flow_id, step), flow_step.update(flow_id, step_id, changes), flow_step.remove(flow_id, step_id), task.add(task), task.update(task_id, changes), task.remove(task_id), file.add(task_id, file), file.update(task_id, path, changes), file.remove(task_id, path), subtask.add(task_id, path, subtask), subtask.update(task_id, path, subtask_id, changes), subtask.remove(task_id, path, subtask_id), code_edit.add(task_id, path, subtask_id, code_edit), code_edit.update(task_id, path, subtask_id, code_edit_id, changes), code_edit.remove(task_id, path, subtask_id, code_edit_id), test_case.add(category, test_case), test_case.update(category, test_case_id, changes), test_case.remove(category, test_case_id), assumption.add(assumption), assumption.update(assumption_id, text), assumption.remove(assumption_id). Definition changes may replace complete members and variants arrays.",
                "items": {
                    "type": "object",
                    "properties": {
                        "operation": {
                            "type": "string",
                            "enum": [
                                "document.replace", "overview.update", "usage.update",
                                "definition.add", "definition.update", "definition.remove",
                                "flow.add", "flow.update", "flow.remove",
                                "flow_step.add", "flow_step.update", "flow_step.remove",
                                "task.add", "task.update", "task.remove",
                                "file.add", "file.update", "file.remove",
                                "subtask.add", "subtask.update", "subtask.remove",
                                "code_edit.add", "code_edit.update", "code_edit.remove",
                                "test_case.add", "test_case.update", "test_case.remove",
                                "assumption.add", "assumption.update", "assumption.remove"
                            ]
                        }
                    },
                    "required": ["operation"],
                    "additionalProperties": true
                }
            }
        },
        "required": ["plan_id", "expected_version", "operations"]
    })
}

fn plan_deviation_input_schema() -> Value {
    json!({
        "type": "object",
        "properties": {
            "plan_id": { "type": "string" },
            "kind": { "type": "string", "enum": ["informational", "scope"] },
            "summary": { "type": "string" },
            "reason": { "type": "string" },
            "task_id": { "type": ["string", "null"] },
            "subtask_id": { "type": ["string", "null"] },
            "affected_paths": { "type": "array", "items": { "type": "string" } },
            "proposed_operations": plan_edit_input_schema()["properties"]["operations"].clone()
        },
        "required": ["plan_id", "kind", "summary", "reason"]
    })
}

fn plan_task_report_input_schema() -> Value {
    json!({
        "type": "object",
        "properties": {
            "execution_id": { "type": "string" },
            "task_id": { "type": "string" },
            "state": { "type": "string", "enum": ["complete", "blocked"] },
            "completed_subtask_ids": { "type": "array", "items": { "type": "string" } },
            "completed_code_edit_ids": { "type": "array", "items": { "type": "string" } },
            "test_results": { "type": "array", "items": { "type": "object" } },
            "changed_paths": { "type": "array", "items": { "type": "string" } },
            "summary": { "type": ["string", "null"] },
            "blocking_reason": { "type": ["string", "null"] }
        },
        "required": ["execution_id", "task_id", "state"]
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
                                "required": ["label", "description"]
                            }
                        },
                        "allowFreeform": { "type": "boolean" }
                    },
                    "required": ["header", "question", "options"]
                }
            }
        },
        "required": ["questions"]
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
                "required": ["kind"]
            }
        },
        "required": ["question_id", "response"]
    })
}

/// Build the structured-input contract for removing a resolved decision boundary.
pub fn question_withdraw_input_schema() -> Value {
    json!({
        "type": "object",
        "properties": { "reason": { "type": "string" } },
        "required": ["reason"]
    })
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

    #[test]
    fn exposes_question_plan_and_goal_control_tools() {
        let tool = ControlToolRegistry.mcp_tool_list();
        let name: Vec<_> = tool
            .iter()
            .filter_map(|tool| tool.get("name").and_then(Value::as_str))
            .collect();
        assert_eq!(
            name,
            [
                "harness_plan_create",
                "harness_plan_edit",
                "harness_plan_read",
                "harness_plan_submit",
                "harness_plan_deviation",
                "harness_plan_task_report",
                "harness_plan_question",
                "harness_question_answer",
                "harness_question_withdraw",
                "harness_goal_complete",
                "harness_goal_blocked",
                "harness_goal_status"
            ]
        );
        let question_schema = plan_question_input_schema();
        assert!(question_schema.pointer("/properties/id").is_some());
        assert!(
            question_schema
                .pointer("/properties/questions/items/properties/id")
                .is_some()
        );
        let edit_schema = plan_edit_input_schema();
        let operation_list = edit_schema
            .pointer("/properties/operations/items/properties/operation/enum")
            .and_then(Value::as_array)
            .unwrap();
        assert!(operation_list.contains(&json!("task.add")));
        assert!(!operation_list.iter().any(|operation| {
            operation
                .as_str()
                .is_some_and(|operation| operation.starts_with("member."))
        }));
    }
}
