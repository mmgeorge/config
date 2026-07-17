use crate::backend::BackendOutput;
use crate::plan::{PlanQuestionAnswer, PlanQuestionSet, PlanQuestionWithdrawal};
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
                name: "harness_plan_submit",
                description: "Submit the complete Markdown plan for mandatory user review.",
                input_schema: json!({ "type": "object", "properties": { "markdown": { "type": "string" } }, "required": ["markdown"] }),
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
        "harness_plan_submit" => {
            output.plan_markdown = invocation
                .arguments
                .get("markdown")
                .and_then(Value::as_str)
                .map(str::to_owned);
            output.structured_plan = output.plan_markdown.is_some();
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
                "harness_plan_submit",
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
    }
}
