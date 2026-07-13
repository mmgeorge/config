use anyhow::{Context, Result};
use serde_json::{Value, json};
use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader};

/// Build the shared structured-input contract for planning feedback.
pub fn plan_question_input_schema() -> Value {
    json!({
        "type": "object",
        "properties": {
            "questions": {
                "type": "array",
                "minItems": 1,
                "maxItems": 3,
                "items": {
                    "type": "object",
                    "properties": {
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

/// Run the Harness control-tool MCP server over JSONL stdio.
pub async fn run_stdio() -> Result<()> {
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
            "tools/list" => json!({ "tools": tool_list() }),
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

fn tool_list() -> Vec<Value> {
    vec![
        json!({
            "name": "harness_plan_submit",
            "description": "Submit the complete Markdown plan for mandatory user review.",
            "inputSchema": { "type": "object", "properties": { "markdown": { "type": "string" } }, "required": ["markdown"] }
        }),
        json!({
            "name": "harness_plan_question",
            "description": "Pause any Harness turn and present one to three interactive user questions. Use this for explicit requests for multiple-choice questions as well as planning decisions.",
            "inputSchema": plan_question_input_schema()
        }),
        json!({
            "name": "harness_goal_complete",
            "description": "Mark the active Harness goal complete only after every required task finishes.",
            "inputSchema": { "type": "object", "properties": { "summary": { "type": "string" } }, "required": ["summary"] }
        }),
        json!({
            "name": "harness_goal_blocked",
            "description": "Mark the active Harness goal blocked with concrete evidence.",
            "inputSchema": { "type": "object", "properties": { "reason": { "type": "string" } }, "required": ["reason"] }
        }),
        json!({
            "name": "harness_goal_status",
            "description": "Report nonterminal progress toward the active Harness goal.",
            "inputSchema": { "type": "object", "properties": { "status": { "type": "string" } }, "required": ["status"] }
        }),
    ]
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn exposes_question_plan_and_goal_control_tools() {
        let tool = tool_list();
        let name: Vec<_> = tool
            .iter()
            .filter_map(|tool| tool.get("name").and_then(Value::as_str))
            .collect();
        assert_eq!(
            name,
            [
                "harness_plan_submit",
                "harness_plan_question",
                "harness_goal_complete",
                "harness_goal_blocked",
                "harness_goal_status"
            ]
        );
    }
}
