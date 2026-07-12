use anyhow::{Context, Result};
use serde_json::{Value, json};
use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader};

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
    fn exposes_plan_and_goal_control_tools() {
        let tool = tool_list();
        let name: Vec<_> = tool
            .iter()
            .filter_map(|tool| tool.get("name").and_then(Value::as_str))
            .collect();
        assert_eq!(
            name,
            [
                "harness_plan_submit",
                "harness_goal_complete",
                "harness_goal_blocked",
                "harness_goal_status"
            ]
        );
    }
}
