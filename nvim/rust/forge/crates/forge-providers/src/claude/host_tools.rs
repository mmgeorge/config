//! In-process MCP server that serves Harness control tools over `mcp_message`.
//!
//! The CLI treats `forge-harness-control` as an SDK server: it sends MCP JSON-RPC messages as
//! control requests and expects each JSON-RPC response wrapped in `{"mcp_response": ...}`. The
//! tool list is the current invocation's list, so a call to a tool the invocation did not
//! authorize is answered with a tool error rather than reaching Harness.

use forge_provider_api::control::ControlToolDefinition;
use serde_json::{Value, json};

/// MCP protocol revision this server answers with.
const PROTOCOL_VERSION: &str = "2025-06-18";

/// One decoded MCP message sent to the host tool server.
pub(crate) enum HostToolMessage {
    /// A message answered without host involvement.
    Immediate(Value),
    /// A tool call that Harness executes.
    Call {
        id: Value,
        name: String,
        arguments: Value,
        tool_use_id: Option<String>,
    },
}

/// Decode one MCP message against the tools the current invocation authorizes.
pub(crate) fn handle(message: &Value, tools: &[ControlToolDefinition]) -> HostToolMessage {
    let id = message.get("id").cloned();
    let method = message
        .get("method")
        .and_then(Value::as_str)
        .unwrap_or_default();
    let respond = |result: Value| {
        HostToolMessage::Immediate(json!({
            "mcp_response": {"jsonrpc": "2.0", "id": id.clone().unwrap_or(json!(0)), "result": result}
        }))
    };
    match method {
        "initialize" => respond(json!({
            "protocolVersion": PROTOCOL_VERSION,
            "capabilities": {"tools": {"listChanged": true}},
            "serverInfo": {"name": super::tool_targets::HOST_TOOL_SERVER, "version": env!("CARGO_PKG_VERSION")},
        })),
        "tools/list" => respond(json!({
            "tools": tools.iter().map(ControlToolDefinition::mcp_value).collect::<Vec<_>>(),
        })),
        "tools/call" => {
            let params = &message["params"];
            HostToolMessage::Call {
                id: id.unwrap_or(json!(0)),
                name: params["name"].as_str().unwrap_or_default().to_owned(),
                arguments: params
                    .get("arguments")
                    .cloned()
                    .unwrap_or_else(|| json!({})),
                tool_use_id: params
                    .pointer("/_meta/claudecode~1toolUseId")
                    .and_then(Value::as_str)
                    .map(str::to_owned),
            }
        }
        "ping" => respond(json!({})),
        // Notifications have no id and receive the acknowledgement the CLI expects.
        _ if id.is_none() => respond(json!({})),
        _ => HostToolMessage::Immediate(json!({
            "mcp_response": {"jsonrpc": "2.0", "id": id, "error": {"code": -32601, "message": "Method not found"}}
        })),
    }
}

/// Encode a completed tool call.
pub(crate) fn call_result(id: Value, outcome: Result<String, String>) -> Value {
    let (text, is_error) = match outcome {
        Ok(text) => (text, false),
        Err(text) => (text, true),
    };
    json!({
        "mcp_response": {"jsonrpc": "2.0", "id": id, "result": {
            "content": [{"type": "text", "text": text}],
            "isError": is_error,
        }}
    })
}

/// Build the notification that makes the CLI list the tools again.
pub(crate) fn list_changed() -> Value {
    json!({"jsonrpc": "2.0", "method": "notifications/tools/list_changed"})
}

#[cfg(test)]
mod tests {
    use super::*;

    fn tool() -> ControlToolDefinition {
        ControlToolDefinition {
            name: "plan_edit",
            description: "Edit the plan",
            input_schema: json!({"type":"object"}),
        }
    }

    #[test]
    fn lists_the_invocation_tools_and_routes_calls_with_their_tool_use_id() {
        let HostToolMessage::Immediate(listed) = handle(
            &json!({"jsonrpc":"2.0","id":1,"method":"tools/list"}),
            &[tool()],
        ) else {
            panic!("listing is immediate");
        };
        assert_eq!(
            listed["mcp_response"]["result"]["tools"][0]["name"],
            "plan_edit"
        );
        let HostToolMessage::Call {
            name, tool_use_id, ..
        } = handle(
            &json!({"jsonrpc":"2.0","id":2,"method":"tools/call","params":{
                "name":"plan_edit","arguments":{},"_meta":{"claudecode/toolUseId":"toolu_1"}}}),
            &[tool()],
        )
        else {
            panic!("calls go to the host");
        };
        assert_eq!(name, "plan_edit");
        assert_eq!(tool_use_id.as_deref(), Some("toolu_1"));
        let HostToolMessage::Immediate(acknowledged) = handle(
            &json!({"jsonrpc":"2.0","method":"notifications/initialized"}),
            &[],
        ) else {
            panic!("notifications are acknowledged");
        };
        assert_eq!(acknowledged["mcp_response"]["result"], json!({}));
    }
}
