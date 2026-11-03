use serde_json::{Value, json};
use std::collections::HashMap;

/// Correlate native MCP calls with the host's stable resource names.
#[derive(Default)]
pub struct McpContext {
    binding: HashMap<String, String>,
    call: HashMap<String, Value>,
}

impl McpContext {
    /// Replace resource-name bindings for the next native request.
    pub fn set_bindings(&mut self, binding: HashMap<String, String>) {
        self.binding = binding;
    }

    /// Forget turn-local calls without changing resource-name bindings.
    pub fn clear_calls(&mut self) {
        self.call.clear();
    }

    /// Normalize MCP names and attach the uniquely matching approval target.
    pub fn normalize(&mut self, message: &mut Value) {
        let method = message
            .get("method")
            .and_then(Value::as_str)
            .unwrap_or_default()
            .to_owned();
        let Some(params) = message.get_mut("params") else {
            return;
        };
        if params.pointer("/item/type").and_then(Value::as_str) == Some("mcpToolCall") {
            if let Some(server) = params.pointer_mut("/item/server")
                && let Some(name) = server.as_str().and_then(|name| self.binding.get(name))
            {
                *server = json!(name);
            }
            if let Some(id) = params.pointer("/item/id").and_then(Value::as_str) {
                if method == "item/started" {
                    self.call.insert(
                        id.to_owned(),
                        json!({
                            "thread":params["threadId"], "turn":params["turnId"],
                            "server":params["item"]["server"], "tool":params["item"]["tool"]
                        }),
                    );
                } else if method == "item/completed" {
                    self.call.remove(id);
                }
            }
        }
        if method != "mcpServer/elicitation/request" {
            return;
        }
        if let Some(server) = params.get_mut("serverName")
            && let Some(name) = server.as_str().and_then(|name| self.binding.get(name))
        {
            *server = json!(name);
        }
        if let Some(meta) = params.get("_meta").cloned() {
            params["meta"] = meta;
        }
        if params
            .pointer("/meta/codex_approval_kind")
            .and_then(Value::as_str)
            != Some("mcp_tool_call")
        {
            return;
        }
        let matching: Vec<_> = self
            .call
            .values()
            .filter(|call| {
                call["thread"] == params["threadId"]
                    && call["turn"] == params["turnId"]
                    && call["server"] == params["serverName"]
            })
            .collect();
        if matching.len() == 1
            && params.pointer("/meta/toolName").is_none()
            && params.pointer("/meta/tool_name").is_none()
        {
            params["meta"]["toolName"] = matching[0]["tool"].clone();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Publish canonical resource names and correlate only one matching active call.
    #[test]
    fn mcp_aliases_and_approval_targets_are_normalized_inside_the_adapter() {
        let mut context = McpContext::default();
        context.set_bindings(HashMap::from([(
            "private-alias".into(),
            "workspace tools".into(),
        )]));
        let mut started = json!({"method":"item/started","params":{
            "threadId":"thread","turnId":"turn",
            "item":{"id":"call","type":"mcpToolCall","server":"private-alias","tool":"query"}
        }});
        context.normalize(&mut started);
        assert_eq!(started["params"]["item"]["server"], "workspace tools");
        let mut approval = json!({"method":"mcpServer/elicitation/request","params":{
            "threadId":"thread","turnId":"turn","serverName":"private-alias",
            "_meta":{"codex_approval_kind":"mcp_tool_call"}
        }});
        context.normalize(&mut approval);
        assert_eq!(approval["params"]["serverName"], "workspace tools");
        assert_eq!(approval["params"]["meta"]["toolName"], "query");
        context.clear_calls();
        approval["params"].as_object_mut().unwrap().remove("meta");
        context.normalize(&mut approval);
        assert!(approval.pointer("/params/meta/toolName").is_none());
    }
}
