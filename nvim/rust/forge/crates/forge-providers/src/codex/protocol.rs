use super::CodexConnection;
use anyhow::Result;
use forge_provider_api::trace::ProviderTrace;
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

/// Identify client responses without confusing independent server request IDs with acknowledgments.
pub fn response_id(message: &Value) -> Option<u64> {
    if message.get("method").is_some() {
        return None;
    }
    message.get("id").and_then(Value::as_u64)
}

/// Resolve one matching JSON-RPC response without consuming unrelated messages.
pub fn request_result(message: &Value, id: u64, method: &str) -> Option<Result<Value>> {
    if response_id(message) != Some(id) {
        return None;
    }
    if message.get("result").is_some() == message.get("error").is_some() {
        return Some(Err(forge_provider_api::ProviderError::Protocol {
            message: format!(
                "provider {method} response {id} must contain exactly one of result or error"
            ),
        }
        .into()));
    }
    if let Some(error) = message.get("error") {
        let missing_session = method == "thread/resume"
            && error.get("code").and_then(Value::as_i64) == Some(-32600)
            && error
                .get("message")
                .and_then(Value::as_str)
                .is_some_and(|message| {
                    message.starts_with("no rollout found for thread id ")
                        || message.starts_with("thread not found: ")
                });
        if missing_session {
            return Some(Err(forge_provider_api::ProviderError::SessionMissing {
                message: format!("provider {method} failed: {error}"),
            }
            .into()));
        }
        return Some(Err(forge_provider_api::ProviderError::Provider {
            code: error.get("code").map(Value::to_string),
            message: format!("provider {method} failed: {error}"),
        }
        .into()));
    }
    Some(Ok(message["result"].clone()))
}

/// Receive and trace one native frame before it reaches host callbacks.
pub async fn receive(
    socket: &mut CodexConnection,
    trace: &dyn ProviderTrace,
    session_id: &str,
) -> Result<Value> {
    let received = socket
        .receive()
        .await
        .map_err(forge_provider_api::ProviderError::from)?;
    trace.record(
        session_id,
        "model.received_raw",
        json!({ "bytes": received.encoded_bytes }),
    );
    let message: Value =
        received
            .value
            .map_err(|error| forge_provider_api::ProviderError::Protocol {
                message: format!("decode provider JSON-RPC message: {error}"),
            })?;
    trace.record(session_id, "model.received", message.clone());
    Ok(message)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn response_identity_distinguishes_server_requests_and_validates_response_envelopes() {
        use serde_json::json;
        let server_request = json!({"id":17,"method":"item/tool/call","params":{}});
        assert_eq!(response_id(&server_request), None);
        assert!(request_result(&server_request, 17, "turn/steer").is_none());
        assert_eq!(
            request_result(&json!({"id":17,"result":null}), 17, "turn/steer")
                .unwrap()
                .unwrap(),
            serde_json::Value::Null
        );
        assert!(
            request_result(
                &json!({"id":17,"error":{"code":-32600,"message":"denied"}}),
                17,
                "turn/steer"
            )
            .unwrap()
            .is_err()
        );
        for malformed in [
            json!({"id":17}),
            json!({"id":17,"result":null,"error":{"code":-32600,"message":"denied"}}),
        ] {
            let error = request_result(&malformed, 17, "turn/steer")
                .unwrap()
                .unwrap_err();
            assert!(matches!(
                error.downcast_ref::<forge_provider_api::ProviderError>(),
                Some(forge_provider_api::ProviderError::Protocol { .. })
            ));
            assert!(error.to_string().contains("turn/steer response 17"));
        }
    }

    #[test]
    fn classifies_only_missing_resume_history() {
        for (method, code, message, missing) in [
            (
                "thread/resume",
                -32600,
                "no rollout found for thread id missing",
                true,
            ),
            ("thread/resume", -32600, "thread not found: missing", true),
            ("thread/resume", -32600, "permission denied", false),
            (
                "thread/resume",
                -32603,
                "failed to read thread: file is empty",
                false,
            ),
            (
                "thread/goal/clear",
                -32600,
                "thread not found: missing",
                false,
            ),
        ] {
            let response = serde_json::json!({"id":17,"error":{"code":code,"message":message}});
            let error = request_result(&response, 17, method).unwrap().unwrap_err();
            assert_eq!(
                matches!(
                    error.downcast_ref::<forge_provider_api::ProviderError>(),
                    Some(forge_provider_api::ProviderError::SessionMissing { .. })
                ),
                missing
            );
            assert!(request_result(&response, 18, method).is_none());
        }
    }
}
