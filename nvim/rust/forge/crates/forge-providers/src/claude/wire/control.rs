//! Control requests and responses exchanged in both directions over stream-json.
//!
//! Each side chooses the `request_id` of the requests it sends and answers every request it
//! receives exactly once with the same id. A request this adapter does not implement is answered
//! with an error response, as the CLI expects.

use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};

/// A request the CLI sends to the client.
#[derive(Clone, Debug, PartialEq)]
pub(crate) struct InboundControlRequest {
    pub request_id: String,
    pub request: InboundControl,
}

/// The inbound request kinds this adapter answers; any other subtype receives an error.
#[derive(Clone, Debug, PartialEq)]
pub(crate) enum InboundControl {
    CanUseTool(Box<CanUseTool>),
    HookCallback { callback_id: String },
    McpMessage { server_name: String, message: Value },
    Other { subtype: String },
}

/// A permission request for one tool invocation.
#[derive(Clone, Debug, Deserialize, PartialEq)]
pub(crate) struct CanUseTool {
    pub tool_name: String,
    #[serde(default)]
    pub input: Value,
    pub tool_use_id: String,
    #[serde(default)]
    pub title: Option<String>,
    #[serde(default)]
    pub display_name: Option<String>,
    #[serde(default)]
    pub description: Option<String>,
    #[serde(default)]
    pub blocked_path: Option<String>,
    #[serde(default)]
    pub decision_reason: Option<String>,
    #[serde(default)]
    pub mcp_server: Option<McpServerOrigin>,
    #[serde(default)]
    pub requires_user_interaction: Option<bool>,
    #[serde(default)]
    pub agent_id: Option<String>,
    #[serde(flatten)]
    pub extra: Map<String, Value>,
}

/// The MCP server that owns a requested MCP tool.
#[derive(Clone, Debug, Deserialize, PartialEq)]
pub(crate) struct McpServerOrigin {
    pub name: String,
}

impl InboundControlRequest {
    pub(crate) fn from_value(value: Value) -> Result<Self, serde_json::Error> {
        use serde::de::Error;
        let request_id = value
            .get("request_id")
            .and_then(Value::as_str)
            .ok_or_else(|| serde_json::Error::custom("control request has no id"))?
            .to_owned();
        let request = value
            .get("request")
            .cloned()
            .ok_or_else(|| serde_json::Error::custom("control request has no body"))?;
        let subtype = request
            .get("subtype")
            .and_then(Value::as_str)
            .unwrap_or_default()
            .to_owned();
        let request = match subtype.as_str() {
            "can_use_tool" => {
                InboundControl::CanUseTool(Box::new(serde_json::from_value(request)?))
            }
            "hook_callback" => InboundControl::HookCallback {
                callback_id: request
                    .get("callback_id")
                    .and_then(Value::as_str)
                    .unwrap_or_default()
                    .to_owned(),
            },
            "mcp_message" => InboundControl::McpMessage {
                server_name: request
                    .get("server_name")
                    .and_then(Value::as_str)
                    .ok_or_else(|| serde_json::Error::custom("mcp_message has no server"))?
                    .to_owned(),
                message: request.get("message").cloned().unwrap_or(Value::Null),
            },
            _ => InboundControl::Other { subtype },
        };
        Ok(Self {
            request_id,
            request,
        })
    }
}

/// A response to a control request, in either direction.
#[derive(Clone, Debug, PartialEq)]
pub(crate) struct ControlResponse {
    pub request_id: String,
    pub outcome: Result<Value, ControlError>,
}

/// An error a control request ended with.
#[derive(Clone, Debug, PartialEq)]
pub(crate) struct ControlError {
    pub message: String,
    pub code: Option<String>,
}

impl ControlResponse {
    pub(crate) fn from_value(value: Value) -> Result<Self, serde_json::Error> {
        use serde::de::Error;
        let response = value
            .get("response")
            .ok_or_else(|| serde_json::Error::custom("control response has no body"))?;
        let request_id = response
            .get("request_id")
            .and_then(Value::as_str)
            .ok_or_else(|| serde_json::Error::custom("control response has no id"))?
            .to_owned();
        let outcome = match response.get("subtype").and_then(Value::as_str) {
            Some("success") => Ok(response
                .get("response")
                .cloned()
                .unwrap_or_else(|| Value::Object(Map::new()))),
            Some("error") => Err(ControlError {
                message: response
                    .get("error")
                    .and_then(Value::as_str)
                    .unwrap_or_default()
                    .to_owned(),
                code: response
                    .get("error_code")
                    .and_then(Value::as_str)
                    .map(str::to_owned),
            }),
            _ => return Err(serde_json::Error::custom("control response has no outcome")),
        };
        Ok(Self {
            request_id,
            outcome,
        })
    }
}

/// A request the client sends to the CLI.
#[derive(Clone, Debug, PartialEq, Serialize)]
#[serde(tag = "subtype", rename_all = "snake_case")]
pub(crate) enum OutboundControl {
    Initialize(Initialize),
    Interrupt {
        #[serde(skip_serializing_if = "Option::is_none")]
        cancel_queued: Option<bool>,
    },
    SetPermissionMode {
        mode: String,
    },
    SetModel {
        model: Option<String>,
    },
    ApplyFlagSettings {
        settings: Value,
    },
    McpMessage {
        server_name: String,
        message: Value,
    },
    GetContextUsage {},
}

impl OutboundControl {
    /// Name the request kind for diagnostics without exposing its fields.
    pub(crate) fn subtype(&self) -> &'static str {
        match self {
            Self::Initialize(_) => "initialize",
            Self::Interrupt { .. } => "interrupt",
            Self::SetPermissionMode { .. } => "set_permission_mode",
            Self::SetModel { .. } => "set_model",
            Self::ApplyFlagSettings { .. } => "apply_flag_settings",
            Self::McpMessage { .. } => "mcp_message",
            Self::GetContextUsage {} => "get_context_usage",
        }
    }
}

/// The first request of every session, which configures the CLI for this client.
#[derive(Clone, Debug, Default, PartialEq, Serialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct Initialize {
    /// Hook callbacks registered by id; this adapter registers none.
    pub hooks: Option<Value>,
    /// Names of in-process MCP servers the client serves over `mcp_message`.
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub sdk_mcp_servers: Vec<String>,
    /// System prompt text appended to the CLI's own prompt.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub append_system_prompt: Option<String>,
    /// `false` makes a resumed session use the current system prompt instead of the one it was
    /// created with, so updated host instructions apply after resume.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub system_prompt_snapshot: Option<bool>,
    /// Built-in tool names mapped to replacement tool names.
    #[serde(skip_serializing_if = "Map::is_empty")]
    pub tool_aliases: Map<String, Value>,
}

/// Initialize response fields this adapter reads.
#[derive(Clone, Debug, Default, Deserialize, PartialEq)]
pub(crate) struct InitializeResponse {
    #[serde(default)]
    pub models: Vec<ModelInfo>,
    #[serde(default)]
    pub commands: Vec<Value>,
    #[serde(default)]
    pub pid: Option<u64>,
    #[serde(default)]
    pub capabilities: Vec<String>,
    #[serde(default)]
    pub claude_code_version: Option<String>,
}

/// One model the CLI can run.
#[derive(Clone, Debug, Deserialize, PartialEq)]
#[serde(rename_all = "camelCase")]
pub(crate) struct ModelInfo {
    pub value: String,
    #[serde(default)]
    pub resolved_model: Option<String>,
    #[serde(default)]
    pub display_name: Option<String>,
    #[serde(default)]
    pub description: Option<String>,
    #[serde(default)]
    pub supports_effort: Option<bool>,
    #[serde(default)]
    pub supported_effort_levels: Vec<String>,
    #[serde(default)]
    pub supports_adaptive_thinking: Option<bool>,
}

/// Encode a request the client sends.
pub(crate) fn request_frame(request_id: &str, request: &OutboundControl) -> Value {
    serde_json::json!({
        "type": "control_request",
        "request_id": request_id,
        "request": request,
    })
}

/// Encode the client's answer to a CLI request.
pub(crate) fn response_frame(request_id: &str, outcome: Result<Value, String>) -> Value {
    match outcome {
        Ok(response) => serde_json::json!({
            "type": "control_response",
            "response": {"subtype": "success", "request_id": request_id, "response": response},
        }),
        Err(error) => serde_json::json!({
            "type": "control_response",
            "response": {"subtype": "error", "request_id": request_id, "error": error},
        }),
    }
}

/// The permission decision returned for `can_use_tool`.
#[derive(Clone, Debug, PartialEq, Serialize)]
#[serde(tag = "behavior", rename_all = "snake_case")]
pub(crate) enum PermissionDecision {
    Allow {
        #[serde(rename = "updatedInput")]
        updated_input: Value,
        #[serde(rename = "toolUseID")]
        tool_use_id: String,
    },
    Deny {
        message: String,
        #[serde(skip_serializing_if = "std::ops::Not::not")]
        interrupt: bool,
        #[serde(rename = "toolUseID")]
        tool_use_id: String,
    },
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn outbound_requests_use_the_cli_field_names() {
        let frame = request_frame(
            "req-1",
            &OutboundControl::Initialize(Initialize {
                append_system_prompt: Some("rules".into()),
                system_prompt_snapshot: Some(false),
                ..Default::default()
            }),
        );
        assert_eq!(
            frame,
            json!({"type":"control_request","request_id":"req-1","request":{
                "subtype":"initialize","hooks":null,"appendSystemPrompt":"rules",
                "systemPromptSnapshot":false}})
        );
        assert_eq!(
            serde_json::to_value(OutboundControl::Interrupt {
                cancel_queued: Some(true)
            })
            .unwrap(),
            json!({"subtype":"interrupt","cancel_queued":true})
        );
        assert_eq!(
            serde_json::to_value(PermissionDecision::Allow {
                updated_input: json!({"a":1}),
                tool_use_id: "toolu".into()
            })
            .unwrap(),
            json!({"behavior":"allow","updatedInput":{"a":1},"toolUseID":"toolu"})
        );
    }

    #[test]
    fn responses_decode_success_and_error_outcomes() {
        let success = ControlResponse::from_value(json!({"type":"control_response","response":{
            "subtype":"success","request_id":"one","response":{"still_queued":[]}}}))
        .unwrap();
        assert_eq!(success.outcome, Ok(json!({"still_queued":[]})));
        let failure = ControlResponse::from_value(json!({"type":"control_response","response":{
            "subtype":"error","request_id":"two","error":"nope","error_code":"not_found"}}))
        .unwrap();
        assert_eq!(
            failure.outcome.unwrap_err().code.as_deref(),
            Some("not_found")
        );
    }
}
