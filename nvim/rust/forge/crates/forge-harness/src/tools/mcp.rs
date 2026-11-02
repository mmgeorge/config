use super::ControlToolDefinition;
use serde::Serialize;
use serde_json::Value;

/// Serialize exactly one JSON-RPC result or error while retaining the caller's ID.
#[derive(Serialize)]
pub(crate) struct Response<'a, T> {
    jsonrpc: &'static str,
    id: &'a Value,
    #[serde(flatten)]
    outcome: Outcome<T>,
}

impl<'a, T> Response<'a, T> {
    /// Create a successful response without converting its payload into JSON first.
    pub(crate) fn success(id: &'a Value, result: T) -> Self {
        Self {
            jsonrpc: "2.0",
            id,
            outcome: Outcome::Result(result),
        }
    }
}

impl<'a> Response<'a, ()> {
    /// Create a protocol failure with the existing MCP error code and message.
    pub(crate) fn failure(id: &'a Value, code: i32, message: String) -> Self {
        Self {
            jsonrpc: "2.0",
            id,
            outcome: Outcome::Error(ProtocolError { code, message }),
        }
    }
}

#[derive(Serialize)]
#[serde(rename_all = "lowercase")]
enum Outcome<T> {
    Result(T),
    Error(ProtocolError),
}

#[derive(Serialize)]
struct ProtocolError {
    code: i32,
    message: String,
}

/// Advertise the existing MCP protocol and server identity.
#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct Initialization {
    protocol_version: &'static str,
    capabilities: Capabilities,
    server_info: ServerInfo,
}

impl Initialization {
    /// Create the transport's existing initialization payload.
    pub(crate) fn new(version: &'static str) -> Self {
        Self {
            protocol_version: "2025-03-26",
            capabilities: Capabilities { tools: Empty {} },
            server_info: ServerInfo {
                name: "forge-harness-control",
                version,
            },
        }
    }
}

#[derive(Serialize)]
struct Capabilities {
    tools: Empty,
}

#[derive(Serialize)]
struct ServerInfo {
    name: &'static str,
    version: &'static str,
}

/// Encode an empty MCP result as an object rather than null.
#[derive(Serialize)]
pub(crate) struct Empty {}

/// Borrow the canonical definitions until the transport serializes the response.
#[derive(Serialize)]
pub(crate) struct Tools<'a> {
    pub(crate) tools: &'a [ControlToolDefinition],
}

/// Preserve MCP's required text content and optional structured payload.
#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct CallResult<T = ()> {
    content: [Content; 1],
    #[serde(skip_serializing_if = "Option::is_none")]
    structured_content: Option<T>,
    #[serde(skip_serializing_if = "Option::is_none")]
    is_error: Option<bool>,
}

impl<T: Serialize> CallResult<T> {
    /// Encode a structured result once as the text content required by MCP clients.
    pub(crate) fn structured(payload: T) -> serde_json::Result<Self> {
        let text = crate::codec::to_string(&payload)?;
        Ok(Self {
            content: [Content::Text { text }],
            structured_content: Some(payload),
            is_error: None,
        })
    }
}

impl CallResult {
    /// Create the existing text-only success shape.
    pub(crate) fn text(text: String) -> Self {
        Self {
            content: [Content::Text { text }],
            structured_content: None,
            is_error: None,
        }
    }

    /// Mark a tool failure without turning it into a JSON-RPC protocol error.
    pub(crate) fn failure(text: String) -> Self {
        Self {
            content: [Content::Text { text }],
            structured_content: None,
            is_error: Some(true),
        }
    }
}

#[derive(Serialize)]
#[serde(tag = "type", rename_all = "lowercase")]
enum Content {
    Text { text: String },
}
