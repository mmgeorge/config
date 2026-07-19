use serde::{Deserialize, Serialize};
use serde_json::Value;

/// Represents one request sent from the Neovim client to the broker.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct BrokerRequest {
    pub id: u64,
    pub method: String,
    #[serde(default)]
    pub params: Value,
}

/// Represents one response sent from the broker to the requesting client.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct BrokerResponse {
    pub id: u64,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub result: Option<Value>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error: Option<ProtocolError>,
}

/// Represents a structured broker failure without collapsing it into an empty result.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ProtocolError {
    pub code: String,
    pub message: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub data: Option<Value>,
}

/// Represents an asynchronous state change streamed from the broker.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct BrokerEvent {
    pub session_id: String,
    pub event: String,
    pub payload: Value,
}

/// Represents either a correlated response or an asynchronous event on JSONL stdout.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(untagged)]
pub enum BrokerMessage {
    Response(BrokerResponse),
    Event(BrokerEvent),
}

impl BrokerResponse {
    /// Build a successful response from a serializable result.
    pub fn success<T: Serialize>(id: u64, result: T) -> anyhow::Result<Self> {
        Ok(Self {
            id,
            result: Some(serde_json::to_value(result)?),
            error: None,
        })
    }

    /// Build a failed response from a stable code and user-visible message.
    pub fn failure(id: u64, code: impl Into<String>, message: impl Into<String>) -> Self {
        Self {
            id,
            result: None,
            error: Some(ProtocolError {
                code: code.into(),
                message: message.into(),
                data: None,
            }),
        }
    }

    /// Build a failed response with structured recovery metadata.
    pub fn failure_with_data(
        id: u64,
        code: impl Into<String>,
        message: impl Into<String>,
        data: Value,
    ) -> Self {
        Self {
            id,
            result: None,
            error: Some(ProtocolError {
                code: code.into(),
                message: message.into(),
                data: Some(data),
            }),
        }
    }
}
