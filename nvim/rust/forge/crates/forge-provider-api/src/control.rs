use crate::ProviderError;
use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use serde_json::Value;

/// Describe one host tool independently of its native provider transport.
#[derive(Clone, Debug, Serialize)]
pub struct ControlToolDefinition {
    /// Stable tool name advertised to the model.
    pub name: &'static str,
    /// Instructions for selecting and invoking the tool.
    pub description: &'static str,
    /// JSON Schema for the tool's arguments.
    #[serde(rename = "inputSchema")]
    pub input_schema: Value,
}

impl ControlToolDefinition {
    /// Encode the standard MCP tool-list entry.
    pub fn mcp_value(&self) -> Value {
        serde_json::to_value(self).expect("tool definitions must serialize as JSON")
    }
}

/// Identify one provider-authenticated tool request within its invocation scope.
#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
#[serde(try_from = "String", into = "String")]
pub struct ToolCallId(String);

impl ToolCallId {
    /// Create a tool-call identity from the adapter's authenticated request envelope.
    pub fn new(value: impl Into<String>) -> Result<Self, InvalidToolCallId> {
        let value = value.into();
        if value.trim().is_empty() {
            return Err(InvalidToolCallId);
        }
        Ok(Self(value))
    }

    /// Get the opaque normalized identity for correlation and retained replay.
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

/// Reject absent tool-call identity before host effects can be admitted.
#[derive(Clone, Copy, Debug, thiserror::Error)]
#[error("tool call identity must not be empty")]
pub struct InvalidToolCallId;

impl TryFrom<String> for ToolCallId {
    type Error = InvalidToolCallId;
    /// Validate a serialized identity through the same adapter boundary.
    fn try_from(value: String) -> Result<Self, Self::Error> {
        Self::new(value)
    }
}

impl From<ToolCallId> for String {
    /// Preserve the exact normalized spelling in retained records.
    fn from(value: ToolCallId) -> Self {
        value.0
    }
}

/// Carry a model tool call without host task or plan authority.
#[derive(Clone, Debug)]
pub struct ToolCall {
    /// Identity supplied by the adapter, never extracted from model arguments.
    pub id: ToolCallId,
    /// Name of the advertised host tool.
    pub name: String,
    /// Arguments supplied by the model, validated by the tool owner.
    pub arguments: Value,
}

/// Execute host tools while their authority and mutable state remain with the host.
#[async_trait]
pub trait ProviderTools: Send + Sync {
    /// Admit and execute a model call, then return its model-visible response.
    async fn call(
        &self,
        scope: &crate::InvocationScope,
        call: ToolCall,
    ) -> Result<String, ProviderError>;
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Decoding durable call identity must enforce the same rule as native adapters.
    #[test]
    fn tool_call_identity_rejects_empty_serialized_values() {
        assert!(ToolCallId::new(" ").is_err());
        assert!(serde_json::from_value::<ToolCallId>(serde_json::json!("")).is_err());
        let id: ToolCallId = serde_json::from_value(serde_json::json!("native-call-7")).unwrap();
        assert_eq!(id.as_str(), "native-call-7");
    }
}
