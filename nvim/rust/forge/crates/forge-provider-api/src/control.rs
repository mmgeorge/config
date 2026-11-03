use crate::ProviderError;
use async_trait::async_trait;
use serde::Serialize;
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

/// Carry a model tool call without host task or plan authority.
#[derive(Clone, Debug)]
pub struct ToolCall {
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
