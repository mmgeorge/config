use crate::ProviderError;
use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use serde_json::Value;

/// Describe one host tool independently of its native provider transport.
#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
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

/// Classify a completed host tool call for the provider's native result.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ToolOutcome {
    /// The tool did what was asked; its text is the answer, even when the answer is negative.
    Succeeded,
    /// The host refused or failed the request; its text is the reason the model reads.
    Failed,
}

/// Carry a host tool's model-visible text together with its outcome.
///
/// Adapters report `Failed` through the native failure field (Claude `isError`, Codex
/// `success: false`, Copilot `resultType: "failure"`), so the provider and the timeline show a
/// failed call with its reason instead of a success whose text happens to describe an error.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ToolReply {
    /// Text returned to the model.
    pub text: String,
    /// Whether the call succeeded or was refused.
    pub outcome: ToolOutcome,
}

impl ToolReply {
    /// Build a successful reply.
    pub fn succeeded(text: impl Into<String>) -> Self {
        Self {
            text: text.into(),
            outcome: ToolOutcome::Succeeded,
        }
    }

    /// Build a refused or failed reply whose text states the reason.
    pub fn failed(text: impl Into<String>) -> Self {
        Self {
            text: text.into(),
            outcome: ToolOutcome::Failed,
        }
    }

    /// Get whether the host refused or failed the call.
    pub fn is_failure(&self) -> bool {
        self.outcome == ToolOutcome::Failed
    }
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

    /// Admit and execute a model call, returning its text and whether the host refused it.
    ///
    /// Hosts that classify refusals override this; the default reports every answered call as
    /// a success. An `Err` remains a transport or authority failure without model-visible text.
    async fn call_reply(
        &self,
        scope: &crate::InvocationScope,
        call: ToolCall,
    ) -> Result<ToolReply, ProviderError> {
        self.call(scope, call).await.map(ToolReply::succeeded)
    }

    /// Execute for an independently registered native child using its original parent authority.
    /// Hosts must explicitly support this boundary; root execution is never a fallback.
    async fn call_observed_child(
        &self,
        _parent: &crate::InvocationScope,
        _child: &crate::TurnAddress,
        _call: ToolCall,
    ) -> Result<String, ProviderError> {
        Err(crate::ProviderFailure::new(
            crate::ProviderOperation::TurnStart,
            crate::ProviderFailureKind::Authorization,
        )
        .into())
    }

    /// Execute for a native child and report whether the host refused the call.
    async fn call_observed_child_reply(
        &self,
        parent: &crate::InvocationScope,
        child: &crate::TurnAddress,
        call: ToolCall,
    ) -> Result<ToolReply, ProviderError> {
        self.call_observed_child(parent, child, call)
            .await
            .map(ToolReply::succeeded)
    }

    /// Explain to the model why a tool registered for the conversation is unavailable in this
    /// invocation. Adapters with a fixed tool catalog return it when the model calls such a tool.
    fn unavailable_reason(&self, name: &str) -> String {
        format!("`{name}` is not available in this turn.")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Unsupported observed-child execution must not reuse the root tool authority.
    #[tokio::test]
    async fn observed_child_tools_deny_by_default_without_calling_root() {
        struct RootTools;
        #[async_trait]
        impl ProviderTools for RootTools {
            async fn call(
                &self,
                _: &crate::InvocationScope,
                _: ToolCall,
            ) -> Result<String, ProviderError> {
                panic!("observed child execution must never fall back to root execution")
            }
        }
        let parent = crate::InvocationScope::new(crate::testing::root_thread("session"), "parent");
        let child_scope = crate::InvocationScope::new(
            parent
                .thread_id
                .child(&crate::AgentId::new("child").unwrap())
                .unwrap(),
            "child-turn",
        );
        let child = crate::TurnAddress {
            thread_id: child_scope.thread_id.clone(),
            turn_id: child_scope.turn_id().unwrap(),
            scope: child_scope,
        };
        let result = RootTools
            .call_observed_child(
                &parent,
                &child,
                ToolCall {
                    id: ToolCallId::new("child-call").unwrap(),
                    name: "host_tool".into(),
                    arguments: serde_json::json!({}),
                },
            )
            .await;
        assert!(matches!(result, Err(ProviderError::Failure(error))
            if error.kind() == crate::ProviderFailureKind::Authorization));
    }
}
