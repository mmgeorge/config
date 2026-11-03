//! Provider-neutral controls for deterministic test providers.

use async_trait::async_trait;
use serde::{Deserialize, Serialize};

use crate::{ExecutionOwner, MessageRole, PromptMode, ProviderError, error::ProviderFailureKind};

/// Identify one response barrier without exposing the mock provider's queue state.
#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
#[serde(try_from = "String", into = "String")]
pub struct TestHoldId(String);

impl TestHoldId {
    /// Create a nonempty response barrier identity.
    pub fn new(value: impl Into<String>) -> Result<Self, TestHoldIdError> {
        let value = value.into();
        if value.trim().is_empty() {
            return Err(TestHoldIdError);
        }
        Ok(Self(value))
    }

    /// Borrow the opaque response barrier identity.
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl TryFrom<String> for TestHoldId {
    type Error = TestHoldIdError;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        Self::new(value)
    }
}

impl From<TestHoldId> for String {
    fn from(value: TestHoldId) -> Self {
        value.0
    }
}

/// Reject an empty test barrier identity.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct TestHoldIdError;

impl std::fmt::Display for TestHoldIdError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str("test response hold identity is empty")
    }
}

impl std::error::Error for TestHoldIdError {}

/// Control one deterministic response barrier by normalized owner and prompt mode.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
#[serde(tag = "operation", rename_all = "snake_case", deny_unknown_fields)]
pub enum TestControlCommand {
    /// Hold the next matching prompt, optionally across owners of the same test provider.
    Hold {
        mode: Option<PromptMode>,
        #[serde(default)]
        any_owner: bool,
    },
    /// Wait until an armed hold owns a matching normalized prompt.
    Wait { id: TestHoldId },
    /// Release a response after it has reached its hold.
    Release { id: TestHoldId },
    /// Cancel an armed or arrived response hold.
    Cancel { id: TestHoldId },
    /// Queue one provider-neutral normalized prompt sequence for the next owner prompt.
    Script { script: TestPromptScript },
}

/// Describe one normalized message or host-tool interaction in a test response.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case", deny_unknown_fields)]
pub enum TestPromptStep {
    Message {
        role: MessageRole,
        text: String,
    },
    ToolCall {
        name: String,
        arguments: serde_json::Value,
    },
}

/// Select a typed terminal outcome for one scripted prompt.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum TestPromptTerminal {
    Completed,
    Cancelled,
    Failed(ProviderFailureKind),
    ObservationLost(crate::lifecycle::ObservationFailure),
    RejectedBeforeDispatch(ProviderFailureKind),
    DeliveryUncertain,
}

/// Queue normalized events and a terminal outcome for one future prompt.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct TestPromptScript {
    pub steps: Vec<TestPromptStep>,
    pub terminal: TestPromptTerminal,
}

/// Report a matching prompt that reached a test response barrier.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct PendingTestResponse {
    /// Identify the hold that owns this response.
    pub id: TestHoldId,
    /// Identify the normalized conversation that reached the barrier.
    pub owner: ExecutionOwner,
    /// Preserve the prompt's normalized execution mode.
    pub mode: PromptMode,
    /// Preserve the normalized prompt text for test assertions.
    pub prompt: String,
}

/// Return one provider-neutral test-control result.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "state", rename_all = "snake_case")]
pub enum TestControlResult {
    Held { id: TestHoldId },
    Pending { response: PendingTestResponse },
    Released { id: TestHoldId },
    Cancelled { id: TestHoldId },
    Scripted,
}

/// Control deterministic normalized test flows without exposing provider protocol details.
#[async_trait]
pub trait ProviderTestControl: Send + Sync {
    /// Arm, observe, release, or cancel one barrier owned by the selected test session.
    async fn dispatch(
        &self,
        owner: &ExecutionOwner,
        command: TestControlCommand,
    ) -> Result<TestControlResult, ProviderError>;

    /// Hold this normalized prompt when a matching barrier is armed.
    async fn arrive(
        &self,
        owner: &ExecutionOwner,
        mode: PromptMode,
        prompt: String,
    ) -> Result<(), ProviderError>;

    /// Consume the next script queued for this owner, if any.
    fn take_script(&self, owner: &ExecutionOwner) -> Option<TestPromptScript>;

    /// Cancel barriers for one owner, or all owners during provider shutdown.
    fn cancel_owner(&self, owner: Option<&ExecutionOwner>);
}
