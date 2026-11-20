//! Provider-neutral controls for deterministic test providers.

use async_trait::async_trait;
use serde::{Deserialize, Serialize};

use crate::{ExecutionMode, ProviderError, ThreadId, error::ProviderFailureKind};

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
        mode: Option<ExecutionMode>,
        #[serde(default)]
        any_owner: bool,
    },
    /// Wait until an armed hold owns a matching normalized prompt.
    Wait { id: TestHoldId },
    /// Release a response after it has reached its hold.
    Release { id: TestHoldId },
    /// Cancel an armed or arrived response hold.
    Cancel { id: TestHoldId },
    /// Cancel every hold the selected owner armed, as closing that owner's session does.
    CancelOwner,
    /// Queue one provider-neutral normalized prompt sequence for the next owner prompt.
    Script { script: TestPromptScript },
    /// Reject workspace model discovery so models are listed only from an open binding, as
    /// ACP agents negotiate their catalogs per session.
    RequireBindingModelCatalog,
    /// Read the turn checkpoints the selected owner's native history retains, oldest first.
    History,
    /// Fail every later close of the selected owner's binding, as a native side that cannot
    /// collect its process does; only forced termination then removes the binding.
    FailClose,
    /// Never settle a later interrupt of the selected owner's turn, as a native side that
    /// stopped answering does; only forced termination then ends the turn.
    HoldInterrupt,
    /// Hold the selected owner's next native conversation rewind, as a slow native rewind does.
    /// The result names the hold, which `Wait` and `Release` then address like a response hold.
    HoldRewind,
    /// Answer the selected owner's next native compaction without compacting, in the given
    /// words, as a native side that finds too little conversation to compact does.
    DeclineCompaction { explanation: String },
}

/// Select native fixture content without treating reasoning as an assistant prose role.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum TestMessageContent {
    /// Emit intermediate assistant progress prose.
    Commentary,
    /// Emit positively classified final-answer prose.
    Response,
    /// Emit a distinct native reasoning representation.
    Reasoning(crate::ReasoningKind),
}

/// Describe one normalized message or host-tool interaction in a test response.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case", deny_unknown_fields)]
pub enum TestPromptStep {
    /// Hold this exact point in a scripted response until the test releases it.
    Barrier { id: TestHoldId },
    /// Hold native resource collection after the terminal event has been delivered.
    CollectionBarrier { id: TestHoldId },
    Message {
        content: TestMessageContent,
        text: String,
    },
    /// Start assistant prose that arrives before its role is known, as Copilot streams a
    /// message until its final snapshot names the phase. `item` names the message for the
    /// steps that continue it.
    UnclassifiedMessageStart { item: String, text: String },
    /// Append streamed text to a message the script started.
    MessageDelta { item: String, text: String },
    /// Classify a streamed message and freeze its accumulated text, as a final native
    /// snapshot does.
    MessageCompletion {
        item: String,
        content: TestMessageContent,
    },
    ToolCall {
        name: String,
        arguments: serde_json::Value,
    },
    /// Emit a native tool lifecycle event independently of host-tool execution.
    ToolEvent {
        /// Identify the tool across announcement, execution, and completion steps.
        id: crate::ItemId,
        /// Carry the normalized lifecycle event to the host.
        event: TestToolEvent,
    },
    /// Exercise ordinary authorized file editing rather than a host plan patch.
    FileWrite { path: String, text: String },
    /// Report a successful native file read without launching a shell.
    FileRead { path: String },
    /// Publish one complete native checklist replacement for the scripted turn.
    Checklist {
        update: crate::ProviderChecklistUpdate,
    },
    /// Run one native subagent the provider delegates to on its own, as Copilot runs a built-in
    /// subagent: the agent's start, one response message in the agent's own turn, then its
    /// completed finish.
    NativeSubagent {
        name: String,
        text: String,
        /// Hold the running agent until the delegating turn is cancelled, then report its
        /// response and completed finish before the turn ends interrupted. Copilot can deliver
        /// a subagent's output after the user stopped its parent in this order, because the
        /// subagent finished before the stop reached Copilot while the host still had its
        /// output queued.
        #[serde(default)]
        finishes_after_cancel: bool,
    },
}

/// Control announcement and execution independently for a scripted native tool.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case", deny_unknown_fields)]
pub enum TestToolEvent {
    /// Announce a tool before execution begins.
    Announced {
        /// Describe the native tool to display.
        descriptor: crate::ToolDescriptor,
    },
    /// Report positive evidence that the announced tool started executing.
    ExecutionStarted,
    /// Settle the announced tool with its actual outcome.
    Completed {
        /// Preserve success, failure, denial, or interruption.
        outcome: crate::ToolOutcome,
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
    /// Select when native acceptance becomes observable, independently of dispatch.
    #[serde(default)]
    pub acceptance: TestInputAcceptance,
    pub steps: Vec<TestPromptStep>,
    pub terminal: TestPromptTerminal,
}

/// Exercise normalized acceptance timing without naming a provider or protocol.
#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum TestInputAcceptance {
    /// Acknowledge before emitting the response, while the invocation remains active.
    #[default]
    BeforeResponse,
    /// Acknowledge only after the response's native terminal evidence is published.
    AfterResponse,
}

/// Report a matching prompt that reached a test response barrier.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct PendingTestResponse {
    /// Identify the hold that owns this response.
    pub id: TestHoldId,
    /// Identify the normalized conversation that reached the barrier.
    pub thread_id: ThreadId,
    /// Preserve the prompt's normalized execution mode.
    pub mode: ExecutionMode,
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
    OwnerCancelled,
    Scripted,
    Configured,
    History { checkpoints: Vec<String> },
}

/// Control deterministic normalized test flows without exposing provider protocol details.
#[async_trait]
pub trait ProviderTestControl: Send + Sync {
    /// Arm, observe, release, or cancel one barrier owned by the selected test session.
    async fn dispatch(
        &self,
        thread_id: &ThreadId,
        command: TestControlCommand,
    ) -> Result<TestControlResult, ProviderError>;

    /// Hold this normalized prompt when a matching barrier is armed.
    async fn arrive(
        &self,
        thread_id: &ThreadId,
        mode: ExecutionMode,
        prompt: String,
    ) -> Result<(), ProviderError>;

    /// Consume the next script queued for this owner, if any.
    fn take_script(&self, thread_id: &ThreadId) -> Option<TestPromptScript>;

    /// Cancel barriers for one owner, or all owners during provider shutdown.
    fn cancel_thread(&self, thread_id: Option<&ThreadId>);
}

/// Model the host-issued root thread of the named test namespace.
///
/// Adapter fixtures stand in for the host, which alone issues root thread identities.
pub fn root_thread(namespace: impl AsRef<str>) -> ThreadId {
    let namespace = namespace.as_ref();
    ThreadId::host(namespace, &format!("{namespace}:agent:primary"))
        .expect("test namespaces are nonempty")
}
