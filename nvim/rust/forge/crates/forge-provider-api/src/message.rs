use serde::{Deserialize, Serialize};

#[derive(Clone, Copy, Debug, Eq, PartialEq, Deserialize, Serialize)]
#[serde(tag = "kind", content = "role", rename_all = "snake_case")]
pub enum MessageClassification {
    Unclassified,
    Known(AgentMessageRole),
}

/// Define the immutable item type and its initial native contents.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "kind", content = "start", rename_all = "snake_case")]
pub enum ItemStart {
    /// Start an observed user message.
    UserMessage(UserMessageStart),
    /// Start assistant commentary or response prose.
    AgentMessage(AgentMessageStart),
    /// Start an explicitly identified reasoning representation.
    Reasoning(ReasoningStart),
    /// Start a requested command operation.
    CommandExecution(crate::tool::ToolDescriptor),
    /// Start a native file-change operation.
    FileChange(crate::tool::ToolDescriptor),
    /// Start another native tool operation.
    ToolCall(crate::tool::ToolDescriptor),
}

/// Describe activity on one admitted item without completing its turn.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "kind", content = "event", rename_all = "snake_case")]
pub enum ItemEvent {
    /// Admit the item's immutable type and initial contents.
    Started(ItemStart),
    /// Apply ordered message content or classification.
    Message { event: MessageItemEvent },
    /// Apply tool metadata, execution, output, or changes.
    Tool { event: crate::tool::ToolItemEvent },
    /// Freeze the matching item type's final snapshot.
    Completed(ItemCompletion),
}

/// Preserve message interpretation and ordered text.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "kind", content = "value", rename_all = "snake_case")]
pub enum MessageItemEvent {
    /// Record a positive prose classification or explicit absence.
    Classified(MessageClassification),
    /// Append text to the existing item.
    Delta(String),
}

/// Classify assistant prose independently of reasoning representation.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum AgentMessageRole {
    /// Preserve intermediate progress text.
    Commentary,
    /// Preserve positively identified answer text.
    Response,
}

/// Preserve native reasoning text separately from reasoning summaries.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum ReasoningKind {
    /// Preserve a native reasoning-content stream.
    Full,
    /// Preserve a native reasoning summary.
    Summary,
}

/// Freeze the same item type that was admitted.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "kind", content = "completion", rename_all = "snake_case")]
pub enum ItemCompletion {
    /// Freeze an observed user message.
    UserMessage(MessageCompletion),
    /// Freeze completed or interrupted assistant prose.
    AgentMessage(MessageCompletion),
    /// Freeze completed or interrupted reasoning.
    Reasoning(MessageCompletion),
    /// Finish a command operation with its actual outcome.
    CommandExecution(crate::tool::ToolCompletion),
    /// Finish a native file-change operation.
    FileChange(crate::tool::ToolCompletion),
    /// Finish another native tool operation.
    ToolCall(crate::tool::ToolCompletion),
}

/// Distinguish authoritative final text from interrupted partial content.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum MessageOutcome {
    /// The provider positively established authoritative final content.
    Completed,
    /// The provider stopped with partial content.
    Interrupted,
}

/// Preserve native user text and an optional matched submission.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct UserMessageStart {
    /// Preserve observed user text.
    pub text: String,
    /// Link submitted attachments through their existing journal entry.
    pub submission_id: Option<crate::SubmissionId>,
}

/// Start assistant prose without guessing a missing native phase.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct AgentMessageStart {
    /// Preserve initial observed text.
    pub text: String,
    /// Preserve the positively observed prose role, if any.
    pub classification: MessageClassification,
}

/// Start a native reasoning representation.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ReasoningStart {
    /// Preserve initial reasoning text.
    pub text: String,
    /// Distinguish full reasoning from its summary.
    pub kind: ReasoningKind,
}

/// Freeze authoritative final text or the content left by interruption.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct MessageCompletion {
    /// Replace previously accumulated text with this final snapshot.
    pub text: String,
    /// Preserve whether the snapshot is authoritative final content.
    pub outcome: MessageOutcome,
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Keep immutable reasoning type and representation through the event transport.
    #[test]
    fn reasoning_start_roundtrips_with_its_explicit_representation() {
        let event = ItemEvent::Started(ItemStart::Reasoning(ReasoningStart {
            text: "summary".into(),
            kind: ReasoningKind::Summary,
        }));
        let encoded = serde_json::to_string(&event).unwrap();
        let decoded: ItemEvent = serde_json::from_str(&encoded).unwrap();
        assert!(matches!(
            decoded,
            ItemEvent::Started(ItemStart::Reasoning(ReasoningStart {
                kind: ReasoningKind::Summary,
                ..
            }))
        ));
    }
}
