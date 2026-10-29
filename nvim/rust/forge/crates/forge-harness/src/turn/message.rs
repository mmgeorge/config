use crate::backend::MessageRole;
use serde::{Deserialize, Serialize};

/// Distinguishes provider reasoning from user-visible assistant text.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum MessageKind {
    Unknown,
    Assistant,
    Reasoning,
    ReasoningSummary,
}

/// Distinguishes progress commentary from the provider's final answer.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum MessageDelivery {
    Unknown,
    Commentary,
    Final,
}

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum MessageState {
    Streaming,
    Completed,
    Interrupted,
}

/// Owns one contiguous provider message independently of presentation folds.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Message {
    pub(crate) id: String,
    pub(crate) provider_id: Option<String>,
    pub(crate) role: Option<MessageRole>,
    pub(crate) state: MessageState,
    pub(crate) text: String,
}

impl Message {
    /// Return the stable local message identity.
    pub fn id(&self) -> &str {
        &self.id
    }

    /// Return the normalized provider message role.
    pub fn kind(&self) -> MessageKind {
        match self.role {
            None => MessageKind::Unknown,
            Some(MessageRole::ThoughtSummary) => MessageKind::ReasoningSummary,
            Some(MessageRole::Thought) => MessageKind::Reasoning,
            Some(MessageRole::Commentary | MessageRole::Response) => MessageKind::Assistant,
        }
    }

    /// Return the provider identity used to coalesce replayed message deltas.
    pub fn provider_id(&self) -> Option<&str> {
        self.provider_id.as_deref()
    }

    /// Return whether the message is progress commentary or the final answer.
    pub fn delivery(&self) -> MessageDelivery {
        match self.role {
            None => MessageDelivery::Unknown,
            Some(MessageRole::Response) => MessageDelivery::Final,
            Some(MessageRole::Thought | MessageRole::ThoughtSummary | MessageRole::Commentary) => {
                MessageDelivery::Commentary
            }
        }
    }

    pub fn role(&self) -> Option<MessageRole> {
        self.role
    }

    pub fn state(&self) -> MessageState {
        self.state
    }

    /// Borrow all received text without granting mutation of completed history.
    pub fn text(&self) -> &str {
        &self.text
    }
}

/// Preserves first-seen message and tool positions within one provider execution.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum TurnItem {
    Message { id: String },
    Tool { id: String },
}
