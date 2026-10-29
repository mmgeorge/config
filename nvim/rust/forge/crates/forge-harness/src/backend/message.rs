use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Eq, Hash, PartialEq, Deserialize, Serialize)]
#[serde(transparent)]
/// Provider message identity scoped by the event's execution address.
pub struct MessageId(pub String);

#[derive(Clone, Copy, Debug, Eq, PartialEq, Deserialize, Serialize)]
#[serde(rename_all = "snake_case")]
/// Semantic message role, independent of text streaming or completion.
pub enum MessageRole {
    Thought,
    ThoughtSummary,
    Commentary,
    Response,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
/// Ordered updates to one message. Complete text replaces accumulated deltas.
pub enum MessageUpdate {
    Started,
    Delta { text: String },
    Completed { text: String },
    Classified,
    Interrupted,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
/// Normalized message update. An absent role remains unresolved, not commentary.
pub struct ProviderMessage {
    pub id: MessageId,
    pub role: Option<MessageRole>,
    pub update: MessageUpdate,
}

impl ProviderMessage {
    /// Borrows text only for delta and complete-snapshot updates.
    pub fn text(&self) -> Option<&str> {
        match &self.update {
            MessageUpdate::Delta { text } | MessageUpdate::Completed { text } => Some(text),
            _ => None,
        }
    }

    pub(crate) fn is_snapshot(&self) -> bool {
        matches!(self.update, MessageUpdate::Completed { .. })
    }
}

impl MessageRole {
    pub(crate) fn from_phase(phase: &str) -> anyhow::Result<Self> {
        match phase {
            "thought" | "thinking" | "analysis" => Ok(Self::Thought),
            "commentary" => Ok(Self::Commentary),
            "response" | "final" | "final_answer" => Ok(Self::Response),
            _ => anyhow::bail!("unsupported provider message phase: {phase}"),
        }
    }
}
