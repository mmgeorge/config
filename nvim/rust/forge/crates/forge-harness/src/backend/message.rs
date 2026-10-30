use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Eq, Hash, PartialEq, Deserialize, Serialize)]
#[serde(tag = "origin", content = "id", rename_all = "snake_case")]
pub enum MessageId {
    Native(String),
    Synthetic(String),
}

impl MessageId {
    pub fn as_str(&self) -> &str {
        match self {
            Self::Native(id) | Self::Synthetic(id) => id,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Deserialize, Serialize)]
#[serde(rename_all = "snake_case")]
/// Semantic message role, independent of text streaming or completion.
pub enum MessageRole {
    Thought,
    ThoughtSummary,
    Commentary,
    Response,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Deserialize, Serialize)]
#[serde(tag = "kind", content = "role", rename_all = "snake_case")]
pub enum MessageClassification {
    Unclassified,
    Known(MessageRole),
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
/// Ordered updates to one message. Complete text replaces accumulated deltas.
pub enum MessageUpdate {
    Started {
        classification: MessageClassification,
        text: String,
    },
    Delta {
        text: String,
    },
    Completed {
        text: String,
    },
    Classified {
        role: MessageRole,
    },
    Interrupted,
}

impl MessageUpdate {
    pub fn text(&self) -> Option<&str> {
        match self {
            Self::Started { text, .. } | Self::Delta { text } | Self::Completed { text } => {
                Some(text)
            }
            _ => None,
        }
    }
}
