use forge_provider_api::{
    AgentMessageRole, ItemId, MessageClassification, ReasoningKind, SubmissionId,
};
use serde::{Deserialize, Serialize};

/// Describe the presentation role without conflating prose with reasoning.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum MessageKind {
    User,
    Unknown,
    Assistant,
    Reasoning,
    ReasoningSummary,
}

/// Preserve positive prose classification separately from content completion.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum MessageDelivery {
    Unknown,
    Commentary,
    Final,
}

/// Preserve the native content boundary independently of its owning turn.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum MessageState {
    Streaming,
    Completed,
    Interrupted,
}

/// Keep each message item's immutable native content family explicit.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "content", rename_all = "snake_case")]
pub(crate) enum MessageContent {
    User {
        submission_id: Option<SubmissionId>,
    },
    Agent {
        classification: MessageClassification,
    },
    Reasoning {
        kind: ReasoningKind,
    },
}

/// Retain the admitted text so replay cannot replace an item's initial evidence.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub(crate) struct MessageStart {
    pub content: MessageContent,
    pub text: String,
}

/// Own one native message or reasoning item under its original turn.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Message {
    pub(crate) id: String,
    pub(crate) provider_id: ItemId,
    pub(crate) content: MessageContent,
    pub(crate) start: MessageStart,
    pub(crate) state: MessageState,
    pub(crate) text: String,
}

impl Message {
    /// Get the stable local rendering identity.
    pub fn id(&self) -> &str {
        &self.id
    }
    /// Get the positively established content family.
    pub fn kind(&self) -> MessageKind {
        match &self.content {
            MessageContent::User { .. } => MessageKind::User,
            MessageContent::Agent {
                classification: MessageClassification::Unclassified,
            } => MessageKind::Unknown,
            MessageContent::Agent {
                classification: MessageClassification::Known(_),
            } => MessageKind::Assistant,
            MessageContent::Reasoning {
                kind: ReasoningKind::Full,
            } => MessageKind::Reasoning,
            MessageContent::Reasoning {
                kind: ReasoningKind::Summary,
            } => MessageKind::ReasoningSummary,
        }
    }
    /// Get the original native item's host-normalized identity.
    pub fn provider_id(&self) -> &ItemId {
        &self.provider_id
    }
    /// Get the native input association without copying attachment contents.
    pub fn submission_id(&self) -> Option<&SubmissionId> {
        match &self.content {
            MessageContent::User { submission_id } => submission_id.as_ref(),
            MessageContent::Agent { .. } | MessageContent::Reasoning { .. } => None,
        }
    }
    /// Get the optional prose role without guessing a reasoning or user role.
    pub fn role(&self) -> Option<AgentMessageRole> {
        match &self.content {
            MessageContent::Agent {
                classification: MessageClassification::Known(role),
            } => Some(*role),
            MessageContent::Agent {
                classification: MessageClassification::Unclassified,
            }
            | MessageContent::User { .. }
            | MessageContent::Reasoning { .. } => None,
        }
    }
    /// Get the native reasoning representation when this is a reasoning item.
    pub fn reasoning_kind(&self) -> Option<ReasoningKind> {
        match self.content {
            MessageContent::Reasoning { kind } => Some(kind),
            MessageContent::User { .. } | MessageContent::Agent { .. } => None,
        }
    }
    /// Get prose classification while retaining absence for non-prose content.
    pub fn classification(&self) -> MessageClassification {
        match self.content {
            MessageContent::Agent { classification } => classification,
            MessageContent::User { .. } | MessageContent::Reasoning { .. } => {
                MessageClassification::Unclassified
            }
        }
    }
    /// Get the rendering phase independently of item completion.
    pub fn delivery(&self) -> MessageDelivery {
        match &self.content {
            MessageContent::User { .. } => MessageDelivery::Final,
            MessageContent::Reasoning { .. } => MessageDelivery::Commentary,
            MessageContent::Agent {
                classification: MessageClassification::Unclassified,
            } => MessageDelivery::Unknown,
            MessageContent::Agent {
                classification: MessageClassification::Known(AgentMessageRole::Commentary),
            } => MessageDelivery::Commentary,
            MessageContent::Agent {
                classification: MessageClassification::Known(AgentMessageRole::Response),
            } => MessageDelivery::Final,
        }
    }
    /// Get the immutable content lifecycle state.
    pub fn state(&self) -> MessageState {
        self.state
    }
    /// Get retained text without granting mutation of terminal history.
    pub fn text(&self) -> &str {
        &self.text
    }
}

/// Preserve first-seen native item positions within one host turn.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum TurnItem {
    Message { id: String },
    Tool { id: String },
}
