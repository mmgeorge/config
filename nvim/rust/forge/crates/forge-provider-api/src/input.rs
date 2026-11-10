use serde::{Deserialize, Serialize};

/// The user's own text or explicit skill selection, before native skill resolution.
///
/// Harness-authored content never appears here; it travels as typed
/// [`crate::forge::TurnInput`] elements beside this input.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum PromptInput {
    Text(String),
    Skill { name: String, arguments: String },
}

impl PromptInput {
    /// Describe the input as the user wrote it, for transcripts and labels.
    pub fn display_text(&self) -> String {
        match self {
            Self::Text(text) => text.clone(),
            Self::Skill { name, arguments } if arguments.is_empty() => format!("${name}"),
            Self::Skill { name, arguments } => format!("${name} {arguments}"),
        }
    }
}

/// Identify one validated response to a host-owned structured question.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct QuestionReply {
    /// Identify the durable question independently of provider message IDs.
    pub question_id: String,
    /// Identify the question revision the user answered.
    pub revision: u64,
    /// Identify the durable host message recording this action.
    pub message_id: String,
    /// Preserve the question prompt for providers whose native reply requires it.
    pub question: String,
    /// Carry the structured response without embedding a provider wire envelope.
    pub response: QuestionReplyResponse,
}

/// Preserve the full user-visible meaning of one structured question choice.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct QuestionReplyOption {
    pub id: String,
    pub label: String,
    pub description: String,
    pub recommended: bool,
}

/// Preserve the distinct host question actions accepted by a provider adapter.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum QuestionReplyResponse {
    /// Record a validated answer and whether additional required questions remain.
    Answer {
        answer: String,
        required_questions_remain: bool,
    },
    /// Ask for clarification without resolving the question.
    Clarification {
        options: Vec<QuestionReplyOption>,
        text: String,
        reply_to: Option<String>,
    },
    /// Correct an answer that was previously recorded.
    Correction {
        answer: String,
        required_questions_remain: bool,
    },
}

/// Render admitted but definitely undelivered inputs ahead of the newest input.
///
/// Each earlier input is rendered as its [`crate::forge::earlier_input_block`] followed by its
/// text, so the marker and the earlier input's own elements precede the user's words.
pub fn recovered_text(current: &str, previous: &[(crate::forge::TurnInput, String)]) -> String {
    let mut parts = previous
        .iter()
        .enumerate()
        .map(|(index, (input, text))| {
            [
                crate::forge::earlier_input_block(index + 1, input),
                text.clone(),
            ]
            .into_iter()
            .filter(|part| !part.is_empty())
            .collect::<Vec<_>>()
            .join("\n")
        })
        .collect::<Vec<_>>();
    parts.push(current.to_owned());
    parts.retain(|part| !part.is_empty());
    parts.join("\n\n")
}

/// Carry prepared user text and explicitly selected files into a native provider adapter.
#[derive(Clone, Debug)]
pub struct MessageInput {
    /// User text and labels prepared by the input owner.
    pub prompt: String,
    /// Files already admitted and resolved by the input owner.
    pub attachments: Vec<FileAttachment>,
}

/// Identify one admitted file independently of a provider's attachment wire format.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FileAttachment {
    /// Canonical admitted file retained for the provider invocation.
    pub path: std::path::PathBuf,
    /// Stable host-visible label shared by prompt context and provider output.
    pub label: String,
    /// Detected media type, normalized before crossing the provider boundary.
    pub media_type: String,
    /// Admitted bytes used by native encoders for inline image or document input.
    pub contents: std::sync::Arc<[u8]>,
    /// Prepared neutral context wrapper for providers that retain files by path.
    pub context: String,
}

/// Select the provider-neutral execution posture for one admitted prompt.
///
/// Harness workflows such as plans, goals, and reviews are host intent: Harness renders them
/// into the prompt's instructions and tool set. This option carries only the one behavior a
/// native runtime can apply on its own, a read-only planning posture.
#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum ExecutionMode {
    /// Run with the provider's ordinary execution posture.
    #[default]
    Default,
    /// Prefer the provider's native planning posture when it offers one.
    Planning,
}
