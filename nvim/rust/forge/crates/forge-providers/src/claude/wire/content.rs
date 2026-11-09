//! Message content blocks shared by assistant, user, and stream frames.

use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};

/// Decode a tagged payload whose tag the CLI may extend beyond the variants this adapter types.
///
/// Known tags decode strictly into their typed variant so a malformed known payload is a protocol
/// error, while a tag this adapter does not interpret keeps its raw payload and never fails.
pub(crate) fn decode_tagged<Known: serde::de::DeserializeOwned>(
    value: Value,
    known_tags: &[&str],
) -> Result<Tagged<Known>, serde_json::Error> {
    let tag = value
        .get("type")
        .and_then(Value::as_str)
        .unwrap_or_default()
        .to_owned();
    if known_tags.contains(&tag.as_str()) {
        Ok(Tagged::Known(serde_json::from_value(value)?))
    } else {
        Ok(Tagged::Other {
            kind: tag,
            payload: value,
        })
    }
}

/// Hold either a typed payload or the raw payload of a tag this adapter does not interpret.
#[derive(Clone, Debug, PartialEq)]
pub(crate) enum Tagged<Known> {
    Known(Known),
    Other { kind: String, payload: Value },
}

/// A content block of an assistant or user message.
#[derive(Clone, Debug, PartialEq)]
pub(crate) enum ContentBlock {
    Text(TextBlock),
    Thinking(ThinkingBlock),
    RedactedThinking,
    ToolUse(ToolUseBlock),
    ToolResult(ToolResultBlock),
    Image,
    /// A block kind the Messages API defines but this adapter does not interpret, such as server
    /// tool results, citations-only documents, or compaction blocks.
    Other {
        kind: String,
        payload: Value,
    },
}

/// Text produced by the model.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub(crate) struct TextBlock {
    pub text: String,
}

/// Reasoning text, which is empty unless the CLI runs with a thinking display mode.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub(crate) struct ThinkingBlock {
    #[serde(default)]
    pub thinking: String,
}

/// A client-tool invocation requested by the model.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub(crate) struct ToolUseBlock {
    pub id: String,
    pub name: String,
    #[serde(default)]
    pub input: Value,
}

/// The result of one tool invocation, reported in a CLI-authored user message.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub(crate) struct ToolResultBlock {
    pub tool_use_id: String,
    #[serde(default)]
    pub content: Value,
    #[serde(default)]
    pub is_error: Option<bool>,
}

impl ToolResultBlock {
    /// Flatten tool result content into display text.
    pub(crate) fn text(&self) -> String {
        match &self.content {
            Value::String(text) => text.clone(),
            Value::Array(parts) => parts
                .iter()
                .filter_map(|part| part.get("text").and_then(Value::as_str))
                .collect::<Vec<_>>()
                .join("\n"),
            Value::Null => String::new(),
            other => other.to_string(),
        }
    }
}

#[derive(Deserialize)]
#[serde(tag = "type", rename_all = "snake_case")]
enum KnownContentBlock {
    Text(TextBlock),
    Thinking(ThinkingBlock),
    RedactedThinking {},
    ToolUse(ToolUseBlock),
    ToolResult(ToolResultBlock),
    Image {},
}

const CONTENT_BLOCK_TAGS: &[&str] = &[
    "text",
    "thinking",
    "redacted_thinking",
    "tool_use",
    "tool_result",
    "image",
];

impl<'de> Deserialize<'de> for ContentBlock {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let value = Value::deserialize(deserializer)?;
        Ok(
            match decode_tagged::<KnownContentBlock>(value, CONTENT_BLOCK_TAGS)
                .map_err(serde::de::Error::custom)?
            {
                Tagged::Known(KnownContentBlock::Text(block)) => Self::Text(block),
                Tagged::Known(KnownContentBlock::Thinking(block)) => Self::Thinking(block),
                Tagged::Known(KnownContentBlock::RedactedThinking {}) => Self::RedactedThinking,
                Tagged::Known(KnownContentBlock::ToolUse(block)) => Self::ToolUse(block),
                Tagged::Known(KnownContentBlock::ToolResult(block)) => Self::ToolResult(block),
                Tagged::Known(KnownContentBlock::Image {}) => Self::Image,
                Tagged::Other { kind, payload } => Self::Other { kind, payload },
            },
        )
    }
}

/// User message content, which the CLI sends either as plain text or as blocks.
#[derive(Clone, Debug, Deserialize, PartialEq)]
#[serde(untagged)]
pub(crate) enum UserContent {
    Text(String),
    Blocks(Vec<ContentBlock>),
}

/// An assistant API message as the CLI reports it, one content block per frame.
#[derive(Clone, Debug, Deserialize, PartialEq)]
pub(crate) struct AssistantMessage {
    pub id: String,
    #[serde(default)]
    pub model: Option<String>,
    #[serde(default)]
    pub content: Vec<ContentBlock>,
    #[serde(default)]
    pub stop_reason: Option<String>,
    #[serde(flatten)]
    pub extra: Map<String, Value>,
}

/// A user message as the CLI reports it, chiefly tool results it appends itself.
#[derive(Clone, Debug, Deserialize, PartialEq)]
pub(crate) struct UserMessage {
    pub content: UserContent,
}

/// Token accounting reported on results and API message deltas.
#[derive(Clone, Copy, Debug, Default, Deserialize, PartialEq)]
pub(crate) struct Usage {
    #[serde(default)]
    pub input_tokens: Option<u64>,
    #[serde(default)]
    pub output_tokens: Option<u64>,
    #[serde(default)]
    pub cache_creation_input_tokens: Option<u64>,
    #[serde(default)]
    pub cache_read_input_tokens: Option<u64>,
}

impl Usage {
    /// Get total prompt tokens including cache reads and writes.
    pub(crate) fn total_input(&self) -> Option<u64> {
        self.input_tokens?
            .checked_add(self.cache_creation_input_tokens.unwrap_or(0))?
            .checked_add(self.cache_read_input_tokens.unwrap_or(0))
    }
}

/// Encode user input blocks sent to the CLI.
#[derive(Clone, Debug, PartialEq, Serialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub(crate) enum InputBlock {
    Text { text: String },
    Image { source: ImageSource },
}

/// Inline image data for an input block.
#[derive(Clone, Debug, PartialEq, Serialize)]
pub(crate) struct ImageSource {
    #[serde(rename = "type")]
    pub kind: &'static str,
    pub media_type: String,
    pub data: String,
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn unknown_block_kinds_keep_their_payload_and_known_kinds_decode_strictly() {
        let blocks: Vec<ContentBlock> = serde_json::from_value(json!([
            {"type":"text","text":"hello"},
            {"type":"server_tool_use","id":"srv","name":"web_search","input":{}},
            {"type":"tool_use","id":"toolu","name":"Read","input":{"file_path":"a"}},
        ]))
        .unwrap();
        assert_eq!(
            blocks[0],
            ContentBlock::Text(TextBlock {
                text: "hello".into()
            })
        );
        assert!(
            matches!(&blocks[1], ContentBlock::Other { kind, .. } if kind == "server_tool_use")
        );
        assert!(matches!(&blocks[2], ContentBlock::ToolUse(block) if block.name == "Read"));
        assert!(serde_json::from_value::<ContentBlock>(json!({"type":"tool_use"})).is_err());
    }

    #[test]
    fn tool_results_flatten_text_parts() {
        let block: ContentBlock = serde_json::from_value(json!({
            "type":"tool_result","tool_use_id":"toolu",
            "content":[{"type":"text","text":"one"},{"type":"text","text":"two"}]
        }))
        .unwrap();
        let ContentBlock::ToolResult(result) = block else {
            panic!("tool result decodes as a tool result");
        };
        assert_eq!(result.text(), "one\ntwo");
    }
}
