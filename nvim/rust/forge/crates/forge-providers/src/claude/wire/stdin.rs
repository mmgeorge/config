//! User frames the client writes to stdin in `--input-format stream-json` mode.

use super::content::InputBlock;
use serde::Serialize;

/// One user input, which starts a turn when the CLI is idle and is queued while a turn runs.
#[derive(Clone, Debug, PartialEq, Serialize)]
pub(crate) struct UserInput {
    #[serde(rename = "type")]
    pub kind: &'static str,
    pub message: UserInputMessage,
    pub parent_tool_use_id: Option<String>,
    pub session_id: &'static str,
    /// Client-stamped identity; the CLI reports this command's queue lifecycle under it and
    /// records it as the transcript entry's identity.
    pub uuid: String,
    /// `true` delivers the text verbatim, without slash-command or mention expansion.
    #[serde(skip_serializing_if = "std::ops::Not::not")]
    pub client_composed: bool,
    /// `false` appends the message to the transcript without starting a model request.
    #[serde(rename = "shouldQuery", skip_serializing_if = "Option::is_none")]
    pub should_query: Option<bool>,
    pub origin: InputOrigin,
}

/// The `message` body of a user input.
#[derive(Clone, Debug, PartialEq, Serialize)]
pub(crate) struct UserInputMessage {
    pub role: &'static str,
    pub content: Vec<InputBlock>,
}

/// Attribute input to the human at the keyboard, which the CLI requires to trust it.
#[derive(Clone, Debug, PartialEq, Serialize)]
pub(crate) struct InputOrigin {
    pub kind: &'static str,
}

impl UserInput {
    /// Build a verbatim user input.
    pub(crate) fn composed(uuid: String, content: Vec<InputBlock>) -> Self {
        Self {
            kind: "user",
            message: UserInputMessage {
                role: "user",
                content,
            },
            parent_tool_use_id: None,
            session_id: "",
            uuid,
            client_composed: true,
            should_query: None,
            origin: InputOrigin { kind: "human" },
        }
    }

    /// Build a slash command the CLI dispatches itself, such as `/compact`.
    pub(crate) fn command(uuid: String, text: &str) -> Self {
        let mut input = Self::composed(
            uuid,
            vec![InputBlock::Text {
                text: text.to_owned(),
            }],
        );
        input.client_composed = false;
        input
    }

    /// Build a transcript note that the CLI records without querying the model.
    pub(crate) fn note(uuid: String, text: &str) -> Self {
        let mut input = Self::composed(
            uuid,
            vec![InputBlock::Text {
                text: text.to_owned(),
            }],
        );
        input.should_query = Some(false);
        input
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn composed_input_is_verbatim_and_attributed_to_the_human() {
        let input = UserInput::composed(
            "uuid-1".into(),
            vec![InputBlock::Text {
                text: "/goal x".into(),
            }],
        );
        assert_eq!(
            serde_json::to_value(input).unwrap(),
            json!({"type":"user","message":{"role":"user","content":[{"type":"text","text":"/goal x"}]},
                "parent_tool_use_id":null,"session_id":"","uuid":"uuid-1","client_composed":true,
                "origin":{"kind":"human"}})
        );
        let note = serde_json::to_value(UserInput::note("uuid-2".into(), "rewound")).unwrap();
        assert_eq!(note["shouldQuery"], json!(false));
        let command =
            serde_json::to_value(UserInput::command("uuid-3".into(), "/compact")).unwrap();
        assert!(command.get("client_composed").is_none());
    }
}
