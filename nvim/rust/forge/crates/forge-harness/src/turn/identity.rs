use serde::Serialize;

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize)]
#[serde(transparent)]
/// Identifies a retained execution turn independently of its provider's request ID.
pub struct TurnId(String);

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize)]
#[serde(transparent)]
/// Identifies a provider tool call within one retained turn.
pub struct ToolCallId(String);

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize)]
/// Preserves source identity without concatenating independently generated IDs.
pub struct ToolAddress {
    turn: TurnId,
    tool: ToolCallId,
}

impl ToolAddress {
    pub fn new(turn: &str, tool: &str) -> Self {
        Self {
            turn: TurnId(turn.to_owned()),
            tool: ToolCallId(tool.to_owned()),
        }
    }

    pub(crate) fn legacy_key(&self) -> String {
        format!("{}:{}", self.turn.0, self.tool.0)
    }
}
