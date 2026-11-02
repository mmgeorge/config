use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Deserialize, Serialize)]
/// Retains a confirmed selected-skill read and its originating provider tool.
pub struct SkillRead {
    /// Identifies the selected catalog definition, independently of profile aliases.
    pub resource_id: String,
    /// Names the skill displayed in the timeline.
    pub name: String,
    /// Opens the canonical skill definition after runtime profiles are removed.
    pub path: String,
    /// Identifies the provider tool within its owning turn.
    pub tool_id: String,
}
