use serde::{Deserialize, Serialize};

/// Describes one complete native checklist replacement.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ProviderChecklistUpdate {
    pub scope_id: String,
    pub name: Option<String>,
    pub complete: bool,
    #[serde(default = "default_true")]
    pub replace_entries: bool,
    pub entry_list: Vec<ProviderChecklistEntry>,
}

/// Describes one entry in a native checklist.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ProviderChecklistEntry {
    pub provider_id: Option<String>,
    pub content: String,
    pub priority: Option<String>,
    pub status: ChecklistStatus,
    pub provider_ordinal: usize,
}

/// Describes native task progress and retained superseded history.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum ChecklistStatus {
    Pending,
    InProgress,
    Completed,
    Superseded,
}

fn default_true() -> bool {
    true
}
