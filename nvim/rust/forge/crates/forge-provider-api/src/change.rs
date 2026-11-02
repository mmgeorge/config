use serde::{Deserialize, Serialize};

/// Describes a native file operation without consulting workspace state.
#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum ProviderChangeKind {
    Add,
    Delete,
    #[default]
    Update,
    Move,
}

/// Carries a provider-reported path and textual patch.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct ProviderFileChange {
    pub path: String,
    #[serde(default)]
    pub move_path: Option<String>,
    pub kind: ProviderChangeKind,
    pub diff: String,
}

impl ProviderFileChange {
    /// Separate a trailing move annotation from complete patch content.
    pub fn split_move_report(kind: ProviderChangeKind, diff: &str) -> (&str, Option<&str>) {
        if matches!(kind, ProviderChangeKind::Add | ProviderChangeKind::Delete) {
            return (diff, None);
        }
        let Some((patch, destination)) = diff.rsplit_once("\nMoved to: ") else {
            return (diff, None);
        };
        let destination = destination.trim_end_matches(['\r', '\n']);
        if destination.is_empty() || destination.contains(['\r', '\n']) {
            return (diff, None);
        }
        (patch, Some(destination))
    }
}

/// Collects native file edits for one tool lifecycle item.
#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub struct ProviderChangeSet {
    #[serde(default)]
    pub file: Vec<ProviderFileChange>,
}

impl ProviderChangeSet {
    /// Report whether the native item includes file edits.
    pub fn is_empty(&self) -> bool {
        self.file.is_empty()
    }
}
