use anyhow::{Result, ensure};
use forge_buffer::{
    block::TextPosition,
    identity::{BlockId, DocumentId, DocumentRevision, InputSequence, TargetId, ViewId},
};
use serde::{Deserialize, Serialize};

use crate::{StatusSection, context::StatusContext};

#[derive(Clone, Debug, PartialEq, Serialize)]
/// Complete status inventory. Lua projects headers independently of native diff bodies.
pub struct StatusSnapshot {
    pub document: DocumentId,
    pub revision: DocumentRevision,
    pub view: StatusView,
    pub head: StatusHead,
    pub context: Option<StatusContext>,
    pub section: Vec<StatusSectionRecord>,
    pub file: Vec<StatusFile>,
    pub pending: Vec<u64>,
}

#[derive(Clone, Debug, PartialEq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum StatusView {
    Status,
    Local {
        path: String,
    },
    Comparison {
        title: String,
        path: Option<String>,
        worktree: bool,
    },
}

#[derive(Clone, Debug, PartialEq, Serialize)]
pub struct StatusHead {
    pub state: &'static str,
    pub reference: Option<String>,
    pub object: Option<String>,
}

#[derive(Clone, Debug, PartialEq, Serialize)]
pub struct StatusSectionRecord {
    pub kind: StatusSection,
    pub file: Vec<u64>,
}

#[derive(Clone, Debug, PartialEq, Serialize)]
pub struct StatusFile {
    pub id: u64,
    pub section: StatusSection,
    pub change: &'static str,
    pub path: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub origin: Option<String>,
    pub untracked: bool,
    pub stats: StatusStatistics,
    pub generation: u64,
}

#[derive(Clone, Copy, Debug, PartialEq, Serialize)]
#[serde(tag = "state", rename_all = "snake_case")]
pub enum StatusStatistics {
    Unknown,
    Exact { added: u64, deleted: u64 },
    ExceedsLimit,
}

#[derive(Clone, Debug, Serialize)]
/// Contiguous inventory change. Body revisions advance independently of this revision.
pub struct StatusDelta {
    pub document: DocumentId,
    pub base: DocumentRevision,
    pub next: DocumentRevision,
    pub removed: Vec<u64>,
    pub file: Vec<StatusFile>,
    pub section: Vec<StatusSectionRecord>,
    pub pending: Vec<u64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub head: Option<StatusHead>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub context: Option<StatusContext>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
/// Captured action with native identities instead of Lua header layout coordinates.
pub struct StatusInput {
    pub document: DocumentId,
    pub revision: DocumentRevision,
    pub view: ViewId,
    pub sequence: InputSequence,
    pub action: String,
    pub location: StatusLocation,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case", deny_unknown_fields)]
pub enum StatusLocation {
    Boundary {
        after_files: bool,
    },
    File {
        id: u64,
    },
    Section {
        section: StatusSection,
    },
    Context {
        role: String,
    },
    Body {
        file: u64,
        generation: u64,
        revision: DocumentRevision,
        block: BlockId,
        position: TextPosition,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        target: Option<TargetId>,
    },
}

impl StatusInput {
    pub fn validate(&self) -> Result<()> {
        self.document.validate()?;
        self.revision.validate()?;
        self.view.validate()?;
        self.sequence.validate()?;
        ensure!(
            !self.action.is_empty() && self.action.len() <= 64,
            "invalid status action"
        );
        Ok(())
    }

    pub fn position(&self) -> TextPosition {
        match &self.location {
            StatusLocation::Body { position, .. } => *position,
            _ => TextPosition { row: 0, column: 0 },
        }
    }
}

#[derive(Clone, Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct StatusSelection {
    pub target: Vec<StatusLocation>,
}

pub(crate) fn file_target(id: u64) -> TargetId {
    TargetId(format!("file:{id}"))
}
