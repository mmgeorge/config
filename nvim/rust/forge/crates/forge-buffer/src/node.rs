use serde::{Deserialize, Serialize};

use crate::identity::FoldId;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
/// Identifies the content owned by a presentation node.
pub enum NodeKind {
    /// One admitted interaction and its activity.
    Exchange,
    /// Assistant text without an expansion control.
    Message,
    /// Consecutive tool calls in one provider turn.
    ToolGroup,
    /// One tool heading and its output source.
    Tool,
    /// A collection of changed files.
    Changes,
    /// One file and its diff hunks.
    File,
    /// One contiguous diff range.
    Hunk,
    /// Other nested timeline details.
    Group,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
/// Describes source activity independently of expansion and loading.
pub enum NodeLifecycle {
    /// The source can still receive provider events.
    Live,
    /// The source activity has ended.
    Settled,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
/// Selects the materialized extent of a node's content.
pub enum NodeDisplay {
    /// Retain only the actionable heading.
    Heading,
    /// Show the automatic bounded live preview.
    Preview,
    /// Materialize content using the node's paging quota.
    Full,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
/// Shares node identity and presentation state in the same patch as its text.
pub struct NodeState {
    /// Stable identity within the presentation lifetime.
    pub id: FoldId,
    /// Immediate enclosing node, if any.
    pub parent: Option<FoldId>,
    /// Source category used by the renderer and action dispatcher.
    pub kind: NodeKind,
    /// Sibling position in source order.
    pub order: usize,
    /// Source activity independent of the display choice.
    pub lifecycle: NodeLifecycle,
    /// Display chosen by source lifecycle when no user override exists.
    pub default_display: NodeDisplay,
    /// Explicit shared user choice, or no override.
    pub expansion: Option<bool>,
    /// Resolved display for this publication.
    pub display: NodeDisplay,
    /// Incarnation checked by incoming node actions.
    pub generation: u64,
    /// Revision of this node's source content.
    pub content_revision: u64,
    /// Number of materialized body rows.
    pub loaded_rows: usize,
    /// Number of materialized body bytes.
    pub loaded_bytes: usize,
    /// Whether additional source content remains available.
    pub more: bool,
}

impl NodeState {
    /// Creates source metadata before the presentation owner assigns its incarnation.
    pub fn new(id: FoldId, kind: NodeKind, display: NodeDisplay) -> Self {
        Self {
            id,
            parent: None,
            kind,
            order: 0,
            lifecycle: NodeLifecycle::Settled,
            default_display: display,
            expansion: None,
            display,
            generation: 0,
            content_revision: 0,
            loaded_rows: 0,
            loaded_bytes: 0,
            more: false,
        }
    }

    /// Applies explicit intent without changing the source lifecycle default.
    pub fn resolve(&mut self, expansion: Option<bool>) {
        self.expansion = expansion;
        self.display = match expansion {
            Some(true) => NodeDisplay::Full,
            Some(false) => NodeDisplay::Heading,
            None => self.default_display,
        };
    }
}
