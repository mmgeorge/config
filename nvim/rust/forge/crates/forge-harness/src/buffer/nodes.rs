use std::collections::{HashMap, HashSet};

use anyhow::{Result, ensure};

use forge_buffer::block::BufferBlock;
use forge_buffer::identity::FoldId;
use forge_buffer::identity::{DocumentId, ViewId};
use forge_buffer::node::NodeState;
use forge_buffer::width::WidthProfile;
use serde::Deserialize;

#[derive(Deserialize)]
#[serde(tag = "action", rename_all = "snake_case")]
/// Changes explicit expansion or advances the current node's loaded extent.
pub enum NodeAction {
    /// Sets a shared explicit override independently of automatic preview policy.
    SetExpansion {
        /// Whether the node materializes its content.
        expanded: bool,
    },
    /// Extends the current page while retaining the expansion choice.
    LoadMore,
    /// Reopens from the initial page after an explicit retry.
    RetryLoading,
}

#[derive(Deserialize)]
/// Binds a node action to the requesting view and source incarnation.
pub struct NodeRequest {
    /// Exact presentation lifetime.
    pub document: DocumentId,
    /// Registered window issuing the action.
    pub view: ViewId,
    /// Monotonic action sequence shared by this presentation's windows.
    pub sequence: u64,
    /// Stable content owner within the presentation.
    pub node: String,
    /// Expected source incarnation.
    pub generation: u64,
    #[serde(flatten)]
    /// Requested expansion or loading transition.
    pub action: NodeAction,
    /// Desired page size in display rows.
    pub rows: usize,
    /// View geometry used to count wrapped display rows.
    pub width: WidthProfile,
}

#[derive(Default)]
/// Owns expansion choices and node incarnations for one open timeline presentation.
pub(super) struct NodeMap {
    /// Shared user overrides, independent of source lifecycle.
    pub choice: HashMap<String, bool>,
    /// Latest accepted explicit transition per node.
    pub intent_sequence: HashMap<String, u64>,
    /// Raw and parsed tool sources retained outside the native buffer.
    pub tool: HashMap<String, super::tool::ToolOutputView>,
    /// Formatted source byte windows retained for explicitly opened tools.
    pub tool_limit: HashMap<String, usize>,
    state: HashMap<String, NodeState>,
    text: HashMap<String, forge_buffer::text::BufferText>,
    generation: u64,
    anchor: HashMap<String, forge_buffer::identity::BlockId>,
    child: HashMap<String, HashSet<String>>,
}

impl NodeMap {
    /// Retires a deleted source node so its next incarnation receives a new generation.
    pub fn retire(&mut self, id: &str) {
        self.choice.remove(id);
        self.intent_sequence.remove(id);
        self.tool_limit.remove(id);
        if let Some(state) = self.state.remove(id)
            && let Some(parent) = state.parent
        {
            if let Some(children) = self.child.get_mut(&parent.0) {
                children.remove(id);
            }
        }
        self.child.remove(id);
        self.text.remove(id);
        self.anchor.remove(id);
    }

    /// Advances only the source node whose content changed.
    pub fn advance(&mut self, id: &str) -> Result<()> {
        if let Some(state) = self.state.get_mut(id) {
            ensure!(
                state.content_revision < forge_buffer::MAX_COUNTER,
                "node revision exhausted"
            );
            state.content_revision += 1;
        }
        Ok(())
    }
    /// Retains identity and explicit intent when source defaults change.
    pub fn register(&mut self, block: &BufferBlock) -> Result<()> {
        for fold in &block.metadata.fold {
            self.anchor.insert(fold.id.0.clone(), block.id.clone());
        }
        let Some(source) = &block.metadata.node else {
            return Ok(());
        };
        self.anchor.insert(source.id.0.clone(), block.id.clone());
        let previous = self.state.get(&source.id.0);
        let changed = previous.is_none_or(|state| {
            state.default_display != source.default_display
                || state.parent != source.parent
                || state.lifecycle != source.lifecycle
        }) || self.text.get(&source.id.0) != Some(&block.text);
        ensure!(
            previous
                .is_none_or(|state| !changed || state.content_revision < forge_buffer::MAX_COUNTER),
            "node revision exhausted"
        );
        ensure!(
            previous.is_some() || self.generation < forge_buffer::MAX_COUNTER,
            "node generation exhausted"
        );
        if let Some(previous) = previous
            && previous.parent != source.parent
            && let Some(parent) = &previous.parent
            && let Some(children) = self.child.get_mut(&parent.0)
        {
            children.remove(&source.id.0);
        }
        if let Some(parent) = &source.parent {
            self.child
                .entry(parent.0.clone())
                .or_default()
                .insert(source.id.0.clone());
        }
        let revision = previous.map_or(1, |state| state.content_revision + u64::from(changed));
        let generation = self
            .state
            .get(&source.id.0)
            .map(|state| state.generation)
            .unwrap_or_else(|| {
                self.generation += 1;
                self.generation
            });
        let mut state = source.clone();
        state.generation = generation;
        state.content_revision = revision;
        state.resolve(self.choice.get(&state.id.0).copied());
        self.state.insert(state.id.0.clone(), state);
        self.text.insert(source.id.0.clone(), block.text.clone());
        Ok(())
    }

    /// Lists descendants whose materialized page quotas end with their parent closure.
    pub fn descendants(&self, id: &str) -> Vec<String> {
        let mut pending = vec![id];
        let mut descendants = Vec::new();
        while let Some(parent) = pending.pop() {
            if let Some(children) = self.child.get(parent) {
                for child in children {
                    descendants.push(child.clone());
                    pending.push(child);
                }
            }
        }
        descendants
    }

    /// Resolves one node's source anchor without scanning its containing exchange.
    pub fn anchor(&self, id: &str) -> Option<&forge_buffer::identity::BlockId> {
        self.anchor.get(id)
    }

    /// Finds the enclosing file whose shared viewport budget governs a hunk.
    pub fn file_owner<'a>(&'a self, id: &'a str) -> &'a str {
        let mut current = id;
        while let Some(state) = self.state.get(current) {
            if state.kind == forge_buffer::node::NodeKind::File {
                return current;
            }
            let Some(parent) = &state.parent else {
                break;
            };
            current = &parent.0;
        }
        id
    }

    /// Identifies whether a node materializes viewport-sized content.
    pub fn kind(&self, id: &str) -> Option<forge_buffer::node::NodeKind> {
        self.state.get(id).map(|state| state.kind)
    }

    /// Returns the current incarnation for stale-action validation.
    pub fn generation(&self, id: &str) -> Option<u64> {
        self.state.get(id).map(|state| state.generation)
    }

    /// Publishes metadata with the content that the same revision materializes.
    pub fn project(&self, block: &mut BufferBlock) {
        let Some(source) = block.metadata.node.as_ref() else {
            return;
        };
        let Some(state) = self.state.get(&source.id.0) else {
            return;
        };
        let generation = state.generation;
        let revision = state.content_revision;
        let source = block.metadata.node.as_mut().expect("validated source node");
        source.generation = generation;
        source.resolve(self.choice.get(&source.id.0).copied());
        source.content_revision = revision;
    }
}

/// Resolves emitted node parents once when a source subtree is constructed.
pub(super) fn link(blocks: &mut [BufferBlock]) {
    let position: HashMap<_, _> = blocks
        .iter()
        .enumerate()
        .map(|(index, block)| (block.id.clone(), index))
        .collect();
    let mut ancestor: Vec<(FoldId, usize)> = Vec::new();
    let mut order: HashMap<Option<FoldId>, usize> = HashMap::new();
    for (index, block) in blocks.iter_mut().enumerate() {
        while ancestor.last().is_some_and(|(_, end)| *end < index) {
            ancestor.pop();
        }
        block.metadata.content_node = block
            .metadata
            .node
            .as_ref()
            .map(|node| node.id.clone())
            .or_else(|| ancestor.last().map(|(id, _)| id.clone()));
        if let Some(node) = &mut block.metadata.node {
            node.parent = ancestor.last().map(|(id, _)| id.clone());
            let next = order.entry(node.parent.clone()).or_default();
            node.order = *next;
            *next += 1;
            if let Some(fold) = block.metadata.fold.iter().find(|fold| fold.id == node.id)
                && let Some(end) = position.get(&fold.end.block)
            {
                ancestor.push((node.id.clone(), *end));
            }
        }
    }
}
