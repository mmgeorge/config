use std::collections::{HashMap, HashSet};

use anyhow::{Context, Result, ensure};
use forge_buffer::block::{BlockAnchor, BlockMetadata, BufferBlock, DeferredSection, TextPosition};
use forge_buffer::identity::{BlockId, DocumentId, ViewId};
use forge_buffer::patch::BufferPatch;
use forge_buffer::text::BufferText;
use forge_buffer::width::WidthProfile;

use super::document::{TranscriptChange, TranscriptDocument, TranscriptEntry, TranscriptSource};

const PAGE_BYTES: usize = 64 * 1024;

#[derive(Clone)]
struct PageQuota {
    bytes: usize,
    rows: usize,
    width: Option<WidthProfile>,
}

impl PageQuota {
    fn bytes(bytes: usize) -> Self {
        Self {
            bytes,
            rows: usize::MAX,
            width: None,
        }
    }

    fn consume(&mut self, blocks: &[BufferBlock]) -> Result<()> {
        for block in blocks {
            self.bytes = self.bytes.saturating_sub(block.text.byte_count());
            if let Some(width) = &self.width {
                for row in 0..block.text.row_count() {
                    let cells = width.cells(block.text.row(row).expect("source row"), 0)?;
                    self.rows = self
                        .rows
                        .saturating_sub(cells.div_ceil(width.columns).max(1));
                }
            } else {
                self.rows = self.rows.saturating_sub(block.text.row_count());
            }
        }
        Ok(())
    }

    fn exhausted(&self) -> bool {
        self.bytes == 0 || self.rows == 0
    }
}

/// Owns the loaded projection independently of the complete Rust transcript.
pub(super) struct SectionProjection {
    pub document: TranscriptDocument,
    view: HashMap<ViewId, u64>,
    pub(super) nodes: super::nodes::NodeMap,
    complete: HashSet<BlockId>,
    owner: HashMap<String, String>,
    file: HashMap<String, Vec<String>>,
    quota: HashMap<String, PageQuota>,
    entry_sections: HashMap<String, Vec<String>>,
    pending: HashSet<String>,
    #[cfg(test)]
    pub projected_entries: usize,
}

impl SectionProjection {
    pub fn new(source: &mut TranscriptDocument, id: DocumentId) -> Result<Self> {
        let mut owner = HashMap::new();
        let mut file = HashMap::new();
        let mut entry_sections = HashMap::new();
        let mut entry = Vec::new();
        let mut nodes = super::nodes::NodeMap::default();
        let mut complete = HashSet::new();
        for identity in source.entry_ids() {
            let original = source.source_entry(&identity)?;
            entry_sections.insert(identity, index(&original, &mut owner, &mut file));
            for block in &original.block {
                nodes.register(block)?;
            }
            let mut rendered = project(original, source.document.revision().0, &HashMap::new(), &HashMap::new())?;
            for block in &mut rendered.block {
                if source.document.block(&block.id).is_some_and(|source| source.text == block.text) {
                    complete.insert(block.id.clone());
                }
                nodes.project(block);
            }
            entry.push(rendered);
        }
        let projection = Self {
            document: TranscriptDocument::initialize(id, "loaded-transcript".into(), 0, entry)?,
            view: HashMap::new(),
            nodes,
            complete,
            owner,
            file,
            quota: HashMap::new(),
            entry_sections,
            pending: HashSet::new(),
            #[cfg(test)]
            projected_entries: 0,
        };
        source.dirty.clear();
        source.block_dirty.clear();
        source.body_dirty.clear();
        source.structure_dirty = false;
        Ok(projection)
    }

    pub fn refresh(&mut self, source: &mut TranscriptDocument) -> Result<Vec<BufferPatch>> {
        let mut splices = Vec::new();
        let mut node_updates = Vec::new();
        for change in std::mem::take(&mut source.body_dirty) {
            if source.dirty.contains(&change.owner) { continue; }
            let call = change.anchor.0.strip_suffix(":preview")
                .or_else(|| change.anchor.0.split_once(":output:").map(|(call, _)| call))
                .or_else(|| change.anchor.0.strip_suffix(":hidden"));
            let node_id = call.map(|call| format!("{call}:tool"));
            let mut heading = node_id.as_ref().and_then(|id|
                self.document.document.block(&BlockId(id.clone())).cloned());
            if let Some(id) = &node_id { self.nodes.advance(id)?; }
            if self.document.document.block_index(&change.anchor).is_none() { continue; }
            if change.removed.iter().any(|id| !self.complete.contains(id)) {
                if let Some(id) = node_id { self.pending.insert(id); }
                else { source.dirty.insert(change.owner); }
                continue;
            }
            let blocks = change.inserted.iter().map(|id| {
                source.document.block(id).cloned().context("streaming splice source is missing")
            }).collect::<Result<Vec<_>>>()?;
            let removed_bytes: usize = change.removed.iter().filter_map(|id| self.document.document.block(id))
                .map(|block| block.text.byte_count()).sum();
            let inserted_bytes: usize = blocks.iter().map(|block| block.text.byte_count()).sum();
            if let Some(heading) = &mut heading {
                let node = heading.metadata.node.as_mut().expect("tool heading node");
                let next_bytes = node.loaded_bytes.saturating_sub(removed_bytes) + inserted_bytes;
                let maximum = self.quota.get(&node.id.0).map_or(PAGE_BYTES, |quota|quota.bytes);
                if next_bytes > maximum {
                    self.pending.insert(node.id.0.clone());
                    continue;
                }
                node.loaded_bytes = next_bytes;
                let removed_rows: usize = change.removed.iter().filter_map(|id| self.document.document.block(id))
                    .map(|block|block.text.row_count()).sum();
                node.loaded_rows = node.loaded_rows.saturating_sub(removed_rows)
                    + blocks.iter().map(|block|block.text.row_count()).sum::<usize>();
                let maximum_rows = self.quota.get(&node.id.0).map_or(48, |quota|quota.rows);
                if node.loaded_rows > maximum_rows {
                    self.pending.insert(node.id.0.clone());
                    continue;
                }
                self.nodes.project(heading);
            }
            for id in &change.removed { self.complete.remove(id); }
            for id in &change.inserted { self.complete.insert(id.clone()); }
            splices.push(TranscriptChange::ToolBody { owner: change.owner, anchor: change.anchor,
                removed: change.removed.len(), block: blocks });
            if let Some(heading) = heading { node_updates.push(heading); }
        }
        let changed_blocks = std::mem::take(&mut source.block_dirty);
        let mut direct = node_updates;
        for id in changed_blocks {
            let Some(owner) = source.block_owner(&id) else { continue; };
            if source.dirty.contains(owner) { continue; }
            let Some(previous) = self.document.document.block(&id) else { continue; };
            if !self.complete.contains(&id) {
                source.dirty.insert(owner.to_owned());
                continue;
            }
            let Some(current) = source.document.block(&id) else { continue; };
            if let (Some(node), Some(prior)) = (&current.metadata.node, &previous.metadata.node)
                && node.kind == forge_buffer::node::NodeKind::Tool && node.more && !prior.more {
                self.nodes.register(current)?;
                self.pending.insert(node.id.0.clone());
                continue;
            }
            let mut block = current.clone();
            block.metadata.fold.clone_from(&previous.metadata.fold);
            block.metadata.section.clone_from(&previous.metadata.section);
            if let (Some(node), Some(prior)) = (&mut block.metadata.node, &previous.metadata.node) {
                node.loaded_rows = prior.loaded_rows;
                node.loaded_bytes = prior.loaded_bytes;
                node.more = prior.more;
            }
            self.nodes.register(&block)?;
            self.nodes.project(&mut block);
            direct.push(block);
        }
        let dirty = source.dirty.clone();
        let structural = source.structure_dirty;
        let mut projected = HashMap::new();
        let mut prepared_owner = HashMap::new();
        let mut prepared_file = HashMap::new();
        let mut prepared_sections = HashMap::new();
        let mut retired_sections = Vec::new();
        for id in &dirty {
            if source.entry_position(id).is_none() {
                continue;
            }
            #[cfg(test)]
            { self.projected_entries += 1; }
            let original = source.source_entry(id)?;
            for block in &original.block {
                self.nodes.register(&block)?;
            }
            prepared_sections.insert(
                id.clone(),
                index(&original, &mut prepared_owner, &mut prepared_file),
            );
            projected.insert(
                id.clone(),
                project(original, self.document.document.revision().0 + 1, &self.nodes.choice, &self.quota)?,
            );
        }
        for id in &dirty {
            if let Some(previous) = self.entry_sections.remove(id) {
                for section in previous {
                    if self.owner.get(&section) == Some(id) {
                        self.owner.remove(&section);
                        self.file.remove(&section);
                        retired_sections.push(section);
                    }
                }
            }
        }
        self.owner.extend(prepared_owner);
        self.file.extend(prepared_file);
        self.entry_sections.extend(prepared_sections);
        for section in retired_sections {
            if !self.owner.contains_key(&section) {
                self.quota.remove(&section);

            }
        }
        for entry in projected.values_mut() {
            for block in &mut entry.block { self.nodes.project(block); }
        }
        let displaced: HashSet<String> = projected
            .values()
            .flat_map(|entry| {
                entry.block.iter().filter_map(|block| {
                    self.document
                        .block_owner(&block.id)
                        .filter(|owner| *owner != entry.id)
                        .map(str::to_owned)
                })
            })
            .collect();
        let mut patch = Vec::new();
        for change in splices {
            let TranscriptChange::ToolBody { owner, .. } = &change else { unreachable!() };
            if !dirty.contains(owner) { patch.extend(self.document.tool_layout(vec![change])?); }
        }
        for mut block in direct {
            if source.block_owner(&block.id).is_some_and(|owner| !dirty.contains(owner)) {
                if let Some(current) = self.document.document.block(&block.id) {
                    block.metadata.fold.clone_from(&current.metadata.fold);
                }
                if let Some(update) = self.document.append(block)? { patch.push(update); }
            }
        }
        for entry in projected.values() {
            if let Ok(previous) = self.document.source_entry(&entry.id) {
                for block in previous.block { self.complete.remove(&block.id); }
            }
            for block in &entry.block {
                if source.document.block(&block.id).is_some_and(|source| source.text == block.text) {
                    self.complete.insert(block.id.clone());
                }
            }
        }
        if structural || !displaced.is_empty() {
            let desired = source.entry_ids();
            let retained: HashSet<_> = desired.iter().collect();
            let mut current = self.document.entry_ids();
            for position in (0..current.len()).rev() {
                if !retained.contains(&current[position]) || displaced.contains(&current[position])
                {
                    let id = current.remove(position);
                    if !retained.contains(&id) {
                        if let Some(sections) = self.entry_sections.remove(&id) {
                            for section in sections {
                                if self.owner.get(&section) == Some(&id) {
                                    self.owner.remove(&section);
                                    self.file.remove(&section);
                                    self.quota.remove(&section);
                                }
                            }
                        }
                    } else if !projected.contains_key(&id) {
                        anyhow::bail!("transferred section owner was not updated: {id}");
                    }
                    patch.extend(self.document.tool_layout(vec![TranscriptChange::Remove {
                        index: position,
                        id,
                    }])?);
                }
            }
            for (position, id) in desired.iter().enumerate() {
                if current.get(position) == Some(id) {
                    if let Some(entry) = projected.remove(id) {
                        patch.extend(self.document.tool_layout(vec![
                            TranscriptChange::Replace {
                                index: position,
                                entry,
                            },
                        ])?);
                    }
                } else {
                    if let Some(previous) = current.iter().position(|existing| existing == id) {
                        let moved = current.remove(previous);
                        patch.extend(self.document.tool_layout(vec![
                            TranscriptChange::Remove {
                                index: previous,
                                id: moved,
                            },
                        ])?);
                    }
                    let entry = projected
                        .remove(id)
                        .context("inserted section source is missing")?;
                    patch.extend(self.document.tool_layout(vec![TranscriptChange::Insert {
                        index: position,
                        entry,
                    }])?);
                    current.insert(position, id.clone());
                }
            }
        } else {
            for (id, entry) in projected {
                let position = source
                    .entry_position(&id)
                    .context("section owner disappeared")?;
                patch.extend(self.document.tool_layout(vec![TranscriptChange::Replace {
                    index: position,
                    entry,
                }])?);
            }
        }
        let mut pending = std::mem::take(&mut self.pending).into_iter().filter_map(|id| {
            let anchor = self.nodes.anchor(&id)?;
            let start = source.document.block_index(anchor)?;
            let end = source.document.block(anchor)?.metadata.fold.iter().find(|fold| fold.id.0 == id)
                .and_then(|fold| source.document.block_index(&fold.end.block)).unwrap_or(start);
            Some((start, end, id))
        }).collect::<Vec<_>>();
        pending.sort_by_key(|(start, end, _)| (*start, std::cmp::Reverse(*end)));
        let mut covered = None;
        for (source_start, source_end, id) in pending {
            if covered.is_some_and(|end| source_start <= end) { continue; }
            covered = Some(source_end);
            let mut candidate = project(Self::subtree(&self.nodes, source, &id)?,
                self.document.document.revision().0 + 1, &self.nodes.choice, &self.quota)?;
            if dirty.contains(&candidate.id) { continue; }
            let Some(anchor) = self.nodes.anchor(&id) else { continue; };
            let Some(previous) = self.document.document.block(anchor) else { continue; };
            let start = self.document.document.block_index(anchor).context("loaded node anchor disappeared")?;
            let end = previous.metadata.fold.iter().find(|fold| fold.id.0 == id)
                .and_then(|fold| self.document.document.block_index(&fold.end.block)).unwrap_or(start);
            let removed = end + 1 - start;
            for block in self.document.document.blocks(self.document.document.revision(), start..end + 1)? {
                self.complete.remove(&block.id);
            }
            for block in &mut candidate.block {
                self.nodes.project(block);
                if source.document.block(&block.id).is_some_and(|original| original.text == block.text) {
                    self.complete.insert(block.id.clone());
                }
            }
            patch.extend(self.document.tool_layout(vec![TranscriptChange::ToolBody {
                owner: candidate.id, anchor: anchor.clone(), removed, block: candidate.block,
            }])?);
        }
        for id in dirty {
            source.dirty.remove(&id);
        }
        source.structure_dirty = false;
        self.document.dirty.clear();
        self.document.block_dirty.clear();
        self.document.body_dirty.clear();
        Ok(patch)
    }

    fn subtree<'source>(nodes: &super::nodes::NodeMap, document: &'source TranscriptDocument, id: &str) -> Result<TranscriptSource<'source>> {
        let anchor = nodes.anchor(id).context("node anchor is unavailable")?;
        let block = document.document.block(anchor).context("node source disappeared")?;
        let start = document.document.block_index(anchor).context("node source position disappeared")?;
        let end = block.metadata.fold.iter().find(|fold| fold.id.0 == id)
            .and_then(|fold| document.document.block_index(&fold.end.block)).unwrap_or(start);
        Ok(TranscriptSource {
            id: document.block_owner(anchor).context("node owner disappeared")?,
            block: document.document.blocks(document.document.revision(), start..end + 1)?.collect(),
        })
    }

    #[cfg(test)]
    pub fn quota_for_test(&mut self, id: &str, bytes: usize) {
        self.quota.insert(id.into(), PageQuota::bytes(bytes));
    }

    /// Drops intent only when a canonical entry is deleted, not when the scope changes.
    pub fn retire_entry(&mut self, id: &str) {
        if let Some(nodes) = self.entry_sections.get(id) {
            for node in nodes { self.nodes.retire(node); self.quota.remove(node); self.pending.remove(node); }
        }
    }

    pub fn register(&mut self, view: ViewId) {
        self.view.entry(view).or_default();
    }

    pub fn close(&mut self, source: &mut TranscriptDocument, view: &ViewId) {
        self.view.remove(view);
        if self.view.is_empty() {
            for id in self.nodes.choice.keys() {
                if let Some(owner) = self.owner.get(id) { source.dirty.insert(owner.clone()); }
            }
            self.nodes.choice.clear();
            self.nodes.intent_sequence.clear();
            self.quota.clear();
        }
    }

    /// Shared explicit choices survive parent closure and scope switches.
    pub fn expansion(&self, id: &str) -> Option<bool> { self.nodes.choice.get(id).copied() }

    /// Rejects retired identities before any source materialization can occur.
    pub fn admit(&self, view: &ViewId, sequence: u64, id: &str, generation: u64) -> Result<bool> {
        let last = self.view.get(view).context("node view is closed")?;
        ensure!(sequence <= forge_buffer::MAX_COUNTER, "node sequence exhausted");
        if sequence <= *last || self.nodes.intent_sequence.get(id).is_some_and(|prior| sequence <= *prior) { return Ok(false); }
        ensure!(self.nodes.generation(id) == Some(generation), "node belongs to a retired content generation");
        Ok(true)
    }


    pub fn set(
        &mut self,
        source: &mut TranscriptDocument,
        view: ViewId,
        sequence: u64,
        id: &str,
        expanded: bool,
        more: bool,
        page: Option<(usize, WidthProfile)>,
    ) -> Result<()> {
        let last = self.view.get(&view).context("section view is closed")?;
        ensure!(sequence <= forge_buffer::MAX_COUNTER, "section sequence exhausted");
        if sequence <= *last || (more && (!expanded || self.nodes.choice.get(id) == Some(&false))) {
            return Ok(());
        }
        self.owner.get(id).context("section is no longer available")?;
        let children = self.file.get(id);
        let file = children.is_some();
        let paged = file || self.nodes.kind(id) == Some(forge_buffer::node::NodeKind::Tool);
        let affected = self.nodes.file_owner(id).to_owned();
        let loaded = if more { Some(Self::subtree(&self.nodes, &self.document, &affected)?) } else { None };
        if let Some(loaded) = &loaded {
            if !loaded.block.iter().any(|block| block.metadata.section.iter()
                .any(|section| section.id.0 == id && section.more)) {
                *self.view.get_mut(&view).expect("validated section view") = sequence;
                return Ok(());
            }
        }
        if let Some((rows, width)) = &page {
            ensure!(
                (1..=8192).contains(rows),
                "section row budget exceeds layout limits"
            );
            width.validate()?;
        }
        let resized = page.as_ref().is_some_and(|(_, width)| {
            self.quota.get(id).and_then(|quota| quota.width.as_ref()) != Some(width)
        });
        let loaded_rows = if more && paged && resized {
            if let Some((_, width)) = &page {
                let loaded = Self::subtree(&self.nodes, &self.document, &affected)?;
                let start = loaded
                    .block
                    .iter()
                    .position(|block| block.metadata.fold.iter().any(|fold| fold.id.0 == id));
                if let Some(start) = start {
                    let end_id = &loaded.block[start]
                        .metadata
                        .fold
                        .iter()
                        .find(|fold| fold.id.0 == id)
                        .unwrap()
                        .end
                        .block;
                    let end = loaded
                        .block
                        .iter()
                        .position(|block| &block.id == end_id)
                        .unwrap_or(start);
                    let mut rows = 0;
                    for block in &loaded.block[start + 1..end.saturating_add(1).max(start + 1)] {
                        if block.id.0.ends_with(":deferred-body") {
                            continue;
                        }
                        for row in 0..block.text.row_count() {
                            rows += width
                                .cells(block.text.row(row).unwrap(), 0)?
                                .div_ceil(width.columns)
                                .max(1);
                        }
                    }
                    rows
                } else {
                    0
                }
            } else {
                0
            }
        } else {
            0
        };
        let previous_intent = self.nodes.choice.get(id).copied();
        let previous_quota = self.quota.get(id).cloned();
        if !more { self.nodes.choice.insert(id.into(), expanded); }
        if !expanded { self.quota.remove(id); }
        let prepared = (|| -> Result<TranscriptEntry> {
        let next_quota = &mut self.quota;
        if expanded {
            let quota = next_quota.entry(id.into()).or_insert_with(|| {
                if paged {
                    let (rows, width) = page.clone().unwrap_or((48, WidthProfile::default()));
                    PageQuota {
                        bytes: PAGE_BYTES,
                        rows,
                        width: Some(width),
                    }
                } else {
                    PageQuota::bytes(PAGE_BYTES)
                }
            });
            if more {
                if paged && quota.width.is_some() {
                    let (rows, width) = page.unwrap_or((48, WidthProfile::default()));
                    quota.rows = quota.rows.max(loaded_rows).saturating_add(rows);
                    quota.bytes = quota.bytes.saturating_add(PAGE_BYTES);
                    quota.width = Some(width);
                } else {
                    quota.bytes = quota.bytes.saturating_add(PAGE_BYTES);
                }
            }
        }
            let candidate = project(
                Self::subtree(&self.nodes, source, &affected)?,
                self.document.document.revision().0 + 1,
                &self.nodes.choice,
                &self.quota,
            )?;
            if let Some(loaded) = &loaded {
                let before = page_boundary(&loaded.block, id);
                let after = page_boundary(&candidate.block.iter().collect::<Vec<_>>(), id);
                ensure!(after.is_none() || after != before, "section loading made no progress; reopen the section to retry");
            }
            Ok(candidate)
        })();
        match prepared {
            Ok(_) => {},
            Err(error) => {
            match previous_intent { Some(value) => { self.nodes.choice.insert(id.into(), value); }, None => { self.nodes.choice.remove(id); } }
            match previous_quota { Some(value) => { self.quota.insert(id.into(), value); }, None => { self.quota.remove(id); } }
            return Err(error);
            }
        };
        *self.view.get_mut(&view).expect("validated section view") = sequence;
        if !expanded {
            for child in self.nodes.descendants(id) { self.quota.remove(&child); }
        }
        self.nodes.intent_sequence.insert(id.into(), sequence);
        self.pending.insert(affected);
        Ok(())
    }
}

fn page_boundary(blocks: &[&BufferBlock], id: &str) -> Option<(BlockId, usize)> {
    let boundary = blocks.iter().position(|block| block.metadata.section.iter()
        .any(|section| section.id.0 == id && section.more))?;
    blocks[..boundary].iter().rev().find(|block| block.text.row_count() > 0)
        .map(|block| (block.id.clone(), block.text.byte_count()))
}

fn index(
    entry: &TranscriptSource<'_>,
    owner: &mut HashMap<String, String>,
    file: &mut HashMap<String, Vec<String>>,
) -> Vec<String> {
    let mut sections = Vec::new();
    for (start, block) in entry.block.iter().enumerate() {
        if let Some(node) = &block.metadata.node
            && node.kind != forge_buffer::node::NodeKind::Message
            && !block.metadata.fold.iter().any(|fold| fold.id == node.id) {
            owner.insert(node.id.0.clone(), entry.id.to_owned());
            sections.push(node.id.0.clone());
        }
        for fold in &block.metadata.fold {
            owner.insert(fold.id.0.clone(), entry.id.to_owned());
            sections.push(fold.id.0.clone());
            if fold.expand_children {
                let end = entry
                    .block
                    .iter()
                    .position(|block| block.id == fold.end.block)
                    .unwrap_or(start);
                file.insert(
                    fold.id.0.clone(),
                    entry.block[start + 1..end.saturating_add(1).max(start + 1)]
                        .iter()
                        .flat_map(|block| block.metadata.fold.iter().map(|fold| fold.id.0.clone()))
                        .collect(),
                );
            }
        }
    }
    sections
}

pub(crate) fn preview(entry: TranscriptEntry) -> Result<TranscriptEntry> {
    project(TranscriptSource { id: &entry.id, block: entry.block.iter().collect() },
        0, &HashMap::new(), &HashMap::new())
}

fn project(
    entry: TranscriptSource<'_>,
    revision: u64,
    intent: &HashMap<String, bool>,
    quota: &HashMap<String, PageQuota>,
) -> Result<TranscriptEntry> {
    let position: HashMap<_, _> = entry
        .block
        .iter()
        .enumerate()
        .map(|(index, block)| (&block.id, index))
        .collect();
    let mut output = Vec::new();
    render(
        &entry.block,
        0,
        entry.block.len(),
        revision,
        intent,
        quota,
        &position,
        &mut output,
        PageQuota::bytes(usize::MAX),
        false,
        None,
    )?;
    Ok(TranscriptEntry {
        id: entry.id.to_owned(),
        block: output,
    })
}

fn render(
    source: &[&BufferBlock],
    mut cursor: usize,
    end: usize,
    revision: u64,
    intent: &HashMap<String, bool>,
    quota: &HashMap<String, PageQuota>,
    position: &HashMap<&BlockId, usize>,
    output: &mut Vec<BufferBlock>,
    mut budget: PageQuota,
    within_file: bool,
    parent_file: Option<&str>,
) -> Result<bool> {
    while cursor < end {
        if budget.exhausted() && source[cursor].text.row_count() > 0 {
            return Ok(true);
        }
        let source_block = source[cursor];
        let selected = source_block
            .metadata
            .fold
            .iter()
            .filter_map(|fold| {
                position
                    .get(&fold.end.block)
                    .copied()
                    .map(|last| (last + 1, fold))
            })
            .filter(|(last, _)| *last <= end && *last > cursor + 1)
            .max_by_key(|(last, _)| *last);
        if let Some((after, original)) = selected {
            let id = &original.id.0;
            let file = original.expand_children;
            let expanded = intent.get(id).copied().unwrap_or_else(|| {
                parent_file.is_some_and(|file| intent.get(file) == Some(&true)) || !original.closed
            });
            let heading = output.len();
            let mut block = source_block.clone();
            block.metadata.fold.clear();
            output.push(block);
            let mut remaining = budget.clone();
            remaining.consume(&output[heading..])?;
            let child_budget = if within_file {
                remaining
            } else {
                quota
                    .get(id)
                    .cloned()
                    .unwrap_or_else(|| {
                        let mut budget = PageQuota::bytes(PAGE_BYTES);
                        if source_block.metadata.node.as_ref().is_some_and(|node|
                            node.kind == forge_buffer::node::NodeKind::Tool) {
                            budget.rows = 48;
                        }
                        budget
                    })
            };
            let more = expanded
                && (render(
                    source,
                    cursor + 1,
                    after,
                    revision,
                    intent,
                    quota,
                    position,
                    output,
                    child_budget,
                    within_file || file,
                    if file { Some(id.as_str()) } else { parent_file },
                )? || source_block.metadata.node.as_ref().is_some_and(|node| node.more));
            let boundary = more && !within_file;
            if boundary {
                output.push(BufferBlock {
                    id: BlockId(format!("{id}:deferred-body")),
                    text: BufferText::from_rows([if boundary {
                        "  More content available"
                    } else {
                        ""
                    }])?,
                    metadata: BlockMetadata {
                        section: if boundary {
                            vec![DeferredSection {
                                id: original.id.clone(),
                                revision,
                                open: true,
                                more: true,
                            }]
                        } else {
                            Vec::new()
                        },
                        ..Default::default()
                    },
                });
            }
            let last = output.last().expect("section body");
            let mut fold = original.clone();
            fold.end = BlockAnchor {
                block: last.id.clone(),
                position: TextPosition {
                    row: last.text.row_count(),
                    column: 0,
                },
            };
            fold.closed = !expanded;
            if output[heading + 1..].iter().any(|block| block.text.row_count() > 0) { output[heading].metadata.fold.push(fold); }
            let loaded_rows = output[heading + 1..].iter().map(|block|block.text.row_count()).sum();
            let loaded_bytes = output[heading + 1..].iter().map(|block|block.text.byte_count()).sum();
            if let Some(node) = &mut output[heading].metadata.node {
                node.resolve(intent.get(id).copied());
                node.loaded_rows = loaded_rows;
                node.loaded_bytes = loaded_bytes;
                node.more = more;
                node.content_revision = revision;
            }
            output[heading].metadata.section.push(DeferredSection {
                id: original.id.clone(),
                revision,
                open: expanded,
                more: false,
            });
            if !within_file {
                budget.consume(&output[heading..heading + 1])?;
            } else {
                budget.consume(&output[heading..])?;
            }
            cursor = after;
            if more && within_file {
                return Ok(true);
            }
        } else {
            let block = prefix(source_block, &budget)?;
            let truncated = block.text != source_block.text;
            budget.consume(std::slice::from_ref(&block))?;
            output.push(block);
            if truncated {
                return Ok(true);
            }
            cursor += 1;
        }
    }
    Ok(false)
}

fn prefix(source: &BufferBlock, budget: &PageQuota) -> Result<BufferBlock> {
    if source.text.row_count() == 0 {
        return Ok(source.clone());
    }
    if budget.width.is_none() && source.text.byte_count() <= budget.bytes
        && source.text.row_count() <= budget.rows {
        return Ok(source.clone());
    }
    let mut rows = Vec::new();
    let mut remaining = budget.bytes;
    let mut remaining_rows = budget.rows;
    for index in 0..source.text.row_count() {
        if remaining == 0 || remaining_rows == 0 {
            break;
        }
        let row = source.text.row(index).expect("source row");
        let mut end = row.len().min(remaining.saturating_sub(1));
        while !row.is_char_boundary(end) {
            end -= 1;
        }
        if let Some(width) = &budget.width {
            let cells = width.cells(&row[..end], 0)?;
            remaining_rows = remaining_rows.saturating_sub(cells.div_ceil(width.columns).max(1));
        } else {
            remaining_rows = remaining_rows.saturating_sub(1);
        }
        rows.push(&row[..end]);
        remaining = remaining.saturating_sub(end + 1);
        if end != row.len() {
            break;
        }
    }
    if rows.is_empty() {
        rows.push("");
    }
    let complete_row = source.text.row(rows.len() - 1) == rows.last().copied();
    if rows.len() == source.text.row_count() && complete_row {
        return Ok(source.clone());
    }
    let end = if complete_row {
        TextPosition {
            row: rows.len(),
            column: 0,
        }
    } else {
        TextPosition {
            row: rows.len() - 1,
            column: rows.last().expect("prefix row").len(),
        }
    };
    let metadata = &source.metadata;
    Ok(BufferBlock {
        id: source.id.clone(),
        text: BufferText::from_rows(&rows)?,
        metadata: BlockMetadata {
            status: metadata.status.as_ref().filter(|status| status.row < rows.len()).cloned(),
            node: metadata.node.clone(),
            content_node: metadata.content_node.clone(),
            section: metadata.section.clone(),
            collapse: metadata.collapse.clone(),
            layout: metadata.layout.clone(),
            markdown: metadata.markdown,
            target: metadata.target.iter().filter(|item| item.range.end <= end).cloned().collect(),
            decoration: metadata.decoration.iter().filter(|item| item.range.end <= end).cloned().collect(),
            visible_decoration: metadata.visible_decoration.iter().filter(|item| item.range.end <= end).cloned().collect(),
            source_highlight: metadata.source_highlight.iter().filter(|item| item.range.end <= end).cloned().collect(),
            conceal: metadata.conceal.iter().filter(|item| item.range.end <= end).cloned().collect(),
            source_overlay: metadata.source_overlay.iter().filter(|item| item.range.end <= end).cloned().collect(),
            editable_region: metadata.editable_region.iter().filter(|item| item.range.end <= end).cloned().collect(),
            fold: metadata.fold.iter().filter(|item| item.end.block == source.id && item.end.position <= end).cloned().collect(),
            gutter: metadata.gutter.iter().filter(|item| item.position.row < rows.len() && item.position <= end).cloned().collect(),
        },
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use forge_buffer::block::FoldRange;
    use forge_buffer::identity::FoldId;

    fn owned_entry(source: &TranscriptDocument, id: &str) -> Result<TranscriptEntry> {
        let entry = source.source_entry(id)?;
        Ok(TranscriptEntry { id: entry.id.to_owned(), block: entry.block.into_iter().cloned().collect() })
    }

    #[test]
    fn empty_tool_blocks_do_not_add_rows_or_truncate_following_tools() -> Result<()> {
        let mut block = Vec::new();
        for (id, rows) in [
            ("group", vec!["Running 2 tools"]),
            ("first", vec!["first tool"]),
            ("first:output", vec!["first output", "", "more output"]),
            ("first:hidden", Vec::new()),
            ("second", vec!["second tool"]),
            ("second:output", vec!["second output"]),
            ("second:hidden", Vec::new()),
        ] {
            block.push(BufferBlock {
                id: BlockId(id.into()),
                text: BufferText::from_rows(rows)?,
                metadata: Default::default(),
            });
        }
        block[0].metadata.fold.push(FoldRange {
            id: FoldId("tools".into()),
            start: TextPosition { row: 0, column: 0 },
            end: BlockAnchor {
                block: BlockId("second:hidden".into()),
                position: TextPosition { row: 0, column: 0 },
            },
            closed: true,
            heading_start: None,
            collapsed_suffix: None,
            collapse_children: false,
            expand_children: false,
        });
        let expected: Vec<_> = block.iter().map(|block| block.text.clone()).collect();
        let mut source = TranscriptDocument::initialize(
            DocumentId("source".into()),
            "session".into(),
            0,
            vec![TranscriptEntry {
                id: "entry".into(),
                block,
            }],
        )?;
        let mut visible = SectionProjection::new(&mut source, DocumentId("visible".into()))?;
        let view = ViewId("view".into());
        visible.register(view.clone());
        for (sequence, expanded) in [(1, true), (2, false), (3, true)] {
            visible.set(
                &mut source,
                view.clone(),
                sequence,
                "tools",
                expanded,
                false,
                None,
            )?;
            visible.refresh(&mut source)?;
            let snapshot = visible.document.snapshot()?;
            if expanded {
                assert_eq!(
                    snapshot
                        .block
                        .iter()
                        .map(|block| block.text.clone())
                        .collect::<Vec<_>>(),
                    expected,
                );
                assert!(
                    snapshot.block.iter().all(|block| block
                        .metadata
                        .section
                        .iter()
                        .all(|section| !section.more))
                );
            }
        }
        Ok(())
    }

    fn source() -> Result<TranscriptDocument> {
        let mut heading = BufferBlock {
            id: BlockId("heading".into()),
            text: BufferText::from_rows(["Tools"])?,
            metadata: Default::default(),
        };
        let body = BufferBlock {
            id: BlockId("body".into()),
            text: BufferText::from_rows(["secret source body".repeat(8000)])?,
            metadata: Default::default(),
        };
        heading.metadata.fold.push(FoldRange {
            id: FoldId("tools".into()),
            start: TextPosition { row: 0, column: 0 },
            end: BlockAnchor {
                block: body.id.clone(),
                position: TextPosition { row: 1, column: 0 },
            },
            closed: true,
            heading_start: None,
            collapsed_suffix: None,
            collapse_children: false,
            expand_children: false,
        });
        TranscriptDocument::initialize(
            DocumentId("source".into()),
            "session".into(),
            0,
            vec![TranscriptEntry {
                id: "entry".into(),
                block: vec![heading, body],
            }],
        )
    }

    #[test]
    fn section_continues_loading_past_previous_body_limit() -> Result<()> {
        let mut blocks = source()?.snapshot()?.block;
        blocks[1].text = BufferText::from_rows(["x".repeat(16 * 1024 * 1024 + 2 * PAGE_BYTES)])?;
        let mut source = TranscriptDocument::initialize(
            DocumentId("large-source".into()), "session".into(), 0,
            vec![TranscriptEntry { id: "entry".into(), block: blocks }],
        )?;
        let mut visible = SectionProjection::new(&mut source, DocumentId("visible".into()))?;
        let view = ViewId("view".into());
        visible.register(view.clone());
        visible.quota_for_test("tools", 16 * 1024 * 1024);
        visible.set(&mut source, view.clone(), 1, "tools", true, false, None)?;
        visible.refresh(&mut source)?;
        let before = visible.document.document.block(&BlockId("body".into())).unwrap().text.byte_count();
        visible.set(&mut source, view, 2, "tools", true, true, None)?;
        visible.refresh(&mut source)?;
        let after = visible.document.document.block(&BlockId("body".into())).unwrap().text.byte_count();
        assert!(after > before && after > 16 * 1024 * 1024);
        assert!(visible.document.snapshot()?.block.iter().any(|block|
            block.metadata.section.iter().any(|section| section.id.0 == "tools" && section.more)));
        Ok(())
    }

    #[test]
    fn expansion_is_shared_and_closing_a_view_preserves_other_view_choices() -> Result<()> {
        let mut source = source()?;
        let mut visible = SectionProjection::new(&mut source, DocumentId("visible".into()))?;
        let first = ViewId("first".into());
        let second = ViewId("second".into());
        visible.register(first.clone());
        visible.register(second.clone());
        assert!(
            visible
                .document
                .document
                .block(&BlockId("body".into()))
                .is_none()
        );
        assert!(serde_json::to_vec(&visible.document.snapshot()?)?.len() < 2048);
        visible.set(&mut source, first.clone(), 1, "tools", true, false, None)?;
        visible.refresh(&mut source)?;
        assert!(
            visible
                .document
                .document
                .block(&BlockId("body".into()))
                .is_some()
        );
        assert!(
            visible
                .document
                .document
                .block(&BlockId("body".into()))
                .unwrap()
                .text
                .byte_count()
                <= PAGE_BYTES
        );
        visible.set(&mut source, second.clone(), 1, "tools", true, false, None)?;
        visible.set(&mut source, first.clone(), 2, "tools", false, false, None)?;
        visible.refresh(&mut source)?;
        assert!(
            visible
                .document
                .document
                .block(&BlockId("body".into()))
                .is_none()
        );
        visible.close(&mut source, &second);
        visible.refresh(&mut source)?;
        assert!(
            visible
                .document
                .document
                .block(&BlockId("body".into()))
                .is_none()
        );
        visible.set(&mut source, first.clone(), 1, "tools", true, false, None)?;
        assert!(visible.refresh(&mut source)?.is_empty());
        visible.close(&mut source, &first);
        assert!(
            visible
                .set(&mut source, first, 3, "tools", true, false, None)
                .is_err()
        );
        Ok(())
    }
    #[test]
    fn nested_bodies_stay_deferred_and_unicode_pages_retain_source() -> Result<()> {
        let initial = source()?;
        let mut block = initial.snapshot()?.block;
        block[1].text = BufferText::from_rows(["🦀".repeat(40000)])?;
        let mut outer = block[0].clone();
        outer.id = BlockId("outer".into());
        outer.metadata.fold[0].id = FoldId("outer-fold".into());
        block.insert(0, outer);
        let mut source = TranscriptDocument::initialize(
            DocumentId("source".into()),
            "session".into(),
            0,
            vec![TranscriptEntry {
                id: "entry".into(),
                block,
            }],
        )?;
        let mut visible = SectionProjection::new(&mut source, DocumentId("visible".into()))?;
        let view = ViewId("view".into());
        visible.register(view.clone());
        visible.set(
            &mut source,
            view.clone(),
            1,
            "outer-fold",
            true,
            false,
            None,
        )?;
        visible.refresh(&mut source)?;
        assert!(
            visible
                .document
                .document
                .block(&BlockId("heading".into()))
                .is_some()
        );
        assert!(
            visible
                .document
                .document
                .block(&BlockId("body".into()))
                .is_none()
        );
        visible.set(&mut source, view.clone(), 2, "tools", true, false, None)?;
        visible.refresh(&mut source)?;
        let assert_parent_endpoint = |visible: &SectionProjection| -> Result<()> {
            let snapshot = visible.document.snapshot()?;
            let parent = snapshot.block.iter().find(|block| block.id.0 == "outer").unwrap();
            let last = snapshot.block.last().unwrap();
            assert_eq!(parent.metadata.fold[0].end, BlockAnchor {
                block: last.id.clone(),
                position: TextPosition { row: last.text.row_count(), column: 0 },
            }, "expanding the final child must extend the enclosing exchange");
            Ok(())
        };
        assert_parent_endpoint(&visible)?;
        let first = visible
            .document
            .document
            .block(&BlockId("body".into()))
            .unwrap()
            .text
            .byte_count();
        assert!(first <= PAGE_BYTES);
        visible.set(&mut source, view.clone(), 3, "tools", true, true, None)?;
        visible.refresh(&mut source)?;
        assert_parent_endpoint(&visible)?;
        let second = visible
            .document
            .document
            .block(&BlockId("body".into()))
            .unwrap()
            .text
            .byte_count();
        assert!(second > first && second <= 2 * PAGE_BYTES);
        assert_eq!(
            source
                .document
                .block(&BlockId("body".into()))
                .unwrap()
                .text
                .row(0)
                .unwrap()
                .chars()
                .count(),
            40000
        );
        for (sequence, expanded) in [(4, false), (5, true)] {
            visible.set(&mut source, view.clone(), sequence, "tools", expanded, false, None)?;
            visible.refresh(&mut source)?;
            assert_parent_endpoint(&visible)?;
        }
        Ok(())
    }

    #[test]
    fn change_files_page_hunks_together_and_preserve_prefix_and_actions() -> Result<()> {
        use super::super::changes::ChangeTree;
        use super::super::transcript::TranscriptRenderer;
        let width = WidthProfile {
            columns: 80,
            ..WidthProfile::default()
        };
        let renderer = TranscriptRenderer::new(&width)?;
        let mut patch = String::from("--- a/first.rs\n+++ b/first.rs\n");
        for hunk in 0..20 {
            let line = hunk * 10 + 1;
            patch.push_str(&format!(
                "@@ -{line},2 +{line},2 @@\n-old_{hunk}\n+new_{hunk}\n context_{hunk}\n"
            ));
        }
        patch.push_str("--- a/second.rs\n+++ b/second.rs\n@@ -1 +1 @@\n-before\n+after\n");
        let tree = ChangeTree::render(&renderer, "changes", "Changes", "", &patch, None, None)?;
        let mut source = TranscriptDocument::initialize(
            DocumentId("source".into()),
            "session".into(),
            0,
            vec![TranscriptEntry {
                id: "entry".into(),
                block: tree.block,
            }],
        )?;
        let mut visible = SectionProjection::new(&mut source, DocumentId("visible".into()))?;
        let view = ViewId("view".into());
        visible.register(view.clone());
        visible.set(&mut source, view.clone(), 1, "changes", true, false, None)?;
        visible.set(
            &mut source,
            view.clone(),
            2,
            "changes:file:0",
            true,
            false,
            Some((12, width.clone())),
        )?;
        visible.refresh(&mut source)?;
        let first = visible.document.snapshot()?;
        first.validate()?;
        let first_body: Vec<_> = first
            .block
            .iter()
            .filter(|block| block.id.0.contains(":rows:"))
            .cloned()
            .collect();
        assert_eq!(first_body.len(), 3, "a page counts rows across small hunks");
        assert!(
            first
                .block
                .iter()
                .any(|block| block.id.0 == "changes:file:1")
        );
        assert!(
            !first
                .block
                .iter()
                .any(|block| block.id.0.starts_with("changes:file:1:hunk"))
        );
        let boundary: Vec<_> = first
            .block
            .iter()
            .flat_map(|block| &block.metadata.section)
            .filter(|section| section.more)
            .collect();
        assert_eq!(boundary.len(), 1);
        assert_eq!(boundary[0].id.0, "changes:file:0");
        assert!(
            first_body
                .iter()
                .all(|block| !block.metadata.target.is_empty())
        );
        let second = ViewId("second".into());
        visible.register(second.clone());
        visible.set(
            &mut source,
            second,
            1,
            "changes:file:0",
            true,
            false,
            Some((12, width.clone())),
        )?;
        visible.set(
            &mut source,
            view.clone(),
            3,
            "changes:file:0",
            true,
            true,
            Some((16, width.clone())),
        )?;
        visible.refresh(&mut source)?;
        let next = visible.document.snapshot()?;
        next.validate()?;
        for block in first_body {
            assert!(next.block.contains(&block), "loaded prefix changed");
        }
        assert!(
            next.block
                .iter()
                .filter(|block| block.id.0.contains(":rows:"))
                .count()
                > 3
        );
        visible.set(
            &mut source,
            view.clone(),
            4,
            "changes:file:0:hunk:0",
            false,
            false,
            None,
        )?;
        visible.refresh(&mut source)?;
        assert!(
            !visible
                .document
                .snapshot()?
                .block
                .iter()
                .any(|block| block.id.0 == "changes:file:0:hunk:0:rows:0"),
            "a window with the file closed retained a collapsed hunk body"
        );
        visible.set(
            &mut source,
            view.clone(),
            5,
            "changes:file:0",
            false,
            false,
            Some((12, width.clone())),
        )?;
        visible.set(
            &mut source,
            view.clone(),
            6,
            "changes:file:0",
            true,
            true,
            Some((12, width.clone())),
        )?;
        visible.refresh(&mut source)?;
        assert!(
            !visible
                .document
                .snapshot()?
                .block
                .iter()
                .any(|block| block.id.0.contains(":rows:"))
        );
        visible.set(
            &mut source,
            view,
            7,
            "changes:file:0",
            true,
            false,
            Some((12, width)),
        )?;
        visible.refresh(&mut source)?;
        let reopened = visible.document.snapshot()?;
        assert!(reopened.block.iter().all(|block| block.id.0 != "changes:file:0:hunk:0:rows:0"),
            "reopening a parent discarded the child's explicit closure");
        let reopened_count = reopened.block.iter().filter(|block| block.id.0.contains(":rows:")).count();
        assert!(reopened_count > 0 && reopened_count < 8, "reopening should restore the first page only");
        Ok(())
    }

    #[test]
    fn display_row_pages_split_large_hunks_without_losing_row_metadata() -> Result<()> {
        let mut initial = owned_entry(&source()?, "entry")?;
        initial.block[0].metadata.fold[0].expand_children = true;
        initial.block[1].text =
            BufferText::from_rows((0..100).map(|row| format!("{row:03} {}", "x".repeat(16))))?;
        let mut source = TranscriptDocument::initialize(
            DocumentId("source".into()),
            "session".into(),
            0,
            vec![initial],
        )?;
        let mut visible = SectionProjection::new(&mut source, DocumentId("visible".into()))?;
        let view = ViewId("view".into());
        visible.register(view.clone());
        let width = WidthProfile {
            columns: 10,
            ..WidthProfile::default()
        };
        visible.set(
            &mut source,
            view.clone(),
            1,
            "tools",
            true,
            false,
            Some((12, width.clone())),
        )?;
        visible.refresh(&mut source)?;
        let first = visible
            .document
            .snapshot()?
            .block
            .into_iter()
            .find(|block| block.id.0 == "body")
            .unwrap();
        assert_eq!(
            first.text.row_count(),
            6,
            "wrapped rows count against display budget"
        );
        visible.set(&mut source, view, 2, "tools", true, true, Some((8, width)))?;
        visible.refresh(&mut source)?;
        let next = visible
            .document
            .snapshot()?
            .block
            .into_iter()
            .find(|block| block.id.0 == "body")
            .unwrap();
        assert_eq!(next.text.row_count(), 10);
        assert_eq!(first.text.wire_rows(), next.text.wire_rows()[..6]);
        let narrow = WidthProfile {
            columns: 2,
            ..WidthProfile::default()
        };
        let view = ViewId("view".into());
        visible.set(
            &mut source,
            view,
            3,
            "tools",
            true,
            true,
            Some((20, narrow)),
        )?;
        visible.refresh(&mut source)?;
        let resized = visible
            .document
            .snapshot()?
            .block
            .into_iter()
            .find(|block| block.id.0 == "body")
            .unwrap();
        assert_eq!(
            resized.text.row_count(),
            12,
            "narrow resize dropped previously loaded rows"
        );
        assert_eq!(next.text.wire_rows(), resized.text.wire_rows()[..10]);
        Ok(())
    }

    #[test]
    fn reordering_existing_entries_removes_the_previous_identity_before_insertion() -> Result<()> {
        let mut source = source()?;
        let extra = TranscriptEntry {
            id: "extra".into(),
            block: vec![BufferBlock {
                id: BlockId("extra".into()),
                text: BufferText::from_rows(["Extra"])?,
                metadata: Default::default(),
            }],
        };
        let original = owned_entry(&source, "entry")?;
        source.replace_scope(vec![original, extra], 0)?;
        let mut visible = SectionProjection::new(&mut source, DocumentId("visible".into()))?;
        let original = owned_entry(&source, "entry")?;
        let extra = owned_entry(&source, "extra")?;
        source.replace_scope(vec![extra, original], 0)?;
        visible.refresh(&mut source)?;
        assert_eq!(visible.document.entry_ids(), ["extra", "entry"]);
        Ok(())
    }

    #[test]
    fn moving_a_section_between_retained_entries_releases_its_previous_blocks_first() -> Result<()>
    {
        let mut source = source()?;
        let extra = TranscriptEntry {
            id: "extra".into(),
            block: vec![BufferBlock {
                id: BlockId("extra".into()),
                text: BufferText::from_rows(["Extra"])?,
                metadata: Default::default(),
            }],
        };
        let original = owned_entry(&source, "entry")?;
        source.replace_scope(vec![extra, original], 0)?;
        let mut visible = SectionProjection::new(&mut source, DocumentId("visible".into()))?;
        let mut receiver = owned_entry(&source, "extra")?;
        let mut sender = owned_entry(&source, "entry")?;
        receiver.block.append(&mut sender.block);
        sender.block.push(BufferBlock {
            id: BlockId("remaining".into()),
            text: BufferText::from_rows(["Remaining"])?,
            metadata: Default::default(),
        });
        source = TranscriptDocument::initialize(
            DocumentId("source".into()),
            "session".into(),
            1,
            vec![receiver, sender],
        )?;
        visible.refresh(&mut source)?;
        assert_eq!(
            visible.owner.get("tools").map(String::as_str),
            Some("extra")
        );
        assert_eq!(visible.document.entry_ids(), ["extra", "entry"]);
        Ok(())
    }
}
