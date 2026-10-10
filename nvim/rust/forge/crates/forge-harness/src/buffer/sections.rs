use std::collections::{HashMap, HashSet};

use anyhow::{Context, Result, ensure};
use forge_buffer::block::{BlockAnchor, BlockMetadata, BufferBlock, DeferredSection, TextPosition};
use forge_buffer::identity::{BlockId, DocumentId, ViewId};
use forge_buffer::patch::BufferPatch;
use forge_buffer::text::BufferText;
use forge_buffer::width::WidthProfile;

use super::document::{TranscriptChange, TranscriptDocument, TranscriptEntry};

const PAGE_BYTES: usize = 64 * 1024;
const MAX_BODY_BYTES: usize = 16 * 1024 * 1024;

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
    view: HashMap<ViewId, (u64, HashMap<String, bool>)>,
    owner: HashMap<String, String>,
    file: HashMap<String, Vec<String>>,
    quota: HashMap<String, PageQuota>,
    entry_sections: HashMap<String, Vec<String>>,
}

impl SectionProjection {
    pub fn new(source: &mut TranscriptDocument, id: DocumentId) -> Result<Self> {
        let mut owner = HashMap::new();
        let mut file = HashMap::new();
        let mut entry_sections = HashMap::new();
        let mut entry = Vec::new();
        for identity in source.entry_ids() {
            let original = source.source_entry(&identity)?;
            entry_sections.insert(identity, index(&original, &mut owner, &mut file));
            entry.push(project(
                original,
                source.document.revision().0,
                &HashMap::new(),
                &HashMap::new(),
            )?);
        }
        source.dirty.clear();
        source.structure_dirty = false;
        Ok(Self {
            document: TranscriptDocument::initialize(id, "loaded-transcript".into(), 0, entry)?,
            view: HashMap::new(),
            owner,
            file,
            quota: HashMap::new(),
            entry_sections,
        })
    }

    pub fn refresh(&mut self, source: &mut TranscriptDocument) -> Result<Vec<BufferPatch>> {
        let dirty = std::mem::take(&mut source.dirty);
        let structural = std::mem::take(&mut source.structure_dirty);
        let mut projected = HashMap::new();
        for id in &dirty {
            if let Some(previous) = self.entry_sections.remove(id) {
                for section in previous {
                    if self.owner.get(&section) == Some(id) {
                        self.owner.remove(&section);
                        self.file.remove(&section);
                    }
                }
            }
        }
        for id in dirty {
            let original = source.source_entry(&id)?;
            self.entry_sections.insert(
                id.clone(),
                index(&original, &mut self.owner, &mut self.file),
            );
            projected.insert(
                id,
                project(
                    original,
                    self.document.document.revision().0 + 1,
                    &self.view,
                    &self.quota,
                )?,
            );
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
        Ok(patch)
    }

    #[cfg(test)]
    pub fn quota_for_test(&mut self, id: &str, bytes: usize) {
        self.quota.insert(id.into(), PageQuota::bytes(bytes));
    }

    pub fn register(&mut self, view: ViewId) {
        self.view.entry(view).or_default();
    }

    pub fn close(&mut self, source: &mut TranscriptDocument, view: &ViewId) {
        if let Some((_, intent)) = self.view.remove(view) {
            for id in intent.keys() {
                if let Some(owner) = self.owner.get(id) {
                    source.dirty.insert(owner.clone());
                }
            }
        }
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
        let owner = self
            .owner
            .get(id)
            .context("section is no longer available")?
            .clone();
        let children = self.file.get(id);
        let file = children.is_some();
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
        let loaded_rows = if more && file && resized {
            if let Some((_, width)) = &page {
                let loaded = self.document.source_entry(&owner)?;
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
                    for block in &loaded.block[start + 1..=end] {
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
        let (last, intent) = self.view.get_mut(&view).context("section view is closed")?;
        ensure!(
            sequence <= forge_buffer::MAX_COUNTER,
            "section sequence exhausted"
        );
        if sequence <= *last {
            return Ok(());
        }
        if more && (!expanded || intent.get(id) == Some(&false)) {
            return Ok(());
        }
        *last = sequence;
        if !more {
            intent.insert(id.into(), expanded);
            if expanded {
                for child in children.into_iter().flatten() {
                    intent.remove(child);
                }
            }
        }
        if expanded {
            let quota = self.quota.entry(id.into()).or_insert_with(|| {
                if file {
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
                if file && quota.width.is_some() {
                    let (rows, width) = page.unwrap_or((48, WidthProfile::default()));
                    ensure!(quota.rows < 1_048_576, "section exceeds loaded-row limit");
                    ensure!(
                        quota.bytes < MAX_BODY_BYTES,
                        "section exceeds the 16 MiB loaded-body limit"
                    );
                    quota.rows = quota.rows.max(loaded_rows).saturating_add(rows);
                    quota.bytes = (quota.bytes + PAGE_BYTES).min(MAX_BODY_BYTES);
                    quota.width = Some(width);
                } else {
                    ensure!(
                        quota.bytes < MAX_BODY_BYTES,
                        "section exceeds the 16 MiB loaded-body limit"
                    );
                    quota.bytes = (quota.bytes + PAGE_BYTES).min(MAX_BODY_BYTES);
                }
            }
        }
        source.dirty.insert(owner);
        Ok(())
    }
}

fn index(
    entry: &TranscriptEntry,
    owner: &mut HashMap<String, String>,
    file: &mut HashMap<String, Vec<String>>,
) -> Vec<String> {
    let mut sections = Vec::new();
    for (start, block) in entry.block.iter().enumerate() {
        for fold in &block.metadata.fold {
            owner.insert(fold.id.0.clone(), entry.id.clone());
            sections.push(fold.id.0.clone());
            if fold.expand_children {
                let end = entry
                    .block
                    .iter()
                    .position(|block| block.id == fold.end.block)
                    .unwrap_or(start);
                file.insert(
                    fold.id.0.clone(),
                    entry.block[start + 1..=end]
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
    project(entry, 0, &HashMap::new(), &HashMap::new())
}

fn project(
    entry: TranscriptEntry,
    revision: u64,
    view: &HashMap<ViewId, (u64, HashMap<String, bool>)>,
    quota: &HashMap<String, PageQuota>,
) -> Result<TranscriptEntry> {
    let position: HashMap<_, _> = entry
        .block
        .iter()
        .enumerate()
        .map(|(index, block)| (block.id.clone(), index))
        .collect();
    let mut output = Vec::new();
    render(
        &entry.block,
        0,
        entry.block.len(),
        revision,
        view,
        quota,
        &position,
        &mut output,
        PageQuota::bytes(usize::MAX),
        false,
        None,
    )?;
    Ok(TranscriptEntry {
        id: entry.id,
        block: output,
    })
}

fn render(
    source: &[BufferBlock],
    mut cursor: usize,
    end: usize,
    revision: u64,
    view: &HashMap<ViewId, (u64, HashMap<String, bool>)>,
    quota: &HashMap<String, PageQuota>,
    position: &HashMap<BlockId, usize>,
    output: &mut Vec<BufferBlock>,
    mut budget: PageQuota,
    within_file: bool,
    parent_file: Option<&str>,
) -> Result<bool> {
    while cursor < end {
        if budget.exhausted() {
            return Ok(true);
        }
        let source_block = &source[cursor];
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
            let expanded = if view.is_empty() {
                !original.closed
            } else {
                view.values().any(|(_, intent)| {
                    let inherited = parent_file.is_some_and(|file| intent.get(file) == Some(&true));
                    intent
                        .get(id)
                        .copied()
                        .unwrap_or(inherited || !original.closed)
                })
            };
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
                    .unwrap_or_else(|| PageQuota::bytes(PAGE_BYTES))
            };
            let more = expanded
                && render(
                    source,
                    cursor + 1,
                    after,
                    revision,
                    view,
                    quota,
                    position,
                    output,
                    child_budget,
                    within_file || file,
                    if file { Some(id.as_str()) } else { parent_file },
                )?;
            if file && more {
                let bytes: usize = output[heading + 1..]
                    .iter()
                    .map(|block| block.text.byte_count())
                    .sum();
                ensure!(
                    bytes < MAX_BODY_BYTES - 4,
                    "section exceeds the 16 MiB loaded-body limit"
                );
            }
            let boundary = more && !within_file;
            if !expanded || boundary || output.len() == heading + 1 {
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
            if within_file {
                fold.closed = !expanded;
            }
            output[heading].metadata.fold.push(fold);
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
    let text = BufferText::from_rows(&rows)?;
    if text == source.text {
        return Ok(source.clone());
    }
    let complete_row = source.text.row(rows.len() - 1) == rows.last().copied();
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
    let mut block = source.clone();
    block.text = text;
    block.metadata.target.retain(|item| item.range.end <= end);
    block
        .metadata
        .decoration
        .retain(|item| item.range.end <= end);
    block
        .metadata
        .visible_decoration
        .retain(|item| item.range.end <= end);
    block
        .metadata
        .source_highlight
        .retain(|item| item.range.end <= end);
    block.metadata.conceal.retain(|item| item.range.end <= end);
    block
        .metadata
        .source_overlay
        .retain(|item| item.range.end <= end);
    block
        .metadata
        .editable_region
        .retain(|item| item.range.end <= end);
    block
        .metadata
        .gutter
        .retain(|item| item.position.row < block.text.row_count() && item.position <= end);
    block
        .metadata
        .fold
        .retain(|item| item.end.block == block.id && item.end.position <= end);
    Ok(block)
}

#[cfg(test)]
mod tests {
    use super::*;
    use forge_buffer::block::FoldRange;
    use forge_buffer::identity::FoldId;

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
    fn closed_content_is_absent_and_window_demand_is_a_union() -> Result<()> {
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
                .is_some()
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
        let first = visible
            .document
            .document
            .block(&BlockId("body".into()))
            .unwrap()
            .text
            .byte_count();
        assert!(first <= PAGE_BYTES);
        visible.set(&mut source, view, 3, "tools", true, true, None)?;
        visible.refresh(&mut source)?;
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
            false,
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
        assert_eq!(
            visible
                .document
                .snapshot()?
                .block
                .iter()
                .map(|block| (&block.id, &block.text))
                .collect::<Vec<_>>(),
            next.block
                .iter()
                .map(|block| (&block.id, &block.text))
                .collect::<Vec<_>>(),
            "reopening retains the loaded quota"
        );
        Ok(())
    }

    #[test]
    fn display_row_pages_split_large_hunks_without_losing_row_metadata() -> Result<()> {
        let mut initial = source()?.source_entry("entry")?;
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
        let original = source.source_entry("entry")?;
        source.replace_scope(vec![original, extra], 0)?;
        let mut visible = SectionProjection::new(&mut source, DocumentId("visible".into()))?;
        let original = source.source_entry("entry")?;
        let extra = source.source_entry("extra")?;
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
        let original = source.source_entry("entry")?;
        source.replace_scope(vec![extra, original], 0)?;
        let mut visible = SectionProjection::new(&mut source, DocumentId("visible".into()))?;
        let mut receiver = source.source_entry("extra")?;
        let mut sender = source.source_entry("entry")?;
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
