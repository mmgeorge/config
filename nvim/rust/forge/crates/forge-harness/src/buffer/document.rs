use std::collections::HashSet;
use std::ops::Range;

use anyhow::{Context, Result, ensure};
use forge_buffer::block::BufferBlock;
use forge_buffer::document::BufferDocument;
use forge_buffer::identity::{BlockId, DocumentId};
use forge_buffer::patch::{BufferPatch, BufferSnapshot};
use forge_buffer::view::DocumentViews;

const MAX_DOCUMENT_BYTES: usize = 32 * 1024 * 1024;
const MAX_DOCUMENT_BLOCKS: usize = 65_536;

pub struct TranscriptEntry {
    pub id: String,
    pub block: Vec<BufferBlock>,
}

pub enum TranscriptChange {
    Insert {
        index: usize,
        entry: TranscriptEntry,
    },
    Replace {
        index: usize,
        entry: TranscriptEntry,
    },
    Remove {
        index: usize,
        id: String,
    },
}

struct EntryPosition {
    id: String,
    first: BlockId,
    blocks: usize,
}

pub struct HarnessDocument {
    session_id: String,
    timeline_revision: u64,
    pub(super) document: BufferDocument,
    entry: Vec<EntryPosition>,
    synchronized: bool,
    pub views: DocumentViews,
}

impl HarnessDocument {
    pub fn initialize(
        document_id: DocumentId,
        session_id: String,
        timeline_revision: u64,
        entry: Vec<TranscriptEntry>,
    ) -> Result<Self> {
        ensure!(
            !session_id.is_empty() && session_id.len() <= 128,
            "invalid transcript session identity"
        );
        ensure!(
            timeline_revision <= forge_buffer::MAX_COUNTER,
            "invalid timeline revision"
        );
        let (position, block) = prepare_entries(entry)?;
        Ok(Self {
            document: BufferDocument::new(document_id, block)?,
            session_id,
            timeline_revision,
            entry: position,
            synchronized: true,
            views: DocumentViews::default(),
        })
    }

    pub fn snapshot(&self) -> Result<BufferSnapshot> {
        ensure!(
            self.synchronized,
            "transcript requires a new canonical snapshot"
        );
        Ok(self.document.snapshot())
    }

    pub fn apply_layout(&mut self, entry: Vec<TranscriptEntry>) -> Result<Vec<BufferPatch>> {
        ensure!(
            self.synchronized && entry.len() == self.entry.len(),
            "layout source differs from canonical timeline"
        );
        for (current, replacement) in self.entry.iter().zip(&entry) {
            ensure!(
                current.id == replacement.id,
                "layout changed a timeline entry identity"
            );
            validate_entry(replacement)?;
        }
        let mut patch = Vec::new();
        for (index, entry) in entry.into_iter().enumerate() {
            match self.apply_change(TranscriptChange::Replace { index, entry }) {
                Ok(Some(applied)) => patch.push(applied),
                Ok(None) => {}
                Err(failure) => {
                    self.synchronized = false;
                    return Err(failure);
                }
            }
        }
        Ok(patch)
    }

    pub fn replace_scope(
        &mut self,
        entry: Vec<TranscriptEntry>,
        timeline_revision: u64,
    ) -> Result<Vec<BufferPatch>> {
        ensure!(
            self.synchronized && timeline_revision <= forge_buffer::MAX_COUNTER,
            "invalid transcript scope revision"
        );
        let patch = if entry.len() == self.entry.len()
            && self
                .entry
                .iter()
                .zip(&entry)
                .all(|(current, replacement)| current.id == replacement.id)
        {
            self.apply_layout(entry)?
        } else {
            let (position, block) = prepare_entries(entry)?;
            let patch = self.document.edit(0..self.document.block_count(), block)?;
            self.entry = position;
            patch.into_iter().collect()
        };
        self.timeline_revision = timeline_revision;
        Ok(patch)
    }

    /// Applies one ordered provider event, touching only its changed entry ranges.
    pub fn apply_event(
        &mut self,
        session_id: &str,
        base: u64,
        next: u64,
        changes: Vec<TranscriptChange>,
    ) -> Result<Vec<BufferPatch>> {
        ensure!(
            self.synchronized && session_id == self.session_id,
            "transcript event belongs to another session lifetime"
        );
        ensure!(
            base == self.timeline_revision
                && next == base.checked_add(1).context("timeline revision exhausted")?
                && next <= forge_buffer::MAX_COUNTER,
            "transcript event revision gap"
        );
        let result = changes
            .into_iter()
            .try_fold(Vec::new(), |mut patch, change| {
                if let Some(applied) = self.apply_change(change)? {
                    patch.push(applied);
                }
                Ok::<_, anyhow::Error>(patch)
            });
        match result {
            Ok(patch) => {
                self.timeline_revision = next;
                Ok(patch)
            }
            Err(error) => {
                self.synchronized = false;
                Err(error)
            }
        }
    }

    /// Replaces the active block without traversing unrelated transcript entries.
    pub fn append(&mut self, block: BufferBlock) -> Result<Option<BufferPatch>> {
        ensure!(
            self.synchronized,
            "transcript requires a new canonical snapshot"
        );
        let index = self
            .document
            .block_index(&block.id)
            .context("unknown active transcript block")?;
        self.check_capacity(index..index + 1, std::slice::from_ref(&block))?;
        Ok(self.document.edit(index..index + 1, vec![block])?)
    }

    fn apply_change(&mut self, change: TranscriptChange) -> Result<Option<BufferPatch>> {
        match change {
            TranscriptChange::Insert { index, entry } => {
                validate_entry(&entry)?;
                ensure!(
                    index <= self.entry.len()
                        && !self.entry.iter().any(|existing| existing.id == entry.id),
                    "invalid transcript insertion"
                );
                let start = if index == self.entry.len() {
                    self.document.block_count()
                } else {
                    self.entry_range(index)?.start
                };
                self.check_capacity(start..start, &entry.block)?;
                let position = EntryPosition {
                    id: entry.id,
                    first: entry.block[0].id.clone(),
                    blocks: entry.block.len(),
                };
                let patch = self.document.edit(start..start, entry.block)?;
                self.entry.insert(index, position);
                Ok(patch)
            }
            TranscriptChange::Replace { index, entry } => {
                validate_entry(&entry)?;
                ensure!(
                    self.entry
                        .get(index)
                        .is_some_and(|existing| existing.id == entry.id),
                    "transcript replacement identity differs"
                );
                let range = self.entry_range(index)?;
                self.check_capacity(range.clone(), &entry.block)?;
                let mut prefix = 0;
                let mut suffix = 0;
                {
                    let previous = self
                        .document
                        .blocks(self.document.revision(), range.clone())?
                        .collect::<Vec<_>>();
                    while prefix < previous.len().min(entry.block.len())
                        && previous[prefix] == &entry.block[prefix]
                    {
                        prefix += 1;
                    }
                    while suffix < (previous.len() - prefix).min(entry.block.len() - prefix)
                        && previous[previous.len() - suffix - 1]
                            == &entry.block[entry.block.len() - suffix - 1]
                    {
                        suffix += 1;
                    }
                }
                let position = EntryPosition {
                    id: entry.id,
                    first: entry.block[0].id.clone(),
                    blocks: entry.block.len(),
                };
                let end = entry.block.len() - suffix;
                let changed = entry
                    .block
                    .into_iter()
                    .skip(prefix)
                    .take(end - prefix)
                    .collect();
                let patch = self
                    .document
                    .edit(range.start + prefix..range.end - suffix, changed)?;
                self.entry[index] = position;
                Ok(patch)
            }
            TranscriptChange::Remove { index, id } => {
                ensure!(
                    self.entry.get(index).is_some_and(|entry| entry.id == id),
                    "transcript removal identity differs"
                );
                let range = self.entry_range(index)?;
                let patch = self.document.edit(range, Vec::new())?;
                self.entry.remove(index);
                Ok(patch)
            }
        }
    }

    fn entry_range(&self, index: usize) -> Result<Range<usize>> {
        let entry = self.entry.get(index).context("unknown transcript entry")?;
        let start = self
            .document
            .block_index(&entry.first)
            .context("transcript entry lost its first block")?;
        Ok(start..start + entry.blocks)
    }

    fn check_capacity(&self, range: Range<usize>, block: &[BufferBlock]) -> Result<()> {
        let removed = self
            .document
            .blocks(self.document.revision(), range.clone())?
            .map(|block| block.text.byte_count())
            .sum::<usize>();
        let inserted = block
            .iter()
            .map(|block| block.text.byte_count())
            .sum::<usize>();
        ensure!(
            self.document.text_bytes() - removed + inserted <= MAX_DOCUMENT_BYTES
                && self.document.block_count() - range.len() + block.len() <= MAX_DOCUMENT_BLOCKS,
            "transcript exceeds native document capacity"
        );
        Ok(())
    }
}

fn prepare_entries(entry: Vec<TranscriptEntry>) -> Result<(Vec<EntryPosition>, Vec<BufferBlock>)> {
    let mut identities = HashSet::new();
    let mut position = Vec::new();
    let mut block = Vec::new();
    let mut bytes = 0;
    for entry in entry {
        validate_entry(&entry)?;
        ensure!(
            identities.insert(entry.id.clone()),
            "duplicate transcript entry identity"
        );
        bytes += entry
            .block
            .iter()
            .map(|block| block.text.byte_count())
            .sum::<usize>();
        ensure!(
            bytes <= MAX_DOCUMENT_BYTES && block.len() + entry.block.len() <= MAX_DOCUMENT_BLOCKS,
            "transcript exceeds native document capacity"
        );
        position.push(EntryPosition {
            id: entry.id,
            first: entry.block[0].id.clone(),
            blocks: entry.block.len(),
        });
        block.extend(entry.block);
    }
    Ok((position, block))
}

fn validate_entry(entry: &TranscriptEntry) -> Result<()> {
    ensure!(
        !entry.id.is_empty() && entry.id.len() <= 256 && !entry.block.is_empty(),
        "invalid transcript entry"
    );
    for block in &entry.block {
        block.validate()?;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use forge_buffer::block::BlockMetadata;
    use forge_buffer::text::BufferText;

    fn block(id: &str, text: &str) -> BufferBlock {
        BufferBlock {
            id: BlockId(id.into()),
            text: BufferText::from_rows([text]).unwrap(),
            metadata: BlockMetadata::default(),
        }
    }

    #[test]
    fn one_active_replacement_keeps_settled_blocks_out_of_the_patch() {
        let mut entry = (0..1000)
            .map(|index| TranscriptEntry {
                id: format!("entry-{index}"),
                block: vec![block(&format!("block-{index}"), "settled")],
            })
            .collect::<Vec<_>>();
        entry.push(TranscriptEntry {
            id: "active".into(),
            block: vec![block("prompt", "> prompt"), block("thought", "first")],
        });
        let mut document = HarnessDocument::initialize(
            DocumentId("harness:session".into()),
            "session".into(),
            1,
            entry,
        )
        .unwrap();
        let patch = document
            .apply_event(
                "session",
                1,
                2,
                vec![TranscriptChange::Replace {
                    index: 1000,
                    entry: TranscriptEntry {
                        id: "active".into(),
                        block: vec![block("prompt", "> prompt"), block("thought", "second")],
                    },
                }],
            )
            .unwrap();
        assert_eq!(patch.len(), 1);
        assert_eq!(patch[0].text_edit.len(), 1);
        assert_eq!(patch[0].text_edit[0].start_row, 1001);
        assert_eq!(patch[0].metadata_edit.len(), 1);
        assert_eq!(patch[0].metadata_edit[0].block.0, "thought");
    }

    #[test]
    fn invalid_event_cannot_publish_a_partial_snapshot_or_accept_later_events() {
        let mut document = HarnessDocument::initialize(
            DocumentId("harness:session".into()),
            "session".into(),
            1,
            Vec::new(),
        )
        .unwrap();
        assert!(
            document
                .apply_event(
                    "session",
                    1,
                    2,
                    vec![
                        TranscriptChange::Insert {
                            index: 0,
                            entry: TranscriptEntry {
                                id: "entry".into(),
                                block: vec![block("prompt", "> prompt")]
                            },
                        },
                        TranscriptChange::Remove {
                            index: 2,
                            id: "unknown".into()
                        }
                    ]
                )
                .is_err()
        );
        assert!(document.snapshot().is_err());
        assert!(document.append(block("prompt", "late")).is_err());
    }
}
