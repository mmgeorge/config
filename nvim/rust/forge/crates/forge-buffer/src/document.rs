//! Revision publication follows complete validation of the candidate document.

use std::collections::HashMap;
use std::ops::Range;

use crate::ContractError;
use crate::block::BufferBlock;
use crate::editable::{
    EditAcknowledgement, LocalEdit, LocalEditResult, rebase_fold_endpoint, replace_region,
};
use crate::identity::{BlockId, DocumentId, DocumentRevision, EditSequence, RegionId};
use crate::patch::{BlockOrderEdit, BufferPatch, BufferSnapshot, MetadataEdit, TextEdit};
use crate::sequence::{BlockSequence, SequenceEdit};
use crate::text::BufferText;

#[derive(Debug, Clone)]
pub struct BufferDocument {
    id: DocumentId,
    revision: DocumentRevision,
    sequence: BlockSequence,
    accepted_edit: HashMap<RegionId, EditSequence>,
}

/// A validated edit retains its body and affected fold owners and exclusively borrows its document.
/// Dropping the preparation leaves text, revisions, and accepted sequences unchanged.
pub struct PreparedLocalEdit<'document> {
    replacement: PreparedDocumentEdit<'document>,
    acknowledgement: EditAcknowledgement,
}

pub struct PreparedDocumentEdit<'document> {
    document: &'document mut BufferDocument,
    edits: Vec<SequenceEdit>,
    affected_region: Vec<RegionId>,
    next_rows: usize,
    next_bytes: usize,
    patch: BufferPatch,
}
impl PreparedDocumentEdit<'_> {
    pub fn commit(self) -> BufferPatch {
        self.document
            .sequence
            .apply_validated_edits(self.edits, self.next_rows, self.next_bytes);
        for region in self.affected_region {
            if self.document.sequence.region_block(&region).is_none() {
                self.document.accepted_edit.remove(&region);
            }
        }
        self.document.revision = self.patch.next;
        self.patch
    }
}

pub enum LocalEditPreparation<'document> {
    Prepared(Box<PreparedLocalEdit<'document>>),
    Rejected(LocalEditResult),
}

impl PreparedLocalEdit<'_> {
    /// Publishes the validated replacement after the caller admits its canonical edit.
    /// The exclusive borrow prevents the document from changing between validation and commit.
    pub fn commit(self) -> LocalEditResult {
        self.replacement.document.accepted_edit.insert(
            self.acknowledgement.region.clone(),
            self.acknowledgement.sequence,
        );
        LocalEditResult::Accepted {
            acknowledgement: self.acknowledgement,
            patch: Box::new(self.replacement.commit()),
        }
    }
}

impl BufferDocument {
    pub fn new(id: DocumentId, block: Vec<BufferBlock>) -> Result<Self, ContractError> {
        id.validate()?;
        Ok(Self {
            id,
            revision: DocumentRevision(0),
            sequence: BlockSequence::new(block)?,
            accepted_edit: HashMap::new(),
        })
    }

    pub fn snapshot(&self) -> BufferSnapshot {
        BufferSnapshot {
            document: self.id.clone(),
            revision: self.revision,
            block: self.sequence.blocks().cloned().collect(),
        }
    }

    pub fn locate(
        &self,
        revision: DocumentRevision,
        row: usize,
    ) -> Result<(&BufferBlock, usize), ContractError> {
        if revision != self.revision {
            return Err(ContractError("position belongs to a stale revision"));
        }
        self.sequence
            .locate(row)
            .ok_or(ContractError("row is outside document"))
    }

    pub fn block_count(&self) -> usize {
        self.sequence.len()
    }

    pub fn revision(&self) -> DocumentRevision {
        self.revision
    }

    pub fn block_index(&self, id: &BlockId) -> Option<usize> {
        self.sequence.index(id)
    }

    pub fn block(&self, id: &BlockId) -> Option<&BufferBlock> {
        let index = self.sequence.index(id)?;
        self.sequence.range(index..index + 1).ok()?.next()
    }

    pub fn blocks(
        &self,
        revision: DocumentRevision,
        range: Range<usize>,
    ) -> Result<crate::sequence::BlockIter<'_>, ContractError> {
        if revision != self.revision {
            return Err(ContractError("block selection belongs to a stale revision"));
        }
        self.sequence.range(range)
    }

    pub fn text_bytes(&self) -> usize {
        self.sequence.byte_count()
    }

    /// Returns the exact physical row count without materializing document blocks.
    pub fn row_count(&self) -> usize {
        self.sequence.row_count()
    }

    /// Accepts exact region text without requiring a matching layout revision.
    /// Conflicts and stale sequences leave text, metadata, and both revisions unchanged.
    pub fn accept_local_edit(&mut self, edit: LocalEdit) -> Result<LocalEditResult, ContractError> {
        Ok(match self.prepare_local_edit(edit)? {
            LocalEditPreparation::Prepared(prepared) => prepared.commit(),
            LocalEditPreparation::Rejected(result) => result,
        })
    }

    /// Validates one region replacement without publishing text or acknowledgement state.
    /// A prepared edit holds the document exclusively until committed or dropped.
    pub fn prepare_local_edit(
        &mut self,
        edit: LocalEdit,
    ) -> Result<LocalEditPreparation<'_>, ContractError> {
        edit.document.validate()?;
        edit.region.validate()?;
        edit.base.validate()?;
        edit.sequence.validate()?;
        if edit.document != self.id || edit.sequence.0 == 0 {
            return Err(ContractError("local edit has invalid document or sequence"));
        }
        let Some(block) = self.sequence.region_block(&edit.region) else {
            return Ok(LocalEditPreparation::Rejected(
                LocalEditResult::UnknownRegion,
            ));
        };
        if self
            .accepted_edit
            .get(&edit.region)
            .is_some_and(|accepted| edit.sequence <= *accepted)
        {
            return Ok(LocalEditPreparation::Rejected(
                LocalEditResult::StaleSequence,
            ));
        }
        let region_index = block
            .metadata
            .editable_region
            .iter()
            .position(|region| region.id == edit.region)
            .expect("indexed region");
        let region = &block.metadata.editable_region[region_index];
        if edit.sequence <= region.sequence {
            return Ok(LocalEditPreparation::Rejected(
                LocalEditResult::StaleSequence,
            ));
        }
        if edit.base != region.revision {
            return Ok(LocalEditPreparation::Rejected(LocalEditResult::Conflict {
                current: region.revision,
            }));
        }
        let acknowledgement = EditAcknowledgement {
            document: self.id.clone(),
            region: edit.region.clone(),
            sequence: edit.sequence,
            revision: region.revision.next()?,
        };
        let position = self.sequence.index(&block.id).expect("indexed block");
        let mut replacement_block = replace_region(block.clone(), region_index, &edit.text)?;
        replacement_block.metadata.editable_region[region_index].sequence = edit.sequence;
        let inserted_end = replacement_block.metadata.editable_region[region_index]
            .range
            .end;
        let mut edits = Vec::new();
        for owner in self.sequence.fold_endpoint_owner(&block.id) {
            if owner.id == block.id {
                continue;
            }
            let mut replacement = owner.clone();
            for fold in &mut replacement.metadata.fold {
                if fold.end.block == block.id {
                    rebase_fold_endpoint(&mut fold.end.position, &region.range, inserted_end);
                }
            }
            if replacement.metadata != owner.metadata {
                let position = self.sequence.index(&owner.id).expect("indexed fold owner");
                edits.push(SequenceEdit {
                    range: position..position + 1,
                    block: vec![replacement],
                });
            }
        }
        edits.push(SequenceEdit {
            range: position..position + 1,
            block: vec![replacement_block],
        });
        let replacement = self
            .prepare_edits(edits)?
            .expect("region revision advanced");
        Ok(LocalEditPreparation::Prepared(Box::new(
            PreparedLocalEdit {
                replacement,
                acknowledgement,
            },
        )))
    }

    pub fn edit(
        &mut self,
        range: Range<usize>,
        block: Vec<BufferBlock>,
    ) -> Result<Option<BufferPatch>, ContractError> {
        self.edit_many(vec![SequenceEdit { range, block }])
    }

    /// Publishes disjoint block changes against one exact base revision.
    /// All ranges and combined identities are validated before any mutation.
    pub fn edit_many(
        &mut self,
        edits: Vec<SequenceEdit>,
    ) -> Result<Option<BufferPatch>, ContractError> {
        Ok(self.prepare_edits(edits)?.map(PreparedDocumentEdit::commit))
    }

    pub fn prepare_edits(
        &mut self,
        mut edits: Vec<SequenceEdit>,
    ) -> Result<Option<PreparedDocumentEdit<'_>>, ContractError> {
        edits.sort_by_key(|edit| std::cmp::Reverse(edit.range.start));
        let (next_rows, next_bytes) = self.sequence.validate_edits(&edits)?;
        let mut result: Option<BufferPatch> = None;
        for edit in &edits {
            let Some(mut patch) = self.prepare_edit(edit.range.clone(), &edit.block)? else {
                continue;
            };
            if let Some(result) = &mut result {
                result.next_rows = result
                    .next_rows
                    .checked_sub(patch.base_rows.saturating_sub(patch.next_rows))
                    .and_then(|rows| {
                        rows.checked_add(patch.next_rows.saturating_sub(patch.base_rows))
                    })
                    .ok_or(ContractError("transaction row count overflow"))?;
                result.next_blocks = result
                    .next_blocks
                    .checked_sub(patch.base_blocks.saturating_sub(patch.next_blocks))
                    .and_then(|count| {
                        count.checked_add(patch.next_blocks.saturating_sub(patch.base_blocks))
                    })
                    .ok_or(ContractError("transaction block count overflow"))?;
                result.text_edit.append(&mut patch.text_edit);
                result.block_edit.append(&mut patch.block_edit);
                result.metadata_edit.append(&mut patch.metadata_edit);
                result.removed_block.append(&mut patch.removed_block);
            } else {
                result = Some(patch);
            }
        }
        let Some(mut patch) = result else {
            return Ok(None);
        };
        let incoming: std::collections::HashSet<_> = edits
            .iter()
            .flat_map(|edit| edit.block.iter().map(|block| &block.id))
            .collect();
        patch.removed_block.retain(|id| !incoming.contains(id));
        let mut pending = patch.text_edit.into_iter().peekable();
        let mut text_edit = Vec::new();
        while let Some(first) = pending.next() {
            let mut start_row = first.start_row;
            let mut removed_rows = first.removed_rows;
            let mut adjacent = vec![first];
            while pending
                .peek()
                .is_some_and(|edit| edit.start_row + edit.removed_rows == start_row)
            {
                let lower = pending.next().expect("peeked edit");
                start_row = lower.start_row;
                removed_rows += lower.removed_rows;
                adjacent.push(lower);
            }
            if adjacent.len() == 1 {
                text_edit.push(adjacent.pop().expect("first edit"));
            } else {
                text_edit.push(TextEdit {
                    start_row,
                    removed_rows,
                    text: BufferText::from_rows(
                        adjacent.iter().rev().flat_map(|edit| edit.text.wire_rows()),
                    )?,
                });
            }
        }
        patch.text_edit = text_edit;
        patch.validate()?;
        let affected_region: Vec<_> = edits
            .iter()
            .flat_map(|edit| {
                self.sequence
                    .range(edit.range.clone())
                    .expect("validated range")
                    .flat_map(|block| {
                        block
                            .metadata
                            .editable_region
                            .iter()
                            .map(|region| region.id.clone())
                    })
            })
            .collect();
        Ok(Some(PreparedDocumentEdit {
            document: self,
            edits,
            affected_region,
            next_rows,
            next_bytes,
            patch,
        }))
    }

    fn prepare_edit(
        &self,
        range: Range<usize>,
        block: &[BufferBlock],
    ) -> Result<Option<BufferPatch>, ContractError> {
        let old: Vec<_> = self.sequence.range(range.clone())?.collect();
        if old.iter().copied().eq(block.iter()) {
            return Ok(None);
        }
        let start_row = if range.start == self.sequence.len() {
            self.sequence.row_count()
        } else {
            let first = self
                .sequence
                .range(range.start..range.start + 1)?
                .next()
                .expect("existing block");
            self.sequence.position(&first.id).expect("indexed block")
        };
        let next = self.revision.next()?;
        let old_rows: Vec<&str> = old
            .iter()
            .flat_map(|block| block.text.wire_rows())
            .collect();
        let new_rows: Vec<&str> = block
            .iter()
            .flat_map(|block| block.text.wire_rows())
            .collect();
        let prefix = old_rows
            .iter()
            .zip(&new_rows)
            .take_while(|(old, new)| old == new)
            .count();
        let suffix = old_rows[prefix..]
            .iter()
            .rev()
            .zip(new_rows[prefix..].iter().rev())
            .take_while(|(old, new)| old == new)
            .count();
        let text_edit = if old_rows == new_rows {
            Vec::new()
        } else {
            vec![TextEdit {
                start_row: start_row + prefix,
                removed_rows: old_rows.len() - prefix - suffix,
                text: BufferText::from_rows(new_rows[prefix..new_rows.len() - suffix].iter())?,
            }]
        };
        let old_block: HashMap<_, _> = old.iter().map(|block| (&block.id, *block)).collect();
        let metadata_edit = block
            .iter()
            .filter(|block| {
                old_block
                    .get(&block.id)
                    .is_none_or(|old| old.metadata != block.metadata || old.text != block.text)
            })
            .map(|block| MetadataEdit {
                block: block.id.clone(),
                row_count: block.text.row_count(),
                metadata: block.metadata.clone(),
            })
            .collect();
        let retained: std::collections::HashSet<_> = block.iter().map(|block| &block.id).collect();
        let patch = BufferPatch {
            document: self.id.clone(),
            base: self.revision,
            next,
            base_rows: self.sequence.row_count(),
            next_rows: self
                .sequence
                .row_count()
                .checked_sub(old_rows.len())
                .and_then(|rows| rows.checked_add(new_rows.len()))
                .ok_or(ContractError("document row count overflow"))?,
            base_blocks: self.sequence.len(),
            next_blocks: self.sequence.len() - old.len() + block.len(),
            text_edit,
            metadata_edit,
            removed_block: old
                .iter()
                .filter(|block| !retained.contains(&block.id))
                .map(|block| block.id.clone())
                .collect(),
            block_edit: if old
                .iter()
                .map(|block| &block.id)
                .eq(block.iter().map(|block| &block.id))
            {
                Vec::new()
            } else {
                vec![BlockOrderEdit {
                    start_block: range.start,
                    removed_blocks: old.len(),
                    inserted: block.iter().map(|block| block.id.clone()).collect(),
                }]
            },
        };
        Ok(Some(patch))
    }
}

#[cfg(test)]
mod prepared_document_tests {
    use super::*;
    use crate::block::BlockMetadata;
    #[test]
    fn generated_preparation_aborts_then_commits_one_revision() {
        let block = |name: &str, text: &str| BufferBlock {
            id: BlockId(name.into()),
            text: BufferText::from_rows([text]).unwrap(),
            metadata: BlockMetadata::default(),
        };
        let mut document = BufferDocument::new(
            DocumentId("prepared-generated".into()),
            vec![block("first", "old"), block("last", "retained")],
        )
        .unwrap();
        let before = serde_json::to_value(document.snapshot()).unwrap();
        let edit = || {
            vec![SequenceEdit {
                range: 0..1,
                block: vec![block("first", "new"), block("inserted", "generated")],
            }]
        };
        drop(document.prepare_edits(edit()).unwrap().unwrap());
        assert_eq!(serde_json::to_value(document.snapshot()).unwrap(), before);
        let base = document.revision();
        let patch = document.prepare_edits(edit()).unwrap().unwrap().commit();
        assert_eq!(patch.base, base);
        assert_eq!(patch.next, base.next().unwrap());
        assert_eq!(document.revision(), patch.next);
        assert_eq!(
            document
                .block(&BlockId("last".into()))
                .unwrap()
                .text
                .wire_rows(),
            vec!["retained"]
        );
    }
}
