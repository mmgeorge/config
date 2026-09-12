//! Every edit addresses the same base revision and applies in descending order.

use serde::{Deserialize, Serialize};

use crate::ContractError;
use crate::block::{BlockMetadata, BufferBlock};
use crate::identity::{BlockId, DocumentId, DocumentRevision};
use crate::sequence::BlockSequence;
use crate::text::BufferText;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TextEdit {
    pub start_row: usize,
    pub removed_rows: usize,
    pub text: BufferText,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct MetadataEdit {
    pub block: BlockId,
    pub row_count: usize,
    pub metadata: BlockMetadata,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct BlockOrderEdit {
    pub start_block: usize,
    pub removed_blocks: usize,
    pub inserted: Vec<BlockId>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct BufferPatch {
    pub document: DocumentId,
    pub base: DocumentRevision,
    pub next: DocumentRevision,
    pub base_rows: usize,
    pub next_rows: usize,
    pub base_blocks: usize,
    pub next_blocks: usize,
    pub text_edit: Vec<TextEdit>,
    pub metadata_edit: Vec<MetadataEdit>,
    pub removed_block: Vec<BlockId>,
    pub block_edit: Vec<BlockOrderEdit>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct BufferSnapshot {
    pub document: DocumentId,
    pub revision: DocumentRevision,
    pub block: Vec<BufferBlock>,
}

impl BufferSnapshot {
    pub fn validate(&self) -> Result<(), ContractError> {
        self.document.validate()?;
        self.revision.validate()?;
        BlockSequence::new(self.block.clone())?;
        Ok(())
    }
}

impl BufferPatch {
    pub fn validate(&self) -> Result<(), ContractError> {
        self.document.validate()?;
        if self.base.next()? != self.next {
            return Err(ContractError("patch must advance exactly one revision"));
        }
        if [
            self.base_rows,
            self.next_rows,
            self.base_blocks,
            self.next_blocks,
        ]
        .into_iter()
        .any(|count| count > crate::MAX_COUNTER as usize)
        {
            return Err(ContractError(
                "patch count exceeds Lua's exact integer range",
            ));
        }
        let mut previous_start = None;
        let mut next_rows = self.base_rows;
        for edit in &self.text_edit {
            let end = edit
                .start_row
                .checked_add(edit.removed_rows)
                .ok_or(ContractError("edit row count overflow"))?;
            if end > self.base_rows
                || previous_start.is_some_and(|start| end > start || edit.start_row == start)
            {
                return Err(ContractError(
                    "edits must be disjoint, descending, and inside the base",
                ));
            }
            next_rows = next_rows
                .checked_sub(edit.removed_rows)
                .and_then(|rows| rows.checked_add(edit.text.row_count()))
                .ok_or(ContractError("result row count overflow"))?;
            previous_start = Some(edit.start_row);
        }
        if next_rows != self.next_rows {
            return Err(ContractError("patch result row count differs"));
        }
        let mut inserted = std::collections::HashSet::new();
        let mut previous_start = None;
        let mut next_blocks = self.base_blocks;
        for edit in &self.block_edit {
            let end = edit
                .start_block
                .checked_add(edit.removed_blocks)
                .ok_or(ContractError("block edit count overflow"))?;
            if end > self.base_blocks
                || previous_start.is_some_and(|start| end > start || edit.start_block == start)
            {
                return Err(ContractError(
                    "block edits must be disjoint, descending, and inside the base",
                ));
            }
            next_blocks = next_blocks
                .checked_sub(edit.removed_blocks)
                .and_then(|count| count.checked_add(edit.inserted.len()))
                .ok_or(ContractError("result block count overflow"))?;
            previous_start = Some(edit.start_block);
            for block in &edit.inserted {
                block.validate()?;
                if !inserted.insert(block) {
                    return Err(ContractError("duplicate inserted block identity"));
                }
            }
        }
        if next_blocks != self.next_blocks {
            return Err(ContractError("patch result block count differs"));
        }
        let mut changed = std::collections::HashSet::new();
        for edit in &self.metadata_edit {
            edit.block.validate()?;
            if !changed.insert(&edit.block) {
                return Err(ContractError("metadata block is repeated"));
            }
        }
        let mut removed = std::collections::HashSet::new();
        for block in &self.removed_block {
            block.validate()?;
            if inserted.contains(block) || changed.contains(block) || !removed.insert(block) {
                return Err(ContractError("removed block is retained or repeated"));
            }
        }
        Ok(())
    }
}

/// Applies text only after validating the entire patch and its displayed base.
pub fn apply_reference(
    document: &DocumentId,
    revision: DocumentRevision,
    rows: &[String],
    patch: &BufferPatch,
) -> Result<Vec<String>, ContractError> {
    patch.validate()?;
    if document != &patch.document || revision != patch.base || rows.len() != patch.base_rows {
        return Err(ContractError("patch does not match displayed document"));
    }
    let mut result = rows.to_vec();
    for edit in &patch.text_edit {
        result.splice(
            edit.start_row..edit.start_row + edit.removed_rows,
            edit.text.wire_rows().into_iter().map(str::to_owned),
        );
    }
    Ok(result)
}

/// Validates replica reconstruction, including identity, order, and metadata.
///
/// This reference path scans the snapshot. Native adapters use indexed ranges,
/// but must reject the same malformed transitions before modifying a buffer.
pub fn apply_snapshot_reference(
    snapshot: &BufferSnapshot,
    patch: &BufferPatch,
) -> Result<BufferSnapshot, ContractError> {
    snapshot.validate()?;
    if snapshot.block.len() != patch.base_blocks {
        return Err(ContractError("patch does not match displayed block count"));
    }
    let rows: Vec<String> = snapshot
        .block
        .iter()
        .flat_map(|block| block.text.wire_rows().into_iter().map(str::to_owned))
        .collect();
    let rows = apply_reference(&snapshot.document, snapshot.revision, &rows, patch)?;
    let mut order: Vec<_> = snapshot
        .block
        .iter()
        .map(|block| block.id.clone())
        .collect();
    for edit in &patch.block_edit {
        order.splice(
            edit.start_block..edit.start_block + edit.removed_blocks,
            edit.inserted.clone(),
        );
    }
    let original: std::collections::HashMap<_, _> = snapshot
        .block
        .iter()
        .map(|block| (&block.id, block))
        .collect();
    let metadata: std::collections::HashMap<_, _> = patch
        .metadata_edit
        .iter()
        .map(|edit| (&edit.block, edit))
        .collect();
    let retained: std::collections::HashSet<_> = order.iter().collect();
    if patch
        .metadata_edit
        .iter()
        .any(|edit| !retained.contains(&edit.block))
    {
        return Err(ContractError("metadata refers to an absent block"));
    }
    let expected_removed: std::collections::HashSet<_> = original
        .keys()
        .copied()
        .filter(|id| !retained.contains(id))
        .collect();
    if expected_removed != patch.removed_block.iter().collect() {
        return Err(ContractError(
            "removed block identities differ from the displayed base",
        ));
    }
    let mut offset: usize = 0;
    let mut block = Vec::with_capacity(order.len());
    for id in order {
        let (row_count, presentation) = if let Some(edit) = metadata.get(&id) {
            (edit.row_count, edit.metadata.clone())
        } else {
            let previous = original
                .get(&id)
                .ok_or(ContractError("inserted block has no metadata"))?;
            (previous.text.row_count(), previous.metadata.clone())
        };
        let after = offset
            .checked_add(row_count)
            .filter(|after| *after <= rows.len())
            .ok_or(ContractError("block rows exceed patched text"))?;
        block.push(BufferBlock {
            id,
            text: BufferText::from_rows(&rows[offset..after])?,
            metadata: presentation,
        });
        offset = after;
    }
    if offset != rows.len() {
        return Err(ContractError("block rows do not cover patched text"));
    }
    let result = BufferSnapshot {
        document: snapshot.document.clone(),
        revision: patch.next,
        block,
    };
    result.validate()?;
    Ok(result)
}
