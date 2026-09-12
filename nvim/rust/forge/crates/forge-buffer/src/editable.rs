//! Local text acceptance uses region revisions independently of generated layout.

use serde::{Deserialize, Serialize};

use crate::ContractError;
use crate::block::{BufferBlock, TextPosition, TextRange};
use crate::identity::{DocumentId, EditSequence, RegionId, RegionRevision};
use crate::patch::BufferPatch;
use crate::text::BufferText;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LocalEdit {
    pub document: DocumentId,
    pub region: RegionId,
    pub base: RegionRevision,
    pub sequence: EditSequence,
    pub text: BufferText,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct EditAcknowledgement {
    pub document: DocumentId,
    pub region: RegionId,
    pub sequence: EditSequence,
    pub revision: RegionRevision,
}

#[derive(Debug, Clone)]
pub enum LocalEditResult {
    Accepted {
        acknowledgement: EditAcknowledgement,
        patch: Box<BufferPatch>,
    },
    Conflict {
        current: RegionRevision,
    },
    UnknownRegion,
    StaleSequence,
}

pub(crate) fn replace_region(
    mut block: BufferBlock,
    region_index: usize,
    replacement: &BufferText,
) -> Result<BufferBlock, ContractError> {
    let region = &block.metadata.editable_region[region_index];
    let range = region.range.clone();
    let revision = region.revision.next()?;
    let offset = |position: TextPosition| {
        block.text.wire_rows()[..position.row]
            .iter()
            .map(|row| row.len() + 1)
            .sum::<usize>()
            + position.column
    };
    let start = offset(range.start);
    let end = offset(range.end);
    let mut source = String::with_capacity(block.text.byte_count());
    for row in block.text.wire_rows() {
        source.push_str(row);
        source.push('\n');
    }
    let text = replacement.wire_rows().join("\n");
    source.replace_range(start..end, &text);
    let inserted_end = if replacement.row_count() <= 1 {
        TextPosition {
            row: range.start.row,
            column: range.start.column + text.len(),
        }
    } else {
        TextPosition {
            row: range.start.row + replacement.row_count() - 1,
            column: replacement
                .row(replacement.row_count() - 1)
                .expect("last row")
                .len(),
        }
    };
    if !source.is_empty() && !source.ends_with('\n') {
        source.push('\n');
    }
    block.text = BufferText::from_rows(source.split_terminator('\n'))?;
    let shift = |position: &mut TextPosition| {
        if position.row == range.end.row {
            position.row = inserted_end.row;
            position.column = inserted_end.column + position.column - range.end.column;
        } else {
            position.row = inserted_end.row + position.row - range.end.row;
        }
    };
    let rebase = |candidate: &mut TextRange| {
        if candidate.end <= range.start {
            return true;
        }
        if candidate.start >= range.end {
            shift(&mut candidate.start);
            shift(&mut candidate.end);
            return true;
        }
        false
    };
    let rebase_decoration = |candidate: &mut TextRange| {
        if *candidate == range {
            candidate.end = TextPosition {
                row: range.start.row,
                column: range.start.column
                    + replacement.row(0).expect("replacement first row").len(),
            };
            return true;
        }
        rebase(candidate)
    };
    block
        .metadata
        .target
        .retain_mut(|target| rebase(&mut target.range));
    block
        .metadata
        .decoration
        .retain_mut(|decoration| rebase_decoration(&mut decoration.range));
    block
        .metadata
        .visible_decoration
        .retain_mut(|decoration| rebase_decoration(&mut decoration.range));
    block
        .metadata
        .source_highlight
        .retain_mut(|decoration| rebase_decoration(&mut decoration.range));
    block
        .metadata
        .conceal
        .retain_mut(|conceal| rebase(&mut conceal.range));
    block
        .metadata
        .source_overlay
        .retain_mut(|overlay| rebase(&mut overlay.range));
    for (index, region) in block.metadata.editable_region.iter_mut().enumerate() {
        if index == region_index {
            region.range.end = inserted_end;
            region.revision = revision;
        } else if !rebase(&mut region.range) {
            return Err(ContractError("local edit overlaps another editable region"));
        }
    }
    for fold in &mut block.metadata.fold {
        if fold.start > range.start {
            rebase_fold_endpoint(&mut fold.start, &range, inserted_end);
        }
        if fold.end.block == block.id {
            rebase_fold_endpoint(&mut fold.end.position, &range, inserted_end);
        }
    }
    block.validate()?;
    Ok(block)
}

pub(crate) fn rebase_fold_endpoint(
    position: &mut TextPosition,
    range: &TextRange,
    inserted_end: TextPosition,
) {
    if *position < range.start {
        return;
    }
    if *position <= range.end {
        *position = inserted_end;
    } else if position.row == range.end.row {
        position.row = inserted_end.row;
        position.column = inserted_end.column + position.column - range.end.column;
    } else {
        position.row = inserted_end.row + position.row - range.end.row;
    }
}
