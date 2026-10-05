//! Projects compact container signatures as real text while retaining source block identities.

use std::collections::HashMap;

use crate::ContractError;
use crate::block::{BlockAnchor, BufferBlock, Collapse, TextPosition};
use crate::identity::FoldId;
use crate::text::BufferText;

/// Applies body visibility to folds with a compact suffix and repairs enclosing native endpoints.
/// Container rows must occupy individual blocks. Explicit choices override the fold's default.
pub fn project(
    mut block: Vec<BufferBlock>,
    state: &HashMap<FoldId, bool>,
) -> Result<Vec<BufferBlock>, ContractError> {
    let index: HashMap<_, _> = block
        .iter()
        .enumerate()
        .map(|(index, block)| (block.id.clone(), index))
        .collect();
    let mut container = Vec::new();
    for (opening, owner) in block.iter().enumerate() {
        for fold in &owner.metadata.fold {
            let Some(suffix) = &fold.collapsed_suffix else {
                continue;
            };
            let end = *index
                .get(&fold.end.block)
                .ok_or(ContractError("collapse endpoint is missing"))?;
            let boundary = end + usize::from(fold.end.position.row > 0);
            let heading = fold
                .heading_start
                .as_ref()
                .and_then(|anchor| index.get(&anchor.block))
                .copied()
                .unwrap_or(opening);
            if owner.text.row_count() != 1 || opening >= boundary || heading > opening {
                return Err(ContractError(
                    "collapse requires individual ordered container rows",
                ));
            }
            let closed = state.get(&fold.id).copied().unwrap_or(fold.closed);
            container.push((
                heading,
                opening,
                boundary,
                suffix.clone(),
                Collapse {
                    id: fold.id.clone(),
                    closed,
                    opening: BlockAnchor {
                        block: owner.id.clone(),
                        position: fold.start,
                    },
                },
            ));
        }
    }
    // Innermost containers own Tab throughout their heading and body.
    container.sort_by_key(|(heading, _, end, _, collapse)| (end - heading, collapse.id.clone()));
    let mut retained = vec![true; block.len()];
    for (heading, opening, boundary, suffix, collapse) in &container {
        for owner in &mut block[*heading..*boundary] {
            owner.metadata.collapse.push(collapse.clone());
        }
        if collapse.closed {
            retained[opening + 1..*boundary].fill(false);
            let owner = &mut block[*opening];
            let text = format!("{}{}", owner.text.row(0).unwrap_or(""), suffix);
            owner.text = BufferText::from_rows([text.as_str()])?;
        }
    }
    let mut previous = Vec::with_capacity(block.len());
    let mut last = None;
    for (position, keep) in retained.iter().enumerate() {
        if *keep {
            last = Some(position);
        }
        previous.push(last);
    }
    let endpoint: Vec<_> = block
        .iter()
        .map(|owner| BlockAnchor {
            block: owner.id.clone(),
            position: TextPosition {
                row: owner.text.row_count(),
                column: 0,
            },
        })
        .collect();
    for (position, owner) in block.iter_mut().enumerate() {
        if !retained[position] {
            continue;
        }
        owner
            .metadata
            .fold
            .retain(|fold| fold.collapsed_suffix.is_none());
        for fold in &mut owner.metadata.fold {
            let end = index[&fold.end.block] + usize::from(fold.end.position.row > 0);
            let Some(last) = end.checked_sub(1).and_then(|end| previous[end]) else {
                return Err(ContractError("native fold has no visible endpoint"));
            };
            fold.end = endpoint[last].clone();
        }
    }
    Ok(block
        .into_iter()
        .zip(retained)
        .filter_map(|(block, keep)| keep.then_some(block))
        .collect())
}
