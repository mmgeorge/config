//! An AVL sequence indexes stable identities, row counts, and byte counts.
//!
//! Construction is O(B). Coordinate lookup is O(log B). Splicing K blocks costs
//! O(log B + K), excluding validation and allocation of their text and metadata.

use std::collections::{HashMap, HashSet};
use std::ops::Range;

use crate::ContractError;
use crate::block::BufferBlock;
use crate::identity::{BlockId, FoldId, RegionId};

type NodeIndex = usize;
type NodeLink = Option<NodeIndex>;

#[derive(Debug, Clone)]
struct SequenceNode {
    block: BufferBlock,
    parent: NodeLink,
    left: NodeLink,
    right: NodeLink,
    height: usize,
    count: usize,
    rows: usize,
    bytes: usize,
}

#[derive(Debug, Clone, Default)]
pub struct BlockSequence {
    root: NodeLink,
    node: Vec<Option<SequenceNode>>,
    free: Vec<NodeIndex>,
    locator: HashMap<BlockId, NodeIndex>,
    region_owner: HashMap<RegionId, BlockId>,
    fold_owner: HashMap<FoldId, BlockId>,
    fold_endpoint: HashMap<BlockId, HashSet<BlockId>>,
}

#[derive(Debug, Clone)]
pub struct SequenceEdit {
    pub range: Range<usize>,
    pub block: Vec<BufferBlock>,
}

pub struct BlockIter<'sequence> {
    sequence: &'sequence BlockSequence,
    pending: Vec<NodeIndex>,
    remaining: usize,
}

impl BlockSequence {
    pub fn new(block: Vec<BufferBlock>) -> Result<Self, ContractError> {
        let mut sequence = Self::default();
        sequence.splice(0..0, block)?;
        Ok(sequence)
    }

    pub fn blocks(&self) -> BlockIter<'_> {
        self.iter_from(0, self.len())
    }

    pub fn region_block(&self, region: &RegionId) -> Option<&BufferBlock> {
        let owner = self.region_owner.get(region)?;
        Some(&self.entry(*self.locator.get(owner)?).block)
    }

    pub(crate) fn fold_endpoint_owner(
        &self,
        endpoint: &BlockId,
    ) -> impl Iterator<Item = &BufferBlock> {
        self.fold_endpoint
            .get(endpoint)
            .into_iter()
            .flatten()
            .map(|owner| &self.entry(self.locator[owner]).block)
    }

    pub fn range(&self, range: Range<usize>) -> Result<BlockIter<'_>, ContractError> {
        self.validate_range(&range)?;
        Ok(self.iter_from(range.start, range.end - range.start))
    }

    pub fn len(&self) -> usize {
        self.count(self.root)
    }
    pub fn is_empty(&self) -> bool {
        self.root.is_none()
    }
    pub fn row_count(&self) -> usize {
        self.rows(self.root)
    }
    pub fn byte_count(&self) -> usize {
        self.bytes(self.root)
    }

    pub fn locate(&self, mut row: usize) -> Option<(&BufferBlock, usize)> {
        let mut current = self.root;
        while let Some(index) = current {
            let node = self.entry(index);
            let before = self.rows(node.left);
            if row < before {
                current = node.left;
            } else if row < before + node.block.text.row_count() {
                return Some((&node.block, row - before));
            } else {
                row -= before + node.block.text.row_count();
                current = node.right;
            }
        }
        None
    }

    pub fn position(&self, id: &BlockId) -> Option<usize> {
        let mut index = *self.locator.get(id)?;
        let mut row = self.rows(self.entry(index).left);
        while let Some(parent) = self.entry(index).parent {
            let ancestor = self.entry(parent);
            if ancestor.right == Some(index) {
                row += self.rows(ancestor.left) + ancestor.block.text.row_count();
            }
            index = parent;
        }
        Some(row)
    }

    pub fn index(&self, id: &BlockId) -> Option<usize> {
        let mut index = *self.locator.get(id)?;
        let mut position = self.count(self.entry(index).left);
        while let Some(parent) = self.entry(index).parent {
            let ancestor = self.entry(parent);
            if ancestor.right == Some(index) {
                position += self.count(ancestor.left) + 1;
            }
            index = parent;
        }
        Some(position)
    }

    pub fn splice(
        &mut self,
        range: Range<usize>,
        block: Vec<BufferBlock>,
    ) -> Result<(), ContractError> {
        self.apply_edits(vec![SequenceEdit { range, block }])
    }

    /// Commits descending disjoint ranges after validating their combined result.
    /// Block and region identities can move between removed ranges in one edit.
    pub fn apply_edits(&mut self, edits: Vec<SequenceEdit>) -> Result<(), ContractError> {
        let (next_rows, next_bytes) = self.validate_edits(&edits)?;
        self.apply_validated_edits(edits, next_rows, next_bytes);
        Ok(())
    }

    pub(crate) fn apply_validated_edits(
        &mut self,
        edits: Vec<SequenceEdit>,
        next_rows: usize,
        next_bytes: usize,
    ) {
        let mut lower_removed: usize = edits.iter().map(|edit| edit.range.len()).sum();
        let mut insertion = Vec::with_capacity(edits.len());
        for edit in edits {
            let (before, remaining) = self.split(self.root, edit.range.start);
            let (removed, after) = self.split(remaining, edit.range.len());
            self.release(removed);
            self.root = self.concatenate(before, after);
            self.set_parent(self.root, None);
            lower_removed -= edit.range.len();
            insertion.push((edit.range.start - lower_removed, edit.block));
        }
        for (position, block) in insertion {
            let (before, after) = self.split(self.root, position);
            let count = block.len();
            let middle = self.build(&mut block.into_iter(), count);
            let prefix = self.concatenate(before, middle);
            self.root = self.concatenate(prefix, after);
            self.set_parent(self.root, None);
        }
        debug_assert_eq!(self.row_count(), next_rows);
        debug_assert_eq!(self.byte_count(), next_bytes);
    }

    pub(crate) fn validate_edits(
        &self,
        edits: &[SequenceEdit],
    ) -> Result<(usize, usize), ContractError> {
        let mut removed = HashSet::new();
        let mut previous_start = None;
        let mut next_rows = self.row_count();
        let mut next_bytes = self.byte_count();
        for edit in edits {
            self.validate_range(&edit.range)?;
            if previous_start
                .is_some_and(|start| edit.range.end > start || edit.range.start == start)
            {
                return Err(ContractError(
                    "sequence edits must be descending and disjoint",
                ));
            }
            previous_start = Some(edit.range.start);
            for block in self.range(edit.range.clone())? {
                removed.insert(&block.id);
                next_rows -= block.text.row_count();
                next_bytes -= block.text.byte_count();
            }
        }
        let mut incoming = HashSet::new();
        let mut incoming_region = HashSet::new();
        let mut incoming_fold = HashSet::new();
        for block in edits.iter().flat_map(|edit| &edit.block) {
            block.validate()?;
            if !incoming.insert(&block.id)
                || (self.locator.contains_key(&block.id) && !removed.contains(&block.id))
            {
                return Err(ContractError("duplicate block identity"));
            }
            for region in &block.metadata.editable_region {
                if !incoming_region.insert(&region.id)
                    || self
                        .region_owner
                        .get(&region.id)
                        .is_some_and(|owner| !removed.contains(owner))
                {
                    return Err(ContractError("duplicate document region identity"));
                }
            }
            for fold in &block.metadata.fold {
                if !incoming_fold.insert(&fold.id)
                    || self
                        .fold_owner
                        .get(&fold.id)
                        .is_some_and(|owner| !removed.contains(owner))
                {
                    return Err(ContractError("duplicate document fold identity"));
                }
            }
            next_rows = next_rows
                .checked_add(block.text.row_count())
                .ok_or(ContractError("sequence row count overflow"))?;
            next_bytes = next_bytes
                .checked_add(block.text.byte_count())
                .ok_or(ContractError("sequence byte count overflow"))?;
        }
        self.validate_folds(edits, &removed)?;
        Ok((next_rows, next_bytes))
    }

    fn validate_folds(
        &self,
        edits: &[SequenceEdit],
        removed: &HashSet<&BlockId>,
    ) -> Result<(), ContractError> {
        let incoming: HashMap<_, _> = edits
            .iter()
            .flat_map(|edit| &edit.block)
            .map(|block| (&block.id, block))
            .collect();
        let mut incoming_position = HashMap::new();
        let mut delta: isize = 0;
        for edit in edits.iter().rev() {
            let start = edit
                .range
                .start
                .checked_add_signed(delta)
                .ok_or(ContractError("fold position overflow"))?;
            for (offset, block) in edit.block.iter().enumerate() {
                incoming_position.insert(&block.id, start + offset);
            }
            delta += edit.block.len() as isize - edit.range.len() as isize;
        }
        let resolve = |id: &BlockId| -> Option<&BufferBlock> {
            incoming.get(id).copied().or_else(|| {
                if removed.contains(id) {
                    None
                } else {
                    self.locator.get(id).map(|index| &self.entry(*index).block)
                }
            })
        };
        let position = |id: &BlockId| -> Option<usize> {
            if let Some(position) = incoming_position.get(id) {
                return Some(*position);
            }
            if removed.contains(id) {
                return None;
            }
            let original = self.index(id)?;
            let mut delta: isize = 0;
            for edit in edits {
                if edit.range.end <= original {
                    delta += edit.block.len() as isize - edit.range.len() as isize;
                }
            }
            original.checked_add_signed(delta)
        };
        let mut affected: HashSet<&BlockId> = incoming.keys().copied().collect();
        for endpoint in removed {
            if let Some(owners) = self.fold_endpoint.get(*endpoint) {
                affected.extend(owners.iter().filter(|owner| !removed.contains(owner)));
            }
        }
        for owner in affected {
            let block = resolve(owner).ok_or(ContractError("fold owner is absent"))?;
            let start_index = position(owner).ok_or(ContractError("fold owner has no position"))?;
            for fold in &block.metadata.fold {
                let endpoint =
                    resolve(&fold.end.block).ok_or(ContractError("fold endpoint was removed"))?;
                let end_index = position(&fold.end.block)
                    .ok_or(ContractError("fold endpoint has no position"))?;
                crate::block::TextRange {
                    start: fold.end.position,
                    end: fold.end.position,
                }
                .validate(&endpoint.text)?;
                if start_index > end_index
                    || (start_index == end_index && fold.start >= fold.end.position)
                {
                    return Err(ContractError("fold range is empty or reversed"));
                }
            }
        }
        Ok(())
    }

    fn entry(&self, index: NodeIndex) -> &SequenceNode {
        self.node[index].as_ref().expect("live sequence node")
    }

    fn entry_mut(&mut self, index: NodeIndex) -> &mut SequenceNode {
        self.node[index].as_mut().expect("live sequence node")
    }

    fn height(&self, node: NodeLink) -> usize {
        node.map_or(0, |index| self.entry(index).height)
    }
    fn count(&self, node: NodeLink) -> usize {
        node.map_or(0, |index| self.entry(index).count)
    }
    fn rows(&self, node: NodeLink) -> usize {
        node.map_or(0, |index| self.entry(index).rows)
    }
    fn bytes(&self, node: NodeLink) -> usize {
        node.map_or(0, |index| self.entry(index).bytes)
    }

    fn set_parent(&mut self, node: NodeLink, parent: NodeLink) {
        if let Some(index) = node {
            self.entry_mut(index).parent = parent;
        }
    }

    fn refresh(&mut self, index: NodeIndex) {
        let node = self.entry(index);
        let height = 1 + self.height(node.left).max(self.height(node.right));
        let count = 1 + self.count(node.left) + self.count(node.right);
        let rows = node.block.text.row_count() + self.rows(node.left) + self.rows(node.right);
        let bytes = node.block.text.byte_count() + self.bytes(node.left) + self.bytes(node.right);
        let node = self.entry_mut(index);
        node.height = height;
        node.count = count;
        node.rows = rows;
        node.bytes = bytes;
    }

    fn attach(&mut self, index: NodeIndex, left: NodeLink, right: NodeLink) {
        let node = self.entry_mut(index);
        node.left = left;
        node.right = right;
        self.set_parent(left, Some(index));
        self.set_parent(right, Some(index));
        self.refresh(index);
    }

    fn rotate_left(&mut self, root: NodeIndex) -> NodeIndex {
        let pivot = self.entry(root).right.expect("right-heavy root");
        let before = self.entry(root).left;
        let middle = self.entry(pivot).left;
        let after = self.entry(pivot).right;
        let parent = self.entry(root).parent;
        self.attach(root, before, middle);
        self.attach(pivot, Some(root), after);
        self.set_parent(Some(pivot), parent);
        pivot
    }

    fn rotate_right(&mut self, root: NodeIndex) -> NodeIndex {
        let pivot = self.entry(root).left.expect("left-heavy root");
        let before = self.entry(pivot).left;
        let middle = self.entry(pivot).right;
        let after = self.entry(root).right;
        let parent = self.entry(root).parent;
        self.attach(root, middle, after);
        self.attach(pivot, before, Some(root));
        self.set_parent(Some(pivot), parent);
        pivot
    }

    fn balance(&mut self, root: NodeIndex) -> NodeIndex {
        self.refresh(root);
        let left = self.entry(root).left;
        let right = self.entry(root).right;
        if self.height(left) > self.height(right) + 1 {
            let child = left.expect("left-heavy root");
            if self.height(self.entry(child).right) > self.height(self.entry(child).left) {
                let child = self.rotate_left(child);
                self.attach(root, Some(child), right);
            }
            self.rotate_right(root)
        } else if self.height(right) > self.height(left) + 1 {
            let child = right.expect("right-heavy root");
            if self.height(self.entry(child).left) > self.height(self.entry(child).right) {
                let child = self.rotate_right(child);
                self.attach(root, left, Some(child));
            }
            self.rotate_left(root)
        } else {
            root
        }
    }

    fn join(&mut self, left: NodeLink, pivot: NodeIndex, right: NodeLink) -> NodeIndex {
        if self.height(left) > self.height(right) + 1 {
            let root = left.expect("taller left tree");
            let before = self.entry(root).left;
            let middle = self.join(self.entry(root).right, pivot, right);
            self.attach(root, before, Some(middle));
            self.balance(root)
        } else if self.height(right) > self.height(left) + 1 {
            let root = right.expect("taller right tree");
            let after = self.entry(root).right;
            let middle = self.join(left, pivot, self.entry(root).left);
            self.attach(root, Some(middle), after);
            self.balance(root)
        } else {
            self.attach(pivot, left, right);
            pivot
        }
    }

    fn split(&mut self, root: NodeLink, count: usize) -> (NodeLink, NodeLink) {
        let Some(root) = root else {
            return (None, None);
        };
        let left = self.entry(root).left;
        let right = self.entry(root).right;
        let left_count = self.count(left);
        let result = if count <= left_count {
            let (before, middle) = self.split(left, count);
            let after = self.join(middle, root, right);
            (before, Some(after))
        } else {
            let (middle, after) = self.split(right, count - left_count - 1);
            let before = self.join(left, root, middle);
            (Some(before), after)
        };
        self.set_parent(result.0, None);
        self.set_parent(result.1, None);
        result
    }

    fn concatenate(&mut self, left: NodeLink, right: NodeLink) -> NodeLink {
        match (left, right) {
            (None, tree) | (tree, None) => tree,
            (Some(left), Some(right)) => {
                let (before, pivot) = self.split(Some(left), self.count(Some(left)) - 1);
                Some(self.join(before, pivot.expect("last block"), Some(right)))
            }
        }
    }

    fn build(&mut self, blocks: &mut impl Iterator<Item = BufferBlock>, count: usize) -> NodeLink {
        if count == 0 {
            return None;
        }
        let left = self.build(blocks, count / 2);
        let block = blocks.next().expect("declared inserted block count");
        let index = if let Some(index) = self.free.pop() {
            index
        } else {
            self.node.push(None);
            self.node.len() - 1
        };
        self.locator.insert(block.id.clone(), index);
        for region in &block.metadata.editable_region {
            self.region_owner
                .insert(region.id.clone(), block.id.clone());
        }
        for fold in &block.metadata.fold {
            self.fold_owner.insert(fold.id.clone(), block.id.clone());
            self.fold_endpoint
                .entry(fold.end.block.clone())
                .or_default()
                .insert(block.id.clone());
        }
        self.node[index] = Some(SequenceNode {
            block,
            parent: None,
            left: None,
            right: None,
            height: 1,
            count: 1,
            rows: 0,
            bytes: 0,
        });
        let right = self.build(blocks, count - count / 2 - 1);
        self.attach(index, left, right);
        Some(index)
    }

    fn release(&mut self, root: NodeLink) {
        if let Some(index) = root {
            let node = self.node[index].take().expect("removed live node");
            self.release(node.left);
            self.release(node.right);
            self.locator.remove(&node.block.id);
            for region in node.block.metadata.editable_region {
                self.region_owner.remove(&region.id);
            }
            for fold in node.block.metadata.fold {
                self.fold_owner.remove(&fold.id);
                if let Some(owners) = self.fold_endpoint.get_mut(&fold.end.block) {
                    owners.remove(&node.block.id);
                    if owners.is_empty() {
                        self.fold_endpoint.remove(&fold.end.block);
                    }
                }
            }
            self.free.push(index);
        }
    }

    fn validate_range(&self, range: &Range<usize>) -> Result<(), ContractError> {
        if range.start > range.end || range.end > self.len() {
            return Err(ContractError("block splice is outside sequence"));
        }
        Ok(())
    }

    fn iter_from(&self, mut offset: usize, remaining: usize) -> BlockIter<'_> {
        let mut pending = Vec::new();
        let mut current = self.root;
        while let Some(index) = current {
            let node = self.entry(index);
            let before = self.count(node.left);
            if offset <= before {
                pending.push(index);
                if offset == before {
                    break;
                }
                current = node.left;
            } else {
                offset -= before + 1;
                current = node.right;
            }
        }
        BlockIter {
            sequence: self,
            pending,
            remaining,
        }
    }
}

impl<'sequence> Iterator for BlockIter<'sequence> {
    type Item = &'sequence BufferBlock;

    fn next(&mut self) -> Option<Self::Item> {
        if self.remaining == 0 {
            return None;
        }
        let index = self.pending.pop()?;
        let node = self.sequence.entry(index);
        let mut current = node.right;
        while let Some(index) = current {
            self.pending.push(index);
            current = self.sequence.entry(index).left;
        }
        self.remaining -= 1;
        Some(&node.block)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.remaining, Some(self.remaining))
    }
}

impl ExactSizeIterator for BlockIter<'_> {}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block::{BlockMetadata, EditableRegion, TextPosition, TextRange};
    use crate::identity::RegionRevision;
    use crate::text::BufferText;

    fn block(identity: usize, rows: usize) -> BufferBlock {
        BufferBlock {
            id: BlockId(format!("block-{identity}")),
            text: BufferText::from_rows((0..rows).map(|_| "λ")).unwrap(),
            metadata: BlockMetadata::default(),
        }
    }

    fn audit(
        sequence: &BlockSequence,
        root: NodeLink,
        parent: NodeLink,
    ) -> (usize, usize, usize, usize) {
        let Some(index) = root else {
            return (0, 0, 0, 0);
        };
        let node = sequence.entry(index);
        assert_eq!(node.parent, parent);
        assert_eq!(sequence.locator.get(&node.block.id), Some(&index));
        let (left_height, left_count, left_rows, left_bytes) =
            audit(sequence, node.left, Some(index));
        let (right_height, right_count, right_rows, right_bytes) =
            audit(sequence, node.right, Some(index));
        assert!(
            left_height.abs_diff(right_height) <= 1,
            "unbalanced block {}",
            node.block.id.0
        );
        let expected = (
            1 + left_height.max(right_height),
            1 + left_count + right_count,
            node.block.text.row_count() + left_rows + right_rows,
            node.block.text.byte_count() + left_bytes + right_bytes,
        );
        assert_eq!((node.height, node.count, node.rows, node.bytes), expected);
        expected
    }

    #[test]
    fn mixed_range_splices_preserve_balance_parent_links_and_empty_blocks() {
        let mut sequence = BlockSequence::default();
        let mut expected = Vec::new();
        let mut random = 90210u64;
        let mut identity = 0;
        for operation in 0..4000 {
            random = random
                .wrapping_mul(6364136223846793005)
                .wrapping_add(1442695040888963407);
            let start = random as usize % (expected.len() + 1);
            let removed = ((random >> 12) as usize % 12).min(expected.len() - start);
            let inserted: Vec<_> = (0..(random >> 20) as usize % 9)
                .map(|offset| {
                    identity += 1;
                    block(identity, (operation + offset) % 5)
                })
                .collect();
            sequence
                .splice(start..start + removed, inserted.clone())
                .unwrap();
            expected.splice(start..start + removed, inserted);
            let (_, count, rows, bytes) = audit(&sequence, sequence.root, None);
            assert_eq!(count, expected.len());
            assert_eq!(sequence.locator.len(), count);
            assert_eq!(
                sequence.node.iter().filter(|node| node.is_some()).count(),
                count
            );
            assert_eq!(sequence.free.len() + count, sequence.node.len());
            assert_eq!(sequence.row_count(), rows);
            assert_eq!(sequence.byte_count(), bytes);
            assert_eq!(
                sequence.blocks().collect::<Vec<_>>(),
                expected.iter().collect::<Vec<_>>()
            );
            let mut position = 0;
            for (index, block) in expected.iter().enumerate() {
                assert_eq!(sequence.index(&block.id), Some(index));
                assert_eq!(sequence.position(&block.id), Some(position));
                for row in 0..block.text.row_count() {
                    let (found, relative) = sequence.locate(position + row).unwrap();
                    assert_eq!(found.id, block.id);
                    assert_eq!(relative, row);
                }
                position += block.text.row_count();
            }
            assert!(sequence.locate(position).is_none());
            let range_start = start.min(expected.len());
            let range_end = (range_start + 7).min(expected.len());
            assert_eq!(
                sequence
                    .range(range_start..range_end)
                    .unwrap()
                    .collect::<Vec<_>>(),
                expected[range_start..range_end].iter().collect::<Vec<_>>()
            );
        }
    }

    #[test]
    fn adversarial_append_prepend_and_bulk_removal_keep_logarithmic_height() {
        let mut sequence =
            BlockSequence::new((0..20_000).map(|identity| block(identity, 1)).collect()).unwrap();
        for identity in 20_000..22_000 {
            let position = if identity % 2 == 0 { 0 } else { sequence.len() };
            sequence
                .splice(position..position, vec![block(identity, 1)])
                .unwrap();
        }
        let (height, count, _, _) = audit(&sequence, sequence.root, None);
        assert!(height <= 2 * (count + 1).ilog2() as usize);
        sequence.splice(5..sequence.len() - 5, vec![]).unwrap();
        assert_eq!(sequence.len(), 10);
        audit(&sequence, sequence.root, None);
        let capacity = sequence.node.len();
        for identity in 30_000..31_000 {
            sequence.splice(4..5, vec![block(identity, 3)]).unwrap();
        }
        assert_eq!(
            sequence.node.len(),
            capacity,
            "replacement must reuse released node slots"
        );
        audit(&sequence, sequence.root, None);
        sequence.splice(0..sequence.len(), vec![]).unwrap();
        assert!(sequence.is_empty());
        assert!(sequence.locator.is_empty());
        assert_eq!(sequence.row_count(), 0);
    }

    #[test]
    fn failed_splice_preserves_sequence_and_region_owners() {
        let mut first = block(1, 1);
        first.metadata.editable_region.push(EditableRegion {
            id: RegionId("region".into()),
            revision: RegionRevision(0),
            sequence: crate::identity::EditSequence(0),
            range: TextRange {
                start: TextPosition { row: 0, column: 0 },
                end: TextPosition { row: 1, column: 0 },
            },
        });
        let mut sequence = BlockSequence::new(vec![first.clone(), block(2, 0)]).unwrap();
        let mut duplicate_region = first.clone();
        duplicate_region.id = BlockId("other".into());
        for (range, inserted) in [
            (1..2, vec![first.clone()]),
            (1..2, vec![duplicate_region]),
            (3..3, vec![]),
        ] {
            let before = sequence.blocks().cloned().collect::<Vec<_>>();
            assert!(sequence.splice(range, inserted).is_err());
            assert_eq!(sequence.blocks().cloned().collect::<Vec<_>>(), before);
            audit(&sequence, sequence.root, None);
        }
        sequence.splice(0..1, vec![first]).unwrap();
        assert_eq!(sequence.region_owner.len(), 1);
        sequence.splice(0..1, vec![]).unwrap();
        assert!(sequence.region_owner.is_empty());
    }

    #[test]
    fn fold_endpoint_edits_are_validated_before_publication() {
        use crate::block::{BlockAnchor, FoldRange};
        let mut first = block(1, 1);
        first.metadata.fold.push(FoldRange {
            id: FoldId("section".into()),
            start: TextPosition { row: 0, column: 0 },
            end: BlockAnchor {
                block: BlockId("block-2".into()),
                position: TextPosition { row: 1, column: 0 },
            },
            closed: true,
        });
        let mut sequence = BlockSequence::new(vec![first, block(2, 1)]).unwrap();
        let before = sequence.blocks().cloned().collect::<Vec<_>>();
        assert!(sequence.splice(1..2, vec![]).is_err());
        assert!(sequence.splice(1..2, vec![block(2, 0)]).is_err());
        assert_eq!(sequence.blocks().cloned().collect::<Vec<_>>(), before);
        sequence.splice(1..2, vec![block(2, 2)]).unwrap();
        assert_eq!(sequence.fold_owner.len(), 1);
        sequence.splice(0..2, vec![]).unwrap();
        assert!(sequence.fold_owner.is_empty());
        assert!(sequence.fold_endpoint.is_empty());
    }
}
