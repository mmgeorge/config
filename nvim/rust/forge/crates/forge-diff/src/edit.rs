//! Captured byte replacements survive unrelated local edits without selecting a new diff hunk.

use std::{
    collections::{HashMap, HashSet},
    fmt,
    ops::Range,
};

use crate::{
    raw::{RawDiff, RawHunk, RawHunkId, compute_hunks},
    semantic_view::{DeclarationInventory, SourceComparison},
    source::{SourceError, SourcePair, SourceVersion},
    view::DiffVisibility,
};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum EditDirection {
    Forward,
    Reverse,
}

#[derive(Clone, Debug)]
struct ByteReplacement {
    range: Range<usize>,
    source: SourceVersion,
    replacement: Range<usize>,
}

struct ScopedRun<'source> {
    owner: Option<&'source str>,
    occurrence: usize,
    bytes: Range<usize>,
    admitted: bool,
}

/// Pins the exact selected bytes independently of display groups and subsequent raw hunk IDs.
#[derive(Clone, Debug)]
pub struct EditSelection {
    before: SourceVersion,
    replacement: Vec<ByteReplacement>,
}

#[derive(Clone, Debug)]
pub struct AppliedEdit {
    selection: EditSelection,
    after: SourceVersion,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum EditError {
    UnknownHunk,
    DifferentSource,
    AmbiguousScope,
    Overlap,
    SizeOverflow,
    Allocation(String),
    Source(SourceError),
}

impl fmt::Display for EditError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnknownHunk => formatter.write_str("selected edit belongs to another comparison"),
            Self::DifferentSource => formatter.write_str("selected edit source changed externally"),
            Self::AmbiguousScope => {
                formatter.write_str("partial edit has no unique source ownership anchor")
            }
            Self::Overlap => formatter.write_str("selected edit overlaps a newer local change"),
            Self::SizeOverflow => {
                formatter.write_str("selected edit size exceeds addressable memory")
            }
            Self::Allocation(error) => write!(formatter, "allocate selected edit: {error}"),
            Self::Source(error) => error.fmt(formatter),
        }
    }
}

impl std::error::Error for EditError {}

impl EditSelection {
    /// Captures a deduplicated subset of one comparison, rejecting foreign hunk identities.
    pub fn capture(
        analysis: &RawDiff,
        selected: &[RawHunkId],
        direction: EditDirection,
    ) -> Result<Self, EditError> {
        let selected: HashSet<_> = selected.iter().copied().collect();
        let pair = analysis.source();
        let (before, source) = match direction {
            EditDirection::Forward => (&pair.old, &pair.new),
            EditDirection::Reverse => (&pair.new, &pair.old),
        };
        let replacement: Vec<_> = analysis
            .hunks()
            .iter()
            .filter(|hunk| selected.contains(&hunk.id))
            .map(|hunk| {
                let (range, replacement) = match direction {
                    EditDirection::Forward => (hunk.old_bytes.clone(), hunk.new_bytes.clone()),
                    EditDirection::Reverse => (hunk.new_bytes.clone(), hunk.old_bytes.clone()),
                };
                ByteReplacement {
                    range,
                    source: source.clone(),
                    replacement,
                }
            })
            .collect();
        if replacement.len() != selected.len() {
            return Err(EditError::UnknownHunk);
        }
        Ok(Self {
            before: before.clone(),
            replacement,
        })
    }

    /// Capture raw edits within selected declarations or visibility, including descendants.
    /// Hidden source bytes remain pinned to the baseline in both edit directions. Foreign
    /// comparisons and ambiguous ownership fail before a selection can be published.
    pub fn capture_scoped(
        analysis: &RawDiff,
        selected: &[RawHunkId],
        direction: EditDirection,
        comparison: &SourceComparison,
        declaration: Option<&[&str]>,
        visibility: DiffVisibility,
    ) -> Result<Self, EditError> {
        if comparison.source.old.identity() != analysis.source().old.identity()
            || comparison.source.new.identity() != analysis.source().new.identity()
        {
            return Err(EditError::DifferentSource);
        }
        if !comparison.classified {
            if declaration.is_some() {
                return Err(EditError::AmbiguousScope);
            }
            return Self::capture(analysis, selected, direction);
        }
        if declaration.is_none() && visibility == DiffVisibility::All {
            return Self::capture(analysis, selected, direction);
        }
        let old_owner = scoped_owner(&comparison.before, declaration)?;
        let new_owner = scoped_owner(&comparison.after, declaration)?;
        if declaration.is_some()
            && !old_owner.iter().any(|selected| *selected)
            && !new_owner.iter().any(|selected| *selected)
        {
            return Err(EditError::AmbiguousScope);
        }
        let selected = selected.iter().copied().collect::<HashSet<_>>();
        let mut found = 0;
        let mut replacement = Vec::new();
        let before = match direction {
            EditDirection::Forward => &analysis.source().old,
            EditDirection::Reverse => &analysis.source().new,
        };
        for hunk in analysis
            .hunks()
            .iter()
            .filter(|hunk| selected.contains(&hunk.id))
        {
            found += 1;
            let old = scoped_runs(
                analysis,
                hunk,
                true,
                &comparison.before,
                &old_owner,
                declaration.is_none(),
                visibility,
            );
            let new = scoped_runs(
                analysis,
                hunk,
                false,
                &comparison.after,
                &new_owner,
                declaration.is_none(),
                visibility,
            );
            let (before_run, after_run, source, range, incoming) = match direction {
                EditDirection::Forward => (
                    old,
                    new,
                    &analysis.source().new,
                    hunk.old_bytes.clone(),
                    hunk.new_bytes.clone(),
                ),
                EditDirection::Reverse => (
                    new,
                    old,
                    &analysis.source().old,
                    hunk.new_bytes.clone(),
                    hunk.old_bytes.clone(),
                ),
            };
            if before_run.iter().chain(&after_run).all(|run| run.admitted) {
                replacement.push(ByteReplacement {
                    range,
                    source: source.clone(),
                    replacement: incoming,
                });
            } else if before_run.iter().chain(&after_run).any(|run| run.admitted) {
                let content = scoped_replacement(before, source, &before_run, &after_run)?;
                if before.bytes()[range.clone()] != content {
                    let source = SourceVersion::new(content, before.identity().representation)
                        .map_err(EditError::Source)?;
                    let baseline = SourceVersion::new(
                        before.bytes()[range.clone()].to_vec(),
                        before.identity().representation,
                    )
                    .map_err(EditError::Source)?;
                    let scoped = compute_hunks(SourcePair {
                        old: baseline,
                        new: source,
                    });
                    replacement.extend(scoped.hunks().iter().map(|hunk| ByteReplacement {
                        range: range.start + hunk.old_bytes.start..range.start + hunk.old_bytes.end,
                        source: scoped.source().new.clone(),
                        replacement: hunk.new_bytes.clone(),
                    }));
                }
            }
        }
        if found != selected.len() {
            return Err(EditError::UnknownHunk);
        }
        Ok(Self {
            before: before.clone(),
            replacement,
        })
    }

    pub fn before(&self) -> &SourceVersion {
        &self.before
    }

    /// Combine disjoint captures against the same baseline, rejecting overlapping replacements.
    pub fn combine(mut self, other: Self) -> Result<Self, EditError> {
        if self.before.identity() != other.before.identity() {
            return Err(EditError::DifferentSource);
        }
        self.replacement.extend(other.replacement);
        self.replacement
            .sort_by_key(|replacement| replacement.range.start);
        self.replacement.dedup_by(|right, left| {
            left.range == right.range
                && left.source.bytes()[left.replacement.clone()]
                    == right.source.bytes()[right.replacement.clone()]
        });
        if self.replacement.windows(2).any(|pair| {
            pair[0].range.end > pair[1].range.start || pair[0].range.start == pair[1].range.start
        }) {
            return Err(EditError::Overlap);
        }
        Ok(self)
    }

    /// Rejects replacements outside a captured boundary or against another baseline.
    pub fn validate_subset(&self, boundary: &Self) -> Result<(), EditError> {
        if self.before.identity() != boundary.before.identity() {
            return Err(EditError::DifferentSource);
        }
        for replacement in &self.replacement {
            let index = boundary
                .replacement
                .partition_point(|allowed| allowed.range.end < replacement.range.start);
            if !boundary.replacement.get(index).is_some_and(|allowed| {
                allowed.range.start <= replacement.range.start
                    && replacement.range.end <= allowed.range.end
            }) {
                return Err(EditError::AmbiguousScope);
            }
        }
        Ok(())
    }

    pub fn is_empty(&self) -> bool {
        self.replacement.is_empty()
    }

    pub fn retained_bytes(&self) -> usize {
        let mut source = HashSet::from([self.before.identity()]);
        self.before.bytes().len()
            + self
                .replacement
                .iter()
                .map(|replacement| {
                    std::mem::size_of::<ByteReplacement>()
                        + if source.insert(replacement.source.identity()) {
                            replacement.source.bytes().len()
                        } else {
                            0
                        }
                })
                .sum::<usize>()
    }

    /// Compares exact replacements after both selections reach the same source version.
    pub fn same_edit(&self, other: &Self) -> bool {
        self.before.identity() == other.before.identity()
            && self.replacement.len() == other.replacement.len()
            && self
                .replacement
                .iter()
                .zip(&other.replacement)
                .all(|(left, right)| {
                    left.range == right.range
                        && left.source.bytes()[left.replacement.clone()]
                            == right.source.bytes()[right.replacement.clone()]
                })
    }

    /// Applies only the captured replacements, preserving every unselected byte and line ending.
    pub fn apply(&self) -> Result<AppliedEdit, EditError> {
        if self.is_empty() {
            return Ok(AppliedEdit {
                selection: self.clone(),
                after: self.before.clone(),
            });
        }
        let size =
            self.replacement
                .iter()
                .try_fold(self.before.bytes().len(), |size, replacement| {
                    size.checked_sub(replacement.range.len())
                        .and_then(|size| size.checked_add(replacement.replacement.len()))
                        .ok_or(EditError::SizeOverflow)
                })?;
        let mut content = Vec::new();
        content
            .try_reserve_exact(size)
            .map_err(|error| EditError::Allocation(error.to_string()))?;
        let mut position = 0;
        for replacement in &self.replacement {
            content.extend_from_slice(&self.before.bytes()[position..replacement.range.start]);
            content.extend_from_slice(&replacement.source.bytes()[replacement.replacement.clone()]);
            position = replacement.range.end;
        }
        content.extend_from_slice(&self.before.bytes()[position..]);
        let after = SourceVersion::new(content, self.before.identity().representation)
            .map_err(EditError::Source)?;
        Ok(AppliedEdit {
            selection: self.clone(),
            after,
        })
    }

    /// Maps a selection through a known local edit. Identical replacements become no-ops.
    /// Overlapping replacements fail instead of guessing an alignment in repeated source text.
    pub fn rebase_over(&self, applied: &AppliedEdit) -> Result<Self, EditError> {
        if self.before.identity() != applied.selection.before.identity() {
            return Err(EditError::DifferentSource);
        }
        let mut replacement = Vec::new();
        for selected in &self.replacement {
            let mut offset: isize = 0;
            let mut duplicate = false;
            for previous in &applied.selection.replacement {
                if selected.range == previous.range
                    && selected.source.bytes()[selected.replacement.clone()]
                        == previous.source.bytes()[previous.replacement.clone()]
                {
                    duplicate = true;
                    break;
                }
                if previous.range.is_empty()
                    && selected.range.is_empty()
                    && previous.range.start == selected.range.start
                {
                    return Err(EditError::Overlap);
                }
                if previous.range.end <= selected.range.start {
                    offset += previous.replacement.len() as isize - previous.range.len() as isize;
                } else if selected.range.end > previous.range.start {
                    return Err(EditError::Overlap);
                }
            }
            if !duplicate {
                let mut selected = selected.clone();
                selected.range = selected
                    .range
                    .start
                    .checked_add_signed(offset)
                    .ok_or(EditError::Overlap)?
                    ..selected
                        .range
                        .end
                        .checked_add_signed(offset)
                        .ok_or(EditError::Overlap)?;
                replacement.push(selected);
            }
        }
        Ok(Self {
            before: applied.after.clone(),
            replacement,
        })
    }
}

fn scoped_owner(
    inventory: &DeclarationInventory,
    identity: Option<&[&str]>,
) -> Result<Vec<bool>, EditError> {
    let Some(identity) = identity else {
        return Ok(vec![true; inventory.declaration.len()]);
    };
    let mut selected = vec![false; inventory.declaration.len()];
    let mut found = HashSet::new();
    for (index, declaration) in inventory.declaration.iter().enumerate() {
        if identity.contains(&inventory.identity[index].as_str()) {
            if !found.insert(inventory.identity[index].as_str()) {
                return Err(EditError::AmbiguousScope);
            }
            selected[index] = true;
        } else {
            selected[index] = declaration.parent.is_some_and(|parent| selected[parent]);
        }
    }
    Ok(selected)
}

fn scoped_runs<'source>(
    analysis: &RawDiff,
    hunk: &RawHunk,
    old_side: bool,
    inventory: &'source DeclarationInventory,
    selected: &[bool],
    unclassified: bool,
    visibility: DiffVisibility,
) -> Vec<ScopedRun<'source>> {
    let (source, bytes, first) = if old_side {
        (
            &analysis.source().old,
            &hunk.old_bytes,
            hunk.old_lines.start,
        )
    } else {
        (
            &analysis.source().new,
            &hunk.new_bytes,
            hunk.new_lines.start,
        )
    };
    let mut runs = Vec::<ScopedRun<'source>>::new();
    let mut offset = bytes.start;
    for (line, text) in source.text()[bytes.clone()]
        .split_inclusive('\n')
        .enumerate()
    {
        let row = first + line;
        let owner = inventory.row_owner.get(row).copied().flatten();
        let admitted = owner.map_or(unclassified, |owner| selected[owner])
            && (visibility == DiffVisibility::All
                || inventory.public_rows.get(row).copied().unwrap_or(true));
        let identity = owner
            .map(|owner| inventory.identity[owner].as_str())
            .or(admitted.then_some(""));
        let end = offset + text.len();
        if let Some(previous) = runs
            .last_mut()
            .filter(|previous| previous.owner == identity && previous.admitted == admitted)
        {
            previous.bytes.end = end;
        } else {
            runs.push(ScopedRun {
                owner: identity,
                occurrence: 0,
                bytes: offset..end,
                admitted,
            });
        }
        offset = end;
    }
    let mut occurrence = HashMap::new();
    for run in &mut runs {
        let count = occurrence.entry(run.owner).or_insert(0);
        run.occurrence = *count;
        *count += 1;
    }
    runs
}

fn scoped_index<'source>(
    runs: &[ScopedRun<'source>],
) -> Result<HashMap<(&'source str, usize), usize>, EditError> {
    runs.iter()
        .enumerate()
        .map(|(position, run)| {
            Ok((
                (run.owner.ok_or(EditError::AmbiguousScope)?, run.occurrence),
                position,
            ))
        })
        .collect()
}

fn scoped_replacement(
    before: &SourceVersion,
    after: &SourceVersion,
    previous: &[ScopedRun<'_>],
    incoming: &[ScopedRun<'_>],
) -> Result<Vec<u8>, EditError> {
    let old = scoped_index(previous)?;
    let new = scoped_index(incoming)?;
    for run in previous
        .iter()
        .chain(incoming)
        .filter(|run| run.admitted && run.occurrence > 0)
    {
        let key = (run.owner.expect("indexed source owner"), run.occurrence);
        if !old.contains_key(&key) || !new.contains_key(&key) {
            return Err(EditError::AmbiguousScope);
        }
    }
    let mut retained = vec![Vec::new(); incoming.len() + 1];
    for (position, run) in previous.iter().enumerate().filter(|(_, run)| !run.admitted) {
        let owner = (run.owner.expect("indexed source owner"), run.occurrence);
        if let Some(index) = new.get(&owner) {
            if incoming[*index].admitted {
                return Err(EditError::AmbiguousScope);
            }
            continue;
        }
        let next = previous[position + 1..]
            .iter()
            .find_map(|run| new.get(&(run.owner?, run.occurrence)).copied());
        let preceding = previous[..position]
            .iter()
            .rev()
            .find_map(|run| new.get(&(run.owner?, run.occurrence)).copied());
        let anchor = match (preceding, next) {
            (Some(previous), Some(next)) if previous >= next => {
                return Err(EditError::AmbiguousScope);
            }
            (_, Some(next)) => next,
            (Some(previous), None) => previous + 1,
            (None, None) if incoming.iter().any(|run| run.admitted) => {
                return Err(EditError::AmbiguousScope);
            }
            (None, None) => 0,
        };
        retained[anchor].push(run.bytes.clone());
    }
    let mut piece = Vec::new();
    for (position, retained) in retained.iter().enumerate() {
        for bytes in retained {
            piece.push((before, bytes.clone()));
        }
        let Some(run) = incoming.get(position) else {
            break;
        };
        if run.admitted {
            if old
                .get(&(run.owner.expect("indexed source owner"), run.occurrence))
                .is_some_and(|index| !previous[*index].admitted)
            {
                return Err(EditError::AmbiguousScope);
            }
            piece.push((after, run.bytes.clone()));
        } else if let Some(index) =
            old.get(&(run.owner.expect("indexed source owner"), run.occurrence))
        {
            if previous[*index].admitted {
                return Err(EditError::AmbiguousScope);
            }
            piece.push((before, previous[*index].bytes.clone()));
        }
    }
    let length = piece.iter().try_fold(0usize, |length, (_, bytes)| {
        length
            .checked_add(bytes.len())
            .ok_or(EditError::SizeOverflow)
    })?;
    let mut content = Vec::new();
    content
        .try_reserve_exact(length)
        .map_err(|error| EditError::Allocation(error.to_string()))?;
    for (source, bytes) in piece {
        content.extend_from_slice(&source.bytes()[bytes]);
    }
    Ok(content)
}

impl AppliedEdit {
    pub fn after(&self) -> &SourceVersion {
        &self.after
    }

    pub fn retained_bytes(&self) -> usize {
        self.selection.retained_bytes() + self.after.bytes().len()
    }

    /// Reverses this local transition using its original bytes, without recomputing a diff.
    pub fn inverse(&self) -> EditSelection {
        let mut offset: isize = 0;
        let replacement = self
            .selection
            .replacement
            .iter()
            .map(|replacement| {
                let start = replacement
                    .range
                    .start
                    .checked_add_signed(offset)
                    .expect("validated edit offset");
                offset += replacement.replacement.len() as isize - replacement.range.len() as isize;
                ByteReplacement {
                    range: start..start + replacement.replacement.len(),
                    source: self.selection.before.clone(),
                    replacement: replacement.range.clone(),
                }
            })
            .collect();
        EditSelection {
            before: self.after.clone(),
            replacement,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        raw::compute_hunks,
        source::{Representation, SourcePair},
    };

    fn comparison(before: &str, after: &str) -> RawDiff {
        compute_hunks(SourcePair {
            old: SourceVersion::new(before.as_bytes().to_vec(), Representation::GitCanonical)
                .unwrap(),
            new: SourceVersion::new(after.as_bytes().to_vec(), Representation::GitCanonical)
                .unwrap(),
        })
    }

    #[test]
    fn every_subset_preserves_unselected_bytes_and_inverts_exactly() {
        for terminator in ["\n", "\r\n"] {
            for final_newline in [false, true] {
                let before = (0..12)
                    .map(|line| format!("line {line}"))
                    .collect::<Vec<_>>();
                let mut after = before.clone();
                after[1] = "replacement α".into();
                after[5] = "two\ninserted".replace('\n', terminator);
                after.remove(9);
                let suffix = if final_newline { terminator } else { "" };
                let before = before.join(terminator) + suffix;
                let after = after.join(terminator) + suffix;
                let analysis = comparison(&before, &after);
                assert_eq!(analysis.hunks().len(), 3);
                for mask in 0..8 {
                    let selected: Vec<_> = analysis
                        .hunks()
                        .iter()
                        .enumerate()
                        .filter(|(index, _)| mask & (1 << index) != 0)
                        .map(|(_, hunk)| hunk.id)
                        .collect();
                    let captured =
                        EditSelection::capture(&analysis, &selected, EditDirection::Forward)
                            .unwrap();
                    let applied = captured.apply().unwrap();
                    assert_eq!(
                        applied.inverse().apply().unwrap().after().bytes(),
                        before.as_bytes()
                    );
                    let mut reference = before.as_bytes().to_vec();
                    for hunk in analysis
                        .hunks()
                        .iter()
                        .rev()
                        .filter(|hunk| selected.contains(&hunk.id))
                    {
                        reference.splice(
                            hunk.old_bytes.clone(),
                            after.as_bytes()[hunk.new_bytes.clone()].iter().copied(),
                        );
                    }
                    assert_eq!(applied.after().bytes(), reference);
                }
            }
        }
    }

    #[test]
    fn pending_edits_survive_insertions_and_rollback_without_new_hunk_ids() {
        let analysis = comparison("a\nb\nc\nd\ne\nf\n", "a\ninsert\nb\nc\nd\nchanged\nf\n");
        assert_eq!(analysis.hunks().len(), 2);
        let first =
            EditSelection::capture(&analysis, &[analysis.hunks()[0].id], EditDirection::Forward)
                .unwrap();
        let second =
            EditSelection::capture(&analysis, &[analysis.hunks()[1].id], EditDirection::Forward)
                .unwrap();
        let applied = first.apply().unwrap();
        let rebased = second.rebase_over(&applied).unwrap();
        assert_eq!(
            rebased.apply().unwrap().after().bytes(),
            analysis.source().new.bytes()
        );
        let reverted = applied.inverse().apply().unwrap();
        assert_eq!(
            rebased
                .rebase_over(&reverted)
                .unwrap()
                .apply()
                .unwrap()
                .after()
                .bytes(),
            second.apply().unwrap().after().bytes()
        );
        assert!(first.rebase_over(&applied).unwrap().is_empty());
    }

    #[test]
    fn foreign_and_conflicting_edits_cannot_change_selection() {
        let first = comparison("same\n", "first\n");
        let second = comparison("same\n", "second\n");
        assert!(matches!(
            EditSelection::capture(&first, &[second.hunks()[0].id], EditDirection::Forward),
            Err(EditError::UnknownHunk)
        ));
        let first =
            EditSelection::capture(&first, &[first.hunks()[0].id], EditDirection::Forward).unwrap();
        let second =
            EditSelection::capture(&second, &[second.hunks()[0].id], EditDirection::Forward)
                .unwrap();
        assert!(matches!(
            second.rebase_over(&first.apply().unwrap()),
            Err(EditError::Overlap)
        ));
    }

    #[test]
    fn creation_deletion_and_reverse_preserve_missing_final_newline() {
        for (before, after) in [("", "new"), ("old", ""), ("old\r\n", "new")] {
            let analysis = comparison(before, after);
            let selected: Vec<_> = analysis.hunks().iter().map(|hunk| hunk.id).collect();
            for (direction, expected) in [
                (EditDirection::Forward, after),
                (EditDirection::Reverse, before),
            ] {
                assert_eq!(
                    EditSelection::capture(&analysis, &selected, direction)
                        .unwrap()
                        .apply()
                        .unwrap()
                        .after()
                        .text(),
                    expected
                );
            }
        }
    }
}
