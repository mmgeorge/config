//! Captured byte replacements survive unrelated local edits without selecting a new diff hunk.

use std::{collections::HashSet, fmt, ops::Range};

use crate::{
    raw::{RawDiff, RawHunkId},
    source::{SourceError, SourceVersion},
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
    Overlap,
    Source(SourceError),
}

impl fmt::Display for EditError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnknownHunk => formatter.write_str("selected edit belongs to another comparison"),
            Self::DifferentSource => formatter.write_str("selected edit source changed externally"),
            Self::Overlap => formatter.write_str("selected edit overlaps a newer local change"),
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
        let replacement: Vec<_> = analysis.hunks().iter()
            .filter(|hunk| selected.contains(&hunk.id))
            .map(|hunk| {
                let (range, replacement) = match direction {
                    EditDirection::Forward => (hunk.old_bytes.clone(), hunk.new_bytes.clone()),
                    EditDirection::Reverse => (hunk.new_bytes.clone(), hunk.old_bytes.clone()),
                };
                ByteReplacement { range, source: source.clone(), replacement }
            })
            .collect();
        if replacement.len() != selected.len() {
            return Err(EditError::UnknownHunk);
        }
        Ok(Self { before: before.clone(), replacement })
    }

    pub fn before(&self) -> &SourceVersion { &self.before }

    pub fn is_empty(&self) -> bool { self.replacement.is_empty() }

    pub fn retained_bytes(&self) -> usize {
        let mut source = HashSet::from([self.before.identity()]);
        self.before.bytes().len() + self.replacement.iter().map(|replacement| {
            std::mem::size_of::<ByteReplacement>() + if source.insert(replacement.source.identity()) { replacement.source.bytes().len() } else { 0 }
        }).sum::<usize>()
    }

    /// Compares exact replacements after both selections reach the same source version.
    pub fn same_edit(&self, other: &Self) -> bool {
        self.before.identity() == other.before.identity()
            && self.replacement.len() == other.replacement.len()
            && self.replacement.iter().zip(&other.replacement).all(|(left, right)| {
                left.range == right.range
                    && left.source.bytes()[left.replacement.clone()] == right.source.bytes()[right.replacement.clone()]
            })
    }

    /// Applies only the captured replacements, preserving every unselected byte and line ending.
    pub fn apply(&self) -> Result<AppliedEdit, EditError> {
        if self.is_empty() {
            return Ok(AppliedEdit { selection: self.clone(), after: self.before.clone() });
        }
        let size = self.replacement.iter().fold(self.before.bytes().len(), |size, replacement| {
            size - replacement.range.len() + replacement.replacement.len()
        });
        if size > crate::source::MAX_SOURCE_BYTES {
            return Err(EditError::Source(SourceError::TooLarge {
                bytes: size, limit: crate::source::MAX_SOURCE_BYTES,
            }));
        }
        let mut content = Vec::with_capacity(size);
        let mut position = 0;
        for replacement in &self.replacement {
            content.extend_from_slice(&self.before.bytes()[position..replacement.range.start]);
            content.extend_from_slice(&replacement.source.bytes()[replacement.replacement.clone()]);
            position = replacement.range.end;
        }
        content.extend_from_slice(&self.before.bytes()[position..]);
        let after = SourceVersion::new(content, self.before.identity().representation)
            .map_err(EditError::Source)?;
        Ok(AppliedEdit { selection: self.clone(), after })
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
                if previous.range.is_empty() && selected.range.is_empty()
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
                selected.range = selected.range.start.checked_add_signed(offset).ok_or(EditError::Overlap)?
                    ..selected.range.end.checked_add_signed(offset).ok_or(EditError::Overlap)?;
                replacement.push(selected);
            }
        }
        Ok(Self { before: applied.after.clone(), replacement })
    }
}

impl AppliedEdit {
    pub fn after(&self) -> &SourceVersion { &self.after }

    pub fn retained_bytes(&self) -> usize { self.selection.retained_bytes() + self.after.bytes().len() }

    /// Reverses this local transition using its original bytes, without recomputing a diff.
    pub fn inverse(&self) -> EditSelection {
        let mut offset: isize = 0;
        let replacement = self.selection.replacement.iter().map(|replacement| {
            let start = replacement.range.start.checked_add_signed(offset).expect("validated edit offset");
            offset += replacement.replacement.len() as isize - replacement.range.len() as isize;
            ByteReplacement {
                range: start..start + replacement.replacement.len(),
                source: self.selection.before.clone(),
                replacement: replacement.range.clone(),
            }
        }).collect();
        EditSelection { before: self.after.clone(), replacement }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{raw::compute_hunks, source::{Representation, SourcePair}};

    fn comparison(before: &str, after: &str) -> RawDiff {
        compute_hunks(SourcePair {
            old: SourceVersion::new(before.as_bytes().to_vec(), Representation::GitCanonical).unwrap(),
            new: SourceVersion::new(after.as_bytes().to_vec(), Representation::GitCanonical).unwrap(),
        }).unwrap()
    }

    #[test]
    fn every_subset_preserves_unselected_bytes_and_inverts_exactly() {
        for terminator in ["\n", "\r\n"] {
            for final_newline in [false, true] {
                let before = (0..12).map(|line| format!("line {line}")).collect::<Vec<_>>();
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
                    let selected: Vec<_> = analysis.hunks().iter().enumerate()
                        .filter(|(index, _)| mask & (1 << index) != 0)
                        .map(|(_, hunk)| hunk.id).collect();
                    let captured = EditSelection::capture(&analysis, &selected, EditDirection::Forward).unwrap();
                    let applied = captured.apply().unwrap();
                    assert_eq!(applied.inverse().apply().unwrap().after().bytes(), before.as_bytes());
                    let mut reference = before.as_bytes().to_vec();
                    for hunk in analysis.hunks().iter().rev().filter(|hunk| selected.contains(&hunk.id)) {
                        reference.splice(hunk.old_bytes.clone(), after.as_bytes()[hunk.new_bytes.clone()].iter().copied());
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
        let first = EditSelection::capture(&analysis, &[analysis.hunks()[0].id], EditDirection::Forward).unwrap();
        let second = EditSelection::capture(&analysis, &[analysis.hunks()[1].id], EditDirection::Forward).unwrap();
        let applied = first.apply().unwrap();
        let rebased = second.rebase_over(&applied).unwrap();
        assert_eq!(rebased.apply().unwrap().after().bytes(), analysis.source().new.bytes());
        let reverted = applied.inverse().apply().unwrap();
        assert_eq!(rebased.rebase_over(&reverted).unwrap().apply().unwrap().after().bytes(), second.apply().unwrap().after().bytes());
        assert!(first.rebase_over(&applied).unwrap().is_empty());
    }

    #[test]
    fn foreign_and_conflicting_edits_cannot_change_selection() {
        let first = comparison("same\n", "first\n");
        let second = comparison("same\n", "second\n");
        assert!(matches!(EditSelection::capture(&first, &[second.hunks()[0].id], EditDirection::Forward), Err(EditError::UnknownHunk)));
        let first = EditSelection::capture(&first, &[first.hunks()[0].id], EditDirection::Forward).unwrap();
        let second = EditSelection::capture(&second, &[second.hunks()[0].id], EditDirection::Forward).unwrap();
        assert!(matches!(second.rebase_over(&first.apply().unwrap()), Err(EditError::Overlap)));
    }

    #[test]
    fn creation_deletion_and_reverse_preserve_missing_final_newline() {
        for (before, after) in [("", "new"), ("old", ""), ("old\r\n", "new")] {
            let analysis = comparison(before, after);
            let selected: Vec<_> = analysis.hunks().iter().map(|hunk| hunk.id).collect();
            for (direction, expected) in [(EditDirection::Forward, after), (EditDirection::Reverse, before)] {
                assert_eq!(EditSelection::capture(&analysis, &selected, direction).unwrap().apply().unwrap().after().text(), expected);
            }
        }
    }
}
