//! Declaration identity matching shared by source comparisons and authored details.

use std::collections::{HashMap, HashSet};
use std::ops::Range;

use crate::display::{DisplayRow, RowKind};
use crate::raw::LineAnchor;
use crate::source::SourceError;

/// Borrowed declaration identity and zero-based, end-exclusive presentation extent.
/// Identities include lexical ownership and exclude occurrence ordinals.
pub struct DeclarationBoundary<'source> {
    pub identity: &'source str,
    pub lines: Range<usize>,
    pub signature: usize,
}

/// Corresponding indices in the old and new declaration inventories.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct DeclarationPair {
    pub old_index: usize,
    pub new_index: usize,
}

/// Ordered comparison boundaries and semantic revisions in the new presentation.
/// Signature markers indicate a changed declaration, including detail-only edits.
#[derive(Default)]
pub struct DeclarationAlignment {
    pub anchors: Vec<LineAnchor>,
    modified_signature: HashSet<usize>,
    modified_body: Vec<Range<usize>>,
}

impl DeclarationAlignment {
    /// Compare complete declaration extents after matching their lexical identities.
    ///
    /// Sources can contain exact implementation text or authored design details.
    /// Invalid line ranges return InconsistentMetadata without publishing partial output.
    /// This operation does not parse syntax or alter the authoritative source diff.
    pub fn compare(
        old_source: &str,
        new_source: &str,
        old: &[DeclarationBoundary<'_>],
        new: &[DeclarationBoundary<'_>],
    ) -> Result<Self, SourceError> {
        let old_lines = old_source.lines().collect::<Vec<_>>();
        let new_lines = new_source.lines().collect::<Vec<_>>();
        for (declaration, count) in [(old, old_lines.len()), (new, new_lines.len())] {
            if declaration.iter().any(|entry| {
                entry.lines.start >= entry.lines.end
                    || entry.lines.end > count
                    || !entry.lines.contains(&entry.signature)
            }) {
                return Err(SourceError::InconsistentMetadata);
            }
        }
        let mut result = Self::default();
        for pair in ordered_declarations(old, new) {
            let before = &old[pair.old_index];
            let after = &new[pair.new_index];
            result.anchors.extend([
                LineAnchor {
                    old: before.lines.start,
                    new: after.lines.start,
                },
                LineAnchor {
                    old: before.lines.end,
                    new: after.lines.end,
                },
            ]);
            if old_lines[before.lines.clone()] != new_lines[after.lines.clone()] {
                result.modified_signature.insert(after.signature);
                result.modified_body.push(after.lines.clone());
            }
        }
        Ok(result)
    }

    /// Classify changed declaration headings and added rows inside matched revisions.
    /// Source coordinates and raw edit categories remain unchanged.
    pub fn modified(&self, row: &DisplayRow) -> bool {
        row.new_coordinate().is_some_and(|coordinate| {
            let next = self
                .modified_body
                .partition_point(|scope| scope.end <= coordinate.line);
            self.modified_signature.contains(&coordinate.line)
                || row.kind() == RowKind::Added
                    && self
                        .modified_body
                        .get(next)
                        .is_some_and(|scope| scope.contains(&coordinate.line))
        })
    }
}

/// Match identities occurring exactly once on each side, retaining old source order.
/// Ambiguous identities remain unmatched, including overloads and repeated owners.
pub fn match_declarations(
    old: &[DeclarationBoundary<'_>],
    new: &[DeclarationBoundary<'_>],
) -> Vec<DeclarationPair> {
    let old_index = unique_index(old);
    let new_index = unique_index(new);
    let mut pairs = old
        .iter()
        .enumerate()
        .filter_map(|(index, declaration)| {
            (old_index.get(declaration.identity) == Some(&Some(index)))
                .then(|| new_index.get(declaration.identity).copied().flatten())
                .flatten()
                .map(|new_index| DeclarationPair {
                    old_index: index,
                    new_index,
                })
        })
        .collect::<Vec<_>>();
    pairs.sort_by_key(|pair| old[pair.old_index].lines.start);
    pairs
}

/// Select nonoverlapping ordered pairs for a line comparison's structural anchors.
/// Reordered and nested regions not admitted by the chain retain ordinary comparison.
/// Matching and chain selection use O(n log n) time and O(n) auxiliary storage.
pub fn ordered_declarations(
    old: &[DeclarationBoundary<'_>],
    new: &[DeclarationBoundary<'_>],
) -> Vec<DeclarationPair> {
    let pairs = match_declarations(old, new);
    let mut tails: Vec<usize> = Vec::new();
    let mut previous = vec![None; pairs.len()];
    for (index, pair) in pairs.iter().enumerate() {
        let start = new[pair.new_index].lines.start;
        let length = tails.partition_point(|&tail| new[pairs[tail].new_index].lines.start < start);
        if length > 0 {
            previous[index] = Some(tails[length - 1]);
        }
        if length == tails.len() {
            tails.push(index);
        } else {
            tails[length] = index;
        }
    }
    let mut selected = Vec::new();
    let mut cursor = tails.last().copied();
    while let Some(index) = cursor {
        selected.push(pairs[index]);
        cursor = previous[index];
    }
    selected.reverse();
    let mut old_end = 0;
    let mut new_end = 0;
    selected.retain(|pair| {
        let before = &old[pair.old_index].lines;
        let after = &new[pair.new_index].lines;
        if before.start < old_end || after.start < new_end {
            return false;
        }
        old_end = before.end;
        new_end = after.end;
        true
    });
    selected
}

fn unique_index<'source>(
    declaration: &[DeclarationBoundary<'source>],
) -> HashMap<&'source str, Option<usize>> {
    let mut index = HashMap::new();
    for (position, entry) in declaration.iter().enumerate() {
        index
            .entry(entry.identity)
            .and_modify(|position| *position = None)
            .or_insert(Some(position));
    }
    index
}

#[cfg(test)]
mod tests {
    use super::*;

    fn boundary(identity: &str, lines: Range<usize>) -> DeclarationBoundary<'_> {
        DeclarationBoundary {
            identity,
            signature: lines.start,
            lines,
        }
    }

    #[test]
    fn detail_only_edits_mark_unchanged_signature_without_changing_coordinates() {
        use crate::display::{DisplaySource, RowCoordinates};
        let old = "pub fn parse() {\n    old();\n}\n";
        let new = "pub fn parse() {\n    new();\n}\n";
        let declaration = [boundary("parse", 0..3)];
        let comparison =
            DeclarationAlignment::compare(old, new, &declaration, &declaration).unwrap();
        let heading = DisplayRow {
            group_index: 0,
            text: "pub fn parse() {".into(),
            source: DisplaySource::Diff(RowCoordinates::Context {
                old_line: 0,
                new_line: 0,
            }),
            raw_id: None,
            emphasis: Vec::new(),
            emphasis_fallback: None,
        };
        assert!(comparison.modified(&heading));
        assert_eq!(heading.kind(), RowKind::Context);
        assert!(heading.emphasis.is_empty());
        let unchanged =
            DeclarationAlignment::compare(old, old, &declaration, &declaration).unwrap();
        assert!(!unchanged.modified(&heading));
        let invalid = [boundary("parse", 0..4)];
        assert!(matches!(
            DeclarationAlignment::compare(old, new, &invalid, &declaration),
            Err(SourceError::InconsistentMetadata)
        ));
    }

    #[test]
    fn unique_owners_match_reordering_without_pairing_overloads() {
        let old = [
            boundary("overloaded", 0..1),
            boundary("overloaded", 1..2),
            boundary("Api.first", 2..5),
            boundary("Api.second", 5..8),
        ];
        let new = [
            boundary("overloaded", 0..1),
            boundary("Api.second", 1..4),
            boundary("Api.first", 4..7),
        ];
        assert_eq!(
            match_declarations(&old, &new),
            vec![
                DeclarationPair {
                    old_index: 2,
                    new_index: 2
                },
                DeclarationPair {
                    old_index: 3,
                    new_index: 1
                }
            ]
        );
        let ordered = ordered_declarations(&old, &new);
        assert_eq!(ordered.len(), 1);
        assert_eq!(
            ordered[0],
            DeclarationPair {
                old_index: 3,
                new_index: 1
            }
        );
    }

    #[test]
    fn nested_extents_cannot_create_crossing_anchors() {
        let old = [
            boundary("Api", 0..20),
            boundary("Api.first", 2..10),
            boundary("other", 20..25),
        ];
        let new = [
            boundary("Api", 0..30),
            boundary("Api.first", 2..15),
            boundary("other", 30..35),
        ];
        assert_eq!(
            ordered_declarations(&old, &new),
            vec![
                DeclarationPair {
                    old_index: 0,
                    new_index: 0
                },
                DeclarationPair {
                    old_index: 2,
                    new_index: 2
                }
            ]
        );
    }
}
