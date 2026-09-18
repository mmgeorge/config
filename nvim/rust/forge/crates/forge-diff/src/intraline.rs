//! Bounded replacement emphasis uses UTF-8 byte ranges without changing base diff backgrounds.

use std::ops::Range;

use imara_diff::{Algorithm, Diff, InternedInput};

#[derive(Clone, Copy, Debug)]
pub struct IntralinePolicy {
    pub line_pairs: usize,
    pub input_bytes: usize,
    pub output_spans: usize,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum FallbackReason {
    UnpairedLines,
    PairLimit,
    ByteLimit,
    SpanLimit,
    InvalidLine,
    DecorationLimit,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct LineEmphasis {
    pub old: Vec<Range<usize>>,
    pub new: Vec<Range<usize>>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum IntralineResult {
    Emphasis(Vec<LineEmphasis>),
    /// Retains removed/added line backgrounds without partial or misleading intraline spans.
    LineStyle(FallbackReason),
}

impl Default for IntralinePolicy {
    fn default() -> Self {
        Self {
            line_pairs: 64,
            input_bytes: 16 * 1024,
            output_spans: 256,
        }
    }
}

/// Compare adjacent removed/added runs in a saved hunk without pairing across context rows.
///
/// Each replacement uses the same admission limits as raw diffs. Unpaired or over-budget runs
/// retain line backgrounds with empty emphasis ranges. Returned indices match the source rows.
pub fn patch_emphasis(
    policy: IntralinePolicy,
    rows: &[crate::patch::PatchRow<'_>],
) -> Vec<Vec<Range<usize>>> {
    use crate::display::RowKind;
    let mut result = vec![Vec::new(); rows.len()];
    let mut position = 0;
    while position < rows.len() {
        if rows[position].kind != RowKind::Removed {
            position += 1;
            continue;
        }
        let removed_start = position;
        while position < rows.len() && rows[position].kind == RowKind::Removed {
            position += 1;
        }
        let added_start = position;
        while position < rows.len() && rows[position].kind == RowKind::Added {
            position += 1;
        }
        let removed_count = added_start - removed_start;
        if removed_count != position - added_start || removed_count > policy.line_pairs.min(64) {
            continue;
        }
        let old = rows[removed_start..added_start]
            .iter()
            .map(|row| row.text.trim_end_matches('\r'))
            .collect::<Vec<_>>();
        let new = rows[added_start..position]
            .iter()
            .map(|row| row.text.trim_end_matches('\r'))
            .collect::<Vec<_>>();
        if let IntralineResult::Emphasis(lines) = compare_replacement(policy, &old, &new) {
            for (index, line) in lines.into_iter().enumerate() {
                result[removed_start + index] = line.old;
                result[added_start + index] = line.new;
            }
        }
    }
    result
}

/// Pairs replacement rows positionally only when both sides have the same line count.
/// Admission precedes character indexing and comparison. Any output overflow discards the batch.
pub fn compare_replacement(policy: IntralinePolicy, old: &[&str], new: &[&str]) -> IntralineResult {
    if old.len() != new.len() {
        return IntralineResult::LineStyle(FallbackReason::UnpairedLines);
    }
    if old.len() > policy.line_pairs.min(64) {
        return IntralineResult::LineStyle(FallbackReason::PairLimit);
    }
    let mut remaining_bytes = policy.input_bytes.min(16 * 1024);
    for line in old.iter().chain(new) {
        let Some(remaining) = remaining_bytes.checked_sub(line.len()) else {
            return IntralineResult::LineStyle(FallbackReason::ByteLimit);
        };
        remaining_bytes = remaining;
        if line.contains(['\n', '\0']) {
            return IntralineResult::LineStyle(FallbackReason::InvalidLine);
        }
    }
    let mut remaining_spans = policy.output_spans.min(256);
    let mut emphasis = Vec::with_capacity(old.len());
    for (old, new) in old.iter().zip(new) {
        let mut input = InternedInput::<char>::default();
        input.update_before(old.chars());
        input.update_after(new.chars());
        let old_offset = character_offsets(old);
        let new_offset = character_offsets(new);
        let comparison = Diff::compute(Algorithm::Histogram, &input);
        let mut line = LineEmphasis {
            old: Vec::new(),
            new: Vec::new(),
        };
        for hunk in comparison.hunks() {
            let count = usize::from(!hunk.before.is_empty()) + usize::from(!hunk.after.is_empty());
            let Some(remaining) = remaining_spans.checked_sub(count) else {
                return IntralineResult::LineStyle(FallbackReason::SpanLimit);
            };
            remaining_spans = remaining;
            if !hunk.before.is_empty() {
                line.old.push(
                    old_offset[hunk.before.start as usize]..old_offset[hunk.before.end as usize],
                );
            }
            if !hunk.after.is_empty() {
                line.new.push(
                    new_offset[hunk.after.start as usize]..new_offset[hunk.after.end as usize],
                );
            }
        }
        emphasis.push(line);
    }
    IntralineResult::Emphasis(emphasis)
}

fn character_offsets(text: &str) -> Vec<usize> {
    let mut offset = text
        .char_indices()
        .map(|(position, _)| position)
        .collect::<Vec<_>>();
    offset.push(text.len());
    offset
}

#[cfg(test)]
mod patch_tests {
    use super::*;
    use crate::patch::UnifiedPatch;

    #[test]
    fn saved_replacements_do_not_pair_across_context_or_unmatched_runs() {
        let patch = UnifiedPatch::parse("--- a/file\n+++ b/file\n@@ -1,4 +1,5 @@\n-let café = 1;\n+let café = 2;\n context\n-removed\n+first\n+second\n tail\n").unwrap();
        let rows = &patch.file[0].hunk[0].row;
        let emphasis = patch_emphasis(IntralinePolicy::default(), rows);
        assert_eq!(
            emphasis[0]
                .iter()
                .map(|range| &rows[0].text[range.clone()])
                .collect::<String>(),
            "1"
        );
        assert_eq!(
            emphasis[1]
                .iter()
                .map(|range| &rows[1].text[range.clone()])
                .collect::<String>(),
            "2"
        );
        assert!(emphasis[2..].iter().all(Vec::is_empty));
        let policy = IntralinePolicy {
            input_bytes: 1,
            ..Default::default()
        };
        assert!(patch_emphasis(policy, rows).iter().all(Vec::is_empty));
    }
}
