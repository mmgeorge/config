use std::ops::Range;

use anyhow::{Context, Result, ensure};

/// Borrows the exact UTF-8 source against which an edit set is prepared.
pub(super) struct SourceRevision<'source>(&'source str);

pub(super) struct SourceReplacement<'replacement> {
    pub range: Range<usize>,
    pub expected: &'replacement str,
    pub replacement: &'replacement str,
}

struct SourceRange(Range<usize>);

struct PreparedReplacement<'replacement> {
    range: SourceRange,
    replacement: &'replacement str,
}

/// Owns validated, disjoint ranges and retains the source borrow until application.
pub(super) struct PreparedSourceEdits<'source, 'replacement> {
    source: SourceRevision<'source>,
    replacement: Vec<PreparedReplacement<'replacement>>,
}

impl<'source> SourceRevision<'source> {
    pub(super) fn new(source: &'source str) -> Self {
        Self(source)
    }

    pub(super) fn prepare_tokens<'replacement>(
        self,
        positions: impl IntoIterator<Item = (u32, u32)>,
        expected: &'replacement str,
        replacement: &'replacement str,
    ) -> Result<PreparedSourceEdits<'source, 'replacement>> {
        let mut lines = vec![0];
        lines.extend(
            self.0
                .bytes()
                .enumerate()
                .filter_map(|(offset, byte)| (byte == b'\n').then_some(offset + 1)),
        );
        let edits = positions
            .into_iter()
            .map(|(line, column)| {
                let line = line
                    .checked_sub(1)
                    .context("source line must be positive")?;
                let start = lines
                    .get(line as usize)
                    .context("source line is unavailable")?
                    .checked_add(column as usize)
                    .context("source column is outside the snapshot")?;
                let end = start
                    .checked_add(expected.len())
                    .context("source range is outside the snapshot")?;
                Ok(SourceReplacement {
                    range: start..end,
                    expected,
                    replacement,
                })
            })
            .collect::<Result<Vec<_>>>()?;
        self.prepare(edits)
    }

    pub(super) fn prepare<'replacement>(
        self,
        mut edits: Vec<SourceReplacement<'replacement>>,
    ) -> Result<PreparedSourceEdits<'source, 'replacement>> {
        edits.sort_by_key(|edit| edit.range.start);
        let mut previous_end = 0;
        let mut replacement = Vec::with_capacity(edits.len());
        for edit in edits {
            ensure!(edit.range.start >= previous_end, "source edits overlap");
            let selected = self
                .0
                .get(edit.range.clone())
                .context("source range is not valid UTF-8")?;
            ensure!(selected == edit.expected, "source token changed");
            previous_end = edit.range.end;
            replacement.push(PreparedReplacement {
                range: SourceRange(edit.range),
                replacement: edit.replacement,
            });
        }
        Ok(PreparedSourceEdits {
            source: self,
            replacement,
        })
    }
}

impl PreparedSourceEdits<'_, '_> {
    pub(super) fn apply(self) -> String {
        let mut output = String::with_capacity(self.source.0.len());
        let mut cursor = 0;
        for edit in self.replacement {
            output.push_str(&self.source.0[cursor..edit.range.0.start]);
            output.push_str(edit.replacement);
            cursor = edit.range.0.end;
        }
        output.push_str(&self.source.0[cursor..]);
        output
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn prepared_edits_preserve_unicode_and_reject_stale_or_overlapping_ranges() -> Result<()> {
        let source = "λ first\nλ second\n";
        let prepared =
            SourceRevision::new(source).prepare_tokens([(2, 0), (1, 0)], "λ", "alpha")?;
        assert_eq!(prepared.apply(), "alpha first\nalpha second\n");
        assert!(
            SourceRevision::new(source)
                .prepare_tokens([(0, 0)], "λ", "alpha")
                .is_err()
        );
        assert!(
            SourceRevision::new(source)
                .prepare_tokens([(1, 1)], "λ", "alpha")
                .is_err()
        );
        assert!(
            SourceRevision::new(source)
                .prepare_tokens([(1, 0)], "changed", "alpha")
                .is_err()
        );
        assert!(
            SourceRevision::new(source)
                .prepare(vec![
                    SourceReplacement {
                        range: 0..2,
                        expected: "λ",
                        replacement: "alpha"
                    },
                    SourceReplacement {
                        range: 0..2,
                        expected: "λ",
                        replacement: "beta"
                    },
                ])
                .is_err()
        );
        assert_eq!(source, "λ first\nλ second\n");
        Ok(())
    }
}
