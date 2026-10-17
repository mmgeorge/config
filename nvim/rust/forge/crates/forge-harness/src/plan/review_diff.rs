use anyhow::Result;
use forge_diff::cache::{AnalysisBytes, AnalysisCache, AnalysisKey, CacheLimits};
use forge_diff::display::{ChunkLimits, DisplayCursor, DisplayRow, DisplayState, RowKind};
use forge_diff::display::DisplayHunk;
use forge_diff::source::{Representation, SourceCoordinate, SourcePair, SourceSide, SourceVersion};
use forge_diff::syntax::{HunkContext, SyntaxPoint, SyntaxRange};

use super::calls::CallPresentation;

/// Declaration comparison with source coordinates retained for review navigation.
pub(super) struct FileDiff {
    /// Captured source path, absent for a new file.
    pub old_path: Option<String>,
    /// Proposed source path, absent for a removed file.
    pub new_path: Option<String>,
    /// Shared diff groups including structural context.
    pub hunk: Vec<ReviewHunk>,
}

impl FileDiff {
    /// Compares declarations with regular diff context and bounded reference excerpts.
    pub fn new(
        path: &str,
        destination: &str,
        before: Option<&CallPresentation>,
        after: Option<&CallPresentation>,
        revealed: &[(SourceSide, u32)],
    ) -> Result<Self> {
        let old = before
            .map(|source| source.declaration.text.as_str())
            .unwrap_or_default();
        let new = after
            .map(|source| source.declaration.text.as_str())
            .unwrap_or_default();
        let source = SourcePair {
            old: SourceVersion::new(old.as_bytes().to_vec(), Representation::DisplayOnly)?,
            new: SourceVersion::new(new.as_bytes().to_vec(), Representation::DisplayOnly)?,
        };
        let diff = forge_diff::raw::compute_hunks(source)?;
        let mut result = Self {
            old_path: before.map(|_| path.into()),
            new_path: after.map(|_| destination.into()),
            hunk: Vec::new(),
        };
        let old_context = before
            .map(|source| DeclarationContext::new(path, source))
            .transpose()?;
        let new_context = after
            .map(|source| DeclarationContext::new(destination, source))
            .transpose()?;
        let bytes = AnalysisBytes {
            source: diff.retained_source_bytes(),
            result: diff.retained_result_bytes(),
        };
        let mut cache = AnalysisCache::new(CacheLimits {
            entries: 1,
            source_bytes: bytes.source,
            result_bytes: bytes.result,
        });
        let key = AnalysisKey {
            old: diff.source().old.identity(),
            new: diff.source().new.identity(),
        };
        let reservation = cache
            .reserve(bytes)
            .map_err(|error| anyhow::anyhow!("retain declaration comparison: {error:?}"))?;
        let handle = cache
            .insert(key, reservation, diff)
            .map_err(|error| anyhow::anyhow!("retain declaration comparison: {error:?}"))?;
        let mut cursor = DisplayCursor::new(handle, 3).with_context(|side, row| match side {
            SourceSide::Old => old_context.as_ref()?.at(row),
            SourceSide::New => new_context.as_ref()?.at(row),
        });
        while let Some(group) = cursor.group(result.hunk.len()) {
            result.hunk.push(ReviewHunk {
                group: group.clone(),
                row: Vec::new(),
                contextual: false,
            });
        }
        loop {
            let chunk = cursor.next_rows(ChunkLimits::default());
            for row in chunk.rows {
                result.hunk[row.group_index].row.push(row);
            }
            if chunk.state == DisplayState::Complete {
                break;
            }
        }
        if let Some(before) = before {
            for hunk in &mut result.hunk {
                hunk.separate_function_sides(before, after);
            }
        }
        for (side, source, context) in [
            (SourceSide::Old, before, old_context.as_ref()),
            (SourceSide::New, after, new_context.as_ref()),
        ] {
            if !revealed.iter().any(|(selected, _)| *selected == side) {
                continue;
            }
            let Some(source) = source else {
                continue;
            };
            let lines = source.declaration.text.lines().collect::<Vec<_>>();
            let mut selected = std::collections::BTreeSet::new();
            for &(_, row) in revealed.iter().filter(|(selected, _)| *selected == side) {
                let row = row as usize;
                if row >= lines.len() {
                    continue;
                }
                selected.extend(row.saturating_sub(3)..(row + 4).min(lines.len()));
                if let Some(context) = context.and_then(|context| context.at(row)) {
                    selected.extend(context.before);
                    selected.extend(context.after);
                }
            }
            let displayed = result
                .hunk
                .iter()
                .flat_map(|hunk| &hunk.row)
                .filter_map(|row| {
                    let coordinate = if side == SourceSide::Old {
                        (row.kind == RowKind::Removed || row.new.is_none())
                            .then_some(row.old)
                            .flatten()
                    } else {
                        row.new
                    };
                    coordinate.map(|coordinate| coordinate.line)
                })
                .collect::<std::collections::HashSet<_>>();
            selected.retain(|line| !displayed.contains(line));
            let (Some(&first), Some(&last)) = (selected.first(), selected.last()) else {
                continue;
            };
            let mut rows = Vec::new();
            let mut previous = None;
            for line in selected {
                if previous.is_some_and(|previous| line > previous + 1) {
                    rows.push(DisplayRow {
                        group_index: result.hunk.len(),
                        text: "...".into(),
                        kind: RowKind::Context,
                        old: None,
                        new: None,
                        raw_id: None,
                        emphasis: Vec::new(),
                        emphasis_fallback: None,
                    });
                }
                let coordinate = SourceCoordinate {
                    side,
                    line,
                    byte_column: 0,
                };
                rows.push(DisplayRow {
                    group_index: result.hunk.len(),
                    text: lines[line].into(),
                    kind: RowKind::Context,
                    old: (side == SourceSide::Old).then_some(coordinate),
                    new: (side == SourceSide::New).then_some(coordinate),
                    raw_id: None,
                    emphasis: Vec::new(),
                    emphasis_fallback: None,
                });
                previous = Some(line);
            }
            let range = first..last + 1;
            let (old_lines, new_lines) = if side == SourceSide::Old {
                (range, 0..0)
            } else {
                (0..0, range)
            };
            result.hunk.push(ReviewHunk {
                group: DisplayHunk::context(old_lines, new_lines),
                row: rows,
                contextual: true,
            });
        }
        Ok(result)
    }
}

/// Source rows and coordinates for one declaration display group.
pub(super) struct ReviewHunk {
    pub contextual: bool,
    pub group: DisplayHunk,
    /// Display rows with shared intraline emphasis and absolute source coordinates.
    pub row: Vec<DisplayRow>,
}

impl ReviewHunk {
    fn separate_function_sides(&mut self, before: &CallPresentation, after: Option<&CallPresentation>) {
        // Shared context can align bodies from different functions. Separate their
        // source coordinates before folds consume contiguous display rows.
        let baseline = before.declaration.text.lines().collect::<Vec<_>>();
        for (&start, body) in &before.body {
            let first = body.heading;
            let end = body.end;
            let Some(opening) = self.row.iter().position(|row|
                row.old.is_some_and(|coordinate| coordinate.line == first)) else {
                continue;
            };
            let Some(closing) = self.row.iter().position(|row|
                row.old.is_some_and(|coordinate| coordinate.line == end)) else {
                continue;
            };
            if closing <= opening {
                continue;
            }
            let proposed = after.and_then(|after| after.body.iter().find(|(_, candidate)| candidate.owner == body.owner));
            let shares_boundaries = proposed.is_some_and(|(&proposed_start, proposed_body)| {
                self.row.iter().any(|row| row.old.is_some_and(|coordinate| coordinate.line == start)
                    && row.new.is_some_and(|coordinate| coordinate.line == proposed_start))
                    && self.row[closing].new.is_some_and(|coordinate| coordinate.line == proposed_body.end)
            });
            let crosses_declaration = self.row[opening..=closing].iter().any(|row| row.new.is_some_and(|coordinate|
                proposed.is_none_or(|(_, candidate)| !(candidate.heading..=candidate.end).contains(&coordinate.line))));
            if shares_boundaries && !crosses_declaration {
                continue;
            }
            if !self.row[opening..=closing].iter().any(|row| row.new.is_some()) {
                continue;
            }
            let insertion = self.row[..opening].iter().rposition(|row| row.old.is_some())
                .map_or(opening, |previous| previous + 1);
            let group_index = self.row[opening].group_index;
            let mut removed = std::collections::BTreeMap::new();
            let mut following = Vec::new();
            for mut row in self.row.drain(opening..=closing) {
                if row.old.is_some_and(|coordinate| (first..=end).contains(&coordinate.line)) {
                    let line = row.old.unwrap().line;
                    if row.new.is_some() {
                        let mut old = row.clone();
                        old.new = None;
                        removed.insert(line, old);
                        row.old = None;
                        following.push(row);
                    } else {
                        removed.insert(line, row);
                    }
                } else {
                    following.push(row);
                }
            }
            self.row.splice(opening..opening, following);
            let removed = (first..=end).map(|line| removed.remove(&line).unwrap_or_else(|| DisplayRow {
                group_index, text: baseline[line].into(), kind: RowKind::Context,
                old: Some(SourceCoordinate { side: SourceSide::Old, line, byte_column: 0 }),
                new: None, raw_id: None, emphasis: Vec::new(), emphasis_fallback: None,
            })).collect::<Vec<_>>();
            self.row.splice(insertion..insertion, removed);
        }
    }
}

struct DeclarationContext {
    scope: Vec<(usize, usize, usize)>,
    offset: Vec<usize>,
}

impl DeclarationContext {
    fn new(path: &str, source: &CallPresentation) -> Result<Self> {
        let mut scope = Vec::new();
        for fold in source.folds(path)? {
            if let (Some(&start), Some(&end), Some(&heading)) = (
                source.declaration_row.get(&fold.start),
                source.declaration_row.get(&fold.end),
                source.declaration_row.get(&fold.heading),
            ) {
                scope.push((start, end, heading));
            }
        }
        for (&start, body) in &source.body {
            scope.push((start, body.end, body.heading));
        }
        scope.sort_by_key(|&(start, end, _)| end - start);
        let mut offset = vec![0];
        offset.extend(
            source
                .declaration
                .text
                .match_indices('\n')
                .map(|(index, _)| index + 1),
        );
        if *offset.last().unwrap() != source.declaration.text.len() {
            offset.push(source.declaration.text.len());
        }
        Ok(Self { scope, offset })
    }

    fn at(&self, row: usize) -> Option<HunkContext> {
        let selected = self
            .scope
            .iter()
            .copied()
            .filter(|&(start, end, _)| start <= row && row <= end)
            .take(3)
            .collect::<Vec<_>>();
        let &(start, end, _) = selected.first()?;
        let range = |start, end| SyntaxRange {
            start_byte: self.offset[start],
            end_byte: self.offset[end + 1],
            start: SyntaxPoint {
                row: start,
                column: 0,
            },
            end: SyntaxPoint {
                row: end + 1,
                column: 0,
            },
        };
        Some(HunkContext {
            label: String::new(),
            scope: range(start, end),
            ancestor: selected
                .iter()
                .skip(1)
                .map(|&(start, end, _)| range(start, end))
                .collect(),
            before: selected
                .iter()
                .flat_map(|&(start, _, heading)| heading..=start)
                .filter(|&line| line < row)
                .collect(),
            after: selected
                .iter()
                .map(|&(_, end, _)| end)
                .filter(|&line| line > row)
                .collect(),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn reference_navigation_only_adds_the_selected_context() {
        let mut source = String::from("impl State {\n");
        for index in 0..1000 {
            source.push_str(&format!("  pub fn method_{index:04}(&self) -> u64;\n"));
        }
        source.push_str("}\n");
        let before = super::super::calls::present("state.rs", &source, &[]).unwrap();
        for modified in [false, true] {
            let proposal = if modified {
                source.replace("method_0500(&self) -> u64", "method_0500(&self) -> String")
            } else {
                source.clone()
            };
            let after = super::super::calls::present("state.rs", &proposal, &[]).unwrap();
            for side in [SourceSide::Old, SourceSide::New] {
                let selected = before
                    .declaration
                    .source
                    .iter()
                    .position(|position| position.is_some_and(|position| position.line == 252))
                    .unwrap() as u32;
                let diff = FileDiff::new(
                    "state.rs",
                    "state.rs",
                    Some(&before),
                    Some(&after),
                    &[(side, selected)],
                )
                .unwrap();
                let rows = diff
                    .hunk
                    .iter()
                    .flat_map(|hunk| &hunk.row)
                    .collect::<Vec<_>>();
                assert!(rows.len() < 40, "reference exposed {} rows", rows.len());
                assert!(rows.iter().any(|row| row.text.contains("method_0250")));
                assert!(!rows.iter().any(|row| row.text.contains("method_0900")));
                assert!(rows.iter().any(|row| row.text == "impl State {"));
            }
        }
    }

    #[test]
    fn nearby_methods_share_their_implementation_context() {
        let mut source = String::from("impl State {\n");
        for index in 0..50 {
            source.push_str(&format!("  pub fn method_{index:02}(&self) -> u64;\n"));
        }
        source.push_str("}\n");
        let proposed = source
            .replace("method_06(&self) -> u64", "method_06(&self) -> String")
            .replace("method_11(&self) -> u64", "method_11(&self) -> String")
            .replace("method_35(&self) -> u64", "method_35(&self) -> String");
        let before = super::super::calls::present("state.rs", &source, &[]).unwrap();
        let after = super::super::calls::present("state.rs", &proposed, &[]).unwrap();
        let diff = FileDiff::new("state.rs", "state.rs", Some(&before), Some(&after), &[]).unwrap();
        let rows = diff
            .hunk
            .iter()
            .flat_map(|hunk| &hunk.row)
            .collect::<Vec<_>>();
        assert_eq!(diff.hunk.len(), 2);
        assert_eq!(
            rows.iter().filter(|row| row.text == "impl State {").count(),
            1
        );
        assert_eq!(rows.iter().filter(|row| row.text == "}").count(), 1);
        assert!(
            diff.hunk[0]
                .row
                .iter()
                .any(|row| row.text.contains("method_06"))
        );
        assert!(
            diff.hunk[0]
                .row
                .iter()
                .any(|row| row.text.contains("method_11"))
        );
        assert!(
            diff.hunk[1]
                .row
                .iter()
                .any(|row| row.text.contains("method_35"))
        );
        assert!(!rows.iter().any(|row| row.text.contains("method_25")));
        for side in [SourceSide::Old, SourceSide::New] {
            let positions = rows
                .iter()
                .filter_map(|row| match side {
                    SourceSide::Old => row.old,
                    SourceSide::New => row.new,
                })
                .map(|coordinate| coordinate.line)
                .collect::<Vec<_>>();
            assert!(positions.windows(2).all(|pair| pair[0] < pair[1]));
        }
    }
}
