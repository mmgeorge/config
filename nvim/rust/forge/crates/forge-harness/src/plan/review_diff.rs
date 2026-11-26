use anyhow::Result;
use forge_diff::cache::{AnalysisBytes, AnalysisCache, AnalysisKey, CacheLimits};
use forge_diff::display::DisplayHunk;
use forge_diff::display::{ChunkLimits, DisplayCursor, DisplayRow, DisplayState, RowKind};
use forge_diff::source::{Representation, SourceCoordinate, SourcePair, SourceSide, SourceVersion};
use forge_diff::syntax::{DeclarationFilter, HunkContext, SyntaxPoint, SyntaxRange};

use super::calls::CallPresentation;
use super::render::FileVisibility;

pub(super) struct ReviewSides {
    pub before: Option<std::sync::Arc<CallPresentation>>,
    pub after: Option<std::sync::Arc<CallPresentation>>,
}

impl ReviewSides {
    pub fn new(design: &super::DeclarationDesign, path: &str, destination: &str) -> Result<Self> {
        let parsed = design.parsed()?;
        let before = design
            .baseline
            .get(path)
            .map(|file| {
                super::calls::present(
                    path,
                    &file.text,
                    design
                        .baseline_calls
                        .get(path)
                        .map(Vec::as_slice)
                        .unwrap_or_default(),
                )
            })
            .transpose()?;
        let after = parsed
            .proposed
            .get(destination)
            .map(|text| {
                super::calls::present(
                    destination,
                    text,
                    parsed
                        .proposed_calls
                        .get(destination)
                        .map(Vec::as_slice)
                        .unwrap_or_default(),
                )
            })
            .transpose()?;
        Ok(Self { before, after })
    }
}

/// Declaration comparison with source coordinates retained for review navigation.
pub(super) struct FileDiff {
    /// Captured source path, absent for a new file.
    pub old_path: Option<String>,
    /// Proposed source path, absent for a removed file.
    pub new_path: Option<String>,
    /// Shared diff groups including structural context.
    pub hunk: Vec<ReviewHunk>,
    /// Matched callable ownership used by review-only modification styling.
    pub alignment: forge_diff::declaration_change::DeclarationAlignment,
}

impl FileDiff {
    /// Context and hunk boundaries do not make an otherwise hidden change reviewable.
    pub fn visibility(&self, sides: &ReviewSides) -> Result<FileVisibility> {
        let mut modes = [false; 8];
        for (path, source, kind) in [
            (
                self.old_path.as_deref(),
                sides.before.as_deref(),
                RowKind::Removed,
            ),
            (
                self.new_path.as_deref(),
                sides.after.as_deref(),
                RowKind::Added,
            ),
        ] {
            let (Some(path), Some(source)) = (path, source) else {
                continue;
            };
            let mut changed = self
                .hunk
                .iter()
                .flat_map(|hunk| &hunk.row)
                .filter(|row| row.kind() == kind && !row.text.trim().is_empty())
                .filter_map(|row| row.new_coordinate().or_else(|| row.old_coordinate()))
                .peekable();
            if changed.peek().is_none() {
                continue;
            }
            for filter in DeclarationFilter::modes() {
                let visible = &mut modes[filter.index()];
                if !*visible {
                    let rows = &super::calls::visibility(path, source, filter)?.rows;
                    *visible = changed
                        .clone()
                        .any(|coordinate| rows.get(coordinate.line) == Some(&true));
                }
            }
        }
        Ok(FileVisibility::Modes(modes))
    }

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
        let alignment = super::review_alignment::compare(path, destination, before, after)?;
        let diff = if alignment.anchors.is_empty() {
            forge_diff::raw::compute_hunks(source)
        } else {
            forge_diff::raw::compute_anchored_hunks(source, &alignment.anchors)?
        };
        let mut result = Self {
            old_path: before.map(|_| path.into()),
            new_path: after.map(|_| destination.into()),
            hunk: Vec::new(),
            alignment,
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
                        (row.kind() == RowKind::Removed || row.new_coordinate().is_none())
                            .then_some(row.old_coordinate())
                            .flatten()
                    } else {
                        row.new_coordinate()
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
                        source: forge_diff::display::DisplaySource::Gap,
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
                    source: forge_diff::display::DisplaySource::Context(coordinate),
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
