//! Demand-driven rows preserve raw targets while bounding each delivery independently.

use std::ops::Range;

use crate::body::{BodyKind, BodyPolicy, UnavailableReason};
use crate::cache::AnalysisHandle;
use crate::intraline::{FallbackReason, IntralinePolicy, IntralineResult, compare_replacement};
use crate::raw::{RawDiff, RawHunkId};
use crate::source::{SourceCoordinate, SourceSide};
use crate::syntax::SyntaxHandle;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct DisplayHunk {
    pub raw_ids: Vec<RawHunkId>,
    pub old_lines: Range<usize>,
    pub new_lines: Range<usize>,
    pub(crate) raw_range: Range<usize>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum RowKind {
    Context,
    Removed,
    Added,
}

/// Text excludes diff gutters and source line terminators. Coordinates remain source-relative.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct DisplayRow {
    pub group_index: usize,
    pub text: String,
    pub kind: RowKind,
    pub old: Option<SourceCoordinate>,
    pub new: Option<SourceCoordinate>,
    pub raw_id: Option<RawHunkId>,
    pub emphasis: Vec<Range<usize>>,
    pub emphasis_fallback: Option<FallbackReason>,
}

#[derive(Clone, Copy, Debug)]
pub struct ChunkLimits {
    pub rows: usize,
    pub text_bytes: usize,
    pub decorations: usize,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum DisplayState {
    More,
    Complete,
    Unavailable(UnavailableReason),
}

#[derive(Debug)]
pub struct DisplayChunk {
    pub rows: Vec<DisplayRow>,
    pub text_bytes: usize,
    pub decorations: usize,
    pub state: DisplayState,
}

/// Owns one immutable analysis and fixed context setting for the entire delivery lifecycle.
pub struct DisplayCursor {
    diff: AnalysisHandle,
    group: Vec<DisplayHunk>,
    group_index: usize,
    raw_index: usize,
    old: SourcePosition,
    new: SourcePosition,
    initialized: bool,
    terminal: Option<DisplayState>,
    emphasis: Option<(RawHunkId, IntralineResult)>,
    compact_group_edges: bool,
    structural: Vec<StructuralContext>,
    before_index: usize,
    after_index: usize,
}

#[derive(Default)]
struct StructuralContext {
    label: Option<String>,
    before: Vec<Candidate>,
    after: Vec<Candidate>,
}

#[derive(Default)]
struct SourcePosition {
    line: usize,
    byte: usize,
}

#[derive(Clone)]
struct Candidate {
    kind: RowKind,
    old: Option<SourceCoordinate>,
    new: Option<SourceCoordinate>,
    raw_id: Option<RawHunkId>,
    bytes: Range<usize>,
    advance_old: usize,
    advance_new: usize,
    structural_before: Option<bool>,
}

impl Default for ChunkLimits {
    fn default() -> Self {
        Self {
            rows: 256,
            text_bytes: 128 * 1024,
            decorations: 8192,
        }
    }
}

impl DisplayCursor {
    pub fn context_label(&self, index: usize) -> Option<&str> {
        self.structural.get(index)?.label.as_deref()
    }

    /// Adds source-bound parent rows without changing the raw membership of any display group.
    /// Only unchanged rows are shared as context. Changes in other groups retain their own targets.
    pub fn with_syntax_context(mut self, old: Option<&SyntaxHandle>, new: Option<&SyntaxHandle>) -> Self {
        let old_offset = source_offsets(self.diff.source().old.bytes());
        let new_offset = source_offsets(self.diff.source().new.bytes());
        self.structural = self.group.iter().map(|group| {
            let mut context = StructuralContext::default();
            let mut selected = std::collections::BTreeSet::new();
            for hunk in &self.diff.hunks()[group.raw_range.clone()] {
                for (side, syntax, lines) in [(SourceSide::Old, old, &hunk.old_lines), (SourceSide::New, new, &hunk.new_lines)] {
                    for row in lines.clone() {
                        let Some(scope) = syntax.and_then(|syntax| syntax.hunk_context(row)) else { continue; };
                        if context.label.is_none() || side == SourceSide::New { context.label = Some(scope.label); }
                        for row in scope.before.into_iter().chain(scope.after) {
                            if let Some((old, new)) = unchanged_coordinates(&self.diff, side, row) {
                                selected.insert((old, new));
                            }
                        }
                    }
                }
            }
            let first = &self.diff.hunks()[group.raw_range.start];
            let last = &self.diff.hunks()[group.raw_range.end - 1];
            for (old, new) in selected {
                let before = old < first.old_lines.start && new < first.new_lines.start;
                let after = old >= last.old_lines.end && new >= last.new_lines.end;
                if !before && !after { continue; }
                let Some(&old_byte) = old_offset.get(old) else { continue; };
                let Some(&new_byte) = new_offset.get(new) else { continue; };
                let (bytes, advance_old) = line_range(self.diff.source().old.bytes(), old_byte);
                let (_, advance_new) = line_range(self.diff.source().new.bytes(), new_byte);
                let candidate = Candidate {
                    kind: RowKind::Context,
                    old: Some(SourceCoordinate { side: SourceSide::Old, line: old, byte_column: 0 }),
                    new: Some(SourceCoordinate { side: SourceSide::New, line: new, byte_column: 0 }),
                    raw_id: None, bytes, advance_old, advance_new, structural_before: Some(before),
                };
                if before { context.before.push(candidate); } else { context.after.push(candidate); }
            }
            context
        }).collect();
        self
    }

    pub fn group(&self, index: usize) -> Option<&DisplayHunk> {
        self.group.get(index)
    }
    pub fn new(diff: AnalysisHandle, context: usize, kind: BodyKind) -> Self {
        Self::with_group_edges(diff, context, kind, false)
    }

    /// Groups nearby changes with the requested context radius while omitting
    /// unchanged rows before the first and after the last raw hunk in each group.
    pub fn compact(diff: AnalysisHandle, context: usize, kind: BodyKind) -> Self {
        Self::with_group_edges(diff, context, kind, true)
    }

    fn with_group_edges(
        diff: AnalysisHandle,
        context: usize,
        kind: BodyKind,
        compact_group_edges: bool,
    ) -> Self {
        let lines = match kind {
            BodyKind::Added | BodyKind::Modified => diff.source().new.newline().line_count,
            BodyKind::Deleted => diff.source().old.newline().line_count,
        };
        let terminal = BodyPolicy::default()
            .may_preview_full_file(kind, lines)
            .err()
            .map(DisplayState::Unavailable);
        let group = if terminal.is_none() {
            group_hunks(&diff, context)
        } else {
            Vec::new()
        };
        Self {
            diff,
            group,
            group_index: 0,
            raw_index: 0,
            old: SourcePosition::default(),
            new: SourcePosition::default(),
            initialized: false,
            terminal,
            emphasis: None,
            compact_group_edges,
            structural: Vec::new(),
            before_index: 0,
            after_index: 0,
        }
    }

    /// Selects the nearest changed group without projecting preceding groups.
    /// Row delivery remains bounded by `ChunkLimits`, including large added files.
    pub fn at_source_line(
        diff: AnalysisHandle,
        context: usize,
        side: SourceSide,
        line: usize,
    ) -> Self {
        let mut cursor = Self::new(diff, context, BodyKind::Modified);
        let selected = cursor
            .group
            .iter()
            .min_by_key(|group| {
                let range = match side {
                    SourceSide::Old => &group.old_lines,
                    SourceSide::New => &group.new_lines,
                };
                if line < range.start {
                    range.start - line
                } else {
                    line.saturating_sub(range.end.saturating_sub(1))
                }
            })
            .cloned();
        cursor.group = selected.into_iter().collect();
        cursor
    }

    /// Emits at most the requested limits, capped by the protocol's initial engineering limits.
    /// A zero budget preserves the cursor. An oversized line terminates without splitting it.
    pub fn next_rows(&mut self, limits: ChunkLimits) -> DisplayChunk {
        let row_limit = limits.rows.min(256).min(limits.decorations.min(8192));
        let byte_limit = limits.text_bytes.min(128 * 1024);
        let decoration_limit = limits.decorations.min(8192);
        let mut chunk = DisplayChunk {
            rows: Vec::new(),
            text_bytes: 0,
            decorations: 0,
            state: self.terminal.clone().unwrap_or(DisplayState::More),
        };
        if self.terminal.is_some() || row_limit == 0 || byte_limit == 0 {
            return chunk;
        }
        while chunk.rows.len() < row_limit {
            let Some(candidate) = self.next_candidate() else {
                self.terminal = Some(DisplayState::Complete);
                chunk.state = DisplayState::Complete;
                return chunk;
            };
            let source = if candidate.kind == RowKind::Added {
                self.diff.source().new.bytes()
            } else {
                self.diff.source().old.bytes()
            };
            let text = &source[candidate.bytes.clone()];
            let charge = text.len() + 1;
            if charge > byte_limit {
                let state = DisplayState::Unavailable(UnavailableReason::LineBytes {
                    coordinate: candidate
                        .old
                        .or(candidate.new)
                        .expect("every row has a source coordinate"),
                    line_bytes: text.len(),
                    limit: byte_limit,
                });
                self.terminal = Some(state.clone());
                chunk.state = state;
                return chunk;
            }
            if charge > byte_limit - chunk.text_bytes {
                break;
            }
            let (mut emphasis, mut emphasis_fallback) = self.row_emphasis(&candidate);
            if emphasis.len() + 1 > decoration_limit {
                emphasis.clear();
                emphasis_fallback = Some(FallbackReason::DecorationLimit);
            }
            let decoration_charge = emphasis.len() + 1;
            if decoration_charge > decoration_limit - chunk.decorations {
                break;
            }
            let source = if candidate.kind == RowKind::Added {
                self.diff.source().new.bytes()
            } else {
                self.diff.source().old.bytes()
            };
            chunk.rows.push(DisplayRow {
                group_index: self.group_index,
                text: std::str::from_utf8(&source[candidate.bytes.clone()])
                    .expect("validated source line")
                    .to_owned(),
                kind: candidate.kind,
                old: candidate.old,
                new: candidate.new,
                raw_id: candidate.raw_id,
                emphasis,
                emphasis_fallback,
            });
            chunk.text_bytes += charge;
            chunk.decorations += decoration_charge;
            if let Some(before) = candidate.structural_before {
                if before { self.before_index += 1; } else { self.after_index += 1; }
                continue;
            }
            if candidate.old.is_some() {
                self.old.line += 1;
                self.old.byte = candidate.advance_old;
            }
            if candidate.new.is_some() {
                self.new.line += 1;
                self.new.byte = candidate.advance_new;
            }
        }
        chunk
    }

    fn row_emphasis(
        &mut self,
        candidate: &Candidate,
    ) -> (Vec<Range<usize>>, Option<FallbackReason>) {
        let Some(identity) = candidate.raw_id else {
            return (Vec::new(), None);
        };
        let hunk = &self.diff.hunks()[self.raw_index];
        if self
            .emphasis
            .as_ref()
            .is_none_or(|(cached, _)| *cached != identity)
        {
            let policy = IntralinePolicy::default();
            let result = if hunk.old_lines.len() != hunk.new_lines.len() {
                IntralineResult::LineStyle(FallbackReason::UnpairedLines)
            } else if hunk.old_lines.len() > policy.line_pairs {
                IntralineResult::LineStyle(FallbackReason::PairLimit)
            } else if hunk.old_bytes.len() + hunk.new_bytes.len() > policy.input_bytes {
                IntralineResult::LineStyle(FallbackReason::ByteLimit)
            } else {
                let old = display_lines(&self.diff.source().old.bytes()[hunk.old_bytes.clone()]);
                let new = display_lines(&self.diff.source().new.bytes()[hunk.new_bytes.clone()]);
                compare_replacement(policy, &old, &new)
            };
            self.emphasis = Some((identity, result));
        }
        match &self
            .emphasis
            .as_ref()
            .expect("current hunk emphasis is initialized")
            .1
        {
            IntralineResult::LineStyle(reason) => (Vec::new(), Some(*reason)),
            IntralineResult::Emphasis(line) => {
                let spans = if candidate.kind == RowKind::Removed {
                    &line[candidate.old.expect("removed source coordinate").line
                        - hunk.old_lines.start]
                        .old
                } else {
                    &line[candidate.new.expect("added source coordinate").line
                        - hunk.new_lines.start]
                        .new
                };
                (spans.clone(), None)
            }
        }
    }

    fn next_candidate(&mut self) -> Option<Candidate> {
        loop {
            let group = self.group.get(self.group_index)?;
            if !self.initialized {
                let first = &self.diff.hunks()[group.raw_range.start];
                let old_start = if self.compact_group_edges {
                    first.old_lines.start
                } else {
                    group.old_lines.start
                };
                let new_start = if self.compact_group_edges {
                    first.new_lines.start
                } else {
                    group.new_lines.start
                };
                self.old.skip_to(self.diff.source().old.bytes(), old_start);
                self.new.skip_to(self.diff.source().new.bytes(), new_start);
                self.raw_index = group.raw_range.start;
                self.initialized = true;
            }
            if let Some(candidate) = self.structural.get(self.group_index).and_then(|context| context.before.get(self.before_index)) {
                return Some(candidate.clone());
            }
            let (kind, raw_id) = if self.raw_index < group.raw_range.end {
                let hunk = &self.diff.hunks()[self.raw_index];
                if self.old.line < hunk.old_lines.start {
                    (RowKind::Context, None)
                } else if self.old.line < hunk.old_lines.end {
                    (RowKind::Removed, Some(hunk.id))
                } else if self.new.line < hunk.new_lines.end {
                    (RowKind::Added, Some(hunk.id))
                } else {
                    self.raw_index += 1;
                    continue;
                }
            } else if !self.compact_group_edges && self.old.line < group.old_lines.end {
                (RowKind::Context, None)
            } else {
                if let Some(candidate) = self.structural.get(self.group_index).and_then(|context| context.after.get(self.after_index)) {
                    return Some(candidate.clone());
                }
                self.group_index += 1;
                self.initialized = false;
                self.before_index = 0;
                self.after_index = 0;
                continue;
            };
            let old = (kind != RowKind::Added).then_some(SourceCoordinate {
                side: SourceSide::Old,
                line: self.old.line,
                byte_column: 0,
            });
            let new = (kind != RowKind::Removed).then_some(SourceCoordinate {
                side: SourceSide::New,
                line: self.new.line,
                byte_column: 0,
            });
            let (old_bytes, advance_old) = if old.is_some() {
                line_range(self.diff.source().old.bytes(), self.old.byte)
            } else {
                (0..0, self.old.byte)
            };
            let (new_bytes, advance_new) = if new.is_some() {
                line_range(self.diff.source().new.bytes(), self.new.byte)
            } else {
                (0..0, self.new.byte)
            };
            return Some(Candidate {
                kind,
                old,
                new,
                raw_id,
                bytes: if kind == RowKind::Added {
                    new_bytes
                } else {
                    old_bytes
                },
                advance_old,
                advance_new,
                structural_before: None,
            });
        }
    }
}

fn source_offsets(source: &[u8]) -> Vec<usize> {
    std::iter::once(0).chain(memchr::memchr_iter(b'\n', source).map(|position| position + 1)).collect()
}

fn unchanged_coordinates(diff: &RawDiff, side: SourceSide, line: usize) -> Option<(usize, usize)> {
    let mut offset = 0isize;
    for hunk in diff.hunks() {
        let (selected, other) = match side {
            SourceSide::Old => (&hunk.old_lines, &hunk.new_lines),
            SourceSide::New => (&hunk.new_lines, &hunk.old_lines),
        };
        if selected.contains(&line) { return None; }
        if selected.end > line { break; }
        offset += other.len() as isize - selected.len() as isize;
    }
    let other = line.checked_add_signed(offset)?;
    Some(match side { SourceSide::Old => (line, other), SourceSide::New => (other, line) })
}

#[cfg(test)]
mod selected_group_tests {
    use super::*;
    use crate::{
        engine::{DiffEngine, DiffRequest},
        source::{Representation, SourcePair, SourceVersion},
        workers::WorkPriority,
    };

    #[tokio::test]
    async fn selected_group_keeps_absolute_coordinates_and_bounded_delivery() {
        let old = (0..100)
            .map(|line| format!("line {line}\n"))
            .collect::<String>();
        let new = old
            .replace("line 2\n", "first change\n")
            .replace("line 80\n", "selected change\n");
        let engine = DiffEngine::new(Default::default(), 1);
        let analysis = engine
            .compare(DiffRequest {
                source: SourcePair {
                    old: SourceVersion::new(old.into_bytes(), Representation::Raw).unwrap(),
                    new: SourceVersion::new(new.into_bytes(), Representation::Raw).unwrap(),
                },
                priority: WorkPriority::Visible,
            })
            .await
            .unwrap();
        let mut cursor = DisplayCursor::at_source_line(analysis, 3, SourceSide::New, 80);
        assert_eq!(cursor.group(0).unwrap().new_lines, 77..84);
        assert!(cursor.group(1).is_none());
        let chunk = cursor.next_rows(ChunkLimits {
            rows: 3,
            ..Default::default()
        });
        assert_eq!(chunk.rows.len(), 3);
        assert_eq!(chunk.rows[0].new.unwrap().line, 77);
        let chunk = cursor.next_rows(ChunkLimits::default());
        assert!(
            chunk
                .rows
                .iter()
                .any(|row| row.text == "selected change" && row.new.unwrap().line == 80)
        );
        assert!(!chunk.rows.iter().any(|row| row.text == "first change"));
        assert_eq!(chunk.state, DisplayState::Complete);
    }

    #[tokio::test]
    async fn compact_cursor_keeps_inter_hunk_context_and_omits_group_edges() {
        let old = (0..20)
            .map(|line| format!("line {line}\n"))
            .collect::<String>();
        let new = old
            .replace("line 1\n", "first change\n")
            .replace("line 5\n", "nearby change\n")
            .replace("line 18\n", "separate change\n");
        let engine = DiffEngine::new(Default::default(), 1);
        let analysis = engine
            .compare(DiffRequest {
                source: SourcePair {
                    old: SourceVersion::new(old.into_bytes(), Representation::Raw).unwrap(),
                    new: SourceVersion::new(new.into_bytes(), Representation::Raw).unwrap(),
                },
                priority: WorkPriority::Visible,
            })
            .await
            .unwrap();
        let mut cursor = DisplayCursor::compact(analysis, 3, BodyKind::Modified);
        let chunk = cursor.next_rows(ChunkLimits::default());
        let rows: Vec<_> = chunk.rows.iter().map(|row| row.text.as_str()).collect();
        assert_eq!(
            rows,
            [
                "line 1",
                "first change",
                "line 2",
                "line 3",
                "line 4",
                "line 5",
                "nearby change",
                "line 18",
                "separate change",
            ]
        );
        assert_eq!(chunk.state, DisplayState::Complete);
    }
}

impl SourcePosition {
    fn skip_to(&mut self, content: &[u8], target: usize) {
        while self.line < target {
            self.byte = line_range(content, self.byte).1;
            self.line += 1;
        }
    }
}

/// Merges touching context windows without changing the contributing raw identities.
pub fn group_hunks(diff: &RawDiff, context: usize) -> Vec<DisplayHunk> {
    let mut group: Vec<DisplayHunk> = Vec::new();
    for (index, hunk) in diff.hunks().iter().enumerate() {
        let old_lines = hunk.old_lines.start.saturating_sub(context)
            ..hunk
                .old_lines
                .end
                .saturating_add(context)
                .min(diff.source().old.newline().line_count);
        let new_lines = hunk.new_lines.start.saturating_sub(context)
            ..hunk
                .new_lines
                .end
                .saturating_add(context)
                .min(diff.source().new.newline().line_count);
        if let Some(previous) = group.last_mut()
            && old_lines.start <= previous.old_lines.end
            && new_lines.start <= previous.new_lines.end
        {
            previous.old_lines.end = previous.old_lines.end.max(old_lines.end);
            previous.new_lines.end = previous.new_lines.end.max(new_lines.end);
            previous.raw_range.end = index + 1;
            previous.raw_ids.push(hunk.id);
        } else {
            group.push(DisplayHunk {
                raw_ids: vec![hunk.id],
                old_lines,
                new_lines,
                raw_range: index..index + 1,
            });
        }
    }
    group
}

fn line_range(content: &[u8], start: usize) -> (Range<usize>, usize) {
    let end = content[start..]
        .iter()
        .position(|byte| *byte == b'\n')
        .map_or(content.len(), |offset| start + offset + 1);
    let mut text_end = end;
    if text_end > start && content[text_end - 1] == b'\n' {
        text_end -= 1;
        if text_end > start && content[text_end - 1] == b'\r' {
            text_end -= 1;
        }
    }
    (start..text_end, end)
}

fn display_lines(content: &[u8]) -> Vec<&str> {
    let mut line = Vec::new();
    let mut position = 0;
    while position < content.len() {
        let (range, next) = line_range(content, position);
        line.push(std::str::from_utf8(&content[range]).expect("validated source line"));
        position = next;
    }
    line
}
