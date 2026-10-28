//! Demand-driven rows preserve raw targets while bounding each delivery independently.

use std::{
    collections::{BTreeMap, BTreeSet},
    ops::Range,
};

use crate::cache::AnalysisHandle;
use crate::intraline::{FallbackReason, IntralinePolicy, IntralineResult, compare_replacement};
use crate::raw::{RawDiff, RawHunkId};
use crate::source::{SourceCoordinate, SourceSide};
use crate::syntax::{HunkContext, SyntaxHandle, SyntaxRange};

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct DisplayHunk {
    pub raw_ids: Vec<RawHunkId>,
    pub old_lines: Range<usize>,
    pub new_lines: Range<usize>,
    pub(crate) raw_range: Range<usize>,
}

impl DisplayHunk {
    /// Describes caller-supplied context without associating executable raw edits.
    pub fn context(old_lines: Range<usize>, new_lines: Range<usize>) -> Self {
        Self {
            raw_ids: Vec::new(),
            old_lines,
            new_lines,
            raw_range: 0..0,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum RowKind {
    Context,
    Removed,
    Added,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
/// Coordinates carry the source lines required by each change kind.
pub enum RowCoordinates {
    Added { new_line: usize },
    Removed { old_line: usize },
    Context { old_line: usize, new_line: usize },
}

impl RowCoordinates {
    pub fn kind(self) -> RowKind {
        match self {
            Self::Added { .. } => RowKind::Added,
            Self::Removed { .. } => RowKind::Removed,
            Self::Context { .. } => RowKind::Context,
        }
    }

    pub fn old_line(self) -> Option<usize> {
        match self {
            Self::Added { .. } => None,
            Self::Removed { old_line } | Self::Context { old_line, .. } => Some(old_line),
        }
    }

    pub fn new_line(self) -> Option<usize> {
        match self {
            Self::Removed { .. } => None,
            Self::Added { new_line } | Self::Context { new_line, .. } => Some(new_line),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
/// Distinguishes paired diff rows, unchanged context from one source, and omission markers.
pub enum DisplaySource {
    Diff(RowCoordinates),
    Context(SourceCoordinate),
    Gap,
}

/// Text excludes diff gutters and source line terminators. Coordinates remain source-relative.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct DisplayRow {
    pub group_index: usize,
    pub text: String,
    pub source: DisplaySource,
    pub raw_id: Option<RawHunkId>,
    pub emphasis: Vec<Range<usize>>,
    pub emphasis_fallback: Option<FallbackReason>,
}

impl DisplayRow {
    pub fn kind(&self) -> RowKind {
        match self.source {
            DisplaySource::Diff(coordinates) => coordinates.kind(),
            DisplaySource::Context(_) | DisplaySource::Gap => RowKind::Context,
        }
    }

    pub fn old_coordinate(&self) -> Option<SourceCoordinate> {
        match self.source {
            DisplaySource::Diff(coordinates) => {
                coordinates.old_line().map(|line| SourceCoordinate {
                    side: SourceSide::Old,
                    line,
                    byte_column: 0,
                })
            }
            DisplaySource::Context(coordinate) if coordinate.side == SourceSide::Old => {
                Some(coordinate)
            }
            DisplaySource::Context(_) | DisplaySource::Gap => None,
        }
    }

    pub fn new_coordinate(&self) -> Option<SourceCoordinate> {
        match self.source {
            DisplaySource::Diff(coordinates) => {
                coordinates.new_line().map(|line| SourceCoordinate {
                    side: SourceSide::New,
                    line,
                    byte_column: 0,
                })
            }
            DisplaySource::Context(coordinate) if coordinate.side == SourceSide::New => {
                Some(coordinate)
            }
            DisplaySource::Context(_) | DisplaySource::Gap => None,
        }
    }

    /// Identifies an omission marker without source, navigation, or edit coordinates.
    pub fn is_gap(&self) -> bool {
        matches!(self.source, DisplaySource::Gap)
    }
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
    context: usize,
    structural: Vec<StructuralContext>,
    before_index: usize,
    after_index: usize,
    previous_group: Option<usize>,
    previous_old: Option<SourcePosition>,
    previous_new: Option<SourcePosition>,
    gap_emitted: bool,
}

#[derive(Default)]
struct StructuralContext {
    label: Option<String>,
    before: Vec<Candidate>,
    after: Vec<Candidate>,
}

#[derive(Default)]
struct ContextPlan {
    label: Option<String>,
    leading_scope: [Option<SyntaxRange>; 2],
    trailing_scope: [Option<SyntaxRange>; 2],
    selected: BTreeSet<(usize, usize)>,
}

impl ContextPlan {
    fn collect(
        diff: &RawDiff,
        group: &DisplayHunk,
        context_at: &impl Fn(SourceSide, usize) -> Option<HunkContext>,
    ) -> Self {
        let mut plan = Self::default();
        for (index, side) in [(0, SourceSide::Old), (1, SourceSide::New)] {
            for hunk in &diff.hunks()[group.raw_range.clone()] {
                let lines = if side == SourceSide::Old {
                    &hunk.old_lines
                } else {
                    &hunk.new_lines
                };
                for row in lines.clone() {
                    let Some(context) = context_at(side, row) else {
                        continue;
                    };
                    plan.leading_scope[index].get_or_insert(context.scope);
                    plan.trailing_scope[index] = Some(context.scope);
                    if plan.label.is_none() || side == SourceSide::New {
                        let source = match side {
                            SourceSide::Old => diff.source().old.text(),
                            SourceSide::New => diff.source().new.text(),
                        };
                        let start = source[..context.scope.start_byte]
                            .rfind('\n')
                            .map_or(0, |position| position + 1);
                        let end = source[start..]
                            .find('\n')
                            .map_or(source.len(), |position| start + position);
                        plan.label = Some(source[start..end].trim().to_owned());
                    }
                    for row in context.before.into_iter().chain(context.after) {
                        if let Some(coordinates) = unchanged_coordinates(diff, side, row) {
                            plan.selected.insert(coordinates);
                        }
                    }
                }
            }
        }
        plan
    }

    fn related(&self, next: &Self) -> bool {
        for side in [1, 0] {
            if let (Some(previous), Some(next)) =
                (self.trailing_scope[side], next.leading_scope[side])
            {
                return (previous.start_byte <= next.start_byte
                    && previous.end_byte >= next.end_byte)
                    || (next.start_byte <= previous.start_byte
                        && next.end_byte >= previous.end_byte);
            }
        }
        false
    }

    fn extend(&mut self, next: Self) {
        if next.label.is_some() {
            self.label = next.label;
        }
        for side in 0..2 {
            if next.trailing_scope[side].is_some() {
                self.trailing_scope[side] = next.trailing_scope[side];
            }
        }
        self.selected.extend(next.selected);
    }
}

#[derive(Clone, Copy, Default)]
struct SourcePosition {
    line: usize,
    byte: usize,
}

#[derive(Clone)]
struct Candidate {
    coordinates: RowCoordinates,
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
    /// Returns the source declaration introducing a group's innermost changed scope.
    pub fn context_label(&self, index: usize) -> Option<&str> {
        self.structural.get(index)?.label.as_deref()
    }

    /// Merges related scopes across at most twice the context radius between padded windows.
    /// Raw identities remain intact. Shared boundaries appear once, outside changed rows.
    pub fn with_syntax_context(
        self,
        old: Option<&SyntaxHandle>,
        new: Option<&SyntaxHandle>,
    ) -> Self {
        self.with_context(|side, row| match side {
            SourceSide::Old => old?.hunk_context(row),
            SourceSide::New => new?.hunk_context(row),
        })
    }

    /// Groups nearby changes and emits shared scope boundaries once per file.
    /// Context coordinates and byte ranges must address this cursor's source pair.
    pub fn with_context(
        mut self,
        context_at: impl Fn(SourceSide, usize) -> Option<HunkContext>,
    ) -> Self {
        let old_offset = source_offsets(self.diff.source().old.bytes());
        let new_offset = source_offsets(self.diff.source().new.bytes());
        let mut grouped: Vec<(DisplayHunk, ContextPlan)> = Vec::new();
        for next in std::mem::take(&mut self.group) {
            let context = ContextPlan::collect(&self.diff, &next, &context_at);
            if let Some((previous, previous_context)) = grouped.last_mut()
                && next.old_lines.start.saturating_sub(previous.old_lines.end)
                    <= self.context.saturating_mul(2)
                && next.new_lines.start.saturating_sub(previous.new_lines.end)
                    <= self.context.saturating_mul(2)
                && previous_context.related(&context)
            {
                previous.old_lines.end = previous.old_lines.end.max(next.old_lines.end);
                previous.new_lines.end = previous.new_lines.end.max(next.new_lines.end);
                previous.raw_range.end = next.raw_range.end;
                previous.raw_ids.extend(next.raw_ids);
                previous_context.extend(context);
            } else {
                grouped.push((next, context));
            }
        }
        let (group, mut plan): (Vec<_>, Vec<_>) = grouped.into_iter().unzip();
        self.group = group;
        let visible: Vec<_> = self
            .group
            .iter()
            .map(|group| {
                let first = &self.diff.hunks()[group.raw_range.start];
                let last = &self.diff.hunks()[group.raw_range.end - 1];
                (
                    first.old_lines.start..last.old_lines.end,
                    first.new_lines.start..last.new_lines.end,
                )
            })
            .collect();
        let mut owner = BTreeMap::new();
        // Shared opening rows belong to the first group and closing rows to the last.
        for (index, context) in plan.iter_mut().enumerate() {
            context.selected.retain(|&(old, new)| {
                let candidate = visible.partition_point(|(range, _)| range.end <= old);
                !visible
                    .get(candidate)
                    .is_some_and(|(old_range, new_range)| {
                        old_range.contains(&old) && new_range.contains(&new)
                    })
            });
            for &(old, new) in &context.selected {
                if old < visible[index].0.start && new < visible[index].1.start {
                    owner.entry((old, new)).or_insert(index);
                } else if old >= visible[index].0.end && new >= visible[index].1.end {
                    owner.insert((old, new), index);
                }
            }
        }
        self.structural = plan
            .into_iter()
            .enumerate()
            .map(|(index, plan)| {
                let mut context = StructuralContext {
                    label: plan.label,
                    ..Default::default()
                };
                for (old, new) in plan.selected {
                    if owner.get(&(old, new)) != Some(&index) {
                        continue;
                    }
                    let before = old < visible[index].0.start && new < visible[index].1.start;
                    let Some(&old_byte) = old_offset.get(old) else {
                        continue;
                    };
                    let Some(&new_byte) = new_offset.get(new) else {
                        continue;
                    };
                    let (bytes, advance_old) = line_range(self.diff.source().old.bytes(), old_byte);
                    let (_, advance_new) = line_range(self.diff.source().new.bytes(), new_byte);
                    let candidate = Candidate {
                        coordinates: RowCoordinates::Context {
                            old_line: old,
                            new_line: new,
                        },
                        raw_id: None,
                        bytes,
                        advance_old,
                        advance_new,
                        structural_before: Some(before),
                    };
                    if before {
                        context.before.push(candidate);
                    } else {
                        context.after.push(candidate);
                    }
                }
                context
            })
            .collect();
        self
    }

    pub fn group(&self, index: usize) -> Option<&DisplayHunk> {
        self.group.get(index)
    }
    /// Groups nearby edits while omitting ordinary context outside each group.
    /// Structural context and unchanged rows between grouped edits remain visible.
    pub fn new(diff: AnalysisHandle, context: usize) -> Self {
        let group = group_hunks(&diff, context);
        Self {
            diff,
            group,
            group_index: 0,
            raw_index: 0,
            old: SourcePosition::default(),
            new: SourcePosition::default(),
            initialized: false,
            terminal: None,
            emphasis: None,
            context,
            structural: Vec::new(),
            before_index: 0,
            after_index: 0,
            previous_group: None,
            previous_old: None,
            previous_new: None,
            gap_emitted: false,
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
        let mut cursor = Self::new(diff, context);
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

    /// Emit bounded batches while allowing one complete source line beyond the byte target.
    /// Preserve the cursor for a zero budget without splitting source text.
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
            if let Some((gap, continuation)) = self.gap_row(&candidate) {
                let charge = gap.text.len() + 1;
                if !chunk.rows.is_empty() && charge > byte_limit.saturating_sub(chunk.text_bytes) {
                    break;
                }
                if chunk.decorations == decoration_limit {
                    break;
                }
                if let Some((old, new)) = continuation {
                    self.previous_old = Some(old);
                    self.previous_new = Some(new);
                }
                self.gap_emitted = true;
                chunk.text_bytes += charge;
                chunk.decorations += 1;
                chunk.rows.push(gap);
                continue;
            }
            let source = if candidate.coordinates.kind() == RowKind::Added {
                self.diff.source().new.text()
            } else {
                self.diff.source().old.text()
            };
            let text = &source[candidate.bytes.clone()];
            let charge = text.len() + 1;
            if !chunk.rows.is_empty() && charge > byte_limit.saturating_sub(chunk.text_bytes) {
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
            let source = if candidate.coordinates.kind() == RowKind::Added {
                self.diff.source().new.text()
            } else {
                self.diff.source().old.text()
            };
            chunk.rows.push(DisplayRow {
                group_index: self.group_index,
                text: source[candidate.bytes.clone()].to_owned(),
                source: DisplaySource::Diff(candidate.coordinates),
                raw_id: candidate.raw_id,
                emphasis,
                emphasis_fallback,
            });
            chunk.text_bytes += charge;
            chunk.decorations += decoration_charge;
            if self.previous_group != Some(self.group_index) {
                self.previous_old = None;
                self.previous_new = None;
            }
            self.previous_group = Some(self.group_index);
            if let Some(old) = candidate.coordinates.old_line() {
                self.previous_old = Some(SourcePosition {
                    line: old,
                    byte: candidate.advance_old,
                });
            }
            if let Some(new) = candidate.coordinates.new_line() {
                self.previous_new = Some(SourcePosition {
                    line: new,
                    byte: candidate.advance_new,
                });
            }
            self.gap_emitted = false;
            if let Some(before) = candidate.structural_before {
                if before {
                    self.before_index += 1;
                } else {
                    self.after_index += 1;
                }
                continue;
            }
            if candidate.coordinates.old_line().is_some() {
                self.old.line += 1;
                self.old.byte = candidate.advance_old;
            }
            if candidate.coordinates.new_line().is_some() {
                self.new.line += 1;
                self.new.byte = candidate.advance_new;
            }
        }
        chunk
    }

    fn gap_row(
        &self,
        next: &Candidate,
    ) -> Option<(DisplayRow, Option<(SourcePosition, SourcePosition)>)> {
        if self.gap_emitted || self.previous_group != Some(self.group_index) {
            return None;
        }
        let distance = |previous: Option<SourcePosition>, next: Option<usize>| {
            previous
                .zip(next)
                .map(|(previous, next)| next.saturating_sub(previous.line + 1))
        };
        let old_gap = distance(self.previous_old, next.coordinates.old_line());
        let new_gap = distance(self.previous_new, next.coordinates.new_line());
        if old_gap.is_none_or(|gap| gap == 0) && new_gap.is_none_or(|gap| gap == 0) {
            return None;
        }
        let (text, source, continuation) =
            match (old_gap, new_gap, self.previous_old, self.previous_new) {
                (Some(1), Some(1), Some(old), Some(new)) => {
                    let (range, old_byte) = line_range(self.diff.source().old.bytes(), old.byte);
                    let new_byte = line_range(self.diff.source().new.bytes(), new.byte).1;
                    let old = SourcePosition {
                        line: old.line + 1,
                        byte: old_byte,
                    };
                    let new = SourcePosition {
                        line: new.line + 1,
                        byte: new_byte,
                    };
                    (
                        self.diff.source().old.text()[range].to_owned(),
                        DisplaySource::Diff(RowCoordinates::Context {
                            old_line: old.line,
                            new_line: new.line,
                        }),
                        Some((old, new)),
                    )
                }
                _ => {
                    let source = if next.coordinates.kind() == RowKind::Added {
                        self.diff.source().new.text()
                    } else {
                        self.diff.source().old.text()
                    };
                    let text = &source[next.bytes.clone()];
                    let indent = &text[..text.len() - text.trim_start().len()];
                    (format!("{indent}..."), DisplaySource::Gap, None)
                }
            };
        Some((
            DisplayRow {
                group_index: self.group_index,
                text,
                source,
                raw_id: None,
                emphasis: Vec::new(),
                emphasis_fallback: None,
            },
            continuation,
        ))
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
            .is_some_and(|(cached, _)| *cached != identity)
        {
            self.emphasis = None;
        }
        let (_, result) = self.emphasis.get_or_insert_with(|| {
            let policy = IntralinePolicy::default();
            let result = if hunk.old_lines.len() != hunk.new_lines.len() {
                IntralineResult::LineStyle(FallbackReason::UnpairedLines)
            } else if hunk.old_lines.len() > policy.line_pairs {
                IntralineResult::LineStyle(FallbackReason::PairLimit)
            } else if hunk.old_bytes.len() + hunk.new_bytes.len() > policy.input_bytes {
                IntralineResult::LineStyle(FallbackReason::ByteLimit)
            } else {
                let old = display_lines(&self.diff.source().old.text()[hunk.old_bytes.clone()]);
                let new = display_lines(&self.diff.source().new.text()[hunk.new_bytes.clone()]);
                compare_replacement(policy, &old, &new)
            };
            (identity, result)
        });
        match result {
            IntralineResult::LineStyle(reason) => (Vec::new(), Some(*reason)),
            IntralineResult::Emphasis(line) => {
                let spans = match candidate.coordinates {
                    RowCoordinates::Removed { old_line } => {
                        &line[old_line - hunk.old_lines.start].old
                    }
                    RowCoordinates::Added { new_line } => {
                        &line[new_line - hunk.new_lines.start].new
                    }
                    RowCoordinates::Context { .. } => return (Vec::new(), None),
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
                let old_start = first.old_lines.start;
                let new_start = first.new_lines.start;
                self.old.skip_to(self.diff.source().old.bytes(), old_start);
                self.new.skip_to(self.diff.source().new.bytes(), new_start);
                self.raw_index = group.raw_range.start;
                self.initialized = true;
            }
            if let Some(candidate) = self
                .structural
                .get(self.group_index)
                .and_then(|context| context.before.get(self.before_index))
            {
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
            } else {
                if let Some(candidate) = self
                    .structural
                    .get(self.group_index)
                    .and_then(|context| context.after.get(self.after_index))
                {
                    return Some(candidate.clone());
                }
                self.group_index += 1;
                self.initialized = false;
                self.before_index = 0;
                self.after_index = 0;
                continue;
            };
            let coordinates = match kind {
                RowKind::Added => RowCoordinates::Added {
                    new_line: self.new.line,
                },
                RowKind::Removed => RowCoordinates::Removed {
                    old_line: self.old.line,
                },
                RowKind::Context => RowCoordinates::Context {
                    old_line: self.old.line,
                    new_line: self.new.line,
                },
            };
            let (old_bytes, advance_old) = if coordinates.old_line().is_some() {
                line_range(self.diff.source().old.bytes(), self.old.byte)
            } else {
                (0..0, self.old.byte)
            };
            let (new_bytes, advance_new) = if coordinates.new_line().is_some() {
                line_range(self.diff.source().new.bytes(), self.new.byte)
            } else {
                (0..0, self.new.byte)
            };
            return Some(Candidate {
                coordinates,
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
    std::iter::once(0)
        .chain(memchr::memchr_iter(b'\n', source).map(|position| position + 1))
        .collect()
}

fn unchanged_coordinates(diff: &RawDiff, side: SourceSide, line: usize) -> Option<(usize, usize)> {
    let mut offset = 0isize;
    for hunk in diff.hunks() {
        let (selected, other) = match side {
            SourceSide::Old => (&hunk.old_lines, &hunk.new_lines),
            SourceSide::New => (&hunk.new_lines, &hunk.old_lines),
        };
        if selected.contains(&line) {
            return None;
        }
        if selected.end > line {
            break;
        }
        offset += other.len() as isize - selected.len() as isize;
    }
    let other = line.checked_add_signed(offset)?;
    Some(match side {
        SourceSide::Old => (line, other),
        SourceSide::New => (other, line),
    })
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

fn display_lines(content: &str) -> Vec<&str> {
    let mut line = Vec::new();
    let mut position = 0;
    while position < content.len() {
        let (range, next) = line_range(content.as_bytes(), position);
        line.push(&content[range]);
        position = next;
    }
    line
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
        assert_eq!(chunk.rows.len(), 2);
        assert_eq!(chunk.rows[0].old_coordinate().unwrap().line, 80);
        assert!(
            chunk.rows.iter().any(
                |row| row.text == "selected change" && row.new_coordinate().unwrap().line == 80
            )
        );
        let chunk = cursor.next_rows(ChunkLimits::default());
        assert!(chunk.rows.is_empty());
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
        let mut cursor = DisplayCursor::new(analysis, 3);
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
