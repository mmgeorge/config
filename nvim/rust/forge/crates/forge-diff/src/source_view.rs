//! Demand-loaded source detail rows share exact coordinates with the Full presentation.

use std::collections::HashSet;
use std::ops::Range;
use std::sync::Arc;

use crate::cache::AnalysisHandle;
use crate::display::{
    ChunkLimits, DisplayChunk, DisplayCursor, DisplayHunk, DisplayRow, DisplayState,
};
use crate::presentation::RowChange;
use crate::raw::{RawDiff, RawHunkId};
use crate::semantic_view::SourceComparison;
use crate::source::{SourceCoordinate, SourceError, SourceIdentity, SourceSide};
use crate::view::DiffVisibility;

/// Signature text and change evidence supplied to the common declaration renderer.
pub struct DeclarationSignature {
    pub origin: SourceCoordinate,
    pub source_identity: SourceIdentity,
    pub identity: String,
    pub text: String,
    pub old_line: Option<usize>,
    pub new_line: Option<usize>,
    pub change: RowChange,
    pub signature_changed: bool,
    pub detail_changes: usize,
}

/// Bounded detail delivery for an entire comparison or one selected semantic declaration.
/// Creating a cursor formats no body rows and never reparses or reacquires sources.
#[derive(Clone)]
pub struct SourceViewCursor {
    cursor: DisplayCursor,
    comparison: Option<Arc<SourceComparison>>,
    extent: Option<[Option<Range<usize>>; 2]>,
    declaration: Option<usize>,
    unclassified: Option<Box<UnclassifiedScope>>,
    visibility: DiffVisibility,
}

#[derive(Clone)]
struct UnclassifiedScope {
    all_hunk: HashSet<RawHunkId>,
    public_hunk: HashSet<RawHunkId>,
    all_group: HashSet<usize>,
    public_group: HashSet<usize>,
}

impl SourceViewCursor {
    pub(crate) fn with_visibility(
        mut self,
        visibility: DiffVisibility,
    ) -> Result<Self, SourceError> {
        if visibility == DiffVisibility::Public && self.comparison.is_none() {
            return Err(SourceError::InconsistentMetadata);
        }
        self.visibility = visibility;
        Ok(self)
    }
    /// Preserve ordinary Full/All delivery without preparing semantic analysis.
    pub fn full(
        diff: AnalysisHandle,
        old: Option<&crate::syntax::SyntaxHandle>,
        new: Option<&crate::syntax::SyntaxHandle>,
    ) -> Self {
        let cursor = DisplayCursor::new(diff, 3);
        let cursor = if old.is_some() || new.is_some() {
            cursor.with_syntax_context(old, new)
        } else {
            cursor
        };
        Self {
            cursor,
            comparison: None,
            extent: None,
            declaration: None,
            unclassified: None,
            visibility: DiffVisibility::All,
        }
    }

    /// Select Full rows or one declaration's details by its comparison-local change index.
    ///
    /// Foreign comparisons return InconsistentIdentity. An invalid declaration index
    /// returns InconsistentMetadata. Neither error creates a partially usable cursor.
    pub fn new(
        diff: AnalysisHandle,
        comparison: Arc<SourceComparison>,
        declaration: Option<usize>,
        visibility: DiffVisibility,
    ) -> Result<Self, SourceError> {
        if diff.source().old.identity() != comparison.source.old.identity()
            || diff.source().new.identity() != comparison.source.new.identity()
        {
            return Err(SourceError::InconsistentIdentity);
        }
        let extent = declaration
            .map(|index| {
                let change = comparison
                    .change
                    .get(index)
                    .ok_or(SourceError::InconsistentMetadata)?;
                Ok::<_, SourceError>([
                    change.before.map(|index| {
                        let entry = &comparison.before.declaration[index];
                        entry.start_line - 1..entry.end_line
                    }),
                    change.after.map(|index| {
                        let entry = &comparison.after.declaration[index];
                        entry.start_line - 1..entry.end_line
                    }),
                ])
            })
            .transpose()?;
        let cursor = match &extent {
            Some([old, new]) => {
                DisplayCursor::within_source_ranges(diff, 3, old.clone(), new.clone())
            }
            None => DisplayCursor::new(diff, 3),
        };
        Ok(Self {
            cursor,
            comparison: Some(comparison),
            extent,
            declaration,
            unclassified: None,
            visibility,
        })
    }

    /// Deliver raw rows with no unique changed declaration. Unsupported sources and
    /// ambiguous same-line declarations remain accessible through this cursor.
    /// Unchanged context cannot create a fallback section without an unclassified edit.
    pub fn unclassified(
        diff: AnalysisHandle,
        comparison: Arc<SourceComparison>,
        visibility: DiffVisibility,
    ) -> Result<Self, SourceError> {
        let mut cursor = Self::new(diff.clone(), comparison.clone(), None, visibility)?;
        let mut scope = UnclassifiedScope::new(&diff, &comparison);
        let mut index = 0;
        while let Some(group) = cursor.cursor.group(index) {
            if group
                .raw_ids
                .iter()
                .any(|identity| scope.all_hunk.contains(identity))
            {
                scope.all_group.insert(index);
            }
            if group
                .raw_ids
                .iter()
                .any(|identity| scope.public_hunk.contains(identity))
            {
                scope.public_group.insert(index);
            }
            index += 1;
        }
        cursor.unclassified = Some(Box::new(scope));
        Ok(cursor)
    }

    /// Return a bounded batch, retaining continuation when a filtered batch is empty.
    /// Limits bound source work per call rather than the total accessible content.
    pub fn next_rows(&mut self, limits: ChunkLimits) -> DisplayChunk {
        if self
            .unclassified
            .as_ref()
            .is_some_and(|scope| match self.visibility {
                DiffVisibility::All => scope.all_hunk.is_empty(),
                DiffVisibility::Public => scope.public_hunk.is_empty(),
            })
        {
            return DisplayChunk {
                rows: Vec::new(),
                text_bytes: 0,
                decorations: 0,
                state: DisplayState::Complete,
            };
        }
        let mut chunk = self.cursor.next_rows(limits);
        chunk.rows.retain(|row| self.includes(row));
        chunk.text_bytes = chunk.rows.iter().map(|row| row.text.len() + 1).sum();
        chunk.decorations = chunk.rows.iter().map(|row| row.emphasis.len() + 1).sum();
        chunk
    }

    /// Resolve the selected row's display group without changing its raw edit identities.
    pub fn group(&self, index: usize) -> Option<&DisplayHunk> {
        self.cursor.group(index)
    }

    fn includes(&self, row: &DisplayRow) -> bool {
        let Some(comparison) = &self.comparison else {
            return true;
        };
        if !comparison.visible_row(row, self.visibility) {
            return false;
        }
        if self.unclassified.is_some() || self.declaration.is_some() {
            if row.is_gap() {
                return false;
            }
            let old = row
                .old_coordinate()
                .and_then(|coordinate| comparison.row_change(true, coordinate.line));
            let new = row
                .new_coordinate()
                .and_then(|coordinate| comparison.row_change(false, coordinate.line));
            let owner = match (old, new) {
                (Some(old), Some(new)) if old != new => None,
                (Some(owner), _) | (_, Some(owner)) => Some(owner),
                _ => None,
            };
            if let Some(scope) = &self.unclassified {
                let (hunk, group) = match self.visibility {
                    DiffVisibility::All => (&scope.all_hunk, &scope.all_group),
                    DiffVisibility::Public => (&scope.public_hunk, &scope.public_group),
                };
                return owner.is_none()
                    && group.contains(&row.group_index)
                    && row.raw_id.is_none_or(|identity| hunk.contains(&identity));
            }
            if owner != self.declaration {
                return false;
            }
        }
        let Some([old, new]) = &self.extent else {
            return true;
        };
        row.is_gap()
            || row.old_coordinate().is_some_and(|coordinate| {
                old.as_ref()
                    .is_some_and(|extent| extent.contains(&coordinate.line))
            })
            || row.new_coordinate().is_some_and(|coordinate| {
                new.as_ref()
                    .is_some_and(|extent| extent.contains(&coordinate.line))
            })
    }
}

impl SourceComparison {
    /// Prepare one declaration heading from original signature bytes, including documentation.
    /// A body-only change receives Modified without altering the signature-change evidence.
    /// Invalid indices return InconsistentMetadata. No implementation body text is copied.
    pub fn signature(&self, index: usize) -> Result<DeclarationSignature, SourceError> {
        let change = self
            .change
            .get(index)
            .ok_or(SourceError::InconsistentMetadata)?;
        let (inventory, source, position) = if let Some(index) = change.after {
            (&self.after, &self.source.new, index)
        } else if let Some(index) = change.before {
            (&self.before, &self.source.old, index)
        } else {
            return Err(SourceError::InconsistentMetadata);
        };
        let declaration = &inventory.declaration[position];
        let mut text = source.text()[declaration.signature_bytes.clone()]
            .trim_end()
            .to_owned();
        if source.bytes().get(declaration.detail_bytes.start) == Some(&b'{') {
            text.push_str(" {");
        }
        let row_change = match (change.before, change.after) {
            (None, Some(_)) => RowChange::Added,
            (Some(_), None) => RowChange::Removed,
            _ => RowChange::Modified,
        };
        Ok(DeclarationSignature {
            source_identity: source.identity(),
            origin: SourceCoordinate {
                side: if change.after.is_some() {
                    SourceSide::New
                } else {
                    SourceSide::Old
                },
                line: declaration.start_line - 1,
                byte_column: declaration.signature_bytes.start
                    - source.text()[..declaration.signature_bytes.start]
                        .rfind('\n')
                        .map_or(0, |column| column + 1),
            },
            identity: inventory.identity[position].clone(),
            text,
            old_line: change
                .before
                .map(|index| self.before.declaration[index].line),
            new_line: change.after.map(|index| self.after.declaration[index].line),
            change: row_change,
            signature_changed: change.signature_changed,
            detail_changes: change.detail_region.len(),
        })
    }
}

impl UnclassifiedScope {
    fn new(diff: &RawDiff, comparison: &SourceComparison) -> Self {
        let mut scope = Self {
            all_hunk: HashSet::new(),
            public_hunk: HashSet::new(),
            all_group: HashSet::new(),
            public_group: HashSet::new(),
        };
        for hunk in diff.hunks() {
            for (old_side, lines, inventory) in [
                (true, &hunk.old_lines, &comparison.before),
                (false, &hunk.new_lines, &comparison.after),
            ] {
                for line in lines.clone() {
                    if comparison.row_change(old_side, line).is_none() {
                        scope.all_hunk.insert(hunk.id);
                        if !comparison.classified
                            || inventory.public_rows.get(line).copied().unwrap_or(true)
                        {
                            scope.public_hunk.insert(hunk.id);
                        }
                    }
                }
            }
        }
        scope
    }
}
