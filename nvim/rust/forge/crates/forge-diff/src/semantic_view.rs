//! Exact-source declaration changes retain raw edit identity independently of display mode.

use crate::declaration_change::{DeclarationBoundary, match_declarations};
use crate::display::DisplayRow;
use crate::raw::{RawDiff, RawHunkId};
use crate::source::{SourceError, SourcePair};
use crate::syntax::{DeclarationVisibility, SourceDeclaration, SyntaxHandle, SyntaxLanguage};
use crate::view::DiffVisibility;
use std::collections::BTreeSet;
use std::ops::Range;

/// One changed declaration's old/new inventory indices and independent change evidence.
pub struct DeclarationChange {
    pub before: Option<usize>,
    pub after: Option<usize>,
    pub signature_changed: bool,
    pub details_changed: bool,
    /// Raw edit regions touching changed details. Parent aggregation deduplicates these ids.
    pub detail_region: Vec<RawHunkId>,
    pub public_detail_region: Vec<RawHunkId>,
    pub public_changed: bool,
}

/// Declaration evidence derived from an existing syntax handle without reparsing.
pub struct DeclarationInventory {
    pub declaration: Vec<SourceDeclaration>,
    pub identity: Vec<String>,
    pub public_rows: Vec<bool>,
    /// Unique deepest semantic declaration per physical source row. Ambiguous rows stay raw.
    pub row_owner: Vec<Option<usize>>,
    semantic: Vec<bool>,
    private_scope: Vec<bool>,
    public_prefix: Vec<usize>,
}

/// A changed declaration in source order, with its enclosing changed declaration.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SourceDeclarationNode {
    pub change: usize,
    pub parent: Option<usize>,
}

/// Immutable analysis reusable across visibility, presentation, and fold changes.
/// Unrecognized regions remain in the retained raw comparison and are never inferred unchanged.
pub struct SourceComparison {
    pub source: SourcePair,
    pub before: DeclarationInventory,
    pub after: DeclarationInventory,
    pub change: Vec<DeclarationChange>,
    /// Both nonempty source versions have syntax evidence. False requires raw fallback.
    pub classified: bool,
    before_change: Vec<Option<usize>>,
    after_change: Vec<Option<usize>>,
}

impl SourceComparison {
    /// Admit exact source rows independently of Full/Semantic presentation.
    /// Rows without classification remain visible under Public.
    pub fn visible_row(&self, row: &DisplayRow, visibility: DiffVisibility) -> bool {
        if visibility == DiffVisibility::All || !self.classified || row.is_gap() {
            return true;
        }
        row.old_coordinate().is_some_and(|coordinate| {
            self.before
                .public_rows
                .get(coordinate.line)
                .copied()
                .unwrap_or(true)
        }) || row.new_coordinate().is_some_and(|coordinate| {
            self.after
                .public_rows
                .get(coordinate.line)
                .copied()
                .unwrap_or(true)
        })
    }

    /// Classify source changes using previously parsed exact source versions.
    ///
    /// Missing syntax produces an empty inventory for that side. Foreign syntax returns
    /// InconsistentIdentity before any result is published. No parser or I/O runs here.
    pub fn analyze(
        diff: &RawDiff,
        old: Option<&SyntaxHandle>,
        new: Option<&SyntaxHandle>,
    ) -> Result<Self, SourceError> {
        for (syntax, source) in [(old, &diff.source().old), (new, &diff.source().new)] {
            if syntax.is_some_and(|syntax| syntax.source_identity() != source.identity()) {
                return Err(SourceError::InconsistentIdentity);
            }
        }
        let mut before = DeclarationInventory::from_syntax(old);
        let mut after = DeclarationInventory::from_syntax(new);
        let classified = (old.is_some() || diff.source().old.bytes().is_empty())
            && (new.is_some() || diff.source().new.bytes().is_empty());
        if !classified {
            return Ok(Self {
                source: diff.source().clone(),
                before,
                after,
                change: Vec::new(),
                classified,
                before_change: Vec::new(),
                after_change: Vec::new(),
            });
        }
        let pairs = match_declarations(&before.boundaries(), &after.boundaries());
        let mut paired_old = vec![None; before.declaration.len()];
        let mut paired_new = vec![false; after.declaration.len()];
        let old_visibility = before.public_rows.clone();
        let new_visibility = after.public_rows.clone();
        for pair in pairs {
            paired_old[pair.old_index] = Some(pair.new_index);
            paired_new[pair.new_index] = true;
            let old_entry = &before.declaration[pair.old_index];
            let new_entry = &after.declaration[pair.new_index];
            if !old_entry.syntax_valid || !new_entry.syntax_valid {
                continue;
            }
            let old_public = !before.private_scope[pair.old_index]
                && old_visibility
                    .get(old_entry.line - 1)
                    .copied()
                    .unwrap_or(true);
            let new_public = !after.private_scope[pair.new_index]
                && new_visibility
                    .get(new_entry.line - 1)
                    .copied()
                    .unwrap_or(true);
            if old_public && !new_public {
                after.private_scope[pair.new_index] = false;
                for row in new_entry.start_line - 1..new_entry.end_line {
                    if after.row_owner[row].is_none_or(|owner| owner == pair.new_index) {
                        after.public_rows[row] = true;
                    }
                }
            } else if new_public && !old_public {
                before.private_scope[pair.old_index] = false;
                for row in old_entry.start_line - 1..old_entry.end_line {
                    if before.row_owner[row].is_none_or(|owner| owner == pair.old_index) {
                        before.public_rows[row] = true;
                    }
                }
            }
        }
        for inventory in [&mut before, &mut after] {
            inventory.public_prefix = std::iter::once(0)
                .chain(inventory.public_rows.iter().scan(0, |count, visible| {
                    *count += usize::from(*visible);
                    Some(*count)
                }))
                .collect();
        }
        let mut occurrence = std::collections::HashMap::<&str, [usize; 2]>::new();
        for (side, inventory) in [&before, &after].into_iter().enumerate() {
            for identity in &inventory.identity {
                occurrence.entry(identity).or_default()[side] += 1;
            }
        }
        let mut change = Vec::new();
        for (index, paired) in paired_old.into_iter().enumerate() {
            if !before.semantic[index]
                || occurrence[before.identity[index].as_str()]
                    .iter()
                    .any(|count| *count > 1)
            {
                continue;
            }
            let entry = classify(
                diff,
                Some((&before, index)),
                paired.map(|index| (&after, index)),
            );
            if entry.signature_changed || entry.details_changed {
                change.push(entry);
            }
        }
        for (index, paired) in paired_new.into_iter().enumerate() {
            if !paired
                && after.semantic[index]
                && occurrence[after.identity[index].as_str()]
                    .iter()
                    .all(|count| *count <= 1)
            {
                let entry = classify(diff, None, Some((&after, index)));
                if entry.signature_changed || entry.details_changed {
                    change.push(entry);
                }
            }
        }
        let mut before_change = vec![None; before.declaration.len()];
        let mut after_change = vec![None; after.declaration.len()];
        for (index, entry) in change.iter().enumerate() {
            if let Some(position) = entry.before {
                before_change[position] = Some(index);
            }
            if let Some(position) = entry.after {
                after_change[position] = Some(index);
            }
        }
        Ok(Self {
            source: diff.source().clone(),
            before,
            after,
            change,
            classified,
            before_change,
            after_change,
        })
    }

    /// Return changed declarations in source order. Public retains structural ancestors
    /// of admitted children without admitting the ancestors' private implementation rows.
    pub fn declaration_tree(&self, visibility: DiffVisibility) -> Vec<SourceDeclarationNode> {
        let mut parent = Vec::with_capacity(self.change.len());
        let mut admitted = Vec::with_capacity(self.change.len());
        for entry in &self.change {
            let (inventory, position, mapping) = if let Some(position) = entry.after {
                (&self.after, position, &self.after_change)
            } else {
                (
                    &self.before,
                    entry.before.expect("changed declaration has a source"),
                    &self.before_change,
                )
            };
            let mut ancestor = inventory.declaration[position].parent;
            let enclosing = loop {
                let Some(position) = ancestor else { break None };
                if let Some(change) = mapping[position] {
                    break Some(change);
                }
                ancestor = inventory.declaration[position].parent;
            };
            parent.push(enclosing);
            admitted.push(visibility == DiffVisibility::All || entry.public_changed);
        }
        for index in 0..admitted.len() {
            if admitted[index] {
                let mut ancestor = parent[index];
                while let Some(index) = ancestor {
                    if admitted[index] {
                        break;
                    }
                    admitted[index] = true;
                    ancestor = parent[index];
                }
            }
        }
        let source_order = |index: &usize| {
            let entry = &self.change[*index];
            entry.after.map_or_else(
                || {
                    (
                        self.before.declaration[entry.before.unwrap()].start_line,
                        self.before.declaration[entry.before.unwrap()].depth,
                        *index,
                    )
                },
                |position| {
                    (
                        self.after.declaration[position].start_line,
                        self.after.declaration[position].depth,
                        *index,
                    )
                },
            )
        };
        let mut child = vec![Vec::new(); self.change.len()];
        let mut root = Vec::new();
        for (index, admitted) in admitted.into_iter().enumerate() {
            if admitted {
                if let Some(parent) = parent[index] {
                    child[parent].push(index);
                } else {
                    root.push(index);
                }
            }
        }
        root.sort_by_key(source_order);
        for child in &mut child {
            child.sort_by_key(source_order);
        }
        let mut pending = root.into_iter().rev().collect::<Vec<_>>();
        let mut ordered = Vec::new();
        while let Some(change) = pending.pop() {
            ordered.push(SourceDeclarationNode {
                change,
                parent: parent[change],
            });
            pending.extend(child[change].iter().rev().copied());
        }
        ordered
    }

    /// Resolve one physical row to its changed declaration. No owner means the raw
    /// fallback retains the row, including ambiguous siblings and unsupported regions.
    pub fn row_change(&self, old_side: bool, line: usize) -> Option<usize> {
        let (inventory, mapping) = if old_side {
            (&self.before, &self.before_change)
        } else {
            (&self.after, &self.after_change)
        };
        let owner = inventory.row_owner.get(line).copied().flatten()?;
        mapping.get(owner).copied().flatten()
    }
}

impl DeclarationInventory {
    fn from_syntax(syntax: Option<&SyntaxHandle>) -> Self {
        let declaration = syntax.map(SyntaxHandle::declarations).unwrap_or_default();
        let mut identity = Vec::<String>::with_capacity(declaration.len());
        for entry in &declaration {
            let parent = entry.parent.map_or("", |parent| identity[parent].as_str());
            identity.push(format!("{parent}/{}:{}", entry.kind, entry.name));
        }
        let mut semantic = Vec::with_capacity(declaration.len());
        let mut executable = Vec::with_capacity(declaration.len());
        let mut private_scope = Vec::with_capacity(declaration.len());
        for entry in &declaration {
            let inside_callable = entry.parent.is_some_and(|parent| executable[parent]);
            executable.push(entry.callable || inside_callable);
            let classified = entry.syntax_valid
                && (!inside_callable
                    || entry.callable
                    || !matches!(
                        entry.kind.as_str(),
                        "variable_declarator"
                            | "variable_declaration"
                            | "assignment_statement"
                            | "field"
                    ));
            semantic.push(classified);
            let entry_point = syntax
                .is_some_and(|syntax| syntax.language() == SyntaxLanguage::Rust)
                && entry.parent.is_none()
                && entry.callable
                && entry.name == "main";
            private_scope.push(
                entry.parent.is_some_and(|parent| private_scope[parent])
                    || classified && entry.visibility == "private" && !entry_point,
            );
        }
        let mut public_rows = syntax
            .map(|syntax| DeclarationVisibility::from_source(syntax, true).rows)
            .unwrap_or_default();
        let row_owner = row_owners(
            &declaration,
            &semantic,
            syntax
                .map(|syntax| syntax.source().text())
                .unwrap_or_default(),
        );
        for (row, owner) in row_owner.iter().enumerate() {
            if owner.is_some_and(|owner| private_scope[owner]) {
                public_rows[row] = false;
            }
        }
        Self {
            declaration,
            identity,
            public_rows,
            row_owner,
            semantic,
            private_scope,
            public_prefix: Vec::new(),
        }
    }

    fn visible_edit(&self, diff: &RawDiff, index: usize, old_side: bool, owner: usize) -> bool {
        if self.private_scope[owner] {
            return false;
        }
        let declaration = &self.declaration[owner];
        let hunk = &diff.hunks()[index];
        let lines = if old_side {
            &hunk.old_lines
        } else {
            &hunk.new_lines
        };
        let start = lines.start.max(declaration.start_line - 1);
        let end = lines.end.min(declaration.end_line);
        start < end && self.public_prefix[end] > self.public_prefix[start]
    }

    fn boundaries(&self) -> Vec<DeclarationBoundary<'_>> {
        self.declaration
            .iter()
            .zip(&self.identity)
            .map(|(entry, identity)| DeclarationBoundary {
                identity,
                lines: entry.start_line - 1..entry.end_line,
                signature: entry.line - 1,
            })
            .collect()
    }
}

fn row_owners(
    declaration: &[SourceDeclaration],
    semantic: &[bool],
    source: &str,
) -> Vec<Option<usize>> {
    let mut event = Vec::with_capacity(declaration.len() * 2);
    for (index, entry) in declaration
        .iter()
        .enumerate()
        .filter(|(index, _)| semantic[*index])
    {
        event.push((entry.start_line - 1, true, entry.depth, index));
        event.push((entry.end_line, false, entry.depth, index));
    }
    event.sort_unstable();
    let mut active = BTreeSet::new();
    let mut next = 0;
    let mut owner = Vec::new();
    let mut offset = 0;
    for (line, content) in source.split_inclusive('\n').enumerate() {
        while next < event.len() && event[next].0 <= line {
            let (_, opening, depth, index) = event[next];
            if opening {
                active.insert((depth, index));
            } else {
                active.remove(&(depth, index));
            }
            next += 1;
        }
        let mut deepest = active.iter().rev();
        owner.push(
            deepest
                .next()
                .and_then(|&(depth, index)| {
                    deepest
                        .next()
                        .is_none_or(|(other_depth, _)| *other_depth != depth)
                        .then_some(index)
                })
                .filter(|index| {
                    let extent = &declaration[*index].source_bytes;
                    let prefix_end = extent.start.saturating_sub(offset).min(content.len());
                    let suffix_start = extent.end.saturating_sub(offset).min(content.len());
                    content[..prefix_end].trim().is_empty()
                        && content[suffix_start..].trim().is_empty()
                }),
        );
        offset += content.len();
    }
    owner
}

fn classify(
    diff: &RawDiff,
    before: Option<(&DeclarationInventory, usize)>,
    after: Option<(&DeclarationInventory, usize)>,
) -> DeclarationChange {
    let old = before.map(|(inventory, index)| &inventory.declaration[index]);
    let new = after.map(|(inventory, index)| &inventory.declaration[index]);
    let unmatched_changed = old.is_some_and(|entry| {
        intersecting_edits(diff, &entry.source_bytes, true)
            .next()
            .is_some()
    }) || new.is_some_and(|entry| {
        intersecting_edits(diff, &entry.source_bytes, false)
            .next()
            .is_some()
    });
    let signature_changed = match (old, new) {
        (Some(old), Some(new)) => {
            old.visibility != new.visibility
                || diff.source().old.bytes()[old.signature_bytes.clone()]
                    != diff.source().new.bytes()[new.signature_bytes.clone()]
        }
        _ => unmatched_changed,
    };
    let details_changed = match (old, new) {
        (Some(old), Some(new)) => {
            diff.source().old.bytes()[old.detail_bytes.clone()]
                != diff.source().new.bytes()[new.detail_bytes.clone()]
        }
        (Some(entry), None) | (None, Some(entry)) => {
            unmatched_changed && !entry.detail_bytes.is_empty()
        }
        (None, None) => false,
    };
    let detail_region = if details_changed {
        let mut region = Vec::new();
        for (entry, old_side) in [(old, true), (new, false)] {
            if let Some(entry) = entry {
                region.extend(intersecting_edits(diff, &entry.detail_bytes, old_side));
            }
        }
        region.sort_unstable();
        region.dedup();
        region
            .into_iter()
            .map(|index| diff.hunks()[index].id)
            .collect()
    } else {
        Vec::new()
    };
    let mut public_detail_region = BTreeSet::new();
    let mut public_changed = false;
    for (source, old_side) in [(before, true), (after, false)] {
        if let Some((inventory, index)) = source {
            let declaration = &inventory.declaration[index];
            public_changed |= intersecting_edits(diff, &declaration.source_bytes, old_side)
                .any(|region| inventory.visible_edit(diff, region, old_side, index));
            if details_changed {
                public_detail_region.extend(
                    intersecting_edits(diff, &declaration.detail_bytes, old_side)
                        .filter(|region| inventory.visible_edit(diff, *region, old_side, index)),
                );
            }
        }
    }
    DeclarationChange {
        before: before.map(|(_, index)| index),
        after: after.map(|(_, index)| index),
        signature_changed,
        details_changed,
        detail_region,
        public_detail_region: public_detail_region
            .into_iter()
            .map(|index| diff.hunks()[index].id)
            .collect(),
        public_changed,
    }
}

pub(crate) fn intersecting_edits<'source>(
    diff: &'source RawDiff,
    source: &'source Range<usize>,
    old_side: bool,
) -> impl Iterator<Item = usize> + 'source {
    let range = move |hunk: &crate::raw::RawHunk| {
        if old_side {
            hunk.old_bytes.clone()
        } else {
            hunk.new_bytes.clone()
        }
    };
    let first = diff
        .hunks()
        .partition_point(|hunk| range(hunk).end <= source.start);
    diff.hunks()
        .iter()
        .enumerate()
        .skip(first)
        .take_while(move |(_, hunk)| range(hunk).start < source.end)
        .filter(move |(_, hunk)| !range(hunk).is_empty() && source.start < range(hunk).end)
        .map(|(index, _)| index)
}
