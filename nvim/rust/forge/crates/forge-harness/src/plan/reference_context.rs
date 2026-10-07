use std::collections::{HashMap, HashSet};

use anyhow::{Context, Result};
use forge_buffer::block::Decoration;
use forge_buffer::width::WidthProfile;
use serde::Serialize;

use super::references::PlanReference;
use super::review_source::PlanReviewSource;
use super::{PlanNavigationAnchor, PlanReviewTarget};

#[derive(Eq, Hash, PartialEq)]
enum ReferenceLocation {
    Declaration(String, String, u32),
    Call(String, String, String, String, String),
    Object(String),
}

impl ReferenceLocation {
    fn anchor(anchor: &PlanNavigationAnchor) -> Self {
        match &anchor.target {
            PlanReviewTarget::Declaration {
                path, side, line, ..
            } => Self::Declaration(path.clone(), side.clone(), *line),
            PlanReviewTarget::Call {
                path,
                side,
                owner,
                name,
                kind,
            } => Self::Call(
                path.clone(),
                side.clone(),
                owner.clone(),
                name.clone(),
                kind.label().into(),
            ),
            _ => Self::Object(anchor.json_path.clone()),
        }
    }

    fn reference(reference: &PlanReference) -> Self {
        if let Some(anchor) = &reference.anchor {
            return Self::Object(anchor.json_path.clone());
        }
        if matches!(
            reference.kind.as_str(),
            "call" | "property" | "callback" | "value"
        ) {
            Self::Call(
                reference.path.clone(),
                reference.side.clone(),
                reference.owner.clone(),
                reference.name.clone(),
                reference.kind.clone(),
            )
        } else {
            Self::Declaration(
                reference.path.clone(),
                reference.side.clone(),
                reference.line,
            )
        }
    }
}

#[derive(Serialize)]
struct ReferenceLine {
    text: String,
    text_highlight: Vec<Decoration>,
}

#[derive(Serialize)]
/// One semantic reference paired with its immutable, unfolded review line.
pub(super) struct ReferenceEntry<'entry> {
    #[serde(flatten)]
    reference: &'entry PlanReference,
    #[serde(flatten)]
    line: &'entry ReferenceLine,
}

/// Caches full-line syntax independently of review folds, annotations, and viewport widths.
pub(super) struct ReferenceContext {
    row: HashMap<ReferenceLocation, ReferenceLine>,
}

impl ReferenceContext {
    /// Project one saved side once, reusing the review's existing syntax handles.
    pub(super) fn build(source: &PlanReviewSource, baseline: bool) -> Result<Self> {
        let (blocks, targets) = if source.document.design.is_some() {
            let mut document = source.document.clone();
            let design = document.design.as_mut().unwrap();
            if baseline {
                design.proposed.clear();
                design.proposed_calls.clear();
            } else {
                design.baseline.clear();
                design.baseline_calls.clear();
            }
            design.moved.clear();
            super::design_review::project(
                &document,
                &WidthProfile::default(),
                &[],
                &HashMap::new(),
                None,
                &source.declaration_syntax,
                false,
                None,
                &HashSet::new(),
            )?
        } else {
            super::review_projection::project(
                source,
                &WidthProfile::default(),
                &[],
                &HashMap::new(),
                None,
            )?
        };
        let mut row = HashMap::new();
        for block in blocks {
            for target in &block.metadata.target {
                let anchor = targets
                    .get(&target.id)
                    .context("reference context target is unavailable")?;
                let position = target.range.start.row;
                let text = block
                    .text
                    .row(position)
                    .context("reference context row is unavailable")?;
                let text_highlight = block
                    .metadata
                    .visible_decoration
                    .iter()
                    .filter_map(|decoration| {
                        if decoration.range.start.row > position
                            || decoration.range.end.row < position
                        {
                            return None;
                        }
                        let mut decoration = decoration.clone();
                        decoration.range.start.column = if decoration.range.start.row == position {
                            decoration.range.start.column
                        } else {
                            0
                        };
                        decoration.range.end.column = if decoration.range.end.row == position {
                            decoration.range.end.column
                        } else {
                            text.len()
                        };
                        decoration.range.start.row = 0;
                        decoration.range.end.row = 0;
                        Some(decoration)
                    })
                    .collect();
                row.entry(ReferenceLocation::anchor(anchor))
                    .or_insert_with(|| ReferenceLine {
                        text: text.into(),
                        text_highlight,
                    });
            }
        }
        Ok(Self { row })
    }

    /// Borrow a reference's display line without changing its navigation identity.
    pub(super) fn entry<'entry>(
        &'entry self,
        reference: &'entry PlanReference,
    ) -> Result<ReferenceEntry<'entry>> {
        let line = self
            .row
            .get(&ReferenceLocation::reference(reference))
            .context("plan reference display line is unavailable")?;
        Ok(ReferenceEntry { reference, line })
    }
}
