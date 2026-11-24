use super::{DeclarationDesign, PlanReviewTarget, RenderedPlan};
use anyhow::{Context, Result};
use std::collections::{BTreeSet, HashMap};
use std::sync::{Arc, OnceLock};

pub(super) type ReviewSymbolCache = Arc<OnceLock<super::review_symbols::NarrativeSymbols>>;

/// The saved navigation index owns canonical line numbers even before a body is loaded.
#[derive(Default)]
pub(crate) struct ReviewFiles {
    pub files: Vec<FileHeading>,
    pub loaded: BTreeSet<String>,
    by_path: HashMap<String, usize>,
    pub(super) symbols: ReviewSymbolCache,
}

#[derive(Clone)]
pub(crate) struct FileHeading {
    pub id: String,
    pub before: String,
    pub after: String,
    pub delta: Option<super::design::DeclarationDelta>,
    pub visibility: super::render::FileVisibility,
    pub rows: u32,
}

impl ReviewFiles {
    pub fn new(
        design: &DeclarationDesign,
        rendered: &RenderedPlan,
        expanded: &BTreeSet<String>,
    ) -> Result<Self> {
        let mut result = Self::default();
        let parsed = design.parsed()?;
        let line_count = rendered.markdown.lines().count();
        let mut end = super::design_review::test_source_range(rendered)?.0 as u32;
        // The blank separator immediately before Tests belongs to the section, not its last file.
        for anchor in rendered.navigation.anchor.iter().rev() {
            let PlanReviewTarget::File { path } = &anchor.target else {
                continue;
            };
            if !anchor.json_path.starts_with("/design/proposed/") {
                continue;
            }
            let before = design
                .moved
                .iter()
                .find(|(_, to)| *to == path)
                .map_or(path, |(from, _)| from);
            let id = format!("plan:design:file:{}", super::digest(before.as_bytes()));
            anyhow::ensure!(
                (1..=line_count).contains(&(anchor.line as usize)),
                "file heading is outside the saved plan"
            );
            let visibility = match rendered.navigation.file_visibility.get(path) {
                Some(visibility) => *visibility,
                None => {
                    // Older indexes lack visibility. Derive it once for this review from its saved sides.
                    let sides = super::review_diff::ReviewSides::new(design, before, path)?;
                    super::review_diff::FileDiff::new(
                        before,
                        path,
                        sides.before.as_deref(),
                        sides.after.as_deref(),
                        &[],
                    )?
                    .visibility(&sides)?
                }
            };
            result.files.push(FileHeading {
                id,
                before: before.clone(),
                after: path.clone(),
                delta: parsed.review_delta(design, before, path),
                visibility,
                rows: end
                    .checked_sub(anchor.line)
                    .context("invalid plan file range")?,
            });
            end = anchor.line;
        }
        result.files.reverse();
        for (index, file) in result.files.iter().enumerate() {
            result.by_path.insert(file.after.clone(), index);
            result.by_path.insert(file.before.clone(), index);
            if expanded.contains(&file.id) {
                result.loaded.insert(file.id.clone());
            }
        }
        for test in &design.document.tests {
            let id = format!("plan:tests:file:{}", super::digest(test.file.as_bytes()));
            if expanded.contains(&id) {
                result.loaded.insert(id);
            }
        }
        Ok(result)
    }

    /// Visible prose and value references use the complete declaration namespace.
    pub(super) fn symbols(
        &self,
        design: &DeclarationDesign,
        trace: Option<&super::review_source::ReviewTrace>,
    ) -> Result<&super::review_symbols::NarrativeSymbols> {
        if let Some(symbols) = self.symbols.get() {
            return Ok(symbols);
        }
        let started = std::time::Instant::now();
        let symbols =
            super::review_symbols::NarrativeSymbols::new(design, std::path::Path::new(""))?;
        if let Some(trace) = trace {
            trace.record(
                "plan.review.narrative_symbols",
                started.elapsed(),
                symbols.proposed.len(),
            );
        }
        Ok(self.symbols.get_or_init(|| symbols))
    }

    pub fn for_path(&self, path: &str) -> Option<&FileHeading> {
        self.by_path.get(path).map(|&index| &self.files[index])
    }
}
