use std::{
    collections::{HashMap, HashSet},
    sync::Arc,
};

use anyhow::{Context, Result};
use forge_diff::{
    cache::AnalysisHandle,
    edit::{EditDirection, EditSelection},
    raw::{RawDiff, RawHunkId},
    semantic_view::SourceComparison,
    syntax::SyntaxHandle,
    view::DiffVisibility,
    workers::{WorkBudget, WorkPriority},
};

use crate::{StatusSection, StatusService, document::FileTarget, mutation::ProjectedIndex};
use forge_buffer::{
    block::TargetRange,
    identity::{BlockId, DocumentRevision, TargetId},
};
use forge_git::snapshot::ObservedPath;

/// Per-hunk scopes prevent a raw selection from broadening another declaration's edits.
#[derive(Clone, Default)]
pub(crate) struct ChangeSelection {
    pub whole: bool,
    pub hunk: HashMap<RawHunkId, Option<HashSet<String>>>,
}

impl ChangeSelection {
    pub fn include(&mut self, raw: &[RawHunkId], declaration: Option<&str>) {
        for identity in raw {
            let scope = declaration.map(|identity| HashSet::from([identity.to_owned()]));
            match self.hunk.entry(*identity) {
                std::collections::hash_map::Entry::Vacant(entry) => {
                    entry.insert(scope);
                }
                std::collections::hash_map::Entry::Occupied(mut entry) => {
                    match (entry.get_mut(), scope) {
                        (Some(selected), Some(scope)) => selected.extend(scope),
                        (_, None) => {
                            entry.insert(None);
                        }
                        (None, Some(_)) => {}
                    }
                }
            }
        }
    }

    pub fn requires_comparison(&self, visibility: DiffVisibility) -> bool {
        visibility == DiffVisibility::Public
            || !self.whole && self.hunk.values().any(Option::is_some)
    }

    pub fn raw_ids(&self, analysis: &RawDiff) -> Vec<RawHunkId> {
        if self.whole {
            analysis.hunks().iter().map(|hunk| hunk.id).collect()
        } else {
            self.hunk.keys().copied().collect()
        }
    }

    pub fn capture(
        &self,
        analysis: &RawDiff,
        comparison: Option<&SourceComparison>,
        visibility: DiffVisibility,
        direction: EditDirection,
    ) -> Result<Option<EditSelection>> {
        if self.whole && visibility == DiffVisibility::All {
            return Ok(None);
        }
        let raw = self.raw_ids(analysis);
        if !self.requires_comparison(visibility) {
            return Ok(Some(EditSelection::capture(analysis, &raw, direction)?));
        }
        let comparison = comparison.context("selected source classification expired")?;
        let mut result: Option<EditSelection> = None;
        for identity in raw {
            let declaration = if self.whole {
                None
            } else {
                self.hunk[&identity]
                    .as_ref()
                    .map(|scope| scope.iter().map(String::as_str).collect::<Vec<_>>())
            };
            let captured = EditSelection::capture_scoped(
                analysis,
                &[identity],
                direction,
                comparison,
                declaration.as_deref(),
                visibility,
            )?;
            result = Some(match result {
                Some(previous) => previous.combine(captured)?,
                None => captured,
            });
        }
        if result.is_none() {
            result = Some(EditSelection::capture(analysis, &[], direction)?);
        }
        Ok(result)
    }
}

#[derive(Clone)]
pub(crate) struct SelectedSource {
    pub(crate) observed: ObservedPath,
    pub(crate) section: StatusSection,
    pub(crate) analysis: Option<AnalysisHandle>,
    pub(crate) comparison: Option<Arc<SourceComparison>>,
    pub(crate) old_syntax: Option<SyntaxHandle>,
    pub(crate) new_syntax: Option<SyntaxHandle>,
    pub(crate) prepared: Option<forge_git::snapshot::PreparedAnalysis>,
    pub(crate) projected: Option<ProjectedIndex>,
    pub(crate) generation: u64,
    pub(crate) body_revision: Option<DocumentRevision>,
    pub(crate) block: HashMap<BlockId, Vec<TargetRange>>,
    pub(crate) hunk: HashMap<RawHunkId, TargetId>,
    pub(crate) group: Vec<(crate::document::DisplayGroupTarget, Vec<RawHunkId>)>,
}

impl SelectedSource {
    pub(crate) async fn prepare_comparison(
        &mut self,
        service: &StatusService,
        repository: &Arc<forge_git::repository::RepositoryState>,
        whole: bool,
    ) -> Result<()> {
        if self.comparison.is_some() {
            return Ok(());
        }
        if self.analysis.is_none() {
            match service
                .load(
                    repository,
                    &self.observed.change,
                    self.section,
                    self.prepared.as_ref(),
                    self.projected.as_ref(),
                )
                .await
            {
                Ok((analysis, old, new, diagnostic)) => {
                    if let Some(diagnostic) = diagnostic {
                        anyhow::bail!("cannot classify a collapsed Public selection: {diagnostic}");
                    }
                    self.analysis = Some(analysis);
                    self.old_syntax = old;
                    self.new_syntax = new;
                }
                Err(error)
                    if whole
                        && error
                            .downcast_ref::<crate::source::SourceUnavailable>()
                            .is_some() =>
                {
                    return Ok(());
                }
                Err(error) => return Err(error),
            }
        }
        if let Some(language) = crate::source::language(&self.observed.change.path) {
            let analysis = self
                .analysis
                .as_ref()
                .context("selected analysis expired")?;
            for (source, syntax) in [
                (&analysis.source().old, &mut self.old_syntax),
                (&analysis.source().new, &mut self.new_syntax),
            ] {
                if syntax.is_none() {
                    *syntax = Some(
                        service
                            .syntax
                            .analyze(forge_diff::syntax::SyntaxRequest {
                                source: source.clone(),
                                language,
                                priority: WorkPriority::Foreground,
                                deadline: None,
                            })
                            .await
                            .map_err(|error| {
                                anyhow::anyhow!("selected source classification failed: {error:?}")
                            })?,
                    );
                }
            }
        }
        let analysis = self.analysis.clone().context("selected analysis expired")?;
        let old = self.old_syntax.clone();
        let new = self.new_syntax.clone();
        let permit = service
            .diff
            .analysis_pool()
            .reserve(
                WorkPriority::Foreground,
                WorkBudget::new(
                    analysis.retained_source_bytes() + analysis.retained_result_bytes(),
                    None,
                ),
            )
            .map_err(|error| {
                anyhow::anyhow!("selected source classification admission failed: {error:?}")
            })?;
        let ticket = permit.ticket();
        let (sender, receiver) = tokio::sync::oneshot::channel();
        permit.submit(move |_| {
            let result =
                SourceComparison::analyze(&analysis, old.as_ref(), new.as_ref()).map(Arc::new);
            let _ = sender.send(result);
        });
        let result = receiver
            .await
            .context("selected source classification worker ended");
        ticket.completed().await;
        self.comparison = Some(result??);
        Ok(())
    }

    pub(crate) fn capture(
        file: &FileTarget,
        hunk: &HashMap<TargetId, crate::document::HunkTarget>,
    ) -> Self {
        Self {
            observed: observed(file),
            section: file.section,
            analysis: file.projection.analysis().cloned(),
            comparison: file
                .source_view
                .as_ref()
                .and_then(|view| view.comparison().cloned()),
            old_syntax: file.old_syntax.clone(),
            new_syntax: file.new_syntax.clone(),
            prepared: file.prepared.clone(),
            projected: file.projected.clone(),
            generation: file.generation,
            body_revision: file.body.as_ref().map(|body| body.revision()),
            block: file
                .body
                .as_ref()
                .map(|body| {
                    body.snapshot()
                        .block
                        .into_iter()
                        .map(|block| (block.id, block.metadata.target))
                        .collect()
                })
                .unwrap_or_default(),
            hunk: file.hunk.clone(),
            group: file
                .group
                .values()
                .filter_map(|group| {
                    hunk.get(&group.target)
                        .map(|hunk| (group.clone(), hunk.raw.clone()))
                })
                .collect(),
        }
    }
}

pub(crate) fn observed(file: &FileTarget) -> ObservedPath {
    ObservedPath {
        change: file.record.clone(),
        worktree: file.stamp.clone(),
        origin: file.origin_stamp.clone(),
        analysis: Default::default(),
    }
}
