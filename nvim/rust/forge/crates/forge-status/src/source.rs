use std::sync::Arc;

use anyhow::{Result, ensure};
use forge_diff::{
    body::BodyKind,
    source::{Representation, SourcePair, SourceVersion},
    syntax::SyntaxLanguage,
};
use forge_git::{
    content::{ContentRequest, ContentResult, ContentSource, WorktreeConversion},
    repository::RepositoryState,
    snapshot::{ChangeKind, ObjectState, PathRecord, PathState},
    store::RepositoryStore,
};

use crate::StatusSection;

#[derive(Debug)]
pub(crate) struct SourceUnavailable(pub String);

impl std::fmt::Display for SourceUnavailable {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str(&self.0)
    }
}

impl std::error::Error for SourceUnavailable {}

struct SourceSelection {
    old: Option<ContentSource>,
    new: Option<ContentSource>,
    kind: BodyKind,
}

fn selection(record: &PathRecord, section: StatusSection) -> Result<SourceSelection> {
    let section = if section == StatusSection::Ignored {
        if matches!(record.state, PathState::Untracked) {
            StatusSection::Untracked
        } else {
            StatusSection::Unstaged
        }
    } else {
        section
    };
    let (old, new, kind) = match (&record.state, section) {
        (PathState::Tracked { head, index, .. }, StatusSection::Staged) => {
            (object(head), object(index), kind(record.staged))
        }
        (PathState::Tracked { index, .. }, StatusSection::Unstaged) => (
            object(index),
            Some(ContentSource::Worktree {
                path: record.path.clone(),
                conversion: WorktreeConversion::GitCanonical,
            }),
            kind(record.unstaged),
        ),
        (PathState::Untracked, StatusSection::Untracked) => (
            None,
            Some(ContentSource::Worktree {
                path: record.path.clone(),
                conversion: WorktreeConversion::GitCanonical,
            }),
            BodyKind::Added,
        ),
        _ => {
            return Err(SourceUnavailable(
                "conflicted or unsupported source requires explicit index-stage selection".into(),
            )
            .into());
        }
    };
    Ok(SourceSelection { old, new, kind })
}

/// Compare expanded LFS display content or reuse verified canonical analysis for ordinary files.
pub(crate) async fn analysis(
    store: &RepositoryStore,
    repository: &Arc<RepositoryState>,
    record: &PathRecord,
    section: StatusSection,
    prepared: Option<&forge_git::snapshot::PreparedAnalysis>,
    diff: &Arc<forge_diff::engine::DiffEngine>,
) -> Result<(forge_diff::cache::AnalysisHandle, BodyKind)> {
    let selected = selection(record, section)?;
    if let Some(source) = repository
        .lfs_diff_sources(store, record.path.clone(), [selected.old, selected.new])
        .await?
        .value
    {
        let analysis = diff
            .compare(forge_diff::engine::DiffRequest {
                source,
                priority: forge_diff::workers::WorkPriority::Visible,
            })
            .await
            .map_err(|error| anyhow::anyhow!("LFS diff unavailable: {error:?}"))?;
        return Ok((analysis, selected.kind));
    }
    if let Some(prepared) = prepared
        && let Some(analysis) = prepared.load(repository, store).await?
    {
        let kind = selection(record, section)?.kind;
        return Ok((analysis, kind));
    }
    let (source, kind) = sources(store, repository, record, section).await?;
    let analysis = diff
        .compare(forge_diff::engine::DiffRequest {
            source,
            priority: forge_diff::workers::WorkPriority::Visible,
        })
        .await
        .map_err(|error| anyhow::anyhow!("diff unavailable: {error:?}"))?;
    Ok((analysis, kind))
}

pub(crate) async fn sources(
    store: &RepositoryStore,
    repository: &Arc<RepositoryState>,
    record: &PathRecord,
    section: StatusSection,
) -> Result<(SourcePair, BodyKind)> {
    let selected = selection(record, section)?;
    let old = acquire(store, repository, selected.old).await?;
    let new = acquire(store, repository, selected.new).await?;
    Ok((SourcePair { old, new }, selected.kind))
}

fn object(state: &ObjectState) -> Option<ContentSource> {
    (state.mode != 0 && !state.object.is_null()).then_some(ContentSource::Object(state.object))
}

fn kind(change: ChangeKind) -> BodyKind {
    match change {
        ChangeKind::Added => BodyKind::Added,
        ChangeKind::Deleted => BodyKind::Deleted,
        _ => BodyKind::Modified,
    }
}

pub(crate) async fn acquire(
    store: &RepositoryStore,
    repository: &Arc<RepositoryState>,
    source: Option<ContentSource>,
) -> Result<SourceVersion> {
    let Some(source) = source else {
        return Ok(SourceVersion::new(
            Vec::new(),
            Representation::GitCanonical,
        )?);
    };
    match repository
        .content(
            store,
            ContentRequest {
                source,
                expected: None,
            },
        )
        .await?
        .value
    {
        ContentResult::Ready(content) => Ok(content.source),
        ContentResult::Missing => Ok(SourceVersion::new(
            Vec::new(),
            Representation::GitCanonical,
        )?),
        ContentResult::Failed(error) => Err(error),
        unavailable => {
            Err(SourceUnavailable(format!("source unavailable: {unavailable:?}")).into())
        }
    }
}

pub(crate) fn language(path: &forge_git::RepositoryPath) -> Option<SyntaxLanguage> {
    SyntaxLanguage::for_path(&path.display_label())
}

/// Select syntax only for complete editable sources, never expanded LFS display content.
pub(crate) fn diff_language(
    path: &forge_git::RepositoryPath,
    analysis: Option<&forge_diff::cache::AnalysisHandle>,
) -> Option<SyntaxLanguage> {
    let source = analysis?.source();
    if source.old.identity().representation == Representation::DisplayOnly
        || source.new.identity().representation == Representation::DisplayOnly
    {
        return None;
    }
    language(path)
}

/// Reject LFS mutations before publication, including collapsed files and stale display analyses.
pub(crate) async fn ensure_writable(
    store: &RepositoryStore,
    repository: &Arc<RepositoryState>,
    selected: Vec<(
        forge_git::RepositoryPath,
        Option<forge_diff::cache::AnalysisHandle>,
    )>,
) -> Result<()> {
    for (path, analysis) in &selected {
        if let Some(analysis) = analysis {
            let source = analysis.source();
            ensure!(
                source.old.identity().representation != Representation::DisplayOnly
                    && source.new.identity().representation != Representation::DisplayOnly,
                "LFS diffs are read-only: {}",
                path.display_label()
            );
        }
    }
    let paths = repository
        .lfs_paths(store, selected.into_iter().map(|(path, _)| path).collect())
        .await?
        .value;
    ensure!(
        paths.is_empty(),
        "LFS diffs are read-only: {}",
        paths
            .iter()
            .map(|path| path.display_label())
            .collect::<Vec<_>>()
            .join(", ")
    );
    Ok(())
}
