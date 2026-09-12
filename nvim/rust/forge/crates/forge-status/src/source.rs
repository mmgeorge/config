use std::sync::Arc;

use anyhow::Result;
use forge_diff::{
    body::BodyKind,
    source::{Representation, SourcePair, SourceVersion},
    syntax::SyntaxLanguage,
};
use forge_git::{
    content::{ContentLimits, ContentRequest, ContentResult, ContentSource, WorktreeConversion},
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

pub(crate) async fn analysis(
    store: &RepositoryStore,
    repository: &Arc<RepositoryState>,
    record: &PathRecord,
    section: StatusSection,
    prepared: Option<&forge_git::snapshot::PreparedAnalysis>,
    diff: &Arc<forge_diff::engine::DiffEngine>,
) -> Result<(forge_diff::cache::AnalysisHandle, BodyKind)> {
    if let Some(prepared) = prepared
        && let Some(analysis) = prepared.load(repository, store).await?
    {
        let kind = selection(record, section)?.kind;
        validate_preview(analysis.source(), kind)?;
        return Ok((analysis, kind));
    }
    let (source, kind) = sources(store, repository, record, section).await?;
    validate_preview(&source, kind)?;
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

fn validate_preview(source: &SourcePair, kind: BodyKind) -> Result<()> {
    forge_diff::body::BodyPolicy::default()
        .may_preview_full_file(
            kind,
            match kind {
                BodyKind::Deleted => source.old.newline().line_count,
                _ => source.new.newline().line_count,
            },
        )
        .map_err(|reason| SourceUnavailable(format!("{reason:?}")))?;
    Ok(())
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
                limits: ContentLimits::default(),
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
