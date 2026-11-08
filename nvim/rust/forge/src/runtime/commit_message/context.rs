use std::path::PathBuf;
use std::sync::Arc;

use anyhow::{Context, Result, bail, ensure};
use forge_diff::engine::{DiffEngine, DiffRequest};
use forge_diff::source::{Representation, SourcePair, SourceVersion};
use forge_diff::syntax::SyntaxEngine;
use forge_diff::workers::WorkPriority;
use forge_git::content::{ContentRequest, ContentResult, ContentSource, WorktreeConversion};
use forge_git::repository::RepositoryState;
use forge_git::snapshot::{ChangeKind, PathState};
use forge_git::store::RepositoryStore;
use forge_harness::provider::TextGeneration;

use super::excerpt::ChangedFile;
use super::{Comparison, GenerationRequest, excerpt};

pub struct CommitContext {
    pub background: Option<forge_harness::background::BackgroundContext>,
    pub prompt: Option<String>,
    pub source_requests: usize,
    pub diff_pairs: usize,
}

struct ComparisonSource {
    text: Option<SourceVersion>,
}

pub async fn collect(
    store: &RepositoryStore,
    diff: &Arc<DiffEngine>,
    syntax: &Arc<SyntaxEngine>,
    request: &GenerationRequest,
) -> Result<CommitContext> {
    let background = if request.comparison == Comparison::PullRequest {
        None
    } else {
        Some(forge_harness::background::BackgroundContext::read(store, &request.workspace).await?)
    };
    let repository = store
        .open(PathBuf::from(&request.workspace))
        .await?
        .context("generation requires a Git worktree")?;
    let paths = if matches!(
        request.comparison,
        Comparison::LatestCommit | Comparison::PullRequest
    ) {
        forge_git::revision::comparison::observe(
            store,
            Arc::clone(&repository),
            forge_git::revision::comparison::ComparisonRequest {
                reference: request.base.clone().unwrap_or_else(|| "HEAD".into()),
                head: request.head.clone(),
                worktree: false,
                path: None,
            },
        )
        .await?
        .value
        .path
        .into_iter()
        .map(|observed| observed.change)
        .collect()
    } else {
        store
            .read(Arc::clone(&repository), 0, |local, cancellation| {
                forge_git::reader::StatusReader::status_comparison(&local, &mut || {
                    cancellation.check()
                })
            })
            .await?
            .value
            .path
    };
    let comparison = match request.comparison {
        Comparison::Status
            if paths
                .iter()
                .any(|record| record.staged != ChangeKind::Unchanged) =>
        {
            Comparison::Staged
        }
        Comparison::Status => Comparison::Head,
        comparison => comparison,
    };
    let lfs_paths = {
        let root = repository
            .identity
            .worktree_root
            .clone()
            .context("generation requires a worktree")?;
        let mut candidates = std::collections::BTreeSet::new();
        for record in &paths {
            candidates.insert(forge_git::checkpoint::path_key(record.path.raw()));
            if let PathState::Tracked {
                relocation: Some(relocation),
                ..
            } = &record.state
            {
                candidates.insert(forge_git::checkpoint::path_key(relocation.origin.raw()));
            }
        }
        store
            .read(Arc::clone(&repository), 0, move |_local, cancellation| {
                cancellation.check()?;
                let excluded = forge_git::checkpoint::lfs_paths(&root, &candidates)?;
                cancellation.check()?;
                Ok(excluded)
            })
            .await?
            .value
    };
    let mut files = Vec::new();
    let mut source_requests = 0;
    let mut diff_pairs = 0;
    for record in &paths {
        if lfs_paths.contains(&forge_git::checkpoint::path_key(record.path.raw())) {
            continue;
        }
        if request
            .ignored_paths
            .iter()
            .any(|ignored| record.path.raw() == ignored.as_bytes())
        {
            continue;
        }
        if comparison == Comparison::Staged && record.staged == ChangeKind::Unchanged {
            continue;
        }
        let (head, index, relocation) = match &record.state {
            PathState::Tracked {
                head,
                index,
                relocation,
            } => (Some(head), Some(index), relocation.as_ref()),
            PathState::Untracked if comparison == Comparison::Head => (None, None, None),
            PathState::Conflict { .. } => bail!("generation requires resolved index conflicts"),
            _ => continue,
        };
        if relocation.is_some_and(|relocation| {
            lfs_paths.contains(&forge_git::checkpoint::path_key(relocation.origin.raw()))
        }) {
            continue;
        }
        ensure!(
            record.submodule.is_none(),
            "generation cannot describe a changed submodule as a text diff"
        );
        if let Some(relocation) = relocation.filter(|value| value.kind == ChangeKind::Renamed) {
            files.push(ChangedFile {
                path: String::from_utf8_lossy(record.path.raw()).into_owned(),
                note: format!(
                    "Renamed from {}",
                    String::from_utf8_lossy(relocation.origin.raw())
                        .replace('\n', "\\n")
                        .replace('\r', "\\r")
                ),
                analysis: None,
            });
            continue;
        }
        let old_mode = head.map_or(0, |head| head.mode);
        let new_mode = if comparison != Comparison::Head {
            index.unwrap().mode
        } else if matches!(record.state, PathState::Untracked) {
            let path = forge_git::identity::validate_path(
                repository
                    .identity
                    .worktree_root
                    .as_ref()
                    .context("generation requires a worktree")?,
                &record.path,
            )?;
            store
                .read(Arc::clone(&repository), 0, move |_local, cancellation| {
                    cancellation.check()?;
                    let metadata = match std::fs::symlink_metadata(path) {
                        Ok(metadata) => metadata,
                        Err(error) if error.kind() == std::io::ErrorKind::NotFound => return Ok(0),
                        Err(error) => return Err(error.into()),
                    };
                    if metadata.file_type().is_symlink() {
                        return Ok(0o120000);
                    }
                    #[cfg(unix)]
                    {
                        use std::os::unix::fs::PermissionsExt;
                        if _local
                            .config_snapshot()
                            .boolean("core.filemode")
                            .unwrap_or(true)
                            && metadata.permissions().mode() & 0o111 != 0
                        {
                            return Ok(0o100755);
                        }
                    }
                    Ok(0o100644)
                })
                .await?
                .value
        } else {
            record.worktree_mode.unwrap_or(0)
        };
        let old_object = head
            .filter(|head| !head.object.is_null())
            .map(|head| head.object);
        let indexed = index
            .filter(|index| !index.object.is_null())
            .map(|index| index.object);
        let missing = new_mode == 0;
        let use_index = comparison != Comparison::Head
            || (record.unstaged == ChangeKind::Unchanged && indexed.is_some());
        if use_index && old_object == indexed && old_mode == new_mode && relocation.is_none() {
            continue;
        }
        source_requests += usize::from(old_object.is_some()) + usize::from(!missing);
        let old = if let Some(head) = head.filter(|head| !head.object.is_null()) {
            source(store, &repository, ContentSource::Object(head.object)).await?
        } else {
            empty()?
        };
        let new = if matches!(
            comparison,
            Comparison::Staged | Comparison::LatestCommit | Comparison::PullRequest
        ) {
            if index.is_none_or(|index| index.object.is_null()) {
                empty()?
            } else {
                source(
                    store,
                    &repository,
                    ContentSource::Object(index.unwrap().object),
                )
                .await?
            }
        } else if record.unstaged == ChangeKind::Deleted || record.worktree_mode == Some(0) {
            empty()?
        } else if use_index {
            source(
                store,
                &repository,
                ContentSource::Object(index.unwrap().object),
            )
            .await?
        } else {
            source(
                store,
                &repository,
                ContentSource::Worktree {
                    path: record.path.clone(),
                    conversion: WorktreeConversion::GitCanonical,
                },
            )
            .await?
        };
        let (Some(old), Some(new)) = (old.text, new.text) else {
            files.push(ChangedFile {
                path: String::from_utf8_lossy(record.path.raw()).into_owned(),
                note: format!("binary or unsupported text, mode {old_mode:o} -> {new_mode:o}"),
                analysis: None,
            });
            continue;
        };
        diff_pairs += 1;
        let analysis = diff
            .compare(DiffRequest {
                source: SourcePair { old, new },
                priority: WorkPriority::Speculative,
            })
            .await
            .map_err(|error| anyhow::anyhow!("commit comparison failed: {error:?}"))?;
        if analysis.hunks().is_empty() && old_mode == new_mode && relocation.is_none() {
            continue;
        }
        let mut note = String::new();
        if let Some(relocation) = relocation {
            note = format!(
                "{:?} from {}",
                relocation.kind,
                String::from_utf8_lossy(relocation.origin.raw())
            );
        }
        if old_mode != new_mode {
            if !note.is_empty() {
                note.push_str(", ");
            }
            note.push_str(&format!("mode {old_mode:o} -> {new_mode:o}"));
        }
        files.push(ChangedFile {
            path: String::from_utf8_lossy(record.path.raw()).into_owned(),
            note,
            analysis: Some(analysis),
        });
    }
    let prompt = if files.is_empty() {
        None
    } else {
        let header = if request.comparison == Comparison::PullRequest {
            ""
        } else {
            "Generate a conventional commit message for these changes.\n\n"
        };
        // The background wrapper surrounds the excerpt after truncation, so the default budget
        // reserves its bytes too; otherwise the wrapped prompt overruns `MAX_INPUT_BYTES`.
        let background_wrapper_bytes = background.as_ref().map_or(0, |background| {
            background_comparison_prompt(background, "").len()
        });
        let limit = request.max_bytes.unwrap_or_else(|| {
            excerpt::MAX_INPUT_BYTES.saturating_sub(
                forge_harness::provider::BACKGROUND_SYSTEM_MESSAGE.len()
                    + TextGeneration::CommitMessage.background_input("").len()
                    + header.len()
                    + background_wrapper_bytes,
            )
        });
        Some(format!(
            "{header}{}",
            excerpt::prompt(&mut files, syntax, limit).await
        ))
    };
    let prompt = if let Some(background) = &background {
        background.verify(store).await?;
        prompt.map(|diff| background_comparison_prompt(background, &diff))
    } else {
        prompt
    };
    Ok(CommitContext {
        background,
        prompt,
        source_requests,
        diff_pairs,
    })
}

/// Frames the current changes with the comparison point and recent history the model may borrow
/// style from.
fn background_comparison_prompt(
    background: &forge_harness::background::BackgroundContext,
    diff: &str,
) -> String {
    format!(
        "Current comparison at HEAD {} on {}. Earlier requests and generated drafts are history, not current changes, and a prior draft does not mean a commit happened.\n\nLast 10 reachable commits (examples of style and terminology only):\n{}\nCurrent changes:\n{}",
        background.head.as_deref().unwrap_or("unborn"),
        background.branch.as_deref().unwrap_or("detached HEAD"),
        background.history,
        diff
    )
}

pub(super) fn generation_was_superseded(failure: &anyhow::Error) -> bool {
    let message = format!("{failure:#}");
    forge_git::repository::transient_observation_failure(failure)
        || message.contains("snapshot was invalidated")
        || message.contains("repository read was superseded by invalidation")
        || message.contains("repository changed during commit comparison")
}

async fn source(
    store: &RepositoryStore,
    repository: &Arc<RepositoryState>,
    source: ContentSource,
) -> Result<ComparisonSource> {
    let acquired = repository
        .content(
            store,
            ContentRequest {
                source,
                expected: None,
            },
        )
        .await?
        .value;
    match acquired {
        ContentResult::Ready(content) => Ok(ComparisonSource {
            text: Some(content.source),
        }),
        ContentResult::Missing => empty(),
        ContentResult::Failed(error) => Err(error),
        ContentResult::Binary
        | ContentResult::Unavailable(forge_git::content::ContentUnavailable::UnsupportedEncoding) => {
            Ok(ComparisonSource { text: None })
        }
        unavailable => bail!("commit comparison source unavailable: {unavailable:?}"),
    }
}

fn empty() -> Result<ComparisonSource> {
    let text = SourceVersion::new(Vec::new(), Representation::GitCanonical)?;
    Ok(ComparisonSource { text: Some(text) })
}

#[cfg(test)]
mod tests {
    use super::*;
    use forge_git::test_support::{
        commit_index, init_repository, move_tracked, reset_index_to_head, set_config, set_identity,
        stage, stage_all,
    };

    #[tokio::test]
    async fn lfs_files_are_excluded_before_content_acquisition_in_every_comparison() {
        let directory = tempfile::tempdir().unwrap();
        let root = directory.path();
        init_repository(root);
        set_identity(root);
        set_config(root, "core", "autocrlf", "false");
        std::fs::write(root.join(".gitattributes"), "*.asset filter=lfs\n").unwrap();
        for path in ["tracked.asset", "deleted.asset", "renamed.asset"] {
            std::fs::write(root.join(path), "original filtered content\n").unwrap();
        }
        std::fs::write(root.join("source.txt"), "before\n").unwrap();
        stage_all(root);
        commit_index(root, "initial");
        let blocked = "echo invoked > filter-invoked; exit 9";
        set_config(root, "filter \"lfs\"", "process", "");
        set_config(root, "filter \"lfs\"", "clean", blocked);
        set_config(root, "filter \"lfs\"", "required", "true");
        std::fs::write(root.join("tracked.asset"), "changed filtered content\n").unwrap();
        std::fs::write(root.join("untracked.asset"), "new filtered content\n").unwrap();
        std::fs::remove_file(root.join("deleted.asset")).unwrap();
        move_tracked(root, "renamed.asset", "replacement.txt");
        std::fs::write(root.join("source.txt"), "after\n").unwrap();
        let store = RepositoryStore::default();
        let diff = DiffEngine::with_cache(Arc::clone(&store.analysis), 4);
        let syntax = SyntaxEngine::new(diff.analysis_pool());
        let mut request = GenerationRequest {
            operation: "generate".into(),
            workspace: root.to_string_lossy().into_owned(),
            comparison: Comparison::Head,
            base: None,
            head: None,
            max_bytes: None,
            ignored_paths: vec![],
            generation: None,
            input: None,
        };
        let context = collect(&store, &diff, &syntax, &request).await.unwrap();
        let prompt = context.prompt.unwrap();
        assert!(
            prompt.contains("- before") && prompt.contains("+ after"),
            "{prompt}"
        );
        assert!(!prompt.contains(".asset"), "{prompt}");
        assert!(!prompt.contains("filtered content"), "{prompt}");
        assert_eq!(context.source_requests, 2);
        assert_eq!(context.diff_pairs, 1);
        assert!(!root.join("filter-invoked").exists());

        stage_all(root);
        std::fs::remove_file(root.join("tracked.asset")).unwrap();
        std::fs::remove_file(root.join("untracked.asset")).unwrap();
        for comparison in [
            Comparison::Head,
            Comparison::Staged,
            Comparison::LatestCommit,
            Comparison::PullRequest,
        ] {
            if comparison == Comparison::LatestCommit {
                commit_index(root, "changed");
            }
            if comparison == Comparison::PullRequest {
                request.base = Some("HEAD^".into());
                request.head = Some("HEAD".into());
            }
            request.comparison = comparison;
            let context = collect(&store, &diff, &syntax, &request).await.unwrap();
            let prompt = context.prompt.unwrap();
            assert!(
                prompt.contains("- before") && prompt.contains("+ after"),
                "{prompt}"
            );
            assert!(
                !prompt.contains(".asset") && !prompt.contains("replacement.txt"),
                "{prompt}"
            );
            assert!(!prompt.contains("filtered content"), "{prompt}");
            assert_eq!(context.source_requests, 2);
            assert_eq!(context.diff_pairs, 1);
            assert!(!root.join("filter-invoked").exists());
        }
        std::fs::write(root.join("tracked.asset"), "only LFS changes remain\n").unwrap();
        request.comparison = Comparison::Head;
        assert!(
            collect(&store, &diff, &syntax, &request)
                .await
                .unwrap()
                .prompt
                .is_none()
        );
    }

    #[tokio::test]
    async fn status_draft_prefers_staged_and_falls_back_to_non_ignored_unstaged() {
        let directory = tempfile::tempdir().unwrap();
        let root = directory.path();
        init_repository(root);
        set_identity(root);
        set_config(root, "core", "autocrlf", "false");
        std::fs::write(root.join("tracked.txt"), "before\n").unwrap();
        std::fs::write(root.join("deleted.txt"), "removed\n").unwrap();
        std::fs::write(root.join(".gitignore"), "git-ignored.txt\n").unwrap();
        stage_all(root);
        commit_index(root, "initial");
        std::fs::write(root.join("tracked.txt"), "staged value\n").unwrap();
        stage(root, &["tracked.txt"]);
        std::fs::write(root.join("tracked.txt"), "worktree value\n").unwrap();
        std::fs::remove_file(root.join("deleted.txt")).unwrap();
        std::fs::write(root.join("untracked.txt"), "new value\n").unwrap();
        std::fs::write(root.join("excluded.txt"), "status ignored value\n").unwrap();
        std::fs::write(root.join("git-ignored.txt"), "git ignored value\n").unwrap();
        let store = RepositoryStore::default();
        let diff = DiffEngine::with_cache(Arc::clone(&store.analysis), 4);
        let syntax = SyntaxEngine::new(diff.analysis_pool());
        let mut request = GenerationRequest {
            operation: "generate".into(),
            workspace: root.to_string_lossy().into_owned(),
            comparison: Comparison::Status,
            base: None,
            head: None,
            max_bytes: None,
            ignored_paths: vec!["excluded.txt".into()],
            generation: None,
            input: None,
        };
        let prompt = collect(&store, &diff, &syntax, &request)
            .await
            .unwrap()
            .prompt
            .unwrap();
        assert!(prompt.contains("+ staged value"));
        assert!(
            !prompt.contains("worktree value")
                && !prompt.contains("new value")
                && !prompt.contains("- removed")
                && !prompt.contains("ignored value")
        );
        request.comparison = Comparison::Staged;
        request.ignored_paths.clear();
        let staged = collect(&store, &diff, &syntax, &request)
            .await
            .unwrap()
            .prompt
            .unwrap();
        assert!(staged.contains("+ staged value"));
        assert!(
            !staged.contains("worktree value")
                && !staged.contains("new value")
                && !staged.contains("- removed")
        );
        request.comparison = Comparison::LatestCommit;
        let latest = collect(&store, &diff, &syntax, &request)
            .await
            .unwrap()
            .prompt
            .unwrap();
        assert!(latest.contains("+ before") && latest.contains("+ removed"));
        assert!(!latest.contains("staged value") && !latest.contains("worktree value"));
        request.comparison = Comparison::Staged;
        stage(root, &["excluded.txt"]);
        let staged = collect(&store, &diff, &syntax, &request)
            .await
            .unwrap()
            .prompt
            .unwrap();
        assert!(staged.contains("status ignored value"));
        reset_index_to_head(root);
        assert!(
            collect(&store, &diff, &syntax, &request)
                .await
                .unwrap()
                .prompt
                .is_none()
        );
        request.comparison = Comparison::Status;
        request.ignored_paths = vec!["excluded.txt".into()];
        let fallback = collect(&store, &diff, &syntax, &request)
            .await
            .unwrap()
            .prompt
            .unwrap();
        assert!(
            fallback.contains("+ worktree value")
                && fallback.contains("+ new value")
                && fallback.contains("- removed")
        );
        assert!(!fallback.contains("staged value") && !fallback.contains("ignored value"));
        stage(root, &["excluded.txt"]);
        assert!(
            collect(&store, &diff, &syntax, &request)
                .await
                .unwrap()
                .prompt
                .is_none(),
            "filtered staged changes must not fall back to unrelated unstaged changes"
        );
    }
}
