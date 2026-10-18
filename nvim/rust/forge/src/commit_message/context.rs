use std::path::PathBuf;
use std::sync::Arc;

use anyhow::{Context, Result, bail, ensure};
use forge_diff::engine::{DiffEngine, DiffRequest};
use forge_diff::source::{Representation, SourcePair, SourceVersion};
use forge_diff::syntax::SyntaxEngine;
use forge_diff::workers::WorkPriority;
use forge_harness::backend::TextGeneration;
use forge_git::content::{ContentRequest, ContentResult, ContentSource, WorktreeConversion};
use forge_git::repository::RepositoryState;
use forge_git::snapshot::{ChangeKind, PathState};
use forge_git::store::RepositoryStore;

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
    let background = if request.comparison == Comparison::PullRequest { None } else {
        Some(forge_harness::background::BackgroundContext::read(store, &request.workspace).await?)
    };
    let repository = store
        .open(PathBuf::from(&request.workspace))
        .await?
        .context("generation requires a Git worktree")?;
    let paths = if matches!(request.comparison, Comparison::LatestCommit | Comparison::PullRequest) {
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
    let mut files = Vec::new();
    let mut source_requests = 0;
    let mut diff_pairs = 0;
    for record in &paths {
        if request
            .ignored_paths
            .iter()
            .any(|ignored| record.path.raw() == ignored.as_bytes())
        {
            continue;
        }
        if request.comparison == Comparison::Staged && record.staged == ChangeKind::Unchanged {
            continue;
        }
        let (head, index, relocation) = match &record.state {
            PathState::Tracked {
                head,
                index,
                relocation,
            } => (Some(head), Some(index), relocation.as_ref()),
            PathState::Untracked if request.comparison == Comparison::Head => (None, None, None),
            PathState::Conflict { .. } => bail!("generation requires resolved index conflicts"),
            _ => continue,
        };
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
        let new_mode = if request.comparison != Comparison::Head {
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
        let use_index = request.comparison != Comparison::Head
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
        let new = match request.comparison {
            Comparison::Staged | Comparison::LatestCommit | Comparison::PullRequest
                if index.is_none_or(|index| index.object.is_null()) =>
            {
                empty()?
            }
            Comparison::Staged | Comparison::LatestCommit | Comparison::PullRequest => {
                source(
                    store,
                    &repository,
                    ContentSource::Object(index.unwrap().object),
                )
                .await?
            }
            Comparison::Head
                if record.unstaged == ChangeKind::Deleted || record.worktree_mode == Some(0) =>
            {
                empty()?
            }
            Comparison::Head if use_index => {
                source(
                    store,
                    &repository,
                    ContentSource::Object(index.unwrap().object),
                )
                .await?
            }
            Comparison::Head => {
                source(
                    store,
                    &repository,
                    ContentSource::Worktree {
                        path: record.path.clone(),
                        conversion: WorktreeConversion::GitCanonical,
                    },
                )
                .await?
            }
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
        let header = if request.comparison == Comparison::PullRequest { "" } else {
            "Generate a conventional commit message for these changes.\n\n"
        };
        let limit = request.max_bytes.unwrap_or_else(|| excerpt::MAX_INPUT_BYTES
            .saturating_sub(TextGeneration::CommitMessage.instructions().len() + header.len()));
        Some(format!("{header}{}", excerpt::prompt(&mut files, syntax, limit).await))
    };
    let prompt = if let Some(background) = &background {
        background.verify(store).await?;
        prompt.map(|diff| format!("Current authoritative comparison at HEAD {} on {}. Earlier requests and generated drafts are historical, not current changes. Prior drafts do not prove a commit occurred.\n\nLast 10 reachable commits (style and terminology examples only):\n{}\nCurrent authoritative changes:\n{}",
            background.head.as_deref().unwrap_or("unborn"), background.branch.as_deref().unwrap_or("detached HEAD"), background.history, diff))
    } else { prompt };
    Ok(CommitContext {
        background,
        prompt,
        source_requests,
        diff_pairs,
    })
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
    use std::process::Command;
    fn git(root: &std::path::Path, arguments: &[&str]) {
        let output = Command::new("git")
            .arg("-C")
            .arg(root)
            .args(arguments)
            .output()
            .unwrap();
        assert!(
            output.status.success(),
            "{}",
            String::from_utf8_lossy(&output.stderr)
        );
    }

    #[tokio::test]
    async fn pull_request_context_uses_selected_merge_base_and_pinned_commits() {
        let directory = tempfile::tempdir().unwrap();
        let root = directory.path();
        git(root, &["init", "--quiet", "--initial-branch=main"]);
        git(root, &["config", "user.name", "Forge Test"]);
        git(root, &["config", "user.email", "forge@example.invalid"]);
        git(root, &["config", "core.autocrlf", "false"]);
        std::fs::write(root.join("source.rs"), "fn original() {}\n").unwrap();
        git(root, &["add", "."]);
        git(root, &["commit", "--quiet", "-m", "initial"]);
        git(root, &["checkout", "--quiet", "-b", "feature"]);
        std::fs::write(root.join("feature.rs"), "fn feature_marker() { hidden_body(); }\n").unwrap();
        git(root, &["add", "."]);
        git(root, &["commit", "--quiet", "-m", "feature"]);
        git(root, &["branch", "selected-base"]);
        git(root, &["mv", "source.rs", "renamed.rs"]);
        git(root, &["commit", "--quiet", "-m", "rename"]);
        let output = Command::new("git").arg("-C").arg(root).args(["rev-parse", "HEAD"])
            .output().unwrap();
        assert!(output.status.success());
        let head = String::from_utf8(output.stdout).unwrap().trim().to_owned();
        git(root, &["checkout", "--quiet", "main"]);
        std::fs::write(root.join("base_only.rs"), "fn base_only_marker() {}\n").unwrap();
        git(root, &["add", "."]);
        git(root, &["commit", "--quiet", "-m", "advance base"]);
        std::fs::write(root.join("uncommitted.rs"), "fn uncommitted_marker() {}\n").unwrap();
        git(root, &["add", "uncommitted.rs"]);
        let store = RepositoryStore::default();
        let diff = DiffEngine::with_cache(Arc::clone(&store.analysis), 4);
        let syntax = SyntaxEngine::new(diff.analysis_pool());
        let mut request = GenerationRequest {
            operation: "context".into(), workspace: root.to_string_lossy().into_owned(),
            comparison: Comparison::PullRequest, base: Some("main".into()), head: Some(head),
            max_bytes: Some(4000), ignored_paths: vec![], generation: None,
        };
        let context = collect(&store, &diff, &syntax, &request).await.unwrap();
        let prompt = context.prompt.unwrap();
        assert!(prompt.contains("+ fn feature_marker() {"), "{prompt}");
        assert!(prompt.contains("renamed.rs (Renamed from source.rs)"), "{prompt}");
        assert!(!prompt.contains("base_only") && !prompt.contains("uncommitted"), "{prompt}");
        assert!(!prompt.contains("conventional commit"));
        assert!(prompt.len() <= 4000);
        request.base = Some("selected-base".into());
        let selected = collect(&store, &diff, &syntax, &request).await.unwrap();
        let selected = selected.prompt.unwrap();
        assert!(selected.contains("renamed.rs (Renamed from source.rs)"));
        assert!(!selected.contains("feature.rs") && !selected.contains("base_only"));
        request.base = Some("missing-base".into());
        assert!(collect(&store, &diff, &syntax, &request).await.is_err());
        request.base = Some("main".into());
        request.head = Some("missing-head".into());
        assert!(collect(&store, &diff, &syntax, &request).await.is_err());
    }

    #[tokio::test]
    async fn renamed_files_remain_inventory_only_in_every_comparison() {
        for (destination, modified, binary) in [
            ("renamed.rs", false, false),
            ("renamed.rs", true, false),
            ("SOURCE.rs", false, false),
            ("binary.dat", false, true),
        ] {
            let directory = tempfile::tempdir().unwrap();
            let root = directory.path();
            git(root, &["init", "--quiet"]);
            git(root, &["config", "user.name", "Forge Test"]);
            git(root, &["config", "user.email", "forge@example.invalid"]);
            git(root, &["config", "core.autocrlf", "false"]);
            git(root, &["config", "core.ignorecase", "false"]);
            git(root, &["config", "status.renames", "false"]);
            let source = format!(
                "fn renamed_marker() {{\n{}}}\n",
                "    original_body();\n".repeat(30)
            );
            let contents = if binary {
                b"binary\0contents".as_slice()
            } else {
                source.as_bytes()
            };
            std::fs::write(root.join("source.rs"), contents).unwrap();
            git(root, &["add", "."]);
            git(root, &["commit", "--quiet", "-m", "initial"]);
            git(root, &["mv", "source.rs", "rename-step"]);
            git(root, &["mv", "rename-step", destination]);
            if modified {
                std::fs::write(
                    root.join(destination),
                    source.replacen("original_body", "edited_body", 1),
                )
                .unwrap();
                git(root, &["add", destination]);
            }
            let store = RepositoryStore::default();
            let diff = DiffEngine::with_cache(Arc::clone(&store.analysis), 4);
            let syntax = SyntaxEngine::new(diff.analysis_pool());
            let mut request = GenerationRequest {
                operation: "preview".into(),
                workspace: root.to_string_lossy().into_owned(),
                comparison: Comparison::Head,
                base: None,
                head: None,
                max_bytes: None,
                ignored_paths: vec![],
                                generation: None,
            };
            for (label, comparison) in [
                ("head", Comparison::Head),
                ("staged", Comparison::Staged),
                ("latest_commit", Comparison::LatestCommit),
            ] {
                if comparison == Comparison::LatestCommit {
                    git(root, &["commit", "--quiet", "-m", "rename"]);
                }
                request.comparison = comparison;
                let context = collect(&store, &diff, &syntax, &request).await.unwrap();
                let prompt = context.prompt.unwrap();
                let entry = format!("  {destination} (Renamed from source.rs)\n");
                assert_eq!(prompt.matches(&entry).count(), 1, "{label}: {prompt}");
                assert!(
                    !prompt.contains("renamed_marker") && !prompt.contains("edited_body"),
                    "{prompt}"
                );
                assert_eq!(context.source_requests, 0, "{label}");
                assert_eq!(context.diff_pairs, 0, "{label}");
                assert_eq!(syntax.usage().cached_entries, 0);
            }
        }
    }

    #[tokio::test]
    async fn commit_context_caps_large_files_and_keeps_inventory_for_latest_commit() {
        let directory = tempfile::Builder::new()
            .prefix(".forge-commit-")
            .tempdir_in(std::env::current_dir().unwrap())
            .unwrap();
        let root = directory.path();
        git(root, &["init", "--quiet"]);
        git(root, &["config", "user.name", "Forge Test"]);
        git(root, &["config", "user.email", "forge@example.invalid"]);
        git(root, &["config", "core.autocrlf", "false"]);
        std::fs::write(root.join("first.txt"), "before\n").unwrap();
        git(root, &["add", "."]);
        git(root, &["commit", "--quiet", "-m", "initial"]);
        let large = format!("{}\nlarge tail λ\n", "complete ".repeat(100_000));
        std::fs::write(root.join("first.txt"), &large).unwrap();
        std::fs::write(root.join("second.txt"), "later file remains present\n").unwrap();
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
        };
        let context = collect(&store, &diff, &syntax, &request).await.unwrap();
        let prompt = context.prompt.unwrap();
        assert!(prompt.len() <= excerpt::MAX_INPUT_BYTES);
        assert!(prompt.contains("first.txt") && prompt.contains("second.txt"));
        assert!(!prompt.contains("large tail λ"));
        assert_eq!(context.diff_pairs, 2);
        git(root, &["add", "."]);
        git(root, &["commit", "--quiet", "-m", "complete"]);
        request.comparison = Comparison::LatestCommit;
        let prompt = collect(&store, &diff, &syntax, &request)
            .await
            .unwrap()
            .prompt
            .unwrap();
        assert!(prompt.len() <= excerpt::MAX_INPUT_BYTES);
        assert!(prompt.contains("first.txt") && prompt.contains("second.txt"));
        assert!(!prompt.contains("large tail λ"));
    }

    #[tokio::test]
    async fn commit_context_survives_competing_syntax_pool_saturation() {
        use forge_diff::workers::WorkBudget;
        let directory = tempfile::tempdir().unwrap();
        let root = directory.path();
        git(root, &["init", "--quiet"]);
        git(root, &["config", "user.name", "Forge Test"]);
        git(root, &["config", "user.email", "forge@example.invalid"]);
        git(root, &["config", "core.autocrlf", "false"]);
        std::fs::write(root.join("source.rs"), "fn before() {}\n").unwrap();
        git(root, &["add", "."]);
        git(root, &["commit", "--quiet", "-m", "initial"]);
        std::fs::write(root.join("source.rs"), "fn staged() {}\n").unwrap();
        git(root, &["add", "source.rs"]);
        std::fs::write(root.join("source.rs"), "fn after() {}\n").unwrap();
        let store = RepositoryStore::default();
        let diff = DiffEngine::with_cache(Arc::clone(&store.analysis), 4);
        let syntax = SyntaxEngine::new(diff.analysis_pool());
        let pool = diff.analysis_pool();
        let occupied: Vec<_> = (0..4)
            .map(|_| {
                pool.reserve(WorkPriority::Visible, WorkBudget::new(0, None))
                    .unwrap()
            })
            .collect();
        let request = GenerationRequest {
            operation: "generate".into(),
            workspace: root.to_string_lossy().into_owned(),
            comparison: Comparison::Head,
            base: None,
            head: None,
            max_bytes: None,
            ignored_paths: vec![],
                        generation: None,
        };
        let mut pending = Box::pin(collect(&store, &diff, &syntax, &request));
        let before_release =
            tokio::time::timeout(std::time::Duration::from_secs(2), &mut pending).await;
        assert_eq!(pool.usage().admitted_jobs, 4);
        drop(occupied);
        let result = match before_release {
            Ok(result) => result,
            Err(_) => tokio::time::timeout(std::time::Duration::from_secs(5), pending)
                .await
                .unwrap(),
        };
        let context = result.unwrap_or_else(|error| {
            panic!("temporary syntax load permanently failed commit preparation: {error:#}")
        });
        let prompt = context.prompt.unwrap();
        assert!(prompt.contains("- fn before() {") && prompt.contains("+ fn after() {"));
        assert_eq!(pool.usage().admitted_jobs, 0);
    }

    #[tokio::test]
    async fn draft_uses_all_changes_and_explicit_regeneration_uses_only_staged() {
        let directory = tempfile::tempdir().unwrap();
        let root = directory.path();
        git(root, &["init", "--quiet"]);
        git(root, &["config", "user.name", "Forge Test"]);
        git(root, &["config", "user.email", "forge@example.invalid"]);
        git(root, &["config", "core.autocrlf", "false"]);
        std::fs::write(root.join("tracked.txt"), "before\n").unwrap();
        std::fs::write(root.join("deleted.txt"), "removed\n").unwrap();
        std::fs::write(root.join(".gitignore"), "git-ignored.txt\n").unwrap();
        git(root, &["add", "."]);
        git(root, &["commit", "--quiet", "-m", "initial"]);
        std::fs::write(root.join("tracked.txt"), "staged value\n").unwrap();
        git(root, &["add", "tracked.txt"]);
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
            comparison: Comparison::Head,
            base: None,
            head: None,
            max_bytes: None,
            ignored_paths: vec!["excluded.txt".into()],
                        generation: None,
        };
        let prompt = collect(&store, &diff, &syntax, &request)
            .await
            .unwrap()
            .prompt
            .unwrap();
        assert!(
            prompt.contains("+ worktree value")
                && prompt.contains("+ new value")
                && prompt.contains("- removed")
        );
        assert!(!prompt.contains("staged value") && !prompt.contains("ignored value"));
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
        git(root, &["add", "excluded.txt"]);
        let staged = collect(&store, &diff, &syntax, &request)
            .await
            .unwrap()
            .prompt
            .unwrap();
        assert!(staged.contains("status ignored value"));
        git(root, &["reset", "--quiet"]);
        assert!(
            collect(&store, &diff, &syntax, &request)
                .await
                .unwrap()
                .prompt
                .is_none()
        );
    }
}
