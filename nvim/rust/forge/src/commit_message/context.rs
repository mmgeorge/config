use std::path::PathBuf;
use std::process::Command;
use std::sync::Arc;
use std::time::Duration;

use anyhow::{Context, Result, bail, ensure};
use forge_diff::engine::{DiffEngine, DiffRequest};
use forge_diff::source::{Representation, SourcePair, SourceVersion};
use forge_diff::workers::WorkPriority;
use forge_git::command::{CommandLimits, read_command};
use forge_git::content::{ContentRequest, ContentResult, ContentSource, WorktreeConversion};
use forge_git::repository::RepositoryState;
use forge_git::snapshot::{ChangeKind, PathState};
use forge_git::store::RepositoryStore;

use super::{Comparison, GenerationRequest};

pub struct CommitContext {
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
    request: &GenerationRequest,
) -> Result<CommitContext> {
    let repository = store
        .open(PathBuf::from(&request.workspace))
        .await?
        .context("generation requires a Git worktree")?;
    if request.comparison == Comparison::LatestCommit {
        return collect_latest_commit(store, repository).await;
    }
    let observation = store
        .read(Arc::clone(&repository), 0, |local, cancellation| {
            forge_git::reader::StatusReader::status_comparison(&local, &mut || cancellation.check())
        })
        .await?
        .value;
    let mut summary = String::new();
    let mut body = String::new();
    let mut changed = 0;
    let mut source_requests = 0;
    let mut diff_pairs = 0;
    for record in &observation.path {
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
        let old_mode = head.map_or(0, |head| head.mode);
        let new_mode = if request.comparison == Comparison::Staged {
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
        let use_index = request.comparison == Comparison::Staged
            || (record.unstaged == ChangeKind::Unchanged && indexed.is_some());
        if use_index && old_object == indexed && old_mode == new_mode {
            continue;
        }
        changed += 1;
        source_requests += usize::from(old_object.is_some()) + usize::from(!missing);
        let old = if let Some(head) = head.filter(|head| !head.object.is_null()) {
            source(store, &repository, ContentSource::Object(head.object)).await?
        } else {
            empty()?
        };
        let new = match request.comparison {
            Comparison::LatestCommit => unreachable!("latest commit uses committed sources"),
            Comparison::Staged if index.is_none_or(|index| index.object.is_null()) => empty()?,
            Comparison::Staged => {
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
            summary.push_str(&format!(
                "{}: binary or unsupported text content changed, mode {:o} -> {:o}\n",
                String::from_utf8_lossy(record.path.raw()),
                old_mode,
                new_mode
            ));
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
            changed -= 1;
            continue;
        }
        let label = String::from_utf8_lossy(record.path.raw());
        if let Some(relocation) = relocation {
            summary.push_str(&format!(
                "{label}: {:?} from {}\n",
                relocation.kind,
                String::from_utf8_lossy(relocation.origin.raw())
            ));
        }
        summary.push_str(&format!(
            "{label}: {} changed hunks, mode {:o} -> {:o}\n",
            analysis.hunks().len(),
            old_mode,
            new_mode
        ));
        body.push_str(&format!("diff --git a/{label} b/{label}\n"));
        for hunk in analysis.hunks() {
            body.push_str(&format!(
                "@@ -{},{} +{},{} @@\n",
                hunk.old_lines.start + 1,
                hunk.old_lines.len(),
                hunk.new_lines.start + 1,
                hunk.new_lines.len()
            ));
            append_lines(
                &mut body,
                &analysis.source().old.bytes()[hunk.old_bytes.clone()],
                '-',
            );
            append_lines(
                &mut body,
                &analysis.source().new.bytes()[hunk.new_bytes.clone()],
                '+',
            );
        }
    }
    let prompt = (changed != 0).then(|| format!("Generate a conventional commit message for these changes.\n\nDiff summary:\n{summary}\nDiff:\n{body}"));
    Ok(CommitContext {
        prompt,
        source_requests,
        diff_pairs,
    })
}

async fn collect_latest_commit(
    store: &RepositoryStore,
    repository: Arc<RepositoryState>,
) -> Result<CommitContext> {
    let root = repository
        .identity
        .worktree_root
        .clone()
        .context("generation requires a worktree")?;
    let output = store
        .read(repository, 0, move |_local, cancellation| {
            let mut command = Command::new("git");
            command.arg("-C").arg(root).args([
                "show",
                "--format=",
                "--no-color",
                "--no-ext-diff",
                "HEAD",
            ]);
            read_command(
                &mut command,
                CommandLimits {
                    stderr_bytes: 4096,
                    timeout: Duration::from_secs(30),
                },
                || cancellation.check(),
            )
        })
        .await?
        .value;
    ensure!(
        output.status.success(),
        "cannot read latest commit: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let diff = String::from_utf8_lossy(&output.stdout);
    if diff.trim().is_empty() {
        return Ok(CommitContext {
            prompt: None,
            source_requests: 1,
            diff_pairs: 0,
        });
    }
    let diff_pairs = diff
        .lines()
        .filter(|line| line.starts_with("diff --git "))
        .count();
    let prompt = format!(
        "Generate a conventional commit message for the latest commit.\n\nDiff:\n{diff}"
    );
    Ok(CommitContext {
        prompt: Some(prompt),
        source_requests: 1,
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

fn append_lines(destination: &mut String, source: &[u8], prefix: char) {
    for line in String::from_utf8_lossy(source).split_inclusive('\n') {
        destination.push(prefix);
        destination.push_str(line);
        if !line.ends_with('\n') {
            destination.push_str("\n\\ No newline at end of file\n");
        }
    }
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

    #[test]
    fn diff_lines_preserve_complete_multibyte_content_and_terminal_marker() {
        let source = format!("{}\nlast λ", "α".repeat(180_001));
        let mut body = String::new();
        append_lines(&mut body, source.as_bytes(), '+');
        assert_eq!(
            body,
            format!("+{}\n+last λ\n\\ No newline at end of file\n", "α".repeat(180_001))
        );
    }

    #[tokio::test]
    async fn commit_context_preserves_large_files_later_files_and_latest_commit() {
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
        let large = format!("{}\nlarge tail λ\n", "complete ".repeat(40_000));
        std::fs::write(root.join("first.txt"), &large).unwrap();
        std::fs::write(root.join("second.txt"), "later file remains present\n").unwrap();
        let store = RepositoryStore::default();
        let diff = DiffEngine::with_cache(Arc::clone(&store.analysis), 4);
        let mut request = GenerationRequest {
            operation: "generate".into(),
            workspace: root.to_string_lossy().into_owned(),
            comparison: Comparison::Head,
            ignored_paths: vec![],
            model: None,
            generation: None,
        };
        let context = collect(&store, &diff, &request).await.unwrap();
        let prompt = context.prompt.unwrap();
        assert!(prompt.contains(&large[..large.len() - 1].replace('\n', "\n+")));
        assert!(prompt.contains("+later file remains present"));
        assert!(!prompt.contains("truncated"));
        assert_eq!(context.diff_pairs, 2);
        git(root, &["add", "."]);
        git(root, &["commit", "--quiet", "-m", "complete"]);
        request.comparison = Comparison::LatestCommit;
        let prompt = collect(&store, &diff, &request)
            .await
            .unwrap()
            .prompt
            .unwrap();
        assert!(prompt.contains("+large tail λ") && prompt.contains("+later file remains present"));
        assert!(prompt.len() > 256 * 1024);
        assert!(!prompt.contains("truncated"));
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
            ignored_paths: vec![],
            model: None,
            generation: None,
        };
        let mut pending = Box::pin(collect(&store, &diff, &request));
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
        assert!(prompt.contains("-fn before() {}") && prompt.contains("+fn after() {}"));
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
        let mut request = GenerationRequest {
            operation: "generate".into(),
            workspace: root.to_string_lossy().into_owned(),
            comparison: Comparison::Head,
            ignored_paths: vec!["excluded.txt".into()],
            model: None,
            generation: None,
        };
        let prompt = collect(&store, &diff, &request)
            .await
            .unwrap()
            .prompt
            .unwrap();
        assert!(
            prompt.contains("+worktree value")
                && prompt.contains("+new value")
                && prompt.contains("-removed")
        );
        assert!(!prompt.contains("staged value") && !prompt.contains("ignored value"));
        request.comparison = Comparison::Staged;
        request.ignored_paths.clear();
        let staged = collect(&store, &diff, &request)
            .await
            .unwrap()
            .prompt
            .unwrap();
        assert!(staged.contains("+staged value"));
        assert!(
            !staged.contains("worktree value")
                && !staged.contains("new value")
                && !staged.contains("removed")
        );
        request.comparison = Comparison::LatestCommit;
        let latest = collect(&store, &diff, &request)
            .await
            .unwrap()
            .prompt
            .unwrap();
        assert!(latest.contains("+before") && latest.contains("+removed"));
        assert!(!latest.contains("staged value") && !latest.contains("worktree value"));
        request.comparison = Comparison::Staged;
        git(root, &["add", "excluded.txt"]);
        let staged = collect(&store, &diff, &request)
            .await
            .unwrap()
            .prompt
            .unwrap();
        assert!(staged.contains("status ignored value"));
        git(root, &["reset", "--quiet"]);
        assert!(
            collect(&store, &diff, &request)
                .await
                .unwrap()
                .prompt
                .is_none()
        );
    }
}
