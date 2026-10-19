use std::{
    collections::HashSet,
    process::Command,
    sync::Arc,
    time::{Duration, Instant},
};

use anyhow::{Context, Result, ensure};

use crate::{
    RepositoryPath,
    command::{CommandLimits, read_command},
    repository::{RepositoryRead, RepositoryState},
    resolve_argument,
    snapshot::{
        ChangeKind, ContentClassification, LineStats, ObjectState, ObservedPath, PathRecord,
        PathState, Relocation, WorktreeStamp, read_index_stamp,
    },
    store::RepositoryStore,
};

/// Stops pull request preparation when the selected commits cannot merge cleanly.
#[derive(Debug)]
pub struct MergeConflict;

impl std::fmt::Display for MergeConflict {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str("Merge conflicts. Resolve them before creating the PR.")
    }
}

impl std::error::Error for MergeConflict {}

#[derive(Clone, Debug)]
pub struct ComparisonRequest {
    pub reference: String,
    /// Pinned head for a merge-base comparison, or absent for existing comparison modes.
    pub head: Option<String>,
    pub worktree: bool,
    pub path: Option<RepositoryPath>,
}

#[derive(Debug)]
pub struct ComparisonObservation {
    pub commit: gix::ObjectId,
    pub worktree: bool,
    pub path: Vec<ObservedPath>,
}

pub async fn observe(
    store: &RepositoryStore,
    repository: Arc<RepositoryState>,
    request: ComparisonRequest,
) -> Result<RepositoryRead<ComparisonObservation>> {
    ensure!(
        !request.reference.is_empty() && !request.reference.chars().any(char::is_control),
        "invalid comparison reference"
    );
    ensure!(
        request.head.as_ref().is_none_or(|head| !request.worktree
            && !head.is_empty()
            && !head.chars().any(char::is_control)),
        "invalid comparison head"
    );
    let path = request.path.as_ref().map(resolve_argument).transpose()?;
    let retained = request.reference.capacity()
        + request.head.as_ref().map_or(0, String::capacity)
        + request
            .path
            .as_ref()
            .map_or(0, RepositoryPath::retained_bytes);
    let worker = Arc::clone(&repository);
    let generation = repository.generation();
    let analysis = Arc::clone(&store.analysis);
    let skip_line_stats = store
        .diagnostic_skip_line_stats
        .load(std::sync::atomic::Ordering::Relaxed);
    store
        .read(repository, retained, move |local, cancellation| {
            let started = Instant::now();
            let mut check = || {
                cancellation.check()?;
                ensure!(
                    started.elapsed() < Duration::from_secs(30),
                    "comparison enumeration exceeded 30 seconds"
                );
                Ok(())
            };
            let root = local.workdir().context("comparison requires worktree")?;
            let index = request
                .worktree
                .then(|| read_index_stamp(&worker.identity.index, &mut check))
                .transpose()?;
            check()?;
            let spec = format!("{}^{{commit}}", request.reference);
            let commit = local
                .rev_parse_single(spec.as_str())
                .context("comparison reference unavailable")?
                .detach();
            let head = request
                .head
                .as_ref()
                .map(|head| {
                    local
                        .rev_parse_single(format!("{head}^{{commit}}").as_str())
                        .map(|resolved| resolved.detach())
                        .context("comparison head unavailable")
                })
                .transpose()?;
            let baseline = if let Some(head) = head {
                local
                    .merge_base(commit, head)
                    .context("comparison branches have no merge base")?
                    .detach()
            } else {
                commit
            };
            check()?;
            if let Some(head) = head {
                ensure_mergeable(root, commit, head, &mut check)?;
            }
            let mut command = Command::new("git");
            command
                .args([
                    "--no-pager",
                    "--no-optional-locks",
                    "--literal-pathspecs",
                    "-C",
                ])
                .arg(root);
            if request.worktree {
                command.arg("diff");
            } else {
                command.args([
                    "diff-tree",
                    "--root",
                    "--no-commit-id",
                    "--first-parent",
                    "-m",
                    "-r",
                ]);
            }
            command
                .args([
                    "--raw",
                    "--no-abbrev",
                    "-z",
                    "--find-renames=50%",
                    "--no-ext-diff",
                    "--no-textconv",
                ])
                .arg(baseline.to_string());
            if let Some(head) = head {
                command.arg(head.to_string());
            }
            command.arg("--");
            if let Some(path) = path {
                command.arg(path);
            }
            let output = read_command(
                &mut command,
                CommandLimits {
                    stderr_bytes: 4096,
                    timeout: Duration::from_secs(30),
                },
                &mut check,
            )?;
            ensure!(
                output.status.success(),
                "comparison enumeration failed: {}",
                String::from_utf8_lossy(&output.stderr)
            );
            let records = parse(&output.stdout, request.worktree)?;
            let mut path = Vec::with_capacity(records.len());
            for change in records {
                check()?;
                let origin = match &change.state {
                    PathState::Tracked {
                        relocation: Some(_),
                        ..
                    } if request.worktree => Some(WorktreeStamp::Unobserved),
                    _ => None,
                };
                path.push(ObservedPath {
                    analysis: Default::default(),
                    change,
                    worktree: WorktreeStamp::Unobserved,
                    origin,
                });
            }
            if head.is_none() {
                crate::snapshot::collect_line_stats(
                    &local,
                    &worker.identity,
                    &mut path,
                    Some(&analysis),
                    Some(&worker.line_stats),
                    skip_line_stats,
                    &mut check,
                )?;
            }
            for observed in &mut path {
                for prepared in observed.analysis.iter_mut().flatten() {
                    prepared.generation = Some(generation);
                }
            }
            if let Some(index) = index {
                ensure!(
                    index == read_index_stamp(&worker.identity.index, &mut check)?,
                    "index changed during comparison enumeration"
                );
            }
            Ok(ComparisonObservation {
                commit,
                worktree: request.worktree,
                path,
            })
        })
        .await
}

fn ensure_mergeable(
    root: &std::path::Path,
    base: gix::ObjectId,
    head: gix::ObjectId,
    check: &mut dyn FnMut() -> Result<()>,
) -> Result<()> {
    let output = read_command(
        Command::new("git")
            .args(["--no-pager", "--no-optional-locks", "-C"])
            .arg(root)
            .args(["merge-tree", "--write-tree", "--quiet"])
            .arg(base.to_string())
            .arg(head.to_string()),
        CommandLimits {
            stderr_bytes: 4096,
            timeout: Duration::from_secs(30),
        },
        check,
    )?;
    match output.status.code() {
        Some(0) => Ok(()),
        Some(1) => Err(MergeConflict.into()),
        _ => anyhow::bail!(
            "merge conflict check failed: {}",
            String::from_utf8_lossy(&output.stderr)
        ),
    }
}

fn parse(bytes: &[u8], worktree: bool) -> Result<Vec<PathRecord>> {
    if bytes.is_empty() {
        return Ok(Vec::new());
    }
    ensure!(
        bytes.last() == Some(&0),
        "comparison metadata is not NUL terminated"
    );
    let mut segment = bytes[..bytes.len() - 1].split(|byte| *byte == 0);
    let mut records = Vec::new();
    let mut seen = HashSet::new();
    while let Some(header) = segment.next() {
        let field: Vec<_> = header.split(|byte| *byte == b' ').collect();
        ensure!(
            field.len() == 5 && field[0].first() == Some(&b':'),
            "invalid raw comparison header"
        );
        let mode = |bytes: &[u8]| -> Result<u32> {
            Ok(u32::from_str_radix(std::str::from_utf8(bytes)?, 8)?)
        };
        let old = ObjectState {
            mode: mode(&field[0][1..])?,
            object: gix::ObjectId::from_hex(field[2])?,
        };
        let new = ObjectState {
            mode: mode(field[1])?,
            object: gix::ObjectId::from_hex(field[3])?,
        };
        let kind = match field[4].first() {
            Some(b'A') => ChangeKind::Added,
            Some(b'D') => ChangeKind::Deleted,
            Some(b'M') => ChangeKind::Modified,
            Some(b'T') => ChangeKind::TypeChanged,
            Some(b'R') => ChangeKind::Renamed,
            Some(b'C') => ChangeKind::Copied,
            Some(b'U') => ChangeKind::Unmerged,
            _ => anyhow::bail!("unsupported raw comparison status"),
        };
        let first =
            RepositoryPath::new(segment.next().context("comparison path missing")?.to_vec())?;
        let (path, relocation) = if matches!(kind, ChangeKind::Renamed | ChangeKind::Copied) {
            let similarity = std::str::from_utf8(&field[4][1..])?.parse::<u8>()?;
            ensure!(similarity <= 100, "invalid relocation similarity");
            (
                RepositoryPath::new(
                    segment
                        .next()
                        .context("comparison destination missing")?
                        .to_vec(),
                )?,
                Some(Relocation {
                    kind,
                    origin: first,
                    similarity,
                }),
            )
        } else {
            (first, None)
        };
        ensure!(seen.insert(path.clone()), "duplicate comparison path");
        records.push(PathRecord {
            path,
            staged: if worktree {
                ChangeKind::Unchanged
            } else {
                kind
            },
            unstaged: if worktree {
                kind
            } else {
                ChangeKind::Unchanged
            },
            worktree_mode: worktree.then_some(new.mode),
            submodule: None,
            state: PathState::Tracked {
                head: old.clone(),
                index: if worktree { old } else { new },
                relocation,
            },
            staged_stats: LineStats::Unknown,
            unstaged_stats: LineStats::Unknown,
            content: ContentClassification::Unknown,
        });
    }
    Ok(records)
}

#[cfg(test)]
mod tests {
    use super::*;
    #[tokio::test]
    async fn conflicting_ranges_stop_without_changing_checkout_state() {
        for scenario in ["text", "binary", "rename"] {
            let directory = tempfile::tempdir().unwrap();
            let root = directory.path();
            let git = |arguments: &[&str]| crate::test_support::git(root, arguments);
            git(&["init", "--quiet", "--initial-branch=main"]);
            git(&["config", "user.name", "Forge"]);
            git(&["config", "user.email", "forge@example.invalid"]);
            git(&["config", "core.autocrlf", "false"]);
            let binary = scenario == "binary";
            let original: &[u8] = if binary {
                b"original\0bytes"
            } else {
                b"original\n"
            };
            std::fs::write(root.join("shared.txt"), original).unwrap();
            git(&["add", "."]);
            git(&["commit", "--quiet", "-m", "initial"]);
            git(&["checkout", "--quiet", "-b", "feature"]);
            if scenario == "rename" {
                git(&["mv", "shared.txt", "feature.txt"]);
            } else {
                let content: &[u8] = if binary { b"head\0bytes" } else { b"head\n" };
                std::fs::write(root.join("shared.txt"), content).unwrap();
            }
            git(&["commit", "--quiet", "-am", "head"]);
            git(&["checkout", "--quiet", "main"]);
            if scenario == "rename" {
                git(&["mv", "shared.txt", "base.txt"]);
            } else {
                let content: &[u8] = if binary { b"base\0bytes" } else { b"base\n" };
                std::fs::write(root.join("shared.txt"), content).unwrap();
            }
            git(&["commit", "--quiet", "-am", "base"]);
            std::fs::write(root.join("staged.txt"), "staged contents\n").unwrap();
            git(&["add", "staged.txt"]);
            std::fs::write(root.join("untracked.txt"), "untracked contents\n").unwrap();
            let head_before = git(&["rev-parse", "HEAD"]);
            let status_before = git(&["status", "--porcelain=v2"]);
            let index_before = std::fs::read(root.join(".git/index")).unwrap();
            let store = RepositoryStore::default();
            let repository = store.open(root.to_owned()).await.unwrap().unwrap();
            let failure = observe(
                &store,
                repository,
                ComparisonRequest {
                    reference: "main".into(),
                    head: Some("feature".into()),
                    worktree: false,
                    path: None,
                },
            )
            .await
            .err()
            .expect("conflicting range was accepted");
            assert!(failure.is::<MergeConflict>(), "{scenario}: {failure:#}");
            assert_eq!(git(&["rev-parse", "HEAD"]), head_before);
            assert_eq!(git(&["status", "--porcelain=v2"]), status_before);
            assert_eq!(
                std::fs::read(root.join(".git/index")).unwrap(),
                index_before
            );
            assert_eq!(
                std::fs::read(root.join("staged.txt")).unwrap(),
                b"staged contents\n"
            );
            assert_eq!(
                std::fs::read(root.join("untracked.txt")).unwrap(),
                b"untracked contents\n"
            );
        }
    }

    #[tokio::test]
    async fn native_comparison_pins_commit_and_reads_worktree_or_commit_sources() {
        let directory = tempfile::tempdir().unwrap();
        let git = |args: &[&str]| crate::test_support::git(directory.path(), args);
        git(&["init", "--initial-branch=main"]);
        git(&["config", "user.name", "Forge"]);
        git(&["config", "user.email", "forge@example.invalid"]);
        git(&["config", "core.autocrlf", "false"]);
        std::fs::write(directory.path().join("literal [path].txt"), b"original\n").unwrap();
        git(&["add", "."]);
        git(&["commit", "-m", "initial"]);
        let store = RepositoryStore::default();
        let repository = store
            .open(directory.path().to_owned())
            .await
            .unwrap()
            .unwrap();
        let historical = observe(
            &store,
            Arc::clone(&repository),
            ComparisonRequest {
                reference: "HEAD".into(),
                head: None,
                worktree: false,
                path: None,
            },
        )
        .await
        .unwrap()
        .value;
        assert_eq!(historical.path.len(), 1);
        assert_eq!(historical.path[0].change.staged, ChangeKind::Added);
        assert_eq!(
            historical.path[0].change.staged_stats,
            LineStats::Exact {
                added: 1,
                deleted: 0
            }
        );
        std::fs::write(directory.path().join("literal [path].txt"), b"changed\n").unwrap();
        let current = observe(
            &store,
            repository,
            ComparisonRequest {
                reference: historical.commit.to_string(),
                head: None,
                worktree: true,
                path: Some(RepositoryPath::new(b"literal [path].txt".to_vec()).unwrap()),
            },
        )
        .await
        .unwrap()
        .value;
        assert_eq!(current.commit, historical.commit);
        assert_eq!(current.path.len(), 1);
        assert_eq!(current.path[0].change.unstaged, ChangeKind::Modified);
        assert_eq!(
            current.path[0].change.unstaged_stats,
            LineStats::Exact {
                added: 1,
                deleted: 1
            }
        );
        let PathState::Tracked { index, .. } = &current.path[0].change.state else {
            panic!("missing branch source")
        };
        let PathState::Tracked {
            index: original, ..
        } = &historical.path[0].change.state
        else {
            panic!("missing historical source")
        };
        assert_eq!(index, original);
    }
    #[test]
    fn raw_comparison_preserves_literal_rename_paths_and_modes() {
        let bytes = format!(
            ":100644 100755 {} {} R075\0old\nname\0new\tname\0",
            "a".repeat(40),
            "b".repeat(40)
        );
        let record = parse(bytes.as_bytes(), false).unwrap();
        assert_eq!(record[0].path.raw(), b"new\tname");
        let PathState::Tracked {
            head,
            index,
            relocation: Some(relocation),
        } = &record[0].state
        else {
            panic!("missing source identities")
        };
        assert_eq!(head.mode, 0o100644);
        assert_eq!(index.mode, 0o100755);
        assert_eq!(relocation.origin.raw(), b"old\nname");
        assert!(parse(&bytes.as_bytes()[..bytes.len() - 1], false).is_err());
    }
}
