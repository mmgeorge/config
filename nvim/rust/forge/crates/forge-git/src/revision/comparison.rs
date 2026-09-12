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

#[derive(Clone, Debug)]
pub struct ComparisonRequest {
    pub reference: String,
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
        !request.reference.is_empty()
            && request.reference.len() <= 4096
            && !request.reference.chars().any(char::is_control),
        "invalid comparison reference"
    );
    let path = request.path.as_ref().map(resolve_argument).transpose()?;
    let retained = request.reference.capacity()
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
            let mut command = Command::new("git");
            command
                .args([
                    "--no-pager",
                    "--no-optional-locks",
                    "--literal-pathspecs",
                    "-C",
                ])
                .arg(root)
                .args(["rev-parse", "--verify", "--end-of-options"])
                .arg(format!("{}^{{commit}}", request.reference));
            let output = read_command(
                &mut command,
                CommandLimits {
                    stdout_bytes: 128,
                    stderr_bytes: 4096,
                    timeout: Duration::from_secs(30),
                },
                &mut check,
            )?;
            ensure!(
                output.status.success(),
                "comparison reference unavailable: {}",
                String::from_utf8_lossy(&output.stderr)
            );
            let commit = gix::ObjectId::from_hex(output.stdout.trim_ascii())
                .context("invalid comparison commit")?;
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
                .arg(commit.to_string())
                .arg("--");
            if let Some(path) = path {
                command.arg(path);
            }
            let output = read_command(
                &mut command,
                CommandLimits {
                    stdout_bytes: 16 * 1024 * 1024,
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
            crate::snapshot::collect_line_stats(
                &local,
                &worker.identity,
                &mut path,
                Some(&analysis),
                Some(&worker.line_stats),
                skip_line_stats,
                &mut check,
            )?;
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

fn parse(bytes: &[u8], worktree: bool) -> Result<Vec<PathRecord>> {
    ensure!(
        bytes.len() <= 16 * 1024 * 1024,
        "comparison metadata exceeds 16 MiB"
    );
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
        ensure!(records.len() < 10_000, "comparison exceeds 10000 paths");
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
