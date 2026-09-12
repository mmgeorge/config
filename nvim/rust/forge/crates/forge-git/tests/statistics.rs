mod support;

use std::sync::Arc;

use forge_diff::{
    cache::{AnalysisStore, CacheLimits},
    engine::{DiffEngine, DiffRequest},
    workers::WorkPriority,
};
use forge_git::{snapshot::LineStats, store::RepositoryStore};
use support::git;

fn fixture() -> tempfile::TempDir {
    let directory = tempfile::tempdir().unwrap();
    let root = directory.path();
    git(root, &["init", "--quiet"]);
    git(root, &["config", "core.autocrlf", "false"]);
    git(root, &["config", "core.safecrlf", "false"]);
    git(root, &["config", "user.name", "Forge"]);
    git(root, &["config", "user.email", "forge@example.invalid"]);
    for name in ["staged", "unstaged", "both", "deleted", "renamed"] {
        std::fs::write(root.join(name), "original\n").unwrap();
    }
    git(root, &["add", "."]);
    git(root, &["commit", "--quiet", "-m", "base"]);
    directory
}

#[tokio::test]
async fn diagnostic_skip_bypasses_cached_counts_and_source_acquisition() {
    let directory = fixture();
    let root = directory.path();
    std::fs::write(root.join("unstaged"), "original\nworktree\n").unwrap();
    let store = RepositoryStore::default();
    let repository = store.open(root.to_path_buf()).await.unwrap().unwrap();
    repository.observe_with_timing(&store).await.unwrap();
    store
        .diagnostic_skip_line_stats
        .store(true, std::sync::atomic::Ordering::Relaxed);
    let (observation, timing) = repository.observe_with_timing(&store).await.unwrap();
    assert!(timing.line_stats_detail.skipped);
    assert_eq!(timing.line_stats_detail.skipped_pairs, 1);
    assert_eq!(timing.line_stats_detail.eligible_pairs, 1);
    assert_eq!(timing.line_stats_detail.source_reads, 0);
    assert_eq!(timing.line_stats_detail.count_cache_hits, 0);
    for path in &observation.path {
        assert_eq!(path.change.staged_stats, LineStats::Unknown);
        assert_eq!(path.change.unstaged_stats, LineStats::Unknown);
        assert!(path.analysis.iter().all(Option::is_none));
    }
    store
        .diagnostic_skip_line_stats
        .store(false, std::sync::atomic::Ordering::Relaxed);
    let (_, timing) = repository.observe_with_timing(&store).await.unwrap();
    assert!(!timing.line_stats_detail.skipped);
    assert_eq!(timing.line_stats_detail.count_cache_hits, 1);
}

#[tokio::test]
async fn counts_only_changed_sides_reuses_index_and_retains_display_analysis() {
    let directory = fixture();
    let root = directory.path();
    for name in ["staged", "both"] {
        std::fs::write(root.join(name), "original\nstaged\n").unwrap();
        git(root, &["add", name]);
    }
    std::fs::write(root.join("both"), "original\nstaged\nworktree\n").unwrap();
    std::fs::write(root.join("unstaged"), "original\nworktree\n").unwrap();
    std::fs::remove_file(root.join("deleted")).unwrap();
    git(root, &["mv", "renamed", "destination"]);
    std::fs::write(root.join("untracked"), "first\r\nlast").unwrap();
    let store = RepositoryStore::default();
    let repository = store.open(root.to_path_buf()).await.unwrap().unwrap();
    let (observation, timing) = repository.observe_with_timing(&store).await.unwrap();
    let find = |name: &str| {
        observation
            .path
            .iter()
            .find(|path| path.change.path.raw() == name.as_bytes())
            .unwrap()
    };
    let added = LineStats::Exact {
        added: 1,
        deleted: 0,
    };
    assert_eq!(find("staged").change.staged_stats, added);
    assert_eq!(find("staged").change.unstaged_stats, LineStats::Unknown);
    assert_eq!(find("unstaged").change.unstaged_stats, added);
    assert_eq!(find("unstaged").change.staged_stats, LineStats::Unknown);
    assert_eq!(find("both").change.staged_stats, added);
    assert_eq!(find("both").change.unstaged_stats, added);
    assert_eq!(
        find("deleted").change.unstaged_stats,
        LineStats::Exact {
            added: 0,
            deleted: 1
        }
    );
    assert_eq!(
        find("destination").change.staged_stats,
        LineStats::Exact {
            added: 0,
            deleted: 0
        }
    );
    assert_eq!(
        find("untracked").change.unstaged_stats,
        LineStats::Exact {
            added: 2,
            deleted: 0
        }
    );
    assert_eq!(timing.line_stats_detail.unchanged_side_pairs, 0);
    assert_eq!(timing.line_stats_detail.source_reads, 12);
    let prepared = find("both").analysis[0].as_ref().unwrap();
    let retained = prepared.load(&repository, &store).await.unwrap().unwrap();
    assert_eq!(
        retained.source().new.bytes(),
        b"original\nstaged\nworktree\n"
    );
    let engine = DiffEngine::with_cache(Arc::clone(&store.analysis), 1);
    let displayed = engine
        .compare(DiffRequest {
            source: retained.source().clone(),
            priority: WorkPriority::Visible,
        })
        .await
        .unwrap();
    assert!(std::ptr::eq(&*retained, &*displayed));
    let (_, repeated) = repository.observe_with_timing(&store).await.unwrap();
    assert_eq!(repeated.line_stats_detail.compared_pairs, 0);
    assert_eq!(repeated.line_stats_detail.fast_count_pairs, 0);
    assert_eq!(repeated.line_stats_detail.cache_hits, 7);
    assert_eq!(repeated.line_stats_detail.count_cache_hits, 7);
    assert_eq!(repeated.line_stats_detail.source_reads, 0);
    std::fs::write(root.join("both"), "changed after snapshot\n").unwrap();
    assert!(prepared.load(&repository, &store).await.unwrap().is_none());
    std::fs::write(root.join("deleted"), "recreated\n").unwrap();
    assert!(
        find("deleted").analysis[0]
            .as_ref()
            .unwrap()
            .load(&repository, &store)
            .await
            .unwrap()
            .is_none()
    );
    repository.invalidate().unwrap();
    assert!(
        find("staged").analysis[1]
            .as_ref()
            .unwrap()
            .load(&repository, &store)
            .await
            .unwrap()
            .is_none()
    );
}

#[tokio::test]
async fn saturated_cache_preserves_exact_counts_and_unavailable_classification() {
    let directory = fixture();
    let root = directory.path();
    std::fs::write(
        root.join(".git/info/attributes"),
        "encoded working-tree-encoding=UTF-16LE\nfiltered filter=external\n",
    )
    .unwrap();
    git(
        root,
        &["config", "filter.external.clean", "must-not-execute"],
    );
    for (name, bytes) in [
        ("empty", b"".as_slice()),
        ("unterminated", b"one"),
        ("crlf", b"one\r\ntwo\r\n"),
        ("binary", b"one\0two"),
        ("invalid", b"\xff"),
        ("encoded", b"a\0\n\0"),
        ("filtered", b"one\n"),
    ] {
        std::fs::write(root.join(name), bytes).unwrap();
    }
    std::fs::write(
        root.join("oversized"),
        vec![b'x'; forge_diff::source::MAX_SOURCE_BYTES + 1],
    )
    .unwrap();
    let mut store = RepositoryStore::default();
    store.analysis = Arc::new(AnalysisStore::new(CacheLimits {
        entries: 0,
        source_bytes: 0,
        result_bytes: 0,
    }));
    let repository = store.open(root.to_path_buf()).await.unwrap().unwrap();
    let observation = repository.observe(&store).await.unwrap();
    for (name, count) in [
        ("empty", 0),
        ("unterminated", 1),
        ("crlf", 2),
        ("encoded", 1),
    ] {
        let path = observation
            .path
            .iter()
            .find(|path| path.change.path.raw() == name.as_bytes())
            .unwrap();
        assert_eq!(
            path.change.unstaged_stats,
            LineStats::Exact {
                added: count,
                deleted: 0
            },
            "{name}"
        );
        assert!(path.analysis.iter().all(Option::is_none));
    }
    for name in ["binary", "invalid", "filtered", "oversized"] {
        let path = observation
            .path
            .iter()
            .find(|path| path.change.path.raw() == name.as_bytes())
            .unwrap();
        assert_eq!(path.change.unstaged_stats, LineStats::Unknown, "{name}");
    }
    assert_eq!(store.analysis.usage().admitted_entries, 0);
}

#[tokio::test]
async fn retained_worktree_analysis_rejects_changed_configuration_attributes_and_index() {
    for change in ["configuration", "attributes", "index"] {
        let directory = fixture();
        let root = directory.path();
        std::fs::write(root.join("unstaged"), "original\nnew\n").unwrap();
        let store = RepositoryStore::default();
        let repository = store.open(root.to_path_buf()).await.unwrap().unwrap();
        let observation = repository.observe(&store).await.unwrap();
        let prepared = observation.path[0].analysis[0].as_ref().unwrap();
        assert!(prepared.load(&repository, &store).await.unwrap().is_some());
        match change {
            "configuration" => {
                git(root, &["config", "core.autocrlf", "true"]);
            }
            "attributes" => {
                std::fs::write(root.join(".git/info/attributes"), "unstaged -text\n").unwrap();
            }
            "index" => {
                git(root, &["add", "unstaged"]);
            }
            _ => unreachable!(),
        }
        assert!(
            prepared.load(&repository, &store).await.unwrap().is_none(),
            "{change}"
        );
    }
}

#[tokio::test]
async fn count_cache_checks_size_and_modified_time_and_refresh_expires_preserved_metadata() {
    let directory = fixture();
    let root = directory.path();
    let path = root.join("untracked");
    std::fs::write(&path, b"one\nxxxx").unwrap();
    let modified = std::fs::metadata(&path).unwrap().modified().unwrap();
    let store = RepositoryStore::default();
    let repository = store.open(root.to_path_buf()).await.unwrap().unwrap();
    let (first, _) = repository.observe_with_timing(&store).await.unwrap();
    assert_eq!(
        first.path[0].change.unstaged_stats,
        LineStats::Exact {
            added: 2,
            deleted: 0
        }
    );

    std::fs::write(&path, b"a\nb\nc\nd\n").unwrap();
    std::fs::File::options()
        .write(true)
        .open(&path)
        .unwrap()
        .set_times(std::fs::FileTimes::new().set_modified(modified))
        .unwrap();
    let (cached, timing) = repository.observe_with_timing(&store).await.unwrap();
    assert_eq!(
        cached.path[0].change.unstaged_stats,
        first.path[0].change.unstaged_stats
    );
    assert_eq!(timing.line_stats_detail.source_reads, 0);

    repository.invalidate().unwrap();
    let (refreshed, timing) = repository.observe_with_timing(&store).await.unwrap();
    assert_eq!(
        refreshed.path[0].change.unstaged_stats,
        LineStats::Exact {
            added: 4,
            deleted: 0
        }
    );
    assert_eq!(timing.line_stats_detail.source_reads, 1);

    std::fs::write(&path, b"a\nb\nc\nd\ne\n").unwrap();
    std::fs::File::options()
        .write(true)
        .open(&path)
        .unwrap()
        .set_times(std::fs::FileTimes::new().set_modified(modified))
        .unwrap();
    let (resized, timing) = repository.observe_with_timing(&store).await.unwrap();
    assert_eq!(
        resized.path[0].change.unstaged_stats,
        LineStats::Exact {
            added: 5,
            deleted: 0
        }
    );
    assert_eq!(timing.line_stats_detail.source_reads, 1);

    std::fs::write(&path, b"a\nb\nc\nxxxx").unwrap();
    std::fs::File::options()
        .write(true)
        .open(&path)
        .unwrap()
        .set_times(
            std::fs::FileTimes::new().set_modified(modified + std::time::Duration::from_secs(2)),
        )
        .unwrap();
    let (modified, timing) = repository.observe_with_timing(&store).await.unwrap();
    assert_eq!(
        modified.path[0].change.unstaged_stats,
        LineStats::Exact {
            added: 4,
            deleted: 0
        }
    );
    assert_eq!(timing.line_stats_detail.source_reads, 1);
}

#[tokio::test]
async fn count_cache_recomputes_changed_index_objects_without_explicit_invalidation() {
    let directory = fixture();
    let root = directory.path();
    std::fs::write(root.join("staged"), b"original\nfirst\n").unwrap();
    git(root, &["add", "staged"]);
    let store = RepositoryStore::default();
    let repository = store.open(root.to_path_buf()).await.unwrap().unwrap();
    repository.observe(&store).await.unwrap();
    let (_, cached) = repository.observe_with_timing(&store).await.unwrap();
    assert_eq!(cached.line_stats_detail.source_reads, 0);
    std::fs::write(root.join("staged"), b"original\nfirst\nsecond\n").unwrap();
    git(root, &["add", "staged"]);
    let (changed, timing) = repository.observe_with_timing(&store).await.unwrap();
    assert_eq!(
        changed.path[0].change.staged_stats,
        LineStats::Exact {
            added: 2,
            deleted: 0
        }
    );
    assert_eq!(timing.line_stats_detail.count_cache_hits, 0);
    assert_eq!(timing.line_stats_detail.source_reads, 2);
}
