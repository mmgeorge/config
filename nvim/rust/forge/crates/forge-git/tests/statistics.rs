use forge_git::test_support as support;

use std::sync::Arc;

use forge_diff::{
    engine::{DiffEngine, DiffRequest},
    workers::WorkPriority,
};
use forge_git::{snapshot::LineStats, store::RepositoryStore};
use support::{
    commit_index, init_repository, move_tracked, set_config, set_identity, stage, stage_all,
};

fn fixture() -> tempfile::TempDir {
    let directory = tempfile::tempdir().unwrap();
    let root = directory.path();
    init_repository(root);
    set_config(root, "core", "autocrlf", "false");
    set_config(root, "core", "safecrlf", "false");
    set_identity(root);
    for name in ["staged", "unstaged", "both", "deleted", "renamed"] {
        std::fs::write(root.join(name), "original\n").unwrap();
    }
    stage_all(root);
    commit_index(root, "base");
    directory
}

#[tokio::test]
async fn counts_only_changed_sides_reuses_index_and_retains_display_analysis() {
    let directory = fixture();
    let root = directory.path();
    for name in ["staged", "both"] {
        std::fs::write(root.join(name), "original\nstaged\n").unwrap();
        stage(root, &[name]);
    }
    std::fs::write(root.join("both"), "original\nstaged\nworktree\n").unwrap();
    std::fs::write(root.join("unstaged"), "original\nworktree\n").unwrap();
    std::fs::remove_file(root.join("deleted")).unwrap();
    move_tracked(root, "renamed", "destination");
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
                set_config(root, "core", "autocrlf", "true");
            }
            "attributes" => {
                std::fs::write(root.join(".git/info/attributes"), "unstaged -text\n").unwrap();
            }
            "index" => {
                stage(root, &["unstaged"]);
            }
            _ => unreachable!(),
        }
        assert!(
            prepared.load(&repository, &store).await.unwrap().is_none(),
            "{change}"
        );
    }
}
