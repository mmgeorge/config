use std::fs;
use std::time::Duration;

use anyhow::Result;
use forge_github::issue_store::IssueStore;
use forge_github::lease::RepositoryLease;
use forge_github::model::{SnapshotState, SyncScope};
use redb::Database;
use serde_json::json;

#[test]
fn completion_recovery_observes_committed_revisions_after_interrupted_publication() -> Result<()> {
    let root = tempfile::tempdir()?;
    let database = root.path().join("issues.redb");
    let output = root.path().join("open-snapshot.json");
    let store = IssueStore::new(database.clone(), "owner/repo", Duration::ZERO)?;
    let empty = store.reconcile_snapshot(SnapshotState::Open, &output)?;
    assert!(!empty.ready && !empty.republished && !output.exists());
    let page = |title: &str| {
        serde_json::from_value(json!({
            "issues":[{"repo":"owner/repo","number":1,"title":title,"state":"OPEN",
                "url":"https://github.com/owner/repo/issues/1"}], "completed":true,
        }))
    };
    assert_eq!(
        store
            .upsert_page(SyncScope::Open, page("first")?)?
            .state
            .revision,
        1
    );
    assert!(
        store
            .reconcile_snapshot(SnapshotState::Open, &output)?
            .republished
    );
    let first = fs::read(&output)?;
    assert!(
        !store
            .reconcile_snapshot(SnapshotState::Open, &output)?
            .republished
    );
    assert_eq!(fs::read(&output)?, first);
    assert_eq!(
        store
            .upsert_page(SyncScope::Open, page("second")?)?
            .state
            .revision,
        2
    );
    assert_eq!(fs::read(&output)?, first);
    drop(store);
    let reopened = IssueStore::new(database, "owner/repo", Duration::ZERO)?;
    let recovery = reopened.reconcile_snapshot(SnapshotState::Open, &output)?;
    assert!(recovery.ready && recovery.republished && recovery.state.revision == 2);
    let current: serde_json::Value = serde_json::from_slice(&fs::read(&output)?)?;
    assert_eq!(current["revision"], 2);
    assert_eq!(current["issues"][0]["title"], "second");
    for invalid in [
        json!({"repo":"owner/repo","state":"open","issue_count":0,"issues":[]}),
        json!({"repo":"foreign/repo","revision":2,"state":"open","issue_count":0,"issues":[]}),
        json!({"repo":"owner/repo","revision":2,"state":"all","issue_count":0,"issues":[]}),
        json!({"repo":"owner/repo","revision":2,"state":"open","issue_count":3,"issues":[]}),
    ] {
        fs::write(&output, serde_json::to_vec(&invalid)?)?;
        assert!(
            reopened
                .reconcile_snapshot(SnapshotState::Open, &output)?
                .republished
        );
        let repaired: serde_json::Value = serde_json::from_slice(&fs::read(&output)?)?;
        assert_eq!(repaired, current);
    }
    fs::write(&output, "{\"repo\":")?;
    assert!(
        reopened
            .reconcile_snapshot(SnapshotState::Open, &output)?
            .republished
    );
    Ok(())
}

#[test]
fn deletion_requires_exclusive_ownership_and_preserves_unrelated_data() -> Result<()> {
    let root = tempfile::tempdir()?;
    let directory = root.path().join("repo");
    let database_path = directory.join("issues/issues.redb");
    let store = IssueStore::new(database_path.clone(), "owner/repo", Duration::ZERO)?;
    store.read_state()?;
    fs::write(directory.join("review.json"), "retained until deletion")?;
    let unrelated = root.path().join("unrelated");
    fs::write(&unrelated, "preserved")?;
    let operation = RepositoryLease::operation(&directory)?;
    assert!(
        store
            .delete_repository_cache()
            .unwrap_err()
            .to_string()
            .contains("Busy")
    );
    assert!(database_path.exists());
    drop(operation);
    let deletion = RepositoryLease::deletion(&directory)?;
    assert!(store.read_state().unwrap_err().to_string().contains("Busy"));
    assert!(store.delete_repository_cache().is_err());
    drop(deletion);
    let legacy_database = Database::create(&database_path)?;
    assert!(store.delete_repository_cache().is_err());
    assert!(database_path.exists());
    assert!(directory.join("review.json").exists());
    drop(legacy_database);
    fs::create_dir(directory.join("issues/sync.lock"))?;
    assert!(
        store
            .delete_repository_cache()
            .unwrap_err()
            .to_string()
            .contains("Busy")
    );
    assert!(database_path.exists());
    fs::remove_dir(directory.join("issues/sync.lock"))?;
    assert!(store.delete_repository_cache()?);
    assert!(!directory.exists());
    assert_eq!(fs::read_to_string(unrelated)?, "preserved");
    assert!(!store.delete_repository_cache()?);
    assert_eq!(store.read_state()?.repo, "owner/repo");
    Ok(())
}
