use std::fs;
use std::time::{Duration, Instant};

use anyhow::Result;
use forge_github::issue_store::IssueStore;
use forge_github::lease::RepositoryLease;
use forge_github::model::{DetailRecord, PageInput, SnapshotState, SyncScope};
use redb::{Database, TableDefinition};
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
fn legacy_state_migrates_at_commit_and_exhausted_revisions_roll_back_the_whole_page() -> Result<()>
{
    let root = tempfile::tempdir()?;
    let database_path = root.path().join("issues.redb");
    let store = IssueStore::new(database_path.clone(), "owner/repo", Duration::ZERO)?;
    store.read_state()?;
    let seed = |revision: Option<u64>| -> Result<()> {
        let database = Database::create(&database_path)?;
        let transaction = database.begin_write()?;
        {
            let mut table =
                transaction.open_table(TableDefinition::<&str, &str>::new("sync_state"))?;
            let mut state = json!({"repo":"owner/repo","open_historical_complete":true});
            if let Some(revision) = revision {
                state["revision"] = revision.into();
            }
            table.insert("owner/repo", serde_json::to_string(&state)?.as_str())?;
        }
        transaction.commit()?;
        Ok(())
    };
    seed(None)?;
    assert_eq!(store.read_state()?.revision, 0);
    let output = root.path().join("snapshot.json");
    assert!(
        store
            .reconcile_snapshot(SnapshotState::Open, &output)?
            .republished
    );
    seed(Some(9_007_199_254_740_991))?;
    let page = serde_json::from_value(
        json!({"issues":[{"repo":"owner/repo","number":1,"title":"must roll back",
        "state":"OPEN","url":"https://github.com/owner/repo/issues/1"}]}),
    )?;
    assert!(
        store
            .upsert_page(SyncScope::Open, page)
            .unwrap_err()
            .to_string()
            .contains("revision exhausted")
    );
    let snapshot = store.snapshot(SnapshotState::Open)?;
    assert_eq!(snapshot.revision, 9_007_199_254_740_991);
    assert_eq!(snapshot.issue_count, 0);
    seed(Some(9_007_199_254_740_992))?;
    assert!(
        store
            .read_state()
            .unwrap_err()
            .to_string()
            .contains("revision exceeds")
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

#[test]
fn deletion_rejects_non_repository_layouts() -> Result<()> {
    let root = tempfile::tempdir()?;
    let database_path = root.path().join("issues.redb");
    let store = IssueStore::new(database_path.clone(), "owner/repo", Duration::ZERO)?;
    store.read_state()?;
    assert!(store.delete_repository_cache().is_err());
    assert!(database_path.exists());
    Ok(())
}

#[test]
fn construction_validates_identity_without_creating_storage() -> Result<()> {
    let directory = tempfile::tempdir()?;
    let database_path = directory.path().join("nested/issues.redb");
    assert!(IssueStore::new(database_path.clone(), "invalid", Duration::ZERO).is_err());
    let store = IssueStore::new(database_path.clone(), "Owner/Repo", Duration::ZERO)?;
    assert!(!database_path.parent().unwrap().exists());
    assert_eq!(store.read_state()?.repo, "owner/repo");
    assert!(database_path.exists());
    Ok(())
}

#[test]
fn operation_completion_releases_database_ownership() -> Result<()> {
    let directory = tempfile::tempdir()?;
    let database_path = directory.path().join("issues.redb");
    let store = IssueStore::new(database_path.clone(), "owner/repo", Duration::ZERO)?;
    let detail = store.upsert_detail(
        12,
        DetailRecord {
            repo: "untrusted/record".into(),
            number: 999,
            fetched_at: 123,
            item: json!({"title":"Cached detail"}),
        },
    )?;
    assert_eq!(detail.repo, "owner/repo");
    assert_eq!(detail.number, 12);
    drop(Database::create(&database_path)?);
    let other = IssueStore::new(database_path.clone(), "owner/repo", Duration::ZERO)?;
    let result = other.read_details(&[13, 12, 12])?;
    assert!(!result.details[0].found);
    assert_eq!(result.details[1].number, 12);
    assert_eq!(result.details[2].number, 12);
    drop(Database::create(&database_path)?);
    assert!(store.read_detail(12)?.found);
    Ok(())
}

#[test]
fn snapshot_publication_preserves_previous_output_when_database_decode_fails() -> Result<()> {
    let directory = tempfile::tempdir()?;
    let database_path = directory.path().join("issues.redb");
    let output = directory.path().join("open-snapshot.json");
    let store = IssueStore::new(database_path.clone(), "owner/repo", Duration::ZERO)?;
    let page: PageInput = serde_json::from_value(json!({"issues":[{
        "repo":"owner/repo", "number":1, "title":"First", "state":"OPEN",
        "url":"https://github.com/owner/repo/issues/1", "body":"Excluded body"
    }]}))?;
    store.upsert_page(SyncScope::Open, page)?;
    fs::write(&output, b"previous snapshot")?;
    assert_eq!(store.publish_snapshot(SnapshotState::Open, &output)?, 1);
    let previous = fs::read(&output)?;
    let decoded: serde_json::Value = serde_json::from_slice(&previous)?;
    assert_eq!(decoded["issues"][0]["title"], "First");
    assert!(decoded["issues"][0].get("body").is_none());
    assert!(
        store
            .publish_snapshot(SnapshotState::Open, &database_path)
            .is_err()
    );

    let database = Database::create(&database_path)?;
    let transaction = database.begin_write()?;
    {
        let mut issues = transaction.open_table(TableDefinition::<&str, &str>::new("issues"))?;
        issues.insert("owner/repo\0invalid-record", "invalid JSON")?;
    }
    transaction.commit()?;
    drop(database);
    assert!(
        store
            .publish_snapshot(SnapshotState::Open, &output)
            .is_err()
    );
    assert_eq!(fs::read(&output)?, previous);
    assert_eq!(fs::read_dir(directory.path())?.count(), 2);
    Ok(())
}

#[test]
fn lock_timeout_retains_database_without_archiving_it() -> Result<()> {
    let directory = tempfile::tempdir()?;
    let database_path = directory.path().join("issues.redb");
    let store = IssueStore::new(
        database_path.clone(),
        "owner/repo",
        Duration::from_millis(15),
    )?;
    store.read_state()?;
    let database = Database::create(&database_path)?;
    let started = Instant::now();
    let error = store.read_state().unwrap_err();
    assert!(format!("{error:#}").contains("database lock"));
    assert!(started.elapsed() < Duration::from_secs(1));
    assert!(database_path.exists());
    assert_eq!(fs::read_dir(directory.path())?.count(), 1);
    drop(database);
    assert_eq!(store.read_state()?.repo, "owner/repo");
    Ok(())
}
