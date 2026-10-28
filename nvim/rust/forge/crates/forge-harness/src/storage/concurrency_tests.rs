use std::fs;
use std::io::{Read, Write};
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Stdio};
use std::time::{Duration, Instant};

use anyhow::{Context, Result, ensure};
use rusqlite::params;

use super::SqliteStore;
use super::test::session;
use crate::session::{SessionLeaseConflict, SessionStore};

struct ProcessOwner {
    child: Child,
    root: PathBuf,
    client: String,
}

impl ProcessOwner {
    fn spawn(root: &Path, session_id: &str, client: &str) -> Result<Self> {
        let log = fs::File::create(root.join(format!("{client}.log")))?;
        let child = Command::new(std::env::current_exe()?)
            .args([
                "--exact",
                "storage::concurrency_tests::session_process_worker",
                "--nocapture",
            ])
            .env("FORGE_STORAGE_TEST_DIRECTORY", root)
            .env("FORGE_STORAGE_TEST_SESSION", session_id)
            .env("FORGE_STORAGE_TEST_CLIENT", client)
            .stdin(Stdio::piped())
            .stdout(Stdio::from(log.try_clone()?))
            .stderr(Stdio::from(log))
            .spawn()?;
        Ok(Self {
            child,
            root: root.to_owned(),
            client: client.to_owned(),
        })
    }

    fn outcome(&mut self) -> Result<String> {
        let path = self.root.join(format!("{}.outcome", self.client));
        wait_until(|| {
            if path.exists() {
                return Ok(true);
            }
            if let Some(status) = self.child.try_wait()? {
                anyhow::bail!("worker exited with {status}: {}", self.log());
            }
            Ok(false)
        })?;
        Ok(fs::read_to_string(path)?)
    }

    fn finish(&mut self, crash: bool) -> Result<()> {
        if crash {
            self.child.kill()?;
        } else {
            self.release()?;
        }
        let mut status = None;
        wait_until(|| {
            status = self.child.try_wait()?;
            Ok(status.is_some())
        })?;
        ensure!(
            crash || status.unwrap().success(),
            "worker failed: {}",
            self.log()
        );
        Ok(())
    }

    fn release(&mut self) -> Result<()> {
        if let Some(mut input) = self.child.stdin.take() {
            if let Err(error) = input.write_all(b"x") {
                if error.kind() != std::io::ErrorKind::BrokenPipe {
                    return Err(error.into());
                }
            }
        }
        Ok(())
    }

    fn log(&self) -> String {
        fs::read_to_string(self.root.join(format!("{}.log", self.client)))
            .unwrap_or_else(|error| error.to_string())
    }
}

impl Drop for ProcessOwner {
    fn drop(&mut self) {
        let _ = self.child.kill();
        let _ = self.child.wait();
    }
}

fn wait_until(mut ready: impl FnMut() -> Result<bool>) -> Result<()> {
    let deadline = Instant::now() + Duration::from_secs(15);
    while !ready()? {
        ensure!(
            Instant::now() < deadline,
            "storage worker did not finish within 15 seconds"
        );
        std::thread::sleep(Duration::from_millis(10));
    }
    Ok(())
}

#[test]
fn session_process_worker() -> Result<()> {
    let Some(root) = std::env::var_os("FORGE_STORAGE_TEST_DIRECTORY") else {
        return Ok(());
    };
    let root = PathBuf::from(root);
    let session_id = std::env::var("FORGE_STORAGE_TEST_SESSION")?;
    let client = std::env::var("FORGE_STORAGE_TEST_CLIENT")?;
    wait_until(|| Ok(root.join("start").exists()))?;
    let mut store = SqliteStore::open(&root)?;
    let outcome = match store.acquire_session_lease(&session_id, &client) {
        Ok(_) => "owned",
        Err(error) if error.is::<SessionLeaseConflict>() => "conflict",
        Err(error) => return Err(error),
    };
    let temporary = root.join(format!("{client}.pending"));
    fs::write(&temporary, outcome)?;
    fs::rename(temporary, root.join(format!("{client}.outcome")))?;
    if outcome == "owned" {
        let mut command = [0];
        std::io::stdin().read_exact(&mut command)?;
        for ordinal in 0..16 {
            store.rename_owned_session(
                &session_id,
                &client,
                &format!("{client}-{ordinal}"),
                None,
                ordinal,
            )?;
        }
        store.release_session_lease(&session_id, &client)?;
    }
    Ok(())
}

#[test]
fn session_ownership_survives_process_boundaries() -> Result<()> {
    for crash in [false, true] {
        let directory = tempfile::tempdir()?;
        let root = directory.path();
        let mut store = SqliteStore::open(root)?;
        store.save_session(&session("shared", "workspace"))?;
        store.save_session(&session("independent", "workspace"))?;
        let plan = root.join("preserved-plan");
        fs::write(&plan, "preserve active session")?;

        let mut first = ProcessOwner::spawn(root, "shared", "first")?;
        let mut second = ProcessOwner::spawn(root, "shared", "second")?;
        let mut independent = ProcessOwner::spawn(root, "independent", "independent")?;
        fs::write(root.join("start"), "start together")?;
        let first_outcome = first.outcome()?;
        let second_outcome = second.outcome()?;
        ensure!(
            matches!(
                (first_outcome.as_str(), second_outcome.as_str()),
                ("owned", "conflict") | ("conflict", "owned")
            ),
            "same session admitted competing owners: {first_outcome}, {second_outcome}"
        );
        assert_eq!(independent.outcome()?, "owned");
        let stale = store
            .load_session("shared")?
            .context("shared session disappeared")?;
        let error = store
            .delete_session_with("shared", || {
                fs::remove_file(&plan)?;
                Ok(())
            })
            .unwrap_err();
        assert!(error.is::<SessionLeaseConflict>());
        assert_eq!(fs::read_to_string(&plan)?, "preserve active session");
        assert!(
            store
                .acquire_session_lease("shared", "replacement")
                .is_err()
        );
        assert!(
            store
                .rename_owned_session("shared", "replacement", "invalid", None, 1)
                .is_err()
        );

        independent.release()?;
        if first_outcome == "owned" {
            first.finish(crash)?;
            second.finish(false)?;
        } else {
            second.finish(crash)?;
            first.finish(false)?;
        }
        independent.finish(false)?;
        assert_eq!(
            store.load_session("independent")?.unwrap().name,
            "independent-15"
        );
        store.acquire_session_lease("shared", "replacement")?;
        assert!(
            store
                .save_owned_session(&stale, stale.lease_owner.as_deref().unwrap())
                .is_err()
        );
        store.rename_owned_session("shared", "replacement", "recovered", None, 20)?;
        assert!(store.delete_session("shared").is_err());
        store.release_session_lease("shared", "replacement")?;
        store.delete_session_with("shared", || {
            fs::remove_file(&plan)?;
            Ok(())
        })?;
        assert!(store.load_session("shared")?.is_none());
        assert!(!plan.exists());
    }
    Ok(())
}

#[test]
fn deletion_holds_ownership_through_cleanup_and_releases_on_failure() -> Result<()> {
    let directory = tempfile::tempdir()?;
    let mut store = SqliteStore::open(directory.path())?;
    store.save_session(&session("target", "workspace"))?;
    let mut competitor = SqliteStore::open(directory.path())?;
    let failure = store
        .delete_session_with("target", || {
            let conflict = competitor
                .acquire_session_lease("target", "competitor")
                .unwrap_err();
            assert!(conflict.is::<SessionLeaseConflict>());
            anyhow::bail!("cleanup failure")
        })
        .unwrap_err();
    assert!(format!("{failure:#}").contains("cleanup failure"));
    assert!(store.load_session("target")?.is_some());
    assert!(
        competitor
            .acquire_session_lease("target", "competitor")
            .is_err()
    );
    assert!(store.list_session(None)?.is_empty());
    drop(competitor);
    store.recover_session_deletions()?;
    assert!(store.load_session("target")?.is_none());
    Ok(())
}

#[test]
fn rename_preserves_current_state_and_commits_its_event_atomically() -> Result<()> {
    let directory = tempfile::tempdir()?;
    let mut store = SqliteStore::open(directory.path())?;
    store.save_session(&session("target", "workspace"))?;
    store.acquire_session_lease("target", "owner")?;
    store.connection.execute("UPDATE session_record SET payload=json_set(payload,'$.session.resolved_model','latest') WHERE id='target'", [])?;
    let renamed = store.rename_owned_session("target", "owner", "renamed", Some("target"), 5)?;
    assert_eq!(renamed.resolved_model.as_deref(), Some("latest"));
    assert_eq!(store.list_session_event("target")?.len(), 1);
    assert!(
        store
            .rename_owned_session("target", "other", "invalid", None, 6)
            .is_err()
    );
    assert!(
        store
            .rename_owned_session("target", "owner", "invalid", Some("target"), 6)
            .is_err()
    );
    assert_eq!(store.list_session_event("target")?.len(), 1);
    store.connection.execute_batch("CREATE TRIGGER reject_rename_event BEFORE INSERT ON session_event_record BEGIN SELECT RAISE(ABORT,'event persistence failed'); END;")?;
    assert!(
        store
            .rename_owned_session("target", "owner", "rolled-back", None, 7)
            .is_err()
    );
    assert_eq!(store.load_session("target")?.unwrap().name, "renamed");
    store.connection.execute("UPDATE session_record SET payload=json_set(payload,'$.session.lease_expires_at_ms',0) WHERE id=?1", params!["target"])?;
    assert_eq!(store.load_session("target")?.unwrap().name, "renamed");
    let mut competitor = SqliteStore::open(directory.path())?;
    assert!(
        competitor
            .acquire_session_lease("target", "other")
            .unwrap_err()
            .is::<SessionLeaseConflict>()
    );
    Ok(())
}

#[test]
fn metadata_rename_survives_runtime_writes_and_expires_with_owner() -> Result<()> {
    let directory = tempfile::tempdir()?;
    let mut store = SqliteStore::open(directory.path())?;
    store.save_session(&session("target", "workspace"))?;
    let mut stale = store.acquire_session_lease("target", "owner")?;
    let writer = store.session_metadata_writer("target", "owner")?;
    writer.rename("independent rename", Some("target"), 50)?;
    assert_eq!(
        store
            .initialize_session_name("target", "owner", "automatic title")?
            .name,
        "independent rename"
    );
    stale.resolved_model = Some("runtime progress".into());
    store.save_owned_session(&stale, "owner")?;
    let current = store.load_session("target")?.unwrap();
    assert_eq!(current.name, "independent rename");
    assert_eq!(current.resolved_model, stale.resolved_model);
    store.save_session(&stale)?;
    assert_eq!(
        store.load_session("target")?.unwrap().name,
        "independent rename"
    );
    assert!(
        writer
            .rename("stale generation", Some("target"), 51)
            .is_err()
    );
    assert_eq!(store.list_session_event("target")?.len(), 1);
    store.release_session_lease("target", "owner")?;
    let mut successor = SqliteStore::open(directory.path())?;
    successor.acquire_session_lease("target", "successor")?;
    assert!(writer.rename("expired writer", None, 52).is_err());
    assert_eq!(
        successor.load_session("target")?.unwrap().name,
        "independent rename"
    );
    Ok(())
}
