use anyhow::{Context, Result, ensure};
use forge_git::store::RepositoryStore;
use forge_harness::{checkpoint::GitCheckpoint, storage::SqliteStore};
use forge_harness::session::{HarnessSession, SessionStore};
use std::{path::PathBuf, sync::Arc, time::{Duration, Instant}};

#[tokio::main]
async fn main() -> Result<()> {
    let workspace = PathBuf::from(std::env::args_os().nth(1).context("usage: checkpoint_latency <repository>")?);
    let data = tempfile::tempdir()?;
    let repositories = Arc::new(RepositoryStore::default());
    let mut store = SqliteStore::open(data.path())?;
    let checkpoint = GitCheckpoint::new(&workspace);
    for session in ["first", "second"] {
        store.save_session(&serde_json::from_value::<HarnessSession>(serde_json::json!({
            "id": session,
            "primary_agent_id": HarnessSession::primary_agent_id(session),
            "name": "Checkpoint latency",
            "workspace": workspace,
            "backend": "benchmark",
            "provider_fork_state": { "state": "ready" },
            "model": "",
            "effort": "",
            "execution_mode": "read",
            "created_at_ms": 0,
            "updated_at_ms": 0
        }))?)?;
    }
    let mut exceeded = false;
    for (iteration, label) in ["cold", "reuse", "new-session"].into_iter().enumerate() {
        let session = if iteration == 2 { "second" } else { "first" };
        let started = Instant::now();
        let record = checkpoint.capture(&store.objects, &repositories, session, iteration as i64).await?;
        store.save_checkpoint(&record)?;
        let metadata = serde_json::to_vec(&record)?;
        let elapsed = started.elapsed();
        println!("{label}: {} ms, {} overrides, {} LFS exclusions, {} metadata bytes",
            elapsed.as_millis(), record.file.len(), record.excluded.len(), metadata.len());
        ensure!(store.load_checkpoint(&record.id)?.is_some(), "checkpoint was not persisted");
        exceeded |= elapsed >= Duration::from_secs(1);
    }
    ensure!(!exceeded, "checkpoint exceeded the one-second requirement");
    Ok(())
}
