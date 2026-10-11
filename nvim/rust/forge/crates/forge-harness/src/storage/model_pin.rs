use std::path::Path;
use std::time::Duration;

use anyhow::{Context, Result, ensure};
use rusqlite::{Connection, params};

/// Persists backend-wide pins independently of session leases and format versions.
pub(crate) struct ModelPinStore {
    connection: Connection,
}

impl ModelPinStore {
    /// Opens the Harness database without loading or resetting session state.
    pub(crate) fn open(data_root: &Path) -> Result<Self> {
        std::fs::create_dir_all(data_root).context("create model pin data directory")?;
        let connection = Connection::open(data_root.join("harness.sqlite3"))?;
        connection.busy_timeout(Duration::from_secs(5))?;
        connection.pragma_update(None, "journal_mode", "WAL")?;
        connection.pragma_update(None, "synchronous", "FULL")?;
        connection.execute_batch(
            "CREATE TABLE IF NOT EXISTS model_pin (
                backend TEXT NOT NULL,
                model TEXT NOT NULL,
                PRIMARY KEY (backend, model)
            );",
        )?;
        Ok(Self { connection })
    }

    /// Loads pin membership for one backend without requiring a model catalog.
    pub(crate) fn load(&self, backend: &str) -> Result<Vec<String>> {
        ensure!(!backend.trim().is_empty(), "model pin backend cannot be empty");
        let mut statement = self.connection.prepare(
            "SELECT model FROM model_pin WHERE backend = ?1 ORDER BY model",
        )?;
        Ok(statement.query_map([backend], |row| row.get(0))?
            .collect::<rusqlite::Result<Vec<_>>>()?)
    }

    /// Sets one pin idempotently without replacing other concurrent pin writes.
    pub(crate) fn set(&mut self, backend: &str, model: &str, pinned: bool) -> Result<Vec<String>> {
        ensure!(!backend.trim().is_empty(), "model pin backend cannot be empty");
        ensure!(!model.trim().is_empty(), "model pin identifier cannot be empty");
        let transaction = self.connection.transaction()?;
        if pinned {
            transaction.execute("INSERT OR IGNORE INTO model_pin (backend, model) VALUES (?1, ?2)", params![backend, model])?;
        } else {
            transaction.execute("DELETE FROM model_pin WHERE backend = ?1 AND model = ?2", params![backend, model])?;
        }
        let identifier_list = {
            let mut statement = transaction.prepare("SELECT model FROM model_pin WHERE backend = ?1 ORDER BY model")?;
            statement.query_map([backend], |row| row.get(0))?.collect::<rusqlite::Result<Vec<_>>>()?
        };
        transaction.commit()?;
        Ok(identifier_list)
    }
}

#[cfg(test)]
mod test {
    use super::ModelPinStore;

    #[test]
    fn model_pins_persist_independently_and_isolate_backends() -> anyhow::Result<()> {
        let directory = tempfile::tempdir()?;
        let mut store = ModelPinStore::open(directory.path())?;
        assert!(store.load("copilot")?.is_empty());
        assert_eq!(store.set("copilot", "temporarily-unavailable", true)?, ["temporarily-unavailable"]);
        store.set("copilot", "temporarily-unavailable", true)?;
        store.set("codex", "another-model", true)?;
        assert!(store.set("copilot", " ", true).is_err());
        drop(store);
        let session_store = super::super::SqliteStore::open(directory.path())?;
        drop(session_store);
        let mut reopened = ModelPinStore::open(directory.path())?;
        assert_eq!(reopened.load("copilot")?, ["temporarily-unavailable"]);
        assert_eq!(reopened.load("codex")?, ["another-model"]);
        assert!(reopened.set("copilot", "temporarily-unavailable", false)?.is_empty());
        assert!(reopened.set("copilot", "temporarily-unavailable", false)?.is_empty());
        assert_eq!(reopened.load("codex")?, ["another-model"]);
        Ok(())
    }

    #[test]
    fn model_pins_concurrent_writes_preserve_other_models() -> anyhow::Result<()> {
        let directory = tempfile::tempdir()?;
        ModelPinStore::open(directory.path())?;
        let barrier = std::sync::Arc::new(std::sync::Barrier::new(2));
        let handle_list = ["first", "second"].map(|model| {
            let data_root = directory.path().to_owned();
            let barrier = barrier.clone();
            std::thread::spawn(move || -> anyhow::Result<()> {
                let mut store = ModelPinStore::open(&data_root)?;
                barrier.wait();
                store.set("copilot", model, true)?;
                Ok(())
            })
        });
        for handle in handle_list { handle.join().unwrap()?; }
        assert_eq!(ModelPinStore::open(directory.path())?.load("copilot")?, ["first", "second"]);
        Ok(())
    }
}
