use std::path::Path;

use anyhow::{Result, ensure};
use rusqlite::{Connection, params};

/// Persists backend-wide pins independently of session leases and format versions.
pub(crate) struct ModelPinStore {
    connection: Connection,
}

impl ModelPinStore {
    /// Opens the Harness database without loading or resetting session state.
    pub(crate) fn open(data_root: &Path) -> Result<Self> {
        let super::SqliteStore { connection, .. } = super::SqliteStore::open(data_root)?;
        Ok(Self { connection })
    }

    /// Loads pin membership for one backend without requiring a model catalog.
    pub(crate) fn load(&self, backend: &str) -> Result<Vec<String>> {
        ensure!(
            !backend.trim().is_empty(),
            "model pin backend cannot be empty"
        );
        let mut statement = self
            .connection
            .prepare("SELECT model FROM model_pin WHERE backend = ?1 ORDER BY model")?;
        Ok(statement
            .query_map([backend], |row| row.get(0))?
            .collect::<rusqlite::Result<Vec<_>>>()?)
    }

    /// Sets one pin idempotently without replacing other concurrent pin writes.
    pub(crate) fn set(&mut self, backend: &str, model: &str, pinned: bool) -> Result<Vec<String>> {
        ensure!(
            !backend.trim().is_empty(),
            "model pin backend cannot be empty"
        );
        ensure!(
            !model.trim().is_empty(),
            "model pin identifier cannot be empty"
        );
        let transaction = self
            .connection
            .transaction_with_behavior(rusqlite::TransactionBehavior::Immediate)?;
        if pinned {
            transaction.execute(
                "INSERT OR IGNORE INTO model_pin (backend, model) VALUES (?1, ?2)",
                params![backend, model],
            )?;
        } else {
            transaction.execute(
                "DELETE FROM model_pin WHERE backend = ?1 AND model = ?2",
                params![backend, model],
            )?;
        }
        let identifier_list = {
            let mut statement = transaction
                .prepare("SELECT model FROM model_pin WHERE backend = ?1 ORDER BY model")?;
            statement
                .query_map([backend], |row| row.get(0))?
                .collect::<rusqlite::Result<Vec<_>>>()?
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
        assert_eq!(
            store.set("copilot", "temporarily-unavailable", true)?,
            ["temporarily-unavailable"]
        );
        store.set("copilot", "temporarily-unavailable", true)?;
        store.set("codex", "another-model", true)?;
        assert!(store.set("copilot", " ", true).is_err());
        drop(store);
        let session_store = super::super::SqliteStore::open(directory.path())?;
        drop(session_store);
        let mut reopened = ModelPinStore::open(directory.path())?;
        assert_eq!(reopened.load("copilot")?, ["temporarily-unavailable"]);
        assert_eq!(reopened.load("codex")?, ["another-model"]);
        assert!(
            reopened
                .set("copilot", "temporarily-unavailable", false)?
                .is_empty()
        );
        assert!(
            reopened
                .set("copilot", "temporarily-unavailable", false)?
                .is_empty()
        );
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
        for handle in handle_list {
            handle.join().unwrap()?;
        }
        assert_eq!(
            ModelPinStore::open(directory.path())?.load("copilot")?,
            ["first", "second"]
        );
        Ok(())
    }

    #[test]
    fn model_pin_write_waits_for_another_connection() -> anyhow::Result<()> {
        let directory = tempfile::tempdir()?;
        let mut holder = ModelPinStore::open(directory.path())?;
        let mut contender = ModelPinStore::open(directory.path())?;
        let transaction = holder
            .connection
            .transaction_with_behavior(rusqlite::TransactionBehavior::Immediate)?;
        let (started, waiting) = std::sync::mpsc::channel();
        let (completed, outcome) = std::sync::mpsc::channel();
        let writer = std::thread::spawn(move || {
            started.send(()).unwrap();
            completed
                .send(contender.set("copilot", "waiting-model", true))
                .unwrap();
        });
        waiting.recv()?;
        assert!(matches!(
            outcome.recv_timeout(std::time::Duration::from_millis(50)),
            Err(std::sync::mpsc::RecvTimeoutError::Timeout)
        ));
        transaction.commit()?;
        assert_eq!(
            outcome.recv_timeout(std::time::Duration::from_secs(2))??,
            ["waiting-model"]
        );
        writer.join().unwrap();
        assert_eq!(holder.load("copilot")?, ["waiting-model"]);
        Ok(())
    }
}
