use std::path::Path;

use anyhow::{Result, ensure};
use rusqlite::params;

/// Persists provider-wide pins independently of session leases and format versions.
pub(crate) struct ModelPinStore {
    connection: rusqlite::Connection,
}

impl ModelPinStore {
    /// Open persistent preferences without opening disposable history.
    pub(crate) fn open(data_root: &Path) -> Result<Self> {
        let connection = super::preferences::open(data_root)?;
        Ok(Self { connection })
    }

    /// Loads pin membership for one provider without requiring a model catalog.
    pub(crate) fn load(&self, provider: &str) -> Result<Vec<String>> {
        ensure!(
            !provider.trim().is_empty(),
            "model pin provider cannot be empty"
        );
        let mut statement = self
            .connection
            .prepare("SELECT model FROM model_pin WHERE backend = ?1 ORDER BY model")?;
        Ok(statement
            .query_map([provider], |row| row.get(0))?
            .collect::<rusqlite::Result<Vec<_>>>()?)
    }

    /// Sets one pin idempotently without replacing other concurrent pin writes.
    pub(crate) fn set(&mut self, provider: &str, model: &str, pinned: bool) -> Result<Vec<String>> {
        ensure!(
            !provider.trim().is_empty(),
            "model pin provider cannot be empty"
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
                params![provider, model],
            )?;
        } else {
            transaction.execute(
                "DELETE FROM model_pin WHERE backend = ?1 AND model = ?2",
                params![provider, model],
            )?;
        }
        let identifier_list = {
            let mut statement = transaction
                .prepare("SELECT model FROM model_pin WHERE backend = ?1 ORDER BY model")?;
            statement
                .query_map([provider], |row| row.get(0))?
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
                store.set("fixture-beta", model, true)?;
                Ok(())
            })
        });
        for handle in handle_list {
            handle.join().unwrap()?;
        }
        assert_eq!(
            ModelPinStore::open(directory.path())?.load("fixture-beta")?,
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
                .send(contender.set("fixture-beta", "waiting-model", true))
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
        assert_eq!(holder.load("fixture-beta")?, ["waiting-model"]);
        Ok(())
    }
}
