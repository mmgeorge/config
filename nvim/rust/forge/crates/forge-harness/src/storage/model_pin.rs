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
