use super::*;

const INTENT_KIND: &str = "configuration_intent_v1";

/// Retain the exact settings mutation paired with a committed session configuration.
#[derive(Clone, Deserialize, Serialize)]
pub(crate) struct ConfigurationIntent {
    pub(crate) id: String,
    pub(crate) session: String,
    pub(crate) workspace: String,
    pub(crate) provider: String,
    pub(crate) revision: u64,
    pub(crate) scope: crate::settings::SettingScope,
    pub(crate) portable: Option<crate::settings::PortableSettings>,
    pub(crate) provider_setting: Option<serde_json::Value>,
}

/// Retain the settings request and preference write in the same session transaction.
#[derive(Deserialize, Serialize)]
struct PendingConfiguration {
    intent: ConfigurationIntent,
    preference: Option<HarnessPreference>,
}

impl SqliteStore {
    #[cfg(test)]
    pub(crate) fn execute_configuration_test_sql(&self, sql: &str) -> Result<()> {
        self.connection.execute_batch(sql)?;
        Ok(())
    }
    pub(super) fn ensure_no_configuration_intent(
        connection: &Connection,
        session: &str,
    ) -> Result<()> {
        let pending: bool = connection.query_row(
            "SELECT EXISTS(SELECT 1 FROM command_receipt WHERE session_id=?1 AND result=?2)",
            params![session, INTENT_KIND],
            |row| row.get(0),
        )?;
        anyhow::ensure!(!pending, "session configuration requires durable recovery");
        Ok(())
    }
    /// Replay only an unacknowledged settings mutation from an owned host commit.
    pub(crate) fn recover_configuration_intent(
        &mut self,
        data_root: &Path,
        session: &str,
    ) -> Result<bool> {
        let Some(intent) = self.pending_configuration_intent(session)? else {
            return Ok(false);
        };
        let mut settings = crate::settings::SettingsStore::open(data_root)?;
        if !settings.configuration_applied(&intent)? {
            settings
                .commit_configuration(
                    intent.revision,
                    crate::settings::SettingTarget {
                        scope: intent.scope,
                        workspace: &intent.workspace,
                        session: &intent.session,
                    },
                    intent.portable.as_ref(),
                    intent
                        .provider_setting
                        .as_ref()
                        .map(|value| (intent.provider.as_str(), value)),
                    Some(&intent),
                    || Ok(()),
                )
                .context(
                    "configuration recovery could not safely apply the captured settings revision",
                )?;
        }
        self.clear_configuration_intent(&intent)?;
        Ok(true)
    }
    /// Atomically retain the settings and preference intent with the owned session.
    pub(crate) fn commit_configuration_intent(
        &mut self,
        session: &HarnessSession,
        client: &str,
        preference: Option<&HarnessPreference>,
        intent: &ConfigurationIntent,
    ) -> Result<()> {
        let transaction = self
            .connection
            .transaction_with_behavior(TransactionBehavior::Immediate)?;
        let payload: String = transaction.query_row(
            "SELECT payload FROM session_record WHERE id=?1",
            [&session.id],
            |row| row.get(0),
        )?;
        let current =
            decode_current_session(&payload)?.context("configuration session is missing")?;
        anyhow::ensure!(
            current.lease_owner.as_deref() == Some(client),
            "Harness session lease ownership changed"
        );
        anyhow::ensure!(
            intent.session == session.id
                && intent.workspace == session.workspace
                && intent.provider == session.provider,
            "configuration intent owner mismatch"
        );
        let pending: bool = transaction.query_row(
            "SELECT EXISTS(SELECT 1 FROM command_receipt WHERE session_id=?1 AND result=?2)",
            params![session.id, INTENT_KIND],
            |row| row.get(0),
        )?;
        anyhow::ensure!(!pending, "a configuration intent requires recovery");
        transaction.execute("UPDATE session_record SET workspace=?2, updated_at_ms=?3, payload=json_set(?4,'$.session.name',json_extract(payload,'$.session.name'),'$.session.active_task',json_extract(payload,'$.session.active_task')) WHERE id=?1", params![session.id, session.workspace, session.updated_at_ms, encode_session(session)?])?;
        let pending = PendingConfiguration {
            intent: intent.clone(),
            preference: preference.cloned(),
        };
        transaction.execute(
            "INSERT INTO command_receipt(session_id,id,request,result) VALUES(?1,?2,?3,?4)",
            params![session.id, intent.id, encode(&pending)?, INTENT_KIND],
        )?;
        transaction.commit()?;
        self.publish_admission(&session.id)
    }

    pub(crate) fn pending_configuration_intent(
        &self,
        session: &str,
    ) -> Result<Option<ConfigurationIntent>> {
        let mut statement = self
            .connection
            .prepare("SELECT request FROM command_receipt WHERE session_id=?1 AND result=?2")?;
        let mut rows = statement.query(params![session, INTENT_KIND])?;
        let Some(row) = rows.next()? else {
            return Ok(None);
        };
        let pending: PendingConfiguration = decode(&row.get::<_, String>(0)?)?;
        let intent = pending.intent;
        anyhow::ensure!(
            rows.next()?.is_none() && intent.session == session,
            "ambiguous configuration intents"
        );
        Ok(Some(intent))
    }

    pub(crate) fn clear_configuration_intent(
        &mut self,
        intent: &ConfigurationIntent,
    ) -> Result<()> {
        let payload: String = self.connection.query_row(
            "SELECT request FROM command_receipt WHERE session_id=?1 AND id=?2 AND result=?3",
            params![intent.session, intent.id, INTENT_KIND],
            |row| row.get(0),
        )?;
        let pending: PendingConfiguration = decode(&payload)?;
        if let Some(preference) = pending.preference {
            super::preferences::apply_configuration(&self.data_root, intent, &preference)?;
        }
        let deleted = self.connection.execute(
            "DELETE FROM command_receipt WHERE session_id=?1 AND id=?2 AND result=?3",
            params![intent.session, intent.id, INTENT_KIND],
        )?;
        anyhow::ensure!(
            deleted == 1,
            "configuration intent disappeared before completion"
        );
        Ok(())
    }
}
