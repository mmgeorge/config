use super::*;
use std::sync::Arc;

#[derive(Clone, Copy, PartialEq, Eq)]
struct StorageVersion {
    external: i64,
    local: u64,
}

pub(super) struct CurrentExchangeCache {
    session_id: String,
    version: StorageVersion,
    state: Arc<CurrentExchangeState>,
}

#[derive(Default, PartialEq)]
pub(crate) struct CurrentExchangeState {
    pub(crate) latest: Option<AdmissionExchange>,
    pending: Option<String>,
}

#[derive(Clone, Deserialize, PartialEq)]
pub(crate) struct AdmissionExchange {
    pub(crate) id: String,
    pub(crate) state: crate::exchange::ExchangeState,
    pub(crate) kind: crate::exchange::ExchangeKind,
    pub(crate) plan_id: Option<String>,
    pub(crate) execution_id: Option<String>,
    pub(crate) goal_id: Option<String>,
    pub(crate) disposition: crate::exchange::HistoryDisposition,
    pub(crate) check_setup: Option<crate::checks::CheckSetupState>,
    pub(crate) created_at_ms: i64,
}

impl SqliteStore {
    /// Reuses current admission state until this connection or another writer changes storage.
    /// Refreshes only admission fields and pending-question identity on one read snapshot.
    /// Failed refreshes retain the previous cache but return the storage error to the caller.
    pub(crate) fn current_exchange_state(
        &self,
        session_id: &str,
    ) -> Result<Arc<CurrentExchangeState>> {
        let version = StorageVersion {
            external: self
                .connection
                .pragma_query_value(None, "data_version", |row| row.get(0))?,
            local: self.connection.total_changes(),
        };
        // A caller-owned transaction can roll back or hold an older snapshot. Never let it
        // consume or publish the cache used by subsequent autocommit reads.
        if !self.connection.is_autocommit() {
            return self.read_current_exchange_state(session_id).map(Arc::new);
        }
        if let Some(cached) = self.current_exchange.borrow().as_ref()
            && cached.session_id == session_id
            && cached.version == version
        {
            return Ok(cached.state.clone());
        }
        // Capture the version before opening the snapshot. A concurrent commit can cause
        // an extra refresh, but cannot label an old snapshot with the newer version.
        let refreshed = self.read_snapshot(|| self.read_current_exchange_state(session_id))?;
        let state = match self.current_exchange.borrow().as_ref() {
            Some(cached) if cached.session_id == session_id && *cached.state == refreshed => {
                cached.state.clone()
            }
            _ => Arc::new(refreshed),
        };
        *self.current_exchange.borrow_mut() = Some(CurrentExchangeCache {
            session_id: session_id.to_owned(),
            version,
            state: state.clone(),
        });
        Ok(state)
    }

    pub(crate) fn latest_exchange(&self, session_id: &str) -> Result<Option<Exchange>> {
        let read = || {
            self.current_exchange_state(session_id)?
                .latest
                .as_ref()
                .map(|exchange| self.load_exchange(&exchange.id))
                .transpose()
                .map(Option::flatten)
        };
        if self.connection.is_autocommit() {
            self.read_snapshot(read)
        } else {
            read()
        }
    }

    pub(crate) fn pending_exchange(&self, session_id: &str) -> Result<Option<Exchange>> {
        let state = self.current_exchange_state(session_id)?;
        let Some(id) = state.pending.as_deref() else {
            return Ok(None);
        };
        self.load_exchange(id).map(|exchange| {
            exchange.filter(|exchange| exchange.awaiting_input && exchange.elicitation.is_some())
        })
    }

    fn read_current_exchange_state(&self, session_id: &str) -> Result<CurrentExchangeState> {
        let latest = self.load_payload(
            "SELECT json_object('id',id,'state',json_extract(payload,'$.state'), \
             'kind',coalesce(json_extract(payload,'$.kind'),'chat'), \
             'plan_id',json_extract(payload,'$.plan_id'),'execution_id',json_extract(payload,'$.execution_id'), \
             'goal_id',json_extract(payload,'$.goal_id'),'disposition',coalesce(json_extract(payload,'$.disposition'),'current'), \
             'check_setup',json_extract(payload,'$.check_setup'),'created_at_ms',json_extract(payload,'$.created_at_ms')) \
             FROM exchange_record WHERE session_id=?1 AND agent_id=?2 ORDER BY ordinal DESC LIMIT 1",
            params![session_id, HarnessSession::primary_agent_id(session_id)],
        )?;
        let pending = self.connection.query_row(
                "SELECT id FROM exchange_record WHERE session_id=?1 AND agent_id=?2 AND json_extract(payload,'$.awaiting_input')=1 AND json_type(payload,'$.elicitation')='object' ORDER BY ordinal DESC LIMIT 1",
                params![session_id, HarnessSession::primary_agent_id(session_id)],
                |row| row.get(0),
            ).optional()?;
        Ok(CurrentExchangeState { latest, pending })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    fn exchange(session_id: &str, id: &str, ordinal: u64) -> Exchange {
        serde_json::from_value(json!({
            "id":id,"session_id":session_id,"agent_id":HarnessSession::primary_agent_id(session_id),
            "ordinal":ordinal,"prompt":id,"state":"complete","node_list":[],
            "created_at_ms":1,"attributed_matches_checkpoint":false
        }))
        .unwrap()
    }

    #[test]
    fn admission_state_excludes_transcript_and_survives_unrelated_writes() -> Result<()> {
        let root = tempfile::tempdir()?;
        let mut store = SqliteStore::open(root.path())?;
        store.save_session(&super::super::test::session("session", "workspace"))?;
        store.save_exchange(&exchange("session", "latest", 1))?;
        let cached = store.current_exchange_state("session")?;
        store.connection.execute("UPDATE exchange_record SET payload=json_set(payload,'$.node_list','not an array','$.prompt',json_object('invalid','transcript')) WHERE id='latest'", [])?;
        assert!(store.load_exchange("latest").is_err());
        assert!(Arc::ptr_eq(
            &cached,
            &store.current_exchange_state("session")?
        ));
        let writer = SqliteStore::open(root.path())?;
        writer.connection.execute(
            "UPDATE session_record SET updated_at_ms=updated_at_ms+1 WHERE id='session'",
            [],
        )?;
        assert!(Arc::ptr_eq(
            &cached,
            &store.current_exchange_state("session")?
        ));
        writer.connection.execute("UPDATE exchange_record SET payload=json_set(payload,'$.state','running') WHERE id='latest'", [])?;
        let changed = store.current_exchange_state("session")?;
        assert!(!Arc::ptr_eq(&cached, &changed));
        assert_eq!(
            changed.latest.as_ref().unwrap().state,
            crate::exchange::ExchangeState::Running
        );
        Ok(())
    }

    #[test]
    fn current_state_reuses_memory_and_tracks_local_writes_deletion_and_sessions() -> Result<()> {
        let root = tempfile::tempdir()?;
        let mut store = SqliteStore::open(root.path())?;
        store.save_session(&super::super::test::session("session", "workspace"))?;
        store.save_session(&super::super::test::session("other", "workspace"))?;
        assert!(store.latest_exchange("session")?.is_none());
        let first = exchange("session", "first", 1);
        store.save_exchange(&first)?;
        let cached = store.current_exchange_state("session")?;
        assert!(Arc::ptr_eq(
            &cached,
            &store.current_exchange_state("session")?
        ));
        let mut second = exchange("session", "second", 2);
        store.save_exchange(&second)?;
        assert_eq!(store.latest_exchange("session")?.unwrap().id, "second");
        second.check_setup = Some(crate::checks::CheckSetupState::Discovery);
        store.save_exchange(&second)?;
        assert!(
            store
                .latest_exchange("session")?
                .unwrap()
                .check_setup
                .is_some()
        );
        store.delete_exchange("second")?;
        assert_eq!(store.latest_exchange("session")?.unwrap().id, "first");
        assert!(store.latest_exchange("other")?.is_none());
        assert_eq!(store.latest_exchange("session")?.unwrap().id, "first");
        let reopened = SqliteStore::open(root.path())?;
        assert_eq!(reopened.latest_exchange("session")?.unwrap().id, "first");
        Ok(())
    }

    #[test]
    fn current_state_observes_independent_answers_and_preserved_commits() -> Result<()> {
        let root = tempfile::tempdir()?;
        let mut store = SqliteStore::open(root.path())?;
        store.save_session(&super::super::test::session("session", "workspace"))?;
        let mut question = exchange("session", "question", 1);
        question.awaiting_input = true;
        question.elicitation = Some(crate::plan::PlanElicitation::new(serde_json::from_value(
            json!({
                "id":"set","questions":[{"id":"choice","header":"Choice","question":"Which?",
                "options":[{"label":"First","description":"First choice","recommended":true}],"allow_freeform":true}]
            }),
        )?));
        store.save_exchange(&question)?;
        store.save_exchange(&exchange("session", "latest", 2))?;
        assert_eq!(store.pending_exchange("session")?.unwrap().id, "question");
        let mut writer = SqliteStore::open(root.path())?;
        let request = json!({"operation_id":"answer","target":{"owner":"interaction","id":"question",
            "set_id":"set","revision":1},"question_id":"choice","response":{"kind":"selected","option":"First"}});
        writer.connection.execute_batch("CREATE TRIGGER fail_receipt BEFORE INSERT ON command_receipt BEGIN SELECT RAISE(ABORT,'receipt failed'); END;")?;
        assert!(writer.answer_question_command("session", &request).is_err());
        assert!(
            store
                .pending_exchange("session")?
                .unwrap()
                .elicitation
                .unwrap()
                .answer
                .is_empty()
        );
        writer
            .connection
            .execute_batch("DROP TRIGGER fail_receipt")?;
        writer.answer_question_command("session", &request)?;
        assert_eq!(
            store
                .pending_exchange("session")?
                .unwrap()
                .elicitation
                .unwrap()
                .answer
                .len(),
            1
        );
        store.save_exchange(&question)?;
        assert_eq!(
            store
                .pending_exchange("session")?
                .unwrap()
                .elicitation
                .unwrap()
                .answer
                .len(),
            1
        );
        question.elicitation = None;
        question.awaiting_input = false;
        writer.save_exchange(&question)?;
        assert!(store.pending_exchange("session")?.is_none());
        Ok(())
    }

    #[test]
    fn current_state_never_publishes_rolled_back_or_old_transaction_reads() -> Result<()> {
        let root = tempfile::tempdir()?;
        let mut store = SqliteStore::open(root.path())?;
        store.save_session(&super::super::test::session("session", "workspace"))?;
        store.save_exchange(&exchange("session", "first", 1))?;
        store.current_exchange_state("session")?;
        let transaction = store.connection.unchecked_transaction()?;
        transaction.execute("UPDATE exchange_record SET payload=json_set(payload,'$.prompt','tentative') WHERE id='first'", [])?;
        assert_eq!(
            store.latest_exchange("session")?.unwrap().prompt,
            "tentative"
        );
        transaction.rollback()?;
        assert_eq!(store.latest_exchange("session")?.unwrap().prompt, "first");
        let transaction = store.connection.unchecked_transaction()?;
        assert_eq!(store.latest_exchange("session")?.unwrap().prompt, "first");
        let mut writer = SqliteStore::open(root.path())?;
        let mut updated = exchange("session", "first", 1);
        updated.prompt = "committed elsewhere".into();
        writer.save_exchange(&updated)?;
        assert_eq!(store.latest_exchange("session")?.unwrap().prompt, "first");
        transaction.commit()?;
        assert_eq!(
            store.latest_exchange("session")?.unwrap().prompt,
            "committed elsewhere"
        );
        Ok(())
    }

    #[test]
    fn current_state_skips_historical_hydration_and_propagates_refresh_errors() -> Result<()> {
        let root = tempfile::tempdir()?;
        let mut store = SqliteStore::open(root.path())?;
        store.save_session(&super::super::test::session("session", "workspace"))?;
        store.save_exchange(&exchange("session", "old", 1))?;
        let latest = exchange("session", "latest", 2);
        store.save_exchange(&latest)?;
        store.connection.execute("UPDATE exchange_record SET payload=json_set(payload,'$.state','invalid') WHERE id='old'", [])?;
        assert!(store.list_exchange("session").is_err());
        assert_eq!(store.latest_exchange("session")?.unwrap().id, "latest");
        let cached = store.current_exchange_state("session")?;
        store.connection.execute("UPDATE exchange_record SET payload=json_set(payload,'$.state','invalid') WHERE id='latest'", [])?;
        assert!(store.current_exchange_state("session").is_err());
        assert!(Arc::ptr_eq(
            &cached,
            &store.current_exchange.borrow().as_ref().unwrap().state
        ));
        store.save_exchange(&latest)?;
        assert_eq!(store.latest_exchange("session")?.unwrap().id, "latest");
        Ok(())
    }
}
