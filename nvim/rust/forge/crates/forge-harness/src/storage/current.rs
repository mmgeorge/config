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
            Ok(self.list_exchange_payload("SELECT payload FROM exchange_record WHERE session_id=?1 AND agent_id=?2 ORDER BY ordinal DESC LIMIT 1",params![session_id,HarnessSession::primary_agent_id(session_id)])?.pop())
        };
        if self.connection.is_autocommit() {
            self.read_snapshot(read)
        } else {
            read()
        }
    }

    pub(crate) fn exchange_in_state(
        &self,
        session_id: &str,
        state: crate::exchange::ExchangeState,
    ) -> Result<Option<Exchange>> {
        let read = || {
            Ok(self.list_exchange_payload("SELECT payload FROM exchange_record WHERE session_id=?1 AND agent_id=?2 AND json_extract(payload,'$.state')=?3 ORDER BY ordinal LIMIT 1",params![session_id,HarnessSession::primary_agent_id(session_id),serde_json::to_value(state)?.as_str()])?.pop())
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
            exchange
                .filter(|exchange| exchange.awaiting_input() && exchange.elicitation().is_some())
        })
    }

    fn read_current_exchange_state(&self, session_id: &str) -> Result<CurrentExchangeState> {
        let latest = self.load_payload(
            "SELECT json_object('id',id,'state',json_extract(payload,'$.state'), \
             'disposition',coalesce(json_extract(payload,'$.disposition'),'current'), \
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
