use super::SqliteStore;
use crate::exchange::{Exchange, ExchangeNode};
use anyhow::{Context, Result, ensure};
use std::collections::{HashMap, HashSet};

/// Freeze the exact history and replacement session for an atomic rollback commit.
pub(crate) struct PreparedRollback {
    session: crate::session::HarnessSession,
    expected_session: String,
    expected_exchanges: Vec<(String, String)>,
    updates: Vec<Exchange>,
}

impl PreparedRollback {
    pub(super) fn apply(&self, transaction: &rusqlite::Transaction<'_>) -> Result<()> {
        use rusqlite::params;
        let payload: String = transaction.query_row(
            "SELECT payload FROM session_record WHERE id=?1",
            [&self.session.id],
            |row| row.get(0),
        )?;
        ensure!(
            payload == self.expected_session,
            "rollback session changed before commit"
        );
        let count: i64 = transaction.query_row(
            "SELECT count(*) FROM exchange_record WHERE session_id=?1",
            [&self.session.id],
            |row| row.get(0),
        )?;
        ensure!(
            usize::try_from(count)? == self.expected_exchanges.len(),
            "rollback history changed before commit"
        );
        for (id, expected) in &self.expected_exchanges {
            let current: String = transaction.query_row(
                "SELECT payload FROM exchange_record WHERE id=?1 AND session_id=?2",
                params![id, self.session.id],
                |row| row.get(0),
            )?;
            ensure!(
                &current == expected,
                "rollback exchange changed before commit"
            );
        }
        for exchange in &self.updates {
            transaction.execute(
                "UPDATE exchange_record SET payload=?2 WHERE id=?1",
                params![exchange.id, serde_json::to_string(exchange)?],
            )?;
        }
        transaction.execute(
            "UPDATE session_record SET payload=json_set(?2,'$.session.name',json_extract(payload,'$.session.name'),'$.session.active_task',json_extract(payload,'$.session.active_task')),updated_at_ms=?3 WHERE id=?1",
            params![self.session.id, super::encode_session(&self.session)?, self.session.updated_at_ms],
        )?;
        Ok(())
    }
}

impl SqliteStore {
    /// Reject restoration until every affected exchange and descendant has settled.
    pub(crate) fn require_settled_exchange_tree(
        &self,
        session_id: &str,
        roots: &[String],
    ) -> Result<()> {
        let records: Vec<Exchange> = self.list_payload(
            "SELECT payload FROM exchange_record WHERE session_id=?1",
            [session_id],
        )?;
        let by_id: HashMap<_, _> = records
            .iter()
            .map(|exchange| (exchange.id.as_str(), exchange))
            .collect();
        let mut pending = roots.to_vec();
        let mut visited = HashSet::new();
        while let Some(id) = pending.pop() {
            if !visited.insert(id.clone()) {
                continue;
            }
            let exchange = by_id
                .get(id.as_str())
                .context("rollback descendant state is missing")?;
            ensure!(
                exchange.is_finished(),
                "rollback requires settled descendant exchanges"
            );
            ensure!(
                exchange
                    .turn
                    .iter()
                    .all(|turn| turn.state() != crate::turn::TurnState::Running),
                "rollback requires settled descendant turns"
            );
            for node in &exchange.node_list {
                if let ExchangeNode::AgentReference { agent } = node {
                    let child = by_id
                        .get(agent.child_exchange_id.as_str())
                        .context("rollback descendant state is missing")?;
                    ensure!(
                        child.agent_id == agent.child_agent_id,
                        "rollback delegation has a different child owner"
                    );
                    pending.push(child.id.clone());
                }
            }
        }
        Ok(())
    }

    /// Capture rollback evidence without changing task or conversation state.
    pub(crate) fn prepare_rollback(
        &self,
        session: &crate::session::HarnessSession,
        roots: &[String],
    ) -> Result<PreparedRollback> {
        use crate::exchange::HistoryDisposition;
        let session_id = &session.id;
        let expected_session: String = self.connection.query_row(
            "SELECT payload FROM session_record WHERE id=?1",
            [session_id],
            |row| row.get(0),
        )?;
        let stored = super::decode_current_session(&expected_session)?
            .context("rollback session is unavailable")?;
        ensure!(
            stored.lease_owner == session.lease_owner,
            "Harness session lease ownership changed"
        );
        let expected_exchanges = self
            .connection
            .prepare("SELECT id,payload FROM exchange_record WHERE session_id=?1")?
            .query_map([session_id], |row| Ok((row.get(0)?, row.get(1)?)))?
            .collect::<rusqlite::Result<Vec<(String, String)>>>()?;
        self.require_settled_exchange_tree(session_id, roots)?;
        let records: Vec<Exchange> = self.list_payload(
            "SELECT payload FROM exchange_record WHERE session_id=?1",
            [session_id],
        )?;
        let mut by_id: HashMap<_, _> = records
            .into_iter()
            .map(|exchange| (exchange.id.clone(), exchange))
            .collect();
        let mut updates = Vec::new();
        for (index, root) in roots.iter().enumerate() {
            let disposition = if index == 0 {
                HistoryDisposition::RolledBack
            } else {
                HistoryDisposition::Superseded
            };
            let mut pending = vec![root.clone()];
            while let Some(id) = pending.pop() {
                let Some(mut exchange) = by_id.remove(&id) else {
                    continue;
                };
                for node in &exchange.node_list {
                    if let ExchangeNode::AgentReference { agent } = node {
                        pending.push(agent.child_exchange_id.clone());
                    }
                }
                exchange.set_disposition(disposition)?;
                updates.push(exchange);
            }
        }
        Ok(PreparedRollback {
            session: session.clone(),
            expected_session,
            expected_exchanges,
            updates,
        })
    }

    /// Commit rollback history and the replacement provider identity together.
    pub(crate) fn record_rollback(
        &mut self,
        session: &crate::session::HarnessSession,
        roots: &[String],
    ) -> Result<()> {
        let prepared = self.prepare_rollback(session, roots)?;
        let transaction = self
            .connection
            .transaction_with_behavior(rusqlite::TransactionBehavior::Immediate)?;
        prepared.apply(&transaction)?;
        transaction.commit()?;
        self.refresh_admission_membership(&session.id)
    }

    /// Resolve a child selection to the primary exchange that owns its checkpoint.
    pub(crate) fn checkpoint_owner(&self, session_id: &str, exchange_id: &str) -> Result<String> {
        let records: Vec<Exchange> = self.list_payload(
            "SELECT payload FROM exchange_record WHERE session_id=?1",
            [session_id],
        )?;
        let by_id: HashMap<_, _> = records
            .iter()
            .map(|exchange| (exchange.id.as_str(), exchange))
            .collect();
        let mut current = exchange_id;
        let mut visited = HashSet::new();
        loop {
            ensure!(
                visited.insert(current),
                "checkpoint ownership contains a cycle"
            );
            let exchange = by_id
                .get(current)
                .context("selected exchange is missing from this session")?;
            let mut parents = records.iter().filter(|parent| {
                parent.node_list.iter().any(|node| {
                    matches!(node,
                        ExchangeNode::AgentReference { agent }
                            if agent.child_exchange_id == exchange.id
                                && agent.child_agent_id == exchange.agent_id
                    )
                })
            });
            let Some(parent) = parents.next() else {
                return Ok(exchange.id.clone());
            };
            ensure!(
                parents.next().is_none(),
                "child exchange has multiple checkpoint owners"
            );
            current = &parent.id;
        }
    }
}

impl SqliteStore {
    pub(crate) fn capture_timeline_owner(
        &self,
        session_id: &str,
        exchange_id: &str,
    ) -> Result<crate::timeline::TimelineOwner> {
        let exchange = self
            .load_exchange_metadata(exchange_id)?
            .context("timeline event owner does not exist")?;
        ensure!(
            exchange.session_id == session_id,
            "timeline event crosses session ownership"
        );
        Ok(crate::timeline::TimelineOwner::exchange(&exchange))
    }

    pub(crate) fn validate_timeline_owner(
        &self,
        owner: &crate::timeline::TimelineOwner,
    ) -> Result<()> {
        ensure!(
            !owner.session_id().is_empty(),
            "timeline owner requires a session identity"
        );
        if let Some(exchange_id) = owner.exchange_id() {
            let exchange = self
                .load_exchange_metadata(exchange_id)?
                .context("timeline event owner does not exist")?;
            owner.validate_exchange(&exchange)?;
        }
        Ok(())
    }
}
