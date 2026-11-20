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
    /// Session rows recorded during the rolled-back interactions, as stored and as re-owned.
    retired_events: Vec<RetiredSessionEvent>,
}

/// Move one session row into the rolled-back history it was recorded during.
struct RetiredSessionEvent {
    id: String,
    expected: String,
    replacement: String,
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
        for event in &self.retired_events {
            let written = transaction.execute(
                "UPDATE session_event_record SET payload=?3 WHERE id=?1 AND session_id=?2 AND payload=?4",
                params![event.id, self.session.id, event.replacement, event.expected],
            )?;
            ensure!(written == 1, "rollback session event changed before commit");
        }
        transaction.execute(
            "UPDATE session_record SET payload=json_set(?2,'$.session.name',json_extract(payload,'$.session.name'),'$.session.active_task',json_extract(payload,'$.session.active_task')),updated_at_ms=?3 WHERE id=?1",
            params![self.session.id, super::encode_session(&self.session)?, self.session.updated_at_ms],
        )?;
        Ok(())
    }
}

/// Name the unresolved obligation that keeps a delegated exchange tree unsettled.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum DescendantObligation {
    /// No stored exchange matches the delegation reference.
    Missing,
    /// The delegation names a different child agent than the stored exchange's owner.
    OwnerMismatch,
    /// The exchange has not reached a terminal outcome.
    Unfinished,
    /// The exchange is terminal but still records a running native turn.
    RunningTurn,
}

impl std::fmt::Display for DescendantObligation {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str(match self {
            Self::Missing => "its stored state is missing",
            Self::OwnerMismatch => "its delegation names a different child owner",
            Self::Unfinished => "it has not reached a terminal outcome",
            Self::RunningTurn => "it still records a running native turn",
        })
    }
}

/// Identify the first exchange in a delegation tree that has not settled.
///
/// The caller adds its operation, such as checkpoint save or rollback, as context; the fields
/// identify the work without carrying prompts or provider output.
#[derive(Clone, Debug, Eq, PartialEq, thiserror::Error)]
#[error(
    "exchange {exchange_id} owned by {agent_id} (delegated by {}) is unsettled: {obligation}; state {}",
    parent_exchange_id.as_deref().unwrap_or("the requested scope"),
    state.map_or_else(|| "unknown".to_owned(), |state| format!("{state:?}"))
)]
pub(crate) struct DescendantUnsettled {
    pub parent_exchange_id: Option<String>,
    pub agent_id: String,
    pub exchange_id: String,
    pub state: Option<crate::exchange::ExchangeState>,
    pub obligation: DescendantObligation,
}

impl SqliteStore {
    /// Require every listed exchange and each exchange it delegated to be terminal.
    ///
    /// Checkpoint capture, finalization, and rollback all rely on this one predicate, so the
    /// failure is typed and names the unsettled exchange rather than the calling operation.
    pub(crate) fn require_settled_exchange_tree(
        &self,
        session_id: &str,
        roots: &[String],
    ) -> Result<()> {
        self.require_settled_from(
            session_id,
            roots.iter().map(|id| (id.clone(), None, None)).collect(),
        )
    }

    /// Require every exchange that `parent` delegated, transitively, to be terminal.
    ///
    /// The parent itself may still be finalizing; this is the precondition for committing it.
    pub(crate) fn require_settled_descendants(&self, parent: &Exchange) -> Result<()> {
        self.require_settled_from(
            &parent.session_id,
            parent
                .node_list
                .iter()
                .filter_map(|node| match node {
                    ExchangeNode::AgentReference { agent } => Some((
                        agent.child_exchange_id.clone(),
                        Some(parent.id.clone()),
                        Some(agent.child_agent_id.clone()),
                    )),
                    _ => None,
                })
                .collect(),
        )
    }

    /// Walk a delegation tree from exchange identities with their known referencing parent.
    fn require_settled_from(
        &self,
        session_id: &str,
        mut pending: Vec<(String, Option<String>, Option<String>)>,
    ) -> Result<()> {
        let records: Vec<Exchange> = self.list_payload(
            "SELECT payload FROM exchange_record WHERE session_id=?1",
            [session_id],
        )?;
        let by_id: HashMap<_, _> = records
            .iter()
            .map(|exchange| (exchange.id.as_str(), exchange))
            .collect();
        let mut visited = HashSet::new();
        while let Some((id, parent_exchange_id, delegated_agent_id)) = pending.pop() {
            if !visited.insert(id.clone()) {
                continue;
            }
            let unsettled = |exchange: Option<&Exchange>, obligation| DescendantUnsettled {
                parent_exchange_id: parent_exchange_id.clone(),
                agent_id: exchange.map_or_else(
                    || delegated_agent_id.clone().unwrap_or_default(),
                    |exchange| exchange.agent_id.clone(),
                ),
                exchange_id: id.clone(),
                state: exchange.map(Exchange::state),
                obligation,
            };
            let Some(exchange) = by_id.get(id.as_str()).copied() else {
                return Err(unsettled(None, DescendantObligation::Missing).into());
            };
            if delegated_agent_id
                .as_ref()
                .is_some_and(|agent_id| *agent_id != exchange.agent_id)
            {
                return Err(unsettled(Some(exchange), DescendantObligation::OwnerMismatch).into());
            }
            if !exchange.is_finished() {
                return Err(unsettled(Some(exchange), DescendantObligation::Unfinished).into());
            }
            if exchange
                .turn
                .iter()
                .any(|turn| turn.state() == crate::turn::TurnState::Running)
            {
                return Err(unsettled(Some(exchange), DescendantObligation::RunningTurn).into());
            }
            for node in &exchange.node_list {
                if let ExchangeNode::AgentReference { agent } = node {
                    pending.push((
                        agent.child_exchange_id.clone(),
                        Some(exchange.id.clone()),
                        Some(agent.child_agent_id.clone()),
                    ));
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
        let retired_events = match roots
            .first()
            .and_then(|root| updates.iter().find(|exchange| &exchange.id == root))
        {
            Some(first) => self.retired_session_events(session_id, first)?,
            None => Vec::new(),
        };
        Ok(PreparedRollback {
            session: session.clone(),
            expected_session,
            expected_exchanges,
            updates,
            retired_events,
        })
    }

    /// Re-own the session rows recorded since `first` began to `first`, so they retire with it.
    ///
    /// Task lifecycle, configuration, and compaction rows recorded during the rolled-back
    /// interactions describe history the rollback removes; owned by the first rolled-back
    /// exchange, the timeline hides them with it. A rename or a fork origin describes the session
    /// itself and stays.
    fn retired_session_events(
        &self,
        session_id: &str,
        first: &Exchange,
    ) -> Result<Vec<RetiredSessionEvent>> {
        use crate::timeline::{SessionEventKind, SessionEventRecord, TimelineOwner};
        let stored = self
            .connection
            .prepare("SELECT id,payload FROM session_event_record WHERE session_id=?1")?
            .query_map([session_id], |row| Ok((row.get(0)?, row.get(1)?)))?
            .collect::<rusqlite::Result<Vec<(String, String)>>>()?;
        let owner = TimelineOwner::exchange(first);
        let mut retired = Vec::new();
        for (id, expected) in stored {
            let mut event: SessionEventRecord = super::decode(&expected)?;
            if !matches!(event.owner, TimelineOwner::Session { .. })
                || event.created_at_ms < first.created_at_ms
                || matches!(
                    event.detail,
                    SessionEventKind::Renamed { .. } | SessionEventKind::Forked { .. }
                )
            {
                continue;
            }
            event.owner = owner.clone();
            retired.push(RetiredSessionEvent {
                id,
                expected,
                replacement: super::encode(&event)?,
            });
        }
        Ok(retired)
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

#[cfg(test)]
mod tests {
    use super::{DescendantObligation, DescendantUnsettled};
    use crate::agent::{Agent, AgentSource, AgentTask};
    use crate::session::SessionStore;
    use anyhow::{Context, Result};

    /// An unfinished delegated child names itself, its owner, and its parent in the typed error,
    /// and settles the tree once it reaches a terminal outcome.
    #[test]
    fn unsettled_descendant_is_identified_until_it_finishes() -> Result<()> {
        let temporary = tempfile::tempdir()?;
        let mut store = super::SqliteStore::open(temporary.path())?;
        store.save_session(&super::super::test::session("session", "workspace"))?;
        store.save_agent_run(&Agent::primary("session", 0))?;
        let agent = Agent::pending("session", "explorer", "explorer", 1);
        let mut parent =
            crate::exchange::Exchange::agent_admission("session", 1, "delegate".into(), 1);
        let task = AgentTask {
            id: "child-task".into(),
            session_id: "session".into(),
            agent_id: agent.id.clone(),
            operation_id: "operation".into(),
            ordinal: 1,
            source: AgentSource::Model,
            parent_exchange_id: parent.id.clone(),
            message: "inspect".into(),
            state: crate::exchange::ExchangeState::Queued,
            final_response: None,
            error: None,
            parent_reported: false,
            cancelled_by: None,
            created_at_ms: 1,
            last_activity_at_ms: None,
            completed_at_ms: None,
        };
        parent.append_agent_task(&task)?;
        store.admit_agent_task(&agent, &task, &parent, None)?;
        let error = store.require_settled_descendants(&parent).unwrap_err();
        let unsettled = error
            .downcast_ref::<DescendantUnsettled>()
            .context("the guard reports a typed descendant")?;
        assert_eq!(unsettled.exchange_id, task.id);
        assert_eq!(unsettled.agent_id, agent.id);
        assert_eq!(
            unsettled.parent_exchange_id.as_deref(),
            Some(parent.id.as_str())
        );
        assert_eq!(unsettled.obligation, DescendantObligation::Unfinished);
        assert!(!format!("{error:#}").contains("rollback"));
        let mut child = store.load_exchange(&task.id)?.context("child admitted")?;
        child.finish(crate::exchange::ExchangeOutcome::Cancelled, 2)?;
        let mut cancelled = task.clone();
        cancelled.state = crate::exchange::ExchangeState::Cancelled;
        cancelled.completed_at_ms = Some(2);
        store.complete_agent_task(&agent, &cancelled, &child)?;
        store.require_settled_descendants(&parent)?;
        Ok(())
    }
}
