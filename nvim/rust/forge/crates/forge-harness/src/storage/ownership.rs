use super::SqliteStore;
use crate::exchange::{Exchange, ExchangeNode};
use anyhow::{Context, Result, ensure};
use std::collections::{HashMap, HashSet};

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
                exchange.completed_at_ms.is_some(),
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

    /// Commit rollback history for primary exchanges and their delegated descendants together.
    pub(crate) fn record_rollback(&mut self, session_id: &str, roots: &[String]) -> Result<()> {
        use crate::exchange::HistoryDisposition;
        use rusqlite::{TransactionBehavior, params};
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
        let transaction = self
            .connection
            .transaction_with_behavior(TransactionBehavior::Immediate)?;
        for exchange in updates {
            transaction.execute(
                "UPDATE exchange_record SET payload=?2 WHERE id=?1",
                params![exchange.id, serde_json::to_string(&exchange)?],
            )?;
        }
        transaction.commit()?;
        Ok(())
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
