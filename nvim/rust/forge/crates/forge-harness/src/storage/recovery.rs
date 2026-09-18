use super::SqliteStore;
use crate::agent::AgentState;
use crate::exchange::{Exchange, ExchangeState};
use crate::turn::TurnState;
use anyhow::Result;
use rusqlite::{TransactionBehavior, params};

impl SqliteStore {
    /// Settle execution that has no runtime owner before publishing a restored session.
    pub(crate) fn interrupt_detached_execution(&mut self, session_id: &str) -> Result<()> {
        let transaction = self
            .connection
            .transaction_with_behavior(TransactionBehavior::Immediate)?;
        let exchange = {
            let mut statement =
                transaction.prepare("SELECT payload FROM exchange_record WHERE session_id=?1")?;
            statement
                .query_map([session_id], |row| row.get::<_, String>(0))?
                .collect::<rusqlite::Result<Vec<_>>>()?
        };
        for payload in exchange {
            let mut exchange: Exchange = serde_json::from_str(&payload)?;
            if !matches!(
                exchange.state,
                ExchangeState::Running | ExchangeState::Queued
            ) {
                continue;
            }
            let running_turn = exchange
                .turn
                .iter()
                .any(|turn| turn.state() == TurnState::Running);
            if exchange.awaiting_input
                && !running_turn
                && exchange.execution_started_at_ms.is_none()
            {
                continue;
            }
            // The persisted clock start is a lower bound, never the restart time.
            let observed_at = exchange
                .execution_started_at_ms
                .unwrap_or(exchange.created_at_ms);
            exchange.finish(ExchangeState::Interrupted, observed_at)?;
            transaction.execute(
                "UPDATE exchange_record SET payload=?2 WHERE id=?1",
                params![exchange.id, serde_json::to_string(&exchange)?],
            )?;
        }
        let agent = {
            let mut statement =
                transaction.prepare("SELECT payload FROM agent_run_record WHERE session_id=?1")?;
            statement
                .query_map([session_id], |row| row.get::<_, String>(0))?
                .collect::<rusqlite::Result<Vec<_>>>()?
        };
        for payload in agent {
            let mut agent: crate::agent::Agent = serde_json::from_str(&payload)?;
            if agent.state.is_open() && !agent.is_primary() {
                agent.state = AgentState::Closed;
                transaction.execute(
                    "UPDATE agent_run_record SET payload=?2 WHERE id=?1",
                    params![agent.id, serde_json::to_string(&agent)?],
                )?;
            }
        }
        transaction.commit()?;
        Ok(())
    }
}
