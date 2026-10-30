use super::SqliteStore;
use crate::agent::AgentState;
use crate::exchange::{Exchange, ExchangeState};
use crate::turn::{TurnOutcome, TurnState};
use anyhow::Result;
use rusqlite::{OptionalExtension, TransactionBehavior, params};

impl SqliteStore {
    /// Settle execution that has no runtime owner before publishing a restored session.
    pub(crate) fn interrupt_detached_execution(&mut self, session_id: &str) -> Result<()> {
        let transaction = self
            .connection
            .transaction_with_behavior(TransactionBehavior::Immediate)?;
        let session_event = {
            let mut statement = transaction
                .prepare("SELECT payload FROM session_event_record WHERE session_id=?1")?;
            statement
                .query_map([session_id], |row| row.get::<_, String>(0))?
                .collect::<rusqlite::Result<Vec<_>>>()?
        };
        for payload in session_event {
            let mut event: crate::timeline::SessionEventRecord = serde_json::from_str(&payload)?;
            if let crate::timeline::SessionEventKind::Compaction { compaction } = &mut event.detail
                && compaction.state == crate::backend::CompactionState::Running
            {
                compaction.state = crate::backend::CompactionState::Unconfirmed;
                compaction.result.error =
                    Some("Runtime stopped before compaction completion was confirmed".into());
                transaction.execute(
                    "UPDATE session_event_record SET payload=?2 WHERE id=?1",
                    params![event.id, serde_json::to_string(&event)?],
                )?;
            }
        }
        let exchange = {
            let mut statement =
                transaction.prepare("SELECT payload FROM exchange_record WHERE session_id=?1")?;
            statement
                .query_map([session_id], |row| row.get::<_, String>(0))?
                .collect::<rusqlite::Result<Vec<_>>>()?
        };
        for payload in exchange {
            let mut exchange: Exchange = serde_json::from_str(&payload)?;
            let running_turn = exchange
                .turn
                .iter()
                .any(|turn| turn.state() == TurnState::Running);
            // Recover only observed execution time, excluding the offline interval.
            let observed_at = exchange.completed_at_ms().unwrap_or_else(|| {
                exchange.execution_started_at_ms().map_or_else(
                    || {
                        exchange.created_at_ms.saturating_add(
                            exchange
                                .duration_ms()
                                .max(exchange.metrics.observed_elapsed_ms)
                                .min(i64::MAX as u64) as i64,
                        )
                    },
                    |started| {
                        let active = exchange
                            .metrics
                            .observed_elapsed_ms
                            .saturating_sub(exchange.duration_ms());
                        started.saturating_add(active.min(i64::MAX as u64) as i64)
                    },
                )
            });
            let background_changed = exchange.interrupt_detached_background_tools(observed_at)?;
            if (exchange.awaiting_input()
                && !running_turn
                && exchange.execution_started_at_ms().is_none())
                || !matches!(
                    exchange.state(),
                    ExchangeState::Running | ExchangeState::Queued | ExchangeState::Finalizing
                )
                || (exchange.state() == ExchangeState::Finalizing && !running_turn)
            {
                if background_changed {
                    transaction.execute(
                        "UPDATE exchange_record SET payload=?2 WHERE id=?1",
                        params![exchange.id, serde_json::to_string(&exchange)?],
                    )?;
                }
                continue;
            }
            if exchange.state() == ExchangeState::Finalizing {
                let outcome = match exchange.finalization_outcome() {
                    Some(crate::exchange::ExchangeOutcome::Cancelled) => TurnOutcome::Cancelled,
                    Some(crate::exchange::ExchangeOutcome::Failed) => TurnOutcome::Failed,
                    _ => TurnOutcome::Interrupted,
                };
                for turn in &mut exchange.turn {
                    if turn.state() == TurnState::Running {
                        turn.finish(outcome, observed_at)?;
                    }
                }
                if exchange.finalization_outcome()
                    == Some(crate::exchange::ExchangeOutcome::Complete)
                {
                    exchange.phase = crate::exchange::ExchangePhase::Finalizing(
                        crate::exchange::ExchangeOutcome::Interrupted,
                    );
                }
            } else {
                exchange.finish(crate::exchange::ExchangeOutcome::Interrupted, observed_at)?;
            }
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
            let mut changed = false;
            if agent.state.is_open() && !agent.is_primary() {
                agent.state = AgentState::Closed;
                changed = true;
            }
            for message in &mut agent.inbox {
                if message.delivery != crate::agent::MessageDelivery::Attached {
                    continue;
                }
                let Some(task_id) = message.task_id.as_ref() else {
                    continue;
                };
                let unfinished: bool = transaction.query_row(
                    "SELECT json_extract(payload,'$.completed_at_ms') IS NULL FROM agent_task_record WHERE id=?1 AND session_id=?2",
                    params![task_id,session_id], |row| row.get(0)).optional()?.unwrap_or(false);
                if unfinished {
                    message.delivery = crate::agent::MessageDelivery::Queued;
                    message.task_id = None;
                    changed = true;
                }
            }
            if changed {
                transaction.execute(
                    "UPDATE agent_run_record SET payload=?2 WHERE id=?1",
                    params![agent.id, serde_json::to_string(&agent)?],
                )?;
            }
        }
        let child_task = {
            let mut statement =
                transaction.prepare("SELECT payload FROM agent_task_record WHERE session_id=?1")?;
            statement
                .query_map([session_id], |row| row.get::<_, String>(0))?
                .collect::<rusqlite::Result<Vec<_>>>()?
        };
        for payload in child_task {
            let mut task: crate::agent::AgentTask = serde_json::from_str(&payload)?;
            if task.completed_at_ms.is_some() {
                continue;
            }
            let observed_at: Option<i64> = transaction.query_row(
                "SELECT json_extract(payload,'$.completed_at_ms') FROM exchange_record WHERE id=?1",
                [&task.id],
                |row| row.get(0),
            )?;
            task.state = ExchangeState::Interrupted;
            task.completed_at_ms = Some(observed_at.unwrap_or(task.created_at_ms));
            task.error = Some("Runtime restarted. Inspect completed effects before assigning an explicit follow-up".into());
            task.parent_reported = true;
            transaction.execute(
                "UPDATE agent_task_record SET payload=?2 WHERE id=?1",
                params![task.id, serde_json::to_string(&task)?],
            )?;
        }
        let executions = {
            let mut statement = transaction
                .prepare("SELECT payload FROM plan_execution_record WHERE session_id=?1")?;
            statement
                .query_map([session_id], |row| row.get::<_, String>(0))?
                .collect::<rusqlite::Result<Vec<_>>>()?
        };
        for payload in executions {
            let mut execution: crate::plan::PlanExecutionRecord = serde_json::from_str(&payload)?;
            let mut changed = false;
            for run in &mut execution.check_runs {
                if run.state == crate::checks::CheckState::Running {
                    run.state = crate::checks::CheckState::Interrupted;
                    for command in &mut run.commands {
                        if command.state == crate::checks::CheckState::Running {
                            command.state = crate::checks::CheckState::Interrupted;
                            command.output_bytes = std::fs::metadata(&command.output)
                                .map_or(0, |metadata| metadata.len());
                        }
                    }
                    changed = true;
                }
            }
            if changed {
                transaction.execute("UPDATE plan_execution_record SET payload=json_set(payload,'$.check_runs',json(?2)) WHERE id=?1", params![execution.id,serde_json::to_string(&execution.check_runs)?])?;
            }
        }
        let clearing: bool = transaction.query_row(
            "SELECT COALESCE((SELECT json_extract(payload,'$.action.action')='clear' AND json_extract(payload,'$.state') IN ('admitted','accepted','stopping','running') FROM task_operation WHERE session_id=?1 ORDER BY rowid DESC LIMIT 1),0)",
            [session_id], |row| row.get(0))?;
        transaction.execute("UPDATE task_operation SET payload=json_set(payload,'$.state','cancelled','$.error',NULL) WHERE session_id=?1 AND json_extract(payload,'$.action.action')='pause' AND (json_extract(payload,'$.state') IN ('admitted','accepted','stopping','running') OR (json_extract(payload,'$.state')='outcome_unknown' AND json_extract(payload,'$.error')='Runtime stopped before the operation settled. Inspect task state before resuming'))", [session_id])?;
        transaction.execute("UPDATE task_operation SET payload=json_set(payload,'$.state','outcome_unknown','$.error','Runtime stopped before the operation settled. Inspect task state before resuming') WHERE session_id=?1 AND json_extract(payload,'$.state') IN ('admitted','accepted','stopping','running','applying','verifying','resuming')", [session_id])?;
        super::task::recover_active_task(&transaction, session_id, clearing)?;
        transaction.commit()?;
        self.refresh_admission_membership(session_id)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::backend::{TextUpdate, ToolActivityKind, ToolInvocation, ToolOutcome, ToolUpdate};
    use crate::session::SessionStore;
    use serde_json::json;

    #[derive(Clone, Copy, Debug)]
    enum ForegroundRecoveryCase {
        Finished(crate::exchange::ExchangeOutcome),
        Finalizing,
        Waiting,
    }

    fn detached_exchange(state: ForegroundRecoveryCase, agent_id: &str) -> Result<Exchange> {
        let seed: Exchange = serde_json::from_value(json!({
            "id":"detached", "session_id":"session", "agent_id":agent_id,
            "ordinal":1, "prompt":"Background work", "state":"running", "created_at_ms":10,
            "attributed_matches_checkpoint":false, "node_list":[],
        }))?;
        let address = crate::turn::test_support::address(&seed, "native-turn");
        let mut turn = crate::turn::test_support::TestTurn::new("turn".into(), address, 10);
        for id in ["pending", "settled", "foreground"] {
            turn.record_tool(
                id,
                ToolUpdate::Requested {
                    invocation: ToolInvocation {
                        kind: ToolActivityKind::Command,
                        name: "exec".into(),
                        title: id.into(),
                        arguments: json!({"owner":agent_id}),
                    },
                },
                11,
            )?;
            turn.record_tool(id, ToolUpdate::Started, 11)?;
            turn.record_tool(
                id,
                ToolUpdate::Output {
                    update: TextUpdate::Append(format!("{id}-output\n")),
                },
                12,
            )?;
            if id != "foreground" {
                turn.record_tool(id, ToolUpdate::Backgrounded, 12)?;
            }
            if id == "settled" {
                turn.record_tool(
                    id,
                    ToolUpdate::Finished {
                        outcome: ToolOutcome::Succeeded,
                    },
                    20,
                )?;
            }
        }
        turn.finish(TurnOutcome::Completed, 30)?;
        let mut exchange = turn.into_exchange();
        exchange.finish(crate::exchange::ExchangeOutcome::Complete, 30)?;
        let mut stored = serde_json::to_value(exchange)?;
        stored["duration_ms"] = json!(20);
        stored["metrics"]["observed_elapsed_ms"] = json!(20);
        match state {
            ForegroundRecoveryCase::Waiting => {
                stored["state"] = json!(ExchangeState::Running);
                stored["finalization_outcome"] = serde_json::Value::Null;
                stored["completed_at_ms"] = serde_json::Value::Null;
                stored["awaiting_input"] = json!(true);
            }
            ForegroundRecoveryCase::Finalizing => {
                stored["state"] = json!(ExchangeState::Finalizing);
                stored["completed_at_ms"] = serde_json::Value::Null;
            }
            ForegroundRecoveryCase::Finished(outcome) => {
                stored["state"] = json!(outcome.state());
                stored["finalization_outcome"] = json!(outcome);
            }
        }
        Ok(serde_json::from_value(stored)?)
    }

    /// Recover detached background calls on every preserved foreground state without charging offline time.
    #[test]
    fn detached_background_recovery_preserves_foreground_output_and_timing() -> Result<()> {
        for state in [
            ForegroundRecoveryCase::Finished(crate::exchange::ExchangeOutcome::Complete),
            ForegroundRecoveryCase::Finished(crate::exchange::ExchangeOutcome::Failed),
            ForegroundRecoveryCase::Finished(crate::exchange::ExchangeOutcome::Cancelled),
            ForegroundRecoveryCase::Finalizing,
            ForegroundRecoveryCase::Waiting,
        ] {
            for child in [false, true] {
                let root = tempfile::tempdir()?;
                let mut store = SqliteStore::open(root.path())?;
                store.save_session(&super::super::test::session("session", "workspace"))?;
                let agent_id = if child {
                    let agent = crate::agent::Agent::pending("session", "worker", "child", 1);
                    store.save_agent_run(&agent)?;
                    agent.id
                } else {
                    crate::session::HarnessSession::primary_agent_id("session")
                };
                let original = detached_exchange(state, &agent_id)?;
                store.save_exchange(&original)?;
                let original = store.load_exchange(&original.id)?.unwrap();
                let original_turn = serde_json::to_value(&original.turn)?;
                let original_elapsed = original.elapsed(90_000);
                store.interrupt_detached_execution("session")?;
                let recovered = store.load_exchange(&original.id)?.unwrap();
                assert_eq!(recovered.state(), original.state(), "{state:?}");
                assert_eq!(
                    recovered.finalization_outcome(),
                    original.finalization_outcome()
                );
                assert_eq!(recovered.awaiting_input(), original.awaiting_input());
                assert_eq!(recovered.agent_id, agent_id);
                assert_eq!(serde_json::to_value(&recovered.turn)?, original_turn);
                assert_eq!(recovered.completed_at_ms(), original.completed_at_ms());
                assert_eq!(recovered.duration_ms(), original.duration_ms());
                assert_eq!(recovered.elapsed(90_000), original_elapsed);
                let pending = recovered
                    .tools()
                    .find(|(_, tool)| tool.id == "pending")
                    .unwrap()
                    .1;
                assert_eq!(pending.state(), crate::turn::ToolState::Interrupted);
                assert_eq!(pending.completed_at_ms, Some(30));
                assert_eq!(pending.elapsed_ms(90_000), Some(19));
                assert_eq!(pending.output, "pending-output\n");
                let settled = recovered
                    .tools()
                    .find(|(_, tool)| tool.id == "settled")
                    .unwrap()
                    .1;
                assert_eq!(settled.state(), crate::turn::ToolState::Completed);
                assert_eq!(settled.completed_at_ms, Some(20));
                let foreground = recovered
                    .tools()
                    .find(|(_, tool)| tool.id == "foreground")
                    .unwrap()
                    .1;
                assert_eq!(foreground.state(), crate::turn::ToolState::Interrupted);
                assert_eq!(foreground.completed_at_ms, Some(30));
                let first = serde_json::to_value(&recovered)?;
                store.interrupt_detached_execution("session")?;
                assert_eq!(
                    serde_json::to_value(store.load_exchange(&original.id)?.unwrap())?,
                    first
                );
            }
        }
        Ok(())
    }

    /// Waiting input without a detached background call remains byte-for-byte unchanged.
    #[test]
    fn detached_recovery_preserves_waiting_exchange_without_background_work() -> Result<()> {
        let root = tempfile::tempdir()?;
        let mut store = SqliteStore::open(root.path())?;
        store.save_session(&super::super::test::session("session", "workspace"))?;
        let waiting: Exchange = serde_json::from_value(json!({
            "id":"waiting", "session_id":"session", "agent_id":crate::session::HarnessSession::primary_agent_id("session"),
            "ordinal":1, "prompt":"Await answer", "state":"running", "created_at_ms":10,
            "awaiting_input":true, "attributed_matches_checkpoint":false, "node_list":[],
        }))?;
        store.save_exchange(&waiting)?;
        let before = serde_json::to_value(store.load_exchange("waiting")?.unwrap())?;
        store.interrupt_detached_execution("session")?;
        assert_eq!(
            serde_json::to_value(store.load_exchange("waiting")?.unwrap())?,
            before
        );
        Ok(())
    }
}
