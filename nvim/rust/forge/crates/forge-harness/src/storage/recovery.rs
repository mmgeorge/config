use super::SqliteStore;
use crate::agent::AgentState;
use crate::exchange::{Exchange, ExchangeState};
use crate::turn::TurnState;
use anyhow::Result;
use rusqlite::{OptionalExtension, TransactionBehavior, params};

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
            // Recover only observed execution time, excluding the offline interval.
            let observed_at = exchange.execution_started_at_ms.map_or_else(
                || {
                    exchange.created_at_ms.saturating_add(
                        exchange
                            .duration_ms
                            .max(exchange.metrics.observed_elapsed_ms)
                            .min(i64::MAX as u64) as i64,
                    )
                },
                |started| {
                    let active = exchange
                        .metrics
                        .observed_elapsed_ms
                        .saturating_sub(exchange.duration_ms);
                    started.saturating_add(active.min(i64::MAX as u64) as i64)
                },
            );
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
            let mut changed = false;
            if agent.state.is_open() && !agent.is_primary() {
                agent.state = AgentState::Closed;
                changed = true;
            }
            for message in &mut agent.inbox {
                if message.delivery != crate::agent::MessageDelivery::Attached { continue; }
                let Some(task_id) = message.task_id.as_ref() else { continue; };
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
            let mut statement = transaction.prepare("SELECT payload FROM agent_task_record WHERE session_id=?1")?;
            statement.query_map([session_id],|row|row.get::<_,String>(0))?.collect::<rusqlite::Result<Vec<_>>>()?
        };
        for payload in child_task {
            let mut task:crate::agent::AgentTask = serde_json::from_str(&payload)?;
            if task.completed_at_ms.is_some() {continue;}
            let observed_at:Option<i64> = transaction.query_row("SELECT json_extract(payload,'$.completed_at_ms') FROM exchange_record WHERE id=?1",
                [&task.id],|row|row.get(0))?;
            task.state = ExchangeState::Interrupted;
            task.completed_at_ms = Some(observed_at.unwrap_or(task.created_at_ms));
            task.error = Some("Runtime restarted. Inspect completed effects before assigning an explicit follow-up".into());
            task.parent_reported = true;
            transaction.execute("UPDATE agent_task_record SET payload=?2 WHERE id=?1",params![task.id,serde_json::to_string(&task)?])?;
        }
        let executions = {
            let mut statement=transaction.prepare("SELECT payload FROM plan_execution_record WHERE session_id=?1")?;
            statement.query_map([session_id], |row| row.get::<_,String>(0))?.collect::<rusqlite::Result<Vec<_>>>()?
        };
        for payload in executions {
            let mut execution:crate::plan::PlanExecutionRecord=serde_json::from_str(&payload)?;
            let mut changed=false;
            for run in &mut execution.check_runs {
                if run.state == crate::checks::CheckState::Running {
                    run.state=crate::checks::CheckState::Interrupted;
                    for command in &mut run.commands {
                        if command.state == crate::checks::CheckState::Running {
                            command.state=crate::checks::CheckState::Interrupted;
                            command.output_bytes=std::fs::metadata(&command.output).map_or(0,|metadata|metadata.len());
                        }
                    }
                    changed=true;
                }
            }
            if changed { transaction.execute("UPDATE plan_execution_record SET payload=json_set(payload,'$.check_runs',json(?2)) WHERE id=?1", params![execution.id,serde_json::to_string(&execution.check_runs)?])?; }
        }
        transaction.execute("UPDATE goal_record SET payload=json_set(payload,'$.state','paused') WHERE session_id=?1 AND json_extract(payload,'$.state')='active'", [session_id])?;
        transaction.execute("UPDATE plan_execution_record SET payload=json_set(payload,'$.state','paused','$.generation',json_extract(payload,'$.generation')+1) WHERE session_id=?1 AND json_extract(payload,'$.state')='active'", [session_id])?;
        let tasks = {
            let mut statement = transaction.prepare("SELECT payload FROM task_record WHERE session_id=?1")?;
            statement.query_map([session_id], |row| row.get::<_, String>(0))?.collect::<rusqlite::Result<Vec<_>>>()?
        };
        for payload in tasks {
            use crate::task::{TaskKind, TaskStatus};
            let mut task: crate::task::TaskRecord = serde_json::from_str(&payload)?;
            if task.status.terminal() { continue; }
            if let Some(goal_id) = &task.goal_id {
                let goal: Option<String> = transaction.query_row("SELECT payload FROM goal_record WHERE id=?1", [goal_id], |row| row.get(0)).optional()?;
                if let Some(goal) = goal {
                    let goal: crate::goal::GoalRecord = serde_json::from_str(&goal)?;
                    if goal.state == crate::goal::GoalState::Complete { task.status = TaskStatus::Completed; }
                    if goal.state == crate::goal::GoalState::Cleared { task.status = TaskStatus::Cancelled; }
                }
                let phase: Option<String> = transaction.query_row("SELECT json_extract(payload,'$.phase') FROM plan_execution_record WHERE session_id=?1 AND json_extract(payload,'$.goal_id')=?2 ORDER BY rowid DESC LIMIT 1", params![session_id,goal_id], |row| row.get(0)).optional()?;
                if let Some(phase) = phase { task.phase = phase; }
            }
            if matches!(task.status, TaskStatus::Running | TaskStatus::Stopping | TaskStatus::Ready) {
                let waiting = if task.kind == TaskKind::Plan {
                    transaction.query_row("SELECT json_extract(payload,'$.state') IN ('awaiting_review','awaiting_input') FROM plan_record WHERE id=?1", [&task.plan_id], |row| row.get::<_, bool>(0)).optional()?.unwrap_or(false)
                } else { false };
                task.status = if waiting { TaskStatus::Waiting } else { TaskStatus::Paused };
                task.reason = Some("Runtime restarted. Inspect completed effects before resuming".into());
                task.generation += 1;
            }
            transaction.execute("UPDATE task_record SET payload=?2 WHERE id=?1", params![task.id,serde_json::to_string(&task)?])?;
        }
        let clearing: bool = transaction.query_row(
            "SELECT COALESCE((SELECT json_extract(payload,'$.action.action')='clear' AND json_extract(payload,'$.state') IN ('admitted','accepted','stopping','running') FROM task_operation WHERE session_id=?1 ORDER BY rowid DESC LIMIT 1),0)",
            [session_id], |row| row.get(0))?;
        if clearing {
            transaction.execute("UPDATE task_record SET payload=json_set(payload,'$.status','cancelled','$.reason','Clear request recovered after runtime shutdown') WHERE id=(SELECT json_extract(payload,'$.session.current_task_id') FROM session_record WHERE id=?1) AND json_extract(payload,'$.status') NOT IN ('completed','cancelled')", [session_id])?;
            transaction.execute("UPDATE session_record SET payload=json_set(payload,'$.session.current_task_id',NULL,'$.session.active_plan_id',NULL,'$.session.goal_id',NULL) WHERE id=?1", [session_id])?;
        }
        transaction.execute("UPDATE task_operation SET payload=json_set(payload,'$.state','outcome_unknown','$.error','Runtime stopped before the operation settled. Inspect task state before resuming') WHERE session_id=?1 AND json_extract(payload,'$.state') IN ('admitted','accepted','stopping','running')", [session_id])?;
        transaction.commit()?;
        Ok(())
    }
}
