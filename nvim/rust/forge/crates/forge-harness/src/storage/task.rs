use anyhow::{Context, Result, ensure};
use rusqlite::{OptionalExtension, TransactionBehavior, params};

use crate::task::{ActiveTask, TaskRevision, TaskWorkflow};

use super::SqliteStore;

/// Records that must become visible in the same commit as the session task mutation.
#[derive(Default)]
pub(crate) struct TaskEffects {
    pub plan_guard: Vec<PlanArtifactGuard>,
    pub retire_questions: bool,
    pub plan: Vec<crate::plan::PlanRecord>,
    pub execution: Vec<crate::plan::PlanExecutionRecord>,
    pub lifecycle: Vec<crate::plan::PlanLifecycleRecord>,
    pub event: Vec<crate::timeline::SessionEventRecord>,
    pub exchange: Vec<crate::exchange::Exchange>,
}

pub(crate) struct PlanArtifactGuard {
    pub id: String,
    pub document_version: u64,
    pub review_digest: Option<String>,
}

impl SqliteStore {
    /// Reads the finalized source boundary before admitting its persisted continuation.
    pub(crate) fn continuation_source_complete(
        &self,
        session_id: &str,
        turn: &crate::task::TaskTurnId,
    ) -> Result<bool> {
        completed_task_turn(&self.connection, session_id, turn)
            .context("read task continuation source boundary")
    }

    pub(super) fn active_plan_view(
        &self,
        plan_id: &str,
    ) -> Result<Option<crate::plan::PlanRecord>> {
        self.load_payload("SELECT json_extract(s.payload,'$.session.active_task.plan.artifact') FROM plan_record p JOIN session_record s ON s.id=p.session_id WHERE p.id=?1 AND json_extract(s.payload,'$.session.active_task.plan.artifact.id')=p.id", [plan_id])
    }

    pub(super) fn active_execution_view(
        &self,
        execution_id: &str,
    ) -> Result<Option<crate::plan::PlanExecutionRecord>> {
        self.load_payload("SELECT json_extract(s.payload,'$.session.active_task.execution.record') FROM plan_execution_record p JOIN session_record s ON s.id=p.session_id WHERE p.id=?1 AND json_extract(s.payload,'$.session.active_task.execution.record.id')=p.id", [execution_id])
    }

    pub(crate) fn load_active_task(&self, session_id: &str) -> Result<Option<ActiveTask>> {
        let payload: Option<String> = self
            .connection
            .query_row(
                "SELECT json_extract(payload,'$.session.active_task') FROM session_record WHERE id=?1 AND json_extract(payload,'$.format_version')=?2",
                params![session_id, super::SESSION_FORMAT_VERSION],
                |row| row.get(0),
            )
            .optional()?
            .context("task session is missing or incompatible")?;
        payload.map(|payload| super::decode(&payload)).transpose()
    }

    /// Commits a task change only while the admitting revision remains current.
    pub(crate) fn commit_task_change<Output>(
        &mut self,
        session_id: &str,
        expected: Option<&TaskRevision>,
        now_ms: i64,
        change: impl FnOnce(&mut Option<ActiveTask>) -> Result<Output>,
    ) -> Result<Output> {
        self.commit_task_effects(session_id, expected, now_ms, None, |task| {
            Ok((change(task)?, TaskEffects::default()))
        })?
        .context("task change unexpectedly reused a command")
    }

    pub(crate) fn commit_task_effects<Output>(
        &mut self,
        session_id: &str,
        expected: Option<&TaskRevision>,
        now_ms: i64,
        command: Option<&crate::task::TaskOperation>,
        change: impl FnOnce(&mut Option<ActiveTask>) -> Result<(Output, TaskEffects)>,
    ) -> Result<Option<Output>> {
        let transaction = self
            .connection
            .transaction_with_behavior(TransactionBehavior::Immediate)?;
        if let Some(command) = command {
            let existing: Option<String> = transaction
                .query_row(
                    "SELECT payload FROM task_operation WHERE id=?1 AND session_id=?2",
                    params![command.id, session_id],
                    |row| row.get(0),
                )
                .optional()?;
            if let Some(existing) = existing {
                let existing: crate::task::TaskOperation = super::decode(&existing)?;
                ensure!(
                    existing.action == command.action,
                    "operation ID was reused for a different action"
                );
                ensure!(
                    existing.state == "completed",
                    "task command has an unfinished legacy operation"
                );
                return Ok(None);
            }
        }
        let payload: Option<String> = transaction
            .query_row(
                "SELECT json_extract(payload,'$.session.active_task') FROM session_record WHERE id=?1 AND json_extract(payload,'$.format_version')=?2",
                params![session_id, super::SESSION_FORMAT_VERSION],
                |row| row.get(0),
            )
            .optional()?
            .context("task session is missing or incompatible")?;
        let mut active: Option<ActiveTask> =
            payload.map(|payload| super::decode(&payload)).transpose()?;
        if active.as_ref().map(|task| &task.revision) != expected {
            return Err(crate::task::StaleTask.into());
        }
        let previous = active.clone();
        let (output, mut effects) = change(&mut active)?;
        for guard in &effects.plan_guard {
            let payload: String = transaction.query_row(
                "SELECT payload FROM plan_record WHERE id=?1 AND session_id=?2",
                params![guard.id, session_id],
                |row| row.get(0),
            )?;
            let artifact: crate::plan::PlanRecord = super::decode(&payload)?;
            ensure!(
                artifact.document_version == guard.document_version
                    && artifact.review_digest == guard.review_digest,
                "reviewed plan changed before task commit"
            );
        }
        let entry_changed =
            previous
                .as_ref()
                .zip(active.as_ref())
                .is_some_and(|(previous, current)| {
                    previous.entry.as_ref().map(|entry| &entry.id)
                        != current.entry.as_ref().map(|entry| &entry.id)
                        || previous.kind() != current.kind()
                });
        let retire_questions =
            effects.retire_questions || entry_changed || previous.is_some() && active.is_none();
        if retire_questions {
            super::question::retire_task_questions(
                &transaction,
                session_id,
                previous.as_ref(),
                now_ms,
            )?;
        }
        if let Some(ActiveTask {
            workflow: TaskWorkflow::Plan { plan },
            ..
        }) = &mut active
        {
            let mut payload = serde_json::to_value(&plan.artifact)?;
            payload["schema_version"] = serde_json::json!(crate::plan::PLAN_SCHEMA_VERSION);
            let preserved = super::question::preserve_answers(
                &transaction,
                "plan_record",
                &plan.artifact.id,
                super::encode(&payload)?,
            )?;
            plan.artifact = super::decode(&preserved)?;
            effects.plan.push(plan.artifact.clone());
        }
        for artifact in &effects.plan {
            ensure!(
                artifact.session_id == session_id,
                "plan belongs to another session"
            );
            let mut payload = serde_json::to_value(artifact)?;
            payload["schema_version"] = serde_json::json!(crate::plan::PLAN_SCHEMA_VERSION);
            let payload = super::question::preserve_answers(
                &transaction,
                "plan_record",
                &artifact.id,
                super::encode(&payload)?,
            )?;
            let written = transaction.execute(
                "INSERT INTO plan_record(id,session_id,payload) VALUES(?1,?2,?3) ON CONFLICT(id) DO UPDATE SET payload=excluded.payload WHERE plan_record.session_id=excluded.session_id",
                params![artifact.id, session_id, payload],
            )?;
            ensure!(written == 1, "plan cannot change its owning session");
        }
        if let Some(ActiveTask {
            workflow: TaskWorkflow::Execute { execution },
            ..
        }) = &active
        {
            effects.execution.push(execution.record.clone());
        }
        for record in &effects.execution {
            ensure!(
                record.session_id == session_id,
                "execution belongs to another session"
            );
            let mut payload = serde_json::to_value(record)?;
            payload["schema_version"] = serde_json::json!(crate::plan::PLAN_SCHEMA_VERSION);
            let written = transaction.execute(
                "INSERT INTO plan_execution_record(id,session_id,payload) VALUES(?1,?2,?3) ON CONFLICT(id) DO UPDATE SET payload=excluded.payload WHERE plan_execution_record.session_id=excluded.session_id",
                params![record.id, session_id, super::encode(&payload)?],
            )?;
            ensure!(written == 1, "execution cannot change its owning session");
        }
        for lifecycle in &effects.lifecycle {
            ensure!(
                lifecycle.session_id == session_id,
                "lifecycle belongs to another session"
            );
            let mut payload = serde_json::to_value(lifecycle)?;
            payload["schema_version"] = serde_json::json!(crate::plan::PLAN_SCHEMA_VERSION);
            let written = transaction.execute(
                "INSERT INTO plan_lifecycle_record(id,session_id,payload) VALUES(?1,?2,?3) ON CONFLICT(id) DO UPDATE SET payload=excluded.payload WHERE plan_lifecycle_record.session_id=excluded.session_id",
                params![lifecycle.id, session_id, super::encode(&payload)?],
            )?;
            ensure!(written == 1, "lifecycle cannot change its owning session");
        }
        for exchange in &effects.exchange {
            ensure!(
                exchange.session_id == session_id,
                "exchange belongs to another session"
            );
            let payload = super::question::preserve_answers(
                &transaction,
                "exchange_record",
                &exchange.id,
                super::tool_output::encode(exchange)?,
            )?;
            let written = transaction.execute(
                "UPDATE exchange_record SET payload=?3 WHERE id=?1 AND session_id=?2",
                params![exchange.id, session_id, payload],
            )?;
            ensure!(written == 1, "task effect exchange is missing");
            super::tool_output::synchronize(&transaction, exchange)?;
        }
        for event in &effects.event {
            ensure!(
                event.session_id == session_id && event.owner.session_id() == session_id,
                "task event belongs to another session"
            );
            if let Some(exchange_id) = event.owner.exchange_id() {
                let payload: String = transaction.query_row(
                    "SELECT payload FROM exchange_record WHERE id=?1 AND session_id=?2",
                    params![exchange_id, session_id],
                    |row| row.get(0),
                )?;
                let exchange: crate::exchange::Exchange = super::decode(&payload)?;
                event.owner.validate_exchange(&exchange)?;
            }
            let previous: Option<String> = transaction
                .query_row(
                    "SELECT payload FROM session_event_record WHERE id=?1",
                    [&event.id],
                    |row| row.get(0),
                )
                .optional()?;
            if let Some(previous) = previous {
                let previous: crate::timeline::SessionEventRecord = super::decode(&previous)?;
                ensure!(
                    previous.owner == event.owner && previous.session_id == event.session_id,
                    "task event ownership cannot change"
                );
            }
            let written = transaction.execute(
                "INSERT INTO session_event_record(id,session_id,payload) VALUES(?1,?2,?3) ON CONFLICT(id) DO UPDATE SET payload=excluded.payload WHERE session_event_record.session_id=excluded.session_id",
                params![event.id, session_id, super::encode(event)?],
            )?;
            ensure!(written == 1, "task event cannot change its owning session");
        }
        let encoded = super::encode(&active)?;
        transaction.execute(
            "UPDATE session_record SET updated_at_ms=max(updated_at_ms,?2), payload=json_set(payload,'$.session.active_task',json(?3),'$.session.updated_at_ms',max(updated_at_ms,?2)) WHERE id=?1",
            params![session_id, now_ms, encoded],
        )?;
        if let Some(command) = command {
            let mut receipt = command.clone();
            receipt.state = "completed".into();
            receipt.revision += 1;
            transaction.execute(
                "INSERT INTO task_operation(id,session_id,payload) VALUES(?1,?2,?3)",
                params![receipt.id, session_id, super::encode(&receipt)?],
            )?;
        }
        transaction.commit()?;
        if retire_questions {
            self.refresh_admission_membership(session_id)?;
        } else {
            self.publish_admission(session_id)?;
        }
        Ok(Some(output))
    }
}

pub(super) fn recover_active_task(
    connection: &rusqlite::Connection,
    session_id: &str,
    clearing: bool,
) -> Result<()> {
    let payload: Option<String> = connection.query_row(
        "SELECT json_extract(payload,'$.session.active_task') FROM session_record WHERE id=?1",
        [session_id],
        |row| row.get(0),
    )?;
    let Some(mut active): Option<ActiveTask> =
        payload.map(|payload| super::decode(&payload)).transpose()?
    else {
        return Ok(());
    };
    if clearing {
        connection.execute("UPDATE session_record SET payload=json_set(payload,'$.session.active_task',NULL) WHERE id=?1", [session_id])?;
        return Ok(());
    }
    if active
        .entry
        .as_ref()
        .is_some_and(|entry| entry.delivery == crate::task::TaskInputDelivery::Pending)
    {
        return Ok(());
    }
    if let Some(turn) = active.turn.as_ref()
        && turn
            .continuation
            .as_ref()
            .is_some_and(|input| input.delivery == crate::task::TaskInputDelivery::Pending)
        && completed_task_turn(connection, session_id, &turn.id)?
    {
        return Ok(());
    }
    match &mut active.workflow {
        TaskWorkflow::Goal { goal } => {
            if goal.state == crate::goal::GoalState::Active {
                goal.state = crate::goal::GoalState::Paused;
            }
        }
        TaskWorkflow::Plan { plan } => {
            if matches!(
                plan.artifact.state,
                crate::plan::PlanState::Generating
                    | crate::plan::PlanState::Revising
                    | crate::plan::PlanState::ConfiguringChecks
            ) {
                plan.paused = true;
            }
        }
        TaskWorkflow::Execute { execution } => {
            if execution.record.state == crate::plan::PlanExecutionState::Active {
                execution.record.state = crate::plan::PlanExecutionState::Paused;
                execution.record.generation += 1;
            }
            for run in &mut execution.record.check_runs {
                if run.state == crate::checks::CheckState::Running {
                    run.state = crate::checks::CheckState::Interrupted;
                    for command in &mut run.commands {
                        if command.state == crate::checks::CheckState::Running {
                            command.state = crate::checks::CheckState::Interrupted;
                            command.output_bytes = std::fs::metadata(&command.output)
                                .map_or(0, |metadata| metadata.len());
                        }
                    }
                }
            }
        }
    }
    active.renew(active.updated_at_ms);
    if let TaskWorkflow::Execute { execution } = &active.workflow {
        let mut payload = serde_json::to_value(&execution.record)?;
        payload["schema_version"] = serde_json::json!(crate::plan::PLAN_SCHEMA_VERSION);
        connection.execute(
            "UPDATE plan_execution_record SET payload=?2 WHERE id=?1 AND session_id=?3",
            params![execution.record.id, super::encode(&payload)?, session_id],
        )?;
    }
    connection.execute("UPDATE session_record SET payload=json_set(payload,'$.session.active_task',json(?2)) WHERE id=?1", params![session_id,super::encode(&active)?])?;
    Ok(())
}

fn completed_task_turn(
    connection: &rusqlite::Connection,
    session_id: &str,
    turn: &crate::task::TaskTurnId,
) -> Result<bool> {
    let latest: Option<(i64, String, i64)> = connection.query_row(
        "SELECT ordinal,json_extract(payload,'$.state'),COALESCE(json_array_length(json_extract(payload,'$.turn')),0) FROM exchange_record WHERE session_id=?1 AND agent_id=?2 ORDER BY ordinal DESC LIMIT 1",
        params![session_id, crate::session::HarnessSession::primary_agent_id(session_id)],
        |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?)),
    ).optional()?;
    let Some((ordinal, state, turn_count)) = latest else {
        return Ok(false);
    };
    Ok(u64::try_from(ordinal)? == turn.exchange_ordinal
        && state == "complete"
        && usize::try_from(turn_count)? == turn.turn_ordinal)
}

pub(super) fn read_record<Record: serde::de::DeserializeOwned>(
    connection: &rusqlite::Connection,
    table: &str,
    id: &str,
) -> Result<Option<Record>> {
    let payload = connection
        .query_row(
            &format!("SELECT payload FROM {table} WHERE id=?1"),
            [id],
            |row| row.get::<_, String>(0),
        )
        .optional()?;
    payload
        .map(|payload| {
            let value: serde_json::Value = super::decode(&payload)?;
            ensure!(
                value["id"].as_str() == Some(id),
                "selected task record identity is invalid"
            );
            if matches!(table, "plan_record" | "plan_execution_record") {
                ensure!(
                    value["schema_version"].as_u64()
                        == Some(crate::plan::PLAN_SCHEMA_VERSION.into()),
                    "selected task artifact is incompatible"
                );
            }
            Ok(serde_json::from_value(value)?)
        })
        .transpose()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::session::{SessionStore, continuation::ContinuationBudget};

    #[test]
    fn finished_task_clears_slot_and_archives_outcome_atomically() -> Result<()> {
        let (_root, mut store, controller) = crate::task::SessionTaskController::test_goal();
        let active = controller.current()?.unwrap();
        store.connection.execute_batch("CREATE TRIGGER reject_outcome BEFORE INSERT ON session_event_record BEGIN SELECT RAISE(ABORT,'outcome unavailable'); END;")?;
        assert!(
            controller
                .finish(
                    &mut store,
                    &active.permit(),
                    crate::task::TaskOutcomeState::Complete,
                    "Finished".into(),
                    2
                )
                .is_err()
        );
        assert_eq!(controller.current()?.unwrap().revision, active.revision);
        store
            .connection
            .execute_batch("DROP TRIGGER reject_outcome")?;
        controller.finish(
            &mut store,
            &active.permit(),
            crate::task::TaskOutcomeState::Complete,
            "Finished".into(),
            3,
        )?;
        assert!(controller.current()?.is_none());
        let event = store.list_session_event("session")?;
        assert!(event.iter().any(|event| matches!(&event.detail, crate::timeline::SessionEventKind::TaskFinished { outcome } if outcome.summary == "Finished")));
        Ok(())
    }

    #[test]
    fn task_recovery_does_not_duplicate_entry_or_continuation() -> Result<()> {
        for entry in [true, false] {
            let (_root, mut store, controller) = crate::task::SessionTaskController::test_goal();
            let original = controller.current()?.unwrap();
            if entry {
                let mut task = original.clone();
                task.request_entry("Exact pending instruction".into(), 2)?;
                controller.change(
                    &mut store,
                    Some(&original.revision),
                    crate::task::TaskChange::Install(Box::new(task)),
                    2,
                )?;
            } else {
                let exchange: crate::exchange::Exchange = serde_json::from_value(
                    serde_json::json!({
                        "id":"source","session_id":"session","agent_id":crate::session::HarnessSession::primary_agent_id("session"),
                        "ordinal":1,"prompt":"Source","state":"complete","created_at_ms":1,"node_list":[],"attributed_matches_checkpoint":false
                    }),
                )?;
                store.save_exchange(&exchange)?;
                controller.observe_goal(
                    &mut store,
                    &original.permit(),
                    crate::task::TaskTurnId {
                        exchange_ordinal: 1,
                        turn_ordinal: 0,
                    },
                    crate::goal::TurnEvidence {
                        tool_called: true,
                        ..Default::default()
                    },
                    2,
                )?;
            }
            let before = controller.current()?.unwrap();
            store.interrupt_detached_execution("session")?;
            let after = controller.current()?.unwrap();
            assert_eq!(after.revision, before.revision);
            assert_eq!(after.entry, before.entry);
            assert_eq!(after.turn, before.turn);
        }
        Ok(())
    }

    #[test]
    fn plan_install_rolls_back_its_artifact_and_entry_together() -> Result<()> {
        let (root, mut store, controller) = crate::task::SessionTaskController::test_goal();
        let workspace = tempfile::tempdir()?;
        crate::checks::configure_fixture(workspace.path());
        let files = crate::plan::PlanFileStore::new(root.path(), workspace.path());
        let prepared = files.prepare_entry("session", "Plan the change".into(), None, 2)?;
        let plan_id = prepared.record.id.clone();
        let mut plan = ActiveTask::plan(prepared.record, 2);
        plan.request_entry(prepared.instruction, 2)?;
        let previous = controller.current()?.unwrap();
        store.connection.execute_batch("CREATE TRIGGER fail_plan BEFORE INSERT ON plan_record BEGIN SELECT RAISE(ABORT,'plan unavailable'); END;")?;
        assert!(
            controller
                .change(
                    &mut store,
                    Some(&previous.revision),
                    crate::task::TaskChange::Install(Box::new(plan.clone())),
                    2
                )
                .is_err()
        );
        assert_eq!(controller.current()?.unwrap().revision, previous.revision);
        assert!(store.load_plan(&plan_id)?.is_none());
        store.connection.execute_batch("DROP TRIGGER fail_plan")?;
        let installed = controller
            .change(
                &mut store,
                Some(&previous.revision),
                crate::task::TaskChange::Install(Box::new(plan)),
                3,
            )?
            .unwrap();
        assert!(!installed.entry_accepted());
        assert_eq!(
            store.load_plan(&plan_id)?.unwrap().request,
            "Plan the change"
        );
        Ok(())
    }

    #[test]
    fn task_commit_preserves_unrelated_session_updates_and_rejects_stale_writers() -> Result<()> {
        let root = tempfile::tempdir()?;
        let mut owner = SqliteStore::open(root.path())?;
        let mut stale = super::super::test::session("session", "workspace");
        owner.save_session(&stale)?;
        stale = owner.acquire_session_lease(&stale.id, "owner")?;
        let admission = owner.session_admission(&stale.id)?;
        let mut writer = SqliteStore::open(root.path())?;
        let task = ActiveTask::goal("Finish".into(), ContinuationBudget::default(), false, 1);
        owner.commit_task_change(&stale.id, None, 1, |active| {
            *active = Some(task.clone());
            Ok(())
        })?;
        assert_eq!(
            admission.runtime()?.active_task.as_ref().unwrap().revision,
            task.revision
        );
        assert_eq!(
            admission.snapshot()?.task_revision.as_ref(),
            Some(&task.revision)
        );

        stale.model = "updated-model".into();
        writer.save_session(&stale)?;
        let saved = owner.load_session(&stale.id)?.unwrap();
        assert_eq!(saved.model, "updated-model");
        assert_eq!(saved.active_task.as_ref().unwrap().revision, task.revision);

        stale.effort = "updated-effort".into();
        writer.save_owned_session(&stale, "owner")?;
        let saved = owner.load_session(&stale.id)?.unwrap();
        assert_eq!(saved.effort, "updated-effort");
        assert_eq!(saved.active_task.as_ref().unwrap().revision, task.revision);

        let replacement = ActiveTask::goal(
            "Replacement".into(),
            ContinuationBudget::default(),
            false,
            2,
        );
        writer.commit_task_change(&stale.id, Some(&task.revision), 2, |active| {
            *active = Some(replacement.clone());
            Ok(())
        })?;
        assert!(
            owner
                .commit_task_change(&stale.id, Some(&task.revision), 3, |active| {
                    *active = None;
                    Ok(())
                })
                .is_err()
        );
        assert_eq!(
            admission.runtime()?.active_task.as_ref().unwrap().revision,
            replacement.revision
        );
        assert_eq!(
            owner.load_session(&stale.id)?.unwrap().model,
            "updated-model"
        );
        Ok(())
    }

    #[test]
    fn failed_task_commit_preserves_the_published_revision() -> Result<()> {
        let root = tempfile::tempdir()?;
        let mut store = SqliteStore::open(root.path())?;
        let session = super::super::test::session("session", "workspace");
        store.save_session(&session)?;
        let admission = store.session_admission(&session.id)?;
        store.connection.execute_batch("CREATE TRIGGER reject_task BEFORE UPDATE ON session_record BEGIN SELECT RAISE(ABORT,'failed task commit'); END;")?;
        let task = ActiveTask::goal("Finish".into(), ContinuationBudget::default(), false, 1);
        assert!(
            store
                .commit_task_change(&session.id, None, 1, |active| {
                    *active = Some(task);
                    Ok(())
                })
                .is_err()
        );
        assert!(admission.runtime()?.active_task.is_none());
        assert!(store.load_active_task(&session.id)?.is_none());
        Ok(())
    }
}
