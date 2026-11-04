use anyhow::{Context, Result, ensure};
use rusqlite::{OptionalExtension, TransactionBehavior, params};

use crate::task::{Task, TaskRevision, WorkflowSnapshot};

use super::SqliteStore;

/// Records that must become visible in the same commit as the session task mutation.
#[derive(Default)]
pub(crate) struct TaskEffects {
    /// Revalidate captured workflow and question revisions before reserving continuation input.
    pub continuation_guard: Vec<crate::exchange::command::WorkflowContinuationRequest>,
    /// Replace provider/session identity and exact history atomically with task retirement.
    pub rollback: Option<Box<super::ownership::PreparedRollback>>,
    /// Commit task-command evidence in the same transaction as its domain mutation.
    pub owner_task_commit: Option<crate::exchange::journal::TaskCommandCommit>,
    pub plan_guard: Vec<PlanArtifactGuard>,
    /// Captured saved-plan authority revalidated in the task installation transaction.
    pub source_plan: Vec<crate::exchange::command::PlanSourceGuard>,
    pub retire_questions: bool,
    pub plan: Vec<crate::plan::PlanRecord>,
    pub execution: Vec<crate::plan::PlanExecutionRecord>,
    pub lifecycle: Vec<crate::plan::PlanLifecycleRecord>,
    pub event: Vec<crate::timeline::SessionEventRecord>,
    pub exchange: Vec<crate::exchange::Exchange>,
    pub checkpoint: Vec<crate::checkpoint::CheckpointRecord>,
}

pub(crate) struct PlanArtifactGuard {
    pub id: String,
    pub document_version: u64,
    pub review_digest: Option<String>,
}

/// Distinguish an exact mutation contract from attribution across a legal phase handoff.
enum TaskFence<'permit> {
    Revision(Option<&'permit TaskRevision>),
    Observation(&'permit crate::task::TaskPermit),
}

/// Revalidate the selected continuation source inside the same transaction as its reservation.
fn validate_continuation_guard(
    transaction: &rusqlite::Transaction<'_>,
    session: &str,
    request: &crate::exchange::command::WorkflowContinuationRequest,
    task: Option<&Task>,
) -> Result<()> {
    use crate::exchange::command::{
        QuestionWorkflow, TaskCommandFailure, WorkflowContinuationTarget,
    };
    let validate_exchange =
        |selection: &crate::exchange::command::ExchangeContinuationSelection,
         questions: Option<&crate::exchange::command::PlanQuestionSetSelection>|
         -> Result<()> {
            let latest: Option<String> = transaction.query_row("SELECT id FROM exchange_record WHERE session_id=?1 AND agent_id=?2 ORDER BY ordinal DESC LIMIT 1", params![session,crate::session::HarnessSession::primary_agent_id(session)], |row| row.get(0)).optional()?;
            ensure!(
                latest.as_deref() == Some(selection.id().as_str()),
                "continuation source is no longer current"
            );
            let payload: String = transaction.query_row(
                "SELECT payload FROM exchange_record WHERE id=?1 AND session_id=?2",
                params![selection.id().as_str(), session],
                |row| row.get(0),
            )?;
            let exchange: crate::exchange::Exchange = super::decode(&payload)?;
            selection.validate(&exchange, task)?;
            if let Some(questions) = questions {
                questions.validate(exchange.elicitation().ok_or(TaskCommandFailure::Changed)?)?;
            }
            Ok(())
        };
    match &request.target {
        WorkflowContinuationTarget::Exchange(selection) => validate_exchange(selection, None),
        WorkflowContinuationTarget::Plan(selection) => {
            selection.validate_target(task)?;
            Ok(())
        }
        WorkflowContinuationTarget::Questions(selection) => match &selection.workflow {
            QuestionWorkflow::Exchange(exchange) => {
                validate_exchange(exchange, Some(&selection.questions))
            }
            QuestionWorkflow::PlanReview(source) => {
                source.validate_target(task)?;
                let plan: crate::plan::PlanRecord =
                    read_record(transaction, "plan_record", source.source().id().as_str())?
                        .ok_or(TaskCommandFailure::PlanMissing)?;
                selection.questions.validate(
                    &plan
                        .review
                        .as_ref()
                        .ok_or(TaskCommandFailure::Changed)?
                        .elicitation,
                )?;
                Ok(())
            }
            QuestionWorkflow::Plan {
                source,
                task: selected,
            } => {
                selected.validate(task)?;
                let plan: crate::plan::PlanRecord =
                    read_record(transaction, "plan_record", source.id().as_str())?
                        .ok_or(TaskCommandFailure::PlanMissing)?;
                source.validate(&plan)?;
                selection.questions.validate(
                    plan.elicitation
                        .as_ref()
                        .ok_or(TaskCommandFailure::Changed)?,
                )?;
                Ok(())
            }
        },
    }
}

/// Validate ownership of live task artifacts without interpreting obsolete session formats.
pub(super) fn validate_task(
    connection: &rusqlite::Connection,
    session_id: &str,
    active: &crate::task::Task,
) -> Result<()> {
    match &active.workflow {
        crate::task::WorkflowSnapshot::Goal { .. } => {}
        crate::task::WorkflowSnapshot::Plan { plan } => {
            ensure!(
                plan.artifact.session_id == session_id,
                "active plan belongs to another session"
            );
            let artifact: crate::plan::PlanRecord =
                super::task::read_record(connection, "plan_record", &plan.artifact.id)?
                    .context("active plan artifact is missing")?;
            ensure!(
                artifact.session_id == session_id,
                "active plan artifact belongs to another session"
            );
        }
        crate::task::WorkflowSnapshot::Execute { execution } => {
            ensure!(
                execution.record.session_id == session_id,
                "active execution belongs to another session"
            );
            let artifact: crate::plan::PlanRecord =
                super::task::read_record(connection, "plan_record", &execution.record.plan_id)?
                    .context("active execution plan is missing")?;
            ensure!(
                artifact.session_id == session_id,
                "active execution plan belongs to another session"
            );
            let record: crate::plan::PlanExecutionRecord = super::task::read_record(
                connection,
                "plan_execution_record",
                &execution.record.id,
            )?
            .context("active execution artifact is missing")?;
            ensure!(
                record.session_id == session_id && record.plan_id == artifact.id,
                "active execution artifact has inconsistent ownership"
            );
        }
    }
    Ok(())
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

    pub(crate) fn load_active_task(&self, session_id: &str) -> Result<Option<Task>> {
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
        change: impl FnOnce(&mut Option<Task>) -> Result<Output>,
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
        change: impl FnOnce(&mut Option<Task>) -> Result<(Output, TaskEffects)>,
    ) -> Result<Option<Output>> {
        self.commit_task_effects_fenced(
            session_id,
            TaskFence::Revision(expected),
            now_ms,
            command,
            change,
        )
    }

    /// Count one admitted turn after its own phase transition without weakening mutation fencing.
    pub(crate) fn commit_task_observation<Output>(
        &mut self,
        session_id: &str,
        permit: &crate::task::TaskPermit,
        now_ms: i64,
        change: impl FnOnce(&mut Option<Task>) -> Result<(Output, TaskEffects)>,
    ) -> Result<Option<Output>> {
        self.commit_task_effects_fenced(
            session_id,
            TaskFence::Observation(permit),
            now_ms,
            None,
            change,
        )
    }

    /// Commit a task and its effects against an unchanged working document in the same fence.
    pub(crate) fn commit_task_effects_with_document<Output>(
        &mut self,
        session_id: &str,
        permit: &crate::task::TaskPermit,
        now_ms: i64,
        document: crate::plan::WorkingDocumentGuard<'_>,
        change: impl FnOnce(&mut Option<Task>) -> Result<(Output, TaskEffects)>,
    ) -> Result<Output> {
        let admission = self.session_admission(session_id)?;
        admission.with_task_mutation(|| {
            permit.validate(admission.runtime()?.active_task.as_ref())?;
            document.commit(session_id, |document| {
                self.commit_task_effects_locked(
                    session_id,
                    TaskFence::Revision(Some(permit.revision())),
                    now_ms,
                    None,
                    Some(document),
                    |active| {
                        permit.validate(active.as_ref())?;
                        change(active)
                    },
                )?
                .context("document task commit unexpectedly reused a command")
            })
        })
    }

    fn commit_task_effects_fenced<Output>(
        &mut self,
        session_id: &str,
        fence: TaskFence<'_>,
        now_ms: i64,
        command: Option<&crate::task::TaskOperation>,
        change: impl FnOnce(&mut Option<Task>) -> Result<(Output, TaskEffects)>,
    ) -> Result<Option<Output>> {
        let admission = self.session_admission(session_id)?;
        admission.with_task_mutation(|| {
            self.commit_task_effects_locked(session_id, fence, now_ms, command, None, change)
        })
    }

    /// Commit and publish while the session's task mutation gate excludes dependent writes.
    fn commit_task_effects_locked<Output>(
        &mut self,
        session_id: &str,
        fence: TaskFence<'_>,
        now_ms: i64,
        command: Option<&crate::task::TaskOperation>,
        document: Option<&crate::plan::WorkingDocumentCommit<'_>>,
        change: impl FnOnce(&mut Option<Task>) -> Result<(Output, TaskEffects)>,
    ) -> Result<Option<Output>> {
        let artifacts =
            crate::plan::PlanFileStore::new(self.data_root.clone(), std::path::PathBuf::new());
        let mut affected_plans = Vec::new();
        let result = (|| {
            if let Some(command) = command {
                let existing: Option<String> = self
                    .connection
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
            let observed_payload: Option<String> = self.connection.query_row(
            "SELECT json_extract(payload,'$.session.active_task') FROM session_record WHERE id=?1 AND json_extract(payload,'$.format_version')=?2",
            params![session_id, super::SESSION_FORMAT_VERSION], |row| row.get(0),
        ).optional()?.context("task session is missing or incompatible")?;
            let mut prepared_active: Option<Task> = observed_payload
                .as_ref()
                .map(|payload| super::decode(payload))
                .transpose()?;
            match &fence {
                TaskFence::Revision(expected)
                    if prepared_active.as_ref().map(|task| &task.revision) != *expected =>
                {
                    return Err(crate::task::StaleTask.into());
                }
                TaskFence::Observation(permit) => {
                    permit.validate_observation(prepared_active.as_ref())?
                }
                TaskFence::Revision(_) => {}
            }
            let (output, mut effects) = change(&mut prepared_active)?;
            // The active planning task also writes its artifact below. Include that
            // implicit effect before sealing files or acquiring SQLite's write lock.
            if let Some(task) = &prepared_active
                && let WorkflowSnapshot::Plan { plan } = &task.workflow
            {
                effects
                    .plan
                    .retain(|artifact| artifact.id != plan.artifact.id);
                effects.plan.push(plan.artifact.clone());
            }
            for plan in &effects.plan {
                let claimed = match document.filter(|document| document.matches(plan)) {
                    Some(document) => document.claim_artifacts(plan)?,
                    None => artifacts.claim_plan_artifacts(plan)?,
                };
                if claimed {
                    affected_plans.push(plan.id.clone());
                }
            }
            let transaction = self
                .connection
                .transaction_with_behavior(TransactionBehavior::Immediate)?;
            Self::ensure_no_configuration_intent(&transaction, session_id)?;
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
            ensure!(
                payload == observed_payload,
                "task changed during artifact preparation"
            );
            let mut active: Option<Task> =
                payload.map(|payload| super::decode(&payload)).transpose()?;
            match fence {
                TaskFence::Revision(expected)
                    if active.as_ref().map(|task| &task.revision) != expected =>
                {
                    return Err(crate::task::StaleTask.into());
                }
                TaskFence::Observation(permit) => permit.validate_observation(active.as_ref())?,
                TaskFence::Revision(_) => {}
            }
            let previous = active.clone();
            active = prepared_active;
            if let Some(rollback) = &effects.rollback {
                rollback.apply(&transaction)?;
            }
            for guard in &effects.continuation_guard {
                validate_continuation_guard(&transaction, session_id, guard, previous.as_ref())?;
            }
            for selection in &effects.source_plan {
                let source: crate::plan::PlanRecord =
                    read_record(&transaction, "plan_record", selection.id().as_str())?
                        .ok_or(crate::exchange::command::TaskCommandFailure::PlanMissing)?;
                selection.validate(&source)?;
            }
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
            let completed = effects.event.iter().any(|event| matches!(&event.detail,
            crate::timeline::SessionEventKind::TaskFinished { outcome } if matches!(outcome.state, crate::task::TaskOutcomeState::Complete)));
            if !completed
                && let Some(previous) = previous.as_ref()
                && active.as_ref().is_none_or(|current| {
                    current.id != previous.id
                        || current.status() == crate::task::TaskStatus::Cancelled
                })
            {
                crate::question::retire_task(&transaction, session_id, previous.id.as_str())?;
            }
            if retire_questions {
                super::question::retire_task_questions(
                    &transaction,
                    session_id,
                    previous.as_ref(),
                    now_ms,
                )?;
            }
            if let Some(task) = &mut active {
                task.edit(|draft| {
                    if let WorkflowSnapshot::Plan { plan } = &mut draft.workflow {
                        let mut payload = serde_json::to_value(&plan.artifact)?;
                        payload["schema_version"] =
                            serde_json::json!(crate::plan::PLAN_SCHEMA_VERSION);
                        let preserved = super::question::preserve_answers(
                            &transaction,
                            "plan_record",
                            &plan.artifact.id,
                            super::encode(&payload)?,
                        )?;
                        plan.artifact = super::decode(&preserved)?;
                        let prepared = effects
                            .plan
                            .iter_mut()
                            .find(|artifact| artifact.id == plan.artifact.id)
                            .context(
                                "active plan artifact was not prepared before the transaction",
                            )?;
                        *prepared = plan.artifact.clone();
                    }
                    Ok(())
                })?;
            }
            for checkpoint in &effects.checkpoint {
                ensure!(
                    checkpoint.session_id == session_id,
                    "checkpoint belongs to another session"
                );
                let written = transaction.execute(
                "INSERT INTO checkpoint_record(id,session_id,payload) VALUES(?1,?2,?3) ON CONFLICT(id) DO UPDATE SET payload=excluded.payload WHERE checkpoint_record.session_id=excluded.session_id",
                params![checkpoint.id, session_id, super::encode(checkpoint)?],
            )?;
                ensure!(written == 1, "checkpoint cannot change its owning session");
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
            if let Some(task) = &active
                && let WorkflowSnapshot::Execute { execution } = &task.workflow
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
            if let Some(commit) = effects.owner_task_commit {
                crate::exchange::journal::commit_task_command(&transaction, session_id, commit)?;
            }
            transaction.commit()?;
            if retire_questions {
                self.refresh_admission_membership(session_id)?;
            } else {
                self.publish_admission(session_id)?;
            }
            Ok(Some(output))
        })();
        // Reconcile only after SQL has committed or its transaction has rolled back.
        for id in affected_plans {
            if let Some(plan) = self.load_plan(&id)? {
                match document.filter(|document| document.matches(&plan)) {
                    Some(document) => document.reconcile(&plan)?,
                    None => artifacts.reconcile_plan_artifacts(&plan)?,
                }
            }
        }
        result
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
    let Some(mut active): Option<Task> =
        payload.map(|payload| super::decode(&payload)).transpose()?
    else {
        return Ok(());
    };
    if clearing {
        connection.execute("UPDATE session_record SET payload=json_set(payload,'$.session.active_task',NULL) WHERE id=?1", [session_id])?;
        return Ok(());
    }
    let mut draft = active.draft();
    match &mut draft.workflow {
        WorkflowSnapshot::Goal { goal } => {
            if goal.state == crate::goal::GoalState::Active {
                goal.state = crate::goal::GoalState::Paused;
            }
        }
        WorkflowSnapshot::Plan { plan } => {
            if !matches!(
                plan.artifact.state,
                crate::plan::PlanState::Accepted
                    | crate::plan::PlanState::Cancelled
                    | crate::plan::PlanState::Rejected
            ) {
                plan.paused = true;
            }
        }
        WorkflowSnapshot::Execute { execution } => {
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
    draft.renew(draft.updated_at_ms);
    active = Task::try_from(draft)?;
    if let WorkflowSnapshot::Execute { execution } = &active.workflow {
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
        "SELECT ordinal,json_extract(payload,'$.state'),COALESCE(json_array_length(json_extract(payload,'$.turn')),0) FROM exchange_record WHERE session_id=?1 AND agent_id=?2 AND ordinal=?3",
        params![session_id, crate::session::HarnessSession::primary_agent_id(session_id), i64::try_from(turn.exchange_ordinal)?],
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
                    crate::task::TaskChange::Update(Box::new(task)),
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
            assert_ne!(after.revision, before.revision);
            assert_eq!(after.id, before.id);
            assert_eq!(after.status(), crate::task::TaskStatus::Paused);
            assert_eq!(after.entry, before.entry);
            assert_eq!(
                after.turn.as_ref().map(|turn| &turn.id),
                before.turn.as_ref().map(|turn| &turn.id)
            );
            if let Some(input) = after
                .turn
                .as_ref()
                .and_then(|turn| turn.continuation.as_ref())
            {
                assert_eq!(input.delivery, crate::task::TaskInputDelivery::Cancelled);
            }
        }
        Ok(())
    }

    /// Task metadata must not recover a live file job before its owning result commits.
    #[test]
    fn native_receipt_and_metadata_preserve_pending_plan_artifacts_until_owner_commit() -> Result<()>
    {
        let (root, mut store, controller) = crate::task::SessionTaskController::test_goal();
        let workspace = tempfile::tempdir()?;
        crate::checks::configure_fixture(workspace.path());
        let files = crate::plan::PlanFileStore::new(root.path(), workspace.path());
        let mut document = crate::plan::test_fixture("pending-artifact", "Preserve live edits");
        let mut design = crate::plan::DeclarationDesign::open(workspace.path())?;
        design.document.objective = "Preserve live artifact ownership".into();
        design.document.background =
            "Metadata commits can arrive between file preparation and commit.".into();
        design.document.design = "Reconcile only the journal owned by the file revision.".into();
        design.proposed.insert(
            "src/owner.rs".into(),
            "/// Own the state.
pub struct Owner;
"
            .into(),
        );
        document.design = Some(design);
        let prepared =
            files.prepare_entry("session", "Preserve live edits".into(), Some(document), 2)?;
        let id = prepared.record.id.clone();
        let mut task = Task::plan(prepared.record, 2)?;
        task.edit(|draft| draft.request_entry("Prepare the plan".into(), 2))?;
        let current = controller.current()?.unwrap();
        controller.change(
            &mut store,
            Some(&current.revision),
            crate::task::TaskChange::Install(Box::new(task)),
            2,
        )?;
        let prior = store.load_plan(&id)?.unwrap();
        let original = files.read_working_document("session", &id)?;
        let mut changed = original.clone();
        changed.version += 1;
        changed.overview = "The live worker has written this pending edit.".into();
        files.write_working_mutation_guarded(&prior, &original, &changed)?;
        let journal = root
            .path()
            .join("plans/session")
            .join(&id)
            .join("artifact-recovery.json");
        let pending = std::fs::read(&journal)?;

        let current = controller.current()?.unwrap();
        assert!(controller.acknowledge_dispatched_input(
            &mut store,
            &current.permit().receipt(),
            &current.entry.as_ref().unwrap().id,
            crate::task::TaskReceiptInput::Entry,
            3,
        )?);
        assert_eq!(std::fs::read(&journal)?, pending);
        assert_eq!(files.read_working_document("session", &id)?, changed);
        let current = controller.current()?.unwrap();
        controller.change(
            &mut store,
            Some(&current.revision),
            crate::task::TaskChange::Pause,
            3,
        )?;
        assert_eq!(std::fs::read(&journal)?, pending);
        assert_eq!(files.read_working_document("session", &id)?, changed);
        let mut metadata = store.load_plan(&id)?.unwrap();
        metadata.updated_at_ms = 4;
        store.save_plan(&metadata)?;
        assert_eq!(std::fs::read(&journal)?, pending);
        assert_eq!(files.read_working_document("session", &id)?, changed);

        let current = controller.current()?.unwrap();
        let mut target = metadata;
        target.document_version = changed.version;
        target.title = changed.title.clone();
        controller.update(&mut store, &current.permit(), 5, |task| {
            let WorkflowSnapshot::Plan { plan } = &mut task.workflow else {
                unreachable!()
            };
            plan.artifact = target.clone();
            Ok(())
        })?;
        assert!(!journal.exists());
        assert_eq!(files.read_working_document("session", &id)?, changed);
        assert_eq!(
            store.load_plan(&id)?.unwrap().document_version,
            changed.version
        );

        let prior = store.load_plan(&id)?.unwrap();
        let mut submitted = prior.clone();
        submitted.model_revision += 1;
        let (document, _, digest) = files.submit_document_revision(
            "session",
            &id,
            submitted.model_revision,
            submitted.document_version,
        )?;
        submitted.submitted_version = Some(document.version);
        submitted.review_digest = Some(digest);
        let pending_submission = std::fs::read(&journal)?;
        let current = controller.current()?.unwrap();
        controller.change(
            &mut store,
            Some(&current.revision),
            crate::task::TaskChange::Pause,
            6,
        )?;
        store.save_plan(&prior)?;
        assert_eq!(std::fs::read(&journal)?, pending_submission);
        let current = controller.current()?.unwrap();
        controller.update(&mut store, &current.permit(), 7, |task| {
            let WorkflowSnapshot::Plan { plan } = &mut task.workflow else {
                unreachable!()
            };
            plan.artifact = submitted.clone();
            Ok(())
        })?;
        assert!(!journal.exists());
        assert_eq!(
            store.load_plan(&id)?.unwrap().model_revision,
            submitted.model_revision
        );
        Ok(())
    }

    /// Implicit planning artifacts participate in the same file/database commit as explicit effects.
    #[test]
    fn implicit_plan_effect_reconciles_artifacts_on_commit_and_rollback() -> Result<()> {
        let (root, mut store, controller) = crate::task::SessionTaskController::test_goal();
        let workspace = tempfile::tempdir()?;
        crate::checks::configure_fixture(workspace.path());
        let files = crate::plan::PlanFileStore::new(root.path(), workspace.path());
        let mut document = crate::plan::test_fixture("implicit-artifact", "Plan the change");
        let mut design = crate::plan::DeclarationDesign::open(workspace.path())?;
        design.document.objective = "Preserve the plan artifact commit boundary".into();
        design.document.background =
            "A planning task owns a working artifact and its submitted revisions.".into();
        design.document.design =
            "Commit the artifact together with the task and recover either database outcome."
                .into();
        design.proposed.insert(
            "src/owner.rs".into(),
            "/// Own the plan artifact.\npub struct Owner;\n".into(),
        );
        document.design = Some(design);
        let prepared =
            files.prepare_entry("session", "Plan the change".into(), Some(document), 2)?;
        let id = prepared.record.id.clone();
        let previous = controller.current()?.unwrap();
        controller.change(
            &mut store,
            Some(&previous.revision),
            crate::task::TaskChange::Install(Box::new(Task::plan(prepared.record, 2)?)),
            2,
        )?;
        for (fail, held_document) in [(true, false), (false, false), (true, true), (false, true)] {
            let current = controller.current()?.unwrap();
            let mut target = store.load_plan(&id)?.unwrap();
            target.model_revision += 1;
            let (_, _, digest) = files.submit_document_revision(
                "session",
                &id,
                target.model_revision,
                target.document_version,
            )?;
            target.review_digest = Some(digest);
            let journal = root
                .path()
                .join("plans/session")
                .join(&id)
                .join("artifact-recovery.json");
            assert!(journal.exists());
            if fail {
                store.connection.execute_batch("CREATE TRIGGER reject_implicit_plan BEFORE UPDATE ON session_record BEGIN SELECT RAISE(ABORT,'implicit plan commit failure'); END;")?;
            }
            let result = if held_document {
                let document = files.read_working_document("session", &id)?;
                store.commit_task_effects_with_document(
                    "session",
                    &current.permit(),
                    3,
                    files.guard_working_document(&document),
                    |active| {
                        let task = active.as_mut().context("planning task disappeared")?;
                        task.edit(|draft| {
                            let WorkflowSnapshot::Plan { plan } = &mut draft.workflow else {
                                unreachable!()
                            };
                            plan.artifact = target.clone();
                            Ok(())
                        })?;
                        Ok(((), TaskEffects::default()))
                    },
                )
            } else {
                controller.update(&mut store, &current.permit(), 3, |task| {
                    let WorkflowSnapshot::Plan { plan } = &mut task.workflow else {
                        unreachable!()
                    };
                    plan.artifact = target.clone();
                    Ok(())
                })
            };
            assert_eq!(result.is_err(), fail);
            assert!(
                !journal.exists(),
                "the implicit artifact journal must resolve on either SQL outcome"
            );
            let persisted = store.load_plan(&id)?.unwrap();
            assert_eq!(
                persisted.model_revision,
                if fail {
                    target.model_revision - 1
                } else {
                    target.model_revision
                }
            );
            if fail {
                store
                    .connection
                    .execute_batch("DROP TRIGGER reject_implicit_plan")?;
            }
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
        let mut plan = Task::plan(prepared.record, 2)?;
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
        let task = Task::goal("Finish".into(), ContinuationBudget::default(), false, 1);
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

        let replacement = Task::goal(
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
        let task = Task::goal("Finish".into(), ContinuationBudget::default(), false, 1);
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
