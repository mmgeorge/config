//! Retain terminal intent separately from the workflow transition it may authorize.

use anyhow::{Result, ensure};
use rusqlite::{OptionalExtension, params};
use serde::{Deserialize, Serialize};

use super::SqliteStore;

/// Persist whether recorded terminal intent still waits or reached a definite disposition.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
#[serde(tag = "state", rename_all = "snake_case")]
pub(crate) enum TerminalDisposition {
    /// The owning invocation has not committed or rejected this request.
    Pending,
    /// The task transition committed atomically with its replay result.
    Committed { result: serde_json::Value },
    /// A definite failure or superseding control prevents later advancement.
    Rejected { reason: String },
}

/// Commit this receipt in the same SQL transaction as its task transition.
pub(crate) struct TerminalCommit {
    pub id: String,
    pub result: serde_json::Value,
}

impl SqliteStore {
    /// A replay must retain exactly the first immutable intent and its original disposition.
    #[cfg(test)]
    pub(crate) fn record_terminal_request(
        &mut self,
        session: &str,
        id: &str,
        intent: &impl Serialize,
    ) -> Result<()> {
        self.record_task_terminal_request(session, id, None, intent, None)
    }

    /// Record immutable intent and its optional presentation under the original task owner.
    pub(crate) fn record_task_terminal_request(
        &mut self,
        session: &str,
        id: &str,
        task: Option<&crate::task::TaskId>,
        intent: &impl Serialize,
        event: Option<crate::timeline::SessionEventRecord>,
    ) -> Result<()> {
        let intent = serde_json::to_string(intent)?;
        let tx = self
            .connection
            .transaction_with_behavior(rusqlite::TransactionBehavior::Immediate)?;
        let existing: Option<String> = tx
            .query_row(
                "SELECT intent FROM terminal_request WHERE session_id=?1 AND id=?2",
                params![session, id],
                |row| row.get(0),
            )
            .optional()?;
        if let Some(existing) = existing {
            ensure!(
                existing == intent,
                "terminal request identity was reused with different intent"
            );
        } else {
            tx.execute("INSERT INTO terminal_request(session_id,id,intent,disposition,task_id) VALUES(?1,?2,?3,?4,?5)",
                params![session, id, intent, serde_json::to_string(&TerminalDisposition::Pending)?, task.map(|id| id.as_str())])?;
        }
        if let Some(event) = event {
            ensure!(
                event.session_id == session && event.owner.session_id() == session,
                "terminal event belongs to another session"
            );
            tx.execute("INSERT INTO session_event_record(id,session_id,payload) VALUES(?1,?2,?3) ON CONFLICT(id) DO NOTHING",
                params![event.id, session, super::encode(&event)?])?;
        }
        tx.commit()?;
        Ok(())
    }

    /// Find the original committed decision for the same submitted content and caller settings.
    pub(crate) fn committed_plan_acceptance(
        &self,
        session: &str,
        selection: &crate::exchange::command::PlanExecutionSelection,
    ) -> Result<Option<crate::exchange::command::CommandId>> {
        let mut query = self.connection.prepare(
            "SELECT id,intent FROM terminal_request WHERE session_id=?1 AND id LIKE 'plan_acceptance:%' AND json_extract(disposition,'$.state')='committed' ORDER BY rowid",
        )?;
        let rows = query.query_map([session], |row| {
            Ok((row.get::<_, String>(0)?, row.get::<_, String>(1)?))
        })?;
        for row in rows {
            let (id, intent) = row?;
            let (original, _): (
                crate::exchange::command::PlanExecutionSelection,
                crate::task::command::ExecuteSelection,
            ) = serde_json::from_str(&intent)?;
            if original.source().id() == selection.source().id()
                && original.source().document_version() == selection.source().document_version()
                && original.source().review_digest() == selection.source().review_digest()
                && original.normalized_request() == selection.normalized_request()
            {
                return Ok(Some(crate::exchange::command::CommandId::new(
                    id.strip_prefix("plan_acceptance:")
                        .expect("query selected acceptance identity"),
                )?));
            }
        }
        Ok(None)
    }

    /// Pending rows prove no terminal transaction committed before the old owner disappeared.
    pub(crate) fn pending_terminal_requests(
        &self,
        session: &str,
    ) -> Result<Vec<(String, Option<String>)>> {
        let mut query = self.connection.prepare("SELECT id,task_id FROM terminal_request WHERE session_id=?1 AND json_extract(disposition,'$.state')='pending'")?;
        Ok(query
            .query_map([session], |row| Ok((row.get(0)?, row.get(1)?)))?
            .collect::<rusqlite::Result<Vec<_>>>()?)
    }

    /// Get the durable disposition without interpreting pending intent as task success.
    pub(crate) fn terminal_disposition(
        &self,
        session: &str,
        id: &str,
    ) -> Result<Option<TerminalDisposition>> {
        let payload: Option<String> = self
            .connection
            .query_row(
                "SELECT disposition FROM terminal_request WHERE session_id=?1 AND id=?2",
                params![session, id],
                |row| row.get(0),
            )
            .optional()?;
        payload
            .map(|payload| Ok(serde_json::from_str(&payload)?))
            .transpose()
    }

    /// A failed or superseded request remains inspectable and cannot later become successful.
    pub(crate) fn reject_terminal_request(
        &mut self,
        session: &str,
        id: &str,
        reason: &str,
    ) -> Result<()> {
        let tx = self
            .connection
            .transaction_with_behavior(rusqlite::TransactionBehavior::Immediate)?;
        let payload: Option<String> = tx
            .query_row(
                "SELECT disposition FROM terminal_request WHERE session_id=?1 AND id=?2",
                params![session, id],
                |row| row.get(0),
            )
            .optional()?;
        if payload
            .map(|payload| serde_json::from_str::<TerminalDisposition>(&payload))
            .transpose()?
            == Some(TerminalDisposition::Pending)
        {
            tx.execute(
                "UPDATE terminal_request SET disposition=?3 WHERE session_id=?1 AND id=?2",
                params![
                    session,
                    id,
                    serde_json::to_string(&TerminalDisposition::Rejected {
                        reason: reason.into()
                    })?
                ],
            )?;
            update_presentation(
                &tx,
                session,
                id,
                &format!("Execution did not start: {reason}"),
            )?;
        }
        tx.commit()?;
        Ok(())
    }
}

/// Commit a terminal receipt and settle its timeline row.
///
/// A plan acceptance names entering execution only when the commit installs the Execute task
/// (`executing`). An acceptance that became a revision first commits on the planning task, and
/// a revision accepted while the execution runs commits on the Execute task it keeps, so in both
/// cases its pending row is removed; the revision writes its own lifecycle row.
pub(super) fn commit(
    transaction: &rusqlite::Transaction<'_>,
    session: &str,
    receipt: TerminalCommit,
    executing: bool,
) -> Result<()> {
    let state: String = transaction.query_row(
        "SELECT disposition FROM terminal_request WHERE session_id=?1 AND id=?2",
        params![session, receipt.id],
        |row| row.get(0),
    )?;
    let committed = TerminalDisposition::Committed {
        result: receipt.result,
    };
    let previous: TerminalDisposition = serde_json::from_str(&state)?;
    ensure!(
        previous == TerminalDisposition::Pending || previous == committed,
        "terminal request is no longer pending"
    );
    transaction.execute(
        "UPDATE terminal_request SET disposition=?3 WHERE session_id=?1 AND id=?2",
        params![session, receipt.id, serde_json::to_string(&committed)?],
    )?;
    if receipt.id.starts_with("plan_acceptance:") && !executing {
        transaction.execute(
            "DELETE FROM session_event_record WHERE id=?1 AND session_id=?2",
            params![format!("{session}:terminal:{}", receipt.id), session],
        )?;
        return Ok(());
    }
    update_presentation(
        transaction,
        session,
        &receipt.id,
        "Approved plan entered execution",
    )?;
    Ok(())
}

fn update_presentation(
    transaction: &rusqlite::Transaction<'_>,
    session: &str,
    id: &str,
    message: &str,
) -> Result<()> {
    let event_id = format!("{session}:terminal:{id}");
    let payload: Option<String> = transaction
        .query_row(
            "SELECT payload FROM session_event_record WHERE id=?1 AND session_id=?2",
            params![event_id, session],
            |row| row.get(0),
        )
        .optional()?;
    if let Some(payload) = payload {
        let mut event: crate::timeline::SessionEventRecord = super::decode(&payload)?;
        event.detail = crate::timeline::SessionEventKind::TaskAdvancement {
            message: message.into(),
        };
        transaction.execute(
            "UPDATE session_event_record SET payload=?2 WHERE id=?1",
            params![event_id, super::encode(&event)?],
        )?;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn terminal_receipts_survive_reopen_and_reject_conflicting_intent() -> Result<()> {
        let (root, mut store, _) = crate::task::SessionTaskController::test_goal();
        store.record_terminal_request(
            "session",
            "request",
            &serde_json::json!({"phase":"implement"}),
        )?;
        store.record_terminal_request(
            "session",
            "request",
            &serde_json::json!({"phase":"implement"}),
        )?;
        assert!(
            store
                .record_terminal_request(
                    "session",
                    "request",
                    &serde_json::json!({"phase":"verify"})
                )
                .is_err()
        );
        {
            let tx = store
                .connection
                .transaction_with_behavior(rusqlite::TransactionBehavior::Immediate)?;
            commit(
                &tx,
                "session",
                TerminalCommit {
                    id: "request".into(),
                    result: serde_json::json!({"next":"checking"}),
                },
                false,
            )?;
            // Dropping the enclosing transaction must also undo its receipt.
        }
        assert_eq!(
            store.terminal_disposition("session", "request")?,
            Some(TerminalDisposition::Pending)
        );
        let tx = store
            .connection
            .transaction_with_behavior(rusqlite::TransactionBehavior::Immediate)?;
        commit(
            &tx,
            "session",
            TerminalCommit {
                id: "request".into(),
                result: serde_json::json!({"next":"checking"}),
            },
            false,
        )?;
        tx.commit()?;
        let reopened = SqliteStore::open(root.path())?;
        assert_eq!(
            reopened.terminal_disposition("session", "request")?,
            Some(TerminalDisposition::Committed {
                result: serde_json::json!({"next":"checking"})
            })
        );
        Ok(())
    }

    #[test]
    fn recovery_rejects_uncommitted_intent_without_touching_replacement_tasks() -> Result<()> {
        let (_root, mut store, controller) = crate::task::SessionTaskController::test_goal();
        let original = controller.current()?.unwrap();
        store.record_task_terminal_request(
            "session",
            "old-request",
            Some(&original.id),
            &"intent",
            None,
        )?;
        controller.recover_terminal_requests(&mut store, 2)?;
        assert_eq!(
            controller.current()?.unwrap().status(),
            crate::task::TaskStatus::Paused
        );
        assert!(matches!(
            store.terminal_disposition("session", "old-request")?,
            Some(TerminalDisposition::Rejected { .. })
        ));
        controller.change(
            &mut store,
            Some(&controller.current()?.unwrap().revision),
            crate::task::TaskChange::Install(Box::new(crate::task::Task::goal(
                "Replacement".into(),
                Default::default(),
                3,
            ))),
            3,
        )?;
        store.record_task_terminal_request(
            "session",
            "late-old-request",
            Some(&original.id),
            &"intent",
            None,
        )?;
        let replacement = controller.current()?.unwrap();
        controller.recover_terminal_requests(&mut store, 4)?;
        assert_eq!(
            controller.current()?.unwrap().revision,
            replacement.revision
        );
        Ok(())
    }

    #[test]
    fn rejected_terminal_receipt_rolls_back_task_change() -> Result<()> {
        let (_root, mut store, controller) = crate::task::SessionTaskController::test_goal();
        store.record_terminal_request("session", "rejected", &"intent")?;
        store.reject_terminal_request("session", "rejected", "native turn interrupted")?;
        let before = controller.current()?.unwrap();
        let result = store.commit_task_effects("session", Some(&before.revision), 50, |active| {
            let mut draft = active.as_ref().unwrap().draft();
            draft.updated_at_ms = 50;
            *active = Some(crate::task::Task::try_from(draft)?);
            Ok((
                (),
                crate::storage::TaskEffects {
                    terminal_commit: Some(TerminalCommit {
                        id: "rejected".into(),
                        result: serde_json::json!(true),
                    }),
                    ..Default::default()
                },
            ))
        });
        assert!(result.is_err());
        assert_eq!(
            serde_json::to_value(controller.current()?)?,
            serde_json::to_value(Some(before))?
        );
        assert!(matches!(
            store.terminal_disposition("session", "rejected")?,
            Some(TerminalDisposition::Rejected { .. })
        ));
        Ok(())
    }

    /// Commit one plan acceptance whose pending row was recorded, with `install` producing the
    /// task the commit leaves, and return the transcript rows the acceptance left.
    fn acceptance_rows(
        install: impl FnOnce(&crate::task::Task) -> Result<crate::task::Task>,
        prepare_execute: bool,
    ) -> Result<Vec<String>> {
        use crate::plan::PlanExecutionRecord;
        let (_root, mut store, controller) = crate::task::SessionTaskController::test_goal();
        if prepare_execute {
            let previous = controller.current()?.unwrap();
            let record: PlanExecutionRecord = serde_json::from_value(serde_json::json!({
                "id":"execution", "session_id":"session", "plan_id":"accepted-plan",
                "state":"active", "phase":"implement", "original_revision":1,"revision":1,
                "generation":0, "lifecycle":[],"findings":[],
                "progress":crate::plan::execution::SemanticProgress::default(),
                "verification":[],"revision_history":[],"resume_after_review":false,
                "observed_source":{},"observed_document_version":0,"observed_check":[],
                "created_at_ms":1
            }))?;
            controller.change(
                &mut store,
                Some(&previous.revision),
                crate::task::TaskChange::Install(Box::new(crate::task::Task::execute(
                    "Execute the accepted plan".into(),
                    record,
                    Default::default(),
                    2,
                )?)),
                2,
            )?;
        }
        let before = controller.current()?.unwrap();
        store.record_task_terminal_request(
            "session",
            "plan_acceptance:accept",
            Some(&before.id),
            &"intent",
            Some(crate::timeline::SessionEventRecord {
                id: "session:terminal:plan_acceptance:accept".into(),
                session_id: "session".into(),
                owner: crate::timeline::TimelineOwner::session("session"),
                created_at_ms: 3,
                detail: crate::timeline::SessionEventKind::TaskAdvancement {
                    message: "Plan approved; execution is waiting".into(),
                },
            }),
        )?;
        let installed = install(&before)?;
        store.commit_task_effects("session", Some(&before.revision), 4, |active| {
            *active = Some(installed);
            Ok((
                (),
                crate::storage::TaskEffects {
                    terminal_commit: Some(TerminalCommit {
                        id: "plan_acceptance:accept".into(),
                        result: serde_json::json!(true),
                    }),
                    ..Default::default()
                },
            ))
        })?;
        Ok(store
            .list_session_event("session")?
            .into_iter()
            .filter_map(|event| match event.detail {
                crate::timeline::SessionEventKind::TaskAdvancement { message } => Some(message),
                _ => None,
            })
            .collect())
    }

    /// An acceptance that installs the Execute task says execution started.
    #[test]
    fn an_acceptance_that_starts_execution_says_so() -> Result<()> {
        let rows = acceptance_rows(
            |_| {
                let record = serde_json::from_value(serde_json::json!({
                    "id":"execution", "session_id":"session", "plan_id":"accepted-plan",
                    "state":"active", "phase":"implement", "original_revision":1,"revision":1,
                    "generation":0, "lifecycle":[],"findings":[],
                    "progress":crate::plan::execution::SemanticProgress::default(),
                    "verification":[],"revision_history":[],"resume_after_review":false,
                    "observed_source":{},"observed_document_version":0,"observed_check":[],
                    "created_at_ms":1
                }))?;
                crate::task::Task::execute(
                    "Execute the accepted plan".into(),
                    record,
                    Default::default(),
                    4,
                )
            },
            false,
        )?;
        assert_eq!(rows, ["Approved plan entered execution"]);
        Ok(())
    }

    /// A revision accepted while its execution runs leaves no "entered execution" row.
    ///
    /// The execution was already running, so the row repeated the start and, recorded at the
    /// session level, was drawn after the whole Resolve exchange; the revision's own lifecycle
    /// row records the acceptance where it happened.
    #[test]
    fn a_revision_accepted_during_execution_adds_no_entered_execution_row() -> Result<()> {
        let rows = acceptance_rows(
            |current| {
                let mut draft = current.draft();
                draft.updated_at_ms = 4;
                crate::task::Task::try_from(draft)
            },
            true,
        )?;
        assert!(rows.is_empty(), "{rows:?}");
        Ok(())
    }
}
