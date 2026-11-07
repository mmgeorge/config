use anyhow::{Context, Result};
use rusqlite::{OptionalExtension, TransactionBehavior, params};
use serde_json::{Value, json};

use super::SqliteStore;
use crate::plan::PlanElicitation;

const REVIEW_ELICITATION_PATH: &str = "$.review.elicitation";

pub(super) fn preserve_answers(
    transaction: &rusqlite::Transaction<'_>,
    table: &str,
    id: &str,
    encoded: String,
) -> Result<String> {
    let mut incoming: Value = serde_json::from_str(&encoded)?;
    let stored: Option<String> = transaction
        .query_row(
            &format!("SELECT payload FROM {table} WHERE id=?1"),
            [id],
            |row| row.get(0),
        )
        .optional()?;
    if let Some(stored) = stored {
        preserve_withdrawals(table, &mut incoming, &serde_json::from_str(&stored)?)?;
    }
    for (pointer, path) in [
        ("/elicitation", "$.elicitation"),
        ("/review/elicitation", REVIEW_ELICITATION_PATH),
    ] {
        let Some(proposed) = incoming
            .pointer_mut(pointer)
            .filter(|value| value.is_object())
        else {
            continue;
        };
        let stored: Option<String> = transaction
            .query_row(
                &format!("SELECT json_extract(payload,?2) FROM {table} WHERE id=?1"),
                params![id, path],
                |row| row.get(0),
            )
            .optional()?
            .flatten();
        let Some(stored) = stored else { continue };
        let stored: Value = serde_json::from_str(&stored)?;
        if stored.pointer("/question_set/id") == proposed.pointer("/question_set/id")
            && stored["revision"] == proposed["revision"]
            && stored["answer_revision"].as_u64().unwrap_or(0)
                > proposed["answer_revision"].as_u64().unwrap_or(0)
        {
            *proposed = stored;
        }
    }
    Ok(serde_json::to_string(&incoming)?)
}

fn preserve_withdrawals(table: &str, incoming: &mut Value, stored: &Value) -> Result<()> {
    if table == "exchange_record" {
        for withdrawal in stored["node_list"]
            .as_array()
            .into_iter()
            .flatten()
            .filter(|node| node["kind"] == "question_withdrawn")
        {
            if incoming["elicitation"]["question_set"]["id"] == withdrawal["question"]["id"] {
                incoming["elicitation"] = Value::Null;
                incoming["awaiting_input"] = json!(false);
                incoming["check_setup"] = Value::Null;
            }
            let nodes = incoming["node_list"]
                .as_array_mut()
                .context("exchange source nodes are missing")?;
            if !nodes.iter().any(|node| node["id"] == withdrawal["id"]) {
                nodes.push(withdrawal.clone());
            }
        }
    } else if table == "plan_record" {
        let withdrawn: Vec<_> = stored["question_ledger"]["resolution"]
            .as_array()
            .into_iter()
            .flatten()
            .filter(|resolution| resolution["kind"] == "withdrawn")
            .cloned()
            .collect();
        for pointer in ["/elicitation", "/review/elicitation"] {
            let retired = incoming
                .pointer(pointer)
                .and_then(|elicitation| elicitation["question_set"]["questions"].as_array())
                .is_some_and(|questions| {
                    questions.iter().any(|question| {
                        withdrawn
                            .iter()
                            .any(|resolution| resolution["question_id"] == question["id"])
                    })
                });
            if retired {
                if pointer == "/elicitation" {
                    incoming["elicitation"] = Value::Null;
                    if incoming["state"] == "awaiting_input" {
                        incoming["state"] = stored["state"].clone();
                    }
                } else {
                    incoming["review"] = Value::Null;
                }
            }
        }
        if !withdrawn.is_empty() {
            if incoming["question_ledger"].is_null() {
                incoming["question_ledger"] = json!({"resolution":[]});
            }
            if incoming["question_ledger"]["resolution"].is_null() {
                incoming["question_ledger"]["resolution"] = json!([]);
            }
            let resolution = incoming["question_ledger"]["resolution"]
                .as_array_mut()
                .context("plan question resolutions are missing")?;
            for withdrawal in withdrawn {
                if let Some(existing) = resolution
                    .iter_mut()
                    .find(|item| item["question_id"] == withdrawal["question_id"])
                {
                    *existing = withdrawal;
                } else {
                    resolution.push(withdrawal);
                }
            }
        }
    }
    Ok(())
}

pub(super) fn retire_task_questions(
    transaction: &rusqlite::Transaction<'_>,
    session_id: &str,
    previous: Option<&crate::task::Task>,
    now_ms: i64,
) -> Result<()> {
    let reason = "The task was replaced or cleared";
    let mut statement = transaction.prepare("SELECT id,payload FROM exchange_record WHERE session_id=?1 AND agent_id=?2 AND (json_extract(payload,'$.awaiting_input')=1 OR json_type(payload,'$.elicitation')='object')")?;
    let pending = statement
        .query_map(
            params![
                session_id,
                crate::session::HarnessSession::primary_agent_id(session_id)
            ],
            |row| Ok((row.get::<_, String>(0)?, row.get::<_, String>(1)?)),
        )?
        .collect::<rusqlite::Result<Vec<_>>>()?;
    drop(statement);
    for (id, payload) in pending {
        let mut exchange: Value = super::decode(&payload)?;
        if let Some(question) = exchange
            .pointer("/elicitation/question_set")
            .filter(|question| question.is_object())
            .cloned()
        {
            let answer = exchange["elicitation"]
                .get("answer")
                .cloned()
                .unwrap_or_else(|| json!([]));
            let nodes = exchange["node_list"]
                .as_array_mut()
                .context("pending exchange source nodes are missing")?;
            let withdrawal = format!(
                "{id}:withdrawn:{}",
                question["id"]
                    .as_str()
                    .context("pending question identity is missing")?
            );
            if !nodes.iter().any(|node| node["id"] == withdrawal) {
                nodes.push(json!({"kind":"question_withdrawn","id":withdrawal,"question":question,"answer":answer,"reason":reason}));
            }
        }
        exchange["awaiting_input"] = json!(false);
        exchange["elicitation"] = Value::Null;
        exchange["check_setup"] = Value::Null;
        transaction.execute(
            "UPDATE exchange_record SET payload=?2 WHERE id=?1",
            params![id, super::encode(&exchange)?],
        )?;
    }
    let plan_id = previous.and_then(|task| match &task.workflow {
        crate::task::WorkflowSnapshot::Plan { plan } => Some(plan.artifact.id.as_str()),
        crate::task::WorkflowSnapshot::Execute { execution } => {
            Some(execution.record.plan_id.as_str())
        }
        crate::task::WorkflowSnapshot::Goal { .. } => None,
    });
    let Some(plan_id) = plan_id else {
        return Ok(());
    };
    let Some(mut plan): Option<crate::plan::PlanRecord> =
        super::task::read_record(transaction, "plan_record", plan_id)?
    else {
        return Ok(());
    };
    let mut question = Vec::new();
    if let Some(elicitation) = plan.elicitation.take() {
        question.push(elicitation);
    }
    if let Some(acceptance) = plan.review.take() {
        question.push(acceptance.elicitation);
    }
    // A set whose every question already has an answer was resolved, not withdrawn.
    // Acceptance clears such a review, and retiring it would record a false withdrawal.
    question.retain(|elicitation| {
        elicitation.question_set.questions.iter().any(|item| {
            !elicitation
                .answer
                .iter()
                .any(|answer| answer.question_id == item.id)
        })
    });
    if question.is_empty() {
        return Ok(());
    }
    if plan.state == crate::plan::PlanState::AwaitingInput {
        plan.state = crate::plan::PlanState::Cancelled;
    }
    plan.updated_at_ms = now_ms;
    let latest: Option<String> = transaction.query_row("SELECT payload FROM exchange_record WHERE session_id=?1 AND agent_id=?2 ORDER BY ordinal DESC LIMIT 1", params![session_id,crate::session::HarnessSession::primary_agent_id(session_id)],|row|row.get(0)).optional()?;
    let anchor = latest
        .map(|payload| -> Result<_> {
            let exchange: crate::exchange::Exchange = super::decode(&payload)?;
            Ok(crate::plan::ExchangeAnchor::capture(&exchange))
        })
        .transpose()?;
    for elicitation in question {
        let question = elicitation.question_set;
        for item in &question.questions {
            plan.question_ledger.resolve(item, None, now_ms);
            if let Some(resolution) =
                plan.question_ledger
                    .resolution
                    .iter_mut()
                    .find(|resolution| {
                        resolution.question_id == item.id
                            && resolution.kind == crate::plan::PlanQuestionResolutionKind::Withdrawn
                    })
            {
                resolution.response = elicitation
                    .answer
                    .iter()
                    .find(|answer| answer.question_id == item.id)
                    .map(|answer| answer.response.clone());
            }
        }
        let lifecycle = crate::plan::PlanLifecycleRecord {
            id: format!("{plan_id}:withdrawn:{}", question.id),
            session_id: session_id.into(),
            plan_id: plan_id.into(),
            title: plan.title.clone(),
            anchor: anchor.clone(),
            kind: crate::plan::PlanLifecycleKind::QuestionWithdrawn,
            model_revision: plan.model_revision,
            user_revision: plan.user_revision,
            overall_comment: None,
            annotation: Vec::new(),
            question: Some(question),
            answer: Some(reason.into()),
            created_at_ms: now_ms,
        };
        let mut payload = serde_json::to_value(&lifecycle)?;
        payload["schema_version"] = json!(crate::plan::PLAN_SCHEMA_VERSION);
        transaction.execute("INSERT INTO plan_lifecycle_record(id,session_id,payload) VALUES(?1,?2,?3) ON CONFLICT(id) DO NOTHING",params![lifecycle.id,session_id,super::encode(&payload)?])?;
    }
    let mut payload = serde_json::to_value(&plan)?;
    payload["schema_version"] = json!(crate::plan::PLAN_SCHEMA_VERSION);
    transaction.execute(
        "UPDATE plan_record SET payload=?2 WHERE id=?1",
        params![plan.id, super::encode(&payload)?],
    )?;
    Ok(())
}

impl SqliteStore {
    /// Commit a validated decision only from its session owner's event boundary.
    pub(crate) fn commit_question_answer(
        &mut self,
        permit: &crate::exchange::QuestionUpdatePermit<'_>,
        request: &crate::plan::PlanQuestionAnswerRequest,
    ) -> std::result::Result<
        crate::plan::PlanQuestionAnswerCommit,
        crate::plan::PlanQuestionAnswerError,
    > {
        use crate::plan::PlanQuestionAnswerError;
        let session_id = permit.session();
        let admission = self
            .session_admission(session_id)
            .map_err(PlanQuestionAnswerError::State)?;
        admission
            .with_task_mutation(|| Ok(self.commit_question_answer_locked(session_id, request)))
            .map_err(PlanQuestionAnswerError::State)?
    }

    /// Couple the answer, dependent task snapshot, and immutable retry receipt in one transaction.
    fn commit_question_answer_locked(
        &mut self,
        session_id: &str,
        request: &crate::plan::PlanQuestionAnswerRequest,
    ) -> std::result::Result<
        crate::plan::PlanQuestionAnswerCommit,
        crate::plan::PlanQuestionAnswerError,
    > {
        use crate::plan::{PlanQuestionAnswerCommit, PlanQuestionAnswerError, PlanQuestionOwner};
        let identity = request.operation_id();
        let source = request.source_id();
        let question_id = request.question_id();
        let encoded = request.encode_receipt()?;
        let transaction = self
            .connection
            .transaction_with_behavior(TransactionBehavior::Immediate)?;
        let previous: Option<(String, String)> = transaction
            .query_row(
                "SELECT request,result FROM command_receipt WHERE session_id=?1 AND id=?2",
                params![session_id, identity],
                |row| Ok((row.get(0)?, row.get(1)?)),
            )
            .optional()?;
        if let Some((previous, result)) = previous {
            if previous != encoded {
                return Err(PlanQuestionAnswerError::IdentityConflict);
            }
            let result = PlanQuestionAnswerCommit::recover(request, &result)?;
            crate::exchange::SessionPublications::retain_plan_answer(
                &transaction,
                session_id,
                request,
                &result,
            )
            .map_err(|error| PlanQuestionAnswerError::State(error.into()))?;
            transaction.commit()?;
            return Ok(result);
        }

        // Validate captured source and question revisions before changing either owner record.
        let (table, pointer, condition) = match request.owner() {
            PlanQuestionOwner::Interaction => (
                "exchange_record",
                "$.elicitation",
                "json_extract(payload,'$.awaiting_input')=1",
            ),
            PlanQuestionOwner::Plan => (
                "plan_record",
                "$.elicitation",
                "json_extract(payload,'$.state')='awaiting_input'",
            ),
            PlanQuestionOwner::PlanReview => (
                "plan_record",
                REVIEW_ELICITATION_PATH,
                "json_extract(payload,'$.state')='awaiting_review'",
            ),
        };
        let text: Option<String> = transaction.query_row(
            &format!("SELECT json_extract(payload,?3) FROM {table} WHERE session_id=?1 AND id=?2 AND {condition}"),
            params![session_id, source, pointer], |row| row.get(0),
        ).optional()?.flatten();
        let mut elicitation: PlanElicitation =
            serde_json::from_str(&text.ok_or(PlanQuestionAnswerError::TargetMissing)?)?;
        if matches!(request.owner(), PlanQuestionOwner::PlanReview) {
            elicitation.require_review_confirmation();
        }
        if elicitation.question_set.id != request.set_id()
            || elicitation.revision != request.revision()
        {
            return Err(PlanQuestionAnswerError::Changed);
        }
        let order = request.answer_order();
        let superseded =
            order.is_some_and(|(writer, sequence)| {
                elicitation.answer_order.get(question_id).is_some_and(
                    |(previous_writer, previous)| previous_writer == writer && previous >= sequence,
                )
            });
        if !superseded {
            elicitation
                .answer(question_id, request.response().clone())
                .map_err(PlanQuestionAnswerError::InvalidAnswer)?;
            if let Some(order) = order {
                elicitation
                    .answer_order
                    .insert(question_id.into(), order.clone());
            }
        }
        transaction.execute(
            &format!("UPDATE {table} SET payload=json_set(payload,?3,json(?4)) WHERE session_id=?1 AND id=?2"),
            params![session_id, source, pointer, serde_json::to_string(&elicitation)?],
        )?;
        if !matches!(request.owner(), PlanQuestionOwner::Interaction) {
            let plan: crate::plan::PlanRecord =
                super::task::read_record(&transaction, table, source)
                    .map_err(PlanQuestionAnswerError::State)?
                    .ok_or(PlanQuestionAnswerError::TargetMissing)?;
            let active: Option<String> = transaction.query_row(
                "SELECT json_extract(payload,'$.session.active_task') FROM session_record WHERE id=?1", [session_id], |row| row.get(0),
            )?;
            let mut active: Option<crate::task::Task> = active
                .as_deref()
                .map(super::decode)
                .transpose()
                .map_err(PlanQuestionAnswerError::State)?;
            let owns_plan = active.as_ref().is_some_and(|task| matches!(&task.workflow,
                crate::task::WorkflowSnapshot::Plan { plan: current } if current.artifact.id == source));
            if !owns_plan && matches!(request.owner(), PlanQuestionOwner::Plan) {
                return Err(PlanQuestionAnswerError::TargetMissing);
            }
            if let Some(active) = active.as_mut().filter(|_| owns_plan) {
                crate::task::SessionTaskController::apply_plan_answer(active, plan)
                    .map_err(PlanQuestionAnswerError::InvalidAnswer)?;
                transaction.execute(
                    "UPDATE session_record SET payload=json_set(payload,'$.session.active_task',json(?2)) WHERE id=?1",
                    params![session_id, super::encode(active).map_err(PlanQuestionAnswerError::State)?],
                )?;
            }
        }

        // Commit the result with its answer; retry returns this snapshot even after later answers.
        let result = PlanQuestionAnswerCommit::new(request, elicitation)?;
        transaction.execute(
            "INSERT INTO command_receipt(session_id,id,request,result) VALUES(?1,?2,?3,?4)",
            params![
                session_id,
                identity,
                encoded,
                serde_json::to_string(&result)?
            ],
        )?;
        crate::exchange::SessionPublications::retain_plan_answer(
            &transaction,
            session_id,
            request,
            &result,
        )
        .map_err(|error| PlanQuestionAnswerError::State(error.into()))?;
        transaction.commit()?;
        self.publish_admission(session_id)
            .map_err(PlanQuestionAnswerError::State)?;
        Ok(result)
    }

    /// Exercise old JSON fixtures through the same validated transaction used by owner events.
    #[cfg(test)]
    pub(crate) fn answer_question_command(
        &mut self,
        session_id: &str,
        request: &Value,
    ) -> Result<Value> {
        let request = crate::plan::PlanQuestionAnswerRequest::parse(request.clone())?;
        let permit = crate::exchange::QuestionUpdatePermit::for_test(session_id);
        Ok(serde_json::to_value(
            self.commit_question_answer(&permit, &request)?,
        )?)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::session::SessionStore;

    #[test]
    fn replacement_and_clear_retire_questions_atomically() -> Result<()> {
        for clear in [false, true] {
            let (_root, mut store, controller) = crate::task::SessionTaskController::test_goal();
            let question =
                crate::plan::PlanQuestionSet::freeform("Which scope should be implemented?".into())
                    .normalize()?;
            let elicitation = PlanElicitation::new(question.clone());
            let plan: crate::plan::PlanRecord = serde_json::from_value(json!({
                "id":"plan","session_id":"session","request":"Plan the change","state":"awaiting_input",
                "working_path":"","model_revision":1,"user_revision":0,"created_at_ms":0,"updated_at_ms":0,
                "elicitation":elicitation
            }))?;
            let current = controller.current()?.unwrap();
            controller.change(
                &mut store,
                Some(&current.revision),
                crate::task::TaskChange::Install(Box::new(crate::task::Task::plan(
                    plan.clone(),
                    1,
                )?)),
                1,
            )?;
            let mut exchange: crate::exchange::Exchange = serde_json::from_value(json!({
                "id":"exchange","session_id":"session","agent_id":"session:agent:primary","ordinal":1,
                "prompt":"Preserve original prompt","state":"complete","created_at_ms":1,"node_list":[],"attributed_matches_checkpoint":false
            }))?;
            exchange.set_elicitation(Some(elicitation));
            exchange.set_awaiting_input(true);
            store.save_exchange(&exchange)?;
            let active = controller.current()?.unwrap();
            controller.change(
                &mut store,
                Some(&active.revision),
                crate::task::TaskChange::Pause,
                2,
            )?;
            assert!(
                store
                    .session_admission("session")?
                    .snapshot()?
                    .has_elicitation
            );
            let paused = controller.current()?.unwrap();
            let change = || {
                if clear {
                    crate::task::TaskChange::Clear
                } else {
                    crate::task::TaskChange::Install(Box::new(crate::task::Task::goal(
                        "Replacement".into(),
                        Default::default(),
                        3,
                    )))
                }
            };
            store.connection.execute_batch("CREATE TRIGGER reject_retirement BEFORE UPDATE ON session_record BEGIN SELECT RAISE(ABORT,'retirement unavailable'); END;")?;
            assert!(
                controller
                    .change(&mut store, Some(&paused.revision), change(), 3)
                    .is_err()
            );
            assert!(
                store
                    .load_exchange("exchange")?
                    .unwrap()
                    .elicitation()
                    .is_some()
            );
            assert!(store.load_plan("plan")?.unwrap().elicitation.is_some());
            store
                .connection
                .execute_batch("DROP TRIGGER reject_retirement")?;
            controller.change(&mut store, Some(&paused.revision), change(), 3)?;
            assert!(
                !store
                    .session_admission("session")?
                    .snapshot()?
                    .has_elicitation
            );
            let retired = store.load_exchange("exchange")?.unwrap();
            assert_eq!(retired.prompt, "Preserve original prompt");
            assert!(retired.elicitation().is_none() && !retired.awaiting_input());
            assert!(retired.node_list.iter().any(|node| matches!(node,crate::exchange::ExchangeNode::QuestionWithdrawn { question: retired,.. } if retired == &question)));
            assert!(store.load_plan("plan")?.unwrap().elicitation.is_none());
            assert!(
                store
                    .list_plan_lifecycle("session")?
                    .iter()
                    .any(
                        |event| event.kind == crate::plan::PlanLifecycleKind::QuestionWithdrawn
                            && event.question.as_ref() == Some(&question)
                    )
            );
            store.save_exchange(&exchange)?;
            store.save_plan(&plan)?;
            assert!(
                store
                    .load_exchange("exchange")?
                    .unwrap()
                    .elicitation()
                    .is_none()
            );
            assert!(store.load_plan("plan")?.unwrap().elicitation.is_none());
        }
        Ok(())
    }

    #[test]
    fn retiring_a_fully_answered_question_set_records_no_withdrawal() -> Result<()> {
        let (_root, mut store, controller) = crate::task::SessionTaskController::test_goal();
        let question =
            crate::plan::PlanQuestionSet::freeform("Which scope should be implemented?".into())
                .normalize()?;
        let mut elicitation = PlanElicitation::new(question.clone());
        elicitation.answer = question
            .questions
            .iter()
            .map(|item| crate::plan::PlanQuestionAnswer {
                source: crate::session::ResolutionSource::User,
                question_id: item.id.clone(),
                response: crate::plan::PlanQuestionResponse::Other {
                    text: "Only the CLI flag".into(),
                },
            })
            .collect();
        let plan: crate::plan::PlanRecord = serde_json::from_value(json!({
            "id":"plan","session_id":"session","request":"Plan the change","state":"awaiting_input",
            "working_path":"","model_revision":1,"user_revision":0,"created_at_ms":0,"updated_at_ms":0,
            "elicitation":elicitation
        }))?;
        let current = controller.current()?.unwrap();
        controller.change(
            &mut store,
            Some(&current.revision),
            crate::task::TaskChange::Install(Box::new(crate::task::Task::plan(plan, 1)?)),
            1,
        )?;
        let active = controller.current()?.unwrap();
        controller.change(
            &mut store,
            Some(&active.revision),
            crate::task::TaskChange::Clear,
            2,
        )?;
        assert!(
            !store
                .list_plan_lifecycle("session")?
                .iter()
                .any(|event| event.kind == crate::plan::PlanLifecycleKind::QuestionWithdrawn),
            "an answered question set must not be reported as withdrawn"
        );
        Ok(())
    }

    #[test]
    fn answer_receipt_is_atomic_and_preserves_newer_answers() -> Result<()> {
        let root = tempfile::tempdir()?;
        let mut store = SqliteStore::open(root.path())?;
        store.save_session(&super::super::test::session("session", "workspace"))?;
        let mut exchange: crate::exchange::Exchange = serde_json::from_value(json!({
            "id":"exchange","session_id":"session","agent_id":"session:agent:primary","ordinal":1,
            "prompt":"question","kind":"chat","state":"complete","turn":[],"node_list":[],
            "created_at_ms":1,"attributed_matches_checkpoint":false,"awaiting_input":true
        }))?;
        exchange.set_elicitation(Some(PlanElicitation::new(serde_json::from_value(json!({
            "id":"set","questions":[{"id":"question","header":"Choice","question":"Which?",
                "options":[{"label":"First","description":"First choice","recommended":true}],
                "allow_freeform":true}]
        }))?)));
        store.save_exchange(&exchange)?;
        let request = json!({"operation_id":"answer","target":{"owner":"interaction","id":"exchange",
            "set_id":"set","revision":1},"question_id":"question","response":{"kind":"selected","option":"First"}});
        store.connection.execute_batch("CREATE TRIGGER fail_receipt BEFORE INSERT ON command_receipt BEGIN SELECT RAISE(ABORT,'receipt failed'); END;")?;
        assert!(store.answer_question_command("session", &request).is_err());
        assert!(
            store
                .load_exchange("exchange")?
                .unwrap()
                .elicitation()
                .unwrap()
                .answer
                .is_empty()
        );
        store
            .connection
            .execute_batch("DROP TRIGGER fail_receipt")?;
        let result = store.answer_question_command("session", &request)?;
        let mut newer = request.clone();
        newer["operation_id"] = json!("newer");
        newer["answer_order"] = json!(["writer", 2]);
        newer["response"] = json!({"kind":"other","text":"Newer answer"});
        store.answer_question_command("session", &newer)?;
        let mut delayed = request.clone();
        delayed["operation_id"] = json!("delayed");
        delayed["answer_order"] = json!(["writer", 1]);
        let latest = store.answer_question_command("session", &delayed)?;
        assert_eq!(
            latest["active_elicitation"]["elicitation"]["answer"][0]["response"]["text"],
            "Newer answer"
        );
        store.save_exchange(&exchange)?;
        assert_eq!(
            store
                .load_exchange("exchange")?
                .unwrap()
                .elicitation()
                .unwrap()
                .answer
                .len(),
            1
        );
        let mut reopened = SqliteStore::open(root.path())?;
        assert_eq!(
            reopened.answer_question_command("session", &request)?,
            result
        );
        let mut changed = request.clone();
        changed["response"] = json!({"kind":"other","text":"Different"});
        assert!(
            reopened
                .answer_question_command("session", &changed)
                .is_err()
        );
        exchange.set_elicitation(None);
        exchange.set_awaiting_input(false);
        store.save_exchange(&exchange)?;
        assert_eq!(
            reopened.answer_question_command("session", &request)?,
            result
        );
        let mut stale = request;
        stale["operation_id"] = json!("late");
        assert!(reopened.answer_question_command("session", &stale).is_err());
        Ok(())
    }

    #[test]
    fn acceptance_answer_survives_stale_plan_publication_and_reopen() -> Result<()> {
        let (root, mut store, controller) = crate::task::SessionTaskController::test_goal();
        let acceptance = crate::plan::PlanReview::new(
            "review-digest".into(),
            &[
                crate::session::ApprovalMode::Ask,
                crate::session::ApprovalMode::Deny,
            ],
        )?;
        let plan: crate::plan::PlanRecord = serde_json::from_value(json!({
            "id":"plan", "session_id":"session", "request":"Review the change",
            "state":"awaiting_review", "working_path":"", "model_revision":1,
            "submitted_version":1,
            "user_revision":0, "created_at_ms":1, "updated_at_ms":1,
            "review_digest":"review-digest", "review":acceptance
        }))?;
        let previous = controller.current()?.unwrap();
        controller.change(
            &mut store,
            Some(&previous.revision),
            crate::task::TaskChange::Install(Box::new(crate::task::Task::plan(plan.clone(), 1)?)),
            1,
        )?;
        let request = json!({
            "operation_id":"acceptance-answer",
            "target":{"owner":"plan_acceptance","id":"plan","set_id":plan.review.as_ref().unwrap().elicitation.question_set.id,"revision":1},
            "question_id":plan.review.as_ref().unwrap().elicitation.question_set.questions[0].id,
            "response":{"kind":"selected","option":"ask"}
        });
        let receipt = store.answer_question_command("session", &request)?;
        assert_eq!(
            receipt["active_elicitation"]["elicitation"]["answer"]
                .as_array()
                .unwrap()
                .len(),
            1
        );
        store.save_plan(&plan)?;
        let saved = store.load_plan("plan")?.unwrap();
        assert_eq!(
            saved.review.unwrap().approval_mode()?,
            crate::session::ApprovalMode::Ask
        );
        drop(store);
        let mut reopened = SqliteStore::open(root.path())?;
        assert_eq!(
            reopened.answer_question_command("session", &request)?,
            receipt
        );
        assert_eq!(
            reopened
                .load_plan("plan")?
                .unwrap()
                .review
                .unwrap()
                .elicitation
                .answer
                .len(),
            1
        );
        let current = controller.current()?.unwrap();
        let replacement = crate::plan::PlanReview::new(
            "review-digest".into(),
            &[
                crate::session::ApprovalMode::Ask,
                crate::session::ApprovalMode::Deny,
            ],
        )?;
        controller.update(&mut reopened, &current.permit(), 2, |task| {
            let crate::task::WorkflowSnapshot::Plan { plan } = &mut task.workflow else {
                anyhow::bail!("planning task disappeared")
            };
            plan.artifact.review = Some(replacement);
            Ok(())
        })?;
        let mut stale = request.clone();
        stale["operation_id"] = json!("old-review-set-new-answer");
        let stale = crate::plan::PlanQuestionAnswerRequest::parse(stale)?;
        let permit = crate::exchange::QuestionUpdatePermit::for_test("session");
        assert!(matches!(
            reopened.commit_question_answer(&permit, &stale),
            Err(crate::plan::PlanQuestionAnswerError::Changed)
        ));
        assert!(
            reopened
                .load_plan("plan")?
                .unwrap()
                .review
                .unwrap()
                .elicitation
                .answer
                .is_empty()
        );
        assert_eq!(
            reopened.answer_question_command("session", &request)?,
            receipt
        );
        Ok(())
    }
    /// Answer standalone review questions atomically without replacing an unrelated active task.
    #[test]
    fn standalone_review_answers_preserve_unrelated_task_and_retry_receipt() -> Result<()> {
        let (root, mut store, controller) = crate::task::SessionTaskController::test_goal();
        let unrelated = serde_json::to_value(controller.current()?.unwrap())?;
        let review = crate::plan::PlanReview::new(
            "standalone-digest".into(),
            &[
                crate::session::ApprovalMode::Ask,
                crate::session::ApprovalMode::Deny,
            ],
        )?;
        let plan: crate::plan::PlanRecord = serde_json::from_value(json!({
            "id":"standalone-review","session_id":"session","request":"Review saved work",
            "state":"awaiting_review","working_path":"","model_revision":1,"submitted_version":1,
            "user_revision":0,"created_at_ms":1,"updated_at_ms":1,
            "review_digest":"standalone-digest","review":review
        }))?;
        store.save_plan(&plan)?;
        let elicitation = &plan.review.as_ref().unwrap().elicitation;
        let request = json!({
            "operation_id":"standalone-answer","target":{"owner":"plan_review","id":plan.id,
                "set_id":elicitation.question_set.id,"revision":elicitation.revision},
            "question_id":elicitation.question_set.questions[0].id,
            "response":{"kind":"selected","option":"ask"},"answer_order":["reviewer",1]
        });
        store.connection.execute_batch("CREATE TRIGGER reject_standalone_receipt BEFORE INSERT ON command_receipt BEGIN SELECT RAISE(ABORT,'receipt failed'); END;")?;
        assert!(store.answer_question_command("session", &request).is_err());
        assert!(
            store
                .load_plan(&plan.id)?
                .unwrap()
                .review
                .unwrap()
                .elicitation
                .answer
                .is_empty()
        );
        assert_eq!(
            serde_json::to_value(controller.current()?.unwrap())?,
            unrelated
        );
        store
            .connection
            .execute_batch("DROP TRIGGER reject_standalone_receipt")?;
        let receipt = store.answer_question_command("session", &request)?;
        assert_eq!(
            store
                .load_plan(&plan.id)?
                .unwrap()
                .review
                .unwrap()
                .approval_mode()?,
            crate::session::ApprovalMode::Ask
        );
        assert_eq!(
            serde_json::to_value(controller.current()?.unwrap())?,
            unrelated
        );
        let mut newer = request.clone();
        newer["operation_id"] = json!("standalone-newer-answer");
        newer["answer_order"] = json!(["reviewer", 2]);
        newer["response"] = json!({"kind":"selected","option":"deny"});
        store.answer_question_command("session", &newer)?;
        drop(store);
        let mut reopened = SqliteStore::open(root.path())?;
        assert_eq!(
            reopened.answer_question_command("session", &request)?,
            receipt
        );
        assert_eq!(
            reopened
                .load_plan(&plan.id)?
                .unwrap()
                .review
                .unwrap()
                .approval_mode()?,
            crate::session::ApprovalMode::Deny
        );
        assert_eq!(
            serde_json::to_value(controller.current()?.unwrap())?,
            unrelated
        );
        Ok(())
    }
}
