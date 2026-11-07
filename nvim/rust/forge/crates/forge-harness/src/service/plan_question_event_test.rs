use super::*;

/// Install normalized plan questions without depending on a provider's question protocol.
async fn fixture() -> Result<(tempfile::TempDir, Arc<SessionController>, Value)> {
    let (root, controller, _) = super::question_event_test::fixture().await?;
    let session = controller.observer.session_id();
    let questions =
        crate::plan::PlanQuestionSet::freeform("Which scope should be implemented?".into())
            .normalize()?;
    let elicitation = crate::plan::PlanElicitation::new(questions);
    let request = json!({
        "operation_id":"plan-answer",
        "target":{"owner":"plan","id":"owner-plan","set_id":elicitation.question_set.id,"revision":elicitation.revision},
        "question_id":elicitation.question_set.questions[0].id,
        "response":{"kind":"other","text":"Keep the current public API"},
        "answer_order":["view",1],
    });
    let plan: crate::plan::PlanRecord = serde_json::from_value(json!({
        "id":"owner-plan","session_id":session,"request":"Clarify the scope","state":"awaiting_input",
        "working_path":"","model_revision":1,"user_revision":0,"created_at_ms":1,"updated_at_ms":1,
        "elicitation":elicitation,
    }))?;
    let mut store = SqliteStore::open(&controller.data_root)?;
    let current = controller.task_controller.current()?;
    controller.task_controller.change(
        &mut store,
        current.as_ref().map(|task| &task.revision),
        crate::task::TaskChange::Install(Box::new(crate::task::Task::plan(plan, 1)?)),
        1,
    )?;
    Ok((root, controller, request))
}

/// Answers must commit headlessly while compatibility execution/control/broker lanes are occupied.
#[tokio::test]
async fn plan_answers_commit_under_owner_and_replay_original_publication() -> Result<()> {
    let (_root, controller, request) = fixture().await?;
    let session = controller.observer.session_id();
    let publications = crate::exchange::SessionPublications::open(&controller.data_root, session)?;
    let execution = controller.coordinator.execution.acquire().await?;
    let control = controller
        .coordinator
        .control
        .clone()
        .acquire_owned()
        .await?;
    let broker = controller.broker.lock().await;
    let events = controller.exchange_owner.question_events();
    let first = tokio::time::timeout(
        crate::hang_guard::HANG_GUARD,
        events.answer_plan(crate::plan::PlanQuestionAnswerRequest::parse(
            request.clone(),
        )?),
    )
    .await??;
    let first = serde_json::to_value(first)?;
    let mut newer = request.clone();
    newer["operation_id"] = json!("newer-plan-answer");
    newer["answer_order"] = json!(["view", 2]);
    newer["response"] = json!({"kind":"other","text":"Extend the public API"});
    events
        .answer_plan(crate::plan::PlanQuestionAnswerRequest::parse(newer)?)
        .await?;
    let replay = events
        .answer_plan(crate::plan::PlanQuestionAnswerRequest::parse(
            request.clone(),
        )?)
        .await?;
    assert_eq!(serde_json::to_value(replay)?, first);
    let plan = SqliteStore::read_only(&controller.data_root)?
        .load_plan("owner-plan")?
        .unwrap();
    let answers = plan.elicitation.unwrap();
    assert_eq!(answers.answer_revision, 2);
    assert_eq!(
        serde_json::to_value(&answers.answer[0].response)?,
        json!({"kind":"other","text":"Extend the public API"})
    );
    let updates = publications.after(None, std::num::NonZeroU16::new(64).unwrap())?;
    assert_eq!(
        updates
            .iter()
            .filter(|event| event.event().event == "question_updated")
            .count(),
        2
    );
    let mut conflict = request;
    conflict["response"] = json!({"kind":"other","text":"Different retry content"});
    assert!(matches!(
        events
            .answer_plan(crate::plan::PlanQuestionAnswerRequest::parse(conflict)?)
            .await,
        Err(crate::exchange::QuestionEventError::PlanAnswer(
            crate::plan::PlanQuestionAnswerError::IdentityConflict
        ))
    ));
    drop(broker);
    drop(control);
    drop(execution);
    controller.finish_exchange_owner().await?;
    Ok(())
}

/// An admitted event survives a dropped waiter; replay must not repeat its answer or notification.
#[tokio::test]
async fn dropped_plan_answer_waiter_retains_commit_and_one_publication() -> Result<()> {
    use std::{future::Future, task::Poll};
    let (_root, controller, request) = fixture().await?;
    let publications = crate::exchange::SessionPublications::open(
        &controller.data_root,
        controller.observer.session_id(),
    )?;
    let events = controller.exchange_owner.question_events();
    let mut waiting = Box::pin(
        events.answer_plan(crate::plan::PlanQuestionAnswerRequest::parse(
            request.clone(),
        )?),
    );
    std::future::poll_fn(|context| {
        assert!(matches!(waiting.as_mut().poll(context), Poll::Pending));
        Poll::Ready(())
    })
    .await;
    drop(waiting);
    events
        .answer_plan(crate::plan::PlanQuestionAnswerRequest::parse(request)?)
        .await?;
    let plan = SqliteStore::read_only(&controller.data_root)?
        .load_plan("owner-plan")?
        .unwrap();
    assert_eq!(plan.elicitation.unwrap().answer_revision, 1);
    assert_eq!(
        publications
            .after(None, std::num::NonZeroU16::new(64).unwrap())?
            .iter()
            .filter(|event| event.event().event == "question_updated")
            .count(),
        1
    );
    controller.finish_exchange_owner().await?;
    Ok(())
}

/// A late answer cannot recreate questions after the selected planning task has been cleared.
#[tokio::test]
async fn plan_answer_event_rejects_replaced_task_without_restoring_questions() -> Result<()> {
    let (_root, controller, request) = fixture().await?;
    let mut store = SqliteStore::open(&controller.data_root)?;
    let current = controller.task_controller.current()?.unwrap();
    controller.task_controller.change(
        &mut store,
        Some(&current.revision),
        crate::task::TaskChange::Clear,
        2,
    )?;
    let error = controller
        .exchange_owner
        .question_events()
        .answer_plan(crate::plan::PlanQuestionAnswerRequest::parse(request)?)
        .await
        .err()
        .context("stale answer unexpectedly committed")?;
    assert_eq!(error.code(), "question_target_missing");
    assert!(controller.task_controller.current()?.is_none());
    assert!(
        store
            .load_plan("owner-plan")?
            .unwrap()
            .elicitation
            .is_none()
    );
    controller.finish_exchange_owner().await?;
    Ok(())
}

/// Failure retaining the notification must roll back the answer, task update, and receipt together.
#[tokio::test]
async fn plan_answer_publication_failure_rolls_back_decision_and_receipt() -> Result<()> {
    let (_root, controller, request) = fixture().await?;
    let session = controller.observer.session_id();
    let database =
        rusqlite::Connection::open(crate::storage::state::database(&controller.data_root))?;
    database.execute_batch("CREATE TRIGGER fail_plan_answer_publication BEFORE INSERT ON owner_publication WHEN json_extract(NEW.event,'$.event')='question_updated' BEGIN SELECT RAISE(ABORT,'publication failed'); END;")?;
    let events = controller.exchange_owner.question_events();
    let error = events
        .answer_plan(crate::plan::PlanQuestionAnswerRequest::parse(
            request.clone(),
        )?)
        .await
        .err()
        .context("answer committed without its required notification")?;
    assert_eq!(error.code(), "question_answer_state_unavailable");
    let plan = SqliteStore::read_only(&controller.data_root)?
        .load_plan("owner-plan")?
        .unwrap();
    assert!(plan.elicitation.unwrap().answer.is_empty());
    let current = controller.task_controller.current()?.unwrap();
    let crate::task::WorkflowSnapshot::Plan { plan } = &current.workflow else {
        anyhow::bail!("planning task was displaced")
    };
    assert!(
        plan.artifact
            .elicitation
            .as_ref()
            .unwrap()
            .answer
            .is_empty()
    );
    let receipt_count: i64 = database.query_row(
        "SELECT COUNT(*) FROM command_receipt WHERE session_id=?1 AND id='plan-answer'",
        [session],
        |row| row.get(0),
    )?;
    assert_eq!(receipt_count, 0);
    database.execute_batch("DROP TRIGGER fail_plan_answer_publication")?;
    events
        .answer_plan(crate::plan::PlanQuestionAnswerRequest::parse(request)?)
        .await?;
    let plan = SqliteStore::read_only(&controller.data_root)?
        .load_plan("owner-plan")?
        .unwrap();
    assert_eq!(plan.elicitation.unwrap().answer_revision, 1);
    controller.finish_exchange_owner().await?;
    let retained = crate::exchange::SessionPublications::open(&controller.data_root, session)?;
    assert_eq!(
        retained
            .after(None, std::num::NonZeroU16::new(64).unwrap())?
            .iter()
            .filter(|event| event.event().event == "question_updated")
            .count(),
        1
    );
    Ok(())
}

/// Old answer receipts acquire their missing outbox entry without repeating the stored decision.
#[tokio::test]
async fn legacy_plan_answer_receipt_backfills_publication_without_reapplying_answer() -> Result<()>
{
    let (_root, controller, request) = fixture().await?;
    let events = controller.exchange_owner.question_events();
    let original = events
        .answer_plan(crate::plan::PlanQuestionAnswerRequest::parse(
            request.clone(),
        )?)
        .await?;
    let database =
        rusqlite::Connection::open(crate::storage::state::database(&controller.data_root))?;
    database.execute("DELETE FROM owner_publication_origin WHERE session_id=?1 AND json_extract(origin,'$.kind')='plan_question_answer'", [controller.observer.session_id()])?;
    database.execute("DELETE FROM owner_publication WHERE session_id=?1 AND json_extract(event,'$.event')='question_updated'", [controller.observer.session_id()])?;
    let recovered = events
        .answer_plan(crate::plan::PlanQuestionAnswerRequest::parse(request)?)
        .await?;
    assert_eq!(
        serde_json::to_value(recovered)?,
        serde_json::to_value(original)?
    );
    let plan = SqliteStore::read_only(&controller.data_root)?
        .load_plan("owner-plan")?
        .unwrap();
    assert_eq!(plan.elicitation.unwrap().answer_revision, 1);
    let count: i64 = database.query_row("SELECT COUNT(*) FROM owner_publication WHERE session_id=?1 AND json_extract(event,'$.event')='question_updated'", [controller.observer.session_id()], |row| row.get(0))?;
    assert_eq!(count, 1);
    controller.finish_exchange_owner().await?;
    Ok(())
}
