use super::question_event_test::{context, creation, fixture};
use super::*;

/// Commit a decision through production owner intake without running a native provider turn.
async fn answer(
    controller: &SessionController,
    question: &crate::question::QuestionId,
    message: &str,
) -> Result<()> {
    controller
        .exchange_owner
        .question_events()
        .change(crate::exchange::QuestionChange::user(
            crate::question::QuestionCommand {
                operation_id: message.into(),
                question_id: question.clone(),
                revision: 1,
                action: crate::question::QuestionAction::Answer {
                    answer: crate::question::QuestionAnswer::Other {
                        text: "Workspace".into(),
                    },
                },
            },
        )?)
        .await?;
    Ok(())
}

/// Observe the actual durable admission rather than treating a notification as proof of dispatch.
async fn await_input(
    controller: &SessionController,
    message: &str,
) -> Result<crate::input::InputUpdate> {
    tokio::time::timeout(Duration::from_secs(3), async {
        loop {
            if let Some(receipt) = controller
                .coordinator
                .submissions
                .admission_receipt(&format!("question:{message}"))?
                && controller.questions.outbox()?.is_empty()
            {
                break Ok::<_, anyhow::Error>(receipt);
            }
            tokio::task::yield_now().await;
        }
    })
    .await?
}

/// A fast answer cannot disappear when its tool observer abandons the wait before reading it.
#[tokio::test]
async fn abandoned_wait_recovers_a_previously_suppressed_answer_once() -> Result<()> {
    let (_root, controller, exchange) = fixture()?;
    let wait = controller
        .exchange_owner
        .question_events()
        .begin_wait(creation(
            &context(&controller, &exchange),
            &exchange,
            "abandoned-wait",
        )?)
        .await?;
    let question = wait.question_ids()[0].clone();
    answer(&controller, &question, "answer-before-abandonment").await?;
    assert!(controller.questions.outbox()?.is_empty());
    assert!(
        controller
            .coordinator
            .submissions
            .snapshot()?
            .messages
            .is_empty()
    );
    drop(wait);
    let receipt = await_input(&controller, "answer-before-abandonment").await?;
    assert!(receipt.error_code.is_none());
    assert_eq!(
        controller
            .coordinator
            .submissions
            .snapshot()?
            .messages
            .len(),
        1
    );
    controller.exchange_owner.reconcile_questions(None).await?;
    assert_eq!(
        controller
            .coordinator
            .submissions
            .snapshot()?
            .messages
            .len(),
        1
    );
    controller.finish_exchange_owner().await?;
    Ok(())
}

/// Owner retirement must distinguish already observed answers from corrections committed afterward.
#[tokio::test]
async fn observed_wait_recovers_only_a_later_unobserved_correction() -> Result<()> {
    let (_root, controller, exchange) = fixture()?;
    let wait = controller
        .exchange_owner
        .question_events()
        .begin_wait(creation(
            &context(&controller, &exchange),
            &exchange,
            "observed-wait",
        )?)
        .await?;
    let question = wait.question_ids()[0].clone();
    answer(&controller, &question, "observed-answer").await?;
    let observed = wait.answers().await?;
    assert_eq!(observed.len(), 1);
    // Simulate a committed correction racing the next retirement step, while its old destination remains registered.
    controller
        .questions
        .apply(crate::question::QuestionCommand {
            operation_id: "later-correction".into(),
            question_id: question,
            revision: 1,
            action: crate::question::QuestionAction::Correct {
                text: "Only the current project".into(),
            },
        })?;
    let receipt = await_input(&controller, "later-correction").await?;
    assert!(receipt.error_code.is_none());
    assert!(
        controller
            .coordinator
            .submissions
            .admission_receipt("question:observed-answer")?
            .is_none()
    );
    let queue = controller.coordinator.submissions.snapshot()?;
    assert_eq!(queue.messages.len(), 1);
    assert!(matches!(
        &queue.messages[0].purpose,
        crate::input::InputPurpose::Question {
            message_kind: crate::question::QuestionMessageKind::Correction,
            ..
        }
    ));
    controller.finish_exchange_owner().await?;
    Ok(())
}

/// Persistent storage rejection releases the live observer before recovery while retaining the exact retirement fact.
#[tokio::test]
async fn wait_retirement_retries_observation_storage_without_queuing_an_answer_copy() -> Result<()>
{
    let (_root, controller, exchange) = fixture()?;
    controller.trace.configure(true)?;
    let wait = controller
        .exchange_owner
        .question_events()
        .begin_wait(creation(
            &context(&controller, &exchange),
            &exchange,
            "retirement-retry",
        )?)
        .await?;
    let question = wait.question_ids()[0].clone();
    answer(&controller, &question, "answer-with-retirement-failure").await?;
    let connection = rusqlite::Connection::open(controller.data_root.join("harness.sqlite3"))?;
    connection.busy_timeout(Duration::from_secs(5))?;
    connection.execute_batch("CREATE TRIGGER reject_wait_observation BEFORE UPDATE ON session_question_state WHEN json_array_length(json_extract(NEW.payload,'$.observed_tool_messages')) > json_array_length(json_extract(OLD.payload,'$.observed_tool_messages')) BEGIN SELECT RAISE(ABORT,'injected wait retirement failure'); END;")?;
    let outcome = tokio::time::timeout(Duration::from_secs(3), wait.answers())
        .await
        .context("persistent retirement failure left the response observer blocked")?;
    assert!(matches!(
        outcome,
        Err(crate::question::QuestionWaitError::StateUnavailable)
    ));
    let failed_payload: String = connection.query_row(
        "SELECT payload FROM session_question_state WHERE session_id=?1",
        [controller.observer.session_id()],
        |row| row.get(0),
    )?;
    let failed_state: Value = serde_json::from_str(&failed_payload)?;
    assert!(
        failed_state["wait_results"]
            .as_object()
            .is_some_and(|results| results.is_empty())
    );
    let path = controller
        .trace
        .session_status(controller.observer.session_id())
        .path;
    tokio::time::timeout(Duration::from_secs(3), async {
        loop {
            let trace = controller.trace.clone();
            let path = path.clone();
            let contents = tokio::task::spawn_blocking(move || -> Result<String> {
                trace.flush()?;
                Ok(std::fs::read_to_string(path).unwrap_or_default())
            })
            .await??;
            if contents.contains("exchange.question_wait.retirement_failed") {
                break Ok::<_, anyhow::Error>(());
            }
            tokio::task::yield_now().await;
        }
    })
    .await??;
    assert!(
        controller
            .coordinator
            .submissions
            .snapshot()?
            .messages
            .is_empty()
    );
    connection.execute_batch("DROP TRIGGER reject_wait_observation;")?;
    tokio::time::timeout(Duration::from_secs(3), async {
        loop {
            let payload: String = connection.query_row(
                "SELECT payload FROM session_question_state WHERE session_id=?1",
                [controller.observer.session_id()],
                |row| row.get(0),
            )?;
            let state: Value = serde_json::from_str(&payload)?;
            if state["wait_results"]
                .as_object()
                .is_some_and(|results| !results.is_empty())
                && state["observed_tool_messages"]
                    .as_array()
                    .is_some_and(|messages| {
                        messages
                            .iter()
                            .any(|message| message == "answer-with-retirement-failure")
                    })
            {
                break Ok::<_, anyhow::Error>(());
            }
            tokio::task::yield_now().await;
        }
    })
    .await??;
    controller.exchange_owner.reconcile_questions(None).await?;
    assert!(
        controller
            .coordinator
            .submissions
            .snapshot()?
            .messages
            .is_empty()
    );
    assert!(controller.questions.outbox()?.is_empty());
    controller.finish_exchange_owner().await?;
    Ok(())
}

/// Closing the owner must release a still-live guard without requiring the caller to poll or drop it.
#[tokio::test]
async fn owner_close_seals_live_question_waits_and_preserves_answer_history() -> Result<()> {
    let (_root, controller, exchange) = fixture()?;
    let wait = controller
        .exchange_owner
        .question_events()
        .begin_wait(creation(
            &context(&controller, &exchange),
            &exchange,
            "closed-wait",
        )?)
        .await?;
    let question = wait.question_ids()[0].clone();
    answer(&controller, &question, "answer-before-close").await?;
    tokio::time::timeout(Duration::from_secs(3), controller.finish_exchange_owner()).await??;
    assert!(matches!(
        wait.answers().await,
        Err(crate::question::QuestionWaitError::OwnerClosed)
    ));
    assert!(
        matches!(&controller.questions.snapshot()?.questions[0].resolution, crate::question::QuestionResolution::Answered { message_id, .. } if message_id == "answer-before-close")
    );
    assert!(
        controller
            .coordinator
            .submissions
            .snapshot()?
            .messages
            .is_empty()
    );
    assert!(controller.questions.outbox()?.is_empty());
    Ok(())
}

/// Capacity rejection must precede question publication, and shutdown must retire a full resource set.
#[tokio::test]
async fn question_wait_capacity_rejects_before_publication_and_close_retires_live_guards()
-> Result<()> {
    let (_root, controller, exchange) = fixture()?;
    let context = context(&controller, &exchange);
    let events = controller.exchange_owner.question_events();
    let mut waits = Vec::new();
    for index in 0..forge_protocol::MAX_ACTIVE_REQUESTS {
        let creation = crate::exchange::QuestionCreation::new(
            crate::exchange::QuestionOperationId::new(format!("wait-{index}"))?,
            exchange.as_str().into(),
            None,
            vec![crate::question::QuestionDraft {
                header: "Scope".into(),
                question: "Which scope?".into(),
                requirement: crate::question::QuestionRequirement::Optional,
                options: Vec::new(),
            }],
            crate::exchange::QuestionCreationOrigin::Discussion,
            crate::tools::TaskAuthority::capture(&context).unwrap(),
        )?;
        waits.push(events.begin_wait(creation).await?);
    }
    let rejected = events
        .begin_wait(creation(&context, &exchange, "over-capacity")?)
        .await;
    assert!(matches!(
        rejected,
        Err(crate::exchange::QuestionEventError::WaitCapacity)
    ));
    assert_eq!(
        controller.questions.snapshot()?.questions.len(),
        forge_protocol::MAX_ACTIVE_REQUESTS
    );
    tokio::time::timeout(Duration::from_secs(3), controller.finish_exchange_owner()).await??;
    for wait in waits {
        assert!(matches!(
            wait.answers().await,
            Err(crate::question::QuestionWaitError::OwnerClosed)
        ));
    }
    Ok(())
}

/// Independent observers of one question must not turn a later abandoned guard into duplicate input.
#[tokio::test]
async fn observed_shared_wait_prevents_an_abandoned_guard_from_queuing_duplicate_answer()
-> Result<()> {
    let (_root, controller, exchange) = fixture()?;
    controller.trace.configure(true)?;
    let context = context(&controller, &exchange);
    let creation = || -> Result<crate::exchange::QuestionCreation> {
        Ok(crate::exchange::QuestionCreation::new(
            crate::exchange::QuestionOperationId::new("shared-wait".into())?,
            exchange.as_str().into(),
            None,
            vec![crate::question::QuestionDraft {
                header: "Scope".into(),
                question: "Which scope?".into(),
                requirement: crate::question::QuestionRequirement::Required,
                options: Vec::new(),
            }],
            crate::exchange::QuestionCreationOrigin::Discussion,
            crate::tools::TaskAuthority::capture(&context).unwrap(),
        )?)
    };
    let events = controller.exchange_owner.question_events();
    let first = events.begin_wait(creation()?).await?;
    let second = events.begin_wait(creation()?).await?;
    assert_eq!(first.question_ids(), second.question_ids());
    answer(&controller, &first.question_ids()[0], "shared-answer").await?;
    first.answers().await?;
    drop(second);
    let path = controller
        .trace
        .session_status(controller.observer.session_id())
        .path;
    tokio::time::timeout(Duration::from_secs(3), async {
        loop {
            let trace = controller.trace.clone();
            let path = path.clone();
            let contents = tokio::task::spawn_blocking(move || -> Result<String> {
                trace.flush()?;
                Ok(std::fs::read_to_string(path).unwrap_or_default())
            })
            .await??;
            if contents
                .lines()
                .filter(|line| line.contains("exchange.question_wait.retired"))
                .count()
                == 2
            {
                break Ok::<_, anyhow::Error>(());
            }
            tokio::task::yield_now().await;
        }
    })
    .await??;
    assert!(controller.questions.outbox()?.is_empty());
    assert!(
        controller
            .coordinator
            .submissions
            .snapshot()?
            .messages
            .is_empty()
    );
    controller.finish_exchange_owner().await?;
    Ok(())
}

/// Retirement of an old task's wait cannot restore delivery work that task clearing already revoked.
#[tokio::test]
async fn abandoned_wait_does_not_restore_an_answer_after_its_task_is_cleared() -> Result<()> {
    let (_root, controller, exchange) = fixture()?;
    let task = controller
        .task_controller
        .change(
            &mut SqliteStore::open(&controller.data_root)?,
            None,
            crate::task::TaskChange::Install(Box::new(crate::task::Task::goal(
                "Question task".into(),
                Default::default(),
                1,
            ))),
            1,
        )?
        .unwrap();
    let context = context(&controller, &exchange);
    let requested = crate::exchange::QuestionCreation::new(
        crate::exchange::QuestionOperationId::new("cleared-task-wait".into())?,
        exchange.as_str().into(),
        Some(task.clone()),
        vec![crate::question::QuestionDraft {
            header: "Scope".into(),
            question: "Which scope?".into(),
            requirement: crate::question::QuestionRequirement::Required,
            options: Vec::new(),
        }],
        crate::exchange::QuestionCreationOrigin::Workflow,
        crate::tools::TaskAuthority::capture(&context).unwrap(),
    )?;
    let wait = controller
        .exchange_owner
        .question_events()
        .begin_wait(requested)
        .await?;
    let question = wait.question_ids()[0].clone();
    answer(&controller, &question, "answer-before-task-clear").await?;
    controller.task_controller.change(
        &mut SqliteStore::open(&controller.data_root)?,
        Some(&task.revision),
        crate::task::TaskChange::Clear,
        2,
    )?;
    assert!(!controller.questions.delivery_is_current(&question)?);
    drop(wait);
    controller.exchange_owner.reconcile_questions(None).await?;
    assert!(controller.questions.outbox()?.is_empty());
    assert!(
        controller
            .coordinator
            .submissions
            .snapshot()?
            .messages
            .is_empty()
    );
    assert!(
        controller
            .coordinator
            .submissions
            .admission_receipt("question:answer-before-task-clear")?
            .is_none()
    );
    controller.finish_exchange_owner().await?;
    Ok(())
}
