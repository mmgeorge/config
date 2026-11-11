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
    async {
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
    }
    .await
}

/// An answer moved to the outbox by a later abandonment is admitted without another request.
///
/// The owner reconciles question input once after the answer, while the wait still owns it.
/// Only the committed question change from the later abandonment can then admit the answer.
#[tokio::test]
async fn late_abandonment_admits_its_answer_without_an_unrelated_request() -> Result<()> {
    let (_root, controller, exchange) = fixture().await?;
    let wait = controller
        .exchange_owner
        .question_events()
        .begin_wait(creation(
            &context(&controller, &exchange),
            &exchange,
            "late-abandoned-wait",
        )?)
        .await?;
    let question = wait.question_ids()[0].clone();
    answer(&controller, &question, "answer-before-late-abandonment").await?;
    controller.exchange_owner.reconcile_questions(None).await?;
    assert!(controller.questions.outbox()?.is_empty());
    drop(wait);
    let receipt = await_input(&controller, "answer-before-late-abandonment").await?;
    assert!(receipt.error_code.is_none());
    controller.finish_exchange_owner().await?;
    Ok(())
}

/// A fast answer cannot disappear when its tool observer abandons the wait before reading it.
#[tokio::test]
async fn abandoned_wait_recovers_a_previously_suppressed_answer_once() -> Result<()> {
    let (_root, controller, exchange) = fixture().await?;
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
