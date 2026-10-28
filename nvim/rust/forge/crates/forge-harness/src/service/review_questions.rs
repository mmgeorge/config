use super::*;
use crate::plan::review_answer::ReviewAnswerScope;

pub(super) async fn answer(
    registry: Arc<SessionControllerRegistry>,
    source: Arc<SessionController>,
    session_id: String,
    request: Request,
    message_sink: &MessageSender,
) -> Result<()> {
    let (document, questions, selection) = source
        .broker
        .lock()
        .await
        .prepare_review_questions(&request.params)?;
    if !questions.is_empty() {
        let (child, gate) = start_session_fork(
            Arc::clone(&registry),
            session_id,
            json!({"name":"Plan Review answers"}),
            None,
            message_sink,
        )
        .await?;
        gate.wait().await?;
        let _execution = child
            .coordinator
            .execution
            .clone()
            .acquire_owned()
            .await
            .context("review answer session is closed")?;
        let mut broker = child.broker.lock().await;
        for question in questions {
            let scope =
                ReviewAnswerScope::new(Arc::clone(&source.plan_review), document.clone(), question);
            broker.run_review_question(scope, &selection).await?;
        }
    }
    message_sink
        .send_response(Response::success(
            request.id,
            json!({"annotation":source.plan_review.annotations(&document)?}),
        )?)
        .await?;
    Ok(())
}
