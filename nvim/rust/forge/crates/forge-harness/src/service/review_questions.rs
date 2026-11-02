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
    let mut pending = tokio::task::JoinSet::new();
    let request_id = request.id;
    for question in questions {
        let registry = Arc::clone(&registry);
        let source = Arc::clone(&source);
        let session_id = session_id.clone();
        let document = document.clone();
        let selection = selection.clone();
        let message_sink = message_sink.clone();
        pending.spawn(async move {
            let (child, gate) = start_session_fork(
                registry,
                session_id,
                json!({"name":"Plan Review answer"}),
                None,
                crate::session::SessionPurpose::PlanReviewAnswer,
                &message_sink,
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
            let scope = ReviewAnswerScope::new(Arc::clone(&source.plan_review), document, question);
            let (sink, mut updates) = crate::broker::publication::channel();
            let _generation = child.provider_lane.read().await;
            let catalog_job = if selection.model.is_some() {
                Some(
                    child
                        .broker
                        .lock()
                        .await
                        .prepare_configuration_catalog_for_mode(
                            crate::provider::PromptMode::PlanQuestion,
                        )?,
                )
            } else {
                None
            };
            let catalog_result = match catalog_job {
                Some(job) => Some(job.run().await?),
                None => None,
            };
            let dispatch = {
                let mut broker = child.broker.lock().await;
                let catalog = match catalog_result {
                    Some(result) => broker.commit_configuration_catalog(result)?,
                    None => Vec::new(),
                };
                let prompt = broker.prepare_review_question(
                    &scope,
                    &selection,
                    &catalog,
                    Some((&message_sink, request_id)),
                )?;
                broker.begin_review_question_dispatch(prompt, scope.question.body.clone(), sink)?
            };
            drop(_generation);
            let forwarder = tokio::spawn(async move {
                while updates.recv().await?.is_some() {}
                Ok::<(), anyhow::Error>(())
            });
            let result = drive_owned_foreground(&child, dispatch, &message_sink).await;
            forwarder
                .await
                .context("review publication worker failed")??;
            let result = result?;
            let value = result.response.result().ok_or_else(|| {
                anyhow::anyhow!(
                    "review question dispatch failed: {:?}",
                    result.response.error()
                )
            })?;
            child
                .broker
                .lock()
                .await
                .finish_review_question(scope, value)
        });
    }
    let mut failures = Vec::new();
    while let Some(result) = pending.join_next().await {
        match result {
            Ok(Ok(())) => {}
            Ok(Err(error)) => failures.push(format!("{error:#}")),
            Err(error) => failures.push(format!("review answer task failed: {error}")),
        }
    }
    ensure!(failures.is_empty(), "{}", failures.join("\n"));
    message_sink
        .send_response(Response::success(
            request.id,
            json!({"annotation":source.plan_review.annotations(&document)?}),
        )?)
        .await?;
    Ok(())
}
