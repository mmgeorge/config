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
            let scope = ReviewAnswerScope::new(Arc::clone(&source.plan_review), document, question);
            let (sink, updates) = crate::broker::publication::channel();
            let _generation = child.provider_lane.read().await;
            let catalog_job = if selection.model.is_some() {
                Some(child.broker.lock().await.prepare_configuration_catalog()?)
            } else {
                None
            };
            let catalog_result = match catalog_job {
                Some(job) => Some(job.run().await?),
                None => None,
            };
            drop(_generation);
            let result = observe_answer(
                child.exchange_owner.review_question(
                    scope,
                    selection,
                    catalog_result,
                    sink,
                    message_sink.clone(),
                    request_id,
                ),
                updates,
            )
            .await?;
            if let Some(error) = result.response.error() {
                anyhow::bail!(
                    "review question dispatch failed ({}): {}",
                    error.code,
                    error.message
                );
            }
            Ok::<(), anyhow::Error>(())
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

/// Drain an answer's accepted publications at completion without awaiting producer destruction.
async fn observe_answer(
    answer: impl std::future::Future<Output = Result<crate::broker::DispatchResult>>,
    mut updates: crate::broker::publication::SessionUpdateStream,
) -> Result<crate::broker::DispatchResult> {
    let (finished, mut completion) = tokio::sync::oneshot::channel();
    let forwarder = tokio::spawn(async move {
        loop {
            tokio::select! {
                _ = &mut completion => {
                    updates.close();
                    break;
                }
                update = updates.recv() => {
                    if update?.is_none() { return Ok::<(), anyhow::Error>(()); }
                }
            }
        }
        while updates.recv().await?.is_some() {}
        Ok(())
    });
    let result = answer.await;
    let _ = finished.send(());
    let forwarded = forwarder
        .await
        .context("review publication worker failed")
        .and_then(|result| result);
    match (result, forwarded) {
        (Ok(result), Ok(())) => Ok(result),
        (Err(error), Ok(())) | (Ok(_), Err(error)) => Err(error),
        (Err(error), Err(publication)) => {
            Err(error.context(format!("review publication also failed: {publication:#}")))
        }
    }
}
