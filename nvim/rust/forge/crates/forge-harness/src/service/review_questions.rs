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

#[cfg(test)]
mod tests {
    use super::*;

    /// Report failed child initialization after accepted updates drain even while its owner retains a sender.
    #[tokio::test]
    async fn failed_review_answer_settles_with_retained_publication_producer() -> Result<()> {
        let (retained, updates) = crate::broker::publication::channel();
        retained
            .send_wait(crate::broker::publication::SessionUpdate::Diagnostic {
                diagnostic: crate::provider::ProviderDiagnostic {
                    kind: forge_provider_api::ProviderFailureKind::Rejected,
                    message: forge_provider_api::SanitizedMessage::new("child open rejected")?,
                    fatal: true,
                },
            })
            .await?;
        let mut flushed = std::pin::pin!(retained.flush());
        assert!(futures_util::poll!(flushed.as_mut()).is_pending());
        let failure =
            crate::provider::ProviderError::Failure(forge_provider_api::ProviderFailure::new(
                forge_provider_api::ProviderOperation::SessionOpen,
                forge_provider_api::ProviderFailureKind::Rejected,
            ));
        let error = tokio::time::timeout(
            Duration::from_secs(1),
            observe_answer(async { Err(failure.into()) }, updates),
        )
        .await
        .context("failed child answer waited for retained producer destruction")?
        .err()
        .context("failed child initialization became a successful answer")?;
        assert!(
            matches!(error.downcast_ref::<crate::provider::ProviderError>(),
                Some(crate::provider::ProviderError::Failure(failure))
                    if failure.operation() == forge_provider_api::ProviderOperation::SessionOpen
                        && failure.kind() == forge_provider_api::ProviderFailureKind::Rejected
            )
        );
        flushed.await?;
        assert_eq!(
            retained.flush().await.unwrap_err().kind(),
            std::io::ErrorKind::BrokenPipe
        );
        Ok(())
    }
}
