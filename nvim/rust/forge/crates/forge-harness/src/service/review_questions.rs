use super::*;
use crate::broker::review_question::ReviewAnswerModel;
use crate::plan::review_answer::ReviewAnswerScope;

pub(super) async fn answer(
    registry: Arc<SessionControllerRegistry>,
    source: Arc<SessionController>,
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
    let mut chains = HashMap::<_, Vec<_>>::new();
    for question in questions {
        chains
            .entry(question.answer_session)
            .or_default()
            .push(question);
    }
    for (answer_session, questions) in chains {
        let registry = Arc::clone(&registry);
        let source = Arc::clone(&source);
        let document = document.clone();
        let selection = selection.clone();
        let message_sink = message_sink.clone();
        let lane = source
            .review_answer_lane
            .lock()
            .await
            .entry(answer_session)
            .or_default()
            .clone();
        pending.spawn(async move {
            let _turn = lane.lock().await;
            let child = review_standby::claim(
                &registry,
                &source,
                answer_session,
                &selection,
                &message_sink,
            )
            .await?;
            for question in questions {
                if !source.plan_review.question_pending(&document, &question)? {
                    continue;
                }
                let scope = ReviewAnswerScope::new(
                    Arc::clone(&source.plan_review),
                    document.clone(),
                    question,
                );
                answer_question(&child, scope, &selection, &message_sink, request_id).await?;
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

/// Resume the chain's durable provider binding, or fork the source for its first Ask.
pub(super) async fn open_answer_session(
    registry: &Arc<SessionControllerRegistry>,
    source: &str,
    answer_session: uuid::Uuid,
    message_sink: &MessageSender,
) -> Result<Arc<SessionController>> {
    let id = answer_session.to_string();
    let saved = crate::storage::HistoryReader::current(std::path::Path::new(
        &registry.initialize.data_root,
    ))?
    .load_session(&id)?;
    if let Some(saved) = saved {
        ensure!(
            saved.purpose == crate::session::SessionPurpose::PlanReviewAnswer,
            "review answer session has the wrong purpose"
        );
        let child = registry.resolve_reopening(&id).await?;
        registry
            .await_provider_fork(&id, &child, message_sink)
            .await?;
        return Ok(child);
    }
    let (child, gate) = start_session_fork(
        registry.clone(),
        source.to_owned(),
        json!({"name":"Plan Review answer"}),
        None,
        crate::broker::ForkTarget::PlanReviewAnswer(answer_session),
        message_sink,
    )
    .await?;
    gate.wait().await?;
    Ok(child)
}

async fn answer_question(
    child: &SessionController,
    scope: Arc<ReviewAnswerScope>,
    selection: &ReviewAnswerModel,
    message_sink: &MessageSender,
    request_id: u64,
) -> Result<()> {
    let (sink, updates) = crate::broker::publication::channel();
    let generation = child.provider_lane.read().await;
    let catalog_result = match selection {
        ReviewAnswerModel::Override(_) => {
            let job = child.broker.lock().await.prepare_configuration_catalog()?;
            Some(job.run().await?)
        }
        ReviewAnswerModel::Inherited { .. } => None,
    };
    drop(generation);
    let result = observe_answer(
        child.exchange_owner.review_question(
            scope,
            selection.clone(),
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
