//! Drive the owned foreground lifecycle for result-oriented broker callers.

use super::*;
use futures_util::FutureExt;

impl HarnessBroker {
    /// Normalize a local prompt before entering the same owned admission as the service.
    pub(crate) async fn dispatch_local_prompt(&mut self, request: Request) -> DispatchResult {
        let id = request.id;
        self.trace_foreground_request(&request);
        let result = async {
            let attachments = if self.foreground_requires_attachments(&request)? {
                crate::attachment::resolve_input(&self.data_root, &request.params).await?
            } else {
                Vec::new()
            };
            let (sink, updates) = publication::channel();
            let mut dispatch = self.begin_foreground_dispatch(request, sink)?;
            dispatch.set_attachments(attachments)?;
            self.drive_local_foreground(dispatch, updates).await
        }
        .await;
        match result {
            Ok(result) => {
                self.trace_foreground_response(id, &result);
                result
            }
            Err(error) => {
                self.trace_foreground_error(id, &error);
                let response = if let Some(retracted) = error.downcast_ref::<TurnRetracted>() {
                    Response::failure_with_data(
                        id,
                        "turn_retracted",
                        retracted.to_string(),
                        json!({"prompt":retracted.prompt}),
                    )
                } else {
                    let code = super::request_failure_code(&error);
                    Response::failure(id, code, crate::storage::error_message(&error))
                };
                DispatchResult {
                    response,
                    event: Vec::new(),
                }
            }
        }
    }

    /// Poll the same owned jobs and settlement phases used by the service driver.
    pub(crate) async fn drive_local_foreground(
        &mut self,
        dispatch: TurnTransaction,
        updates: publication::SessionUpdateStream,
    ) -> Result<DispatchResult> {
        Box::pin(self.drive_local_foreground_initial(dispatch, updates, None)).await
    }

    pub(crate) async fn drive_local_foreground_initial(
        &mut self,
        mut dispatch: TurnTransaction,
        mut updates: publication::SessionUpdateStream,
        initial: Option<ForegroundJob>,
    ) -> Result<DispatchResult> {
        let mut jobs: tokio::task::JoinSet<(bool, Result<ForegroundSignal>)> =
            tokio::task::JoinSet::new();
        let mut publications = std::collections::VecDeque::new();
        let mut publication_active = false;
        let mut drain = false;
        let mut forwarded = Vec::new();
        let mut update_open = true;
        let mut deferred_failure = None;
        if initial.is_none() && self.observer.session_events().is_closed() {
            let previous = self.observer.session_events();
            while !previous.ready_for_replacement()? {
                match self.drain_session_events() {
                    Ok(events) => forwarded.extend(events),
                    Err(error)
                        if matches!(
                            error.downcast_ref::<ProviderError>(),
                            Some(ProviderError::Observation(_))
                        ) =>
                    {
                        forwarded.push(self.event("provider_event", json!({
                            "kind":"error", "text":format!("Session observation failed: {}", crate::storage::error_message(&error)),
                        }))?);
                    }
                    Err(error) => return Err(error),
                }
            }
            let registry = self.execution_registry();
            let observer = registry
                .replace(crate::provider::ThreadOwner::root(&self.session.id))
                .await?;
            self.observer.replace_session_events(observer);
            self.task_controller.reset_configuration_generation()?;
            self.clear_effective_capabilities()?;
        }
        if let Some(job) = initial {
            let publication = job.is_publication();
            publication_active = publication;
            jobs.spawn(local_foreground_job(job, publication));
        }
        loop {
            // This standalone driver holds exclusive broker ownership; the previous
            // reducer step is its command/effect boundary, with no service command queue.
            dispatch.authorize_submission();
            let (signal, completed_publication) = if let Some(error) = deferred_failure.take() {
                (ForegroundSignal::EffectFailed(error), false)
            } else if drain && jobs.is_empty() && publications.is_empty() {
                drain = false;
                (ForegroundSignal::PublicationDrained, false)
            } else {
                tokio::select! {
                    biased;
                    update = updates.recv(), if update_open => {
                        match update? {
                            Some(update) => {
                                if let Some(sink) = self.event_sink.clone() {
                                    if let Err(error) = sink.send_wait(update).await {
                                        deferred_failure = Some(error.into());
                                    }
                                    continue;
                                }
                                forwarded.push(publication::encode_session_event(&self.session.id, &update)?);
                            }
                            None => update_open = false,
                        }
                        continue;
                    }
                    signal = dispatch.next() => (signal.unwrap_or_else(ForegroundSignal::EffectFailed), false),
                    result = jobs.join_next(), if !jobs.is_empty() => match result {
                        Some(Ok((publication, result))) => (result.unwrap_or_else(ForegroundSignal::EffectFailed), publication),
                        Some(Err(error)) => (ForegroundSignal::EffectFailed(anyhow::anyhow!("foreground effect task failed: {error}")), false),
                        None => (ForegroundSignal::EffectFailed(anyhow::anyhow!("foreground effect task disappeared")), false),
                    }
                }
            };
            // Match the service owner's per-signal diagnostics so a hot reducer loop is visible.
            self.trace.record(
                &self.session.id,
                "exchange.event.read",
                json!({"signal": signal.diagnostic_name()}),
            );
            let signal = match signal {
                ForegroundSignal::ObservationPublicationNeedsForward { events } => {
                    forwarded.extend(events);
                    ForegroundSignal::ObservationPublicationDelivered(Ok(Vec::new()))
                }
                ForegroundSignal::CommandEventsNeedForward { events } => {
                    forwarded.extend(events);
                    ForegroundSignal::CommandEventsForwarded(Ok(()))
                }
                ForegroundSignal::CommandNeedsForward { events } => {
                    forwarded.extend(events);
                    ForegroundSignal::CommandPublished(Ok(Vec::new()))
                }
                ForegroundSignal::PermissionNeedsForward { events } => {
                    forwarded.extend(events);
                    ForegroundSignal::PermissionPublished(Ok(Vec::new()))
                }
                ForegroundSignal::AdmissionEventsNeedForward { events } => {
                    forwarded.extend(events);
                    ForegroundSignal::AdmissionEventsForwarded(Ok(()))
                }
                ForegroundSignal::FailureAdmissionEventsNeedForward { events } => {
                    forwarded.extend(events);
                    ForegroundSignal::FailureAdmissionEventsForwarded(Ok(()))
                }
                ForegroundSignal::FailurePublicationNeedsForward { events } => {
                    forwarded.extend(events);
                    ForegroundSignal::FailurePublicationCompleted(Ok(Vec::new()))
                }
                ForegroundSignal::AutoCompactionNeedsForward { events } => {
                    forwarded.extend(events);
                    ForegroundSignal::AutoCompactionPublished(Ok(Vec::new()))
                }
                ForegroundSignal::CheckRepairNeedsForward { events } => {
                    forwarded.extend(events);
                    ForegroundSignal::CheckRepairPublished(Ok(Vec::new()))
                }
                ForegroundSignal::ClarificationNeedsForward { events } => {
                    forwarded.extend(events);
                    ForegroundSignal::ClarificationPublished(Ok(Vec::new()))
                }
                ForegroundSignal::FinalResponseNeedsForward { events } => {
                    forwarded.extend(events);
                    ForegroundSignal::FinalResponsePublished(Ok(Vec::new()))
                }
                ForegroundSignal::PlanTerminalNeedsForward { events, receipts } => {
                    forwarded.extend(events);
                    ForegroundSignal::PlanTerminalPublished(
                        receipts.acknowledge().map(|()| Vec::new()),
                    )
                }
                signal => signal,
            };
            let sink = dispatch.publication_sink()?;
            let action = self.with_publication_sink(&sink, |broker| {
                match broker.step_foreground(&mut dispatch, signal) {
                    Ok(action) => Ok(action),
                    Err(error) => broker.fail_foreground_dispatch(&mut dispatch, error),
                }
            })?;
            if completed_publication {
                publication_active = false;
                if let Some(job) = publications.pop_front() {
                    publication_active = true;
                    jobs.spawn(local_foreground_job(job, true));
                }
            }
            match action {
                ForegroundAction::Wait => {}
                ForegroundAction::WaitForPublicationDrain => drain = true,
                action @ (ForegroundAction::Effect(_)
                | ForegroundAction::CancelJobsAndEffect(_)) => {
                    let job = match action {
                        ForegroundAction::Effect(job) => job,
                        ForegroundAction::CancelJobsAndEffect(job) => {
                            drain = false;
                            job
                        }
                        _ => unreachable!(),
                    };
                    let publication = job.is_publication();
                    if publication && publication_active {
                        publications.push_back(job);
                    } else {
                        publication_active |= publication;
                        jobs.spawn(local_foreground_job(job, publication));
                    }
                }
                ForegroundAction::Finish => {
                    anyhow::ensure!(
                        jobs.is_empty() && publications.is_empty(),
                        "foreground effects remain pending"
                    );
                    let mut result = self.finish_foreground(dispatch)?;
                    forwarded.append(&mut result.event);
                    result.event = forwarded;
                    return Ok(result);
                }
                ForegroundAction::FinishFailure(error) => {
                    anyhow::ensure!(
                        jobs.is_empty() && publications.is_empty(),
                        "foreground failure still has pending effects"
                    );
                    return Err(error);
                }
            }
        }
    }
}

async fn local_foreground_job(
    job: ForegroundJob,
    publication: bool,
) -> (bool, Result<ForegroundSignal>) {
    let result = std::panic::AssertUnwindSafe(job.run()).catch_unwind().await;
    (
        publication,
        result.unwrap_or_else(|_| Err(anyhow::anyhow!("foreground effect panicked"))),
    )
}
