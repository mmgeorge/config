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
                    let code = if error.is::<RequestNotAdmitted>() {
                        "not_admitted"
                    } else if error.is::<TurnCancelled>() {
                        "turn_cancelled"
                    } else if error.is::<crate::provider::tool_timeout::ToolTimeout>() {
                        "tool_timeout"
                    } else {
                        "request_failed"
                    };
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
        dispatch: ForegroundDispatch,
        updates: publication::SessionUpdateStream,
    ) -> Result<DispatchResult> {
        Box::pin(self.drive_local_foreground_initial(dispatch, updates, None)).await
    }

    pub(crate) async fn drive_local_foreground_initial(
        &mut self,
        mut dispatch: ForegroundDispatch,
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
                            "kind":"error", "text":format!("Session observation failed: {error:#}"),
                        }))?);
                    }
                    Err(error) => return Err(error),
                }
            }
            let registry = self.execution_registry();
            let observer = registry
                .replace(crate::provider::ExecutionOwner::root(&self.session.id))
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
            if self
                .observer
                .session_events()
                .take_idle_settlement_request()
            {
                let (sink, idle_updates) = publication::channel();
                if let Some((idle, job)) = self.begin_idle_foreground(sink)? {
                    let mut result = Box::pin(self.drive_local_foreground_initial(
                        idle,
                        idle_updates,
                        Some(job),
                    ))
                    .await?;
                    forwarded.append(&mut result.event);
                }
            }
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

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn approval_timeout_stages_owned_continuation_and_preserves_queued_purpose() -> Result<()>
    {
        use crate::permissions::matcher::{PermissionRequest, PermissionTarget};
        use crate::provider::approval::ApprovalResolution;
        let workspace = super::super::test::repository();
        let data = tempfile::tempdir()?;
        let mut broker =
            super::super::test::planning_question_broker(workspace.path(), data.path(), false);
        let owner = crate::provider::ExecutionOwner::root(&broker.session.id);
        broker.permission_coordinator.configure_timeout(
            &broker.session.id,
            crate::session::InputTimeout {
                amount: 1,
                unit: crate::session::TimeoutUnit::S,
            },
        )?;
        let scope = crate::provider::InvocationScope::new(owner.clone(), "approval-source");
        let (sink, mut events) = crate::provider::events::channel(scope.clone());
        let coordinator = broker.permission_coordinator.clone();
        let waiter = tokio::spawn(async move {
            coordinator
                .authorize(
                    ApprovalMode::Ask,
                    PermissionRequest {
                        id: "timed-approval".into(),
                        provider: forge_provider_api::ProviderId::new("fixture")
                            .expect("fixture identity is valid"),
                        reason: None,
                        target_list: vec![PermissionTarget::Command {
                            command: "echo approval-timeout".into(),
                            shell: None,
                        }],
                    },
                    Some(crate::provider::TurnAddress {
                        scope,
                        thread_id: "thread".into(),
                        turn_id: "turn".into(),
                    }),
                    Some(&sink),
                )
                .await
        });
        anyhow::ensure!(events.recv().await?.is_some(), "approval was not observed");
        anyhow::ensure!(
            tokio::time::timeout(std::time::Duration::from_secs(3), waiter).await???
                == ApprovalResolution::TimedOut,
            "approval did not time out"
        );
        broker.input_purpose = crate::input::InputPurpose::QuestionContinuation {
            exchange_id: "queued-owner".into(),
        };
        let (publication, _updates) = publication::channel();
        let (dispatch, _) = broker.with_publication_sink(&publication, |broker| {
            broker.begin_approval_timeout_dispatch(publication.clone())
        })?;
        let ForegroundDispatch::Admitting(admission) =
            dispatch.context("approval continuation was not staged")?
        else {
            anyhow::bail!("approval continuation has the wrong phase");
        };
        let staged = admission
            .staged_interaction
            .context("approval continuation has no exchange")?;
        anyhow::ensure!(
            matches!(
                staged.exchange.origin,
                crate::exchange::ExchangeOrigin::TaskContinuation { .. }
            ),
            "approval continuation lost its timeout origin"
        );
        anyhow::ensure!(
            matches!(broker.input_purpose, crate::input::InputPurpose::QuestionContinuation { ref exchange_id } if exchange_id == "queued-owner"),
            "approval continuation consumed queued input purpose"
        );
        anyhow::ensure!(
            !broker.permission_coordinator.has_timeout_feedback(&owner)?,
            "approval feedback remained queued"
        );
        Ok(())
    }

    #[tokio::test]
    async fn local_prompt_records_request_success_and_admission_failure() -> Result<()> {
        let workspace = super::super::test::repository();
        let data = tempfile::tempdir()?;
        let mut broker =
            super::super::test::planning_question_broker(workspace.path(), data.path(), false);
        broker.trace.configure(true)?;
        let success = broker
            .dispatch(Request {
                id: 71,
                method: "session.prompt.submit".into(),
                params: json!({"text":"/plan"}),
            })
            .await;
        anyhow::ensure!(
            success.response.error().is_none(),
            "plan picker should succeed"
        );
        let failure = broker
            .dispatch(Request {
                id: 72,
                method: "session.prompt.submit".into(),
                params: json!({}),
            })
            .await;
        anyhow::ensure!(
            failure.response.error().is_some(),
            "missing prompt should fail admission"
        );
        broker.trace.flush()?;
        let lines = std::fs::read_to_string(broker.trace.session_status(&broker.session.id).path)?;
        let entries = lines
            .lines()
            .map(serde_json::from_str::<Value>)
            .collect::<std::result::Result<Vec<_>, _>>()?;
        for (id, terminal) in [(71, "lua.rpc.completed"), (72, "lua.rpc.failed")] {
            anyhow::ensure!(
                entries
                    .iter()
                    .any(|entry| entry["event"] == "lua.rpc.received"
                        && entry["payload"]["id"] == id),
                "request trace missing for {id}"
            );
            anyhow::ensure!(
                entries
                    .iter()
                    .any(|entry| entry["event"] == terminal && entry["payload"]["id"] == id),
                "terminal trace missing for {id}"
            );
        }
        Ok(())
    }
}
