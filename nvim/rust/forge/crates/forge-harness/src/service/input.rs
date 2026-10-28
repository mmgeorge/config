use super::*;
use crate::backend::submission::{Submission, SubmissionState};
use crate::input::{
    InputAction, InputDelivery, InputIntent, InputMessage, InputOperation, InputRoute, InputUpdate,
};

impl SessionController {
    pub(super) fn publish_input_pending(&self) -> Result<()> {
        let task_id = self.observer.input_task_id()?;
        let (revision, pending) = self
            .coordinator
            .submissions
            .has_pending_for_task(task_id.as_deref())?;
        self.observer.completion_queue.update(revision, pending)
    }

    pub(super) async fn publish_input(
        &self,
        update: InputUpdate,
        sink: &MessageSender,
    ) -> Result<()> {
        self.publish_input_pending()?;
        for message in &update.upsert {
            self.trace.record(
                self.observer.session_id(),
                "input.state",
                json!({
                    "message_id":message.id,"submission_id":message.submission_id,
                    "revision":message.revision,"status":message.delivery,
                }),
            );
        }
        let mut payload = serde_json::to_value(update)?;
        payload["execution_busy"] = json!(self.coordinator.execution.available_permits() == 0);
        Ok(sink
            .send_event(SessionEvent {
                session_id: self.catalog_request.read().await.harness_session_id.clone(),
                event: "input_changed".into(),
                payload,
            })
            .await?)
    }

    pub(super) async fn input_request(
        &self,
        request: Request,
        method: HarnessMethod,
        sink: &MessageSender,
    ) -> Result<()> {
        let _control = self
            .coordinator
            .control
            .clone()
            .acquire_owned()
            .await
            .context("Harness session is closing")?;
        ensure!(
            !self.input_stopping.load(Ordering::Acquire),
            "Harness session is closing"
        );
        let result = match method {
            HarnessMethod::InputApply => {
                let operation: InputOperation = crate::codec::from_value(request.params)?;
                if self
                    .coordinator
                    .submissions
                    .operation(&operation.id)?
                    .is_some()
                {
                    let update = self.coordinator.submissions.apply(operation)?;
                    sink.send_response(Response::success(request.id, update)?)
                        .await?;
                    return Ok(());
                }
                if let InputAction::Submit { message } = &operation.action {
                    ensure!(
                        message.task_id == self.observer.input_task_id()?,
                        "message belongs to another task"
                    );
                    if let Some(config) = &message.config {
                        let config = config
                            .as_object()
                            .context("queued configuration must be an object")?;
                        ensure!(
                            config.keys().all(|key| matches!(
                                key.as_str(),
                                "model" | "effort" | "service_tier" | "context_window"
                            )),
                            "queued input can only select model settings"
                        );
                    }
                }
                if let InputAction::Retry { id, revision } = &operation.action {
                    let snapshot = self.coordinator.submissions.snapshot()?;
                    if let Some(message) =
                        snapshot.messages.iter().find(|message| &message.id == id)
                    {
                        ensure!(
                            message.revision == *revision,
                            "message changed before retry"
                        );
                        if matches!(
                            message.delivery,
                            InputDelivery::Unknown | InputDelivery::Failed
                        ) && message.submission_id.is_some()
                        {
                            let journal = &self.submission_store;
                            let submission = journal.submission(
                                message
                                    .submission_id
                                    .as_deref()
                                    .context("delivery evidence is missing")?,
                            )?;
                            let record = submission.reconcile(self.backend.as_ref()).await?;
                            if matches!(record.delivery, SubmissionState::Accepted { .. }) {
                                let mut update = self
                                    .coordinator
                                    .submissions
                                    .delivery(id, None, None, None)?;
                                update.operation_id = Some(operation.id.clone());
                                self.publish_input(update.clone(), sink).await?;
                                sink.send_response(Response::success(request.id, update)?)
                                    .await?;
                                return Ok(());
                            }
                            ensure!(
                                !matches!(record.delivery, SubmissionState::Sending { .. }),
                                "Delivery is still unconfirmed. The message was not resent."
                            );
                            submission.cancel()?;
                            if message.delivery == InputDelivery::Unknown {
                                self.publish_input(
                                    self.coordinator.submissions.delivery(
                                        id,
                                        Some(InputDelivery::Failed),
                                        None,
                                        Some(
                                            "Non-delivery confirmed. Retry to send this message."
                                                .into(),
                                        ),
                                    )?,
                                    sink,
                                )
                                .await?;
                                anyhow::bail!(
                                    "Non-delivery confirmed. Retry to send this message."
                                );
                            }
                        }
                    }
                }
                if let InputAction::Commit { edits, .. } = &operation.action {
                    for edit in edits {
                        if let Some(config) = &edit.config {
                            let config = config
                                .as_object()
                                .context("queued configuration must be an object")?;
                            ensure!(
                                config.keys().all(|key| matches!(
                                    key.as_str(),
                                    "model" | "effort" | "service_tier" | "context_window"
                                )),
                                "queued input can only select model settings"
                            );
                        }
                        crate::attachment::resolve_input(
                            &self.data_root,
                            &json!({"attachments":edit.attachments}),
                        )
                        .await?;
                    }
                }
                let update = self.coordinator.submissions.apply(operation)?;
                if let Err(error) = self.publish_input(update.clone(), sink).await {
                    sink.send_response(Response::failure(
                        request.id,
                        "outcome_unknown",
                        format!("Input admitted but state publication failed: {error:#}"),
                    ))
                    .await?;
                    return Ok(());
                }
                serde_json::to_value(update)?
            }
            HarnessMethod::InputOperation => serde_json::to_value(
                self.coordinator.submissions.operation(
                    request.params["id"]
                        .as_str()
                        .context("input operation identity is required")?,
                )?,
            )?,
            HarnessMethod::InputGet => {
                let mut snapshot = serde_json::to_value(self.coordinator.submissions.snapshot()?)?;
                snapshot["execution_busy"] =
                    json!(self.coordinator.execution.available_permits() == 0);
                snapshot
            }
            _ => unreachable!(),
        };
        sink.send_response(Response::success(request.id, result)?)
            .await?;
        self.coordinator.submissions.changed.notify_one();
        Ok(())
    }

    pub(super) async fn attach_input(self: &Arc<Self>, sink: &MessageSender) -> Result<()> {
        let mut task = self.input_task.lock().await;
        if task.is_some() {
            return Ok(());
        }
        let owner = Arc::downgrade(self);
        let input = self.coordinator.submissions.clone();
        let admission_changed = self.observer.admission.changed.clone();
        let sink = sink.clone();
        *task = Some(tokio::spawn(async move {
            let mut jobs = JoinSet::new();
            let mut job_owner = std::collections::HashMap::new();
            let mut steering = false;
            loop {
                let notified = input.changed.notified();
                let state_changed = admission_changed.notified();
                let Some(controller) = owner.upgrade() else {
                    break;
                };
                if controller.input_stopping.load(Ordering::Acquire) {
                    while jobs.join_next().await.is_some() {}
                    break;
                }
                let progress = controller
                    .admit_input(&sink, &mut jobs, &mut steering, &mut job_owner)
                    .await;
                let wait_execution = match progress {
                    Ok(InputProgress::Started) => continue,
                    Ok(InputProgress::WaitExecution) => true,
                    Ok(InputProgress::Waiting) => false,
                    Err(error) => {
                        let _ = sink.send_event(SessionEvent {
                            session_id: controller.catalog_request.read().await.harness_session_id.clone(),
                            event: "backend_event".into(),
                            payload: json!({"kind":"error","text":format!("Input delivery failed: {error:#}")}),
                        }).await;
                        false
                    }
                };
                let admission = controller.coordinator.execution.clone();
                drop(controller);
                tokio::select! {
                    _ = notified => {},
                    _ = state_changed => {},
                    result = jobs.join_next_with_id(), if !jobs.is_empty() => {
                        match result {
                            Some(Ok((id, was_steering))) => {
                                job_owner.remove(&id);
                                if was_steering { steering = false; }
                            }
                            Some(Err(error)) => {
                                let Some(controller) = owner.upgrade() else { break };
                                if let Some(id) = job_owner.remove(&error.id()) {
                                    if let Ok(snapshot) = controller.coordinator.submissions.snapshot() {
                                        if snapshot.messages.iter().any(|message| message.id == id && message.route == Some(InputRoute::Steering)) {
                                            steering = false;
                                        }
                                    }
                                    if let Err(failure) = controller.input_failure(&id, format!("Input worker failed: {error}"), &sink).await {
                                        let _ = sink.send_event(SessionEvent {
                                            session_id: controller.catalog_request.read().await.harness_session_id.clone(),
                                            event: "backend_event".into(),
                                            payload: json!({"kind":"error","text":format!("Could not reconcile failed input worker: {failure:#}")}),
                                        }).await;
                                    }
                                }
                                let _ = sink.send_event(SessionEvent {
                                    session_id: controller.catalog_request.read().await.harness_session_id.clone(),
                                    event: "backend_event".into(),
                                    payload: json!({"kind":"error","text":format!("Input worker failed: {error}")}),
                                }).await;
                            }
                            None => {}
                        }
                    }
                    permit = admission.acquire_owned(), if wait_execution => { drop(permit); }
                }
            }
        }));
        self.coordinator.submissions.changed.notify_one();
        Ok(())
    }

    async fn admit_input(
        self: &Arc<Self>,
        sink: &MessageSender,
        jobs: &mut JoinSet<bool>,
        steering: &mut bool,
        job_owner: &mut std::collections::HashMap<tokio::task::Id, String>,
    ) -> Result<InputProgress> {
        let input = self.coordinator.submissions.snapshot()?;
        if input.suspended {
            return Ok(InputProgress::Waiting);
        }
        let Ok(control) = self.coordinator.control.clone().try_acquire_owned() else {
            return Ok(InputProgress::Waiting);
        };
        let snapshot = self.observer.input_readiness()?;
        if input.messages.iter().any(|message| {
            message.task_id == snapshot.session.current_task_id
                && message.delivery == InputDelivery::Unknown
        }) {
            return Ok(InputProgress::Waiting);
        }
        let mut pending = input.messages.into_iter().filter(|message| {
            message.task_id == snapshot.session.current_task_id
                && !matches!(
                    message.delivery,
                    InputDelivery::Delivering | InputDelivery::Accepted
                )
        });
        let Some(mut message) = pending.next() else {
            drop(control);
            return self.complete_input_checks(sink, jobs).await;
        };
        if snapshot.checks_running && message.intent == InputIntent::Auto {
            if let Some(run_id) = self.checks.active()? {
                if let Err(error) = self.interrupt_checks(&snapshot.session.id, &run_id).await {
                    self.publish_input(
                        self.coordinator.submissions.delivery(
                            &message.id,
                            Some(InputDelivery::Failed),
                            None,
                            Some(format!("Could not interrupt checks: {error:#}")),
                        )?,
                        sink,
                    )
                    .await?;
                    return Ok(InputProgress::Waiting);
                }
            }
            self.coordinator.submissions.changed.notify_one();
            return Ok(InputProgress::Waiting);
        }
        let admission = self.coordinator.execution.clone().try_acquire_owned().ok();
        let steering_available = admission.is_none()
            && !*steering
            && (self.parent_input.is_active()
                || snapshot.native_steer
                    && self.backend.has_active_turn(&snapshot.session.id).await);
        if steering_available
            && message.intent == InputIntent::Queue
            && message.delivery == InputDelivery::Queued
        {
            let Some(immediate) = pending.find(|message| {
                message.intent != InputIntent::Queue || message.delivery != InputDelivery::Queued
            }) else {
                return Ok(InputProgress::WaitExecution);
            };
            message = immediate;
        }
        if message.delivery != InputDelivery::Queued || message.reservation.is_some() {
            return Ok(InputProgress::Waiting);
        }
        if snapshot.session.current_task_id != message.task_id
            || snapshot
                .operation_state
                .as_deref()
                .is_some_and(|operation| {
                    matches!(
                        operation,
                        "admitted"
                            | "accepted"
                            | "stopping"
                            | "applying"
                            | "verifying"
                            | "resuming"
                    )
                })
            || (message.intent == InputIntent::Queue && snapshot.has_elicitation)
            || snapshot.has_approval
            || snapshot.checks_running
            || snapshot.task.iter().any(|task| {
                Some(&task.id) == message.task_id.as_ref()
                    && matches!(
                        task.status,
                        crate::task::TaskStatus::Stopping
                            | crate::task::TaskStatus::Blocked
                            | crate::task::TaskStatus::Failed
                            | crate::task::TaskStatus::Cancelled
                    )
            })
            || (message.intent == InputIntent::Queue
                && snapshot.task.iter().any(|task| {
                    Some(&task.id) == message.task_id.as_ref()
                        && task.status == crate::task::TaskStatus::Paused
                }))
        {
            return Ok(InputProgress::Waiting);
        }
        let steer =
            steering_available && message.intent == InputIntent::Auto && message.config.is_none();
        if admission.is_none() && !steer {
            return Ok(InputProgress::WaitExecution);
        }
        let route = if steer {
            InputRoute::Steering
        } else {
            InputRoute::Turn
        };
        let Some(message) =
            self.coordinator
                .submissions
                .claim(&message.id, message.revision, route)?
        else {
            return Ok(InputProgress::Waiting);
        };
        if admission.is_some() {
            self.cancellation.arm(true);
        }
        drop(control);
        let controller = self.clone();
        let sink = sink.clone();
        *steering |= steer;
        let message_id = message.id.clone();
        let worker = jobs.spawn(async move {
            let result = controller.deliver_input(&message, steer, admission, &sink).await;
            if let Err(error) = result {
                let diagnostic = crate::storage::error_message(&error);
                if let Err(persistence) = controller.input_failure(&message.id, diagnostic.clone(), &sink).await {
                        let _ = sink.send_event(SessionEvent {
                            session_id: controller.catalog_request.read().await.harness_session_id.clone(),
                            event: "backend_event".into(),
                            payload: json!({"kind":"error","text":format!("Input failed: {error:#}; recording the failure also failed: {persistence:#}")}),
                        }).await;
                } else {
                    let _ = sink.send_event(SessionEvent {
                        session_id: controller.catalog_request.read().await.harness_session_id.clone(),
                        event: "backend_event".into(),
                        payload: json!({"kind":"error","text":diagnostic}),
                    }).await;
                }
            }
            controller.coordinator.submissions.changed.notify_one();
            steer
        });
        job_owner.insert(worker.id(), message_id);
        Ok(InputProgress::Started)
    }

    async fn complete_input_checks(
        self: &Arc<Self>,
        sink: &MessageSender,
        jobs: &mut JoinSet<bool>,
    ) -> Result<InputProgress> {
        if !self.observer.completion_queue.deferred()? || self.checks.active()?.is_some() {
            return Ok(InputProgress::Waiting);
        }
        let Ok(control) = self.coordinator.control.clone().try_acquire_owned() else {
            return Ok(InputProgress::Waiting);
        };
        let Ok(admission) = self.coordinator.execution.clone().try_acquire_owned() else {
            return Ok(InputProgress::WaitExecution);
        };
        self.cancellation.arm(false);
        drop(control);
        let controller = self.clone();
        let sink = sink.clone();
        jobs.spawn(async move {
            let session_id = controller.catalog_request.read().await.harness_session_id.clone();
            let mut broker = controller.broker.lock().await;
            let result = dispatch_task_attempt(&mut broker, &session_id,
                Request { id: 0, method: "checks.complete".into(), params: json!({}) },
                &sink, controller.catalog_request.clone()).await;
            drop(broker);
            drop(admission);
            if let Err(error) = result {
                let _ = sink.send_event(SessionEvent {
                    session_id, event: "backend_event".into(),
                    payload: json!({"kind":"error","text":format!("Queue completion checks failed: {error:#}")}),
                }).await;
            }
            controller.coordinator.submissions.changed.notify_one();
            false
        });
        Ok(InputProgress::Started)
    }

    async fn input_failure(&self, id: &str, error: String, sink: &MessageSender) -> Result<()> {
        let message = self
            .coordinator
            .submissions
            .snapshot()?
            .messages
            .into_iter()
            .find(|message| message.id == id)
            .context("failed input is no longer retained")?;
        let delivery = if let Some(id) = &message.submission_id {
            let journal = &self.submission_store;
            let submission = journal.submission(id)?;
            match submission.record()?.delivery {
                SubmissionState::Accepted { .. } => None,
                SubmissionState::Sending { .. } => Some(InputDelivery::Unknown),
                SubmissionState::Prepared => {
                    submission.cancel()?;
                    Some(InputDelivery::Failed)
                }
                SubmissionState::Cancelled => Some(InputDelivery::Failed),
            }
        } else {
            Some(InputDelivery::Failed)
        };
        self.publish_input(
            self.coordinator
                .submissions
                .delivery(id, delivery, None, Some(error))?,
            sink,
        )
        .await
    }

    async fn deliver_input(
        &self,
        message: &InputMessage,
        steer: bool,
        admission: Option<ExecutionPermit>,
        sink: &MessageSender,
    ) -> Result<()> {
        let catalog = self.catalog_request.read().await.clone();
        let trace = self.trace.clone();
        let session_id = &catalog.harness_session_id;
        trace.record(
            session_id,
            "input.delivery_started",
            json!({"message_id":message.id,"phase":if steer {"steering"} else {"turn"}}),
        );
        let attachments = crate::attachment::resolve_input(
            &self.data_root,
            &json!({"attachments":message.attachments}),
        )
        .await?;
        let input = crate::backend::BackendInput::from_text(message.text.clone())
            .with_attachments(attachments.clone());
        let provider = self.backend.descriptor().kind;
        let message_id = message.id.clone();
        let submission_catalog = catalog.clone();
        let submission = self
            .submission_store
            .persist(move |journal| {
                journal.prepare_input(&submission_catalog, input, provider, Some(message_id))
            })
            .await?;
        self.publish_input(
            self.coordinator.submissions.delivery(
                &message.id,
                Some(InputDelivery::Delivering),
                Some(submission.id().into()),
                None,
            )?,
            sink,
        )
        .await?;
        if self.coordinator.submissions.snapshot()?.suspended
            || *self.cancellation.subscribe().borrow()
        {
            submission.cancel()?;
            self.publish_input(
                self.coordinator.submissions.delivery(
                    &message.id,
                    Some(InputDelivery::Queued),
                    None,
                    None,
                )?,
                sink,
            )
            .await?;
            return Ok(());
        }
        let result = if steer {
            let input = crate::backend::SteeringInput::User {
                text: message.text.clone(),
                attachments,
                submission: Some(submission.clone()),
            };
            if self.parent_input.is_active() {
                self.parent_input.steer(input).await
            } else {
                self.backend
                    .steer_session(&catalog.harness_session_id, input)
                    .await
            }
        } else {
            let mut broker = self.broker.lock().await;
            if let Some(config) = &message.config {
                let mut config = config.clone();
                config["validate"] = json!(true);
                dispatch_task_attempt(
                    &mut broker,
                    &catalog.harness_session_id,
                    Request {
                        id: 0,
                        method: "session.configure".into(),
                        params: config,
                    },
                    sink,
                    self.catalog_request.clone(),
                )
                .await?;
            }
            broker.input_submission = Some(submission.clone());
            if message.text.trim().is_empty() {
                broker.input_submission = None;
                submission.cancel()?;
                drop(broker);
                drop(admission);
                self.publish_input(
                    self.coordinator
                        .submissions
                        .delivery(&message.id, None, None, None)?,
                    sink,
                )
                .await?;
                return Ok(());
            }
            let params = json!({"text":message.text,"attachments":message.attachments});
            let (event_sink, mut events) = crate::backend::events::channel();
            let output = sink.clone();
            let session_id = catalog.harness_session_id.clone();
            let input = self.coordinator.submissions.clone();
            let forwarder = tokio::spawn(async move {
                while let Some(event) = events.recv().await? {
                    if event.kind == "execution_state" || event.kind == "turn_started" {
                        input.changed.notify_one();
                    }
                    let (event_name, payload) = if event.kind == "document_changed" {
                        ("document_changed", event.data)
                    } else {
                        ("backend_event", serde_json::to_value(event)?)
                    };
                    output
                        .send_event(SessionEvent {
                            session_id: session_id.clone(),
                            event: event_name.into(),
                            payload,
                        })
                        .await?;
                }
                Ok::<(), anyhow::Error>(())
            });
            let mut acceptance = submission.acceptance();
            trace.record(
                &catalog.harness_session_id,
                "input.dispatch_started",
                json!({"message_id":message.id,"submission_id":submission.id()}),
            );
            let mut response = {
                let mut dispatch = Box::pin(
                    broker.dispatch_stream(
                        Request {
                            id: 0,
                            method: if message.text == "/compact" {
                                "session.compact"
                            } else {
                                "prompt.submit"
                            }
                            .into(),
                            params,
                        },
                        event_sink,
                    ),
                );
                tokio::select! {
                    biased;
                    accepted = acceptance.changed() => {
                        accepted.context("submission acceptance channel closed")?;
                        self.publish_input(self.coordinator.submissions.delivery(
                            &message.id, Some(InputDelivery::Accepted), None, None,
                        )?, sink).await?;
                        dispatch.await
                    }
                    response = &mut dispatch => response,
                }
            };
            self.trace.record(
                &catalog.harness_session_id,
                "input.dispatch_finished",
                json!({"message_id":message.id}),
            );
            let continuation_store = SqliteStore::open(&self.data_root)?;
            let continuation_owner = continuation_store
                .latest_task_operation(&catalog.harness_session_id)?
                .map(|operation| operation.id);
            let mut continuing = response.response.error().is_none()
                && response
                    .event
                    .iter()
                    .any(|event| event.event == "goal_continue_requested");
            for event in response.event.drain(..) {
                sink.send_event(event).await?;
            }
            while continuing {
                if continuation_store
                    .latest_task_operation(&catalog.harness_session_id)?
                    .map(|operation| operation.id)
                    != continuation_owner
                {
                    break;
                }
                match dispatch_task_attempt(
                    &mut broker,
                    &catalog.harness_session_id,
                    Request {
                        id: 0,
                        method: "goal.continue".into(),
                        params: json!({}),
                    },
                    sink,
                    self.catalog_request.clone(),
                )
                .await
                {
                    Ok(next) => continuing = next,
                    Err(error) => {
                        response.response =
                            Response::failure(0, "continuation_failed", format!("{error:#}"));
                        break;
                    }
                }
            }
            broker.input_submission = None;
            self.trace.record(
                &catalog.harness_session_id,
                "input.catalog_pending",
                json!({"message_id":message.id}),
            );
            *self.catalog_request.write().await = broker.backend_catalog_request();
            drop(broker);
            self.trace.record(
                &catalog.harness_session_id,
                "input.forwarder_pending",
                json!({"message_id":message.id}),
            );
            forwarder.await??;
            self.trace.record(
                &catalog.harness_session_id,
                "input.forwarder_finished",
                json!({"message_id":message.id}),
            );
            for event in response.event {
                sink.send_event(event).await?;
            }
            if response
                .response
                .error()
                .is_some_and(|error| error.code == "turn_retracted")
            {
                submission.cancel()?;
                drop(admission);
                let mut update = self.coordinator.submissions.delivery(
                    &message.id,
                    Some(InputDelivery::Queued),
                    None,
                    None,
                )?;
                update.retracted = Some(message.id.clone());
                self.publish_input(update, sink).await?;
                return Ok(());
            }
            if message.text == "/compact" && response.response.error().is_none() {
                submission.cancel()?;
                drop(admission);
                self.publish_input(
                    self.coordinator
                        .submissions
                        .delivery(&message.id, None, None, None)?,
                    sink,
                )
                .await?;
                return Ok(());
            }
            match response.response.error() {
                Some(error) => Err(anyhow::anyhow!("{}", error.message)),
                None => Ok(()),
            }
        };
        drop(admission);
        self.settle_input(message, submission, result, steer, sink)
            .await
    }

    async fn settle_input(
        &self,
        message: &InputMessage,
        submission: Arc<Submission>,
        result: Result<()>,
        steer: bool,
        sink: &MessageSender,
    ) -> Result<()> {
        let record = submission.reconcile(self.backend.as_ref()).await?;
        let error = result.err().map(|error| format!("{error:#}"));
        if matches!(record.delivery, SubmissionState::Accepted { .. })
            && let Some(error) = &error
            && !*self.cancellation.subscribe().borrow()
        {
            sink.send_event(SessionEvent {
                session_id: record.request.harness_session_id.clone(),
                event: "backend_event".into(),
                payload: json!({"kind":"error","text":error}),
            })
            .await?;
        }
        let delivery = match record.delivery {
            SubmissionState::Accepted { .. } => None,
            SubmissionState::Prepared
                if steer
                    && !self.parent_input.is_active()
                    && !self
                        .backend
                        .has_active_turn(&record.request.harness_session_id)
                        .await =>
            {
                Some(InputDelivery::Queued)
            }
            SubmissionState::Prepared | SubmissionState::Cancelled => Some(InputDelivery::Failed),
            SubmissionState::Sending { .. } => Some(InputDelivery::Unknown),
        };
        let error = match delivery {
            Some(InputDelivery::Queued) => None,
            Some(InputDelivery::Failed) => {
                Some(error.unwrap_or_else(|| "Provider did not confirm message acceptance".into()))
            }
            Some(InputDelivery::Unknown) => Some(
                error.unwrap_or_else(|| "Delivery unconfirmed. Reconcile before retrying.".into()),
            ),
            _ => error,
        };
        // Proven non-delivery can queue, but it must not be recovered a second time by the adapter.
        if matches!(record.delivery, SubmissionState::Prepared) {
            submission.cancel()?;
        }
        self.publish_input(
            self.coordinator.submissions.delivery(
                &message.id,
                delivery,
                Some(submission.id().into()),
                error,
            )?,
            sink,
        )
        .await
    }
}

enum InputProgress {
    Started,
    WaitExecution,
    Waiting,
}
