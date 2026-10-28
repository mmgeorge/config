use super::*;
use crate::backend::submission::{Submission, SubmissionState, SubmissionStore};
use crate::input::{
    InputAction, InputDelivery, InputIntent, InputMessage, InputOperation, InputRoute, InputUpdate,
};

impl SessionController {
    pub(super) fn publish_input_pending(&self) -> Result<()> {
        let input = self.input.snapshot()?;
        let snapshot = self.observer.snapshot()?;
        let pending = input.messages.iter().any(|message| {
            message.task_id == snapshot.session.current_task_id
                && !matches!(
                    message.delivery,
                    InputDelivery::Delivering | InputDelivery::Accepted | InputDelivery::Failed
                )
        });
        self.observer
            .completion_queue
            .update(input.revision, pending)
    }

    pub(super) async fn publish_input(
        &self,
        update: InputUpdate,
        sink: &MessageSender,
    ) -> Result<()> {
        self.publish_input_pending()?;
        let mut payload = serde_json::to_value(update)?;
        payload["execution_busy"] = json!(self.execution_admission.available_permits() == 0);
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
        let result = match method {
            HarnessMethod::InputApply => {
                let operation: InputOperation = crate::codec::from_value(request.params)?;
                if self.input.operation(&operation.id)?.is_some() {
                    let update = self.input.apply(operation)?;
                    sink.send_response(Response::success(request.id, update)?)
                        .await?;
                    return Ok(());
                }
                if let InputAction::Submit { message } = &operation.action {
                    let snapshot = self.observer.snapshot()?;
                    ensure!(
                        message.task_id == snapshot.session.current_task_id,
                        "message belongs to another task"
                    );
                    crate::attachment::resolve_input(
                        &self.data_root,
                        &json!({"attachments":message.attachments}),
                    )
                    .await?;
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
                let update = self.input.apply(operation)?;
                self.publish_input(update.clone(), sink).await?;
                serde_json::to_value(update)?
            }
            HarnessMethod::InputOperation => serde_json::to_value(
                self.input.operation(
                    request.params["id"]
                        .as_str()
                        .context("input operation identity is required")?,
                )?,
            )?,
            HarnessMethod::InputGet => {
                let mut snapshot = serde_json::to_value(self.input.snapshot()?)?;
                snapshot["execution_busy"] =
                    json!(self.execution_admission.available_permits() == 0);
                snapshot
            }
            _ => unreachable!(),
        };
        sink.send_response(Response::success(request.id, result)?)
            .await?;
        self.input.changed.notify_one();
        Ok(())
    }

    pub(super) async fn attach_input(self: &Arc<Self>, sink: &MessageSender) -> Result<()> {
        let mut task = self.input_task.lock().await;
        if task.is_some() {
            return Ok(());
        }
        let owner = Arc::downgrade(self);
        let input = self.input.clone();
        let sink = sink.clone();
        *task = Some(tokio::spawn(async move {
            let mut jobs = JoinSet::new();
            let mut steering = false;
            loop {
                let notified = input.changed.notified();
                let Some(controller) = owner.upgrade() else {
                    break;
                };
                if controller.input_stopping.load(Ordering::Acquire) {
                    while jobs.join_next().await.is_some() {}
                    break;
                }
                let progress = controller
                    .admit_input(&sink, &mut jobs, &mut steering)
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
                let admission = controller.execution_admission.clone();
                drop(controller);
                tokio::select! {
                    _ = notified => {},
                    result = jobs.join_next(), if !jobs.is_empty() => {
                        match result {
                            Some(Ok(was_steering)) => { if was_steering { steering = false; } }
                            Some(Err(error)) => {
                                let Some(controller) = owner.upgrade() else { break };
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
        self.input.changed.notify_one();
        Ok(())
    }

    async fn admit_input(
        self: &Arc<Self>,
        sink: &MessageSender,
        jobs: &mut JoinSet<bool>,
        steering: &mut bool,
    ) -> Result<InputProgress> {
        let input = self.input.snapshot()?;
        if input.suspended {
            return Ok(InputProgress::Waiting);
        }
        let snapshot = self.observer.snapshot()?;
        let mut pending = input.messages.into_iter().filter(|message| {
            message.task_id == snapshot.session.current_task_id
                && !matches!(
                    message.delivery,
                    InputDelivery::Delivering | InputDelivery::Accepted | InputDelivery::Failed
                )
        });
        let Some(mut message) = pending.next() else {
            return self.complete_input_checks(sink, jobs).await;
        };
        let Ok(control) = self.control_admission.clone().try_acquire_owned() else {
            return Ok(InputProgress::Waiting);
        };
        let admission = self.execution_admission.clone().try_acquire_owned().ok();
        let steering_available = admission.is_none()
            && !*steering
            && (self.parent_input.is_active()
                || snapshot.capability.native_steer
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
            || snapshot.task_operation.as_ref().is_some_and(|operation| {
                matches!(
                    operation["state"].as_str(),
                    Some(
                        "admitted"
                            | "accepted"
                            | "stopping"
                            | "applying"
                            | "verifying"
                            | "resuming"
                    )
                )
            })
            || (message.intent == InputIntent::Queue && snapshot.active_elicitation.is_some())
            || !snapshot.approval.is_empty()
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
        let Some(message) = self.input.claim(&message.id, message.revision, route)? else {
            return Ok(InputProgress::Waiting);
        };
        if admission.is_some() {
            self.cancellation.arm(true);
        }
        drop(control);
        let controller = self.clone();
        let sink = sink.clone();
        *steering |= steer;
        jobs.spawn(async move {
            let result = controller.deliver_input(&message, steer, admission, &sink).await;
            if let Err(error) = result {
                if let Err(persistence) = controller.input_failure(&message.id, format!("{error:#}"), &sink).await {
                        let _ = sink.send_event(SessionEvent {
                            session_id: controller.catalog_request.read().await.harness_session_id.clone(),
                            event: "backend_event".into(),
                            payload: json!({"kind":"error","text":format!("Input failed: {error:#}; recording the failure also failed: {persistence:#}")}),
                        }).await;
                }
            }
            controller.input.changed.notify_one();
            steer
        });
        Ok(InputProgress::Started)
    }

    async fn complete_input_checks(
        self: &Arc<Self>,
        sink: &MessageSender,
        jobs: &mut JoinSet<bool>,
    ) -> Result<InputProgress> {
        let snapshot = self.observer.snapshot()?;
        if !snapshot.checks_deferred || snapshot.checks_running {
            return Ok(InputProgress::Waiting);
        }
        let Ok(control) = self.control_admission.clone().try_acquire_owned() else {
            return Ok(InputProgress::Waiting);
        };
        let Ok(admission) = self.execution_admission.clone().try_acquire_owned() else {
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
            controller.input.changed.notify_one();
            false
        });
        Ok(InputProgress::Started)
    }

    async fn input_failure(&self, id: &str, error: String, sink: &MessageSender) -> Result<()> {
        let message = self
            .input
            .snapshot()?
            .messages
            .into_iter()
            .find(|message| message.id == id)
            .context("failed input is no longer retained")?;
        let delivery = if let Some(id) = &message.submission_id {
            let journal = SubmissionStore::open(&self.data_root, self.trace.clone())?;
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
        self.publish_input(self.input.delivery(id, delivery, None, Some(error))?, sink)
            .await
    }

    async fn deliver_input(
        &self,
        message: &InputMessage,
        steer: bool,
        admission: Option<OwnedSemaphorePermit>,
        sink: &MessageSender,
    ) -> Result<()> {
        let catalog = self.catalog_request.read().await.clone();
        let attachments = crate::attachment::resolve_input(
            &self.data_root,
            &json!({"attachments":message.attachments}),
        )
        .await?;
        let journal = SubmissionStore::open(&self.data_root, self.trace.clone())?;
        let submission = journal.prepare_steering(
            &catalog,
            crate::backend::BackendInput::from_text(message.text.clone())
                .with_attachments(attachments.clone()),
            self.backend.descriptor().kind,
        )?;
        self.publish_input(
            self.input.delivery(
                &message.id,
                Some(InputDelivery::Delivering),
                Some(submission.id().into()),
                None,
            )?,
            sink,
        )
        .await?;
        if self.input.snapshot()?.suspended || *self.cancellation.subscribe().borrow() {
            submission.cancel()?;
            self.publish_input(
                self.input
                    .delivery(&message.id, Some(InputDelivery::Queued), None, None)?,
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
                self.publish_input(self.input.delivery(&message.id, None, None, None)?, sink)
                    .await?;
                return Ok(());
            }
            let params = json!({"text":message.text,"attachments":message.attachments});
            let (event_sink, mut events) = crate::backend::events::channel();
            let output = sink.clone();
            let session_id = catalog.harness_session_id.clone();
            let input = self.input.clone();
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
                        self.publish_input(self.input.delivery(
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
                let mut update =
                    self.input
                        .delivery(&message.id, Some(InputDelivery::Queued), None, None)?;
                update.retracted = Some(message.id.clone());
                self.publish_input(update, sink).await?;
                return Ok(());
            }
            if message.text == "/compact" && response.response.error().is_none() {
                submission.cancel()?;
                drop(admission);
                self.publish_input(self.input.delivery(&message.id, None, None, None)?, sink)
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
            SubmissionState::Prepared if steer => Some(InputDelivery::Queued),
            SubmissionState::Prepared | SubmissionState::Cancelled => Some(InputDelivery::Failed),
            SubmissionState::Sending { .. } => Some(InputDelivery::Unknown),
        };
        // Proven non-delivery can queue, but it must not be recovered a second time by the adapter.
        if matches!(record.delivery, SubmissionState::Prepared) {
            submission.cancel()?;
        }
        if delivery == Some(InputDelivery::Queued) {
            let mut queued = message.clone();
            queued.intent = InputIntent::Queue;
            self.input.set_intent(&queued.id, InputIntent::Queue)?;
        }
        self.publish_input(
            self.input
                .delivery(&message.id, delivery, Some(submission.id().into()), error)?,
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
