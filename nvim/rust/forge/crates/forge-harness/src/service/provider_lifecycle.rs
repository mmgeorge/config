//! Own service listeners while the exchange owner retains provider initialization work.

use super::*;
use crate::exchange::provider_lifecycle::{
    ProviderInitialization, ProviderLifecycle, ProviderLifecycleFailure,
};
use std::{
    future::Future,
    pin::Pin,
    task::{Context as TaskContext, Poll},
};

/// Retain the precise inbox alongside its one observation task.
pub(super) struct SessionObservationTask {
    inbox: Arc<crate::provider::session_execution::SessionObservationInbox>,
    task: tokio::task::JoinHandle<Result<()>>,
}

impl SessionObservationTask {
    /// Get whether the original observer has completed without consuming its result.
    pub(super) fn is_finished(&self) -> bool {
        self.task.is_finished()
    }
    /// Stop the listener only when its entire session is explicitly being destroyed.
    pub(super) fn abort(&self) {
        self.task.abort();
    }
}

impl Future for SessionObservationTask {
    type Output = Result<Result<()>, tokio::task::JoinError>;
    /// Join the original listener while retaining it if this observation is cancelled.
    fn poll(mut self: Pin<&mut Self>, context: &mut TaskContext<'_>) -> Poll<Self::Output> {
        Pin::new(&mut self.task).poll(context)
    }
}

impl SessionController {
    /// Create a weak callback so retained owner work cannot form a controller cycle.
    pub(super) fn provider_lifecycle(self: &Arc<Self>) -> ProviderLifecycle {
        let controller = Arc::downgrade(self);
        ProviderLifecycle::new(move |provider| {
            let controller = controller.clone();
            Box::pin(async move {
                let controller = controller
                    .upgrade()
                    .ok_or(ProviderLifecycleFailure::ControllerClosed)?;
                controller
                    .prepare_provider_initialization(provider, None)
                    .await
            })
        })
    }

    /// Prepare one exact published generation without allocating a native conversation.
    ///
    /// An interactive session needs its observation listener and root owner before the first
    /// lazy open publishes into them. The native conversation itself opens or resumes inside
    /// `ProviderBridge::turn_start`, so settings changes before the first input stay local.
    async fn prepare_provider_initialization(
        self: &Arc<Self>,
        provider: Arc<crate::provider::Provider>,
        sink: Option<MessageSender>,
    ) -> std::result::Result<ProviderInitialization, ProviderLifecycleFailure> {
        if self.input_stopping.load(Ordering::Acquire) {
            return Err(ProviderLifecycleFailure::ControllerClosed);
        }
        let revision = self.mcp_revision.load(Ordering::Acquire);
        let (owner, registry) = {
            let mut broker = self.broker.lock().await;
            if !Arc::ptr_eq(&provider, &broker.provider_handle()) {
                return Err(ProviderLifecycleFailure::GenerationChanged);
            }
            (broker.interactive_owner(), broker.execution_registry())
        };
        let Some(owner) = owner else {
            return Ok(ProviderInitialization::NotRequired { provider });
        };
        self.attach_provider_events(&provider, sink).await?;
        registry
            .owner(owner)
            .await
            .map_err(ProviderLifecycleFailure::Provider)?;
        if self.input_stopping.load(Ordering::Acquire) {
            return Err(ProviderLifecycleFailure::ControllerClosed);
        }
        if revision != self.mcp_revision.load(Ordering::Acquire)
            || !Arc::ptr_eq(&provider, &self.provider_handle())
        {
            return Err(ProviderLifecycleFailure::GenerationChanged);
        }
        Ok(ProviderInitialization::Deferred { provider })
    }

    /// Retain startup work while the owner alone commits its initialized native identity.
    pub(super) async fn start_provider(self: &Arc<Self>, sink: &MessageSender) {
        let mut startup = self.provider_startup.lock().await;
        if let Some(task) = startup.as_ref() {
            if !task.is_finished()
                || self
                    .provider_startup_error
                    .lock()
                    .expect("provider startup state poisoned")
                    .is_none()
            {
                return;
            }
            startup.take();
        }
        *self
            .provider_startup_error
            .lock()
            .expect("provider startup state poisoned") = None;
        let controller = Arc::downgrade(self);
        let provider = self.provider_handle();
        let sink = sink.clone();
        *startup = Some(tokio::spawn(async move {
            let Some(controller) = controller.upgrade() else {
                return;
            };
            let initialized = controller
                .prepare_provider_initialization(provider, Some(sink.clone()))
                .await;
            let result = controller
                .exchange_owner
                .initialize_provider(initialized, Some(sink.clone()))
                .await;
            if let Err(error) = result {
                if matches!(
                    error,
                    ProviderLifecycleFailure::GenerationChanged
                        | ProviderLifecycleFailure::ControllerClosed
                ) {
                    return;
                }
                // Startup never resumes native history, so a missing saved conversation surfaces
                // as the first input's turn failure rather than here.
                let message = format!("Provider initialization failed: {error}");
                *controller
                    .provider_startup_error
                    .lock()
                    .expect("provider startup state poisoned") = Some(message.clone());
                let session_id = controller.observer.session_id().to_owned();
                controller.trace.record(
                    &session_id,
                    "provider.initialization_failed",
                    json!({"reason":error.to_string()}),
                );
                // Keep startup collection independent of a disconnected or saturated UI.
                if let Err(publication) = controller.publications.publish(
                    SessionEvent {
                        session_id: session_id.clone(),
                        event: "provider_event".into(),
                        payload: json!({"kind":"error", "text":message}),
                    },
                    Some(&sink),
                ) {
                    controller.trace.record(
                        &session_id,
                        "provider.initialization_failure_publication_failed",
                        json!({"reason":publication.to_string()}),
                    );
                }
            }
        }));
    }

    /// Attach one listener to the current inbox, collecting the previous listener first.
    ///
    /// The caller never waits on the broker; the listener installs the generation's root intake.
    async fn attach_provider_events(
        self: &Arc<Self>,
        provider: &Arc<crate::provider::Provider>,
        messages: Option<MessageSender>,
    ) -> std::result::Result<(), ProviderLifecycleFailure> {
        let mut slot = self.session_event_task.lock().await;
        let inbox = self.observer.session_events();
        if let Some(previous) = slot.as_mut() {
            if Arc::ptr_eq(&previous.inbox, &inbox) {
                return Ok(());
            }
            if !previous.inbox.is_closed() && !previous.is_finished() {
                return Err(ProviderLifecycleFailure::PreviousObservationLive);
            }
            // Consume the old task only after its actual completion; cancelled joins retain it.
            let result = previous.await;
            slot.take();
            if let Err(error) =
                result.map_err(|_| ProviderLifecycleFailure::ObservationUnavailable)?
            {
                self.trace.record(
                    self.observer.session_id(),
                    "provider.previous_observation_collected",
                    json!({"failure":format!("{error:#}")}),
                );
            }
        }
        let session_id = self.observer.session_id().to_owned();
        let controller = Arc::downgrade(self);
        let broker = Arc::clone(&self.broker);
        let provider = Arc::clone(provider);
        let events = inbox.clone();
        let task = tokio::spawn(async move {
            let result: Result<()> = async {
                // Routing attaches this listener before every session request, including local
                // policy commands that must not wait on a busy session's broker. The listener
                // therefore installs root intake itself; the inbox buffers events meanwhile.
                let registry = {
                    let mut broker = broker.lock().await;
                    // A replaced generation installs its own intake through provider
                    // initialization, so a stale caller snapshot installs nothing.
                    Arc::ptr_eq(&provider, &broker.provider_handle())
                        .then(|| broker.execution_registry())
                };
                drop(broker);
                drop(provider);
                if let Some(registry) = registry {
                    registry
                        .owner(crate::provider::ThreadOwner::root(&session_id))
                        .await
                        .context("install root provider intake")?;
                }
                loop {
                    if events.has_pending()? {
                        let Some(controller) = controller.upgrade() else {
                            return Ok(());
                        };
                        match controller
                            .exchange_owner
                            .observe_provider_retained(messages.clone())
                            .await
                        {
                            Ok(()) => {}
                            Err(error)
                                if error.owner_closed()
                                    && controller.input_stopping.load(Ordering::Acquire) =>
                            {
                                // Explicit shutdown keeps the final inbox drain with the owner.
                                // A late wake cannot turn that completed owner lifetime into a failure.
                                return Ok(());
                            }
                            Err(error) => return Err(error.into()),
                        }
                        continue;
                    }
                    if events.is_closed() {
                        if let Some(controller) = controller.upgrade() {
                            match controller
                                .exchange_owner
                                .observe_provider_retained(messages.clone())
                                .await
                            {
                                Ok(()) => {}
                                Err(error)
                                    if error.owner_closed()
                                        && controller.input_stopping.load(Ordering::Acquire) => {}
                                Err(error) => return Err(error.into()),
                            }
                        }
                        return Ok(());
                    }
                    events.notified().await?;
                }
            }
            .await;
            if let Err(error) = &result
                && let Some(controller) = controller.upgrade()
            {
                controller.trace.record(
                    &session_id,
                    "provider.observation_failed",
                    observation_failure_trace(error),
                );
                // Retain the diagnostic before a nonblocking offer; observer collection never waits on stale UI capacity.
                controller.publications.publish(SessionEvent { session_id, event: "provider_event".into(),
                        payload: json!({"kind":"error", "text":format!("Session observation failed: {}", crate::storage::error_message(error))}) }, messages.as_ref())
                        .context("retain provider observation failure")?;
            }
            result
        });
        *slot = Some(SessionObservationTask { inbox, task });
        Ok(())
    }

    /// Attach session services and the current generation's owned observation stream.
    pub(super) async fn attach_session_events(
        self: &Arc<Self>,
        messages: &MessageSender,
    ) -> Result<()> {
        self.attach_timeout(messages).await?;
        self.attach_agents(messages).await?;
        self.attach_provider_events(&self.provider_handle(), Some(messages.clone()))
            .await?;
        Ok(())
    }

    /// Collect the precise listener without losing its join handle on caller cancellation.
    pub(super) async fn finish_session_events(&self) -> Result<()> {
        let mut slot = self.session_event_task.lock().await;
        if let Some(pending) = slot.as_mut() {
            let result = pending.await;
            slot.take();
            result.context("session observation task failed during shutdown")??;
        }
        Ok(())
    }
}

/// Describe a provider observation failure through the trace policy's typed fields.
///
/// The trace keeps only allowlisted metadata, so free-form error text never reaches it;
/// the stage code and normalized observation kind identify the cause instead.
fn observation_failure_trace(error: &anyhow::Error) -> serde_json::Value {
    let error_code = error
        .chain()
        .find_map(|cause| cause.downcast_ref::<crate::exchange::ProviderObservationError>())
        .map_or(
            "observation_intake_failed",
            crate::exchange::ProviderObservationError::code,
        );
    let kind = error
        .chain()
        .find_map(|cause| cause.downcast_ref::<forge_provider_api::lifecycle::ObservationFailure>())
        .and_then(|failure| serde_json::to_value(failure).ok())
        .and_then(|failure| failure.get("kind").cloned());
    json!({"error_code":error_code, "kind":kind, "reason":format!("{error:#}")})
}

#[cfg(test)]
mod tests {
    /// The observation failure trace names its stage and normalized cause without native text.
    #[test]
    fn observation_failure_trace_records_typed_cause() {
        let error = anyhow::Error::from(crate::exchange::ProviderObservationError::Commit(
            anyhow::Error::from(crate::provider::ProviderError::Observation(
                forge_provider_api::lifecycle::ObservationFailure::OwnershipConflict,
            )),
        ));
        let payload = super::observation_failure_trace(&error);
        assert_eq!(payload["error_code"], "observation_commit_failed");
        assert_eq!(payload["kind"], "ownership_conflict");
    }
}
