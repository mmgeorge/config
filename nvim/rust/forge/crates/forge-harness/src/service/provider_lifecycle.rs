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

    /// Initialize one exact published generation without committing its native identity.
    async fn prepare_provider_initialization(
        self: &Arc<Self>,
        provider: Arc<crate::provider::Provider>,
        sink: Option<MessageSender>,
    ) -> std::result::Result<ProviderInitialization, ProviderLifecycleFailure> {
        if self.input_stopping.load(Ordering::Acquire) {
            return Err(ProviderLifecycleFailure::ControllerClosed);
        }
        let revision = self.mcp_revision.load(Ordering::Acquire);
        let (request, registry) = {
            let mut broker = self.broker.lock().await;
            if !Arc::ptr_eq(&provider, &broker.provider_handle()) {
                return Err(ProviderLifecycleFailure::GenerationChanged);
            }
            (
                broker
                    .initialization_request()
                    .map_err(|_| ProviderLifecycleFailure::StateUnavailable)?,
                broker.execution_registry(),
            )
        };
        let Some(request) = request else {
            return Ok(ProviderInitialization::NotRequired { provider });
        };
        self.attach_provider_events(&provider, sink).await?;
        let owner = request
            .owner()
            .map_err(|_| ProviderLifecycleFailure::StateUnavailable)?;
        registry
            .owner(owner)
            .await
            .map_err(ProviderLifecycleFailure::Provider)?;
        let provider_session_id = provider.session_open(request.clone()).await?;
        if self.input_stopping.load(Ordering::Acquire) {
            return Err(ProviderLifecycleFailure::ControllerClosed);
        }
        if revision != self.mcp_revision.load(Ordering::Acquire)
            || !Arc::ptr_eq(&provider, &self.provider_handle())
        {
            return Err(ProviderLifecycleFailure::GenerationChanged);
        }
        Ok(ProviderInitialization::Ready {
            provider,
            request: Box::new(request),
            provider_session_id,
        })
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
            let result = match initialized {
                Ok(initialized) => {
                    controller
                        .exchange_owner
                        .initialize_provider(initialized, Some(sink.clone()))
                        .await
                }
                Err(error) => Err(error),
            };
            if let Err(error) = result {
                if matches!(
                    error,
                    ProviderLifecycleFailure::GenerationChanged
                        | ProviderLifecycleFailure::ControllerClosed
                ) {
                    return;
                }
                let message = if matches!(&error, ProviderLifecycleFailure::Provider(crate::provider::ProviderError::Failure(failure))
                    if failure.kind() == forge_provider_api::ProviderFailureKind::SessionMissing)
                {
                    format!(
                        "Provider initialization failed: {error}. Saved provider history is unavailable; retained conversation was preserved"
                    )
                } else {
                    format!("Provider initialization failed: {error}")
                };
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
        let registry = {
            let mut broker = self.broker.lock().await;
            if !Arc::ptr_eq(provider, &broker.provider_handle()) {
                return Err(ProviderLifecycleFailure::GenerationChanged);
            }
            broker.execution_registry()
        };
        registry
            .owner(crate::provider::ExecutionOwner::root(&session_id))
            .await
            .map_err(|_| ProviderLifecycleFailure::ObservationUnavailable)?;
        let controller = Arc::downgrade(self);
        let events = inbox.clone();
        let task = tokio::spawn(async move {
            let result: Result<()> = async {
                loop {
                    if events.has_pending()? {
                        events.take_idle_settlement_request();
                        let Some(controller) = controller.upgrade() else {
                            return Ok(());
                        };
                        controller
                            .exchange_owner
                            .observe_provider_retained(messages.clone())
                            .await?;
                        continue;
                    }
                    if events.is_closed() {
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
                    json!({"reason":format!("{error:#}")}),
                );
                // Retain the diagnostic before a nonblocking offer; observer collection never waits on stale UI capacity.
                controller.publications.publish(SessionEvent { session_id, event: "provider_event".into(),
                        payload: json!({"kind":"error", "text":format!("Session observation failed: {error:#}")}) }, messages.as_ref())
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
