use std::sync::{Arc, Mutex};

use forge_provider_api::{
    FailureDisposition, OpenedSession, OperationContext, ProviderError, ProviderFailure,
    ProviderFailureKind, ProviderOperation, ProviderPhase, SessionRequest,
};

use super::operation::OperationControl;

/// Retain one exact open request while its native disposition remains unresolved.
#[derive(Default)]
pub(super) struct SessionOpening {
    state: Mutex<OpenState>,
    failed_output: Mutex<Option<FailedOpen>>,
}

/// Retain the sole returned stream until exact native cleanup succeeds after metadata failure.
struct FailedOpen {
    binding: forge_provider_api::SessionBinding,
    events: Arc<tokio::sync::Mutex<forge_provider_api::ProviderEventStream>>,
    close: Option<OperationControl>,
}

#[derive(Default)]
enum OpenState {
    #[default]
    Idle,
    Pending(PendingOpen),
    Closed(Option<PendingOpen>),
}

struct PendingOpen {
    operation: OperationControl,
    request: SessionRequest,
}

impl SessionOpening {
    /// Resolve retained opening work before the owner can report its session collected.
    pub(super) async fn collect(
        &self,
        provider: &dyn forge_provider_api::provider::Provider,
        trace: Arc<crate::trace::TraceStore>,
        core: &crate::exchange::core_log::CoreEventLog,
    ) -> Result<Option<OpenedSession>, ProviderError> {
        self.cancel();
        if self
            .failed_output
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .is_some()
        {
            self.collect_failed_output(provider).await?;
            return Ok(None);
        }
        let pending = {
            let state = self
                .state
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            match &*state {
                OpenState::Closed(Some(pending)) => {
                    Some((pending.operation.context(), pending.request.clone()))
                }
                OpenState::Closed(None) => None,
                OpenState::Idle | OpenState::Pending(_) => {
                    unreachable!("collection closed opening admission")
                }
            }
        };
        let Some((operation, request)) = pending else {
            return Ok(None);
        };
        let owner = request.owner.clone();
        let result = super::operation::observe_owned(
            trace,
            core,
            ProviderOperation::SessionOpen,
            &operation,
            forge_provider_api::EventScope::Binding { owner },
            None,
            super::operation::ObservationPurpose::RetainedCleanup,
            provider.session_open(operation.clone(), request),
        )
        .await;
        let settled = self.complete(&operation, result.result.as_ref().err());
        let result = self.resolve_observation(provider, result).await;
        match result {
            Ok(opened) => Ok(Some(opened)),
            Err(_)
                if settled
                    && self
                        .failed_output
                        .lock()
                        .unwrap_or_else(std::sync::PoisonError::into_inner)
                        .is_none() =>
            {
                Ok(None)
            }
            Err(error) => Err(error),
        }
    }

    /// Observe one retained transaction and commit its disposition before returning to the caller.
    pub(super) async fn open(
        &self,
        provider: &dyn forge_provider_api::provider::Provider,
        trace: Arc<crate::trace::TraceStore>,
        core: &crate::exchange::core_log::CoreEventLog,
        request: SessionRequest,
    ) -> Result<OpenedSession, ProviderError> {
        let (operation, request, purpose) = self.begin(request)?;
        let owner = request.owner.clone();
        let result = super::operation::observe_owned(
            trace,
            core,
            ProviderOperation::SessionOpen,
            &operation,
            forge_provider_api::EventScope::Binding { owner },
            None,
            purpose,
            provider.session_open(operation.clone(), request),
        )
        .await;
        let result = self.resolve_observation(provider, result).await;
        self.complete(&operation, result.as_ref().err());
        result
    }

    /// Collect a successful owned open before exposing its failed completion-evidence result.
    async fn resolve_observation(
        &self,
        provider: &dyn forge_provider_api::provider::Provider,
        observed: super::operation::ObservedResult<OpenedSession>,
    ) -> Result<OpenedSession, ProviderError> {
        match (observed.result, observed.completion_failure) {
            (result, None) => result,
            (Err(primary), Some(secondary)) => Err(ProviderError::Combined {
                primary: Box::new(primary),
                secondary: vec![secondary],
            }),
            (Ok(opened), Some(primary)) => {
                let OpenedSession {
                    binding,
                    capabilities: _,
                    events,
                } = opened;
                *self
                    .failed_output
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner) = Some(FailedOpen {
                    binding,
                    events: Arc::new(tokio::sync::Mutex::new(events)),
                    close: None,
                });
                *self
                    .state
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner) = OpenState::Closed(None);
                match self.collect_failed_output(provider).await {
                    Ok(()) => Err(primary),
                    Err(secondary) => Err(ProviderError::Combined {
                        primary: Box::new(primary),
                        secondary: vec![secondary],
                    }),
                }
            }
        }
    }

    /// Drain the retained stream while closing its exact binding; explicit retries allocate a new cleanup attempt.
    async fn collect_failed_output(
        &self,
        provider: &dyn forge_provider_api::provider::Provider,
    ) -> Result<(), ProviderError> {
        let target = {
            let mut failed = self
                .failed_output
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            match failed.as_mut() {
                Some(failed) => {
                    if failed.close.is_none() {
                        failed.close =
                            Some(OperationControl::fresh(ProviderOperation::SessionClose)?);
                    }
                    Some((
                        failed.binding.id.clone(),
                        failed
                            .close
                            .as_ref()
                            .expect("close context was installed")
                            .context(),
                        failed.events.clone(),
                    ))
                }
                None => None,
            }
        };
        if let Some((binding, context, events)) = target {
            let closing = provider.session_close(context.clone(), &binding);
            tokio::pin!(closing);
            let drain = async {
                let mut events = events.lock().await;
                while matches!(events.recv().await, Ok(Some(_))) {}
            };
            let result = tokio::select! {
                result = &mut closing => result,
                () = drain => closing.await,
            };
            let mut failed = self
                .failed_output
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            match result {
                Ok(()) => {
                    failed.take();
                }
                Err(error) => {
                    // A returned failure remains recorded under its original ID. A later explicit
                    // collection request may retry the exact binding through the adapter's shared collector.
                    if let Some(failed) = failed.as_mut()
                        && failed
                            .close
                            .as_ref()
                            .is_some_and(|close| close.context().id() == context.id())
                    {
                        failed.close = None;
                    }
                    return Err(error);
                }
            }
        }
        Ok(())
    }

    /// Start a new attempt or observe the original request with its original operation identity.
    fn begin(
        &self,
        request: SessionRequest,
    ) -> Result<
        (
            OperationContext,
            SessionRequest,
            super::operation::ObservationPurpose,
        ),
        ProviderError,
    > {
        let mut state = self
            .state
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let purpose = if matches!(*state, OpenState::Idle) {
            *state = OpenState::Pending(PendingOpen {
                operation: OperationControl::fresh(ProviderOperation::SessionOpen)?,
                request,
            });
            super::operation::ObservationPurpose::Admission
        } else {
            super::operation::ObservationPurpose::RetainedCleanup
        };
        match &*state {
            OpenState::Pending(pending) => Ok((
                pending.operation.context(),
                pending.request.clone(),
                purpose,
            )),
            OpenState::Closed(_) => Err(ProviderFailure::new(
                ProviderOperation::SessionOpen,
                ProviderFailureKind::BindingClosed,
            )
            .at_phase(ProviderPhase::Admission, FailureDisposition::Invalidated)
            .into()),
            OpenState::Idle => unreachable!("idle open attempt was installed"),
        }
    }

    /// Release a completed attempt only after success or proof that preparation had no effects.
    fn complete(&self, operation: &OperationContext, error: Option<&ProviderError>) -> bool {
        let settled = match error {
            None | Some(ProviderError::Unsupported(_)) => true,
            Some(ProviderError::Failure(failure)) => {
                failure.disposition() == FailureDisposition::Closed
                    || (matches!(
                        failure.phase(),
                        ProviderPhase::Admission
                            | ProviderPhase::Preparation
                            | ProviderPhase::Persistence
                    ) && failure.disposition() == FailureDisposition::Unchanged)
            }
            Some(_) => false,
        };
        if !settled {
            return false;
        }
        let mut state = self
            .state
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        match &mut *state {
            OpenState::Pending(pending) if pending.operation.context().id() == operation.id() => {
                *state = OpenState::Idle;
            }
            OpenState::Closed(pending) => {
                if pending
                    .as_ref()
                    .is_some_and(|pending| pending.operation.context().id() == operation.id())
                {
                    *pending = None;
                }
            }
            OpenState::Idle | OpenState::Pending(_) => {}
        }
        true
    }

    /// Revoke the retained attempt without replacing its request or releasing unresolved work.
    pub(super) fn cancel(&self) {
        let mut state = self
            .state
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        match std::mem::replace(&mut *state, OpenState::Closed(None)) {
            OpenState::Idle => {}
            OpenState::Pending(pending) => {
                pending.operation.cancel();
                *state = OpenState::Closed(Some(pending));
            }
            OpenState::Closed(pending) => *state = OpenState::Closed(pending),
        }
    }
}
