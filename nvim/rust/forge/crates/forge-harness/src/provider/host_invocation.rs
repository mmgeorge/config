use std::sync::{Arc, Mutex};

use async_trait::async_trait;
use forge_provider_api::{
    BindingId, InvocationScope, ProviderError,
    control::{ProviderTools, ToolCall},
    error::{ProviderFailure, ProviderFailureKind, ProviderOperation},
    session::{HostEffectFailure, ProviderHostEffects},
    turn_control::{HostTurnStatus, TurnControl},
};
use tokio::sync::Notify;

use crate::tools::{ControlToolExecutor, ControlToolInvocation, ControlTurnContext};

use super::{CompletionSlot, ProviderCompletion};

/// Retain host callback authority and effects for exactly one normalized invocation.
pub(crate) struct HostInvocation {
    binding: BindingId,
    scope: InvocationScope,
    executor: ControlToolExecutor,
    permissions: Arc<crate::permissions::provider::ProviderPermissionContext>,
    directories: Mutex<Option<super::approval::DirectoryExecution>>,
    tools: std::collections::HashSet<String>,
    checkpoint: Option<crate::checkpoint::ToolGate>,
    completion: Arc<CompletionSlot>,
    endpoint: Arc<crate::tools::control_session::ControlSession>,
    endpoint_lease: Arc<super::bridge::EndpointLease>,
    endpoint_collected: tokio::sync::OnceCell<Result<(), HostEffectFailure>>,
    endpoint_closed: tokio::sync::OnceCell<()>,
    relay: tokio::sync::Mutex<HostEventRelay>,
    skills: Mutex<Option<crate::skill::SkillExecution>>,
    skill_observer: Option<Arc<dyn forge_provider_api::events::EventObserver>>,
    state: Mutex<InvocationState>,
    changed: Notify,
}

/// Own the callback transports and policy leases retained by one invocation.
pub(crate) struct HostInvocationResources {
    pub(crate) permissions: Arc<crate::permissions::provider::ProviderPermissionContext>,
    pub(crate) directories: Option<super::approval::DirectoryExecution>,
    pub(crate) endpoint: Arc<crate::tools::control_session::ControlSession>,
    pub(crate) endpoint_lease: Arc<super::bridge::EndpointLease>,
    pub(crate) relay: HostEventRelay,
    pub(crate) skills: Option<crate::skill::SkillExecution>,
}

/// Retain host event publication until the foreground barrier has drained it.
pub(crate) struct HostEventRelay {
    stop: Option<tokio::sync::oneshot::Sender<()>>,
    worker: Option<tokio::task::JoinHandle<Result<(), HostEffectFailure>>>,
    result: Option<Result<(), HostEffectFailure>>,
}

impl HostEventRelay {
    pub(crate) fn start(
        binding: BindingId,
        mut events: forge_provider_api::events::ProviderEventStream,
        sender: tokio::sync::mpsc::UnboundedSender<
            Result<
                forge_provider_api::ProviderEvent,
                forge_provider_api::events::EventDeliveryError,
            >,
        >,
    ) -> Self {
        let (stop, mut stopping) = tokio::sync::oneshot::channel();
        let worker = tokio::spawn(async move {
            let forward = |event: forge_provider_api::BoundProviderEvent| {
                if event.binding_id != binding {
                    let _ = sender.send(Err(
                        forge_provider_api::events::EventDeliveryError::Observation(
                            forge_provider_api::lifecycle::ObservationFailure::OwnershipConflict,
                        ),
                    ));
                    return Err(HostEffectFailure::Retired);
                }
                sender
                    .send(Ok(event.event))
                    .map_err(|_| HostEffectFailure::Retired)
            };
            loop {
                tokio::select! {
                    _ = &mut stopping => break,
                    event = events.recv() => match event {
                        Ok(Some(event)) => forward(event)?,
                        Ok(None) => return Ok(()),
                        Err(error) => {
                            let _ = sender.send(Err(error));
                            return Err(HostEffectFailure::Uncertain);
                        }
                    }
                }
            }
            loop {
                match events.try_recv() {
                    Ok(event) => forward(event)?,
                    Err(
                        forge_provider_api::events::EventDeliveryError::Empty
                        | forge_provider_api::events::EventDeliveryError::Closed,
                    ) => return Ok(()),
                    Err(error) => {
                        let _ = sender.send(Err(error));
                        return Err(HostEffectFailure::Uncertain);
                    }
                }
            }
        });
        Self {
            stop: Some(stop),
            worker: Some(worker),
            result: None,
        }
    }

    /// Stop after producers settle, then collect every already-published event.
    async fn finish(&mut self) -> Result<(), HostEffectFailure> {
        if let Some(result) = self.result {
            return result;
        }
        if let Some(stop) = self.stop.take() {
            let _ = stop.send(());
        }
        let result = match self.worker.as_mut() {
            Some(worker) => worker.await.unwrap_or(Err(HostEffectFailure::Uncertain)),
            None => Err(HostEffectFailure::Uncertain),
        };
        self.worker.take();
        self.result = Some(result);
        result
    }
}

#[derive(Default)]
struct InvocationState {
    admission: Admission,
    pending: usize,
    failure: Option<HostEffectFailure>,
    execution_finished: bool,
    foreground_settled: bool,
}

#[derive(Default)]
enum Admission {
    #[default]
    Open,
    Finishing,
    Cancelled,
}

struct AdmittedEffect<'owner> {
    owner: &'owner HostInvocation,
    published: bool,
}

impl HostInvocation {
    /// Create immutable routing before native input admission can trigger callbacks.
    pub(crate) fn new(
        binding: BindingId,
        scope: InvocationScope,
        context: ControlTurnContext,
        tools: &[forge_provider_api::control::ControlToolDefinition],
        resources: HostInvocationResources,
    ) -> Self {
        let HostInvocationResources {
            permissions,
            directories,
            endpoint,
            endpoint_lease,
            relay,
            skills,
        } = resources;
        let completion = context.provider_completion.clone();
        let skill_observer = skills
            .as_ref()
            .map(crate::skill::SkillExecution::event_observer);
        let checkpoint = context.checkpoint.clone();
        Self {
            binding,
            scope,
            executor: ControlToolExecutor::new(context),
            permissions,
            directories: Mutex::new(directories),
            tools: tools.iter().map(|tool| tool.name.to_owned()).collect(),
            checkpoint,
            completion,
            endpoint,
            endpoint_lease,
            endpoint_collected: tokio::sync::OnceCell::new(),
            endpoint_closed: tokio::sync::OnceCell::new(),
            relay: tokio::sync::Mutex::new(relay),
            skills: Mutex::new(skills),
            skill_observer,
            state: Mutex::new(InvocationState::default()),
            changed: Notify::new(),
        }
    }

    /// Cancel this invocation's tools without granting replacement callbacks its authority.
    pub(crate) fn cancel(&self) {
        let permission_failure = self.permissions.retire().err();
        let mut state = self
            .state
            .lock()
            .expect("host invocation state is available");
        state.admission = Admission::Cancelled;
        if permission_failure.is_some() {
            state.failure.get_or_insert(HostEffectFailure::Uncertain);
        }
        drop(state);
        self.executor.cancel();
        self.directories
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .take();
        self.endpoint.cancel_admission();
        self.skills
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .take();
        self.changed.notify_waiters();
    }

    /// Keep settled invocations and their late background observers outside foreground cleanup.
    pub(crate) fn foreground_pending(&self) -> bool {
        self.state
            .lock()
            .map_or(true, |state| !state.foreground_settled)
    }

    /// Append host-owned skill-read observations after their normalized source event.
    pub(crate) fn observe(
        &self,
        event: &forge_provider_api::ProviderEvent,
    ) -> Result<
        Vec<forge_provider_api::ProviderEvent>,
        forge_provider_api::events::EventDeliveryError,
    > {
        self.skill_observer.as_ref().map_or_else(
            || Ok(Vec::new()),
            |observer| observer.additional_events(event),
        )
    }

    /// Collect retained callback delivery after native execution has relinquished authority.
    pub(crate) async fn collect(&self) -> Result<(), HostEffectFailure> {
        self.settle(&self.scope).await
    }

    /// Admit a callback only while its immutable invocation retains authority.
    fn admit(&self, scope: &InvocationScope) -> Result<AdmittedEffect<'_>, ProviderError> {
        if scope != &self.scope {
            return Err(rejected());
        }
        let mut state = self.state.lock().map_err(|_| rejected())?;
        if !matches!(state.admission, Admission::Open) {
            return Err(rejected());
        }
        state.pending = state.pending.checked_add(1).ok_or_else(rejected)?;
        Ok(AdmittedEffect {
            owner: self,
            published: false,
        })
    }

    /// Close callback admission and await publication without depending on terminal delivery.
    async fn settle(&self, scope: &InvocationScope) -> Result<(), HostEffectFailure> {
        if scope != &self.scope {
            return Err(HostEffectFailure::Retired);
        }
        {
            let mut state = self
                .state
                .lock()
                .map_err(|_| HostEffectFailure::Uncertain)?;
            if matches!(state.admission, Admission::Open) {
                state.admission = Admission::Finishing;
            }
        }
        let endpoint_result = *self
            .endpoint_collected
            .get_or_init(|| async {
                self.endpoint.finish().await;
                let completion = self.endpoint.take_output().await;
                self.completion
                    .publish(completion)
                    .map_err(|_| HostEffectFailure::Uncertain)
            })
            .await;
        // Cache only successful server collection: a timed-out close retains its handle for retry.
        let endpoint_close = self
            .endpoint_closed
            .get_or_try_init(|| self.endpoint_lease.close())
            .await
            .map(|_| ());
        self.executor.finish().await;
        loop {
            let changed = self.changed.notified();
            tokio::pin!(changed);
            changed.as_mut().enable();
            {
                let mut state = self
                    .state
                    .lock()
                    .map_err(|_| HostEffectFailure::Uncertain)?;
                if matches!(state.admission, Admission::Open) {
                    state.admission = Admission::Finishing;
                }
                if state.pending == 0 {
                    break;
                }
            }
            changed.await;
        }
        let relay_result = self.relay.lock().await.finish().await;
        self.skills
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .take();
        endpoint_result?;
        endpoint_close?;
        relay_result?;
        let mut state = self
            .state
            .lock()
            .map_err(|_| HostEffectFailure::Uncertain)?;
        if let Some(failure) = state.failure {
            return Err(failure);
        }
        state.foreground_settled = true;
        Ok(())
    }
}

impl Drop for AdmittedEffect<'_> {
    /// Keep cancellation during host publication visible to the terminal barrier.
    fn drop(&mut self) {
        let mut state = self
            .owner
            .state
            .lock()
            .expect("host invocation state is available");
        state.pending -= 1;
        if !self.published {
            state.failure = Some(HostEffectFailure::Uncertain);
        }
        drop(state);
        self.owner.changed.notify_waiters();
    }
}

#[async_trait]
impl ProviderTools for HostInvocation {
    async fn call(&self, scope: &InvocationScope, call: ToolCall) -> Result<String, ProviderError> {
        if !self.tools.contains(&call.name) {
            return Err(rejected());
        }
        let mut admitted = self.admit(scope)?;
        let invocation = ControlToolInvocation {
            task_permit: None,
            name: call.name,
            arguments: call.arguments,
        };
        let outcome = self
            .executor
            .execute(invocation.clone())
            .await
            .map_err(|error| {
                if error.is::<crate::broker::TurnCancelled>() {
                    ProviderError::Cancelled
                } else {
                    ProviderError::Failure(ProviderFailure::new(
                        ProviderOperation::HostEffectPublication,
                        ProviderFailureKind::Unknown,
                    ))
                }
            })?;
        let mut completion = ProviderCompletion::default();
        let message = match &outcome.result {
            Ok(result) => {
                if let Some(effect) = &result.invocation {
                    crate::tools::apply_invocation(effect, &mut completion).map_err(|_| {
                        ProviderError::Failure(ProviderFailure::new(
                            ProviderOperation::HostEffectPublication,
                            ProviderFailureKind::Rejected,
                        ))
                    })?;
                }
                result.message.clone()
            }
            Err(error) => crate::tools::control_tool_failure_json(
                &invocation,
                error,
                outcome.plan_document.as_ref(),
            ),
        };
        self.completion.publish(completion)?;
        self.state
            .lock()
            .map_err(|_| rejected())?
            .execution_finished |= outcome.execution_finished;
        admitted.published = true;
        Ok(message)
    }
}

#[async_trait]
impl ProviderHostEffects for HostInvocation {
    async fn settle_foreground(
        &self,
        binding: &BindingId,
        scope: &InvocationScope,
    ) -> Result<(), HostEffectFailure> {
        if binding != &self.binding {
            return Err(HostEffectFailure::Retired);
        }
        self.settle(scope).await
    }
}

#[async_trait]
impl TurnControl for HostInvocation {
    async fn before_send(&self, scope: &InvocationScope) -> Result<(), ProviderError> {
        let mut admitted = self.admit(scope)?;
        admitted.published = true;
        if let Some(gate) = &self.checkpoint {
            loop {
                let changed = self.changed.notified();
                tokio::pin!(changed);
                changed.as_mut().enable();
                if matches!(
                    self.state.lock().map_err(|_| rejected())?.admission,
                    Admission::Cancelled
                ) {
                    return Err(ProviderError::Cancelled);
                }
                tokio::select! {
                    result = gate.wait() => return result.map_err(|_| rejected()),
                    _ = changed => {}
                }
            }
        }
        Ok(())
    }

    async fn receive(&self, scope: &InvocationScope) -> Result<(), ProviderError> {
        if scope != &self.scope {
            return Err(rejected());
        }
        std::future::pending().await
    }

    async fn close(&self, scope: &InvocationScope) -> Result<(), ProviderError> {
        self.settle(scope).await.map_err(|_| {
            ProviderError::Failure(ProviderFailure::new(
                ProviderOperation::HostEffectPublication,
                ProviderFailureKind::Unknown,
            ))
        })
    }

    async fn status(&self, scope: &InvocationScope) -> Result<HostTurnStatus, ProviderError> {
        if scope != &self.scope {
            return Err(rejected());
        }
        let preview = self.completion.preview()?;
        let completion = preview.completion.unwrap_or_default();
        let state = self.state.lock().map_err(|_| rejected())?;
        Ok(HostTurnStatus {
            replay_safe: super::retry::replay_safe(&completion),
            control_handoff: completion.evidence.structured_complete
                || completion.evidence.structured_blocked
                || completion.plan_submit.is_some()
                || completion.plan_question.is_some(),
            cancelled: matches!(state.admission, Admission::Cancelled),
            error: completion.control_error.map(|_| {
                ProviderError::Failure(ProviderFailure::new(
                    ProviderOperation::HostEffectPublication,
                    ProviderFailureKind::Rejected,
                ))
            }),
        })
    }

    fn tool_finished(&self, scope: &InvocationScope, _name: Option<&str>) -> bool {
        scope == &self.scope
            && self
                .state
                .lock()
                .is_ok_and(|state| state.execution_finished)
    }
}

/// Reject callbacks without exposing host storage or provider implementation state.
fn rejected() -> ProviderError {
    ProviderError::Failure(ProviderFailure::new(
        ProviderOperation::HostEffectPublication,
        ProviderFailureKind::Rejected,
    ))
}
