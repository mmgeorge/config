use crate::provider::ScopeOwner;
use std::sync::{Arc, Mutex};

use async_trait::async_trait;
use forge_provider_api::{
    BindingId, InvocationScope, ProviderError,
    control::{ProviderTools, ToolCall},
    error::{ProviderFailure, ProviderFailureKind, ProviderOperation},
    session::{HostEffectFailure, ProviderHostEffects},
    turn_control::{HostTurnStatus, TurnControl},
};
use futures_util::FutureExt;
use tokio::sync::Notify;

use crate::tools::{ControlToolExecutor, ControlToolInvocation, ControlTurnContext};

use super::{CompletionSlot, ProviderCompletion};

/// Retain host callback authority and effects for exactly one normalized invocation.
pub(crate) struct HostInvocation {
    provider_closed: Arc<std::sync::atomic::AtomicBool>,
    binding: BindingId,
    scope: InvocationScope,
    request: super::ProviderRequest,
    executor: ControlToolExecutor,
    template: ControlTurnContext,
    observed_authority: Arc<super::bridge::ObservedPermissionAuthority>,
    observed_tools: tokio::sync::Mutex<
        std::collections::HashMap<forge_provider_api::TurnId, Arc<ObservedTools>>,
    >,
    authority: Option<crate::tools::TaskAuthority>,
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
    skill_observer: Option<Arc<dyn super::events::EventObserver>>,
    state: Mutex<InvocationState>,
    changed: Notify,
    live_host_effects: Option<super::LiveHostEffectSender>,
    calls: Mutex<std::collections::HashMap<forge_provider_api::control::ToolCallId, Arc<HostCall>>>,
}

/// Retain a native child's executor and physical receipts independently from its parent's settlement.
struct ObservedTools {
    scope: InvocationScope,
    template: ControlTurnContext,
    executor: ControlToolExecutor,
    completion: Arc<CompletionSlot>,
    sink: super::HarnessEventSink,
    calls: Mutex<std::collections::HashMap<forge_provider_api::control::ToolCallId, Arc<HostCall>>>,
}
impl ObservedTools {
    /// Replay the original response and physical receipts without repeating committed file writes.
    async fn call(&self, call: ToolCall) -> Result<String, ProviderError> {
        let (record, fresh) = {
            let mut calls = self.calls.lock().map_err(|_| rejected())?;
            if let Some(record) = calls.get(&call.id) {
                if record.name != call.name || record.arguments != call.arguments {
                    return Err(rejected());
                }
                (record.clone(), false)
            } else {
                let record = Arc::new(HostCall {
                    name: call.name.clone(),
                    arguments: call.arguments.clone(),
                    state: Mutex::new(HostCallState::Running),
                    changed: Notify::new(),
                });
                calls.insert(call.id.clone(), record.clone());
                (record, true)
            }
        };
        if fresh {
            let _running = RunningHostCall(record.clone());
            let invocation = ControlToolInvocation {
                call_id: Some(call.id),
                task_permit: None,
                name: call.name,
                arguments: call.arguments,
            };
            let result = match self.executor.execute(invocation.clone()).await {
                Ok(outcome) => match outcome.result {
                    Ok(result) => Ok(result.message),
                    Err(error) => Ok(crate::tools::control_tool_failure_text(
                        &invocation,
                        &error,
                        outcome.plan_document.as_ref(),
                    )),
                },
                Err(_) => Err(rejected()),
            };
            *record.state.lock().map_err(|_| rejected())? = HostCallState::Complete(result);
            record.changed.notify_waiters();
        }
        loop {
            let changed = record.changed.notified();
            tokio::pin!(changed);
            changed.as_mut().enable();
            let state = record.state.lock().map_err(|_| rejected())?.clone();
            match state {
                HostCallState::Running => changed.await,
                HostCallState::Complete(result) => {
                    for change in self.completion.collected_file_changes() {
                        self.sink.send_host_file_change(change)?;
                    }
                    return result;
                }
                HostCallState::Published(_) => return Err(rejected()),
            }
        }
    }
}

struct HostCall {
    name: String,
    arguments: serde_json::Value,
    state: Mutex<HostCallState>,
    changed: Notify,
}

#[derive(Clone)]
enum HostCallState {
    Running,
    Published(HostCallPublication),
    Complete(Result<String, ProviderError>),
}

#[derive(Clone)]
struct HostCallPublication {
    result: futures_util::future::Shared<
        futures_util::future::BoxFuture<'static, Result<String, ProviderError>>,
    >,
}

struct RunningHostCall(Arc<HostCall>);

impl Drop for RunningHostCall {
    /// Preserve ambiguity if a callback disappears before publishing a replayable batch.
    fn drop(&mut self) {
        let mut state = self
            .0
            .state
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        if matches!(*state, HostCallState::Running) {
            *state = HostCallState::Complete(Err(ProviderError::DeliveryUncertain));
        }
        self.0.changed.notify_waiters();
    }
}

/// Own the callback transports and policy leases retained by one invocation.
pub(crate) struct HostInvocationResources {
    pub(crate) observed_authority: Arc<super::bridge::ObservedPermissionAuthority>,
    pub(crate) provider_closed: Arc<std::sync::atomic::AtomicBool>,
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
        _binding: BindingId,
        mut events: super::events::HarnessEventStream,
        sender: tokio::sync::mpsc::UnboundedSender<
            Result<
                super::bridge::ProviderObservation,
                forge_provider_api::events::EventDeliveryError,
            >,
        >,
    ) -> Self {
        let (stop, mut stopping) = tokio::sync::oneshot::channel();
        let worker = tokio::spawn(async move {
            let forward = |event: super::session_execution::OwnerObservation| {
                sender
                    .send(Ok(super::bridge::ProviderObservation {
                        event,
                        native: None,
                    }))
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
    native_input: NativeInputAdmission,
    pending: usize,
    failure: Option<HostEffectFailure>,
    execution_finished: bool,
    foreground_settled: bool,
}

/// Distinguish host-only preparation from an input request handed to the adapter.
#[derive(Clone, Copy, Default)]
pub(super) enum NativeInputAdmission {
    #[default]
    Preparing,
    Requested,
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
        request: &super::ProviderRequest,
        context: ControlTurnContext,
        tools: &[forge_provider_api::control::ControlToolDefinition],
        resources: HostInvocationResources,
    ) -> Self {
        let HostInvocationResources {
            observed_authority,
            provider_closed,
            permissions,
            directories,
            endpoint,
            endpoint_lease,
            relay,
            skills,
        } = resources;
        let completion = context.provider_completion.clone();
        let live_host_effects = context.live_host_effects.clone();
        let skill_observer = skills
            .as_ref()
            .map(crate::skill::SkillExecution::event_observer);
        let checkpoint = context.checkpoint.clone();
        Self {
            provider_closed,
            binding,
            scope,
            request: request.invocation_snapshot(),
            authority: crate::tools::TaskAuthority::capture(&context),
            template: context.clone(),
            observed_authority,
            observed_tools: Default::default(),
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
            live_host_effects,
            calls: Mutex::new(std::collections::HashMap::new()),
        }
    }

    /// Retain host replay risk for this original delegation subtree without absorbing child results.
    pub(crate) fn mark_descendant_work(
        &self,
        parent: &InvocationScope,
    ) -> Result<(), ProviderError> {
        if self.observed_authority.owns_parent(parent) {
            self.completion.record_descendant_work()?;
        }
        Ok(())
    }
    /// Retire only independently owned child executors at their actual native lifecycle boundary.
    pub(crate) async fn retire_observed(
        &self,
        thread: Option<&forge_provider_api::ThreadId>,
        turn: Option<&forge_provider_api::TurnId>,
    ) -> Result<(), ProviderError> {
        let children = self.observed_tools.lock().await;
        for (id, child) in children.iter() {
            if turn.is_some_and(|turn| turn != id)
                || thread.is_some_and(|thread| thread != &child.scope.owner().thread_id())
            {
                continue;
            }
            child.executor.cancel();
            for change in child.completion.collected_file_changes() {
                child.sink.send_host_file_change(change)?;
            }
        }
        Ok(())
    }
    /// Collect every independently retained child callback after binding fencing wakes pending proof admission.
    pub(crate) async fn collect_observed(&self) -> Result<(), ProviderError> {
        let children = self
            .observed_tools
            .lock()
            .await
            .values()
            .cloned()
            .collect::<Vec<_>>();
        let mut mutation_failed = false;
        for child in children {
            child.executor.close().await;
            mutation_failed |= child.executor.mutation_failed();
            for change in child.completion.collected_file_changes() {
                child.sink.send_host_file_change(change)?;
            }
        }
        if mutation_failed {
            return Err(ProviderFailure::new(
                ProviderOperation::TurnStart,
                ProviderFailureKind::Runtime,
            )
            .at_phase(
                forge_provider_api::ProviderPhase::Collection,
                forge_provider_api::FailureDisposition::Uncertain,
            )
            .into());
        }
        Ok(())
    }
    /// Get immutable invocation preferences only for the fully matching invocation scope.
    pub(crate) fn request_snapshot(
        &self,
        scope: &InvocationScope,
    ) -> Result<super::ProviderRequest, ProviderError> {
        if &self.scope != scope {
            return Err(forge_provider_api::lifecycle::LifecycleError::OwnershipMismatch.into());
        }
        Ok(self.request.clone())
    }

    /// Cancel this invocation's tools without granting replacement callbacks its authority.
    pub(super) fn cancel(&self) -> NativeInputAdmission {
        let permission_failure = self.permissions.retire().err();
        let mut state = self
            .state
            .lock()
            .expect("host invocation state is available");
        state.admission = Admission::Cancelled;
        if permission_failure.is_some() {
            state.failure.get_or_insert(HostEffectFailure::Uncertain);
        }
        let native_input = state.native_input;
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
        native_input
    }

    /// Keep settled invocations and their late background observers outside foreground cleanup.
    pub(crate) fn foreground_pending(&self) -> bool {
        self.state
            .lock()
            .map_or(true, |state| !state.foreground_settled)
    }

    /// Commit the native-call boundary while this exact host invocation still permits admission.
    pub(crate) fn request_native_input(&self) -> Result<(), ProviderError> {
        let mut state = self.state.lock().map_err(|_| rejected())?;
        match state.admission {
            Admission::Open => {}
            Admission::Cancelled => {
                return Err(ProviderError::PreparationFailed(Box::new(
                    ProviderError::Cancelled,
                )));
            }
            Admission::Finishing => {
                return Err(ProviderError::PreparationFailed(Box::new(rejected())));
            }
        }
        state.native_input = NativeInputAdmission::Requested;
        Ok(())
    }

    /// Verify original invocation authority and get its immutable native generation.
    pub(super) fn binding_for(&self, scope: &InvocationScope) -> Result<&BindingId, ProviderError> {
        if &self.scope != scope {
            return Err(rejected());
        }
        Ok(&self.binding)
    }

    /// Append host-owned skill-read observations after their normalized source event.
    pub(crate) fn observe(
        &self,
        event: &forge_provider_api::ProviderEvent,
    ) -> Result<Vec<super::events::HarnessEvent>, forge_provider_api::events::EventDeliveryError>
    {
        let additional = self
            .skill_observer
            .as_ref()
            .map_or_else(
                || Ok(Vec::new()),
                |observer| observer.additional_events(&self.scope.clone().into(), event),
            )?
            .into_iter()
            .map(|event| super::events::HarnessEvent::Host {
                scope: self.scope.clone().into(),
                event,
            })
            .collect::<Vec<_>>();
        Ok(additional)
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
        if self
            .provider_closed
            .load(std::sync::atomic::Ordering::Acquire)
        {
            return Err(ProviderFailure::new(
                ProviderOperation::TurnStart,
                ProviderFailureKind::BindingClosed,
            )
            .at_phase(
                forge_provider_api::ProviderPhase::Admission,
                forge_provider_api::FailureDisposition::Invalidated,
            )
            .into());
        }
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
        if self.executor.mutation_failed() || self.endpoint.mutation_failed() {
            return Err(HostEffectFailure::Uncertain);
        }
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

impl HostInvocation {
    /// Request the live logical commit while retaining the exact batch for replay after a lost waiter.
    fn publication(
        &self,
        call_id: forge_provider_api::control::ToolCallId,
        batch: super::HostEffectBatch,
        message: String,
    ) -> HostCallPublication {
        let owner = self.live_host_effects.clone();
        let scope = self.scope.clone();
        let authority = self.authority.clone();
        let result = async move {
            let completion = &batch.completion;
            if !completion.plan_edit.is_empty()
                || !completion.design_patch.is_empty()
                || completion.plan_submit.is_some()
                || completion.goal_control.is_some()
                || completion.plan_question.is_some()
                || completion.question_withdrawal.is_some()
            {
                let owner = owner.ok_or_else(|| {
                    ProviderError::Failure(ProviderFailure::new(
                        ProviderOperation::TurnStart,
                        ProviderFailureKind::Rejected,
                    ))
                })?;
                let committed = owner.commit(scope, call_id, batch).await?;
                if let Some(permit) = committed.task_permit
                    && let Some(authority) = &authority
                {
                    // The logical effect already committed. A later user revocation
                    // prevents future callbacks; it cannot change this retained result.
                    let _ = authority.advance(permit);
                }
            }
            Ok(message)
        }
        .boxed()
        .shared();
        HostCallPublication { result }
    }

    /// Execute once, publish stable batch identity, and await any required live owner commit.
    async fn execute_call(
        &self,
        call: ToolCall,
        record: &HostCall,
        admitted: &mut AdmittedEffect<'_>,
    ) -> Result<String, ProviderError> {
        let call_id = call.id.clone();
        let invocation = ControlToolInvocation {
            call_id: Some(call.id),
            task_permit: None,
            name: call.name,
            arguments: call.arguments,
        };
        let outcome = self.executor.execute(invocation.clone()).await;
        // A call that fails before producing its batch, such as a question wait retired by
        // cancellation, publishes no host effect. Its absence is certain, so it must not leave
        // the invocation's collection uncertain and fail the owner's observation.
        if outcome.is_err() {
            admitted.published = true;
        }
        let outcome = outcome.map_err(|error| {
            if error.is::<crate::broker::TurnCancelled>() {
                ProviderError::Cancelled
            } else {
                ProviderError::Failure(ProviderFailure::new(
                    ProviderOperation::TurnStart,
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
                            ProviderOperation::TurnStart,
                            ProviderFailureKind::Rejected,
                        ))
                    })?;
                }
                result.message.clone()
            }
            Err(error) => crate::tools::control_tool_failure_text(
                &invocation,
                error,
                outcome.plan_document.as_ref(),
            ),
        };
        let batch = self.completion.publish_batch(completion)?;
        let publication = self.publication(call_id, batch, message);
        *record.state.lock().map_err(|_| rejected())? =
            HostCallState::Published(publication.clone());
        record.changed.notify_waiters();
        self.state
            .lock()
            .map_err(|_| rejected())?
            .execution_finished |= outcome.execution_finished;
        admitted.published = true;
        publication.result.await
    }
}

#[async_trait]
impl ProviderTools for HostInvocation {
    /// Execute only a positively registered native child's independently forked host context.
    async fn call_observed_child(
        &self,
        parent: &InvocationScope,
        child: &forge_provider_api::TurnAddress,
        call: ToolCall,
    ) -> Result<String, ProviderError> {
        if self
            .provider_closed
            .load(std::sync::atomic::Ordering::Acquire)
        {
            return Err(ProviderError::Cancelled);
        }
        if !self.tools.contains(&call.name) {
            return Err(rejected());
        }
        let (proof, sink) = self.observed_authority.resolve(parent, child).await?;
        self.mark_descendant_work(parent)?;
        let tools = {
            let mut children = self.observed_tools.lock().await;
            for ancestor in self.observed_authority.ancestry(&proof)? {
                let scope = ancestor.turn().ok_or_else(rejected)?;
                let id = scope.turn_id().map_err(|_| rejected())?;
                if children.contains_key(&id) {
                    continue;
                }
                let template = if ancestor.parent() == &self.scope {
                    self.template.clone()
                } else {
                    children
                        .values()
                        .find(|tools| &tools.scope == ancestor.parent())
                        .ok_or_else(rejected)?
                        .template
                        .clone()
                };
                let sink = if scope == &child.scope {
                    sink.clone()
                } else {
                    self.observed_authority.tool_sink(&ancestor)?
                };
                let context = template.fork_observed(&ancestor, sink.clone())?;
                children.insert(
                    id,
                    Arc::new(ObservedTools {
                        scope: scope.clone(),
                        completion: context.provider_completion.clone(),
                        template: context.clone(),
                        executor: ControlToolExecutor::new(context),
                        sink,
                        calls: Default::default(),
                    }),
                );
            }
            children.get(&child.turn_id).cloned().ok_or_else(rejected)?
        };
        tools.call(call).await
    }
    async fn call(&self, scope: &InvocationScope, call: ToolCall) -> Result<String, ProviderError> {
        if scope != &self.scope || !self.tools.contains(&call.name) {
            return Err(rejected());
        }
        // A committed reply remains deliverable after its own effect retires task authority.
        let retained = {
            let calls = self.calls.lock().map_err(|_| rejected())?;
            calls
                .get(&call.id)
                .map(|record| {
                    if record.name != call.name || record.arguments != call.arguments {
                        return Err(rejected());
                    }
                    Ok(record.state.lock().map_err(|_| rejected())?.clone())
                })
                .transpose()?
        };
        match retained {
            Some(HostCallState::Complete(result)) => return result,
            Some(HostCallState::Published(publication)) => return publication.result.await,
            Some(HostCallState::Running) | None => {}
        }
        let mut admitted = self.admit(scope)?;
        let (record, fresh) = {
            let mut calls = self.calls.lock().map_err(|_| rejected())?;
            if let Some(record) = calls.get(&call.id) {
                if record.name != call.name || record.arguments != call.arguments {
                    return Err(rejected());
                }
                (record.clone(), false)
            } else {
                let record = Arc::new(HostCall {
                    name: call.name.clone(),
                    arguments: call.arguments.clone(),
                    state: Mutex::new(HostCallState::Running),
                    changed: Notify::new(),
                });
                calls.insert(call.id.clone(), record.clone());
                (record, true)
            }
        };
        if fresh {
            let _running = RunningHostCall(record.clone());
            let result = self.execute_call(call, &record, &mut admitted).await;
            *record.state.lock().map_err(|_| rejected())? = HostCallState::Complete(result.clone());
            record.changed.notify_waiters();
            return result;
        }
        admitted.published = true;
        loop {
            let changed = record.changed.notified();
            tokio::pin!(changed);
            changed.as_mut().enable();
            let state = record.state.lock().map_err(|_| rejected())?.clone();
            match state {
                HostCallState::Running => changed.await,
                HostCallState::Published(publication) => {
                    let result = publication.result.await;
                    *record.state.lock().map_err(|_| rejected())? =
                        HostCallState::Complete(result.clone());
                    record.changed.notify_waiters();
                    return result;
                }
                HostCallState::Complete(result) => return result,
            }
        }
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
                ProviderOperation::TurnStart,
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
                || completion.goal_control.is_some()
                || completion.plan_submit.is_some()
                || completion.plan_question.is_some(),
            cancelled: matches!(state.admission, Admission::Cancelled),
            error: completion.control_error.map(|_| {
                ProviderError::Failure(ProviderFailure::new(
                    ProviderOperation::TurnStart,
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
        ProviderOperation::TurnStart,
        ProviderFailureKind::Rejected,
    ))
}
