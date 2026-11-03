//! Provider-neutral scripted backend for Harness lifecycle and contract tests.

use async_trait::async_trait;
use forge_provider_api::{
    ProviderError, ProviderFailure, ProviderFailureKind, ProviderOperation,
    approval::ApprovalMode,
    compaction::CompactionResult,
    completion::ProviderCompletion,
    events::{self, ProviderEventSink, ProviderEventStream},
    lifecycle::{
        ExecutionFailure, ExecutionObservation, ExecutionResult, ExecutionUpdate,
        ObservationFailure,
    },
    permissions::{PermissionRequest, PermissionResponse, PermissionTarget},
    provider::Provider,
    session::{
        BindingId, ContinuationOutcome, InputAcceptance, PromptRequest, RecoveryToken,
        SessionBinding, SessionRequest, SteeringRequest,
    },
    testing::{
        PendingTestResponse, ProviderTestControl, TestControlCommand, TestControlResult,
        TestHoldId, TestPromptScript, TestPromptStep, TestPromptTerminal,
    },
    turn::TurnOutcome,
    *,
};
use std::{
    collections::{HashMap, VecDeque},
    sync::Arc,
};
use tokio::sync::{Mutex, watch};

struct MockSession {
    binding: SessionBinding,
    owner: ExecutionOwner,
    sink: ProviderEventSink,
    stream: Mutex<Option<ProviderEventStream>>,
    _resource_lease: Option<Arc<dyn session::ResourceLease>>,
    selection: Mutex<session::ResourceSelection>,
    model: Mutex<session::ModelSelection>,
    access: Mutex<access::AccessPolicy>,
    approval: Mutex<ApprovalMode>,
    operation: Mutex<()>,
    admission: Mutex<()>,
    active: Mutex<Option<MockInvocation>>,
    rollback_cancelled: Mutex<bool>,
}

struct MockInvocation {
    scope: InvocationScope,
    mode: PromptMode,
    cancelled: watch::Sender<bool>,
    tasks: Arc<Mutex<MockTasks>>,
    pending: Arc<std::sync::atomic::AtomicUsize>,
    phase: MockInputPhase,
    steering_tail: Option<tokio::sync::oneshot::Receiver<()>>,
}

/// Assign each admitted steering input to the current response or a later response.
enum MockInputPhase {
    Receiving(Vec<MockSteering>),
    Responding,
}

struct MockSteering {
    text: String,
    submission: Option<SubmissionId>,
}

#[derive(Default)]
struct MockTasks {
    handles: VecDeque<tokio::task::JoinHandle<Result<(), ProviderError>>>,
    errors: Vec<ProviderError>,
}

impl MockTasks {
    /// Join retained tasks without dropping ownership if the collection waiter is cancelled.
    async fn collect(&mut self) -> Result<(), ProviderError> {
        while let Some(handle) = self.handles.front_mut() {
            let result = handle.await.unwrap_or_else(|_| {
                Err(failure(
                    ProviderOperation::CleanupExecution,
                    ProviderFailureKind::Unknown,
                ))
            });
            self.handles.pop_front();
            if let Err(error) = result {
                self.errors.push(error);
            }
        }
        match self.errors.split_first() {
            None => Ok(()),
            Some((primary, [])) => Err(primary.clone()),
            Some((primary, secondary)) => Err(ProviderError::Combined {
                primary: Box::new(primary.clone()),
                secondary: secondary.to_vec(),
            }),
        }
    }
}

/// Count admitted response workers until their futures complete or unwind.
struct MockWork(Arc<std::sync::atomic::AtomicUsize>);

impl MockWork {
    /// Create a guard for one response before its worker can be observed.
    fn new(pending: &Arc<std::sync::atomic::AtomicUsize>) -> Self {
        pending.fetch_add(1, std::sync::atomic::Ordering::AcqRel);
        Self(pending.clone())
    }
}

impl Drop for MockWork {
    fn drop(&mut self) {
        self.0.fetch_sub(1, std::sync::atomic::Ordering::AcqRel);
    }
}

#[derive(Clone)]
enum HoldState {
    Armed,
    Pending(PendingTestResponse),
    Released,
    Cancelled,
}

struct ResponseHold {
    id: TestHoldId,
    owner: ExecutionOwner,
    any_owner: bool,
    mode: Option<PromptMode>,
    state: watch::Sender<HoldState>,
}

#[derive(Default)]
struct MockControl {
    holds: std::sync::Mutex<VecDeque<Arc<ResponseHold>>>,
    scripts: std::sync::Mutex<HashMap<ExecutionOwner, VecDeque<testing::TestPromptScript>>>,
}

/// Native provider-neutral mock used by shared Harness acceptance and lifecycle flows.
pub struct MockProvider {
    bindings: Mutex<HashMap<BindingId, Arc<MockSession>>>,
    owners: Mutex<HashMap<ExecutionOwner, BindingId>>,
    control: Arc<MockControl>,
}

impl MockProvider {
    /// Construct a deterministic mock backend without Harness policy or workflow dependencies.
    pub fn new() -> Result<Self, ProviderError> {
        Ok(Self {
            bindings: Mutex::new(HashMap::new()),
            owners: Mutex::new(HashMap::new()),
            control: Arc::new(MockControl::default()),
        })
    }

    async fn binding(&self, id: &BindingId) -> Result<Arc<MockSession>, ProviderError> {
        self.bindings.lock().await.get(id).cloned().ok_or_else(|| {
            failure(
                ProviderOperation::NativeTransport,
                ProviderFailureKind::SessionMissing,
            )
        })
    }

    async fn submit(
        &self,
        binding: &BindingId,
        request: PromptRequest,
    ) -> Result<InputAcceptance, ProviderError> {
        let session = self.binding(binding).await?;
        let _admission = session.admission.lock().await;
        self.collect_finished(&session).await?;
        if request.scope.owner != session.owner || request.receipt.id() != &request.submission_id {
            return Err(failure(
                ProviderOperation::AdmitInput,
                ProviderFailureKind::Rejected,
            ));
        }
        if session.stream.lock().await.is_some() {
            return Err(failure(
                ProviderOperation::CollectEventReader,
                ProviderFailureKind::Rejected,
            ));
        }
        if let Err(error) = validate_invocation(&request) {
            return Err(reject_before_dispatch(&request.receipt, error).await);
        }
        if let Err(error) = request.turn_control.before_send(&request.scope).await {
            return Err(reject_before_dispatch(&request.receipt, error).await);
        }
        let script = self.control.take_script(&session.owner);
        if let Some(script) = &script {
            match &script.terminal {
                TestPromptTerminal::RejectedBeforeDispatch(kind) => {
                    let error = failure(ProviderOperation::AdmitInput, *kind);
                    return match request.receipt.clone().record_rejected(error.clone()).await {
                        Ok(()) => Err(ProviderError::PreparationFailed(Box::new(error))),
                        Err(cleanup) => Err(ProviderError::PreparationCleanup {
                            cause: Box::new(error),
                            cleanup: Box::new(cleanup),
                        }),
                    };
                }
                TestPromptTerminal::DeliveryUncertain => {
                    let token = RecoveryToken::new(
                        format!("mock-sending\n{}", request.submission_id.as_str()).into_bytes(),
                    )
                    .map_err(|_| {
                        failure(ProviderOperation::AdmitInput, ProviderFailureKind::Unknown)
                    })?;
                    request.receipt.clone().record_sending(Some(token)).await?;
                    request
                        .receipt
                        .clone()
                        .record_failure(ProviderError::DeliveryUncertain)
                        .await?;
                    return Err(ProviderError::DeliveryUncertain);
                }
                _ => {}
            }
        }
        let (cancelled, cancel_receiver) = watch::channel(false);
        let (accepted, acceptance) = tokio::sync::oneshot::channel();
        *session.rollback_cancelled.lock().await = false;
        let pending = Arc::new(std::sync::atomic::AtomicUsize::new(0));
        let tasks = Arc::new(Mutex::new(MockTasks::default()));
        let mut active = session.active.lock().await;
        if active.is_some() {
            return Err(failure(
                ProviderOperation::AdmitInput,
                ProviderFailureKind::Rejected,
            ));
        }
        *active = Some(MockInvocation {
            scope: request.scope.clone(),
            cancelled,
            mode: request.mode,
            tasks: tasks.clone(),
            pending: pending.clone(),
            phase: MockInputPhase::Receiving(Vec::new()),
            steering_tail: None,
        });
        let control = self.control.clone();
        let for_task = session.clone();
        let request_scope = request.scope.clone();
        let work = MockWork::new(&pending);
        let task = tokio::spawn(async move {
            let _work = work;
            run_prompt(
                for_task,
                control,
                request,
                script,
                cancel_receiver,
                accepted,
            )
            .await
        });
        debug_assert_eq!(
            active.as_ref().expect("active invocation inserted").scope,
            request_scope
        );
        drop(active);
        tasks.lock().await.handles.push_back(task);
        let accepted = acceptance.await;
        drop(_admission);
        match accepted {
            Ok(result) => result,
            Err(_) => match self.cleanup_execution(binding).await {
                Err(error) => Err(error),
                Ok(()) => Err(failure(
                    ProviderOperation::AdmitInput,
                    ProviderFailureKind::Unknown,
                )),
            },
        }
    }

    /// Collect a completed invocation before admitting its replacement.
    async fn collect_finished(&self, session: &Arc<MockSession>) -> Result<(), ProviderError> {
        let completed = {
            let mut active = session.active.lock().await;
            match active.as_mut() {
                Some(invocation)
                    if invocation
                        .pending
                        .load(std::sync::atomic::Ordering::Acquire)
                        == 0 =>
                {
                    Some(invocation.tasks.clone())
                }
                _ => None,
            }
        };
        if let Some(tasks) = completed {
            tasks.lock().await.collect().await?;
            session.active.lock().await.take();
        }
        Ok(())
    }
}

/// Reject unsupported mock input configuration before the receipt records dispatch.
fn validate_invocation(request: &PromptRequest) -> Result<(), ProviderError> {
    if request.goal.is_some() {
        return Err(
            capability::UnsupportedCapability(capability::ProviderFeature::NativeGoal).into(),
        );
    }
    validate_model_selection(&request.configuration.model)
}

/// Validate every normalized model option the deterministic mock advertises.
fn validate_model_selection(model: &session::ModelSelection) -> Result<(), ProviderError> {
    let descriptor =
        crate::registry::descriptor(&ProviderId::new("mock").expect("mock provider ID is valid"))
            .expect("mock is registered");
    model
        .service_tier
        .validate(&descriptor.capability)
        .map_err(|_| capability::UnsupportedCapability(capability::ProviderFeature::ServiceTier))?;
    if model
        .model
        .as_deref()
        .is_some_and(|model| model != "mock-model")
    {
        return Err(failure(
            ProviderOperation::ReconcileSettings,
            ProviderFailureKind::Rejected,
        ));
    }
    if model
        .context_window
        .as_deref()
        .is_some_and(|window| window != "default")
    {
        return Err(capability::UnsupportedCapability(
            capability::ProviderFeature::ContextWindowSelection,
        )
        .into());
    }
    if let generation::ReasoningSelection::Level(level) = &model.reasoning
        && !["low", "medium", "high"].contains(&level.as_str())
    {
        return Err(capability::UnsupportedCapability(
            capability::ProviderFeature::EffortSelection,
        )
        .into());
    }
    Ok(())
}

/// Record a definite no-send result while preserving an independent receipt failure.
async fn reject_before_dispatch(
    receipt: &Arc<dyn submission::InputReceipt>,
    cause: ProviderError,
) -> ProviderError {
    match receipt.clone().record_rejected(cause.clone()).await {
        Ok(()) => ProviderError::PreparationFailed(Box::new(cause)),
        Err(cleanup) => ProviderError::PreparationCleanup {
            cause: Box::new(cause),
            cleanup: Box::new(cleanup),
        },
    }
}

#[async_trait]
impl ProviderTestControl for MockControl {
    async fn dispatch(
        &self,
        owner: &ExecutionOwner,
        command: TestControlCommand,
    ) -> Result<TestControlResult, ProviderError> {
        match command {
            TestControlCommand::Hold { mode, any_owner } => {
                let id = TestHoldId::new(uuid::Uuid::new_v4().to_string()).map_err(|_| {
                    failure(ProviderOperation::AdmitInput, ProviderFailureKind::Unknown)
                })?;
                let (state, _) = watch::channel(HoldState::Armed);
                self.holds.lock().unwrap().push_back(Arc::new(ResponseHold {
                    id: id.clone(),
                    owner: owner.clone(),
                    any_owner,
                    mode,
                    state,
                }));
                Ok(TestControlResult::Held { id })
            }
            TestControlCommand::Wait { id } => {
                let mut state = self.find(owner, &id)?.state.subscribe();
                loop {
                    match state.borrow_and_update().clone() {
                        HoldState::Pending(response) => {
                            return Ok(TestControlResult::Pending { response });
                        }
                        HoldState::Cancelled => return Err(ProviderError::Cancelled),
                        HoldState::Armed | HoldState::Released => {}
                    }
                    state.changed().await.map_err(|_| {
                        failure(ProviderOperation::AdmitInput, ProviderFailureKind::Unknown)
                    })?;
                }
            }
            TestControlCommand::Release { id } => {
                let hold = self.find(owner, &id)?;
                if !matches!(*hold.state.borrow(), HoldState::Pending(_)) {
                    return Err(failure(
                        ProviderOperation::AdmitInput,
                        ProviderFailureKind::Rejected,
                    ));
                }
                hold.state.send_replace(HoldState::Released);
                self.remove(&id);
                Ok(TestControlResult::Released { id })
            }
            TestControlCommand::Cancel { id } => {
                let hold = self.find(owner, &id)?;
                hold.state.send_replace(HoldState::Cancelled);
                self.remove(&id);
                Ok(TestControlResult::Cancelled { id })
            }
            TestControlCommand::Script { script } => {
                self.scripts
                    .lock()
                    .unwrap()
                    .entry(owner.clone())
                    .or_default()
                    .push_back(script);
                Ok(TestControlResult::Scripted)
            }
        }
    }

    async fn arrive(
        &self,
        owner: &ExecutionOwner,
        mode: PromptMode,
        prompt: String,
    ) -> Result<(), ProviderError> {
        let hold = {
            let holds = self.holds.lock().unwrap();
            holds
                .iter()
                .find(|hold| {
                    (hold.any_owner || hold.owner == *owner)
                        && hold.mode.is_none_or(|expected| expected == mode)
                        && matches!(*hold.state.borrow(), HoldState::Armed)
                })
                .cloned()
        };
        let Some(hold) = hold else {
            return Ok(());
        };
        let pending = PendingTestResponse {
            id: hold.id.clone(),
            owner: owner.clone(),
            mode,
            prompt,
        };
        hold.state.send_replace(HoldState::Pending(pending));
        let mut state = hold.state.subscribe();
        loop {
            match state.borrow_and_update().clone() {
                HoldState::Released => return Ok(()),
                HoldState::Cancelled => return Err(ProviderError::Cancelled),
                HoldState::Armed | HoldState::Pending(_) => {}
            }
            state.changed().await.map_err(|_| {
                failure(ProviderOperation::AdmitInput, ProviderFailureKind::Unknown)
            })?;
        }
    }

    fn cancel_owner(&self, owner: Option<&ExecutionOwner>) {
        self.holds.lock().unwrap().retain(|hold| {
            let matched_owner = owner.is_none_or(|owner| hold.owner == *owner
                || matches!(&*hold.state.borrow(), HoldState::Pending(PendingTestResponse { owner: actual, .. }) if actual == owner));
            if matched_owner {
                hold.state.send_replace(HoldState::Cancelled);
                false
            } else { true }
        });
        if let Some(owner) = owner {
            self.scripts.lock().unwrap().remove(owner);
        } else {
            self.scripts.lock().unwrap().clear();
        }
    }

    fn take_script(&self, owner: &ExecutionOwner) -> Option<testing::TestPromptScript> {
        let mut scripts = self.scripts.lock().unwrap();
        let queue = scripts.get_mut(owner)?;
        let script = queue.pop_front();
        if queue.is_empty() {
            scripts.remove(owner);
        }
        script
    }
}

impl MockControl {
    fn find(
        &self,
        owner: &ExecutionOwner,
        id: &TestHoldId,
    ) -> Result<Arc<ResponseHold>, ProviderError> {
        self.holds
            .lock()
            .unwrap()
            .iter()
            .find(|hold| hold.owner == *owner && hold.id == *id)
            .cloned()
            .ok_or_else(|| failure(ProviderOperation::AdmitInput, ProviderFailureKind::Rejected))
    }

    fn remove(&self, id: &TestHoldId) {
        self.holds.lock().unwrap().retain(|hold| hold.id != *id);
    }
}

async fn run_prompt(
    session: Arc<MockSession>,
    control: Arc<MockControl>,
    request: PromptRequest,
    script: Option<TestPromptScript>,
    cancelled: watch::Receiver<bool>,
    accepted: tokio::sync::oneshot::Sender<Result<InputAcceptance, ProviderError>>,
) -> Result<(), ProviderError> {
    let mut progress = PromptProgress {
        scope: request.scope.clone(),
        sink: None,
        accepted: false,
        terminal_published: false,
    };
    let result = run_prompt_inner(
        session.clone(),
        control,
        request,
        script,
        cancelled,
        accepted,
        &mut progress,
    )
    .await;
    let Err(primary) = result else {
        return Ok(());
    };
    if !progress.accepted || progress.terminal_published {
        return Err(primary);
    }
    let Some(sink) = progress.sink.as_ref() else {
        return Err(primary);
    };
    match publish_unobserved_failure(&session, sink, &progress.scope, &primary).await {
        Ok(()) => Err(primary),
        Err(cleanup) => Err(ProviderError::Combined {
            primary: Box::new(primary),
            secondary: vec![cleanup],
        }),
    }
}

struct PromptProgress {
    scope: InvocationScope,
    sink: Option<ProviderEventSink>,
    accepted: bool,
    terminal_published: bool,
}

async fn run_prompt_inner(
    session: Arc<MockSession>,
    control: Arc<MockControl>,
    request: PromptRequest,
    script: Option<TestPromptScript>,
    mut cancelled: watch::Receiver<bool>,
    accepted: tokio::sync::oneshot::Sender<Result<InputAcceptance, ProviderError>>,
    progress: &mut PromptProgress,
) -> Result<(), ProviderError> {
    let _operation = session.operation.lock().await;
    let binding = &session.binding.id;
    let owner = session.owner.clone();
    let scope = request.scope.clone();
    *session.model.lock().await = request.configuration.model.clone();
    *session.access.lock().await = request.configuration.access.clone();
    *session.approval.lock().await = request.configuration.approval;
    let sink = session.sink.invocation_sink(&scope)?;
    progress.sink = Some(sink.clone());
    let token = RecoveryToken::new(
        format!("mock-sending\n{}", request.submission_id.as_str()).into_bytes(),
    )
    .map_err(|_| failure(ProviderOperation::AdmitInput, ProviderFailureKind::Unknown))?;
    request
        .receipt
        .clone()
        .record_sending(Some(token))
        .await
        .map_err(|error| ProviderError::PreparationFailed(Box::new(error)))?;
    let provider_session_id = session
        .binding
        .provider_session_id
        .as_ref()
        .map(|id| id.as_str())
        .unwrap_or_default();
    request
        .receipt
        .clone()
        .record_accepted(Some(
            RecoveryToken::new(
                format!(
                    "mock-accepted\n{}\n{provider_session_id}",
                    request.submission_id.as_str()
                )
                .into_bytes(),
            )
            .map_err(|_| failure(ProviderOperation::AdmitInput, ProviderFailureKind::Unknown))?,
        ))
        .await?;
    progress.accepted = true;
    let acceptance = InputAcceptance {
        submission_id: request.submission_id.clone(),
    };
    let _ = accepted.send(Ok(acceptance));

    let thread_id = session
        .binding
        .provider_session_id
        .as_ref()
        .map(|id| id.as_str().to_owned())
        .unwrap_or_else(|| "mock-session".into());
    let turn = TurnAddress {
        scope: scope.clone(),
        thread_id,
        turn_id: uuid::Uuid::new_v4().to_string().into(),
    };
    sink.send_wait(ProviderEvent::Execution {
        observation: lifecycle(
            &scope,
            ExecutionUpdate::Started {
                address: turn.clone(),
            },
        )?,
    })
    .await?;
    sink.send_wait(ProviderEvent::Turn {
        address: turn.clone(),
        update: TurnUpdate::Started {
            started_at_ms: now_ms(),
        },
    })
    .await?;
    let mut prompt_parts = Vec::new();
    for (index, recovered) in request.recovered_inputs.iter().enumerate() {
        prompt_parts.push(format!(
            "Earlier user input {} was saved locally but not accepted:",
            index + 1
        ));
        if !recovered.instructions.is_empty() {
            prompt_parts.push(recovered.instructions.clone());
        }
        prompt_parts.extend(
            recovered
                .attachments
                .iter()
                .map(|attachment| attachment.context.clone()),
        );
        prompt_parts.push(render_input(&recovered.input));
    }
    if !request.recovered_inputs.is_empty() {
        prompt_parts.push("Newest user input:".into());
    }
    if !request.instructions.is_empty() {
        prompt_parts.push(request.instructions.clone());
    }
    if let Some(task) = &request.task {
        prompt_parts.push(task.heading());
    }
    prompt_parts.extend(
        request
            .attachments
            .iter()
            .map(|attachment| attachment.context.clone()),
    );
    prompt_parts.push(render_input(&request.input));
    let prompt = prompt_parts.join("\n\n");
    let hold = tokio::select! {
        result = control.arrive(&owner, request.mode, prompt.clone()) => result,
        _ = cancelled.wait_for(|value| *value) => Err(ProviderError::Cancelled),
    };
    let steering = {
        let mut active = session.active.lock().await;
        let invocation = active
            .as_mut()
            .filter(|active| active.scope == scope)
            .ok_or_else(|| failure(ProviderOperation::AdmitInput, ProviderFailureKind::Rejected))?;
        match std::mem::replace(&mut invocation.phase, MockInputPhase::Responding) {
            MockInputPhase::Receiving(steering) => steering,
            MockInputPhase::Responding => Vec::new(),
        }
    };
    let mut submissions = vec![request.submission_id.clone()];
    submissions.extend(steering.iter().filter_map(|input| input.submission.clone()));
    if let Err(error) = hold {
        if matches!(error, ProviderError::Cancelled) {
            publish_terminal(
                &session,
                &sink,
                &scope,
                Some(turn.clone()),
                TurnOutcome::Cancelled,
                &submissions,
                None,
            )
            .await?;
            progress.terminal_published = true;
            *session.rollback_cancelled.lock().await = true;
            return Ok(());
        }
        return Err(error);
    }

    let script = match script {
        Some(script) => script,
        None => {
            let steering = if steering.is_empty() {
                String::new()
            } else {
                format!(
                    "\n\nSteering constraints:\n{}",
                    steering
                        .iter()
                        .map(|input| format!("- {}", input.text))
                        .collect::<Vec<_>>()
                        .join("\n")
                )
            };
            TestPromptScript {
                steps: vec![TestPromptStep::Message {
                    role: MessageRole::Response,
                    text: format!("Mock response: {prompt}{steering}"),
                }],
                terminal: TestPromptTerminal::Completed,
            }
        }
    };
    let steps = if script.steps.is_empty() {
        vec![TestPromptStep::Message {
            role: MessageRole::Response,
            text: format!("Mock response: {prompt}"),
        }]
    } else {
        script.steps.clone()
    };
    for step in steps {
        match step {
            TestPromptStep::Message { role, text } => {
                let address = MessageAddress {
                    turn: turn.clone(),
                    message_id: MessageId::Synthetic(uuid::Uuid::new_v4().to_string()),
                };
                sink.send_wait(ProviderEvent::Message {
                    address: address.clone(),
                    update: MessageUpdate::Started {
                        classification: MessageClassification::Known(role),
                        text: text.clone(),
                    },
                })
                .await?;
                sink.send_wait(ProviderEvent::Message {
                    address,
                    update: MessageUpdate::Completed { text },
                })
                .await?;
            }
            TestPromptStep::ToolCall { name, arguments } => {
                if let Err(error) = run_tool_call(&request, &sink, &turn, &name, arguments).await {
                    if matches!(error, ProviderError::Cancelled) {
                        publish_terminal(
                            &session,
                            &sink,
                            &scope,
                            Some(turn),
                            TurnOutcome::Cancelled,
                            &submissions,
                            None,
                        )
                        .await?;
                        progress.terminal_published = true;
                        return Ok(());
                    }
                    return Err(error);
                }
            }
        }
    }
    let terminal = script.terminal.clone();
    let scripted_outcome = match &terminal {
        TestPromptTerminal::Completed => TurnOutcome::Completed,
        TestPromptTerminal::Cancelled => TurnOutcome::Cancelled,
        TestPromptTerminal::Failed(_) => TurnOutcome::Failed,
        TestPromptTerminal::ObservationLost(_) => TurnOutcome::Interrupted,
        TestPromptTerminal::RejectedBeforeDispatch(_) | TestPromptTerminal::DeliveryUncertain => {
            unreachable!("pre-dispatch terminals are handled before task creation")
        }
    };
    let outcome = match request.turn_control.close(&scope).await {
        Ok(()) => scripted_outcome,
        Err(_) => {
            publish_publication_failed(
                &session,
                &sink,
                &scope,
                turn,
                &submissions,
                scripted_outcome,
                session::HostEffectFailure::Uncertain,
            )
            .await?;
            progress.terminal_published = true;
            return Ok(());
        }
    };
    if let Err(host_failure) = request
        .host_effects
        .settle_foreground(binding, &scope)
        .await
    {
        publish_publication_failed(
            &session,
            &sink,
            &scope,
            turn,
            &submissions,
            outcome,
            host_failure,
        )
        .await?;
        progress.terminal_published = true;
        return Ok(());
    }
    let status = match request.turn_control.status(&scope).await {
        Ok(status) => status,
        Err(_) => {
            publish_publication_failed(
                &session,
                &sink,
                &scope,
                turn,
                &submissions,
                outcome,
                session::HostEffectFailure::Uncertain,
            )
            .await?;
            progress.terminal_published = true;
            return Ok(());
        }
    };
    let outcome = if status.cancelled {
        TurnOutcome::Cancelled
    } else if status.error.is_some() {
        TurnOutcome::Failed
    } else {
        outcome
    };
    if let TestPromptTerminal::ObservationLost(failure) = terminal.clone() {
        sink.send_wait(ProviderEvent::Turn {
            address: turn.clone(),
            update: TurnUpdate::Finished {
                outcome: TurnOutcome::Interrupted,
                completed_at_ms: now_ms(),
            },
        })
        .await?;
        sink.send_wait(ProviderEvent::Execution {
            observation: lifecycle(
                &scope,
                ExecutionUpdate::ObservationLost {
                    failure,
                    completion: Some(completion(&session, None).await.into()),
                },
            )?,
        })
        .await?;
        progress.terminal_published = true;
        return Ok(());
    }
    let failure = match terminal {
        TestPromptTerminal::Failed(kind) => Some(kind),
        _ => None,
    };
    publish_terminal(
        &session,
        &sink,
        &scope,
        Some(turn),
        outcome,
        &submissions,
        failure,
    )
    .await?;
    progress.terminal_published = true;
    Ok(())
}

/// Publish a later accepted input on the retained session stream after foreground settlement.
async fn run_steering(
    session: Arc<MockSession>,
    control: Arc<MockControl>,
    scope: InvocationScope,
    mode: PromptMode,
    input: MockSteering,
    mut cancelled: watch::Receiver<bool>,
) -> Result<(), ProviderError> {
    let _operation = session.operation.lock().await;
    let sink = session.sink.invocation_sink(&scope)?;
    let result = async {
        let turn = TurnAddress {
            scope: scope.clone(),
            thread_id: session
                .binding
                .provider_session_id
                .as_ref()
                .map(|id| id.as_str().to_owned())
                .unwrap_or_default(),
            turn_id: uuid::Uuid::new_v4().to_string().into(),
        };
        sink.send_wait(ProviderEvent::Execution {
            observation: lifecycle(
                &scope,
                ExecutionUpdate::Started {
                    address: turn.clone(),
                },
            )?,
        })
        .await?;
        sink.send_wait(ProviderEvent::Turn {
            address: turn.clone(),
            update: TurnUpdate::Started {
                started_at_ms: now_ms(),
            },
        })
        .await?;
        let held = if *cancelled.borrow() {
            Err(ProviderError::Cancelled)
        } else {
            tokio::select! {
                result = control.arrive(&session.owner, mode, input.text.clone()) => result,
                _ = cancelled.wait_for(|value| *value) => Err(ProviderError::Cancelled),
            }
        };
        let outcome = match held {
            Ok(()) => {
                let text = format!("Mock response: {}", input.text);
                let address = MessageAddress {
                    turn: turn.clone(),
                    message_id: MessageId::Synthetic(uuid::Uuid::new_v4().to_string()),
                };
                sink.send_wait(ProviderEvent::Message {
                    address: address.clone(),
                    update: MessageUpdate::Started {
                        classification: MessageClassification::Known(MessageRole::Response),
                        text: text.clone(),
                    },
                })
                .await?;
                sink.send_wait(ProviderEvent::Message {
                    address,
                    update: MessageUpdate::Completed { text },
                })
                .await?;
                TurnOutcome::Completed
            }
            Err(ProviderError::Cancelled) => TurnOutcome::Cancelled,
            Err(error) => return Err(error),
        };
        let submissions: Vec<_> = input.submission.into_iter().collect();
        publish_terminal(
            &session,
            &sink,
            &scope,
            Some(turn),
            outcome,
            &submissions,
            None,
        )
        .await
    }
    .await;
    match result {
        Ok(()) => Ok(()),
        Err(primary) => match publish_unobserved_failure(&session, &sink, &scope, &primary).await {
            Ok(()) => Err(primary),
            Err(cleanup) => Err(ProviderError::Combined {
                primary: Box::new(primary),
                secondary: vec![cleanup],
            }),
        },
    }
}

/// End an accepted invocation's stream when no native terminal outcome was published.
async fn publish_unobserved_failure(
    session: &MockSession,
    sink: &ProviderEventSink,
    scope: &InvocationScope,
    error: &ProviderError,
) -> Result<(), ProviderError> {
    let failure = if matches!(error, ProviderError::DeliveryUncertain) {
        ObservationFailure::DeliveryUncertain
    } else {
        ObservationFailure::Protocol
    };
    sink.send_wait(ProviderEvent::Execution {
        observation: lifecycle(
            scope,
            ExecutionUpdate::ObservationLost {
                failure,
                completion: Some(completion(session, None).await.into()),
            },
        )?,
    })
    .await?;
    Ok(())
}

async fn run_tool_call(
    request: &PromptRequest,
    sink: &ProviderEventSink,
    turn: &TurnAddress,
    name: &str,
    arguments: serde_json::Value,
) -> Result<(), ProviderError> {
    if !request.tools.iter().any(|tool| tool.name == name) {
        return Err(failure(
            ProviderOperation::AdmitInput,
            ProviderFailureKind::Rejected,
        ));
    }
    let address = ToolAddress {
        turn: turn.clone(),
        tool_id: uuid::Uuid::new_v4().to_string().into(),
    };
    let invocation = ToolInvocation {
        semantics: Default::default(),
        kind: ToolActivityKind::ToolCall,
        name: name.into(),
        title: name.into(),
        arguments: arguments.clone(),
    };
    sink.send_wait(ProviderEvent::Tool {
        address: address.clone(),
        update: ToolUpdate::Requested { invocation },
    })
    .await?;
    let permission = PermissionRequest {
        id: uuid::Uuid::new_v4().to_string(),
        provider: ProviderId::new("mock").expect("mock ID is valid"),
        reason: None,
        target_list: vec![PermissionTarget::HostTool { name: name.into() }],
    };
    request.permissions.before_tool(&request.scope).await?;
    match request
        .permissions
        .authorize(&request.scope, permission)
        .await
    {
        PermissionResponse::Allow => {
            sink.send_wait(ProviderEvent::Tool {
                address: address.clone(),
                update: ToolUpdate::Started,
            })
            .await?;
            match request
                .provider_tools
                .call(
                    &request.scope,
                    control::ToolCall {
                        name: name.into(),
                        arguments,
                    },
                )
                .await
            {
                Ok(output) => {
                    sink.send_wait(ProviderEvent::Tool {
                        address: address.clone(),
                        update: ToolUpdate::Output {
                            update: TextUpdate::Replace(output),
                        },
                    })
                    .await?;
                    sink.send_wait(ProviderEvent::Tool {
                        address,
                        update: ToolUpdate::Finished {
                            outcome: ToolOutcome::Succeeded,
                        },
                    })
                    .await?;
                }
                Err(_) => {
                    sink.send_wait(ProviderEvent::Tool {
                        address,
                        update: ToolUpdate::Finished {
                            outcome: ToolOutcome::Failed {
                                diagnostic: ToolDiagnostic {
                                    kind: ProviderFailureKind::Unknown,
                                    message: diagnostic::SanitizedMessage::new("host tool failed")
                                        .expect("static diagnostic is valid"),
                                },
                            },
                        },
                    })
                    .await?
                }
            }
        }
        PermissionResponse::Deny { reason } => {
            sink.send_wait(ProviderEvent::Tool {
                address,
                update: ToolUpdate::Finished {
                    outcome: ToolOutcome::Denied {
                        reason: forge_provider_api::diagnostic::SanitizedMessage::new(
                            reason
                                .filter(|text| !text.trim().is_empty())
                                .unwrap_or_else(|| "Permission denied".into()),
                        )
                        .expect("denial fallback is nonempty"),
                    },
                },
            })
            .await?;
        }
        PermissionResponse::Cancelled => {
            sink.send_wait(ProviderEvent::Tool {
                address,
                update: ToolUpdate::Finished {
                    outcome: ToolOutcome::Cancelled,
                },
            })
            .await?;
            return Err(ProviderError::Cancelled);
        }
    }
    Ok(())
}

async fn publish_publication_failed(
    session: &MockSession,
    sink: &ProviderEventSink,
    scope: &InvocationScope,
    address: TurnAddress,
    submissions: &[SubmissionId],
    outcome: TurnOutcome,
    failure: session::HostEffectFailure,
) -> Result<(), ProviderError> {
    sink.send_wait(ProviderEvent::Turn {
        address: address.clone(),
        update: TurnUpdate::Finished {
            outcome,
            completed_at_ms: now_ms(),
        },
    })
    .await?;
    let completion = completion(session, None).await;
    let result = match outcome {
        TurnOutcome::Completed => ExecutionResult::Completed {
            completion: completion.into(),
        },
        TurnOutcome::Cancelled => ExecutionResult::Cancelled {
            completion: completion.into(),
        },
        TurnOutcome::Failed | TurnOutcome::Interrupted => ExecutionResult::Failed {
            completion: completion.into(),
            failure: ExecutionFailure::Terminal {
                kind: ProviderFailureKind::Unknown,
            },
        },
    };
    let update = ExecutionUpdate::PublicationFailed {
        address,
        covered_submissions: submissions.to_vec(),
        completed_at_ms: now_ms(),
        result,
        failure,
    };
    sink.send_wait(ProviderEvent::Execution {
        observation: lifecycle(scope, update)?,
    })
    .await?;
    Ok(())
}

async fn publish_terminal(
    session: &MockSession,
    sink: &ProviderEventSink,
    scope: &InvocationScope,
    address: Option<TurnAddress>,
    outcome: TurnOutcome,
    submissions: &[SubmissionId],
    failure_kind: Option<ProviderFailureKind>,
) -> Result<(), ProviderError> {
    let completion = completion(session, None).await;
    if let Some(address) = address.clone() {
        sink.send_wait(ProviderEvent::Turn {
            address: address.clone(),
            update: TurnUpdate::Finished {
                outcome,
                completed_at_ms: now_ms(),
            },
        })
        .await?;
    }
    let result = match outcome {
        TurnOutcome::Completed => ExecutionResult::Completed {
            completion: completion.into(),
        },
        TurnOutcome::Cancelled => ExecutionResult::Cancelled {
            completion: completion.into(),
        },
        TurnOutcome::Failed | TurnOutcome::Interrupted => ExecutionResult::Failed {
            completion: completion.into(),
            failure: ExecutionFailure::Terminal {
                kind: failure_kind.unwrap_or(ProviderFailureKind::Unknown),
            },
        },
    };
    let update = if let Some(address) = address {
        ExecutionUpdate::Finished {
            address,
            covered_submissions: submissions.to_vec(),
            completed_at_ms: now_ms(),
            result,
        }
    } else {
        ExecutionUpdate::ResolvedWithoutExecution {
            covered_submissions: submissions.to_vec(),
            completed_at_ms: now_ms(),
            result,
        }
    };
    sink.send_wait(ProviderEvent::Execution {
        observation: lifecycle(scope, update)?,
    })
    .await?;
    Ok(())
}

fn lifecycle(
    scope: &InvocationScope,
    update: ExecutionUpdate,
) -> Result<ExecutionObservation, ProviderError> {
    ExecutionObservation::new(scope.clone(), update).map_err(Into::into)
}

fn render_input(input: &PromptInput) -> String {
    match input {
        PromptInput::Text(text) => text.clone(),
        PromptInput::Skill { name, arguments } => format!("/{name} {arguments}"),
        PromptInput::QuestionReply(reply) => {
            let response = match &reply.response {
                QuestionReplyResponse::Answer {
                    answer,
                    required_questions_remain,
                } => format!(
                    "Answer: {answer}\nRequired questions remain: {required_questions_remain}"
                ),
                QuestionReplyResponse::Clarification {
                    options,
                    text,
                    reply_to,
                } => {
                    let options = options
                        .iter()
                        .map(|option| {
                            format!(
                                "{} [{}]: {}{}",
                                option.label,
                                option.id,
                                option.description,
                                if option.recommended {
                                    " (recommended)"
                                } else {
                                    ""
                                }
                            )
                        })
                        .collect::<Vec<_>>()
                        .join("\n");
                    format!(
                        "Clarification: {text}\nOptions:\n{options}\nReply to: {}",
                        reply_to.as_deref().unwrap_or("none")
                    )
                }
                QuestionReplyResponse::Correction {
                    answer,
                    required_questions_remain,
                } => format!(
                    "Correction: {answer}\nRequired questions remain: {required_questions_remain}"
                ),
            };
            format!(
                "Question response for {} (revision {}, message {}):\n{}\n{}",
                reply.question_id, reply.revision, reply.message_id, reply.question, response
            )
        }
    }
}

fn now_ms() -> i64 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default()
        .as_millis() as i64
}

async fn completion(session: &MockSession, model: Option<String>) -> ProviderCompletion {
    let kind = ProviderId::new("mock").expect("mock provider ID is valid");
    let selected_model = session.model.lock().await.model.clone();
    ProviderCompletion {
        provider_session_id: session.binding.provider_session_id.clone(),
        capability: crate::registry::descriptor(&kind)
            .expect("mock provider is registered")
            .capability,
        runtime: ProviderRuntime {
            provider: "Mock provider".into(),
            model: model.or(selected_model),
        },
        ..Default::default()
    }
}

fn failure(operation: ProviderOperation, kind: ProviderFailureKind) -> ProviderError {
    ProviderError::Failure(ProviderFailure::new(operation, kind))
}

#[async_trait]
impl Provider for MockProvider {
    async fn initialize_session(
        &self,
        request: SessionRequest,
    ) -> Result<SessionBinding, ProviderError> {
        let existing = self.owners.lock().await.get(&request.owner).cloned();
        if let Some(existing) = existing {
            self.retire_session(&existing).await?;
        }
        let binding_id = BindingId::new(uuid::Uuid::new_v4().to_string()).map_err(|_| {
            failure(
                ProviderOperation::InitializeSession,
                ProviderFailureKind::Unknown,
            )
        })?;
        let provider_session_id = request.provider_session_id.or_else(|| {
            Some(
                uuid::Uuid::new_v4()
                    .to_string()
                    .try_into()
                    .expect("UUID is nonempty"),
            )
        });
        let binding = SessionBinding {
            id: binding_id.clone(),
            provider_session_id,
        };
        let owner_scope = EventScope::SessionOperation {
            owner: request.owner.clone(),
            operation_id: binding_id.as_str().to_owned(),
        };
        let (sink, stream) = events::channel(binding_id.clone(), owner_scope);
        let session = Arc::new(MockSession {
            binding: binding.clone(),
            owner: request.owner.clone(),
            sink,
            stream: Mutex::new(Some(stream)),
            _resource_lease: request.resource_lease,
            selection: Mutex::new(request.resources),
            model: Mutex::new(request.model),
            access: Mutex::new(request.access),
            approval: Mutex::new(request.approval),
            operation: Mutex::new(()),
            admission: Mutex::new(()),
            active: Mutex::new(None),
            rollback_cancelled: Mutex::new(false),
        });
        self.bindings
            .lock()
            .await
            .insert(binding_id.clone(), session);
        self.owners.lock().await.insert(request.owner, binding_id);
        Ok(binding)
    }

    async fn take_session_events(
        &self,
        binding: &BindingId,
    ) -> Result<ProviderEventStream, ProviderError> {
        self.binding(binding)
            .await?
            .stream
            .lock()
            .await
            .take()
            .ok_or_else(|| {
                failure(
                    ProviderOperation::CollectEventReader,
                    ProviderFailureKind::Rejected,
                )
            })
    }

    async fn session_prompt(
        &self,
        binding: &BindingId,
        request: PromptRequest,
    ) -> Result<InputAcceptance, ProviderError> {
        self.submit(binding, request).await
    }

    async fn continue_session(
        &self,
        binding: &BindingId,
        request: session::ContinuationRequest,
    ) -> Result<ContinuationOutcome, ProviderError> {
        match request {
            session::ContinuationRequest::NotConsumed { request, .. } => Ok(
                ContinuationOutcome::InputAccepted(self.submit(binding, *request).await?),
            ),
            session::ContinuationRequest::RetryableExecution { .. } => Err(failure(
                ProviderOperation::AdmitInput,
                ProviderFailureKind::Rejected,
            )),
        }
    }

    async fn session_steer(
        &self,
        binding: &BindingId,
        request: SteeringRequest,
    ) -> Result<session::SteeringAcceptance, ProviderError> {
        let session = self.binding(binding).await?;
        let _admission = session.admission.lock().await;
        {
            let active = session.active.lock().await;
            if request.scope.owner != session.owner
                || active.as_ref().is_none_or(|active| {
                    active.scope != request.scope || *active.cancelled.borrow()
                })
            {
                return Err(failure(
                    ProviderOperation::AdmitInput,
                    ProviderFailureKind::Rejected,
                ));
            }
        }
        let submission_id = request.delivery.submission_id().cloned();
        if let (Some(submission_id), Some(receipt)) = (&submission_id, request.delivery.receipt()) {
            let token = RecoveryToken::new(
                format!("mock-steering\n{}", submission_id.as_str()).into_bytes(),
            )
            .map_err(|_| failure(ProviderOperation::AdmitInput, ProviderFailureKind::Unknown))?;
            receipt.clone().record_sending(Some(token)).await?;
            receipt.clone().record_accepted(None).await?;
        }
        let input = render_input(&request.input);
        let input = request.task.as_ref().map_or(input.clone(), |task| {
            format!("{}\n\n{input}", task.heading())
        });
        let attachments = request
            .attachments
            .iter()
            .map(|attachment| attachment.context.as_str())
            .collect::<Vec<_>>()
            .join("\n");
        let input = if attachments.is_empty() {
            input
        } else {
            format!("{attachments}\n\n{input}")
        };
        let mut active = session.active.lock().await;
        let invocation = active.as_mut().expect("admission retains the invocation");
        let steering = MockSteering {
            text: input,
            submission: submission_id.clone(),
        };
        match &mut invocation.phase {
            MockInputPhase::Receiving(pending) => pending.push(steering),
            MockInputPhase::Responding => {
                let work = MockWork::new(&invocation.pending);
                let tasks = invocation.tasks.clone();
                let scope = invocation.scope.clone();
                let mode = invocation.mode;
                let cancelled = invocation.cancelled.subscribe();
                let predecessor = invocation.steering_tail.take();
                let (completed, next) = tokio::sync::oneshot::channel();
                invocation.steering_tail = Some(next);
                let session = session.clone();
                let control = self.control.clone();
                drop(active);
                let task = tokio::spawn(async move {
                    let _work = work;
                    if let Some(predecessor) = predecessor {
                        let _ = predecessor.await;
                    }
                    let result =
                        run_steering(session, control, scope, mode, steering, cancelled).await;
                    let _ = completed.send(());
                    result
                });
                tasks.lock().await.handles.push_back(task);
            }
        }
        Ok(match submission_id {
            Some(submission_id) => session::SteeringAcceptance::Journaled { submission_id },
            None => session::SteeringAcceptance::Context,
        })
    }

    async fn retire_session(&self, binding: &BindingId) -> Result<(), ProviderError> {
        let session = self.binding(binding).await?;
        self.cleanup_execution(binding).await?;
        self.bindings.lock().await.remove(binding);
        self.owners.lock().await.remove(&session.owner);
        Ok(())
    }

    async fn fence_session(&self, binding: &BindingId) -> Result<(), ProviderError> {
        self.binding(binding)
            .await?
            .sink
            .fail(events::EventDeliveryError::Observation(
                ObservationFailure::Disconnected,
            ));
        self.cleanup_execution(binding).await
    }

    async fn recover_session(
        &self,
        mut request: SessionRequest,
        token: Option<RecoveryToken>,
    ) -> Result<SessionBinding, ProviderError> {
        if let Some(token) = token {
            let encoded = std::str::from_utf8(token.as_bytes()).map_err(|_| {
                failure(
                    ProviderOperation::RecoverSession,
                    ProviderFailureKind::Protocol,
                )
            })?;
            let mut fields = encoded.splitn(3, '\n');
            if fields.next() != Some("mock-accepted") || fields.next().is_none() {
                return Err(failure(
                    ProviderOperation::RecoverSession,
                    ProviderFailureKind::Rejected,
                ));
            }
            let provider_session_id = fields
                .next()
                .filter(|id| !id.is_empty())
                .ok_or_else(|| {
                    failure(
                        ProviderOperation::RecoverSession,
                        ProviderFailureKind::Protocol,
                    )
                })?
                .to_owned()
                .try_into()
                .map_err(|_| {
                    failure(
                        ProviderOperation::RecoverSession,
                        ProviderFailureKind::Protocol,
                    )
                })?;
            if request
                .provider_session_id
                .as_ref()
                .is_some_and(|current| current != &provider_session_id)
            {
                return Err(failure(
                    ProviderOperation::RecoverSession,
                    ProviderFailureKind::Rejected,
                ));
            }
            request.provider_session_id = Some(provider_session_id);
        }
        self.initialize_session(request).await
    }

    async fn reconcile_submission(
        &self,
        submission_id: &SubmissionId,
        token: Option<&RecoveryToken>,
        accepted_receipts: &[RecoveryToken],
    ) -> Result<SubmissionEvidence, ProviderError> {
        let Some(token) = token else {
            return Ok(SubmissionEvidence::Unknown);
        };
        let expected = format!("mock-sending\n{}", submission_id.as_str());
        if token.as_bytes() != expected.as_bytes() {
            return Ok(SubmissionEvidence::Unknown);
        }
        let accepted = accepted_receipts.iter().find_map(|receipt| {
            let encoded = std::str::from_utf8(receipt.as_bytes()).ok()?;
            let mut fields = encoded.splitn(3, '\n');
            if fields.next()? != "mock-accepted" || fields.next()? != submission_id.as_str() {
                return None;
            }
            let provider_session_id = fields.next()?.to_owned().try_into().ok()?;
            Some((receipt.clone(), provider_session_id))
        });
        Ok(match accepted {
            Some((receipt, provider_session_id)) => SubmissionEvidence::Accepted {
                receipt: Some(receipt),
                provider_session_id: Some(provider_session_id),
            },
            None => SubmissionEvidence::Unknown,
        })
    }

    async fn shutdown(&self) -> Result<(), ProviderError> {
        self.control.cancel_owner(None);
        let ids = self
            .bindings
            .lock()
            .await
            .keys()
            .cloned()
            .collect::<Vec<_>>();
        for id in ids {
            self.retire_session(&id).await?;
        }
        Ok(())
    }

    fn descriptor(&self) -> ProviderDescriptor {
        crate::registry::descriptor(&ProviderId::new("mock").expect("mock ID is valid"))
            .expect("mock is registered")
    }

    fn test_control(&self) -> Option<Arc<dyn ProviderTestControl>> {
        Some(self.control.clone())
    }

    async fn model_list(
        &self,
        request: catalog::ProviderCatalogRequest,
    ) -> Result<Vec<ProviderModel>, ProviderError> {
        let owner = request.owner().map_err(|_| {
            failure(
                ProviderOperation::ReconcileSettings,
                ProviderFailureKind::Rejected,
            )
        })?;
        let binding = self.owners.lock().await.get(&owner).cloned();
        let selected = if let Some(binding) = binding {
            Some(self.binding(&binding).await?)
        } else {
            None
        };
        let (selected_reasoning, selected_context_window) = if let Some(session) = selected {
            let model = session.model.lock().await;
            let reasoning = match &model.reasoning {
                generation::ReasoningSelection::Default => Some("medium".into()),
                generation::ReasoningSelection::Lowest => Some("low".into()),
                generation::ReasoningSelection::Level(level) => Some(level.clone()),
            };
            (reasoning, model.context_window.clone())
        } else {
            (None, None)
        };
        Ok(vec![ProviderModel {
            service_tiers: vec![
                ServiceTier::Standard,
                ServiceTier::Fast,
                ServiceTier::Ultrafast,
            ],
            id: "mock-model".into(),
            reasoning: vec!["low".into(), "medium".into(), "high".into()],
            default_reasoning: Some("medium".into()),
            selected_reasoning,
            context_window: vec![ProviderContextWindow {
                id: "default".into(),
                token_limit: Some(100_000),
            }],
            default_context_window: Some("default".into()),
            selected_context_window,
            vision: false,
            description: Some("Deterministic provider-neutral test model.".into()),
            is_default: true,
        }])
    }

    async fn generate_text(&self, request: TextGenerationRequest) -> Result<String, ProviderError> {
        request
            .validator
            .validate("The requested change is ready for review.")
    }

    async fn capabilities(
        &self,
        _request: catalog::ProviderCatalogRequest,
    ) -> Result<capability::CapabilityManifest, ProviderError> {
        Ok(capability::CapabilityManifest::from_descriptor(
            self.descriptor(),
        ))
    }

    async fn reconcile_settings(
        &self,
        request: catalog::ProviderCatalogRequest,
        settings: session::SessionSettings,
        resources: session::ResourceSelection,
    ) -> Result<(), ProviderError> {
        let owner = request.owner().map_err(|_| {
            failure(
                ProviderOperation::ReconcileSettings,
                ProviderFailureKind::Rejected,
            )
        })?;
        let binding = self
            .owners
            .lock()
            .await
            .get(&owner)
            .cloned()
            .ok_or_else(|| {
                failure(
                    ProviderOperation::ReconcileSettings,
                    ProviderFailureKind::SessionMissing,
                )
            })?;
        let session = self.binding(&binding).await?;
        let _operation = session.operation.lock().await;
        if session.active.lock().await.is_some() {
            return Err(failure(
                ProviderOperation::ReconcileSettings,
                ProviderFailureKind::Rejected,
            ));
        }
        if let Some(model) = &settings.model {
            validate_model_selection(model)?;
        }
        *session.selection.lock().await = resources;
        if let Some(model) = settings.model {
            *session.model.lock().await = model;
        }
        if let Some(access) = settings.access {
            *session.access.lock().await = access;
        }
        if let Some(approval) = settings.approval {
            *session.approval.lock().await = approval;
        }
        Ok(())
    }

    async fn skill_list(
        &self,
        request: catalog::ProviderCatalogRequest,
    ) -> Result<Vec<catalog::SkillDefinition>, ProviderError> {
        let owner = request.owner().map_err(|_| {
            failure(
                ProviderOperation::ReconcileSettings,
                ProviderFailureKind::Rejected,
            )
        })?;
        let Some(binding) = self.owners.lock().await.get(&owner).cloned() else {
            return Ok(Vec::new());
        };
        Ok(self
            .binding(&binding)
            .await?
            .selection
            .lock()
            .await
            .skills
            .clone())
    }

    async fn mcp_configuration(
        &self,
        request: catalog::ProviderCatalogRequest,
    ) -> Result<Vec<catalog::McpDefinition>, ProviderError> {
        let owner = request.owner().map_err(|_| {
            failure(
                ProviderOperation::ReconcileSettings,
                ProviderFailureKind::Rejected,
            )
        })?;
        let Some(binding) = self.owners.lock().await.get(&owner).cloned() else {
            return Ok(Vec::new());
        };
        let session = self.binding(&binding).await?;
        Ok(session
            .selection
            .lock()
            .await
            .mcp
            .iter()
            .map(|launch| catalog::McpDefinition {
                resource_id: None,
                scope: None,
                source_path: None,
                name: launch.name.clone(),
                transport: match &launch.transport {
                    forge_provider_api::session::McpTransport::Stdio { .. } => {
                        catalog::McpTransportKind::Stdio
                    }
                    forge_provider_api::session::McpTransport::Http { .. } => {
                        catalog::McpTransportKind::Http
                    }
                    forge_provider_api::session::McpTransport::Sse { .. } => {
                        catalog::McpTransportKind::Sse
                    }
                },
                enabled: true,
                status: catalog::McpStatus::Connected,
                status_detail: None,
                token_count: None,
                token_estimated: false,
                tools: Vec::new(),
                tool_error: None,
            })
            .collect())
    }

    async fn mcp_list(
        &self,
        request: catalog::ProviderCatalogRequest,
    ) -> Result<Vec<catalog::McpDefinition>, ProviderError> {
        self.mcp_configuration(request).await
    }

    async fn fork(
        &self,
        request: session::ConversationForkRequest,
    ) -> Result<ProviderForkResult, ProviderError> {
        let source_owner = request.source.owner().map_err(|_| {
            failure(
                ProviderOperation::NativeTransport,
                ProviderFailureKind::Rejected,
            )
        })?;
        if request.target.owner == source_owner {
            return Err(failure(
                ProviderOperation::NativeTransport,
                ProviderFailureKind::Rejected,
            ));
        }
        if request.checkpoint.is_some() {
            return Err(capability::UnsupportedCapability(
                capability::ProviderFeature::CheckpointFork,
            )
            .into());
        }
        validate_model_selection(&request.target.configuration.model)?;
        let id = request
            .source
            .provider_session_id
            .map(String::from)
            .unwrap_or_else(|| "mock-session".into());
        Ok(ProviderForkResult::unprofiled(
            format!("{id}-fork").try_into().map_err(|_| {
                failure(
                    ProviderOperation::NativeTransport,
                    ProviderFailureKind::Unknown,
                )
            })?,
        ))
    }

    async fn compact(
        &self,
        binding: &BindingId,
        request: CompactionRequest,
    ) -> Result<Compaction, ProviderError> {
        let session = self.binding(binding).await?;
        let _operation = session.operation.lock().await;
        if session.active.lock().await.is_some() {
            return Err(failure(
                ProviderOperation::AdmitInput,
                ProviderFailureKind::Rejected,
            ));
        }
        let result = Compaction {
            id: request.operation_id,
            thread_id: session
                .binding
                .provider_session_id
                .as_ref()
                .map(|id| id.as_str().to_owned())
                .unwrap_or_default(),
            state: CompactionState::Completed,
            manual: true,
            started_at_ms: None,
            completed_at_ms: Some(now_ms()),
            result: CompactionResult {
                summary_content: Some(
                    "Mock compacted context preserves the conversation constraints.".into(),
                ),
                pre_compaction_tokens: Some(80_000),
                post_compaction_tokens: Some(20_000),
                tokens_removed: Some(60_000),
                ..Default::default()
            },
        };
        let sink = session.sink.bind_scope(request.scope)?;
        sink.send_wait(ProviderEvent::Compaction {
            scope: sink.scope().clone(),
            update: Box::new(result.clone()),
        })
        .await?;
        Ok(result)
    }

    async fn cancel_session(&self, binding: &BindingId) -> Result<(), ProviderError> {
        self.cleanup_execution(binding).await
    }

    async fn cleanup_execution(&self, binding: &BindingId) -> Result<(), ProviderError> {
        let session = self.binding(binding).await?;
        let _admission = session.admission.lock().await;
        let tasks = {
            let mut active = session.active.lock().await;
            active.as_mut().map(|invocation| {
                invocation.cancelled.send_replace(true);
                invocation.tasks.clone()
            })
        };
        self.control.cancel_owner(Some(&session.owner));
        if let Some(tasks) = tasks {
            tasks.lock().await.collect().await?;
            session.active.lock().await.take();
        }
        Ok(())
    }

    async fn has_active_turn(&self, owner: &ExecutionOwner) -> bool {
        let Some(binding) = self.owners.lock().await.get(owner).cloned() else {
            return false;
        };
        let Ok(session) = self.binding(&binding).await else {
            return false;
        };
        session
            .active
            .lock()
            .await
            .as_ref()
            .is_some_and(|invocation| {
                invocation
                    .pending
                    .load(std::sync::atomic::Ordering::Acquire)
                    != 0
            })
    }

    async fn active_session_id_for(
        &self,
        owner: &ExecutionOwner,
    ) -> Result<Option<ProviderSessionId>, ProviderError> {
        let Some(binding) = self.owners.lock().await.get(owner).cloned() else {
            return Ok(None);
        };
        Ok(self
            .binding(&binding)
            .await?
            .binding
            .provider_session_id
            .clone())
    }

    async fn rollback_cancelled_turn(&self, binding: &BindingId) -> Result<bool, ProviderError> {
        let session = self.binding(binding).await?;
        let mut safe = session.rollback_cancelled.lock().await;
        Ok(std::mem::replace(&mut *safe, false))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use async_trait::async_trait;
    use forge_provider_api::lifecycle::{ExecutionResult, ExecutionUpdate};
    use forge_provider_api::{
        control::{ProviderTools, ToolCall},
        permissions::{PermissionRequest, PermissionResponse, ProviderPermissions},
        submission::InputReceipt,
        turn_control::{HostTurnStatus, TurnControl},
    };
    use std::sync::Mutex as StdMutex;

    struct Trace;
    impl trace::ProviderTrace for Trace {
        fn record(&self, _: trace::ProviderTraceRecord) {}
    }

    #[derive(Default)]
    struct ReceiptState {
        sending: bool,
        accepted: bool,
    }
    struct Receipt {
        id: SubmissionId,
        state: StdMutex<ReceiptState>,
    }
    #[async_trait]
    impl InputReceipt for Receipt {
        fn id(&self) -> &SubmissionId {
            &self.id
        }
        async fn record_sending(
            self: Arc<Self>,
            _: Option<RecoveryToken>,
        ) -> Result<(), ProviderError> {
            self.state.lock().unwrap().sending = true;
            Ok(())
        }
        async fn record_accepted(
            self: Arc<Self>,
            _: Option<RecoveryToken>,
        ) -> Result<(), ProviderError> {
            self.state.lock().unwrap().accepted = true;
            Ok(())
        }
        async fn record_rejected(self: Arc<Self>, _: ProviderError) -> Result<(), ProviderError> {
            Ok(())
        }
        async fn record_failure(self: Arc<Self>, _: ProviderError) -> Result<(), ProviderError> {
            Ok(())
        }
    }

    struct Permissions;
    #[async_trait]
    impl ProviderPermissions for Permissions {
        async fn authorize(&self, _: &InvocationScope, _: PermissionRequest) -> PermissionResponse {
            PermissionResponse::Allow
        }
        async fn before_tool(&self, _: &InvocationScope) -> Result<(), ProviderError> {
            Ok(())
        }
    }

    struct Tools;
    #[async_trait]
    impl ProviderTools for Tools {
        async fn call(&self, _: &InvocationScope, _: ToolCall) -> Result<String, ProviderError> {
            Ok("fixture tool result".into())
        }
    }

    struct Control;
    #[async_trait]
    impl TurnControl for Control {
        async fn before_send(&self, _: &InvocationScope) -> Result<(), ProviderError> {
            Ok(())
        }
        async fn receive(&self, _: &InvocationScope) -> Result<(), ProviderError> {
            Ok(())
        }
        async fn close(&self, _: &InvocationScope) -> Result<(), ProviderError> {
            Ok(())
        }
        async fn status(&self, _: &InvocationScope) -> Result<HostTurnStatus, ProviderError> {
            Ok(HostTurnStatus {
                replay_safe: false,
                control_handoff: false,
                cancelled: false,
                error: None,
            })
        }
        fn tool_finished(&self, _: &InvocationScope, _: Option<&str>) -> bool {
            false
        }
    }

    struct Effects;
    #[async_trait]
    impl ProviderHostEffects for Effects {
        async fn settle_foreground(
            &self,
            _: &BindingId,
            _: &InvocationScope,
        ) -> Result<(), session::HostEffectFailure> {
            Ok(())
        }
    }

    async fn initialized(
        provider: &MockProvider,
    ) -> (SessionBinding, ExecutionOwner, ProviderEventStream) {
        let owner = make_owner("mock-lifecycle");
        let binding = provider
            .initialize_session(SessionRequest {
                owner: owner.clone(),
                mode: SessionMode::Interactive,
                workspace: ".".into(),
                provider_session_id: None,
                model: Default::default(),
                access: Default::default(),
                approval: ApprovalMode::Ask,
                resources: Default::default(),
                resource_lease: None,
                tools: Vec::new(),
                tool_endpoint: None,
                system_instructions: String::new(),
                trace: Arc::new(Trace),
            })
            .await
            .unwrap();
        let stream = provider.take_session_events(&binding.id).await.unwrap();
        (binding, owner, stream)
    }

    fn prompt(owner: ExecutionOwner, exchange: &str, receipt: Arc<Receipt>) -> PromptRequest {
        PromptRequest {
            scope: InvocationScope::new(owner, exchange),
            mode: PromptMode::Chat,
            configuration: session::InvocationConfiguration {
                model: Default::default(),
                access: Default::default(),
                approval: ApprovalMode::Ask,
            },
            task: None,
            goal: None,
            submission_id: receipt.id.clone(),
            input: PromptInput::Text("hello".into()),
            recovered_inputs: Vec::new(),
            instructions: String::new(),
            attachments: Vec::new(),
            tools: Vec::new(),
            receipt,
            tool_endpoint: None,
            resource_lease: None,
            permissions: Arc::new(Permissions),
            provider_tools: Arc::new(Tools),
            turn_control: Arc::new(Control),
            host_effects: Arc::new(Effects),
        }
    }

    fn make_owner(name: &str) -> ExecutionOwner {
        ExecutionOwner::root(name)
    }

    #[tokio::test]
    async fn normalized_response_holds_match_owner_mode_and_release_once() {
        let control = MockControl::default();
        let owner = make_owner("session");
        let held = control
            .dispatch(
                &owner,
                TestControlCommand::Hold {
                    mode: Some(PromptMode::PlanQuestion),
                    any_owner: false,
                },
            )
            .await
            .unwrap();
        let TestControlResult::Held { id } = held else {
            panic!("hold result missing")
        };
        assert!(
            control
                .dispatch(
                    &make_owner("other"),
                    TestControlCommand::Wait { id: id.clone() }
                )
                .await
                .is_err()
        );
        assert!(
            control
                .dispatch(&owner, TestControlCommand::Release { id: id.clone() })
                .await
                .is_err()
        );
        assert!(
            control
                .arrive(&owner, PromptMode::Chat, "wrong mode".into())
                .await
                .is_ok()
        );
        let arriving = {
            let control = Arc::new(control);
            let arrival_control = control.clone();
            let arriving = tokio::spawn(async move {
                arrival_control
                    .arrive(
                        &make_owner("session"),
                        PromptMode::PlanQuestion,
                        "normalized prompt".into(),
                    )
                    .await
            });
            let pending = control
                .dispatch(
                    &make_owner("session"),
                    TestControlCommand::Wait { id: id.clone() },
                )
                .await
                .unwrap();
            let TestControlResult::Pending { response } = pending else {
                panic!("pending response missing")
            };
            assert_eq!(response.prompt, "normalized prompt");
            control
                .dispatch(
                    &make_owner("session"),
                    TestControlCommand::Release { id: id.clone() },
                )
                .await
                .unwrap();
            assert!(arriving.await.unwrap().is_ok());
            control
        };
        assert!(arriving.holds.lock().unwrap().is_empty());
        assert!(
            arriving
                .dispatch(&make_owner("session"), TestControlCommand::Release { id })
                .await
                .is_err()
        );
    }

    #[tokio::test]
    async fn owner_cancellation_wakes_any_owner_response_waiter() {
        let control = Arc::new(MockControl::default());
        let source = make_owner("source");
        let target = make_owner("target");
        let held = control
            .dispatch(
                &source,
                TestControlCommand::Hold {
                    mode: None,
                    any_owner: true,
                },
            )
            .await
            .unwrap();
        let TestControlResult::Held { id } = held else {
            panic!("hold result missing")
        };
        let arrival_control = control.clone();
        let arrival_target = target.clone();
        let arrival = tokio::spawn(async move {
            arrival_control
                .arrive(&arrival_target, PromptMode::Chat, "prompt".into())
                .await
        });
        let observed = control
            .dispatch(&source, TestControlCommand::Wait { id })
            .await
            .unwrap();
        assert!(
            matches!(observed, TestControlResult::Pending { response } if response.owner == target)
        );
        control.cancel_owner(Some(&target));
        assert!(matches!(
            arrival.await.unwrap(),
            Err(ProviderError::Cancelled)
        ));
    }

    /// Retire the old generation without holding the registry lock needed by retirement.
    #[tokio::test]
    async fn replacing_session_retires_previous_binding() {
        let provider = MockProvider::new().unwrap();
        let (first, _, _old_events) = initialized(&provider).await;
        let (second, _, _new_events) =
            tokio::time::timeout(std::time::Duration::from_secs(2), initialized(&provider))
                .await
                .unwrap();
        assert_ne!(first.id, second.id);
        assert!(provider.binding(&first.id).await.is_err());
        assert!(provider.binding(&second.id).await.is_ok());
        provider.shutdown().await.unwrap();
    }

    /// Cover admitted steering before the response boundary and after foreground completion.
    #[tokio::test]
    async fn steering_covers_live_input_and_cancels_retained_responses_in_order() {
        let provider = MockProvider::new().unwrap();
        let (binding, owner, mut stream) = initialized(&provider).await;
        let control = provider.test_control().unwrap();
        let TestControlResult::Held { id } = control
            .dispatch(
                &owner,
                TestControlCommand::Hold {
                    mode: Some(PromptMode::Chat),
                    any_owner: false,
                },
            )
            .await
            .unwrap()
        else {
            panic!("hold is registered")
        };
        let receipt = |id: &str| {
            Arc::new(Receipt {
                id: SubmissionId::try_from(id.to_owned()).unwrap(),
                state: StdMutex::new(ReceiptState::default()),
            })
        };
        let first = receipt("first");
        let request = prompt(owner.clone(), "steering", first.clone());
        let scope = request.scope.clone();
        provider.session_prompt(&binding.id, request).await.unwrap();
        control
            .dispatch(&owner, TestControlCommand::Wait { id: id.clone() })
            .await
            .unwrap();
        let live = receipt("live");
        provider
            .session_steer(
                &binding.id,
                SteeringRequest {
                    scope: scope.clone(),
                    delivery: session::SteeringDelivery::Journaled {
                        receipt: live.clone(),
                    },
                    input: PromptInput::Text("live constraint".into()),
                    attachments: Vec::new(),
                    task: None,
                    goal: None,
                },
            )
            .await
            .unwrap();
        control
            .dispatch(&owner, TestControlCommand::Release { id })
            .await
            .unwrap();
        tokio::time::timeout(std::time::Duration::from_secs(2), async {
            loop {
                let event = stream.recv().await.unwrap().unwrap().event;
                if let ProviderEvent::Execution { observation } = event
                    && let ExecutionUpdate::Finished {
                        covered_submissions,
                        result,
                        ..
                    } = observation.update()
                {
                    assert_eq!(
                        covered_submissions,
                        &vec![first.id.clone(), live.id.clone()]
                    );
                    assert!(matches!(result, ExecutionResult::Completed { .. }));
                    break;
                }
            }
        })
        .await
        .unwrap();
        let TestControlResult::Held { id } = control
            .dispatch(
                &owner,
                TestControlCommand::Hold {
                    mode: Some(PromptMode::Chat),
                    any_owner: false,
                },
            )
            .await
            .unwrap()
        else {
            panic!("idle hold is registered")
        };
        let late = receipt("late");
        provider
            .session_steer(
                &binding.id,
                SteeringRequest {
                    scope: scope.clone(),
                    delivery: session::SteeringDelivery::Journaled {
                        receipt: late.clone(),
                    },
                    input: PromptInput::Text("continue after settlement".into()),
                    attachments: Vec::new(),
                    task: None,
                    goal: None,
                },
            )
            .await
            .unwrap();
        control
            .dispatch(&owner, TestControlCommand::Wait { id })
            .await
            .unwrap();
        assert!(provider.has_active_turn(&owner).await);
        let queued = receipt("queued");
        provider
            .session_steer(
                &binding.id,
                SteeringRequest {
                    scope: scope.clone(),
                    delivery: session::SteeringDelivery::Journaled {
                        receipt: queued.clone(),
                    },
                    input: PromptInput::Text("queued after retained response".into()),
                    attachments: Vec::new(),
                    task: None,
                    goal: None,
                },
            )
            .await
            .unwrap();
        provider.cancel_session(&binding.id).await.unwrap();
        let mut completed = Vec::new();
        tokio::time::timeout(std::time::Duration::from_secs(2), async {
            while completed.len() < 2 {
                let event = stream.recv().await.unwrap().unwrap().event;
                if let ProviderEvent::Execution { observation } = event
                    && let ExecutionUpdate::Finished {
                        covered_submissions,
                        result,
                        ..
                    } = observation.update()
                {
                    assert_eq!(observation.scope(), &scope);
                    assert!(matches!(result, ExecutionResult::Cancelled { .. }));
                    completed.extend(covered_submissions.clone());
                }
            }
        })
        .await
        .unwrap();
        assert_eq!(completed, vec![late.id.clone(), queued.id.clone()]);
        assert!(!provider.has_active_turn(&owner).await);
        provider.shutdown().await.unwrap();
    }

    #[tokio::test]
    async fn accepted_held_prompt_cancels_with_terminal_evidence_and_session_stream_reuse() {
        let provider = MockProvider::new().unwrap();
        let (binding, owner, mut stream) = initialized(&provider).await;
        let control = provider.test_control().unwrap();
        let held = control
            .dispatch(
                &owner,
                TestControlCommand::Hold {
                    mode: Some(PromptMode::Chat),
                    any_owner: false,
                },
            )
            .await
            .unwrap();
        let TestControlResult::Held { id } = held else {
            panic!("hold result missing")
        };
        let receipt = Arc::new(Receipt {
            id: SubmissionId::try_from("first".to_owned()).unwrap(),
            state: StdMutex::new(ReceiptState::default()),
        });
        let request = prompt(owner.clone(), "first", receipt.clone());
        let scope = request.scope.clone();
        let accepted = provider.session_prompt(&binding.id, request).await.unwrap();
        assert_eq!(accepted.submission_id, receipt.id);
        {
            let receipt_state = receipt.state.lock().unwrap();
            assert!(receipt_state.sending && receipt_state.accepted);
        }
        let pending = control
            .dispatch(&owner, TestControlCommand::Wait { id: id.clone() })
            .await
            .unwrap();
        assert!(matches!(pending, TestControlResult::Pending { .. }));
        let mut saw_start = false;
        while let Ok(Some(event)) = stream.try_next() {
            if let ProviderEvent::Execution { observation } = event.event
                && matches!(observation.update(), ExecutionUpdate::Started { .. })
            {
                assert_eq!(observation.scope(), &scope);
                saw_start = true;
            }
        }
        assert!(saw_start);
        provider.cancel_session(&binding.id).await.unwrap();
        tokio::time::timeout(std::time::Duration::from_secs(1), async {
            loop {
                let event = stream
                    .recv()
                    .await
                    .unwrap()
                    .expect("session stream remains open");
                if let ProviderEvent::Execution { observation } = event.event
                    && let ExecutionUpdate::Finished {
                        result: ExecutionResult::Cancelled { .. },
                        ..
                    } = observation.update()
                {
                    assert_eq!(observation.scope(), &scope);
                    break;
                }
            }
        })
        .await
        .unwrap();
        assert!(!provider.has_active_turn(&owner).await);
        let next = Arc::new(Receipt {
            id: SubmissionId::try_from("second".to_owned()).unwrap(),
            state: StdMutex::new(ReceiptState::default()),
        });
        provider
            .session_prompt(&binding.id, prompt(owner.clone(), "second", next))
            .await
            .unwrap();
        tokio::time::timeout(std::time::Duration::from_secs(1), async {
            loop {
                let event = stream
                    .recv()
                    .await
                    .unwrap()
                    .expect("session stream remains open after cancellation");
                if let ProviderEvent::Execution { observation } = event.event
                    && observation.scope().owner == owner
                    && matches!(
                        observation.update(),
                        ExecutionUpdate::Finished {
                            result: ExecutionResult::Completed { .. },
                            ..
                        }
                    )
                {
                    break;
                }
            }
        })
        .await
        .unwrap();
        provider.shutdown().await.unwrap();
    }
}
