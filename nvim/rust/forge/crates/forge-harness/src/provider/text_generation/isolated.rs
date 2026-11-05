//! Compose temporary text work from the ordinary provider lifecycle.

use crate::provider::operation::OperationControl;
use forge_provider_api::{
    self as api, ProviderError, ProviderFailure, ProviderFailureKind, ProviderOperation,
};
use futures_util::{
    FutureExt,
    future::{BoxFuture, Shared},
};
use std::{
    collections::HashMap,
    sync::{Arc, Mutex},
};

type Collection = Shared<BoxFuture<'static, Result<(), ProviderError>>>;

enum ReceiptObservation {
    Sending {
        _token: Option<api::RecoveryToken>,
    },
    Accepted {
        _receipt: Option<api::RecoveryToken>,
    },
    Rejected {
        _reason: ProviderError,
    },
    Failed {
        _reason: ProviderError,
    },
}

/// Retain temporary generation and cleanup after individual result waiters disappear.
#[derive(Default)]
pub(crate) struct TextGenerationJobs {
    state: Mutex<JobState>,
}

impl TextGenerationJobs {
    /// Admit isolated work and await its result without transferring collection ownership.
    pub(crate) async fn generate(
        &self,
        provider: Arc<dyn api::provider::Provider>,
        trace: Arc<dyn api::trace::ProviderTrace>,
        request: api::TextGenerationRequest,
    ) -> Result<String, ProviderError> {
        let (send, receive) = tokio::sync::oneshot::channel();
        {
            let mut state = self
                .state
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            if state.closed {
                return Err(failure(
                    ProviderOperation::InputSubmit,
                    ProviderFailureKind::Rejected,
                ));
            }
            state
                .jobs
                .retain(|job| !matches!(job.collection.clone().now_or_never(), Some(Ok(()))));
            let (cancellation, observer) = api::OperationCancellation::channel();
            let worker = tokio::spawn(async move {
                let result = generate(provider, trace, request, observer).await;
                let _ = send.send(result);
            });
            let collection = async move {
                worker
                    .await
                    .map_err(|_| failure(ProviderOperation::Shutdown, ProviderFailureKind::Runtime))
            }
            .boxed()
            .shared();
            state.jobs.push(UtilityJob {
                collection,
                cancellation,
            });
        }
        receive
            .await
            .map_err(|_| failure(ProviderOperation::InputSubmit, ProviderFailureKind::Runtime))?
    }

    /// Close utility admission and join every retained worker without holding its state lock.
    pub(crate) async fn shutdown(&self) -> Result<(), ProviderError> {
        let jobs = {
            let mut state = self
                .state
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            state.closed = true;
            state.jobs.clone()
        };
        for job in &jobs {
            job.cancellation.cancel();
        }
        let mut first = None;
        for job in jobs {
            if let Err(error) = job.collection.await {
                first.get_or_insert(error);
            }
        }
        first.map_or(Ok(()), Err)
    }
}

#[derive(Default)]
struct JobState {
    closed: bool,
    jobs: Vec<UtilityJob>,
}

/// Retain collection and explicit shutdown authority independently of result waiters.
#[derive(Clone)]
struct UtilityJob {
    collection: Collection,
    cancellation: api::OperationCancellationSource,
}

/// Keep receipt evidence in temporary host storage independent of interactive journals.
struct TemporaryReceipt {
    id: api::SubmissionId,
    attempt: api::DeliveryAttemptId,
    observations: Mutex<Vec<ReceiptObservation>>,
}

#[async_trait::async_trait]
impl api::submission::InputReceipt for TemporaryReceipt {
    /// Get the isolated invocation's journal identity.
    fn id(&self) -> &api::SubmissionId {
        &self.id
    }
    /// Get this send attempt independently of the logical input identity.
    fn attempt_id(&self) -> Result<api::DeliveryAttemptId, ProviderError> {
        Ok(self.attempt.clone())
    }
    /// Retain opaque sending evidence before native dispatch.
    async fn record_sending(
        self: Arc<Self>,
        token: Option<api::RecoveryToken>,
    ) -> Result<(), ProviderError> {
        self.observations
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .push(ReceiptObservation::Sending { _token: token });
        Ok(())
    }
    /// Retain native acceptance without replacing earlier evidence.
    async fn record_accepted(
        self: Arc<Self>,
        receipt: Option<api::RecoveryToken>,
    ) -> Result<(), ProviderError> {
        self.observations
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .push(ReceiptObservation::Accepted { _receipt: receipt });
        Ok(())
    }
    /// Retain positive native rejection without authorizing an automatic resend.
    async fn record_rejected(self: Arc<Self>, reason: ProviderError) -> Result<(), ProviderError> {
        self.observations
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .push(ReceiptObservation::Rejected { _reason: reason });
        Ok(())
    }
    /// Retain an uncertain failure separately from delivery evidence.
    async fn record_failure(self: Arc<Self>, reason: ProviderError) -> Result<(), ProviderError> {
        self.observations
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .push(ReceiptObservation::Failed { _reason: reason });
        Ok(())
    }
}

/// Reject every tool and permission callback for the isolated invocation.
struct IsolatedCallbacks;

#[async_trait::async_trait]
impl api::permissions::ProviderPermissions for IsolatedCallbacks {
    /// Reject permissions immediately without opening a user prompt.
    async fn authorize(
        &self,
        _: &api::InvocationScope,
        _: api::permissions::PermissionRequest,
    ) -> api::permissions::PermissionResponse {
        api::permissions::PermissionResponse::Deny { reason: None }
    }
    /// Reject native tools even if an adapter violates its isolated policy.
    async fn before_tool(&self, _: &api::InvocationScope) -> Result<(), ProviderError> {
        Err(failure(
            ProviderOperation::InputSubmit,
            ProviderFailureKind::Policy,
        ))
    }
}

#[async_trait::async_trait]
impl api::control::ProviderTools for IsolatedCallbacks {
    /// Reject callbacks without giving utility work access to host tools.
    async fn call(
        &self,
        _: &api::InvocationScope,
        _: api::control::ToolCall,
    ) -> Result<String, ProviderError> {
        Err(failure(
            ProviderOperation::InputSubmit,
            ProviderFailureKind::Policy,
        ))
    }
}

#[async_trait::async_trait]
impl api::turn_control::TurnControl for IsolatedCallbacks {
    /// Permit only the single explicit utility input.
    async fn before_send(&self, _: &api::InvocationScope) -> Result<(), ProviderError> {
        Ok(())
    }
    /// Observe native updates without granting host workflow authority.
    async fn receive(&self, _: &api::InvocationScope) -> Result<(), ProviderError> {
        Ok(())
    }
    /// Finish the utility callback boundary.
    async fn close(&self, _: &api::InvocationScope) -> Result<(), ProviderError> {
        Ok(())
    }
    /// Exclude retries, goals, and control handoffs from isolated work.
    async fn status(
        &self,
        _: &api::InvocationScope,
    ) -> Result<api::turn_control::HostTurnStatus, ProviderError> {
        Ok(api::turn_control::HostTurnStatus {
            replay_safe: false,
            control_handoff: false,
            cancelled: false,
            error: None,
        })
    }
    /// Report no completed host tools.
    fn tool_finished(&self, _: &api::InvocationScope, _: Option<&str>) -> bool {
        false
    }
}

#[async_trait::async_trait]
impl api::ProviderHostEffects for IsolatedCallbacks {
    /// Settle an invocation which cannot admit host effects.
    async fn settle_foreground(
        &self,
        _: &api::BindingId,
        _: &api::InvocationScope,
    ) -> Result<(), api::HostEffectFailure> {
        Ok(())
    }
}

/// Open temporary history, collect normalized final text, validate it, and always close.
async fn generate(
    provider: Arc<dyn api::provider::Provider>,
    trace: Arc<dyn api::trace::ProviderTrace>,
    request: api::TextGenerationRequest,
    cancellation: api::OperationCancellation,
) -> Result<String, ProviderError> {
    // Resolve the model before opening temporary history.
    let catalog_operation = OperationControl::fresh(ProviderOperation::ModelList)?;
    let models = observe_operation(
        &catalog_operation,
        &cancellation,
        api::ProviderPhase::Observation,
        api::FailureDisposition::Unchanged,
        provider.model_list(
            catalog_operation.context(),
            api::catalog::QueryContext::Workspace(api::catalog::WorkspaceContext {
                workspace: request.workspace.clone(),
            }),
        ),
    )
    .await?;
    let selected = models.iter().find(|model| {
        model.id == request.model || (request.model == "default" && model.is_default)
    });
    let selected = selected.ok_or_else(|| {
        failure(
            ProviderOperation::InputSubmit,
            ProviderFailureKind::Rejected,
        )
    })?;
    let reasoning = match request.reasoning {
        api::ReasoningSelection::Default => api::ReasoningSelection::Default,
        api::ReasoningSelection::Lowest => api::generation::lowest_effort(selected)?.map_or(
            api::ReasoningSelection::Default,
            api::ReasoningSelection::Level,
        ),
        api::ReasoningSelection::Level(level) => {
            if !selected.reasoning.contains(&level) {
                return Err(failure(
                    ProviderOperation::InputSubmit,
                    ProviderFailureKind::Rejected,
                ));
            }
            api::ReasoningSelection::Level(level)
        }
    };
    let model = api::ModelSelection {
        model: Some(selected.id.clone()),
        reasoning,
        context_window: None,
        service_tier: Default::default(),
    };
    let access = api::access::AccessPolicy {
        write_access: api::access::WriteScope::Off,
        ..Default::default()
    };
    let owner = api::ExecutionOwner::root(format!("utility-{}", uuid::Uuid::new_v4()));
    let scope = api::InvocationScope::new(owner.clone(), uuid::Uuid::new_v4().to_string());
    let open_operation = OperationControl::fresh(ProviderOperation::SessionOpen)?;
    let opened = observe_operation(
        &open_operation,
        &cancellation,
        api::ProviderPhase::Preparation,
        api::FailureDisposition::Uncertain,
        provider.session_open(
            open_operation.context(),
            api::SessionRequest {
                owner,
                mode: api::SessionMode::Background,
                workspace: request.workspace,
                history: api::SessionHistory::IsolatedText(api::IsolatedTextPolicy::new()),
                configuration: api::SessionConfiguration {
                    model: model.clone(),
                    access: access.clone(),
                    approval: api::approval::ApprovalMode::Deny,
                    resources: Default::default(),
                },
                resource_lease: None,
                tools: Vec::new(),
                tool_endpoint: None,
                system_instructions: request.instructions,
                trace,
            },
        ),
    )
    .await?;

    // Install the sole reader before the ordinary input submission can publish.
    let binding = opened.binding.id;
    let mut events = opened.events;
    let submission_id = api::SubmissionId::try_from(uuid::Uuid::new_v4().to_string())
        .expect("a UUID is a nonempty submission identity");
    let attempt_id = api::DeliveryAttemptId::new(uuid::Uuid::new_v4().to_string())
        .expect("a UUID is a nonempty attempt identity");
    let receipt = Arc::new(TemporaryReceipt {
        id: submission_id.clone(),
        attempt: attempt_id.clone(),
        observations: Default::default(),
    });
    let (dispatch, _) = api::submission::DispatchReporter::channel(
        binding.clone(),
        scope.clone(),
        submission_id.clone(),
        attempt_id.clone(),
    );
    let callbacks = Arc::new(IsolatedCallbacks);
    let prompt = api::PromptRequest {
        scope: scope.clone(),
        mode: api::PromptMode::Chat,
        configuration: api::InvocationConfiguration {
            model,
            access,
            approval: api::approval::ApprovalMode::Deny,
        },
        task: None,
        tools: Vec::new(),
        tool_endpoint: None,
        resource_lease: None,
        submission_id: submission_id.clone(),
        attempt_id,
        input: api::PromptInput::Text(request.input),
        dispatch,
        recovered_inputs: Vec::new(),
        instructions: String::new(),
        attachments: Vec::new(),
        receipt,
        permissions: callbacks.clone(),
        provider_tools: callbacks.clone(),
        turn_control: callbacks.clone(),
        host_effects: callbacks,
    };
    let submit_operation = OperationControl::fresh(ProviderOperation::InputSubmit)?;
    let result = observe_operation(
        &submit_operation,
        &cancellation,
        api::ProviderPhase::Observation,
        api::FailureDisposition::Uncertain,
        async {
            let (accepted, text) = tokio::try_join!(
                provider.input_submit(submit_operation.context(), &binding, prompt),
                collect(&mut events, &binding, &scope, &submission_id)
            )?;
            if accepted.submission_id != submission_id {
                return Err(failure(
                    ProviderOperation::InputSubmit,
                    ProviderFailureKind::Protocol,
                ));
            }
            request.validator.validate(&text)
        },
    )
    .await;
    if result.is_err() {
        submit_operation.cancel();
    }

    // Closing revokes callbacks and collects even after validation or explicit cancellation.
    let close_operation = OperationControl::fresh(ProviderOperation::SessionClose)?;
    let closed = close(provider.as_ref(), &close_operation, &binding, &mut events).await;
    match (result, closed) {
        (Ok(text), Ok(())) => Ok(text),
        (Err(primary), Err(secondary)) => Err(ProviderError::Combined {
            primary: Box::new(primary),
            secondary: vec![secondary],
        }),
        (Err(error), Ok(())) | (Ok(_), Err(error)) => Err(error),
    }
}

/// Observe the operation policy and explicit utility shutdown without imposing a generation deadline.
async fn observe_operation<T>(
    control: &OperationControl,
    cancellation: &api::OperationCancellation,
    phase: api::ProviderPhase,
    disposition: api::FailureDisposition,
    work: impl std::future::Future<Output = Result<T, ProviderError>>,
) -> Result<T, ProviderError> {
    let context = control.context();
    tokio::select! {
        result = work => result,
        _ = cancellation.cancelled() => {
            control.cancel();
            Err(api::OperationInterruption::Cancelled.failure(control.operation(), phase, disposition))
        }
        interruption = context.interrupted() => {
            control.cancel();
            Err(interruption.failure(control.operation(), phase, disposition))
        }
    }
}

/// Drain the sole stream through collection so cancellation cannot block native publishers.
async fn close(
    provider: &dyn api::provider::Provider,
    control: &OperationControl,
    binding: &api::BindingId,
    events: &mut api::ProviderEventStream,
) -> Result<(), ProviderError> {
    let context = control.context();
    let collection = provider.session_close(context.clone(), binding);
    tokio::pin!(collection);
    let mut stream_open = true;
    loop {
        tokio::select! {
            result = &mut collection => return result,
            interruption = context.interrupted() => {
                control.cancel();
                return Err(interruption.failure(ProviderOperation::SessionClose, api::ProviderPhase::Close, api::FailureDisposition::Invalidated));
            }
            event = events.recv(), if stream_open => {
                match event {
                    Ok(Some(_)) => {},
                    Ok(None) | Err(_) => stream_open = false,
                }
            }
        }
    }
}

/// Collect final response messages while excluding reasoning, commentary, and tool output.
async fn collect(
    events: &mut api::ProviderEventStream,
    binding: &api::BindingId,
    scope: &api::InvocationScope,
    submission: &api::SubmissionId,
) -> Result<String, ProviderError> {
    let mut messages = HashMap::<api::MessageId, (api::MessageClassification, String)>::new();
    let mut order = Vec::new();
    let mut terminal = false;
    let mut sequence = api::EventSequence::default();
    while let Some(event) = events.recv().await.map_err(|_| {
        failure(
            ProviderOperation::InputSubmit,
            ProviderFailureKind::Protocol,
        )
    })? {
        if &event.binding_id != binding {
            return Err(failure(
                ProviderOperation::InputSubmit,
                ProviderFailureKind::Protocol,
            ));
        }
        if event.sequence <= sequence {
            return Err(failure(
                ProviderOperation::InputSubmit,
                ProviderFailureKind::Protocol,
            ));
        }
        sequence = event.sequence;
        match event.event {
            api::ProviderEvent::Message { address, update } => {
                if &address.turn.scope != scope {
                    return Err(failure(
                        ProviderOperation::InputSubmit,
                        ProviderFailureKind::Protocol,
                    ));
                }
                let message = messages
                    .entry(address.message_id.clone())
                    .or_insert_with(|| {
                        order.push(address.message_id.clone());
                        (api::MessageClassification::Unclassified, String::new())
                    });
                match update {
                    api::MessageUpdate::Started {
                        classification,
                        text,
                    } => *message = (classification, text),
                    api::MessageUpdate::Delta { text } => message.1.push_str(&text),
                    api::MessageUpdate::Completed { text } => message.1 = text,
                    api::MessageUpdate::Classified { role } => {
                        message.0 = api::MessageClassification::Known(role)
                    }
                    api::MessageUpdate::Interrupted => {}
                }
            }
            api::ProviderEvent::Execution { observation } => {
                if observation.scope() != scope {
                    return Err(failure(
                        ProviderOperation::InputSubmit,
                        ProviderFailureKind::Protocol,
                    ));
                }
                match observation.update() {
                    api::ExecutionUpdate::Finished {
                        covered_submissions,
                        result,
                        ..
                    }
                    | api::ExecutionUpdate::ResolvedWithoutExecution {
                        covered_submissions,
                        result,
                        ..
                    } => {
                        if !covered_submissions.contains(submission)
                            || !matches!(result, api::ExecutionResult::Completed { .. })
                        {
                            return Err(failure(
                                ProviderOperation::InputSubmit,
                                ProviderFailureKind::Rejected,
                            ));
                        }
                        terminal = true;
                    }
                    api::ExecutionUpdate::Started { .. } => {}
                    api::ExecutionUpdate::PublicationFailed { .. }
                    | api::ExecutionUpdate::ResolutionPublicationFailed { .. }
                    | api::ExecutionUpdate::ObservationLost { .. } => {
                        return Err(failure(
                            ProviderOperation::InputSubmit,
                            ProviderFailureKind::Protocol,
                        ));
                    }
                }
            }
            api::ProviderEvent::Settled { settlement } => {
                if settlement.binding_id != *binding
                    || settlement.scope != *scope
                    || !terminal
                    || settlement.watermark > sequence
                    || !matches!(settlement.result, api::ExecutionResult::Completed { .. })
                {
                    return Err(failure(
                        ProviderOperation::InputSubmit,
                        ProviderFailureKind::Protocol,
                    ));
                }
                return Ok(order
                    .iter()
                    .filter_map(|id| messages.get(id))
                    .filter(|(role, _)| {
                        *role == api::MessageClassification::Known(api::MessageRole::Response)
                    })
                    .map(|(_, text)| text.as_str())
                    .collect::<Vec<_>>()
                    .join("\n"));
            }
            api::ProviderEvent::SettlementFailed { failure, .. } => {
                return Err(ProviderError::Failure(failure));
            }
            api::ProviderEvent::Tool { .. } | api::ProviderEvent::Approval { .. } => {
                return Err(failure(
                    ProviderOperation::InputSubmit,
                    ProviderFailureKind::Policy,
                ));
            }
            api::ProviderEvent::Terminals { .. }
            | api::ProviderEvent::CapabilitiesChanged { .. }
            | api::ProviderEvent::SessionIdentity { .. }
            | api::ProviderEvent::Turn { .. }
            | api::ProviderEvent::Usage { .. }
            | api::ProviderEvent::Runtime { .. }
            | api::ProviderEvent::Compaction { .. }
            | api::ProviderEvent::Agent { .. }
            | api::ProviderEvent::Checklist { .. }
            | api::ProviderEvent::DirectoryAccess { .. }
            | api::ProviderEvent::InputAccepted { .. }
            | api::ProviderEvent::Retry { .. }
            | api::ProviderEvent::Diagnostic { .. }
            | api::ProviderEvent::ParentBoundary { .. } => {}
        }
    }
    Err(failure(
        ProviderOperation::InputSubmit,
        ProviderFailureKind::Protocol,
    ))
}

/// Classify shared utility failures without exposing native protocol details.
fn failure(operation: ProviderOperation, kind: ProviderFailureKind) -> ProviderError {
    ProviderError::Failure(ProviderFailure::new(operation, kind))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::provider::test_support::EventFixtureProvider;
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::time::Duration;

    struct Trace;
    impl api::trace::ProviderTrace for Trace {
        /// Accept normalized traces without acquiring persistent session state.
        fn record(&self, _: api::trace::ProviderTraceRecord) {}
    }

    struct Validator {
        calls: Arc<AtomicUsize>,
        reject: bool,
    }
    impl api::TextValidator for Validator {
        /// Count shared host validation and optionally reject otherwise valid native output.
        fn validate(&self, text: &str) -> Result<String, ProviderError> {
            self.calls.fetch_add(1, Ordering::AcqRel);
            if self.reject {
                return Err(failure(
                    ProviderOperation::InputSubmit,
                    ProviderFailureKind::Rejected,
                ));
            }
            Ok(text.to_owned())
        }
    }

    /// Construct an isolated request with host-owned validation and opaque model selection.
    fn request(calls: Arc<AtomicUsize>, reject: bool) -> api::TextGenerationRequest {
        api::TextGenerationRequest {
            workspace: "utility-workspace".into(),
            model: "utility-model".into(),
            reasoning: api::ReasoningSelection::Lowest,
            instructions: "Return the requested text only".into(),
            input: "evidence".into(),
            validator: Arc::new(Validator { calls, reject }),
        }
    }

    /// Advertise the model needed by the shared catalog resolution contract.
    fn models() -> Vec<api::ProviderModel> {
        vec![serde_json::from_value(serde_json::json!({
            "id":"utility-model","reasoning":["low","high"],"context_window":[],"vision":false,"is_default":true,
        })).expect("fixture normalized model")]
    }

    /// Publish native terminal evidence; the fixture separately collects and publishes settlement.
    async fn finish(
        request: &api::PromptRequest,
        sink: &api::ProviderEventSink,
        cancelled: bool,
    ) -> Result<(), ProviderError> {
        let address = crate::provider::test_support::address(
            &request.scope,
            "utility-history",
            "utility-turn",
        );
        sink.send_wait(api::ProviderEvent::Execution {
            observation: api::ExecutionObservation::new(
                request.scope.clone(),
                api::ExecutionUpdate::Started {
                    address: address.clone(),
                },
            )?,
        })
        .await?;
        for event in crate::provider::test_support::message(
            &address,
            "reasoning",
            Some(api::MessageRole::Thought),
            "excluded reasoning",
        ) {
            sink.send_wait(event).await?;
        }
        for event in crate::provider::test_support::message(
            &address,
            "answer",
            Some(api::MessageRole::Response),
            "final text",
        ) {
            sink.send_wait(event).await?;
        }
        let completion = api::completion::ProviderCompletion::default().into();
        let result = if cancelled {
            api::ExecutionResult::Cancelled { completion }
        } else {
            api::ExecutionResult::Completed { completion }
        };
        sink.send_wait(api::ProviderEvent::Execution {
            observation: api::ExecutionObservation::new(
                request.scope.clone(),
                api::ExecutionUpdate::Finished {
                    address,
                    covered_submissions: vec![request.submission_id.clone()],
                    completed_at_ms: 1,
                    result,
                },
            )?,
        })
        .await?;
        Ok(())
    }

    /// Preserve worker ownership after dropped waiters and close history after validator rejection.
    #[tokio::test]
    async fn dropped_waiter_and_validator_rejection_collect_temporary_sessions()
    -> anyhow::Result<()> {
        for (drop_waiter, reject) in [(true, false), (false, true)] {
            let entered = Arc::new(tokio::sync::Notify::new());
            let release = Arc::new(tokio::sync::Semaphore::new(0));
            let entered_worker = entered.clone();
            let release_worker = release.clone();
            let descriptor = forge_providers::registry::descriptor(
                &forge_providers::registry::nonselectable_id().expect("fixture identity"),
            )?;
            let native = EventFixtureProvider::new(descriptor, move |request, sink, _| {
                let entered = entered_worker.clone();
                let release = release_worker.clone();
                async move {
                    entered.notify_one();
                    release
                        .acquire()
                        .await
                        .expect("test gate remains open")
                        .forget();
                    finish(&request, &sink, false).await
                }
            })
            .with_models(models());
            let closed = Arc::new(AtomicUsize::new(0));
            let observed_close = closed.clone();
            native.with_session_close(move |_| {
                let closed = observed_close.clone();
                async move {
                    closed.fetch_add(1, Ordering::AcqRel);
                    Ok(())
                }
            });
            let validated = Arc::new(AtomicUsize::new(0));
            let jobs = Arc::new(TextGenerationJobs::default());
            let running = jobs.clone();
            let input = request(validated.clone(), reject);
            let waiter =
                tokio::spawn(async move { running.generate(native, Arc::new(Trace), input).await });
            entered.notified().await;
            if drop_waiter {
                waiter.abort();
            }
            release.add_permits(1);
            if !drop_waiter {
                assert!(waiter.await?.is_err());
            }
            tokio::time::timeout(Duration::from_secs(3), async {
                while closed.load(Ordering::Acquire) == 0 {
                    tokio::task::yield_now().await;
                }
            })
            .await?;
            jobs.shutdown().await?;
            assert_eq!(validated.load(Ordering::Acquire), 1);
            assert_eq!(closed.load(Ordering::Acquire), 1);
        }
        Ok(())
    }

    /// Keep utility work alive beyond the former implicit deadline until explicit shutdown.
    #[tokio::test(start_paused = true)]
    async fn generation_remains_active_until_explicit_shutdown() -> anyhow::Result<()> {
        let entered = Arc::new(tokio::sync::Notify::new());
        let entered_worker = entered.clone();
        let descriptor = forge_providers::registry::descriptor(
            &forge_providers::registry::nonselectable_id().expect("fixture identity"),
        )?;
        let native = EventFixtureProvider::new(descriptor, move |request, sink, mut cancelled| {
            let entered = entered_worker.clone();
            async move {
                entered.notify_one();
                if !*cancelled.borrow() {
                    let _ = cancelled.changed().await;
                }
                finish(&request, &sink, true).await
            }
        })
        .with_models(models());
        let closed = Arc::new(AtomicUsize::new(0));
        let observed_close = closed.clone();
        native.with_session_close(move |_| {
            let closed = observed_close.clone();
            async move {
                closed.fetch_add(1, Ordering::AcqRel);
                Ok(())
            }
        });
        let validated = Arc::new(AtomicUsize::new(0));
        let input = request(validated.clone(), false);
        let jobs = Arc::new(TextGenerationJobs::default());
        let running = jobs.clone();
        let waiter =
            tokio::spawn(async move { running.generate(native, Arc::new(Trace), input).await });
        entered.notified().await;
        tokio::time::advance(Duration::from_secs(121)).await;
        assert!(!waiter.is_finished());
        assert_eq!(closed.load(Ordering::Acquire), 0);
        jobs.shutdown().await?;
        let error = waiter.await?.unwrap_err();
        assert!(
            matches!(error, ProviderError::Failure(problem) if problem.kind() == ProviderFailureKind::Cancelled)
        );
        assert_eq!(validated.load(Ordering::Acquire), 0);
        assert_eq!(closed.load(Ordering::Acquire), 1);
        Ok(())
    }

    /// Cancel live utility input explicitly on shutdown while preserving a separate cleanup context.
    #[tokio::test]
    async fn utility_shutdown_explicitly_cancels_input_and_collects_temporary_history()
    -> anyhow::Result<()> {
        let entered = Arc::new(tokio::sync::Notify::new());
        let entered_worker = entered.clone();
        let descriptor = forge_providers::registry::descriptor(
            &forge_providers::registry::nonselectable_id().expect("fixture identity"),
        )?;
        let native = EventFixtureProvider::new(descriptor, move |request, sink, mut cancelled| {
            let entered = entered_worker.clone();
            async move {
                entered.notify_one();
                if !*cancelled.borrow() {
                    let _ = cancelled.changed().await;
                }
                finish(&request, &sink, true).await
            }
        })
        .with_models(models());
        let closed = Arc::new(AtomicUsize::new(0));
        let observed_close = closed.clone();
        native.with_session_close(move |_| {
            let closed = observed_close.clone();
            async move {
                closed.fetch_add(1, Ordering::AcqRel);
                Ok(())
            }
        });
        let validated = Arc::new(AtomicUsize::new(0));
        let input = request(validated.clone(), false);
        let jobs = Arc::new(TextGenerationJobs::default());
        let running = jobs.clone();
        let waiter =
            tokio::spawn(async move { running.generate(native, Arc::new(Trace), input).await });
        entered.notified().await;
        tokio::time::timeout(Duration::from_secs(3), jobs.shutdown()).await??;
        let error = waiter.await?.unwrap_err();
        assert!(
            matches!(error, ProviderError::Failure(problem) if problem.kind() == ProviderFailureKind::Cancelled)
        );
        assert_eq!(validated.load(Ordering::Acquire), 0);
        assert_eq!(closed.load(Ordering::Acquire), 1);
        Ok(())
    }

    /// Preserve large response content and message collections through host validation and cleanup.
    #[tokio::test]
    async fn large_output_and_many_messages_validate_and_collect_temporary_history()
    -> anyhow::Result<()> {
        let descriptor = forge_providers::registry::descriptor(
            &forge_providers::registry::nonselectable_id().expect("fixture identity"),
        )?;
        let native = EventFixtureProvider::new(descriptor, move |request, sink, _| async move {
            let address = crate::provider::test_support::address(
                &request.scope,
                "utility-history",
                "oversized-turn",
            );
            for event in crate::provider::test_support::message(
                &address,
                "oversized",
                Some(api::MessageRole::Response),
                "x".repeat(1024 * 1024 + 1),
            ) {
                sink.send_wait(event).await?;
            }
            for index in 0..1025 {
                for event in crate::provider::test_support::message(
                    &address,
                    format!("extra-{index}"),
                    Some(api::MessageRole::Response),
                    "extra",
                ) {
                    sink.send_wait(event).await?;
                }
            }
            finish(&request, &sink, false).await
        })
        .with_models(models());
        let closed = Arc::new(AtomicUsize::new(0));
        let observed_close = closed.clone();
        native.with_session_close(move |_| {
            let closed = observed_close.clone();
            async move {
                closed.fetch_add(1, Ordering::AcqRel);
                Ok(())
            }
        });
        let validated = Arc::new(AtomicUsize::new(0));
        let jobs = TextGenerationJobs::default();
        let text = jobs
            .generate(native, Arc::new(Trace), request(validated.clone(), false))
            .await?;
        assert!(text.starts_with(&"x".repeat(1024 * 1024 + 1)));
        assert_eq!(text.matches("extra").count(), 1025);
        assert!(text.ends_with("final text"));
        jobs.shutdown().await?;
        assert_eq!(validated.load(Ordering::Acquire), 1);
        assert_eq!(closed.load(Ordering::Acquire), 1);
        Ok(())
    }
}
