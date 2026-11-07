//! Compose temporary text work from the ordinary provider lifecycle.
use crate::provider::ScopeOwner;

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
                    ProviderOperation::TurnStart,
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
            .map_err(|_| failure(ProviderOperation::TurnStart, ProviderFailureKind::Runtime))?
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
            ProviderOperation::TurnStart,
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
            ProviderOperation::TurnStart,
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
    let selected = selected
        .ok_or_else(|| failure(ProviderOperation::TurnStart, ProviderFailureKind::Rejected))?;
    let reasoning = match request.reasoning {
        api::ReasoningSelection::Default => api::ReasoningSelection::Default,
        api::ReasoningSelection::Lowest => api::generation::lowest_effort(selected)?.map_or(
            api::ReasoningSelection::Default,
            api::ReasoningSelection::Level,
        ),
        api::ReasoningSelection::Level(level) => {
            if !selected.reasoning.contains(&level) {
                return Err(failure(
                    ProviderOperation::TurnStart,
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
    let owner = crate::provider::ThreadOwner::root(format!("utility-{}", uuid::Uuid::new_v4()));
    let scope = api::InvocationScope::new(owner.clone(), uuid::Uuid::new_v4().to_string());
    let open_operation = OperationControl::fresh(ProviderOperation::ThreadOpen)?;
    let opened = observe_operation(
        &open_operation,
        &cancellation,
        api::ProviderPhase::Preparation,
        api::FailureDisposition::Uncertain,
        provider.thread_open(
            open_operation.context(),
            api::SessionRequest {
                thread_id: owner.thread_id(),
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
        thread_id: (scope.clone()).owner().thread_id(),
        turn_id: (scope.clone())
            .turn_id()
            .expect("admitted scope has a nonempty host turn identity"),

        scope: scope.clone(),
        execution: api::ExecutionMode::Default,
        configuration: api::InvocationConfiguration {
            model,
            access,
            approval: api::approval::ApprovalMode::Deny,
        },
        tools: Vec::new(),
        tool_endpoint: None,
        resource_lease: None,
        submission_id: submission_id.clone(),
        attempt_id: attempt_id.clone(),
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
    let submit_operation = OperationControl::fresh(ProviderOperation::TurnStart)?;
    let result = observe_operation(
        &submit_operation,
        &cancellation,
        api::ProviderPhase::Observation,
        api::FailureDisposition::Uncertain,
        async {
            let (accepted, text) = tokio::try_join!(
                provider.turn_start(submit_operation.context(), &binding, prompt),
                collect(&mut events, &binding, &scope, &submission_id, &attempt_id)
            )?;
            if accepted.submission_id != submission_id {
                return Err(failure(
                    ProviderOperation::TurnStart,
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
    let close_operation = OperationControl::fresh(ProviderOperation::ThreadClose)?;
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
    let collection = provider.thread_close(context.clone(), binding);
    tokio::pin!(collection);
    let mut stream_open = true;
    loop {
        tokio::select! {
            result = &mut collection => return result,
            interruption = context.interrupted() => {
                control.cancel();
                return Err(interruption.failure(ProviderOperation::ThreadClose, api::ProviderPhase::Close, api::FailureDisposition::Invalidated));
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

/// Preserve message item kind while selecting only explicit final-answer prose.
enum IsolatedMessage {
    Agent {
        classification: api::MessageClassification,
        text: String,
    },
    Other,
}

/// Collect explicit response items through the authoritative provider-collection boundary.
async fn collect(
    events: &mut api::ProviderEventStream,
    binding: &api::BindingId,
    scope: &api::InvocationScope,
    submission: &api::SubmissionId,
    attempt: &api::DeliveryAttemptId,
) -> Result<String, ProviderError> {
    let mut thread_errors = Vec::new();
    let mut messages = HashMap::<api::ItemId, IsolatedMessage>::new();
    let mut order = Vec::new();
    let thread_id = scope.owner().thread_id();
    let turn_id = scope.turn_id().map_err(|_| {
        failure(
            ProviderOperation::TurnStart,
            ProviderFailureKind::WrongInvocation,
        )
    })?;
    let mut sequence = api::EventSequence::default();
    while let Some(envelope) = events.recv().await? {
        if &envelope.binding_id != binding || envelope.sequence <= sequence {
            return Err(failure(
                ProviderOperation::TurnStart,
                ProviderFailureKind::Protocol,
            ));
        }
        sequence = envelope.sequence;
        match envelope.event {
            api::ProviderEvent::Provider { event } => match event {
                api::ProviderConnectionEvent::Unavailable(unavailable) => {
                    return Err(unavailable.failure.into());
                }
                api::ProviderConnectionEvent::CapabilitiesChanged(_)
                | api::ProviderConnectionEvent::Warning(_) => {}
            },
            api::ProviderEvent::Thread {
                thread_id: observed_thread,
                event,
            } => {
                if observed_thread != thread_id {
                    return Err(failure(
                        ProviderOperation::TurnStart,
                        ProviderFailureKind::WrongInvocation,
                    ));
                }
                match event {
                    api::ThreadEvent::Error { error } => {
                        thread_errors.push(ProviderError::Failure(error))
                    }
                    api::ThreadEvent::IdentityResolved(_)
                    | api::ThreadEvent::ContextUsage(_)
                    | api::ThreadEvent::TerminalsUpdated(_)
                    | api::ThreadEvent::CompactionUpdated(_) => {}
                }
            }
            api::ProviderEvent::Turn {
                thread_id: observed_thread,
                turn_id: observed_turn,
                event,
            } => {
                if observed_thread != thread_id || observed_turn != turn_id {
                    return Err(failure(
                        ProviderOperation::TurnStart,
                        ProviderFailureKind::WrongInvocation,
                    ));
                }
                match event {
                    api::TurnEvent::Completed(completed) => {
                        let result = completed.result();
                        if !result.covered_inputs().iter().any(|input| {
                            input.submission_id() == submission && input.attempt_id() == attempt
                        }) {
                            return Err(failure(
                                ProviderOperation::TurnStart,
                                ProviderFailureKind::Protocol,
                            ));
                        }
                        match result.outcome() {
                            api::turn::TurnOutcome::Completed {
                                stop_reason: api::turn::TurnCompletionReason::Normal,
                            } => {}
                            api::turn::TurnOutcome::Completed {
                                stop_reason:
                                    api::turn::TurnCompletionReason::TokenLimit
                                    | api::turn::TurnCompletionReason::RequestLimit
                                    | api::turn::TurnCompletionReason::Refusal,
                            } => {
                                return Err(failure(
                                    ProviderOperation::TurnStart,
                                    ProviderFailureKind::Rejected,
                                ));
                            }
                            api::turn::TurnOutcome::Failed { error, .. } => {
                                return Err(error.into());
                            }
                            api::turn::TurnOutcome::Interrupted => {
                                return Err(ProviderError::Cancelled);
                            }
                        }
                        return Ok(order
                            .iter()
                            .filter_map(|id| match messages.get(id) {
                                Some(IsolatedMessage::Agent {
                                    classification:
                                        api::MessageClassification::Known(
                                            api::AgentMessageRole::Response,
                                        ),
                                    text,
                                }) => Some(text.as_str()),
                                Some(IsolatedMessage::Agent { .. } | IsolatedMessage::Other)
                                | None => None,
                            })
                            .collect::<Vec<_>>()
                            .join("\n"));
                    }
                    api::TurnEvent::Error { error } => thread_errors.push(error.into()),
                    api::TurnEvent::Started(_)
                    | api::TurnEvent::ModelResolved(_)
                    | api::TurnEvent::Usage(_)
                    | api::TurnEvent::ChecklistUpdated(_) => {}
                }
            }
            api::ProviderEvent::Item {
                thread_id: observed_thread,
                turn_id: observed_turn,
                item_id,
                event,
            } => {
                if observed_thread != thread_id || observed_turn != turn_id {
                    return Err(failure(
                        ProviderOperation::TurnStart,
                        ProviderFailureKind::WrongInvocation,
                    ));
                }
                match event {
                    api::ItemEvent::Started(start) => {
                        if messages.contains_key(&item_id) {
                            return Err(failure(
                                ProviderOperation::TurnStart,
                                ProviderFailureKind::Protocol,
                            ));
                        }
                        let message = match start {
                            api::ItemStart::AgentMessage(start) => IsolatedMessage::Agent {
                                classification: start.classification,
                                text: start.text,
                            },
                            api::ItemStart::UserMessage(_) | api::ItemStart::Reasoning(_) => {
                                IsolatedMessage::Other
                            }
                            api::ItemStart::CommandExecution(_)
                            | api::ItemStart::FileChange(_)
                            | api::ItemStart::ToolCall(_) => {
                                return Err(failure(
                                    ProviderOperation::TurnStart,
                                    ProviderFailureKind::Policy,
                                ));
                            }
                        };
                        order.push(item_id.clone());
                        messages.insert(item_id, message);
                    }
                    api::ItemEvent::Message { event } => {
                        let message = messages.get_mut(&item_id).ok_or_else(|| {
                            failure(ProviderOperation::TurnStart, ProviderFailureKind::Protocol)
                        })?;
                        if let IsolatedMessage::Agent {
                            classification,
                            text,
                        } = message
                        {
                            match event {
                                api::MessageItemEvent::Classified(observed) => {
                                    *classification = observed
                                }
                                api::MessageItemEvent::Delta(delta) => text.push_str(&delta),
                            }
                        }
                    }
                    api::ItemEvent::Completed(completion) => {
                        let message = messages.get_mut(&item_id).ok_or_else(|| {
                            failure(ProviderOperation::TurnStart, ProviderFailureKind::Protocol)
                        })?;
                        match completion {
                            api::ItemCompletion::AgentMessage(completion) => {
                                let IsolatedMessage::Agent { text, .. } = message else {
                                    return Err(failure(
                                        ProviderOperation::TurnStart,
                                        ProviderFailureKind::Protocol,
                                    ));
                                };
                                *text = completion.text;
                                if completion.outcome == api::MessageOutcome::Interrupted {
                                    return Err(ProviderError::Cancelled);
                                }
                            }
                            api::ItemCompletion::UserMessage(_)
                            | api::ItemCompletion::Reasoning(_) => {
                                if matches!(message, IsolatedMessage::Agent { .. }) {
                                    return Err(failure(
                                        ProviderOperation::TurnStart,
                                        ProviderFailureKind::Protocol,
                                    ));
                                }
                            }
                            api::ItemCompletion::CommandExecution(_)
                            | api::ItemCompletion::FileChange(_)
                            | api::ItemCompletion::ToolCall(_) => {
                                return Err(failure(
                                    ProviderOperation::TurnStart,
                                    ProviderFailureKind::Policy,
                                ));
                            }
                        }
                    }
                    api::ItemEvent::Tool { .. } => {
                        return Err(failure(
                            ProviderOperation::TurnStart,
                            ProviderFailureKind::Policy,
                        ));
                    }
                }
            }
            api::ProviderEvent::Agent { .. } => {
                return Err(failure(
                    ProviderOperation::TurnStart,
                    ProviderFailureKind::Policy,
                ));
            }
        }
    }
    let primary = failure(ProviderOperation::TurnStart, ProviderFailureKind::Protocol);
    Err(if thread_errors.is_empty() {
        primary
    } else {
        ProviderError::Combined {
            primary: Box::new(primary),
            secondary: thread_errors,
        }
    })
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
                    ProviderOperation::TurnStart,
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

    /// Supply native response-cycle evidence to the fixture's actual retained runner.
    async fn finish(
        request: &api::PromptRequest,
        sink: &api::AdapterActivityWriter,
        start: &api::NativeTurnStart,
        cancelled: bool,
    ) -> Result<api::TurnResult, ProviderError> {
        start.observed(0).await?;
        let address = api::TurnAddress {
            scope: request.scope.clone(),
            thread_id: request.thread_id.clone(),
            turn_id: request.turn_id.clone(),
        };
        for (id, role, text) in [
            (
                "reasoning",
                api::TestMessageContent::Reasoning(forge_provider_api::ReasoningKind::Full),
                "excluded reasoning",
            ),
            ("answer", api::TestMessageContent::Response, "final text"),
        ] {
            for event in crate::provider::test_support::message(&address, id, Some(role), text) {
                let api::ProviderEvent::Item {
                    thread_id,
                    turn_id,
                    item_id,
                    event,
                } = event
                else {
                    unreachable!("message fixture emits only item activity")
                };
                sink.send_wait(api::AdapterActivity::Item {
                    thread_id,
                    turn_id,
                    item_id,
                    event,
                })
                .await?;
            }
        }
        api::TurnResult::new(
            if cancelled {
                api::turn::TurnOutcome::Interrupted
            } else {
                api::turn::TurnOutcome::Completed {
                    stop_reason: api::turn::TurnCompletionReason::Normal,
                }
            },
            1,
            None,
            vec![api::CoveredInput::new(
                request.submission_id.clone(),
                request.attempt_id.clone(),
            )],
            vec![],
            None,
        )
        .map_err(|_| failure(ProviderOperation::TurnStart, ProviderFailureKind::Protocol))
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
            let native = EventFixtureProvider::new(descriptor, move |request, sink, start, _| {
                let entered = entered_worker.clone();
                let release = release_worker.clone();
                async move {
                    entered.notify_one();
                    release
                        .acquire()
                        .await
                        .expect("test gate remains open")
                        .forget();
                    finish(&request, &sink, &start, false).await
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
        let native =
            EventFixtureProvider::new(descriptor, move |request, sink, start, mut cancelled| {
                let entered = entered_worker.clone();
                async move {
                    entered.notify_one();
                    if !*cancelled.borrow() {
                        let _ = cancelled.changed().await;
                    }
                    finish(&request, &sink, &start, true).await
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
        let native =
            EventFixtureProvider::new(descriptor, move |request, sink, start, mut cancelled| {
                let entered = entered_worker.clone();
                async move {
                    entered.notify_one();
                    if !*cancelled.borrow() {
                        let _ = cancelled.changed().await;
                    }
                    finish(&request, &sink, &start, true).await
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
        let native =
            EventFixtureProvider::new(descriptor, move |request, sink, start, _| async move {
                let address = api::TurnAddress {
                    scope: request.scope.clone(),
                    thread_id: request.thread_id.clone(),
                    turn_id: request.turn_id.clone(),
                };
                start.observed(0).await?;
                for event in crate::provider::test_support::message(
                    &address,
                    "oversized",
                    Some(api::TestMessageContent::Response),
                    "x".repeat(1024 * 1024 + 1),
                ) {
                    let api::ProviderEvent::Item {
                        thread_id,
                        turn_id,
                        item_id,
                        event,
                    } = event
                    else {
                        unreachable!("message fixture emits item activity")
                    };
                    sink.send_wait(api::AdapterActivity::Item {
                        thread_id,
                        turn_id,
                        item_id,
                        event,
                    })
                    .await?;
                }
                for index in 0..1025 {
                    for event in crate::provider::test_support::message(
                        &address,
                        format!("extra-{index}"),
                        Some(api::TestMessageContent::Response),
                        "extra",
                    ) {
                        let api::ProviderEvent::Item {
                            thread_id,
                            turn_id,
                            item_id,
                            event,
                        } = event
                        else {
                            unreachable!("message fixture emits item activity")
                        };
                        sink.send_wait(api::AdapterActivity::Item {
                            thread_id,
                            turn_id,
                            item_id,
                            event,
                        })
                        .await?;
                    }
                }
                finish(&request, &sink, &start, false).await
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
