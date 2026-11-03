use super::{
    AcpKind,
    conversation::{AcpConversation, ConversationOptions},
    runtime::AcpRuntime,
    session::AcpSessionState,
};
use async_trait::async_trait;
use forge_provider_api::approval::ApprovalMode;
use forge_provider_api::compaction::CompactionResult;
use forge_provider_api::events::{self, ProviderEventSink, ProviderEventStream};
use forge_provider_api::session::{
    BindingId, InputAcceptance, PromptRequest, RecoveredInput, SessionBinding, SessionRequest,
    SteeringRequest,
};
use forge_provider_api::{ExecutionOwner, ProviderEvent};
use forge_provider_api::{ProviderError, ProviderFailure, ProviderFailureKind, ProviderOperation};
use std::{collections::HashMap, path::PathBuf, sync::Arc};
use tokio::sync::Mutex;

struct AcpBinding {
    binding: SessionBinding,
    owner: ExecutionOwner,
    conversation: Arc<AcpConversation>,
    state: Arc<AcpSessionState>,
    sink: ProviderEventSink,
    stream: Mutex<Option<ProviderEventStream>>,
    resources: Mutex<Option<crate::profile::ResourceProfile>>,
    _lease: Option<Arc<dyn forge_provider_api::session::ResourceLease>>,
    selection: forge_provider_api::session::ResourceSelection,
    settings: Mutex<AcpSettings>,
    operation: Mutex<()>,
    active: Mutex<Option<ActiveExecution>>,
    rollback_cancelled: Mutex<bool>,
    last_prompt: Mutex<Option<PromptBaseline>>,
    continuations: Mutex<HashMap<Vec<u8>, ContinuationGrant>>,
}

impl AcpBinding {
    /// Check whether native input preparation or a prompt worker is still running.
    async fn has_active_prompt(&self) -> bool {
        self.active
            .lock()
            .await
            .as_ref()
            .is_some_and(|active| active.task.as_ref().is_none_or(|task| !task.is_finished()))
    }

    /// Join completed prompt work before admitting another operation.
    async fn collect_finished_prompt(&self) -> Result<(), ProviderError> {
        let finished = self
            .active
            .lock()
            .await
            .as_ref()
            .and_then(|active| active.task.as_ref())
            .is_some_and(|task| task.is_finished());
        if finished {
            self.collect_prompt().await?;
        }
        Ok(())
    }

    /// Preserve the active worker until its normalized result is available.
    async fn collect_prompt(&self) -> Result<(), ProviderError> {
        let retained = self.active.lock().await.as_ref().and_then(|active| {
            active
                .task
                .as_ref()
                .map(|task| (active.scope.clone(), task.clone()))
        });
        let Some((scope, task)) = retained else {
            return Ok(());
        };
        let result = task.join().await;
        let mut active = self.active.lock().await;
        if active.as_ref().is_some_and(|active| active.scope == scope) {
            active.take();
        }
        result
    }
}

pub(super) struct ContinuationGrant {
    binding: BindingId,
    scope: forge_provider_api::InvocationScope,
    submission_id: Option<forge_provider_api::SubmissionId>,
    receipt: Option<Arc<dyn forge_provider_api::submission::InputReceipt>>,
    input: forge_provider_api::input::PromptInput,
    attachments: Vec<forge_provider_api::input::FileAttachment>,
    task: Option<forge_provider_api::input::TaskStart>,
    goal: Option<forge_provider_api::NativeGoalCommand>,
    mode: forge_provider_api::input::PromptMode,
    configuration: forge_provider_api::session::InvocationConfiguration,
    tools: Vec<forge_provider_api::control::ControlToolDefinition>,
    tool_endpoint: Option<forge_provider_api::session::HostToolEndpoint>,
    resource_lease: Option<Arc<dyn forge_provider_api::session::ResourceLease>>,
    instructions: String,
    recovered_inputs: Vec<RecoveredInput>,
}

#[derive(Clone)]
pub(super) struct PromptBaseline {
    scope: forge_provider_api::InvocationScope,
    mode: forge_provider_api::input::PromptMode,
    configuration: forge_provider_api::session::InvocationConfiguration,
    tools: Vec<forge_provider_api::control::ControlToolDefinition>,
    tool_endpoint: Option<forge_provider_api::session::HostToolEndpoint>,
    resource_lease: Option<Arc<dyn forge_provider_api::session::ResourceLease>>,
    instructions: String,
    recovered_inputs: Vec<RecoveredInput>,
}

impl PromptBaseline {
    pub(super) fn from_request(request: &PromptRequest) -> Self {
        Self {
            scope: request.scope.clone(),
            mode: request.mode,
            configuration: request.configuration.clone(),
            tools: request.tools.clone(),
            tool_endpoint: request.tool_endpoint.clone(),
            resource_lease: request.resource_lease.clone(),
            instructions: request.instructions.clone(),
            recovered_inputs: request.recovered_inputs.clone(),
        }
    }
}

impl ContinuationGrant {
    pub(super) fn from_steering(
        binding: BindingId,
        request: &SteeringRequest,
        base: &PromptBaseline,
    ) -> Self {
        Self {
            binding,
            scope: request.scope.clone(),
            submission_id: request.delivery.submission_id().cloned(),
            receipt: request.delivery.receipt().cloned(),
            input: request.input.clone(),
            attachments: request.attachments.clone(),
            task: request.task.clone(),
            goal: request.goal.clone(),
            mode: base.mode,
            configuration: base.configuration.clone(),
            tools: base.tools.clone(),
            tool_endpoint: base.tool_endpoint.clone(),
            resource_lease: base.resource_lease.clone(),
            instructions: base.instructions.clone(),
            recovered_inputs: base.recovered_inputs.clone(),
        }
    }

    fn matches(
        &self,
        binding: &BindingId,
        owner: &ExecutionOwner,
        request: &PromptRequest,
    ) -> bool {
        self.binding == *binding
            && request.scope == self.scope
            && request.scope.owner == *owner
            && self
                .submission_id
                .as_ref()
                .is_none_or(|id| id == &request.submission_id)
            && request.receipt.id() == &request.submission_id
            && self
                .receipt
                .as_ref()
                .is_none_or(|receipt| Arc::ptr_eq(receipt, &request.receipt))
            && same_input(&self.input, &request.input)
            && same_attachments(&self.attachments, &request.attachments)
            && self.task == request.task
            && self.goal == request.goal
            && self.mode == request.mode
            && self.configuration.model == request.configuration.model
            && self.configuration.access == request.configuration.access
            && self.configuration.approval == request.configuration.approval
            && self
                .tools
                .iter()
                .map(|tool| tool.mcp_value())
                .collect::<Vec<_>>()
                == request
                    .tools
                    .iter()
                    .map(|tool| tool.mcp_value())
                    .collect::<Vec<_>>()
            && same_tool_endpoint(self.tool_endpoint.as_ref(), request.tool_endpoint.as_ref())
            && same_resource_lease(
                self.resource_lease.as_ref(),
                request.resource_lease.as_ref(),
            )
            && self.instructions == request.instructions
            && same_recovered_inputs(&self.recovered_inputs, &request.recovered_inputs)
    }
}

fn same_tool_endpoint(
    left: Option<&forge_provider_api::session::HostToolEndpoint>,
    right: Option<&forge_provider_api::session::HostToolEndpoint>,
) -> bool {
    match (left, right) {
        (None, None) => true,
        (Some(left), Some(right)) => left.url == right.url && left.headers == right.headers,
        _ => false,
    }
}

fn same_resource_lease(
    left: Option<&Arc<dyn forge_provider_api::session::ResourceLease>>,
    right: Option<&Arc<dyn forge_provider_api::session::ResourceLease>>,
) -> bool {
    match (left, right) {
        (None, None) => true,
        (Some(left), Some(right)) => Arc::ptr_eq(left, right),
        _ => false,
    }
}

pub(super) fn take_continuation_grant(
    grants: &mut HashMap<Vec<u8>, ContinuationGrant>,
    token: &[u8],
    binding: &BindingId,
    owner: &ExecutionOwner,
    request: &PromptRequest,
) -> bool {
    if !grants
        .get(token)
        .is_some_and(|grant| grant.matches(binding, owner, request))
    {
        return false;
    }
    grants.remove(token);
    true
}

fn same_input(
    left: &forge_provider_api::input::PromptInput,
    right: &forge_provider_api::input::PromptInput,
) -> bool {
    use forge_provider_api::input::PromptInput;
    match (left, right) {
        (PromptInput::Text(left), PromptInput::Text(right)) => left == right,
        (
            PromptInput::Skill {
                name: left_name,
                arguments: left_arguments,
            },
            PromptInput::Skill {
                name: right_name,
                arguments: right_arguments,
            },
        ) => left_name == right_name && left_arguments == right_arguments,
        (PromptInput::QuestionReply(left), PromptInput::QuestionReply(right)) => left == right,
        _ => false,
    }
}

fn same_attachments(
    left: &[forge_provider_api::input::FileAttachment],
    right: &[forge_provider_api::input::FileAttachment],
) -> bool {
    left.len() == right.len()
        && left.iter().zip(right).all(|(left, right)| {
            left.path == right.path
                && left.label == right.label
                && left.media_type == right.media_type
                && left.contents.as_ref() == right.contents.as_ref()
                && left.context == right.context
        })
}

fn same_recovered_inputs(left: &[RecoveredInput], right: &[RecoveredInput]) -> bool {
    left.len() == right.len()
        && left.iter().zip(right).all(|(left, right)| {
            same_input(&left.input, &right.input)
                && same_attachments(&left.attachments, &right.attachments)
                && left.instructions == right.instructions
        })
}

struct ActiveExecution {
    scope: forge_provider_api::InvocationScope,
    cancelled: bool,
    task: Option<Arc<crate::retained_task::RetainedTask>>,
}

struct AcpSettings {
    model: forge_provider_api::session::ModelSelection,
    access: forge_provider_api::access::AccessPolicy,
    approval: ApprovalMode,
}

/// Own ACP processes, retained conversations, native event decoding, and completion.
pub struct AcpProvider {
    kind: AcpKind,
    runtime: AcpRuntime,
    runtime_root: PathBuf,
    bindings: Mutex<HashMap<BindingId, Arc<AcpBinding>>>,
    owners: Mutex<HashMap<ExecutionOwner, BindingId>>,
    preparation: Mutex<HashMap<ExecutionOwner, Arc<Mutex<()>>>>,
}

impl AcpProvider {
    /// Create a native ACP owner from the registry identity and configured launch command.
    pub fn new(
        id: forge_provider_api::ProviderId,
        command: Vec<String>,
        runtime_root: PathBuf,
    ) -> Result<Self, ProviderError> {
        let kind = match id.as_str() {
            "claude" => AcpKind::Claude,
            "antigravity" => AcpKind::Antigravity,
            _ => {
                return Err(failure(
                    ProviderOperation::InitializeSession,
                    ProviderFailureKind::Rejected,
                ));
            }
        };
        if command.is_empty() {
            return Err(failure(
                ProviderOperation::InitializeSession,
                ProviderFailureKind::Rejected,
            ));
        }
        Ok(Self {
            kind,
            runtime: AcpRuntime::new(kind, command, &runtime_root),
            runtime_root,
            bindings: Mutex::new(HashMap::new()),
            owners: Mutex::new(HashMap::new()),
            preparation: Mutex::new(HashMap::new()),
        })
    }

    async fn binding(&self, id: &BindingId) -> Result<Arc<AcpBinding>, ProviderError> {
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
        session.collect_finished_prompt().await?;
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
        let mut active = session.active.lock().await;
        if active.is_some() {
            return Err(failure(
                ProviderOperation::AdmitInput,
                ProviderFailureKind::Rejected,
            ));
        }
        *active = Some(ActiveExecution {
            scope: request.scope.clone(),
            cancelled: false,
            task: None,
        });
        drop(active);
        *session.rollback_cancelled.lock().await = false;
        if let Err(error) = request.turn_control.before_send(&request.scope).await {
            let mut active = session.active.lock().await;
            if active
                .as_ref()
                .is_some_and(|active| active.scope == request.scope)
            {
                active.take();
            }
            return Err(reject_before_dispatch(&request.receipt, error).await);
        }
        let mut active = session.active.lock().await;
        if active.as_ref().is_some_and(|active| active.cancelled) {
            active.take();
            drop(active);
            *session.rollback_cancelled.lock().await = true;
            return Err(reject_before_dispatch(&request.receipt, ProviderError::Cancelled).await);
        }
        let (started, start) = tokio::sync::oneshot::channel();
        let (accepted, acceptance) = tokio::sync::oneshot::channel();
        let session_for_task = session.clone();
        let binding_for_task = binding.clone();
        let task = tokio::spawn(async move {
            let _ = start.await;
            run_prompt(
                session_for_task.clone(),
                &binding_for_task,
                request,
                accepted,
            )
            .await
        });
        active.as_mut().expect("active execution inserted").task =
            Some(Arc::new(crate::retained_task::RetainedTask::new(task)));
        let _ = started.send(());
        drop(active);
        acceptance.await.map_err(|_| {
            failure(
                ProviderOperation::AdmitInput,
                ProviderFailureKind::Transport,
            )
        })?
    }

    async fn initialize_handshake(
        &self,
        runtime: &super::runtime::RuntimeStatus,
        workspace: &str,
    ) -> Result<serde_json::Value, ProviderError> {
        let profile = super::profile::create(self.kind, &self.runtime_root, &[])?;
        let connection = super::AcpConnection::launch(
            super::AcpLaunch {
                command: &runtime.command,
                environment: &runtime.environment,
                workspace,
                profile: (self.kind == AcpKind::Antigravity).then_some(profile.path()),
            },
            Arc::new(AcpSessionState::new_native(self.kind)),
        )
        .await
        .map_err(|_| {
            failure(
                ProviderOperation::InitializeSession,
                ProviderFailureKind::Transport,
            )
        })?;
        let requirement = super::conversation::runtime_requirement(
            self.kind,
            runtime
                .command
                .first()
                .map(PathBuf::from)
                .unwrap_or_else(|| "ACP".into()),
        )?;
        let result = async {
            let initialized = connection
                .request("initialize", initialize_params())
                .await
                .map_err(|_| {
                    failure(
                        ProviderOperation::InitializeSession,
                        ProviderFailureKind::Protocol,
                    )
                })?;
            super::conversation::validate_initialization(&requirement, &initialized)?;
            Ok(initialized)
        }
        .await;
        let collected = connection.shutdown().await.map_err(|_| {
            failure(
                ProviderOperation::CollectNativeRuntime,
                ProviderFailureKind::Transport,
            )
        });
        match (result, collected) {
            (Ok(initialized), Ok(())) => Ok(initialized),
            (Err(cause), Ok(())) => Err(cause),
            (Ok(_), Err(cleanup)) => Err(cleanup),
            (Err(cause), Err(cleanup)) => Err(ProviderError::PreparationCleanup {
                cause: Box::new(cause),
                cleanup: Box::new(cleanup),
            }),
        }
    }
}

fn initialize_params() -> serde_json::Value {
    serde_json::json!({
        "protocolVersion":1,
        "clientInfo":{"name":"forge","title":"Forge","version":env!("CARGO_PKG_VERSION")},
        "clientCapabilities":{"auth":{"terminal":true},"session":{"compaction":{},"configOptions":{"boolean":{}}},"subagents":{}}
    })
}

async fn run_prompt(
    session: Arc<AcpBinding>,
    binding: &BindingId,
    request: PromptRequest,
    accepted: tokio::sync::oneshot::Sender<Result<InputAcceptance, ProviderError>>,
) -> Result<(), ProviderError> {
    let mut accepted = Some(accepted);
    let result = async {
        let _operation = session.operation.lock().await;
        if let Err(error) = session
            .conversation
            .bind_tool_endpoint(request.tool_endpoint.as_ref())
            .await
        {
            return Err(reject_before_dispatch(&request.receipt, error).await);
        }
        let event_sink = session.sink.invocation_sink(&request.scope)?;
        session
            .state
            .begin_native(
                session.conversation.id(),
                event_sink.clone(),
                request.permissions.clone(),
            )
            .await?;
        let address = session
            .state
            .address(session.conversation.id())
            .await
            .ok_or_else(|| failure(ProviderOperation::AdmitInput, ProviderFailureKind::Protocol))?;
        let current_model = {
            let settings = session.settings.lock().await;
            settings.model.clone()
        };
        if request.goal.is_some() {
            let error = forge_provider_api::capability::UnsupportedCapability(
                forge_provider_api::capability::ProviderFeature::NativeGoal,
            )
            .into();
            return Err(reject_before_dispatch(&request.receipt, error).await);
        }
        let prompt = match prompt_content(&request, session.conversation.supports_images()) {
            Ok(prompt) => prompt,
            Err(error) => return Err(reject_before_dispatch(&request.receipt, error).await),
        };
        if let Err(error) = configure_model(
            &session,
            &request.configuration.model,
            &current_model,
            request.mode,
        )
        .await
        {
            return Err(reject_before_dispatch(&request.receipt, error).await);
        }
        {
            let mut settings = session.settings.lock().await;
            settings.model = request.configuration.model.clone();
            settings.access = request.configuration.access.clone();
            settings.approval = request.configuration.approval;
        }
        let response = session
            .conversation
            .submit(
                super::submission::AcpSubmission::Prompt,
                serde_json::json!({"sessionId":session.conversation.id(),"prompt":prompt}),
                Some(request.receipt.clone()),
            )
            .await;
        let response = match response {
            Ok(response) => response,
            Err(error) => {
                let started = session.state.execution_started().await;
                let proven_unconsumed = match &error {
                    ProviderError::NotConsumed { .. } | ProviderError::Cancelled => true,
                    ProviderError::Failure(native) => {
                        native.kind() == ProviderFailureKind::Rejected
                    }
                    _ => false,
                };
                let uncertain_delivery = !proven_unconsumed;
                let mut cleanup = Vec::new();
                if let Err(cleanup_error) = request.turn_control.close(&request.scope).await {
                    cleanup.push(cleanup_error);
                }
                if let Err(cleanup_error) = request
                    .host_effects
                    .settle_foreground(binding, &request.scope)
                    .await
                {
                    let kind = match cleanup_error {
                        forge_provider_api::session::HostEffectFailure::Rejected => {
                            ProviderFailureKind::Rejected
                        }
                        forge_provider_api::session::HostEffectFailure::Uncertain
                        | forge_provider_api::session::HostEffectFailure::Retired => {
                            ProviderFailureKind::Unknown
                        }
                    };
                    cleanup.push(failure(ProviderOperation::HostEffectPublication, kind));
                }
                if started || uncertain_delivery {
                    let native_failure = match &error {
                        ProviderError::DeliveryUncertain => {
                            forge_provider_api::lifecycle::ObservationFailure::DeliveryUncertain
                        }
                        ProviderError::Failure(native)
                            if native.kind() == ProviderFailureKind::Protocol =>
                        {
                            forge_provider_api::lifecycle::ObservationFailure::Protocol
                        }
                        _ => forge_provider_api::lifecycle::ObservationFailure::Disconnected,
                    };
                    if let Err(cleanup_error) = session
                        .state
                        .finish(forge_provider_api::turn::TurnOutcome::Failed)
                        .await
                    {
                        cleanup.push(cleanup_error);
                    }
                    let event = execution_observation(
                        &request.scope,
                        forge_provider_api::lifecycle::ExecutionUpdate::ObservationLost {
                            failure: native_failure,
                            completion: if started {
                                Some(session.state.take_completion().await.into())
                            } else {
                                None
                            },
                        },
                    );
                    match event {
                        Ok(event) => {
                            if let Err(cleanup_error) = event_sink
                                .send_wait(ProviderEvent::Execution { observation: event })
                                .await
                            {
                                cleanup.push(cleanup_error.into());
                            }
                        }
                        Err(cleanup_error) => cleanup.push(cleanup_error),
                    }
                } else {
                    session.state.revoke_callbacks().await;
                    if let Err(cleanup_error) = session
                        .state
                        .finish(forge_provider_api::turn::TurnOutcome::Failed)
                        .await
                    {
                        cleanup.push(cleanup_error);
                    }
                }
                return Err(with_cleanup(error, cleanup));
            }
        };
        *session.last_prompt.lock().await = Some(PromptBaseline::from_request(&request));
        if let Some(sender) = accepted.take() {
            let _ = sender.send(Ok(InputAcceptance {
                submission_id: request.submission_id.clone(),
            }));
        }
        let native_outcome = match super::prompt_outcome(&response) {
            Ok(super::PromptOutcome::Completed) => forge_provider_api::turn::TurnOutcome::Completed,
            Ok(super::PromptOutcome::Cancelled) => forge_provider_api::turn::TurnOutcome::Cancelled,
            Err(_) => {
                return Err(failure(
                    ProviderOperation::AdmitInput,
                    ProviderFailureKind::Protocol,
                ));
            }
        };
        if let Err(error) = request.turn_control.close(&request.scope).await {
            let publication = publish_publication_failed(
                &session,
                &request.scope,
                &event_sink,
                address,
                &request.submission_id,
                native_outcome,
                forge_provider_api::session::HostEffectFailure::Uncertain,
            )
            .await;
            return Err(match publication {
                Ok(()) => error,
                Err(cleanup) => with_cleanup(error, vec![cleanup]),
            });
        }
        if let Err(host_failure) = request
            .host_effects
            .settle_foreground(binding, &request.scope)
            .await
        {
            let publication = publish_publication_failed(
                &session,
                &request.scope,
                &event_sink,
                address,
                &request.submission_id,
                native_outcome,
                host_failure,
            )
            .await;
            let error = failure(
                ProviderOperation::HostEffectPublication,
                ProviderFailureKind::Unknown,
            );
            return Err(match publication {
                Ok(()) => error,
                Err(cleanup) => with_cleanup(error, vec![cleanup]),
            });
        }
        let status = match request.turn_control.status(&request.scope).await {
            Ok(status) => status,
            Err(error) => {
                let publication = publish_publication_failed(
                    &session,
                    &request.scope,
                    &event_sink,
                    address,
                    &request.submission_id,
                    native_outcome,
                    forge_provider_api::session::HostEffectFailure::Uncertain,
                )
                .await;
                return Err(match publication {
                    Ok(()) => error,
                    Err(cleanup) => with_cleanup(error, vec![cleanup]),
                });
            }
        };
        let outcome = if native_outcome == forge_provider_api::turn::TurnOutcome::Cancelled
            || status.cancelled
        {
            forge_provider_api::turn::TurnOutcome::Cancelled
        } else if status.error.is_some() {
            forge_provider_api::turn::TurnOutcome::Failed
        } else {
            forge_provider_api::turn::TurnOutcome::Completed
        };
        let mut terminal_errors = Vec::new();
        if let Err(error) = session.state.finish(outcome).await {
            terminal_errors.push(error);
        }
        let completion = session.state.take_completion().await.into();
        let result = match outcome {
            forge_provider_api::turn::TurnOutcome::Completed => {
                forge_provider_api::lifecycle::ExecutionResult::Completed { completion }
            }
            forge_provider_api::turn::TurnOutcome::Cancelled => {
                forge_provider_api::lifecycle::ExecutionResult::Cancelled { completion }
            }
            forge_provider_api::turn::TurnOutcome::Failed
            | forge_provider_api::turn::TurnOutcome::Interrupted => {
                forge_provider_api::lifecycle::ExecutionResult::Failed {
                    completion,
                    failure: forge_provider_api::lifecycle::ExecutionFailure::Terminal {
                        kind: ProviderFailureKind::Unknown,
                    },
                }
            }
        };
        let update = if session.state.execution_started().await {
            forge_provider_api::lifecycle::ExecutionUpdate::Finished {
                address,
                covered_submissions: vec![request.submission_id.clone()],
                completed_at_ms: super::children::timestamp(),
                result,
            }
        } else {
            forge_provider_api::lifecycle::ExecutionUpdate::ResolvedWithoutExecution {
                covered_submissions: vec![request.submission_id.clone()],
                completed_at_ms: super::children::timestamp(),
                result,
            }
        };
        match execution_observation(&request.scope, update) {
            Ok(event) => {
                if let Err(error) = event_sink
                    .send_wait(ProviderEvent::Execution { observation: event })
                    .await
                {
                    terminal_errors.push(error.into());
                }
            }
            Err(error) => terminal_errors.push(error),
        }
        if terminal_errors.is_empty() {
            Ok(())
        } else {
            Err(combine_errors(terminal_errors))
        }
    }
    .await;
    match result {
        Ok(()) => Ok(()),
        Err(error) => {
            let mut error = error;
            if accepted.is_some() && session.state.has_active_work().await {
                session.state.revoke_callbacks().await;
                if let Err(cleanup) = session
                    .state
                    .finish(forge_provider_api::turn::TurnOutcome::Failed)
                    .await
                {
                    error = with_cleanup(error, vec![cleanup]);
                }
            }
            if let Some(sender) = accepted.take() {
                // A delivered rejection already reports the operation failure to its caller.
                return match sender.send(Err(error)) {
                    Ok(()) => Ok(()),
                    Err(result) => result.map(|_| ()),
                };
            }
            // Accepted work must never disappear without a terminal observation or explicit loss.
            session
                .sink
                .fail(events::EventDeliveryError::Observation(match &error {
                    ProviderError::Failure(failure)
                        if failure.kind() == ProviderFailureKind::Protocol =>
                    {
                        forge_provider_api::lifecycle::ObservationFailure::Protocol
                    }
                    _ => forge_provider_api::lifecycle::ObservationFailure::Disconnected,
                }));
            Err(error)
        }
    }
}

fn execution_observation(
    scope: &forge_provider_api::InvocationScope,
    update: forge_provider_api::lifecycle::ExecutionUpdate,
) -> Result<forge_provider_api::lifecycle::ExecutionObservation, ProviderError> {
    forge_provider_api::lifecycle::ExecutionObservation::new(scope.clone(), update)
        .map_err(Into::into)
}

/// Apply one invocation's supported ACP model and reasoning choices before prompt dispatch.
async fn configure_model(
    session: &AcpBinding,
    requested: &forge_provider_api::session::ModelSelection,
    retained: &forge_provider_api::session::ModelSelection,
    mode: forge_provider_api::PromptMode,
) -> Result<(), ProviderError> {
    use forge_provider_api::ServiceTier;
    use forge_provider_api::generation::ReasoningSelection;

    if requested.service_tier != ServiceTier::Standard {
        return Err(forge_provider_api::capability::UnsupportedCapability(
            forge_provider_api::capability::ProviderFeature::ServiceTier,
        )
        .into());
    }
    if requested.context_window.is_some() {
        return Err(forge_provider_api::capability::UnsupportedCapability(
            forge_provider_api::capability::ProviderFeature::ContextWindowSelection,
        )
        .into());
    }
    if requested.model.is_none() && retained.model.is_some() {
        return Err(forge_provider_api::capability::UnsupportedCapability(
            forge_provider_api::capability::ProviderFeature::ModelDefaultReset,
        )
        .into());
    }
    if requested.reasoning == ReasoningSelection::Default
        && retained.reasoning != ReasoningSelection::Default
    {
        return Err(forge_provider_api::capability::UnsupportedCapability(
            forge_provider_api::capability::ProviderFeature::ReasoningDefaultReset,
        )
        .into());
    }

    let models = session.conversation.models().await.map_err(|_| {
        failure(
            ProviderOperation::ReconcileSettings,
            ProviderFailureKind::Protocol,
        )
    })?;
    let selected_model = match requested.model.as_deref() {
        Some(model) => {
            if !models.iter().any(|candidate| candidate.id == model) {
                return Err(failure(
                    ProviderOperation::ReconcileSettings,
                    ProviderFailureKind::Rejected,
                ));
            }
            Some(model)
        }
        None => models
            .iter()
            .find(|model| model.is_default)
            .map(|model| model.id.as_str()),
    };
    if let ReasoningSelection::Level(level) = &requested.reasoning
        && !selected_model
            .and_then(|id| models.iter().find(|model| model.id == id))
            .is_some_and(|model| model.reasoning.contains(level))
    {
        return Err(forge_provider_api::capability::UnsupportedCapability(
            forge_provider_api::capability::ProviderFeature::EffortSelection,
        )
        .into());
    }
    let effort = match &requested.reasoning {
        ReasoningSelection::Default => "default",
        ReasoningSelection::Level(level) => level.as_str(),
        ReasoningSelection::Lowest => {
            let reasoning = selected_model
                .and_then(|id| models.iter().find(|model| model.id == id))
                .and_then(|model| model.reasoning.first())
                .ok_or({
                    forge_provider_api::capability::UnsupportedCapability(
                        forge_provider_api::capability::ProviderFeature::EffortSelection,
                    )
                })?;
            reasoning.as_str()
        }
    };
    session
        .conversation
        .configure(
            requested.model.as_deref().unwrap_or("default"),
            effort,
            mode,
        )
        .await
        .map_err(|_| {
            failure(
                ProviderOperation::ReconcileSettings,
                ProviderFailureKind::Protocol,
            )
        })
}

/// Preserve the primary provider result when required cleanup also fails.
fn with_cleanup(primary: ProviderError, cleanup: Vec<ProviderError>) -> ProviderError {
    if cleanup.is_empty() {
        primary
    } else {
        ProviderError::Combined {
            primary: Box::new(primary),
            secondary: cleanup,
        }
    }
}

/// Preserve the first failure and attach every independent publication cleanup failure.
fn combine_errors(mut errors: Vec<ProviderError>) -> ProviderError {
    let primary = errors.remove(0);
    with_cleanup(primary, errors)
}

/// Record a definite no-send result while preserving an independent receipt failure.
async fn reject_before_dispatch(
    receipt: &Arc<dyn forge_provider_api::submission::InputReceipt>,
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

async fn publish_publication_failed(
    session: &AcpBinding,
    scope: &forge_provider_api::InvocationScope,
    sink: &ProviderEventSink,
    address: forge_provider_api::TurnAddress,
    submission: &forge_provider_api::SubmissionId,
    outcome: forge_provider_api::turn::TurnOutcome,
    failure: forge_provider_api::session::HostEffectFailure,
) -> Result<(), ProviderError> {
    let finish_error = session.state.finish(outcome).await.err();
    let completion = session.state.take_completion().await.into();
    let result = match outcome {
        forge_provider_api::turn::TurnOutcome::Completed => {
            forge_provider_api::lifecycle::ExecutionResult::Completed { completion }
        }
        forge_provider_api::turn::TurnOutcome::Cancelled => {
            forge_provider_api::lifecycle::ExecutionResult::Cancelled { completion }
        }
        forge_provider_api::turn::TurnOutcome::Failed
        | forge_provider_api::turn::TurnOutcome::Interrupted => {
            forge_provider_api::lifecycle::ExecutionResult::Failed {
                completion,
                failure: forge_provider_api::lifecycle::ExecutionFailure::Terminal {
                    kind: ProviderFailureKind::Unknown,
                },
            }
        }
    };
    let observation = execution_observation(
        scope,
        forge_provider_api::lifecycle::ExecutionUpdate::PublicationFailed {
            address,
            covered_submissions: vec![submission.clone()],
            completed_at_ms: super::children::timestamp(),
            result,
            failure,
        },
    );
    let publication_result: Result<(), ProviderError> = match observation {
        Ok(observation) => sink
            .send_wait(ProviderEvent::Execution { observation })
            .await
            .map_err(Into::into),
        Err(error) => Err(error),
    };
    match (finish_error, publication_result) {
        (None, Ok(())) => Ok(()),
        (Some(error), Ok(())) | (None, Err(error)) => Err(error),
        (Some(primary), Err(secondary)) => Err(with_cleanup(primary, vec![secondary])),
    }
}

struct AcpCheckpoint<'token> {
    accepted: bool,
    session_id: &'token str,
}

fn parse_checkpoint(
    token: &forge_provider_api::RecoveryToken,
) -> Result<AcpCheckpoint<'_>, ProviderError> {
    let value = std::str::from_utf8(token.as_bytes()).map_err(|_| {
        failure(
            ProviderOperation::RecoverSession,
            ProviderFailureKind::Protocol,
        )
    })?;
    let mut fields = value.splitn(3, '\n');
    let state = fields.next();
    let method = fields.next();
    let session_id = fields.next().filter(|id| !id.trim().is_empty());
    let (Some(state), Some(method), Some(session_id)) = (state, method, session_id) else {
        return Err(failure(
            ProviderOperation::RecoverSession,
            ProviderFailureKind::Protocol,
        ));
    };
    if !matches!(method, "session/prompt" | "_session/steering") {
        return Err(failure(
            ProviderOperation::RecoverSession,
            ProviderFailureKind::Protocol,
        ));
    }
    let accepted = match state {
        "accepted" => true,
        "v1" => false,
        _ => {
            return Err(failure(
                ProviderOperation::RecoverSession,
                ProviderFailureKind::Protocol,
            ));
        }
    };
    Ok(AcpCheckpoint {
        accepted,
        session_id,
    })
}

#[async_trait]
impl forge_provider_api::provider::Provider for AcpProvider {
    async fn initialize_session(
        &self,
        request: SessionRequest,
    ) -> Result<SessionBinding, ProviderError> {
        let preparation = self
            .preparation
            .lock()
            .await
            .entry(request.owner.clone())
            .or_insert_with(|| Arc::new(Mutex::new(())))
            .clone();
        let _preparation = preparation.lock().await;
        let existing = self.owners.lock().await.get(&request.owner).cloned();
        if let Some(existing) = existing {
            self.retire_session(&existing).await?;
        }
        let runtime = self.runtime.resolve().map_err(|_| {
            failure(
                ProviderOperation::InitializeSession,
                ProviderFailureKind::Runtime,
            )
        })?;
        let binding_id = BindingId::new(uuid::Uuid::new_v4().to_string()).map_err(|_| {
            failure(
                ProviderOperation::InitializeSession,
                ProviderFailureKind::Unknown,
            )
        })?;
        let scope = forge_provider_api::EventScope::SessionOperation {
            owner: request.owner.clone(),
            operation_id: binding_id.as_str().to_owned(),
        };
        let (sink, stream) = events::channel(binding_id.clone(), scope);
        let profile =
            super::profile::create(self.kind, &self.runtime_root, &request.resources.skills)?;
        let selection = request.resources.clone();
        let state = Arc::new(AcpSessionState::new_native(self.kind));
        let conversation = AcpConversation::open(
            super::AcpLaunch {
                command: &runtime.command,
                environment: &runtime.environment,
                workspace: &request.workspace,
                profile: (self.kind == AcpKind::Antigravity).then_some(profile.path()),
            },
            state.clone(),
            ConversationOptions {
                kind: self.kind,
                workspace: request.workspace.clone(),
                session_id: request
                    .provider_session_id
                    .as_ref()
                    .map(ToString::to_string),
                control_endpoint: request.tool_endpoint.clone(),
                mcp_servers: super::resources::mcp_launches(&request.resources.mcp)?,
                resources: mcp_definitions(&selection),
                profile: profile.path().to_owned(),
                skills: request
                    .resources
                    .skills
                    .iter()
                    .filter(|skill| skill.enabled)
                    .map(|skill| skill.name.clone())
                    .collect(),
            },
        )
        .await?;
        let session_id: forge_provider_api::ProviderSessionId =
            conversation.id().to_owned().try_into().map_err(|_| {
                failure(
                    ProviderOperation::InitializeSession,
                    ProviderFailureKind::Protocol,
                )
            })?;
        let binding = SessionBinding {
            id: binding_id.clone(),
            provider_session_id: Some(session_id),
        };
        let retained = Arc::new(AcpBinding {
            binding: binding.clone(),
            owner: request.owner.clone(),
            conversation: Arc::new(conversation),
            state,
            sink,
            stream: Mutex::new(Some(stream)),
            resources: Mutex::new(Some(profile)),
            _lease: request.resource_lease,
            selection,
            settings: Mutex::new(AcpSettings {
                model: request.model,
                access: request.access,
                approval: request.approval,
            }),
            operation: Mutex::new(()),
            active: Mutex::new(None),
            rollback_cancelled: Mutex::new(false),
            last_prompt: Mutex::new(None),
            continuations: Mutex::new(HashMap::new()),
        });
        self.bindings
            .lock()
            .await
            .insert(binding_id.clone(), retained);
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
        request: forge_provider_api::session::ContinuationRequest,
    ) -> Result<forge_provider_api::session::ContinuationOutcome, ProviderError> {
        match request {
            forge_provider_api::session::ContinuationRequest::NotConsumed {
                request,
                authorization,
            } => {
                let session = self.binding(binding).await?;
                let mut grants = session.continuations.lock().await;
                if !take_continuation_grant(
                    &mut grants,
                    authorization.as_bytes(),
                    binding,
                    &session.owner,
                    &request,
                ) {
                    return Err(failure(
                        ProviderOperation::AdmitInput,
                        ProviderFailureKind::Rejected,
                    ));
                }
                drop(grants);
                let operation = session.operation.lock().await;
                drop(operation);
                session.collect_prompt().await?;
                Ok(
                    forge_provider_api::session::ContinuationOutcome::InputAccepted(
                        self.submit(binding, *request).await?,
                    ),
                )
            }
            forge_provider_api::session::ContinuationRequest::RetryableExecution { .. } => Err(
                failure(ProviderOperation::AdmitInput, ProviderFailureKind::Rejected),
            ),
        }
    }

    async fn session_steer(
        &self,
        binding: &BindingId,
        request: SteeringRequest,
    ) -> Result<forge_provider_api::session::SteeringAcceptance, ProviderError> {
        if self.kind != AcpKind::Claude {
            return Err(failure(
                ProviderOperation::AdmitInput,
                ProviderFailureKind::Rejected,
            ));
        }
        let session = self.binding(binding).await?;
        if request.scope.owner != session.owner {
            return Err(failure(
                ProviderOperation::AdmitInput,
                ProviderFailureKind::Rejected,
            ));
        }
        if session
            .active
            .lock()
            .await
            .as_ref()
            .is_none_or(|active| active.scope != request.scope)
        {
            return Err(failure(
                ProviderOperation::AdmitInput,
                ProviderFailureKind::Rejected,
            ));
        }
        let base_prompt = session
            .last_prompt
            .lock()
            .await
            .clone()
            .filter(|prompt| prompt.scope == request.scope)
            .ok_or_else(|| failure(ProviderOperation::AdmitInput, ProviderFailureKind::Rejected))?;
        let input = prompt_input(&request.input);
        let text = request.task.as_ref().map_or(input.clone(), |task| {
            format!("{}\n\n{input}", task.heading())
        });
        let prompt = super::attachments::input(
            text,
            &request.attachments,
            session.conversation.supports_images(),
        )?;
        let receipt = request.delivery.receipt().cloned();
        let submission_id = request.delivery.submission_id().cloned();
        let response = match session
            .conversation
            .steer(
                serde_json::json!({"sessionId":session.conversation.id(),"prompt":prompt}),
                receipt,
            )
            .await
        {
            Ok(response) => response,
            Err(ProviderError::NotConsumed { continuation }) => {
                let grant =
                    ContinuationGrant::from_steering(binding.clone(), &request, &base_prompt);
                let mut grants = session.continuations.lock().await;
                if grants.contains_key(continuation.as_bytes()) {
                    return Err(failure(
                        ProviderOperation::AdmitInput,
                        ProviderFailureKind::Rejected,
                    ));
                }
                grants.insert(continuation.as_bytes().to_vec(), grant);
                return Err(ProviderError::NotConsumed { continuation });
            }
            Err(error) => return Err(error),
        };
        if response == super::submission::SteeringAcceptance::UnobservableExecution {
            self.fence_session(binding).await?;
            return Err(ProviderError::Observation(
                forge_provider_api::lifecycle::ObservationFailure::Disconnected,
            ));
        }
        Ok(match submission_id {
            Some(submission_id) => {
                forge_provider_api::session::SteeringAcceptance::Journaled { submission_id }
            }
            None => forge_provider_api::session::SteeringAcceptance::Context,
        })
    }

    async fn setup(
        &self,
        request: forge_provider_api::catalog::ProviderCatalogRequest,
    ) -> Result<forge_provider_api::ProviderSetup, ProviderError> {
        let status = self.runtime.status();
        let provider_version = match status.provider_version().await {
            Ok(Some(version)) => forge_provider_api::VersionObservation::Known { version },
            Ok(None) => forge_provider_api::VersionObservation::NotReported,
            Err(ProviderError::Failure(error)) => forge_provider_api::VersionObservation::Failed {
                failure: error.kind(),
            },
            Err(_) => forge_provider_api::VersionObservation::Failed {
                failure: ProviderFailureKind::Unknown,
            },
        };
        let origin = if status.managed {
            forge_provider_api::RuntimeOrigin::Managed
        } else {
            forge_provider_api::RuntimeOrigin::Configured
        };
        let mut versions = forge_provider_api::RuntimeVersions {
            provider: provider_version,
            adapter: status.adapter_version.clone().map_or(
                forge_provider_api::VersionObservation::NotReported,
                |version| forge_provider_api::VersionObservation::Known { version },
            ),
        };
        if !status.available {
            return Ok(forge_provider_api::ProviderSetup {
                runtime: forge_provider_api::RuntimeAvailability::Missing,
                origin,
                versions,
                authentication: Vec::new(),
            });
        }
        let initialized = match self.initialize_handshake(&status, &request.workspace).await {
            Ok(initialized) => initialized,
            Err(ProviderError::RuntimeVersion(error)) => {
                versions.adapter = match error {
                    forge_provider_api::version::RuntimeVersionError::Unsupported {
                        detected,
                        ..
                    } => forge_provider_api::VersionObservation::Known {
                        version: detected.to_string(),
                    },
                    forge_provider_api::version::RuntimeVersionError::Missing { .. } => {
                        forge_provider_api::VersionObservation::NotReported
                    }
                    forge_provider_api::version::RuntimeVersionError::Malformed { .. } => {
                        forge_provider_api::VersionObservation::Failed {
                            failure: ProviderFailureKind::Protocol,
                        }
                    }
                    forge_provider_api::version::RuntimeVersionError::Probe { .. } => {
                        forge_provider_api::VersionObservation::Failed {
                            failure: ProviderFailureKind::Initialization,
                        }
                    }
                };
                return Ok(forge_provider_api::ProviderSetup {
                    runtime: forge_provider_api::RuntimeAvailability::Incompatible,
                    origin,
                    versions,
                    authentication: Vec::new(),
                });
            }
            Err(_) => {
                return Ok(forge_provider_api::ProviderSetup {
                    runtime: forge_provider_api::RuntimeAvailability::Unavailable,
                    origin,
                    versions,
                    authentication: Vec::new(),
                });
            }
        };
        let authentication = initialized["authMethods"]
            .as_array()
            .into_iter()
            .flatten()
            .filter_map(|method| {
                let id = forge_provider_api::provider::AuthenticationMethodId::new(
                    method["id"].as_str()?,
                )
                .ok()?;
                let terminal = method["type"] == "terminal";
                let interaction = if terminal {
                    let program = status.command.first().map(PathBuf::from)?;
                    let arguments = status
                        .command
                        .iter()
                        .skip(1)
                        .cloned()
                        .chain(
                            method["args"]
                                .as_array()
                                .into_iter()
                                .flatten()
                                .filter_map(serde_json::Value::as_str)
                                .map(str::to_owned),
                        )
                        .collect();
                    forge_provider_api::provider::AuthenticationInteraction::Terminal {
                        launch: forge_provider_api::provider::TerminalLaunch {
                            program,
                            arguments,
                            environment: status.environment.clone(),
                            working_directory: Some(PathBuf::from(&request.workspace)),
                        },
                    }
                } else {
                    forge_provider_api::provider::AuthenticationInteraction::ProviderManaged
                };
                Some(forge_provider_api::provider::AuthenticationMethod {
                    id,
                    label: method["name"].as_str().unwrap_or("Sign in").to_owned(),
                    description: method["description"]
                        .as_str()
                        .unwrap_or_default()
                        .to_owned(),
                    interaction,
                })
            })
            .collect();
        if let Some(version) = initialized
            .pointer("/agentInfo/version")
            .and_then(serde_json::Value::as_str)
        {
            versions.adapter = forge_provider_api::VersionObservation::Known {
                version: version.to_owned(),
            };
        }
        Ok(forge_provider_api::ProviderSetup {
            runtime: forge_provider_api::RuntimeAvailability::Available,
            origin,
            versions,
            authentication,
        })
    }

    async fn capabilities(
        &self,
        request: forge_provider_api::catalog::ProviderCatalogRequest,
    ) -> Result<forge_provider_api::capability::CapabilityManifest, ProviderError> {
        let owner = request.owner().map_err(|_| {
            failure(
                ProviderOperation::InitializeSession,
                ProviderFailureKind::Rejected,
            )
        })?;
        let Some(binding) = self.owners.lock().await.get(&owner).cloned() else {
            return Ok(
                forge_provider_api::capability::CapabilityManifest::from_descriptor(
                    self.descriptor(),
                ),
            );
        };
        let session = self.binding(&binding).await?;
        let mut descriptor = self.descriptor();
        descriptor.capability = session
            .conversation
            .capabilities(descriptor.capability)
            .await;
        let mut manifest =
            forge_provider_api::capability::CapabilityManifest::from_descriptor(descriptor);
        manifest.provider_version = session.conversation.version();
        Ok(manifest)
    }

    async fn reconcile_settings(
        &self,
        request: forge_provider_api::catalog::ProviderCatalogRequest,
        settings: forge_provider_api::session::SessionSettings,
        resources: forge_provider_api::session::ResourceSelection,
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
        if session.has_active_prompt().await || session.state.has_active_work().await {
            return Err(failure(
                ProviderOperation::ReconcileSettings,
                ProviderFailureKind::Rejected,
            ));
        }
        if !same_resources(&session.selection, &resources) {
            return Err(forge_provider_api::capability::UnsupportedCapability(
                forge_provider_api::capability::ProviderFeature::ResourceSelection,
            )
            .into());
        }
        let stored_model = {
            let current = session.settings.lock().await;
            current.model.clone()
        };
        let model = settings.model.unwrap_or_else(|| stored_model.clone());
        configure_model(
            &session,
            &model,
            &stored_model,
            forge_provider_api::PromptMode::Chat,
        )
        .await?;
        let mut current = session.settings.lock().await;
        current.model = model;
        current.access = settings.access.unwrap_or_else(|| current.access.clone());
        current.approval = settings.approval.unwrap_or(current.approval);
        Ok(())
    }

    async fn model_list(
        &self,
        request: forge_provider_api::catalog::ProviderCatalogRequest,
    ) -> Result<Vec<forge_provider_api::ProviderModel>, ProviderError> {
        let owner = request.owner().map_err(|_| {
            failure(
                ProviderOperation::ReconcileSettings,
                ProviderFailureKind::Rejected,
            )
        })?;
        let Some(binding) = self.owners.lock().await.get(&owner).cloned() else {
            return Ok(Vec::new());
        };
        self.binding(&binding)
            .await?
            .conversation
            .models()
            .await
            .map_err(|_| {
                failure(
                    ProviderOperation::ReconcileSettings,
                    ProviderFailureKind::Protocol,
                )
            })
    }

    async fn skill_list(
        &self,
        request: forge_provider_api::catalog::ProviderCatalogRequest,
    ) -> Result<Vec<forge_provider_api::catalog::SkillDefinition>, ProviderError> {
        let owner = request.owner().map_err(|_| {
            failure(
                ProviderOperation::ReconcileSettings,
                ProviderFailureKind::Rejected,
            )
        })?;
        let Some(binding) = self.owners.lock().await.get(&owner).cloned() else {
            return Ok(Vec::new());
        };
        Ok(self.binding(&binding).await?.selection.skills.clone())
    }

    async fn mcp_configuration(
        &self,
        request: forge_provider_api::catalog::ProviderCatalogRequest,
    ) -> Result<Vec<forge_provider_api::catalog::McpDefinition>, ProviderError> {
        let owner = request.owner().map_err(|_| {
            failure(
                ProviderOperation::ReconcileSettings,
                ProviderFailureKind::Rejected,
            )
        })?;
        let Some(binding) = self.owners.lock().await.get(&owner).cloned() else {
            return Ok(Vec::new());
        };
        Ok(mcp_definitions(&self.binding(&binding).await?.selection))
    }

    async fn mcp_list(
        &self,
        request: forge_provider_api::catalog::ProviderCatalogRequest,
    ) -> Result<Vec<forge_provider_api::catalog::McpDefinition>, ProviderError> {
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
        let mut definitions = mcp_definitions(&session.selection);
        let catalog = session.state.catalog().await;
        if let Some(resources) = catalog["mcpResources"].as_array() {
            for definition in &mut definitions {
                if let Some(native) = resources
                    .iter()
                    .find(|resource| resource["name"] == definition.name)
                {
                    match native["state"].as_str() {
                        Some("requested") => {
                            definition.status = forge_provider_api::catalog::McpStatus::Loading
                        }
                        Some("disabled") => {
                            definition.status = forge_provider_api::catalog::McpStatus::Disabled
                        }
                        Some("unavailable") => {
                            definition.status = forge_provider_api::catalog::McpStatus::Unavailable;
                            definition.status_detail =
                                Some(normalized_mcp_reason(native["reason"].as_str()));
                        }
                        _ => {}
                    }
                }
            }
        }
        Ok(definitions)
    }

    async fn authenticate(
        &self,
        request: forge_provider_api::catalog::ProviderCatalogRequest,
        method: forge_provider_api::provider::AuthenticationMethodId,
    ) -> Result<(), ProviderError> {
        let status = self.runtime.status();
        if !status.available {
            return Err(failure(
                ProviderOperation::Authenticate,
                ProviderFailureKind::Runtime,
            ));
        }
        let profile = super::profile::create(self.kind, &self.runtime_root, &[])?;
        let state = Arc::new(AcpSessionState::new_native(self.kind));
        let connection = super::AcpConnection::launch(
            super::AcpLaunch {
                command: &status.command,
                environment: &status.environment,
                workspace: &request.workspace,
                profile: (self.kind == AcpKind::Antigravity).then_some(profile.path()),
            },
            state,
        )
        .await
        .map_err(|_| {
            failure(
                ProviderOperation::Authenticate,
                ProviderFailureKind::Transport,
            )
        })?;
        let result = async {
            let initialized = connection
                .request("initialize", initialize_params())
                .await
                .map_err(|_| {
                    failure(
                        ProviderOperation::Authenticate,
                        ProviderFailureKind::Protocol,
                    )
                })?;
            let selected = initialized["authMethods"]
                .as_array()
                .into_iter()
                .flatten()
                .find(|candidate| candidate["id"].as_str() == Some(method.as_str()))
                .ok_or_else(|| {
                    failure(
                        ProviderOperation::Authenticate,
                        ProviderFailureKind::Rejected,
                    )
                })?;
            if selected["type"] == "terminal" {
                // Terminal authentication is owned by the normalized setup interaction;
                // this provider operation must not launch or impersonate that action.
                return Err(forge_provider_api::capability::UnsupportedCapability(
                    forge_provider_api::capability::ProviderFeature::TerminalAuthentication,
                )
                .into());
            }
            connection
                .request(
                    "authenticate",
                    serde_json::json!({"methodId":method.as_str()}),
                )
                .await
                .map(|_| ())
                .map_err(|_| {
                    failure(
                        ProviderOperation::Authenticate,
                        ProviderFailureKind::Transport,
                    )
                })
        }
        .await;
        let collected = connection.shutdown().await.map_err(|_| {
            failure(
                ProviderOperation::CollectNativeRuntime,
                ProviderFailureKind::Transport,
            )
        });
        match (result, collected) {
            (Ok(()), Ok(())) => Ok(()),
            (Err(cause), Ok(())) | (Ok(()), Err(cause)) => Err(cause),
            (Err(cause), Err(cleanup)) => Err(ProviderError::PreparationCleanup {
                cause: Box::new(cause),
                cleanup: Box::new(cleanup),
            }),
        }
    }

    async fn fork(
        &self,
        request: forge_provider_api::session::ConversationForkRequest,
    ) -> Result<forge_provider_api::ProviderForkResult, ProviderError> {
        if self.kind != AcpKind::Claude {
            return Err(failure(
                ProviderOperation::InitializeSession,
                ProviderFailureKind::Rejected,
            ));
        }
        if request.checkpoint.is_some() {
            return Err(forge_provider_api::capability::UnsupportedCapability(
                forge_provider_api::capability::ProviderFeature::CheckpointFork,
            )
            .into());
        }
        let source_owner = request.source.owner().map_err(|_| {
            failure(
                ProviderOperation::InitializeSession,
                ProviderFailureKind::Rejected,
            )
        })?;
        if request.target.owner == source_owner {
            return Err(failure(
                ProviderOperation::InitializeSession,
                ProviderFailureKind::Rejected,
            ));
        }
        if request.target.configuration.model.service_tier
            != forge_provider_api::ServiceTier::Standard
        {
            return Err(forge_provider_api::capability::UnsupportedCapability(
                forge_provider_api::capability::ProviderFeature::ServiceTier,
            )
            .into());
        }
        if request.target.configuration.model.context_window.is_some() {
            return Err(forge_provider_api::capability::UnsupportedCapability(
                forge_provider_api::capability::ProviderFeature::ContextWindowSelection,
            )
            .into());
        }
        let binding = self
            .owners
            .lock()
            .await
            .get(&source_owner)
            .cloned()
            .ok_or_else(|| {
                failure(
                    ProviderOperation::NativeTransport,
                    ProviderFailureKind::SessionMissing,
                )
            })?;
        let session = self.binding(&binding).await?;
        let _operation = session.operation.lock().await;
        if session.has_active_prompt().await || session.state.has_active_work().await {
            return Err(failure(
                ProviderOperation::NativeTransport,
                ProviderFailureKind::Rejected,
            ));
        }
        let source_model = session.settings.lock().await.model.clone();
        let target_model = &request.target.configuration.model;
        if target_model.model.is_none() && source_model.model.is_some() {
            return Err(forge_provider_api::capability::UnsupportedCapability(
                forge_provider_api::capability::ProviderFeature::ModelDefaultReset,
            )
            .into());
        }
        if target_model.reasoning == forge_provider_api::generation::ReasoningSelection::Default
            && source_model.reasoning != forge_provider_api::generation::ReasoningSelection::Default
        {
            return Err(forge_provider_api::capability::UnsupportedCapability(
                forge_provider_api::capability::ProviderFeature::ReasoningDefaultReset,
            )
            .into());
        }
        let models = session.conversation.models().await.map_err(|_| {
            failure(
                ProviderOperation::ReconcileSettings,
                ProviderFailureKind::Protocol,
            )
        })?;
        if let Some(model) = target_model.model.as_deref()
            && !models.iter().any(|candidate| candidate.id == model)
        {
            return Err(failure(
                ProviderOperation::ReconcileSettings,
                ProviderFailureKind::Rejected,
            ));
        }
        let selected_model = target_model.model.as_deref().or_else(|| {
            models
                .iter()
                .find(|model| model.is_default)
                .map(|model| model.id.as_str())
        });
        let effort = match &target_model.reasoning {
            forge_provider_api::generation::ReasoningSelection::Default => None,
            forge_provider_api::generation::ReasoningSelection::Level(level) => {
                if !selected_model
                    .and_then(|id| models.iter().find(|model| model.id == id))
                    .is_some_and(|model| model.reasoning.contains(level))
                {
                    return Err(forge_provider_api::capability::UnsupportedCapability(
                        forge_provider_api::capability::ProviderFeature::EffortSelection,
                    )
                    .into());
                }
                Some(level.as_str())
            }
            forge_provider_api::generation::ReasoningSelection::Lowest => Some(
                selected_model
                    .and_then(|id| models.iter().find(|model| model.id == id))
                    .and_then(|model| model.reasoning.first())
                    .ok_or({
                        forge_provider_api::capability::UnsupportedCapability(
                            forge_provider_api::capability::ProviderFeature::EffortSelection,
                        )
                    })?
                    .as_str(),
            ),
        };
        let skills = request
            .target
            .resources
            .skills
            .iter()
            .filter(|skill| skill.enabled)
            .map(|skill| skill.name.clone())
            .collect::<Vec<_>>();
        let profile = super::profile::create(
            self.kind,
            &self.runtime_root,
            &request.target.resources.skills,
        )?;
        let mcp = super::resources::mcp_launches(&request.target.resources.mcp)?;
        let id = session
            .conversation
            .fork(
                &request.target.workspace,
                mcp,
                &skills,
                profile.path(),
                target_model.model.as_deref(),
                effort,
            )
            .await?;
        let id = id.try_into().map_err(|_| {
            failure(
                ProviderOperation::InitializeSession,
                ProviderFailureKind::Protocol,
            )
        })?;
        Ok(forge_provider_api::ProviderForkResult::unprofiled(id))
    }

    async fn compact(
        &self,
        binding: &BindingId,
        request: forge_provider_api::CompactionRequest,
    ) -> Result<forge_provider_api::Compaction, ProviderError> {
        if self.kind != AcpKind::Claude {
            return Err(failure(
                ProviderOperation::NativeTransport,
                ProviderFailureKind::Rejected,
            ));
        }
        let session = self.binding(binding).await?;
        let _operation = session.operation.lock().await;
        if session.has_active_prompt().await {
            return Err(failure(
                ProviderOperation::AdmitInput,
                ProviderFailureKind::Rejected,
            ));
        }
        if request.scope.owner() != &session.owner {
            return Err(failure(
                ProviderOperation::AdmitInput,
                ProviderFailureKind::Rejected,
            ));
        }
        let sink = session.sink.bind_scope(request.scope.clone())?;
        session
            .state
            .begin_compaction(session.conversation.id(), sink)
            .await?;
        let result = session.conversation.request("session/prompt", serde_json::json!({
            "sessionId":session.conversation.id(), "prompt":[{"type":"text","text":"/compact"}]
        })).await;
        session.state.finish_compaction().await;
        let response = result?;
        super::validate_prompt_response(response.clone()).map_err(|_| {
            failure(
                ProviderOperation::NativeTransport,
                ProviderFailureKind::Protocol,
            )
        })?;
        if response["stopReason"] == "cancelled" {
            return Err(ProviderError::Cancelled);
        }
        Ok(session
            .state
            .latest_compaction()
            .await
            .unwrap_or(forge_provider_api::Compaction {
                id: request.operation_id,
                thread_id: session.conversation.id().to_owned(),
                state: forge_provider_api::CompactionState::Unconfirmed,
                manual: true,
                started_at_ms: None,
                completed_at_ms: None,
                result: CompactionResult::default(),
            }))
    }

    async fn cancel_session(&self, binding: &BindingId) -> Result<(), ProviderError> {
        self.cleanup_execution(binding).await
    }

    async fn cleanup_execution(&self, binding: &BindingId) -> Result<(), ProviderError> {
        let session = self.binding(binding).await?;
        let running = {
            let mut active = session.active.lock().await;
            if let Some(active) = active.as_mut().filter(|active| active.task.is_none()) {
                active.cancelled = true;
            }
            active
                .as_ref()
                .and_then(|active| active.task.as_ref())
                .is_some_and(|task| !task.is_finished())
        };
        if (running || session.state.has_active_work().await)
            && session.conversation.cancel().is_err()
        {
            return Err(failure(
                ProviderOperation::CleanupExecution,
                ProviderFailureKind::Transport,
            ));
        }
        if let Err(error) = session.collect_prompt().await {
            session.state.revoke_callbacks().await;
            session.sink.fail(events::EventDeliveryError::Observation(
                forge_provider_api::lifecycle::ObservationFailure::Disconnected,
            ));
            return Err(error);
        }
        let _operation = session.operation.lock().await;
        Ok(())
    }

    async fn retire_session(&self, binding: &BindingId) -> Result<(), ProviderError> {
        let session = self.binding(binding).await?;
        self.cleanup_execution(binding).await?;
        session.conversation.shutdown().await?;
        let mut resources = session.resources.lock().await;
        if let Some(profile) = resources.as_mut() {
            profile
                .close()
                .map_err(|error| error.into_provider_error(ProviderOperation::RetireSession))?;
        }
        resources.take();
        drop(resources);
        self.bindings.lock().await.remove(binding);
        self.owners.lock().await.remove(&session.owner);
        Ok(())
    }

    async fn fence_session(&self, binding: &BindingId) -> Result<(), ProviderError> {
        let session = self.binding(binding).await?;
        session.state.revoke_callbacks().await;
        session.sink.fail(events::EventDeliveryError::Observation(
            forge_provider_api::lifecycle::ObservationFailure::Disconnected,
        ));
        self.cleanup_execution(binding).await
    }

    async fn recover_session(
        &self,
        mut request: SessionRequest,
        token: Option<forge_provider_api::session::RecoveryToken>,
    ) -> Result<SessionBinding, ProviderError> {
        if let Some(token) = token.as_ref() {
            let checkpoint = parse_checkpoint(token)?;
            if request
                .provider_session_id
                .as_ref()
                .is_some_and(|id| id.as_str() != checkpoint.session_id)
            {
                return Err(failure(
                    ProviderOperation::RecoverSession,
                    ProviderFailureKind::Rejected,
                ));
            }
            request.provider_session_id =
                Some(checkpoint.session_id.to_owned().try_into().map_err(|_| {
                    failure(
                        ProviderOperation::RecoverSession,
                        ProviderFailureKind::Protocol,
                    )
                })?);
        }
        self.initialize_session(request).await
    }

    async fn reconcile_submission(
        &self,
        _submission_id: &forge_provider_api::SubmissionId,
        token: Option<&forge_provider_api::RecoveryToken>,
        accepted_receipts: &[forge_provider_api::RecoveryToken],
    ) -> Result<forge_provider_api::SubmissionEvidence, ProviderError> {
        let Some(token) = token else {
            return Ok(forge_provider_api::SubmissionEvidence::Unknown);
        };
        let checkpoint = parse_checkpoint(token)?;
        if checkpoint.accepted
            && accepted_receipts
                .iter()
                .any(|receipt| receipt.as_bytes() == token.as_bytes())
        {
            let provider_session_id =
                Some(checkpoint.session_id.to_owned().try_into().map_err(|_| {
                    failure(
                        ProviderOperation::RecoverSession,
                        ProviderFailureKind::Protocol,
                    )
                })?);
            return Ok(forge_provider_api::SubmissionEvidence::Accepted {
                receipt: Some(token.clone()),
                provider_session_id,
            });
        }
        // ACP v1 does not expose a client-assigned input identity or exact history evidence.
        Ok(forge_provider_api::SubmissionEvidence::Unknown)
    }

    async fn shutdown(&self) -> Result<(), ProviderError> {
        let bindings = self
            .bindings
            .lock()
            .await
            .keys()
            .cloned()
            .collect::<Vec<_>>();
        let mut errors = Vec::new();
        for binding in bindings {
            if let Err(error) = self.retire_session(&binding).await {
                errors.push(error);
            }
        }
        if errors.is_empty() {
            Ok(())
        } else {
            Err(ProviderError::Combined {
                primary: Box::new(errors.remove(0)),
                secondary: errors,
            })
        }
    }

    fn descriptor(&self) -> forge_provider_api::ProviderDescriptor {
        crate::registry::descriptor(&self.kind.id()).expect("ACP kind is registered")
    }

    async fn has_active_turn(&self, owner: &ExecutionOwner) -> bool {
        let Some(binding) = self.owners.lock().await.get(owner).cloned() else {
            return false;
        };
        let Ok(session) = self.binding(&binding).await else {
            return false;
        };
        session.has_active_prompt().await
    }

    async fn active_session_id_for(
        &self,
        owner: &ExecutionOwner,
    ) -> Result<Option<forge_provider_api::ProviderSessionId>, ProviderError> {
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
        let mut rollback = session.rollback_cancelled.lock().await;
        Ok(std::mem::replace(&mut *rollback, false))
    }
}

pub(super) fn normalized_mcp_reason(
    reason: Option<&str>,
) -> forge_provider_api::diagnostic::SanitizedMessage {
    let message = match reason {
        Some("authentication_required") | Some("authenticationRequired") => {
            "MCP resource requires authentication."
        }
        Some("permission_denied") | Some("permissionDenied") => "MCP resource access was denied.",
        Some("connection_failed") | Some("connectionFailed") => "MCP resource connection failed.",
        Some("initialization_failed") | Some("initializationFailed") => {
            "MCP resource could not initialize."
        }
        _ => "MCP resource is unavailable.",
    };
    forge_provider_api::diagnostic::SanitizedMessage::new(message)
        .expect("static normalized MCP messages are nonempty")
}

fn failure(operation: ProviderOperation, kind: ProviderFailureKind) -> ProviderError {
    ProviderError::Failure(ProviderFailure::new(operation, kind))
}

fn prompt_input(input: &forge_provider_api::PromptInput) -> String {
    match input {
        forge_provider_api::PromptInput::Text(text) => text.clone(),
        forge_provider_api::PromptInput::Skill { name, arguments } => {
            format!("/{name} {arguments}")
        }
        forge_provider_api::PromptInput::QuestionReply(reply) => {
            let response = match &reply.response {
                forge_provider_api::QuestionReplyResponse::Answer {
                    answer,
                    required_questions_remain,
                } => {
                    format!(
                        "Answer: {answer}\nRequired questions remain: {required_questions_remain}"
                    )
                }
                forge_provider_api::QuestionReplyResponse::Clarification {
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
                forge_provider_api::QuestionReplyResponse::Correction {
                    answer,
                    required_questions_remain,
                } => {
                    format!(
                        "Correction: {answer}\nRequired questions remain: {required_questions_remain}"
                    )
                }
            };
            format!(
                "Question response for {} (revision {}, message {}):\n{}\n{}",
                reply.question_id, reply.revision, reply.message_id, reply.question, response
            )
        }
    }
}

/// Encode current and recovered typed inputs without dropping their instructions or files.
fn prompt_content(
    request: &PromptRequest,
    supports_images: bool,
) -> Result<Vec<serde_json::Value>, ProviderError> {
    let compact = request.recovered_inputs.is_empty()
        && matches!(&request.input, forge_provider_api::PromptInput::Text(text) if text == "/compact");
    if compact {
        return Ok(vec![serde_json::json!({"type":"text","text":"/compact"})]);
    }

    let mut content = Vec::new();
    for (index, recovered) in request.recovered_inputs.iter().enumerate() {
        content.push(serde_json::json!({
            "type":"text",
            "text":format!("Earlier user input {} was saved locally but not accepted:", index + 1),
        }));
        if !recovered.instructions.is_empty() {
            content.push(serde_json::json!({"type":"text","text":recovered.instructions}));
        }
        let mut attachments =
            super::attachments::input(String::new(), &recovered.attachments, supports_images)?;
        attachments.pop();
        content.extend(attachments);
        content.push(serde_json::json!({"type":"text","text":prompt_input(&recovered.input)}));
    }
    if !request.recovered_inputs.is_empty() {
        content.push(serde_json::json!({"type":"text","text":"Newest user input:"}));
    }
    if !request.instructions.is_empty() {
        content.push(serde_json::json!({"type":"text","text":request.instructions}));
    }
    if let Some(task) = &request.task {
        content.push(serde_json::json!({"type":"text","text":task.heading()}));
    }
    let mut attachments =
        super::attachments::input(String::new(), &request.attachments, supports_images)?;
    attachments.pop();
    content.extend(attachments);
    content.push(serde_json::json!({"type":"text","text":prompt_input(&request.input)}));
    Ok(content)
}

fn mcp_definitions(
    selection: &forge_provider_api::session::ResourceSelection,
) -> Vec<forge_provider_api::catalog::McpDefinition> {
    selection
        .mcp
        .iter()
        .map(|launch| forge_provider_api::catalog::McpDefinition {
            resource_id: None,
            scope: None,
            source_path: None,
            name: launch.name.clone(),
            transport: match &launch.transport {
                forge_provider_api::session::McpTransport::Stdio { .. } => {
                    forge_provider_api::catalog::McpTransportKind::Stdio
                }
                forge_provider_api::session::McpTransport::Http { .. } => {
                    forge_provider_api::catalog::McpTransportKind::Http
                }
                forge_provider_api::session::McpTransport::Sse { .. } => {
                    forge_provider_api::catalog::McpTransportKind::Sse
                }
            },
            enabled: true,
            status: forge_provider_api::catalog::McpStatus::Loading,
            status_detail: None,
            token_count: None,
            token_estimated: false,
            tools: Vec::new(),
            tool_error: None,
        })
        .collect()
}

fn same_resources(
    current: &forge_provider_api::session::ResourceSelection,
    desired: &forge_provider_api::session::ResourceSelection,
) -> bool {
    current.skills == desired.skills
        && current.mcp.len() == desired.mcp.len()
        && current
            .mcp
            .iter()
            .zip(&desired.mcp)
            .all(|(current, desired)| {
                if current.name != desired.name
                    || current.timeout != desired.timeout
                    || current.working_directory != desired.working_directory
                    || current.tools != desired.tools
                {
                    return false;
                }
                match (&current.transport, &desired.transport) {
                    (
                        forge_provider_api::session::McpTransport::Stdio {
                            command: left_command,
                            arguments: left_arguments,
                            environment: left_environment,
                        },
                        forge_provider_api::session::McpTransport::Stdio {
                            command: right_command,
                            arguments: right_arguments,
                            environment: right_environment,
                        },
                    ) => {
                        left_command == right_command
                            && left_arguments == right_arguments
                            && left_environment == right_environment
                    }
                    (
                        forge_provider_api::session::McpTransport::Http {
                            url: left_url,
                            headers: left_headers,
                        },
                        forge_provider_api::session::McpTransport::Http {
                            url: right_url,
                            headers: right_headers,
                        },
                    ) => left_url == right_url && left_headers == right_headers,
                    (
                        forge_provider_api::session::McpTransport::Sse {
                            url: left_url,
                            headers: left_headers,
                        },
                        forge_provider_api::session::McpTransport::Sse {
                            url: right_url,
                            headers: right_headers,
                        },
                    ) => left_url == right_url && left_headers == right_headers,
                    _ => false,
                }
            })
}
