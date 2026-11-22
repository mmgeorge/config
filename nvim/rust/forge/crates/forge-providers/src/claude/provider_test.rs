//! Provider contract tests against an in-memory Claude Code peer.

use super::process::{ClaudeLauncher, LaunchSpec, SessionStart};
use super::provider::ClaudeProvider;
use super::test_peer::{
    PeerHandler, PeerLauncher, lifecycle, result_interrupted, result_success, success, text_message,
};
use async_trait::async_trait;
use forge_provider_api::provider::Provider;
use forge_provider_api::submission::InputReceipt;
use forge_provider_api::{ProviderError, RecoveryToken, SubmissionId};
use serde_json::{Value, json};
use std::sync::{Arc, Mutex};

#[derive(Default)]
struct ReceiptState {
    sending: bool,
    accepted: bool,
    rejected: bool,
    /// The journal owner already retired this attempt, so it refuses a later rejection.
    retired: bool,
}

struct FixtureReceipt {
    id: SubmissionId,
    state: Mutex<ReceiptState>,
}

fn receipt(id: &str) -> Arc<FixtureReceipt> {
    Arc::new(FixtureReceipt {
        id: SubmissionId::try_from(id.to_owned()).unwrap(),
        state: Mutex::default(),
    })
}

#[async_trait]
impl InputReceipt for FixtureReceipt {
    fn id(&self) -> &SubmissionId {
        &self.id
    }
    fn attempt_id(&self) -> Result<forge_provider_api::DeliveryAttemptId, ProviderError> {
        Ok(forge_provider_api::DeliveryAttemptId::new(format!(
            "attempt-{}",
            self.id.as_str()
        ))?)
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
        let mut state = self.state.lock().unwrap();
        if state.retired {
            return Err(forge_provider_api::ProviderFailure::new(
                forge_provider_api::ProviderOperation::TurnStart,
                forge_provider_api::ProviderFailureKind::Persistence,
            )
            .at_phase(
                forge_provider_api::ProviderPhase::Persistence,
                forge_provider_api::FailureDisposition::Unchanged,
            )
            .into());
        }
        state.rejected = true;
        Ok(())
    }
    async fn record_failure(self: Arc<Self>, _: ProviderError) -> Result<(), ProviderError> {
        Ok(())
    }
}

/// Answer every permission request with one fixed decision.
struct FixedPermissions(forge_provider_api::permissions::PermissionResponse);

#[async_trait]
impl forge_provider_api::permissions::ProviderPermissions for FixedPermissions {
    async fn authorize(
        &self,
        _: &forge_provider_api::InvocationScope,
        _: forge_provider_api::permissions::PermissionRequest,
    ) -> forge_provider_api::permissions::PermissionResponse {
        self.0.clone()
    }
    async fn before_tool(
        &self,
        _: &forge_provider_api::InvocationScope,
    ) -> Result<(), ProviderError> {
        Ok(())
    }
}

struct NoTools;

#[async_trait]
impl forge_provider_api::control::ProviderTools for NoTools {
    async fn call(
        &self,
        _: &forge_provider_api::InvocationScope,
        _: forge_provider_api::control::ToolCall,
    ) -> Result<String, ProviderError> {
        Err(ProviderError::Cancelled)
    }
}

struct OpenTurnControl;

#[async_trait]
impl forge_provider_api::turn_control::TurnControl for OpenTurnControl {
    async fn before_send(
        &self,
        _: &forge_provider_api::InvocationScope,
    ) -> Result<(), ProviderError> {
        Ok(())
    }
    async fn receive(&self, _: &forge_provider_api::InvocationScope) -> Result<(), ProviderError> {
        Ok(())
    }
    async fn close(&self, _: &forge_provider_api::InvocationScope) -> Result<(), ProviderError> {
        Ok(())
    }
    async fn status(
        &self,
        _: &forge_provider_api::InvocationScope,
    ) -> Result<forge_provider_api::turn_control::HostTurnStatus, ProviderError> {
        Ok(forge_provider_api::turn_control::HostTurnStatus {
            replay_safe: false,
            cancelled: false,
            error: None,
        })
    }
}

/// Hold every prompt before dispatch, as the host does while its baseline checkpoint is captured.
#[derive(Default)]
struct HeldTurnControl {
    /// Signals that a prompt reached the hold.
    entered: tokio::sync::Notify,
}

#[async_trait]
impl forge_provider_api::turn_control::TurnControl for HeldTurnControl {
    async fn before_send(
        &self,
        _: &forge_provider_api::InvocationScope,
    ) -> Result<(), ProviderError> {
        self.entered.notify_one();
        std::future::pending().await
    }
    async fn receive(&self, _: &forge_provider_api::InvocationScope) -> Result<(), ProviderError> {
        Ok(())
    }
    async fn close(&self, _: &forge_provider_api::InvocationScope) -> Result<(), ProviderError> {
        Ok(())
    }
    async fn status(
        &self,
        _: &forge_provider_api::InvocationScope,
    ) -> Result<forge_provider_api::turn_control::HostTurnStatus, ProviderError> {
        Ok(forge_provider_api::turn_control::HostTurnStatus {
            replay_safe: false,
            cancelled: false,
            error: None,
        })
    }
}

struct SettledHostEffects;

#[async_trait]
impl forge_provider_api::session::ProviderHostEffects for SettledHostEffects {
    async fn settle_foreground(
        &self,
        _: &forge_provider_api::session::BindingId,
        _: &forge_provider_api::InvocationScope,
    ) -> Result<(), forge_provider_api::session::HostEffectFailure> {
        Ok(())
    }
}

struct IgnoreTrace;

impl forge_provider_api::trace::ProviderTrace for IgnoreTrace {
    fn record(&self, _: forge_provider_api::trace::ProviderTraceRecord) {}
}

fn operation_context() -> forge_provider_api::OperationContext {
    let (_, cancellation) = forge_provider_api::OperationCancellation::channel();
    forge_provider_api::OperationContext::new(
        forge_provider_api::OperationId::new(uuid::Uuid::new_v4().to_string()).unwrap(),
        std::time::Duration::from_secs(10),
        cancellation,
    )
    .unwrap()
}

/// Build the context Harness gives a stop's collection and a steer, which carry no deadline of
/// their own, so only the adapter's bounds can end them.
fn unbounded_context() -> forge_provider_api::OperationContext {
    let (_, cancellation) = forge_provider_api::OperationCancellation::channel();
    forge_provider_api::OperationContext::unbounded(
        forge_provider_api::OperationId::new(uuid::Uuid::new_v4().to_string()).unwrap(),
        cancellation,
    )
}

fn session_request(workspace: &std::path::Path) -> forge_provider_api::SessionRequest {
    forge_provider_api::SessionRequest {
        thread_id: forge_provider_api::testing::root_thread("claude-owner"),
        mode: forge_provider_api::SessionMode::Interactive,
        workspace: workspace.to_string_lossy().into_owned(),
        history: forge_provider_api::SessionHistory::Durable { resume: None },
        configuration: forge_provider_api::SessionConfiguration {
            model: Default::default(),
            access: forge_provider_api::access::AccessPolicy {
                sandbox: false,
                ..Default::default()
            },
            approval: forge_provider_api::approval::ApprovalMode::Ask,
            resources: Default::default(),
        },
        resource_lease: None,
        tools: Vec::new(),
        tool_endpoint: None,
        system_instructions: "fixture system".into(),
        trace: Arc::new(IgnoreTrace),
    }
}

fn prompt_request(
    binding: &forge_provider_api::BindingId,
    turn: &str,
    receipt: Arc<FixtureReceipt>,
    permission: forge_provider_api::permissions::PermissionResponse,
) -> forge_provider_api::session::PromptRequest {
    let scope = forge_provider_api::InvocationScope::new(
        forge_provider_api::testing::root_thread("claude-owner"),
        turn,
    );
    let (dispatch, _) = forge_provider_api::submission::DispatchReporter::channel(
        binding.clone(),
        scope.clone(),
        receipt.id.clone(),
        receipt.attempt_id().unwrap(),
    );
    forge_provider_api::session::PromptRequest {
        thread_id: scope.thread_id.clone(),
        turn_id: scope.turn_id().unwrap(),
        scope,
        dispatch,
        execution: forge_provider_api::ExecutionMode::Default,
        configuration: forge_provider_api::session::InvocationConfiguration {
            model: forge_provider_api::session::ModelSelection::default(),
            access: forge_provider_api::access::AccessPolicy {
                sandbox: false,
                ..Default::default()
            },
            approval: forge_provider_api::approval::ApprovalMode::Ask,
        },
        tools: Vec::new(),
        tool_endpoint: None,
        resource_lease: None,
        submission_id: receipt.id.clone(),
        attempt_id: receipt.attempt_id().unwrap(),
        input: forge_provider_api::forge::TurnInput::user(
            forge_provider_api::input::PromptInput::Text("hello".into()),
        ),
        recovered_inputs: Vec::new(),
        context: Default::default(),
        attachments: Vec::new(),
        receipt,
        permissions: Arc::new(FixedPermissions(permission)),
        provider_tools: Arc::new(NoTools),
        turn_control: Arc::new(OpenTurnControl),
        host_effects: Arc::new(SettledHostEffects),
    }
}

/// Start the user message with one `<forge>` block and keep the user's words unwrapped after it.
#[test]
fn prompt_texts_place_the_forge_block_before_the_input() {
    use forge_provider_api::forge::{GoalTask, Task, TurnContext, TurnInput, TurnKind};
    let binding = forge_provider_api::BindingId::new("context-binding").unwrap();
    let mut request = prompt_request(
        &binding,
        "context-turn",
        receipt("context-submission"),
        forge_provider_api::permissions::PermissionResponse::Allow,
    );
    assert_eq!(
        super::provider::prompt_texts(&request),
        ["hello"],
        "an unchanged context adds no text to a user message"
    );
    request.context = TurnContext {
        record: "k3f9".into(),
        permissions: None,
        aside: None,
        agent: None,
        task: Some(Task::Goal(GoalTask {
            objective: "Ship.".into(),
        })),
    };
    request.input = TurnInput::new(TurnKind::PlanDiscussion {
        input: forge_provider_api::input::PromptInput::Text("why?".into()),
    });
    let texts = super::provider::prompt_texts(&request);
    assert_eq!(texts.len(), 2);
    assert!(texts[0].starts_with("<forge>\n<instructions>\nContinue discussing the active plan"));
    assert!(texts[0].ends_with("</task>\n</forge>"));
    assert_eq!(texts[1], "why?");
}

fn steering_request(
    scope: forge_provider_api::InvocationScope,
    text: &str,
) -> forge_provider_api::session::SteeringRequest {
    forge_provider_api::session::SteeringRequest {
        dispatch: None,
        scope,
        input: forge_provider_api::forge::TurnInput::user(
            forge_provider_api::input::PromptInput::Text(text.into()),
        ),
        attachments: Vec::new(),
        delivery: forge_provider_api::session::SteeringDelivery::Context,
    }
}

/// Collect events until the turn completes, returning them and the completed turn.
async fn until_completed(
    stream: &mut forge_provider_api::ProviderEventStream,
) -> (
    Vec<forge_provider_api::ProviderEvent>,
    forge_provider_api::CompletedTurn,
) {
    let mut events = Vec::new();
    loop {
        let envelope = stream.recv().await.unwrap().unwrap();
        if let forge_provider_api::ProviderEvent::Turn {
            event: forge_provider_api::TurnEvent::Completed(completed),
            ..
        } = &envelope.event
        {
            return (events, completed.clone());
        }
        events.push(envelope.event);
    }
}

/// Concatenate the streamed message text of every item, in event order.
fn streamed_text(events: &[forge_provider_api::ProviderEvent]) -> String {
    use forge_provider_api::message::{ItemEvent, ItemStart, MessageItemEvent};
    events
        .iter()
        .filter_map(|event| match event {
            forge_provider_api::ProviderEvent::Item { event, .. } => match event {
                ItemEvent::Started(ItemStart::AgentMessage(start)) => Some(start.text.as_str()),
                ItemEvent::Message {
                    event: MessageItemEvent::Delta(text),
                } => Some(text.as_str()),
                _ => None,
            },
            _ => None,
        })
        .collect()
}

/// Wait until the first assistant message item starts.
async fn until_message(stream: &mut forge_provider_api::ProviderEventStream) {
    loop {
        let envelope = stream.recv().await.unwrap().unwrap();
        if matches!(
            envelope.event,
            forge_provider_api::ProviderEvent::Item {
                event: forge_provider_api::message::ItemEvent::Started(
                    forge_provider_api::message::ItemStart::AgentMessage(_)
                ),
                ..
            }
        ) {
            return;
        }
    }
}

fn classifications(events: &[forge_provider_api::ProviderEvent]) -> Vec<String> {
    events
        .iter()
        .filter_map(|event| match event {
            forge_provider_api::ProviderEvent::Item {
                event:
                    forge_provider_api::message::ItemEvent::Message {
                        event: forge_provider_api::message::MessageItemEvent::Classified(class),
                    },
                ..
            } => Some(format!("{class:?}")),
            _ => None,
        })
        .collect()
}

/// Treat a user frame Forge writes as a prompt, not a control or other input.
fn user_uuid(frame: &Value) -> Option<Value> {
    (frame["type"] == "user").then(|| frame["uuid"].clone())
}

/// Answer each prompt with one streamed reply and a successful result.
fn answering_peer(chunks: &'static [&'static str]) -> PeerHandler {
    Box::new(move |frame| {
        let uuid = user_uuid(frame)?;
        let mut frames = vec![lifecycle(&uuid, "queued"), lifecycle(&uuid, "started")];
        frames.extend(text_message("msg-1", chunks));
        frames.push(lifecycle(&uuid, "completed"));
        frames.push(result_success());
        Some(frames)
    })
}

async fn open(
    launcher: Arc<PeerLauncher>,
    workspace: &std::path::Path,
) -> (ClaudeProvider, forge_provider_api::OpenedSession) {
    let provider = ClaudeProvider::with_launcher(launcher, workspace.join("runtime"));
    let opened = provider
        .thread_open(operation_context(), session_request(workspace))
        .await
        .unwrap();
    (provider, opened)
}

/// A prompt streams its text, classifies the only message as the response, and commits acceptance
/// only after the CLI reports the input queued.
#[tokio::test]
async fn prompt_streams_text_and_completes_with_a_response() {
    let directory = tempfile::tempdir().unwrap();
    let launcher = PeerLauncher::new(|| answering_peer(&["hel", "lo"]));
    let (provider, mut opened) = open(launcher.clone(), directory.path()).await;
    let receipt = receipt("first");
    provider
        .turn_start(
            operation_context(),
            &opened.binding.id,
            prompt_request(
                &opened.binding.id,
                "turn-1",
                receipt.clone(),
                forge_provider_api::permissions::PermissionResponse::Allow,
            ),
        )
        .await
        .unwrap();
    let (events, completed) = until_completed(&mut opened.events).await;

    assert_eq!(streamed_text(&events), "hello");
    assert_eq!(classifications(&events), ["Known(Response)"]);
    assert_eq!(
        completed.result().outcome(),
        forge_provider_api::turn::TurnOutcome::Completed {
            stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal
        }
    );
    assert!(receipt.state.lock().unwrap().accepted);
    let prompt = &launcher.frames("user")[0];
    assert_eq!(prompt["client_composed"], true);
    assert!(prompt["uuid"].is_string());
    provider.shutdown(operation_context()).await.unwrap();
}

/// Startup discovery runs once, and callbacks must name a prompt in the active turn.
#[tokio::test]
async fn initial_and_lazy_instruction_reads_keep_their_original_turn() {
    let directory = tempfile::tempdir().unwrap();
    let workspace = directory.path().to_string_lossy().into_owned();
    let initial = directory
        .path()
        .join("CLAUDE.md")
        .to_string_lossy()
        .into_owned();
    let nested = directory
        .path()
        .join("nested/CLAUDE.md")
        .to_string_lossy()
        .into_owned();
    let launcher = PeerLauncher::new({
        let initial = initial.clone();
        let nested = nested.clone();
        let workspace = workspace.clone();
        move || {
            let initial = initial.clone();
            let nested = nested.clone();
            let workspace = workspace.clone();
            let mut prompt = None;
            let mut previous_prompt = None;
            Box::new(move |frame| {
                if frame["request"]["subtype"] == "get_context_usage" {
                    return Some(vec![success(
                        frame["request_id"].clone(),
                        json!({"totalTokens":100,"maxTokens":200000,"memoryFiles":[{"path":initial,"type":"Project","tokens":20}]}),
                    )]);
                }
                if let Some(uuid) = user_uuid(frame) {
                    prompt = Some(uuid.clone());
                    return Some(vec![
                        lifecycle(&uuid, "queued"),
                        lifecycle(&uuid, "started"),
                        json!({
                            "type":"control_request","request_id":"stale-instruction-load","request":{
                                "subtype":"hook_callback","callback_id":"forge-instructions",
                                "input":{"hook_event_name":"InstructionsLoaded","file_path":format!("{workspace}/stale.md"),"prompt_id":previous_prompt}
                            }
                        }),
                        json!({
                            "type":"control_request","request_id":"instruction-load","request":{
                                "subtype":"hook_callback","callback_id":"forge-instructions",
                                "input":{"hook_event_name":"InstructionsLoaded","file_path":nested,"cwd":workspace,"load_reason":"nested_traversal","prompt_id":uuid}
                            }
                        }),
                    ]);
                }
                if frame["type"] == "control_response"
                    && frame["response"]["request_id"] == "instruction-load"
                {
                    let uuid = prompt.take().unwrap();
                    previous_prompt = Some(uuid.clone());
                    let mut frames = text_message("reply", &["done"]);
                    frames.push(lifecycle(&uuid, "completed"));
                    frames.push(result_success());
                    return Some(frames);
                }
                None
            })
        }
    });
    let (provider, mut opened) = open(launcher.clone(), directory.path()).await;
    for (turn, expected) in [
        ("first", vec![initial.clone(), nested.clone()]),
        ("second", vec![nested.clone()]),
    ] {
        provider
            .turn_start(
                operation_context(),
                &opened.binding.id,
                prompt_request(
                    &opened.binding.id,
                    turn,
                    receipt(turn),
                    forge_provider_api::permissions::PermissionResponse::Allow,
                ),
            )
            .await
            .unwrap();
        let (events, _) = until_completed(&mut opened.events).await;
        let reads = events
            .iter()
            .filter_map(|event| match event {
                forge_provider_api::ProviderEvent::Turn {
                    event: forge_provider_api::TurnEvent::InstructionRead(read),
                    ..
                } => Some(std::path::PathBuf::from(&read.path)),
                _ => None,
            })
            .collect::<Vec<_>>();
        assert_eq!(
            reads,
            expected
                .into_iter()
                .map(std::path::PathBuf::from)
                .collect::<Vec<_>>()
        );
    }
    let queries = launcher
        .frames("control_request")
        .into_iter()
        .filter(|frame| frame["request"]["subtype"] == "get_context_usage")
        .count();
    // Context accounting also queries at each completion; only the extra startup query is once.
    assert_eq!(queries, 3);
    provider.shutdown(operation_context()).await.unwrap();
}

/// Isolated prompts stream normally without a durable identity or inherited tool surface.
#[tokio::test]
async fn isolated_text_uses_an_unpersisted_tool_free_process() {
    let directory = tempfile::tempdir().unwrap();
    let launcher = PeerLauncher::new(|| answering_peer(&["generated", " subject"]));
    let provider =
        ClaudeProvider::with_launcher(launcher.clone(), directory.path().join("runtime"));
    let mut request = session_request(directory.path());
    request.history = forge_provider_api::SessionHistory::Temporary;
    let mut opened = provider
        .thread_open(operation_context(), request)
        .await
        .unwrap();
    assert!(opened.binding.provider_session_id.is_none());
    provider
        .turn_start(
            operation_context(),
            &opened.binding.id,
            prompt_request(
                &opened.binding.id,
                "isolated-turn",
                receipt("isolated"),
                forge_provider_api::permissions::PermissionResponse::Deny {
                    reason: Some("Tools disabled".into()),
                },
            ),
        )
        .await
        .unwrap();
    let (events, _) = until_completed(&mut opened.events).await;
    assert_eq!(streamed_text(&events), "generated subject");
    {
        let record = launcher.record.lock().unwrap();
        let arguments = &record.launches[0];
        for flag in [
            "--no-session-persistence",
            "--safe-mode",
            "--tools=",
            "--permission-prompts=none",
        ] {
            assert!(arguments.iter().any(|argument| argument == flag));
        }
        assert!(
            !arguments
                .iter()
                .any(|argument| argument.starts_with("--session-id=")
                    || argument.starts_with("--resume="))
        );
        let initialize = record
            .frames
            .iter()
            .find(|frame| frame["request"]["subtype"] == "initialize")
            .unwrap();
        assert!(initialize["request"]["sdkMcpServers"].is_null());
        assert_eq!(initialize["request"]["systemPrompt"], "fixture system");
    }
    provider.shutdown(operation_context()).await.unwrap();
}

/// A steer still queued at the CLI's result starts another CLI turn, and the Forge turn spans both.
#[tokio::test]
async fn late_steer_extends_the_turn_through_the_follow_up_result() {
    let directory = tempfile::tempdir().unwrap();
    let launcher = PeerLauncher::new(|| {
        let mut prompt: Option<Value> = None;
        Box::new(move |frame| {
            let uuid = user_uuid(frame)?;
            match prompt.take() {
                None => {
                    prompt = Some(uuid.clone());
                    let mut frames = vec![lifecycle(&uuid, "queued"), lifecycle(&uuid, "started")];
                    frames.extend(text_message("msg-1", &["first"]));
                    Some(frames)
                }
                Some(original) => {
                    // The steer arrives after the model finished: it stays queued across the result.
                    let mut frames = vec![
                        lifecycle(&uuid, "queued"),
                        lifecycle(&original, "completed"),
                        result_success(),
                        lifecycle(&uuid, "started"),
                    ];
                    frames.extend(text_message("msg-2", &[" second"]));
                    frames.push(lifecycle(&uuid, "completed"));
                    frames.push(result_success());
                    Some(frames)
                }
            }
        })
    });
    let (provider, mut opened) = open(launcher.clone(), directory.path()).await;
    let binding = opened.binding.id.clone();
    let request = prompt_request(
        &binding,
        "turn-1",
        receipt("first"),
        forge_provider_api::permissions::PermissionResponse::Allow,
    );
    let scope = request.scope.clone();
    provider
        .turn_start(operation_context(), &binding, request)
        .await
        .unwrap();
    // Wait until the first answer streams so the steer lands after it.
    until_message(&mut opened.events).await;
    provider
        .turn_steer(
            operation_context(),
            &binding,
            steering_request(scope, "more"),
        )
        .await
        .unwrap();
    let (events, completed) = until_completed(&mut opened.events).await;

    assert_eq!(streamed_text(&events), " second");
    assert!(matches!(
        completed.result().outcome(),
        forge_provider_api::turn::TurnOutcome::Completed { .. }
    ));
    assert_eq!(launcher.frames("user").len(), 2);
    provider.shutdown(operation_context()).await.unwrap();
}

/// Interruption sends `interrupt` with queued steers cancelled, and the aborted result ends the
/// turn as interrupted.
#[tokio::test]
async fn interrupt_ends_the_turn_as_interrupted() {
    let directory = tempfile::tempdir().unwrap();
    let launcher = PeerLauncher::new(|| {
        Box::new(move |frame| {
            if let Some(uuid) = user_uuid(frame) {
                let mut frames = vec![lifecycle(&uuid, "queued"), lifecycle(&uuid, "started")];
                frames.extend(text_message("msg-1", &["partial"]));
                return Some(frames);
            }
            if frame["request"]["subtype"] == "interrupt" {
                return Some(vec![
                    success(frame["request_id"].clone(), json!({})),
                    result_interrupted(),
                ]);
            }
            None
        })
    });
    let (provider, mut opened) = open(launcher.clone(), directory.path()).await;
    let binding = opened.binding.id.clone();
    let request = prompt_request(
        &binding,
        "turn-1",
        receipt("first"),
        forge_provider_api::permissions::PermissionResponse::Allow,
    );
    let scope = request.scope.clone();
    provider
        .turn_start(operation_context(), &binding, request)
        .await
        .unwrap();
    until_message(&mut opened.events).await;
    provider
        .turn_interrupt(
            operation_context(),
            &binding,
            forge_provider_api::CancelRequest {
                scope,
                input: forge_provider_api::operations::InterruptInput::Retain,
            },
        )
        .await
        .unwrap();
    let (_, completed) = until_completed(&mut opened.events).await;

    assert_eq!(
        completed.result().outcome(),
        forge_provider_api::turn::TurnOutcome::Interrupted
    );
    let interrupt = launcher
        .frames("control_request")
        .into_iter()
        .find(|frame| frame["request"]["subtype"] == "interrupt")
        .unwrap();
    assert_eq!(interrupt["request"]["cancel_queued"], true);
    provider.shutdown(operation_context()).await.unwrap();
}

/// The prompts of the modelled CLI chain at each model request, the requesting prompt last.
type ModelRequestLog = Arc<Mutex<Vec<Vec<String>>>>;

/// Model a CLI that answers every prompt except the one at `unanswered_request`, whose answer
/// only starts to stream and is never recorded, and that records an interrupt as a user entry.
///
/// The real CLI writes `[Request interrupted by user]` as a user entry that queries nothing, and
/// a `--resume-session-at` launch truncates the chain after the named entry.
fn unanswered_interrupt_peer(
    unanswered_request: usize,
) -> (
    Arc<PeerLauncher>,
    Arc<Mutex<Vec<NativeEntry>>>,
    ModelRequestLog,
) {
    let chain: Arc<Mutex<Vec<NativeEntry>>> = Arc::default();
    let model_requests: ModelRequestLog = Arc::default();
    let peer_chain = chain.clone();
    let peer_requests = model_requests.clone();
    let launcher = PeerLauncher::with_arguments(move |arguments| {
        if !resume_native_chain(&mut peer_chain.lock().unwrap(), arguments) {
            return None;
        }
        let chain = peer_chain.clone();
        let requests = peer_requests.clone();
        let handler: PeerHandler = Box::new(move |frame: &Value| {
            if frame["request"]["subtype"] == "interrupt" {
                chain
                    .lock()
                    .unwrap()
                    .push(NativeEntry::Note(uuid::Uuid::new_v4().to_string()));
                return Some(vec![
                    success(frame["request_id"].clone(), json!({})),
                    result_interrupted(),
                ]);
            }
            let uuid = user_uuid(frame)?;
            let entry_uuid = uuid.as_str().unwrap().to_owned();
            let mut chain = chain.lock().unwrap();
            if frame["shouldQuery"] == false {
                chain.push(NativeEntry::Note(entry_uuid));
                return Some(vec![
                    lifecycle(&uuid, "queued"),
                    lifecycle(&uuid, "completed"),
                ]);
            }
            chain.push(NativeEntry::Prompt(entry_uuid));
            let mut requests = requests.lock().unwrap();
            requests.push(
                chain
                    .iter()
                    .filter_map(|entry| match entry {
                        NativeEntry::Prompt(prompt) => Some(prompt.clone()),
                        _ => None,
                    })
                    .collect(),
            );
            let message_id = format!("msg-{}", requests.len());
            let mut frames = vec![lifecycle(&uuid, "queued"), lifecycle(&uuid, "started")];
            let answer = text_message(&message_id, &["ok"]);
            if requests.len() == unanswered_request + 1 {
                // `message_start`, `content_block_start`, and the first delta only.
                frames.extend(answer.into_iter().take(3));
                return Some(frames);
            }
            let answer_uuid = answer
                .iter()
                .find(|frame| frame["type"] == "assistant")
                .and_then(|frame| frame["uuid"].as_str())
                .unwrap()
                .to_owned();
            chain.push(NativeEntry::Answer(answer_uuid));
            frames.extend(answer);
            frames.push(lifecycle(&uuid, "completed"));
            frames.push(result_success());
            Some(frames)
        });
        Some(handler)
    });
    (launcher, chain, model_requests)
}

/// Run one prompt on `binding`, interrupting it once its answer starts when `interrupt` is set.
async fn run_turn(
    provider: &ClaudeProvider,
    opened: &mut forge_provider_api::OpenedSession,
    turn: &str,
    interrupt: bool,
) -> forge_provider_api::CompletedTurn {
    let binding = opened.binding.id.clone();
    let request = prompt_request(
        &binding,
        turn,
        receipt(turn),
        forge_provider_api::permissions::PermissionResponse::Allow,
    );
    let scope = request.scope.clone();
    provider
        .turn_start(operation_context(), &binding, request)
        .await
        .unwrap();
    if interrupt {
        until_message(&mut opened.events).await;
        provider
            .turn_interrupt(
                operation_context(),
                &binding,
                forge_provider_api::CancelRequest {
                    scope,
                    input: forge_provider_api::operations::InterruptInput::Retain,
                },
            )
            .await
            .unwrap();
    }
    until_completed(&mut opened.events).await.1
}

/// The prompt after an interrupt the CLI recorded no answer for reaches the model without the
/// interrupted request.
///
/// The CLI keeps the interrupted prompt and its interrupt marker as user entries, and the API
/// joins consecutive user entries into one message, so the next prompt once reached the model in
/// one message with the cancelled request, and the model answered the cancelled request.
#[tokio::test]
async fn prompt_after_an_unanswered_interrupt_omits_the_interrupted_request() {
    let directory = tempfile::tempdir().unwrap();
    let (launcher, _, model_requests) = unanswered_interrupt_peer(1);
    let (provider, mut opened) = open(launcher.clone(), directory.path()).await;
    run_turn(&provider, &mut opened, "answered", false).await;
    let interrupted = run_turn(&provider, &mut opened, "interrupted", true).await;
    assert_eq!(
        interrupted.result().outcome(),
        forge_provider_api::turn::TurnOutcome::Interrupted
    );
    let next = run_turn(&provider, &mut opened, "next", false).await;

    assert!(matches!(
        next.result().outcome(),
        forge_provider_api::turn::TurnOutcome::Completed { .. }
    ));
    let requests = model_requests.lock().unwrap().clone();
    assert_eq!(requests.len(), 3);
    let interrupted_prompt = requests[1].last().unwrap();
    assert!(
        !requests[2].contains(interrupted_prompt),
        "the next prompt reached the model after the interrupted request: {requests:?}"
    );
    assert_eq!(
        requests[2],
        vec![requests[0][0].clone(), requests[2][1].clone()]
    );
    provider.shutdown(operation_context()).await.unwrap();
}

/// An unanswered interrupted request that no known entry precedes is withdrawn by a note.
///
/// The binding's first prompt follows history this process never saw, so no resume point can
/// drop it; a note written without a model request tells the model the request was withdrawn.
#[tokio::test]
async fn unanswered_first_interrupt_is_withdrawn_by_a_note() {
    let directory = tempfile::tempdir().unwrap();
    let (launcher, chain, _) = unanswered_interrupt_peer(0);
    let (provider, mut opened) = open(launcher.clone(), directory.path()).await;
    run_turn(&provider, &mut opened, "interrupted", true).await;
    run_turn(&provider, &mut opened, "next", false).await;

    let entries = chain.lock().unwrap().clone();
    let next_prompt = entries
        .iter()
        .rposition(|entry| matches!(entry, NativeEntry::Prompt(_)))
        .unwrap();
    let withdrawal = launcher
        .frames("user")
        .into_iter()
        .find(|frame| frame["shouldQuery"] == false)
        .expect("a note withdrew the interrupted request");
    assert!(
        matches!(&entries[next_prompt - 1], NativeEntry::Note(uuid) if frame_uuid(&withdrawal) == uuid),
        "the withdrawal note directly precedes the next prompt: {entries:?}"
    );
    assert!(
        withdrawal["message"]["content"][0]["text"]
            .as_str()
            .unwrap()
            .contains("withdrew")
    );
    provider.shutdown(operation_context()).await.unwrap();
}

/// Read a written frame's client-stamped uuid.
fn frame_uuid(frame: &Value) -> &str {
    frame["uuid"].as_str().unwrap()
}

/// Collect the token usage the adapter reported for a turn.
fn reported_usage(
    events: &[forge_provider_api::ProviderEvent],
) -> Vec<forge_provider_api::usage::TokenUsage> {
    events
        .iter()
        .filter_map(|event| match event {
            forge_provider_api::ProviderEvent::Turn {
                event: forge_provider_api::TurnEvent::Usage(report),
                ..
            } => report.usage().clone(),
            _ => None,
        })
        .collect()
}

/// Run one turn whose streamed API messages are `frames`, interrupt it, and answer the
/// interrupt with `result`, returning the turn's events.
async fn interrupted_turn_events(
    frames: Vec<Value>,
    result: Value,
) -> Vec<forge_provider_api::ProviderEvent> {
    let directory = tempfile::tempdir().unwrap();
    let launcher = PeerLauncher::new(move || {
        let frames = frames.clone();
        let result = result.clone();
        Box::new(move |frame| {
            if let Some(uuid) = user_uuid(frame) {
                let mut reply = vec![lifecycle(&uuid, "queued"), lifecycle(&uuid, "started")];
                reply.extend(frames.clone());
                return Some(reply);
            }
            if frame["request"]["subtype"] == "interrupt" {
                return Some(vec![
                    success(frame["request_id"].clone(), json!({})),
                    result.clone(),
                ]);
            }
            None
        })
    });
    let (provider, mut opened) = open(launcher, directory.path()).await;
    let binding = opened.binding.id.clone();
    let request = prompt_request(
        &binding,
        "turn-1",
        receipt("first"),
        forge_provider_api::permissions::PermissionResponse::Allow,
    );
    let scope = request.scope.clone();
    provider
        .turn_start(operation_context(), &binding, request)
        .await
        .unwrap();
    until_message(&mut opened.events).await;
    provider
        .turn_interrupt(
            operation_context(),
            &binding,
            forge_provider_api::CancelRequest {
                scope,
                input: forge_provider_api::operations::InterruptInput::Retain,
            },
        )
        .await
        .unwrap();
    let (events, _) = until_completed(&mut opened.events).await;
    provider.shutdown(operation_context()).await.unwrap();
    events
}

/// Build the stream events that start one API message's text, with optional reported usage.
fn started_text_message(message_id: &str, usage: Option<Value>) -> Vec<Value> {
    let event = |event: Value| {
        json!({"type": "stream_event", "event": event, "parent_tool_use_id": null,
            "uuid": uuid::Uuid::new_v4().to_string()})
    };
    let mut message = json!({"id": message_id, "type": "message", "role": "assistant",
        "content": []});
    if let Some(usage) = usage {
        message["usage"] = usage;
    }
    vec![
        event(json!({"type": "message_start", "message": message})),
        event(json!({"type": "content_block_start", "index": 0,
            "content_block": {"type": "text", "text": ""}})),
        event(json!({"type": "content_block_delta", "index": 0,
            "delta": {"type": "text_delta", "text": "partial"}})),
    ]
}

/// The CLI reports zero usage on a result it aborted mid-stream, although the aborted API
/// message already consumed its prompt; the adapter reports what that message's frames carried.
///
/// Input counts are fixed when the message starts, but its output count is final only in its
/// `message_delta`. The CLI copies the `message_start` count into the message's assistant frames,
/// so a message cut off before that report has no output figure, and the turn reports none.
#[tokio::test]
async fn interrupted_result_without_usage_reports_the_streamed_message_usage() {
    let aborted = json!({"type": "result", "subtype": "error_during_execution", "is_error": true,
        "duration_ms": 10, "num_turns": 1, "terminal_reason": "aborted_streaming",
        "usage": {"input_tokens": 0, "cache_creation_input_tokens": 0,
            "cache_read_input_tokens": 0, "output_tokens": 0},
        "uuid": uuid::Uuid::new_v4().to_string()});
    let start_usage = || {
        json!({"input_tokens": 4, "cache_creation_input_tokens": 1173,
            "cache_read_input_tokens": 42936, "output_tokens": 3})
    };
    let assistant = |message_id: &str, stop_reason: Value| {
        json!({"type": "assistant", "parent_tool_use_id": null,
            "uuid": uuid::Uuid::new_v4().to_string(),
            "message": {"id": message_id, "type": "message", "role": "assistant",
                "content": [{"type": "text", "text": "partial"}], "stop_reason": stop_reason,
                "usage": {"input_tokens": 4, "cache_creation_input_tokens": 1173,
                    "cache_read_input_tokens": 42936, "output_tokens": 3}}})
    };
    let usage = |output: Option<u64>| {
        vec![forge_provider_api::usage::TokenUsage {
            input: Some(44113),
            cached_input: Some(42936),
            reasoning: None,
            output,
        }]
    };

    // The assistant frame of a message still streaming repeats the start count.
    let mut streaming = started_text_message("msg-1", Some(start_usage()));
    streaming.push(assistant("msg-1", Value::Null));
    let events = interrupted_turn_events(streaming, aborted.clone()).await;
    assert_eq!(reported_usage(&events), usage(None));

    // A message that finished before the abort reported its final count.
    let mut finished = started_text_message("msg-1", Some(start_usage()));
    finished.push(json!({"type": "stream_event", "parent_tool_use_id": null,
        "uuid": uuid::Uuid::new_v4().to_string(),
        "event": {"type": "message_delta", "delta": {"stop_reason": "end_turn"},
            "usage": {"output_tokens": 412}}}));
    let events = interrupted_turn_events(finished, aborted.clone()).await;
    assert_eq!(reported_usage(&events), usage(Some(412)));

    // An assistant frame that names a stop reason still carries the start count.
    let mut stopped = started_text_message("msg-1", Some(start_usage()));
    stopped.push(assistant("msg-1", json!("end_turn")));
    let events = interrupted_turn_events(stopped, aborted.clone()).await;
    assert_eq!(reported_usage(&events), usage(None));

    // A turn aborted before any API message reported usage has no figures to show.
    let events =
        interrupted_turn_events(started_text_message("msg-1", None), aborted.clone()).await;
    assert_eq!(reported_usage(&events), []);

    // An aborted result can carry its own usage, summed from the same start counts. Its input
    // stands, but its output is no more final than the streamed messages it was summed from.
    let mut summed = aborted;
    summed["usage"] = start_usage();
    let mut streaming = started_text_message("msg-1", Some(start_usage()));
    streaming.push(assistant("msg-1", Value::Null));
    let events = interrupted_turn_events(streaming, summed.clone()).await;
    assert_eq!(reported_usage(&events), usage(None));
    let mut stopped = started_text_message("msg-1", Some(start_usage()));
    stopped.push(assistant("msg-1", json!("end_turn")));
    let events = interrupted_turn_events(stopped, summed).await;
    assert_eq!(reported_usage(&events), usage(None));
}

/// Run one turn whose peer streams `frames` and ends it with `result` on its own, returning the
/// turn's events. No interrupt marks the turn, as when the CLI ends it for a reason the adapter
/// did not request or does not recognize.
async fn ended_turn_events(
    frames: Vec<Value>,
    result: Value,
) -> Vec<forge_provider_api::ProviderEvent> {
    let directory = tempfile::tempdir().unwrap();
    let launcher = PeerLauncher::new(move || {
        let frames = frames.clone();
        let result = result.clone();
        Box::new(move |frame| {
            let uuid = user_uuid(frame)?;
            let mut reply = vec![lifecycle(&uuid, "queued"), lifecycle(&uuid, "started")];
            reply.extend(frames.clone());
            reply.push(result.clone());
            Some(reply)
        })
    });
    let (provider, mut opened) = open(launcher, directory.path()).await;
    let binding = opened.binding.id.clone();
    start_prompt(&provider, &binding, "turn-1").await;
    let (events, _) = until_completed(&mut opened.events).await;
    provider.shutdown(operation_context()).await.unwrap();
    events
}

/// An interrupted long answer never reports the start count its frames repeat as its output.
///
/// The frames replay a Claude Code session whose interrupted answer read `42.9k (99% cached)
/// -> 5 · 0 tps`: one API message whose thinking and text frames both carry `stop_reason: null`
/// and the `message_start` count `output_tokens: 5`, then the interrupt and its aborted result.
/// The log kept no stream frames, so each way the count could have passed as final is replayed:
/// a frame that names a stop reason without a `message_delta`, a message that left no usage
/// record, the `message_delta` the CLI itself writes to close an abandoned stream, and a result
/// the adapter did not recognize as its own interrupt. Only a `message_delta` that names a stop
/// reason makes a message's output final.
#[tokio::test]
async fn interrupted_long_answer_never_reports_its_start_count() {
    let native_usage = || {
        json!({"input_tokens": 4, "cache_creation_input_tokens": 245,
            "cache_read_input_tokens": 42640,
            "cache_creation": {"ephemeral_5m_input_tokens": 0, "ephemeral_1h_input_tokens": 245},
            "output_tokens": 5, "service_tier": "standard", "inference_geo": "global",
            "speed": "standard"})
    };
    let event = |event: Value| {
        json!({"type": "stream_event", "event": event, "parent_tool_use_id": null,
            "uuid": uuid::Uuid::new_v4().to_string()})
    };
    let assistant = |block: Value, stop_reason: Value, usage: Option<Value>| {
        let mut message = json!({"model": "claude-haiku-5-5",
            "id": "msg_011ChCnyRjSJUDQ3HntynsZt", "type": "message", "role": "assistant",
            "content": [block], "stop_reason": stop_reason, "stop_sequence": null,
            "stop_details": null, "container": null, "context_management": null});
        if let Some(usage) = usage {
            message["usage"] = usage;
        }
        json!({"type": "assistant", "parent_tool_use_id": null,
            "uuid": uuid::Uuid::new_v4().to_string(), "message": message})
    };
    let answer = |stop_reason: Value, usage: Option<Value>| {
        let mut start = json!({"id": "msg_011ChCnyRjSJUDQ3HntynsZt", "type": "message",
            "role": "assistant", "content": [], "model": "claude-haiku-5-5"});
        if let Some(usage) = &usage {
            start["usage"] = usage.clone();
        }
        vec![
            event(json!({"type": "message_start", "message": start})),
            event(json!({"type": "content_block_start", "index": 0,
                "content_block": {"type": "thinking", "thinking": ""}})),
            event(json!({"type": "content_block_delta", "index": 0,
                "delta": {"type": "thinking_delta", "thinking": "Drafting the answer."}})),
            assistant(
                json!({"type": "thinking", "thinking": "Drafting the answer.", "signature": ""}),
                Value::Null,
                usage.clone(),
            ),
            event(json!({"type": "content_block_stop", "index": 0})),
            event(json!({"type": "content_block_start", "index": 1,
                "content_block": {"type": "text", "text": ""}})),
            event(json!({"type": "content_block_delta", "index": 1,
                "delta": {"type": "text_delta", "text": "A skip list is a set of sorted lists"}})),
            assistant(
                json!({"type": "text", "text": "A skip list is a set of sorted lists"}),
                stop_reason,
                usage,
            ),
        ]
    };
    let aborted = |terminal_reason: &str, usage: Option<Value>| {
        let mut result = json!({"type": "result", "subtype": "error_during_execution",
            "is_error": true, "duration_ms": 12000, "num_turns": 1,
            "terminal_reason": terminal_reason, "uuid": uuid::Uuid::new_v4().to_string()});
        if let Some(usage) = usage {
            result["usage"] = usage;
        }
        result
    };
    let reported = |events: &[forge_provider_api::ProviderEvent]| {
        reported_usage(events)
            .into_iter()
            .map(|usage| usage.output)
            .collect::<Vec<_>>()
    };

    // The recorded sequence: every frame streaming, the result summing the start counts.
    let events = interrupted_turn_events(
        answer(Value::Null, Some(native_usage())),
        aborted("aborted_streaming", Some(native_usage())),
    )
    .await;
    assert_eq!(
        reported(&events),
        [None],
        "the streaming frames' start count"
    );

    // A frame that names a stop reason still repeats the start count without a message_delta.
    let events = interrupted_turn_events(
        answer(json!("end_turn"), Some(native_usage())),
        aborted("aborted_streaming", Some(native_usage())),
    )
    .await;
    assert_eq!(
        reported(&events),
        [None],
        "a stop reason without a message_delta"
    );

    // On an abort the CLI closes the abandoned stream itself: a `message_delta` that copies the
    // partial message's null stop reason and its start count, then `message_stop`. Only a
    // `message_delta` that names a stop reason ends a message the API finished.
    let mut closed = answer(Value::Null, Some(native_usage()));
    closed.push(event(json!({"type": "content_block_stop", "index": 1})));
    closed.push(event(
        json!({"type": "message_delta", "context_management": null,
        "delta": {"container": null, "stop_details": null, "stop_reason": null,
            "stop_sequence": null},
        "usage": {"output_tokens": 5, "input_tokens": 4, "cache_creation_input_tokens": 245,
            "cache_read_input_tokens": 42640}}),
    ));
    closed.push(event(json!({"type": "message_stop"})));
    let events =
        interrupted_turn_events(closed, aborted("aborted_streaming", Some(native_usage()))).await;
    assert_eq!(
        reported(&events),
        [None],
        "the CLI's closing message_delta for an abandoned stream"
    );

    // A message that left no usage record proves nothing about the result's output.
    let events = interrupted_turn_events(
        answer(Value::Null, None),
        aborted("aborted_streaming", Some(native_usage())),
    )
    .await;
    assert_eq!(reported(&events), [None], "no retained message usage");

    // A result the adapter did not interrupt and does not recognize as an interrupt.
    let events = ended_turn_events(
        answer(Value::Null, Some(native_usage())),
        aborted("aborted_by_user", Some(native_usage())),
    )
    .await;
    assert_eq!(reported(&events), [None], "an unrecognized abort");
}

/// A reply whose API message ended the model's turn is the response before the result arrives.
///
/// The CLI sends its `result` only after post-turn work such as instruction hooks and context
/// accounting, and the reply stayed unclassified, inside the exchange's live activity, until
/// then. The `message_delta` that stops the message with `end_turn` already proves it is the
/// reply, so the adapter classifies it there.
#[tokio::test(start_paused = true)]
async fn end_turn_classifies_the_reply_before_the_result() {
    use forge_provider_api::message::{
        AgentMessageRole, ItemEvent, MessageClassification, MessageItemEvent,
    };
    let directory = tempfile::tempdir().unwrap();
    let launcher = PeerLauncher::new(|| {
        Box::new(move |frame| {
            if let Some(uuid) = user_uuid(frame) {
                let mut frames = vec![lifecycle(&uuid, "queued"), lifecycle(&uuid, "started")];
                frames.extend(text_message("msg-1", &["ready"]));
                return Some(frames);
            }
            // The test interrupts the turn this peer leaves open.
            if frame["request"]["subtype"] == "interrupt" {
                return Some(vec![
                    success(frame["request_id"].clone(), json!({})),
                    result_interrupted(),
                ]);
            }
            None
        })
    });
    let (provider, mut opened) = open(launcher, directory.path()).await;
    let binding = opened.binding.id.clone();
    let request = prompt_request(
        &binding,
        "turn-1",
        receipt("first"),
        forge_provider_api::permissions::PermissionResponse::Allow,
    );
    let scope = request.scope.clone();
    provider
        .turn_start(operation_context(), &binding, request)
        .await
        .unwrap();
    let classified = tokio::time::timeout(std::time::Duration::from_secs(60), async {
        loop {
            let envelope = opened.events.recv().await.unwrap().unwrap();
            if let forge_provider_api::ProviderEvent::Item {
                event:
                    ItemEvent::Message {
                        event: MessageItemEvent::Classified(MessageClassification::Known(role)),
                    },
                ..
            } = envelope.event
            {
                return role;
            }
        }
    })
    .await
    .expect("the reply stayed unclassified until the result");
    assert_eq!(classified, AgentMessageRole::Response);
    provider
        .turn_interrupt(
            operation_context(),
            &binding,
            forge_provider_api::CancelRequest {
                scope,
                input: forge_provider_api::operations::InterruptInput::Retain,
            },
        )
        .await
        .unwrap();
    until_completed(&mut opened.events).await;
    provider.shutdown(operation_context()).await.unwrap();
}

/// A completed turn reports the result's usage once, not again from its message frames.
#[tokio::test]
async fn completed_result_reports_only_its_own_usage() {
    let directory = tempfile::tempdir().unwrap();
    let launcher = PeerLauncher::new(|| {
        Box::new(move |frame| {
            let uuid = user_uuid(frame)?;
            let mut frames = vec![lifecycle(&uuid, "queued"), lifecycle(&uuid, "started")];
            frames.extend(text_message("msg-1", &["done"]));
            frames.push(lifecycle(&uuid, "completed"));
            frames.push(result_success());
            Some(frames)
        })
    });
    let (provider, mut opened) = open(launcher, directory.path()).await;
    let binding = opened.binding.id.clone();
    provider
        .turn_start(
            operation_context(),
            &binding,
            prompt_request(
                &binding,
                "turn-1",
                receipt("first"),
                forge_provider_api::permissions::PermissionResponse::Allow,
            ),
        )
        .await
        .unwrap();
    let (events, _) = until_completed(&mut opened.events).await;
    assert_eq!(
        reported_usage(&events),
        [forge_provider_api::usage::TokenUsage {
            input: Some(1),
            cached_input: None,
            reasoning: None,
            output: Some(2),
        }]
    );
    provider.shutdown(operation_context()).await.unwrap();
}

/// An interrupted reply ends as the turn's response, as a completed one does.
///
/// Harness hides an interrupted response and keeps other prose, so prose the adapter left
/// unclassified stayed in the transcript after Ctrl-C while Codex and Copilot replies left it.
/// The end of a turn settles each pending prose item's role whatever the turn's outcome.
#[tokio::test]
async fn interrupted_reply_is_classified_as_the_response() {
    use forge_provider_api::message::{ItemCompletion, ItemEvent, MessageOutcome};
    let directory = tempfile::tempdir().unwrap();
    let launcher = PeerLauncher::new(|| {
        Box::new(move |frame| {
            if let Some(uuid) = user_uuid(frame) {
                let mut frames = vec![lifecycle(&uuid, "queued"), lifecycle(&uuid, "started")];
                frames.extend(text_message("msg-1", &["partial reply"]));
                return Some(frames);
            }
            if frame["request"]["subtype"] == "interrupt" {
                return Some(vec![
                    success(frame["request_id"].clone(), json!({})),
                    result_interrupted(),
                ]);
            }
            None
        })
    });
    let (provider, mut opened) = open(launcher.clone(), directory.path()).await;
    let binding = opened.binding.id.clone();
    let request = prompt_request(
        &binding,
        "turn-1",
        receipt("first"),
        forge_provider_api::permissions::PermissionResponse::Allow,
    );
    let scope = request.scope.clone();
    provider
        .turn_start(operation_context(), &binding, request)
        .await
        .unwrap();
    until_message(&mut opened.events).await;
    provider
        .turn_interrupt(
            operation_context(),
            &binding,
            forge_provider_api::CancelRequest {
                scope,
                input: forge_provider_api::operations::InterruptInput::Retain,
            },
        )
        .await
        .unwrap();
    let (events, completed) = until_completed(&mut opened.events).await;

    assert_eq!(
        completed.result().outcome(),
        forge_provider_api::turn::TurnOutcome::Interrupted
    );
    assert_eq!(classifications(&events), ["Known(Response)"]);
    let outcomes = events
        .iter()
        .filter_map(|event| match event {
            forge_provider_api::ProviderEvent::Item {
                event: ItemEvent::Completed(ItemCompletion::AgentMessage(completion)),
                ..
            } => Some((completion.text.as_str(), completion.outcome)),
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(outcomes, [("partial reply", MessageOutcome::Interrupted)]);
    provider.shutdown(operation_context()).await.unwrap();
}

/// The CLI closes a running tool with an error result when the client interrupts the turn.
///
/// That result reports no failure of the tool itself, so the tool ends as interrupted, as the
/// tools the CLI leaves without any result do.
#[tokio::test]
async fn interrupted_tool_result_ends_the_tool_as_interrupted() {
    use forge_provider_api::message::{ItemCompletion, ItemEvent, ItemStart};
    let directory = tempfile::tempdir().unwrap();
    let launcher = PeerLauncher::new(|| {
        Box::new(move |frame| {
            if let Some(uuid) = user_uuid(frame) {
                return Some(vec![
                    lifecycle(&uuid, "queued"),
                    lifecycle(&uuid, "started"),
                    json!({"type": "assistant", "message": {"id": "msg-1", "model": "claude-haiku-5-5",
                        "type": "message", "role": "assistant", "content": [{"type": "tool_use",
                            "id": "toolu_1", "name": "Bash", "input": {"command": "sleep 60"}}],
                        "usage": {"input_tokens": 1, "output_tokens": 2}},
                        "parent_tool_use_id": null, "uuid": uuid::Uuid::new_v4().to_string()}),
                ]);
            }
            if frame["request"]["subtype"] == "interrupt" {
                let mut aborted = result_interrupted();
                aborted["terminal_reason"] = json!("aborted_tools");
                return Some(vec![
                    success(frame["request_id"].clone(), json!({})),
                    json!({"type": "user", "message": {"role": "user", "content": [{
                        "type": "tool_result", "tool_use_id": "toolu_1", "is_error": true,
                        "content": "[Request interrupted by user for tool use]"}]},
                        "parent_tool_use_id": null, "uuid": uuid::Uuid::new_v4().to_string()}),
                    aborted,
                ]);
            }
            None
        })
    });
    let (provider, mut opened) = open(launcher.clone(), directory.path()).await;
    let binding = opened.binding.id.clone();
    let request = prompt_request(
        &binding,
        "turn-1",
        receipt("first"),
        forge_provider_api::permissions::PermissionResponse::Allow,
    );
    let scope = request.scope.clone();
    provider
        .turn_start(operation_context(), &binding, request)
        .await
        .unwrap();
    loop {
        let envelope = opened.events.recv().await.unwrap().unwrap();
        if matches!(
            envelope.event,
            forge_provider_api::ProviderEvent::Item {
                event: ItemEvent::Started(ItemStart::CommandExecution(_)),
                ..
            }
        ) {
            break;
        }
    }
    provider
        .turn_interrupt(
            operation_context(),
            &binding,
            forge_provider_api::CancelRequest {
                scope,
                input: forge_provider_api::operations::InterruptInput::Retain,
            },
        )
        .await
        .unwrap();
    let (events, completed) = until_completed(&mut opened.events).await;

    assert_eq!(
        completed.result().outcome(),
        forge_provider_api::turn::TurnOutcome::Interrupted
    );
    let outcomes = events
        .iter()
        .filter_map(|event| match event {
            forge_provider_api::ProviderEvent::Item {
                event: ItemEvent::Completed(ItemCompletion::CommandExecution(completion)),
                ..
            } => Some(completion.outcome.clone()),
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(
        outcomes,
        [forge_provider_api::tool::ToolOutcome::Interrupted]
    );
    provider.shutdown(operation_context()).await.unwrap();
}

/// A `can_use_tool` request reaches Harness permissions, and a denial answers the CLI with deny.
#[tokio::test]
async fn permission_requests_route_through_harness_decisions() {
    let directory = tempfile::tempdir().unwrap();
    let launcher = PeerLauncher::new(|| {
        let mut prompt: Option<Value> = None;
        Box::new(move |frame| {
            if let Some(uuid) = user_uuid(frame) {
                prompt = Some(uuid.clone());
                return Some(vec![
                    lifecycle(&uuid, "queued"),
                    lifecycle(&uuid, "started"),
                    json!({"type": "control_request", "request_id": "cli-1", "request": {
                        "subtype": "can_use_tool", "tool_name": "Bash",
                        "input": {"command": "rm -rf build"}, "tool_use_id": "toolu_1"}}),
                ]);
            }
            if frame["type"] == "control_response" {
                let uuid = prompt.take()?;
                let mut frames = text_message("msg-1", &["denied"]);
                frames.push(lifecycle(&uuid, "completed"));
                frames.push(result_success());
                return Some(frames);
            }
            None
        })
    });
    let (provider, mut opened) = open(launcher.clone(), directory.path()).await;
    let binding = opened.binding.id.clone();
    provider
        .turn_start(
            operation_context(),
            &binding,
            prompt_request(
                &binding,
                "turn-1",
                receipt("first"),
                forge_provider_api::permissions::PermissionResponse::Deny { reason: None },
            ),
        )
        .await
        .unwrap();
    until_completed(&mut opened.events).await;

    let answer = launcher.frames("control_response").pop().unwrap();
    assert_eq!(answer["response"]["request_id"], "cli-1");
    assert_eq!(answer["response"]["response"]["behavior"], "deny");
    provider.shutdown(operation_context()).await.unwrap();
}

/// Workspace model discovery runs a process that persists no transcript.
#[tokio::test]
async fn workspace_models_use_an_unpersisted_process() {
    let directory = tempfile::tempdir().unwrap();
    let launcher = PeerLauncher::new(|| Box::new(|_| None));
    let provider =
        ClaudeProvider::with_launcher(launcher.clone(), directory.path().join("runtime"));
    let models = provider
        .model_list(
            operation_context(),
            forge_provider_api::QueryContext::Workspace(forge_provider_api::WorkspaceContext {
                workspace: directory.path().to_string_lossy().into_owned(),
            }),
        )
        .await
        .unwrap();

    let ids: Vec<_> = models.iter().map(|model| model.id.as_str()).collect();
    assert_eq!(ids, ["default", "haiku"]);
    let launches = launcher.record.lock().unwrap().launches.clone();
    assert!(
        launches[0]
            .iter()
            .any(|argument| argument == "--no-session-persistence")
    );
    provider.shutdown(operation_context()).await.unwrap();
}

/// Unsupported controls fail before any process or profile is created.
#[tokio::test]
async fn unsupported_options_reject_before_launch() {
    use forge_provider_api::capability::{ProviderFeature, UnsupportedCapability};
    let directory = tempfile::tempdir().unwrap();
    let launcher = PeerLauncher::new(|| Box::new(|_| None));
    let provider =
        ClaudeProvider::with_launcher(launcher.clone(), directory.path().join("runtime"));
    let mut request = session_request(directory.path());
    request.configuration.access.sandbox = true;
    let result = provider.thread_open(operation_context(), request).await;

    assert!(matches!(
        result,
        Err(ProviderError::Unsupported(UnsupportedCapability(
            ProviderFeature::OsSandbox
        )))
    ));
    assert!(launcher.record.lock().unwrap().launches.is_empty());
    provider.shutdown(operation_context()).await.unwrap();
}

/// Cut at the first discarded prompt and continue through the same binding.
#[tokio::test]
async fn rewind_keeps_the_live_cli_when_the_cut_is_known() {
    let directory = tempfile::tempdir().unwrap();
    let launcher = PeerLauncher::new(|| {
        let mut answer = answering_peer(&["ok"]);
        Box::new(move |frame| {
            if frame["request"]["subtype"] == "rewind_conversation" {
                return Some(vec![success(
                    frame["request_id"].clone(),
                    json!({
                        "rewound": true,
                        "targetMessageUuid": frame["request"]["target_message_uuid"],
                    }),
                )]);
            }
            answer(frame)
        })
    });
    let (provider, mut opened) = open(launcher.clone(), directory.path()).await;
    let binding = opened.binding.id.clone();
    let kept = run_turn(&provider, &mut opened, "keep", false)
        .await
        .result()
        .checkpoint_id()
        .cloned()
        .unwrap();
    run_turn(&provider, &mut opened, "discard", false).await;
    run_turn(&provider, &mut opened, "discard later", false).await;
    let prompts = launcher.frames("user");

    provider
        .thread_rewind(
            operation_context(),
            &binding,
            forge_provider_api::ThreadRewindRequest {
                after: Some(kept.clone()),
            },
        )
        .await
        .unwrap();
    provider
        .thread_rewind(
            operation_context(),
            &binding,
            forge_provider_api::ThreadRewindRequest { after: Some(kept) },
        )
        .await
        .unwrap();
    run_turn(&provider, &mut opened, "after rewind", false).await;

    assert_eq!(launcher.record.lock().unwrap().launches.len(), 1);
    let rewinds: Vec<_> = launcher
        .frames("control_request")
        .into_iter()
        .filter(|frame| frame["request"]["subtype"] == "rewind_conversation")
        .collect();
    assert_eq!(rewinds.len(), 1);
    assert_eq!(
        rewinds[0]["request"]["target_message_uuid"],
        prompts[1]["uuid"]
    );
    assert_eq!(
        rewinds[0]["request"]["last_seen_user_message_uuid"],
        prompts[2]["uuid"]
    );
    provider.shutdown(operation_context()).await.unwrap();
}

/// Preserve the binding and report the native reason when the CLI detects an unseen turn.
#[tokio::test]
async fn rewind_refusal_does_not_bypass_the_live_history_guard() {
    let directory = tempfile::tempdir().unwrap();
    let launcher = PeerLauncher::new(|| {
        let mut answer = answering_peer(&["ok"]);
        Box::new(move |frame| {
            if frame["request"]["subtype"] == "rewind_conversation" {
                return Some(vec![success(
                    frame["request_id"].clone(),
                    json!({
                        "rewound": false,
                        "reason": "unseen_later_turn",
                        "error": "unseen later turn",
                    }),
                )]);
            }
            answer(frame)
        })
    });
    let (provider, mut opened) = open(launcher.clone(), directory.path()).await;
    let binding = opened.binding.id.clone();
    let kept = run_turn(&provider, &mut opened, "keep", false)
        .await
        .result()
        .checkpoint_id()
        .cloned()
        .unwrap();
    run_turn(&provider, &mut opened, "discard", false).await;

    let error = provider
        .thread_rewind(
            operation_context(),
            &binding,
            forge_provider_api::ThreadRewindRequest { after: Some(kept) },
        )
        .await
        .unwrap_err();

    assert!(error.to_string().contains("unseen later turn"), "{error}");
    run_turn(&provider, &mut opened, "after refusal", false).await;
    assert_eq!(launcher.record.lock().unwrap().launches.len(), 1);
    provider.shutdown(operation_context()).await.unwrap();
}

/// Rewinding restarts the process; the deliberate exit of the old process must not invalidate the
/// binding, so the next prompt runs on the resumed process.
///
/// The rewind truncates in one process and then resumes plainly in another, so that the
/// truncating process's exit records the kept branch as the transcript's leaf.
#[tokio::test]
async fn prompts_run_after_a_rewind_restart() {
    let directory = tempfile::tempdir().unwrap();
    let launcher = PeerLauncher::new(|| {
        Box::new(|frame| {
            let uuid = user_uuid(frame)?;
            if frame["shouldQuery"] == false {
                return Some(vec![
                    lifecycle(&uuid, "queued"),
                    lifecycle(&uuid, "completed"),
                ]);
            }
            let mut frames = vec![lifecycle(&uuid, "queued"), lifecycle(&uuid, "started")];
            frames.extend(text_message("msg-1", &["ok"]));
            frames.push(lifecycle(&uuid, "completed"));
            frames.push(result_success());
            Some(frames)
        })
    });
    let (provider, mut opened) = open(launcher.clone(), directory.path()).await;
    let binding = opened.binding.id.clone();
    let mut checkpoints = Vec::new();
    for turn in ["turn-1", "turn-2"] {
        provider
            .turn_start(
                operation_context(),
                &binding,
                prompt_request(
                    &binding,
                    turn,
                    receipt(turn),
                    forge_provider_api::permissions::PermissionResponse::Allow,
                ),
            )
            .await
            .unwrap();
        let (_, completed) = until_completed(&mut opened.events).await;
        checkpoints.push(completed.result().checkpoint_id().cloned().unwrap());
    }
    provider
        .thread_rewind(
            operation_context(),
            &binding,
            forge_provider_api::ThreadRewindRequest {
                after: Some(checkpoints[0].clone()),
            },
        )
        .await
        .unwrap();
    provider
        .turn_start(
            operation_context(),
            &binding,
            prompt_request(
                &binding,
                "turn-3",
                receipt("turn-3"),
                forge_provider_api::permissions::PermissionResponse::Allow,
            ),
        )
        .await
        .unwrap();
    until_completed(&mut opened.events).await;

    let launches = launcher.record.lock().unwrap().launches.clone();
    assert_eq!(launches.len(), 3);
    assert_eq!(
        argument_value(&launches[1], "--resume-session-at"),
        Some(checkpoints[0].as_str())
    );
    assert_eq!(
        argument_value(&launches[2], "--resume"),
        argument_value(&launches[1], "--resume")
    );
    assert_eq!(argument_value(&launches[2], "--resume-session-at"), None);
    provider.shutdown(operation_context()).await.unwrap();
}

/// One persisted transcript chain entry of the modelled CLI conversation.
#[derive(Clone, Debug, PartialEq)]
enum NativeEntry {
    /// The user entry that starts a turn.
    Prompt(String),
    /// An assistant entry, which a turn reports as its checkpoint.
    Answer(String),
    /// A user entry written with `shouldQuery: false`, such as the adapter's rewind note.
    Note(String),
    /// A steer the running turn absorbed, which the CLI records as a `queued_command` attachment.
    Queued(String),
}

impl NativeEntry {
    fn uuid(&self) -> &str {
        match self {
            Self::Prompt(uuid) | Self::Answer(uuid) | Self::Note(uuid) | Self::Queued(uuid) => uuid,
        }
    }
}

/// Read the value of one `--name=value` argument.
fn argument_value<'arguments>(
    arguments: &'arguments [String],
    name: &str,
) -> Option<&'arguments str> {
    arguments
        .iter()
        .find_map(|argument| argument.strip_prefix(name)?.strip_prefix('='))
}

/// Apply the CLI's truncating resume to a persisted chain, or refuse it as the CLI does.
///
/// `--resume-drops-turn` declares the prompt of the one turn the truncation discards; the CLI
/// exits when the discarded range does not start with that prompt or holds another user entry
/// or absorbed queued content.
fn resume_native_chain(chain: &mut Vec<NativeEntry>, arguments: &[String]) -> bool {
    if argument_value(arguments, "--resume").is_none() {
        chain.clear();
        return true;
    }
    let Some(resume_at) = argument_value(arguments, "--resume-session-at") else {
        return true;
    };
    let Some(kept) = chain.iter().position(|entry| entry.uuid() == resume_at) else {
        return false;
    };
    let discarded = &chain[kept + 1..];
    if let Some(dropped_prompt) = argument_value(arguments, "--resume-drops-turn") {
        let starts_with_prompt =
            discarded.is_empty() || discarded[0] == NativeEntry::Prompt(dropped_prompt.to_owned());
        let attributable = discarded
            .iter()
            .skip(1)
            .all(|entry| matches!(entry, NativeEntry::Answer(_)));
        if !starts_with_prompt || !attributable {
            return false;
        }
    }
    chain.truncate(kept + 1);
    true
}

/// A rewind succeeds however many rewinds came before it in the session, because each rewind
/// appends a note to the native chain that a later guarded resume must not count as content of
/// the dropped turn: neither the note before a dropped turn's prompt nor the note after it.
#[tokio::test]
async fn rewinds_after_earlier_rewinds_resume_past_their_notes() {
    let directory = tempfile::tempdir().unwrap();
    let chain: Arc<Mutex<Vec<NativeEntry>>> = Arc::default();
    let peer_chain = chain.clone();
    let launcher = PeerLauncher::with_arguments(move |arguments| {
        if !resume_native_chain(&mut peer_chain.lock().unwrap(), arguments) {
            return None;
        }
        let chain = peer_chain.clone();
        let handler: PeerHandler = Box::new(move |frame: &Value| {
            let uuid = user_uuid(frame)?;
            let entry_uuid = uuid.as_str().unwrap().to_owned();
            if frame["shouldQuery"] == false {
                chain.lock().unwrap().push(NativeEntry::Note(entry_uuid));
                return Some(vec![
                    lifecycle(&uuid, "queued"),
                    lifecycle(&uuid, "completed"),
                ]);
            }
            let answer = text_message("msg-1", &["ok"]);
            let answer_uuid = answer
                .iter()
                .find(|frame| frame["type"] == "assistant")
                .and_then(|frame| frame["uuid"].as_str())
                .unwrap()
                .to_owned();
            chain.lock().unwrap().extend([
                NativeEntry::Prompt(entry_uuid),
                NativeEntry::Answer(answer_uuid),
            ]);
            let mut frames = vec![lifecycle(&uuid, "queued"), lifecycle(&uuid, "started")];
            frames.extend(answer);
            frames.push(lifecycle(&uuid, "completed"));
            frames.push(result_success());
            Some(frames)
        });
        Some(handler)
    });
    let (provider, mut opened) = open(launcher.clone(), directory.path()).await;
    let binding = opened.binding.id.clone();
    let mut checkpoint_of_turn =
        std::collections::HashMap::<&str, forge_provider_api::CheckpointId>::new();
    let mut rewind_count = 0;
    for step in [
        "turn-1",
        "turn-2",
        "rewind:turn-1",
        "turn-3",
        "rewind:turn-1",
        "turn-4",
        "turn-5",
        "rewind:turn-4",
        "rewind:turn-1",
    ] {
        if let Some(target) = step.strip_prefix("rewind:") {
            rewind_count += 1;
            let rewound = provider
                .thread_rewind(
                    operation_context(),
                    &binding,
                    forge_provider_api::ThreadRewindRequest {
                        after: Some(checkpoint_of_turn[target].clone()),
                    },
                )
                .await;
            assert!(
                rewound.is_ok(),
                "rewind {rewind_count} to {target} failed: {rewound:?}"
            );
            continue;
        }
        provider
            .turn_start(
                operation_context(),
                &binding,
                prompt_request(
                    &binding,
                    step,
                    receipt(step),
                    forge_provider_api::permissions::PermissionResponse::Allow,
                ),
            )
            .await
            .unwrap();
        let (_, completed) = until_completed(&mut opened.events).await;
        checkpoint_of_turn.insert(step, completed.result().checkpoint_id().cloned().unwrap());
    }

    let kept_turns = chain
        .lock()
        .unwrap()
        .iter()
        .filter(|entry| !matches!(entry, NativeEntry::Note(_)))
        .count();
    assert_eq!(kept_turns, 2, "only turn-1 remains in the native chain");
    provider.shutdown(operation_context()).await.unwrap();
}

/// A rewind that drops a turn which absorbed a steer succeeds, because the CLI records the
/// absorbed steer as queued content that its `--resume-drops-turn` guard refuses to discard, so
/// the adapter resumes such a range unguarded.
#[tokio::test]
async fn rewinds_drop_a_turn_that_absorbed_a_steer() {
    let directory = tempfile::tempdir().unwrap();
    let chain: Arc<Mutex<Vec<NativeEntry>>> = Arc::default();
    let peer_chain = chain.clone();
    let launcher = PeerLauncher::with_arguments(move |arguments| {
        if !resume_native_chain(&mut peer_chain.lock().unwrap(), arguments) {
            return None;
        }
        let chain = peer_chain.clone();
        let mut held_prompt: Option<Value> = None;
        let handler: PeerHandler = Box::new(move |frame: &Value| {
            let uuid = user_uuid(frame)?;
            let entry_uuid = uuid.as_str().unwrap().to_owned();
            if frame["shouldQuery"] == false {
                chain.lock().unwrap().push(NativeEntry::Note(entry_uuid));
                return Some(vec![
                    lifecycle(&uuid, "queued"),
                    lifecycle(&uuid, "completed"),
                ]);
            }
            let answer = text_message("msg-1", &["ok"]);
            let answer_uuid = answer
                .iter()
                .find(|frame| frame["type"] == "assistant")
                .and_then(|frame| frame["uuid"].as_str())
                .unwrap()
                .to_owned();
            let steered = chain.lock().unwrap().len() == 2;
            if let Some(original) = held_prompt.take() {
                chain.lock().unwrap().extend([
                    NativeEntry::Queued(entry_uuid),
                    NativeEntry::Answer(answer_uuid),
                ]);
                let mut frames = vec![lifecycle(&uuid, "queued"), lifecycle(&uuid, "started")];
                frames.extend(answer);
                frames.push(lifecycle(&original, "completed"));
                frames.push(lifecycle(&uuid, "completed"));
                frames.push(result_success());
                return Some(frames);
            }
            chain.lock().unwrap().extend([
                NativeEntry::Prompt(entry_uuid),
                NativeEntry::Answer(answer_uuid),
            ]);
            let mut frames = vec![lifecycle(&uuid, "queued"), lifecycle(&uuid, "started")];
            frames.extend(answer);
            if steered {
                held_prompt = Some(uuid);
                return Some(frames);
            }
            frames.push(lifecycle(&uuid, "completed"));
            frames.push(result_success());
            Some(frames)
        });
        Some(handler)
    });
    let (provider, mut opened) = open(launcher.clone(), directory.path()).await;
    let binding = opened.binding.id.clone();
    provider
        .turn_start(
            operation_context(),
            &binding,
            prompt_request(
                &binding,
                "turn-1",
                receipt("turn-1"),
                forge_provider_api::permissions::PermissionResponse::Allow,
            ),
        )
        .await
        .unwrap();
    let (_, kept) = until_completed(&mut opened.events).await;
    let kept_checkpoint = kept.result().checkpoint_id().cloned().unwrap();
    let steered = prompt_request(
        &binding,
        "steered-turn",
        receipt("steered-turn"),
        forge_provider_api::permissions::PermissionResponse::Allow,
    );
    let scope = steered.scope.clone();
    provider
        .turn_start(operation_context(), &binding, steered)
        .await
        .unwrap();
    until_message(&mut opened.events).await;
    provider
        .turn_steer(
            operation_context(),
            &binding,
            steering_request(scope, "check results arrived"),
        )
        .await
        .unwrap();
    until_completed(&mut opened.events).await;

    let rewound = provider
        .thread_rewind(
            operation_context(),
            &binding,
            forge_provider_api::ThreadRewindRequest {
                after: Some(kept_checkpoint),
            },
        )
        .await;

    assert!(
        rewound.is_ok(),
        "rewind past the steered turn failed: {rewound:?}"
    );
    let remaining = chain
        .lock()
        .unwrap()
        .iter()
        .filter(|entry| !matches!(entry, NativeEntry::Note(_)))
        .count();
    assert_eq!(remaining, 2, "only turn-1 remains in the native chain");
    provider.shutdown(operation_context()).await.unwrap();
}

/// Answer prompts with one reply each and acknowledge notes, which run no model turn.
fn noting_peer() -> PeerHandler {
    Box::new(|frame| {
        let uuid = user_uuid(frame)?;
        if frame["shouldQuery"] == false {
            return Some(vec![
                lifecycle(&uuid, "queued"),
                lifecycle(&uuid, "completed"),
            ]);
        }
        let mut frames = vec![lifecycle(&uuid, "queued"), lifecycle(&uuid, "started")];
        frames.extend(text_message("msg-1", &["ok"]));
        frames.push(lifecycle(&uuid, "completed"));
        frames.push(result_success());
        Some(frames)
    })
}

/// Fork a live binding into a separately owned target conversation.
async fn fork_binding(
    provider: &ClaudeProvider,
    binding: &forge_provider_api::BindingId,
    workspace: &std::path::Path,
) -> forge_provider_api::ProviderSessionId {
    let source = session_request(workspace);
    provider
        .thread_fork(
            operation_context(),
            forge_provider_api::session::SessionForkRequest {
                source: forge_provider_api::ForkSource::Binding(binding.clone()),
                target: forge_provider_api::ForkTarget {
                    thread_id: forge_provider_api::testing::root_thread("claude-fork-owner"),
                    workspace: source.workspace,
                    configuration: source.configuration,
                    system_instructions: source.system_instructions,
                },
            },
        )
        .await
        .unwrap()
        .provider_session_id
}

/// A fork made after a rewind copies the history the rewind kept, and the fork's first turn
/// follows that history directly.
///
/// The rewind leaves the discarded turn in the CLI transcript beside the kept history, and an
/// unpositioned `--fork-session` copied the discarded branch, so the fork names the kept turn's
/// checkpoint. The note that materializes the fork is the target's newest entry; resuming the
/// target at the same checkpoint discards it when the first prompt is appended, so the model never
/// reads the note or the "No response requested." reply the CLI adds after a trailing user entry.
#[tokio::test]
async fn forks_after_a_rewind_copy_the_kept_history() {
    let directory = tempfile::tempdir().unwrap();
    let launcher = PeerLauncher::new(noting_peer);
    let (provider, mut opened) = open(launcher.clone(), directory.path()).await;
    let binding = opened.binding.id.clone();
    let mut checkpoints = Vec::new();
    for turn in ["turn-1", "turn-2"] {
        provider
            .turn_start(
                operation_context(),
                &binding,
                prompt_request(
                    &binding,
                    turn,
                    receipt(turn),
                    forge_provider_api::permissions::PermissionResponse::Allow,
                ),
            )
            .await
            .unwrap();
        let (_, completed) = until_completed(&mut opened.events).await;
        checkpoints.push(completed.result().checkpoint_id().cloned().unwrap());
    }
    provider
        .thread_rewind(
            operation_context(),
            &binding,
            forge_provider_api::ThreadRewindRequest {
                after: Some(checkpoints[0].clone()),
            },
        )
        .await
        .unwrap();

    let target = fork_binding(&provider, &binding, directory.path()).await;
    let mut target_request = session_request(directory.path());
    target_request.thread_id = forge_provider_api::testing::root_thread("claude-fork-owner");
    target_request.history = forge_provider_api::SessionHistory::Durable {
        resume: Some(target.clone()),
    };
    provider
        .thread_open(operation_context(), target_request)
        .await
        .unwrap();

    let launches = launcher.record.lock().unwrap().launches.clone();
    let fork = launches
        .iter()
        .find(|arguments| {
            arguments
                .iter()
                .any(|argument| argument == "--fork-session")
        })
        .expect("the fork launched a process");
    assert_eq!(
        argument_value(fork, "--resume-session-at"),
        Some(checkpoints[0].as_str()),
        "the fork copies the kept history, not the discarded turn"
    );
    let target_open = launches.last().unwrap();
    assert_eq!(
        argument_value(target_open, "--resume"),
        Some(target.as_str())
    );
    assert_eq!(
        argument_value(target_open, "--resume-session-at"),
        Some(checkpoints[0].as_str()),
        "the target resumes before the note that materialized it"
    );
    provider.shutdown(operation_context()).await.unwrap();
}

/// One modelled CLI session transcript: every entry under the entry it follows, and the leaf
/// checkpoints the CLI's processes recorded when they exited.
#[derive(Clone, Default)]
struct SessionTranscript {
    parent_of: std::collections::HashMap<String, Option<String>>,
    entries: Vec<String>,
    checkpoints: Vec<String>,
}

impl SessionTranscript {
    fn append(&mut self, entry: String, parent: Option<String>) {
        self.parent_of.insert(entry.clone(), parent);
        self.entries.push(entry);
    }

    /// Report whether `entry` is `ancestor` or follows it.
    fn descends(&self, entry: &str, ancestor: &str) -> bool {
        let mut current = Some(entry.to_owned());
        while let Some(visited) = current {
            if visited == ancestor {
                return true;
            }
            current = self.parent_of.get(&visited).cloned().flatten();
        }
        false
    }

    /// Get the chain a resume loads, oldest entry first.
    ///
    /// The CLI loads the chain of its newest entry when that entry follows the newest recorded
    /// checkpoint, and otherwise the chain of the checkpoint, which it takes for the branch the
    /// user kept.
    fn loaded_chain(&self) -> Vec<String> {
        let leaf = match (self.checkpoints.last(), self.entries.last()) {
            (Some(checkpoint), Some(newest)) if !self.descends(newest, checkpoint) => checkpoint,
            (_, Some(newest)) => newest,
            (Some(checkpoint), None) => checkpoint,
            (None, None) => return Vec::new(),
        };
        let mut chain = Vec::new();
        let mut current = Some(leaf.clone());
        while let Some(entry) = current {
            current = self.parent_of.get(&entry).cloned().flatten();
            chain.push(entry);
        }
        chain.reverse();
        chain
    }
}

/// The modelled transcripts of every session one CLI installation wrote.
type SessionTranscripts = Arc<Mutex<std::collections::HashMap<String, SessionTranscript>>>;

/// The session and newest entry of one modelled CLI process.
struct ModelledProcess {
    transcripts: SessionTranscripts,
    session: Option<String>,
    tip: Option<String>,
}

impl ModelledProcess {
    fn append(&mut self, entry: String) {
        if let Some(session) = &self.session {
            self.transcripts
                .lock()
                .unwrap()
                .entry(session.clone())
                .or_default()
                .append(entry.clone(), self.tip.clone());
        }
        self.tip = Some(entry);
    }
}

impl Drop for ModelledProcess {
    /// Record the process's newest entry as the session's leaf checkpoint, as the CLI does when
    /// its input closes and it exits.
    fn drop(&mut self) {
        if let (Some(session), Some(tip)) = (&self.session, &self.tip) {
            self.transcripts
                .lock()
                .unwrap()
                .entry(session.clone())
                .or_default()
                .checkpoints
                .push(tip.clone());
        }
    }
}

/// Model a CLI whose transcripts branch where a resume truncates them, which answers every prompt
/// except the one at `unanswered_request` and records an interrupt as a user entry.
///
/// A resume or fork names `--resume-session-at` only within the chain it loaded, and otherwise
/// prints the CLI's "No message found" refusal and exits.
fn branching_transcript_peer(unanswered_request: usize) -> (Arc<PeerLauncher>, SessionTranscripts) {
    let transcripts: SessionTranscripts = Arc::default();
    let requests = Arc::new(std::sync::atomic::AtomicUsize::new(0));
    let peer_transcripts = transcripts.clone();
    let launcher = PeerLauncher::with_refusals(move |arguments| {
        let resume_at = argument_value(arguments, "--resume-session-at");
        let named_session = argument_value(arguments, "--session-id").map(str::to_owned);
        let (session, tip) = match argument_value(arguments, "--resume") {
            Some(resumed) => {
                let mut sessions = peer_transcripts.lock().unwrap();
                let mut chain = sessions
                    .get(resumed)
                    .cloned()
                    .unwrap_or_default()
                    .loaded_chain();
                if let Some(resume_at) = resume_at {
                    let kept = chain
                        .iter()
                        .position(|entry| entry == resume_at)
                        .ok_or_else(|| {
                            format!("No message found with message.uuid of: {resume_at}")
                        })?;
                    chain.truncate(kept + 1);
                }
                let session = if arguments
                    .iter()
                    .any(|argument| argument == "--fork-session")
                {
                    let target = named_session
                        .clone()
                        .expect("a fork names its target session");
                    let copy = sessions.entry(target.clone()).or_default();
                    let mut parent = None;
                    for entry in &chain {
                        copy.append(entry.clone(), parent.replace(entry.clone()));
                    }
                    target
                } else {
                    resumed.to_owned()
                };
                (Some(session), chain.last().cloned())
            }
            None => (named_session, None),
        };
        let mut process = ModelledProcess {
            transcripts: peer_transcripts.clone(),
            session,
            tip,
        };
        let requests = requests.clone();
        let handler: PeerHandler = Box::new(move |frame: &Value| {
            if frame["request"]["subtype"] == "interrupt" {
                process.append(uuid::Uuid::new_v4().to_string());
                return Some(vec![
                    success(frame["request_id"].clone(), json!({})),
                    result_interrupted(),
                ]);
            }
            let uuid = user_uuid(frame)?;
            process.append(uuid.as_str().unwrap().to_owned());
            if frame["shouldQuery"] == false {
                return Some(vec![
                    lifecycle(&uuid, "queued"),
                    lifecycle(&uuid, "completed"),
                ]);
            }
            let request = requests.fetch_add(1, std::sync::atomic::Ordering::AcqRel);
            let message_id = format!("msg-{request}");
            let mut frames = vec![lifecycle(&uuid, "queued"), lifecycle(&uuid, "started")];
            let answer = text_message(&message_id, &["ok"]);
            if request == unanswered_request {
                // `message_start`, `content_block_start`, and the first delta only.
                frames.extend(answer.into_iter().take(3));
                return Some(frames);
            }
            let answer_uuid = answer
                .iter()
                .find(|frame| frame["type"] == "assistant")
                .and_then(|frame| frame["uuid"].as_str())
                .unwrap()
                .to_owned();
            process.append(answer_uuid);
            frames.extend(answer);
            frames.push(lifecycle(&uuid, "completed"));
            frames.push(result_success());
            Some(frames)
        });
        Ok(handler)
    });
    (launcher, transcripts)
}

/// Fork the conversation at a checkpoint into a separately owned target, as a session fork does.
async fn fork_checkpoint(
    provider: &ClaudeProvider,
    provider_session_id: forge_provider_api::ProviderSessionId,
    checkpoint: forge_provider_api::CheckpointId,
    workspace: &std::path::Path,
) -> Result<forge_provider_api::ProviderSessionId, ProviderError> {
    let source = session_request(workspace);
    provider
        .thread_fork(
            operation_context(),
            forge_provider_api::session::SessionForkRequest {
                source: forge_provider_api::ForkSource::Checkpoint {
                    provider_session_id,
                    checkpoint,
                },
                target: forge_provider_api::ForkTarget {
                    thread_id: forge_provider_api::testing::root_thread("claude-fork-owner"),
                    workspace: source.workspace,
                    configuration: source.configuration,
                    system_instructions: source.system_instructions,
                },
            },
        )
        .await
        .map(|forked| forked.provider_session_id)
}

/// A session whose stopped request was withdrawn forks at its last answered turn.
///
/// The withdrawal restarts the CLI at the answer before the request. The process it replaces
/// records the request's interrupt marker as the transcript's leaf checkpoint when it exits, and
/// the CLI keeps loading that abandoned branch while every later entry follows another one, so a
/// fork at a later answer was refused: "No message found with message.uuid of" that answer.
#[tokio::test]
async fn a_session_with_a_withdrawn_request_forks_at_its_last_answer() {
    let directory = tempfile::tempdir().unwrap();
    let (launcher, transcripts) = branching_transcript_peer(1);
    let (provider, mut opened) = open(launcher.clone(), directory.path()).await;
    let source = opened.binding.provider_session_id.clone().unwrap();
    let first = run_turn(&provider, &mut opened, "answered", false).await;
    run_turn(&provider, &mut opened, "stopped", true).await;
    let last = run_turn(&provider, &mut opened, "after the stop", false).await;
    let last_answer = last.result().checkpoint_id().cloned().unwrap();

    let forked = fork_checkpoint(&provider, source, last_answer.clone(), directory.path()).await;

    let target = forked.expect("the fork at the last answered turn was refused");
    let copied = transcripts.lock().unwrap()[target.as_str()].entries.clone();
    for answer in [first.result().checkpoint_id().unwrap(), &last_answer] {
        assert!(
            copied.iter().any(|entry| entry == answer.as_str()),
            "the fork lacks the answer {answer:?}: {copied:?}"
        );
    }
    provider.shutdown(operation_context()).await.unwrap();
}

/// A fork the CLI refuses reports the CLI's own reason, not only the failed initialization.
#[tokio::test]
async fn a_refused_fork_reports_the_cli_reason() {
    let directory = tempfile::tempdir().unwrap();
    let (launcher, _) = branching_transcript_peer(usize::MAX);
    let (provider, mut opened) = open(launcher.clone(), directory.path()).await;
    let source = opened.binding.provider_session_id.clone().unwrap();
    run_turn(&provider, &mut opened, "answered", false).await;
    let missing = forge_provider_api::CheckpointId::new("entry-the-cli-never-wrote").unwrap();

    let refused = fork_checkpoint(&provider, source, missing, directory.path())
        .await
        .expect_err("the CLI refuses a fork at an entry it never wrote");

    assert!(
        refused
            .to_string()
            .contains("No message found with message.uuid of: entry-the-cli-never-wrote"),
        "the fork failure omits the CLI's reason: {refused}"
    );
    provider.shutdown(operation_context()).await.unwrap();
}

/// MCP credentials reach the CLI through the session's private profile, never its command line.
#[tokio::test]
async fn mcp_credentials_stay_off_the_command_line() {
    let directory = tempfile::tempdir().unwrap();
    let launcher = PeerLauncher::new(|| Box::new(|_| None));
    let provider =
        ClaudeProvider::with_launcher(launcher.clone(), directory.path().join("runtime"));
    let mut request = session_request(directory.path());
    request
        .configuration
        .resources
        .mcp
        .push(forge_provider_api::McpLaunch {
            name: "remote".into(),
            transport: forge_provider_api::McpTransport::Http {
                url: "https://mcp.example.invalid".into(),
                headers: [("Authorization".to_owned(), "Bearer secret-token".to_owned())]
                    .into_iter()
                    .collect(),
            },
            timeout: None,
            working_directory: None,
            tools: None,
        });
    provider
        .thread_open(operation_context(), request)
        .await
        .unwrap();

    let launch = launcher.record.lock().unwrap().launches[0].clone();
    assert!(
        launch
            .iter()
            .all(|argument| !argument.contains("secret-token"))
    );
    let config = launch[launch
        .iter()
        .position(|argument| argument == "--mcp-config")
        .unwrap()
        + 1]
    .clone();
    let written = std::fs::read_to_string(config).unwrap();
    assert!(written.contains("secret-token"));
    provider.shutdown(operation_context()).await.unwrap();
}

/// A CLI that never answers an interrupt cannot leave the turn running forever.
#[tokio::test(start_paused = true)]
async fn unanswered_interrupt_ends_the_turn() {
    let directory = tempfile::tempdir().unwrap();
    let launcher = PeerLauncher::new(|| {
        Box::new(move |frame| {
            if let Some(uuid) = user_uuid(frame) {
                let mut frames = vec![lifecycle(&uuid, "queued"), lifecycle(&uuid, "started")];
                frames.extend(text_message("msg-1", &["partial"]));
                return Some(frames);
            }
            // The wedged CLI ignores the interrupt request entirely.
            (frame["request"]["subtype"] == "interrupt").then(Vec::new)
        })
    });
    let (provider, mut opened) = open(launcher.clone(), directory.path()).await;
    let binding = opened.binding.id.clone();
    let request = prompt_request(
        &binding,
        "turn-1",
        receipt("first"),
        forge_provider_api::permissions::PermissionResponse::Allow,
    );
    let scope = request.scope.clone();
    provider
        .turn_start(operation_context(), &binding, request)
        .await
        .unwrap();
    until_message(&mut opened.events).await;
    let interrupt = provider.turn_interrupt(
        operation_context(),
        &binding,
        forge_provider_api::CancelRequest {
            scope,
            input: forge_provider_api::operations::InterruptInput::Retain,
        },
    );
    let ended = async {
        loop {
            match opened.events.recv().await {
                Ok(Some(envelope)) => {
                    if let forge_provider_api::ProviderEvent::Turn {
                        event: forge_provider_api::TurnEvent::Completed(_),
                        ..
                    } = envelope.event
                    {
                        return "completed";
                    }
                }
                // Invalidation closes the binding's observation; Harness restarts the provider.
                Ok(None) | Err(_) => return "invalidated",
            }
        }
    };
    let (_, ended) = tokio::join!(interrupt, ended);

    assert_eq!(ended, "invalidated");
    let mut request = prompt_request(
        &binding,
        "turn-2",
        receipt("second"),
        forge_provider_api::permissions::PermissionResponse::Allow,
    );
    request.input = forge_provider_api::forge::TurnInput::user(
        forge_provider_api::input::PromptInput::Text("again".into()),
    );
    assert!(
        provider
            .turn_start(operation_context(), &binding, request)
            .await
            .is_err()
    );
    provider.shutdown(operation_context()).await.unwrap();
}

/// Answer the first `get_context_usage` request, the startup instruction query, and leave every
/// later one unanswered, as a CLI whose context accounting hangs does.
fn context_query_hangs_after_startup(prompt: PeerHandler) -> PeerHandler {
    let mut prompt = prompt;
    let mut answered = false;
    Box::new(move |frame| {
        if frame["request"]["subtype"] == "get_context_usage" {
            if std::mem::replace(&mut answered, true) {
                return Some(Vec::new());
            }
            return None;
        }
        prompt(frame)
    })
}

/// Count the `get_context_usage` requests Forge has written.
fn context_queries(launcher: &PeerLauncher) -> usize {
    launcher
        .frames("control_request")
        .iter()
        .filter(|frame| frame["request"]["subtype"] == "get_context_usage")
        .count()
}

/// A turn whose `result` arrived completes although the context-usage query after it never
/// returns.
///
/// The figure only decorates the status line, so the turn completes and classifies its reply as
/// the response without it. The outer bound fails the test instead of hanging it.
#[tokio::test(start_paused = true)]
async fn completed_turn_does_not_wait_for_an_unanswered_context_query() {
    let directory = tempfile::tempdir().unwrap();
    let launcher =
        PeerLauncher::new(|| context_query_hangs_after_startup(answering_peer(&["ready"])));
    let (provider, mut opened) = open(launcher.clone(), directory.path()).await;
    let binding = opened.binding.id.clone();
    start_prompt(&provider, &binding, "turn-1").await;
    let (events, completed) = tokio::time::timeout(
        std::time::Duration::from_secs(60),
        until_completed(&mut opened.events),
    )
    .await
    .expect("the turn completes without its context usage");

    assert_eq!(classifications(&events), ["Known(Response)"]);
    assert_eq!(
        completed.result().outcome(),
        forge_provider_api::turn::TurnOutcome::Completed {
            stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal
        }
    );
    assert_eq!(context_queries(&launcher), 2);
    provider.shutdown(operation_context()).await.unwrap();
}

/// An interrupt that arrives after the CLI's `result`, while the adapter still finishes the turn,
/// ends the interrupt operation promptly.
///
/// No `interrupt` request is needed, because the CLI already ended its turn; the interrupt only
/// abandons the context-usage query. The outer bound is shorter than that query's own deadline.
#[tokio::test(start_paused = true)]
async fn interrupt_after_the_result_abandons_the_context_query() {
    let directory = tempfile::tempdir().unwrap();
    let launcher = PeerLauncher::new(|| {
        context_query_hangs_after_startup(Box::new(|frame| {
            if frame["request"]["subtype"] == "interrupt" {
                return Some(Vec::new());
            }
            answering_peer(&["ready"])(frame)
        }))
    });
    let (provider, mut opened) = open(launcher.clone(), directory.path()).await;
    let binding = opened.binding.id.clone();
    let scope = start_prompt(&provider, &binding, "turn-1").await;
    while context_queries(&launcher) < 2 {
        tokio::time::sleep(std::time::Duration::from_millis(1)).await;
    }
    tokio::time::timeout(
        std::time::Duration::from_secs(1),
        provider.turn_interrupt(
            unbounded_context(),
            &binding,
            forge_provider_api::CancelRequest {
                scope,
                input: forge_provider_api::operations::InterruptInput::Retain,
            },
        ),
    )
    .await
    .expect("the interrupt ends before the context query's deadline")
    .unwrap();
    let (_, completed) = until_completed(&mut opened.events).await;

    assert_eq!(
        completed.result().outcome(),
        forge_provider_api::turn::TurnOutcome::Completed {
            stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal
        }
    );
    assert!(
        launcher
            .frames("control_request")
            .iter()
            .all(|frame| frame["request"]["subtype"] != "interrupt")
    );
    provider.shutdown(operation_context()).await.unwrap();
}

/// A steer the CLI never acknowledges cannot hold an interrupted turn open.
///
/// The CLI answers the interrupt and ends its turn, but the steer's acknowledgement wait keeps the
/// steering boundary that ends the turn. The interrupt bounds that wait by the interrupt grace
/// period, as it bounds a prompt's, and the steer fails because its consumption is unknown.
#[tokio::test(start_paused = true)]
async fn unacknowledged_steer_cannot_outlive_its_interrupt() {
    let directory = tempfile::tempdir().unwrap();
    let launcher = PeerLauncher::new(|| {
        let mut prompted = false;
        Box::new(move |frame| {
            if frame["request"]["subtype"] == "interrupt" {
                return Some(vec![
                    success(frame["request_id"].clone(), json!({})),
                    result_interrupted(),
                ]);
            }
            let uuid = user_uuid(frame)?;
            if std::mem::replace(&mut prompted, true) {
                // The wedged CLI never acknowledges the steer.
                return Some(Vec::new());
            }
            let mut frames = vec![lifecycle(&uuid, "queued"), lifecycle(&uuid, "started")];
            frames.extend(text_message("msg-1", &["partial"]));
            Some(frames)
        })
    });
    let (provider, mut opened) = open(launcher.clone(), directory.path()).await;
    let binding = opened.binding.id.clone();
    let scope = start_prompt(&provider, &binding, "turn-1").await;
    until_message(&mut opened.events).await;
    let steer = tokio::spawn({
        let provider = provider.clone();
        let binding = binding.clone();
        let scope = scope.clone();
        async move {
            provider
                .turn_steer(
                    unbounded_context(),
                    &binding,
                    journaled_steering(scope, "more", "steer"),
                )
                .await
        }
    });
    while launcher.frames("user").len() < 2 {
        tokio::time::sleep(std::time::Duration::from_millis(1)).await;
    }
    let interrupted = tokio::time::timeout(
        std::time::Duration::from_secs(60),
        provider.turn_interrupt(
            unbounded_context(),
            &binding,
            forge_provider_api::CancelRequest {
                scope,
                input: forge_provider_api::operations::InterruptInput::Retain,
            },
        ),
    )
    .await;

    assert!(interrupted.is_ok(), "the interrupt operation ended");
    assert!(steer.await.unwrap().is_err());
    provider.shutdown(operation_context()).await.unwrap();
}

/// Interrupting an input the CLI has queued but not started ends the turn without a `result`.
///
/// A CLI that is still starting, as after a resume or a fork, queues the first input. An interrupt
/// then cancels that input: the CLI reports `command_lifecycle cancelled`, lists the input in the
/// interrupt response, and prints no `result`, because no turn ever ran. The paused clock runs the
/// interrupt grace period out at once, so a binding that waited for a `result` is invalidated here
/// and refuses the second turn.
#[tokio::test(start_paused = true)]
async fn interrupting_a_queued_input_ends_the_turn_and_keeps_the_binding() {
    let directory = tempfile::tempdir().unwrap();
    let launcher = PeerLauncher::new(|| {
        let mut queued_input: Option<Value> = None;
        let mut cancelled = false;
        Box::new(move |frame| {
            if let Some(uuid) = user_uuid(frame) {
                if !cancelled {
                    queued_input = Some(uuid.clone());
                    return Some(vec![lifecycle(&uuid, "queued")]);
                }
                let mut frames = vec![lifecycle(&uuid, "queued"), lifecycle(&uuid, "started")];
                frames.extend(text_message("msg-1", &["answer"]));
                frames.push(result_success());
                frames.push(lifecycle(&uuid, "completed"));
                return Some(frames);
            }
            if frame["request"]["subtype"] == "interrupt" {
                cancelled = true;
                let uuid = queued_input
                    .clone()
                    .expect("the interrupt follows an input");
                return Some(vec![
                    lifecycle(&uuid, "cancelled"),
                    success(
                        frame["request_id"].clone(),
                        json!({"still_queued": [], "cancelled": [uuid]}),
                    ),
                ]);
            }
            None
        })
    });
    let (provider, mut opened) = open(launcher.clone(), directory.path()).await;
    let binding = opened.binding.id.clone();
    let queued_receipt = receipt("first");
    let request = prompt_request(
        &binding,
        "turn-1",
        queued_receipt.clone(),
        forge_provider_api::permissions::PermissionResponse::Allow,
    );
    let scope = request.scope.clone();
    // `turn_start` returns once the turn starts natively, which this input never does.
    let start = tokio::spawn({
        let provider = provider.clone();
        let binding = binding.clone();
        async move {
            provider
                .turn_start(operation_context(), &binding, request)
                .await
        }
    });
    while !queued_receipt.state.lock().unwrap().accepted {
        tokio::task::yield_now().await;
    }

    provider
        .turn_interrupt(
            operation_context(),
            &binding,
            forge_provider_api::CancelRequest {
                scope,
                input: forge_provider_api::operations::InterruptInput::Retain,
            },
        )
        .await
        .unwrap();
    assert!(
        matches!(start.await.unwrap(), Err(ProviderError::Cancelled)),
        "the cancelled input started no turn"
    );

    let mut request = prompt_request(
        &binding,
        "turn-2",
        receipt("second"),
        forge_provider_api::permissions::PermissionResponse::Allow,
    );
    request.input = forge_provider_api::forge::TurnInput::user(
        forge_provider_api::input::PromptInput::Text("again".into()),
    );
    provider
        .turn_start(operation_context(), &binding, request)
        .await
        .unwrap();
    let (events, completed) = until_completed(&mut opened.events).await;
    assert_eq!(streamed_text(&events), "answer");
    assert!(matches!(
        completed.result().outcome(),
        forge_provider_api::turn::TurnOutcome::Completed { .. }
    ));
    provider.shutdown(operation_context()).await.unwrap();
}

/// Interrupting a prompt that is still held before dispatch reports the turn collected.
///
/// The host holds every prompt before dispatch until its baseline checkpoint is captured, and the
/// adapter installs the turn's collector before that hold. A cancel that arrives during the hold
/// ends the turn as a pre-dispatch rejection: nothing was written to the CLI, so no native turn
/// exists and the interrupt has nothing left to stop. Reporting that rejection as an interrupt
/// failure made the host treat the stop as uncertain and restart a healthy provider. The second
/// prompt covers the journal the host has already retired, which refuses the adapter's rejection
/// record.
#[tokio::test]
async fn interrupting_a_prompt_held_before_dispatch_reports_it_collected() {
    let directory = tempfile::tempdir().unwrap();
    let launcher = PeerLauncher::new(|| answering_peer(&["answer"]));
    let (provider, mut opened) = open(launcher.clone(), directory.path()).await;
    let binding = opened.binding.id.clone();
    for (turn, retired) in [("turn-1", false), ("turn-2", true)] {
        let held_receipt = receipt(turn);
        held_receipt.state.lock().unwrap().retired = retired;
        let control = Arc::new(HeldTurnControl::default());
        let mut request = prompt_request(
            &binding,
            turn,
            held_receipt,
            forge_provider_api::permissions::PermissionResponse::Allow,
        );
        request.turn_control = control.clone();
        let scope = request.scope.clone();
        let start = tokio::spawn({
            let provider = provider.clone();
            let binding = binding.clone();
            async move {
                provider
                    .turn_start(operation_context(), &binding, request)
                    .await
            }
        });
        control.entered.notified().await;

        let interrupted = provider
            .turn_interrupt(
                operation_context(),
                &binding,
                forge_provider_api::CancelRequest {
                    scope,
                    input: forge_provider_api::operations::InterruptInput::Retain,
                },
            )
            .await;
        assert!(
            interrupted.is_ok(),
            "{turn}: interrupting an undispatched prompt failed: {interrupted:?}"
        );
        assert!(
            start.await.unwrap().is_err(),
            "{turn}: the cancelled prompt reported acceptance"
        );
        assert!(
            launcher.frames("user").is_empty(),
            "{turn}: the cancelled prompt reached the CLI"
        );
    }

    let mut request = prompt_request(
        &binding,
        "turn-3",
        receipt("third"),
        forge_provider_api::permissions::PermissionResponse::Allow,
    );
    request.input = forge_provider_api::forge::TurnInput::user(
        forge_provider_api::input::PromptInput::Text("again".into()),
    );
    provider
        .turn_start(operation_context(), &binding, request)
        .await
        .unwrap();
    let (events, completed) = until_completed(&mut opened.events).await;
    assert_eq!(streamed_text(&events), "answer");
    assert!(matches!(
        completed.result().outcome(),
        forge_provider_api::turn::TurnOutcome::Completed { .. }
    ));
    provider.shutdown(operation_context()).await.unwrap();
}

/// A CLI that never acknowledges an input cannot keep that input's interrupt waiting forever.
///
/// A frozen CLI receives the prompt and reports nothing, so the prompt worker is still waiting for
/// the CLI to acknowledge the prompt when the interrupt arrives. The interrupt bounds that wait by
/// the interrupt grace period and then invalidates the binding, as it does for a CLI that ignores
/// an interrupt during a turn. The paused clock runs both that period and this test's outer bound
/// out at once.
#[tokio::test(start_paused = true)]
async fn unacknowledged_input_cannot_outlive_its_interrupt() {
    let directory = tempfile::tempdir().unwrap();
    let launcher = PeerLauncher::new(|| {
        Box::new(move |frame| {
            // The frozen CLI reads the prompt and the interrupt and answers neither.
            (user_uuid(frame).is_some() || frame["request"]["subtype"] == "interrupt")
                .then(Vec::new)
        })
    });
    let (provider, _opened) = open(launcher.clone(), directory.path()).await;
    let binding = _opened.binding.id.clone();
    let request = prompt_request(
        &binding,
        "turn-1",
        receipt("first"),
        forge_provider_api::permissions::PermissionResponse::Allow,
    );
    let scope = request.scope.clone();
    // Harness gives a turn start no deadline, so only the interrupt can end this wait.
    let (_, start_cancellation) = forge_provider_api::OperationCancellation::channel();
    let start_context = forge_provider_api::OperationContext::unbounded(
        forge_provider_api::OperationId::new(uuid::Uuid::new_v4().to_string()).unwrap(),
        start_cancellation,
    );
    let start = tokio::spawn({
        let provider = provider.clone();
        let binding = binding.clone();
        async move { provider.turn_start(start_context, &binding, request).await }
    });
    while launcher.frames("user").is_empty() {
        tokio::task::yield_now().await;
    }

    let interrupt = provider.turn_interrupt(
        operation_context(),
        &binding,
        forge_provider_api::CancelRequest {
            scope,
            input: forge_provider_api::operations::InterruptInput::Retain,
        },
    );
    let started = tokio::time::timeout(std::time::Duration::from_secs(120), async {
        let _ = interrupt.await;
        start.await.unwrap()
    })
    .await
    .expect("the unacknowledged prompt outlived its interrupt");
    assert!(
        started.is_err(),
        "a prompt the CLI never acknowledged started"
    );

    let mut request = prompt_request(
        &binding,
        "turn-2",
        receipt("second"),
        forge_provider_api::permissions::PermissionResponse::Allow,
    );
    request.input = forge_provider_api::forge::TurnInput::user(
        forge_provider_api::input::PromptInput::Text("again".into()),
    );
    assert!(
        provider
            .turn_start(operation_context(), &binding, request)
            .await
            .is_err(),
        "the binding of a frozen CLI accepted another turn"
    );
    provider.shutdown(operation_context()).await.unwrap();
}

/// Launch peers as a CLI that holds no transcript, so every resume exits before its handshake.
///
/// The real CLI writes a conversation when its first turn runs. Resuming a session id that has
/// only ever launched prints "No conversation found" and exits.
struct TranscriptlessLauncher {
    peers: Arc<PeerLauncher>,
}

#[async_trait]
impl ClaudeLauncher for TranscriptlessLauncher {
    fn available(&self) -> bool {
        self.peers.available()
    }

    fn executable(&self) -> std::path::PathBuf {
        self.peers.executable()
    }

    async fn version(
        &self,
        context: &forge_provider_api::OperationContext,
    ) -> Result<String, ProviderError> {
        self.peers.version(context).await
    }

    fn launch(&self, spec: &LaunchSpec) -> Result<super::transport::ProcessIo, ProviderError> {
        if !matches!(spec.start, SessionStart::Resume { .. }) {
            return self.peers.launch(spec);
        }
        self.peers
            .record
            .lock()
            .unwrap()
            .launches
            .push(spec.arguments());
        Ok(super::transport::ProcessIo {
            stdout: Box::new(tokio::io::empty()),
            stdin: Box::new(tokio::io::sink()),
            process: None,
        })
    }
}

/// Expose a fixture transcript directory through the same boundary as the installed CLI.
struct RecoveryLauncher {
    peer: Arc<PeerLauncher>,
    projects: std::path::PathBuf,
}

#[async_trait]
impl ClaudeLauncher for RecoveryLauncher {
    fn available(&self) -> bool {
        self.peer.available()
    }

    fn executable(&self) -> std::path::PathBuf {
        self.peer.executable()
    }

    async fn version(
        &self,
        context: &forge_provider_api::OperationContext,
    ) -> Result<String, ProviderError> {
        self.peer.version(context).await
    }

    fn transcript_projects(&self) -> Option<std::path::PathBuf> {
        Some(self.projects.clone())
    }

    fn launch(&self, spec: &LaunchSpec) -> Result<super::transport::ProcessIo, ProviderError> {
        self.peer.launch(spec)
    }
}

/// Finish durable recovery before publishing the reopened binding or admitting a new prompt.
#[tokio::test]
async fn reopened_binding_repairs_a_pending_stop_before_admission() -> anyhow::Result<()> {
    use super::recovery::{RepairIntent, RepairJournal};
    let root = tempfile::tempdir()?;
    let runtime = root.path().join("runtime");
    let projects = root.path().join("projects");
    let workspace = projects.join("workspace");
    std::fs::create_dir_all(&workspace)?;
    let session = uuid::Uuid::new_v4();
    let retained = uuid::Uuid::new_v4();
    let stopped = uuid::Uuid::new_v4();
    let transcript = workspace.join(format!("{session}.jsonl"));
    let saved = [
        json!({"type":"assistant","uuid":retained,"parentUuid":null}),
        json!({"type":"user","uuid":stopped,"parentUuid":retained}),
        json!({"type":"last-prompt","leafUuid":stopped}),
    ]
    .iter()
    .map(ToString::to_string)
    .collect::<Vec<_>>()
    .join("\n");
    std::fs::write(&transcript, format!("{saved}\n"))?;
    let journal = RepairJournal::new(&runtime, projects.clone(), &session.to_string()).unwrap();
    let mut request = session_request(root.path());
    journal.save(&RepairIntent {
        scope: forge_provider_api::InvocationScope::new(request.thread_id.clone(), "cancelled"),
        inputs: vec![stopped],
        retained: Some(retained),
        unanswered: true,
    })?;
    let native = PeerLauncher::new(move || {
        let mut answer = answering_peer(&["ready"]);
        let transcript = transcript.clone();
        let saved = saved.clone();
        Box::new(move |frame| {
            if frame["request"]["subtype"] == "rewind_conversation" {
                assert_eq!(frame["request"]["target_message_uuid"], stopped.to_string());
                let marker = json!({"type":"last-prompt","leafUuid":retained,"rewound":true,"cutUuid":stopped});
                std::fs::write(&transcript, format!("{saved}\n{marker}\n")).unwrap();
                return Some(vec![success(
                    frame["request_id"].clone(),
                    json!({
                        "rewound":true,"targetMessageUuid":stopped,
                    }),
                )]);
            }
            answer(frame)
        })
    });
    let provider = ClaudeProvider::with_launcher(
        Arc::new(RecoveryLauncher {
            peer: native,
            projects,
        }),
        runtime,
    );
    request.history = forge_provider_api::SessionHistory::Durable {
        resume: Some(session.to_string().try_into()?),
    };
    let mut opened = provider.thread_open(operation_context(), request).await?;
    assert!(journal.load()?.is_none());
    let answer = run_turn(&provider, &mut opened, "next", false).await;
    assert!(matches!(
        answer.result().outcome(),
        forge_provider_api::turn::TurnOutcome::Completed { .. }
    ));
    provider.shutdown(operation_context()).await?;
    Ok(())
}

/// Name how each recorded launch opened its conversation.
fn session_starts(launcher: &PeerLauncher) -> Vec<String> {
    launcher
        .record
        .lock()
        .unwrap()
        .launches
        .iter()
        .flat_map(|launch| {
            launch
                .iter()
                .filter(|argument| {
                    argument.starts_with("--session-id=") || argument.starts_with("--resume=")
                })
                .cloned()
        })
        .collect()
}

/// Harness restarts a provider by reopening the session under the id the first open reported. A
/// CLI replaced before its first turn ran holds no conversation under that id, so the reopen
/// starts the conversation again instead of failing every later prompt.
#[tokio::test]
async fn reopening_a_conversation_the_cli_never_ran_starts_it_again() {
    let directory = tempfile::tempdir().unwrap();
    let peers = PeerLauncher::new(|| answering_peer(&["hel", "lo"]));
    let provider = ClaudeProvider::with_launcher(
        Arc::new(TranscriptlessLauncher {
            peers: peers.clone(),
        }),
        directory.path().join("runtime"),
    );
    let first = provider
        .thread_open(operation_context(), session_request(directory.path()))
        .await
        .unwrap();
    let session_id = first.binding.provider_session_id.clone().unwrap();

    let mut request = session_request(directory.path());
    request.history = forge_provider_api::SessionHistory::Durable {
        resume: Some(session_id.clone()),
    };
    let mut reopened = provider
        .thread_open(operation_context(), request)
        .await
        .expect("the conversation the CLI never ran reopens");

    assert_eq!(
        reopened.binding.provider_session_id.as_ref(),
        Some(&session_id)
    );
    assert_eq!(
        session_starts(&peers),
        [
            format!("--session-id={}", session_id.as_str()),
            format!("--resume={}", session_id.as_str()),
            format!("--session-id={}", session_id.as_str()),
        ]
    );
    provider
        .turn_start(
            operation_context(),
            &reopened.binding.id,
            prompt_request(
                &reopened.binding.id,
                "turn-1",
                receipt("first"),
                forge_provider_api::permissions::PermissionResponse::Allow,
            ),
        )
        .await
        .unwrap();
    let (events, _) = until_completed(&mut reopened.events).await;
    assert_eq!(streamed_text(&events), "hello");
    provider.shutdown(operation_context()).await.unwrap();
}

/// A resource change restarts the process on the same session. Before the first turn the CLI
/// holds no conversation to resume, and the restart must still keep the binding.
#[tokio::test]
async fn a_resource_change_before_the_first_turn_keeps_the_binding() {
    let directory = tempfile::tempdir().unwrap();
    let peers = PeerLauncher::new(|| answering_peer(&["hel", "lo"]));
    let provider = ClaudeProvider::with_launcher(
        Arc::new(TranscriptlessLauncher {
            peers: peers.clone(),
        }),
        directory.path().join("runtime"),
    );
    let mut opened = provider
        .thread_open(operation_context(), session_request(directory.path()))
        .await
        .unwrap();
    let mut configuration = session_request(directory.path()).configuration;
    configuration
        .resources
        .mcp
        .push(forge_provider_api::McpLaunch {
            name: "remote".into(),
            transport: forge_provider_api::McpTransport::Http {
                url: "https://mcp.example.invalid".into(),
                headers: Default::default(),
            },
            timeout: None,
            working_directory: None,
            tools: None,
        });

    provider
        .thread_configure(operation_context(), &opened.binding.id, configuration)
        .await
        .expect("the resource change restarts a conversation the CLI never ran");

    provider
        .turn_start(
            operation_context(),
            &opened.binding.id,
            prompt_request(
                &opened.binding.id,
                "turn-1",
                receipt("first"),
                forge_provider_api::permissions::PermissionResponse::Allow,
            ),
        )
        .await
        .unwrap();
    let (events, _) = until_completed(&mut opened.events).await;
    assert_eq!(streamed_text(&events), "hello");
    provider.shutdown(operation_context()).await.unwrap();
}

/// A conversation whose turn ran is on disk, so a failed resume of it is a real failure: starting
/// it again under the same id would replace the history Harness still shows.
#[tokio::test]
async fn a_conversation_that_ran_a_turn_is_never_started_again() {
    let directory = tempfile::tempdir().unwrap();
    let peers = PeerLauncher::new(|| answering_peer(&["hel", "lo"]));
    let provider = ClaudeProvider::with_launcher(
        Arc::new(TranscriptlessLauncher {
            peers: peers.clone(),
        }),
        directory.path().join("runtime"),
    );
    let mut opened = provider
        .thread_open(operation_context(), session_request(directory.path()))
        .await
        .unwrap();
    let session_id = opened.binding.provider_session_id.clone().unwrap();
    provider
        .turn_start(
            operation_context(),
            &opened.binding.id,
            prompt_request(
                &opened.binding.id,
                "turn-1",
                receipt("first"),
                forge_provider_api::permissions::PermissionResponse::Allow,
            ),
        )
        .await
        .unwrap();
    until_completed(&mut opened.events).await;

    let mut request = session_request(directory.path());
    request.history = forge_provider_api::SessionHistory::Durable {
        resume: Some(session_id.clone()),
    };
    let reopened = provider.thread_open(operation_context(), request).await;

    assert!(matches!(
        reopened,
        Err(ProviderError::Failure(failure))
            if failure.kind() == forge_provider_api::ProviderFailureKind::Initialization
    ));
    assert_eq!(
        session_starts(&peers),
        [
            format!("--session-id={}", session_id.as_str()),
            format!("--resume={}", session_id.as_str()),
        ]
    );
    provider.shutdown(operation_context()).await.unwrap();
}

/// A manual compaction's notifications and result carry the host operation identity.
///
/// The adapter once named the attempt after the `/compact` input's own uuid, so the host
/// settled its own record from the result and kept the notifications as a second compaction:
/// the transcript drew `Context compacted` twice for one `/compact`.
#[tokio::test]
async fn manual_compaction_reports_the_host_operation_identity() {
    let directory = tempfile::tempdir().unwrap();
    let launcher = scripted_peer(vec![|_| {
        let system = |fields: Value| {
            let mut frame = json!({"type": "system", "uuid": uuid::Uuid::new_v4().to_string()});
            frame
                .as_object_mut()
                .unwrap()
                .extend(fields.as_object().unwrap().clone());
            frame
        };
        vec![
            system(json!({"subtype": "status", "status": "compacting"})),
            system(json!({"subtype": "compact_boundary", "compact_metadata":
                {"trigger": "manual", "pre_tokens": 62121, "post_tokens": 13787}})),
            system(json!({"subtype": "status", "status": null, "compact_result": "success"})),
            json!({"type": "result", "subtype": "success", "is_error": false,
                "local_command": "compact", "uuid": uuid::Uuid::new_v4().to_string()}),
        ]
    }]);
    let (provider, mut opened) = open(launcher, directory.path()).await;
    let context = operation_context();
    let operation_id = context.id().clone();
    let request = forge_provider_api::CompactionRequest {
        scope: forge_provider_api::EventScope::SessionOperation {
            thread_id: forge_provider_api::testing::root_thread("claude-owner"),
            operation_id: operation_id.clone(),
        },
    };
    let compacted = provider
        .thread_compact(context, &opened.binding.id, request)
        .await
        .unwrap()
        .compaction;
    assert_eq!(compacted.id, operation_id.as_str());
    assert_eq!(compacted.result.context_tokens_after, Some(13787));
    let mut observed = Vec::new();
    while let Some(envelope) = opened.events.try_next().unwrap() {
        if let forge_provider_api::ProviderEvent::Thread {
            event: forge_provider_api::ThreadEvent::CompactionUpdated(compaction),
            ..
        } = envelope.event
        {
            observed.push(compaction.id.clone());
        }
    }
    assert!(!observed.is_empty());
    assert!(
        observed.iter().all(|id| id == operation_id.as_str()),
        "{observed:?}"
    );
    provider.shutdown(operation_context()).await.unwrap();
}

/// A `/compact` the CLI declines keeps the CLI's own words as the explanation.
///
/// With too little conversation the CLI answers "Not enough messages to compact." through a
/// failed compaction status, whose `compact_error` it sends only in some sessions, and through
/// the command's result text. The adapter once kept neither, so the transcript read
/// `Context compaction failed` and the notice `context compaction failed (Unknown)`.
#[tokio::test]
async fn declined_compaction_keeps_the_cli_explanation() {
    let directory = tempfile::tempdir().unwrap();
    fn declined(compact_error: Option<&str>) -> Vec<Value> {
        let mut status = json!({"type": "system", "subtype": "status", "status": null,
            "compact_result": "failed", "uuid": uuid::Uuid::new_v4().to_string()});
        if let Some(compact_error) = compact_error {
            status["compact_error"] = json!(compact_error);
        }
        vec![
            status,
            json!({"type": "result", "subtype": "success", "is_error": false,
                "result": "Not enough messages to compact.", "local_command": "compact",
                "uuid": uuid::Uuid::new_v4().to_string()}),
        ]
    }
    let launcher = scripted_peer(vec![
        |_| declined(Some("Not enough messages to compact.")),
        |_| declined(None),
    ]);
    let (provider, opened) = open(launcher, directory.path()).await;
    for _ in 0..2 {
        let context = operation_context();
        let request = forge_provider_api::CompactionRequest {
            scope: forge_provider_api::EventScope::SessionOperation {
                thread_id: forge_provider_api::testing::root_thread("claude-owner"),
                operation_id: context.id().clone(),
            },
        };
        let compaction = provider
            .thread_compact(context, &opened.binding.id, request)
            .await
            .unwrap()
            .compaction;
        assert_eq!(
            compaction.state,
            forge_provider_api::CompactionState::Failed
        );
        assert_eq!(
            compaction
                .result
                .provider_explanation
                .as_ref()
                .map(forge_provider_api::SanitizedMessage::as_str),
            Some("Not enough messages to compact.")
        );
    }
    provider.shutdown(operation_context()).await.unwrap();
}

/// Produces one user frame's replies from every input uuid written so far.
type ScriptedReply = fn(&[Value]) -> Vec<Value>;

/// Answer the nth user frame with `script[n]`, given every input uuid written so far, and end an
/// interrupted turn the way the CLI does.
fn scripted_peer(script: Vec<ScriptedReply>) -> Arc<PeerLauncher> {
    let script = Arc::new(script);
    PeerLauncher::new(move || {
        let script = script.clone();
        let mut uuids = Vec::new();
        Box::new(move |frame| {
            if frame["request"]["subtype"] == "interrupt" {
                return Some(vec![
                    success(frame["request_id"].clone(), json!({})),
                    result_interrupted(),
                ]);
            }
            uuids.push(user_uuid(frame)?);
            Some(
                script
                    .get(uuids.len() - 1)
                    .map_or_else(Vec::new, |step| step(&uuids)),
            )
        })
    })
}

/// Steer under a journaled receipt, the delivery whose consumption Harness waits for.
fn journaled_steering(
    scope: forge_provider_api::InvocationScope,
    text: &str,
    id: &str,
) -> forge_provider_api::session::SteeringRequest {
    forge_provider_api::session::SteeringRequest {
        dispatch: None,
        delivery: forge_provider_api::session::SteeringDelivery::Journaled {
            receipt: receipt(id),
        },
        ..steering_request(scope, text)
    }
}

/// Summarize events as streamed text and consumed user inputs, in arrival order.
fn consumption_trace(events: &[forge_provider_api::ProviderEvent]) -> Vec<String> {
    use forge_provider_api::message::{ItemCompletion, ItemEvent, ItemStart, MessageItemEvent};
    events
        .iter()
        .filter_map(|event| match event {
            forge_provider_api::ProviderEvent::Item { event, .. } => match event {
                ItemEvent::Started(ItemStart::UserMessage(start)) => Some(format!(
                    "user {} {}",
                    start.text,
                    start
                        .submission_id
                        .as_ref()
                        .map_or("-", SubmissionId::as_str)
                )),
                ItemEvent::Completed(ItemCompletion::UserMessage(_)) => Some("user done".into()),
                ItemEvent::Started(ItemStart::AgentMessage(start)) if !start.text.is_empty() => {
                    Some(start.text.clone())
                }
                ItemEvent::Message {
                    event: MessageItemEvent::Delta(text),
                } => Some(text.clone()),
                _ => None,
            },
            _ => None,
        })
        .collect()
}

async fn start_prompt(
    provider: &ClaudeProvider,
    binding: &forge_provider_api::BindingId,
    turn: &str,
) -> forge_provider_api::InvocationScope {
    let request = prompt_request(
        binding,
        turn,
        receipt(turn),
        forge_provider_api::permissions::PermissionResponse::Allow,
    );
    let scope = request.scope.clone();
    provider
        .turn_start(operation_context(), binding, request)
        .await
        .unwrap();
    scope
}

/// Steering folded into the running CLI turn reports consumption when the CLI starts it, which
/// the pinned 2.1.317 CLI does after the tool result and before the model request that reads it.
#[tokio::test]
async fn folded_steer_reports_consumption_at_its_start() {
    let directory = tempfile::tempdir().unwrap();
    let launcher = scripted_peer(vec![
        |uuids| {
            let mut frames = vec![
                lifecycle(&uuids[0], "queued"),
                lifecycle(&uuids[0], "started"),
            ];
            frames.extend(text_message("msg-1", &["first"]));
            frames
        },
        |uuids| {
            let mut frames = vec![
                lifecycle(&uuids[1], "queued"),
                lifecycle(&uuids[1], "started"),
            ];
            frames.extend(text_message("msg-2", &[" second"]));
            frames.extend([
                lifecycle(&uuids[1], "completed"),
                lifecycle(&uuids[0], "completed"),
                result_success(),
            ]);
            frames
        },
    ]);
    let (provider, mut opened) = open(launcher, directory.path()).await;
    let binding = opened.binding.id.clone();
    let scope = start_prompt(&provider, &binding, "turn-1").await;
    until_message(&mut opened.events).await;
    provider
        .turn_steer(
            operation_context(),
            &binding,
            journaled_steering(scope, "more", "steer"),
        )
        .await
        .unwrap();
    let (events, _) = until_completed(&mut opened.events).await;
    assert_eq!(
        consumption_trace(&events),
        ["user more steer", "user done", " second"]
    );
    provider.shutdown(operation_context()).await.unwrap();
}

/// A steer queued across the CLI's result starts a follow-up CLI turn inside the same Forge turn,
/// and its consumption report precedes that turn's output.
#[tokio::test]
async fn deferred_steer_reports_consumption_before_the_follow_up_turn() {
    let directory = tempfile::tempdir().unwrap();
    let launcher = scripted_peer(vec![
        |uuids| {
            let mut frames = vec![
                lifecycle(&uuids[0], "queued"),
                lifecycle(&uuids[0], "started"),
            ];
            frames.extend(text_message("msg-1", &["first"]));
            frames
        },
        |uuids| {
            let mut frames = vec![
                lifecycle(&uuids[1], "queued"),
                lifecycle(&uuids[0], "completed"),
                result_success(),
                lifecycle(&uuids[1], "started"),
            ];
            frames.extend(text_message("msg-2", &[" second"]));
            frames.extend([lifecycle(&uuids[1], "completed"), result_success()]);
            frames
        },
    ]);
    let (provider, mut opened) = open(launcher, directory.path()).await;
    let binding = opened.binding.id.clone();
    let scope = start_prompt(&provider, &binding, "turn-1").await;
    until_message(&mut opened.events).await;
    provider
        .turn_steer(
            operation_context(),
            &binding,
            journaled_steering(scope, "more", "steer"),
        )
        .await
        .unwrap();
    let (events, completed) = until_completed(&mut opened.events).await;
    assert_eq!(
        consumption_trace(&events),
        ["user more steer", "user done", " second"]
    );
    assert!(matches!(
        completed.result().outcome(),
        forge_provider_api::turn::TurnOutcome::Completed { .. }
    ));
    provider.shutdown(operation_context()).await.unwrap();
}

/// Only a journaled steer the CLI starts reports consumption, and only once: the prompt, context
/// input, a cancelled steer, and a steer that completes without starting report nothing.
#[tokio::test]
async fn only_started_journaled_steers_report_consumption() {
    let directory = tempfile::tempdir().unwrap();
    let launcher = scripted_peer(vec![
        |uuids| {
            let mut frames = vec![
                lifecycle(&uuids[0], "queued"),
                lifecycle(&uuids[0], "started"),
            ];
            frames.extend(text_message("msg-1", &["first"]));
            frames
        },
        |uuids| {
            vec![
                lifecycle(&uuids[1], "queued"),
                lifecycle(&uuids[1], "started"),
            ]
        },
        |uuids| {
            vec![
                lifecycle(&uuids[2], "queued"),
                lifecycle(&uuids[2], "cancelled"),
            ]
        },
        |uuids| {
            vec![
                lifecycle(&uuids[3], "queued"),
                lifecycle(&uuids[3], "completed"),
            ]
        },
        |uuids| {
            let mut frames = vec![
                lifecycle(&uuids[4], "queued"),
                lifecycle(&uuids[4], "started"),
                lifecycle(&uuids[4], "started"),
            ];
            frames.extend(text_message("msg-2", &[" second"]));
            frames.extend([
                lifecycle(&uuids[1], "completed"),
                lifecycle(&uuids[4], "completed"),
                lifecycle(&uuids[0], "completed"),
                result_success(),
            ]);
            frames
        },
    ]);
    let (provider, mut opened) = open(launcher, directory.path()).await;
    let binding = opened.binding.id.clone();
    let scope = start_prompt(&provider, &binding, "turn-1").await;
    until_message(&mut opened.events).await;
    for request in [
        steering_request(scope.clone(), "context"),
        journaled_steering(scope.clone(), "cancelled", "cancelled"),
        journaled_steering(scope.clone(), "skipped", "skipped"),
        journaled_steering(scope, "twice", "twice"),
    ] {
        provider
            .turn_steer(operation_context(), &binding, request)
            .await
            .unwrap();
    }
    let (events, _) = until_completed(&mut opened.events).await;
    assert_eq!(
        consumption_trace(&events),
        ["user twice twice", "user done", " second"]
    );
    provider.shutdown(operation_context()).await.unwrap();
}

/// A steer its interrupted invocation never started cannot report into the next invocation.
#[tokio::test]
async fn stale_steer_start_reports_nothing_to_a_successor() {
    let directory = tempfile::tempdir().unwrap();
    let launcher = scripted_peer(vec![
        |uuids| {
            let mut frames = vec![
                lifecycle(&uuids[0], "queued"),
                lifecycle(&uuids[0], "started"),
            ];
            frames.extend(text_message("msg-1", &["partial"]));
            frames
        },
        |uuids| vec![lifecycle(&uuids[1], "queued")],
        |uuids| {
            let mut frames = vec![
                lifecycle(&uuids[1], "started"),
                lifecycle(&uuids[2], "queued"),
                lifecycle(&uuids[2], "started"),
            ];
            frames.extend(text_message("msg-3", &["next"]));
            frames.extend([lifecycle(&uuids[2], "completed"), result_success()]);
            frames
        },
    ]);
    let (provider, mut opened) = open(launcher, directory.path()).await;
    let binding = opened.binding.id.clone();
    let scope = start_prompt(&provider, &binding, "turn-1").await;
    until_message(&mut opened.events).await;
    provider
        .turn_steer(
            operation_context(),
            &binding,
            journaled_steering(scope.clone(), "late", "late"),
        )
        .await
        .unwrap();
    provider
        .turn_interrupt(
            operation_context(),
            &binding,
            forge_provider_api::CancelRequest {
                scope,
                input: forge_provider_api::operations::InterruptInput::Retain,
            },
        )
        .await
        .unwrap();
    let (first, completed) = until_completed(&mut opened.events).await;
    assert_eq!(
        completed.result().outcome(),
        forge_provider_api::turn::TurnOutcome::Interrupted
    );
    assert!(
        consumption_trace(&first)
            .iter()
            .all(|entry| !entry.starts_with("user"))
    );
    start_prompt(&provider, &binding, "turn-2").await;
    let (second, _) = until_completed(&mut opened.events).await;
    assert_eq!(consumption_trace(&second), ["next"]);
    provider.shutdown(operation_context()).await.unwrap();
}

/// How the modelled CLI of `stopped_steer_peer` answers one prompt.
#[derive(Clone, Copy)]
enum PromptReply {
    /// Answer and end the CLI turn.
    Answered,
    /// Answer, then hold the CLI turn's result until a steer arrives. The steer stays queued
    /// across that result and starts a follow-up CLI turn, which no answer ever ends.
    AnsweredAwaitingSteer,
    /// Start the prompt and answer nothing, as a model request that a stop ends first.
    Started,
}

/// Build the notice the modelled CLI sends once it started an input that it will not answer,
/// which tells a test that the adapter has read every frame before it.
fn retry_notice() -> Value {
    json!({"type": "system", "subtype": "api_retry", "attempt": 1, "max_retries": 3,
        "error": "overloaded"})
}

/// Collect events until the modelled CLI's retry notice arrives.
async fn until_notice(
    stream: &mut forge_provider_api::ProviderEventStream,
) -> Vec<forge_provider_api::ProviderEvent> {
    let mut events = Vec::new();
    loop {
        let envelope = stream.recv().await.unwrap().unwrap();
        if matches!(
            envelope.event,
            forge_provider_api::ProviderEvent::Provider {
                event: forge_provider_api::ProviderConnectionEvent::Warning(_),
            }
        ) {
            return events;
        }
        events.push(envelope.event);
    }
}

/// Model a CLI that records every started input as a user entry, answers prompts by `script`,
/// records an interrupt as a user entry, and refuses a guarded resume as the real CLI does.
///
/// The CLI writes a steer it runs after a result as its own prompt entry, and its
/// `[Request interrupted by user]` marker as a user entry that queries nothing. Each model
/// request logs the prompts of the chain it reads, the requesting prompt last.
fn stopped_steer_peer(
    script: &'static [PromptReply],
) -> (
    Arc<PeerLauncher>,
    Arc<Mutex<Vec<NativeEntry>>>,
    ModelRequestLog,
) {
    let chain: Arc<Mutex<Vec<NativeEntry>>> = Arc::default();
    let model_requests: ModelRequestLog = Arc::default();
    // The script position survives the restarts that rewinds and withdrawals cause.
    let answered_prompts = Arc::new(std::sync::atomic::AtomicUsize::new(0));
    let peer_chain = chain.clone();
    let peer_requests = model_requests.clone();
    let launcher = PeerLauncher::with_arguments(move |arguments| {
        if !resume_native_chain(&mut peer_chain.lock().unwrap(), arguments) {
            return None;
        }
        let chain = peer_chain.clone();
        let requests = peer_requests.clone();
        let answered_prompts = answered_prompts.clone();
        let mut held_prompt: Option<Value> = None;
        let handler: PeerHandler = Box::new(move |frame: &Value| {
            if frame["request"]["subtype"] == "interrupt" {
                chain
                    .lock()
                    .unwrap()
                    .push(NativeEntry::Note(uuid::Uuid::new_v4().to_string()));
                let mut interrupted = result_interrupted();
                interrupted["duration_ms"] = json!(1);
                return Some(vec![
                    success(frame["request_id"].clone(), json!({})),
                    interrupted,
                ]);
            }
            let uuid = user_uuid(frame)?;
            let entry_uuid = uuid.as_str().unwrap().to_owned();
            let mut chain = chain.lock().unwrap();
            if frame["shouldQuery"] == false {
                chain.push(NativeEntry::Note(entry_uuid));
                return Some(vec![
                    lifecycle(&uuid, "queued"),
                    lifecycle(&uuid, "completed"),
                ]);
            }
            chain.push(NativeEntry::Prompt(entry_uuid));
            let mut requests = requests.lock().unwrap();
            requests.push(
                chain
                    .iter()
                    .filter_map(|entry| match entry {
                        NativeEntry::Prompt(prompt) => Some(prompt.clone()),
                        _ => None,
                    })
                    .collect(),
            );
            if let Some(held) = held_prompt.take() {
                return Some(vec![
                    lifecycle(&uuid, "queued"),
                    lifecycle(&held, "completed"),
                    result_success(),
                    lifecycle(&uuid, "started"),
                    retry_notice(),
                ]);
            }
            let reply = script[answered_prompts.fetch_add(1, std::sync::atomic::Ordering::SeqCst)];
            let mut frames = vec![lifecycle(&uuid, "queued"), lifecycle(&uuid, "started")];
            if matches!(reply, PromptReply::Started) {
                frames.push(retry_notice());
                return Some(frames);
            }
            let answer = text_message(&format!("msg-{}", requests.len()), &["ok"]);
            let answer_uuid = answer
                .iter()
                .find(|frame| frame["type"] == "assistant")
                .and_then(|frame| frame["uuid"].as_str())
                .unwrap()
                .to_owned();
            chain.push(NativeEntry::Answer(answer_uuid));
            frames.extend(answer);
            if matches!(reply, PromptReply::AnsweredAwaitingSteer) {
                held_prompt = Some(uuid);
                return Some(frames);
            }
            frames.push(lifecycle(&uuid, "completed"));
            frames.push(result_success());
            Some(frames)
        });
        Some(handler)
    });
    (launcher, chain, model_requests)
}

/// A turn whose steer the CLI ran after the turn's answer, stopped before the follow-up answered.
struct StoppedFollowUp {
    provider: ClaudeProvider,
    opened: forge_provider_api::OpenedSession,
    model_requests: ModelRequestLog,
    /// Every event of the steered turn after its answer began.
    events: Vec<forge_provider_api::ProviderEvent>,
    completed: forge_provider_api::CompletedTurn,
}

/// Answer one turn, then steer the next one after its answer and stop it while the follow-up CLI
/// turn that the steer started has not answered.
async fn stop_a_follow_up_steer(directory: &std::path::Path) -> StoppedFollowUp {
    let (launcher, _, model_requests) = stopped_steer_peer(&[
        PromptReply::Answered,
        PromptReply::AnsweredAwaitingSteer,
        PromptReply::Answered,
    ]);
    let (provider, mut opened) = open(launcher, directory).await;
    let binding = opened.binding.id.clone();
    run_turn(&provider, &mut opened, "turn-1", false).await;
    let scope = start_prompt(&provider, &binding, "steered").await;
    until_message(&mut opened.events).await;
    provider
        .turn_steer(
            operation_context(),
            &binding,
            journaled_steering(scope.clone(), "mention anchor", "steer"),
        )
        .await
        .unwrap();
    let mut events = until_notice(&mut opened.events).await;
    provider
        .turn_interrupt(
            operation_context(),
            &binding,
            forge_provider_api::CancelRequest {
                scope,
                input: forge_provider_api::operations::InterruptInput::Retain,
            },
        )
        .await
        .unwrap();
    let (rest, completed) = until_completed(&mut opened.events).await;
    events.extend(rest);
    assert_eq!(
        completed.result().outcome(),
        forge_provider_api::turn::TurnOutcome::Interrupted
    );
    StoppedFollowUp {
        provider,
        opened,
        model_requests,
        events,
        completed,
    }
}

/// A steer whose follow-up CLI turn a stop ended before any answer is not reported as read, so
/// Harness lists it as not confirmed read below the cancelled heading.
///
/// The steer was once reported consumed when the CLI started it, and the settled exchange drew it
/// as a consumed prompt with nothing after it, which read as an exchange without an outcome.
#[tokio::test]
async fn a_steer_stopped_before_its_follow_up_answered_is_not_reported_read() {
    let directory = tempfile::tempdir().unwrap();
    let stopped = stop_a_follow_up_steer(directory.path()).await;

    let trace = consumption_trace(&stopped.events);
    assert!(
        trace.iter().all(|entry| !entry.starts_with("user")),
        "the unanswered steer was reported read: {trace:?}"
    );
    stopped
        .provider
        .shutdown(operation_context())
        .await
        .unwrap();
}

/// A steer whose follow-up a stop ended unanswered is removed from the CLI history, so the next
/// prompt reaches the model alone and a rewind that drops the next turn passes the CLI's guard.
///
/// The steer's entry and the interrupt marker once stayed between the steered turn's answer and
/// the next prompt, and the guarded resume that drops the next turn refused them.
#[tokio::test]
async fn a_steer_stopped_before_its_follow_up_answered_is_withdrawn() {
    let directory = tempfile::tempdir().unwrap();
    let mut stopped = stop_a_follow_up_steer(directory.path()).await;
    let binding = stopped.opened.binding.id.clone();
    let kept = stopped.completed.result().checkpoint_id().cloned().unwrap();
    run_turn(&stopped.provider, &mut stopped.opened, "next", false).await;

    let requests = stopped.model_requests.lock().unwrap().clone();
    let steer = requests[2].last().unwrap().clone();
    let next = requests.last().unwrap();
    assert!(
        !next.contains(&steer),
        "the next prompt reached the model after the stopped steer: {requests:?}"
    );
    let rewound = stopped
        .provider
        .thread_rewind(
            operation_context(),
            &binding,
            forge_provider_api::ThreadRewindRequest { after: Some(kept) },
        )
        .await;
    assert!(
        rewound.is_ok(),
        "rewind past the next turn failed: {rewound:?}"
    );
    stopped
        .provider
        .shutdown(operation_context())
        .await
        .unwrap();
}

/// A Forge turn that ran a follow-up CLI turn reports the time of the CLI turns that reported
/// its tokens, so its speed is computed over the interval that produced them.
///
/// The turn once reported only the last CLI turn's duration, a stopped follow-up's few
/// milliseconds, and the heading divided the whole answer by it.
#[tokio::test]
async fn a_stopped_follow_up_turn_times_only_the_answered_run() {
    let directory = tempfile::tempdir().unwrap();
    let stopped = stop_a_follow_up_steer(directory.path()).await;

    assert_eq!(stopped.completed.result().reported_duration_ms(), Some(10));
    stopped
        .provider
        .shutdown(operation_context())
        .await
        .unwrap();
}

/// A steer that Harness delivered as its own turn and that a stop ended before any answer is
/// removed from the CLI history, although the turn reports no execution.
///
/// The turn settled as cancelled without a ledger entry, so nothing withdrew its request: the next
/// prompt reached the model after it, and the guarded resume that drops the next turn refused the
/// steer's entry and its interrupt marker.
#[tokio::test]
async fn a_steer_turn_stopped_before_any_answer_is_withdrawn() {
    let directory = tempfile::tempdir().unwrap();
    let (launcher, _, model_requests) = stopped_steer_peer(&[
        PromptReply::Answered,
        PromptReply::Started,
        PromptReply::Answered,
    ]);
    let (provider, mut opened) = open(launcher, directory.path()).await;
    let binding = opened.binding.id.clone();
    let kept = run_turn(&provider, &mut opened, "turn-1", false)
        .await
        .result()
        .checkpoint_id()
        .cloned()
        .unwrap();
    let request = prompt_request(
        &binding,
        "steer-turn",
        receipt("steer-turn"),
        forge_provider_api::permissions::PermissionResponse::Allow,
    );
    let scope = request.scope.clone();
    // `turn_start` returns once the turn starts natively, which this input never does.
    let start = tokio::spawn({
        let provider = provider.clone();
        let binding = binding.clone();
        async move {
            provider
                .turn_start(operation_context(), &binding, request)
                .await
        }
    });
    until_notice(&mut opened.events).await;
    provider
        .turn_interrupt(
            operation_context(),
            &binding,
            forge_provider_api::CancelRequest {
                scope,
                input: forge_provider_api::operations::InterruptInput::Retain,
            },
        )
        .await
        .unwrap();
    assert!(matches!(
        start.await.unwrap(),
        Err(ProviderError::Cancelled)
    ));
    run_turn(&provider, &mut opened, "next", false).await;

    let requests = model_requests.lock().unwrap().clone();
    let steer = requests[1].last().unwrap().clone();
    assert!(
        !requests.last().unwrap().contains(&steer),
        "the next prompt reached the model after the stopped steer: {requests:?}"
    );
    let rewound = provider
        .thread_rewind(
            operation_context(),
            &binding,
            forge_provider_api::ThreadRewindRequest { after: Some(kept) },
        )
        .await;
    assert!(
        rewound.is_ok(),
        "rewind past the next turn failed: {rewound:?}"
    );
    provider.shutdown(operation_context()).await.unwrap();
}

/// A rewind the CLI refuses at startup logs the CLI's own reason, which it prints as an error
/// `result` before it exits, instead of only the closed output.
#[tokio::test]
async fn a_refused_rewind_logs_the_cli_reason() {
    use crate::testing::diagnostics::CapturedFields;
    use tracing::instrument::WithSubscriber;
    use tracing_subscriber::layer::SubscriberExt;
    let directory = tempfile::tempdir().unwrap();
    let launcher = PeerLauncher::with_arguments(|arguments| {
        argument_value(arguments, "--resume-session-at")
            .is_none()
            .then(|| answering_peer(&["ok"]))
    });
    let (provider, mut opened) = open(launcher, directory.path()).await;
    let binding = opened.binding.id.clone();
    let kept = run_turn(&provider, &mut opened, "turn-1", false)
        .await
        .result()
        .checkpoint_id()
        .cloned()
        .unwrap();
    run_turn(&provider, &mut opened, "turn-2", false).await;
    let captured = Arc::new(Mutex::new(Vec::new()));
    let subscriber = tracing_subscriber::Registry::default().with(CapturedFields(captured.clone()));

    let rewound = provider
        .thread_rewind(
            operation_context(),
            &binding,
            forge_provider_api::ThreadRewindRequest { after: Some(kept) },
        )
        .with_subscriber(subscriber)
        .await;

    assert!(rewound.is_err());
    let recorded = captured.lock().unwrap().join("\n");
    // The host's log keeps `error` among an event's free-text fields and drops other names.
    assert!(
        recorded.contains(&format!("error={}", super::test_peer::LAUNCH_REFUSAL)),
        "the CLI's reason is missing from the diagnostics: {recorded}"
    );
    provider.shutdown(operation_context()).await.unwrap();
}
