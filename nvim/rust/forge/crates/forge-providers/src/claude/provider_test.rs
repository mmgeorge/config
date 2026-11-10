//! Provider contract tests against an in-memory Claude Code peer.

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
        self.state.lock().unwrap().rejected = true;
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
            control_handoff: false,
            cancelled: false,
            error: None,
        })
    }
    fn tool_finished(&self, _: &forge_provider_api::InvocationScope, _: Option<&str>) -> bool {
        false
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
        input: forge_provider_api::input::PromptInput::Text("hello".into()),
        recovered_inputs: Vec::new(),
        context: Default::default(),
        instructions: "fixture instructions".into(),
        attachments: Vec::new(),
        receipt,
        permissions: Arc::new(FixedPermissions(permission)),
        provider_tools: Arc::new(NoTools),
        turn_control: Arc::new(OpenTurnControl),
        host_effects: Arc::new(SettledHostEffects),
    }
}

/// Start the user message with one `<forge>` block holding changed context and the input's note.
#[test]
fn prompt_texts_place_the_forge_block_before_the_input() {
    use forge_provider_api::turn_context::{ContextPart, ContextSection, TurnContext};
    let binding = forge_provider_api::BindingId::new("context-binding").unwrap();
    let mut request = prompt_request(
        &binding,
        "context-turn",
        receipt("context-submission"),
        forge_provider_api::permissions::PermissionResponse::Allow,
    );
    request.instructions.clear();
    assert_eq!(
        super::provider::prompt_texts(&request),
        ["hello"],
        "an unchanged context without a note adds no text"
    );
    request.instructions = "fixture note".into();
    request.context = TurnContext {
        record: "k3f9".into(),
        parts: vec![
            ContextPart::new(ContextSection::Task, "Plan task.").with_attribute("kind", "plan"),
        ],
    };
    assert_eq!(
        super::provider::prompt_texts(&request),
        [
            "<forge>\n<note>\nfixture note\n</note>\n<task kind=\"plan\">\nPlan task.\n</task>\n</forge>",
            "hello"
        ]
    );
}

fn steering_request(
    scope: forge_provider_api::InvocationScope,
    text: &str,
) -> forge_provider_api::session::SteeringRequest {
    forge_provider_api::session::SteeringRequest {
        scope,
        input: forge_provider_api::input::PromptInput::Text(text.into()),
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
            forge_provider_api::CancelRequest { scope },
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

/// Rewinding restarts the process; the deliberate exit of the old process must not invalidate the
/// binding, so the next prompt runs on the resumed process.
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
    assert_eq!(launches.len(), 2);
    assert!(
        launches[1]
            .iter()
            .any(|argument| argument.starts_with("--resume-session-at="))
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
        forge_provider_api::CancelRequest { scope },
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
    request.input = forge_provider_api::input::PromptInput::Text("again".into());
    assert!(
        provider
            .turn_start(operation_context(), &binding, request)
            .await
            .is_err()
    );
    provider.shutdown(operation_context()).await.unwrap();
}
