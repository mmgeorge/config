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
            forge_provider_api::CancelRequest { scope },
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
        forge_provider_api::CancelRequest { scope },
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
