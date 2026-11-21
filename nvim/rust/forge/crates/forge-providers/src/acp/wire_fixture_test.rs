use super::{AcpConnection, AcpHandler, AcpLaunch, submission::AcpSubmission};
use async_trait::async_trait;
use forge_provider_api::submission::InputReceipt;
use forge_provider_api::{ProviderError, RecoveryToken, SubmissionId};
use serde_json::{Value, json};
use std::{
    collections::BTreeMap,
    sync::{Arc, Mutex},
};

const BASE_INSTRUCTIONS: &str = "Keep the fixture base instructions.";

#[derive(Default)]
struct ReceiptState {
    sending: bool,
    accepted: bool,
    rejected: bool,
    failed: bool,
    failure: Option<ProviderError>,
    fail_acceptance: bool,
    hold_acceptance: bool,
}

struct FixtureReceipt {
    id: SubmissionId,
    state: Mutex<ReceiptState>,
    acceptance_started: tokio::sync::Notify,
    acceptance_release: tokio::sync::Notify,
}

impl FixtureReceipt {
    fn new() -> Arc<Self> {
        Self::named("fixture-submission")
    }

    /// Allocate an independent journal identity for one wire request.
    fn named(id: &str) -> Arc<Self> {
        Arc::new(Self {
            id: SubmissionId::try_from(id.to_owned()).unwrap(),
            state: Mutex::new(ReceiptState::default()),
            acceptance_started: Default::default(),
            acceptance_release: Default::default(),
        })
    }
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
        let held = self.state.lock().unwrap().hold_acceptance;
        if held {
            self.acceptance_started.notify_one();
            self.acceptance_release.notified().await;
        }
        let mut state = self.state.lock().unwrap();
        if state.fail_acceptance {
            return Err(forge_provider_api::ProviderFailure::new(
                forge_provider_api::ProviderOperation::TurnStart,
                forge_provider_api::ProviderFailureKind::Runtime,
            )
            .into());
        }
        state.accepted = true;
        Ok(())
    }
    async fn record_rejected(self: Arc<Self>, _: ProviderError) -> Result<(), ProviderError> {
        self.state.lock().unwrap().rejected = true;
        Ok(())
    }
    async fn record_failure(self: Arc<Self>, error: ProviderError) -> Result<(), ProviderError> {
        let mut state = self.state.lock().unwrap();
        state.failed = true;
        state.failure = Some(error);
        Ok(())
    }
}

struct IgnoreCallbacks;

#[async_trait]
impl AcpHandler for IgnoreCallbacks {
    async fn notification(&self, _: &str, _: Value) -> Result<(), super::AcpConnectionError> {
        Ok(())
    }
    async fn request(&self, _: &str, _: Value) -> Result<Value, super::AcpConnectionError> {
        Err(super::AcpConnectionError::Configuration(
            "fixture did not expect a callback",
        ))
    }
}

/// Port of the deterministic native-wire peer: one handler instance serves one connection.
fn wire_peer_handler(mode: String) -> super::in_memory_peer::PeerHandler {
    let mut held_prompt: Option<Value> = None;
    let mut pending_cancellation = Vec::new();
    let mut cancelled = false;
    let mut prompt_count = 0;
    Box::new(move |request| {
        let method = request["method"].as_str().unwrap_or_default();
        let mut outgoing = Vec::new();
        let Some(request_id) = request.get("id").filter(|id| !id.is_null()).cloned() else {
            if method == "session/cancel" && mode == "cancel_multiple" {
                cancelled = true;
                outgoing.extend(pending_cancellation.drain(..).map(|prompt_id| {
                    json!({"id":prompt_id,"result":{"stopReason":"cancelled"}})
                }));
            }
            if method == "session/cancel"
                && mode != "held_cancel"
                && let Some(prompt_id) = held_prompt.take()
            {
                outgoing.push(json!({"id":prompt_id,"result":{"stopReason":"cancelled"}}));
            }
            return outgoing;
        };
        let mut result = json!({});
        if method == "fixture/release"
            && let Some(prompt_id) = held_prompt.take()
        {
            outgoing.push(json!({"id":prompt_id,"result":{"stopReason":"end_turn"}}));
        }
        if method == "initialize" {
            let mut capabilities = json!({"loadSession":true,"sessionCapabilities":{"resume":{},"close":{}},"mcpCapabilities":{"http":true}});
            if mode == "native_instructions" {
                capabilities["_meta"] = json!({"antigravity":{"systemPrompt":{"append":true}}});
            }
            result = json!({
                "protocolVersion":1,
                "agentInfo":{"version":"99.0.0"},
                "agentCapabilities":capabilities,
            });
        }
        if (method == "initialize" && mode == "initialize_auth_required")
            || (matches!(method, "session/new" | "session/resume" | "session/load")
                && mode == "auth_required")
        {
            outgoing.push(json!({
                "id":request_id,
                "error":{"code":-32000,"message":"Authentication required with fixture-private detail"},
            }));
            return outgoing;
        }
        if method == "session/load" && mode == "load_applied_error" {
            outgoing.push(json!({
                "id":request_id,
                "error":{"code":-32603,"message":"private failure after native endpoint apply"},
            }));
            return outgoing;
        }
        if method == "session/resume" && mode == "missing_history" {
            let history = request["params"]["sessionId"].as_str().unwrap();
            outgoing.push(json!({
                "id":request_id,
                "error":{
                    "code":-32603,
                    "message":format!("Failed to read session {history}; retry, or load it with session/load"),
                },
            }));
            return outgoing;
        }
        if method == "session/close" {
            match mode.as_str() {
                "close_rejected" => {
                    return vec![json!({
                        "id":request_id,
                        "error":{"code":-32603,"message":"native close failed"},
                    })];
                }
                "close_unanswered" => return outgoing,
                _ => {}
            }
        }
        if method == "session/new" || method == "session/resume" || method == "session/load" {
            let effort = json!({
                "id":"effort","currentValue":"medium",
                "options":[{"value":"low"},{"value":"medium"},{"value":"high"}],
            });
            result = if mode == "config_option_models" {
                json!({
                    "sessionId":"fixture",
                    "configOptions":[
                        {"id":"model","category":"model","currentValue":"default-model",
                         "options":[{"value":"default-model"},{"value":"other"}]},
                        effort,
                    ],
                })
            } else {
                json!({
                    "sessionId":"fixture",
                    "models":{
                        "currentModelId":"default-model",
                        "availableModels":[
                            {"modelId":"default-model"},
                            {"modelId":"other","name":"Other 1.0"},
                        ],
                    },
                    "configOptions":[effort],
                })
            };
        }
        if method == "session/fork" {
            result = json!({"sessionId":"fork-child"});
        }
        if method == "session/prompt" || method == "_session/steering" {
            if mode == "cancel_multiple" && !cancelled {
                pending_cancellation.push(request_id);
                return outgoing;
            }
            if mode == "steering_split" {
                if let Some(previous) = held_prompt.replace(request_id) {
                    outgoing.push(json!({"id":previous,"result":{"stopReason":"end_turn"}}));
                }
                return outgoing;
            }
            if mode == "steer_ends_first" {
                prompt_count += 1;
                if prompt_count == 1 {
                    held_prompt = Some(request_id);
                } else {
                    outgoing.push(json!({"id":request_id,"result":{"stopReason":"cancelled"}}));
                }
                return outgoing;
            }
            if mode == "attachment_steer" {
                if let Some(primary) = held_prompt.take() {
                    outgoing.push(json!({"id":primary,"result":{"stopReason":"end_turn"}}));
                } else {
                    outgoing.push(json!({"method":"session/update","params":{
                        "sessionId":"fixture",
                        "update":{
                            "sessionUpdate":"agent_message_chunk",
                            "content":{"type":"text","text":"answer"},
                        },
                    }}));
                    held_prompt = Some(request_id);
                    return outgoing;
                }
            }
            if mode.starts_with("observed:") || mode == "held" || mode == "held_cancel" {
                outgoing.push(json!({"method":"session/update","params":{
                    "sessionId":"fixture",
                    "update":{
                        "sessionUpdate":"agent_message_chunk",
                        "content":{"type":"text","text":"answer"},
                    },
                }}));
            }
            if matches!(mode.as_str(), "held" | "held_cancel" | "held_no_start")
                && method == "session/prompt"
            {
                held_prompt = Some(request_id);
                return outgoing;
            }
            if mode == "remote_error" {
                outgoing.push(json!({
                    "id":request_id,
                    "error":{"code":-32603,"message":"private native error"},
                }));
                return outgoing;
            }
            if mode == "invalid_request" {
                outgoing.push(json!({
                    "id":request_id,
                    "error":{"code":-32600,"message":"private invalid request detail"},
                }));
                return outgoing;
            }
            result = if method == "_session/steering" {
                if mode == "promptRequired" {
                    json!({"outcome":"promptRequired","reason":"noRunningTurn"})
                } else {
                    json!({"outcome":mode})
                }
            } else if mode == "invalid" {
                json!({})
            } else {
                let stop_reason = match mode.strip_prefix("observed:") {
                    Some(observed) => observed,
                    None if mode == "cancelled" => "cancelled",
                    None => "end_turn",
                };
                json!({"stopReason":stop_reason})
            };
        }
        outgoing.push(json!({"id":request_id,"result":result}));
        outgoing
    })
}

pub(super) struct WireFixture {
    connection: AcpConnection,
    command: Vec<String>,
    peer: super::in_memory_peer::InMemoryPeer,
    // Declared last: fields drop in order, and Windows cannot delete files still open.
    _directory: tempfile::TempDir,
}

impl WireFixture {
    async fn start(mode: &str) -> Result<Self, Box<dyn std::error::Error>> {
        let directory = tempfile::tempdir()?;
        let mode = mode.to_owned();
        let peer = super::in_memory_peer::InMemoryPeer::register(
            directory.path(),
            Box::new(move || wire_peer_handler(mode.clone())),
        );
        let command = vec![peer.program()];
        let environment = BTreeMap::new();
        let workspace = directory.path().to_string_lossy().into_owned();
        let connection = AcpConnection::launch(
            AcpLaunch {
                process_owner: None,
                command: &command,
                environment: &environment,
                workspace: &workspace,
                profile: None,
            },
            Arc::new(IgnoreCallbacks),
        )
        .await?;
        Ok(Self {
            _directory: directory,
            connection,
            command,
            peer,
        })
    }

    async fn ready(&self) -> Result<(), ProviderError> {
        self.connection
            .request(
                "initialize",
                json!({"protocolVersion":1,"clientInfo":{"name":"fixture","version":"0"}}),
            )
            .await
            .map_err(|error| {
                crate::error::acp(error, forge_provider_api::ProviderOperation::ThreadOpen)
            })?;
        self.connection
            .request("session/new", json!({"cwd":".","mcpServers":[]}))
            .await
            .map_err(|error| {
                crate::error::acp(error, forge_provider_api::ProviderOperation::ThreadOpen)
            })?;
        Ok(())
    }
}

struct IgnoreTrace;

impl forge_provider_api::trace::ProviderTrace for IgnoreTrace {
    fn record(&self, _: forge_provider_api::trace::ProviderTraceRecord) {}
}

/// A host that admits a prompt without callbacks or foreground effects.
struct InstructionTurnHost;

#[async_trait]
impl forge_provider_api::permissions::ProviderPermissions for InstructionTurnHost {
    async fn authorize(
        &self,
        _: &forge_provider_api::InvocationScope,
        _: forge_provider_api::permissions::PermissionRequest,
    ) -> forge_provider_api::permissions::PermissionResponse {
        panic!("the instruction fixture does not request tools")
    }

    async fn before_tool(
        &self,
        _: &forge_provider_api::InvocationScope,
    ) -> Result<(), ProviderError> {
        panic!("the instruction fixture does not run tools")
    }
}

#[async_trait]
impl forge_provider_api::control::ProviderTools for InstructionTurnHost {
    async fn call(
        &self,
        _: &forge_provider_api::InvocationScope,
        _: forge_provider_api::control::ToolCall,
    ) -> Result<String, ProviderError> {
        panic!("the instruction fixture does not call host tools")
    }
}

#[async_trait]
impl forge_provider_api::turn_control::TurnControl for InstructionTurnHost {
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

#[async_trait]
impl forge_provider_api::session::ProviderHostEffects for InstructionTurnHost {
    async fn settle_foreground(
        &self,
        _: &forge_provider_api::BindingId,
        _: &forge_provider_api::InvocationScope,
    ) -> Result<(), forge_provider_api::session::HostEffectFailure> {
        Ok(())
    }
}

#[tokio::test]
async fn prompt_terminal_response_commits_native_acceptance_and_normalizes_cancelled_stop() {
    for mode in ["ok", "cancelled"] {
        let fixture = WireFixture::start(mode).await.unwrap();
        fixture.ready().await.unwrap();
        let receipt = FixtureReceipt::new();
        let result = fixture
            .connection
            .submit(
                AcpSubmission::Prompt,
                json!({"sessionId":"fixture","prompt":[{"type":"text","text":"prompt"}]}),
                Some(receipt.clone()),
                None,
                None,
            )
            .await;
        assert!(result.is_ok());
        {
            let state = receipt.state.lock().unwrap();
            assert!(state.sending && state.accepted && !state.rejected && !state.failed);
        }
        fixture.connection.shutdown().await.unwrap();
    }
}

/// Each ACP prompt owns its receipt even when a steer causes the first reply to arrive early.
#[tokio::test]
async fn early_root_reply_cannot_accept_pending_steer() {
    let fixture = WireFixture::start("steering_split").await.unwrap();
    fixture.ready().await.unwrap();
    let original_receipt = FixtureReceipt::named("original");
    let steer_receipt = FixtureReceipt::named("steer");
    let scope = forge_provider_api::InvocationScope::new(
        forge_provider_api::testing::root_thread("fixture-owner"),
        "fixture-turn",
    );
    let (dispatch, mut handoff) = forge_provider_api::submission::DispatchReporter::channel(
        forge_provider_api::BindingId::new("fixture-binding").unwrap(),
        scope.clone(),
        steer_receipt.id().clone(),
        steer_receipt.attempt_id().unwrap(),
    );
    let original = fixture.connection.submit(
        AcpSubmission::Prompt,
        json!({"sessionId":"fixture","prompt":[{"type":"text","text":"original"}]}),
        Some(original_receipt.clone()),
        None,
        None,
    );
    tokio::pin!(original);
    assert!(futures_util::poll!(&mut original).is_pending());

    let steer = fixture.connection.submit(
        AcpSubmission::Steer,
        json!({"sessionId":"fixture","prompt":[{"type":"text","text":"steer"}]}),
        Some(steer_receipt.clone()),
        Some(&dispatch),
        None,
    );
    tokio::pin!(steer);
    assert!(futures_util::poll!(&mut steer).is_pending());
    assert_eq!(handoff.wait().await.unwrap().scope(), &scope);
    original.await.unwrap();
    assert!(original_receipt.state.lock().unwrap().accepted);
    {
        let state = steer_receipt.state.lock().unwrap();
        assert!(state.sending && !state.accepted);
    }

    fixture
        .connection
        .request("fixture/release", json!({}))
        .await
        .unwrap();
    steer.await.unwrap();
    assert!(steer_receipt.state.lock().unwrap().accepted);
    fixture.connection.shutdown().await.unwrap();
}

/// An earlier cancelled steer cannot override the root's later successful reply.
#[tokio::test]
async fn later_root_reply_owns_the_exchange_outcome() {
    let fixture = WireFixture::start("steer_ends_first").await.unwrap();
    fixture.ready().await.unwrap();
    let root = fixture.connection.submit(
        AcpSubmission::Prompt,
        json!({"sessionId":"fixture","prompt":[{"type":"text","text":"root"}]}),
        Some(FixtureReceipt::named("root")),
        None,
        None,
    );
    tokio::pin!(root);
    assert!(futures_util::poll!(&mut root).is_pending());

    let steer = fixture
        .connection
        .submit(
            AcpSubmission::Steer,
            json!({"sessionId":"fixture","prompt":[{"type":"text","text":"steer"}]}),
            Some(FixtureReceipt::named("steer")),
            None,
            None,
        )
        .await
        .unwrap();
    assert_eq!(steer.outcome(), super::PromptOutcome::Cancelled);
    fixture
        .connection
        .request("fixture/release", json!({}))
        .await
        .unwrap();
    let root = root.await.unwrap();
    assert_eq!(
        root.latest(steer).outcome(),
        super::PromptOutcome::Completed(forge_provider_api::turn::TurnCompletionReason::Normal),
    );
    fixture.connection.shutdown().await.unwrap();
}

/// Derive terminal order from the peer even when the consumers resume in reverse order.
#[tokio::test]
async fn wire_reply_order_survives_reverse_waiter_order() {
    let fixture = WireFixture::start("steer_ends_first").await.unwrap();
    fixture.ready().await.unwrap();
    let root = fixture.connection.submit(
        AcpSubmission::Prompt,
        json!({"sessionId":"fixture","prompt":[{"type":"text","text":"root"}]}),
        Some(FixtureReceipt::named("root")),
        None,
        None,
    );
    let steer = fixture.connection.submit(
        AcpSubmission::Steer,
        json!({"sessionId":"fixture","prompt":[{"type":"text","text":"steer"}]}),
        Some(FixtureReceipt::named("steer")),
        None,
        None,
    );
    tokio::pin!(root, steer);
    assert!(futures_util::poll!(&mut root).is_pending());
    assert!(futures_util::poll!(&mut steer).is_pending());
    // The peer replies to the steer first, then releases the root before acknowledging this call.
    fixture.connection.request("fixture/release", json!({})).await.unwrap();
    let root = root.await.unwrap();
    let steer = steer.await.unwrap();
    assert_eq!(
        root.latest(steer).outcome(),
        super::PromptOutcome::Completed(forge_provider_api::turn::TurnCompletionReason::Normal),
    );
    fixture.connection.shutdown().await.unwrap();
}

/// Keep steering open after the primary reply while an admitted native steer is outstanding.
#[tokio::test]
async fn last_native_reply_fences_late_steers_and_stop() {
    let fixture = WireFixture::start("steering_split").await.unwrap();
    fixture.ready().await.unwrap();
    let cancellation = super::connection::PromptCancellation::default();
    let submit = |kind, text| {
        fixture.connection.submit(
            kind,
            json!({"sessionId":"fixture","prompt":[{"type":"text","text":text}]}),
            Some(FixtureReceipt::named(text)),
            None,
            Some(&cancellation),
        )
    };
    let root = submit(AcpSubmission::Prompt, "root");
    let first = submit(AcpSubmission::Steer, "first");
    let second = submit(AcpSubmission::Steer, "second");
    tokio::pin!(root, first, second);
    assert!(futures_util::poll!(&mut root).is_pending());
    assert!(futures_util::poll!(&mut first).is_pending());
    root.await.unwrap();
    assert!(futures_util::poll!(&mut second).is_pending());
    first.await.unwrap();

    // Let the peer reply to the last steer without polling its receipt worker.
    fixture.connection.request("fixture/release", json!({})).await.unwrap();
    let late = submit(AcpSubmission::Steer, "after terminal");
    tokio::pin!(late);
    assert!(matches!(
        futures_util::poll!(&mut late),
        std::task::Poll::Ready(Err(ProviderError::NotConsumed))
    ));
    cancellation.notify(&fixture.connection, "fixture").unwrap();
    second.await.unwrap();
    fixture.connection.request("fixture/barrier", json!({})).await.unwrap();
    assert!(fixture.peer.requests().iter().all(|request| request["method"] != "session/cancel"));
    assert_eq!(
        fixture.peer.requests().iter().filter(|request| request["method"] == "session/prompt").count(),
        3,
    );
    fixture.connection.shutdown().await.unwrap();
}

/// A cancellation observed before publication leaves a steer definitely unsent.
#[tokio::test]
async fn cancelled_invocation_does_not_publish_another_prompt() {
    let fixture = WireFixture::start("ok").await.unwrap();
    fixture.ready().await.unwrap();
    let receipt = FixtureReceipt::named("steer");
    let cancellation = super::connection::PromptCancellation::default();
    cancellation.stop();
    let result = fixture
        .connection
        .submit(
            AcpSubmission::Steer,
            json!({"sessionId":"fixture","prompt":[{"type":"text","text":"steer"}]}),
            Some(receipt.clone()),
            None,
            Some(&cancellation),
        )
        .await;
    assert!(matches!(result, Err(ProviderError::NotConsumed)));
    {
        let state = receipt.state.lock().unwrap();
        assert!(state.sending && state.rejected && !state.accepted && !state.failed);
    }
    assert!(
        fixture
            .peer
            .requests()
            .iter()
            .all(|request| request["method"] != "session/prompt")
    );
    fixture.connection.shutdown().await.unwrap();
}

/// A prevented root prompt supplies the clean pre-dispatch cancellation result.
#[tokio::test]
async fn prevented_root_prompt_reports_clean_cancellation() {
    let fixture = WireFixture::start("ok").await.unwrap();
    fixture.ready().await.unwrap();
    let receipt = FixtureReceipt::named("original");
    let cancellation = super::connection::PromptCancellation::default();
    cancellation.stop();
    let result = fixture
        .connection
        .submit(
            AcpSubmission::Prompt,
            json!({"sessionId":"fixture","prompt":[{"type":"text","text":"original"}]}),
            Some(receipt.clone()),
            None,
            Some(&cancellation),
        )
        .await;
    assert!(result.unwrap_err().is_clean_cancellation());
    assert!(receipt.state.lock().unwrap().rejected);
    assert!(
        fixture
            .peer
            .requests()
            .iter()
            .all(|request| request["method"] != "session/prompt")
    );
    fixture.connection.shutdown().await.unwrap();
}

/// Order direct cancellation after every published request and prevent any later publication.
#[tokio::test]
async fn one_wire_cancel_follows_all_published_prompts_and_prevents_late_steer() {
    let fixture = WireFixture::start("cancel_multiple").await.unwrap();
    fixture.ready().await.unwrap();
    let cancellation = super::connection::PromptCancellation::default();
    let original_receipt = FixtureReceipt::named("original");
    let steer_receipt = FixtureReceipt::named("pending-steer");
    let original = fixture.connection.submit(
        AcpSubmission::Prompt,
        json!({"sessionId":"fixture","prompt":[{"type":"text","text":"original"}]}),
        Some(original_receipt.clone()),
        None,
        Some(&cancellation),
    );
    tokio::pin!(original);
    assert!(futures_util::poll!(&mut original).is_pending());

    let steer = fixture.connection.submit(
        AcpSubmission::Steer,
        json!({"sessionId":"fixture","prompt":[{"type":"text","text":"pending steer"}]}),
        Some(steer_receipt.clone()),
        None,
        Some(&cancellation),
    );
    tokio::pin!(steer);
    assert!(futures_util::poll!(&mut steer).is_pending());

    cancellation.notify(&fixture.connection, "fixture").unwrap();
    cancellation.notify(&fixture.connection, "fixture").unwrap();
    let late_receipt = FixtureReceipt::named("late-steer");
    let late = fixture
        .connection
        .submit(
            AcpSubmission::Steer,
            json!({"sessionId":"fixture","prompt":[{"type":"text","text":"late steer"}]}),
            Some(late_receipt.clone()),
            None,
            Some(&cancellation),
        )
        .await;
    assert!(matches!(late, Err(ProviderError::NotConsumed)));

    let (original, steer) = tokio::join!(original, steer);
    assert_eq!(
        original.unwrap().outcome(),
        super::response::PromptOutcome::Cancelled,
    );
    assert_eq!(
        steer.unwrap().outcome(),
        super::response::PromptOutcome::Cancelled,
    );
    assert!(original_receipt.state.lock().unwrap().accepted);
    assert!(steer_receipt.state.lock().unwrap().accepted);
    assert!(late_receipt.state.lock().unwrap().rejected);

    let requests = fixture.peer.requests();
    let methods = requests
        .iter()
        .filter_map(|request| request["method"].as_str())
        .collect::<Vec<_>>();
    assert_eq!(
        methods,
        ["initialize", "session/new", "session/prompt", "session/prompt", "session/cancel"],
    );
    fixture.connection.shutdown().await.unwrap();
}

/// Open a production ACP binding against the deterministic native-wire peer.
pub(super) async fn provider_fixture(
    mode: &str,
    system_instructions: &str,
) -> (
    WireFixture,
    super::provider::AcpProvider,
    forge_provider_api::OpenedSession,
) {
    use forge_provider_api::provider::Provider;
    let fixture = WireFixture::start(mode).await.unwrap();
    fixture.connection.shutdown().await.unwrap();
    let provider = super::provider::AcpProvider::new(
        forge_provider_api::ProviderId::new("antigravity").unwrap(),
        fixture.command.clone(),
        fixture._directory.path().join("runtime"),
    )
    .unwrap();
    let opened = provider
        .thread_open(
            operation_context(),
            forge_provider_api::SessionRequest {
                thread_id: (forge_provider_api::testing::root_thread("fixture-owner")).clone(),
                mode: forge_provider_api::SessionMode::Interactive,
                workspace: fixture._directory.path().to_string_lossy().into_owned(),
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
                system_instructions: system_instructions.to_owned(),
                trace: Arc::new(IgnoreTrace),
            },
        )
        .await
        .unwrap();
    (fixture, provider, opened)
}

/// Build an ordinary prompt with a journaled input and the fixture host callbacks.
fn fixture_prompt(
    opened: &forge_provider_api::OpenedSession,
    prompt: &str,
) -> forge_provider_api::PromptRequest {
    let receipt = FixtureReceipt::named(prompt);
    fixture_prompt_with_receipt(opened, prompt, receipt)
}

fn fixture_prompt_with_receipt(
    opened: &forge_provider_api::OpenedSession,
    prompt: &str,
    receipt: Arc<FixtureReceipt>,
) -> forge_provider_api::PromptRequest {
    let host = Arc::new(InstructionTurnHost);
    let scope =
        forge_provider_api::InvocationScope::new(opened.binding.thread_id.clone(), prompt);
    let attempt_id = receipt.attempt_id().unwrap();
    let (dispatch, _confirmation) = forge_provider_api::submission::DispatchReporter::channel(
        opened.binding.id.clone(),
        scope.clone(),
        receipt.id.clone(),
        attempt_id.clone(),
    );
    forge_provider_api::PromptRequest {
        thread_id: scope.thread_id.clone(),
        turn_id: scope.turn_id().unwrap(),
        scope,
        dispatch,
        execution: forge_provider_api::ExecutionMode::Default,
        configuration: forge_provider_api::session::InvocationConfiguration {
            model: Default::default(),
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
        attempt_id,
        input: forge_provider_api::forge::TurnInput::user(
            forge_provider_api::input::PromptInput::Text(prompt.into()),
        ),
        recovered_inputs: Vec::new(),
        context: forge_provider_api::forge::TurnContext {
            task: Some(forge_provider_api::forge::Task::None),
            ..Default::default()
        },
        attachments: Vec::new(),
        receipt,
        permissions: host.clone(),
        provider_tools: host.clone(),
        turn_control: host.clone(),
        host_effects: host,
    }
}

/// Drive an ordinary prompt through the production adapter and collect its terminal event.
async fn send_instruction_fixture_prompt(
    provider: &super::provider::AcpProvider,
    opened: &mut forge_provider_api::OpenedSession,
    prompt: &str,
) -> String {
    use forge_provider_api::provider::Provider;
    let request = fixture_prompt(opened, prompt);
    let scope = request.scope.clone();
    let context = request.user_message_block();
    assert!(context.starts_with("<forge>"));
    provider
        .turn_start(operation_context(), &opened.binding.id, request)
        .await
        .unwrap();
    loop {
        let envelope = opened.events.recv().await.unwrap().unwrap();
        match envelope.event {
            forge_provider_api::ProviderEvent::Turn {
                event: forge_provider_api::TurnEvent::Completed(_),
                ..
            } => break,
            forge_provider_api::ProviderEvent::Provider {
                event: forge_provider_api::ProviderConnectionEvent::Unavailable(unavailable),
            } => panic!("the instruction turn failed: {}", unavailable.failure),
            _ => {}
        }
    }
    // Await collection before trying to admit the next prompt on this binding.
    provider
        .turn_interrupt(
            operation_context(),
            &opened.binding.id,
            forge_provider_api::CancelRequest {
                scope,
                input: forge_provider_api::InterruptInput::Retain,
            },
        )
        .await
        .unwrap();
    context
}

/// Unblock the host's unbounded interrupt by reaping the exact unresponsive transport.
#[tokio::test]
async fn forced_binding_collection_reaps_unresponsive_native_prompt() {
    use forge_provider_api::{CancelRequest, InterruptInput, provider::Provider};

    let (fixture, provider, opened) = provider_fixture("held_cancel", "").await;
    let binding = &opened.binding.id;
    let receipt = FixtureReceipt::named("held-prompt");
    let request = fixture_prompt_with_receipt(&opened, "Held prompt", receipt.clone());
    let scope = request.scope.clone();
    let mut prompt = Box::pin(provider.turn_start(operation_context(), binding, request));
    tokio::select! {
        () = fixture.peer.wait_for_request("session/prompt") => {},
        result = &mut prompt => panic!("held prompt returned early: {result:?}"),
    }
    let (_, cancellation) = forge_provider_api::OperationCancellation::channel();
    let interrupt_context = forge_provider_api::OperationContext::unbounded(
        forge_provider_api::OperationId::new("held-interrupt").unwrap(),
        cancellation,
    );
    let mut interrupt = Box::pin(provider.turn_interrupt(
        interrupt_context,
        binding,
        CancelRequest { scope, input: InterruptInput::Retain },
    ));
    tokio::select! {
        () = fixture.peer.wait_for_request("session/cancel") => {},
        result = &mut interrupt => panic!("unresponsive peer acknowledged cancellation: {result:?}"),
    }

    let termination = operation_context();
    provider.thread_terminate(termination.clone(), binding).await.unwrap();
    assert!(prompt.await.is_err());
    assert!(interrupt.await.is_err());
    {
        let receipt = receipt.state.lock().unwrap();
        assert!(receipt.sending && receipt.failed && !receipt.accepted && !receipt.rejected);
    }
    assert!(
        fixture.peer.requests().iter().all(|request| request["method"] != "session/close")
    );
    let successor = provider
        .thread_open(operation_context(), resume_request(&fixture, "successor-owner"))
        .await
        .unwrap();
    assert_ne!(successor.binding.id, *binding);
    provider.thread_terminate(termination, binding).await.unwrap();
    provider.thread_close(operation_context(), binding).await.unwrap();
    provider.thread_close(operation_context(), &successor.binding.id).await.unwrap();
    assert_eq!(
        fixture.peer.requests().iter().filter(|request| request["method"] == "session/close").count(),
        1,
    );
}

/// Reject an unsupported image before wire dispatch while the original turn stays usable.
#[tokio::test]
async fn unsupported_attachment_does_not_poison_a_running_prompt() {
    use forge_provider_api::provider::Provider;
    use forge_provider_api::session::{SteeringDelivery, SteeringRequest};

    let (fixture, provider, mut opened) = provider_fixture("attachment_steer", "").await;
    let binding = opened.binding.id.clone();
    let request = fixture_prompt(&opened, "root");
    let scope = request.scope.clone();
    let primary = provider.turn_start(operation_context(), &binding, request);
    tokio::pin!(primary);
    loop {
        tokio::select! {
            result = &mut primary => panic!("held primary settled: {result:?}"),
            envelope = opened.events.recv() => {
                if matches!(
                    envelope.unwrap().unwrap().event,
                    forge_provider_api::ProviderEvent::Turn {
                        event: forge_provider_api::TurnEvent::Started(_),
                        ..
                    },
                ) {
                    break;
                }
            }
        }
    }

    let receipt = FixtureReceipt::named("unsupported attachment");
    let rejected = provider
        .turn_steer(
            operation_context(),
            &binding,
            SteeringRequest {
                scope: scope.clone(),
                delivery: SteeringDelivery::Journaled { receipt: receipt.clone() },
                dispatch: None,
                input: forge_provider_api::forge::TurnInput::user(
                    forge_provider_api::input::PromptInput::Text("see the image".into()),
                ),
                attachments: vec![forge_provider_api::input::FileAttachment {
                    path: fixture._directory.path().join("image.png"),
                    label: "image.png".into(),
                    media_type: "image/png".into(),
                    contents: Arc::from([1_u8]),
                    context: "image context".into(),
                }],
            },
        )
        .await;
    assert!(matches!(
        rejected,
        Err(ProviderError::PreparationFailed(cause)) if matches!(
            *cause, ProviderError::Unsupported(_)
        ),
    ));
    {
        let receipt = receipt.state.lock().unwrap();
        assert!(receipt.rejected && !receipt.sending && !receipt.accepted);
    }

    provider
        .turn_steer(
            operation_context(),
            &binding,
            SteeringRequest {
                scope,
                delivery: SteeringDelivery::Context,
                dispatch: None,
                input: forge_provider_api::forge::TurnInput::user(
                    forge_provider_api::input::PromptInput::Text("finish".into()),
                ),
                attachments: Vec::new(),
            },
        )
        .await
        .unwrap();
    primary.await.unwrap();
    loop {
        let envelope = opened.events.recv().await.unwrap().unwrap();
        match envelope.event {
            forge_provider_api::ProviderEvent::Turn {
                event: forge_provider_api::TurnEvent::Completed(completed),
                ..
            } => {
                assert_eq!(
                    completed.result().outcome(),
                    forge_provider_api::turn::TurnOutcome::Completed {
                        stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
                    },
                );
                break;
            }
            forge_provider_api::ProviderEvent::Provider {
                event: forge_provider_api::ProviderConnectionEvent::Unavailable(unavailable),
            } => panic!("the primary failed: {}", unavailable.failure),
            _ => {}
        }
    }
    assert_eq!(
        fixture.peer.requests().iter().filter(|request| request["method"] == "session/prompt").count(),
        2,
    );
    provider.thread_close(operation_context(), &binding).await.unwrap();
}

/// Preserve native append on setup and attach the defensive prelude only after resume.
#[tokio::test]
async fn bundled_instruction_append_covers_fresh_and_resumed_history() {
    use forge_provider_api::provider::Provider;
    let (fixture, provider, mut opened) =
        provider_fixture("native_instructions", BASE_INSTRUCTIONS).await;
    let fresh_context = send_instruction_fixture_prompt(&provider, &mut opened, "fixture prompt").await;
    provider
        .thread_close(operation_context(), &opened.binding.id)
        .await
        .unwrap();

    let resumed_instructions = "Use the current fixture base instructions.";
    let mut request = resume_request(&fixture, "resumed-instruction-owner");
    request.system_instructions = resumed_instructions.to_owned();
    let mut resumed = provider
        .thread_open(operation_context(), request)
        .await
        .unwrap();
    let resumed_context = send_instruction_fixture_prompt(&provider, &mut resumed, "fixture prompt").await;
    let followup_context = send_instruction_fixture_prompt(&provider, &mut resumed, "followup").await;
    provider
        .thread_close(operation_context(), &resumed.binding.id)
        .await
        .unwrap();

    let requests = fixture.peer.requests();
    let setup: Vec<_> = requests
        .iter()
        .filter(|request| {
            matches!(
                request["method"].as_str(),
                Some("session/new" | "session/resume")
            )
        })
        .collect();
    assert_eq!(setup.len(), 2);
    assert_eq!(setup[0]["method"], "session/new");
    assert_eq!(setup[1]["method"], "session/resume");
    assert_eq!(setup[1]["params"]["sessionId"], "fixture");
    for (request, instructions) in setup
        .into_iter()
        .zip([BASE_INSTRUCTIONS, resumed_instructions])
    {
        assert_eq!(
            request["params"]["_meta"]["antigravity"]["systemPrompt"],
            json!({"append":instructions})
        );
        assert!(request["params"].get("systemPrompt").is_none());
    }
    let prompts: Vec<_> = requests
        .iter()
        .filter(|request| request["method"] == "session/prompt")
        .map(|request| &request["params"]["prompt"])
        .collect();
    let prelude = forge_provider_api::forge::system_instructions_block(resumed_instructions);
    assert_eq!(
        prompts,
        vec![
            &json!([
                {"type":"text","text":fresh_context},
                {"type":"text","text":"fixture prompt"}
            ]),
            &json!([
                {"type":"text","text":prelude},
                {"type":"text","text":resumed_context},
                {"type":"text","text":"fixture prompt"}
            ]),
            &json!([
                {"type":"text","text":prelude},
                {"type":"text","text":followup_context},
                {"type":"text","text":"followup"}
            ]),
        ]
    );
}

/// Keep fallback instructions available even when an ACP peer reports no compaction event.
#[tokio::test]
async fn unadvertised_instruction_append_uses_the_prompt_prelude() {
    use forge_provider_api::provider::Provider;
    let (fixture, provider, mut opened) = provider_fixture("ok", BASE_INSTRUCTIONS).await;
    send_instruction_fixture_prompt(&provider, &mut opened, "/compact").await;
    let context = send_instruction_fixture_prompt(&provider, &mut opened, "fixture prompt").await;
    let followup_context = send_instruction_fixture_prompt(&provider, &mut opened, "followup").await;
    provider
        .thread_close(operation_context(), &opened.binding.id)
        .await
        .unwrap();

    let requests = fixture.peer.requests();
    let setup = requests
        .iter()
        .find(|request| request["method"] == "session/new")
        .unwrap();
    assert!(setup["params"].get("_meta").is_none());
    assert!(setup["params"].get("systemPrompt").is_none());
    let prompts: Vec<_> = requests
        .iter()
        .filter(|request| request["method"] == "session/prompt")
        .map(|request| &request["params"]["prompt"])
        .collect();
    let prelude = forge_provider_api::forge::system_instructions_block(BASE_INSTRUCTIONS);
    assert_eq!(
        prompts,
        vec![
            &json!([{"type":"text","text":"/compact"}]),
            &json!([
                {"type":"text","text":prelude},
                {"type":"text","text":context},
                {"type":"text","text":"fixture prompt"}
            ]),
            &json!([
                {"type":"text","text":prelude},
                {"type":"text","text":followup_context},
                {"type":"text","text":"followup"}
            ]),
        ]
    );
}

/// Build a durable resume with an independent host owner for history-exclusion regressions.
fn resume_request(fixture: &WireFixture, owner: &str) -> forge_provider_api::SessionRequest {
    forge_provider_api::SessionRequest {
        thread_id: (forge_provider_api::testing::root_thread(owner)).clone(),
        mode: forge_provider_api::SessionMode::Interactive,
        workspace: fixture._directory.path().to_string_lossy().into_owned(),
        history: forge_provider_api::SessionHistory::Durable {
            resume: Some("fixture".to_owned().try_into().unwrap()),
        },
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
        system_instructions: String::new(),
        trace: Arc::new(IgnoreTrace),
    }
}

/// A login-required rejection at open stays an authentication failure, so the user is told to
/// sign in through the provider CLI instead of seeing a generic startup failure.
#[tokio::test]
async fn login_required_open_reports_authentication() {
    use forge_provider_api::provider::Provider;
    for mode in ["initialize_auth_required", "auth_required"] {
        let fixture = WireFixture::start(mode).await.unwrap();
        fixture.connection.shutdown().await.unwrap();
        let provider = super::provider::AcpProvider::new(
            forge_provider_api::ProviderId::new("antigravity").unwrap(),
            fixture.command.clone(),
            fixture._directory.path().join("runtime"),
        )
        .unwrap();
        let rejected = provider
            .thread_open(
                operation_context(),
                resume_request(&fixture, "signed-out-owner"),
            )
            .await
            .err()
            .expect("a signed-out runtime cannot open a conversation");
        assert_antigravity_login(rejected, forge_provider_api::ProviderOperation::ThreadOpen);
    }
}

/// Preserve the sign-in remedy when the disposable session cannot list models.
#[tokio::test]
async fn login_required_model_discovery_reports_authentication() {
    use forge_provider_api::provider::Provider;

    for mode in ["initialize_auth_required", "auth_required"] {
        let fixture = WireFixture::start(mode).await.unwrap();
        fixture.connection.shutdown().await.unwrap();
        let provider = super::provider::AcpProvider::new(
            forge_provider_api::ProviderId::new("antigravity").unwrap(),
            fixture.command.clone(),
            fixture._directory.path().join("runtime"),
        )
        .unwrap();
        let error = provider
            .model_list(
                operation_context(),
                forge_provider_api::QueryContext::Workspace(forge_provider_api::WorkspaceContext {
                    workspace: fixture._directory.path().to_string_lossy().into_owned(),
                }),
            )
            .await
            .unwrap_err();
        assert_antigravity_login(error, forge_provider_api::ProviderOperation::ModelList);
    }
}

/// Keep the requested operation and provide the Antigravity login command.
fn assert_antigravity_login(error: ProviderError, operation: forge_provider_api::ProviderOperation) {
    let rendered = error.to_string();
    let ProviderError::Explained { cause, .. } = error else {
        panic!("unexpected login-required failure: {error:?}");
    };
    let ProviderError::Failure(failure) = *cause else {
        panic!("unexpected login-required cause: {cause:?}");
    };
    assert_eq!(failure.operation(), operation);
    assert_eq!(
        failure.kind(),
        forge_provider_api::ProviderFailureKind::Authentication
    );
    assert!(rendered.contains("agy acp --login"));
    assert!(!rendered.contains("claude"));
    assert!(!rendered.contains("fixture-private"));
}

/// Collect each rejected open before a different owner attempts that history again.
#[tokio::test]
async fn rejected_resume_releases_private_profile_and_history() {
    use forge_provider_api::provider::Provider;
    struct OpenLease;
    impl forge_provider_api::session::ResourceLease for OpenLease {}

    let (fixture, provider, opened) = provider_fixture("missing_history", "").await;
    provider
        .thread_close(operation_context(), &opened.binding.id)
        .await
        .unwrap();
    for owner in ["first-resume", "second-resume"] {
        let lease = Arc::new(OpenLease);
        let retained = Arc::downgrade(&lease);
        let mut request = resume_request(&fixture, owner);
        request.resource_lease = Some(lease);
        let error = provider
            .thread_open(operation_context(), request)
            .await
            .err()
            .expect("the fixture has no saved conversation");
        assert!(matches!(error, ProviderError::Explained { explanation, .. }
            if explanation.as_str() == "Antigravity could not read the saved conversation."));
        assert!(retained.upgrade().is_none());
    }
    let runtime = fixture
        ._directory
        .path()
        .join("runtime/provider-runtime/antigravity");
    let profile_names = std::fs::read_dir(runtime)
        .unwrap()
        .map(|entry| entry.unwrap().file_name())
        .collect::<Vec<_>>();
    assert_eq!(profile_names, [std::ffi::OsString::from("cli")]);
}

/// Preserve the close rejection while allowing a replacement after actual collection.
#[tokio::test]
async fn rejected_native_close_releases_collected_history() {
    use forge_provider_api::provider::Provider;
    let (fixture, provider, opened) = provider_fixture("close_rejected", "").await;
    let result = provider
        .thread_close(operation_context(), &opened.binding.id)
        .await;
    assert!(matches!(result, Err(ProviderError::Failure(failure))
        if failure.operation() == forge_provider_api::ProviderOperation::ThreadClose
            && failure.kind() == forge_provider_api::ProviderFailureKind::Protocol));
    let replacement = provider
        .thread_open(operation_context(), resume_request(&fixture, "replacement-owner"))
        .await
        .unwrap();
    let reopened = provider
        .thread_open(operation_context(), resume_request(&fixture, "replacement-owner"))
        .await
        .unwrap();
    assert_ne!(reopened.binding.id, replacement.binding.id);
    assert_eq!(
        reopened.binding.provider_session_id,
        replacement.binding.provider_session_id,
    );
    assert!(provider
        .thread_close(operation_context(), &reopened.binding.id)
        .await
        .is_err());
}

/// Bound the unanswered native close below the host shutdown budget.
#[tokio::test(start_paused = true)]
async fn unanswered_native_close_reaches_force_collection_within_host_shutdown() {
    use forge_provider_api::provider::Provider;
    let (fixture, provider, opened) = provider_fixture("close_unanswered", "").await;
    let started_at = tokio::time::Instant::now();
    let result = provider
        .thread_close(operation_context(), &opened.binding.id)
        .await;
    assert!(matches!(result, Err(ProviderError::Failure(failure))
        if failure.operation() == forge_provider_api::ProviderOperation::ThreadClose
            && failure.kind() == forge_provider_api::ProviderFailureKind::Timeout));
    assert!(started_at.elapsed() < std::time::Duration::from_secs(2));
    let replacement = provider
        .thread_open(operation_context(), resume_request(&fixture, "replacement-owner"))
        .await
        .unwrap();
    assert!(provider
        .thread_close(operation_context(), &replacement.binding.id)
        .await
        .is_err());
}

/// Concurrent resumes cannot obtain two writable bindings for one native history.
#[tokio::test]
async fn concurrent_resumes_admit_only_one_history_owner() {
    use forge_provider_api::provider::Provider;
    let (fixture, provider, opened) = provider_fixture("ok", "").await;
    provider
        .thread_close(operation_context(), &opened.binding.id)
        .await
        .unwrap();
    let (left, right) = tokio::join!(
        provider.thread_open(operation_context(), resume_request(&fixture, "left-owner")),
        provider.thread_open(operation_context(), resume_request(&fixture, "right-owner"))
    );
    let (winner, rejected) = match (left, right) {
        (Ok(winner), Err(rejected)) | (Err(rejected), Ok(winner)) => (winner, rejected),
        _ => panic!("one exact durable history must admit only one writable owner"),
    };
    assert!(
        matches!(rejected, ProviderError::Failure(failure) if failure.operation() == forge_provider_api::ProviderOperation::ThreadOpen && failure.kind() == forge_provider_api::ProviderFailureKind::HistoryConflict)
    );
    assert_eq!(
        fixture
            .peer
            .requests()
            .iter()
            .filter(|request| request["method"] == "session/resume")
            .count(),
        1
    );
    provider
        .thread_close(operation_context(), &winner.binding.id)
        .await
        .unwrap();
}

/// Create a fresh bounded host operation whose dropped cancellation source stays inert.
fn operation_context() -> forge_provider_api::OperationContext {
    let (_, cancellation) = forge_provider_api::OperationCancellation::channel();
    forge_provider_api::OperationContext::new(
        forge_provider_api::OperationId::new(uuid::Uuid::new_v4().to_string()).unwrap(),
        std::time::Duration::from_secs(10),
        cancellation,
    )
    .unwrap()
}

#[derive(Default)]
struct PendingPermissionCallbacks {
    withdrawn_started: tokio::sync::Notify,
    withdrawn_release: tokio::sync::Notify,
    withdrawn_dropped: tokio::sync::Notify,
    unrelated_started: tokio::sync::Notify,
    unrelated_release: tokio::sync::Notify,
}

struct CallbackDropNotice<'callback>(&'callback tokio::sync::Notify);

impl Drop for CallbackDropNotice<'_> {
    fn drop(&mut self) {
        self.0.notify_one();
    }
}

#[async_trait]
impl AcpHandler for PendingPermissionCallbacks {
    async fn notification(&self, _: &str, _: Value) -> Result<(), super::AcpConnectionError> {
        Ok(())
    }

    async fn request(&self, method: &str, params: Value) -> Result<Value, super::AcpConnectionError> {
        assert_eq!(method, "session/request_permission");
        match params["fixture"].as_str() {
            Some("withdrawn") => {
                let _drop_notice = CallbackDropNotice(&self.withdrawn_dropped);
                self.withdrawn_started.notify_one();
                self.withdrawn_release.notified().await;
                Ok(json!({"outcome":{"outcome":"selected","optionId":"withdrawn-choice"}}))
            }
            Some("unrelated") => {
                self.unrelated_started.notify_one();
                self.unrelated_release.notified().await;
                Ok(json!({"outcome":{"outcome":"selected","optionId":"fixture-choice"}}))
            }
            _ => panic!("unexpected permission callback"),
        }
    }
}

/// A native cancellation withdraws only the approval with the matching wire request ID.
#[tokio::test]
async fn wire_permission_cancellation_drops_only_the_withdrawn_callback() {
    let directory = tempfile::tempdir().unwrap();
    let (reply_sender, mut replies) = tokio::sync::mpsc::unbounded_channel();
    let peer = super::in_memory_peer::InMemoryPeer::register(
        directory.path(),
        Box::new(move || {
            let reply_sender = reply_sender.clone();
            let mut ordinary_messages = wire_peer_handler("ok".into());
            Box::new(move |request| match request["method"].as_str() {
                Some("fixture/inject_permissions") => vec![
                    json!({"id":request["id"],"result":{}}),
                    json!({"id":"withdrawn","method":"session/request_permission","params":{"fixture":"withdrawn"}}),
                    json!({"id":"unrelated","method":"session/request_permission","params":{"fixture":"unrelated"}}),
                ],
                Some("fixture/withdraw_permission") => vec![
                    json!({"method":"$/cancel_request","params":{"requestId":"withdrawn"}}),
                    json!({"id":request["id"],"result":{}}),
                ],
                None => {
                    reply_sender.send(request.clone()).unwrap();
                    Vec::new()
                }
                Some(_) => ordinary_messages(request),
            })
        }),
    );
    let callbacks = Arc::new(PendingPermissionCallbacks::default());
    let connection = AcpConnection::launch(
        AcpLaunch {
            process_owner: None,
            command: &[peer.program()],
            environment: &BTreeMap::new(),
            workspace: directory.path().to_str().unwrap(),
            profile: None,
        },
        callbacks.clone(),
    )
    .await
    .unwrap();
    connection
        .request(
            "initialize",
            json!({"protocolVersion":1,"clientInfo":{"name":"fixture","version":"0"}}),
        )
        .await
        .unwrap();
    connection
        .request("fixture/inject_permissions", json!({}))
        .await
        .unwrap();
    callbacks.withdrawn_started.notified().await;
    callbacks.unrelated_started.notified().await;
    connection
        .request("fixture/withdraw_permission", json!({}))
        .await
        .unwrap();
    // The acknowledgement follows the cancellation frame on the same byte stream.
    callbacks.withdrawn_release.notify_one();
    callbacks.withdrawn_dropped.notified().await;
    let withdrawn = replies.recv().await.unwrap();
    assert_eq!(withdrawn["id"], "withdrawn");
    assert_eq!(withdrawn["result"]["outcome"]["outcome"], "cancelled");

    callbacks.unrelated_release.notify_one();
    let unrelated = replies.recv().await.unwrap();
    assert_eq!(unrelated["id"], "unrelated");
    assert_eq!(unrelated["result"]["outcome"]["optionId"], "fixture-choice");
    connection.shutdown().await.unwrap();
}
