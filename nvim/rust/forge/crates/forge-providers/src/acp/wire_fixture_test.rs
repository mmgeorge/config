use super::{AcpConnection, AcpHandler, AcpLaunch, submission::AcpSubmission};
use async_trait::async_trait;
use forge_provider_api::submission::InputReceipt;
use forge_provider_api::{ProviderError, RecoveryToken, SubmissionId};
use serde_json::{Value, json};
use std::{
    collections::BTreeMap,
    sync::{Arc, Mutex},
};

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
        Arc::new(Self {
            id: SubmissionId::try_from("fixture-submission".to_owned()).unwrap(),
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
    Box::new(move |request| {
        let method = request["method"].as_str().unwrap_or_default();
        let mut outgoing = Vec::new();
        let Some(request_id) = request.get("id").filter(|id| !id.is_null()).cloned() else {
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
            let session_capabilities = match mode.as_str() {
                "native_instructions" => Some(json!({"fork":{},"systemPrompt":{}})),
                "fork_no_instructions" => Some(json!({"fork":{}})),
                "disabled_extensions" => {
                    Some(json!({"systemPrompt":{},"fork":false,"subagents":null}))
                }
                _ => None,
            };
            let mut capabilities = json!({"loadSession":true});
            if let Some(session_capabilities) = session_capabilities {
                capabilities["sessionCapabilities"] = session_capabilities;
            }
            result = json!({
                "protocolVersion":1,
                "agentInfo":{"version":"99.0.0"},
                "agentCapabilities":capabilities,
            });
        }
        if (method == "session/new" || method == "session/load") && mode == "auth_required" {
            outgoing.push(json!({
                "id":request_id,
                "error":{"code":-32000,"message":"Authentication required"},
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
        if method == "session/new" || method == "session/load" {
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

struct WireFixture {
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

/// Open a production ACP binding against the deterministic native-wire peer.
async fn provider_fixture(
    mode: &str,
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
                system_instructions: String::new(),
                trace: Arc::new(IgnoreTrace),
            },
        )
        .await
        .unwrap();
    (fixture, provider, opened)
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
    let fixture = WireFixture::start("auth_required").await.unwrap();
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
    let failure = match rejected {
        ProviderError::Failure(failure) => failure,
        ProviderError::PreparationCleanup { cause, .. } => match *cause {
            ProviderError::Failure(failure) => failure,
            other => panic!("unexpected open failure: {other:?}"),
        },
        other => panic!("unexpected open failure: {other:?}"),
    };
    assert_eq!(
        failure.kind(),
        forge_provider_api::ProviderFailureKind::Authentication
    );
    assert!(failure.to_string().contains("not signed in"));
}

/// Concurrent resumes cannot obtain two writable bindings for one native history.
#[tokio::test]
async fn concurrent_resumes_admit_only_one_history_owner() {
    use forge_provider_api::provider::Provider;
    let (fixture, provider, opened) = provider_fixture("ok").await;
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
            .filter(|request| request["method"] == "session/load")
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
