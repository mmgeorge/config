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

struct TestPermissions;
#[async_trait]
impl forge_provider_api::permissions::ProviderPermissions for TestPermissions {
    async fn authorize(
        &self,
        _: &forge_provider_api::InvocationScope,
        _: forge_provider_api::permissions::PermissionRequest,
    ) -> forge_provider_api::permissions::PermissionResponse {
        forge_provider_api::permissions::PermissionResponse::Allow
    }
    async fn before_tool(
        &self,
        _: &forge_provider_api::InvocationScope,
    ) -> Result<(), ProviderError> {
        Ok(())
    }
}

struct TestTools;
#[async_trait]
impl forge_provider_api::control::ProviderTools for TestTools {
    async fn call(
        &self,
        _: &forge_provider_api::InvocationScope,
        _: forge_provider_api::control::ToolCall,
    ) -> Result<String, ProviderError> {
        Err(ProviderError::Cancelled)
    }
}

struct TestTurnControl;
#[async_trait]
impl forge_provider_api::turn_control::TurnControl for TestTurnControl {
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

struct TestHostEffects;
#[async_trait]
impl forge_provider_api::session::ProviderHostEffects for TestHostEffects {
    async fn settle_foreground(
        &self,
        _: &forge_provider_api::session::BindingId,
        _: &forge_provider_api::InvocationScope,
    ) -> Result<(), forge_provider_api::session::HostEffectFailure> {
        Ok(())
    }
}

fn prompt_request(
    binding: &forge_provider_api::BindingId,
    scope: forge_provider_api::InvocationScope,
    receipt: Arc<FixtureReceipt>,
    input: forge_provider_api::input::PromptInput,
) -> forge_provider_api::session::PromptRequest {
    let (dispatch, _) = forge_provider_api::submission::DispatchReporter::channel(
        binding.clone(),
        scope.clone(),
        receipt.id.clone(),
        receipt.attempt_id().unwrap(),
    );
    forge_provider_api::session::PromptRequest {
        thread_id: (scope).thread_id.clone(),
        turn_id: (scope)
            .turn_id()
            .expect("admitted scope has a nonempty host turn identity"),

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
        input,
        recovered_inputs: Vec::new(),
        instructions: "fixture instructions".into(),
        attachments: Vec::new(),
        receipt,
        permissions: Arc::new(TestPermissions),
        provider_tools: Arc::new(TestTools),
        turn_control: Arc::new(TestTurnControl),
        host_effects: Arc::new(TestHostEffects),
    }
}

fn receipt(id: &str) -> Arc<FixtureReceipt> {
    Arc::new(FixtureReceipt {
        id: SubmissionId::try_from(id.to_owned()).unwrap(),
        state: Mutex::new(ReceiptState::default()),
        acceptance_started: Default::default(),
        acceptance_release: Default::default(),
    })
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
    let mut held_authentication: Option<Value> = None;
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
        if method == "authenticate" && mode == "held_auth" {
            held_authentication = Some(request_id);
            return outgoing;
        }
        if method == "fixture/release_auth"
            && let Some(authentication_id) = held_authentication.take()
        {
            outgoing.push(json!({"id":authentication_id,"result":{}}));
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
    _directory: tempfile::TempDir,
    connection: AcpConnection,
    command: Vec<String>,
    peer: super::in_memory_peer::InMemoryPeer,
}

impl WireFixture {
    async fn start(mode: &str) -> Result<Self, Box<dyn std::error::Error>> {
        let directory = tempfile::tempdir()?;
        let mode = mode.to_owned();
        let peer = super::in_memory_peer::InMemoryPeer::register(
            directory.path(),
            "2.1.302",
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

    /// Build a provider using the production native-executable resolver and a real version peer.
    fn claude_provider(&self, runtime_root: std::path::PathBuf) -> super::provider::AcpProvider {
        let mut runtime = super::runtime::AcpRuntime::new(
            super::AcpKind::Claude,
            self.command.clone(),
            &runtime_root,
        );
        runtime.select_native_command(self.command.clone());
        super::provider::AcpProvider::from_runtime(runtime, runtime_root)
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

/// Observe automatic collection without issuing a public cleanup command.
async fn next_completion(stream: &mut forge_provider_api::ProviderEventStream) {
    async {
        loop {
            let envelope = stream.recv().await.unwrap().unwrap();
            if let forge_provider_api::ProviderEvent::Turn {
                event: forge_provider_api::TurnEvent::Completed(_),
                ..
            } = envelope.event
            {
                return;
            }
        }
    }
    .await
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

/// Workspace discovery reads models from a disposable, unpersisted session before any binding exists.
#[tokio::test]
async fn workspace_model_list_reads_a_disposable_unpersisted_session() {
    use forge_provider_api::provider::Provider;
    let fixture = WireFixture::start("ok").await.unwrap();
    fixture.connection.shutdown().await.unwrap();
    let provider = fixture.claude_provider(fixture._directory.path().join("runtime"));
    let workspace = fixture._directory.path().to_string_lossy().into_owned();

    let models = provider
        .model_list(
            operation_context(),
            forge_provider_api::QueryContext::Workspace(forge_provider_api::WorkspaceContext {
                workspace: workspace.clone(),
            }),
        )
        .await
        .unwrap();

    let summary: Vec<_> = models
        .iter()
        .map(|model| (model.id.as_str(), model.label.as_deref(), model.is_default))
        .collect();
    assert_eq!(
        summary,
        [
            ("default-model", None, true),
            ("other", Some("Other 1.0"), false)
        ]
    );
    let session_new: Vec<_> = fixture
        .peer
        .requests()
        .into_iter()
        .filter(|request| request["method"] == "session/new")
        .collect();
    let discovery = session_new
        .last()
        .expect("discovery opened a native session");
    assert_eq!(discovery["params"]["cwd"], workspace.as_str());
    assert_eq!(discovery["params"]["mcpServers"], json!([]));
    assert_eq!(
        discovery["params"]["_meta"]["claudeCode"]["options"],
        json!({"persistSession":false,"strictMcpConfig":true})
    );
    provider.shutdown(operation_context()).await.unwrap();
}

/// Invalid options must leave existing history, resources and invocation admission untouched.
#[tokio::test]
async fn unsupported_options_reject_before_native_or_profile_changes() {
    use forge_provider_api::{
        capability::{ProviderFeature, UnsupportedCapability},
        provider::Provider,
    };
    let fixture = WireFixture::start("native_instructions").await.unwrap();
    fixture.connection.shutdown().await.unwrap();
    let runtime = fixture._directory.path().join("runtime");
    let provider = fixture.claude_provider(runtime.clone());
    let mut initial = resume_request(&fixture, "preflight-owner");
    initial.history = forge_provider_api::SessionHistory::Durable { resume: None };
    let opened = provider
        .thread_open(operation_context(), initial.clone())
        .await
        .unwrap();
    let binding = opened.binding.id.clone();
    let mut events = opened.events;
    let mut cases = Vec::new();
    let mut sandbox = initial.configuration.clone();
    sandbox.access.sandbox = true;
    cases.push((sandbox, ProviderFeature::OsSandbox));
    let mut isolation = initial.configuration.clone();
    isolation.access.isolation_mode = Some("unsupported".into());
    cases.push((isolation, ProviderFeature::IsolationMode));
    let mut context = initial.configuration.clone();
    context.model.context_window = Some("unsupported".into());
    cases.push((context, ProviderFeature::ContextWindowSelection));
    let mut tier = initial.configuration.clone();
    tier.model.service_tier = forge_provider_api::ServiceTier::Fast;
    cases.push((tier, ProviderFeature::ServiceTier));
    let mut mcp = initial.configuration.clone();
    mcp.resources.mcp.push(forge_provider_api::McpLaunch {
        name: "external".into(),
        transport: forge_provider_api::McpTransport::Http {
            url: "http://localhost.invalid".into(),
            headers: Default::default(),
        },
        timeout: Some(std::time::Duration::from_secs(1)),
        working_directory: None,
        tools: None,
    });
    cases.push((mcp, ProviderFeature::McpLaunchRequirements));
    let before = fixture.peer.requests();
    let profiles = runtime.join("provider-runtime/claude");
    let profile_count = std::fs::read_dir(&profiles).unwrap().count();
    for (index, (configuration, expected)) in cases.into_iter().enumerate() {
        // The same owner would be closed before validating a replacement in the old path.
        let mut replacement = initial.clone();
        replacement.configuration = configuration.clone();
        assert!(
            matches!(provider.thread_open(operation_context(), replacement).await,
            Err(ProviderError::Unsupported(UnsupportedCapability(feature))) if feature == expected)
        );
        assert!(
            matches!(provider.thread_configure(operation_context(), &binding, configuration.clone()).await,
            Err(ProviderError::Unsupported(UnsupportedCapability(feature))) if feature == expected)
        );
        assert!(
            matches!(provider.thread_fork(operation_context(), forge_provider_api::SessionForkRequest {
            source: forge_provider_api::ForkSource::Binding(binding.clone()),
            target: forge_provider_api::ForkTarget { thread_id: forge_provider_api::testing::root_thread("preflight-fork"),
                workspace: initial.workspace.clone(), system_instructions: String::new(), configuration: configuration.clone() },
        }).await, Err(ProviderError::Unsupported(UnsupportedCapability(feature))) if feature == expected)
        );
        if expected != ProviderFeature::McpLaunchRequirements {
            let mut prompt = prompt_request(
                &binding,
                forge_provider_api::InvocationScope::new(
                    initial.thread_id.clone(),
                    format!("invalid-{index}"),
                ),
                receipt(&format!("invalid-{index}")),
                forge_provider_api::PromptInput::Text("must not dispatch".into()),
            );
            prompt.configuration.model = configuration.model;
            prompt.configuration.access = configuration.access;
            prompt.tool_endpoint = Some(forge_provider_api::HostToolEndpoint {
                url: "http://127.0.0.1:41000/mcp".into(),
                headers: Default::default(),
            });
            assert!(
                matches!(provider.turn_start(operation_context(), &binding, prompt).await,
                Err(ProviderError::Unsupported(UnsupportedCapability(feature))) if feature == expected)
            );
        }
        assert_eq!(
            fixture.peer.requests(),
            before,
            "preflight performed native I/O"
        );
        assert_eq!(
            std::fs::read_dir(&profiles).unwrap().count(),
            profile_count,
            "preflight allocated a profile"
        );
    }
    let prompt = prompt_request(
        &binding,
        forge_provider_api::InvocationScope::new(initial.thread_id, "after-preflight"),
        receipt("after-preflight"),
        forge_provider_api::PromptInput::Text("still usable".into()),
    );
    provider
        .turn_start(operation_context(), &binding, prompt)
        .await
        .unwrap();
    next_completion(&mut events).await;
    provider
        .thread_close(operation_context(), &binding)
        .await
        .unwrap();
    provider.shutdown(operation_context()).await.unwrap();
}
