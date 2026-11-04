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
}

struct FixtureReceipt {
    id: SubmissionId,
    state: Mutex<ReceiptState>,
}

impl FixtureReceipt {
    fn new() -> Arc<Self> {
        Arc::new(Self {
            id: SubmissionId::try_from("fixture-submission".to_owned()).unwrap(),
            state: Mutex::new(ReceiptState::default()),
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
        let mut state = self.state.lock().unwrap();
        if state.fail_acceptance {
            return Err(forge_provider_api::ProviderFailure::new(
                forge_provider_api::ProviderOperation::InputSubmit,
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
        scope,
        dispatch,
        mode: forge_provider_api::input::PromptMode::Chat,
        configuration: forge_provider_api::session::InvocationConfiguration {
            model: forge_provider_api::session::ModelSelection::default(),
            access: forge_provider_api::access::AccessPolicy::default(),
            approval: forge_provider_api::approval::ApprovalMode::Ask,
        },
        task: None,
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
    })
}

#[test]
fn invocation_endpoint_replaces_only_the_native_host_server() {
    let configuration = serde_json::json!({
        "sessionId":"retained-history",
        "mcpServers":[
            {"type":"http","name":"forge-harness-control","url":"http://stale.invalid","headers":[]},
            {"type":"stdio","name":"workspace-resource","command":"resource-server","args":["--safe"]}
        ]
    });
    let endpoint = forge_provider_api::session::HostToolEndpoint {
        url: "http://invocation.invalid/tools".into(),
        headers: std::collections::BTreeMap::from([("authorization".into(), "ephemeral".into())]),
    };
    let rebound =
        super::conversation::configuration_with_tool_endpoint(&configuration, Some(&endpoint))
            .unwrap();
    assert_eq!(rebound["sessionId"], "retained-history");
    assert_eq!(rebound["mcpServers"][0]["name"], "forge-harness-control");
    assert_eq!(rebound["mcpServers"][0]["url"], endpoint.url);
    assert_eq!(rebound["mcpServers"][0]["headers"][0]["value"], "ephemeral");
    assert_eq!(rebound["mcpServers"][1], configuration["mcpServers"][1]);

    let cleared = super::conversation::configuration_with_tool_endpoint(&rebound, None).unwrap();
    assert_eq!(
        cleared["mcpServers"],
        serde_json::json!([
            {"type":"stdio","name":"workspace-resource","command":"resource-server","args":["--safe"]}
        ])
    );
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

struct WireFixture {
    _directory: tempfile::TempDir,
    connection: AcpConnection,
    command: Vec<String>,
}

impl WireFixture {
    async fn start(mode: &str) -> Result<Self, Box<dyn std::error::Error>> {
        let directory = tempfile::tempdir()?;
        let script = directory.path().join("peer.cjs");
        let mode_path = directory.path().join("mode");
        std::fs::write(&mode_path, mode)?;
        std::fs::write(
            &script,
            r#"
const fs=require('node:fs');const readline=require('node:readline');
const write=value=>process.stdout.write(JSON.stringify({jsonrpc:'2.0',...value})+'\n');
let heldPrompt;
readline.createInterface({input:process.stdin}).on('line',line=>{
 const request=JSON.parse(line);fs.appendFileSync(process.argv[2]+'.requests',JSON.stringify(request)+'\n');
 if(request.id===undefined){if(request.method==='session/cancel'&&heldPrompt){write({id:heldPrompt,result:{stopReason:'cancelled'}});heldPrompt=undefined;}return;}let result={};
 if(request.method==='fixture/release'&&heldPrompt){write({id:heldPrompt,result:{stopReason:'end_turn'}});heldPrompt=undefined;}
 if(request.method==='initialize')result={protocolVersion:1,agentInfo:{version:'99.0.0'},agentCapabilities:{loadSession:true}};
 if(request.method==='session/load'&&fs.readFileSync(process.argv[2],'utf8')==='load_applied_error'){
  fs.writeFileSync(process.argv[2]+'.endpoint-applied',JSON.stringify(request.params.mcpServers));
  write({id:request.id,error:{code:-32603,message:'private failure after native endpoint apply'}});return;
 }
 if(request.method==='session/new'||request.method==='session/load')result={sessionId:'fixture',models:{currentModelId:'default-model',availableModels:[{modelId:'default-model'},{modelId:'other'}]},configOptions:[{id:'effort',currentValue:'medium',options:[{value:'low'},{value:'medium'},{value:'high'}]}]};
 if(request.method==='session/prompt'||request.method==='_session/steering'){
  const mode=fs.readFileSync(process.argv[2],'utf8');
  if(mode==='held'&&request.method==='session/prompt'){heldPrompt=request.id;return;}
  if(mode==='remote_error'){write({id:request.id,error:{code:-32603,message:'private native error'}});return;}
  if(mode==='invalid_request'){write({id:request.id,error:{code:-32600,message:'private invalid request detail'}});return;}
  if(request.method==='_session/steering')result=mode==='promptRequired'?{outcome:'promptRequired',reason:'noRunningTurn'}:{outcome:mode};
  else result=mode==='invalid'?{}:{stopReason:mode==='cancelled'?'cancelled':'end_turn'};
 }
 write({id:request.id,result});
});
"#,
        )?;
        let node = forge_shell::process::executable("node")
            .ok_or("Node.js is required by the ACP wire fixture")?;
        let command = vec![
            node.to_string_lossy().into_owned(),
            script.to_string_lossy().into_owned(),
            mode_path.to_string_lossy().into_owned(),
        ];
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
                crate::error::acp(error, forge_provider_api::ProviderOperation::SessionOpen)
            })?;
        self.connection
            .request("session/new", json!({"cwd":".","mcpServers":[]}))
            .await
            .map_err(|error| {
                crate::error::acp(error, forge_provider_api::ProviderOperation::SessionOpen)
            })?;
        Ok(())
    }
}

struct IgnoreTrace;

impl forge_provider_api::trace::ProviderTrace for IgnoreTrace {
    fn record(&self, _: forge_provider_api::trace::ProviderTraceRecord) {}
}

struct FailedHostEffects;

#[async_trait]
impl forge_provider_api::session::ProviderHostEffects for FailedHostEffects {
    async fn settle_foreground(
        &self,
        _: &forge_provider_api::BindingId,
        _: &forge_provider_api::InvocationScope,
    ) -> Result<(), forge_provider_api::session::HostEffectFailure> {
        Err(forge_provider_api::session::HostEffectFailure::Uncertain)
    }
}

/// Keep provider-native collection independent of the host foreground-effect barrier.
#[tokio::test]
async fn native_settlement_does_not_wait_for_host_effects() {
    use forge_provider_api::provider::Provider;
    use forge_provider_api::session::{ResourceSelection, SessionMode, SessionRequest};
    use forge_provider_api::{ExecutionOwner, InvocationScope};

    let fixture = WireFixture::start("ok").await.unwrap();
    fixture.connection.shutdown().await.unwrap();
    let provider = super::provider::AcpProvider::new(
        forge_provider_api::ProviderId::new("antigravity").unwrap(),
        fixture.command.clone(),
        fixture._directory.path().join("runtime"),
    )
    .unwrap();
    let owner = ExecutionOwner::root("session");
    let binding = provider
        .session_open(
            operation_context(),
            SessionRequest {
                owner: owner.clone(),
                mode: SessionMode::Interactive,
                workspace: fixture._directory.path().to_string_lossy().into_owned(),
                history: forge_provider_api::SessionHistory::Durable { resume: None },
                configuration: forge_provider_api::SessionConfiguration {
                    model: Default::default(),
                    access: Default::default(),
                    approval: forge_provider_api::approval::ApprovalMode::Ask,
                    resources: ResourceSelection::default(),
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
    let mut stream = binding.events;
    let binding = binding.binding;
    let rejected_receipt = receipt("rejected-submission");
    let mut rejected = prompt_request(
        &binding.id,
        InvocationScope::new(owner.clone(), "rejected-exchange"),
        rejected_receipt.clone(),
        forge_provider_api::PromptInput::Text("test".into()),
    );
    rejected.configuration.model.context_window = Some("fixture-window".into());
    assert!(matches!(
        provider
            .input_submit(operation_context(), &binding.id, rejected)
            .await,
        Err(ProviderError::PreparationFailed(_))
    ));
    assert!(rejected_receipt.state.lock().unwrap().rejected);
    let rejected_scope = match stream.recv().await.unwrap().unwrap().event {
        forge_provider_api::ProviderEvent::Settled { settlement } => settlement.scope,
        event => panic!("unexpected rejected prompt event: {event:?}"),
    };
    provider
        .cancel(
            operation_context(),
            &binding.id,
            forge_provider_api::CancelRequest {
                scope: rejected_scope,
            },
        )
        .await
        .unwrap();
    stream.check().unwrap();
    let receipt = FixtureReceipt::new();
    let mut request = prompt_request(
        &binding.id,
        InvocationScope::new(owner, "exchange"),
        receipt.clone(),
        forge_provider_api::PromptInput::Text("test".into()),
    );
    request.host_effects = Arc::new(FailedHostEffects);
    let scope = request.scope.clone();
    provider
        .input_submit(operation_context(), &binding.id, request)
        .await
        .unwrap();
    assert!(receipt.state.lock().unwrap().accepted);
    let settlement = next_settlement(&mut stream).await;
    assert!(matches!(
        settlement.result,
        forge_provider_api::ExecutionResult::Completed { .. }
    ));
    let repeated = provider
        .cancel(
            operation_context(),
            &binding.id,
            forge_provider_api::CancelRequest { scope },
        )
        .await
        .unwrap();
    assert_eq!(settlement.watermark, repeated.watermark);
    provider
        .session_close(operation_context(), &binding.id)
        .await
        .unwrap();
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

/// Confirm tracked transport handoff while ACP's terminal-only receipt is pending.
#[tokio::test]
async fn prompt_dispatch_does_not_wait_for_terminal_acceptance() {
    let fixture = WireFixture::start("held").await.unwrap();
    fixture.ready().await.unwrap();
    let receipt = FixtureReceipt::new();
    let (dispatch, mut observer) = forge_provider_api::submission::DispatchReporter::channel(
        forge_provider_api::BindingId::new("binding").unwrap(),
        forge_provider_api::InvocationScope::new(
            forge_provider_api::ExecutionOwner::root("session"),
            "invocation",
        ),
        receipt.id.clone(),
        receipt.attempt_id().unwrap(),
    );
    let prompt = fixture.connection.submit(
        AcpSubmission::Prompt,
        json!({"sessionId":"fixture","prompt":[{"type":"text","text":"prompt"}]}),
        Some(receipt.clone()),
        Some(&dispatch),
        None,
    );
    tokio::pin!(prompt);
    tokio::time::timeout(std::time::Duration::from_secs(2), async {
        tokio::select! {
            confirmation = observer.wait() => {
                assert_eq!(confirmation.unwrap().target(), &forge_provider_api::submission::DispatchTarget::Input(receipt.id.clone()));
            }
            result = &mut prompt => panic!("held prompt completed before release: {result:?}"),
        }
    })
    .await
    .expect("transport handoff must not wait for terminal response");
    {
        let state = receipt.state.lock().unwrap();
        assert!(state.sending && !state.accepted);
    }
    fixture
        .connection
        .request("fixture/release", json!({}))
        .await
        .unwrap();
    prompt.await.unwrap();
    assert!(receipt.state.lock().unwrap().accepted);
    fixture.connection.shutdown().await.unwrap();
}

#[tokio::test]
async fn idle_steering_rejection_proves_input_not_consumed() {
    let fixture = WireFixture::start("promptRequired").await.unwrap();
    fixture.ready().await.unwrap();
    let receipt = FixtureReceipt::new();
    let result = fixture
        .connection
        .submit_steering(
            json!({"sessionId":"fixture","prompt":[{"type":"text","text":"steer"}]}),
            Some(receipt.clone()),
        )
        .await;
    assert!(
        matches!(result, Err(ProviderError::PreparationFailed(cause)) if matches!(*cause, ProviderError::Failure(failure) if failure.operation() == forge_provider_api::ProviderOperation::InputSteer && failure.kind() == forge_provider_api::ProviderFailureKind::Rejected && failure.phase() == forge_provider_api::ProviderPhase::Observation && failure.disposition() == forge_provider_api::FailureDisposition::Unchanged))
    );
    {
        let state = receipt.state.lock().unwrap();
        assert!(state.sending && state.rejected && !state.accepted && !state.failed);
    }
    fixture.connection.shutdown().await.unwrap();
}

/// A malformed response after native send must never become replay-eligible admission proof.
#[tokio::test]
async fn malformed_native_outcomes_preserve_uncertainty_in_return_and_receipt() {
    use forge_provider_api::{
        FailureDisposition, ProviderFailureKind, ProviderOperation, ProviderPhase,
    };
    for (kind, operation) in [
        (AcpSubmission::Prompt, ProviderOperation::InputSubmit),
        (AcpSubmission::Steering, ProviderOperation::InputSteer),
    ] {
        let fixture = WireFixture::start("invalid").await.unwrap();
        fixture.ready().await.unwrap();
        let receipt = FixtureReceipt::new();
        let result = fixture
            .connection
            .submit(
                kind,
                json!({"sessionId":"fixture","prompt":[]}),
                Some(receipt.clone()),
                None,
                None,
            )
            .await;
        assert!(
            matches!(result, Err(ProviderError::Failure(failure)) if failure.operation() == operation && failure.kind() == ProviderFailureKind::Protocol && failure.phase() == ProviderPhase::Observation && failure.disposition() == FailureDisposition::Uncertain)
        );
        {
            let state = receipt.state.lock().unwrap();
            assert!(state.sending && state.failed && !state.accepted && !state.rejected);
            assert!(
                matches!(state.failure.as_ref(), Some(ProviderError::Failure(failure)) if failure.operation() == operation && failure.kind() == ProviderFailureKind::Protocol && failure.phase() == ProviderPhase::Observation && failure.disposition() == FailureDisposition::Uncertain)
            );
        }
        fixture.connection.shutdown().await.unwrap();
    }
}

/// Receipt persistence failure cannot undo native acceptance or claim unchanged delivery.
#[tokio::test]
async fn accepted_receipt_failure_preserves_persistence_uncertainty() {
    use forge_provider_api::{
        FailureDisposition, ProviderFailureKind, ProviderOperation, ProviderPhase,
    };
    let fixture = WireFixture::start("ok").await.unwrap();
    fixture.ready().await.unwrap();
    let receipt = FixtureReceipt::new();
    receipt.state.lock().unwrap().fail_acceptance = true;
    let result = fixture
        .connection
        .submit(
            AcpSubmission::Prompt,
            json!({"sessionId":"fixture","prompt":[]}),
            Some(receipt.clone()),
            None,
            None,
        )
        .await;
    assert!(
        matches!(result, Err(ProviderError::Failure(failure)) if failure.operation() == ProviderOperation::InputSubmit && failure.kind() == ProviderFailureKind::Runtime && failure.phase() == ProviderPhase::Persistence && failure.disposition() == FailureDisposition::Uncertain)
    );
    assert!(
        matches!(receipt.state.lock().unwrap().failure.as_ref(), Some(ProviderError::Failure(failure)) if failure.kind() == ProviderFailureKind::Runtime && failure.phase() == ProviderPhase::Persistence && failure.disposition() == FailureDisposition::Uncertain)
    );
    fixture.connection.shutdown().await.unwrap();
}

#[tokio::test]
async fn native_remote_errors_are_redacted_and_recorded_as_failures() {
    let fixture = WireFixture::start("remote_error").await.unwrap();
    fixture.ready().await.unwrap();
    let receipt = FixtureReceipt::new();
    let error = fixture
        .connection
        .submit(
            AcpSubmission::Prompt,
            json!({"sessionId":"fixture","prompt":[{"type":"text","text":"prompt"}]}),
            Some(receipt.clone()),
            None,
            None,
        )
        .await
        .unwrap_err();
    assert!(!error.to_string().contains("private native error"));
    {
        let state = receipt.state.lock().unwrap();
        assert!(state.sending && state.failed && !state.accepted && !state.rejected);
    }
    fixture.connection.shutdown().await.unwrap();
}

#[tokio::test]
async fn typed_json_rpc_rejection_maps_without_exposing_native_message() {
    let fixture = WireFixture::start("invalid_request").await.unwrap();
    fixture.ready().await.unwrap();
    let native = fixture
        .connection
        .request("session/prompt", json!({"sessionId":"fixture","prompt":[]}))
        .await
        .unwrap_err();
    let normalized = crate::error::acp(native, forge_provider_api::ProviderOperation::InputSubmit);
    assert!(
        matches!(normalized, ProviderError::Failure(ref failure) if failure.kind() == forge_provider_api::ProviderFailureKind::Rejected)
    );
    assert!(
        !normalized
            .to_string()
            .contains("private invalid request detail")
    );
    fixture.connection.shutdown().await.unwrap();
}

#[test]
fn mcp_native_reason_text_is_replaced_by_normalized_message() {
    let canary = "private native payload: /home/user/secret-token";
    let detail = super::provider::normalized_mcp_reason(Some(canary));
    assert_eq!(detail.as_str(), "MCP resource is unavailable.");
    assert!(!detail.as_str().contains(canary));
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
        .session_open(
            operation_context(),
            forge_provider_api::SessionRequest {
                owner: forge_provider_api::ExecutionOwner::root("fixture-owner"),
                mode: forge_provider_api::SessionMode::Interactive,
                workspace: fixture._directory.path().to_string_lossy().into_owned(),
                history: forge_provider_api::SessionHistory::Durable { resume: None },
                configuration: forge_provider_api::SessionConfiguration {
                    model: Default::default(),
                    access: Default::default(),
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
async fn next_settlement(
    stream: &mut forge_provider_api::ProviderEventStream,
) -> forge_provider_api::InvocationSettlement {
    tokio::time::timeout(std::time::Duration::from_secs(2), async {
        loop {
            let event = stream.recv().await.unwrap().unwrap();
            if let forge_provider_api::ProviderEvent::Settled { settlement } = event.event {
                assert_eq!(settlement.watermark, event.sequence);
                return *settlement;
            }
        }
    })
    .await
    .expect("native worker must automatically settle")
}

/// Invalid invocation authority must reject before any native input or receipt effect.
#[tokio::test]
async fn input_admission_preserves_wrong_invocation_and_closed_binding_causes() {
    use forge_provider_api::provider::Provider;
    use forge_provider_api::{
        FailureDisposition, ProviderFailureKind, ProviderOperation, ProviderPhase,
    };
    let (fixture, provider, opened) = provider_fixture("ok").await;
    let binding = opened.binding.id;
    for invalid in 0..4 {
        let receipt = receipt(&format!("invalid-{invalid}"));
        let mut request = prompt_request(
            &binding,
            forge_provider_api::InvocationScope::new(
                forge_provider_api::ExecutionOwner::root("fixture-owner"),
                "invalid",
            ),
            receipt.clone(),
            forge_provider_api::PromptInput::Text("must not dispatch".into()),
        );
        match invalid {
            0 => request.scope.owner = forge_provider_api::ExecutionOwner::root("different-owner"),
            1 => request.receipt = FixtureReceipt::new(),
            2 => {
                request.attempt_id =
                    forge_provider_api::DeliveryAttemptId::new("different-attempt").unwrap()
            }
            3 => {
                let (dispatch, _) = forge_provider_api::submission::DispatchReporter::channel(
                    forge_provider_api::BindingId::new("different-binding").unwrap(),
                    request.scope.clone(),
                    request.submission_id.clone(),
                    request.attempt_id.clone(),
                );
                request.dispatch = dispatch;
            }
            _ => unreachable!(),
        }
        assert!(
            matches!(provider.input_submit(operation_context(), &binding, request).await, Err(ProviderError::Failure(failure)) if failure.operation() == ProviderOperation::InputSubmit && failure.kind() == ProviderFailureKind::WrongInvocation && failure.phase() == ProviderPhase::Admission && failure.disposition() == FailureDisposition::Unchanged)
        );
        assert!(!receipt.state.lock().unwrap().sending);
    }
    let idle = forge_provider_api::SteeringRequest {
        scope: forge_provider_api::InvocationScope::new(
            forge_provider_api::ExecutionOwner::root("fixture-owner"),
            "idle-steer",
        ),
        delivery: forge_provider_api::SteeringDelivery::Context,
        input: forge_provider_api::PromptInput::Text("must not steer".into()),
        attachments: Vec::new(),
        task: None,
    };
    assert!(
        matches!(provider.input_steer(operation_context(), &binding, idle.clone()).await, Err(ProviderError::Failure(failure)) if failure.operation() == ProviderOperation::InputSteer && failure.kind() == ProviderFailureKind::WrongInvocation && failure.phase() == ProviderPhase::Admission && failure.disposition() == FailureDisposition::Unchanged)
    );
    let log = std::fs::read_to_string(fixture._directory.path().join("mode.requests")).unwrap();
    assert!(
        !log.lines()
            .any(|line| line.contains("session/prompt") || line.contains("_session/steering"))
    );
    provider
        .session_close(operation_context(), &binding)
        .await
        .unwrap();
    let request = prompt_request(
        &binding,
        idle.scope.clone(),
        receipt("closed"),
        forge_provider_api::PromptInput::Text("closed".into()),
    );
    assert!(
        matches!(provider.input_submit(operation_context(), &binding, request).await, Err(ProviderError::Failure(failure)) if failure.kind() == ProviderFailureKind::BindingClosed && failure.disposition() == FailureDisposition::Closed)
    );
    assert!(
        matches!(provider.input_steer(operation_context(), &binding, idle).await, Err(ProviderError::Failure(failure)) if failure.kind() == ProviderFailureKind::BindingClosed && failure.disposition() == FailureDisposition::Closed)
    );
    provider.shutdown(operation_context()).await.unwrap();
}

/// A stale cancel must replay completed evidence while a successor continues running.
#[tokio::test]
async fn automatic_settlement_and_stale_cancel_preserve_successor() {
    use forge_provider_api::provider::Provider;
    let (fixture, provider, opened) = provider_fixture("ok").await;
    let binding = opened.binding.id;
    let mut events = opened.events;
    let owner = forge_provider_api::ExecutionOwner::root("fixture-owner");
    let first = prompt_request(
        &binding,
        forge_provider_api::InvocationScope::new(owner.clone(), "first"),
        receipt("first"),
        forge_provider_api::PromptInput::Text("first".into()),
    );
    let first_scope = first.scope.clone();
    provider
        .input_submit(operation_context(), &binding, first)
        .await
        .unwrap();
    let settled = next_settlement(&mut events).await;
    assert!(matches!(
        settled.result,
        forge_provider_api::ExecutionResult::Completed { .. }
    ));

    std::fs::write(fixture._directory.path().join("mode"), "held").unwrap();
    let second = prompt_request(
        &binding,
        forge_provider_api::InvocationScope::new(owner, "second"),
        receipt("second"),
        forge_provider_api::PromptInput::Text("second".into()),
    );
    let second_scope = second.scope.clone();
    let submitted_provider = provider.clone();
    let submitted_binding = binding.clone();
    let submitted = tokio::spawn(async move {
        submitted_provider
            .input_submit(operation_context(), &submitted_binding, second)
            .await
    });
    tokio::time::timeout(std::time::Duration::from_secs(2), async {
        loop {
            let log =
                std::fs::read_to_string(fixture._directory.path().join("mode.requests")).unwrap();
            if log
                .lines()
                .filter(|line| line.contains("session/prompt"))
                .count()
                == 2
            {
                break;
            }
            tokio::task::yield_now().await;
        }
    })
    .await
    .unwrap();
    let configuration = forge_provider_api::SessionConfiguration {
        model: Default::default(),
        access: Default::default(),
        approval: forge_provider_api::approval::ApprovalMode::Ask,
        resources: Default::default(),
    };
    assert!(
        matches!(provider.session_configure(operation_context(), &binding, configuration).await, Err(ProviderError::Failure(failure)) if failure.kind() == forge_provider_api::ProviderFailureKind::ExecutionActive)
    );
    let overlapping = prompt_request(
        &binding,
        forge_provider_api::InvocationScope::new(second_scope.owner.clone(), "overlapping"),
        receipt("overlapping"),
        forge_provider_api::PromptInput::Text("must not overlap".into()),
    );
    assert!(
        matches!(provider.input_submit(operation_context(), &binding, overlapping).await, Err(ProviderError::Failure(failure)) if failure.kind() == forge_provider_api::ProviderFailureKind::ExecutionActive && failure.phase() == forge_provider_api::ProviderPhase::Admission && failure.disposition() == forge_provider_api::FailureDisposition::Unchanged)
    );
    assert!(!submitted.is_finished());
    let repeated = provider
        .cancel(
            operation_context(),
            &binding,
            forge_provider_api::CancelRequest { scope: first_scope },
        )
        .await
        .unwrap();
    assert_eq!(repeated.watermark, settled.watermark);
    assert!(matches!(
        repeated.result,
        forge_provider_api::ExecutionResult::Completed { .. }
    ));
    assert!(!submitted.is_finished());
    let cancelled = provider
        .cancel(
            operation_context(),
            &binding,
            forge_provider_api::CancelRequest {
                scope: second_scope.clone(),
            },
        )
        .await
        .unwrap();
    assert_eq!(cancelled.scope, second_scope);
    assert!(matches!(
        cancelled.result,
        forge_provider_api::ExecutionResult::Cancelled { .. }
    ));
    submitted.await.unwrap().unwrap();
    provider
        .session_close(operation_context(), &binding)
        .await
        .unwrap();
}

/// Configuration preserves the public generation and its installed observer.
#[tokio::test]
async fn settled_configuration_preserves_binding_and_event_reader() {
    use forge_provider_api::provider::Provider;
    let (_fixture, provider, opened) = provider_fixture("ok").await;
    let binding = opened.binding.id;
    let mut events = opened.events;
    let configured = provider
        .session_configure(
            operation_context(),
            &binding,
            forge_provider_api::SessionConfiguration {
                model: Default::default(),
                access: Default::default(),
                approval: forge_provider_api::approval::ApprovalMode::Ask,
                resources: Default::default(),
            },
        )
        .await
        .unwrap();
    assert_eq!(
        configured.watermark,
        forge_provider_api::EventSequence::default()
    );
    let request = prompt_request(
        &binding,
        forge_provider_api::InvocationScope::new(
            forge_provider_api::ExecutionOwner::root("fixture-owner"),
            "after-config",
        ),
        receipt("configured"),
        forge_provider_api::PromptInput::Text("after config".into()),
    );
    provider
        .input_submit(operation_context(), &binding, request)
        .await
        .unwrap();
    assert_eq!(next_settlement(&mut events).await.binding_id, binding);
    provider
        .session_close(operation_context(), &binding)
        .await
        .unwrap();
}

/// Task overrides restore concrete native defaults before the next ordinary prompt.
#[tokio::test]
async fn task_model_and_reasoning_override_does_not_replace_session_defaults() {
    use forge_provider_api::provider::Provider;
    let (fixture, provider, opened) = provider_fixture("ok").await;
    let binding = opened.binding.id;
    let mut events = opened.events;
    let owner = forge_provider_api::ExecutionOwner::root("fixture-owner");
    let mut task = prompt_request(
        &binding,
        forge_provider_api::InvocationScope::new(owner.clone(), "task"),
        receipt("task"),
        forge_provider_api::PromptInput::Text("task".into()),
    );
    task.configuration.model.model = Some("other".into());
    task.configuration.model.reasoning =
        forge_provider_api::generation::ReasoningSelection::Level("high".into());
    provider
        .input_submit(operation_context(), &binding, task)
        .await
        .unwrap();
    next_settlement(&mut events).await;
    let ordinary = prompt_request(
        &binding,
        forge_provider_api::InvocationScope::new(owner, "ordinary"),
        receipt("ordinary"),
        forge_provider_api::PromptInput::Text("ordinary".into()),
    );
    provider
        .input_submit(operation_context(), &binding, ordinary)
        .await
        .unwrap();
    next_settlement(&mut events).await;
    let requests = std::fs::read_to_string(fixture._directory.path().join("mode.requests"))
        .unwrap()
        .lines()
        .map(|line| serde_json::from_str::<Value>(line).unwrap())
        .collect::<Vec<_>>();
    let models = requests
        .iter()
        .filter(|request| request["method"] == "session/set_model")
        .map(|request| request["params"]["modelId"].as_str().unwrap())
        .collect::<Vec<_>>();
    assert_eq!(models, vec!["other", "default-model"]);
    let reasoning = requests
        .iter()
        .filter(|request| request["method"] == "session/set_config_option")
        .map(|request| request["params"]["value"].as_str().unwrap())
        .collect::<Vec<_>>();
    assert_eq!(reasoning, vec!["high", "medium"]);
    provider
        .session_close(operation_context(), &binding)
        .await
        .unwrap();
}

struct PendingSend {
    started: tokio::sync::Notify,
}

#[async_trait]
impl forge_provider_api::turn_control::TurnControl for PendingSend {
    async fn before_send(
        &self,
        _: &forge_provider_api::InvocationScope,
    ) -> Result<(), ProviderError> {
        self.started.notify_one();
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
            control_handoff: false,
            cancelled: true,
            error: None,
        })
    }
    fn tool_finished(&self, _: &forge_provider_api::InvocationScope, _: Option<&str>) -> bool {
        false
    }
}

/// Draft restoration requires exact cancelled input/attempt evidence, with no native rollback.
#[tokio::test]
async fn cancelled_before_dispatch_retraction_proves_only_the_exact_input_attempt() {
    use forge_provider_api::provider::Provider;
    let (_fixture, provider, opened) = provider_fixture("ok").await;
    let binding = opened.binding.id;
    let mut events = opened.events;
    let mut request = prompt_request(
        &binding,
        forge_provider_api::InvocationScope::new(
            forge_provider_api::ExecutionOwner::root("fixture-owner"),
            "cancel-before-send",
        ),
        receipt("cancel-before-send"),
        forge_provider_api::PromptInput::Text("draft".into()),
    );
    let pending = Arc::new(PendingSend {
        started: tokio::sync::Notify::new(),
    });
    request.turn_control = pending.clone();
    let retract = forge_provider_api::InputRetractRequest {
        scope: request.scope.clone(),
        submission_id: request.submission_id.clone(),
        attempt_id: request.attempt_id.clone(),
        boundary: forge_provider_api::CheckpointId::new("checked-host-boundary").unwrap(),
        authorization: forge_provider_api::RetractionAuthorization::OutputFreeCancellation,
    };
    let submitted_provider = provider.clone();
    let submitted_binding = binding.clone();
    let submitted = tokio::spawn(async move {
        submitted_provider
            .input_submit(operation_context(), &submitted_binding, request)
            .await
    });
    pending.started.notified().await;
    provider
        .cancel(
            operation_context(),
            &binding,
            forge_provider_api::CancelRequest {
                scope: retract.scope.clone(),
            },
        )
        .await
        .unwrap();
    assert!(submitted.await.unwrap().is_err());
    next_settlement(&mut events).await;
    for _ in 0..2 {
        assert_eq!(
            provider
                .input_retract(operation_context(), &binding, retract.clone())
                .await
                .unwrap(),
            forge_provider_api::InputRetractOutcome::NeverConsumed
        );
    }
    let mut stale = retract;
    stale.attempt_id = forge_provider_api::DeliveryAttemptId::new("different-attempt").unwrap();
    assert_eq!(
        provider
            .input_retract(operation_context(), &binding, stale)
            .await
            .unwrap(),
        forge_provider_api::InputRetractOutcome::Unavailable(
            forge_provider_api::InputRetractUnavailable::Unsupported
        )
    );
    provider
        .session_close(operation_context(), &binding)
        .await
        .unwrap();
}

/// Build a durable resume with an independent host owner for history-exclusion regressions.
fn resume_request(fixture: &WireFixture, owner: &str) -> forge_provider_api::SessionRequest {
    forge_provider_api::SessionRequest {
        owner: forge_provider_api::ExecutionOwner::root(owner),
        mode: forge_provider_api::SessionMode::Interactive,
        workspace: fixture._directory.path().to_string_lossy().into_owned(),
        history: forge_provider_api::SessionHistory::Durable {
            resume: Some("fixture".to_owned().try_into().unwrap()),
        },
        configuration: forge_provider_api::SessionConfiguration {
            model: Default::default(),
            access: Default::default(),
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
        .session_close(operation_context(), &opened.binding.id)
        .await
        .unwrap();
    let (left, right) = tokio::join!(
        provider.session_open(operation_context(), resume_request(&fixture, "left-owner")),
        provider.session_open(operation_context(), resume_request(&fixture, "right-owner"))
    );
    let (winner, rejected) = match (left, right) {
        (Ok(winner), Err(rejected)) | (Err(rejected), Ok(winner)) => (winner, rejected),
        _ => panic!("one exact durable history must admit only one writable owner"),
    };
    assert!(
        matches!(rejected, ProviderError::Failure(failure) if failure.operation() == forge_provider_api::ProviderOperation::SessionOpen && failure.kind() == forge_provider_api::ProviderFailureKind::HistoryConflict)
    );
    let requests =
        std::fs::read_to_string(fixture._directory.path().join("mode.requests")).unwrap();
    assert_eq!(
        requests
            .lines()
            .filter(|request| request.contains("session/load"))
            .count(),
        1
    );
    provider
        .session_close(operation_context(), &winner.binding.id)
        .await
        .unwrap();
}

/// Authority revocation cannot release durable history while native cleanup is still owned.
#[tokio::test]
async fn invalidated_binding_retains_history_exclusion_until_close_collection() {
    use forge_provider_api::provider::Provider;
    let (fixture, provider, opened) = provider_fixture("remote_error").await;
    let binding = opened.binding.id;
    let request = prompt_request(
        &binding,
        forge_provider_api::InvocationScope::new(
            forge_provider_api::ExecutionOwner::root("fixture-owner"),
            "failed-native",
        ),
        receipt("failed-native"),
        forge_provider_api::PromptInput::Text("fail".into()),
    );
    assert!(
        provider
            .input_submit(operation_context(), &binding, request)
            .await
            .is_err()
    );
    assert!(
        matches!(provider.session_open(operation_context(), resume_request(&fixture, "replacement-owner")).await, Err(ProviderError::Failure(failure)) if failure.kind() == forge_provider_api::ProviderFailureKind::HistoryConflict)
    );
    let requests =
        std::fs::read_to_string(fixture._directory.path().join("mode.requests")).unwrap();
    assert_eq!(
        requests
            .lines()
            .filter(|request| request.contains("session/load"))
            .count(),
        0
    );
    let _ = provider.session_close(operation_context(), &binding).await;
    provider
        .session_close(operation_context(), &binding)
        .await
        .unwrap();
    let resumed = provider
        .session_open(
            operation_context(),
            resume_request(&fixture, "replacement-owner"),
        )
        .await
        .unwrap();
    provider
        .session_close(operation_context(), &resumed.binding.id)
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

/// A dropped observation cannot cancel a handed-off native prompt or release its collector.
#[tokio::test]
async fn dropped_submit_awaiter_retains_native_work_until_explicit_cancel() {
    use forge_provider_api::provider::Provider;
    let (fixture, provider, opened) = provider_fixture("held").await;
    let binding = opened.binding.id;
    let mut events = opened.events;
    let request = prompt_request(
        &binding,
        forge_provider_api::InvocationScope::new(
            forge_provider_api::ExecutionOwner::root("fixture-owner"),
            "dropped-awaiter",
        ),
        receipt("dropped-awaiter"),
        forge_provider_api::PromptInput::Text("retained input".into()),
    );
    let scope = request.scope.clone();
    let operation = operation_context();
    let submitted_provider = provider.clone();
    let submitted_binding = binding.clone();
    let awaiting = tokio::spawn(async move {
        submitted_provider
            .input_submit(operation, &submitted_binding, request)
            .await
    });
    tokio::time::timeout(std::time::Duration::from_secs(2), async {
        loop {
            let requests =
                std::fs::read_to_string(fixture._directory.path().join("mode.requests")).unwrap();
            if requests
                .lines()
                .any(|request| request.contains("session/prompt"))
            {
                break;
            }
            tokio::task::yield_now().await;
        }
    })
    .await
    .unwrap();
    awaiting.abort();
    let _ = awaiting.await;
    let requests =
        std::fs::read_to_string(fixture._directory.path().join("mode.requests")).unwrap();
    assert!(
        !requests
            .lines()
            .any(|request| request.contains("session/cancel"))
    );
    provider
        .cancel(
            operation_context(),
            &binding,
            forge_provider_api::CancelRequest { scope },
        )
        .await
        .unwrap();
    assert!(matches!(
        next_settlement(&mut events).await.result,
        forge_provider_api::ExecutionResult::Cancelled { .. }
    ));
    provider
        .session_close(operation_context(), &binding)
        .await
        .unwrap();
}

/// Explicit operation cancellation collects a dispatched input and exact replay cannot send it twice.
#[tokio::test]
async fn cancelled_submit_context_settles_and_replay_never_redispatches() {
    use forge_provider_api::provider::Provider;
    let (fixture, provider, opened) = provider_fixture("held").await;
    let binding = opened.binding.id;
    let mut events = opened.events;
    let request = prompt_request(
        &binding,
        forge_provider_api::InvocationScope::new(
            forge_provider_api::ExecutionOwner::root("fixture-owner"),
            "cancelled-operation",
        ),
        receipt("cancelled-operation"),
        forge_provider_api::PromptInput::Text("cancel input".into()),
    );
    let (source, cancellation) = forge_provider_api::OperationCancellation::channel();
    let context = forge_provider_api::OperationContext::new(
        forge_provider_api::OperationId::new("cancelled-submit").unwrap(),
        std::time::Duration::from_secs(10),
        cancellation,
    )
    .unwrap();
    let submitted_provider = provider.clone();
    let submitted_binding = binding.clone();
    let submitted_context = context.clone();
    let submitted_request = request.clone();
    let awaiting = tokio::spawn(async move {
        submitted_provider
            .input_submit(submitted_context, &submitted_binding, submitted_request)
            .await
    });
    tokio::time::timeout(std::time::Duration::from_secs(2), async {
        loop {
            let requests =
                std::fs::read_to_string(fixture._directory.path().join("mode.requests")).unwrap();
            if requests
                .lines()
                .any(|request| request.contains("session/prompt"))
            {
                break;
            }
            tokio::task::yield_now().await;
        }
    })
    .await
    .unwrap();
    source.cancel();
    let _ = awaiting.await.unwrap();
    assert!(matches!(
        next_settlement(&mut events).await.result,
        forge_provider_api::ExecutionResult::Cancelled { .. }
    ));
    tokio::time::timeout(std::time::Duration::from_secs(2), async {
        loop {
            if provider
                .input_submit(context.clone(), &binding, request.clone())
                .await
                .is_ok()
            {
                break;
            }
            tokio::task::yield_now().await;
        }
    })
    .await
    .expect("authoritative acceptance must replace provisional interruption");
    let requests =
        std::fs::read_to_string(fixture._directory.path().join("mode.requests")).unwrap();
    assert_eq!(
        requests
            .lines()
            .filter(|request| request.contains("session/prompt"))
            .count(),
        1
    );
    provider
        .session_close(operation_context(), &binding)
        .await
        .unwrap();
}

/// An already cancelled configuration context rejects without changing native settings or binding authority.
#[tokio::test]
async fn cancelled_configuration_context_rejects_before_native_mutation() {
    use forge_provider_api::provider::Provider;
    let (fixture, provider, opened) = provider_fixture("ok").await;
    let binding = opened.binding.id;
    let mut events = opened.events;
    let (source, cancellation) = forge_provider_api::OperationCancellation::channel();
    source.cancel();
    let context = forge_provider_api::OperationContext::new(
        forge_provider_api::OperationId::new("cancelled-configure").unwrap(),
        std::time::Duration::from_secs(10),
        cancellation,
    )
    .unwrap();
    let configuration = forge_provider_api::SessionConfiguration {
        model: forge_provider_api::ModelSelection {
            model: Some("other".into()),
            ..Default::default()
        },
        access: Default::default(),
        approval: forge_provider_api::approval::ApprovalMode::Ask,
        resources: Default::default(),
    };
    assert!(
        matches!(provider.session_configure(context, &binding, configuration).await, Err(ProviderError::Failure(failure)) if failure.kind() == forge_provider_api::ProviderFailureKind::Cancelled && failure.phase() == forge_provider_api::ProviderPhase::Admission && failure.disposition() == forge_provider_api::FailureDisposition::Unchanged)
    );
    let requests =
        std::fs::read_to_string(fixture._directory.path().join("mode.requests")).unwrap();
    assert!(
        !requests
            .lines()
            .any(|request| request.contains("session/set_model"))
    );
    let request = prompt_request(
        &binding,
        forge_provider_api::InvocationScope::new(
            forge_provider_api::ExecutionOwner::root("fixture-owner"),
            "after-cancelled-configure",
        ),
        receipt("after-cancelled-configure"),
        forge_provider_api::PromptInput::Text("still reusable".into()),
    );
    provider
        .input_submit(operation_context(), &binding, request)
        .await
        .unwrap();
    next_settlement(&mut events).await;
    provider
        .session_close(operation_context(), &binding)
        .await
        .unwrap();
}

/// A failed acknowledgement after native endpoint replacement cannot leave the old cache usable.
#[tokio::test]
async fn endpoint_reload_error_after_native_apply_invalidates_binding() {
    use forge_provider_api::provider::Provider;
    let (fixture, provider, opened) = provider_fixture("ok").await;
    let binding = opened.binding.id;
    std::fs::write(fixture._directory.path().join("mode"), "load_applied_error").unwrap();
    let mut request = prompt_request(
        &binding,
        forge_provider_api::InvocationScope::new(
            forge_provider_api::ExecutionOwner::root("fixture-owner"),
            "endpoint-reload",
        ),
        receipt("endpoint-reload"),
        forge_provider_api::PromptInput::Text("must not dispatch".into()),
    );
    request.tool_endpoint = Some(forge_provider_api::HostToolEndpoint {
        url: "http://new-callback.invalid/tools".into(),
        headers: BTreeMap::from([("authorization".into(), "new-secret".into())]),
    });
    let error = provider
        .input_submit(operation_context(), &binding, request)
        .await
        .unwrap_err();
    assert!(
        matches!(error, ProviderError::PreparationFailed(cause) if matches!(*cause, ProviderError::Failure(failure) if failure.operation() == forge_provider_api::ProviderOperation::InputSubmit && failure.phase() == forge_provider_api::ProviderPhase::Preparation && failure.disposition() == forge_provider_api::FailureDisposition::Invalidated))
    );
    let applied: Value = serde_json::from_str(
        &std::fs::read_to_string(fixture._directory.path().join("mode.endpoint-applied")).unwrap(),
    )
    .unwrap();
    assert_eq!(applied[0]["url"], "http://new-callback.invalid/tools");
    std::fs::write(fixture._directory.path().join("mode"), "ok").unwrap();
    let request = prompt_request(
        &binding,
        forge_provider_api::InvocationScope::new(
            forge_provider_api::ExecutionOwner::root("fixture-owner"),
            "endpoint-none",
        ),
        receipt("endpoint-none"),
        forge_provider_api::PromptInput::Text("stale URL must not be reused".into()),
    );
    assert!(
        matches!(provider.input_submit(operation_context(), &binding, request).await, Err(ProviderError::Failure(failure)) if failure.kind() == forge_provider_api::ProviderFailureKind::BindingInvalidated)
    );
    let requests =
        std::fs::read_to_string(fixture._directory.path().join("mode.requests")).unwrap();
    assert_eq!(
        requests
            .lines()
            .filter(|request| request.contains("session/prompt"))
            .count(),
        0
    );
    let _ = provider.session_close(operation_context(), &binding).await;
    provider
        .session_close(operation_context(), &binding)
        .await
        .unwrap();
}
