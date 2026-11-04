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
        self.state.lock().unwrap().failed = true;
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
        goal: None,
        submission_id: receipt.id.clone(),
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
 const request=JSON.parse(line);if(request.id===undefined)return;let result={};
 if(request.method==='fixture/release'&&heldPrompt){write({id:heldPrompt,result:{stopReason:'end_turn'}});heldPrompt=undefined;}
 if(request.method==='initialize')result={protocolVersion:1,agentInfo:{version:'99.0.0'},agentCapabilities:{loadSession:true}};
 if(request.method==='session/new')result={sessionId:'fixture'};
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
            .await?;
        self.connection
            .request("session/new", json!({"cwd":".","mcpServers":[]}))
            .await?;
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

/// Retain a post-acceptance worker failure and report observation loss instead of hanging.
#[tokio::test]
async fn accepted_prompt_retains_publication_failure_until_collection() {
    use forge_provider_api::provider::Provider;
    use forge_provider_api::session::{ResourceSelection, SessionMode, SessionRequest};
    use forge_provider_api::{ExecutionOwner, InvocationScope, ProviderFailureKind};

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
        .initialize_session(SessionRequest {
            owner: owner.clone(),
            mode: SessionMode::Interactive,
            workspace: fixture._directory.path().to_string_lossy().into_owned(),
            provider_session_id: None,
            model: Default::default(),
            access: Default::default(),
            approval: forge_provider_api::approval::ApprovalMode::Ask,
            resources: ResourceSelection::default(),
            resource_lease: None,
            tools: Vec::new(),
            tool_endpoint: None,
            system_instructions: String::new(),
            trace: Arc::new(IgnoreTrace),
        })
        .await
        .unwrap();
    let mut stream = provider.take_session_events(&binding.id).await.unwrap();
    let rejected_receipt = receipt("rejected-submission");
    let mut rejected = prompt_request(
        &binding.id,
        InvocationScope::new(owner.clone(), "rejected-exchange"),
        rejected_receipt.clone(),
        forge_provider_api::PromptInput::Text("test".into()),
    );
    rejected.configuration.model.context_window = Some("fixture-window".into());
    assert!(matches!(
        provider.session_prompt(&binding.id, rejected).await,
        Err(ProviderError::PreparationFailed(_))
    ));
    assert!(rejected_receipt.state.lock().unwrap().rejected);
    provider.cleanup_execution(&binding.id).await.unwrap();
    stream.check().unwrap();
    let receipt = FixtureReceipt::new();
    let mut request = prompt_request(
        &binding.id,
        InvocationScope::new(owner, "exchange"),
        receipt.clone(),
        forge_provider_api::PromptInput::Text("test".into()),
    );
    request.host_effects = Arc::new(FailedHostEffects);
    provider.session_prompt(&binding.id, request).await.unwrap();
    assert!(receipt.state.lock().unwrap().accepted);
    let observation = tokio::time::timeout(std::time::Duration::from_secs(2), async {
        loop {
            match stream.recv().await {
                Ok(Some(_)) => {}
                result => break result,
            }
        }
    })
    .await
    .expect("post-acceptance failure must wake the observation owner");
    assert!(observation.is_err());
    let error = provider.cleanup_execution(&binding.id).await.unwrap_err();
    assert!(matches!(error, ProviderError::Failure(failure)
        if failure.kind() == ProviderFailureKind::Unknown
        && failure.operation() == forge_provider_api::ProviderOperation::HostEffectPublication));
    provider.retire_session(&binding.id).await.unwrap();
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
    );
    let prompt = fixture.connection.submit(
        AcpSubmission::Prompt,
        json!({"sessionId":"fixture","prompt":[{"type":"text","text":"prompt"}]}),
        Some(receipt.clone()),
        Some(&dispatch),
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
async fn only_documented_idle_steering_rejection_authorizes_continuation() {
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
    assert!(matches!(result, Err(ProviderError::NotConsumed { .. })));
    {
        let state = receipt.state.lock().unwrap();
        assert!(state.sending && state.rejected && !state.accepted && !state.failed);
    }
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
    let normalized = ProviderError::from(native);
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

struct FixtureResourceLease;
impl forge_provider_api::session::ResourceLease for FixtureResourceLease {}

#[test]
fn steering_continuation_grant_binds_attempted_input_and_is_single_use() {
    use forge_provider_api::session::{SteeringDelivery, SteeringRequest};
    use forge_provider_api::{ExecutionOwner, InvocationScope};
    let binding = forge_provider_api::session::BindingId::new("binding").unwrap();
    let scope = InvocationScope::new(ExecutionOwner::root("session"), "exchange");
    let base_receipt = receipt("base-input");
    let steering_receipt = receipt("steering-input");
    let tool_endpoint = forge_provider_api::session::HostToolEndpoint {
        url: "http://127.0.0.1/invocation".into(),
        headers: std::collections::BTreeMap::from([("authorization".into(), "first".into())]),
    };
    let resource_lease: Arc<dyn forge_provider_api::session::ResourceLease> =
        Arc::new(FixtureResourceLease);
    let original = prompt_request(
        &binding,
        scope.clone(),
        base_receipt,
        forge_provider_api::input::PromptInput::Text("original".into()),
    );
    let mut original = original;
    original.tool_endpoint = Some(tool_endpoint.clone());
    original.resource_lease = Some(resource_lease.clone());
    let steering = SteeringRequest {
        scope: scope.clone(),
        delivery: SteeringDelivery::Journaled {
            receipt: steering_receipt.clone(),
        },
        input: forge_provider_api::input::PromptInput::Text("authorized correction".into()),
        attachments: Vec::new(),
        task: None,
        goal: None,
    };
    let baseline = super::provider::PromptBaseline::from_request(&original);
    let grant =
        super::provider::ContinuationGrant::from_steering(binding.clone(), &steering, &baseline);
    let mut grants = std::collections::HashMap::from([(b"opaque-token".to_vec(), grant)]);

    let mut altered = prompt_request(
        &binding,
        scope.clone(),
        steering_receipt.clone(),
        forge_provider_api::input::PromptInput::Text("different payload".into()),
    );
    assert!(!super::provider::take_continuation_grant(
        &mut grants,
        b"opaque-token",
        &binding,
        &scope.owner,
        &altered
    ));
    assert_eq!(
        grants.len(),
        1,
        "a mismatched attempt must not consume authorization"
    );

    let mismatched_receipt = receipt("forged-submission");
    altered.receipt = mismatched_receipt.clone();
    altered.submission_id = mismatched_receipt.id.clone();
    altered.input = steering.input.clone();
    assert!(!super::provider::take_continuation_grant(
        &mut grants,
        b"opaque-token",
        &binding,
        &scope.owner,
        &altered
    ));
    assert_eq!(
        grants.len(),
        1,
        "a mismatched receipt must not consume authorization"
    );

    altered.receipt = steering_receipt.clone();
    altered.submission_id = steering_receipt.id.clone();
    altered.input = steering.input.clone();
    altered.tool_endpoint = Some(forge_provider_api::session::HostToolEndpoint {
        url: "http://127.0.0.1/forged".into(),
        headers: tool_endpoint.headers.clone(),
    });
    altered.resource_lease = Some(resource_lease.clone());
    assert!(!super::provider::take_continuation_grant(
        &mut grants,
        b"opaque-token",
        &binding,
        &scope.owner,
        &altered
    ));
    altered.tool_endpoint = Some(forge_provider_api::session::HostToolEndpoint {
        url: tool_endpoint.url.clone(),
        headers: std::collections::BTreeMap::from([("authorization".into(), "forged".into())]),
    });
    assert!(!super::provider::take_continuation_grant(
        &mut grants,
        b"opaque-token",
        &binding,
        &scope.owner,
        &altered
    ));
    altered.tool_endpoint = Some(tool_endpoint);
    altered.resource_lease = Some(Arc::new(FixtureResourceLease));
    assert!(!super::provider::take_continuation_grant(
        &mut grants,
        b"opaque-token",
        &binding,
        &scope.owner,
        &altered
    ));
    altered.resource_lease = Some(resource_lease);
    assert!(super::provider::take_continuation_grant(
        &mut grants,
        b"opaque-token",
        &binding,
        &scope.owner,
        &altered
    ));
    assert!(!super::provider::take_continuation_grant(
        &mut grants,
        b"opaque-token",
        &binding,
        &scope.owner,
        &altered
    ));
}

#[test]
fn mcp_native_reason_text_is_replaced_by_normalized_message() {
    let canary = "private native payload: /home/user/secret-token";
    let detail = super::provider::normalized_mcp_reason(Some(canary));
    assert_eq!(detail.as_str(), "MCP resource is unavailable.");
    assert!(!detail.as_str().contains(canary));
}
