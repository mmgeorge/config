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
        thread_id: (scope).owner.thread_id(),
        turn_id: (scope)
            .turn_id()
            .expect("admitted scope has a nonempty host turn identity"),

        scope,
        dispatch,
        mode: forge_provider_api::input::PromptMode::Chat,
        configuration: forge_provider_api::session::InvocationConfiguration {
            model: forge_provider_api::session::ModelSelection::default(),
            access: forge_provider_api::access::AccessPolicy {
                sandbox: false,
                ..Default::default()
            },
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
        acceptance_started: Default::default(),
        acceptance_release: Default::default(),
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

/// Native authentication cannot install a second timer that overrides its enclosing operation.
#[tokio::test(start_paused = true)]
async fn native_authentication_wait_uses_only_enclosing_operation_deadline() {
    let fixture = WireFixture::start("held_auth").await.unwrap();
    let authentication = fixture
        .connection
        .request("authenticate", json!({"methodId":"fixture"}));
    tokio::pin!(authentication);
    assert!(futures_util::poll!(&mut authentication).is_pending());
    // The barrier response proves the peer has received and retained authentication.
    fixture
        .connection
        .request("fixture/barrier", json!({}))
        .await
        .unwrap();
    tokio::time::advance(std::time::Duration::from_secs(121)).await;
    assert!(futures_util::poll!(&mut authentication).is_pending());
    fixture
        .connection
        .request("fixture/release_auth", json!({}))
        .await
        .unwrap();
    authentication.await.unwrap();
    fixture.connection.shutdown().await.unwrap();
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
        std::fs::write(directory.path().join("native-version"), "2.1.302")?;
        std::fs::write(
            &script,
            r#"
const fs=require('node:fs');const readline=require('node:readline');
if(process.argv.includes('--version')){console.log(fs.readFileSync(require('node:path').join(require('node:path').dirname(process.argv[2]),'native-version'),'utf8'));process.exit(0);}
const write=value=>process.stdout.write(JSON.stringify({jsonrpc:'2.0',...value})+'\n');
let heldPrompt;let heldAuthentication;
fs.watch(process.argv[2],()=>{if(heldPrompt!==undefined&&fs.readFileSync(process.argv[2],'utf8')==='release_cancel'){write({id:heldPrompt,result:{stopReason:'end_turn'}});heldPrompt=undefined;}});
readline.createInterface({input:process.stdin}).on('line',line=>{
 const request=JSON.parse(line);fs.appendFileSync(process.argv[2]+'.requests',JSON.stringify(request)+'\n');
 if(request.id===undefined){if(request.method==='session/cancel'&&heldPrompt&&fs.readFileSync(process.argv[2],'utf8')!=='held_cancel'){write({id:heldPrompt,result:{stopReason:'cancelled'}});heldPrompt=undefined;}return;}let result={};
 if(request.method==='fixture/release'&&heldPrompt){write({id:heldPrompt,result:{stopReason:'end_turn'}});heldPrompt=undefined;}
 if(request.method==='authenticate'&&fs.readFileSync(process.argv[2],'utf8')==='held_auth'){heldAuthentication=request.id;return;}
 if(request.method==='fixture/release_auth'&&heldAuthentication!==undefined){write({id:heldAuthentication,result:{}});heldAuthentication=undefined;}
 if(request.method==='initialize')result={protocolVersion:1,agentInfo:{version:'99.0.0'},agentCapabilities:{loadSession:true,...(fs.readFileSync(process.argv[2],'utf8')==='native_instructions'?{sessionCapabilities:{fork:{},systemPrompt:{}}}:fs.readFileSync(process.argv[2],'utf8')==='fork_no_instructions'?{sessionCapabilities:{fork:{}}}:{})}};
 if(request.method==='initialize'&&fs.readFileSync(process.argv[2],'utf8')==='disabled_extensions')result.agentCapabilities.sessionCapabilities={systemPrompt:{},fork:false,subagents:null};
 if(request.method==='session/load'&&fs.readFileSync(process.argv[2],'utf8')==='load_applied_error'){
  fs.writeFileSync(process.argv[2]+'.endpoint-applied',JSON.stringify(request.params.mcpServers));
  write({id:request.id,error:{code:-32603,message:'private failure after native endpoint apply'}});return;
 }
 if(request.method==='session/new'||request.method==='session/load')result={sessionId:'fixture',models:{currentModelId:'default-model',availableModels:[{modelId:'default-model'},{modelId:'other'}]},configOptions:[{id:'effort',currentValue:'medium',options:[{value:'low'},{value:'medium'},{value:'high'}]}]};
 if(request.method==='session/fork')result={sessionId:'fork-child'};
 if(request.method==='session/prompt'||request.method==='_session/steering'){
  const mode=fs.readFileSync(process.argv[2],'utf8');
  if(mode.startsWith('observed:')||mode==='held'||mode==='held_cancel')write({method:'session/update',params:{sessionId:'fixture',update:{sessionUpdate:'agent_message_chunk',content:{type:'text',text:'answer'}}}});
  if((mode==='held'||mode==='held_cancel'||mode==='held_no_start')&&request.method==='session/prompt'){heldPrompt=request.id;return;}
  if(mode==='remote_error'){write({id:request.id,error:{code:-32603,message:'private native error'}});return;}
  if(mode==='invalid_request'){write({id:request.id,error:{code:-32600,message:'private invalid request detail'}});return;}
  if(request.method==='_session/steering')result=mode==='promptRequired'?{outcome:'promptRequired',reason:'noRunningTurn'}:{outcome:mode};
  else result=mode==='invalid'?{}:{stopReason:mode.startsWith('observed:')?mode.slice(9):mode==='cancelled'?'cancelled':'end_turn'};
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
    use forge_provider_api::{InvocationScope, ThreadOwner};

    let fixture = WireFixture::start("ok").await.unwrap();
    fixture.connection.shutdown().await.unwrap();
    let provider = super::provider::AcpProvider::new(
        forge_provider_api::ProviderId::new("antigravity").unwrap(),
        fixture.command.clone(),
        fixture._directory.path().join("runtime"),
    )
    .unwrap();
    let owner = ThreadOwner::root("session");
    let binding = provider
        .thread_open(
            operation_context(),
            SessionRequest {
                thread_id: (owner.clone()).thread_id(),
                owner: owner.clone(),
                mode: SessionMode::Interactive,
                workspace: fixture._directory.path().to_string_lossy().into_owned(),
                history: forge_provider_api::SessionHistory::Durable { resume: None },
                configuration: forge_provider_api::SessionConfiguration {
                    model: Default::default(),
                    access: forge_provider_api::access::AccessPolicy {
                        sandbox: false,
                        ..Default::default()
                    },
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
        .turn_start(operation_context(), &binding.id, request)
        .await
        .unwrap();
    assert!(receipt.state.lock().unwrap().accepted);
    let settlement = next_completion(&mut stream).await;
    assert!(matches!(
        settlement.outcome,
        forge_provider_api::turn::TurnOutcome::Completed { .. }
    ));
    let repeated = provider
        .turn_interrupt(
            operation_context(),
            &binding.id,
            forge_provider_api::CancelRequest { scope },
        )
        .await
        .unwrap();
    assert_eq!(settlement.watermark, repeated.watermark);
    provider
        .thread_close(operation_context(), &binding.id)
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
            forge_provider_api::ThreadOwner::root("session"),
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
        matches!(result, Err(ProviderError::PreparationFailed(cause)) if matches!(*cause, ProviderError::Failure(failure) if failure.operation() == forge_provider_api::ProviderOperation::TurnSteer && failure.kind() == forge_provider_api::ProviderFailureKind::Rejected && failure.phase() == forge_provider_api::ProviderPhase::Observation && failure.disposition() == forge_provider_api::FailureDisposition::Unchanged))
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
        (AcpSubmission::Prompt, ProviderOperation::TurnStart),
        (AcpSubmission::Steering, ProviderOperation::TurnSteer),
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
        matches!(result, Err(ProviderError::Failure(failure)) if failure.operation() == ProviderOperation::TurnStart && failure.kind() == ProviderFailureKind::Runtime && failure.phase() == ProviderPhase::Persistence && failure.disposition() == FailureDisposition::Uncertain)
    );
    assert!(
        matches!(receipt.state.lock().unwrap().failure.as_ref(), Some(ProviderError::Failure(failure)) if failure.kind() == ProviderFailureKind::Runtime && failure.phase() == ProviderPhase::Persistence && failure.disposition() == FailureDisposition::Uncertain)
    );
    let native = fixture
        .connection
        .prompt_end(&forge_provider_api::CoveredInput::new(
            receipt.id().clone(),
            receipt.attempt_id().unwrap(),
        ))
        .expect("native response survives receipt persistence failure");
    assert_eq!(
        native.outcome(),
        super::PromptOutcome::Completed(forge_provider_api::turn::TurnCompletionReason::Normal)
    );
    fixture.connection.shutdown().await.unwrap();
}

/// Native end time and reason are retained before the late ACP v1 acceptance receipt commits.
#[tokio::test]
async fn native_prompt_end_is_retained_while_receipt_acceptance_is_pending() {
    let fixture = WireFixture::start("ok").await.unwrap();
    fixture.ready().await.unwrap();
    let receipt = FixtureReceipt::new();
    receipt.state.lock().unwrap().hold_acceptance = true;
    let submitting = fixture.connection.submit(
        AcpSubmission::Prompt,
        json!({"sessionId":"fixture", "prompt":[]}),
        Some(receipt.clone()),
        None,
        None,
    );
    tokio::pin!(submitting);
    tokio::time::timeout(std::time::Duration::from_secs(2), async {
        tokio::select! {
            _ = receipt.acceptance_started.notified() => {},
            result = &mut submitting => panic!("submission completed before receipt collection: {result:?}"),
        }
    }).await.unwrap();
    let input =
        forge_provider_api::CoveredInput::new(receipt.id().clone(), receipt.attempt_id().unwrap());
    let before = fixture
        .connection
        .prompt_end(&input)
        .expect("native end is retained before acceptance commit");
    assert!(!receipt.state.lock().unwrap().accepted);
    receipt.acceptance_release.notify_one();
    submitting.await.unwrap();
    let after = fixture.connection.prompt_end(&input).unwrap();
    assert_eq!(before.outcome(), after.outcome());
    assert_eq!(before.completed_at_ms(), after.completed_at_ms());
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
    let normalized = crate::error::acp(native, forge_provider_api::ProviderOperation::TurnStart);
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
        .thread_open(
            operation_context(),
            forge_provider_api::SessionRequest {
                thread_id: (forge_provider_api::ThreadOwner::root("fixture-owner")).thread_id(),
                owner: forge_provider_api::ThreadOwner::root("fixture-owner"),
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

/// Preserve the public completion and its exact publication boundary in native fixtures.
struct ObservedCompletion {
    binding_id: forge_provider_api::BindingId,
    thread_id: forge_provider_api::ThreadId,
    turn_id: forge_provider_api::TurnId,
    outcome: forge_provider_api::turn::TurnOutcome,
    watermark: forge_provider_api::EventSequence,
}

/// Observe automatic collection without issuing a public cleanup command.
async fn next_completion(
    stream: &mut forge_provider_api::ProviderEventStream,
) -> ObservedCompletion {
    tokio::time::timeout(std::time::Duration::from_secs(2), async {
        loop {
            let envelope = stream.recv().await.unwrap().unwrap();
            if let forge_provider_api::ProviderEvent::Turn {
                thread_id,
                turn_id,
                event: forge_provider_api::TurnEvent::Completed(completed),
            } = envelope.event
            {
                return ObservedCompletion {
                    binding_id: envelope.binding_id,
                    thread_id,
                    turn_id,
                    outcome: completed.result().outcome(),
                    watermark: envelope.sequence,
                };
            }
        }
    })
    .await
    .expect("native worker must automatically collect")
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
                forge_provider_api::ThreadOwner::root("fixture-owner"),
                "invalid",
            ),
            receipt.clone(),
            forge_provider_api::PromptInput::Text("must not dispatch".into()),
        );
        match invalid {
            0 => request.scope.owner = forge_provider_api::ThreadOwner::root("different-owner"),
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
            matches!(provider.turn_start(operation_context(), &binding, request).await, Err(ProviderError::Failure(failure)) if failure.operation() == ProviderOperation::TurnStart && failure.kind() == ProviderFailureKind::WrongInvocation && failure.phase() == ProviderPhase::Admission && failure.disposition() == FailureDisposition::Unchanged)
        );
        assert!(!receipt.state.lock().unwrap().sending);
    }
    let idle = forge_provider_api::SteeringRequest {
        scope: forge_provider_api::InvocationScope::new(
            forge_provider_api::ThreadOwner::root("fixture-owner"),
            "idle-steer",
        ),
        delivery: forge_provider_api::SteeringDelivery::Context,
        input: forge_provider_api::PromptInput::Text("must not steer".into()),
        attachments: Vec::new(),
        task: None,
    };
    assert!(
        matches!(provider.turn_steer(operation_context(), &binding, idle.clone()).await, Err(ProviderError::Failure(failure)) if failure.operation() == ProviderOperation::TurnSteer && failure.kind() == ProviderFailureKind::WrongInvocation && failure.phase() == ProviderPhase::Admission && failure.disposition() == FailureDisposition::Unchanged)
    );
    let log = std::fs::read_to_string(fixture._directory.path().join("mode.requests")).unwrap();
    assert!(
        !log.lines()
            .any(|line| line.contains("session/prompt") || line.contains("_session/steering"))
    );
    provider
        .thread_close(operation_context(), &binding)
        .await
        .unwrap();
    let request = prompt_request(
        &binding,
        idle.scope.clone(),
        receipt("closed"),
        forge_provider_api::PromptInput::Text("closed".into()),
    );
    assert!(
        matches!(provider.turn_start(operation_context(), &binding, request).await, Err(ProviderError::Failure(failure)) if failure.kind() == ProviderFailureKind::BindingClosed && failure.disposition() == FailureDisposition::Closed)
    );
    assert!(
        matches!(provider.turn_steer(operation_context(), &binding, idle).await, Err(ProviderError::Failure(failure)) if failure.kind() == ProviderFailureKind::BindingClosed && failure.disposition() == FailureDisposition::Closed)
    );
    provider.shutdown(operation_context()).await.unwrap();
}

/// Expired cancellation retains collection without destroying the binding or rewriting late success.
#[tokio::test]
async fn cancel_timeout_retains_binding_until_late_settlement_and_reuse() {
    use forge_provider_api::provider::Provider;
    let (fixture, provider, opened) = provider_fixture("held_cancel").await;
    let binding = opened.binding.id;
    let mut events = opened.events;
    let owner = forge_provider_api::ThreadOwner::root("fixture-owner");
    let scope = forge_provider_api::InvocationScope::new(owner.clone(), "late-cancel");
    let request = prompt_request(
        &binding,
        scope.clone(),
        receipt("late-cancel"),
        forge_provider_api::PromptInput::Text("held native input".into()),
    );
    let (_, cancellation) = forge_provider_api::OperationCancellation::channel();
    let submit_context = forge_provider_api::OperationContext::unbounded(
        forge_provider_api::OperationId::new("held-unbounded-submit").unwrap(),
        cancellation,
    );
    let submitted_provider = provider.clone();
    let submitted_binding = binding.clone();
    let submitted = tokio::spawn(async move {
        submitted_provider
            .turn_start(submit_context, &submitted_binding, request)
            .await
    });
    let log_path = fixture._directory.path().join("mode.requests");
    // The peer's request log is the dispatch barrier, independent of wall-clock delay.
    loop {
        if std::fs::read_to_string(&log_path)
            .unwrap()
            .contains("session/prompt")
        {
            break;
        }
        tokio::task::yield_now().await;
    }
    eprintln!("ACP cancel fixture: native prompt dispatched; pausing Tokio clock");
    let (stop_watchdog, watchdog_stopped) = std::sync::mpsc::channel::<()>();
    let watchdog_fired = Arc::new(std::sync::atomic::AtomicBool::new(false));
    let watchdog_failure = watchdog_fired.clone();
    let release_path = fixture._directory.path().join("mode");
    let watchdog = std::thread::spawn(move || {
        if matches!(
            watchdog_stopped.recv_timeout(std::time::Duration::from_secs(5)),
            Err(std::sync::mpsc::RecvTimeoutError::Timeout)
        ) {
            watchdog_failure.store(true, std::sync::atomic::Ordering::Release);
            eprintln!(
                "ACP cancel fixture WATCHDOG: releasing native peer after missing observer response"
            );
            std::fs::write(release_path, "release_cancel").unwrap();
        }
    });
    tokio::time::pause();
    let (_, cancellation) = forge_provider_api::OperationCancellation::channel();
    let context = forge_provider_api::OperationContext::new(
        forge_provider_api::OperationId::new("late-cancel-observer").unwrap(),
        std::time::Duration::from_secs(30),
        cancellation,
    )
    .unwrap();
    let cancellation = provider.turn_interrupt(
        context.clone(),
        &binding,
        forge_provider_api::CancelRequest {
            scope: scope.clone(),
        },
    );
    tokio::pin!(cancellation);
    assert!(futures_util::poll!(&mut cancellation).is_pending());
    loop {
        if std::fs::read_to_string(&log_path)
            .unwrap()
            .contains("session/cancel")
        {
            break;
        }
        tokio::task::yield_now().await;
    }
    eprintln!(
        "ACP cancel fixture: native cancel dispatched; before advance now={:?}, deadline={:?}, remaining={:?}",
        tokio::time::Instant::now(),
        context.deadline(),
        context.remaining()
    );
    tokio::time::advance(std::time::Duration::from_secs(30)).await;
    let interrupted = context.interrupted();
    tokio::pin!(interrupted);
    let interruption_at_boundary = futures_util::poll!(&mut interrupted);
    eprintln!(
        "ACP cancel fixture: after advance now={:?}, deadline={:?}, remaining={:?}, direct_interruption={interruption_at_boundary:?}",
        tokio::time::Instant::now(),
        context.deadline(),
        context.remaining()
    );
    // Pausing after native I/O preserves a sub-millisecond Instant. Tokio's timer wheel
    // rounds its wakeup to a millisecond tick, so drive that tick without extending the budget.
    let driver_tick = std::time::Duration::from_millis(1);
    tokio::time::advance(driver_tick).await;
    let interruption_after_tick = if interruption_at_boundary.is_pending() {
        futures_util::poll!(&mut interrupted)
    } else {
        interruption_at_boundary
    };
    eprintln!(
        "ACP cancel fixture: timer rounding tick={driver_tick:?}, overdue={:?}, direct_interruption={interruption_after_tick:?}",
        tokio::time::Instant::now().saturating_duration_since(context.deadline().unwrap())
    );
    assert!(
        matches!(
            interruption_after_tick,
            std::task::Poll::Ready(forge_provider_api::OperationInterruption::DeadlineElapsed)
        ),
        "original context deadline did not fire within one timer driver tick"
    );
    let observed = cancellation.await;
    let observed_summary = match &observed {
        Ok(_) => "collected".to_owned(),
        Err(error) => error.to_string(),
    };
    eprintln!(
        "ACP cancel fixture: observer result={observed_summary}, watchdog_fired={}",
        watchdog_fired.load(std::sync::atomic::Ordering::Acquire)
    );
    let _ = stop_watchdog.send(());
    watchdog.join().unwrap();
    assert!(
        !watchdog_fired.load(std::sync::atomic::Ordering::Acquire),
        "cancellation failed to respond at its original deadline plus timer tick: {observed_summary}"
    );
    assert!(
        tokio::time::Instant::now() <= context.deadline().unwrap() + driver_tick,
        "observer exceeded original deadline plus timer driver tick"
    );
    assert!(
        matches!(&observed, Err(ProviderError::Failure(failure))
        if failure.kind() == forge_provider_api::ProviderFailureKind::Timeout
            && failure.disposition() == forge_provider_api::FailureDisposition::Uncertain),
        "unexpected cancellation observer outcome: {observed_summary}"
    );
    let blocked = prompt_request(
        &binding,
        forge_provider_api::InvocationScope::new(owner.clone(), "blocked"),
        receipt("blocked"),
        forge_provider_api::PromptInput::Text("must await collection".into()),
    );
    assert!(
        matches!(provider.turn_start(operation_context(), &binding, blocked).await,
        Err(ProviderError::Failure(failure))
            if failure.kind() == forge_provider_api::ProviderFailureKind::ExecutionActive)
    );
    tokio::time::resume();
    std::fs::write(fixture._directory.path().join("mode"), "release_cancel").unwrap();
    submitted.await.unwrap().unwrap();
    let settled = next_completion(&mut events).await;
    assert_eq!(settled.thread_id, scope.owner.thread_id());
    assert_eq!(settled.turn_id, scope.turn_id().unwrap());
    assert!(matches!(
        settled.outcome,
        forge_provider_api::turn::TurnOutcome::Completed { .. }
    ));
    let replayed = tokio::time::timeout(std::time::Duration::from_secs(2), async {
        loop {
            match provider
                .turn_interrupt(
                    context.clone(),
                    &binding,
                    forge_provider_api::CancelRequest {
                        scope: scope.clone(),
                    },
                )
                .await
            {
                Ok(settlement) => break settlement,
                Err(ProviderError::Failure(failure))
                    if failure.kind() == forge_provider_api::ProviderFailureKind::Timeout => {}
                Err(error) => panic!("late authoritative cancellation failed: {error}"),
            }
            tokio::task::yield_now().await;
        }
    })
    .await
    .unwrap();
    assert_eq!(replayed.watermark, settled.watermark);
    std::fs::write(fixture._directory.path().join("mode"), "ok").unwrap();
    let successor = prompt_request(
        &binding,
        forge_provider_api::InvocationScope::new(owner, "successor"),
        receipt("successor"),
        forge_provider_api::PromptInput::Text("same binding after late collection".into()),
    );
    provider
        .turn_start(operation_context(), &binding, successor)
        .await
        .unwrap();
    assert_eq!(next_completion(&mut events).await.binding_id, binding);
    provider
        .thread_close(operation_context(), &binding)
        .await
        .unwrap();
    provider.shutdown(operation_context()).await.unwrap();
}

/// Root terminal publication retains the native stop condition and follows final content.
#[tokio::test]
async fn collected_root_completion_preserves_each_native_stop_reason_once() {
    use forge_provider_api::{
        provider::Provider,
        turn::{TurnCompletionReason, TurnOutcome},
    };
    for (native, outcome) in [
        (
            "end_turn",
            TurnOutcome::Completed {
                stop_reason: TurnCompletionReason::Normal,
            },
        ),
        (
            "max_tokens",
            TurnOutcome::Completed {
                stop_reason: TurnCompletionReason::TokenLimit,
            },
        ),
        (
            "max_turn_requests",
            TurnOutcome::Completed {
                stop_reason: TurnCompletionReason::RequestLimit,
            },
        ),
        (
            "refusal",
            TurnOutcome::Completed {
                stop_reason: TurnCompletionReason::Refusal,
            },
        ),
        ("cancelled", TurnOutcome::Interrupted),
    ] {
        let (_fixture, provider, opened) = provider_fixture(&format!("observed:{native}")).await;
        let binding = opened.binding.id;
        let mut events = opened.events;
        let request = prompt_request(
            &binding,
            forge_provider_api::InvocationScope::new(
                forge_provider_api::ThreadOwner::root("fixture-owner"),
                native,
            ),
            receipt(native),
            forge_provider_api::PromptInput::Text("respond".into()),
        );
        let scope = request.scope.clone();
        provider
            .turn_start(operation_context(), &binding, request)
            .await
            .unwrap();
        tokio::time::timeout(std::time::Duration::from_secs(2), async {
            let mut message_finished = false;
            loop {
                let event = events.recv().await.unwrap().unwrap();
                match event.event {
                    forge_provider_api::ProviderEvent::Item {
                        event:
                            forge_provider_api::ItemEvent::Completed(
                                forge_provider_api::ItemCompletion::AgentMessage(_),
                            ),
                        ..
                    } => message_finished = true,
                    forge_provider_api::ProviderEvent::Turn {
                        thread_id,
                        turn_id,
                        event: forge_provider_api::TurnEvent::Completed(completed),
                    } => {
                        let observed = completed.result().outcome();
                        assert!(message_finished);
                        assert_eq!(thread_id, scope.owner.thread_id());
                        assert_eq!(turn_id, scope.turn_id().unwrap());
                        assert_eq!(observed, outcome);
                        break;
                    }
                    _ => {}
                }
            }
        })
        .await
        .unwrap();
        provider
            .thread_close(operation_context(), &binding)
            .await
            .unwrap();
    }
}

/// A stale cancel must replay completed evidence while a successor continues running.
#[tokio::test]
async fn automatic_settlement_and_stale_cancel_preserve_successor() {
    use forge_provider_api::provider::Provider;
    let (fixture, provider, opened) = provider_fixture("ok").await;
    let binding = opened.binding.id;
    let mut events = opened.events;
    let owner = forge_provider_api::ThreadOwner::root("fixture-owner");
    let first = prompt_request(
        &binding,
        forge_provider_api::InvocationScope::new(owner.clone(), "first"),
        receipt("first"),
        forge_provider_api::PromptInput::Text("first".into()),
    );
    let first_scope = first.scope.clone();
    provider
        .turn_start(operation_context(), &binding, first)
        .await
        .unwrap();
    let settled = next_completion(&mut events).await;
    assert!(matches!(
        settled.outcome,
        forge_provider_api::turn::TurnOutcome::Completed { .. }
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
            .turn_start(operation_context(), &submitted_binding, second)
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
        access: forge_provider_api::access::AccessPolicy {
            sandbox: false,
            ..Default::default()
        },
        approval: forge_provider_api::approval::ApprovalMode::Ask,
        resources: Default::default(),
    };
    assert!(
        matches!(provider.thread_configure(operation_context(), &binding, configuration).await, Err(ProviderError::Failure(failure)) if failure.kind() == forge_provider_api::ProviderFailureKind::ExecutionActive)
    );
    let overlapping = prompt_request(
        &binding,
        forge_provider_api::InvocationScope::new(second_scope.owner.clone(), "overlapping"),
        receipt("overlapping"),
        forge_provider_api::PromptInput::Text("must not overlap".into()),
    );
    assert!(
        matches!(provider.turn_start(operation_context(), &binding, overlapping).await, Err(ProviderError::Failure(failure)) if failure.kind() == forge_provider_api::ProviderFailureKind::ExecutionActive && failure.phase() == forge_provider_api::ProviderPhase::Admission && failure.disposition() == forge_provider_api::FailureDisposition::Unchanged)
    );
    assert!(!submitted.is_finished());
    let repeated = provider
        .turn_interrupt(
            operation_context(),
            &binding,
            forge_provider_api::CancelRequest { scope: first_scope },
        )
        .await
        .unwrap();
    assert_eq!(repeated.watermark, settled.watermark);
    assert!(!submitted.is_finished());
    let cancelled = provider
        .turn_interrupt(
            operation_context(),
            &binding,
            forge_provider_api::CancelRequest {
                scope: second_scope.clone(),
            },
        )
        .await
        .unwrap();
    assert_eq!(cancelled.turn_id, second_scope.turn_id().unwrap());
    let cancelled_completion = next_completion(&mut events).await;
    assert!(matches!(
        cancelled_completion.outcome,
        forge_provider_api::turn::TurnOutcome::Interrupted
    ));
    submitted.await.unwrap().unwrap();
    provider
        .thread_close(operation_context(), &binding)
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
        .thread_configure(
            operation_context(),
            &binding,
            forge_provider_api::SessionConfiguration {
                model: Default::default(),
                access: forge_provider_api::access::AccessPolicy {
                    sandbox: false,
                    ..Default::default()
                },
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
            forge_provider_api::ThreadOwner::root("fixture-owner"),
            "after-config",
        ),
        receipt("configured"),
        forge_provider_api::PromptInput::Text("after config".into()),
    );
    provider
        .turn_start(operation_context(), &binding, request)
        .await
        .unwrap();
    assert_eq!(next_completion(&mut events).await.binding_id, binding);
    provider
        .thread_close(operation_context(), &binding)
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
    let owner = forge_provider_api::ThreadOwner::root("fixture-owner");
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
        .turn_start(operation_context(), &binding, task)
        .await
        .unwrap();
    next_completion(&mut events).await;
    let ordinary = prompt_request(
        &binding,
        forge_provider_api::InvocationScope::new(owner, "ordinary"),
        receipt("ordinary"),
        forge_provider_api::PromptInput::Text("ordinary".into()),
    );
    provider
        .turn_start(operation_context(), &binding, ordinary)
        .await
        .unwrap();
    next_completion(&mut events).await;
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
        .thread_close(operation_context(), &binding)
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
            forge_provider_api::ThreadOwner::root("fixture-owner"),
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
            .turn_start(operation_context(), &submitted_binding, request)
            .await
    });
    pending.started.notified().await;
    let interrupted = provider
        .turn_interrupt(
            operation_context(),
            &binding,
            forge_provider_api::CancelRequest {
                scope: retract.scope.clone(),
            },
        )
        .await;
    assert!(
        matches!(interrupted, Err(ProviderError::PreparationFailed(cause))
        if cause.is_clean_cancellation())
    );
    assert!(submitted.await.unwrap().is_err());
    while let Some(event) = events.try_next().unwrap() {
        assert!(
            !matches!(event.event, forge_provider_api::ProviderEvent::Turn { turn_id, .. } | forge_provider_api::ProviderEvent::Item { turn_id, .. } if turn_id == retract.scope.turn_id().unwrap())
        );
    }
    for _ in 0..2 {
        assert_eq!(
            provider
                .input_retract(operation_context(), &binding, retract.clone())
                .await
                .unwrap(),
            forge_provider_api::InputRetractOutcome::NeverConsumed
        );
    }
    let successor_owner = retract.scope.owner.clone();
    let mut stale = retract;
    stale.attempt_id = forge_provider_api::DeliveryAttemptId::new("different-attempt").unwrap();
    assert_eq!(
        provider
            .input_retract(operation_context(), &binding, stale)
            .await
            .unwrap(),
        forge_provider_api::InputRetractOutcome::Unavailable(
            forge_provider_api::InputRetractUnavailable::NotCancelled
        )
    );
    let successor = prompt_request(
        &binding,
        forge_provider_api::InvocationScope::new(successor_owner, "after-preparation-cancel"),
        receipt("after-preparation-cancel"),
        forge_provider_api::PromptInput::Text("reuse connection".into()),
    );
    provider
        .turn_start(operation_context(), &binding, successor)
        .await
        .unwrap();
    next_completion(&mut events).await;
    provider
        .thread_close(operation_context(), &binding)
        .await
        .unwrap();
}

/// Build a durable resume with an independent host owner for history-exclusion regressions.
fn resume_request(fixture: &WireFixture, owner: &str) -> forge_provider_api::SessionRequest {
    forge_provider_api::SessionRequest {
        thread_id: (forge_provider_api::ThreadOwner::root(owner)).thread_id(),
        owner: forge_provider_api::ThreadOwner::root(owner),
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

/// Preserve base instructions through endpoint reloads using negotiated native delivery or text fallback.
#[tokio::test]
async fn base_instructions_survive_open_reload_and_resume() {
    use forge_provider_api::provider::Provider;
    for mode in ["native_instructions", "ok", "fork_no_instructions"] {
        let fixture = WireFixture::start(mode).await.unwrap();
        fixture.connection.shutdown().await.unwrap();
        let provider = fixture.claude_provider(fixture._directory.path().join("runtime"));
        let mut request = resume_request(&fixture, "instruction-owner");
        request.history = forge_provider_api::SessionHistory::Durable { resume: None };
        request.system_instructions = "base-instructions".into();
        let opened = provider
            .thread_open(operation_context(), request.clone())
            .await
            .unwrap();
        let fork_capability = opened
            .capabilities
            .capability
            .iter()
            .find(|entry| entry.id == forge_provider_api::capability::CapabilityId::NativeFork)
            .unwrap();
        assert_eq!(
            matches!(
                fork_capability.support,
                forge_provider_api::capability::Support::Unsupported
            ),
            mode != "native_instructions"
        );
        let mut events = opened.events;
        for index in 0..2 {
            let mut prompt = prompt_request(
                &opened.binding.id,
                forge_provider_api::InvocationScope::new(
                    request.owner.clone(),
                    format!("instruction-{index}"),
                ),
                receipt(&format!("instruction-{index}")),
                forge_provider_api::PromptInput::Text(format!("input-{index}")),
            );
            prompt.instructions = format!("dynamic-{index}");
            prompt.tool_endpoint = Some(forge_provider_api::HostToolEndpoint {
                url: format!("http://127.0.0.1:{}/mcp", 41000 + index),
                headers: Default::default(),
            });
            provider
                .turn_start(operation_context(), &opened.binding.id, prompt)
                .await
                .unwrap();
            next_completion(&mut events).await;
        }
        if mode != "ok" {
            let forked = provider
                .thread_fork(
                    operation_context(),
                    forge_provider_api::SessionForkRequest {
                        source: forge_provider_api::ForkSource::Binding(opened.binding.id.clone()),
                        target: forge_provider_api::ForkTarget {
                            owner: forge_provider_api::ThreadOwner::root("fork-owner"),
                            workspace: request.workspace.clone(),
                            configuration: request.configuration.clone(),
                            system_instructions: "fork-only".into(),
                        },
                    },
                )
                .await;
            if mode == "native_instructions" {
                forked.unwrap();
            } else {
                assert!(matches!(forked, Err(ProviderError::Unsupported(_))));
            }
        }
        provider
            .thread_close(operation_context(), &opened.binding.id)
            .await
            .unwrap();
        request.history = forge_provider_api::SessionHistory::Durable {
            resume: Some("fixture".to_owned().try_into().unwrap()),
        };
        request.system_instructions = "resume-base".into();
        let resumed = provider
            .thread_open(operation_context(), request.clone())
            .await
            .unwrap();
        let mut prompt = prompt_request(
            &resumed.binding.id,
            forge_provider_api::InvocationScope::new(request.owner, "resumed-input"),
            receipt("resumed-input"),
            forge_provider_api::PromptInput::Text("resumed-input".into()),
        );
        prompt.instructions.clear();
        provider
            .turn_start(operation_context(), &resumed.binding.id, prompt)
            .await
            .unwrap();
        next_completion(&mut { resumed.events }).await;
        provider
            .thread_close(operation_context(), &resumed.binding.id)
            .await
            .unwrap();
        let requests = std::fs::read_to_string(fixture._directory.path().join("mode.requests"))
            .unwrap()
            .lines()
            .map(|line| serde_json::from_str::<Value>(line).unwrap())
            .collect::<Vec<_>>();
        for request in requests.iter().filter(|request| {
            matches!(
                request["method"].as_str(),
                Some("session/new" | "session/load" | "session/fork")
            )
        }) {
            let options = &request["params"]["_meta"]["claudeCode"]["options"];
            assert_eq!(options["verbatimPrompts"], true);
            for denied in [
                "ProposeGoal",
                "CronCreate",
                "RemoteTrigger",
                "ScheduleWakeup",
            ] {
                assert!(
                    options["disallowedTools"]
                        .as_array()
                        .unwrap()
                        .iter()
                        .any(|tool| tool == denied)
                );
            }
        }
        let prompts = requests
            .iter()
            .filter(|request| request["method"] == "session/prompt")
            .collect::<Vec<_>>();
        assert_eq!(prompts.len(), 3);
        assert_eq!(prompts[2]["params"]["prompt"][0]["text"], "resume-base");
        if mode == "native_instructions" {
            assert_eq!(prompts[0]["params"]["prompt"][0]["text"], "dynamic-0");
            assert_eq!(prompts[1]["params"]["prompt"][0]["text"], "dynamic-1");
            for request in requests.iter().filter(|request| {
                request["method"] == "session/new" || request["method"] == "session/load"
            }) {
                assert_eq!(request["params"]["systemPrompt"]["mode"], "append");
                assert!(matches!(
                    request["params"]["systemPrompt"]["content"][0]["text"].as_str(),
                    Some("base-instructions" | "resume-base")
                ));
            }
            let fork = requests
                .iter()
                .find(|request| request["method"] == "session/fork")
                .unwrap();
            assert_eq!(
                fork["params"]["systemPrompt"]["content"][0]["text"],
                "fork-only"
            );
        } else {
            assert!(
                !requests
                    .iter()
                    .any(|request| request["method"] == "session/fork")
            );
            assert_eq!(
                prompts[0]["params"]["prompt"][0]["text"],
                "base-instructions"
            );
            assert_eq!(
                prompts[1]["params"]["prompt"][0]["text"],
                "base-instructions"
            );
            assert_eq!(prompts[2]["params"]["prompt"][0]["text"], "resume-base");
        }
    }
}

/// An old native CLI cannot silently ignore verbatim input when fresh or resumed history opens.
#[tokio::test]
async fn claude_old_native_cli_rejects_before_history_or_profile_mutation() {
    use forge_provider_api::provider::Provider;
    let fixture = WireFixture::start("native_instructions").await.unwrap();
    fixture.connection.shutdown().await.unwrap();
    std::fs::write(fixture._directory.path().join("native-version"), "2.1.247").unwrap();
    let runtime_root = fixture._directory.path().join("runtime");
    let provider = fixture.claude_provider(runtime_root.clone());
    for resume in [false, true] {
        let mut request = resume_request(&fixture, "unsupported-native");
        if !resume {
            request.history = forge_provider_api::SessionHistory::Durable { resume: None };
        }
        let result = provider.thread_open(operation_context(), request).await;
        assert!(matches!(result, Err(ProviderError::RuntimeVersion(
            forge_provider_api::version::RuntimeVersionError::Unsupported { requirement, detected }
        )) if *requirement.minimum() == semver::Version::new(2, 1, 248)
            && detected == semver::Version::new(2, 1, 247)));
    }
    assert!(!runtime_root.exists());
    assert!(!fixture._directory.path().join("mode.requests").exists());
    provider.shutdown(operation_context()).await.unwrap();
}

/// Native scheduling stays disabled when a fresh Antigravity binding reloads or resumes.
#[tokio::test]
async fn antigravity_native_schedule_filter_survives_endpoint_reload_and_resume() {
    use forge_provider_api::provider::Provider;
    let (fixture, provider, opened) = provider_fixture("ok").await;
    let binding = opened.binding.id;
    let mut events = opened.events;
    let mut request = prompt_request(
        &binding,
        forge_provider_api::InvocationScope::new(
            forge_provider_api::ThreadOwner::root("fixture-owner"),
            "schedule-filter",
        ),
        receipt("schedule-filter"),
        forge_provider_api::PromptInput::Text("ordinary input".into()),
    );
    request.tool_endpoint = Some(forge_provider_api::HostToolEndpoint {
        url: "http://127.0.0.1:41002/mcp".into(),
        headers: Default::default(),
    });
    provider
        .turn_start(operation_context(), &binding, request)
        .await
        .unwrap();
    next_completion(&mut events).await;
    provider
        .thread_close(operation_context(), &binding)
        .await
        .unwrap();
    let resumed = provider
        .thread_open(
            operation_context(),
            resume_request(&fixture, "resumed-owner"),
        )
        .await
        .unwrap();
    provider
        .thread_close(operation_context(), &resumed.binding.id)
        .await
        .unwrap();
    provider.shutdown(operation_context()).await.unwrap();
    let requests = std::fs::read_to_string(fixture._directory.path().join("mode.requests"))
        .unwrap()
        .lines()
        .map(|line| serde_json::from_str::<Value>(line).unwrap())
        .filter(|request| {
            matches!(
                request["method"].as_str(),
                Some("session/new" | "session/load")
            )
        })
        .collect::<Vec<_>>();
    assert!(
        requests
            .iter()
            .any(|request| request["method"] == "session/new")
    );
    assert!(
        requests
            .iter()
            .filter(|request| request["method"] == "session/load")
            .count()
            >= 2
    );
    for request in &requests {
        assert_eq!(
            request["params"]["_meta"]["antigravity"]["disabledTools"],
            json!(["schedule"])
        );
    }
}

/// False and null extension fields cannot advertise or dispatch a native fork.
#[tokio::test]
async fn disabled_native_extensions_reject_fork_before_dispatch() {
    use forge_provider_api::provider::Provider;
    let fixture = WireFixture::start("disabled_extensions").await.unwrap();
    fixture.connection.shutdown().await.unwrap();
    let provider = fixture.claude_provider(fixture._directory.path().join("runtime"));
    let opened = provider
        .thread_open(
            operation_context(),
            resume_request(&fixture, "source-owner"),
        )
        .await
        .unwrap();
    assert!(opened.capabilities.capability.iter().any(|entry| entry.id
        == forge_provider_api::capability::CapabilityId::NativeFork
        && entry.support == forge_provider_api::capability::Support::Unsupported));
    let request = resume_request(&fixture, "target-owner");
    let result = provider
        .thread_fork(
            operation_context(),
            forge_provider_api::SessionForkRequest {
                source: forge_provider_api::ForkSource::Binding(opened.binding.id.clone()),
                target: forge_provider_api::ForkTarget {
                    owner: request.owner,
                    workspace: request.workspace,
                    configuration: request.configuration,
                    system_instructions: "target instructions".into(),
                },
            },
        )
        .await;
    assert!(
        matches!(result, Err(ProviderError::Unsupported(_))),
        "unexpected unsupported fork disposition: {result:?}"
    );
    let requests =
        std::fs::read_to_string(fixture._directory.path().join("mode.requests")).unwrap();
    assert!(
        !requests
            .lines()
            .any(|request| request.contains("session/fork"))
    );
    provider
        .thread_close(operation_context(), &opened.binding.id)
        .await
        .unwrap();
    provider.shutdown(operation_context()).await.unwrap();
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
        .thread_close(operation_context(), &winner.binding.id)
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
            forge_provider_api::ThreadOwner::root("fixture-owner"),
            "failed-native",
        ),
        receipt("failed-native"),
        forge_provider_api::PromptInput::Text("fail".into()),
    );
    assert!(
        provider
            .turn_start(operation_context(), &binding, request)
            .await
            .is_err()
    );
    assert!(
        matches!(provider.thread_open(operation_context(), resume_request(&fixture, "replacement-owner")).await, Err(ProviderError::Failure(failure)) if failure.kind() == forge_provider_api::ProviderFailureKind::HistoryConflict)
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
    let _ = provider.thread_close(operation_context(), &binding).await;
    provider
        .thread_close(operation_context(), &binding)
        .await
        .unwrap();
    let resumed = provider
        .thread_open(
            operation_context(),
            resume_request(&fixture, "replacement-owner"),
        )
        .await
        .unwrap();
    provider
        .thread_close(operation_context(), &resumed.binding.id)
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
            forge_provider_api::ThreadOwner::root("fixture-owner"),
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
            .turn_start(operation, &submitted_binding, request)
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
        .turn_interrupt(
            operation_context(),
            &binding,
            forge_provider_api::CancelRequest { scope },
        )
        .await
        .unwrap();
    assert!(matches!(
        next_completion(&mut events).await.outcome,
        forge_provider_api::turn::TurnOutcome::Interrupted
    ));
    provider
        .thread_close(operation_context(), &binding)
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
            forge_provider_api::ThreadOwner::root("fixture-owner"),
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
            .turn_start(submitted_context, &submitted_binding, submitted_request)
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
        next_completion(&mut events).await.outcome,
        forge_provider_api::turn::TurnOutcome::Interrupted
    ));
    tokio::time::timeout(std::time::Duration::from_secs(2), async {
        loop {
            if provider
                .turn_start(context.clone(), &binding, request.clone())
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
        .thread_close(operation_context(), &binding)
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
        access: forge_provider_api::access::AccessPolicy {
            sandbox: false,
            ..Default::default()
        },
        approval: forge_provider_api::approval::ApprovalMode::Ask,
        resources: Default::default(),
    };
    assert!(
        matches!(provider.thread_configure(context, &binding, configuration).await, Err(ProviderError::Failure(failure)) if failure.kind() == forge_provider_api::ProviderFailureKind::Cancelled && failure.phase() == forge_provider_api::ProviderPhase::Admission && failure.disposition() == forge_provider_api::FailureDisposition::Unchanged)
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
            forge_provider_api::ThreadOwner::root("fixture-owner"),
            "after-cancelled-configure",
        ),
        receipt("after-cancelled-configure"),
        forge_provider_api::PromptInput::Text("still reusable".into()),
    );
    provider
        .turn_start(operation_context(), &binding, request)
        .await
        .unwrap();
    next_completion(&mut events).await;
    provider
        .thread_close(operation_context(), &binding)
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
            forge_provider_api::ThreadOwner::root("fixture-owner"),
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
        .turn_start(operation_context(), &binding, request)
        .await
        .unwrap_err();
    assert!(
        matches!(error, ProviderError::PreparationFailed(cause) if matches!(*cause, ProviderError::Failure(failure) if failure.operation() == forge_provider_api::ProviderOperation::TurnStart && failure.phase() == forge_provider_api::ProviderPhase::Preparation && failure.disposition() == forge_provider_api::FailureDisposition::Invalidated))
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
            forge_provider_api::ThreadOwner::root("fixture-owner"),
            "endpoint-none",
        ),
        receipt("endpoint-none"),
        forge_provider_api::PromptInput::Text("stale URL must not be reused".into()),
    );
    assert!(
        matches!(provider.turn_start(operation_context(), &binding, request).await, Err(ProviderError::Failure(failure)) if failure.kind() == forge_provider_api::ProviderFailureKind::BindingInvalidated)
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
    let _ = provider.thread_close(operation_context(), &binding).await;
    provider
        .thread_close(operation_context(), &binding)
        .await
        .unwrap();
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
    let requests_path = fixture._directory.path().join("mode.requests");
    let before = std::fs::read_to_string(&requests_path).unwrap();
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
            target: forge_provider_api::ForkTarget { owner: forge_provider_api::ThreadOwner::root("preflight-fork"),
                workspace: initial.workspace.clone(), system_instructions: String::new(), configuration: configuration.clone() },
        }).await, Err(ProviderError::Unsupported(UnsupportedCapability(feature))) if feature == expected)
        );
        if expected != ProviderFeature::McpLaunchRequirements {
            let mut prompt = prompt_request(
                &binding,
                forge_provider_api::InvocationScope::new(
                    initial.owner.clone(),
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
            std::fs::read_to_string(&requests_path).unwrap(),
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
        forge_provider_api::InvocationScope::new(initial.owner, "after-preflight"),
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

/// A cancelled prompt accepted before native processing retains delivery without fabricating lifecycle.
#[tokio::test]
async fn accepted_prompt_cancelled_before_start_has_no_started_or_completed_turn() {
    use forge_provider_api::{provider::Provider, *};
    let (fixture, provider, opened) = provider_fixture("held_no_start").await;
    let binding = opened.binding.id;
    let mut events = opened.events;
    let scope = InvocationScope::new(ThreadOwner::root("fixture-owner"), "no-start");
    let accepted = receipt("no-start");
    let request = prompt_request(
        &binding,
        scope.clone(),
        accepted.clone(),
        PromptInput::Text("cancel before work".into()),
    );
    let submitted_provider = provider.clone();
    let submitted_binding = binding.clone();
    let submitted = tokio::spawn(async move {
        submitted_provider
            .turn_start(operation_context(), &submitted_binding, request)
            .await
    });
    tokio::time::timeout(std::time::Duration::from_secs(2), async {
        loop {
            let log =
                std::fs::read_to_string(fixture._directory.path().join("mode.requests")).unwrap();
            if log.lines().any(|line| line.contains("session/prompt")) {
                break;
            }
            tokio::task::yield_now().await;
        }
    })
    .await
    .unwrap();
    let interrupted = provider
        .turn_interrupt(
            operation_context(),
            &binding,
            CancelRequest {
                scope: scope.clone(),
            },
        )
        .await
        .unwrap();
    assert_eq!(interrupted.turn_id, scope.turn_id().unwrap());
    assert!(matches!(
        submitted.await.unwrap(),
        Err(ProviderError::Cancelled)
    ));
    assert!(accepted.state.lock().unwrap().accepted);
    while let Some(event) = events.try_next().unwrap() {
        assert!(!matches!(
            event.event,
            ProviderEvent::Turn {
                event: TurnEvent::Started(_) | TurnEvent::Completed(_),
                ..
            } | ProviderEvent::Provider {
                event: ProviderConnectionEvent::Unavailable(_)
            }
        ));
    }
    provider
        .thread_close(operation_context(), &binding)
        .await
        .unwrap();
}
