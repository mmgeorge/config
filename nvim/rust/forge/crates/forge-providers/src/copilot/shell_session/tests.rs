use super::*;
use crate::copilot::{shell_tools::ShellTool, test_peer};
use async_trait::async_trait;
use forge_provider_api::permissions::{PermissionRequest, PermissionResponse, PermissionTarget};
use std::sync::atomic::{AtomicBool, Ordering};

struct Callbacks {
    cancelled: AtomicBool,
    decision: PermissionResponse,
    permissions: std::sync::Mutex<Vec<(String, PermissionRequest)>>,
    hold_permission: AtomicBool,
    authorizing: watch::Sender<bool>,
    release_permission: watch::Sender<bool>,
}

#[async_trait]
impl NativeCallbackRouter for Callbacks {
    /// Reject calls whose invocation was cancelled before native admission.
    async fn before_tool_native(&self, call: Option<&str>) -> Result<(), ProviderError> {
        assert_eq!(call, Some("outer-call"));
        if self.cancelled.load(Ordering::SeqCst) {
            Err(ProviderError::Cancelled)
        } else {
            Ok(())
        }
    }

    /// Capture the exact native authorization routed through the auxiliary session.
    async fn authorize_native(&self, call: &str, request: PermissionRequest) -> PermissionResponse {
        self.permissions
            .lock()
            .unwrap()
            .push((call.to_owned(), request));
        self.authorizing.send_replace(true);
        if self.hold_permission.load(Ordering::SeqCst) {
            let mut released = self.release_permission.subscribe();
            released.wait_for(|released| *released).await.unwrap();
        }
        self.decision.clone()
    }

    /// Keep custom host tools outside this shell fixture.
    async fn call_native(
        &self,
        _: &str,
        _: forge_provider_api::control::ToolCall,
    ) -> Result<String, ProviderError> {
        panic!("shell fixture must not invoke a host tool")
    }

    /// Leave invocation registration to the test's fixed outer call identity.
    async fn bind_native_call(
        &self,
        _: &str,
        _: &forge_provider_api::InvocationScope,
    ) -> Result<(), ProviderError> {
        Ok(())
    }

    /// Leave invocation retirement to the test's fixed outer call identity.
    async fn release_native_call(&self, _: &str) {}
}

struct Fixture {
    shell: Arc<ShellSession>,
    client: CopilotClient,
    peer: test_peer::SdkPeer,
    callbacks: Arc<Callbacks>,
    _events: forge_provider_api::events::ProviderEventStream,
}

impl Fixture {
    /// Connect the production shell owner to a framed native peer with controllable admission.
    async fn new(decision: PermissionResponse, fail_pause: bool, permissions: bool) -> Self {
        Self::deferred_create(decision, fail_pause, permissions, false).await
    }

    /// Hold native allocation until a fixture control RPC releases its response.
    async fn deferred_create(
        decision: PermissionResponse,
        fail_pause: bool,
        permissions: bool,
        defer_create: bool,
    ) -> Self {
        let mut pending_create = None;
        let mut tasks = Vec::<Value>::new();
        let mut pending = None;
        let (native, peer) = test_peer::client(move |request| {
            let method = request["method"].as_str().unwrap();
            let result = match method {
                "mcp.discover" => json!({"servers":[{
                    "name":"inherited-fixture","enabled":true,"source":"user","type":"stdio"
                }]}),
                "session.create" => {
                    assert_eq!(
                        request["params"]["disabledMcpServers"],
                        json!(["inherited-fixture"])
                    );
                    assert_eq!(request["params"]["mcpServers"], json!({}));
                    if defer_create {
                        pending_create = Some(request.clone());
                        return Vec::new();
                    }
                    json!({"sessionId":request["params"]["sessionId"]})
                }
                "fixture.releaseCreate" => {
                    let pending = pending_create.take().unwrap();
                    return vec![
                        test_peer::response(
                            &pending,
                            json!({"sessionId":pending["params"]["sessionId"]}),
                        ),
                        test_peer::response(request, json!({})),
                    ];
                }
                "session.options.update" => {
                    assert_eq!(request["params"]["includedBuiltinSkills"], json!([]));
                    json!({"success":true})
                }
                "session.delete" => json!({"success":true}),
                "session.detach" => json!({"success":true}),
                "session.queue.setDrainPaused" => {
                    assert_eq!(request["params"]["paused"], true);
                    if fail_pause {
                        return vec![
                            json!({"jsonrpc":"2.0","id":request["id"],"error":{"code":-32603,"message":"fixture pause failure"}}),
                        ];
                    }
                    json!({})
                }
                "session.tools.execute" => {
                    assert_eq!(request["params"]["toolCallId"], "outer-call");
                    if request["params"]["name"] == command_tool().name() {
                        assert_eq!(request["params"]["arguments"]["mode"], "async");
                        assert!(request["params"]["arguments"].get("initial_wait").is_none());
                        if permissions {
                            pending = Some(request.clone());
                            return vec![
                                json!({"jsonrpc":"2.0","method":"session.event","params":{
                                    "sessionId":request["params"]["sessionId"],"event":{
                                        "id":"permission-event","timestamp":"2026-11-04T00:00:00Z","type":"permission.requested",
                                        "data":{"requestId":"permission-id","permissionRequest":{"kind":"shell","toolCallId":"outer-call","fullCommandText":"fixture command"}}
                                    }
                                }}),
                            ];
                        }
                        tasks.push(json!({"id":request["params"]["arguments"]["shellId"],"type":"shell","status":"running","attachmentMode":"attached","executionMode":"background","command":"fixture command","description":"fixture"}));
                    }
                    json!({"textResultForLlm":"fixture output","resultType":"success"})
                }
                "session.permissions.handlePendingPermissionRequest" => {
                    let pending = pending.take().unwrap();
                    let allowed = request["params"]["result"]["kind"] == "approve-once";
                    if allowed {
                        tasks.push(json!({"id":pending["params"]["arguments"]["shellId"],"type":"shell","status":"running","attachmentMode":"attached","executionMode":"background","command":"fixture command"}));
                    }
                    return vec![
                        test_peer::response(request, json!({})),
                        test_peer::response(
                            &pending,
                            json!({"textResultForLlm":"permission outcome","resultType":if allowed {"success"} else {"failure"}}),
                        ),
                    ];
                }
                "session.tasks.list" => json!({"tasks":tasks}),
                "session.tasks.cancel" => {
                    tasks.retain(|task| task["id"] != request["params"]["id"]);
                    json!({"cancelled":true})
                }
                other => panic!("unexpected shell fixture RPC: {other}"),
            };
            vec![test_peer::response(request, result)]
        });
        let client = CopilotClient::new(native);
        let callbacks = Arc::new(Callbacks {
            cancelled: AtomicBool::new(false),
            decision,
            permissions: std::sync::Mutex::new(Vec::new()),
            hold_permission: AtomicBool::new(false),
            authorizing: watch::channel(false).0,
            release_permission: watch::channel(false).0,
        });
        let router: Arc<dyn NativeCallbackRouter> = callbacks.clone();
        let owner = forge_provider_api::testing::root_thread("shell-fixture");
        let (sink, events) = forge_provider_api::events::channel(
            forge_provider_api::BindingId::new("shell-fixture").unwrap(),
            forge_provider_api::EventScope::Binding {
                thread_id: owner.clone(),
            },
        );
        let shell = Arc::new(ShellSession::new(Arc::downgrade(&router), owner, sink));
        shell
            .prepare(client.clone(), std::env::temp_dir(), "main-native-fixture")
            .await
            .unwrap();
        Self {
            shell,
            client,
            peer,
            callbacks,
            _events: events,
        }
    }

    /// Collect private history before releasing the native transport.
    async fn finish(self) -> Vec<Value> {
        self.shell.close().await.unwrap();
        self.client.stop().await.unwrap();
        self.peer.collected().await
    }
}

/// Ensure permanent queue pause precedes native execution and close removes private history.
#[tokio::test]
async fn paused_shell_orders_execution_and_removes_private_history() {
    let fixture = Fixture::new(PermissionResponse::Allow, false, false).await;
    fixture.shell.execute("outer-call", command_tool(), json!({"command":"fixture command","description":"fixture","mode":"async","shellId":"task"})).await.unwrap();
    fixture
        .shell
        .prepare(
            fixture.client.clone(),
            std::env::temp_dir(),
            "main-native-fixture",
        )
        .await
        .unwrap();
    fixture.shell.execute("outer-call", command_tool(), json!({"command":"second command","description":"fixture","mode":"async","shellId":"second-task"})).await.unwrap();
    assert_eq!(
        Arc::strong_count(&fixture.callbacks),
        1,
        "idle SDK handlers must not retain binding authority"
    );
    fixture.shell.close().await.unwrap();
    assert_eq!(
        Arc::strong_count(&fixture.callbacks),
        1,
        "closed helper must release binding authority"
    );
    let requests = fixture.finish().await;
    let methods: Vec<_> = requests
        .iter()
        .map(|request| request["method"].as_str().unwrap())
        .collect();
    let cleanup = methods
        .iter()
        .position(|method| *method == "session.delete")
        .unwrap();
    let create = methods
        .iter()
        .position(|method| *method == "session.create")
        .unwrap();
    assert!(
        cleanup < create,
        "stale private history must be removed before reuse"
    );
    assert_eq!(
        requests[cleanup]["params"]["sessionId"],
        requests[create]["params"]["sessionId"]
    );
    assert_eq!(
        methods
            .iter()
            .filter(|method| **method == "session.delete")
            .count(),
        2
    );
    assert!(
        methods
            .iter()
            .position(|method| *method == "session.queue.setDrainPaused")
            .unwrap()
            < methods
                .iter()
                .position(|method| *method == "session.tools.execute")
                .unwrap()
    );
    assert_eq!(
        methods
            .iter()
            .filter(|method| **method == "session.create")
            .count(),
        1
    );
    assert_eq!(
        methods
            .iter()
            .filter(|method| **method == "session.queue.setDrainPaused")
            .count(),
        1
    );
    assert!(methods.contains(&"session.tasks.cancel"));
    assert!(
        methods
            .iter()
            .position(|method| *method == "session.detach")
            .unwrap()
            < methods
                .iter()
                .rposition(|method| *method == "session.delete")
                .unwrap()
    );
    assert!(!methods.contains(&"session.send"));
}

/// Preserve the outer tool identity and denial through the real SDK permission callback.
#[tokio::test]
async fn paused_shell_routes_exact_permission_and_denial() {
    for decision in [
        PermissionResponse::Allow,
        PermissionResponse::Deny {
            reason: Some("denied".into()),
        },
    ] {
        let allowed = matches!(decision, PermissionResponse::Allow);
        let fixture = Fixture::new(decision, false, true).await;
        let result = fixture
            .shell
            .execute(
                "outer-call",
                command_tool(),
                json!({"command":"fixture command","description":"fixture","mode":"async"}),
            )
            .await
            .unwrap();
        assert_eq!(result.result_type == NativeOutcome::Success, allowed);
        {
            let permissions = fixture.callbacks.permissions.lock().unwrap();
            assert_eq!(permissions.len(), 1);
            assert_eq!(permissions[0].0, "outer-call");
            assert_eq!(permissions[0].1.id, "permission-id");
            assert!(
                matches!(&permissions[0].1.target_list[..], [PermissionTarget::Command { command, .. }] if command == "fixture command")
            );
        }
        fixture.finish().await;
    }
}

/// Select the same native command family as the production auxiliary session.
fn command_tool() -> ShellTool {
    if cfg!(windows) {
        ShellTool::PowerShell
    } else {
        ShellTool::Bash
    }
}

/// Reject a late native approval when its exact invocation was cancelled during authorization.
#[tokio::test]
async fn paused_shell_cancel_during_permission_rejects_late_approval() {
    let fixture = Fixture::new(PermissionResponse::Allow, false, true).await;
    fixture
        .callbacks
        .hold_permission
        .store(true, Ordering::SeqCst);
    let mut authorizing = fixture.callbacks.authorizing.subscribe();
    let shell = fixture.shell.clone();
    let call = tokio::spawn(async move {
        shell
            .execute(
                "outer-call",
                command_tool(),
                json!({"command":"fixture command","description":"fixture","mode":"async"}),
            )
            .await
    });
    authorizing.wait_for(|pending| *pending).await.unwrap();
    fixture.callbacks.cancelled.store(true, Ordering::SeqCst);
    fixture.callbacks.release_permission.send_replace(true);
    let result = call.await.unwrap().unwrap();
    assert!(result.result_type == NativeOutcome::Failure);
    let requests = fixture.finish().await;
    let decision = requests
        .iter()
        .find(|request| request["method"] == "session.permissions.handlePendingPermissionRequest")
        .unwrap();
    assert_eq!(decision["params"]["result"]["kind"], "reject");
}
