use super::*;
use crate::copilot::{shell_tools::ShellTool, test_peer};
use async_trait::async_trait;
use forge_provider_api::permissions::{PermissionRequest, PermissionResponse, PermissionTarget};
use std::sync::atomic::{AtomicBool, Ordering};

#[derive(Clone, Copy)]
enum NativeBehavior {
    Normal,
    DeferredLaunch,
    FailFirstCancel,
    MalformedLaunch,
}

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
    async fn bind_native_call(&self, _: &str, _: &forge_provider_api::InvocationScope) {}

    /// Leave invocation retirement to the test's fixed outer call identity.
    async fn release_native_call(&self, _: &str) {}
}

struct Fixture {
    shell: Arc<ShellSession>,
    client: CopilotClient,
    peer: test_peer::SdkPeer,
    callbacks: Arc<Callbacks>,
    requests: Arc<std::sync::Mutex<Vec<Value>>>,
    launched: watch::Receiver<bool>,
    creating: watch::Receiver<bool>,
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
        Self::configured(
            decision,
            fail_pause,
            permissions,
            defer_create,
            NativeBehavior::Normal,
        )
        .await
    }

    /// Apply one explicit native failure or completion boundary to the framed peer.
    async fn configured(
        decision: PermissionResponse,
        fail_pause: bool,
        permissions: bool,
        defer_create: bool,
        behavior: NativeBehavior,
    ) -> Self {
        let requests = Arc::new(std::sync::Mutex::new(Vec::new()));
        let captured = requests.clone();
        let (launched, launch_receiver) = watch::channel(false);
        let (creating, creating_receiver) = watch::channel(false);
        let mut pending_create = None;
        let mut tasks = Vec::<Value>::new();
        let mut pending = None;
        let mut pending_execute = None;
        let mut failed_cancel = false;
        let (native, peer) = test_peer::client(move |request| {
            captured.lock().unwrap().push(request.clone());
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
                    creating.send_replace(true);
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
                "fixture.releaseExecute" => {
                    let pending: Value = pending_execute.take().unwrap();
                    tasks.push(json!({"id":pending["params"]["arguments"]["shellId"],"type":"shell","status":"running","attachmentMode":"attached","executionMode":"background","command":"fixture command"}));
                    return vec![
                        test_peer::response(
                            &pending,
                            json!({"textResultForLlm":"late launch acknowledgement","resultType":"success"}),
                        ),
                        test_peer::response(request, json!({})),
                    ];
                }
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
                        if matches!(behavior, NativeBehavior::DeferredLaunch) {
                            pending_execute = Some(request.clone());
                            launched.send_replace(true);
                            return Vec::new();
                        }
                        tasks.push(json!({"id":request["params"]["arguments"]["shellId"],"type":"shell","status":"running","attachmentMode":"attached","executionMode":"background","command":"fixture command","description":"fixture"}));
                        launched.send_replace(true);
                        if matches!(behavior, NativeBehavior::MalformedLaunch) {
                            return vec![test_peer::response(
                                request,
                                json!({"resultType":"unknown"}),
                            )];
                        }
                    }
                    json!({"textResultForLlm":"fixture output","resultType":"success"})
                }
                "session.permissions.handlePendingPermissionRequest" => {
                    let pending = pending.take().unwrap();
                    let allowed = request["params"]["result"]["kind"] == "approve-once";
                    if allowed {
                        tasks.push(json!({"id":pending["params"]["arguments"]["shellId"],"type":"shell","status":"running","attachmentMode":"attached","executionMode":"background","command":"fixture command"}));
                        launched.send_replace(true);
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
                    if matches!(behavior, NativeBehavior::FailFirstCancel) && !failed_cancel {
                        failed_cancel = true;
                        return vec![
                            json!({"jsonrpc":"2.0","id":request["id"],"error":{"code":-32603,"message":"fixture cancellation failed"}}),
                        ];
                    }
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
        let owner = ExecutionOwner::root("shell-fixture");
        let (sink, events) = forge_provider_api::events::channel(
            forge_provider_api::BindingId::new("shell-fixture").unwrap(),
            forge_provider_api::EventScope::Binding {
                owner: owner.clone(),
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
            requests,
            launched: launch_receiver,
            creating: creating_receiver,
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

/// A zero soft wait is valid for either public launch mode and is not a native argument.
#[tokio::test]
async fn paused_shell_zero_initial_wait_stays_in_adapter_for_sync_and_async() {
    let fixture = Fixture::new(PermissionResponse::Allow, false, false).await;
    for mode in ["sync", "async"] {
        let result = fixture.shell.execute(
            "outer-call", command_tool(),
            json!({"command":"fixture command", "description":"fixture", "mode":mode, "initial_wait":0, "shellId":format!("zero-{mode}")}),
        ).await.unwrap();
        assert!(matches!(result.result_type, NativeOutcome::Success));
    }
    let requests = fixture.finish().await;
    let launches = requests
        .iter()
        .filter(|request| request["method"] == "session.tools.execute")
        .collect::<Vec<_>>();
    assert_eq!(
        launches.len(),
        2,
        "zero initial wait must not issue an implicit output read"
    );
    for launch in launches {
        assert_eq!(launch["params"]["arguments"]["mode"], "async");
        assert!(launch["params"]["arguments"].get("initial_wait").is_none());
    }
}

/// Fence cancelled calls before allocating auxiliary native state.
#[tokio::test]
async fn paused_shell_cancel_before_launch_allocates_nothing() {
    let fixture = Fixture::new(PermissionResponse::Allow, false, false).await;
    fixture.callbacks.cancelled.store(true, Ordering::SeqCst);
    assert!(matches!(
        fixture
            .shell
            .execute(
                "outer-call",
                command_tool(),
                json!({"command":"fixture command","description":"fixture"})
            )
            .await,
        Err(ProviderError::Cancelled)
    ));
    assert!(fixture.finish().await.is_empty());
}

/// Release a soft foreground wait without stopping the native terminal or reusing its label.
#[tokio::test]
async fn paused_shell_interrupted_wait_retains_terminal_and_rejects_label_reuse() {
    let mut fixture = Fixture::new(PermissionResponse::Allow, false, false).await;
    let shell = fixture.shell.clone();
    let call = tokio::spawn(async move {
        shell.execute("outer-call", command_tool(), json!({"command":"fixture command","description":"fixture","shellId":"task","initial_wait":3600})).await
    });
    tokio::time::timeout(
        Duration::from_secs(2),
        fixture.launched.wait_for(|launched| *launched),
    )
    .await
    .unwrap()
    .unwrap();
    fixture.shell.interrupt_waits();
    tokio::time::timeout(Duration::from_secs(2), call)
        .await
        .unwrap()
        .unwrap()
        .unwrap();
    assert_eq!(fixture.shell.snapshot().await.unwrap().terminal.len(), 1);
    assert!(fixture.shell.execute("outer-call", command_tool(), json!({"command":"second command","description":"fixture","mode":"async","shellId":"task"})).await.is_err());
    assert!(
        !fixture
            .requests
            .lock()
            .unwrap()
            .iter()
            .any(|request| request["method"] == "session.tasks.cancel")
    );
    fixture.finish().await;
}

/// Retain native handoff after its caller disappears and collect it during close.
#[tokio::test]
async fn paused_shell_dropped_awaiter_retains_execution_until_close() {
    let mut fixture = Fixture::new(PermissionResponse::Allow, false, false).await;
    let shell = fixture.shell.clone();
    let call = tokio::spawn(async move {
        shell.execute("outer-call", command_tool(), json!({"command":"fixture command","description":"fixture","shellId":"task","initial_wait":3600})).await
    });
    tokio::time::timeout(
        Duration::from_secs(2),
        fixture.launched.wait_for(|launched| *launched),
    )
    .await
    .unwrap()
    .unwrap();
    call.abort();
    let requests = tokio::time::timeout(Duration::from_secs(2), fixture.finish())
        .await
        .unwrap();
    assert!(
        requests
            .iter()
            .any(|request| request["method"] == "session.delete")
    );
}

/// Dispose a helper whose pause failed, and never allocate again after close.
#[tokio::test]
async fn paused_shell_failed_pause_is_collected_and_close_fences_allocation() {
    let fixture = Fixture::new(PermissionResponse::Allow, true, false).await;
    assert!(
        fixture
            .shell
            .execute(
                "outer-call",
                command_tool(),
                json!({"command":"fixture command","description":"fixture"})
            )
            .await
            .is_err()
    );
    fixture.shell.close().await.unwrap();
    assert!(
        fixture
            .shell
            .execute(
                "outer-call",
                command_tool(),
                json!({"command":"second command","description":"fixture"})
            )
            .await
            .is_err()
    );
    fixture.client.stop().await.unwrap();
    let requests = fixture.peer.collected().await;
    assert_eq!(
        requests
            .iter()
            .filter(|request| request["method"] == "session.create")
            .count(),
        1
    );
    assert_eq!(
        requests
            .iter()
            .filter(|request| request["method"] == "session.delete")
            .count(),
        2
    );
    assert!(
        !requests
            .iter()
            .any(|request| request["method"] == "session.tools.execute")
    );
}

/// Select the same native command family as the production auxiliary session.
fn command_tool() -> ShellTool {
    if cfg!(windows) {
        ShellTool::PowerShell
    } else {
        ShellTool::Bash
    }
}

/// Retain allocation after its initial caller drops, then collect the late native session.
#[tokio::test]
async fn paused_shell_close_collects_dropped_inflight_creation() {
    let mut fixture = Fixture::deferred_create(PermissionResponse::Allow, false, false, true).await;
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
    tokio::time::timeout(
        Duration::from_secs(2),
        fixture.creating.wait_for(|creating| *creating),
    )
    .await
    .unwrap()
    .unwrap();
    call.abort();
    let shell = fixture.shell.clone();
    let close = tokio::spawn(async move { shell.close().await });
    tokio::time::timeout(Duration::from_secs(2), async {
        loop {
            if matches!(
                fixture.shell.state.lock().await.admission,
                Admission::Closed
            ) {
                break;
            }
            tokio::task::yield_now().await;
        }
    })
    .await
    .unwrap();
    fixture
        .client
        .inner
        .call("fixture.releaseCreate", Some(json!({})))
        .await
        .unwrap();
    tokio::time::timeout(Duration::from_secs(2), close)
        .await
        .unwrap()
        .unwrap()
        .unwrap();
    fixture.client.stop().await.unwrap();
    let requests = fixture.peer.collected().await;
    assert_eq!(
        requests
            .iter()
            .filter(|request| request["method"] == "session.create")
            .count(),
        1
    );
    assert_eq!(
        requests
            .iter()
            .filter(|request| request["method"] == "session.delete")
            .count(),
        2
    );
    assert!(
        !requests
            .iter()
            .any(|request| request["method"] == "session.tools.execute")
    );
}

/// Recheck cancellation after slow native initialization and before handing off a command.
#[tokio::test]
async fn paused_shell_cancel_during_creation_prevents_native_launch() {
    let mut fixture = Fixture::deferred_create(PermissionResponse::Allow, false, false, true).await;
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
    tokio::time::timeout(
        Duration::from_secs(2),
        fixture.creating.wait_for(|creating| *creating),
    )
    .await
    .unwrap()
    .unwrap();
    fixture.callbacks.cancelled.store(true, Ordering::SeqCst);
    fixture
        .client
        .inner
        .call("fixture.releaseCreate", Some(json!({})))
        .await
        .unwrap();
    assert!(matches!(
        tokio::time::timeout(Duration::from_secs(2), call)
            .await
            .unwrap()
            .unwrap(),
        Err(ProviderError::Cancelled)
    ));
    let requests = fixture.finish().await;
    assert!(
        !requests
            .iter()
            .any(|request| request["method"] == "session.tools.execute")
    );
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
    tokio::time::timeout(
        Duration::from_secs(2),
        authorizing.wait_for(|pending| *pending),
    )
    .await
    .unwrap()
    .unwrap();
    fixture.callbacks.cancelled.store(true, Ordering::SeqCst);
    fixture.callbacks.release_permission.send_replace(true);
    let result = tokio::time::timeout(Duration::from_secs(2), call)
        .await
        .unwrap()
        .unwrap()
        .unwrap();
    assert!(result.result_type == NativeOutcome::Failure);
    let requests = fixture.finish().await;
    let decision = requests
        .iter()
        .find(|request| request["method"] == "session.permissions.handlePendingPermissionRequest")
        .unwrap();
    assert_eq!(decision["params"]["result"]["kind"], "reject");
}

/// Collect a helper whose native permission request is still awaiting a revoked callback.
#[tokio::test]
async fn paused_shell_close_during_permission_collects_without_launch() {
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
    tokio::time::timeout(
        Duration::from_secs(2),
        authorizing.wait_for(|pending| *pending),
    )
    .await
    .unwrap()
    .unwrap();
    // Binding close revokes its callbacks before collecting the shell helper.
    fixture.callbacks.cancelled.store(true, Ordering::SeqCst);
    let shell = fixture.shell.clone();
    let close = tokio::spawn(async move { shell.close().await });
    tokio::time::timeout(Duration::from_secs(2), async {
        loop {
            if matches!(
                fixture.shell.state.lock().await.admission,
                Admission::Closed
            ) {
                break;
            }
            tokio::task::yield_now().await;
        }
    })
    .await
    .unwrap();
    tokio::time::timeout(Duration::from_secs(2), close)
        .await
        .unwrap()
        .unwrap()
        .unwrap();
    let result = call.await.unwrap();
    assert!(
        result.is_err() || result.is_ok_and(|result| result.result_type == NativeOutcome::Failure)
    );
    assert!(!*fixture.launched.borrow());
    fixture.client.stop().await.unwrap();
    let requests = fixture.peer.collected().await;
    assert!(
        requests
            .iter()
            .any(|request| request["method"] == "session.delete")
    );
}

/// Exercise deletion, mandatory shutdown, and retry after cleanup transport allocation.
/// The seeded phase excludes native process spawning and factory allocation from this coverage.
#[tokio::test]
async fn paused_shell_collected_generation_cleans_history_and_collects_failed_retry_transport() {
    let fixture = Fixture::new(PermissionResponse::Allow, false, false).await;
    fixture.client.stop().await.unwrap();
    let id = SessionId::new("private-history-after-generation-loss");
    for reject_delete in [true, false] {
        let runtime = Arc::new(crate::copilot::runtime::CopilotRuntime::new(Vec::new()));
        let (reader, writer, peer) = test_peer::streams(move |request| {
            let result = match request["method"].as_str().unwrap() {
                "status.get" => json!({"version":"1.0.103"}),
                "session.delete" => {
                    assert_eq!(
                        request["params"]["sessionId"],
                        "private-history-after-generation-loss"
                    );
                    if reject_delete {
                        return vec![
                            json!({"jsonrpc":"2.0","id":request["id"],"error":{"code":-32603,"message":"fixture deletion failed"}}),
                        ];
                    }
                    json!({"success":true})
                }
                other => panic!("unexpected cleanup fixture RPC: {other}"),
            };
            vec![test_peer::response(request, result)]
        });
        runtime
            .attach_streams(reader, writer, std::env::temp_dir())
            .await
            .unwrap();
        *fixture.shell.history_cleanup.lock().await = HistoryCleanup::Deleting(runtime.clone());
        let deletion = tokio::time::timeout(
            Duration::from_secs(2),
            fixture
                .shell
                .delete_history(&fixture.client, &id, &std::env::temp_dir()),
        )
        .await
        .unwrap();
        assert_eq!(deletion.is_err(), reject_delete);
        assert_eq!(
            runtime.start_count(),
            0,
            "the phase fixture must not launch a native executable"
        );
        assert!(
            runtime
                .client(&std::env::temp_dir().to_string_lossy())
                .await
                .is_err(),
            "cleanup shutdown must close admission on every deletion outcome"
        );
        let requests = tokio::time::timeout(Duration::from_secs(2), peer.collected())
            .await
            .unwrap();
        let methods: Vec<_> = requests
            .iter()
            .map(|request| request["method"].as_str().unwrap())
            .collect();
        assert_eq!(methods, ["status.get", "session.delete"]);
        if reject_delete {
            assert!(
                matches!(
                    *fixture.shell.history_cleanup.lock().await,
                    HistoryCleanup::Pending
                ),
                "a failed deletion must permit another cleanup attempt"
            );
        } else {
            assert!(matches!(
                *fixture.shell.history_cleanup.lock().await,
                HistoryCleanup::Complete
            ));
        }
    }
    assert!(
        fixture.peer.collected().await.is_empty(),
        "history deletion must not use the collected original transport"
    );
}

/// Retain uncertain launch ownership until native acknowledgement permits task reconciliation.
#[tokio::test]
async fn paused_shell_close_retains_unacknowledged_launch_until_native_reply() {
    let mut fixture = Fixture::configured(
        PermissionResponse::Allow,
        false,
        false,
        false,
        NativeBehavior::DeferredLaunch,
    )
    .await;
    let shell = fixture.shell.clone();
    let call = tokio::spawn(async move {
        shell.execute("outer-call", command_tool(), json!({"command":"fixture command","description":"fixture","mode":"async","shellId":"blocked-task"})).await
    });
    tokio::time::timeout(
        Duration::from_secs(2),
        fixture.launched.wait_for(|launched| *launched),
    )
    .await
    .unwrap()
    .unwrap();
    assert!(
        !call.is_finished(),
        "native acknowledgement must remain withheld"
    );
    let shell = fixture.shell.clone();
    let close = tokio::spawn(async move { shell.close().await });
    tokio::time::timeout(Duration::from_secs(2), async {
        loop {
            if matches!(
                fixture.shell.state.lock().await.admission,
                Admission::Closed
            ) {
                break;
            }
            tokio::task::yield_now().await;
        }
    })
    .await
    .unwrap();
    assert!(
        !close.is_finished(),
        "uncertain native launch must retain the helper cleanup owner"
    );
    {
        let requests = fixture.requests.lock().unwrap();
        assert!(
            !requests
                .iter()
                .any(|request| request["method"] == "session.detach")
        );
        assert_eq!(
            requests
                .iter()
                .filter(|request| request["method"] == "session.delete")
                .count(),
            1
        );
    }
    fixture
        .client
        .inner
        .call("fixture.releaseExecute", Some(json!({})))
        .await
        .unwrap();
    tokio::time::timeout(Duration::from_secs(2), close)
        .await
        .unwrap()
        .unwrap()
        .unwrap();
    let _settled_execution = tokio::time::timeout(Duration::from_secs(2), call)
        .await
        .unwrap()
        .unwrap();
    assert!(
        fixture.shell.state.lock().await.calls.is_empty(),
        "close must collect every retained execute worker"
    );
    let requests = fixture.finish().await;
    let methods: Vec<_> = requests
        .iter()
        .map(|request| request["method"].as_str().unwrap())
        .collect();
    assert!(
        methods
            .iter()
            .position(|method| *method == "fixture.releaseExecute")
            .unwrap()
            < methods
                .iter()
                .position(|method| *method == "session.tasks.cancel")
                .unwrap()
    );
    assert!(
        methods
            .iter()
            .position(|method| *method == "session.tasks.cancel")
            .unwrap()
            < methods
                .iter()
                .position(|method| *method == "session.detach")
                .unwrap()
    );
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
}

/// Continue sibling cancellation after a failure and retry unfinished quiescence before history removal.
#[tokio::test]
async fn paused_shell_failed_cancel_collects_siblings_and_retries_before_history_delete() {
    let fixture = Fixture::configured(
        PermissionResponse::Allow,
        false,
        false,
        false,
        NativeBehavior::FailFirstCancel,
    )
    .await;
    for id in ["first-task", "second-task"] {
        fixture.shell.execute("outer-call", command_tool(), json!({"command":"fixture command","description":"fixture","mode":"async","shellId":id})).await.unwrap();
    }
    assert!(
        tokio::time::timeout(Duration::from_secs(2), fixture.shell.close())
            .await
            .unwrap()
            .is_err()
    );
    {
        let requests = fixture.requests.lock().unwrap();
        let cancelled: Vec<_> = requests
            .iter()
            .filter(|request| request["method"] == "session.tasks.cancel")
            .map(|request| request["params"]["id"].as_str().unwrap())
            .collect();
        assert_eq!(cancelled, ["first-task", "second-task"]);
        assert!(
            !requests
                .iter()
                .any(|request| request["method"] == "session.detach")
        );
        assert_eq!(
            requests
                .iter()
                .filter(|request| request["method"] == "session.delete")
                .count(),
            1,
            "history must remain until task quiescence is proven"
        );
    }
    tokio::time::timeout(Duration::from_secs(2), fixture.shell.close())
        .await
        .unwrap()
        .unwrap();
    let requests = fixture.finish().await;
    assert_eq!(
        requests
            .iter()
            .filter(|request| request["method"] == "session.delete")
            .count(),
        2
    );
    assert_eq!(
        requests
            .iter()
            .filter(|request| request["method"] == "session.detach")
            .count(),
        1
    );
    let cancelled: Vec<_> = requests
        .iter()
        .filter(|request| request["method"] == "session.tasks.cancel")
        .map(|request| request["params"]["id"].as_str().unwrap())
        .collect();
    assert_eq!(cancelled, ["first-task", "second-task", "first-task"]);
}

/// Preserve an uncertain launch after an invalid acknowledgement until its native process is collected.
#[tokio::test]
async fn paused_shell_malformed_acknowledgement_does_not_release_launch_ownership() {
    let fixture = Fixture::configured(
        PermissionResponse::Allow,
        false,
        false,
        false,
        NativeBehavior::MalformedLaunch,
    )
    .await;
    let result = fixture.shell.execute("outer-call", command_tool(), json!({
        "command":"fixture command","description":"fixture","mode":"async","shellId":"uncertain-task"
    })).await;
    assert!(result.is_err());
    assert!(
        fixture
            .shell
            .state
            .lock()
            .await
            .calls
            .iter()
            .any(|call| *call.launch.borrow() == LaunchDisposition::Uncertain)
    );
    let retry = fixture
        .shell
        .execute(
            "outer-call",
            command_tool(),
            json!({
                "command":"another command","description":"fixture","mode":"async"
            }),
        )
        .await;
    assert!(
        retry.is_err(),
        "an unknown launch cannot authorize another native command"
    );
    assert!(
        tokio::time::timeout(Duration::from_secs(2), fixture.shell.close())
            .await
            .unwrap()
            .is_err()
    );
    {
        let requests = fixture.requests.lock().unwrap();
        assert_eq!(
            requests
                .iter()
                .filter(|request| request["method"] == "session.tools.execute")
                .count(),
            1
        );
        assert!(
            !requests
                .iter()
                .any(|request| request["method"] == "session.detach")
        );
        assert_eq!(
            requests
                .iter()
                .filter(|request| request["method"] == "session.delete")
                .count(),
            1
        );
    }
    fixture.client.stop().await.unwrap();
    // The independent cleanup transport is tested separately; seed its deletion phase here.
    let runtime = Arc::new(crate::copilot::runtime::CopilotRuntime::new(Vec::new()));
    let (reader, writer, cleanup_peer) = test_peer::streams(|request| {
        let result = match request["method"].as_str().unwrap() {
            "status.get" => json!({"version":"1.0.103"}),
            "session.delete" => json!({"success":true}),
            method => panic!("unexpected uncertain-launch cleanup RPC: {method}"),
        };
        vec![test_peer::response(request, result)]
    });
    runtime
        .attach_streams(reader, writer, std::env::temp_dir())
        .await
        .unwrap();
    *fixture.shell.history_cleanup.lock().await = HistoryCleanup::Deleting(runtime.clone());
    tokio::time::timeout(Duration::from_secs(2), fixture.shell.close())
        .await
        .unwrap()
        .unwrap();
    assert!(fixture.shell.state.lock().await.calls.is_empty());
    assert!(
        runtime
            .client(&std::env::temp_dir().to_string_lossy())
            .await
            .is_err()
    );
    let cleanup = cleanup_peer.collected().await;
    assert_eq!(
        cleanup
            .iter()
            .filter(|request| request["method"] == "session.delete")
            .count(),
        1
    );
    fixture.peer.collected().await;
}
