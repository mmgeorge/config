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
    tasks: Arc<std::sync::Mutex<Vec<Value>>>,
    events: forge_provider_api::events::ProviderEventStream,
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
        let tasks = Arc::new(std::sync::Mutex::new(Vec::<Value>::new()));
        let native_tasks = tasks.clone();
        let mut pending = None;
        let (native, peer) = test_peer::client(move |request| {
            let mut tasks = native_tasks.lock().unwrap();
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
                    let decision = &request["params"]["result"];
                    let outcome = if decision["kind"] == "approve-once" {
                        tasks.push(json!({"id":pending["params"]["arguments"]["shellId"],"type":"shell","status":"running","attachmentMode":"attached","executionMode":"background","command":"fixture command"}));
                        json!({"textResultForLlm":"permission outcome","resultType":"success"})
                    } else {
                        native_rejection(decision["feedback"].as_str())
                    };
                    return vec![
                        test_peer::response(request, json!({})),
                        test_peer::response(&pending, outcome),
                    ];
                }
                "session.tasks.list" => json!({"tasks":*tasks}),
                // No process has this id, so the captured command tree is empty and the fixture
                // observes only the request order.
                "session.tasks.getProgress" => {
                    json!({"progress":{"type":"shell","pid":u32::MAX - 1,"recentOutput":""}})
                }
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
            tasks,
            events,
        }
    }

    /// Read the next combined inventory the shell owner published on its binding stream.
    async fn next_inventory(&mut self) -> TerminalInventory {
        loop {
            let observation = self
                .events
                .recv()
                .await
                .unwrap()
                .expect("the binding stream stays open while its shell owner lives");
            if let forge_provider_api::ProviderEvent::Thread {
                event: forge_provider_api::ThreadEvent::TerminalsUpdated(Ok(inventory)),
                ..
            } = observation.event
            {
                return inventory;
            }
        }
    }

    /// Start a launch whose default foreground wait stays owned by the calling invocation.
    fn launch_foreground(
        &self,
    ) -> tokio::task::JoinHandle<Result<NativeToolResult, ProviderError>> {
        let shell = self.shell.clone();
        tokio::spawn(async move {
            shell
                .execute(
                    "outer-call",
                    command_tool(),
                    json!({"command":"fixture command","description":"fixture","initial_wait":30}),
                )
                .await
        })
    }

    /// Collect private history before releasing the native transport.
    async fn finish(self) -> Vec<Value> {
        self.shell.close().await.unwrap();
        self.client.stop().await.unwrap();
        self.peer.collected().await
    }
}

/// Answer a rejected shell permission as Copilot runtime 1.0.103 does.
///
/// Its recorded `tool.execution_complete` events report a rejection that carries feedback as
/// code `denied` with "The user rejected this tool call. User feedback: <feedback>", and one
/// without feedback as code `rejected` with "The user rejected this tool call.".
fn native_rejection(feedback: Option<&str>) -> Value {
    match feedback {
        Some(feedback) => {
            let text = format!("The user rejected this tool call. User feedback: {feedback}");
            json!({"textResultForLlm":text,"resultType":"denied","error":text})
        }
        None => json!({
            "textResultForLlm":"The user rejected this tool call.","resultType":"rejected",
            "error":"The user rejected this tool call."
        }),
    }
}

/// Collect a native shell even when its task row no longer carries presentation metadata.
#[tokio::test]
async fn cleanup_does_not_require_command_metadata() {
    let fixture = Fixture::new(PermissionResponse::Allow, false, false).await;
    fixture
        .shell
        .execute(
            "outer-call",
            command_tool(),
            json!({"command":"fixture command","description":"fixture","mode":"async"}),
        )
        .await
        .unwrap();
    let task_id = {
        let mut tasks = fixture.tasks.lock().unwrap();
        tasks[0].as_object_mut().unwrap().remove("command");
        tasks[0]["id"].clone()
    };
    let requests = fixture.finish().await;
    let cancellation = requests
        .iter()
        .position(|request| {
            request["method"] == "session.tasks.cancel" && request["params"]["id"] == task_id
        })
        .unwrap();
    let detach = requests
        .iter()
        .position(|request| request["method"] == "session.detach")
        .unwrap();
    assert!(cancellation < detach);
    assert!(
        requests[cancellation + 1..detach]
            .iter()
            .any(|request| request["method"] == "session.tasks.list"),
        "collection must reobserve task absence before detaching"
    );
}

/// Retain failed collection until a live task has a cancellable identity.
#[tokio::test]
async fn cleanup_reports_missing_identity_and_can_retry() {
    for (identity, expected) in [
        (None, "terminal identity is missing"),
        (Some(" \t"), "terminal identity is invalid"),
    ] {
        let fixture = Fixture::new(PermissionResponse::Allow, false, false).await;
        fixture
            .shell
            .execute(
                "outer-call",
                command_tool(),
                json!({"command":"fixture command","description":"fixture","mode":"async"}),
            )
            .await
            .unwrap();
        let task_id = fixture.tasks.lock().unwrap()[0]
            .as_object_mut()
            .unwrap()
            .remove("id")
            .unwrap();
        if let Some(identity) = identity {
            fixture.tasks.lock().unwrap()[0]["id"] = json!(identity);
        }
        let error = fixture.shell.close().await.unwrap_err();
        assert!(error.to_string().contains(expected), "{error}");
        fixture.tasks.lock().unwrap()[0]["id"] = task_id;
        fixture.finish().await;
    }
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
        assert!(
            result.result_type
                == if allowed {
                    NativeOutcome::Success
                } else {
                    NativeOutcome::Denied
                }
        );
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

/// Keep the session's shell usable after a rejection, and hand the model the denial reason.
///
/// A denied, expired, or cancelled approval returns a native `denied` or `rejected` result.
/// Each must reach the model as that kind with its text, and the next launch must reach
/// native permission again instead of failing before it.
#[tokio::test]
async fn rejected_launch_keeps_later_launches_available() {
    let reason = "Tool call rejected. Reason: Rejected by the user.";
    for (decision, kind) in [
        (
            PermissionResponse::Deny {
                reason: Some(reason.into()),
            },
            "denied",
        ),
        (PermissionResponse::Cancelled, "rejected"),
    ] {
        let fixture = Fixture::new(decision, false, true).await;
        let launch = || {
            fixture.shell.execute(
                "outer-call",
                command_tool(),
                json!({"command":"fixture command","description":"fixture","mode":"async"}),
            )
        };
        let rejected = launch().await.unwrap();
        let delivered = serde_json::to_value(rejected.into_sdk()).unwrap();
        assert_eq!(delivered["resultType"], kind);
        let text = delivered["textResultForLlm"].as_str().unwrap();
        assert!(
            text.starts_with("The user rejected this tool call."),
            "{text}"
        );
        if kind == "denied" {
            assert!(
                text.ends_with(reason),
                "the model must receive the denial reason: {text}"
            );
        }
        assert_eq!(delivered["error"], delivered["textResultForLlm"]);
        launch().await.unwrap();
        assert_eq!(
            fixture.callbacks.permissions.lock().unwrap().len(),
            2,
            "the launch after a rejection must reach native permission"
        );
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
    assert!(result.result_type == NativeOutcome::Rejected);
    let requests = fixture.finish().await;
    let decision = requests
        .iter()
        .find(|request| request["method"] == "session.permissions.handlePendingPermissionRequest")
        .unwrap();
    assert_eq!(decision["params"]["result"]["kind"], "reject");
}

/// Stop the native task of a launch whose foreground wait the interrupted turn still owns.
#[tokio::test]
async fn interrupted_foreground_launch_cancels_its_native_task() {
    // The woken launch can run before or after its invocation's callback authority is revoked.
    for authority_revoked in [true, false] {
        let mut fixture = Fixture::new(PermissionResponse::Allow, false, false).await;
        let call = fixture.launch_foreground();
        assert_eq!(fixture.next_inventory().await.terminal.len(), 1);
        fixture
            .callbacks
            .cancelled
            .store(authority_revoked, Ordering::SeqCst);
        fixture.shell.cancel_waits();
        assert!(matches!(call.await.unwrap(), Err(ProviderError::Cancelled)));
        assert!(
            fixture.tasks.lock().unwrap().is_empty(),
            "an interrupted foreground launch must not keep its native task running"
        );
        assert!(fixture.next_inventory().await.terminal.is_empty());
        fixture.shell.settle_revoked_launches().await;
        let requests = fixture.finish().await;
        assert!(
            !requests.iter().any(|request| {
                request["method"] == "session.tools.execute"
                    && request["params"]["name"] == command_tool().read_name()
            }),
            "an interrupted launch must not read output for the stopped model turn"
        );
        // The wrapper's process id is read while the wrapper runs, before cancellation stops it.
        let position = |method: &str| {
            requests
                .iter()
                .position(|request| request["method"] == method)
                .unwrap_or_else(|| panic!("the interrupted launch never sent {method}"))
        };
        assert!(position("session.tasks.getProgress") < position("session.tasks.cancel"));
    }
}

/// Keep a command running when steering only releases its foreground wait.
#[tokio::test]
async fn steering_wake_keeps_foreground_launch_running() {
    let mut fixture = Fixture::new(PermissionResponse::Allow, false, false).await;
    let call = fixture.launch_foreground();
    assert_eq!(fixture.next_inventory().await.terminal.len(), 1);
    fixture.shell.interrupt_waits();
    assert!(matches!(
        call.await.unwrap(),
        Ok(output) if output.result_type == NativeOutcome::Success
    ));
    assert_eq!(fixture.tasks.lock().unwrap().len(), 1);
    assert_eq!(fixture.shell.snapshot().await.unwrap().terminal.len(), 1);
    let requests = fixture.finish().await;
    assert!(
        !requests
            .iter()
            .any(|request| request["method"] == "session.tasks.getProgress"),
        "steering must leave the running command's processes alone"
    );
}
