use super::*;
use crate::backend::ExecutionOwner;
use tokio::io::{AsyncReadExt, AsyncWriteExt, DuplexStream};

#[derive(Clone, Copy, PartialEq)]
enum NativeUserDelivery {
    Streaming,
    HistoryOnly,
    NotRecorded,
    ExpiredHistory,
}

#[tokio::test]
async fn copilot_retry_waits_for_idle_and_keeps_the_native_session() -> Result<()> {
    let (fixture, backend, mut request) = fixture()?;
    let submission_store =
        crate::backend::submission::SubmissionStore::open(fixture.path(), backend.trace.clone())?;
    let submission = submission_store.prepare(&request, BackendKind::Copilot)?;
    request.submission = Some(submission.clone());
    let (client_read, mut server_write) = tokio::io::duplex(65536);
    let (client_write, mut server_read) = tokio::io::duplex(65536);
    *backend.client.lock().await = Some(Client::from_streams(
        client_read,
        client_write,
        PathBuf::from(&request.workspace),
    )?);
    let (sink, mut events) = crate::backend::events::channel();
    let server = async {
        let create = read_request(&mut server_read).await?;
        assert_eq!(create["method"], "session.create");
        let session_id = create["params"]["sessionId"]
            .as_str()
            .context("session identity missing")?
            .to_owned();
        respond(
            &mut server_write,
            &create,
            json!({"result":{"sessionId":session_id,"workspacePath":"."}}),
        )
        .await?;
        for attempt in 0..2 {
            let todos = read_request(&mut server_read).await?;
            assert_eq!(todos["method"], "session.plan.readSqlTodosWithDependencies");
            respond(
                &mut server_write,
                &todos,
                json!({"result":{"rows":[],"dependencies":[]}}),
            )
            .await?;
            let tail = read_request(&mut server_read).await?;
            assert_eq!(tail["method"], "session.eventLog.tail");
            respond(
                &mut server_write,
                &tail,
                json!({"result":{"cursor":"retained"}}),
            )
            .await?;
            let send = read_request(&mut server_read).await?;
            assert_eq!(send["method"], "session.send");
            assert_eq!(send["params"]["sessionId"], session_id);
            if attempt == 0 {
                assert!(matches!(
                    submission.record()?.delivery,
                    crate::backend::submission::SubmissionState::Sending { .. }
                ));
            }
            respond(
                &mut server_write,
                &send,
                json!({"result":{"messageId":format!("message-{attempt}")}}),
            )
            .await?;
            write_frame(&mut server_write, json!({"jsonrpc":"2.0","method":"session.event","params":{
                "sessionId":session_id,"event":{"id":format!("turn-{attempt}"),"type":"assistant.turn_start","timestamp":"2026-10-20T12:00:00Z","data":{"turnId":format!("turn-{attempt}")}}
            }})).await?;
            if attempt == 0 {
                write_frame(&mut server_write, json!({"jsonrpc":"2.0","method":"session.event","params":{
                    "sessionId":session_id,"event":{"id":"failure","type":"session.error","timestamp":"2026-10-20T12:00:00Z","data":{"errorType":"query","statusCode":503,"message":"service temporarily unavailable"}}
                }})).await?;
                assert!(
                    tokio::time::timeout(
                        std::time::Duration::from_millis(30),
                        read_request(&mut server_read)
                    )
                    .await
                    .is_err(),
                    "retry began before the provider became idle"
                );
            }
            write_frame(&mut server_write, json!({"jsonrpc":"2.0","method":"session.event","params":{
                "sessionId":session_id,"event":{"id":format!("idle-{attempt}"),"type":"session.idle","timestamp":"2026-10-20T12:00:00Z","data":{}}
            }})).await?;
        }
        Ok::<_, anyhow::Error>(())
    };
    tokio::time::timeout(std::time::Duration::from_secs(5), async {
        tokio::try_join!(
            server,
            crate::backend::retry::prompt(
                &backend,
                request,
                Some(sink),
                &backend.trace,
                &submission_store
            )
        )
    })
    .await??;
    assert!(matches!(
        submission.record()?.delivery,
        crate::backend::submission::SubmissionState::Accepted { .. }
    ));
    let mut retry_count = 0;
    let mut failed_turn = false;
    while let Ok(event) = events.try_recv() {
        assert_ne!(
            event.kind, "error",
            "transient error escaped as a terminal notification"
        );
        if event.kind == "provider_retry" && !event.data.is_null() {
            assert_eq!(event.data["attempt"], 1);
            retry_count += 1;
        }
        failed_turn |= matches!(
            event.turn_boundary,
            Some(super::super::TurnBoundary::Finished {
                outcome: crate::turn::TurnOutcome::Failed
            })
        );
    }
    assert_eq!(retry_count, 2);
    assert!(failed_turn);
    Ok(())
}

#[tokio::test]
async fn copilot_disables_tool_search_and_binds_session_files_on_create_and_resume() -> Result<()> {
    for resumed in [false, true] {
        let (_fixture, backend, mut request) = fixture()?;
        let external = tempfile::tempdir()?;
        let session_directory = external.path().join("custom state location/session notes");
        std::fs::create_dir_all(&session_directory)?;
        request.backend_session_id = resumed.then(|| "retained-session".into());
        let (client_read, mut server_write) = tokio::io::duplex(65536);
        let (client_write, mut server_read) = tokio::io::duplex(65536);
        *backend.client.lock().await = Some(Client::from_streams(
            client_read,
            client_write,
            PathBuf::from(&request.workspace),
        )?);
        let server = async {
            let message = read_request(&mut server_read).await?;
            assert_eq!(
                message["method"],
                if resumed {
                    "session.resume"
                } else {
                    "session.create"
                }
            );
            assert_eq!(message["params"]["toolSearch"], json!({"enabled":false}));
            assert!(
                message["params"]["tools"]
                    .as_array()
                    .context("missing control tools")?
                    .iter()
                    .any(|tool| tool["name"] == "plan_read")
            );
            let session_id = message["params"]["sessionId"]
                .as_str()
                .context("SDK omitted session identity")?;
            respond(
                &mut server_write,
                &message,
                json!({"result":{"sessionId":session_id,"workspacePath":session_directory}}),
            )
            .await?;
            if resumed {
                let reload = read_request(&mut server_read).await?;
                assert_eq!(reload["method"], "session.skills.reload");
                respond(&mut server_write, &reload, json!({"result":{}})).await?;
            }
            Ok::<_, anyhow::Error>(())
        };
        let (session, ()) = tokio::time::timeout(std::time::Duration::from_secs(5), async {
            tokio::try_join!(backend.session(&request), server)
        })
        .await??;
        let (sink, mut events) = crate::backend::events::channel();
        let context = backend.permission_context(&request.runtime_key()?).await;
        context.configure(
            request.owner()?,
            ApprovalMode::Ask,
            request.workspace.clone(),
            Some(sink),
        )?;
        let handler = CopilotPermissionHandler { context };
        let PermissionResult::Decision(expected) = PermissionResult::approve_once() else {
            unreachable!()
        };
        let expected = serde_json::to_value(expected)?;
        for kind in ["read", "write"] {
            let result = tokio::time::timeout(
                std::time::Duration::from_secs(1),
                handler.handle(
                    session.id().clone(),
                    RequestId::new(format!("session-{kind}")),
                    serde_json::from_value(json!({
                        "kind":kind, "path":session_directory.join("plan.md")
                    }))?,
                ),
            )
            .await?;
            let PermissionResult::Decision(decision) = result else {
                panic!("session file access returned no decision")
            };
            assert_eq!(serde_json::to_value(decision)?, expected);
            assert!(
                events.try_recv().is_err(),
                "session file access requested approval"
            );
        }
    }
    Ok(())
}

#[tokio::test]
async fn copilot_file_callbacks_apply_directory_scopes_without_sandbox() -> Result<()> {
    let (root, backend, mut request) = fixture()?;
    let external = tempfile::tempdir()?;
    request.access.read_access = crate::session::DirectoryScope::Workspace;
    request.approval_mode = ApprovalMode::Ask;
    assert!(!request.access.sandbox);
    let coordinator = backend.permission_coordinator.clone();
    let _directories = coordinator.directories(Some(&root.path().join("settings")), &request)?;
    let (sink, mut events) = crate::backend::events::channel();
    let context = Arc::new(CopilotPermissionContext::new(coordinator.clone()));
    context.configure(
        request.owner()?,
        ApprovalMode::Ask,
        request.workspace.clone(),
        Some(sink),
    )?;
    let handler = Arc::new(CopilotPermissionHandler { context });
    let PermissionResult::Decision(expected) = PermissionResult::approve_once() else {
        unreachable!()
    };
    let expected = serde_json::to_value(expected)?;
    for kind in ["read", "write"] {
        let data = json!({"kind":kind,"path":external.path().join("file.txt")});
        let waiting_handler = handler.clone();
        let waiting_data = data.clone();
        let waiter = tokio::spawn(async move {
            waiting_handler
                .handle(
                    serde_json::from_value(json!("native")).unwrap(),
                    serde_json::from_value(json!("file")).unwrap(),
                    serde_json::from_value(waiting_data).unwrap(),
                )
                .await
        });
        let event = tokio::time::timeout(std::time::Duration::from_secs(1), events.recv())
            .await?
            .unwrap()
            .unwrap();
        assert_eq!(event.kind, "approval_requested");
        let approval: crate::backend::approval::ApprovalRequestView =
            serde_json::from_value(event.data)?;
        coordinator
            .resolve(
                &approval.id,
                &[crate::backend::approval::ApprovalAnswer {
                    item_id: approval.item_list[0].id.clone(),
                    choice_id: "allow_global_directory".into(),
                }],
                None,
            )
            .await?;
        let PermissionResult::Decision(decision) = waiter.await? else {
            panic!("approval returned no decision")
        };
        assert_eq!(serde_json::to_value(decision)?, expected);
        assert_eq!(
            events.recv().await.unwrap().unwrap().kind,
            "directory_access_granted"
        );
        assert_eq!(
            events.recv().await.unwrap().unwrap().kind,
            "approval_resolved"
        );
        let repeat = tokio::time::timeout(
            std::time::Duration::from_secs(1),
            handler.handle(
                serde_json::from_value(json!("native"))?,
                serde_json::from_value(json!("repeat"))?,
                serde_json::from_value(data)?,
            ),
        )
        .await?;
        let PermissionResult::Decision(decision) = repeat else {
            panic!("repeat returned no decision")
        };
        assert_eq!(serde_json::to_value(decision)?, expected);
    }
    Ok(())
}

#[tokio::test]
async fn cancellation_during_copilot_startup_does_not_wait_or_send() -> Result<()> {
    let (_fixture, backend, request) = fixture()?;
    let session_id = request.runtime_key()?;
    let slot = backend.session_slot(&session_id).await;
    let startup = slot.lock().await;
    let (sink, _events) = crate::backend::events::channel();
    let mut prompt = Box::pin(backend.prompt_stream(request, sink));
    assert!(futures_util::poll!(prompt.as_mut()).is_pending());
    assert!(backend.has_active_turn(&session_id).await);
    tokio::time::timeout(
        std::time::Duration::from_millis(100),
        backend.cleanup_execution(&session_id),
    )
    .await
    .context("cancellation waited for provider startup")??;
    drop(prompt);
    drop(startup);
    assert_eq!(backend.client_start_count(), 0);
    assert!(!backend.has_active_turn(&session_id).await);
    assert!(backend.rollback_cancelled_turn_for(&session_id).await?);
    Ok(())
}

#[tokio::test]
async fn copilot_submission_fence_survives_cleanup_before_turn_activation() -> Result<()> {
    let turn = CopilotTurnEvent::default();
    let cancelled = Arc::new(AtomicBool::new(true));
    let active = turn.activate(None, Some(cancelled))?;
    let error = active
        .send("before-send".into(), async {
            panic!("a canceled startup sent a native prompt")
        })
        .await
        .unwrap_err();
    assert!(error.is::<crate::broker::TurnCancelled>());
    assert!(!turn.stop().await?);
    Ok(())
}

#[tokio::test]
async fn copilot_cleanup_waits_for_committed_send_and_blocks_late_steering() -> Result<()> {
    let turn = CopilotTurnEvent::default();
    let active = turn.activate(None, None)?;
    let (release, released) = tokio::sync::oneshot::channel();
    let mut sending = Box::pin(active.send("before-send".into(), async {
        released.await?;
        Ok("send-response-id".into())
    }));
    assert!(futures_util::poll!(sending.as_mut()).is_pending());
    let request = turn.request_stop()?;
    assert!(
        request.submitted && request.sending,
        "native abort must be available before send collection"
    );
    let mut stopping = Box::pin(turn.stop());
    assert!(futures_util::poll!(stopping.as_mut()).is_pending());
    release.send(()).unwrap();
    sending.await?;
    assert!(stopping.await?);
    assert!(
        active
            .send("before-send".into(), async {
                panic!("steering escaped a stop fence")
            })
            .await
            .is_err()
    );
    drop(active);
    tokio::time::timeout(std::time::Duration::from_millis(100), turn.wait_finished()).await?;
    assert!(
        matches!(turn.retraction_target()?, CopilotRetraction::History(cursor) if cursor == "before-send"),
        "a sent request must retain its required history boundary"
    );
    Ok(())
}

#[tokio::test]
async fn copilot_cleanup_sends_abort_before_a_pending_send_response() -> Result<()> {
    let (_fixture, backend, request) = fixture()?;
    let session_id = request.runtime_key()?;
    let (client_read, mut server_write) = tokio::io::duplex(65536);
    let (client_write, mut server_read) = tokio::io::duplex(65536);
    *backend.client.lock().await = Some(Client::from_streams(
        client_read,
        client_write,
        PathBuf::from(&request.workspace),
    )?);
    let create = async {
        let message = read_request(&mut server_read).await?;
        assert_eq!(message["method"], "session.create");
        respond(
            &mut server_write,
            &message,
            json!({"result":{
                "sessionId":message["params"]["sessionId"],"workspacePath":"."
            }}),
        )
        .await
    };
    let (session, ()) = tokio::try_join!(backend.session(&request), create)?;
    let turn = backend.turn_event(&session_id).await;
    let active = turn.activate(None, None)?;
    let (submitted, submission) = tokio::sync::oneshot::channel();
    let sending = async {
        let result = active
            .send("before-send".into(), async {
                session
                    .send(github_copilot_sdk::types::MessageOptions::new(
                        "pending send",
                    ))
                    .await
                    .context("fixture send failed")
            })
            .await;
        drop(active);
        result
    };
    let stopping = async {
        submission.await?;
        backend.cleanup_execution(&session_id).await
    };
    let server = async {
        let send = read_request(&mut server_read).await?;
        assert_eq!(send["method"], "session.send");
        submitted.send(()).unwrap();
        let abort = read_request(&mut server_read).await?;
        assert_eq!(
            abort["method"], "session.abort",
            "native interrupt waited for send completion"
        );
        respond(&mut server_write, &abort, json!({"result":{}})).await?;
        respond(
            &mut server_write,
            &send,
            json!({"result":{"messageId":"pending-message"}}),
        )
        .await?;
        let collected_abort = read_request(&mut server_read).await?;
        assert_eq!(collected_abort["method"], "session.abort");
        respond(&mut server_write, &collected_abort, json!({"result":{}})).await?;
        Ok::<_, anyhow::Error>(())
    };
    tokio::time::timeout(std::time::Duration::from_secs(2), async {
        tokio::try_join!(sending, stopping, server)
    })
    .await??;
    Ok(())
}

#[tokio::test]
async fn copilot_stop_retains_native_submission_after_consumer_drop() -> Result<()> {
    let turn = CopilotTurnEvent::default();
    let active = turn.activate(None, None)?;
    active
        .send("before-send".into(), async { Ok("transport-id".into()) })
        .await?;
    drop(active);
    assert!(!turn.is_active());
    assert!(
        turn.stop().await?,
        "dropping a consumer must not suppress native abort"
    );
    Ok(())
}

#[tokio::test]
async fn copilot_native_rewind_targets_the_first_root_user_event_and_retries_failures() -> Result<()>
{
    for first_failure in [
        None,
        Some(json!({"error":{"code":-32601,"message":"history truncation unavailable"}})),
        Some(json!({"result":{"eventsRemoved":0}})),
    ] {
        native_rewind(first_failure, NativeUserDelivery::Streaming).await?;
    }
    Ok(())
}

#[tokio::test]
async fn copilot_new_admission_cannot_rewind_an_earlier_cancelled_turn() -> Result<()> {
    let (_fixture, backend, request) = fixture()?;
    let session_id = request.runtime_key()?;
    let turn = backend.turn_event(&session_id).await;
    let active = turn.activate(None, None)?;
    active
        .send("before-send".into(), async { Ok("transport-id".into()) })
        .await?;
    turn.observe(&serde_json::from_value(json!({
        "id":"previous-user", "type":"user.message", "timestamp":"2026-10-15T17:00:00Z", "data":{}
    }))?)?;
    assert!(turn.stop().await?);
    drop(active);
    assert!(
        matches!(turn.retraction_target()?, CopilotRetraction::Event(id) if id == "previous-user")
    );
    backend.prepare_turn(&session_id).await?;
    assert!(backend.rollback_cancelled_turn_for(&session_id).await?);
    Ok(())
}

#[tokio::test]
async fn copilot_rewind_verifies_history_when_abort_precedes_the_user_event() -> Result<()> {
    native_rewind(None, NativeUserDelivery::HistoryOnly).await?;
    native_rewind(None, NativeUserDelivery::NotRecorded).await?;
    native_rewind(None, NativeUserDelivery::ExpiredHistory).await
}

async fn native_rewind(first_failure: Option<Value>, delivery: NativeUserDelivery) -> Result<()> {
    let (_fixture, backend, request) = fixture()?;
    let session_id = request.runtime_key()?;
    let (client_read, mut server_write) = tokio::io::duplex(65536);
    let (client_write, mut server_read) = tokio::io::duplex(65536);
    let client =
        Client::from_streams(client_read, client_write, PathBuf::from(&request.workspace))?;
    *backend.client.lock().await = Some(client);
    let (sent, sent_ready) = tokio::sync::oneshot::channel();
    let retry = first_failure.is_some();
    let server = async move {
        let create = read_request(&mut server_read).await?;
        assert_eq!(create["method"], "session.create");
        let native_session_id = create["params"]["sessionId"]
            .as_str()
            .context("SDK omitted session identity")?;
        respond(
            &mut server_write,
            &create,
            json!({"result":{"sessionId":native_session_id,"workspacePath":"."}}),
        )
        .await?;
        let todos = read_request(&mut server_read).await?;
        assert_eq!(todos["method"], "session.plan.readSqlTodosWithDependencies");
        respond(
            &mut server_write,
            &todos,
            json!({"result":{"rows":[],"dependencies":[]}}),
        )
        .await?;
        let tail = read_request(&mut server_read).await?;
        assert_eq!(tail["method"], "session.eventLog.tail");
        respond(
            &mut server_write,
            &tail,
            json!({"result":{"cursor":"pre-submit"}}),
        )
        .await?;
        let send = read_request(&mut server_read).await?;
        assert_eq!(send["method"], "session.send");
        assert_ne!(send["params"]["prompt"], "/rewind");
        respond(
            &mut server_write,
            &send,
            json!({"result":{"messageId":"transport-response-id"}}),
        )
        .await?;
        if delivery == NativeUserDelivery::Streaming {
            for (id, agent) in [
                ("child-user", Some("child")),
                ("root-user", None),
                ("reminder", None),
            ] {
                write_frame(
                    &mut server_write,
                    json!({
                        "jsonrpc":"2.0", "method":"session.event", "params":{
                            "sessionId":native_session_id, "event":{
                                "id":id, "type":"user.message", "timestamp":"2026-10-15T17:00:00Z",
                                "agentId":agent, "data":{"content":"fixture"}
                            }
                        }
                    }),
                )
                .await?;
            }
        }
        sent.send(()).unwrap();
        let abort = read_request(&mut server_read).await?;
        assert_eq!(abort["method"], "session.abort");
        respond(&mut server_write, &abort, json!({"result":{}})).await?;
        write_frame(&mut server_write, json!({
            "jsonrpc":"2.0", "method":"session.event", "params":{
                "sessionId":native_session_id, "event":{
                    "id":"idle", "type":"assistant.idle", "timestamp":"2026-10-15T17:00:01Z", "data":{}
                }
            }
        })).await?;
        if delivery != NativeUserDelivery::Streaming {
            let history = read_request(&mut server_read).await?;
            assert_eq!(history["method"], "session.eventLog.read");
            assert_eq!(history["params"]["cursor"], "pre-submit");
            assert_eq!(history["params"]["agentScope"], "primary");
            assert_eq!(history["params"]["types"], json!(["user.message"]));
            let events = if delivery == NativeUserDelivery::HistoryOnly {
                json!([{"id":"root-user", "type":"user.message", "timestamp":"2026-10-15T17:00:00Z", "data":{}}])
            } else {
                json!([])
            };
            respond(
                &mut server_write,
                &history,
                json!({"result":{
                    "cursor":"after-submit", "cursorStatus":if delivery == NativeUserDelivery::ExpiredHistory { "expired" } else { "ok" },
                    "hasMore":false, "events":events,
                }}),
            )
            .await?;
            if matches!(
                delivery,
                NativeUserDelivery::NotRecorded | NativeUserDelivery::ExpiredHistory
            ) {
                return Ok(());
            }
        }
        for attempt in 0..if retry { 2 } else { 1 } {
            let rewind = read_request(&mut server_read).await?;
            assert_eq!(rewind["method"], "session.history.truncate");
            assert_eq!(rewind["params"]["sessionId"], native_session_id);
            assert_eq!(rewind["params"]["eventId"], "root-user");
            let response = if attempt == 0 {
                first_failure
                    .clone()
                    .unwrap_or_else(|| json!({"result":{"eventsRemoved":3}}))
            } else {
                json!({"result":{"eventsRemoved":3}})
            };
            respond(&mut server_write, &rewind, response).await?;
        }
        Ok::<_, anyhow::Error>(())
    };
    let execution = async {
        let (sink, _events) = crate::backend::events::channel();
        let prompt = backend.prompt_stream(request, sink);
        let cleanup = async {
            sent_ready.await?;
            backend.cleanup_execution(&session_id).await
        };
        tokio::try_join!(prompt, cleanup)?;
        if delivery == NativeUserDelivery::ExpiredHistory {
            let error = backend
                .rollback_cancelled_turn_for(&session_id)
                .await
                .unwrap_err();
            assert!(
                error.to_string().contains("history boundary expired"),
                "{error:#}"
            );
            return Ok(());
        }
        if retry {
            let error = backend
                .rollback_cancelled_turn_for(&session_id)
                .await
                .unwrap_err();
            assert!(
                format!("{error:#}").contains("history truncation unavailable")
                    || format!("{error:#}").contains("removed no history events"),
                "{error:#}"
            );
        }
        assert!(backend.rollback_cancelled_turn_for(&session_id).await?);
        assert!(backend.rollback_cancelled_turn_for(&session_id).await?);
        Ok::<_, anyhow::Error>(())
    };
    tokio::time::timeout(std::time::Duration::from_secs(2), async {
        tokio::try_join!(server, execution)
    })
    .await
    .context("native Copilot cancellation/rewind did not settle")??;
    Ok(())
}

fn fixture() -> Result<(tempfile::TempDir, CopilotBackend, BackendRequest)> {
    let fixture = tempfile::tempdir()?;
    let backend = CopilotBackend::new_with_permission_coordinator(
        Vec::new(),
        PermissionCoordinator::transient(fixture.path())?,
        Arc::new(crate::trace::TraceStore::open(fixture.path())?),
        None,
    )?;
    let request = CopilotBackend::catalog_backend_request(&BackendCatalogRequest {
        execution_owner: Some(ExecutionOwner::root("copilot-cancel")),
        harness_session_id: "copilot-cancel".into(),
        workspace: fixture.path().to_string_lossy().into_owned(),
        access: BackendKind::Copilot.resolve_access(crate::session::AccessPolicy::default()),
        approval_mode: ApprovalMode::Ask,
        backend_session_id: None,
    });
    Ok((fixture, backend, request))
}

#[tokio::test]
async fn copilot_rejection_feedback_preserves_denial_origin_and_cancellation() -> Result<()> {
    use crate::backend::approval::{ApprovalAnswer, ApprovalRequestView};
    use crate::permissions::document::{CATEGORY_TOOL, PermissionDecision};

    for scenario in [
        "policy",
        "policy-autonomous",
        "mode",
        "user",
        "timeout",
        "cancel",
        "allow",
        "invalid",
    ] {
        let (_root, backend, request) = fixture()?;
        let coordinator = backend.permission_coordinator.clone();
        let mode = match scenario {
            "policy" | "allow" => ApprovalMode::Allow,
            "mode" | "policy-autonomous" => ApprovalMode::Deny,
            _ => ApprovalMode::Ask,
        };
        coordinator.store().write().unwrap().set_rule_list(&[(
            CATEGORY_TOOL.into(),
            "sample".into(),
            if scenario.starts_with("policy") {
                PermissionDecision::Deny
            } else {
                PermissionDecision::Ask
            },
        )])?;
        coordinator.configure_timeout(
            &request.harness_session_id,
            crate::session::InputTimeout {
                amount: if scenario == "timeout" { 1 } else { 0 },
                unit: crate::session::TimeoutUnit::S,
            },
        )?;
        let before = coordinator.store().read().unwrap().open().1;
        let (sink, mut events) = crate::backend::events::channel();
        let context = backend.permission_context(&request.runtime_key()?).await;
        context.configure(
            request.owner()?,
            mode,
            request.workspace.clone(),
            Some(sink),
        )?;
        let handler = CopilotPermissionHandler {
            context: context.clone(),
        };
        let task = tokio::spawn(async move {
            handler
                .handle(
                    SessionId::new("native"),
                    RequestId::new("request"),
                    serde_json::from_value(if scenario == "invalid" {
                        json!({"kind":"shell","fullCommandText":"echo '","shell":"bash"})
                    } else {
                        json!({"kind":"custom-tool","toolName":"sample"})
                    })
                    .unwrap(),
                )
                .await
        });
        if matches!(scenario, "user" | "cancel" | "timeout") {
            let event = tokio::time::timeout(std::time::Duration::from_secs(2), events.recv())
                .await??
                .context("approval event missing")?;
            let view: ApprovalRequestView = serde_json::from_value(event.data)?;
            if scenario != "timeout" {
                coordinator
                    .resolve(
                        &view.id,
                        &[ApprovalAnswer {
                            item_id: view.item_list[0].id.clone(),
                            choice_id: if scenario == "cancel" {
                                "cancel"
                            } else {
                                "deny_once"
                            }
                            .into(),
                        }],
                        None,
                    )
                    .await?;
            }
        }
        let PermissionResult::Decision(decision) =
            tokio::time::timeout(std::time::Duration::from_secs(3), task).await??
        else {
            panic!("permission returned no decision")
        };
        let decision = serde_json::to_value(decision)?;
        if scenario == "allow" {
            assert_eq!(decision["kind"], "approve-once");
        } else {
            assert_eq!(decision["kind"], "reject");
            if scenario == "cancel" {
                assert!(decision.get("feedback").is_none());
            } else {
                let feedback = decision["feedback"]
                    .as_str()
                    .context("missing rejection feedback")?;
                assert!(
                    feedback.starts_with(
                        "Tool call rejected. The requested operation was not executed."
                    )
                );
                assert!(feedback.contains("Do not retry or bypass this denial."));
                assert!(feedback.contains("explain the blocker to the user"));
                let reason = match scenario {
                    "policy" | "policy-autonomous" => {
                        "Harness permission or filesystem access policy"
                    }
                    "mode" => "Reject-all approval mode is active",
                    "user" => "Rejected by the user",
                    "timeout" => "timeout expired",
                    "invalid" => "permission evaluation failed",
                    _ => unreachable!(),
                };
                assert!(feedback.contains(reason), "{scenario}: {feedback}");
                assert_eq!(
                    feedback.contains("Rejected by the user"),
                    scenario == "user"
                );
                assert_eq!(
                    feedback.contains("Automatically rejected"),
                    scenario != "user"
                );
                if mode == ApprovalMode::Deny {
                    assert!(feedback.contains("operating autonomously"));
                    assert!(feedback.contains("whitelisted commands"));
                    assert!(feedback.contains("Do not wait for user approval"));
                }
            }
        }
        assert_eq!(
            context.state.lock().unwrap().cancelled,
            scenario == "cancel"
        );
        assert_eq!(coordinator.store().read().unwrap().open().1, before);
        context.configure(request.owner()?, mode, request.workspace.clone(), None)?;
        assert!(!context.state.lock().unwrap().cancelled);
    }
    Ok(())
}

#[tokio::test]
async fn copilot_rejection_without_answer_fails_instead_of_completing() -> Result<()> {
    for code in ["rejected", "denied"] {
        for response in ["missing", "commentary", "final", "child", "cancel"] {
            let (_root, backend, request) = fixture()?;
            let expires = code == "denied" && matches!(response, "missing" | "final");
            let owner = request.owner()?;
            backend.permission_coordinator.configure_timeout(
                &request.harness_session_id,
                crate::session::InputTimeout {
                    amount: 1,
                    unit: crate::session::TimeoutUnit::S,
                },
            )?;
            backend
                .permission_coordinator
                .store()
                .write()
                .unwrap()
                .set_rule_list(&[(
                    crate::permissions::document::CATEGORY_BASH.into(),
                    "mock-probe".into(),
                    crate::permissions::document::PermissionDecision::Ask,
                )])?;
            let permission_before = backend
                .permission_coordinator
                .store()
                .read()
                .unwrap()
                .open()
                .1;
            let context = backend.permission_context(&request.runtime_key()?).await;
            let (client_read, mut server_write) = tokio::io::duplex(65536);
            let (client_write, mut server_read) = tokio::io::duplex(65536);
            *backend.client.lock().await = Some(Client::from_streams(
                client_read,
                client_write,
                PathBuf::from(&request.workspace),
            )?);
            let (sink, mut events) = crate::backend::events::channel();
            let server = async {
                let create = read_request(&mut server_read).await?;
                assert_eq!(create["method"], "session.create");
                let session_id = create["params"]["sessionId"]
                    .as_str()
                    .context("missing session")?
                    .to_owned();
                respond(
                    &mut server_write,
                    &create,
                    json!({"result":{"sessionId":session_id,"workspacePath":"."}}),
                )
                .await?;
                for (method, result) in [
                    (
                        "session.plan.readSqlTodosWithDependencies",
                        json!({"rows":[],"dependencies":[]}),
                    ),
                    ("session.eventLog.tail", json!({"cursor":"before"})),
                    ("session.send", json!({"messageId":"prompt"})),
                ] {
                    let message = read_request(&mut server_read).await?;
                    assert_eq!(message["method"], method);
                    respond(&mut server_write, &message, json!({"result":result})).await?;
                }
                let mut notification = vec![
                    ("assistant.turn_start", json!({"turnId":"turn"})),
                    (
                        "tool.execution_start",
                        json!({"toolCallId":"denied","toolName":"bash","arguments":{"command":"echo sample"}}),
                    ),
                    (
                        "tool.execution_complete",
                        json!({"toolCallId":"denied","success":false,"error":{"code":code,"message":"Tool call rejected. Denied by policy."}}),
                    ),
                    (
                        "tool.execution_progress",
                        json!({"toolCallId":"denied","progressMessage":"late progress"}),
                    ),
                ];
                if response != "missing" && response != "cancel" {
                    notification.push((
                    "assistant.message",
                    json!({
                        "messageId":"answer","content":"Cannot run that command.",
                        "phase":if response == "commentary" { "commentary" } else { "final_answer" }
                    }),
                ));
                }
                notification.push(("session.idle", json!({})));
                if response == "cancel" {
                    context.state.lock().unwrap().cancelled = true;
                }
                for (index, (kind, mut data)) in notification.into_iter().enumerate() {
                    if expires && kind == "tool.execution_complete" {
                        write_frame(
                            &mut server_write,
                            json!({"jsonrpc":"2.0","method":"session.event",
                            "params":{"sessionId":session_id,"event":{
                                "id":"permission", "type":"permission.requested",
                                "timestamp":"2026-10-27T12:00:00Z",
                                "data":{"requestId":"permission", "toolCallId":"denied",
                                    "kind":"shell", "fullCommandText":"mock-probe"}
                            }}}),
                        )
                        .await?;
                        let decision = read_request(&mut server_read).await?;
                        assert_eq!(
                            decision["method"],
                            "session.permissions.handlePendingPermissionRequest"
                        );
                        assert_eq!(decision["params"]["requestId"], "permission");
                        assert_eq!(decision["params"]["result"]["kind"], "reject");
                        let feedback = decision["params"]["result"]["feedback"]
                            .as_str()
                            .context("missing native timeout feedback")?;
                        assert!(feedback.contains("Automatically rejected"));
                        assert!(feedback.contains("timeout expired"));
                        assert!(feedback.contains("not executed"));
                        assert!(!feedback.contains("Rejected by the user"));
                        assert!(
                            backend
                                .permission_coordinator
                                .has_timeout_feedback(&owner)?
                        );
                        data["error"]["message"] = json!(feedback);
                        respond(&mut server_write, &decision, json!({"result":{}})).await?;
                    }
                    let mut event = json!({"id":format!("event-{index}"),"type":kind,
                    "timestamp":"2026-10-27T12:00:00Z","data":data});
                    if response == "child" && kind == "assistant.message" {
                        event["agentId"] = json!("child");
                    }
                    write_frame(
                        &mut server_write,
                        json!({"jsonrpc":"2.0","method":"session.event",
                    "params":{"sessionId":session_id,"event":event}}),
                    )
                    .await?;
                }
                Ok::<_, anyhow::Error>(())
            };
            let (server, result) = tokio::time::timeout(std::time::Duration::from_secs(5), async {
                tokio::join!(server, backend.prompt_stream(request.clone(), sink))
            })
            .await?;
            server?;
            match response {
                "final" => {
                    result?;
                }
                "cancel" => assert!(result.unwrap_err().is::<crate::broker::TurnCancelled>()),
                _ => assert!(
                    result
                        .unwrap_err()
                        .to_string()
                        .contains("stopped after a tool rejection without a final response")
                ),
            }
            let mut latest = None;
            while let Ok(event) = events.try_recv() {
                if let Some(activity) = event.activity {
                    latest = Some(activity);
                }
            }
            assert_eq!(
                latest.context("tool status missing")?.status.as_deref(),
                Some("denied")
            );
            assert_eq!(
                backend
                    .permission_coordinator
                    .has_timeout_feedback(&owner)?,
                expires && response != "final"
            );
            assert_eq!(
                backend
                    .permission_coordinator
                    .store()
                    .read()
                    .unwrap()
                    .open()
                    .1,
                permission_before
            );
        }
    }
    Ok(())
}

#[tokio::test]
async fn copilot_submission_reconciliation_excludes_known_messages_and_ambiguous_history()
-> Result<()> {
    for ambiguous in [false, true] {
        let (fixture, backend, request) = fixture()?;
        let journal = crate::backend::submission::SubmissionStore::open(
            fixture.path(),
            backend.trace.clone(),
        )?;
        let submission = journal.prepare(&request, BackendKind::Copilot)?;
        submission.sending(crate::backend::submission::SubmissionTarget::Copilot {
            session_id: "retained-native".into(),
            cursor: "before-send".into(),
            prompt: "same text".into(),
        })?;
        let (client_read, mut server_write) = tokio::io::duplex(65536);
        let (client_write, mut server_read) = tokio::io::duplex(65536);
        *backend.client.lock().await = Some(Client::from_streams(
            client_read,
            client_write,
            PathBuf::from(&request.workspace),
        )?);
        let server = async {
            let resume = read_request(&mut server_read).await?;
            assert_eq!(resume["method"], "session.resume");
            respond(
                &mut server_write,
                &resume,
                json!({"result":{"sessionId":"retained-native","workspacePath":"."}}),
            )
            .await?;
            let reload = read_request(&mut server_read).await?;
            assert_eq!(reload["method"], "session.skills.reload");
            respond(&mut server_write, &reload, json!({"result":{}})).await?;
            for page in 0..2 {
                let read = read_request(&mut server_read).await?;
                assert_eq!(read["method"], "session.eventLog.read");
                assert_eq!(
                    read["params"]["cursor"],
                    if page == 0 {
                        "before-send"
                    } else {
                        "next-page"
                    }
                );
                let message = |id: &str| json!({"id":id,"type":"user.message","timestamp":"2026-10-20T12:00:00Z","data":{"content":"same text","messageId":id}});
                let events = if page == 0 {
                    vec![message("already-acknowledged")]
                } else if ambiguous {
                    vec![message("candidate-one"), message("candidate-two")]
                } else {
                    vec![message("candidate-one")]
                };
                respond(&mut server_write, &read, json!({"result":{"cursor":"next-page","cursorStatus":"ok","hasMore":page == 0,"events":events}})).await?;
            }
            Ok::<_, anyhow::Error>(())
        };
        let known = std::collections::HashSet::from(["already-acknowledged".to_owned()]);
        let record = submission.record()?;
        let (_, receipt) = tokio::time::timeout(std::time::Duration::from_secs(5), async {
            tokio::try_join!(server, backend.reconcile_submission(&record, &known))
        })
        .await??;
        assert_eq!(
            receipt.as_deref(),
            if ambiguous {
                None
            } else {
                Some("candidate-one")
            }
        );
    }
    Ok(())
}

async fn read_request(reader: &mut DuplexStream) -> Result<Value> {
    let mut header = Vec::new();
    while !header.ends_with(b"\r\n\r\n") {
        header.push(reader.read_u8().await?);
    }
    let length = std::str::from_utf8(&header)?
        .trim()
        .strip_prefix("Content-Length: ")
        .context("invalid SDK frame header")?
        .parse::<usize>()?;
    let mut body = vec![0; length];
    reader.read_exact(&mut body).await?;
    Ok(serde_json::from_slice(&body)?)
}

async fn respond(writer: &mut DuplexStream, request: &Value, mut response: Value) -> Result<()> {
    response["jsonrpc"] = json!("2.0");
    response["id"] = request["id"].clone();
    write_frame(writer, response).await
}

async fn write_frame(writer: &mut DuplexStream, value: Value) -> Result<()> {
    let body = serde_json::to_vec(&value)?;
    writer
        .write_all(format!("Content-Length: {}\r\n\r\n", body.len()).as_bytes())
        .await?;
    writer.write_all(&body).await?;
    writer.flush().await?;
    Ok(())
}
