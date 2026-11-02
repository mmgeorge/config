use super::*;
use crate::provider::test_support::ProviderTestExt as _;
use crate::provider::{
    ApprovalUpdate, ExecutionOwner, InvocationScope, ToolOutcome, ToolUpdate, TurnUpdate,
};
use std::sync::atomic::AtomicBool;
use tokio::io::{AsyncReadExt, AsyncWriteExt, DuplexStream};

/// Observe the denial before exercising cancellation of the response that follows it.
struct DeniedToolObserved(Arc<tokio::sync::Notify>);

impl forge_provider_api::events::EventObserver for DeniedToolObserved {
    fn additional_events(
        &self,
        event: &ProviderEvent,
    ) -> std::result::Result<Vec<ProviderEvent>, forge_provider_api::events::EventDeliveryError>
    {
        if matches!(
            event,
            ProviderEvent::Tool {
                update: ToolUpdate::Finished {
                    outcome: ToolOutcome::Denied { .. }
                },
                ..
            }
        ) {
            self.0.notify_one();
        }
        Ok(Vec::new())
    }
}

/// Preserve the repair caution in the native prompt without emitting a UI diagnostic.
#[tokio::test]
async fn copilot_repaired_resume_cautions_native_prompt_without_diagnostic() -> Result<()> {
    let (_fixture, provider, mut request) = fixture()?;
    request.provider_session_id = Some("repaired-native".to_owned().try_into().unwrap());
    request.input = crate::provider::ProviderInput::Text {
        text: "Read the workspace before repeating any action.".into(),
        attachments: Vec::new(),
    };
    let original_input = request.input.clone();
    let (client_read, mut server_write) = tokio::io::duplex(65536);
    let (client_write, mut server_read) = tokio::io::duplex(65536);
    provider
        .runtime
        .attach_streams(client_read, client_write, PathBuf::from(&request.workspace))
        .await?;
    let (sink, mut events) = event_channel(&request)?;
    let server = async {
        let resume = read_request(&mut server_read, &mut server_write).await?;
        assert_eq!(resume["method"], "session.resume");
        respond(
            &mut server_write,
            &resume,
            json!({"result":{"sessionId":"repaired-native","workspacePath":"."}}),
        )
        .await?;
        let reload = read_request(&mut server_read, &mut server_write).await?;
        assert_eq!(reload["method"], "session.skills.reload");
        respond(&mut server_write, &reload, json!({"result":{}})).await?;
        let history = read_request(&mut server_read, &mut server_write).await?;
        assert_eq!(history["method"], "session.eventLog.read");
        respond(&mut server_write, &history, json!({"result":{"cursor":"unsafe","cursorStatus":"ok","hasMore":false,"events":[{"id":"unsafe-assistant","type":"assistant.message","timestamp":"2026-10-30T00:00:00Z","data":{"messageId":"request","content":"","toolRequests":[{"name":"shell","toolCallId":"orphan"}]}}]}})).await?;
        let truncate = read_request(&mut server_read, &mut server_write).await?;
        assert_eq!(truncate["method"], "session.history.truncate");
        assert_eq!(truncate["params"]["eventId"], "unsafe-assistant");
        respond(
            &mut server_write,
            &truncate,
            json!({"result":{"eventsRemoved":1}}),
        )
        .await?;
        let verify = read_request(&mut server_read, &mut server_write).await?;
        assert_eq!(verify["method"], "session.eventLog.read");
        respond(
            &mut server_write,
            &verify,
            json!({"result":{"cursor":"repaired","cursorStatus":"ok","hasMore":false,"events":[]}}),
        )
        .await?;
        let tasks = read_request(&mut server_read, &mut server_write).await?;
        assert_eq!(tasks["method"], "session.plan.readSqlTodosWithDependencies");
        respond(
            &mut server_write,
            &tasks,
            json!({"result":{"rows":[],"dependencies":[]}}),
        )
        .await?;
        let tail = read_request(&mut server_read, &mut server_write).await?;
        assert_eq!(tail["method"], "session.eventLog.tail");
        respond(
            &mut server_write,
            &tail,
            json!({"result":{"cursor":"before-send"}}),
        )
        .await?;
        let send = read_request(&mut server_read, &mut server_write).await?;
        assert_eq!(send["method"], "session.send");
        assert_eq!(send["params"]["sessionId"], "repaired-native");
        let prompt = send["params"]["prompt"]
            .as_str()
            .context("native prompt missing")?;
        assert!(prompt.contains("Recovery notice: an interrupted native tool-call suffix was removed from provider history. Its actions may already have completed or partially changed the workspace. Inspect the workspace before repeating writes; do not replay the interrupted request automatically."));
        assert!(prompt.ends_with("Read the workspace before repeating any action."));
        respond(
            &mut server_write,
            &send,
            json!({"result":{"messageId":"accepted-user"}}),
        )
        .await?;
        for (index, (kind, data)) in [
            ("user.message", json!({"messageId":"accepted-user","turnId":"recovered-turn","content":"Read the workspace before repeating any action."})),
            ("assistant.turn_start", json!({"turnId":"recovered-turn"})),
            (
                "assistant.message",
                json!({"messageId":"answer","phase":"final_answer","content":"Recovered safely"}),
            ),
            ("assistant.turn_end", json!({"turnId":"recovered-turn"})),
            ("session.idle", json!({})),
        ].into_iter().enumerate() {
            let mut event = json!({"id":format!("recovered-event-{index}"),"type":kind,"timestamp":"2026-10-30T00:00:00Z","data":data});
            if index > 0 { event["parentId"] = json!(format!("recovered-event-{}", index - 1)); }
            write_frame(&mut server_write, json!({"jsonrpc":"2.0","method":"session.event","params":{"sessionId":"repaired-native","event":event}})).await?;
        }
        Ok::<_, anyhow::Error>(())
    };
    let (completion, ()) = tokio::time::timeout(std::time::Duration::from_secs(5), async {
        tokio::try_join!(
            async {
                Ok::<_, anyhow::Error>(
                    provider
                        .session_prompt_and_wait(request.clone(), sink)
                        .await?,
                )
            },
            server
        )
    })
    .await??;
    assert_eq!(
        completion
            .provider_session_id
            .as_ref()
            .map(|id| id.as_str()),
        Some("repaired-native")
    );
    assert_eq!(request.input, original_input);
    let mut finished = false;
    while let Some(event) = events.try_next()? {
        assert!(
            !matches!(event, ProviderEvent::Diagnostic { .. }),
            "successful recovery emitted a UI diagnostic: {event:?}"
        );
        if let ProviderEvent::Turn {
            update: TurnUpdate::Finished { outcome, .. },
            ..
        } = event
        {
            assert_eq!(outcome, crate::turn::TurnOutcome::Completed);
            finished = true;
        }
    }
    assert!(finished, "native completion was not published");
    Ok(())
}

/// Fresh native resume validates every page and refuses unverifiable repairs.
#[tokio::test]
async fn copilot_resume_repairs_only_verified_incomplete_history() -> Result<()> {
    for scenario in [
        "valid",
        "repair",
        "expired",
        "stalled",
        "truncate-failed",
        "still-incomplete",
        "prefix-lost",
    ] {
        let (_fixture, provider, mut request) = fixture()?;
        request.provider_session_id = Some("resume-history".to_owned().try_into().unwrap());
        let (client_read, mut server_write) = tokio::io::duplex(65536);
        let (client_write, mut server_read) = tokio::io::duplex(65536);
        provider
            .runtime
            .attach_streams(client_read, client_write, PathBuf::from(&request.workspace))
            .await?;
        let server = async {
            let resume = read_request(&mut server_read, &mut server_write).await?;
            assert_eq!(resume["method"], "session.resume");
            respond(
                &mut server_write,
                &resume,
                json!({"result":{"sessionId":"resume-history","workspacePath":"."}}),
            )
            .await?;
            let reload = read_request(&mut server_read, &mut server_write).await?;
            assert_eq!(reload["method"], "session.skills.reload");
            respond(&mut server_write, &reload, json!({"result":{}})).await?;
            let read = read_request(&mut server_read, &mut server_write).await?;
            assert_eq!(read["method"], "session.eventLog.read");
            assert!(read["params"].get("cursor").is_none_or(Value::is_null));
            let dangling = json!({"id":"unsafe-assistant","type":"assistant.message","timestamp":"2026-10-30T00:00:00Z","data":{"messageId":"request","content":"","toolRequests":[{"name":"shell","toolCallId":"orphan"}]}});
            let prefix = json!({"id":"completed-prefix","type":"assistant.message","timestamp":"2026-10-30T00:00:00Z","data":{"messageId":"completed","content":"Earlier completed work"}});
            respond(&mut server_write, &read, json!({"result":{"cursor":"page-one","cursorStatus":if scenario == "expired" {"expired"} else {"ok"},"hasMore":scenario != "valid" && scenario != "expired","events":[prefix.clone()]}})).await?;
            if scenario != "valid" && scenario != "expired" {
                let page = read_request(&mut server_read, &mut server_write).await?;
                assert_eq!(page["params"]["cursor"], "page-one");
                respond(&mut server_write, &page, json!({"result":{"cursor":if scenario == "stalled" {"page-one"} else {"page-two"},"cursorStatus":"ok","hasMore":scenario == "stalled","events":[dangling.clone()]}})).await?;
                if scenario != "stalled" {
                    let truncate = read_request(&mut server_read, &mut server_write).await?;
                    assert_eq!(truncate["method"], "session.history.truncate");
                    assert_eq!(truncate["params"]["eventId"], "unsafe-assistant");
                    respond(&mut server_write, &truncate, json!({"result":{"eventsRemoved":if scenario == "truncate-failed" {0} else {1}}})).await?;
                    if scenario != "truncate-failed" {
                        let verify = read_request(&mut server_read, &mut server_write).await?;
                        assert_eq!(verify["method"], "session.eventLog.read");
                        let rewind = json!({"id":"rewind","type":"session.snapshot_rewind","timestamp":"2026-10-30T00:00:00Z","ephemeral":true,"data":{"upToEventId":"unsafe-assistant","eventsRemoved":1}});
                        let retained = match scenario {
                            "still-incomplete" => vec![prefix, dangling],
                            "prefix-lost" => vec![],
                            _ => vec![prefix, rewind],
                        };
                        respond(&mut server_write, &verify, json!({"result":{"cursor":"repaired","cursorStatus":"ok","hasMore":false,"events":retained}})).await?;
                    }
                }
            }
            if scenario != "valid" && scenario != "repair" {
                let disconnect = read_request(&mut server_read, &mut server_write).await?;
                assert_eq!(disconnect["method"], "session.detach");
                respond(&mut server_write, &disconnect, json!({"result":{}})).await?;
            }
            Ok::<_, anyhow::Error>(())
        };
        let (result, served) = tokio::time::timeout(std::time::Duration::from_secs(5), async {
            tokio::join!(
                provider.session(&request, CopilotSessionReuse::Compatible),
                server
            )
        })
        .await?;
        served?;
        assert_eq!(
            result.is_ok(),
            scenario == "valid" || scenario == "repair",
            "{scenario}"
        );
        if scenario == "repair" {
            assert!(
                provider
                    .sessions
                    .repair(&request.runtime_key()?)
                    .await
                    .is_some()
            );
        }
    }
    Ok(())
}

#[derive(Clone, Copy, PartialEq)]
enum NativeUserDelivery {
    HistoryOnly,
    NotRecorded,
    ExpiredHistory,
}

fn event_channel(
    request: &ProviderRequest,
) -> Result<(
    ProviderEventSink,
    crate::provider::events::ProviderEventStream,
)> {
    Ok(crate::provider::events::channel(InvocationScope::new(
        request.owner()?,
        "exchange",
    )))
}

/// Replace eager sessions for the first scoped tool catalog, then preserve submitted history.
#[tokio::test]
async fn copilot_prepared_schema_refresh_recreates_before_submission_and_resumes_after()
-> Result<()> {
    for mode in [PromptMode::Chat, PromptMode::Plan] {
        let (root, provider, mut request) = fixture()?;
        crate::settings::SettingsStore::open(root.path())?.configure_catalog(root.path())?;
        let local_session = serde_json::from_value(json!({
            "id":request.harness_session_id,"primary_agent_id":"agent","name":"Local name retained",
            "workspace":request.workspace,"provider":"copilot","model":"default","effort":"medium",
            "provider_fork_state":{"state":"ready"},"approval_mode":"ask",
            "created_at_ms":1,"updated_at_ms":1
        }))?;
        let policy = Arc::new(
            crate::permissions::matcher::CompiledPermissionDocument::compile(
                crate::permissions::document::default_permission_document(),
                root.path(),
            ),
        );
        let (inspection, _lease) = crate::tools::session::HarnessInspectionScope::open(
            local_session,
            root.path().into(),
            "permissions.json".into(),
            policy,
            Arc::new(AtomicBool::new(false)),
            Arc::clone(&provider.trace),
        )?;
        let mut context = ControlTurnContext::inactive(mode);
        context.harness = Some(inspection);
        let (client_read, mut server_write) = tokio::io::duplex(65536);
        let (client_write, mut server_read) = tokio::io::duplex(65536);
        provider
            .runtime
            .attach_streams(client_read, client_write, PathBuf::from(&request.workspace))
            .await?;
        let (sink, mut events) = event_channel(&request)?;
        let server = async {
            let first = read_request(&mut server_read, &mut server_write).await?;
            assert_eq!(first["method"], "session.create");
            let prepared = first["params"]["sessionId"].clone();
            respond(
                &mut server_write,
                &first,
                json!({"result":{"sessionId":prepared,"workspacePath":"."}}),
            )
            .await?;
            let replacement = read_request(&mut server_read, &mut server_write).await?;
            assert_eq!(
                replacement["method"], "session.create",
                "empty native history cannot be resumed"
            );
            let submitted = replacement["params"]["sessionId"].clone();
            assert_ne!(submitted, prepared);
            assert!(
                replacement["params"]["tools"]
                    .as_array()
                    .unwrap()
                    .iter()
                    .any(|tool| tool["name"] == "harness_config")
            );
            respond(
                &mut server_write,
                &replacement,
                json!({"result":{"sessionId":submitted,"workspacePath":"."}}),
            )
            .await?;
            let disconnect = read_request(&mut server_read, &mut server_write).await?;
            assert_eq!(disconnect["method"], "session.detach");
            assert_eq!(disconnect["params"]["sessionId"], prepared);
            respond(&mut server_write, &disconnect, json!({"result":{}})).await?;
            let todos = read_request(&mut server_read, &mut server_write).await?;
            assert_eq!(todos["method"], "session.plan.readSqlTodosWithDependencies");
            respond(
                &mut server_write,
                &todos,
                json!({"result":{"rows":[],"dependencies":[]}}),
            )
            .await?;
            let tail = read_request(&mut server_read, &mut server_write).await?;
            assert_eq!(tail["method"], "session.eventLog.tail");
            respond(
                &mut server_write,
                &tail,
                json!({"result":{"cursor":"before"}}),
            )
            .await?;
            let send = read_request(&mut server_read, &mut server_write).await?;
            assert_eq!(send["method"], "session.send");
            assert_eq!(send["params"]["sessionId"], submitted);
            respond(
                &mut server_write,
                &send,
                json!({"result":{"messageId":"user"}}),
            )
            .await?;
            for (index, (kind, data)) in [
                (
                    "user.message",
                    json!({"messageId":"user","turnId":"turn","content":"fixture"}),
                ),
                ("assistant.turn_start", json!({"turnId":"turn"})),
                (
                    "assistant.message",
                    json!({"messageId":"answer","phase":"final_answer","content":"READY"}),
                ),
                ("assistant.turn_end", json!({"turnId":"turn"})),
                ("session.idle", json!({})),
            ]
            .into_iter()
            .enumerate()
            {
                let mut event = json!({"id":format!("prepared-event-{index}"),"type":kind,"timestamp":"2026-10-30T00:00:00Z","data":data});
                if index > 0 {
                    event["parentId"] = json!(format!("prepared-event-{}", index - 1));
                }
                write_frame(
                    &mut server_write,
                    json!({"jsonrpc":"2.0","method":"session.event","params":{
                        "sessionId":submitted,"event":event
                    }}),
                )
                .await?;
            }
            let disconnect = read_request(&mut server_read, &mut server_write).await?;
            assert_eq!(disconnect["method"], "session.detach");
            respond(&mut server_write, &disconnect, json!({"result":{}})).await?;
            let resume = read_request(&mut server_read, &mut server_write).await?;
            assert_eq!(resume["method"], "session.resume");
            assert_eq!(resume["params"]["sessionId"], submitted);
            assert_eq!(resume["params"]["disableResume"], true);
            respond(
                &mut server_write,
                &resume,
                json!({"result":{"sessionId":submitted,"workspacePath":"."}}),
            )
            .await?;
            let reload = read_request(&mut server_read, &mut server_write).await?;
            assert_eq!(reload["method"], "session.skills.reload");
            respond(&mut server_write, &reload, json!({"result":{}})).await?;
            Ok::<_, anyhow::Error>(())
        };
        let workflow = async {
            let prepared = provider
                .initialize_session(request.clone())
                .await?
                .context("missing prepared identity")?;
            request.provider_session_id = Some(prepared.clone());
            request.mode = mode;
            request.control_context = Some(context);
            let completion = provider
                .session_prompt_and_wait(request.clone(), sink)
                .await?;
            assert_ne!(
                completion
                    .provider_session_id
                    .as_ref()
                    .map(|id| id.as_str()),
                Some(prepared.as_str())
            );
            request.control_context.as_mut().unwrap().mode = PromptMode::PlanQuestion;
            assert_eq!(
                provider.initialize_session(request).await?,
                completion.provider_session_id
            );
            Ok::<_, anyhow::Error>(())
        };
        tokio::time::timeout(std::time::Duration::from_secs(5), async {
            tokio::try_join!(server, workflow)
        })
        .await??;
        while events.try_next()?.is_some() {}
    }
    Ok(())
}

/// Keep the connected prepared session when creation fails, including resource refresh.
#[tokio::test]
async fn copilot_failed_prepared_refresh_keeps_original_until_retry_commits() -> Result<()> {
    let (_root, provider, mut request) = fixture()?;
    let (client_read, mut server_write) = tokio::io::duplex(65536);
    let (client_write, mut server_read) = tokio::io::duplex(65536);
    provider
        .runtime
        .attach_streams(client_read, client_write, PathBuf::from(&request.workspace))
        .await?;
    let server = async {
        let create = read_request(&mut server_read, &mut server_write).await?;
        assert_eq!(create["method"], "session.create");
        let prepared = create["params"]["sessionId"].clone();
        respond(
            &mut server_write,
            &create,
            json!({"result":{"sessionId":prepared,"workspacePath":"."}}),
        )
        .await?;
        let failed = read_request(&mut server_read, &mut server_write).await?;
        assert_eq!(failed["method"], "session.create");
        respond(
            &mut server_write,
            &failed,
            json!({"error":{"code":-32603,"message":"fixture replacement failed"}}),
        )
        .await?;
        let retry = read_request(&mut server_read, &mut server_write).await?;
        assert_eq!(
            retry["method"], "session.create",
            "retry tried to resume an unpersisted ID"
        );
        let replacement = retry["params"]["sessionId"].clone();
        assert_ne!(replacement, prepared);
        respond(
            &mut server_write,
            &retry,
            json!({"result":{"sessionId":replacement,"workspacePath":"."}}),
        )
        .await?;
        let destroy = read_request(&mut server_read, &mut server_write).await?;
        assert_eq!(destroy["method"], "session.detach");
        assert_eq!(destroy["params"]["sessionId"], prepared);
        respond(&mut server_write, &destroy, json!({"result":{}})).await?;
        Ok::<_, anyhow::Error>(())
    };
    let workflow = async {
        let prepared = provider
            .initialize_session(request.clone())
            .await?
            .context("missing prepared identity")?;
        request.provider_session_id = Some(prepared.clone());
        let failure = provider
            .session(&request, CopilotSessionReuse::Refresh)
            .await;
        assert!(failure.is_err());
        let retained = provider
            .session(&request, CopilotSessionReuse::Compatible)
            .await?;
        assert_eq!(retained.id().as_str(), prepared.as_str());
        let replacement = provider
            .session(&request, CopilotSessionReuse::Refresh)
            .await?;
        assert_ne!(replacement.id().as_str(), prepared.as_str());
        Ok::<_, anyhow::Error>(())
    };
    tokio::time::timeout(std::time::Duration::from_secs(5), async {
        tokio::try_join!(server, workflow)
    })
    .await??;
    Ok(())
}

/// Preserve a genuine missing-history failure instead of starting an unrelated conversation.
#[tokio::test]
async fn copilot_missing_resumed_history_remains_strict() -> Result<()> {
    let (_root, provider, mut request) = fixture()?;
    request.provider_session_id = Some("missing-populated-session".to_owned().try_into().unwrap());
    let (client_read, mut server_write) = tokio::io::duplex(65536);
    let (client_write, mut server_read) = tokio::io::duplex(65536);
    provider
        .runtime
        .attach_streams(client_read, client_write, PathBuf::from(&request.workspace))
        .await?;
    let server = async {
        let resume = read_request(&mut server_read, &mut server_write).await?;
        assert_eq!(resume["method"], "session.resume");
        respond(&mut server_write,&resume,json!({"error":{"code":-32603,"message":"Session not found: missing-populated-session"}})).await
    };
    let workflow = async {
        let failure = provider.initialize_session(request).await.unwrap_err();
        assert!(matches!(
            failure.downcast_ref::<ProviderError>(),
            Some(ProviderError::SessionMissing { .. })
        ));
        Ok::<_, anyhow::Error>(())
    };
    tokio::try_join!(server, workflow)?;
    Ok(())
}

#[tokio::test]
async fn copilot_retry_waits_for_idle_and_keeps_the_native_session() -> Result<()> {
    let (fixture, provider, mut request) = fixture()?;
    let submission_store =
        crate::provider::submission::SubmissionStore::open(fixture.path(), provider.trace.clone())?;
    let submission = submission_store.prepare(&request, ProviderKind::Copilot)?;
    request.submission = Some(submission.clone());
    let (client_read, mut server_write) = tokio::io::duplex(65536);
    let (client_write, mut server_read) = tokio::io::duplex(65536);
    provider
        .runtime
        .attach_streams(client_read, client_write, PathBuf::from(&request.workspace))
        .await?;
    let (sink, mut events) = event_channel(&request)?;
    let server = async {
        let create = read_request(&mut server_read, &mut server_write).await?;
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
        let mut first_prompt = None;
        for attempt in 0..2 {
            let todos = read_request(&mut server_read, &mut server_write).await?;
            assert_eq!(todos["method"], "session.plan.readSqlTodosWithDependencies");
            respond(
                &mut server_write,
                &todos,
                json!({"result":{"rows":[],"dependencies":[]}}),
            )
            .await?;
            let tail = read_request(&mut server_read, &mut server_write).await?;
            assert_eq!(tail["method"], "session.eventLog.tail");
            respond(
                &mut server_write,
                &tail,
                json!({"result":{"cursor":"retained"}}),
            )
            .await?;
            let send = read_request(&mut server_read, &mut server_write).await?;
            assert_eq!(send["method"], "session.send");
            assert_eq!(send["params"]["sessionId"], session_id);
            let prompt = send["params"]["prompt"]
                .as_str()
                .context("native retry prompt missing")?;
            if attempt == 0 {
                first_prompt = Some(prompt.to_owned());
            } else {
                assert_ne!(
                    Some(prompt),
                    first_prompt.as_deref(),
                    "accepted input was replayed"
                );
                assert!(prompt.contains(
                    "Continue the previous request after the temporary provider failure"
                ));
            }
            if attempt == 0 {
                assert!(matches!(
                    submission.record()?.delivery,
                    crate::provider::submission::SubmissionState::Sending { .. }
                ));
            }
            respond(
                &mut server_write,
                &send,
                json!({"result":{"messageId":format!("message-{attempt}")}}),
            )
            .await?;
            write_frame(&mut server_write, json!({"jsonrpc":"2.0","method":"session.event","params":{
                "sessionId":session_id,"event":{"id":format!("input-{attempt}"),"type":"user.message","timestamp":"2026-10-20T12:00:00Z","data":{"messageId":format!("message-{attempt}"),"turnId":format!("turn-{attempt}"),"content":prompt}}
            }})).await?;
            write_frame(&mut server_write, json!({"jsonrpc":"2.0","method":"session.event","params":{
                "sessionId":session_id,"event":{"id":format!("start-{attempt}"),"parentId":format!("input-{attempt}"),"type":"assistant.turn_start","timestamp":"2026-10-20T12:00:00Z","data":{"turnId":format!("turn-{attempt}")}}
            }})).await?;
            if attempt == 0 {
                write_frame(&mut server_write, json!({"jsonrpc":"2.0","method":"session.event","params":{
                    "sessionId":session_id,"event":{"id":"failure","parentId":"start-0","type":"session.error","timestamp":"2026-10-20T12:00:00Z","data":{"errorType":"query","statusCode":503,"message":"service temporarily unavailable"}}
                }})).await?;
                assert!(
                    tokio::time::timeout(
                        std::time::Duration::from_millis(30),
                        read_request(&mut server_read, &mut server_write)
                    )
                    .await
                    .is_err(),
                    "retry began before the provider became idle"
                );
            }
            write_frame(&mut server_write, json!({"jsonrpc":"2.0","method":"session.event","params":{
                "sessionId":session_id,"event":{"id":format!("end-{attempt}"),"parentId":if attempt == 0 { "failure".to_owned() } else { format!("start-{attempt}") },"type":"assistant.turn_end","timestamp":"2026-10-20T12:00:00Z","data":{"turnId":format!("turn-{attempt}")}}
            }})).await?;
            write_frame(&mut server_write, json!({"jsonrpc":"2.0","method":"session.event","params":{
                "sessionId":session_id,"event":{"id":format!("idle-{attempt}"),"parentId":format!("end-{attempt}"),"type":"session.idle","timestamp":"2026-10-20T12:00:00Z","data":{}}
            }})).await?;
        }
        Ok::<_, anyhow::Error>(())
    };
    tokio::time::timeout(std::time::Duration::from_secs(5), async {
        tokio::try_join!(
            server,
            crate::provider::test_support::retry_prompt(
                &provider,
                request,
                Some(sink),
                &provider.trace,
                &submission_store
            )
        )
    })
    .await??;
    assert!(matches!(
        submission.record()?.delivery,
        crate::provider::submission::SubmissionState::Accepted { .. }
    ));
    let mut retry_count = 0;
    let mut failed_turn = false;
    while let Ok(event) = events.try_recv() {
        assert!(
            !matches!(event, ProviderEvent::Diagnostic { .. }),
            "transient error escaped as a terminal notification"
        );
        if let ProviderEvent::Retry {
            update: Some(update),
            ..
        } = &event
        {
            assert_eq!(update.attempt, 1);
            retry_count += 1;
        }
        failed_turn |= matches!(
            event,
            ProviderEvent::Turn {
                update: TurnUpdate::Finished {
                    outcome: crate::turn::TurnOutcome::Failed,
                    ..
                },
                ..
            }
        );
    }
    assert_eq!(retry_count, 2);
    assert!(failed_turn);
    Ok(())
}

#[tokio::test]
async fn copilot_disables_tool_search_and_binds_session_files_on_create_and_resume() -> Result<()> {
    for resumed in [false, true] {
        let (_fixture, provider, mut request) = fixture()?;
        let external = tempfile::tempdir()?;
        let session_directory = external.path().join("custom state location/session notes");
        std::fs::create_dir_all(&session_directory)?;
        request.provider_session_id =
            resumed.then(|| "retained-session".to_owned().try_into().unwrap());
        let (client_read, mut server_write) = tokio::io::duplex(65536);
        let (client_write, mut server_read) = tokio::io::duplex(65536);
        provider
            .runtime
            .attach_streams(client_read, client_write, PathBuf::from(&request.workspace))
            .await?;
        let server = async {
            let message = read_request(&mut server_read, &mut server_write).await?;
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
                    .any(|tool| tool["name"] == "plan_file_read")
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
                let reload = read_request(&mut server_read, &mut server_write).await?;
                assert_eq!(reload["method"], "session.skills.reload");
                respond(&mut server_write, &reload, json!({"result":{}})).await?;
                let history = read_request(&mut server_read, &mut server_write).await?;
                assert_eq!(history["method"], "session.eventLog.read");
                respond(&mut server_write, &history, json!({"result":{"cursor":"validated","cursorStatus":"ok","hasMore":false,"events":[]}})).await?;
            }
            Ok::<_, anyhow::Error>(())
        };
        let (session, ()) = tokio::time::timeout(std::time::Duration::from_secs(5), async {
            tokio::try_join!(
                provider.session(&request, CopilotSessionReuse::Compatible),
                server
            )
        })
        .await??;
        let (sink, mut events) = event_channel(&request)?;
        let context = provider.permission_context(&request.runtime_key()?).await;
        context.configure(
            request.owner()?,
            ApprovalMode::Ask,
            request.workspace.clone(),
            Some(sink),
        )?;
        let handler = forge_providers::copilot::permission::handler(context);
        let PermissionResult::Decision {
            decision: expected,
            context: _,
        } = PermissionResult::approve_once()
        else {
            unreachable!()
        };
        let expected = serde_json::to_value(expected)?;
        for kind in ["read", "write"] {
            let result = tokio::time::timeout(
                std::time::Duration::from_secs(1),
                handler.handle(
                    SessionId::new(session.id()),
                    RequestId::new(format!("session-{kind}")),
                    serde_json::from_value(json!({
                        "kind":kind, "path":session_directory.join("plan.md")
                    }))?,
                ),
            )
            .await?;
            let PermissionResult::Decision {
                decision,
                context: _,
            } = result
            else {
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
async fn retired_permission_context_rejects_pending_and_late_callbacks() -> Result<()> {
    let (_root, provider, request) = fixture()?;
    let external = tempfile::tempdir()?;
    let coordinator = provider.permission_coordinator.clone();
    let (sink, mut events) = event_channel(&request)?;
    let context = Arc::new(ProviderPermissionContext::new(coordinator.clone()));
    context.configure(
        request.owner()?,
        ApprovalMode::Ask,
        request.workspace.clone(),
        Some(sink),
    )?;
    let handler = forge_providers::copilot::permission::handler(context.clone());
    let pending_handler = handler.clone();
    let path = external.path().join("pending.txt");
    let pending = tokio::spawn(async move {
        pending_handler
            .handle(
                serde_json::from_value(json!("native")).unwrap(),
                serde_json::from_value(json!("pending")).unwrap(),
                serde_json::from_value(json!({"kind":"write","path":path})).unwrap(),
            )
            .await
    });
    let event = tokio::time::timeout(std::time::Duration::from_secs(1), events.recv())
        .await??
        .context("approval request missing")?;
    let ProviderEvent::Approval {
        update: ApprovalUpdate::Requested(approval),
        ..
    } = event
    else {
        anyhow::bail!("expected approval request");
    };
    context.retire()?;
    assert!(
        coordinator
            .resolve(
                &approval.id,
                &[crate::provider::approval::ApprovalAnswer {
                    item_id: approval.item_list[0].id.clone(),
                    choice_id: "allow_once".into(),
                }],
                None
            )
            .await
            .is_err()
    );
    coordinator.cancel_owner(&request.owner()?, None).await?;
    let PermissionResult::Decision { decision, .. } = pending.await? else {
        anyhow::bail!("missing decision");
    };
    let PermissionResult::Decision {
        decision: allowed, ..
    } = PermissionResult::approve_once()
    else {
        unreachable!()
    };
    assert_ne!(
        serde_json::to_value(decision)?,
        serde_json::to_value(&allowed)?
    );
    assert!(
        forge_provider_api::permissions::ProviderPermissions::before_tool(context.as_ref())
            .await
            .is_err()
    );
    let late = handler
        .handle(
            serde_json::from_value(json!("native"))?,
            serde_json::from_value(json!("late"))?,
            serde_json::from_value(
                json!({"kind":"write","path":external.path().join("late.txt")}),
            )?,
        )
        .await;
    let PermissionResult::Decision { decision, .. } = late else {
        anyhow::bail!("missing late decision");
    };
    assert_ne!(
        serde_json::to_value(decision)?,
        serde_json::to_value(&allowed)?
    );
    Ok(())
}

#[tokio::test]
async fn copilot_file_callbacks_apply_directory_scopes_without_sandbox() -> Result<()> {
    let (root, provider, mut request) = fixture()?;
    let external = tempfile::tempdir()?;
    request.access.read_access = crate::session::DirectoryScope::Workspace;
    request.approval_mode = ApprovalMode::Ask;
    assert!(!request.access.sandbox);
    let coordinator = provider.permission_coordinator.clone();
    let _directories = coordinator.directories(Some(&root.path().join("settings")), &request)?;
    let (sink, mut events) = event_channel(&request)?;
    let context = Arc::new(ProviderPermissionContext::new(coordinator.clone()));
    context.configure(
        request.owner()?,
        ApprovalMode::Ask,
        request.workspace.clone(),
        Some(sink),
    )?;
    let handler = forge_providers::copilot::permission::handler(context);
    let PermissionResult::Decision {
        decision: expected,
        context: _,
    } = PermissionResult::approve_once()
    else {
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
        let ProviderEvent::Approval {
            update: ApprovalUpdate::Requested(approval),
            ..
        } = event
        else {
            panic!("expected requested approval");
        };
        coordinator
            .resolve(
                &approval.id,
                &[crate::provider::approval::ApprovalAnswer {
                    item_id: approval.item_list[0].id.clone(),
                    choice_id: "allow_global_directory".into(),
                }],
                None,
            )
            .await?;
        let PermissionResult::Decision {
            decision,
            context: _,
        } = waiter.await?
        else {
            panic!("approval returned no decision")
        };
        assert_eq!(serde_json::to_value(decision)?, expected);
        assert!(matches!(
            events.recv().await.unwrap().unwrap(),
            ProviderEvent::DirectoryAccess { .. }
        ));
        assert!(matches!(
            events.recv().await.unwrap().unwrap(),
            ProviderEvent::Approval {
                update: ApprovalUpdate::Resolved(_),
                ..
            }
        ));
        let repeat = tokio::time::timeout(
            std::time::Duration::from_secs(1),
            handler.handle(
                serde_json::from_value(json!("native"))?,
                serde_json::from_value(json!("repeat"))?,
                serde_json::from_value(data)?,
            ),
        )
        .await?;
        let PermissionResult::Decision {
            decision,
            context: _,
        } = repeat
        else {
            panic!("repeat returned no decision")
        };
        assert_eq!(serde_json::to_value(decision)?, expected);
    }
    Ok(())
}

#[tokio::test]
async fn cancellation_during_copilot_startup_does_not_wait_or_send() -> Result<()> {
    let (_fixture, provider, request) = fixture()?;
    let session_id = request.runtime_key()?;
    let (client_read, _server_write) = tokio::io::duplex(65536);
    let (client_write, mut server_read) = tokio::io::duplex(65536);
    provider
        .runtime
        .attach_streams(client_read, client_write, PathBuf::from(&request.workspace))
        .await?;
    let (sink, _events) = event_channel(&request)?;
    let mut prompt = Box::pin(provider.session_prompt_and_wait(request, sink));
    let creating = tokio::select! {
        request = read_frame(&mut server_read) => request?,
        result = &mut prompt => panic!("prompt settled before native startup: {result:?}"),
    };
    assert_eq!(creating["method"], "status.get");
    assert!(provider.has_active_turn(&session_id).await);
    tokio::time::timeout(
        std::time::Duration::from_millis(100),
        provider.cleanup_execution(&session_id),
    )
    .await
    .context("cancellation waited for provider startup")??;
    drop(prompt);
    assert_eq!(provider.client_start_count(), 0);
    assert!(!provider.has_active_turn(&session_id).await);
    assert!(provider.rollback_cancelled_turn_for(&session_id).await?);
    Ok(())
}

#[tokio::test]
async fn copilot_submission_fence_survives_cleanup_before_turn_activation() -> Result<()> {
    let turn = CopilotTurnEvent::default();
    let cancelled = Arc::new(AtomicBool::new(true));
    let active = turn.activate(None, Some(cancelled))?;
    let error = active
        .send("before-send".into(), None, async {
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
    let mut sending = Box::pin(active.send("before-send".into(), None, async {
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
            .send("before-send".into(), None, async {
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
    let (_fixture, provider, request) = fixture()?;
    let session_id = request.runtime_key()?;
    let (client_read, mut server_write) = tokio::io::duplex(65536);
    let (client_write, mut server_read) = tokio::io::duplex(65536);
    provider
        .runtime
        .attach_streams(client_read, client_write, PathBuf::from(&request.workspace))
        .await?;
    let create = async {
        let message = read_request(&mut server_read, &mut server_write).await?;
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
    let (session, ()) = tokio::try_join!(
        provider.session(&request, CopilotSessionReuse::Compatible),
        create
    )?;
    let turn = provider.turn_event(&session_id).await;
    let active = turn.activate(None, None)?;
    let (submitted, submission) = tokio::sync::oneshot::channel();
    let sending = async {
        let result = active
            .send("before-send".into(), None, async {
                session
                    .send(forge_provider_api::input::MessageInput {
                        prompt: "pending send".into(),
                        attachments: Vec::new(),
                    })
                    .await
                    .context("fixture send failed")
            })
            .await;
        drop(active);
        result
    };
    let stopping = async {
        submission.await?;
        provider.cleanup_execution(&session_id).await
    };
    let server = async {
        let send = read_request(&mut server_read, &mut server_write).await?;
        assert_eq!(send["method"], "session.send");
        submitted.send(()).unwrap();
        let abort = read_request(&mut server_read, &mut server_write).await?;
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
        let collected_abort = read_request(&mut server_read, &mut server_write).await?;
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
        .send("before-send".into(), None, async {
            Ok("transport-id".into())
        })
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
        native_rewind(first_failure).await?;
    }
    Ok(())
}

#[tokio::test]
async fn copilot_new_admission_cannot_rewind_an_earlier_cancelled_turn() -> Result<()> {
    let (_fixture, provider, request) = fixture()?;
    let session_id = request.runtime_key()?;
    let turn = provider.turn_event(&session_id).await;
    let active = turn.activate(None, None)?;
    active
        .send("before-send".into(), None, async {
            Ok("transport-id".into())
        })
        .await?;
    turn.observe(&serde_json::from_value(json!({
        "id":"previous-user", "type":"user.message", "timestamp":"2026-10-15T17:00:00Z", "data":{}
    }))?)?;
    assert!(turn.stop().await?);
    drop(active);
    assert!(
        matches!(turn.retraction_target()?, CopilotRetraction::Event(id) if id == "previous-user")
    );
    provider.prepare_turn(&session_id).await?;
    assert!(provider.rollback_cancelled_turn_for(&session_id).await?);
    Ok(())
}

#[tokio::test]
async fn copilot_rewind_verifies_history_when_abort_precedes_the_user_event() -> Result<()> {
    native_history_rewind(NativeUserDelivery::HistoryOnly).await?;
    native_history_rewind(NativeUserDelivery::NotRecorded).await?;
    native_history_rewind(NativeUserDelivery::ExpiredHistory).await
}

/// Exercise the retained history boundary without claiming certified native cancellation.
async fn native_history_rewind(delivery: NativeUserDelivery) -> Result<()> {
    let (_fixture, provider, request) = fixture()?;
    let session_id = request.runtime_key()?;
    let (client_read, mut server_write) = tokio::io::duplex(65536);
    let (client_write, mut server_read) = tokio::io::duplex(65536);
    provider
        .runtime
        .attach_streams(client_read, client_write, PathBuf::from(&request.workspace))
        .await?;
    let server = async {
        let create = read_request(&mut server_read, &mut server_write).await?;
        assert_eq!(create["method"], "session.create");
        let native_id = create["params"]["sessionId"].clone();
        respond(
            &mut server_write,
            &create,
            json!({"result":{"sessionId":native_id,"workspacePath":"."}}),
        )
        .await?;
        let history = read_request(&mut server_read, &mut server_write).await?;
        assert_eq!(history["method"], "session.eventLog.read");
        assert_eq!(history["params"]["cursor"], "pre-submit");
        assert_eq!(history["params"]["agentScope"], "primary");
        assert_eq!(history["params"]["types"], json!(["user.message"]));
        let events = if delivery == NativeUserDelivery::HistoryOnly {
            json!([{"id":"root-user","type":"user.message","timestamp":"2026-10-15T17:00:00Z","data":{}}])
        } else {
            json!([])
        };
        respond(&mut server_write, &history, json!({"result":{"cursor":"after-submit","cursorStatus":if delivery == NativeUserDelivery::ExpiredHistory {"expired"} else {"ok"},"hasMore":false,"events":events}})).await?;
        if delivery == NativeUserDelivery::HistoryOnly {
            let truncate = read_request(&mut server_read, &mut server_write).await?;
            assert_eq!(truncate["method"], "session.history.truncate");
            assert_eq!(truncate["params"]["sessionId"], native_id);
            assert_eq!(truncate["params"]["eventId"], "root-user");
            respond(
                &mut server_write,
                &truncate,
                json!({"result":{"eventsRemoved":1}}),
            )
            .await?;
        }
        Ok::<_, anyhow::Error>(())
    };
    let execution = async {
        provider
            .session(&request, CopilotSessionReuse::Compatible)
            .await?;
        let turn = provider.turn_event(&session_id).await;
        let active = turn.activate(None, None)?;
        active
            .send("pre-submit".into(), None, async {
                Ok("transport-response-id".into())
            })
            .await?;
        assert!(turn.stop().await?);
        drop(active);
        if delivery == NativeUserDelivery::ExpiredHistory {
            let error = provider
                .rollback_cancelled_turn_for(&session_id)
                .await
                .unwrap_err();
            assert!(
                error.to_string().contains("history boundary expired"),
                "{error:#}"
            );
        } else {
            assert!(provider.rollback_cancelled_turn_for(&session_id).await?);
            assert!(provider.rollback_cancelled_turn_for(&session_id).await?);
        }
        Ok::<_, anyhow::Error>(())
    };
    tokio::time::timeout(std::time::Duration::from_secs(2), async {
        tokio::try_join!(server, execution)
    })
    .await
    .context("native Copilot history rewind did not settle")??;
    Ok(())
}

async fn native_rewind(first_failure: Option<Value>) -> Result<()> {
    let (_fixture, provider, request) = fixture()?;
    let session_id = request.runtime_key()?;
    let (client_read, mut server_write) = tokio::io::duplex(65536);
    let (client_write, mut server_read) = tokio::io::duplex(65536);
    provider
        .runtime
        .attach_streams(client_read, client_write, PathBuf::from(&request.workspace))
        .await?;
    let (sent, sent_ready) = tokio::sync::oneshot::channel();
    let retry = first_failure.is_some();
    let server = async move {
        let create = read_request(&mut server_read, &mut server_write).await?;
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
        let todos = read_request(&mut server_read, &mut server_write).await?;
        assert_eq!(todos["method"], "session.plan.readSqlTodosWithDependencies");
        respond(
            &mut server_write,
            &todos,
            json!({"result":{"rows":[],"dependencies":[]}}),
        )
        .await?;
        let tail = read_request(&mut server_read, &mut server_write).await?;
        assert_eq!(tail["method"], "session.eventLog.tail");
        respond(
            &mut server_write,
            &tail,
            json!({"result":{"cursor":"pre-submit"}}),
        )
        .await?;
        let send = read_request(&mut server_read, &mut server_write).await?;
        assert_eq!(send["method"], "session.send");
        assert_ne!(send["params"]["prompt"], "/rewind");
        respond(
            &mut server_write,
            &send,
            json!({"result":{"messageId":"transport-response-id"}}),
        )
        .await?;
        for (id, agent) in [
            ("child-user", Some("child")),
            ("root-user", None),
            ("reminder", None),
        ] {
            let data = if id == "root-user" {
                json!({"messageId":"transport-response-id","turnId":"rewind-turn","content":"fixture"})
            } else {
                json!({"content":"fixture"})
            };
            write_frame(
                &mut server_write,
                json!({
                    "jsonrpc":"2.0", "method":"session.event", "params":{
                        "sessionId":native_session_id, "event":{
                            "id":id, "type":"user.message", "timestamp":"2026-10-15T17:00:00Z",
                            "agentId":agent, "data":data
                        }
                    }
                }),
            )
            .await?;
        }
        write_frame(&mut server_write, json!({"jsonrpc":"2.0","method":"session.event","params":{
                "sessionId":native_session_id,"event":{"id":"root-start","parentId":"root-user","type":"assistant.turn_start","timestamp":"2026-10-15T17:00:00Z","data":{"turnId":"rewind-turn"}}
            }})).await?;
        sent.send(()).unwrap();
        let abort = read_request(&mut server_read, &mut server_write).await?;
        assert_eq!(abort["method"], "session.abort");
        respond(&mut server_write, &abort, json!({"result":{}})).await?;
        write_frame(&mut server_write, json!({
            "jsonrpc":"2.0", "method":"session.event", "params":{
                "sessionId":native_session_id, "event":{
                    "id":"idle", "parentId":"root-start", "type":"session.idle", "timestamp":"2026-10-15T17:00:01Z", "data":{"aborted":true}
                }
            }
        })).await?;
        for attempt in 0..if retry { 2 } else { 1 } {
            let rewind = read_request(&mut server_read, &mut server_write).await?;
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
        let (sink, _events) = event_channel(&request)?;
        let prompt = provider.session_prompt_and_wait(request, sink);
        let cleanup = async {
            sent_ready.await?;
            tokio::time::timeout(std::time::Duration::from_secs(1), async {
                loop {
                    if provider
                        .turn_event(&session_id)
                        .await
                        .foreground_anchor()?
                        .is_some()
                    {
                        break Ok::<_, anyhow::Error>(());
                    }
                    tokio::task::yield_now().await;
                }
            })
            .await??;
            provider.cleanup_execution(&session_id).await
        };
        let (result, cleanup) = tokio::join!(prompt, cleanup);
        cleanup?;
        assert!(matches!(result, Err(ProviderError::Cancelled)));
        if retry {
            let error = provider
                .rollback_cancelled_turn_for(&session_id)
                .await
                .unwrap_err();
            assert!(
                format!("{error:#}").contains("history truncation unavailable")
                    || format!("{error:#}").contains("removed no history events"),
                "{error:#}"
            );
        }
        assert!(provider.rollback_cancelled_turn_for(&session_id).await?);
        assert!(provider.rollback_cancelled_turn_for(&session_id).await?);
        Ok::<_, anyhow::Error>(())
    };
    tokio::time::timeout(std::time::Duration::from_secs(2), async {
        tokio::try_join!(server, execution)
    })
    .await
    .context("native Copilot cancellation/rewind did not settle")??;
    Ok(())
}

/// Keep a background wakeup inside its admitted invocation until actual session idle.
#[tokio::test]
async fn copilot_background_wakeup_settles_before_next_prompt() -> Result<()> {
    let (_root, provider, mut request) = fixture()?;
    let (client_read, mut server_write) = tokio::io::duplex(65536);
    let (client_write, mut server_read) = tokio::io::duplex(65536);
    provider
        .runtime
        .attach_streams(client_read, client_write, PathBuf::from(&request.workspace))
        .await?;
    let server = async {
        let create = read_request(&mut server_read, &mut server_write).await?;
        assert_eq!(create["method"], "session.create");
        let session_id = create["params"]["sessionId"].clone();
        respond(
            &mut server_write,
            &create,
            json!({"result":{"sessionId":session_id,"workspacePath":"."}}),
        )
        .await?;
        for attempt in 0..2 {
            for (method, result) in [
                (
                    "session.plan.readSqlTodosWithDependencies",
                    json!({"rows":[],"dependencies":[]}),
                ),
                (
                    "session.eventLog.tail",
                    json!({"cursor":format!("before-{attempt}")}),
                ),
                (
                    "session.send",
                    json!({"messageId":format!("prompt-{attempt}")}),
                ),
            ] {
                let message = read_request(&mut server_read, &mut server_write).await?;
                assert_eq!(message["method"], method);
                if method == "session.send" {
                    assert_eq!(message["params"]["sessionId"], session_id);
                    assert!(
                        message["params"]["prompt"]
                            .as_str()
                            .unwrap()
                            .contains(&format!("request-{attempt}"))
                    );
                }
                respond(&mut server_write, &message, json!({"result":result})).await?;
            }
            let native = if attempt == 0 {
                vec![
                    (
                        "original-input",
                        "user.message",
                        json!({"messageId":"prompt-0","turnId":"0","content":"request-0"}),
                    ),
                    (
                        "original-start",
                        "assistant.turn_start",
                        json!({"turnId":"0","interactionId":"background"}),
                    ),
                    (
                        "shell-start",
                        "tool.execution_start",
                        json!({"toolCallId":"shell","toolName":"powershell","arguments":{"mode":"async"}}),
                    ),
                    (
                        "shell-launch",
                        "tool.execution_complete",
                        json!({"toolCallId":"shell","success":true,"result":{"content":"shell 0 started"}}),
                    ),
                    (
                        "launch-answer",
                        "assistant.message",
                        json!({"messageId":"launch","phase":"final_answer","content":"Started shell 0"}),
                    ),
                    ("original-end", "assistant.turn_end", json!({"turnId":"0"})),
                    ("foreground-idle", "assistant.idle", json!({})),
                    (
                        "late-output",
                        "tool.execution_partial_result",
                        json!({"toolCallId":"shell","partialOutput":"background completed"}),
                    ),
                    (
                        "wakeup-start",
                        "assistant.turn_start",
                        json!({"turnId":"0","interactionId":"background"}),
                    ),
                    (
                        "wakeup-start",
                        "assistant.turn_start",
                        json!({"turnId":"0","interactionId":"background"}),
                    ),
                    (
                        "wakeup-answer",
                        "assistant.message",
                        json!({"messageId":"wakeup","phase":"final_answer","content":"Background completed"}),
                    ),
                    ("wakeup-end", "assistant.turn_end", json!({"turnId":"0"})),
                    ("session-idle", "session.idle", json!({})),
                ]
            } else {
                vec![
                    (
                        "next-input",
                        "user.message",
                        json!({"messageId":"prompt-1","turnId":"0","content":"request-1"}),
                    ),
                    (
                        "next-start",
                        "assistant.turn_start",
                        json!({"turnId":"0","interactionId":"next"}),
                    ),
                    (
                        "next-answer",
                        "assistant.message",
                        json!({"messageId":"next","phase":"final_answer","content":"Next prompt completed"}),
                    ),
                    ("next-end", "assistant.turn_end", json!({"turnId":"0"})),
                    ("next-idle", "session.idle", json!({})),
                ]
            };
            let mut parents = std::collections::HashMap::new();
            let mut previous = None;
            for (id, kind, payload) in native {
                let parent = *parents.entry(id).or_insert(previous);
                write_frame(&mut server_write, json!({"jsonrpc":"2.0","method":"session.event","params":{
                    "sessionId":session_id,"event":{"id":id,"parentId":parent,"type":kind,"timestamp":"2026-10-30T16:32:00Z","data":payload}
                }})).await?;
                previous = Some(id);
            }
        }
        Ok::<_, anyhow::Error>(())
    };
    let workflow = async {
        let mut native_session = None;
        for attempt in 0..2 {
            request.provider_session_id = native_session.clone();
            request.input = crate::provider::ProviderInput::Text {
                text: format!("request-{attempt}"),
                attachments: Vec::new(),
            };
            let (sink, mut events) = event_channel(&request)?;
            let completed = provider
                .session_prompt_and_wait(request.clone(), sink)
                .await?;
            if let Some(previous) = &native_session {
                assert_eq!(completed.provider_session_id.as_ref(), Some(previous));
            }
            native_session = completed.native.provider_session_id;
            let owner = request.owner()?;
            let mut exchange: crate::exchange::Exchange = serde_json::from_value(json!({
                "id":"exchange","session_id":owner.session_id,"agent_id":owner.agent_id,
                "ordinal":1,"prompt":"Background lifecycle","state":"running","created_at_ms":0,
                "attributed_matches_checkpoint":false,"node_list":[]
            }))?;
            let mut started = Vec::new();
            let mut finished = Vec::new();
            let mut foreground_idle = 0;
            while let Some(event) = events.try_next()? {
                match &event {
                    ProviderEvent::Turn {
                        address,
                        update: TurnUpdate::Started { .. },
                    } => started.push(address.clone()),
                    ProviderEvent::Turn {
                        address,
                        update: TurnUpdate::Finished { .. },
                    } => finished.push(address.clone()),
                    ProviderEvent::Runtime {
                        update: crate::provider::RuntimeUpdate::ForegroundIdle,
                        ..
                    } => foreground_idle += 1,
                    _ => {}
                }
                exchange.observe_turn(&event, 0)?;
            }
            assert_eq!(started.len(), if attempt == 0 { 2 } else { 1 });
            assert_eq!(finished, started);
            assert_eq!(foreground_idle, if attempt == 0 { 1 } else { 0 });
            assert_eq!(exchange.tools().count(), if attempt == 0 { 1 } else { 0 });
        }
        Ok::<_, anyhow::Error>(())
    };
    tokio::time::timeout(std::time::Duration::from_secs(5), async {
        tokio::try_join!(server, workflow)
    })
    .await??;
    Ok(())
}

fn fixture() -> Result<(
    tempfile::TempDir,
    crate::provider::test_support::ProviderTestSession<CopilotProvider>,
    ProviderRequest,
)> {
    let fixture = tempfile::tempdir()?;
    let provider = CopilotProvider::new_with_permission_coordinator(
        Vec::new(),
        PermissionCoordinator::transient(fixture.path())?,
        Arc::new(crate::trace::TraceStore::open(fixture.path())?),
        None,
    )?;
    let provider = crate::provider::test_support::ProviderTestSession::new(provider)?;
    let request = CopilotProvider::catalog_provider_request(&ProviderCatalogRequest {
        execution_owner: Some(ExecutionOwner::root("copilot-cancel")),
        harness_session_id: "copilot-cancel".into(),
        workspace: fixture.path().to_string_lossy().into_owned(),
        access: crate::provider::registered_capability(ProviderKind::Copilot)
            .resolve_access(crate::session::AccessPolicy::default()),
        approval_mode: ApprovalMode::Ask,
        provider_session_id: None,
    });
    Ok((fixture, provider, request))
}

#[tokio::test]
async fn copilot_rejection_feedback_preserves_denial_origin_and_cancellation() -> Result<()> {
    use crate::permissions::document::{CATEGORY_TOOL, PermissionDecision};
    use crate::provider::approval::ApprovalAnswer;

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
        let (_root, provider, request) = fixture()?;
        let coordinator = provider.permission_coordinator.clone();
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
        let (sink, mut events) = event_channel(&request)?;
        let context = provider.permission_context(&request.runtime_key()?).await;
        context.configure(
            request.owner()?,
            mode,
            request.workspace.clone(),
            Some(sink),
        )?;
        let handler = forge_providers::copilot::permission::handler(context.clone());
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
            let ProviderEvent::Approval {
                update: ApprovalUpdate::Requested(view),
                ..
            } = event
            else {
                panic!("expected requested approval");
            };
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
        let PermissionResult::Decision {
            decision,
            context: _,
        } = tokio::time::timeout(std::time::Duration::from_secs(3), task).await??
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
                    "mode" => "Tool approval is set to reject all",
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
                    assert!(feedback.contains("no user is reviewing requests"));
                    assert!(feedback.contains("allowed tools and commands"));
                    assert!(feedback.contains("Do not wait for approval"));
                }
            }
        }
        assert_eq!(context.cancelled().unwrap(), scenario == "cancel");
        assert_eq!(coordinator.store().read().unwrap().open().1, before);
        context.configure(request.owner()?, mode, request.workspace.clone(), None)?;
        assert!(!context.cancelled().unwrap());
    }
    Ok(())
}

#[tokio::test]
async fn copilot_rejection_without_answer_fails_instead_of_completing() -> Result<()> {
    for code in ["rejected", "denied"] {
        for response in ["missing", "commentary", "final", "child", "cancel"] {
            let (_root, provider, request) = fixture()?;
            let expires = code == "denied" && matches!(response, "missing" | "final");
            let owner = request.owner()?;
            provider.permission_coordinator.configure_timeout(
                &request.harness_session_id,
                crate::session::InputTimeout {
                    amount: 1,
                    unit: crate::session::TimeoutUnit::S,
                },
            )?;
            provider
                .permission_coordinator
                .store()
                .write()
                .unwrap()
                .set_rule_list(&[(
                    crate::permissions::document::CATEGORY_BASH.into(),
                    "mock-probe".into(),
                    crate::permissions::document::PermissionDecision::Ask,
                )])?;
            let permission_before = provider
                .permission_coordinator
                .store()
                .read()
                .unwrap()
                .open()
                .1;
            let context = provider.permission_context(&request.runtime_key()?).await;
            let (client_read, mut server_write) = tokio::io::duplex(65536);
            let (client_write, mut server_read) = tokio::io::duplex(65536);
            provider
                .runtime
                .attach_streams(client_read, client_write, PathBuf::from(&request.workspace))
                .await?;
            let (sink, mut events) = event_channel(&request)?;
            let denied = Arc::new(tokio::sync::Notify::new());
            let sink = sink.with_observer(Arc::new(DeniedToolObserved(denied.clone())));
            let server = async {
                let create = read_request(&mut server_read, &mut server_write).await?;
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
                    let message = read_request(&mut server_read, &mut server_write).await?;
                    assert_eq!(message["method"], method);
                    respond(&mut server_write, &message, json!({"result":result})).await?;
                }
                let mut notification = vec![
                    (
                        "user.message",
                        json!({"messageId":"prompt","turnId":"turn","content":"Run the fixture command"}),
                    ),
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
                notification.push(("assistant.turn_end", json!({"turnId":"turn"})));
                notification.push(("session.idle", json!({})));
                for (index, (kind, mut data)) in notification.into_iter().enumerate() {
                    if response == "cancel" && kind == "session.idle" {
                        denied.notified().await;
                        context.cancel().unwrap();
                    }
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
                        let decision = read_request(&mut server_read, &mut server_write).await?;
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
                            provider
                                .permission_coordinator
                                .has_timeout_feedback(&owner)?
                        );
                        data["error"]["message"] = json!(feedback);
                        respond(&mut server_write, &decision, json!({"result":{}})).await?;
                    }
                    let mut event = json!({"id":format!("event-{index}"),"type":kind,
                    "timestamp":"2026-10-27T12:00:00Z","data":data});
                    if index > 0 {
                        event["parentId"] = json!(format!("event-{}", index - 1));
                    }
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
                tokio::join!(
                    server,
                    provider.session_prompt_and_wait(request.clone(), sink)
                )
            })
            .await
            .with_context(|| format!("Copilot rejection case code={code}, response={response}"))?;
            server?;
            match response {
                "final" => {
                    result?;
                }
                "cancel" => assert!(matches!(result.unwrap_err(), ProviderError::Cancelled)),
                _ => assert!(
                    result
                        .unwrap_err()
                        .to_string()
                        .contains("stopped after a tool rejection without a final response")
                ),
            }
            let mut latest = None;
            while let Ok(event) = events.try_recv() {
                if let ProviderEvent::Tool {
                    update: ToolUpdate::Finished { outcome },
                    ..
                } = event
                {
                    latest = Some(outcome);
                }
            }
            assert!(matches!(
                latest.context("tool status missing")?,
                ToolOutcome::Denied { .. }
            ));
            assert_eq!(
                provider
                    .permission_coordinator
                    .has_timeout_feedback(&owner)?,
                expires && response != "final"
            );
            assert_eq!(
                provider
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
        let (fixture, provider, request) = fixture()?;
        let journal = crate::provider::submission::SubmissionStore::open(
            fixture.path(),
            provider.trace.clone(),
        )?;
        let submission = journal.prepare(&request, ProviderKind::Copilot)?;
        submission.sending(crate::provider::submission::SubmissionTarget::native(
            "copilot",
            "retained-native",
            serde_json::json!({"cursor":"before-send","prompt":"same text"}),
        ))?;
        let (client_read, mut server_write) = tokio::io::duplex(65536);
        let (client_write, mut server_read) = tokio::io::duplex(65536);
        provider
            .runtime
            .attach_streams(client_read, client_write, PathBuf::from(&request.workspace))
            .await?;
        let server = async {
            let resume = read_request(&mut server_read, &mut server_write).await?;
            assert_eq!(resume["method"], "session.resume");
            respond(
                &mut server_write,
                &resume,
                json!({"result":{"sessionId":"retained-native","workspacePath":"."}}),
            )
            .await?;
            let reload = read_request(&mut server_read, &mut server_write).await?;
            assert_eq!(reload["method"], "session.skills.reload");
            respond(&mut server_write, &reload, json!({"result":{}})).await?;
            let validation = read_request(&mut server_read, &mut server_write).await?;
            assert_eq!(validation["method"], "session.eventLog.read");
            assert!(
                validation["params"]
                    .get("cursor")
                    .is_none_or(Value::is_null)
            );
            respond(&mut server_write, &validation, json!({"result":{"cursor":"validated","cursorStatus":"ok","hasMore":false,"events":[]}})).await?;
            for page in 0..2 {
                let read = read_request(&mut server_read, &mut server_write).await?;
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
            tokio::try_join!(server, provider.reconcile_submission(&record, &known))
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

/// Serve runtime initialization before returning the next operation under test.
async fn read_request(reader: &mut DuplexStream, writer: &mut DuplexStream) -> Result<Value> {
    loop {
        let request = read_frame(reader).await?;
        if request["method"] != "status.get" {
            return Ok(request);
        }
        respond(
            writer,
            &request,
            json!({"result":{"version":"1.0.103","protocolVersion":1}}),
        )
        .await?;
    }
}

/// Reject unsupported connected runtimes before creating native session history.
#[tokio::test]
async fn initialization_rejects_unsupported_runtime_before_session_creation() -> Result<()> {
    for version in ["1.0.102", "invalid"] {
        let (_fixture, provider, request) = fixture()?;
        let (client_read, mut server_write) = tokio::io::duplex(65536);
        let (client_write, mut server_read) = tokio::io::duplex(65536);
        provider
            .runtime
            .attach_streams(client_read, client_write, PathBuf::from(&request.workspace))
            .await?;
        let server = async {
            let probe = read_frame(&mut server_read).await?;
            assert_eq!(probe["method"], "status.get");
            respond(
                &mut server_write,
                &probe,
                json!({"result":{"version":version,"protocolVersion":1}}),
            )
            .await?;
            Ok::<_, anyhow::Error>(())
        };
        let (result, served) = tokio::time::timeout(std::time::Duration::from_secs(2), async {
            tokio::join!(provider.initialize_session(request), server)
        })
        .await?;
        served?;
        let error = result.expect_err("unsupported runtime must fail initialization");
        assert!(
            matches!(
                error.downcast_ref::<ProviderError>(),
                Some(ProviderError::RuntimeVersion(_))
            ),
            "{error:#}"
        );
    }
    Ok(())
}

async fn read_frame(reader: &mut DuplexStream) -> Result<Value> {
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
    if request["method"] == "session.detach"
        && response.get("result").is_some()
        && response["result"].get("success").is_none()
    {
        response["result"]["success"] = json!(true);
    }
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
