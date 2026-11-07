use super::super::{CopilotSessions, PreparedSession, SessionReuse, SessionSelection};
use super::support::recovery_frame;
use super::{Host, read_frame, reply_frame};
use crate::copilot::session::SessionOptions;
use forge_provider_api::trace::ProviderTrace;
use std::sync::Arc;
use tokio::io::AsyncWriteExt;

/// Keep native startup diagnostics separate from its queued event retention.
struct StartupTrace {}

impl ProviderTrace for StartupTrace {
    fn record(&self, _: forge_provider_api::trace::ProviderTraceRecord) {}
}

/// Publish one valid SDK notification without replying to its pending session startup.
async fn startup_notification(
    writer: &mut tokio::io::DuplexStream,
    session: &str,
    sequence: usize,
) {
    let event = serde_json::json!({"jsonrpc":"2.0","method":"session.event","params":{
        "sessionId":session,"event":{"id":format!("startup-{sequence}"),"type":"assistant.message",
        "timestamp":"2026-11-02T00:00:00Z","data":{"messageId":format!("message-{sequence}"),"content":"early startup content"}}
    }});
    let body = serde_json::to_vec(&event).unwrap();
    writer
        .write_all(format!("Content-Length: {}\r\n\r\n", body.len()).as_bytes())
        .await
        .unwrap();
    writer.write_all(&body).await.unwrap();
    writer.flush().await.unwrap();
}

/// Retain startup bursts without fabricating observation loss before native create/resume returns.
#[tokio::test]
async fn startup_burst_is_retained_until_create_or_resume_response() {
    tokio::time::timeout(std::time::Duration::from_secs(10), async {
        for resumed in [false, true] {
            let runtime = crate::copilot::runtime::CopilotRuntime::new(vec!["startup-peer".into()]);
            let (client_write, mut peer_read) = tokio::io::duplex(1 << 20);
            let (mut peer_write, client_read) = tokio::io::duplex(1 << 20);
            runtime
                .attach_streams(client_read, client_write, std::env::temp_dir())
                .await
                .unwrap();
            let (sent, burst_sent) = tokio::sync::oneshot::channel();
            let (release, released) = tokio::sync::oneshot::channel();
            let methods = Arc::new(std::sync::Mutex::new(Vec::new()));
            let recorded = methods.clone();
            let peer = tokio::spawn(async move {
                let status = read_frame(&mut peer_read).await;
                assert_eq!(status["method"], "status.get");
                reply_frame(
                    &mut peer_write,
                    &status,
                    serde_json::json!({"version":"1.0.103"}),
                )
                .await;
                let discovery = read_frame(&mut peer_read).await;
                assert_eq!(discovery["method"], "mcp.discover");
                reply_frame(
                    &mut peer_write,
                    &discovery,
                    serde_json::json!({"servers":[]}),
                )
                .await;
                let create = read_frame(&mut peer_read).await;
                let method = create["method"].as_str().unwrap();
                assert_eq!(
                    method,
                    if resumed {
                        "session.resume"
                    } else {
                        "session.create"
                    }
                );
                recorded.lock().unwrap().push(method.to_owned());
                let native = create["params"]["sessionId"].as_str().unwrap();
                for sequence in 0..=forge_protocol::MAX_PENDING_FRAMES {
                    startup_notification(&mut peer_write, native, sequence).await;
                }
                sent.send(()).unwrap();
                released.await.unwrap();
                reply_frame(
                    &mut peer_write,
                    &create,
                    serde_json::json!({"sessionId":native,"workspacePath":std::env::temp_dir()}),
                )
                .await;
                while let Some(request) = recovery_frame(&mut peer_read).await {
                    let method = request["method"].as_str().unwrap();
                    assert!(
                        matches!(method, "session.detach" | "session.skills.reload" | "session.eventLog.read" | "$/cancelRequest"),
                        "unexpected startup fixture operation: {method}"
                    );
                    recorded.lock().unwrap().push(method.to_owned());
                    if request.get("id").is_some() {
                        let response = match method {
                            "session.eventLog.read" => serde_json::json!({"cursor":"anchor","cursorStatus":"ok","hasMore":false,"events":[]}),
                            "session.skills.reload" => serde_json::json!({}),
                            "session.detach" | "$/cancelRequest" => serde_json::json!({"success":true}),
                            _ => unreachable!("fixture operation was explicitly checked"),
                        };
                        reply_frame(&mut peer_write, &request, response).await;
                    }
                }
            });
            let client = runtime.client(".").await.unwrap();
            let generation = client.generation.clone();
            let trace = Arc::new(StartupTrace {});
            let sessions = Arc::new(CopilotSessions::<tempfile::TempDir>::new(trace));
            let owner = forge_provider_api::testing::root_thread("startup-burst");
            let (sink, mut source) = forge_provider_api::events::channel(
                forge_provider_api::BindingId::new("startup-burst").unwrap(),
                forge_provider_api::EventScope::Binding {
                    thread_id: owner.clone(),
                },
            );
            sessions.subscribe(owner.clone(), sink.clone()).await;
            let profile = tempfile::tempdir().unwrap();
            let profile_path = profile.path().to_owned();
            let acquiring = sessions.clone();
            let key = owner.clone();
            let startup = tokio::spawn(async move {
                acquiring
                    .acquire(
                        async move { Ok((client, None)) },
                        SessionSelection {
                            execution_owner: owner.clone(),
                            correlation_id: owner.to_string(),
                            session_id: resumed.then(|| "native-startup-resume".to_owned()),
                            model: None,
                            context_tier: None,
                            tools: None,
                            reuse: SessionReuse::Compatible,
                        },
                        Arc::new(Host),
                        Arc::new(Host),
                        || async move {
                            Ok(PreparedSession {
                                options: SessionOptions {
                                    isolated_text: false,
                                    session_id: None,
                                    model: None,
                                    reasoning_effort: None,
                                    context_tier: None,
                                    workspace: std::env::temp_dir(),
                                    system_message: String::new(),
                                    resources: crate::copilot::session::SessionResources::default(),
                                    excluded_tools: Vec::new(),
                                    tools: Vec::new(),
                                    native_callbacks: None,
                                },
                                skills: None,
                                mcp: None,
                                profile: Some(profile),
                            })
                        },
                        |_| Ok(()),
                    )
                    .await
            });
            burst_sent.await.unwrap();
            assert!(
                !startup.is_finished(),
                "native startup returned before its held response"
            );
            assert_eq!(generation.failure(), None);
            assert!(
                source.try_next().unwrap().is_none(),
                "queued native content manufactured public lifecycle"
            );
            release.send(()).unwrap();
            let session = startup.await.unwrap().unwrap();
            let scope = forge_provider_api::InvocationScope::new(key.clone(), "startup-observer");
            let context = Arc::new(tokio::sync::Mutex::new(
                crate::copilot::intake::TurnContext {
                    decoder: crate::copilot::event_decoder::CopilotEventDecoder::new(
                        scope.clone(),
                        ".".into(),
                    ),
                    output: forge_provider_api::completion::ProviderCompletion::default(),
                    sink: sink.activity_writer().invocation_writer(&scope).unwrap(),
                },
            ));
            let mut retained = session.events.attach(context).await.unwrap();
            for sequence in 0..=forge_protocol::MAX_PENDING_FRAMES {
                assert_eq!(
                    retained.recv().await.unwrap().id,
                    format!("startup-{sequence}")
                );
            }
            drop(retained);
            assert_eq!(generation.failure(), None);
            assert!(sessions.startups.lock().unwrap().is_empty());
            assert!(sessions.provisional.lock().unwrap().is_empty());
            drop(session);
            sessions.retire(&key).await.unwrap();
            runtime.shutdown().await.unwrap();
            peer.await.unwrap();
            assert!(sessions.leases.lock().unwrap().is_empty());
            assert!(
                !profile_path.exists(),
                "native startup resource was not collected"
            );
            assert!(
                !methods.lock().unwrap().iter().any(|method| matches!(
                    method.as_str(),
                    "session.send" | "session.sendAndWait"
                ))
            );
        }
    })
    .await
    .expect("startup burst was not retained and collected");
}
