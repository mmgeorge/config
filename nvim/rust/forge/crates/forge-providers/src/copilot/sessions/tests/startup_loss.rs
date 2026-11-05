use super::super::{CopilotSessions, PreparedSession, SessionReuse, SessionSelection};
use super::support::recovery_frame;
use super::{Host, read_frame, reply_frame};
use crate::copilot::session::SessionOptions;
use forge_provider_api::ProviderError;
use forge_provider_api::trace::ProviderTrace;
use std::sync::Arc;
use tokio::io::AsyncWriteExt;

/// Observe producer failure independently of the startup RPC held by the peer.
struct StartupOverflowTrace {}

impl ProviderTrace for StartupOverflowTrace {
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

/// Fence startup overflow before create/resume responds and collect every retained startup owner.
#[tokio::test]
async fn startup_overflow_fails_generation_before_create_or_resume_response() {
    tokio::time::timeout(std::time::Duration::from_secs(20), async {
        for _ in 0..10 {
            for resumed in [false, true] {
                let runtime =
                    crate::copilot::runtime::CopilotRuntime::new(vec!["startup-peer".into()]);
                let (client_write, mut peer_read) = tokio::io::duplex(1 << 20);
                let (mut peer_write, client_read) = tokio::io::duplex(1 << 20);
                runtime
                    .attach_streams(client_read, client_write, std::env::temp_dir())
                    .await
                    .unwrap();
                let methods = Arc::new(std::sync::Mutex::new(Vec::new()));
                let recorded = methods.clone();
                let peer = tokio::spawn(async move {
                    let status = read_frame(&mut peer_read).await;
                    assert_eq!(status["method"], "status.get");
                    recorded.lock().unwrap().push("status.get".to_owned());
                    reply_frame(
                        &mut peer_write,
                        &status,
                        serde_json::json!({"version":"1.0.103"}),
                    )
                    .await;
                    let discovery = read_frame(&mut peer_read).await;
                    assert_eq!(discovery["method"], "mcp.discover");
                    recorded.lock().unwrap().push("mcp.discover".to_owned());
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
                    // Overflow the configured startup window before its native response.
                    // Every intake stage must preserve the same typed loss and collection.
                    for sequence in 0..=forge_protocol::MAX_PENDING_FRAMES {
                        startup_notification(&mut peer_write, native, sequence).await;
                    }
                    // Deliberately never send the create/resume response. Fencing must close
                    // this transport and release its startup waiter without help from the peer.
                    while let Some(request) = recovery_frame(&mut peer_read).await {
                        let method = request["method"].as_str().unwrap();
                        assert_eq!(
                            method, "$/cancelRequest",
                            "failed startup admitted another native operation"
                        );
                        recorded.lock().unwrap().push(method.to_owned());
                    }
                });
                let client = runtime.client(".").await.unwrap();
                let observed_generation = client.generation.clone();
                let trace = Arc::new(StartupOverflowTrace {});
                let sessions = Arc::new(CopilotSessions::<tempfile::TempDir>::new(trace.clone()));
                let owner = forge_provider_api::ExecutionOwner::root("startup-overflow");
                let (sink, mut source) = forge_provider_api::events::channel(
                    forge_provider_api::BindingId::new("startup-loss").unwrap(),
                    forge_provider_api::EventScope::Binding {
                        owner: owner.clone(),
                    },
                );
                sessions.subscribe(owner.clone(), sink).await;
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

                                correlation_id: owner.session_id.to_string(),
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
                                        resources:
                                            crate::copilot::session::SessionResources::default(),
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

                // Producer failure is observable while the peer has supplied no startup result.
                assert!(matches!(
                    source.recv().await,
                    Err(forge_provider_api::events::EventDeliveryError::Observation(
                        forge_provider_api::lifecycle::ObservationFailure::Overflow
                    ))
                ));
                assert_eq!(
                    observed_generation.failure(),
                    Some(forge_provider_api::lifecycle::ObservationFailure::Overflow)
                );
                let error = match startup.await.unwrap() {
                    Err(error) => error,
                    Ok(_) => panic!("startup overflow published a usable session"),
                };
                assert!(
                    matches!(
                        error.downcast_ref::<ProviderError>(),
                        Some(ProviderError::Observation(
                            forge_provider_api::lifecycle::ObservationFailure::Overflow
                        ))
                    ),
                    "startup replaced its primary observation loss: {error:#}"
                );
                peer.await.unwrap();
                assert!(sessions.startups.lock().unwrap().is_empty());
                assert!(sessions.provisional.lock().unwrap().is_empty());
                assert!(sessions.leases.lock().unwrap().is_empty());
                assert!(
                    sessions.failed_startups.lock().unwrap().is_empty(),
                    "joined observation loss became a cleanup failure"
                );
                assert!(
                    !profile_path.exists(),
                    "collected startup retained its profile"
                );

                // The same recovery proofs used in production collect the failed generation.
                let failed = runtime.failed_generation().await.unwrap();
                let collected = runtime.collect_failed_client(&failed).await.unwrap();
                let proof = sessions
                    .collect_failed_generation(&collected)
                    .await
                    .unwrap();
                assert!(sessions.generations.lock().await.is_empty());
                assert!(sessions.active(&key).await.is_none());
                runtime.finish_recovery(proof).await.unwrap();
                runtime.shutdown().await.unwrap();
                let recorded = methods.lock().unwrap();
                assert_eq!(
                    recorded
                        .iter()
                        .filter(|method| method.as_str()
                            == if resumed {
                                "session.resume"
                            } else {
                                "session.create"
                            })
                        .count(),
                    1
                );
                assert!(!recorded.iter().any(|method| matches!(
                    method.as_str(),
                    "session.send" | "session.sendAndWait"
                )));
            }
        }
    })
    .await
    .expect("startup overflow did not release and collect its held native startup");
}
