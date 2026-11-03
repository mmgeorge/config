/// Own one real SDK transport and record every native request until transport loss.
struct RecoveryPeer {
    stop: tokio::sync::oneshot::Sender<()>,
    methods: Arc<std::sync::Mutex<Vec<String>>>,
    task: tokio::task::JoinHandle<()>,
}

/// Distinguish actual transport EOF from a malformed frame during recovery.
async fn recovery_frame(reader: &mut tokio::io::DuplexStream) -> Option<Value> {
    let mut header = Vec::new();
    loop {
        match reader.read_u8().await {
            Ok(byte) => header.push(byte),
            Err(error)
                if header.is_empty() && error.kind() == std::io::ErrorKind::UnexpectedEof =>
            {
                return None;
            }
            Err(error) => panic!("recovery frame failed: {error}"),
        }
        if header.ends_with(b"\r\n\r\n") {
            break;
        }
        assert!(header.len() < 1024);
    }
    let size: usize = std::str::from_utf8(&header)
        .unwrap()
        .trim()
        .strip_prefix("Content-Length: ")
        .unwrap()
        .parse()
        .unwrap();
    assert!(size < 1 << 20);
    let mut body = vec![0; size];
    reader.read_exact(&mut body).await.unwrap();
    Some(serde_json::from_slice(&body).unwrap())
}

impl RecoveryPeer {
    /// Attach a fresh peer to the same retained runtime without launching an external CLI.
    async fn attach(
        runtime: &super::super::runtime::CopilotRuntime,
        version: &'static str,
    ) -> Self {
        let (client_write, mut peer_read) = tokio::io::duplex(1 << 20);
        let (mut peer_write, client_read) = tokio::io::duplex(1 << 20);
        runtime
            .attach_streams(client_read, client_write, std::env::temp_dir())
            .await
            .unwrap();
        let (stop, mut stopped) = tokio::sync::oneshot::channel();
        let methods = Arc::new(std::sync::Mutex::new(Vec::new()));
        let observed = methods.clone();
        let task = tokio::spawn(async move {
            loop {
                let request = tokio::select! {
                    _ = &mut stopped => break,
                    request = recovery_frame(&mut peer_read) => match request { Some(request) => request, None => break },
                };
                let method = request["method"].as_str().unwrap();
                observed.lock().unwrap().push(method.to_owned());
                let result = match method {
                    "status.get" => serde_json::json!({"version":version}),
                    "session.create" => {
                        serde_json::json!({"sessionId":request["params"]["sessionId"],"workspacePath":std::env::temp_dir()})
                    }
                    "session.options.update" | "session.detach" => {
                        serde_json::json!({"success":true})
                    }
                    "session.send" => serde_json::json!({"messageId":"accepted-old-input"}),
                    "session.tasks.refresh" | "session.mcp.listConfigured" => continue,
                    other => panic!("unexpected recovery fixture RPC: {other}"),
                };
                reply_frame(&mut peer_write, &request, result).await;
            }
        });
        Self {
            stop,
            methods,
            task,
        }
    }
}

/// Interrupt a held native inventory query and explicitly collect its monitor and subscription.
#[tokio::test]
async fn terminal_monitor_shutdown_cancels_pending_query_without_stopping_healthy_client() {
    tokio::time::timeout(std::time::Duration::from_secs(5), async {
        let runtime = super::super::runtime::CopilotRuntime::new(vec!["terminal-peer".into()]);
        let peer = RecoveryPeer::attach(&runtime, "1.0.103").await;
        let client = runtime.client(".").await.unwrap();
        let sessions = Arc::new(CopilotSessions::<tempfile::TempDir>::new(Arc::new(Trace)));
        let owner = forge_provider_api::ExecutionOwner::root("terminal-owner");
        let _session = recovery_session(&sessions, client.clone(), &owner).await;
        sessions
            .terminals(&owner.runtime_key(), "terminal-owner")
            .await
            .unwrap();
        while !peer
            .methods
            .lock()
            .unwrap()
            .iter()
            .any(|method| method == "session.tasks.refresh")
        {
            tokio::task::yield_now().await;
        }
        tokio::time::timeout(
            std::time::Duration::from_millis(500),
            sessions.close_observers(),
        )
        .await
        .unwrap()
        .unwrap();
        assert!(sessions.terminals.lock().await.is_empty());
        client.ensure_healthy().unwrap();
        sessions.retire(&owner.runtime_key()).await.unwrap();
        runtime.shutdown().await.unwrap();
        let _ = peer.stop.send(());
        peer.task.await.unwrap();
    })
    .await
    .expect("pending terminal query collection fixture timed out");
}

/// Collect a created session and lease when transport loss interrupts post-start MCP preparation.
#[tokio::test]
async fn failed_mcp_configuration_collects_provisional_session_without_dead_detach() {
    tokio::time::timeout(std::time::Duration::from_secs(5), async {
        let runtime = super::super::runtime::CopilotRuntime::new(vec!["configuration-peer".into()]);
        let peer = RecoveryPeer::attach(&runtime, "1.0.103").await;
        let client = runtime.client(".").await.unwrap();
        let sessions = Arc::new(CopilotSessions::<tempfile::TempDir>::new(Arc::new(Trace)));
        let profile = tempfile::tempdir().unwrap();
        let profile_path = profile.path().to_owned();
        let registry = sessions.clone();
        let preparing = tokio::spawn(async move {
            registry
                .acquire(
                    async move { Ok((client, None)) },
                    SessionSelection {
                        execution_owner: forge_provider_api::ExecutionOwner::root("config-owner"),
                        runtime_key: "config-owner".into(),
                        correlation_id: "config-owner".into(),
                        session_id: None,
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
                                session_id: None,
                                model: None,
                                reasoning_effort: None,
                                context_tier: None,
                                workspace: std::env::temp_dir(),
                                mode: forge_provider_api::PromptMode::Chat,
                                system_message: String::new(),
                                resources: super::super::session::ResourceDiscovery::Native,
                                excluded_tools: Vec::new(),
                                tools: Vec::new(),
                            },
                            skills: None,
                            mcp: Some(Vec::new()),
                            profile: Some(profile),
                        })
                    },
                    |_| Ok(()),
                )
                .await
        });
        while !peer
            .methods
            .lock()
            .unwrap()
            .iter()
            .any(|method| method == "session.mcp.listConfigured")
        {
            tokio::task::yield_now().await;
        }
        assert!(
            sessions.startups.lock().unwrap().is_empty(),
            "post-start configuration still owns its startup waiter"
        );
        assert!(
            sessions
                .provisional
                .lock()
                .unwrap()
                .contains_key("config-owner")
        );
        assert!(profile_path.exists());
        peer.stop.send(()).unwrap();
        peer.task.await.unwrap();
        let error = match preparing.await.unwrap() {
            Err(error) => error,
            Ok(_) => panic!("held MCP preparation unexpectedly succeeded"),
        };
        assert!(
            matches!(
                error.downcast_ref::<ProviderError>(),
                Some(ProviderError::PreparationCleanup { .. })
            ),
            "{error:#}"
        );
        let failed = runtime.failed_generation().await.unwrap();
        let client = runtime.collect_failed_client(&failed).await.unwrap();
        let proof = sessions.collect_failed_generation(&client).await.unwrap();
        assert!(sessions.provisional.lock().unwrap().is_empty());
        assert!(sessions.leases.lock().unwrap().is_empty());
        assert!(
            !profile_path.exists(),
            "collected provisional session retained its resource lease"
        );
        assert!(
            !peer
                .methods
                .lock()
                .unwrap()
                .iter()
                .any(|method| method == "session.send")
        );
        runtime.finish_recovery(proof).await.unwrap();
        runtime.shutdown().await.unwrap();
    })
    .await
    .expect("failed MCP configuration collection fixture timed out");
}

/// Prepare a real native session while retaining a disposable resource profile.
async fn recovery_session(
    sessions: &Arc<CopilotSessions<tempfile::TempDir>>,
    client: CopilotClient,
    owner: &forge_provider_api::ExecutionOwner,
) -> Arc<CopilotSession> {
    sessions
        .acquire(
            async move { Ok((client, None)) },
            SessionSelection {
                execution_owner: owner.clone(),
                runtime_key: owner.runtime_key(),
                correlation_id: owner.session_id.to_string(),
                session_id: None,
                model: None,
                context_tier: None,
                tools: None,
                reuse: SessionReuse::Compatible,
            },
            Arc::new(Host),
            Arc::new(Host),
            || async {
                Ok(PreparedSession {
                    options: SessionOptions {
                        session_id: None,
                        model: None,
                        reasoning_effort: None,
                        context_tier: None,
                        workspace: std::env::temp_dir(),
                        mode: forge_provider_api::PromptMode::Chat,
                        system_message: String::new(),
                        resources: super::super::session::ResourceDiscovery::Native,
                        excluded_tools: Vec::new(),
                        tools: Vec::new(),
                    },
                    skills: None,
                    mcp: None,
                    profile: Some(tempfile::tempdir().unwrap()),
                })
            },
            |_| Ok(()),
        )
        .await
        .unwrap()
}

/// Propagate shared transport loss to idle and active owners before retained, cancellable recovery.
#[tokio::test]
async fn failed_shared_generation_collects_all_sessions_before_revalidating_replacement() {
    tokio::time::timeout(std::time::Duration::from_secs(10), async {
        let runtime = super::super::runtime::CopilotRuntime::new(vec!["recovery-peer".into()]);
        let old = RecoveryPeer::attach(&runtime, "1.0.103").await;
        let client = runtime.client(".").await.unwrap();
        let sessions = Arc::new(CopilotSessions::<tempfile::TempDir>::new(Arc::new(Trace)));
        let mut owned = Vec::new();
        for id in ["idle-owner", "active-owner", "catalog-owner"] {
            let owner = forge_provider_api::ExecutionOwner::root(id);
            let (sink, stream) = forge_provider_api::events::channel(
                forge_provider_api::EventScope::SessionOperation {
                    owner: owner.clone(),
                    operation_id: "generation-observation".into(),
                },
            );
            if id != "catalog-owner" {
                sessions.subscribe(owner.runtime_key(), sink).await;
            }
            let session = recovery_session(&sessions, client.clone(), &owner).await;
            owned.push((owner, session, stream));
        }
        let scope = forge_provider_api::InvocationScope::new(owned[1].0.clone(), "active-exchange");
        let (sink, _active_events) = forge_provider_api::events::channel(scope.clone());
        let context = Arc::new(Mutex::new(super::super::intake::TurnContext {
            decoder: super::super::event_decoder::CopilotEventDecoder::new(scope, ".".into()),
            output: Default::default(),
            sink,
        }));
        let mut active = owned[1].1.events.attach(context).await.unwrap();
        owned[1].1.inner.send("one accepted input").await.unwrap();
        old.stop.send(()).unwrap();
        old.task.await.unwrap();
        for (owner, _, stream) in &mut owned {
            if owner.session_id.as_str() == "catalog-owner" {
                continue;
            }
            loop {
                match stream.recv().await {
                    Err(forge_provider_api::events::EventDeliveryError::Observation(
                        forge_provider_api::lifecycle::ObservationFailure::Disconnected,
                    )) => break,
                    Ok(Some(_)) => {}
                    result => {
                        panic!("shared source did not retain typed transport loss: {result:?}")
                    }
                }
            }
        }
        assert!(matches!(
            active.recv().await,
            Err(ProviderError::Observation(
                forge_provider_api::lifecycle::ObservationFailure::Disconnected
            ))
        ));
        let failed = runtime.failed_generation().await.unwrap();
        assert_eq!(
            sessions.fail_generation(&failed).await.unwrap().len(),
            3,
            "callback-only bindings were omitted from client-wide fencing"
        );
        assert!(
            runtime.client(".").await.is_err(),
            "failed cached client was reused"
        );
        let collected = runtime.collect_failed_client(&failed).await.unwrap();
        let slot = sessions.slot(&owned[0].0.runtime_key()).await;
        let hold = slot.lock().await;
        let waiting = sessions.clone();
        let proof = collected.clone();
        let waiter = tokio::spawn(async move { waiting.collect_failed_generation(&proof).await });
        loop {
            if sessions.collections.try_lock().is_err() {
                break;
            }
            tokio::task::yield_now().await;
        }
        waiter.abort();
        match waiter.await {
            Err(error) => assert!(error.is_cancelled()),
            Ok(_) => panic!("cancelled session collection waiter unexpectedly completed"),
        }
        drop(hold);
        let proof = sessions
            .collect_failed_generation(&collected)
            .await
            .unwrap();
        assert!(sessions.startups.lock().unwrap().is_empty());
        assert!(sessions.leases.lock().unwrap().is_empty());
        assert!(sessions.generations.lock().await.is_empty());
        assert!(
            sessions
                .bind_generation(
                    "late-old-binding",
                    client.generation.clone(),
                    forge_provider_api::ExecutionOwner::root("late-old-binding")
                )
                .await
                .is_err()
        );
        assert!(
            sessions.generations.lock().await.is_empty(),
            "a collected generation admitted a late binding"
        );
        for (owner, _, _) in &owned {
            assert!(sessions.active(&owner.runtime_key()).await.is_none());
        }
        runtime.finish_recovery(proof).await.unwrap();
        let rejected = RecoveryPeer::attach(&runtime, "1.0.102").await;
        assert!(matches!(
            runtime.client(".").await,
            Err(ProviderError::RuntimeVersion(_))
        ));
        let _ = rejected.stop.send(());
        rejected.task.await.unwrap();
        assert_eq!(*rejected.methods.lock().unwrap(), ["status.get"]);
        let replacement = RecoveryPeer::attach(&runtime, "1.0.104").await;
        let new_client = runtime.client(".").await.unwrap();
        client.fail_generation(forge_provider_api::lifecycle::ObservationFailure::Overflow);
        new_client.ensure_healthy().unwrap();
        let new_owner = forge_provider_api::ExecutionOwner::root("new-owner");
        let session = recovery_session(&sessions, new_client, &new_owner).await;
        assert_eq!(
            replacement
                .methods
                .lock()
                .unwrap()
                .first()
                .map(String::as_str),
            Some("status.get")
        );
        assert!(
            !replacement
                .methods
                .lock()
                .unwrap()
                .iter()
                .any(|method| method == "session.send"),
            "old input was replayed"
        );
        session.disconnect().await.unwrap();
        runtime.shutdown().await.unwrap();
        let _ = replacement.stop.send(());
        replacement.task.await.unwrap();
        assert_eq!(
            old.methods
                .lock()
                .unwrap()
                .iter()
                .filter(|method| method.as_str() == "session.send")
                .count(),
            1
        );
    })
    .await
    .expect("shared generation recovery fixture timed out");
}
