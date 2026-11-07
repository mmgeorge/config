use super::super::{CopilotSessions, PreparedSession, SessionReuse, SessionSelection};
use super::support::{RecoveryPeer, recovery_session};
use super::{Host, Trace};
use crate::copilot::session::SessionOptions;
use forge_provider_api::ProviderError;
use std::sync::Arc;
use tokio::sync::Mutex;

/// Interrupt a held native inventory query and explicitly collect its monitor and subscription.
#[tokio::test]
async fn terminal_monitor_shutdown_cancels_pending_query_without_stopping_healthy_client() {
    tokio::time::timeout(std::time::Duration::from_secs(5), async {
        let runtime = crate::copilot::runtime::CopilotRuntime::new(vec!["terminal-peer".into()]);
        let peer = RecoveryPeer::attach(&runtime, "1.0.103").await;
        let client = runtime.client(".").await.unwrap();
        let sessions = Arc::new(CopilotSessions::<tempfile::TempDir>::new(Arc::new(Trace)));
        let owner = forge_provider_api::testing::root_thread("terminal-owner");
        let _session = recovery_session(&sessions, client.clone(), &owner).await;
        let (sink, _events) = forge_provider_api::events::channel(
            forge_provider_api::session::BindingId::new("terminal-fixture").unwrap(),
            forge_provider_api::EventScope::Binding {
                thread_id: owner.clone(),
            },
        );
        sessions
            .observe_terminals(crate::copilot::terminal::TerminalPublication {
                owner: owner.clone(),
                sink,
                registry: Arc::new(tokio::sync::Mutex::new(
                    crate::copilot::terminal::TerminalRegistry::default(),
                )),
                publisher: None,
            })
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
        sessions.retire(&owner.clone()).await.unwrap();
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
        let runtime =
            crate::copilot::runtime::CopilotRuntime::new(vec!["configuration-peer".into()]);
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
                        execution_owner: forge_provider_api::testing::root_thread("config-owner"),

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
                .contains_key(&forge_provider_api::testing::root_thread("config-owner"))
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

/// Propagate shared transport loss to idle and active owners before retained, cancellable recovery.
#[tokio::test]
async fn failed_shared_generation_collects_all_sessions_before_revalidating_replacement() {
    tokio::time::timeout(std::time::Duration::from_secs(10), async {
        let runtime = crate::copilot::runtime::CopilotRuntime::new(vec!["recovery-peer".into()]);
        let old = RecoveryPeer::attach(&runtime, "1.0.103").await;
        let client = runtime.client(".").await.unwrap();
        let sessions = Arc::new(CopilotSessions::<tempfile::TempDir>::new(Arc::new(Trace)));
        let mut owned = Vec::new();
        for id in ["idle-owner", "active-owner", "catalog-owner"] {
            let owner = forge_provider_api::testing::root_thread(id);
            let (sink, stream) = forge_provider_api::events::channel(
                forge_provider_api::BindingId::new("recovery-owner").unwrap(),
                forge_provider_api::EventScope::Binding {
                    thread_id: owner.clone(),
                },
            );
            if id != "catalog-owner" {
                sessions.subscribe(owner.clone(), sink).await;
            }
            let session = recovery_session(&sessions, client.clone(), &owner).await;
            owned.push((owner, session, stream));
        }
        let scope = forge_provider_api::InvocationScope::new(owned[1].0.clone(), "active-exchange");
        let (sink, _active_events) = forge_provider_api::events::channel(
            forge_provider_api::BindingId::new("recovery-active").unwrap(),
            scope.clone(),
        );
        let context = Arc::new(Mutex::new(crate::copilot::intake::TurnContext {
            decoder: crate::copilot::event_decoder::CopilotEventDecoder::new(scope, ".".into()),
            output: Default::default(),
            sink: sink.activity_writer(),
        }));
        let mut active = owned[1].1.events.attach(context).await.unwrap();
        owned[1].1.inner.send("one accepted input").await.unwrap();
        old.stop.send(()).unwrap();
        old.task.await.unwrap();
        for (owner, _, stream) in &mut owned {
            if *owner == forge_provider_api::testing::root_thread("catalog-owner") {
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
        let slot = sessions.slot(&owned[0].0.clone()).await;
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
                    &forge_provider_api::testing::root_thread("late-old-binding"),
                    client.generation.clone(),
                    forge_provider_api::testing::root_thread("late-old-binding")
                )
                .await
                .is_err()
        );
        assert!(
            sessions.generations.lock().await.is_empty(),
            "a collected generation admitted a late binding"
        );
        for (owner, _, _) in &owned {
            assert!(sessions.active(&owner.clone()).await.is_none());
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
        let new_owner = forge_provider_api::testing::root_thread("new-owner");
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
