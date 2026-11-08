use super::super::CopilotSessions;
use super::Trace;
use super::support::{RecoveryPeer, recovery_session};
use std::sync::Arc;

/// Interrupt a held native inventory query and explicitly collect its monitor and subscription.
#[tokio::test]
async fn terminal_monitor_shutdown_cancels_pending_query_without_stopping_healthy_client() {
    async {
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
        sessions.close_observers().await.unwrap();
        assert!(sessions.terminals.lock().await.is_empty());
        client.ensure_healthy().unwrap();
        sessions.retire(&owner.clone()).await.unwrap();
        runtime.shutdown().await.unwrap();
        let _ = peer.stop.send(());
        peer.task.await.unwrap();
    }
    .await;
}
