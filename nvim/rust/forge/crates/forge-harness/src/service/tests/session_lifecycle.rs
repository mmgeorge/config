//! Session lifecycle failures must leave every session reachable: a failed deletion, a fork whose
//! announcement fails, a fork interrupted before its task ran, and full initialization after
//! services-only initialization.

use super::super::{prepare_new_session, prepare_provider_fork, start_session_fork};
use super::bootstrap::{initialize, service};
use super::response_for;
use crate::session::{ProviderForkState, SessionPurpose, SessionStore};
use crate::storage::SqliteStore;
use anyhow::{Context, Result};
use forge_protocol::message::Request;
use serde_json::json;
use std::sync::Arc;
use std::time::Duration;

/// A deletion that fails after it stopped the target's input evicts that controller, so the
/// session answers requests again and a retried deletion succeeds.
#[tokio::test]
async fn failed_deletion_evicts_the_stopping_controller() -> Result<()> {
    let fixture = tempfile::tempdir()?;
    let service = service();
    service
        .open_session(1, initialize(&fixture, "mock"))
        .await?;
    let registry = service.registry().await?;
    let source = registry.initial_session_id().to_owned();
    let target = prepare_new_session(
        std::path::Path::new(&registry.initialize.data_root),
        &registry.initialize.client_id,
        "mock",
        false,
        &source,
        &json!({"name":"deletion target"}),
        0,
    )?
    .id;
    let controller = registry.resolve(&target).await?;
    // A subscription that ended in a panic fails deletion after its input is marked stopping.
    let failed_subscription = tokio::spawn(async { panic!("fixture subscription failure") });
    while !failed_subscription.is_finished() {
        tokio::task::yield_now().await;
    }
    *controller.timeout_task.lock().await = Some(failed_subscription);
    drop(controller);

    let (sink, mut output) = forge_protocol::outbound::channel();
    let delete = |id| Request {
        id,
        method: "session.delete".into(),
        params: json!({"session_id": target}),
    };
    let error = service
        .dispatch(Some(source.clone()), delete(2), &sink)
        .await
        .expect_err("the failed subscription fails deletion");
    assert!(
        format!("{error:#}").contains("session subscription failed before deletion"),
        "{error:#}"
    );
    assert!(
        !registry.controller_by_id.read().await.contains_key(&target),
        "a stopping controller must not keep answering for its session"
    );

    service
        .dispatch(Some(source.clone()), delete(3), &sink)
        .await?;
    let deleted = response_for(&mut output, 3).await?;
    assert_eq!(deleted["result"]["deleted"], target.as_str(), "{deleted}");
    service.shutdown(Duration::from_secs(5)).await?;
    Ok(())
}

/// A fork whose response cannot be delivered still runs its native fork, so the child's gate
/// settles instead of holding every later prompt.
#[tokio::test]
async fn undelivered_fork_response_still_settles_the_child() -> Result<()> {
    let fixture = tempfile::tempdir()?;
    let service = service();
    service
        .open_session(1, initialize(&fixture, "mock"))
        .await?;
    let registry = service.registry().await?;
    let source = registry.initial_session_id().to_owned();
    let (closed_sink, output) = forge_protocol::outbound::channel();
    drop(output);

    let forked = start_session_fork(
        Arc::clone(&registry),
        source.clone(),
        json!({}),
        Some(2),
        SessionPurpose::Conversation,
        &closed_sink,
    )
    .await;
    assert!(
        forked.is_err(),
        "the closed output rejects the fork response"
    );
    let mut fork_task = registry.fork_task.lock().await;
    let mut joined = 0;
    while let Some(result) = fork_task.join_next().await {
        result?;
        joined += 1;
    }
    drop(fork_task);
    assert_eq!(joined, 1, "the fork task must start before delivery");
    assert!(registry.provider_fork_by_id.read().await.is_empty());
    let store = SqliteStore::open(std::path::Path::new(&registry.initialize.data_root))?;
    let child = store
        .list_session(None)?
        .into_iter()
        .find(|session| session.id != source)
        .context("forked child missing")?;
    assert!(
        !matches!(
            child.provider_fork_state,
            ProviderForkState::Preparing { .. }
        ),
        "{:?}",
        child.provider_fork_state
    );
    // No service shutdown: after a successful mock native fork, the source and child exchange
    // owners do not finish within the shutdown deadline, independently of this scenario.
    drop(store);
    Ok(())
}

/// A persisted `Preparing` child with no live fork belongs to an interrupted host, and resolving
/// it records the fork as failed instead of leaving the child waiting forever.
#[tokio::test]
async fn resolving_an_interrupted_fork_records_its_failure() -> Result<()> {
    let fixture = tempfile::tempdir()?;
    let service = service();
    service
        .open_session(1, initialize(&fixture, "mock"))
        .await?;
    let registry = service.registry().await?;
    let source = registry.initial_session_id().to_owned();
    let capability = registry.runtime.provider_handle().descriptor().capability;
    let preparation = prepare_provider_fork(
        std::path::Path::new(&registry.initialize.data_root),
        &registry.initialize.client_id,
        registry.initialize.provider.kind.as_str(),
        &capability,
        &source,
        &json!({}),
        SessionPurpose::Conversation,
    )?;

    let child = registry.resolve(&preparation.child.id).await?;
    let state = child.broker.lock().await.provider_fork_state();
    let ProviderForkState::Failed { message, .. } = state else {
        panic!("interrupted fork must fail, found {state:?}");
    };
    assert!(message.contains("interrupted"), "{message}");
    drop(child);
    service.shutdown(Duration::from_secs(5)).await?;
    Ok(())
}

/// Full initialization after services-only initialization adopts a session the registry already
/// owns rather than colliding with this process's lock, and keeps every live controller.
#[tokio::test]
async fn full_initialization_adopts_a_session_owned_by_services() -> Result<()> {
    let fixture = tempfile::tempdir()?;
    let session_id = {
        let earlier = service();
        let opened = earlier
            .open_session(1, initialize(&fixture, "mock"))
            .await?;
        let session_id = opened.result().context("first open failed")?["session"]["id"]
            .as_str()
            .context("session id missing")?
            .to_owned();
        earlier.shutdown(Duration::from_secs(5)).await?;
        session_id
    };

    let service = service();
    let services = service
        .initialize_services(1, initialize(&fixture, "mock"))
        .await?;
    assert!(services.error().is_none(), "{services:?}");
    let registry = service.registry().await?;
    let owned = registry.resolve(&session_id).await?;

    let opened = service
        .open_session(2, initialize(&fixture, "mock"))
        .await?;
    assert!(opened.error().is_none(), "{:?}", opened.error());
    assert_eq!(
        opened.result().context("open result missing")?["session"]["id"],
        session_id.as_str()
    );
    let registry_after = service.registry().await?;
    assert!(Arc::ptr_eq(&registry, &registry_after), "registry replaced");
    assert!(Arc::ptr_eq(&owned, &registry.resolve(&session_id).await?));
    assert_eq!(registry.initial_session_id(), session_id);
    drop(owned);
    service.shutdown(Duration::from_secs(5)).await?;
    Ok(())
}
