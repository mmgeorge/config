use super::super::native_permission;
use super::super::{HarnessBroker, SessionController, prompt};
use super::bootstrap::initialize;
use super::owner_fixture::retain_cleanup_failure;
use anyhow::{Context, Result};
use forge_protocol::message::Request;
use serde_json::json;
use std::sync::Arc;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;
use std::time::Duration;

/// Permanent command-result failure releases observers without dropping native cleanup ownership.
#[tokio::test]
async fn owner_permanent_storage_failure_releases_waiters_and_retains_cleanup() -> Result<()> {
    let root = tempfile::tempdir()?;
    crate::checks::configure_fixture(root.path());
    let invoked = Arc::new(tokio::sync::Notify::new());
    let started = Arc::new(tokio::sync::Notify::new());
    let release = Arc::new(tokio::sync::Semaphore::new(0));
    let collected = Arc::new(AtomicBool::new(false));
    let descriptor = forge_providers::registry::descriptor(
        &forge_providers::registry::nonselectable_id().unwrap(),
    )
    .unwrap();
    let native = crate::provider::test_support::EventFixtureProvider::new(descriptor, {
        let invoked = invoked.clone();
        move |request, _sink, start, mut cancelled| {
            let invoked = invoked.clone();
            async move {
                start.observed(1).await?;
                invoked.notify_one();
                while !*cancelled.borrow_and_update() {
                    cancelled
                        .changed()
                        .await
                        .map_err(|_| crate::provider::ProviderError::Cancelled)?;
                }
                Ok(crate::provider::test_support::native_result(
                    &request,
                    forge_provider_api::turn::TurnOutcome::Interrupted,
                ))
            }
        }
    });
    native.with_cleanup({
        let started = started.clone();
        let release = release.clone();
        let collected = collected.clone();
        move |_| {
            let started = started.clone();
            let release = release.clone();
            let collected = collected.clone();
            async move {
                if !collected.load(Ordering::Acquire) {
                    started.notify_one();
                    release.acquire().await.unwrap().forget();
                    collected.store(true, Ordering::Release);
                }
                Ok(())
            }
        }
    });
    native.with_models(vec![serde_json::from_value(json!({
        "id":"mock-model", "reasoning":["low","medium","high"],
        "context_window":[], "vision":false, "is_default":true,
    }))?]);
    let provider_fixture = crate::provider::test_support::ProviderTestSession::from_native(native)?;
    let mut broker = HarnessBroker::initialize(initialize(&root, "mock"))?;
    broker.set_provider_for_test(provider_fixture.provider_handle());
    let controller = SessionController::new(broker).await?;
    controller.trace.configure_default(true)?;
    let (sink, mut output) = forge_protocol::outbound::channel();
    let reader = tokio::spawn(async move {
        while output.recv().await?.is_some() {}
        Ok::<(), anyhow::Error>(())
    });
    controller.start_provider(&sink).await;
    if let Err(error) = tokio::time::timeout(Duration::from_secs(5), async {
        while !controller.provider_ready().await {
            tokio::task::yield_now().await;
        }
    })
    .await
    {
        return Err(retain_cleanup_failure(
            root,
            &controller,
            anyhow::Error::from(error).context("provider readiness timed out"),
        ));
    }
    controller.attach_input(&sink).await?;
    prompt::apply(
        &controller,
        Request {
            id: 50,
            method: "session.prompt.submit".into(),
            params: json!({"operation_id":"cleanup-live-input","text":"Wait for cleanup"}),
        },
        &sink,
    )
    .await?;
    if let Err(error) = tokio::time::timeout(Duration::from_secs(5), invoked.notified()).await {
        return Err(retain_cleanup_failure(
            root,
            &controller,
            anyhow::Error::from(error).context("fixture invocation never started"),
        ));
    }
    let before = controller.observer.snapshot()?.session.access;
    let command = native_permission::decode_command(&Request {
        id: 51,
        method: "session.command.apply".into(),
        params: json!({"operation_id":"owner-cleanup-timeout","action":"permission","change":{"kind":"toggle_write_enabled"}}),
    })?;
    let mut operation = Box::pin(
        controller
            .exchange_owner
            .execute_identified(command.clone(), sink.clone()),
    );
    let entered = tokio::time::timeout( crate::hang_guard::HANG_GUARD, async {
        tokio::select! {
            result = &mut operation => Err(anyhow::anyhow!("cleanup unexpectedly completed before barrier: {result:?}")),
            () = started.notified() => Ok(()),
        }
    }).await.map_err(anyhow::Error::from).and_then(|result| result);
    if let Err(error) = entered {
        return Err(retain_cleanup_failure(
            root,
            &controller,
            error.context("permission collection did not enter cleanup"),
        ));
    }
    let mut duplicate = Box::pin(
        controller
            .exchange_owner
            .execute_identified(command.clone(), sink.clone()),
    );
    assert!(futures_util::poll!(duplicate.as_mut()).is_pending());
    let mut queued_command = command.clone();
    queued_command.id = crate::exchange::command::CommandId::new("storage-failed-queued")?;
    let mut queued = Box::pin(
        controller
            .exchange_owner
            .execute_identified(queued_command.clone(), sink.clone()),
    );
    assert!(futures_util::poll!(queued.as_mut()).is_pending());
    let database =
        rusqlite::Connection::open(crate::storage::state::database(&controller.data_root))?;
    tokio::time::timeout(crate::hang_guard::HANG_GUARD, async {
        loop {
            let admitted: bool = database.query_row(
                "SELECT EXISTS(SELECT 1 FROM owner_command WHERE id='storage-failed-queued')",
                [],
                |row| row.get(0),
            )?;
            if admitted {
                return Ok::<_, anyhow::Error>(());
            }
            tokio::task::yield_now().await;
        }
    })
    .await??;
    database.execute_batch("CREATE TRIGGER reject_owner_completion_permanently BEFORE UPDATE ON owner_command WHEN json_extract(NEW.state,'$.state')='finished' BEGIN SELECT RAISE(ABORT,'permanent fixture completion failure'); END;")?;
    tokio::time::pause();
    tokio::time::advance(Duration::from_secs(31)).await;
    let results = tokio::time::timeout(Duration::from_secs(1), async {
        tokio::join!(operation, duplicate, queued)
    })
    .await;
    tokio::time::resume();
    let results = results.context("permanent failure left live command waiters unresolved")?;
    for result in [results.0, results.1, results.2] {
        assert!(
            matches!(
                result,
                Err(crate::exchange::command::ExecuteCommandError::OutcomeUncommitted)
            ),
            "{result:?}"
        );
    }
    let rejected = controller
        .exchange_owner
        .execute_identified(queued_command, sink.clone())
        .await;
    assert!(
        matches!(
            rejected,
            Err(crate::exchange::command::ExecuteCommandError::OutcomeUncommitted)
        ),
        "{rejected:?}"
    );
    assert_eq!(controller.observer.snapshot()?.session.access, before);
    assert!(!collected.load(Ordering::Acquire));
    assert_eq!(release.available_permits(), 0);
    release.add_permits(1);
    controller.input_stopping.store(true, Ordering::Release);
    controller.coordinator.submissions.changed.notify_one();
    tokio::time::timeout(crate::hang_guard::HANG_GUARD, async {
        while !collected.load(Ordering::Acquire) {
            tokio::task::yield_now().await;
        }
    })
    .await
    .context("permanent storage failure dropped native cleanup ownership")?;
    assert!(collected.load(Ordering::Acquire));
    let state: String = database.query_row(
        "SELECT json_extract(state,'$.state') FROM owner_command WHERE id='owner-cleanup-timeout'",
        [],
        |row| row.get(0),
    )?;
    assert_eq!(
        state, "active",
        "failed persistence must not manufacture a terminal result"
    );
    let trigger_count: u32 = database.query_row("SELECT count(*) FROM sqlite_master WHERE type='trigger' AND name='reject_owner_completion_permanently'", [], |row| row.get(0))?;
    assert_eq!(
        trigger_count, 1,
        "storage remains failed throughout the regression"
    );
    // Keep the trigger installed: the runtime may retain recovery ownership until
    // the test executor tears down, but no live waiter or native cleanup is lost.
    controller.exchange_owner.close().await;
    reader.abort();
    Ok(())
}

/// A later explicit close retries the retained binding after transient retirement failure.
#[tokio::test]
async fn owner_close_retries_failed_retirement_of_same_binding() -> Result<()> {
    let root = tempfile::tempdir()?;
    crate::checks::configure_fixture(root.path());
    let cleanup_calls = Arc::new(std::sync::atomic::AtomicUsize::new(0));
    let closing_bindings = Arc::new(std::sync::Mutex::new(Vec::new()));
    let retry_allowed = Arc::new(AtomicBool::new(false));
    let descriptor = forge_providers::registry::descriptor(
        &forge_providers::registry::nonselectable_id().unwrap(),
    )
    .unwrap();
    let native = crate::provider::test_support::EventFixtureProvider::new(
        descriptor,
        |request, _, start, _| async move {
            start.observed(1).await?;
            Ok(crate::provider::test_support::native_result(
                &request,
                forge_provider_api::turn::TurnOutcome::Completed {
                    stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
                },
            ))
        },
    );
    native.with_session_close({
        let cleanup_calls = cleanup_calls.clone();
        let closing_bindings = closing_bindings.clone();
        let retry_allowed = retry_allowed.clone();
        move |binding| {
            let cleanup_calls = cleanup_calls.clone();
            let closing_bindings = closing_bindings.clone();
            let retry_allowed = retry_allowed.clone();
            async move {
                closing_bindings.lock().unwrap().push(binding);
                cleanup_calls.fetch_add(1, Ordering::AcqRel);
                if !retry_allowed.load(Ordering::Acquire) {
                    return Err(crate::provider::ProviderError::DeliveryUncertain);
                }
                Ok(())
            }
        }
    });
    native.with_models(vec![serde_json::from_value(json!({
        "id":"mock-model", "reasoning":["low","medium","high"],
        "context_window":[], "vision":false, "is_default":true,
    }))?]);
    let provider_fixture = crate::provider::test_support::ProviderTestSession::from_native(native)?;
    let mut broker = HarnessBroker::initialize(initialize(&root, "mock"))?;
    broker.set_provider_for_test(provider_fixture.provider_handle());
    let controller = SessionController::new(broker).await?;
    controller.trace.configure_default(true)?;
    let (sink, mut output) = forge_protocol::outbound::channel();
    let reader = tokio::spawn(async move {
        while output.recv().await?.is_some() {}
        Ok::<(), anyhow::Error>(())
    });
    controller.start_provider(&sink).await;
    tokio::time::timeout(crate::hang_guard::HANG_GUARD, async {
        while !controller.provider_ready().await {
            tokio::task::yield_now().await;
        }
    })
    .await
    .context("retirement fixture provider did not become ready")?;
    // Startup leaves the native conversation unopened. Open the root binding the
    // way first input does so retirement exercises cleanup rather than an empty slot.
    let open = controller.broker.lock().await.provider_request(
        crate::provider::ProviderInput::from_text(""),
        crate::provider::PromptMode::Chat,
    )?;
    provider_fixture.provider_handle().thread_open(open).await?;
    assert_eq!(cleanup_calls.load(Ordering::Acquire), 0);
    let original_provider = controller.provider_handle();
    let original_inbox = controller.observer.session_events();
    controller.input_stopping.store(true, Ordering::Release);
    controller.exchange_owner.close().await;
    let trace = controller
        .trace
        .session_status(controller.observer.session_id())
        .path;
    let failed_retirement = tokio::time::timeout(crate::hang_guard::HANG_GUARD, async {
        loop {
            let records = std::fs::read_to_string(&trace)?;
            if records.contains("generation_retired")
                && records.contains("exchange.shutdown.collection_failed")
            {
                return Ok::<_, anyhow::Error>(());
            }
            tokio::task::yield_now().await;
        }
    })
    .await
    .context("first retirement did not retain its injected failure")
    .and_then(|result| result);
    if let Err(error) = failed_retirement {
        return Err(retain_cleanup_failure(root, &controller, error));
    }
    assert!(
        !controller
            .exchange_task
            .lock()
            .await
            .as_ref()
            .unwrap()
            .is_finished()
    );
    assert!(Arc::ptr_eq(
        &original_provider,
        &controller.provider_handle()
    ));
    assert!(Arc::ptr_eq(
        &original_inbox,
        &controller.observer.session_events()
    ));
    retry_allowed.store(true, Ordering::Release);
    controller.exchange_owner.close().await;
    let finished = tokio::time::timeout(
        crate::hang_guard::HANG_GUARD,
        controller.finish_exchange_owner(),
    )
    .await
    .context("second explicit close did not finish retained retirement")
    .and_then(|result| result);
    if let Err(error) = finished {
        return Err(retain_cleanup_failure(root, &controller, error));
    }
    assert!(cleanup_calls.load(Ordering::Acquire) >= 2);
    {
        let bindings = closing_bindings.lock().unwrap();
        assert!(bindings.len() >= 2);
        assert!(bindings.iter().all(|binding| binding == &bindings[0]));
    }
    assert!(original_inbox.is_closed());
    assert!(Arc::ptr_eq(
        &original_provider,
        &controller.provider_handle()
    ));
    controller.finish_session_events().await?;
    provider_fixture.provider_handle().shutdown().await?;
    reader.abort();
    Ok(())
}
