use super::super::native_permission;
use super::super::{HarnessBroker, SessionController, prompt};
use super::bootstrap::initialize;
use anyhow::Result;
use forge_protocol::message::Request;
use serde_json::json;
use std::sync::Arc;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;
use std::time::Duration;

/// Database validation failures reach the initialization response with their concrete cause.
#[tokio::test]
async fn initialization_reports_incomplete_current_schema() -> Result<()> {
    let root = tempfile::tempdir()?;
    let settings = initialize(&root, "mock");
    let store = crate::storage::SqliteStore::open(&root.path().join("data"))?;
    let database = crate::storage::test::raw_connection(&root.path().join("data"))?;
    database.execute_batch("DROP TABLE terminal_request")?;
    let service = super::bootstrap::service();
    let response = service.open_session(60, settings).await?;
    let error = response
        .error()
        .expect("initialization must reject incomplete schema");
    assert_eq!(error.code, "initialize_failed");
    assert!(
        error
            .message
            .contains("missing required tables: terminal_request"),
        "{}",
        error.message
    );
    drop(store);
    Ok(())
}

/// Recovery failures remain visible and reject prompts without adding undeliverable input.
#[tokio::test]
async fn owner_startup_failure_reports_cause_and_rejects_prompt() -> Result<()> {
    let root = tempfile::tempdir()?;
    crate::checks::configure_fixture(root.path());
    let broker = HarnessBroker::initialize(initialize(&root, "mock"))?;
    let session = broker.snapshot()?.session.id;
    let database = crate::storage::test::raw_connection(&broker.data_root)?;
    // The schema passes opening validation; corrupt recovery content exercises the later boundary.
    database.execute(
        "INSERT INTO terminal_request(session_id,id,intent,disposition) VALUES(?1,'broken','{}','invalid-json')",
        [&session],
    )?;
    let controller = SessionController::new(broker).await?;
    let (sink, mut output) = forge_protocol::outbound::channel();
    controller.attach_input(&sink).await?;
    let activity = tokio::time::timeout(Duration::from_secs(5), async {
        loop {
            let activity = controller.observer.snapshot()?.activity;
            if activity.phase == crate::exchange::ActivityPhase::Unavailable {
                return Ok::<_, anyhow::Error>(activity);
            }
            tokio::task::yield_now().await;
        }
    })
    .await??;
    let reason = activity.unavailable_reason.as_deref().unwrap();
    assert!(reason.contains("Recover terminal requests"), "{reason}");
    assert!(reason.contains("malformed JSON"), "{reason}");
    prompt::apply(
        &controller,
        Request {
            id: 61,
            method: "session.prompt.submit".into(),
            params: json!({"operation_id":"unavailable-input","text":"keep my draft"}),
        },
        &sink,
    )
    .await?;
    let response: serde_json::Value = tokio::time::timeout(Duration::from_secs(5), async {
        loop {
            let frame = output.recv().await?.expect("prompt response");
            let value: serde_json::Value = serde_json::from_slice(frame.bytes())?;
            if value["id"] == 61 {
                return Ok::<_, anyhow::Error>(value);
            }
        }
    })
    .await??;
    assert_eq!(
        response["error"]["code"], "session_unavailable",
        "{response}"
    );
    assert!(
        response["error"]["message"]
            .as_str()
            .unwrap()
            .contains(reason),
        "{response}"
    );
    assert!(
        controller
            .coordinator
            .submissions
            .snapshot()?
            .messages
            .is_empty()
    );
    controller.exchange_owner.close().await;
    Ok(())
}

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
    controller
        .trace
        .configure_default(crate::trace::LogLevel::Trace)?;
    let (sink, mut output) = forge_protocol::outbound::channel();
    let reader = tokio::spawn(async move {
        while output.recv().await?.is_some() {}
        Ok::<(), anyhow::Error>(())
    });
    controller.start_provider(&sink).await;
    while !controller.provider_ready().await {
        tokio::task::yield_now().await;
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
    invoked.notified().await;
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
    tokio::select! {
        result = &mut operation => anyhow::bail!("cleanup unexpectedly completed before barrier: {result:?}"),
        () = started.notified() => {}
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
    let database = crate::storage::test::raw_connection(&controller.data_root)?;
    async {
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
    }
    .await?;
    database.execute_batch("CREATE TRIGGER reject_owner_completion_permanently BEFORE UPDATE ON owner_command WHEN json_extract(NEW.state,'$.state')='finished' BEGIN SELECT RAISE(ABORT,'permanent fixture completion failure'); END;")?;
    tokio::time::pause();
    tokio::time::advance(Duration::from_secs(31)).await;
    let results = tokio::join!(operation, duplicate, queued);
    tokio::time::resume();
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
    async {
        while !collected.load(Ordering::Acquire) {
            tokio::task::yield_now().await;
        }
    }
    .await;
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
