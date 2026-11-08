use super::*;

/// Replacing the current invocation must never reset an older worker's cancellation.
#[tokio::test]
async fn cancellation_signal_remains_bound_to_its_invocation() {
    let controller = TurnCancellation::new();
    let first = controller.capture();
    controller.request();
    controller.arm();
    let second = controller.capture();
    assert!(first.requested.load(Ordering::Acquire));
    assert!(first.submission_cancelled.load(Ordering::Acquire));
    assert!(!second.requested.load(Ordering::Acquire));
    first.request();
    assert!(!second.requested.load(Ordering::Acquire));
    controller.request();
    assert!(second.requested.load(Ordering::Acquire));
    first.cancelled().await;
    second.cancelled().await;
}
use std::sync::atomic::AtomicUsize;

#[tokio::test]
async fn prepared_foreground_collection_prevents_native_submission() -> Result<()> {
    let workspace = super::test::repository();
    let data = tempfile::tempdir()?;
    let mut broker = super::test::planning_question_broker(workspace.path(), data.path(), false);
    broker.checkpoint = None;
    broker.turn_cancellation.arm();
    let (publication, _updates) = publication::channel();
    let ForegroundDispatch::Admitting(admission) = broker.begin_foreground_dispatch(
        Request {
            id: 1,
            method: "session.prompt.submit".into(),
            params: json!({"text":"Prepared before cancellation"}),
        },
        publication,
    )?
    else {
        anyhow::bail!("fixture did not create foreground admission");
    };
    let mut staged = broker.stage_foreground_interaction(
        "Prepared before cancellation",
        PromptMode::Chat,
        Some(&ExchangeAdmission::chat(
            "Prepared before cancellation".into(),
        )),
        ProviderInput::from_text("Prepared before cancellation"),
    )?;
    broker.begin_exchange_runtime(&mut staged.exchange, broker.clock.now_ms())?;
    let engine = broker
        .execution_registry()
        .owner(crate::provider::ThreadOwner::root(&broker.session.id))
        .await?;
    let mut transaction = broker.construct_foreground_transaction(&staged, engine)?;
    assert!(
        transaction.native.task.is_empty(),
        "prepared admission started native work before owner authorization"
    );
    let calls = Arc::new(AtomicUsize::new(0));
    let invoked = calls.clone();
    transaction.native.pending_submission = Some(Box::pin(async move {
        invoked.fetch_add(1, Ordering::SeqCst);
        std::future::pending::<Result<ProviderAttempt>>().await
    }));
    let mut dispatch = ForegroundDispatch::Running {
        transaction: Box::new(transaction),
        admission,
    };
    broker.step_foreground(
        &mut dispatch,
        ForegroundSignal::CollectionStarted(ForegroundCollection::Cancel(
            crate::exchange::command::CancelledInputPolicy::Retain,
        )),
    )?;
    // The ordinary phase can reopen after the command settles; this collected generation remains fenced.
    dispatch.authorize_submission();
    tokio::task::yield_now().await;
    assert_eq!(calls.load(Ordering::SeqCst), 0);
    Ok(())
}
