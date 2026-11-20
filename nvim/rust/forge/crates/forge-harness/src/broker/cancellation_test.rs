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
    let TurnTransaction::Admitting(admission) = broker.begin_foreground_dispatch(
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
        ProviderInput::from_user_text("Prepared before cancellation"),
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
    let mut dispatch = TurnTransaction::Running {
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

/// An admission the owner started collecting must settle when that collection fails.
///
/// The admission's failure cleanup waits for the owner's collection. A failed collection has to
/// release that wait, or the turn never ends and the provider restart behind it never starts.
#[tokio::test]
async fn failed_owner_collection_releases_a_collected_admission() -> Result<()> {
    let workspace = super::test::repository();
    let data = tempfile::tempdir()?;
    let mut broker = super::test::planning_question_broker(workspace.path(), data.path(), false);
    let (publication, _updates) = publication::channel();
    let mut dispatch = broker.begin_foreground_dispatch(
        Request {
            id: 1,
            method: "session.prompt.submit".into(),
            params: json!({"text":"Admitted before the failed stop"}),
        },
        publication,
    )?;
    assert!(matches!(dispatch, TurnTransaction::Admitting(_)));
    let started = broker.step_foreground(
        &mut dispatch,
        ForegroundSignal::CollectionStarted(ForegroundCollection::Cancel(
            crate::exchange::command::CancelledInputPolicy::RestoreIfUnconsumed,
        )),
    )?;
    assert!(matches!(started, ForegroundAction::Wait));
    let failed = broker.step_foreground(&mut dispatch, ForegroundSignal::CollectionFailed)?;
    assert!(
        matches!(failed, ForegroundAction::WaitForPublicationDrain),
        "a failed collection did not end the admission it was collecting"
    );
    let drained = broker.step_foreground(&mut dispatch, ForegroundSignal::PublicationDrained)?;
    assert!(
        !matches!(drained, ForegroundAction::Wait),
        "the admission still waits for a collection that already failed"
    );
    Ok(())
}

/// A collection signal that reaches an exchange its finalization already settled leaves no stop
/// label behind.
///
/// A cancel's acquisition interrupts the children and waits for them before it announces the
/// collection, while their completions already let the parent finalize. The late announcement
/// then labelled the finished exchange `Stopping`, and the heading kept that label for good.
#[tokio::test]
async fn late_collection_signal_leaves_a_settled_exchange_unlabelled() -> Result<()> {
    let workspace = super::test::repository();
    let data = tempfile::tempdir()?;
    let mut broker = super::test::planning_question_broker(workspace.path(), data.path(), false);
    broker.checkpoint = None;
    let (publication, _updates) = publication::channel();
    let TurnTransaction::Admitting(admission) = broker.begin_foreground_dispatch(
        Request {
            id: 1,
            method: "session.prompt.submit".into(),
            params: json!({"text":"Settled before the stop announced itself"}),
        },
        publication,
    )?
    else {
        anyhow::bail!("fixture did not create foreground admission");
    };
    let mut staged = broker.stage_foreground_interaction(
        "Settled before the stop announced itself",
        PromptMode::Chat,
        Some(&ExchangeAdmission::chat(
            "Settled before the stop announced itself".into(),
        )),
        ProviderInput::from_user_text("Settled before the stop announced itself"),
    )?;
    broker.begin_exchange_runtime(&mut staged.exchange, broker.clock.now_ms())?;
    let engine = broker
        .execution_registry()
        .owner(crate::provider::ThreadOwner::root(&broker.session.id))
        .await?;
    let mut transaction = broker.construct_foreground_transaction(&staged, engine)?;
    transaction.state.interaction.finish(
        crate::exchange::ExchangeOutcome::Cancelled,
        broker.clock.now_ms(),
    )?;
    broker.store.save_exchange(&transaction.state.interaction)?;
    let exchange_id = transaction.state.interaction.id.clone();
    let mut dispatch = TurnTransaction::Running {
        transaction: Box::new(transaction),
        admission,
    };
    broker.step_foreground(
        &mut dispatch,
        ForegroundSignal::CollectionStarted(ForegroundCollection::Cancel(
            crate::exchange::command::CancelledInputPolicy::Retain,
        )),
    )?;
    let stored = broker
        .store
        .load_exchange(&exchange_id)?
        .context("the settled exchange disappeared")?;
    assert_eq!(stored.state(), crate::exchange::ExchangeState::Cancelled);
    assert_eq!(
        stored.collection, None,
        "the settled exchange kept a stop label"
    );
    Ok(())
}
