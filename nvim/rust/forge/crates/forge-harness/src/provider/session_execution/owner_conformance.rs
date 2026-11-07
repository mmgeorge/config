//! Exercise real host intake with independently controlled native start, delivery, and completion.
use super::*;
use crate::provider::submission::{SubmissionState, SubmissionStore};
use crate::provider::test_support::{EventFixtureProvider, ProviderTestSession};
use crate::provider::{PromptMode, ProviderInput, SteeringInput};
use anyhow::{Context, Result};
use forge_provider_api::provider::Provider as NativeProvider;
use forge_provider_api::turn::{TurnCompletionReason, TurnOutcome};
use forge_provider_api::{
    AdapterActivity, AdapterActivityWriter, CoveredInput, NativeTurnStart, PromptRequest,
    ProviderFailure, ProviderFailureKind, ProviderOperation, TurnResult,
};
use std::sync::atomic::AtomicUsize;

type Coverage = Arc<Mutex<Vec<CoveredInput>>>;

/// Control actual native work independently of its already persisted acceptance receipt.
struct NativeCall {
    request: PromptRequest,
    writer: AdapterActivityWriter,
    start: NativeTurnStart,
    started: Arc<AtomicBool>,
    coverage: Coverage,
    finish: Option<oneshot::Sender<Result<TurnResult, ProviderError>>>,
}
impl NativeCall {
    /// Establish positive work on the exact host-admitted turn.
    async fn started(&self) -> Result<()> {
        self.start.observed(1).await?;
        self.started.store(true, Ordering::Release);
        Ok(())
    }
    /// Supply one immutable native outcome with exact accepted input attempts.
    fn finish(&mut self, outcome: TurnOutcome) -> Result<()> {
        let result = TurnResult::new(
            outcome,
            2,
            None,
            self.coverage.lock().unwrap().clone(),
            vec![],
            None,
        )?;
        self.finish
            .take()
            .context("native result was already supplied")?
            .send(Ok(result))
            .map_err(|_| anyhow::anyhow!("native worker no longer owns the result"))
    }
}

/// Hold steering acceptance independently of the native turn's terminal result.
struct SteeringCall {
    request: forge_provider_api::SteeringRequest,
    coverage: Coverage,
    reply: oneshot::Sender<Result<(), ProviderError>>,
}
impl SteeringCall {
    /// Persist native acceptance before releasing the separately held operation waiter.
    async fn accept(&self) -> Result<()> {
        let receipt = self
            .request
            .delivery
            .receipt()
            .context("expected journaled steering")?
            .clone();
        let input = CoveredInput::new(receipt.id().clone(), receipt.attempt_id()?);
        receipt.record_accepted(None).await?;
        let mut coverage = self.coverage.lock().unwrap();
        if !coverage.contains(&input) {
            coverage.push(input);
        }
        Ok(())
    }
    /// Resolve the actual steering operation after the test's independent native evidence.
    fn acknowledge(self, result: Result<(), ProviderError>) -> Result<()> {
        self.reply
            .send(result)
            .map_err(|_| anyhow::anyhow!("steering waiter was collected"))
    }
}

/// Retain real adapter and host owners while tests independently control native evidence.
struct Fixture {
    directory: tempfile::TempDir,
    native: Arc<EventFixtureProvider>,
    session: ProviderTestSession,
    registry: SessionExecutionRegistry,
    store: Arc<SubmissionStore>,
    calls: mpsc::UnboundedReceiver<NativeCall>,
    steering: mpsc::UnboundedReceiver<SteeringCall>,
    dispatch_count: Arc<AtomicUsize>,
}
impl Fixture {
    /// Create the production bridge and retained runner with explicit native controls.
    fn new() -> Result<Self> {
        let directory = tempfile::tempdir()?;
        let trace = Arc::new(crate::trace::TraceStore::open(directory.path())?);
        let store = SubmissionStore::open(directory.path(), trace.clone())?;
        let (calls_tx, calls) = mpsc::unbounded_channel();
        let (steer_tx, steering) = mpsc::unbounded_channel();
        type CoverageByScope =
            Arc<Mutex<HashMap<InvocationScope, (Coverage, tokio::sync::watch::Receiver<bool>)>>>;
        let coverage_by_scope: CoverageByScope = Arc::default();
        let coverage_for_prompt = coverage_by_scope.clone();
        let dispatch_count = Arc::new(AtomicUsize::new(0));
        let dispatches = dispatch_count.clone();
        let mut descriptor = forge_providers::registry::descriptor(
            &forge_providers::registry::nonselectable_id().context("fixture identity missing")?,
        )?;
        descriptor.capability.native_steer = true;
        let native = EventFixtureProvider::new(
            descriptor,
            move |request, writer, start, mut cancelled| {
                let sender = calls_tx.clone();
                let scopes = coverage_for_prompt.clone();
                let dispatches = dispatches.clone();
                async move {
                    dispatches.fetch_add(1, Ordering::SeqCst);
                    let coverage = Arc::new(Mutex::new(request.input_coverage()));
                    scopes
                        .lock()
                        .unwrap()
                        .insert(request.scope.clone(), (coverage.clone(), cancelled.clone()));
                    let started = Arc::new(AtomicBool::new(false));
                    let (finish, response) = oneshot::channel();
                    sender
                        .send(NativeCall {
                            request: request.clone(),
                            writer,
                            start,
                            started: started.clone(),
                            coverage: coverage.clone(),
                            finish: Some(finish),
                        })
                        .map_err(|_| ProviderError::Cancelled)?;
                    tokio::select! {
                        result = response => result.unwrap_or(Err(ProviderError::Cancelled)),
                        _ = cancelled.changed() => {
                            if !started.load(Ordering::Acquire) { return Err(ProviderError::Cancelled); }
                            Ok(TurnResult::new(TurnOutcome::Interrupted, 2, None, coverage.lock().unwrap().clone(), vec![], None).unwrap())
                        }
                    }
                }
            },
        );
        native.with_steering(move |request| {
            let sender = steer_tx.clone();
            let coverage = coverage_by_scope
                .lock()
                .unwrap()
                .get(&request.scope)
                .cloned();
            async move {
                let (coverage, mut cancelled) = coverage.ok_or(ProviderError::NotConsumed)?;
                let (reply, response) = oneshot::channel();
                sender
                    .send(SteeringCall {
                        request,
                        coverage,
                        reply,
                    })
                    .map_err(|_| ProviderError::Cancelled)?;
                if *cancelled.borrow() {
                    return Err(ProviderError::Cancelled);
                }
                tokio::select! {
                    result = response => result.unwrap_or(Err(ProviderError::Cancelled)),
                    _ = cancelled.changed() => Err(ProviderError::Cancelled),
                }
            }
        });
        let session = ProviderTestSession::from_native(native.clone())?;
        let registry = SessionExecutionRegistry::new(session.provider_handle(), trace);
        Ok(Self {
            directory,
            native,
            session,
            registry,
            store,
            calls,
            steering,
            dispatch_count,
        })
    }
    /// Dispatch an explicit original request and keep its publication stream independently owned.
    async fn run(
        &self,
        owner: &ThreadOwner,
        exchange: &str,
    ) -> Result<(
        Arc<SessionExecution>,
        InvocationScope,
        crate::provider::events::HarnessEventStream,
        JoinHandle<Result<NativeReady, ProviderError>>,
    )> {
        let engine = self.registry.owner(owner.clone()).await?;
        let scope = InvocationScope::new(owner.clone(), exchange);
        let (sink, stream) = crate::provider::events::channel(scope.clone());
        let request = request(owner, self.directory.path());
        let running = engine.clone();
        let store = self.store.clone();
        let task =
            tokio::spawn(async move { running.run_until_ready(request, sink, &store).await });
        Ok((engine, scope, stream, task))
    }
    /// Collect original owners through the same shutdown sequence used by production callers.
    async fn close(self) -> Result<()> {
        tokio::time::timeout(
            std::time::Duration::from_secs(5),
            self.registry.begin_orderly_shutdown(),
        )
        .await
        .context("fixture registry shutdown fence did not settle")??;
        tokio::time::timeout(
            std::time::Duration::from_secs(5),
            self.session.as_provider().shutdown(),
        )
        .await
        .context("fixture native shutdown did not settle")??;
        tokio::time::timeout(std::time::Duration::from_secs(5), self.registry.close())
            .await
            .context("fixture execution owners did not collect")??;
        Ok(())
    }
}

/// Match root, managed-child, and isolated Background ownership.
fn owners() -> [ThreadOwner; 3] {
    [
        ThreadOwner::root("conversation"),
        ThreadOwner::new(
            "conversation".into(),
            forge_provider_api::AgentId::new("managed-child").unwrap(),
        )
        .unwrap(),
        ThreadOwner::root("background-session"),
    ]
}
/// Prepare input using the same durable journal and exact owner as production.
fn request(owner: &ThreadOwner, directory: &std::path::Path) -> ProviderRequest {
    ProviderRequest {
        dispatch: Default::default(),
        execution_owner: Some(owner.clone()),
        harness_session_id: owner.session_id().to_string(),
        workspace: directory.to_string_lossy().into_owned(),
        input: ProviderInput::from_text("inspect"),
        mode: if owner.session_id().as_str() == "background-session" {
            PromptMode::Background
        } else {
            PromptMode::Chat
        },
        model: "mock".into(),
        effort: "low".into(),
        context_window: None,
        service_tier: Default::default(),
        access: Default::default(),
        approval_mode: crate::session::ApprovalMode::Ask,
        provider_session_id: None,
        submission: None,
        acknowledged_child_reports: Vec::new(),
        recovered_input: vec![],
        control_context: None,
    }
}
/// Prepare steering with real journal authority rather than a fabricated input identity.
fn steering(fixture: &Fixture, owner: &ThreadOwner) -> Result<SteeringInput> {
    let catalog = crate::provider::ProviderCatalogRequest {
        execution_owner: Some(owner.clone()),
        harness_session_id: owner.session_id().to_string(),
        workspace: fixture.directory.path().to_string_lossy().into_owned(),
        provider_session_id: None,
        access: Default::default(),
        approval_mode: crate::session::ApprovalMode::Ask,
    };
    Ok(SteeringInput::User {
        text: "continue".into(),
        attachments: vec![],
        submission: Some(fixture.store.prepare_steering(
            &catalog,
            ProviderInput::from_text("continue"),
            forge_providers::registry::nonselectable_id().unwrap(),
        )?),
    })
}
/// Await positive start or sole completion after production intake has reduced it.
async fn boundary(
    stream: &mut crate::provider::events::HarnessEventStream,
    completed: bool,
) -> Result<()> {
    loop {
        let event = stream
            .recv()
            .await?
            .context("owner observation ended before boundary")?;
        if let OwnerObservation::Event(HarnessEvent::Native {
            event: boxed_native_event,
            ..
        }) = event
            && let NativeEvent::Turn { event, .. } = boxed_native_event.as_ref()
        {
            match event {
                TurnEvent::Started(_) if !completed => return Ok(()),
                TurnEvent::Completed(_) if completed => return Ok(()),
                TurnEvent::Started(_)
                | TurnEvent::Completed(_)
                | TurnEvent::Error { .. }
                | TurnEvent::ModelResolved(_)
                | TurnEvent::Usage(_)
                | TurnEvent::ChecklistUpdated(_) => {}
            }
        }
    }
}
/// Supply normal native completion without claiming the host task has succeeded.
fn normal() -> TurnOutcome {
    TurnOutcome::Completed {
        stop_reason: TurnCompletionReason::Normal,
    }
}

/// Repeated immutable operations observe their original result even after admission expires.
#[tokio::test(start_paused = true)]
async fn controlled_operation_replays_after_expiry_and_rejects_changed_identity() -> Result<()> {
    let fixture = Fixture::new()?;
    let (source, cancellation) = forge_provider_api::OperationCancellation::channel();
    let context = forge_provider_api::OperationContext::new(
        forge_provider_api::OperationId::new("inspect")?,
        std::time::Duration::from_millis(1),
        cancellation,
    )?;
    let workspace = forge_provider_api::WorkspaceContext {
        workspace: "original".into(),
    };
    let first = fixture
        .native
        .inspect(context.clone(), workspace.clone())
        .await?;
    tokio::time::advance(std::time::Duration::from_millis(2)).await;
    source.cancel();
    let replay = fixture.native.inspect(context.clone(), workspace).await?;
    assert_eq!(first.capabilities.provider, replay.capabilities.provider);
    let error = fixture
        .native
        .inspect(
            context.clone(),
            forge_provider_api::WorkspaceContext {
                workspace: "changed".into(),
            },
        )
        .await
        .unwrap_err();
    assert!(
        matches!(error, ProviderError::Failure(failure) if failure.kind() == ProviderFailureKind::OperationConflict)
    );
    assert!(
        matches!(fixture.native.thread_close(context, &forge_provider_api::BindingId::new("other")?).await,
        Err(ProviderError::Failure(failure)) if failure.kind() == ProviderFailureKind::OperationConflict)
    );
    fixture.close().await
}

/// Reserved preparation cancellation releases its receipt and cannot clear a newer cancellation fence.
#[tokio::test]
async fn reserved_preparation_cancel_releases_receipt_and_stale_resume_preserves_successor_fence()
-> Result<()> {
    let fixture = Fixture::new()?;
    for owner in owners() {
        let engine = fixture.registry.owner(owner.clone()).await?;
        let scope = InvocationScope::new(owner.clone(), "reserved");
        let receipt = fixture.store.prepare(
            &request(&owner, fixture.directory.path()),
            engine.provider_id(),
        )?;
        let weak = Arc::downgrade(&receipt);
        engine.reserve_invocation(scope.clone(), receipt.clone())?;
        let collection = engine.collect_exchange(scope.clone()).await?;
        assert_eq!(receipt.record()?.delivery, SubmissionState::Cancelled);
        drop(receipt);
        assert!(weak.upgrade().is_none());
        engine.resume_exchange(collection.clone()).await?;
        engine.cancel_exchange(scope.clone()).await?;
        assert!(engine.resume_exchange(collection.clone()).await.is_err());
        let fresh = engine.collect_exchange(scope.clone()).await?;
        engine.resume_exchange(fresh).await?;
        let next = InvocationScope::new(owner.clone(), "reserved");
        let mut input = request(&owner, fixture.directory.path());
        input.input = ProviderInput::from_text("new instruction");
        let receipt = fixture.store.prepare(&input, engine.provider_id())?;
        engine.reserve_invocation(next.clone(), receipt.clone())?;
        engine.cancel_exchange(next).await?;
        assert!(engine.resume_exchange(collection).await.is_err());
        assert_eq!(receipt.record()?.delivery, SubmissionState::Cancelled);
    }
    assert_eq!(fixture.dispatch_count.load(Ordering::SeqCst), 0);
    fixture.close().await
}

/// Fresh continuations retain the exchange while stale cancellation cannot interrupt their turn.
#[tokio::test]
async fn ordinary_successor_has_fresh_scope_and_stale_cancel_cannot_stop_it() -> Result<()> {
    let mut fixture = Fixture::new()?;
    for owner in owners() {
        let (engine, scope, mut events, first) = fixture.run(&owner, "successor").await?;
        let mut call = fixture
            .calls
            .recv()
            .await
            .context("first dispatch missing")?;
        call.started().await?;
        call.finish(normal())?;
        boundary(&mut events, true).await?;
        let ready = first.await??;
        let running = engine.clone();
        let store = fixture.store.clone();
        let next = request(&owner, fixture.directory.path());
        let (sink, _next_events) = crate::provider::events::channel(scope.clone());
        let successor = tokio::spawn(async move {
            running
                .continue_attempt(&ready.ticket, next, sink, &store)
                .await
        });
        let mut next = fixture.calls.recv().await.context("successor missing")?;
        assert_ne!(
            call.request.scope.invocation_id,
            next.request.scope.invocation_id
        );
        assert_eq!(
            call.request.scope.exchange_id,
            next.request.scope.exchange_id
        );
        next.started().await?;
        engine.cancel(scope).await?;
        assert!(!successor.is_finished());
        next.finish(normal())?;
        let ContinuationAttempt::Ready(ready) = successor.await?? else {
            anyhow::bail!("successor was stale");
        };
        ready.outcome?;
        ready.ticket.commit().await?;
    }
    fixture.close().await
}

/// Cancellation collects provider preparation without dispatching native input.
#[tokio::test]
async fn prepared_cancellation_collects_native_admission_without_dispatch() -> Result<()> {
    let fixture = Fixture::new()?;
    let entered = Arc::new(tokio::sync::Semaphore::new(0));
    let hook = entered.clone();
    fixture.native.with_pre_dispatch(move |_| {
        let entered = hook.clone();
        async move {
            entered.add_permits(1);
            std::future::pending::<Result<(), ProviderError>>().await
        }
    });
    for owner in owners() {
        let (engine, scope, _events, result) = fixture.run(&owner, "preparing").await?;
        crate::provider::test_support::wait_gate(&entered).await?;
        engine.cancel(scope).await?;
        assert!(matches!(result.await?, Err(ProviderError::Cancelled)));
    }
    assert_eq!(fixture.dispatch_count.load(Ordering::SeqCst), 0);
    fixture.close().await
}

/// Definite predispatch rejection permits explicit owner reuse without replaying an accepted input.
#[tokio::test]
async fn predispatch_rejection_collects_native_scope_before_owner_reuse() -> Result<()> {
    let mut fixture = Fixture::new()?;
    let attempts = Arc::new(AtomicUsize::new(0));
    let hook = attempts.clone();
    fixture.native.with_pre_dispatch(move |_| {
        let first = hook.fetch_add(1, Ordering::SeqCst) % 2 == 0;
        async move {
            if first {
                Err(ProviderFailure::new(
                    ProviderOperation::TurnStart,
                    ProviderFailureKind::Rejected,
                )
                .into())
            } else {
                Ok(())
            }
        }
    });
    for owner in owners() {
        let (_, _, _, result) = fixture.run(&owner, "rejected").await?;
        let error = result
            .await?
            .err()
            .context("predispatch rejection returned readiness")?;
        assert!(
            matches!(error, ProviderError::PreparationFailed(_)),
            "{error:?}"
        );
        let (_, _, _, result) = fixture.run(&owner, "explicit-successor").await?;
        let mut call = fixture
            .calls
            .recv()
            .await
            .context("explicit successor missing")?;
        call.started().await?;
        call.finish(normal())?;
        let ready = result.await??;
        ready.outcome?;
        ready.ticket.commit().await?;
    }
    assert_eq!(fixture.dispatch_count.load(Ordering::SeqCst), 3);
    fixture.close().await
}

/// Ordered native output survives traffic above queue capacity and an interrupting successor.
#[tokio::test]
async fn foreground_backpressure_preserves_burst_cancel_and_successor_order() -> Result<()> {
    let mut fixture = Fixture::new()?;
    for owner in owners() {
        let (engine, scope, mut events, result) = fixture.run(&owner, "burst").await?;
        let call = fixture
            .calls
            .recv()
            .await
            .context("burst dispatch missing")?;
        call.started().await?;
        let writer = call.writer.clone();
        let thread_id = call.request.thread_id.clone();
        let turn_id = call.request.turn_id.clone();
        let count = forge_protocol::MAX_PENDING_FRAMES * 3;
        let producer = tokio::spawn(async move {
            writer
                .send_wait(AdapterActivity::Item {
                    thread_id: thread_id.clone(),
                    turn_id: turn_id.clone(),
                    item_id: forge_provider_api::ItemId::new("message").unwrap(),
                    event: forge_provider_api::message::ItemEvent::Started(
                        forge_provider_api::message::ItemStart::AgentMessage(
                            forge_provider_api::message::AgentMessageStart {
                                text: String::new(),
                                classification:
                                    forge_provider_api::MessageClassification::Unclassified,
                            },
                        ),
                    ),
                })
                .await?;
            for index in 0..count {
                writer
                    .send_wait(AdapterActivity::Item {
                        thread_id: thread_id.clone(),
                        turn_id: turn_id.clone(),
                        item_id: forge_provider_api::ItemId::new("message").unwrap(),
                        event: forge_provider_api::message::ItemEvent::Message {
                            event: forge_provider_api::message::MessageItemEvent::Delta(format!(
                                "{index}:α🦀\n"
                            )),
                        },
                    })
                    .await?;
            }
            Ok::<_, ProviderError>(())
        });
        let mut output = String::new();
        while output.lines().count() < count {
            if let Some(OwnerObservation::Event(HarnessEvent::Native {
                event: boxed_native_event,
                ..
            })) = events.recv().await?
                && let NativeEvent::Item {
                    event:
                        forge_provider_api::message::ItemEvent::Message {
                            event: forge_provider_api::message::MessageItemEvent::Delta(text),
                        },
                    ..
                } = boxed_native_event.as_ref()
            {
                output.push_str(text);
            }
        }
        producer.await??;
        assert_eq!(
            output,
            (0..count)
                .map(|index| format!("{index}:α🦀\n"))
                .collect::<String>()
        );
        engine.cancel(scope).await?;
        let ready = result.await??;
        assert!(matches!(ready.outcome, Err(ProviderError::Cancelled)));
        ready.ticket.commit().await?;
        let (_, _, _, successor) = fixture.run(&owner, "after-burst").await?;
        let mut next = fixture.calls.recv().await.context("successor missing")?;
        next.started().await?;
        next.finish(normal())?;
        let ready = successor.await??;
        ready.ticket.commit().await?;
    }
    fixture.close().await
}

/// A retired normalized turn cannot restart, and a foreign turn cannot steal active ownership.
#[tokio::test]
async fn committed_old_owner_execution_conflicts_with_new_active_exchange_without_losing_attribution()
-> Result<()> {
    let mut fixture = Fixture::new()?;
    for owner in owners() {
        let (_, _, _, first) = fixture.run(&owner, "retired").await?;
        let mut old = fixture.calls.recv().await.context("old dispatch missing")?;
        old.started().await?;
        old.finish(normal())?;
        first.await??.ticket.commit().await?;
        let (engine, scope, mut events, second) = fixture.run(&owner, "active").await?;
        let mut active = fixture
            .calls
            .recv()
            .await
            .context("active dispatch missing")?;
        active.started().await?;
        let wrong_turn = forge_provider_api::TurnId::new("unregistered-turn")?;
        assert!(
            old.writer
                .send_wait(AdapterActivity::Item {
                    thread_id: old.request.thread_id.clone(),
                    turn_id: wrong_turn,
                    item_id: forge_provider_api::ItemId::new("foreign")?,
                    event: forge_provider_api::message::ItemEvent::Message {
                        event: forge_provider_api::message::MessageItemEvent::Delta(
                            "foreign".into()
                        )
                    }
                })
                .await
                .is_err()
        );
        assert_ne!(old.request.scope, scope);
        active.finish(normal())?;
        assert!(second.await?.is_err());
        assert!(!engine.is_healthy().await);
        while let Ok(Some(observation)) = events.try_next() {
            assert!(!matches!(observation,
                OwnerObservation::Event(HarnessEvent::Native { event, .. })
                if matches!(event.as_ref(), NativeEvent::Item { item_id, .. }
                    if item_id.as_str() == "foreign")));
        }
    }
    let error = fixture.close().await.unwrap_err();
    assert!(
        error.chain().any(|cause| matches!(
            cause.downcast_ref::<ObservationFailure>(),
            Some(ObservationFailure::OwnershipConflict)
        )),
        "unexpected retirement failure: {error:#}"
    );
    Ok(())
}

/// Failed native collection cannot manufacture completion or a collected no-start certificate.
#[tokio::test]
async fn intentional_retirement_rejects_accepted_input_without_terminal_proof() -> Result<()> {
    for owner in owners() {
        let mut fixture = Fixture::new()?;
        fixture.native.with_cleanup(|_| async {
            Err(
                ProviderFailure::new(ProviderOperation::TurnStart, ProviderFailureKind::Runtime)
                    .at_phase(
                        forge_provider_api::ProviderPhase::Collection,
                        forge_provider_api::FailureDisposition::Uncertain,
                    )
                    .into(),
            )
        });
        let (engine, _, _, result) = fixture.run(&owner, "accepted-without-proof").await?;
        let call = fixture
            .calls
            .recv()
            .await
            .context("accepted dispatch missing")?;
        let journal = fixture
            .store
            .submission(call.request.submission_id.as_str())?;
        assert!(
            engine
                .close_with_reason(OwnerCloseReason::Cancelled)
                .await
                .is_err()
        );
        assert!(result.await?.is_err());
        assert!(matches!(
            journal.record()?.delivery,
            SubmissionState::Accepted { .. }
        ));
        assert_eq!(fixture.dispatch_count.load(Ordering::SeqCst), 1);
        drop(call);
        // Retry actual resource collection while retaining the historical failed native disposition.
        fixture.native.with_cleanup(|_| async { Ok(()) });
        engine.close().await?;
        fixture.close().await?;
    }
    Ok(())
}

/// A terminal observed before steering acknowledgment preserves rejection or delivery uncertainty.
#[tokio::test]
async fn completion_racing_steering_preserves_proven_rejection_and_delivery_uncertainty()
-> Result<()> {
    for uncertain in [false, true] {
        let mut fixture = Fixture::new()?;
        for owner in owners() {
            let (engine, scope, mut events, result) = fixture.run(&owner, "racing").await?;
            let mut call = fixture.calls.recv().await.context("dispatch missing")?;
            call.started().await?;
            let input = steering(&fixture, &owner)?;
            let receipt = input.submission().unwrap();
            let running = engine.clone();
            let selected = scope.clone();
            let steer = tokio::spawn(async move { running.steer(selected, input).await });
            let pending = fixture.steering.recv().await.context("steer missing")?;
            call.finish(normal())?;
            boundary(&mut events, true).await?;
            assert!(!result.is_finished());
            let failure =
                ProviderFailure::new(ProviderOperation::TurnSteer, ProviderFailureKind::Rejected)
                    .at_phase(
                        if uncertain {
                            forge_provider_api::ProviderPhase::Observation
                        } else {
                            forge_provider_api::ProviderPhase::Admission
                        },
                        if uncertain {
                            forge_provider_api::FailureDisposition::Uncertain
                        } else {
                            forge_provider_api::FailureDisposition::Unchanged
                        },
                    );
            pending.acknowledge(Err(failure.into()))?;
            assert!(steer.await?.is_err());
            if uncertain {
                assert!(result.await?.is_err());
                assert!(!engine.is_healthy().await);
            } else {
                let ready = result.await??;
                ready.outcome?;
                ready.ticket.commit().await?;
                assert_eq!(receipt.record()?.delivery, SubmissionState::Prepared);
            }
        }
        fixture.close().await?;
    }
    Ok(())
}

/// Sole completion covers accepted steering while the separately retained operation acknowledgment is pending.
#[tokio::test]
async fn every_owner_retains_pending_steering_and_idle_observation() -> Result<()> {
    let mut fixture = Fixture::new()?;
    for owner in owners() {
        let (engine, scope, mut events, result) = fixture.run(&owner, "steered").await?;
        let mut call = fixture.calls.recv().await.context("dispatch missing")?;
        call.started().await?;
        let input = steering(&fixture, &owner)?;
        let running = engine.clone();
        let selected = scope.clone();
        let steer = tokio::spawn(async move { running.steer(selected, input).await });
        let pending = fixture.steering.recv().await.context("steering missing")?;
        pending.accept().await?;
        call.finish(normal())?;
        boundary(&mut events, true).await?;
        assert!(
            !result.is_finished(),
            "completion bypassed held operation acknowledgment"
        );
        pending.acknowledge(Ok(()))?;
        steer.await??;
        result.await??.ticket.commit().await?;
        assert!(
            engine
                .steer(scope, steering(&fixture, &owner)?)
                .await
                .is_err()
        );
        assert!(fixture.calls.try_recv().is_err());
    }
    assert_eq!(fixture.dispatch_count.load(Ordering::SeqCst), 3);
    fixture.close().await
}

/// Loss or cancellation of one owner cannot settle peer owners or replay their inputs.
#[tokio::test]
async fn every_owner_isolates_pending_control_cancellation_and_loss() -> Result<()> {
    for target in 0..3 {
        for lost in [false, true] {
            let mut fixture = Fixture::new()?;
            let mut active = Vec::new();
            for owner in owners() {
                let (engine, scope, _, result) = fixture.run(&owner, "isolated").await?;
                let call = fixture.calls.recv().await.context("dispatch missing")?;
                call.started().await?;
                active.push((owner, engine, scope, result, call));
            }
            let (owner, engine, scope, _, call) = &active[target];
            let input = steering(&fixture, owner)?;
            let receipt = input.submission().unwrap();
            let running = engine.clone();
            let selected = scope.clone();
            let steer = tokio::spawn(async move { running.steer(selected, input).await });
            let pending = fixture
                .steering
                .recv()
                .await
                .context("pending control missing")?;
            pending.accept().await?;
            if lost {
                call.writer
                    .fail(crate::provider::events::EventDeliveryError::Observation(
                        ObservationFailure::Overflow,
                    ));
                drop(pending);
            } else {
                let fenced = fixture.registry.fence(owner);
                tokio::pin!(fenced);
                assert!(futures_util::poll!(fenced.as_mut()).is_pending());
                drop(pending);
                fenced.await?;
            }
            let (_, selected, _, result, _) = active.remove(target);
            assert!(result.await?.is_err());
            assert!(!selected.is_healthy().await);
            assert!(steer.await?.is_err());
            assert!(matches!(
                receipt.record()?.delivery,
                SubmissionState::Accepted { .. }
            ));
            for (_, engine, _, result, mut call) in active {
                assert!(!result.is_finished());
                assert!(engine.is_healthy().await);
                call.finish(normal())?;
                result.await??.ticket.commit().await?;
            }
            assert_eq!(fixture.dispatch_count.load(Ordering::SeqCst), 3);
            fixture.close().await?;
        }
    }
    Ok(())
}

/// Panicking native work is normalized without leaking its payload and fences the exact owner.
#[tokio::test]
async fn submission_task_failure_is_normalized_and_fences_owner() -> Result<()> {
    let fixture = Fixture::new()?;
    fixture.native.with_pre_dispatch(|_| async {
        panic!("private native task panic");
    });
    for owner in owners() {
        let (engine, _, _, result) = fixture.run(&owner, "panic").await?;
        let error = match tokio::time::timeout(std::time::Duration::from_secs(3), result)
            .await
            .with_context(|| format!("native panic result did not settle for {owner:?}"))??
        {
            Err(error) => error,
            Ok(_) => anyhow::bail!("panic completed successfully"),
        };
        assert!(!format!("{error:?} {error}").contains("private native task panic"));
        assert!(!engine.is_healthy().await);
    }
    fixture.close().await
}

/// Exact collected cancellation before positive start preserves accepted input without a fictional turn.
#[tokio::test]
async fn accepted_prestart_cancellation_resolves_without_turn_completion() -> Result<()> {
    let mut fixture = Fixture::new()?;
    for owner in owners() {
        let (engine, scope, mut events, result) = fixture.run(&owner, "accepted-prestart").await?;
        let call = fixture
            .calls
            .recv()
            .await
            .context("accepted dispatch missing")?;
        let journal = fixture
            .store
            .submission(call.request.submission_id.as_str())?;
        engine.cancel(scope).await?;
        let ready = result.await??;
        assert!(matches!(ready.outcome, Err(ProviderError::Cancelled)));
        ready.ticket.commit().await?;
        assert!(matches!(
            journal.record()?.delivery,
            SubmissionState::Accepted { .. }
        ));
        while let Some(observation) = events.try_next()? {
            assert!(!matches!(
               observation,
               OwnerObservation::Event(HarnessEvent::Native {
                   event: boxed_native_event,
                   ..
               })
            if matches!(boxed_native_event.as_ref(), NativeEvent::Turn {
                       event: TurnEvent::Started(_) | TurnEvent::Completed(_),
                       ..
                   })));
        }
        assert!(engine.is_healthy().await);
    }
    fixture.close().await
}
