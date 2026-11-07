//! Exercise real host intake with independently controlled native start, delivery, and completion.
use super::*;
use crate::provider::submission::{SubmissionState, SubmissionStore};
use crate::provider::test_support::{EventFixtureProvider, ProviderTestSession};
use crate::provider::{PromptMode, ProviderInput, SteeringInput};
use anyhow::{Context, Result};
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
    reply: oneshot::Sender<Result<(), ProviderError>>,
}
impl SteeringCall {
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
                let (_, mut cancelled) = coverage.ok_or(ProviderError::NotConsumed)?;
                let (reply, response) = oneshot::channel();
                sender
                    .send(SteeringCall { reply })
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
