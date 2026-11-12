use super::super::{
    ExchangeAdmission, ForegroundAction, ForegroundCollection, ForegroundSignal,
    NativeForegroundSignal, PromptMode, ProviderAttempt, ProviderCompletion, ProviderError,
    ProviderEvent, ProviderInput, Request, TurnTransaction, publication,
};
use super::test_support::{completed_message, planning_question_broker, repository};
use crate::provider::ScopeOwner;
use anyhow::Result;
use serde_json::json;
use std::sync::Arc;
use std::sync::atomic::Ordering;

/// Reject new steering as soon as the owner reads native completion, before checkpoint settlement.
#[tokio::test]
async fn native_ready_closes_foreground_steering_admission() -> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    broker.checkpoint = None;
    broker.turn_cancellation.arm();
    let mut staged = broker.stage_foreground_interaction(
        "finish",
        PromptMode::Chat,
        Some(&ExchangeAdmission::chat("finish".into())),
        ProviderInput::from_user_text("finish"),
    )?;
    broker.begin_exchange_runtime(&mut staged.exchange, broker.clock.now_ms())?;
    let engine = broker
        .execution_registry()
        .owner(crate::provider::ThreadOwner::root(&broker.session.id))
        .await?;
    let mut transaction = broker.construct_foreground_transaction(&staged, engine)?;
    transaction.scan_enabled = false;
    transaction.question_enabled = false;
    transaction.agent_command_active = true;
    transaction.execution_open = false;
    transaction.native.pending_submission = Some(Box::pin(async {
        Ok(ProviderAttempt::Ready(Box::new(Ok(
            ProviderCompletion::default(),
        ))))
    }));
    assert!(transaction.accepts_steering());
    transaction.native.authorize_submission();
    async {
        loop {
            if matches!(
                transaction.next().await?,
                ForegroundSignal::Native(NativeForegroundSignal::NativeReady(_))
            ) {
                break Ok::<(), anyhow::Error>(());
            }
        }
    }
    .await?;
    assert!(!transaction.accepts_steering());
    // A result owned by a steering effect must settle before terminal evidence is re-exposed.
    let flight = transaction.steering.pending_deliveries.retain();
    transaction.pending_native_ready = Some(Ok(ProviderCompletion::default()));
    assert!(futures_util::FutureExt::now_or_never(transaction.next()).is_none());
    drop(flight);
    assert!(matches!(
        transaction.next().await?,
        ForegroundSignal::Native(NativeForegroundSignal::NativeReady(_))
    ));
    transaction.acknowledge_native_result();
    transaction.collection = Some(ForegroundCollection::Restart);
    let (sink, _stream) = publication::channel();
    let TurnTransaction::Admitting(admission) = broker.begin_foreground_dispatch(
        Request {
            id: 9,
            method: "session.prompt.submit".into(),
            params: json!({"text":"finish"}),
        },
        sink,
    )?
    else {
        unreachable!()
    };
    let mut dispatch = TurnTransaction::Running {
        transaction: Box::new(transaction),
        admission,
    };
    assert!(matches!(
        broker.step_foreground(
            &mut dispatch,
            ForegroundSignal::EffectFailed(
                ProviderError::PreparationFailed(Box::new(ProviderError::Cancelled)).into(),
            )
        )?,
        ForegroundAction::Wait
    ));
    let TurnTransaction::Running { transaction, .. } = &dispatch else {
        unreachable!()
    };
    assert!(!transaction.failure_cleanup_active);
    assert!(matches!(
        transaction.pending_native_ready,
        Some(Err(ProviderError::Cancelled))
    ));
    let _quiescence = broker.agent_coordinator.begin_quiescence();
    broker.step_foreground(
        &mut dispatch,
        ForegroundSignal::Native(NativeForegroundSignal::NativeReady(ProviderAttempt::Ready(
            Box::new(Ok(ProviderCompletion::default())),
        ))),
    )?;
    assert!(matches!(dispatch, TurnTransaction::Finalizing { .. }));
    Ok(())
}

struct BoundarySteeringProvider {
    capability: crate::provider::ProviderCapability,
    initial_started: Arc<tokio::sync::Semaphore>,
    initial_release: Arc<tokio::sync::Semaphore>,
    steering_deferred: Arc<tokio::sync::Semaphore>,
    continuation_started: Arc<tokio::sync::Semaphore>,
    continuation_accept: Arc<tokio::sync::Semaphore>,
    continuation_release: Arc<tokio::sync::Semaphore>,
    input: Arc<std::sync::Mutex<Vec<String>>>,
    steered: Arc<std::sync::Mutex<Vec<forge_provider_api::CoveredInput>>>,
    fail_response: Arc<std::sync::atomic::AtomicBool>,
}

impl BoundarySteeringProvider {
    fn new() -> Self {
        Self {
            capability: crate::provider::test_support::fixture_capability(),
            initial_started: Arc::new(tokio::sync::Semaphore::new(0)),
            initial_release: Arc::new(tokio::sync::Semaphore::new(0)),
            steering_deferred: Arc::new(tokio::sync::Semaphore::new(0)),
            continuation_started: Arc::new(tokio::sync::Semaphore::new(0)),
            continuation_accept: Arc::new(tokio::sync::Semaphore::new(0)),
            continuation_release: Arc::new(tokio::sync::Semaphore::new(0)),
            input: Default::default(),
            steered: Default::default(),
            fail_response: Default::default(),
        }
    }
}

async fn await_boundary_barrier(semaphore: &tokio::sync::Semaphore) -> Result<()> {
    semaphore.acquire().await?.forget();
    Ok(())
}

impl BoundarySteeringProvider {
    fn fixture(self: &Arc<Self>) -> Arc<crate::provider::test_support::EventFixtureProvider> {
        let mut capability = self.capability.clone();
        capability.integration.tracked_submission = true;
        capability.integration.local_acceptance = false;
        let descriptor = crate::provider::ProviderDescriptor {
            kind: crate::provider::ProviderId::new("test-boundary-steering")
                .expect("fixture ID is valid"),
            label: "Normalized boundary steering fixture".into(),
            capability: capability.clone(),
        };
        let initial_started = self.initial_started.clone();
        let initial_release = self.initial_release.clone();
        let continuation_release = self.continuation_release.clone();
        let input = self.input.clone();
        let steered = self.steered.clone();
        let fail_response = self.fail_response.clone();
        let native_steer = capability.native_steer;
        let provider = crate::provider::test_support::EventFixtureProvider::new(
            descriptor,
            move |request, sink, start, mut cancellation| {
                let initial_started = initial_started.clone();
                let initial_release = initial_release.clone();
                let continuation_release = continuation_release.clone();
                let input = input.clone();
                let steered = steered.clone();
                let fail_response = fail_response.clone();

                async move {
                    start.observed(0).await?;

                    let text = crate::provider::test_support::input_text(&request.input);
                    input.lock().unwrap().push(text.clone());
                    let continuation = text == "boundary correction";
                    let address = crate::provider::TurnAddress {
                        thread_id: (request.scope.clone()).owner().thread_id(),
                        turn_id: (request.scope.clone()).turn_id().unwrap(),
                        scope: request.scope.clone(),
                    };
                    sink.send_wait(crate::provider::test_support::activity(
                        ProviderEvent::Thread {
                            thread_id: (request.scope.clone()).owner().thread_id(),
                            event: forge_provider_api::ThreadEvent::IdentityResolved(
                                "boundary-history"
                                    .to_owned()
                                    .try_into()
                                    .expect("fixture history is nonempty"),
                            ),
                        },
                    )?)
                    .await?;

                    let release = if continuation {
                        &continuation_release
                    } else {
                        initial_started.add_permits(1);
                        &initial_release
                    };
                    let interrupted = if *cancellation.borrow() {
                        true
                    } else {
                        tokio::select! {
                            released = crate::provider::test_support::wait_gate(release) => {
                                released?;
                                false
                            }
                            _ = cancellation.changed() => true,
                        }
                    };
                    let cancelled = interrupted
                        || (fail_response.load(Ordering::Acquire)
                            && (continuation || !native_steer));
                    let outcome = if cancelled {
                        crate::turn::TurnOutcome::Interrupted
                    } else {
                        for event in completed_message(
                            &address,
                            "handled-response",
                            crate::provider::TestMessageContent::Response,
                            format!("handled {text}"),
                        ) {
                            sink.send_wait(crate::provider::test_support::activity(event)?)
                                .await?;
                        }
                        crate::turn::TurnOutcome::Completed {
                            stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
                        }
                    };
                    let native_outcome = outcome;
                    let native_completed_at_ms = 1;

                    let mut coverage = request.input_coverage();
                    coverage.extend(steered.lock().unwrap().iter().cloned());
                    Ok(forge_provider_api::TurnResult::new(
                        native_outcome,
                        native_completed_at_ms,
                        None,
                        coverage,
                        Vec::new(),
                        None,
                    )
                    .expect("fixture native coverage is valid"))
                }
            },
        );
        let continuation_started = self.continuation_started.clone();
        let continuation_accept = self.continuation_accept.clone();
        provider.with_pre_dispatch(move |request| {
            let continuation_started = continuation_started.clone();
            let continuation_accept = continuation_accept.clone();
            async move {
                if matches!(request.input.user_input(), Some(forge_provider_api::PromptInput::Text(text)) if text == "boundary correction") {
                    continuation_started.add_permits(1);
                    crate::provider::test_support::wait_gate(&continuation_accept).await?;
                }
                Ok(())
            }
        });
        let deferred = self.steering_deferred.clone();
        provider.with_steering(move |_request| {
            let deferred = deferred.clone();
            async move {
                deferred.add_permits(1);
                Err(crate::provider::ProviderError::NotConsumed)
            }
        })
    }
}
#[tokio::test]
async fn deferred_steering_acknowledges_only_after_provider_acceptance() -> Result<()> {
    assert_deferred_steering_delivery(false).await
}

/// Retaining a cancelled foreground must release unsent commands and their durable attempts.
#[tokio::test]
async fn cancelled_terminal_releases_undispatched_steering_before_retention() -> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    let mut fixture = BoundarySteeringProvider::new();
    fixture.capability.native_steer = false;
    fixture.fail_response.store(true, Ordering::Release);
    let provider = Arc::new(fixture);
    let session =
        crate::provider::test_support::ProviderTestSession::from_native(provider.fixture())?;
    broker.provider = session.provider_handle();
    let lane = broker.parent_input.clone();
    let catalog = broker.provider_catalog_request()?;
    broker.turn_cancellation().arm();
    let submission = broker.submission_store.prepare_input(
        &catalog,
        ProviderInput::from_user_text("Retain café 東京"),
        broker.provider.descriptor().kind,
        Some("cancelled-followup".into()),
    )?;
    let running = tokio::spawn(async move {
        let result = broker
            .run_interaction(
                "initial request".into(),
                PromptMode::Chat,
                Some(ExchangeAdmission::chat("initial request".into())),
            )
            .await;
        (broker, result)
    });
    await_boundary_barrier(&provider.initial_started).await?;
    let pending = lane.steer(crate::provider::SteeringInput::User {
        text: "Retain café 東京".into(),
        attachments: Vec::new(),
        submission: Some(submission.clone()),
    });
    tokio::pin!(pending);
    assert!(futures_util::poll!(pending.as_mut()).is_pending());
    provider.initial_release.add_permits(1);
    let (_broker, result) = running.await?;
    assert!(result.is_err());
    let rejected = pending.await.unwrap_err();
    assert!(matches!(
        rejected.downcast_ref::<crate::provider::ProviderError>(),
        Some(crate::provider::ProviderError::Cancelled)
    ));
    assert_eq!(
        submission.record()?.delivery,
        crate::provider::submission::SubmissionState::Cancelled
    );
    assert_eq!(
        provider.input.lock().unwrap().as_slice(),
        ["initial request"]
    );
    Ok(())
}

async fn assert_deferred_steering_delivery(fail_response: bool) -> Result<()> {
    async {
        let repository = repository();
        let data = tempfile::tempdir()?;
        let mut broker = planning_question_broker(repository.path(), data.path(), false);
        let provider = Arc::new(BoundarySteeringProvider::new());
        provider.fail_response.store(fail_response, std::sync::atomic::Ordering::Release);
        let session = crate::provider::test_support::ProviderTestSession::from_native(provider.fixture())?;
        broker.provider = session.provider_handle();
        let provider_id = broker.provider.descriptor().kind;
        let lane = broker.parent_input.clone();
        let catalog = broker.provider_catalog_request()?;
        let journal = crate::provider::submission::SubmissionStore::open(data.path(), broker.trace.clone())?;
        let mut running = tokio::spawn(async move {
            let result = broker.run_interaction("initial request".into(), PromptMode::Chat,
                Some(ExchangeAdmission::chat("initial request".into()))).await;
            (broker, result)
        });
        tokio::select! {
            started = await_boundary_barrier(&provider.initial_started) => started?,
            finished = &mut running => {
                let (_, result) = finished?;
                anyhow::bail!("boundary foreground ended before initial native start: {result:?}");
            }
        }
        let submission = journal.prepare_input(&catalog,
            ProviderInput::from_user_text("boundary correction"), provider_id, Some("boundary-message".into()))?;
        let mut steering = Box::pin(lane.steer(crate::provider::SteeringInput::User {
            text: "boundary correction".into(), attachments: Vec::new(), submission: Some(submission.clone()),
        }));
        assert!(futures_util::poll!(steering.as_mut()).is_pending());
        await_boundary_barrier(&provider.steering_deferred).await?;
        provider.initial_release.add_permits(1);
        tokio::select! {
            prepared = await_boundary_barrier(&provider.continuation_started) => prepared?,
            finished = &mut running => {
                let (_, result) = finished?;
                anyhow::bail!("foreground ended before continuation preparation: {result:?}");
            }
        }
        assert!(matches!(submission.record()?.delivery, crate::provider::submission::SubmissionState::Prepared));
        assert!(futures_util::poll!(steering.as_mut()).is_pending(),
            "a continuation was acknowledged while its journal still allowed requeueing");
        provider.continuation_accept.add_permits(1);
        steering.await?;
        assert!(matches!(submission.record()?.delivery, crate::provider::submission::SubmissionState::Accepted { .. }));
        assert!(!running.is_finished(), "steering acknowledgement waited for the full provider response");
        provider.continuation_release.add_permits(1);
        let (broker, result) = running.await?;
        if fail_response { assert!(result.is_err()); } else { result?; }
        let exchange = broker.store.list_exchange(&broker.session.id)?;
        assert_eq!(exchange.len(), 1, "steering created a second exchange");
        assert_eq!(exchange[0].node_list.iter().filter(|node| matches!(node,
            crate::exchange::ExchangeNode::ExchangeInput { prompt } if prompt.text == "boundary correction" && prompt.id == "boundary-message")).count(), 1);
        assert_eq!(*provider.input.lock().unwrap(), ["initial request", "boundary correction"]);
        Ok::<(), anyhow::Error>(())
    }.await
}
