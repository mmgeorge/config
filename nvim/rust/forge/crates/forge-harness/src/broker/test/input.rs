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

/// When the fixture reports the steered input's consumption relative to its acknowledgement.
#[derive(Clone, Copy, PartialEq)]
enum ConsumptionReport {
    /// The provider echoes the input once its response begins, after acknowledgement.
    AfterAcknowledgement,
    /// The provider echoes the input before it acknowledges receipt.
    BeforeAcknowledgement,
    /// The provider covers the input in its terminal result without ever echoing it.
    Never,
}

/// Drive one steered foreground turn and return its prompt-bearing publications and record.
async fn steered_publications(
    report: ConsumptionReport,
) -> Result<(
    Vec<crate::exchange::ExchangeInput>,
    usize,
    crate::exchange::Exchange,
)> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    let started = Arc::new(tokio::sync::Semaphore::new(0));
    let release = Arc::new(tokio::sync::Semaphore::new(0));
    let echo_now = Arc::new(tokio::sync::Semaphore::new(0));
    let echo_sent = Arc::new(tokio::sync::Semaphore::new(0));
    let steered = Arc::new(std::sync::Mutex::new(
        None::<forge_provider_api::CoveredInput>,
    ));
    let mut capability = crate::provider::test_support::fixture_capability();
    capability.integration.tracked_submission = true;
    capability.integration.local_acceptance = false;
    let descriptor = crate::provider::ProviderDescriptor {
        kind: crate::provider::ProviderId::new("test-consumption").expect("fixture ID is valid"),
        label: "Steering consumption fixture".into(),
        capability,
    };
    let echo = |address: &crate::provider::TurnAddress,
                item: &str,
                covered: &forge_provider_api::CoveredInput| {
        let start = forge_provider_api::UserMessageStart {
            text: "prefer the short form".into(),
            submission_id: Some(covered.submission_id().clone()),
        };
        [
            forge_provider_api::ItemEvent::Started(forge_provider_api::ItemStart::UserMessage(
                start,
            )),
            forge_provider_api::ItemEvent::Completed(
                forge_provider_api::ItemCompletion::UserMessage(
                    forge_provider_api::MessageCompletion {
                        text: "prefer the short form".into(),
                        outcome: forge_provider_api::MessageOutcome::Completed,
                    },
                ),
            ),
        ]
        .map(|event| ProviderEvent::Item {
            thread_id: address.thread_id.clone(),
            turn_id: address.turn_id.clone(),
            item_id: forge_provider_api::ItemId::new(item).expect("fixture item is nonempty"),
            event,
        })
    };
    let provider = crate::provider::test_support::EventFixtureProvider::new(descriptor, {
        let (started, release, echo_now, echo_sent, steered) = (
            started.clone(),
            release.clone(),
            echo_now.clone(),
            echo_sent.clone(),
            steered.clone(),
        );
        move |request, sink, start, _cancellation| {
            let (started, release, echo_now, echo_sent, steered) = (
                started.clone(),
                release.clone(),
                echo_now.clone(),
                echo_sent.clone(),
                steered.clone(),
            );
            async move {
                start.observed(0).await?;
                let address = crate::provider::test_support::address(&request.scope);
                started.add_permits(1);
                if report == ConsumptionReport::BeforeAcknowledgement {
                    crate::provider::test_support::wait_gate(&echo_now).await?;
                    let covered = steered.lock().unwrap().clone().expect("steered input");
                    for event in echo(&address, "early-echo", &covered) {
                        sink.send_wait(crate::provider::test_support::activity(event)?)
                            .await?;
                    }
                    echo_sent.add_permits(1);
                }
                crate::provider::test_support::wait_gate(&release).await?;
                let covered = steered.lock().unwrap().clone().expect("steered input");
                if report == ConsumptionReport::AfterAcknowledgement {
                    // A repeated report under a new item must not publish the prompt twice.
                    for item in ["echo", "echo-again"] {
                        for event in echo(&address, item, &covered) {
                            sink.send_wait(crate::provider::test_support::activity(event)?)
                                .await?;
                        }
                    }
                }
                for event in completed_message(
                    &address,
                    "response",
                    crate::provider::TestMessageContent::Response,
                    "Short form",
                ) {
                    sink.send_wait(crate::provider::test_support::activity(event)?)
                        .await?;
                }
                let mut coverage = request.input_coverage();
                coverage.push(covered);
                Ok(forge_provider_api::TurnResult::new(
                    crate::turn::TurnOutcome::Completed {
                        stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
                    },
                    1,
                    None,
                    coverage,
                    Vec::new(),
                    None,
                )
                .expect("fixture native coverage is valid"))
            }
        }
    });
    let provider = provider.with_steering({
        let (steered, echo_now, echo_sent) = (steered.clone(), echo_now.clone(), echo_sent.clone());
        move |request| {
            let (steered, echo_now, echo_sent) =
                (steered.clone(), echo_now.clone(), echo_sent.clone());
            async move {
                let receipt = request.delivery.receipt().expect("journaled steering");
                *steered.lock().unwrap() = Some(forge_provider_api::CoveredInput::new(
                    receipt.id().clone(),
                    receipt.attempt_id()?,
                ));
                if report == ConsumptionReport::BeforeAcknowledgement {
                    echo_now.add_permits(1);
                    crate::provider::test_support::wait_gate(&echo_sent).await?;
                }
                Ok(())
            }
        }
    });
    let session = crate::provider::test_support::ProviderTestSession::from_native(provider)?;
    broker.provider = session.provider_handle();
    let provider_id = broker.provider.descriptor().kind;
    let lane = broker.parent_input.clone();
    let catalog = broker.provider_catalog_request()?;
    let journal =
        crate::provider::submission::SubmissionStore::open(data.path(), broker.trace.clone())?;
    let (sink, mut stream) = publication::channel();
    broker.event_sink = Some(sink);
    let collected = tokio::spawn(async move {
        let mut prompts = Vec::new();
        let mut acknowledgements = 0;
        while let Ok(Some(update)) = stream.recv().await {
            if let publication::SessionUpdate::Timeline {
                update: publication::TimelineUpdate::Node { prompt },
            } = update
            {
                match prompt {
                    Some(prompt) => prompts.push(prompt),
                    None => acknowledgements += 1,
                }
            }
        }
        (prompts, acknowledgements)
    });
    let running = tokio::spawn(async move {
        let result = broker
            .run_interaction(
                "initial request".into(),
                PromptMode::Chat,
                Some(ExchangeAdmission::chat("initial request".into())),
            )
            .await;
        broker.event_sink = None;
        (broker, result)
    });
    await_boundary_barrier(&started).await?;
    let submission = journal.prepare_input(
        &catalog,
        ProviderInput::from_user_text("prefer the short form"),
        provider_id,
        Some("steer-message".into()),
    )?;
    lane.steer(crate::provider::SteeringInput::User {
        text: "prefer the short form".into(),
        attachments: Vec::new(),
        submission: Some(submission),
    })
    .await?;
    release.add_permits(1);
    let (broker, result) = running.await?;
    result?;
    let exchange = broker
        .store
        .list_exchange(&broker.session.id)?
        .pop()
        .expect("steered exchange");
    drop(broker);
    let (prompts, acknowledgements) = collected.await?;
    Ok((prompts, acknowledgements, exchange))
}

/// Report the consumption state of the steered input and whether its row precedes the response.
fn steered_row(exchange: &crate::exchange::Exchange) -> (crate::exchange::InputConsumption, bool) {
    let position = |matches: &dyn Fn(&crate::exchange::ExchangeNode) -> bool| {
        exchange.node_list.iter().position(matches)
    };
    let row = position(&|node| {
        matches!(node, crate::exchange::ExchangeNode::ExchangeInput { prompt } if prompt.id == "steer-message")
    })
    .expect("steered input row");
    let response = position(&|node| {
        matches!(node, crate::exchange::ExchangeNode::TurnContent { .. })
            && exchange.displays_node(node)
    })
    .expect("response node");
    let crate::exchange::ExchangeNode::ExchangeInput { prompt } = &exchange.node_list[row] else {
        unreachable!("position matched an input");
    };
    (prompt.consumption.clone(), row < response)
}

/// The echo publishes the consumed prompt once; acknowledgement alone publishes no prompt.
#[tokio::test]
async fn steering_consumption_publishes_its_prompt_once() -> Result<()> {
    let (prompts, acknowledgements, exchange) =
        steered_publications(ConsumptionReport::AfterAcknowledgement).await?;
    assert_eq!(acknowledgements, 1);
    assert_eq!(prompts.len(), 1, "{prompts:?}");
    assert_eq!(prompts[0].id, "steer-message");
    assert!(prompts[0].consumption.is_consumed());
    assert_eq!(
        steered_row(&exchange),
        (crate::exchange::InputConsumption::Consumed, true)
    );
    Ok(())
}

/// A report that precedes acknowledgement publishes the prompt when acknowledgement adopts it.
#[tokio::test]
async fn early_consumption_publishes_its_prompt_at_acknowledgement() -> Result<()> {
    let (prompts, _, exchange) =
        steered_publications(ConsumptionReport::BeforeAcknowledgement).await?;
    assert_eq!(prompts.len(), 1, "{prompts:?}");
    assert!(prompts[0].consumption.is_consumed());
    assert_eq!(
        steered_row(&exchange),
        (crate::exchange::InputConsumption::Consumed, true)
    );
    Ok(())
}

/// Terminal coverage without a report settles the input unconfirmed and publishes no prompt.
#[tokio::test]
async fn covered_steering_without_report_settles_unconfirmed() -> Result<()> {
    let (prompts, acknowledgements, exchange) =
        steered_publications(ConsumptionReport::Never).await?;
    assert_eq!(acknowledgements, 1);
    assert!(prompts.is_empty(), "{prompts:?}");
    assert_eq!(
        steered_row(&exchange).0,
        crate::exchange::InputConsumption::Unconfirmed
    );
    Ok(())
}
