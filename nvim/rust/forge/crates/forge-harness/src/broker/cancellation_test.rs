use super::*;
use crate::provider::{
    ProviderCompletion, ProviderError, ProviderEventSink, ToolActivityKind, ToolAddress,
    ToolInvocation, ToolOutcome, ToolUpdate, TurnUpdate,
};
use crate::turn::{ToolState, TurnOutcome, TurnState};
use futures_util::FutureExt;
use std::sync::atomic::AtomicUsize;

/// Preserve multiple bounded observation windows and await their final publication before readiness.
#[tokio::test]
async fn foreground_prefix_transfer_preserves_order_and_waits_for_last_publication() -> Result<()> {
    tokio::time::timeout(std::time::Duration::from_secs(10), async {
        let workspace = super::test::repository();
        let data = tempfile::tempdir()?;
        let mut broker =
            super::test::planning_question_broker(workspace.path(), data.path(), false);
        broker.checkpoint = None;
        broker.turn_cancellation.arm(true);
        let (publication, _updates) = publication::channel();
        let ForegroundDispatch::Admitting(admission) = broker.begin_foreground_dispatch(
            Request {
                id: 1,
                method: "session.prompt.submit".into(),
                params: json!({"text":"Observe every frame"}),
            },
            publication,
        )?
        else {
            anyhow::bail!("fixture did not create foreground admission");
        };
        let mut staged = broker.stage_foreground_interaction(
            "Observe every frame",
            PromptMode::Chat,
            Some(&ExchangeAdmission::chat("Observe every frame".into())),
            ProviderInput::from_text("Observe every frame"),
        )?;
        broker.begin_exchange_runtime(&mut staged.exchange, broker.clock.now_ms())?;
        let engine = broker
            .execution_registry()
            .owner(crate::provider::ExecutionOwner::root(&broker.session.id))
            .await?;
        let mut transaction = broker.construct_foreground_transaction(&staged, engine.clone())?;
        // Replace the never-polled submission with accepted observation fixtures, retaining its real host owner.
        transaction.native.task.abort_all();
        while let Some(result) = transaction.native.task.join_next().await {
            match result {
                Err(error) => assert!(error.is_cancelled()),
                Ok(_) => anyhow::bail!("replacement observation fixture dispatched its submission"),
            }
        }
        let scope = transaction.state.scope.clone();
        let (native, stream) = crate::provider::events::channel(scope.clone());
        transaction.native.events = stream;
        transaction.native_open = false;
        transaction.session_events =
            Arc::new(crate::provider::session_execution::SessionObservationInbox::default());
        transaction.question_enabled = false;
        transaction.cleanup_open = false;
        transaction.execution_open = false;
        transaction.agent_revision.borrow_and_update();
        let turn = crate::provider::TurnAddress {
            scope,
            thread_id: "prefix-thread".into(),
            turn_id: "prefix-turn".into(),
        };
        let mut frames = vec![ProviderEvent::Turn {
            address: turn.clone(),
            update: TurnUpdate::Started { started_at_ms: 1 },
        }];
        let mut expected = String::new();
        for index in 0..128 {
            let text = format!("frame-{index:03}: α🦀\n");
            expected.push_str(&text);
            frames.extend(crate::provider::test_support::message(
                &turn,
                format!("message-{index}"),
                Some(crate::provider::MessageRole::Response),
                text,
            ));
        }
        frames.push(ProviderEvent::Turn {
            address: turn.clone(),
            update: TurnUpdate::Finished {
                outcome: TurnOutcome::Completed,
                completed_at_ms: 2,
            },
        });
        transaction
            .pending_session_observations
            .extend(frames[..64].iter().cloned());
        for event in &frames[64..160] {
            native.send(event.clone())?;
        }
        for event in &frames[160..] {
            transaction.session_events.push(event.clone())?;
        }
        transaction.pending_native_ready = Some(Ok(ProviderCompletion::default()));
        transaction.native_goal = Some(NativeGoalTransition::ContinuationInitial {
            observation: frames[0].clone(),
        });
        broker.collect_foreground_observations(&mut transaction)?;
        assert_eq!(
            transaction.pending_session_observations.len(),
            forge_protocol::MAX_PENDING_FRAMES
        );
        assert!(
            transaction.next().now_or_never().is_none(),
            "full held staging consumed another source frame"
        );
        transaction.native_goal = None;
        let mut dispatch = ForegroundDispatch::Running {
            transaction: Box::new(transaction),
            admission,
        };
        let mut action = ForegroundAction::Wait;
        let mut publications = 0;
        loop {
            action = match action {
                ForegroundAction::Wait => {
                    let ForegroundDispatch::Running { transaction, .. } = &mut dispatch else {
                        anyhow::bail!("prefix application changed foreground ownership");
                    };
                    let signal = transaction.next().await?;
                    if matches!(
                        &signal,
                        ForegroundSignal::Native(NativeForegroundSignal::NativeReady(_))
                    ) {
                        assert_eq!(
                            publications,
                            frames.len(),
                            "native readiness bypassed its accepted prefix"
                        );
                        assert!(!transaction.observation_publication_in_flight);
                        let messages: String = transaction
                            .state
                            .interaction
                            .turn
                            .iter()
                            .flat_map(|turn| turn.messages())
                            .map(|message| message.text())
                            .collect();
                        assert_eq!(
                            messages, expected,
                            "bounded transfer changed native bytes/order"
                        );
                        assert_eq!(
                            transaction
                                .state
                                .interaction
                                .turn
                                .last()
                                .context("missing native turn")?
                                .state(),
                            TurnState::Finished {
                                outcome: TurnOutcome::Completed
                            }
                        );
                        break;
                    }
                    broker.step_foreground(&mut dispatch, signal)?
                }
                ForegroundAction::Effect(job @ ForegroundJob::ObservationPublication(_)) => {
                    let ForegroundDispatch::Running { transaction, .. } = &mut dispatch else {
                        anyhow::bail!("publication lost its foreground owner");
                    };
                    assert!(transaction.observation_publication_in_flight);
                    assert!(
                        transaction.next().now_or_never().is_none(),
                        "native result escaped before publication acknowledgment"
                    );
                    let signal = match job.run().await? {
                        ForegroundSignal::ObservationPublicationNeedsForward { events } => {
                            ForegroundSignal::ObservationPublicationDelivered(Ok(events))
                        }
                        signal => signal,
                    };
                    publications += 1;
                    broker.step_foreground(&mut dispatch, signal)?
                }
                _ => {
                    anyhow::bail!("unexpected foreground effect while transferring accepted prefix")
                }
            };
        }
        drop(native);
        engine.close().await?;
        Ok(())
    })
    .await
    .context("bounded foreground prefix did not settle")?
}

#[derive(Clone, Copy)]
enum ProviderFailure {
    Cancelled,
    SimilarMessage,
    Stalled,
}

struct CancelledNativeProvider {
    fixture_events: std::sync::Arc<crate::provider::test_support::SessionEvents>,
    failure: ProviderFailure,
    calls: AtomicUsize,
    cancellations: AtomicUsize,
}

#[async_trait::async_trait]
impl crate::provider::ProviderAdapter for CancelledNativeProvider {
    type Host = crate::provider::HarnessProviderHost;
    /// Advertise the existing mock transport while exercising the real broker lifecycle.
    fn descriptor(&self) -> crate::provider::ProviderDescriptor {
        crate::provider::ProviderDescriptor {
            kind: crate::provider::ProviderKind::Mock,
            label: "Native cancellation fixture".into(),
            capability: crate::provider::mock_capability(),
        }
    }

    /// Require the broker's real checkpoint boundary before emitting native tool activity.
    fn checkpoint_tool_gate(&self) -> bool {
        true
    }

    /// Release the fixture without external resources.
    async fn shutdown(&self) -> std::result::Result<(), crate::provider::ProviderError> {
        let result: anyhow::Result<()> = async move { Ok(()) }.await;
        result.map_err(|source| {
            crate::provider::lifecycle_error::classify(
                forge_provider_api::error::ProviderOperation::Shutdown,
                source,
            )
        })
    }

    /// Keep explicit cleanup independent from the provider's reported cancellation.
    async fn cleanup_execution(
        &self,
        _: &str,
    ) -> std::result::Result<(), crate::provider::ProviderError> {
        let result: anyhow::Result<()> = async move { Ok(()) }.await;
        result.map_err(|source| {
            crate::provider::lifecycle_error::classify(
                forge_provider_api::error::ProviderOperation::CleanupExecution,
                source,
            )
        })
    }

    /// Leave the adapter reusable after cancelled execution.
    async fn cancel_session(
        &self,
        _: &str,
    ) -> std::result::Result<(), crate::provider::ProviderError> {
        let result: anyhow::Result<()> = async move {
            self.cancellations.fetch_add(1, Ordering::SeqCst);
            Ok(())
        }
        .await;
        result.map_err(|source| {
            crate::provider::lifecycle_error::classify(
                forge_provider_api::error::ProviderOperation::CancelSession,
                source,
            )
        })
    }

    /// Report no outstanding provider turn after the synchronous fixture result.
    async fn has_active_turn(&self, _: &str) -> bool {
        false
    }

    /// Avoid inventing provider history for the fixture.
    async fn active_session_id_for(
        &self,
        _: &str,
    ) -> anyhow::Result<Option<forge_provider_api::ProviderSessionId>> {
        Ok(None)
    }

    /// Emit native cancellation evidence on the first call and a successful subsequent response.
    async fn session_events(
        &self,
        owner: crate::provider::ExecutionOwner,
    ) -> std::result::Result<
        crate::provider::events::ProviderEventStream,
        crate::provider::ProviderError,
    > {
        let stream = self.fixture_events.subscribe(owner.clone())?;
        Ok(stream)
    }

    async fn session_prompt(
        self: std::sync::Arc<Self>,
        request: crate::provider::ProviderRequest,
        scope: crate::provider::InvocationScope,
    ) -> std::result::Result<(), crate::provider::ProviderError> {
        let events = self.fixture_events.clone();
        events
            .submit(
                request,
                scope,
                crate::provider::test_support::TerminalBoundary::LocalFixture,
                move |request, sink| async move { self.run_native(request, sink).await },
            )
            .await
    }
}

impl CancelledNativeProvider {
    async fn run_native(
        &self,
        request: ProviderRequest,
        sink: ProviderEventSink,
    ) -> Result<ProviderCompletion, ProviderError> {
        if let Some(checkpoint) = request
            .control_context
            .as_ref()
            .and_then(|context| context.checkpoint.as_ref())
        {
            checkpoint.wait().await.map_err(ProviderError::from)?;
        }
        let call = self.calls.fetch_add(1, Ordering::SeqCst);
        let turn = crate::provider::test_support::address(
            sink.invocation_scope()?,
            "native-thread",
            format!("native-turn-{call}"),
        );
        sink.send_wait(ProviderEvent::Execution {
            observation: forge_provider_api::lifecycle::ExecutionObservation::new(
                turn.scope.clone(),
                forge_provider_api::lifecycle::ExecutionUpdate::Started {
                    address: turn.clone(),
                },
            )
            .expect("execution scope is derived from its native address"),
        })
        .await?;
        sink.send_wait(ProviderEvent::Turn {
            address: turn.clone(),
            update: TurnUpdate::Started { started_at_ms: 1 },
        })
        .await?;
        if call > 0 {
            for event in crate::provider::test_support::message(
                &turn,
                "recovered",
                Some(crate::provider::MessageRole::Response),
                "AFTER-CANCEL-OK",
            ) {
                sink.send_wait(event).await?;
            }
            sink.send_wait(ProviderEvent::Turn {
                address: turn,
                update: TurnUpdate::Finished {
                    outcome: TurnOutcome::Completed,
                    completed_at_ms: 5,
                },
            })
            .await?;
            return Ok(ProviderCompletion::default());
        }
        let address = ToolAddress {
            turn: turn.clone(),
            tool_id: "denied-native-command".into(),
        };
        for update in [
            ToolUpdate::Requested {
                invocation: ToolInvocation {
                    semantics: Default::default(),
                    kind: ToolActivityKind::Command,
                    name: "shell".into(),
                    title: "Denied native command".into(),
                    arguments: json!({"command":"write denied"}),
                },
            },
            ToolUpdate::Started,
        ] {
            sink.send_wait(ProviderEvent::Tool {
                address: address.clone(),
                update,
            })
            .await?;
        }
        if matches!(self.failure, ProviderFailure::Stalled) {
            loop {
                sink.send_wait(ProviderEvent::Tool {
                    address: address.clone(),
                    update: ToolUpdate::Output {
                        update: crate::provider::TextUpdate::Append("still running\n".into()),
                    },
                })
                .await?;
                tokio::time::sleep(std::time::Duration::from_millis(20)).await;
            }
        }
        sink.send_wait(ProviderEvent::Tool {
            address,
            update: ToolUpdate::Finished {
                outcome: ToolOutcome::Denied {
                    reason: "workspace policy".into(),
                },
            },
        })
        .await?;
        let outcome = match self.failure {
            ProviderFailure::Cancelled => TurnOutcome::Cancelled,
            ProviderFailure::SimilarMessage => TurnOutcome::Failed,
            ProviderFailure::Stalled => unreachable!("stalled provider never completes"),
        };
        sink.send_wait(ProviderEvent::Turn {
            address: turn,
            update: TurnUpdate::Finished {
                outcome,
                completed_at_ms: 4,
            },
        })
        .await?;
        Err(match self.failure {
            ProviderFailure::Cancelled => ProviderError::Cancelled,
            ProviderFailure::SimilarMessage => ProviderError::Provider {
                code: Some("provider_failure".into()),
                message: "provider execution cancelled".into(),
            },
            ProviderFailure::Stalled => unreachable!("stalled provider never completes"),
        })
    }
}

/// Preserve native denied-tool history while classifying the enclosing exchange as cancelled.
#[tokio::test]
async fn native_cancelled_error_preserves_denied_history_and_allows_next_prompt() -> Result<()> {
    assert_native_failure(ProviderFailure::Cancelled).await
}

/// Similar provider text must not gain cancellation semantics or trigger automatic replay.
#[tokio::test]
async fn provider_failure_named_cancelled_remains_failed() -> Result<()> {
    assert_native_failure(ProviderFailure::SimilarMessage).await
}

/// Stop a noisy shell command at its deadline, persist failure, and keep the next prompt usable.
#[tokio::test]
async fn shell_timeout_stops_execution_without_replaying_the_command() -> Result<()> {
    tokio::time::timeout(
        std::time::Duration::from_secs(10),
        assert_native_failure(ProviderFailure::Stalled),
    )
    .await?
}

/// Exercise public dispatch, persisted exchange history, and reuse through the real broker.
async fn assert_native_failure(failure: ProviderFailure) -> Result<()> {
    let repository = super::test::repository();
    let data = tempfile::tempdir()?;
    let mut broker = super::test::planning_question_broker(repository.path(), data.path(), false);
    if matches!(failure, ProviderFailure::Stalled) {
        broker.session.tool_timeout = crate::session::InputTimeout {
            amount: 1,
            unit: crate::session::TimeoutUnit::S,
        };
    }
    let provider = Arc::new(CancelledNativeProvider {
        fixture_events: Default::default(),
        failure,
        calls: AtomicUsize::new(0),
        cancellations: AtomicUsize::new(0),
    });
    broker.provider = provider.clone();
    broker.turn_cancellation.arm(true);
    let result = broker
        .dispatch(Request {
            id: 1,
            method: "session.prompt.submit".into(),
            params: json!({"text":"Attempt the native command"}),
        })
        .await;
    let expected = match failure {
        ProviderFailure::Cancelled => ExchangeState::Cancelled,
        ProviderFailure::SimilarMessage | ProviderFailure::Stalled => ExchangeState::Failed,
    };
    let expected_code = match failure {
        ProviderFailure::Cancelled => "turn_cancelled",
        ProviderFailure::SimilarMessage => "request_failed",
        ProviderFailure::Stalled => "tool_timeout",
    };
    assert_eq!(
        result.response.error().context("fixture should fail")?.code,
        expected_code
    );
    assert_eq!(
        provider.calls.load(Ordering::SeqCst),
        1,
        "terminal provider results must not replay"
    );
    let history = broker.store.list_exchange(&broker.session.id)?;
    assert_eq!(history.len(), 1);
    let exchange = &history[0];
    assert_eq!(exchange.state(), expected);
    assert!(exchange.provider_retry.is_none());
    assert!(exchange.finalization_error.is_none());
    let expected_turn = match failure {
        ProviderFailure::Cancelled => TurnOutcome::Cancelled,
        ProviderFailure::SimilarMessage | ProviderFailure::Stalled => TurnOutcome::Failed,
    };
    assert_eq!(
        exchange.turn[0].state(),
        TurnState::Finished {
            outcome: expected_turn
        }
    );
    let tool = exchange
        .tools()
        .next()
        .context("denied native tool disappeared")?
        .1;
    assert_eq!(tool.id, "denied-native-command");
    if matches!(failure, ProviderFailure::Stalled) {
        assert_eq!(tool.state(), ToolState::Failed);
        assert!(provider.cancellations.load(Ordering::SeqCst) > 0);
    } else {
        assert_eq!(tool.state(), ToolState::Denied);
    }
    assert!(
        broker
            .snapshot()?
            .timeline
            .iter()
            .any(|entry| matches!(entry, TimelineEntry::Exchange { id, .. } if id == &exchange.id))
    );
    // Service admission rearms cancellation only after the previous execution settles.
    broker.turn_cancellation.arm(true);
    let recovered = broker
        .dispatch(Request {
            id: 2,
            method: "session.prompt.submit".into(),
            params: json!({"text":"Run the next ordinary prompt"}),
        })
        .await;
    assert!(
        recovered.response.error().is_none(),
        "{:?}",
        recovered.response.error()
    );
    assert_eq!(provider.calls.load(Ordering::SeqCst), 2);
    let history = broker.store.list_exchange(&broker.session.id)?;
    assert_eq!(history.len(), 2);
    assert_eq!(history[0].state(), expected);
    assert_eq!(history[1].state(), ExchangeState::Complete);
    assert!(
        history[1]
            .turn
            .iter()
            .flat_map(|turn| turn.messages())
            .any(|message| message.text() == "AFTER-CANCEL-OK")
    );
    Ok(())
}
