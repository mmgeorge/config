use super::*;
use crate::backend::{
    Backend, BackendCapability, BackendDescriptor, BackendEvent, BackendEventSink, BackendKind,
    BackendRequest, Compaction, CompactionResult, CompactionState, EventScope, ExecutionOwner,
    ProviderDiagnostic,
};

/// Supply a real bounded observation stream without starting a provider process.
struct ObservationBackend {
    owner: ExecutionOwner,
    stream: std::sync::Mutex<Option<crate::backend::events::BackendEventStream>>,
    shutdown_delivery: std::sync::Mutex<Option<(BackendEventSink, Vec<BackendEvent>)>>,
}

#[async_trait::async_trait]
impl Backend for ObservationBackend {
    /// Transfer the fixture stream only to its owning session collector.
    async fn session_event_stream(
        &self,
        owner: ExecutionOwner,
    ) -> Result<Option<crate::backend::events::BackendEventStream>> {
        assert_eq!(owner, self.owner);
        Ok(self.stream.lock().unwrap().take())
    }

    /// Publish any configured final observations, then close the fixture stream.
    async fn shutdown(&self) -> Result<()> {
        let delivery = self.shutdown_delivery.lock().unwrap().take();
        if let Some((sink, events)) = delivery {
            for event in events {
                sink.send_wait(event).await?;
            }
        }
        Ok(())
    }

    /// Leave the observation-only fixture unchanged by execution cleanup.
    async fn cleanup_execution(&self, _: &str) -> Result<()> {
        Ok(())
    }

    /// Accept cancellation without an active foreground execution.
    async fn cancel_session(&self, _: &str) -> Result<()> {
        Ok(())
    }

    /// Report that session observation never starts a foreground turn.
    async fn has_active_turn(&self, _: &str) -> bool {
        false
    }

    /// Report that the fixture owns no native provider session.
    async fn active_session_id_for(&self, _: &str) -> Option<String> {
        None
    }

    /// Describe the observation fixture without optional provider capabilities.
    fn descriptor(&self) -> BackendDescriptor {
        BackendDescriptor {
            kind: BackendKind::Mock,
            label: "session observation fixture".into(),
            capability: BackendCapability::default(),
        }
    }

    /// Reject accidental foreground execution from the observation collector.
    async fn prompt_stream(
        &self,
        _: BackendRequest,
        _: BackendEventSink,
    ) -> Result<crate::backend::BackendCompletion, crate::backend::BackendError> {
        panic!("session observations must not submit a foreground prompt")
    }
}

/// Drain bounded provider intake while execution owns the broker, then publish in source order.
#[tokio::test]
async fn session_observation_intake_does_not_block_on_foreground_broker() -> Result<()> {
    let fixture = tempfile::tempdir()?;
    let broker = HarnessBroker::initialize(serde_json::from_value(json!({
        "workspace":fixture.path(), "data_root":fixture.path().join("data"),
        "client_id":"observation-fixture", "backend":{"kind":"mock","command":[]},
    }))?)?;
    let session_id = broker.snapshot()?.session.id.clone();
    let owner = ExecutionOwner::root(&session_id);
    let scope = EventScope::SessionOperation {
        owner: owner.clone(),
        operation_id: "observation-fixture".into(),
    };
    let (provider_sink, provider_stream) = crate::backend::events::channel(scope.clone());
    let mut controller = SessionController::new(broker)?;
    Arc::get_mut(&mut controller).unwrap().backend = Arc::new(ObservationBackend {
        owner,
        stream: std::sync::Mutex::new(Some(provider_stream)),
        shutdown_delivery: std::sync::Mutex::new(None),
    });
    let (message_sink, mut output) = forge_protocol::outbound::channel();
    controller.attach_session_events(&message_sink).await?;
    let broker_guard = controller.broker.lock().await;
    let diagnostic_count = forge_protocol::MAX_PENDING_FRAMES + 1;
    let producer = tokio::spawn(async move {
        provider_sink
            .send_wait(BackendEvent::Compaction {
                scope: scope.clone(),
                update: Box::new(Compaction {
                    id: "durable-observation".into(),
                    thread_id: "native-observation".into(),
                    state: CompactionState::Completed,
                    manual: false,
                    started_at_ms: Some(10),
                    completed_at_ms: Some(20),
                    result: CompactionResult {
                        context_tokens_before: Some(100),
                        context_tokens_after: Some(25),
                        ..Default::default()
                    },
                }),
            })
            .await?;
        for sequence in 0..diagnostic_count {
            provider_sink
                .send_wait(BackendEvent::Diagnostic {
                    scope: scope.clone(),
                    diagnostic: ProviderDiagnostic {
                        code: Some("observation-fixture".into()),
                        message: format!("observation-{sequence}"),
                        fatal: false,
                    },
                })
                .await?;
        }
        Ok::<_, crate::backend::BackendError>(())
    });
    tokio::time::timeout(Duration::from_secs(2), producer)
        .await
        .context("bounded provider intake waited for the held broker lock")???;
    assert!(controller.observer.session_events.has_pending()?);
    assert!(
        output.try_recv().is_err(),
        "observations published before broker release"
    );
    drop(broker_guard);

    tokio::time::timeout(Duration::from_secs(5), async {
        let mut expected_sequence = 0;
        while expected_sequence < diagnostic_count {
            let frame = output
                .recv()
                .await?
                .context("session observation output closed")?;
            let event: Value = serde_json::from_slice(frame.bytes())?;
            if event["event"] != "backend_event" {
                continue;
            }
            assert_eq!(event["session_id"], session_id);
            assert_eq!(event["payload"]["data"]["code"], "observation-fixture");
            assert_eq!(
                event["payload"]["text"],
                format!("observation-{expected_sequence}")
            );
            expected_sequence += 1;
        }
        Ok::<_, anyhow::Error>(())
    })
    .await
    .context("queued session observations did not publish after broker release")??;
    let stored = SqliteStore::read_only(&controller.data_root)?.list_session_event(&session_id)?;
    let compaction = stored
        .iter()
        .find_map(|record| match &record.detail {
            crate::timeline::SessionEventKind::Compaction { compaction }
                if compaction.id == "durable-observation" =>
            {
                Some(compaction)
            }
            _ => None,
        })
        .context("session observation was not persisted")?;
    assert_eq!(compaction.state, CompactionState::Completed);
    assert_eq!(compaction.result.context_tokens_before, Some(100));
    assert_eq!(compaction.result.context_tokens_after, Some(25));
    Ok(())
}

/// Drain shutdown evidence through the real collector and store before releasing its lease.
#[tokio::test]
async fn shutdown_drains_background_terminal_evidence_before_releasing_lease() -> Result<()> {
    use crate::backend::{
        TextUpdate, ToolActivityKind, ToolAddress, ToolInvocation, ToolOutcome, ToolUpdate,
    };

    let fixture = tempfile::tempdir()?;
    let broker = HarnessBroker::initialize(serde_json::from_value(json!({
        "workspace":fixture.path(), "data_root":fixture.path().join("data"),
        "client_id":"shutdown-observation-fixture", "backend":{"kind":"mock","command":[]},
    }))?)?;
    let session_id = broker.snapshot()?.session.id.clone();
    let owner = ExecutionOwner::root(&session_id);
    let scope = crate::backend::InvocationScope::new(owner.clone(), "background-exchange");
    let address = ToolAddress {
        turn: crate::backend::TurnAddress {
            scope: scope.clone(),
            thread_id: "background-thread".into(),
            turn_id: "background-turn".into(),
        },
        tool_id: "background-tool".into(),
    };
    let mut turn =
        crate::turn::test_support::TestTurn::new("background-turn".into(), address.turn.clone(), 1);
    for update in [
        ToolUpdate::Requested {
            invocation: ToolInvocation {
                kind: ToolActivityKind::Command,
                name: "exec".into(),
                title: "Background command".into(),
                arguments: Value::Null,
            },
        },
        ToolUpdate::Started,
        ToolUpdate::Output {
            update: TextUpdate::Append("BEGIN\n".into()),
        },
        ToolUpdate::Backgrounded,
    ] {
        turn.record_tool("background-tool", update, 2)?;
    }
    turn.finish(crate::turn::TurnOutcome::Completed, 3)?;
    let mut exchange = turn.into_exchange();
    exchange.finish(crate::exchange::ExchangeOutcome::Complete, 3)?;
    let data_root = fixture.path().join("data");
    SqliteStore::open(&data_root)?.save_exchange(&exchange)?;

    let (provider_sink, provider_stream) = crate::backend::events::channel(scope);
    let backend = Arc::new(ObservationBackend {
        owner,
        stream: std::sync::Mutex::new(Some(provider_stream)),
        shutdown_delivery: std::sync::Mutex::new(Some((
            provider_sink,
            vec![
                BackendEvent::Tool {
                    address: address.clone(),
                    update: ToolUpdate::Output {
                        update: TextUpdate::Append("STOPPED\n".into()),
                    },
                },
                BackendEvent::Tool {
                    address,
                    update: ToolUpdate::Finished {
                        outcome: ToolOutcome::Interrupted,
                    },
                },
            ],
        ))),
    });
    let mut controller = SessionController::new(broker)?;
    Arc::get_mut(&mut controller).unwrap().backend = backend.clone();
    let (message_sink, _output) = forge_protocol::outbound::channel();
    controller.attach_session_events(&message_sink).await?;
    let broker_guard = controller.broker.lock().await;
    let shutdown_controller = controller.clone();
    let shutdown = tokio::spawn(async move {
        backend.shutdown().await?;
        shutdown_controller.finish_session_events().await?;
        shutdown_controller.broker.lock().await.release_lease()?;
        Ok::<_, anyhow::Error>(())
    });
    tokio::time::timeout(Duration::from_secs(2), async {
        while !controller.observer.session_events.has_pending()? {
            tokio::task::yield_now().await;
        }
        Ok::<_, anyhow::Error>(())
    })
    .await
    .context("shutdown did not publish terminal evidence")??;
    assert!(
        !shutdown.is_finished(),
        "shutdown released its owner before queued evidence drained"
    );
    assert!(
        SqliteStore::read_only(&data_root)?
            .load_session(&session_id)?
            .unwrap()
            .lease_owner
            .is_some()
    );
    drop(broker_guard);
    tokio::time::timeout(Duration::from_secs(2), shutdown).await???;

    let store = SqliteStore::read_only(&data_root)?;
    let saved = store.load_exchange(&exchange.id)?.unwrap();
    let tool = saved.tools().next().unwrap().1;
    assert_eq!(tool.state(), crate::turn::ToolState::Interrupted);
    assert_eq!(tool.output, "BEGIN\nSTOPPED\n");
    assert_eq!(saved.state(), crate::exchange::ExchangeState::Complete);
    assert!(
        store
            .load_session(&session_id)?
            .unwrap()
            .lease_owner
            .is_none()
    );
    Ok(())
}

/// A cancelled shutdown wait must retain the collector until the provider closes its stream.
#[tokio::test]
async fn cancelled_session_observation_join_retains_collector_for_retry() -> Result<()> {
    let fixture = tempfile::tempdir()?;
    let broker = HarnessBroker::initialize(serde_json::from_value(json!({
        "workspace":fixture.path(), "data_root":fixture.path().join("data"),
        "client_id":"observation-retry-fixture", "backend":{"kind":"mock","command":[]},
    }))?)?;
    let owner = ExecutionOwner::root(&broker.snapshot()?.session.id);
    let (provider_sink, provider_stream) =
        crate::backend::events::channel(EventScope::SessionOperation {
            owner: owner.clone(),
            operation_id: "shutdown-retry".into(),
        });
    let mut controller = SessionController::new(broker)?;
    Arc::get_mut(&mut controller).unwrap().backend = Arc::new(ObservationBackend {
        owner,
        stream: std::sync::Mutex::new(Some(provider_stream)),
        shutdown_delivery: std::sync::Mutex::new(None),
    });
    let (message_sink, _output) = forge_protocol::outbound::channel();
    controller.attach_session_events(&message_sink).await?;
    assert!(
        tokio::time::timeout(Duration::ZERO, controller.finish_session_events())
            .await
            .is_err()
    );
    assert!(controller.session_event_task.lock().await.is_some());
    drop(provider_sink);
    tokio::time::timeout(Duration::from_secs(2), controller.finish_session_events()).await??;
    assert!(controller.session_event_task.lock().await.is_none());
    Ok(())
}
