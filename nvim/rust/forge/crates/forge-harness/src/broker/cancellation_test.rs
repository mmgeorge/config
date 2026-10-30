use super::*;
use crate::backend::{
    BackendCompletion, BackendError, BackendEventSink, ToolActivityKind, ToolAddress,
    ToolInvocation, ToolOutcome, ToolUpdate, TurnUpdate,
};
use crate::turn::{ToolState, TurnOutcome, TurnState};
use std::sync::atomic::AtomicUsize;

#[derive(Clone, Copy)]
enum ProviderFailure {
    Cancelled,
    SimilarMessage,
}

struct CancelledNativeBackend {
    failure: ProviderFailure,
    calls: AtomicUsize,
}

#[async_trait::async_trait]
impl Backend for CancelledNativeBackend {
    /// Advertise the existing mock transport while exercising the real broker lifecycle.
    fn descriptor(&self) -> crate::backend::BackendDescriptor {
        crate::backend::BackendDescriptor {
            kind: crate::backend::BackendKind::Mock,
            label: "Native cancellation fixture".into(),
            capability: crate::backend::mock_capability(),
        }
    }

    /// Require the broker's real checkpoint boundary before emitting native tool activity.
    fn checkpoint_tool_gate(&self) -> bool {
        true
    }

    /// Release the fixture without external resources.
    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    /// Keep explicit cleanup independent from the provider's reported cancellation.
    async fn cleanup_execution(&self, _: &str) -> Result<()> {
        Ok(())
    }

    /// Leave the adapter reusable after cancelled execution.
    async fn cancel_session(&self, _: &str) -> Result<()> {
        Ok(())
    }

    /// Report no outstanding provider turn after the synchronous fixture result.
    async fn has_active_turn(&self, _: &str) -> bool {
        false
    }

    /// Avoid inventing provider history for the fixture.
    async fn active_session_id_for(&self, _: &str) -> Option<String> {
        None
    }

    /// Emit native cancellation evidence on the first call and a successful subsequent response.
    async fn prompt_stream(
        &self,
        request: BackendRequest,
        sink: BackendEventSink,
    ) -> Result<BackendCompletion, BackendError> {
        if let Some(checkpoint) = request
            .control_context
            .as_ref()
            .and_then(|context| context.checkpoint.as_ref())
        {
            checkpoint.wait().await.map_err(BackendError::from)?;
        }
        let call = self.calls.fetch_add(1, Ordering::SeqCst);
        let turn = crate::backend::test_support::address(
            sink.invocation_scope()?,
            "native-thread",
            format!("native-turn-{call}"),
        );
        sink.send_wait(BackendEvent::Turn {
            address: turn.clone(),
            update: TurnUpdate::Started { started_at_ms: 1 },
        })
        .await?;
        if call > 0 {
            for event in crate::backend::test_support::message(
                &turn,
                "recovered",
                Some(crate::backend::MessageRole::Response),
                "AFTER-CANCEL-OK",
            ) {
                sink.send_wait(event).await?;
            }
            sink.send_wait(BackendEvent::Turn {
                address: turn,
                update: TurnUpdate::Finished {
                    outcome: TurnOutcome::Completed,
                    completed_at_ms: 5,
                },
            })
            .await?;
            return Ok(BackendCompletion::default());
        }
        let address = ToolAddress {
            turn: turn.clone(),
            tool_id: "denied-native-command".into(),
        };
        for update in [
            ToolUpdate::Requested {
                invocation: ToolInvocation {
                    kind: ToolActivityKind::Command,
                    name: "shell".into(),
                    title: "Denied native command".into(),
                    arguments: json!({"command":"write denied"}),
                },
            },
            ToolUpdate::Started,
            ToolUpdate::Finished {
                outcome: ToolOutcome::Denied {
                    reason: "workspace policy".into(),
                },
            },
        ] {
            sink.send_wait(BackendEvent::Tool {
                address: address.clone(),
                update,
            })
            .await?;
        }
        let outcome = match self.failure {
            ProviderFailure::Cancelled => TurnOutcome::Cancelled,
            ProviderFailure::SimilarMessage => TurnOutcome::Failed,
        };
        sink.send_wait(BackendEvent::Turn {
            address: turn,
            update: TurnUpdate::Finished {
                outcome,
                completed_at_ms: 4,
            },
        })
        .await?;
        Err(match self.failure {
            ProviderFailure::Cancelled => BackendError::Cancelled,
            ProviderFailure::SimilarMessage => BackendError::Provider {
                code: Some("provider_failure".into()),
                message: "provider execution cancelled".into(),
            },
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

/// Exercise public dispatch, persisted exchange history, and reuse through the real broker.
async fn assert_native_failure(failure: ProviderFailure) -> Result<()> {
    let repository = super::test::repository();
    let data = tempfile::tempdir()?;
    let mut broker = super::test::planning_question_broker(repository.path(), data.path(), false);
    let backend = Arc::new(CancelledNativeBackend {
        failure,
        calls: AtomicUsize::new(0),
    });
    broker.backend = backend.clone();
    let result = broker
        .dispatch(Request {
            id: 1,
            method: "session.prompt.submit".into(),
            params: json!({"text":"Attempt the native command"}),
        })
        .await;
    let expected = match failure {
        ProviderFailure::Cancelled => ExchangeState::Cancelled,
        ProviderFailure::SimilarMessage => ExchangeState::Failed,
    };
    let expected_code = match failure {
        ProviderFailure::Cancelled => "turn_cancelled",
        ProviderFailure::SimilarMessage => "request_failed",
    };
    assert_eq!(
        result.response.error().context("fixture should fail")?.code,
        expected_code
    );
    assert_eq!(
        backend.calls.load(Ordering::SeqCst),
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
        ProviderFailure::SimilarMessage => TurnOutcome::Failed,
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
    assert_eq!(tool.state(), ToolState::Denied);
    assert!(
        broker
            .snapshot()?
            .timeline
            .iter()
            .any(|entry| matches!(entry, TimelineEntry::Exchange { id, .. } if id == &exchange.id))
    );
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
    assert_eq!(backend.calls.load(Ordering::SeqCst), 2);
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
