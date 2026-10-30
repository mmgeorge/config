#[derive(Clone, Copy)]
enum ChildCompletionFailure {
    ProviderCancelled,
    SimilarProviderMessage,
    CleanupFailed,
}

/// Independent typed cancellation must settle the child even without a local cancellation signal.
#[tokio::test]
async fn managed_child_provider_cancellation_is_cancelled_without_watch_signal() -> Result<()> {
    assert_child_completion_failure(ChildCompletionFailure::ProviderCancelled).await
}

/// Cancellation-like provider text remains a failure in the child completion path.
#[tokio::test]
async fn managed_child_cancellation_like_provider_error_remains_failed() -> Result<()> {
    assert_child_completion_failure(ChildCompletionFailure::SimilarProviderMessage).await
}

/// Cleanup failure retains its failure state and obligation even after provider cancellation.
#[tokio::test]
async fn managed_child_cleanup_failure_overrides_provider_cancellation() -> Result<()> {
    assert_child_completion_failure(ChildCompletionFailure::CleanupFailed).await
}

/// Exercise the admitted child coordinator, real event reducer, and durable completion transaction.
async fn assert_child_completion_failure(failure: ChildCompletionFailure) -> Result<()> {
    let repository = repository();
    let data = tempfile::tempdir()?;
    let mut broker = planning_question_broker(repository.path(), data.path(), false);
    let admission = admit_child_fixture(&mut broker, "independent-provider-cancellation")?;
    let owner = admission.owner().clone();
    assert!(
        !*broker
            .agent_coordinator
            .cancellation(&owner.agent_id, &admission.task.id)?
            .borrow()
    );
    let mut event = Vec::new();
    broker
        .observe_child_fixture(
            owner.clone(),
            &admission.task.id,
            child_event(crate::backend::TurnUpdate::Started { started_at_ms: 1 }),
            &mut event,
        )
        .await?;
    broker
        .observe_child_fixture(
            owner.clone(),
            &admission.task.id,
            child_tool("policy denied".into(), crate::backend::ToolStatus::Denied),
            &mut event,
        )
        .await?;
    let (output, cleanup_error, expected) = match failure {
        ChildCompletionFailure::ProviderCancelled => (
            Err(anyhow::Error::new(TurnCancelled)),
            None,
            ExchangeState::Cancelled,
        ),
        ChildCompletionFailure::SimilarProviderMessage => (
            Err(anyhow::Error::new(crate::backend::BackendError::Provider {
                code: None,
                message: "provider execution cancelled".into(),
            })),
            None,
            ExchangeState::Failed,
        ),
        ChildCompletionFailure::CleanupFailed => (
            Err(anyhow::Error::new(TurnCancelled)),
            Some("cleanup transport failed".into()),
            ExchangeState::Failed,
        ),
    };
    let turn_outcome = match failure {
        ChildCompletionFailure::SimilarProviderMessage => crate::turn::TurnOutcome::Failed,
        ChildCompletionFailure::ProviderCancelled | ChildCompletionFailure::CleanupFailed => {
            crate::turn::TurnOutcome::Cancelled
        }
    };
    broker
        .observe_child_fixture(
            owner.clone(),
            &admission.task.id,
            child_event(crate::backend::TurnUpdate::Finished {
                outcome: turn_outcome,
                completed_at_ms: 2,
            }),
            &mut event,
        )
        .await?;
    broker
        .complete_managed_agent(
            owner.clone(),
            admission.task.id.clone(),
            output,
            cleanup_error.clone(),
            &mut event,
        )
        .await?;
    let task = broker
        .agent_coordinator
        .task(&admission.task.id)
        .context("completed child disappeared")?;
    assert_eq!(task.state, expected);
    assert!(task.completed_at_ms.is_some());
    let exchange = broker
        .store
        .load_exchange(&admission.task.id)?
        .context("child exchange disappeared")?;
    assert_eq!(exchange.state(), expected);
    assert_eq!(
        exchange
            .tools()
            .next()
            .context("denied child tool disappeared")?
            .1
            .state(),
        crate::turn::ToolState::Denied
    );
    assert_eq!(exchange.finalization_error, cleanup_error);
    let agent = broker.agent_coordinator.resolve(&owner.agent_id)?;
    match failure {
        ChildCompletionFailure::CleanupFailed => {
            assert_eq!(agent.state, AgentState::Closing);
            assert_eq!(task.error.as_deref(), Some("cleanup transport failed"));
            assert!(
                broker
                    .agent_coordinator
                    .cleanup_task(&owner.agent_id)
                    .is_some()
            );
        }
        ChildCompletionFailure::ProviderCancelled
        | ChildCompletionFailure::SimilarProviderMessage => {
            assert_eq!(agent.state, AgentState::Ready);
            assert!(
                broker
                    .agent_coordinator
                    .cleanup_task(&owner.agent_id)
                    .is_none()
            );
        }
    }
    Ok(())
}
