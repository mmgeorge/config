#[derive(Clone, Copy)]
enum ChildCompletionFailure {
    ProviderCancelled,
    ProviderFailed,
    CleanupFailed,
}

/// Independent typed cancellation must settle the child even without a local cancellation signal.
#[tokio::test]
async fn managed_child_provider_cancellation_is_cancelled_without_watch_signal() -> Result<()> {
    assert_child_completion_failure(ChildCompletionFailure::ProviderCancelled).await
}

/// A normalized provider failure remains distinct from cancellation.
#[tokio::test]
async fn managed_child_normalized_provider_error_remains_failed() -> Result<()> {
    assert_child_completion_failure(ChildCompletionFailure::ProviderFailed).await
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
            .cancellation(owner.agent_id.as_str(), &admission.task.id)?
            .borrow()
    );
    let mut event = Vec::new();
    broker
        .observe_child_fixture(
            owner.clone(),
            &admission.task.id,
            child_event(crate::provider::TurnUpdate::Started { started_at_ms: 1 }),
            &mut event,
        )
        .await?;
    broker
        .observe_child_fixture(
            owner.clone(),
            &admission.task.id,
            child_tool("policy denied".into(), crate::provider::ToolStatus::Denied),
            &mut event,
        )
        .await?;
    let (output, cleanup_error, expected) = match failure {
        ChildCompletionFailure::ProviderCancelled => (
            Err(anyhow::Error::new(TurnCancelled)),
            None,
            ExchangeState::Cancelled,
        ),
        ChildCompletionFailure::ProviderFailed => (
            Err(anyhow::Error::new(crate::provider::ProviderError::from(
                forge_provider_api::ProviderFailure::new(
                    forge_provider_api::ProviderOperation::InputSubmit,
                    forge_provider_api::ProviderFailureKind::Unknown,
                ),
            ))),
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
        ChildCompletionFailure::ProviderFailed => crate::turn::TurnOutcome::Failed,
        ChildCompletionFailure::ProviderCancelled | ChildCompletionFailure::CleanupFailed => {
            crate::turn::TurnOutcome::Cancelled
        }
    };
    broker
        .observe_child_fixture(
            owner.clone(),
            &admission.task.id,
            child_event(crate::provider::TurnUpdate::Finished {
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
    let agent = broker.agent_coordinator.resolve(owner.agent_id.as_str())?;
    match failure {
        ChildCompletionFailure::CleanupFailed => {
            assert_eq!(agent.state, AgentState::Closing);
            assert_eq!(task.error.as_deref(), Some("cleanup transport failed"));
            assert!(
                broker
                    .agent_coordinator
                    .cleanup_task(owner.agent_id.as_str())
                    .is_some()
            );
        }
        ChildCompletionFailure::ProviderCancelled | ChildCompletionFailure::ProviderFailed => {
            assert_eq!(agent.state, AgentState::Ready);
            assert!(
                broker
                    .agent_coordinator
                    .cleanup_task(owner.agent_id.as_str())
                    .is_none()
            );
        }
    }
    Ok(())
}
