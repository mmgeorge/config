use super::super::{AgentState, ExchangeState, TurnCancelled};
use super::test_support::{
    admit_child_fixture, child_address, child_completed, child_event, child_tool,
    planning_question_broker, repository,
};
use anyhow::{Context, Result};

#[derive(Clone, Copy)]
enum ChildCompletionFailure {
    CleanupFailed,
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
            .cancellation(owner.agent_id().as_str(), &admission.task.id)?
            .borrow()
    );
    let mut event = Vec::new();
    broker
        .observe_child_fixture(
            owner.clone(),
            &admission.task.id,
            child_event(
                &child_address(&(owner.clone()), &admission.task.id),
                crate::provider::TurnEvent::Started(forge_provider_api::turn::TurnStarted {
                    started_at_ms: 1,
                }),
            ),
            &mut event,
        )
        .await?;
    broker
        .observe_child_fixture(
            owner.clone(),
            &admission.task.id,
            child_tool(
                &child_address(&(owner.clone()), &admission.task.id),
                "policy denied".into(),
                crate::provider::ToolStatus::Denied,
            ),
            &mut event,
        )
        .await?;
    let (output, cleanup_error, expected) = match failure {
        ChildCompletionFailure::CleanupFailed => (
            Err(anyhow::Error::new(TurnCancelled)),
            Some("cleanup transport failed".into()),
            ExchangeState::Failed,
        ),
    };
    let turn_outcome = match failure {
        ChildCompletionFailure::CleanupFailed => crate::turn::TurnOutcome::Interrupted,
    };
    broker
        .observe_child_fixture(
            owner.clone(),
            &admission.task.id,
            child_completed(
                &child_address(&(owner.clone()), &admission.task.id),
                turn_outcome,
                2,
            ),
            &mut event,
        )
        .await?;
    broker
        .complete_managed_agent(
            owner.clone(),
            admission.task.id.clone(),
            output,
            Vec::new(),
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
    let agent = broker
        .agent_coordinator
        .resolve(owner.agent_id().as_str())?;
    match failure {
        ChildCompletionFailure::CleanupFailed => {
            assert_eq!(agent.state, AgentState::Closing);
            assert_eq!(task.error.as_deref(), Some("cleanup transport failed"));
            assert!(
                broker
                    .agent_coordinator
                    .cleanup_task(owner.agent_id().as_str())
                    .is_some()
            );
        }
    }
    Ok(())
}
