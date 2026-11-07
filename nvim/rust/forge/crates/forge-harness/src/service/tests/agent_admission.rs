use super::super::{HarnessBroker, SessionController};
use super::bootstrap::initialize;
use crate::agent::{
    AgentCommit, AgentOperation, AgentRequest, AgentSelection, AgentSource, SpawnAgentRequest,
};
use crate::exchange::agent_command::{AgentCommand, AgentOutcome};
use crate::exchange::command::{Command, CommandId, CommandOutcome, CommandRequest, InputControl};
use anyhow::{Context, Result, ensure};
use std::time::Duration;

/// Keep public owner control available while a child-start or follow-up waits outside the FIFO.
#[tokio::test]
async fn owner_child_task_backpressure_preserves_public_interrupt_and_exact_admission() -> Result<()>
{
    for followup in [false, true] {
        saturated_child_admission(followup).await?;
    }
    Ok(())
}

/// Fill real coordinator task permits while synthetic child workers wait for cancellation.
async fn saturated_child_admission(followup: bool) -> Result<()> {
    let root = tempfile::tempdir()?;
    crate::checks::configure_fixture(root.path());
    let controller =
        SessionController::new(HarnessBroker::initialize(initialize(&root, "mock"))?).await?;
    let children = controller.agent_coordinator.clone();
    let child_provider = crate::provider::test_support::build_mock(
        controller.data_root.join("admission-child-provider"),
        controller.permission.clone(),
        controller.trace.clone(),
    )?;
    controller
        .broker
        .lock()
        .await
        .agent_provider_factory
        .register(child_provider.clone());
    let (sink, mut output) = forge_protocol::outbound::channel();
    let reader = tokio::spawn(async move {
        while output.recv().await?.is_some() {}
        Ok::<(), anyhow::Error>(())
    });
    let scenario = async {
        let mut active = Vec::new();
        for ordinal in 0..8 {
            let spawn = SpawnAgentRequest {
                profile: Some("worker".into()),
                text: "Held synthetic child".into(),
            };
            let request = AgentRequest {
                operation_id: format!("active-{ordinal}"),
                source: AgentSource::User,
                operation: AgentOperation::Spawn(spawn.clone()),
            };
            let selection = AgentSelection {
                profile: "worker".into(),
                instructions: String::new(),
                provider: forge_providers::registry::nonselectable_id()
                    .context("test provider missing")?,
                model: "mock-model".into(),
                reasoning_effort: "medium".into(),
                context_window: None,
                service_tier: crate::provider::ServiceTier::Standard,
                access: Default::default(),
                approval_mode: crate::session::ApprovalMode::Ask,
            };
            let work = match children
                .reserve_spawn(&request, &spawn, selection, "synthetic-parent", 1)?
                .commit()?
            {
                AgentCommit::Admitted(work) => work,
                AgentCommit::Existing(_) => anyhow::bail!("fresh active child reused an admission"),
            };
            active.push(work);
        }
        ensure!(children.active_count() == 8);
        let sender = children.sender();
        for ordinal in 0..1024 {
            let request = AgentRequest {
                operation_id: format!("pending-{ordinal}"),
                source: AgentSource::User,
                operation: AgentOperation::Followup {
                    target: active[0].agent.id.clone(),
                    message: "Queued work".into(),
                },
            };
            let operation = request.operation_id.clone();
            let (response, _response_waiter) = tokio::sync::oneshot::channel();
            sender
                .send(crate::agent::AgentCommand::Request {
                    request: Box::new(request),
                    authority: None,
                    target: None,
                    response,
                    task_admission: None,
                })
                .await?;
            let Some(crate::agent::AgentCommand::Request {
                request,
                task_admission,
                ..
            }) = children.try_receive()
            else {
                anyhow::bail!("seed request was not retained by the coordinator");
            };
            children
                .reserve_followup(
                    &request,
                    &active[0].agent.id,
                    "Queued work",
                    "synthetic-parent",
                    1,
                )?
                .commit()?;
            children.retain_task_admission(&operation, task_admission);
        }
        let operation = if followup {
            AgentOperation::Followup {
                target: active[1].agent.id.clone(),
                message: "Overflow follow-up".into(),
            }
        } else {
            AgentOperation::Spawn(SpawnAgentRequest {
                profile: Some("worker".into()),
                text: "Overflow child".into(),
            })
        };
        let id = CommandId::new("overflow-public-child")?;
        let request = CommandRequest {
            id: id.clone(),
            command: Command::Agent(AgentCommand::capture(operation, &children)?),
            input_control: InputControl::AtExecution,
        };
        let mut overflow = Box::pin(
            controller
                .exchange_owner
                .execute_identified(request.clone(), sink.clone()),
        );
        ensure!(futures_util::poll!(overflow.as_mut()).is_pending());
        ensure!(
            controller
                .exchange_owner
                .command_status(id.clone())
                .await?
                .is_none(),
            "capacity waiter occupied the parent FIFO before child admission"
        );
        ensure!(futures_util::poll!(overflow.as_mut()).is_pending());
        ensure!(
            controller
                .exchange_owner
                .command_status(id.clone())
                .await?
                .is_none(),
            "child capacity wait published an active parent command"
        );

        let mut cancelled = children.cancellation(&active[0].agent.id, &active[0].task.id)?;
        let settling = children.clone();
        let first_agent = active[0].agent.id.clone();
        let first_task = active[0].task.id.clone();
        let worker = tokio::spawn(async move {
            while !*cancelled.borrow() {
                cancelled.changed().await?;
            }
            settling.settled(
                &first_agent,
                &first_task,
                crate::agent::AgentSettlementDisposition::Ready,
            )?;
            Ok::<(), anyhow::Error>(())
        });
        controller.attach_agents(&sink).await?;
        let interrupt =
            controller
                .exchange_owner
                .execute_command(Command::Agent(AgentCommand::capture(
                    AgentOperation::Interrupt {
                        target: active[0].agent.id.clone(),
                    },
                    &children,
                )?));
        let (interrupted, admitted) = tokio::time::timeout(Duration::from_secs(10), async {
            tokio::join!(interrupt, overflow.as_mut())
        })
        .await
        .context("child saturation blocked public interruption or lost the waiting admission")?;
        let interrupted = interrupted?;
        let admitted = admitted?;
        worker.await??;
        ensure!(
            interrupted.sequence < admitted.sequence,
            "overflow task overtook the control that released capacity"
        );
        let CommandOutcome::Agent(AgentOutcome::Admitted { task, .. }) = &admitted.outcome else {
            anyhow::bail!("overflow request did not finish with one admitted task");
        };
        ensure!(task.operation_id == id.as_str());
        let replay = controller
            .exchange_owner
            .execute_identified(request, sink.clone())
            .await?;
        ensure!(
            replay.sequence == admitted.sequence && replay.revision == admitted.revision,
            "the resumed request was admitted a second time"
        );
        let CommandOutcome::Agent(AgentOutcome::Admitted { task: replayed, .. }) = replay.outcome
        else {
            anyhow::bail!("retained overflow outcome changed on replay");
        };
        ensure!(replayed.id == task.id);
        for work in active.into_iter().skip(1) {
            children.interrupt(&work.agent.id)?;
            children.settled(
                &work.agent.id,
                &work.task.id,
                crate::agent::AgentSettlementDisposition::Ready,
            )?;
        }
        Ok::<(), anyhow::Error>(())
    }
    .await;
    controller
        .input_stopping
        .store(true, std::sync::atomic::Ordering::Release);
    controller.coordinator.submissions.changed.notify_one();
    let teardown = async {
        controller.finish_exchange_owner().await?;
        if let Some(task) = controller.agent_task.lock().await.take() {
            task.abort();
        }
        controller.provider_handle().shutdown().await?;
        child_provider.shutdown().await?;
        controller.finish_session_events().await
    };
    let teardown = tokio::time::timeout(Duration::from_secs(10), teardown).await;
    reader.abort();
    scenario?;
    teardown.context("admission fixture teardown did not settle")??;
    Ok(())
}
