//! A provider-owned native subagent reports its start and finish through the delegating turn and
//! its own content under a child scope. The host must place both in one order, so the subagent's
//! content always finds the record its start created, and the delegating turn completes.

use super::bootstrap::{initialize, service};
use super::owner_transition::{settle_exchanges, submit_input};
use crate::agent::{ObservedAgentAvailability, ObservedAgentState};
use crate::exchange::ExchangeOutcome;
use crate::provider::mock_response::MockResponseCommand;
use crate::timeline::TimelineEntry;
use anyhow::{Context, Result};
use forge_provider_api::agent::AgentOutcome;
use serde_json::json;
use std::time::Duration;

/// Bound a scenario that would otherwise hang when the awaited state never arrives.
const BOUND: Duration = Duration::from_secs(30);

/// Wait until a live native subagent has finished one of its own tools, so a stop that follows
/// interrupts a subagent that is visibly working.
async fn native_subagent_ran_a_tool(
    controller: &crate::service::SessionController,
    session_id: &str,
) -> Result<()> {
    loop {
        let running = crate::storage::HistoryReader::current(&controller.data_root)?
            .list_observed_agents(session_id)?
            .iter()
            .any(|agent| {
                matches!(agent.state(), ObservedAgentState::Active(_))
                    && agent.exchanges().iter().any(|exchange| {
                        exchange.turn.iter().any(|turn| {
                            exchange
                                .tools_for_turn(turn.id())
                                .any(|tool| tool.state() != crate::turn::ToolState::Running)
                        })
                    })
            });
        if running {
            return Ok(());
        }
        tokio::task::yield_now().await;
    }
}

/// Project one exchange's timeline entry with every fold open.
fn exchange_rows(timeline: &[TimelineEntry]) -> Result<Vec<String>> {
    let entry = timeline
        .iter()
        .find(|entry| matches!(entry, TimelineEntry::Exchange { .. }))
        .context("the delegating exchange has no timeline entry")?;
    crate::buffer::projection::tests::project_rows_expanded(entry)
}

/// A native subagent's start, its own turn and message, and its finish complete the delegating
/// turn and leave one finished child record with the subagent's exchange. Its row stays inside
/// the delegating exchange, before the answer that followed it.
#[tokio::test]
async fn native_subagent_content_follows_its_start_and_completes_the_turn() -> Result<()> {
    let root = tempfile::tempdir()?;
    crate::checks::configure_fixture(root.path());
    let service = service();
    service.open_session(1, initialize(&root, "mock")).await?;
    let registry = service.registry().await?;
    let session_id = registry.initial_session_id().to_owned();
    let controller = registry.resolve(&session_id).await?;
    let (sink, mut output) = forge_protocol::outbound::channel();
    let reader = tokio::spawn(async move {
        while output.recv().await?.is_some() {}
        Ok::<(), anyhow::Error>(())
    });
    controller.start_provider(&sink).await;
    controller.attach_input(&sink).await?;
    let scenario = async {
        controller
            .provider_handle()
            .mock_response_control()
            .context("mock controls missing")?
            .dispatch(
                &session_id,
                MockResponseCommand::Script {
                    script: serde_json::from_value(json!({
                        "steps":[
                            {"kind":"native_subagent","name":"search","text":"Found the parser."},
                            {"kind":"message","content":"response","text":"The parser is in src."},
                        ],
                        "terminal":"completed",
                    }))?,
                },
            )
            .await?;
        submit_input(
            &service,
            &controller,
            &session_id,
            10,
            "delegating-input",
            "Find the parser",
            &sink,
        )
        .await?;
        tokio::time::timeout(BOUND, settle_exchanges(&controller, &session_id, 1))
            .await
            .context("the delegating turn never settled")??;
        let store = crate::storage::SqliteStore::open(&controller.data_root)?;
        let exchanges = store.list_exchange(&session_id)?;
        assert_eq!(
            exchanges[0].finalization_outcome(),
            Some(ExchangeOutcome::Complete),
            "a native subagent failed its delegating turn"
        );
        let agents = store.list_observed_agents(&session_id)?;
        assert_eq!(agents.len(), 1, "one native subagent was observed");
        assert!(
            matches!(agents[0].state(), ObservedAgentState::Finished(_)),
            "the subagent kept a live state: {:?}",
            agents[0].state()
        );
        assert_eq!(agents[0].exchanges().len(), 1, "the subagent turn was lost");
        assert_eq!(
            agents[0].availability(),
            ObservedAgentAvailability::Available
        );
        // The delegating exchange's checkpoints own the subagent's edits, so the subagent's own
        // turn reports no missing checkpoints of its own.
        let projected = crate::buffer::projection::project(
            crate::buffer::projection::ProjectionSource::Exchange(&agents[0].exchanges()[0]),
            &forge_buffer::width::WidthProfile::default(),
            crate::buffer::projection::FIXTURE_NOW_MS,
            false,
            &Default::default(),
            &Default::default(),
            &mut Default::default(),
        )?;
        let rendered = projected
            .entry
            .block
            .iter()
            .flat_map(|block| block.text.wire_rows())
            .collect::<Vec<_>>()
            .join("\n");
        assert!(rendered.contains("Found the parser."), "{rendered}");
        assert!(!rendered.contains("Edits unavailable"), "{rendered}");
        // The subagent ran inside the turn's thought, so its row stays there once the turn
        // settles instead of moving below the answer.
        let rows = exchange_rows(&controller.observer.snapshot()?.timeline)?;
        let agent_row = rows
            .iter()
            .position(|row| row.contains("Native agent search · completed"))
            .with_context(|| format!("the exchange lost its native agent row: {rows:#?}"))?;
        let answer_row = rows
            .iter()
            .position(|row| row.contains("The parser is in src."))
            .with_context(|| format!("the exchange lost its answer: {rows:#?}"))?;
        assert!(
            agent_row < answer_row,
            "the native agent row moved below the answer: {rows:#?}"
        );
        Ok::<(), anyhow::Error>(())
    }
    .await;
    service.shutdown(Duration::from_secs(5)).await?;
    reader.abort();
    scenario
}

/// Cancelling the delegating turn ends a running native subagent: its row reads `cancelled`,
/// and nothing the subagent reports after the cancel request is drawn.
///
/// Copilot can finish a subagent just before the user's stop reaches it while the host still
/// holds the subagent's output queued, so the subagent's response and completed finish arrive
/// after the cancel request. Those observations belong to work the user stopped.
#[tokio::test]
async fn cancelled_parent_ends_its_native_subagent_and_discards_later_output() -> Result<()> {
    use crate::exchange::command::{CancelExchangeRequest, CancelledInputPolicy, Command};
    let root = tempfile::tempdir()?;
    crate::checks::configure_fixture(root.path());
    let service = service();
    service.open_session(1, initialize(&root, "mock")).await?;
    let registry = service.registry().await?;
    let session_id = registry.initial_session_id().to_owned();
    let controller = registry.resolve(&session_id).await?;
    let (sink, mut output) = forge_protocol::outbound::channel();
    let reader = tokio::spawn(async move {
        while output.recv().await?.is_some() {}
        Ok::<(), anyhow::Error>(())
    });
    controller.start_provider(&sink).await;
    controller.attach_input(&sink).await?;
    let scenario = async {
        controller
            .provider_handle()
            .mock_response_control()
            .context("mock controls missing")?
            .dispatch(
                &session_id,
                MockResponseCommand::Script {
                    script: serde_json::from_value(json!({
                        "steps":[
                            {"kind":"native_subagent","name":"search",
                             "text":"<final_answer>README.md:11-20</final_answer>",
                             "finishes_after_cancel":true},
                        ],
                        "terminal":"completed",
                    }))?,
                },
            )
            .await?;
        submit_input(
            &service,
            &controller,
            &session_id,
            10,
            "delegating-input",
            "Plan the change",
            &sink,
        )
        .await?;
        tokio::time::timeout(BOUND, native_subagent_ran_a_tool(&controller, &session_id))
            .await
            .context("the native subagent never ran")??;
        controller
            .exchange_owner
            .execute_command(Command::CancelExchange(CancelExchangeRequest {
                input: CancelledInputPolicy::Retain,
            }))
            .await?;
        tokio::time::timeout(BOUND, settle_exchanges(&controller, &session_id, 1))
            .await
            .context("the cancelled turn never settled")??;
        let store = crate::storage::SqliteStore::open(&controller.data_root)?;
        let agents = store.list_observed_agents(&session_id)?;
        assert_eq!(agents.len(), 1, "one native subagent was observed");
        assert_eq!(
            agents[0].state(),
            ObservedAgentState::Finished(AgentOutcome::Interrupted),
            "the cancel did not end the subagent"
        );
        assert_eq!(
            agents[0].availability(),
            ObservedAgentAvailability::Available,
            "the subagent's row reads unavailable instead of cancelled"
        );
        assert!(
            agents[0]
                .exchanges()
                .iter()
                .all(|exchange| exchange.is_finished()),
            "the subagent's turn kept running after the cancel"
        );
        let rows = exchange_rows(&controller.observer.snapshot()?.timeline)?;
        assert!(
            rows.iter()
                .any(|row| row.contains("Native agent search · cancelled")),
            "{rows:#?}"
        );
        assert!(
            !rows.iter().any(|row| row.contains("final_answer")),
            "the subagent's output after the cancel was drawn: {rows:#?}"
        );
        Ok::<(), anyhow::Error>(())
    }
    .await;
    service.shutdown(Duration::from_secs(5)).await?;
    reader.abort();
    scenario
}

/// `/plan cancel` during a planning turn ends the turn's running native subagent at the request,
/// exactly as an interrupt does: its row reads `cancelled`, and nothing the subagent reports
/// after the request is drawn.
///
/// The editor sends `/plan cancel` as the task action `clear`, which stops the planning turn
/// through task collection rather than through exchange cancellation. Copilot still delivers
/// the subagent's queued tools, its `<final_answer>` response, and its completed finish after
/// the stop, and those belong to work the user stopped.
#[tokio::test]
async fn plan_cancel_ends_the_planning_turns_native_subagent_at_the_request() -> Result<()> {
    use forge_protocol::message::Request;
    let root = tempfile::tempdir()?;
    crate::checks::configure_fixture(root.path());
    let service = service();
    service.open_session(1, initialize(&root, "mock")).await?;
    let registry = service.registry().await?;
    let session_id = registry.initial_session_id().to_owned();
    let controller = registry.resolve(&session_id).await?;
    let (sink, mut output) = forge_protocol::outbound::channel();
    let reader = tokio::spawn(async move {
        while output.recv().await?.is_some() {}
        Ok::<(), anyhow::Error>(())
    });
    controller.start_provider(&sink).await;
    controller.attach_input(&sink).await?;
    let task_action = |id: u64, params: serde_json::Value| {
        service.dispatch(
            Some(session_id.clone()),
            Request {
                id,
                method: "session.command.apply".into(),
                params,
            },
            &sink,
        )
    };
    let scenario = async {
        controller
            .provider_handle()
            .mock_response_control()
            .context("mock controls missing")?
            .dispatch(
                &session_id,
                MockResponseCommand::Script {
                    script: serde_json::from_value(json!({
                        "steps":[
                            {"kind":"native_subagent","name":"search",
                             "text":"<final_answer>src/money.ts:1-99</final_answer>",
                             "finishes_after_cancel":true},
                        ],
                        "terminal":"completed",
                    }))?,
                },
            )
            .await?;
        task_action(
            10,
            json!({"action":"plan","text":"Plan the change. Use your code search subagent first."}),
        )
        .await?;
        tokio::time::timeout(BOUND, native_subagent_ran_a_tool(&controller, &session_id))
            .await
            .context("the native subagent never ran")??;
        task_action(11, json!({"action":"clear"})).await?;
        tokio::time::timeout(BOUND, async {
            loop {
                settle_exchanges(&controller, &session_id, 1).await?;
                if controller.task_controller.current()?.is_none() {
                    return Ok::<(), anyhow::Error>(());
                }
                tokio::task::yield_now().await;
            }
        })
        .await
        .context("the cancelled plan never settled")??;
        let store = crate::storage::SqliteStore::open(&controller.data_root)?;
        let agents = store.list_observed_agents(&session_id)?;
        assert_eq!(agents.len(), 1, "one native subagent was observed");
        assert_eq!(
            agents[0].state(),
            ObservedAgentState::Finished(AgentOutcome::Interrupted),
            "the plan cancel did not end the subagent"
        );
        let rows = exchange_rows(&controller.observer.snapshot()?.timeline)?;
        assert!(
            rows.iter()
                .any(|row| row.contains("Native agent search · cancelled")),
            "{rows:#?}"
        );
        assert!(
            !rows.iter().any(|row| row.contains("final_answer")),
            "the subagent's output after the plan cancel was drawn: {rows:#?}"
        );
        Ok::<(), anyhow::Error>(())
    }
    .await;
    service.shutdown(Duration::from_secs(5)).await?;
    reader.abort();
    scenario
}
