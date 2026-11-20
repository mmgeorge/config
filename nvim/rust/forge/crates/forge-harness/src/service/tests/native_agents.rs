//! A provider-owned native subagent reports its start and finish through the delegating turn and
//! its own content under a child scope. The host must place both in one order, so the subagent's
//! content always finds the record its start created, and the delegating turn completes.

use super::bootstrap::{initialize, service};
use super::owner_transition::{settle_exchanges, submit_input};
use crate::agent::{ObservedAgentAvailability, ObservedAgentState};
use crate::exchange::ExchangeOutcome;
use crate::provider::mock_response::MockResponseCommand;
use anyhow::{Context, Result};
use serde_json::json;
use std::time::Duration;

/// Bound a scenario that would otherwise hang when the awaited state never arrives.
const BOUND: Duration = Duration::from_secs(30);

/// A native subagent's start, its own turn and message, and its finish complete the delegating
/// turn and leave one finished child record with the subagent's exchange.
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
        Ok::<(), anyhow::Error>(())
    }
    .await;
    service.shutdown(Duration::from_secs(5)).await?;
    reader.abort();
    scenario
}
