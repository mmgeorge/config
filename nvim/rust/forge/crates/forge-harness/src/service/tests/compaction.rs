use super::owner_fixture::owner_transition_fixture;
use super::owner_transition::settle_exchanges;
use anyhow::{Context, Result};
use forge_protocol::message::Request;
use serde_json::json;
use std::time::Duration;

/// `/compact` during a turn stops the turn, compacts, and runs the turn again, and the
/// transcript records that order inside the one exchange.
///
/// The attempt was once a session-wide row: the interrupted exchange kept its `Stopping`
/// heading for the whole compaction, the replayed turn's output appeared above the compaction
/// row, and nothing said the turn ran again.
#[tokio::test]
async fn compaction_during_a_turn_is_recorded_inside_the_turn_it_restarted() -> Result<()> {
    let fixture = owner_transition_fixture().await?;
    let scenario = async {
        let command = super::super::compaction::decode_command(&Request {
            id: 40,
            method: "session.command.apply".into(),
            params: json!({"operation_id":"turn-compaction","action":"compact"}),
        })?;
        fixture
            .controller
            .exchange_owner
            .execute_identified(command, fixture.sink.clone())
            .await
            .map_err(|error| anyhow::anyhow!("compaction failed: {error}"))?;
        settle_exchanges(&fixture.controller, &fixture.session, 1).await?;
        let store = crate::storage::SqliteStore::open(&fixture.controller.data_root)?;
        let exchange = store
            .latest_exchange(&fixture.session)?
            .context("the interrupted exchange disappeared")?;
        let compaction_position = exchange
            .node_list
            .iter()
            .enumerate()
            .filter(|(_, node)| matches!(node, crate::exchange::ExchangeNode::Compaction { .. }))
            .map(|(position, _)| position)
            .collect::<Vec<_>>();
        assert_eq!(compaction_position.len(), 1, "{:?}", exchange.node_list);
        let last_content = exchange
            .node_list
            .iter()
            .rposition(|node| matches!(node, crate::exchange::ExchangeNode::TurnContent { .. }))
            .context("the replayed turn produced no content")?;
        assert!(
            compaction_position[0] < last_content,
            "the replayed turn's output precedes its compaction: {:?}",
            exchange.node_list
        );
        assert!(
            !store
                .list_session_event(&fixture.session)?
                .iter()
                .any(|event| matches!(
                    event.detail,
                    crate::timeline::SessionEventKind::Compaction { .. }
                )),
            "the compaction was also recorded as a session row"
        );
        let text = expanded_text(&exchange)?;
        assert_eq!(text.matches("Context compacted").count(), 1, "{text}");
        assert!(text.contains("turn ran again"), "{text}");
        assert!(!text.contains("Stopping"), "{text}");
        Ok::<(), anyhow::Error>(())
    }
    .await;
    fixture.service.shutdown(Duration::from_secs(5)).await?;
    fixture.reader.abort();
    scenario
}

/// Render one exchange with every fold open, as a reader expanding the transcript sees it.
fn expanded_text(exchange: &crate::exchange::Exchange) -> Result<String> {
    let mut expansion = std::collections::HashMap::new();
    let mut display = crate::buffer::identity::DisplayRegistry::default();
    loop {
        let projected = crate::buffer::projection::project(
            crate::buffer::projection::ProjectionSource::Exchange(exchange),
            &forge_buffer::width::WidthProfile::default(),
            exchange.created_at_ms,
            false,
            &expansion,
            &std::collections::HashMap::new(),
            &mut display,
        )?;
        let folded = projected
            .entry
            .block
            .iter()
            .filter_map(|block| block.metadata.node.as_ref())
            .filter(|node| node.display != forge_buffer::node::NodeDisplay::Full)
            .map(|node| node.id.0.clone())
            .filter(|id| !expansion.contains_key(id))
            .collect::<Vec<_>>();
        if folded.is_empty() {
            return Ok(projected
                .entry
                .block
                .iter()
                .flat_map(|block| block.text.wire_rows())
                .collect::<Vec<_>>()
                .join("\n"));
        }
        expansion.extend(folded.into_iter().map(|id| (id, true)));
    }
}
