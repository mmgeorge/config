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

/// A provider that declines `/compact` during a turn still hands the turn back: the turn the
/// compaction stopped runs again, the command reports the provider's words, and the exchange
/// settles instead of resting paused.
///
/// The declined attempt once failed the command after the turn had been stopped, so nothing
/// resumed it; the exchange read `Paused` until the next prompt replaced it.
#[tokio::test]
async fn declined_compaction_during_a_turn_runs_the_stopped_turn_again() -> Result<()> {
    let fixture = owner_transition_fixture().await?;
    let scenario = async {
        fixture
            .provider
            .mock_response_control()
            .context("fixture controls missing")?
            .dispatch(
                &fixture.session,
                crate::provider::mock_response::MockResponseCommand::DeclineCompaction {
                    explanation: "Not enough messages to compact.".into(),
                },
            )
            .await?;
        let command = super::super::compaction::decode_command(&Request {
            id: 41,
            method: "session.command.apply".into(),
            params: json!({"operation_id":"declined-compaction","action":"compact"}),
        })?;
        let response = fixture
            .controller
            .exchange_owner
            .execute_identified(command, fixture.sink.clone())
            .await;
        let failure = match response {
            Ok(_) => anyhow::bail!("a declined compaction reported success"),
            Err(error) => error.to_string(),
        };
        assert!(
            failure.contains("Not enough messages to compact."),
            "{failure}"
        );
        // Execution is idle only once nothing holds or waits for the turn; an exchange left
        // paused then never runs again.
        loop {
            if fixture.controller.coordinator.execution.available_permits() == 1
                && fixture
                    .controller
                    .coordinator
                    .submissions
                    .snapshot()?
                    .messages
                    .is_empty()
            {
                break;
            }
            tokio::task::yield_now().await;
        }
        let store = crate::storage::SqliteStore::open(&fixture.controller.data_root)?;
        let exchange = store
            .latest_exchange(&fixture.session)?
            .context("the interrupted exchange disappeared")?;
        let compaction_started_at_ms = exchange
            .node_list
            .iter()
            .find_map(|node| match node {
                crate::exchange::ExchangeNode::Compaction { compaction, .. } => {
                    Some(compaction.started_at_ms)
                }
                _ => None,
            })
            .flatten()
            .context("the declined attempt is not recorded inside the exchange it stopped")?;
        assert!(
            exchange.is_finished(),
            "the stopped turn never ran again: {:?} {:?}",
            exchange.state(),
            exchange.activity()
        );
        assert_eq!(exchange.state(), crate::exchange::ExchangeState::Complete);
        assert!(
            exchange
                .turn
                .iter()
                .any(|turn| turn.started_at_ms() >= compaction_started_at_ms),
            "no turn started after the declined compaction"
        );
        Ok::<(), anyhow::Error>(())
    }
    .await;
    fixture.service.shutdown(Duration::from_secs(5)).await?;
    fixture.reader.abort();
    scenario
}

/// A `/compact` accepted while other owner work holds session control keeps the session working
/// until its attempt starts.
///
/// A task's check run holds session control, and a compaction requested during the checks waits
/// for it. The run settled to an idle phase before the compaction started, so the editor closed
/// the status split for a frame between the checks' status and the compaction's. The test holds
/// control itself, as that run does, so no check command runs.
#[tokio::test]
async fn compaction_waiting_for_session_control_never_publishes_idle() -> Result<()> {
    use super::bootstrap::{initialize, service};
    use crate::input::{InputAction, InputIntent, InputMessage, InputOperation};
    let root = tempfile::tempdir()?;
    let service = service();
    service.open_session(1, initialize(&root, "mock")).await?;
    let registry = service.registry().await?;
    let session = registry.initial_session_id().to_owned();
    let controller = registry.resolve(&session).await?;
    let (sink, mut output) = forge_protocol::outbound::channel();
    let published = std::sync::Arc::new(std::sync::Mutex::new(Vec::<String>::new()));
    let recorded = published.clone();
    let reader = tokio::spawn(async move {
        while let Some(frame) = output.recv().await? {
            let event: serde_json::Value = serde_json::from_slice(frame.bytes())?;
            if event["event"] == "document_changed"
                && let Some(kind) = event["payload"]["status"]["kind"].as_str()
            {
                let phase = event["payload"]["status"]["activity"]
                    .as_str()
                    .filter(|activity| *activity == "handing_off")
                    .unwrap_or(kind);
                recorded.lock().unwrap().push(phase.to_owned());
            }
        }
        Ok::<(), anyhow::Error>(())
    });
    controller.attach_input(&sink).await?;
    let scenario = async {
        // One finished turn gives the provider a conversation to compact.
        controller.coordinator.submissions.apply(InputOperation {
            id: "operation-first".into(),
            control_revision: 0,
            after_control: None,
            action: InputAction::Submit {
                message: Box::new(InputMessage::queued(crate::input::InputDraft {
                    id: "first".into(),
                    text: "input first".into(),
                    purpose: crate::input::InputPurpose::User,
                    intent: InputIntent::Auto,
                    attachments: vec![],
                    config: None,
                })),
            },
        })?;
        let control = loop {
            let settled = crate::storage::SqliteStore::read_only(&controller.data_root)?
                .list_exchange(&session)?
                .iter()
                .any(|exchange| exchange.is_finished());
            if settled
                && controller.coordinator.submissions.snapshot()?.messages.is_empty()
                && let Ok(control) = controller.coordinator.control.clone().try_acquire_owned()
            {
                break control;
            }
            tokio::task::yield_now().await;
        };
        let idle_before = published.lock().unwrap().len();
        let command = super::super::compaction::decode_command(&Request {
            id: 43,
            method: "session.command.apply".into(),
            params: json!({"operation_id":"waiting-compaction","action":"compact"}),
        })?;
        let owner = controller.clone();
        let command_sink = sink.clone();
        let compaction = tokio::spawn(async move {
            owner
                .exchange_owner
                .execute_identified(command, command_sink)
                .await
                .map_err(|error| anyhow::anyhow!("compaction failed: {error}"))
        });
        let database = crate::storage::test::raw_connection(&controller.data_root)?;
        loop {
            let active: bool = database.query_row(
                "SELECT EXISTS(SELECT 1 FROM owner_command WHERE json_extract(request,'$.command.command')='compact_context' AND json_extract(state,'$.state')='active')",
                [],
                |row| row.get(0),
            )?;
            if active {
                break;
            }
            tokio::task::yield_now().await;
        }
        let waiting = controller.observer.snapshot()?.status;
        assert!(
            matches!(
                waiting,
                crate::session::state_machine::SessionPhase::Working {
                    activity: crate::session::state_machine::WorkflowActivity::HandingOff,
                    started_at_ms: None,
                    ..
                }
            ),
            "a compaction waiting for session control read {waiting:?}"
        );
        // The idle status published before the command must be replaced while it waits.
        while !published.lock().unwrap()[idle_before..]
            .iter()
            .any(|phase| phase == "handing_off")
        {
            tokio::task::yield_now().await;
        }
        drop(control);
        compaction.await??;
        let phases = loop {
            let phases = published.lock().unwrap()[idle_before..].to_vec();
            if phases.last().is_some_and(|phase| phase == "idle") {
                break phases;
            }
            tokio::task::yield_now().await;
        };
        // The mock provider compacts at once, so its `Compacting` phase may be superseded
        // before publication; the session is idle only once the compaction has ended.
        assert_eq!(
            phases.first().map(String::as_str),
            Some("handing_off"),
            "{phases:?}"
        );
        assert!(
            phases[..phases.len() - 1].iter().all(|phase| phase != "idle"),
            "the session published an idle phase while its compaction waited: {phases:?}"
        );
        Ok::<(), anyhow::Error>(())
    }
    .await;
    service.shutdown(Duration::from_secs(5)).await?;
    drop(sink);
    reader.abort();
    scenario
}

/// The turn a compaction stopped has no live execution, but the compaction is the work in
/// progress, so the exchange heading reads `Compacting` as the status row does rather than
/// `Paused` for the whole compaction.
#[test]
fn a_compaction_that_stopped_the_turn_heads_its_exchange() -> Result<()> {
    let mut exchange = crate::storage::test::running_exchange("compacting");
    let address = crate::turn::test_support::address(&exchange, "turn");
    exchange.start_turn(address, 0)?;
    exchange.pause_for_restart(1_000)?;
    exchange.observe_compaction(
        root_compaction(
            crate::provider::CompactionState::Running,
            Default::default(),
        ),
        1_000,
    );
    let text = expanded_text(&exchange)?;
    assert!(text.contains("Compacting"), "{text}");
    assert!(!text.contains("Paused"), "{text}");
    // The session status says the stopped turn waits for the compaction, which tells the editor
    // a turn still owns the work.
    assert_eq!(
        crate::session::state_machine::SessionPhase::resolve_status(None, None, Some(&exchange)),
        crate::session::state_machine::SessionPhase::Compacting {
            started_at_ms: 1_000,
            turn_waiting: true,
        }
    );
    Ok(())
}

/// While a compaction holds the turn it stopped, the heading counts the compaction as the
/// status row does, and keeps reading `Compacting` until the stopped turn runs again.
///
/// The heading once showed the stopped turn's frozen clock (`Compacting 1s` while the status
/// counted to 13 s), then read `Paused` between the compaction's end and the turn's restart.
#[test]
fn a_compaction_heading_counts_with_the_status_until_the_turn_runs_again() -> Result<()> {
    let mut exchange = crate::storage::test::running_exchange("compacting");
    let address = crate::turn::test_support::address(&exchange, "turn");
    exchange.start_turn(address, 0)?;
    exchange.pause_for_restart(1_000)?;
    exchange.observe_compaction(
        root_compaction(
            crate::provider::CompactionState::Running,
            Default::default(),
        ),
        1_000,
    );
    let heading = heading_at(&exchange, 14_000)?;
    assert!(heading.contains("Compacting 13s"), "{heading}");
    exchange.observe_compaction(
        root_compaction(
            crate::provider::CompactionState::Completed,
            Default::default(),
        ),
        4_000,
    );
    let heading = heading_at(&exchange, 5_000)?;
    assert!(heading.contains("Compacting 3s"), "{heading}");
    assert!(!heading.contains("Paused"), "{heading}");
    let address = crate::turn::test_support::address(&exchange, "replay");
    exchange.start_turn(address, 5_000)?;
    let heading = heading_at(&exchange, 6_000)?;
    assert!(heading.contains("Thinking"), "{heading}");
    assert!(!heading.contains("Compacting"), "{heading}");
    Ok(())
}

/// Presentation time the test sets explicitly.
struct ManualClock(std::sync::atomic::AtomicI64);

impl crate::broker::Clock for ManualClock {
    fn now_ms(&self) -> i64 {
        self.0.load(std::sync::atomic::Ordering::Relaxed)
    }
}

/// The editor's periodic synchronization advances the `Compacting` heading as it advances a
/// running turn's, with no host event between one second and the next.
///
/// The heading once read `Compacting 0s` for the whole compaction while the status row counted
/// up: a fresh projection gave the right figure, but the per-second refresh skipped every
/// exchange without a running execution clock, and a compaction stops that clock.
#[test]
fn a_compaction_heading_advances_on_each_periodic_sync() -> Result<()> {
    let mut exchange = crate::storage::test::running_exchange("compacting");
    let address = crate::turn::test_support::address(&exchange, "turn");
    exchange.start_turn(address, 0)?;
    exchange.pause_for_restart(1_000)?;
    exchange.observe_compaction(
        root_compaction(
            crate::provider::CompactionState::Running,
            Default::default(),
        ),
        1_000,
    );
    let clock = std::sync::Arc::new(ManualClock(std::sync::atomic::AtomicI64::new(1_000)));
    let mut presentation =
        crate::buffer::session::SessionPresentation::with_clock("session".into(), clock.clone());
    presentation.initialize(vec![
        crate::timeline::TimelineEntry::Exchange {
            id: exchange.id.clone(),
            created_at_ms: exchange.created_at_ms,
            exchange: exchange.clone().into(),
            agent_by_id: Default::default(),
        },
        crate::timeline::TimelineEntry::Status {
            id: "session:status".into(),
            created_at_ms: 0,
            status: crate::session::state_machine::SessionPhase::Compacting {
                started_at_ms: 1_000,
                turn_waiting: true,
            },
            status_thoughts: false,
            agents: Vec::new(),
            checks: None,
        },
    ])?;
    let document = forge_buffer::identity::DocumentId("compaction-heading".into());
    let opened = presentation.open(
        document.clone(),
        forge_buffer::identity::ViewId("compaction-view".into()),
        forge_buffer::width::WidthProfile::default(),
    )?;
    let mut replica = opened.transcript;
    let mut samples = Vec::new();
    for now_ms in [2_000, 3_000, 6_500] {
        clock.0.store(now_ms, std::sync::atomic::Ordering::Relaxed);
        let synced = presentation.sync(&document, replica.revision)?;
        if let Some(snapshot) = synced.snapshot {
            replica = snapshot;
        }
        for patch in &synced.patch {
            replica = forge_buffer::patch::apply_snapshot_reference(&replica, patch)?;
        }
        let heading_id = format!("{}:summary", exchange.id);
        let heading = replica
            .block
            .iter()
            .find(|block| block.id.0 == heading_id)
            .and_then(|block| block.text.wire_rows().first().map(|row| (*row).to_owned()))
            .context("the exchange has no heading")?;
        let status = synced.status.block[0].text.wire_rows()[0].to_owned();
        samples.push((heading, status, synced.activity_due_in_ms.is_some()));
    }
    assert_eq!(
        samples
            .iter()
            .map(|(heading, _, _)| heading.split(" │ ").next().unwrap_or_default())
            .collect::<Vec<_>>(),
        ["● Compacting 1s", "● Compacting 2s", "● Compacting 5s"],
        "{samples:#?}"
    );
    assert!(
        samples
            .iter()
            .all(|(_, status, due)| status.contains("Compacting") && *due),
        "{samples:#?}"
    );
    Ok(())
}

/// A compaction row compares two figures of one measure taken at the compaction. The context
/// meter Harness captured when a compaction stopped a turn predates that turn's own context,
/// so beside the provider's post-compaction count it read as a compaction that grew the
/// context; the provider's own before and after counts are the like-for-like pair.
#[test]
fn a_compaction_row_compares_the_provider_counts_it_reports() -> Result<()> {
    let mut exchange = crate::storage::test::running_exchange("compacted");
    exchange.observe_compaction(
        root_compaction(
            crate::provider::CompactionState::Completed,
            crate::provider::CompactionResult {
                context_tokens_before: Some(7_000),
                context_tokens_after: Some(10_000),
                pre_compaction_tokens: Some(31_000),
                post_compaction_tokens: Some(10_000),
                ..Default::default()
            },
        ),
        1_000,
    );
    let text = expanded_text(&exchange)?;
    assert!(text.contains("31.0k → 10.0k tokens"), "{text}");
    assert!(!text.contains("7.0k"), "{text}");
    Ok(())
}

/// Build a manual compaction of the fixture session's root conversation.
fn root_compaction(
    state: crate::provider::CompactionState,
    result: crate::provider::CompactionResult,
) -> crate::provider::Compaction {
    crate::provider::Compaction {
        id: "compaction".into(),
        thread_id: crate::provider::ThreadOwner::root("session")
            .thread_id()
            .to_string(),
        state,
        manual: true,
        started_at_ms: Some(1_000),
        completed_at_ms: (state != crate::provider::CompactionState::Running).then_some(4_000),
        result,
    }
}

/// Render the exchange's rows other than its compaction rows as they read at `now_ms`.
fn heading_at(exchange: &crate::exchange::Exchange, now_ms: i64) -> Result<String> {
    let projected = crate::buffer::projection::project(
        crate::buffer::projection::ProjectionSource::Exchange(exchange),
        &forge_buffer::width::WidthProfile::default(),
        now_ms,
        false,
        &std::collections::HashMap::new(),
        &std::collections::HashMap::new(),
        &mut crate::buffer::identity::DisplayRegistry::default(),
    )?;
    Ok(projected
        .entry
        .block
        .iter()
        .flat_map(|block| block.text.wire_rows())
        .filter(|row| !row.starts_with('◇'))
        .collect::<Vec<_>>()
        .join(
            "
",
        ))
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
