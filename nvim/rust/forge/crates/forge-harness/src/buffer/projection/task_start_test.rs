use super::*;
use crate::backend::{BackendEvent, ProviderAddress, ToolActivity, ToolActivityKind};
use crate::exchange::{ExchangeOrigin, TaskStart};
use crate::timeline::{SessionEventRecord, TimelineEvent, TimelineOwner};
use serde_json::json;

#[test]
fn wrapped_events_start_folded_and_retain_complete_text_and_actions() -> Result<()> {
    for (columns, title) in [
        (100, "a simple bevy game with 2 systems and 5 components"),
        (28, "a simple bevy game with 2 systems and 5 components"),
        (100, "a simple bevy game\nwith 2 systems and 5 components"),
    ] {
        let width = WidthProfile {
            columns,
            ..Default::default()
        };
        for detail in [
            SessionEventKind::TaskStarted {
                start: TaskStart {
                    kind: crate::task::TaskKind::Plan,
                    title: title.into(),
                },
                prompt: "Keep the task prompt visible.".into(),
            },
            SessionEventKind::Forked {
                source_session_id: "source".into(),
                source_session_name: title.into(),
            },
        ] {
            let entry = TimelineEntry::SessionEvent {
                id: "event".into(),
                created_at_ms: 0,
                event: SessionEventRecord {
                    id: "event".into(),
                    session_id: "session".into(),
                    owner: TimelineOwner::session("session"),
                    created_at_ms: 0,
                    detail,
                },
            };
            let projected = project_at(&entry, &width, 1)?;
            let block = &projected.entry.block[0];
            let multiline = block.text.row_count() > 1;
            let marker = &block
                .metadata
                .layout
                .as_ref()
                .unwrap()
                .marker
                .as_ref()
                .unwrap()
                .text;
            assert_eq!(marker, if multiline { "▸" } else { "◇" });
            assert_eq!(block.metadata.fold.len(), usize::from(multiline));
            if multiline {
                let fold = &block.metadata.fold[0];
                assert!(fold.closed);
                assert_eq!(fold.end.position.row, block.text.row_count());
                assert_eq!(fold.collapsed_suffix, None);
                assert_eq!(
                    block.metadata.node.as_ref().unwrap().default_display,
                    NodeDisplay::Heading
                );
                assert!(block.text.wire_rows().join(" ").contains("components"));
            }
            if block.id.0 == "event" {
                assert!(
                    matches!(projected.action.get(&TargetId("event".into())), Some(TranscriptAction::Session { session_id }) if session_id == "source")
                );
                assert_eq!(
                    block.metadata.target[0].range.end.row,
                    block.text.row_count()
                );
            } else {
                assert_eq!(projected.entry.block.len(), 1);
                assert!(
                    block
                        .text
                        .wire_rows()
                        .join(" ")
                        .split_whitespace()
                        .collect::<Vec<_>>()
                        .join(" ")
                        .contains("Keep the task prompt visible.")
                );
                assert_eq!(projected.prompt, vec![block.id.clone()]);
            }
        }
    }
    Ok(())
}

#[test]
fn task_start_bodies_fold_with_their_heading_across_task_kinds() -> Result<()> {
    use crate::buffer::document::TranscriptDocument;
    use crate::buffer::sections::{SectionProjection, SectionRequest};
    use forge_buffer::identity::{DocumentId, ViewId};

    for kind in [
        crate::task::TaskKind::Plan,
        crate::task::TaskKind::Goal,
        crate::task::TaskKind::Execute,
    ] {
        for prompt in [
            "",
            "Active declaration design:\n```json\n{\"plan_id\":\"retained-plan\",\"requirements\":[]}\n```\nPlanning instructions remain available when expanded.",
        ] {
            let entry = TimelineEntry::SessionEvent {
                id: "task".into(),
                created_at_ms: 0,
                event: SessionEventRecord {
                    id: "task".into(),
                    session_id: "session".into(),
                    owner: TimelineOwner::session("session"),
                    created_at_ms: 0,
                    detail: SessionEventKind::TaskStarted {
                        start: TaskStart {
                            kind,
                            title: "a simple bevy game with 2 systems and 5 components".into(),
                        },
                        prompt: prompt.into(),
                    },
                },
            };
            let restored: TimelineEntry = serde_json::from_value(serde_json::to_value(entry)?)?;
            let projected = project_at(
                &restored,
                &WidthProfile {
                    columns: 140,
                    ..Default::default()
                },
                1,
            )?;
            assert_eq!(projected.entry.block.len(), 1);
            let block = &projected.entry.block[0];
            if prompt.is_empty() {
                assert_eq!(block.text.row_count(), 1);
                assert!(block.metadata.fold.is_empty());
                assert!(block.text.row(0).unwrap().starts_with("◇ "));
                continue;
            }
            let expected = block.text.clone();
            assert!(!expected.row(0).unwrap().contains("Active declaration"));
            let mut source = TranscriptDocument::initialize(
                DocumentId("source".into()),
                "session".into(),
                0,
                vec![projected.entry],
            )?;
            let mut visible = SectionProjection::new(&mut source, DocumentId("visible".into()))?;
            let view = ViewId("view".into());
            visible.register(view.clone());
            for (sequence, expanded) in [(0, false), (1, true), (2, false)] {
                if sequence != 0 {
                    visible.set(
                        &mut source,
                        SectionRequest {
                            view: view.clone(),
                            sequence,
                            id: "task:start",
                            expanded,
                            more: false,
                            page: None,
                        },
                    )?;
                    visible.refresh(&mut source)?;
                }
                let snapshot = visible.document.snapshot()?;
                assert_eq!(snapshot.block.len(), 1);
                let displayed = &snapshot.block[0];
                assert_eq!(displayed.metadata.fold[0].closed, !expanded);
                if expanded {
                    assert_eq!(displayed.text, expected);
                } else {
                    assert_eq!(displayed.text.row_count(), 1);
                    assert_eq!(displayed.text.row(0), expected.row(0));
                }
            }
        }
    }
    Ok(())
}

#[test]
fn task_start_matches_idle_and_active_presentation() -> Result<()> {
    let start = TaskStart {
        kind: crate::task::TaskKind::Goal,
        title: "Reduce startup time".into(),
    };
    let prompt = "Complete the startup work.\nVerify the result.";
    let mut idle: Exchange = serde_json::from_value(json!({
        "id":"idle", "session_id":"session", "agent_id":"primary", "ordinal":1,
        "prompt":prompt, "state":"running", "created_at_ms":0,
        "attributed_matches_checkpoint":false, "node_list":[]
    }))?;
    idle.origin = ExchangeOrigin::TaskStarted {
        start: start.clone(),
    };
    let idle = project_at(
        &TimelineEntry::Exchange {
            id: idle.id.clone(),

            created_at_ms: 0,
            exchange: (idle).into(),

            agent_by_id: HashMap::new(),
        },
        &WidthProfile::default(),
        10,
    )?;

    for before_commentary in [false, true] {
        let mut active: Exchange = serde_json::from_value(json!({
            "id":"active", "session_id":"session", "agent_id":"primary", "ordinal":1,
            "prompt":"Inspect startup", "state":"running", "created_at_ms":0,
            "attributed_matches_checkpoint":false, "node_list":[]
        }))?;
        let address = ProviderAddress {
            owner: None,
            thread_id: "thread".into(),
            turn_id: "turn".into(),
        };
        active.start_turn(address.clone(), 0)?;
        let commentary = BackendEvent {
            received_at_ms: None,
            address: Some(address.clone()),
            turn_boundary: None,
            kind: "assistant_message".into(),
            text: Some("Inspecting startup".into()),
            data: json!({"provider_message_id":"commentary","phase":"commentary"}),
            activity: None,
            summary: None,
            checklist_update: None,
        };
        if !before_commentary {
            active.observe_turn(&commentary, 1)?;
        }
        let event = TimelineEvent::Session {
            event: Box::new(SessionEventRecord {
                id: "task-entry".into(),
                session_id: "session".into(),
                owner: TimelineOwner::exchange(&active),
                created_at_ms: 2,
                detail: SessionEventKind::TaskStarted {
                    start: start.clone(),
                    prompt: prompt.into(),
                },
            }),
        };
        if before_commentary {
            active.observe_turn(&commentary, 3)?;
        }
        active.observe_turn(
            &BackendEvent {
                received_at_ms: None,
                address: Some(address),
                turn_boundary: None,
                kind: "tool".into(),
                text: None,
                data: serde_json::Value::Null,
                activity: Some(ToolActivity {
                    id: "inspect".into(),
                    kind: ToolActivityKind::Command,
                    title: "Inspect files".into(),
                    output: None,
                    output_delta: false,
                    status: Some("completed".into()),
                    change: Default::default(),
                }),
                summary: None,
                checklist_update: None,
            },
            4,
        )?;
        crate::timeline::ownership::insert_event(&mut active, event)?;
        active.reconcile_entries()?;
        let nodes = active.node_list.iter().collect::<Vec<_>>();
        let content = active.content(&nodes, true)?;
        assert_eq!(content.commentary.len(), 1);
        assert_eq!(content.commentary[0].tools.call.len(), 1);
        let active = project_at(
            &TimelineEntry::Exchange {
                id: active.id.clone(),

                created_at_ms: 0,
                exchange: (active).into(),

                agent_by_id: HashMap::new(),
            },
            &WidthProfile::default(),
            10,
        )?;
        for (idle_id, active_id) in [("idle:start", "task-entry:start")] {
            let idle_block = idle
                .entry
                .block
                .iter()
                .find(|block| block.id.0 == idle_id)
                .unwrap();
            let active_block = active
                .entry
                .block
                .iter()
                .find(|block| block.id.0 == active_id)
                .unwrap();
            assert_eq!(idle_block.text.wire_rows(), active_block.text.wire_rows());
            assert_eq!(active_block.metadata.fold.len(), 1);
            assert!(active_block.metadata.fold[0].closed);
            assert_eq!(
                active_block.metadata.node.as_ref().unwrap().default_display,
                NodeDisplay::Heading
            );
            for line in prompt.lines() {
                assert!(
                    active_block
                        .text
                        .wire_rows()
                        .iter()
                        .any(|row| row.trim() == line)
                );
            }
            assert_eq!(
                active_block
                    .metadata
                    .layout
                    .as_ref()
                    .map_or(0, |layout| layout.indent),
                idle_block
                    .metadata
                    .layout
                    .as_ref()
                    .map_or(0, |layout| layout.indent)
                    + 2
            );
        }
        let text = active
            .entry
            .block
            .iter()
            .flat_map(|block| block.text.wire_rows())
            .collect::<Vec<_>>()
            .join("\n");
        assert!(
            text.contains("▸ Goal started: Reduce startup time"),
            "{text}"
        );
        assert!(text.contains(prompt.lines().next().unwrap()), "{text}");
        assert!(
            !text.contains("Steering:") && !text.contains("/goal"),
            "{text}"
        );
    }
    Ok(())
}
