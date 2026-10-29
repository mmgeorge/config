use super::*;
use crate::backend::{BackendEvent, ProviderAddress, ToolActivity, ToolActivityKind};
use crate::exchange::{ExchangeOrigin, TaskStart};
use crate::timeline::{SessionEventRecord, TimelineEvent, TimelineOwner};
use serde_json::json;

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
        let content = active.content(&nodes)?;
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
        for (idle_id, active_id) in [
            ("idle:start", "task-entry:start"),
            ("idle:prompt", "task-entry:prompt"),
        ] {
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
            if idle_id == "idle:prompt" {
                assert_eq!(
                    idle_block.text.wire_rows(),
                    prompt.lines().collect::<Vec<_>>()
                );
            }
            assert!(active_block.metadata.fold.is_empty());
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
            text.contains("◇ Goal started: Reduce startup time"),
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
