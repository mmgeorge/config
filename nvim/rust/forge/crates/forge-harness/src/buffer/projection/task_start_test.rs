use super::*;
use crate::exchange::TaskStart;
use crate::timeline::{SessionEventRecord, TimelineOwner};

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
