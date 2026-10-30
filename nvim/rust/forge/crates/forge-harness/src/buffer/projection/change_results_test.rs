use super::*;
use crate::backend::{
    ProviderChangeKind, ProviderChangeSet, ProviderFileChange, TextUpdate, ToolActivityKind,
    ToolInvocation, ToolOutcome, ToolUpdate,
};
use serde_json::json;

fn exchange() -> Exchange {
    serde_json::from_value(json!({
        "id":"workspace", "session_id":"session", "agent_id":"primary", "ordinal":1,
        "prompt":"Edit", "state":"running", "created_at_ms":0,
        "attributed_matches_checkpoint":false, "node_list":[]
    }))
    .unwrap()
}

fn saved_patch(path: &str, content: &str) -> String {
    format!("--- /dev/null\n+++ b/{path}\n@@ -0,0 +1 @@\n+{content}\n")
}

fn render(exchange: &Exchange) -> ProjectedEntry {
    project(
        ProjectionSource::Exchange(exchange),
        &WidthProfile::default(),
        false,
        &HashMap::new(),
        &HashMap::new(),
        &mut Default::default(),
    )
    .unwrap()
}

fn text(projected: &ProjectedEntry) -> String {
    projected
        .entry
        .block
        .iter()
        .flat_map(|block| block.text.wire_rows())
        .collect::<Vec<_>>()
        .join(" ")
        .split_whitespace()
        .collect::<Vec<_>>()
        .join(" ")
}

fn capture(exchange: &mut Exchange, patch: String) {
    exchange.checkpoint_before = Some("before".into());
    exchange.checkpoint_after = Some("after".into());
    exchange.checkpoint_diff_text = Some(patch);
    exchange
        .finish(crate::exchange::ExchangeOutcome::Failed, 10)
        .unwrap();
}

#[test]
fn empty_workspace_comparisons_have_no_result_row() {
    let mut exchange = exchange();
    capture(&mut exchange, String::new());
    for phase in [
        exchange.phase.clone(),
        crate::exchange::ExchangePhase::Running,
    ] {
        exchange.phase = phase;
        let reopened: Exchange =
            serde_json::from_value(serde_json::to_value(&exchange).unwrap()).unwrap();
        let projected = render(&reopened);
        assert!(
            !projected
                .entry
                .block
                .iter()
                .any(|block| block.id.0 == "workspace:results:changes")
        );
        assert!(!text(&projected).contains("No workspace changes"));
        assert!(
            !projected
                .action
                .contains_key(&TargetId("workspace:results:changes".into()))
        );
    }
}

#[test]
fn workspace_results_ignore_incomplete_provider_reports() {
    let mut exchange = exchange();
    capture(
        &mut exchange,
        saved_patch("formatted.rs", "final formatted text"),
    );
    exchange.attributed_diff_text = Some(saved_patch("incorrect.rs", "provider text"));
    let reopened: Exchange =
        serde_json::from_value(serde_json::to_value(&exchange).unwrap()).unwrap();
    let projected = render(&reopened);
    assert!(text(&projected).contains("Workspace changes 1 file +1 -0"));
    assert!(!text(&projected).contains("incorrect.rs"));
    assert!(
        matches!(&projected.action[&TargetId("workspace:results:changes".into())],
        TranscriptAction::Diff { text, syntax: true, .. } if Some(text) == reopened.checkpoint_diff_text.as_ref())
    );
}

#[test]
fn workspace_results_render_pending_previous_and_failed_capture() {
    let mut exchange = exchange();
    for state in [
        crate::exchange::ExchangeState::Queued,
        crate::exchange::ExchangeState::Running,
        crate::exchange::ExchangeState::Finalizing,
    ] {
        exchange.phase = crate::exchange::ExchangePhase::fixture(state, None, None);
        let restored: Exchange =
            serde_json::from_value(serde_json::to_value(&exchange).unwrap()).unwrap();
        let projected = render(&restored);
        assert!(!text(&projected).contains("Workspace changes pending"));
        assert!(
            !projected
                .entry
                .block
                .iter()
                .any(|block| block.id.0 == "workspace:results:changes")
        );
    }
    exchange.phase = crate::exchange::ExchangePhase::Running;
    capture(&mut exchange, saved_patch("file.rs", "captured"));
    exchange.phase = crate::exchange::ExchangePhase::Running;
    let projected = render(&exchange);
    assert!(text(&projected).contains("previous capture · pending"));
    assert!(text(&projected).contains("file.rs"));
    exchange.finalization_error = Some("storage refused commit".into());
    assert!(
        text(&render(&exchange)).contains("previous capture · unavailable: storage refused commit")
    );
    exchange.checkpoint_diff_text = Some("not a patch".into());
    let malformed = render(&exchange);
    assert!(text(&malformed).contains("preview unavailable"));
    assert!(text(&malformed).contains("previous capture"));
    assert!(!text(&malformed).contains("No workspace changes"));
    exchange.checkpoint_diff_text = None;
    assert!(
        text(&render(&exchange)).contains("Workspace changes unavailable: storage refused commit")
    );
}

#[test]
fn reported_edit_groups_preserve_operation_order_and_failures() {
    let mut exchange = exchange();
    let address = crate::turn::test_support::address(&exchange, "turn");
    exchange.start_turn(address.clone(), 0).unwrap();
    for (index, status) in [
        "completed",
        "failed",
        "denied",
        "cancelled",
        "interrupted",
        "completed",
        "running",
    ]
    .into_iter()
    .enumerate()
    {
        let mut update = vec![
            ToolUpdate::Started,
            ToolUpdate::Output {
                update: TextUpdate::Replace(format!("operation-{index}")),
            },
            ToolUpdate::Changes {
                changes: if index == 5 {
                    ProviderChangeSet::default()
                } else {
                    ProviderChangeSet {
                        file: vec![ProviderFileChange {
                            path: "same.rs".into(),
                            move_path: None,
                            kind: ProviderChangeKind::Update,
                            diff: format!("@@ -1 +1 @@\n-before-{index}\n+after-{index}\n"),
                        }],
                    }
                },
            },
        ];
        let outcome = match status {
            "completed" => Some(ToolOutcome::Succeeded),
            "failed" => Some(ToolOutcome::Failed {
                diagnostic: crate::backend::ToolDiagnostic {
                    code: None,
                    message: "write failed".into(),
                },
            }),
            "denied" => Some(ToolOutcome::Denied {
                reason: "write denied".into(),
            }),
            "cancelled" => Some(ToolOutcome::Cancelled),
            "interrupted" => Some(ToolOutcome::Interrupted),
            _ => None,
        };
        if let Some(outcome) = outcome {
            update.push(ToolUpdate::Finished { outcome });
        }
        crate::turn::test_support::tool(
            &mut exchange,
            &address,
            &format!("write-{index}"),
            ToolInvocation {
                kind: ToolActivityKind::FileChange,
                name: "edit".into(),
                title: "edit".into(),
                arguments: serde_json::json!({}),
            },
            update,
            index as i64 + 1,
        )
        .unwrap();
    }
    let projected = render(&exchange);
    let rendered = text(&projected);
    assert!(rendered.contains("Reported edits · 7 operations · 1 running · 4 unsuccessful"));
    let mut last = 0;
    for expected in [
        "after-0",
        "File changes failed",
        "File changes denied",
        "File changes cancelled",
        "File changes interrupted",
        "details unavailable",
        "Changing 1 file",
    ] {
        let position = rendered
            .find(expected)
            .unwrap_or_else(|| panic!("missing {expected}: {rendered}"));
        assert!(position >= last);
        last = position;
    }
    assert!(!rendered.contains("net diff"));
    let identities = projected
        .entry
        .block
        .iter()
        .map(|block| &block.id)
        .collect::<std::collections::HashSet<_>>();
    assert_eq!(identities.len(), projected.entry.block.len());
}

#[test]
fn workspace_result_preserves_multi_page_content_and_path_identity() {
    let mut exchange = exchange();
    let patch = (0..300)
        .map(|index| saved_patch(&format!("file-{index:03}.txt"), &format!("value-{index}")))
        .collect::<String>();
    capture(&mut exchange, patch.clone());
    let initial = render(&exchange);
    assert!(text(&initial).contains("Workspace changes 300 files +300 -0"));
    assert!(text(&initial).contains("value-299"));
    let file_id = initial
        .entry
        .block
        .iter()
        .find(|block| block.text.wire_rows().join("").contains("file-299.txt"))
        .unwrap()
        .id
        .clone();
    exchange.checkpoint_diff_text = Some(saved_patch("earlier.txt", "new") + &patch);
    let replacement = render(&exchange);
    assert!(replacement.entry.block.iter().any(
        |block| block.id == file_id && block.text.wire_rows().join("").contains("file-299.txt")
    ));
    assert!(
        matches!(&replacement.action[&TargetId("workspace:results:changes".into())], TranscriptAction::Diff { text, .. } if text == exchange.checkpoint_diff_text.as_ref().unwrap())
    );
}
