use super::*;
use crate::provider::{
    ProviderChangeKind, ProviderChangeSet, ProviderFileChange, TextUpdate, ToolOutcome,
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

fn render(exchange: &Exchange) -> ProjectedEntry {
    project(
        ProjectionSource::Exchange(exchange),
        &WidthProfile::default(),
        crate::buffer::projection::FIXTURE_NOW_MS,
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
            forge_provider_api::ItemEvent::Tool {
                event: forge_provider_api::ToolItemEvent::ExecutionStarted,
            },
            forge_provider_api::ItemEvent::Tool {
                event: forge_provider_api::ToolItemEvent::Output(TextUpdate::Replace(format!(
                    "operation-{index}"
                ))),
            },
            forge_provider_api::ItemEvent::Tool {
                event: forge_provider_api::ToolItemEvent::Changes(if index == 5 {
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
                }),
            },
        ];
        let outcome = match status {
            "completed" => Some(ToolOutcome::Succeeded),
            "failed" => Some(ToolOutcome::Failed {
                diagnostic: crate::provider::ToolDiagnostic {
                    kind: forge_provider_api::ProviderFailureKind::Unknown,
                    message: forge_provider_api::SanitizedMessage::new("write failed")
                        .expect("host diagnostic is nonempty"),
                },
            }),
            "denied" => Some(ToolOutcome::Denied {
                reason: forge_provider_api::diagnostic::SanitizedMessage::new("write denied")
                    .expect("static denial is nonempty"),
            }),
            "cancelled" => Some(ToolOutcome::Cancelled),
            "interrupted" => Some(ToolOutcome::Interrupted),
            _ => None,
        };
        if let Some(outcome) = outcome {
            update.push(forge_provider_api::ItemEvent::Completed(
                forge_provider_api::ItemCompletion::FileChange(
                    forge_provider_api::ToolCompletion {
                        outcome,
                        descriptor: None,
                        output: None,
                        changes: Default::default(),
                    },
                ),
            ));
        }
        crate::turn::test_support::tool(
            &mut exchange,
            &address,
            &format!("write-{index}"),
            forge_provider_api::ItemStart::FileChange(forge_provider_api::ToolDescriptor {
                semantics: Default::default(),
                name: "edit".into(),
                title: "edit".into(),
                arguments: forge_provider_api::InspectableArguments::new(serde_json::json!({})),
            }),
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
