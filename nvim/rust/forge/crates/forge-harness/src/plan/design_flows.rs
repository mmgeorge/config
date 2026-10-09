use std::collections::HashSet;

use anyhow::{Result, ensure};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
/// Records one operation from its entry point to its observable result.
pub struct DesignFlow {
    /// Names the operation independently of its implementation steps.
    pub title: String,
    /// Explains the operation's purpose or result in concise prose.
    pub description: String,
    /// Starts the operation's call, data-transfer, or state-transition tree.
    pub root: DesignFlowNode,
}

#[derive(Clone, Debug, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
/// Identifies an operation and the downstream interactions it initiates.
pub struct DesignFlowNode {
    /// Names the concrete object, function, or state change at this point.
    pub text: String,
    /// Labels the incoming transfer or condition, absent on the root.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub via: Option<String>,
    /// Lists downstream interactions in execution order, empty at a result.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub children: Vec<DesignFlowNode>,
}

/// Bounds recursive rendering and rejects empty or ambiguous flow labels before persistence.
pub(super) fn validate(flows: &[DesignFlow]) -> Result<()> {
    ensure!(flows.len() <= 32, "plan flows exceeds 32 entries");
    let mut titles = HashSet::new();
    let mut count = 0;
    for flow in flows {
        validate_label(&flow.title)?;
        ensure!(
            titles.insert(&flow.title),
            "duplicate plan flow title: {}",
            flow.title
        );
        ensure!(
            !flow.description.trim().is_empty(),
            "plan flow requires a description"
        );
        ensure!(
            flow.root.via.is_none(),
            "plan flow root cannot have an incoming via label"
        );
        let mut pending = vec![(&flow.root, 1)];
        while let Some((node, depth)) = pending.pop() {
            count += 1;
            ensure!(count <= 512, "plan flows exceeds 512 nodes");
            ensure!(depth <= 32, "plan flow exceeds 32 node levels");
            validate_label(&node.text)?;
            if let Some(via) = &node.via {
                validate_label(via)?;
            }
            pending.extend(node.children.iter().map(|child| (child, depth + 1)));
        }
    }
    Ok(())
}

fn validate_label(label: &str) -> Result<()> {
    ensure!(
        !label.trim().is_empty() && label.trim() == label && !label.chars().any(char::is_control),
        "plan flow labels must be nonempty, unpadded single lines"
    );
    // Backticks could escape the generated code fence and reinterpret diagram labels as Markdown.
    ensure!(
        !label.contains('`'),
        "plan flow labels must use plain text without backticks"
    );
    Ok(())
}

/// Produces canonical Markdown shared by review, section reads, and revision diffs.
pub(super) fn text(flows: &[DesignFlow]) -> String {
    flows
        .iter()
        .map(|flow| {
            let mut rows = Vec::new();
            append_node(&flow.root, "", "", &mut rows);
            format!(
                "### {}\n\n{}\n\n```text\n{}\n```",
                flow.title,
                flow.description,
                rows.join("\n")
            )
        })
        .collect::<Vec<_>>()
        .join("\n\n")
}

/// Keeps short chains together and wraps longer paths at object boundaries.
fn append_node(node: &DesignFlowNode, prefix: &str, marker: &str, rows: &mut Vec<String>) {
    let label = |node: &DesignFlowNode| match &node.via {
        Some(via) => format!("{via} → {}", node.text),
        None => node.text.clone(),
    };
    let mut line = format!("{prefix}{marker}{}", label(node));
    let mut tail = node;
    while let [child] = tail.children.as_slice() {
        // Keep a branching owner on its own row so its children attach unambiguously.
        if child.children.len() > 1 {
            break;
        }
        let next = format!(" → {}", label(child));
        if line.chars().count() + next.chars().count() > 100 {
            break;
        }
        line.push_str(&next);
        tail = child;
    }
    rows.push(line);
    let continuation = format!(
        "{prefix}{}",
        if marker.starts_with('├') {
            "│  "
        } else if marker.is_empty() {
            ""
        } else {
            "   "
        }
    );
    for (index, child) in tail.children.iter().enumerate() {
        let marker = if index + 1 == tail.children.len() {
            "└─ "
        } else {
            "├─ "
        };
        append_node(child, &continuation, marker, rows);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn flow_diagrams_preserve_transfer_labels_branches_and_long_paths() {
        let flow: DesignFlow = serde_json::from_value(serde_json::json!({
            "title": "Confirm import", "description": "Commit validated rows or retain errors.",
            "root": {"text": "ImportSession.confirm", "children": [{
                "text": "ImportService.commit", "via": "validated rows", "children": [
                    {"text": "ImportStore.save", "via": "valid records", "children": [{"text": "ImportSession.complete", "via": "saved count"}]},
                    {"text": "ImportSession.errors", "via": "invalid rows"}
                ]
            }]}
        })).unwrap();
        validate(std::slice::from_ref(&flow)).unwrap();
        let rendered = text(std::slice::from_ref(&flow));
        assert!(rendered.contains("ImportSession.confirm\n└─ validated rows → ImportService.commit\n   ├─ valid records → ImportStore.save → saved count → ImportSession.complete\n   └─ invalid rows → ImportSession.errors"));
        let mut long = flow;
        long.root.text = "LongOperation".repeat(7);
        assert!(text(&[long]).contains("\n└─ validated rows → ImportService.commit"));
    }

    #[test]
    fn flow_validation_rejects_malformed_labels_and_recursive_overflow() {
        let valid = serde_json::json!({"title":"Cancel", "description":"Stop pending work.", "root":{"text":"cancel_request"}});
        for invalid in [
            serde_json::json!({"text":" "}),
            serde_json::json!({"text":"first\nsecond"}),
            serde_json::json!({"text":"```"}),
            serde_json::json!({"text":"cancel_request", "via":"cancel"}),
            serde_json::json!({"text":"cancel_request", "children":[{"text":"finish", "via":""}]}),
        ] {
            let mut value = valid.clone();
            value["root"] = invalid;
            let flow = serde_json::from_value(value).unwrap();
            assert!(validate(&[flow]).is_err());
        }
        let mut flow: DesignFlow = serde_json::from_value(valid).unwrap();
        let mut node = flow.root.clone();
        for _ in 0..32 {
            node = DesignFlowNode {
                text: "call".into(),
                via: None,
                children: vec![node],
            };
        }
        flow.root = node;
        assert!(
            validate(&[flow])
                .unwrap_err()
                .to_string()
                .contains("32 node levels")
        );
    }
}
