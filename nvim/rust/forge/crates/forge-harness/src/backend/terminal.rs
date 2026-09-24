use anyhow::{Context, Result, ensure};
use serde::Serialize;
use serde_json::Value;

/// Provider-owned shell processes whose lifetime is independent of a Harness exchange.
#[derive(Clone, Debug, Default, Serialize)]
pub struct TerminalSnapshot {
    pub supported: bool,
    pub terminal: Vec<BackgroundTerminal>,
}

/// A live shell identity and its display command, scoped to one provider session.
#[derive(Clone, Debug, Serialize)]
pub struct BackgroundTerminal {
    pub id: String,
    pub command: String,
}

impl TerminalSnapshot {
    /// Normalize a provider inventory while excluding agents and completed shell tasks.
    pub fn parse(value: &Value, copilot: bool) -> Result<Self> {
        let rows = value
            .get(if copilot { "tasks" } else { "data" })
            .and_then(Value::as_array)
            .context("background terminal inventory is missing")?;
        ensure!(
            rows.len() <= 1024,
            "background terminal inventory exceeds 1024 entries"
        );
        let mut terminal = Vec::new();
        for row in rows {
            if copilot
                && (row.get("type").and_then(Value::as_str) != Some("shell")
                    || !matches!(
                        row.get("status").and_then(Value::as_str),
                        Some("running" | "idle")
                    )
                    || row.get("executionMode").and_then(Value::as_str) == Some("sync")
                    || (row.get("attachmentMode").and_then(Value::as_str) == Some("attached")
                        && row.get("executionMode").and_then(Value::as_str) != Some("background")))
            {
                continue;
            }
            let id = row
                .get(if copilot { "id" } else { "processId" })
                .and_then(Value::as_str)
                .context("background terminal identity is missing")?;
            let command = row
                .get("command")
                .and_then(Value::as_str)
                .context("background terminal command is missing")?;
            ensure!(
                id.len() <= 256 && command.len() <= 16384,
                "background terminal exceeds display limits"
            );
            if !terminal
                .iter()
                .any(|item: &BackgroundTerminal| item.id == id)
            {
                terminal.push(BackgroundTerminal {
                    id: id.into(),
                    command: command.into(),
                });
            }
        }
        Ok(Self {
            supported: true,
            terminal,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn inventory_excludes_agents_and_finished_shells_and_rejects_invalid_data() {
        let snapshot = TerminalSnapshot::parse(
            &json!({"tasks":[
                {"type":"agent","status":"running","id":"agent"},
                {"type":"shell","status":"completed","id":"done"},
                {"type":"shell","status":"running","executionMode":"sync","id":"foreground"},
                {"type":"shell","status":"running","attachmentMode":"attached","id":"attached"},
                {"type":"shell","status":"running","id":"shell","command":"cargo test"}
            ]}),
            true,
        )
        .unwrap();
        assert_eq!(snapshot.terminal.len(), 1);
        assert_eq!(snapshot.terminal[0].command, "cargo test");
        assert!(TerminalSnapshot::parse(&json!({}), true).is_err());
        assert!(TerminalSnapshot::parse(&json!({"data":[{"processId":"1"}]}), false).is_err());
        assert!(
            TerminalSnapshot::parse(&json!({"data":[]}), false)
                .unwrap()
                .terminal
                .is_empty()
        );
    }
}
