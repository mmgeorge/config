use anyhow::{Context, Result};
use forge_provider_api::terminal::{BackgroundTerminal, TerminalId, TerminalInventory};
use serde_json::Value;

/// Normalize a provider inventory while excluding agents and completed shell tasks.
pub fn parse_snapshot(value: &Value, copilot: bool) -> Result<TerminalInventory> {
    let rows = value
        .get(if copilot { "tasks" } else { "data" })
        .and_then(Value::as_array)
        .context("background terminal inventory is missing")?;
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
        let id = TerminalId::try_from(id.to_owned())
            .context("background terminal identity is invalid")?;
        let command = row
            .get("command")
            .and_then(Value::as_str)
            .context("background terminal command is missing")?;
        if !terminal
            .iter()
            .any(|item: &BackgroundTerminal| item.id == id)
        {
            terminal.push(BackgroundTerminal {
                id,
                command: forge_shell::command::display_command(
                    command,
                    forge_shell::shell::CommandShell::default(),
                )
                .source,
            });
        }
    }
    Ok(TerminalInventory::observed(terminal))
}
