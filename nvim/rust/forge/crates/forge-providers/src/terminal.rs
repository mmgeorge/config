use forge_provider_api::terminal::{
    BackgroundTerminal, TerminalId, TerminalInventory, TerminalInventoryError,
};
use serde_json::Value;

/// Normalize a provider inventory while excluding agents and completed shell tasks.
pub fn parse_snapshot(
    value: &Value,
    copilot: bool,
) -> Result<TerminalInventory, TerminalInventoryError> {
    let rows = value
        .get(if copilot { "tasks" } else { "data" })
        .and_then(Value::as_array)
        .ok_or(TerminalInventoryError::MissingInventory)?;
    let mut terminal = Vec::new();
    for row in rows {
        if copilot && !copilot_background_shell(row) {
            continue;
        }
        let id = terminal_identity(row, if copilot { "id" } else { "processId" })?;
        let command = row
            .get("command")
            .and_then(Value::as_str)
            .ok_or(TerminalInventoryError::MissingCommand)?;
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

/// Read cancellable Copilot task identities without requiring terminal display metadata.
pub(crate) fn copilot_task_identities(
    tasks: &[Value],
) -> Result<Vec<TerminalId>, TerminalInventoryError> {
    let mut identities = Vec::new();
    for task in tasks.iter().filter(|task| copilot_background_shell(task)) {
        let id = terminal_identity(task, "id")?;
        if !identities.contains(&id) {
            identities.push(id);
        }
    }
    Ok(identities)
}

/// Apply the same native ownership and liveness selection to display and collection.
fn copilot_background_shell(row: &Value) -> bool {
    row.get("type").and_then(Value::as_str) == Some("shell")
        && matches!(
            row.get("status").and_then(Value::as_str),
            Some("running" | "idle")
        )
        && row.get("executionMode").and_then(Value::as_str) != Some("sync")
        && (row.get("attachmentMode").and_then(Value::as_str) != Some("attached")
            || row.get("executionMode").and_then(Value::as_str) == Some("background"))
}

/// Validate the native identity before returning a cancellable terminal.
fn terminal_identity(row: &Value, field: &str) -> Result<TerminalId, TerminalInventoryError> {
    let id = row
        .get(field)
        .and_then(Value::as_str)
        .ok_or(TerminalInventoryError::MissingIdentity)?;
    TerminalId::new(id).map_err(|_| TerminalInventoryError::InvalidIdentity)
}
