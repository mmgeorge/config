use super::schema::{cursor, session};
use crate::tools::error::ToolContext;
use crate::tools::{ControlToolDefinition, schema::strict_object_input_schema};
use serde_json::json;
pub(super) const NAME: &str = "harness_session_log";
/// Describe the harness_session_log tool and its input.
pub(super) fn definition() -> ControlToolDefinition {
    let mut properties = vec![
        session(),
        (
            "limit",
            json!({"type":"integer","minimum":1,"default":1000,"description":"Maximum complete log entries per page; defaults to 1000. Follow next_cursor for older entries."}),
        ),
    ];
    properties.push(cursor());
    ControlToolDefinition {
        name: NAME,
        description: "Read retained diagnostic metadata newest-first, including rotated logs. Omit session_id for \
                current. Each page contains up to limit complete entries (default 1000), without byte-budget \
                splitting. Logs exclude raw prompts, credentials, private reasoning and provider transport \
                bodies. Missing logs are explicit. Follow next_cursor with the same arguments to read older \
                entries; later appends are excluded.",
        input_schema: strict_object_input_schema(properties, &[]),
    }
}

/// Execute harness_session_log within the current turn authority.
pub(super) async fn execute(
    runtime: &mut crate::tools::runtime::ControlToolRuntime,
    invocation: crate::tools::ControlToolInvocation,
    _output: crate::provider::ProviderCompletion,
) -> crate::tools::ToolResult<crate::tools::ControlToolResult> {
    let scope = runtime
        .context
        .harness
        .as_ref()
        .context("Harness inspection requires an authenticated root interaction")?;
    let message = scope.invoke(NAME, invocation.arguments).await?;
    Ok(crate::tools::ControlToolResult {
        invocation: None,
        message,
    })
}
