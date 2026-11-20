use async_trait::async_trait;
use forge_provider_api::ProviderError;
use forge_provider_api::control::{ControlToolDefinition, ToolCall, ToolReply};
use github_copilot_sdk::{Tool, ToolInvocation, ToolResult, ToolResultExpanded, tool::ToolHandler};
use std::sync::Arc;

/// Bind host tool definitions to the native Copilot callback interface.
pub fn tool_list(
    definitions: Vec<ControlToolDefinition>,
    callbacks: Arc<dyn super::provider::NativeCallbackRouter>,
) -> Vec<Tool> {
    let handler: Arc<dyn ToolHandler> = Arc::new(ControlToolHandler { callbacks });
    definitions
        .into_iter()
        .map(|definition| {
            Tool::new(definition.name)
                .with_description(definition.description)
                .with_parameters(definition.input_schema)
                .with_skip_permission(true)
                .with_handler(handler.clone())
        })
        .collect()
}

struct ControlToolHandler {
    callbacks: Arc<dyn super::provider::NativeCallbackRouter>,
}

#[async_trait]
impl ToolHandler for ControlToolHandler {
    async fn call(
        &self,
        invocation: ToolInvocation,
    ) -> Result<ToolResult, github_copilot_sdk::Error> {
        let Ok(id) = forge_provider_api::control::ToolCallId::new(invocation.tool_call_id.clone())
        else {
            return Ok(ToolResult::Text("Invalid tool call identity".into()));
        };
        let name = invocation.tool_name;
        let result = self
            .callbacks
            .call_native(
                &invocation.tool_call_id,
                ToolCall {
                    id,
                    name: name.clone(),
                    arguments: invocation.arguments,
                },
            )
            .await;
        Ok(native_result(&name, result))
    }
}

/// Encode a host tool reply as Copilot's native result.
///
/// A refused or failed call is a `failure` result whose `error` carries the reason, so Copilot
/// reports the call as failed and the timeline row shows why. A call the host could not
/// complete at all keeps that cause instead of a bare "failed".
fn native_result(name: &str, result: Result<ToolReply, ProviderError>) -> ToolResult {
    match result {
        Ok(reply) if reply.is_failure() => ToolResult::Expanded(
            ToolResultExpanded::new(reply.text.clone(), "failure").with_error(reply.text),
        ),
        Ok(reply) => ToolResult::Text(reply.text),
        Err(error) => {
            let reason = format!("Harness could not complete {name}: {error}");
            ToolResult::Expanded(
                ToolResultExpanded::new(reason.clone(), "failure").with_error(reason),
            )
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A refused Harness tool is a native failure carrying its reason, never a success.
    #[test]
    fn refused_and_failed_calls_are_native_failures_with_their_reason() {
        let failure = |result: ToolResult| match result {
            ToolResult::Expanded(expanded) => (expanded.result_type, expanded.error),
            other => panic!("expected a failure result, got {other:?}"),
        };
        let refused = native_result(
            "plan_phase_done",
            Ok(ToolReply::failed(
                "plan_phase_done failed with 1 error: task changed",
            )),
        );
        assert_eq!(
            failure(refused),
            (
                "failure".to_owned(),
                Some("plan_phase_done failed with 1 error: task changed".to_owned())
            )
        );
        let (kind, error) = failure(native_result(
            "plan_phase_done",
            Err(ProviderError::Cancelled),
        ));
        assert_eq!(kind, "failure");
        let error = error.unwrap();
        assert!(
            error.starts_with("Harness could not complete plan_phase_done: "),
            "{error}"
        );
        assert!(
            matches!(
                native_result("plan_read", Ok(ToolReply::succeeded("text"))),
                ToolResult::Text(text) if text == "text"
            ),
            "an answered call stays a plain success"
        );
    }
}
