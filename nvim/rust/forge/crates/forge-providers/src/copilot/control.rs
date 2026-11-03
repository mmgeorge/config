use async_trait::async_trait;
use forge_provider_api::control::{ControlToolDefinition, ToolCall};
use github_copilot_sdk::{Tool, ToolInvocation, ToolResult, tool::ToolHandler};
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
        let name = invocation.tool_name;
        let result = self
            .callbacks
            .call_native(
                &invocation.tool_call_id,
                ToolCall {
                    name: name.clone(),
                    arguments: invocation.arguments,
                },
            )
            .await;
        Ok(ToolResult::Text(match result {
            Ok(message) => message,
            Err(_) => format!("{name} failed"),
        }))
    }
}
