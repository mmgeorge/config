use super::agents::is_tool as is_agent_tool;
use super::checks::is_tool as is_check_tool;
use super::{ControlTurnContext, lsp, session};
use serde_json::Value;

pub use forge_provider_api::control::ControlToolDefinition;

/// Owns the canonical Harness control-tool catalog for every provider adapter.
#[derive(Clone, Debug, Default)]
pub struct ControlToolRegistry;

impl ControlToolRegistry {
    /// Build the complete provider-neutral control-tool definition list.
    pub fn definition_list(&self) -> Vec<ControlToolDefinition> {
        [
            super::agents::definitions(),
            super::repository::definitions(),
            super::plan::definitions(),
            super::questions::definitions(),
            super::goals::definitions(),
            super::checks::definitions(),
            super::lsp::definitions(),
            super::session::definitions(),
            vec![super::config::definition()],
        ]
        .into_iter()
        .flatten()
        .collect()
    }

    /// Expose plan mutation only while authoring a plan or resolving an execution.
    pub fn definition_list_for(
        &self,
        phase: Option<crate::plan::PlanPhase>,
    ) -> Vec<ControlToolDefinition> {
        self.definition_list()
            .into_iter()
            .filter(|definition| {
                !is_agent_tool(definition.name)
                    && (!matches!(
                        definition.name,
                        "plan_file_edit" | "plan_overview_edit" | "plan_submit"
                    ) || phase.is_none_or(|phase| phase == crate::plan::PlanPhase::Resolve))
            })
            .collect()
    }

    /// Select tools from authenticated root ownership and current optional-tool authority.
    pub fn definition_list_for_context(
        &self,
        context: Option<&ControlTurnContext>,
        root: bool,
    ) -> Vec<ControlToolDefinition> {
        if context.is_some_and(|context| context.mode == crate::provider::PromptMode::Background) {
            return Vec::new();
        }
        let phase =
            context.and_then(|context| context.execution.as_ref().map(|execution| execution.phase));
        self.definition_list()
            .into_iter()
            .filter(|definition| {
                if context.is_some_and(|context| {
                    context.mode == crate::provider::PromptMode::PlanQuestion
                }) && !(definition.name.starts_with("repository_")
                    || lsp::is_tool(definition.name)
                    || session::is_tool(definition.name)
                    || definition.name == super::config::NAME)
                {
                    return false;
                }
                if session::is_tool(definition.name) || definition.name == super::config::NAME {
                    return root
                        && context
                            .and_then(|context| context.harness.as_ref())
                            .is_some_and(|scope| {
                                definition.name == "harness_config" || scope.enabled()
                            });
                }
                if is_check_tool(definition.name) {
                    return root && context.is_some_and(|context| context.checks.is_some());
                }
                if lsp::is_tool(definition.name) {
                    return context
                        .and_then(|context| context.lsp.as_ref())
                        .is_some_and(|scope| scope.available());
                }
                if !root {
                    return definition.name.starts_with("repository_")
                        && context.is_some_and(|context| context.repository.is_some());
                }
                if is_agent_tool(definition.name) {
                    return context
                        .and_then(|context| context.agent.as_ref())
                        .is_some_and(|scope| scope.available());
                }
                !matches!(
                    definition.name,
                    "plan_file_edit" | "plan_overview_edit" | "plan_submit"
                ) || phase.is_none_or(|phase| phase == crate::plan::PlanPhase::Resolve)
            })
            .map(|mut definition| {
                if definition.name == "agent_spawn"
                    && let Some(schema) = context
                        .and_then(|context| context.agent.as_ref())
                        .and_then(|scope| scope.profile_schema())
                {
                    definition.input_schema["properties"]["profile"] = schema;
                }
                definition
            })
            .collect()
    }

    /// Advertise optional root tools before an ACP transport admits its first turn.
    pub fn definition_list_for_owner(
        &self,
        owner: &crate::provider::ExecutionOwner,
        agent_tools: bool,
        lsp_tools: bool,
    ) -> Vec<ControlToolDefinition> {
        self.definition_list()
            .into_iter()
            .filter(|definition| {
                if lsp::is_tool(definition.name) {
                    return lsp_tools;
                }
                owner.is_root() && (!is_agent_tool(definition.name) || agent_tools)
            })
            .collect()
    }

    /// Convert the canonical definitions into the MCP tool-list shape.
    pub fn mcp_tool_list(&self) -> Vec<Value> {
        self.definition_list()
            .iter()
            .map(ControlToolDefinition::mcp_value)
            .collect()
    }
}
