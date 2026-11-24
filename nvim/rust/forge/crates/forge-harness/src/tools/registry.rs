use super::agents::is_tool as is_agent_tool;
use super::checks::is_tool as is_check_tool;
use super::{ControlTurnContext, lsp, session};
use serde_json::Value;

pub use forge_provider_api::control::ControlToolDefinition;

/// Identify inspections that remain available after a terminal request seals mutations.
pub(crate) fn is_read_only(name: &str) -> bool {
    super::repository::is_tool(name)
        || super::session::is_tool(name)
        || (super::lsp::is_tool(name) && !super::lsp::is_mutation(name))
        || matches!(
            name,
            "plan_read"
                | "plan_status"
                | "harness_config"
                | "agent_activity"
                | "agent_list"
                | "agent_tool_output"
                | "check_status"
                | "check_list"
                | "check_detail"
                | "check_discover"
                | "check_definition_list"
                | "check_definition_details"
        )
}

/// Exclude trusted replay metadata from provider-visible schemas and state each tool's
/// availability requirement.
fn advertised_definition(mut definition: ControlToolDefinition) -> ControlToolDefinition {
    if definition.name == "plan_submit" {
        remove_harness_submission_fields(&mut definition);
    }
    super::toolsets::with_requirement(definition)
}

/// Hide the plan_submit fields Harness fills in itself.
///
/// The catalog schema keeps them because Harness validates its own recorded invocation against
/// it; the advertised schema is identical in every mode, so a provider that keeps an earlier
/// turn's schema never sees a version requirement that a later turn enforces.
fn remove_harness_submission_fields(definition: &mut ControlToolDefinition) {
    let properties = definition.input_schema["properties"]
        .as_object_mut()
        .expect("plan submit properties are an object");
    properties.remove("authoring_snapshot");
    properties.remove("resolution_remaining");
    properties.remove("expected_version");
}

/// Show plan_submit only while authoring or in Resolve, and plan_edit also in Verify, where it
/// writes `plan:verification.md`.
fn plan_mutation_visible(name: &str, phase: Option<crate::plan::PlanPhase>) -> bool {
    use crate::plan::PlanPhase;
    match name {
        "plan_submit" => phase.is_none_or(|phase| phase == PlanPhase::Resolve),
        "plan_edit" => {
            phase.is_none_or(|phase| matches!(phase, PlanPhase::Resolve | PlanPhase::Verify))
        }
        _ => true,
    }
}

/// Owns the canonical Harness control-tool catalog for every provider adapter.
#[derive(Clone, Debug, Default)]
pub struct ControlToolRegistry;

impl ControlToolRegistry {
    /// Register a stable superset without granting invocation authority to disabled tools.
    pub(crate) fn registration_definitions(
        &self,
        owner: &crate::provider::ThreadOwner,
        mode: crate::provider::PromptMode,
    ) -> Vec<ControlToolDefinition> {
        if mode == crate::provider::PromptMode::Background {
            return Vec::new();
        }
        self.definition_list()
            .into_iter()
            .filter(|definition| {
                owner.is_root()
                    || definition.name.starts_with("repository_")
                    || lsp::is_tool(definition.name)
            })
            .map(advertised_definition)
            .collect()
    }

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
            super::origin::definitions(),
            vec![super::config::definition()],
        ]
        .into_iter()
        .flatten()
        .collect()
    }

    /// Expose plan mutation only while authoring a plan, resolving an execution, or writing the
    /// Verify report.
    pub fn definition_list_for(
        &self,
        phase: Option<crate::plan::PlanPhase>,
    ) -> Vec<ControlToolDefinition> {
        self.definition_list()
            .into_iter()
            .filter(|definition| {
                !is_agent_tool(definition.name) && plan_mutation_visible(definition.name, phase)
            })
            .map(advertised_definition)
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
                if definition.name == "plan_capture_file" {
                    return root && context.is_some_and(|context| context.plan_authoring.is_some());
                }
                if context.is_some_and(|context| context.plan_authoring.is_some())
                    && matches!(definition.name, "plan_read" | "plan_edit")
                {
                    return false;
                }
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
                if super::origin::is_tool(definition.name) {
                    return root && context.is_some_and(|context| context.aside.is_some());
                }
                if is_check_tool(definition.name) {
                    return root && context.is_some_and(|context| context.checks.is_some());
                }
                if lsp::is_tool(definition.name) {
                    if lsp::is_mutation(definition.name)
                        && !context.is_some_and(|context| {
                            lsp::allowed_mode(context.mode) && !context.question_discussion
                        })
                    {
                        return false;
                    }
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
                plan_mutation_visible(definition.name, phase)
            })
            .map(|mut definition| {
                if definition.name == "plan_submit" {
                    remove_harness_submission_fields(&mut definition);
                }
                if definition.name == "plan_submit"
                    && context.is_some_and(|context| context.plan_authoring.is_some())
                {
                    definition.input_schema["properties"].as_object_mut().expect("plan submit properties are an object").remove("reason");
                    definition.description = "Submit the active planning directory for review. Supply plan_id without an expected version. Harness snapshots docs/ and files/, reports parsing and reference diagnostics by file and line, and preserves the editable draft on failure. When captured sources changed, this call refreshes their baselines, reports the differences, and asks you to revise or resubmit. Later ordinary edits require resubmission. Wait for Harness execution instructions; submission alone never authorizes implementation.";
                }
                if definition.name == "plan_status" && context.is_some_and(|context| context.plan_authoring.is_some()) {
                    definition.description = "Show the active plan ID, its planning directory, proposal inventory, and whether authored files differ from the saved submission. Supply plan_id. Read docs/ and files/ with your usual read tools and edit them with your file-editing tool, not virtual plan tools or shell commands.";
                }
                if definition.name == "agent_spawn"
                    && let Some(schema) = context
                        .and_then(|context| context.agent.as_ref())
                        .and_then(|scope| scope.profile_schema())
                {
                    definition.input_schema["properties"]["profile"] = schema;
                }
                super::toolsets::with_requirement(definition)
            })
            .collect()
    }

    /// Advertise optional root tools before a provider transport admits its first turn.
    pub fn definition_list_for_owner(
        &self,
        owner: &crate::provider::ThreadOwner,
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
            .map(advertised_definition)
            .collect()
    }

    /// Convert the canonical definitions into the MCP tool-list shape.
    pub fn mcp_tool_list(&self) -> Vec<Value> {
        self.definition_list()
            .into_iter()
            .map(advertised_definition)
            .map(|definition| definition.mcp_value())
            .collect()
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::plan::PlanPhase;

    #[test]
    fn verify_exposes_plan_edit_without_plan_submit() {
        let visible = |phase| {
            let names = ControlToolRegistry
                .definition_list_for(phase)
                .into_iter()
                .map(|definition| definition.name)
                .collect::<Vec<_>>();
            (names.contains(&"plan_edit"), names.contains(&"plan_submit"))
        };
        assert_eq!(visible(None), (true, true));
        assert_eq!(visible(Some(PlanPhase::Resolve)), (true, true));
        assert_eq!(visible(Some(PlanPhase::Verify)), (true, false));
        assert_eq!(visible(Some(PlanPhase::Implement)), (false, false));
    }

    /// Harness owns the submitted version, so no advertised plan_submit schema asks the model
    /// for one: a provider that keeps an earlier turn's schema still calls it correctly.
    #[test]
    fn plan_submit_never_advertises_an_expected_version() {
        let registry = ControlToolRegistry;
        let advertised = [
            registry.definition_list_for(None),
            registry.definition_list_for(Some(PlanPhase::Resolve)),
            registry.registration_definitions(
                &crate::provider::ThreadOwner::root("session"),
                crate::provider::PromptMode::ExecutePlan,
            ),
            registry.definition_list_for_context(None, true),
        ];
        for list in advertised {
            let submit = list
                .iter()
                .find(|definition| definition.name == "plan_submit")
                .expect("plan_submit is advertised");
            assert!(
                submit.input_schema["properties"]
                    .get("expected_version")
                    .is_none(),
                "{}",
                submit.input_schema
            );
            assert!(
                !submit.description.contains("expected_version"),
                "{}",
                submit.description
            );
        }
    }
}
