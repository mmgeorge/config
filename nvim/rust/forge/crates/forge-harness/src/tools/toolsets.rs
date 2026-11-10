//! Host toolsets the user or environment can switch on and off during a conversation.
//!
//! Each toolset is named by the prefix its tools share, or by its single tool, so the model can
//! map a tool to the `Toolsets` line of the `permissions` context section without reading every
//! description. Tool descriptions and every rejection read their requirement from [`requirement`],
//! so the advertised rule and the enforced rule cannot drift apart. Task tools have no toolset;
//! the active `<task kind="...">` section implies them.

use super::{ControlToolDefinition, ControlTurnContext};
use std::collections::HashMap;
use std::sync::{LazyLock, Mutex};

/// LSP navigation and rename tools, which follow the LSP setting and language-server availability.
pub(crate) const LSP: &str = super::lsp::TOOLSET;
/// Session history inspection tools; `harness_config` stays available without them.
pub(crate) const HARNESS_SESSION: &str = super::session::TOOLSET;
/// Subagent tools, which follow the subagent setting.
pub(crate) const AGENT: &str = super::agents::TOOLSET;
/// Check configuration editing, which only the user can enable with `/checks edit on`.
pub(crate) const CHECK_DEFINITION_EDIT: &str = super::checks::DEFINITION_EDIT;

/// Report whether `name` belongs to the toolset named by `toolset`.
fn member(toolset: &str, name: &str) -> bool {
    match toolset.strip_suffix('*') {
        Some(prefix) => name.starts_with(prefix),
        None => name == toolset,
    }
}

/// Render the `Toolsets` line of the permissions section for one turn.
pub(crate) fn line(context: Option<&ControlTurnContext>) -> String {
    let state = |enabled: bool| if enabled { "on" } else { "off" };
    let lsp = context
        .and_then(|context| context.lsp.as_ref())
        .is_some_and(|scope| scope.available());
    let session = context
        .and_then(|context| context.harness.as_ref())
        .is_some_and(|scope| scope.enabled());
    let agent = context
        .and_then(|context| context.agent.as_ref())
        .is_some_and(|scope| scope.available());
    let editing = context
        .and_then(|context| context.checks.as_ref())
        .is_some_and(|scope| scope.editing_enabled());
    format!(
        "Toolsets: {LSP} {}, {HARNESS_SESSION} {}, {AGENT} {}, {CHECK_DEFINITION_EDIT} {}.",
        state(lsp),
        state(session),
        state(agent),
        state(editing)
    )
}

/// Describe when a tool is available, or `None` when it has no toolset or task requirement.
pub(crate) fn requirement(name: &str) -> Option<&'static str> {
    if member(LSP, name) {
        return Some(
            "Part of the lsp_* toolset, available when the Toolsets line in <permissions> lists lsp_* on.",
        );
    }
    if member(HARNESS_SESSION, name) {
        return Some(
            "Part of the harness_session_* toolset, available when the Toolsets line in <permissions> lists harness_session_* on; harness_config stays available without it.",
        );
    }
    if member(AGENT, name) {
        return Some(
            "Part of the agent_* toolset, available when the Toolsets line in <permissions> lists agent_* on.",
        );
    }
    if member(CHECK_DEFINITION_EDIT, name) {
        return Some(
            "Available when the Toolsets line in <permissions> lists check_definition_edit on; only the user can turn it on, with /checks edit on.",
        );
    }
    Some(match name {
        "plan_capture_file" => "Available while <task kind=\"plan\"> is active.",
        "plan_submit" => {
            "Available while <task kind=\"plan\"> is active, and during <task kind=\"execute\" phase=\"resolve\">."
        }
        "plan_status" => "Available during <task kind=\"plan\"> and <task kind=\"execute\">.",
        "plan_read" => {
            "Available during <task kind=\"execute\">; during planning, read the planning directory with your read tools."
        }
        "plan_edit" => {
            "Available during <task kind=\"execute\"> in phases resolve and verify; during planning, edit the planning directory with your file-editing tool."
        }
        "plan_phase_done" => "Available during <task kind=\"execute\">.",
        "goal_status" | "goal_complete" | "goal_blocked" => {
            "Available during <task kind=\"goal\">."
        }
        _ => return None,
    })
}

/// Explain why a tool cannot be called in this turn, naming the rule that enables it.
pub(crate) fn unavailable_reason(name: &str) -> String {
    match requirement(name) {
        Some(requirement) => format!("`{name}` is not available in this turn. {requirement}"),
        None => format!("`{name}` is not available in this turn."),
    }
}

/// Append a tool's availability requirement to its advertised description.
///
/// Descriptions are `'static`, so each combined text is created once per distinct description
/// and reused for the life of the process.
pub(crate) fn with_requirement(mut definition: ControlToolDefinition) -> ControlToolDefinition {
    static DESCRIPTIONS: LazyLock<Mutex<HashMap<(&'static str, &'static str), &'static str>>> =
        LazyLock::new(Default::default);
    let Some(requirement) = requirement(definition.name) else {
        return definition;
    };
    if definition.description.ends_with(requirement) {
        return definition;
    }
    let mut descriptions = DESCRIPTIONS
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    definition.description = descriptions
        .entry((definition.name, definition.description))
        .or_insert_with(|| {
            Box::leak(format!("{} {requirement}", definition.description).into_boxed_str())
        });
    definition
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Keep each toolset name in step with the tools its family defines.
    #[test]
    fn toolset_names_match_every_tool_in_their_family() {
        for (toolset, definitions) in [
            (LSP, super::super::lsp::definitions()),
            (HARNESS_SESSION, super::super::session::definitions()),
            (AGENT, super::super::agents::definitions()),
        ] {
            assert!(!definitions.is_empty());
            for definition in definitions {
                assert!(
                    member(toolset, definition.name),
                    "{} is outside {toolset}",
                    definition.name
                );
            }
        }
        assert!(!member(
            HARNESS_SESSION,
            super::super::config::definition().name
        ));
        let catalog = super::super::ControlToolRegistry.definition_list();
        assert!(
            catalog
                .iter()
                .any(|tool| tool.name == CHECK_DEFINITION_EDIT)
        );
        for name in [
            "plan_capture_file",
            "plan_submit",
            "plan_status",
            "plan_read",
            "plan_edit",
            "plan_phase_done",
            "goal_status",
            "goal_complete",
            "goal_blocked",
        ] {
            assert!(
                catalog.iter().any(|tool| tool.name == name),
                "{name} is not a registered tool"
            );
            assert!(requirement(name).is_some(), "{name} has no requirement");
        }
    }

    /// Advertise the same requirement that a rejection reports, without repeating it.
    #[test]
    fn descriptions_and_rejections_share_one_requirement() {
        let definition = super::super::ControlToolRegistry
            .definition_list()
            .into_iter()
            .find(|tool| tool.name == CHECK_DEFINITION_EDIT)
            .unwrap();
        let advertised = with_requirement(with_requirement(definition));
        let requirement = requirement(CHECK_DEFINITION_EDIT).unwrap();
        assert!(advertised.description.ends_with(requirement));
        assert_eq!(advertised.description.matches(requirement).count(), 1);
        assert_eq!(
            unavailable_reason(CHECK_DEFINITION_EDIT),
            format!("`check_definition_edit` is not available in this turn. {requirement}")
        );
        assert_eq!(
            unavailable_reason("question_ask"),
            "`question_ask` is not available in this turn."
        );
        assert_eq!(
            line(None),
            "Toolsets: lsp_* off, harness_session_* off, agent_* off, check_definition_edit off."
        );
    }
}
