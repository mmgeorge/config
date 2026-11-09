use super::{ControlToolArgumentError, ControlToolRegistry, schema_violation_list};
use crate::provider::ProviderCompletion;
use crate::tools::error::{ToolContext, ToolResult as Result};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::HashMap;
use std::sync::OnceLock;

/// Represents one provider callback into a Harness control tool.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ControlToolInvocation {
    /// Authenticated adapter identity; untrusted argument decoding cannot populate it.
    #[serde(skip)]
    pub(crate) call_id: Option<forge_provider_api::control::ToolCallId>,
    pub name: String,
    pub arguments: Value,
    #[serde(skip)]
    pub(crate) task_permit: Option<crate::task::TaskPermit>,
}

impl ControlToolInvocation {
    pub(crate) fn admit_task_control(
        &mut self,
        controller: Option<&crate::task::SessionTaskController>,
        turn_permit: Option<&crate::task::TaskPermit>,
    ) -> anyhow::Result<()> {
        if !matches!(
            self.name.as_str(),
            "goal_complete" | "goal_blocked" | "goal_status"
        ) {
            return Ok(());
        }
        let controller = anyhow::Context::context(
            controller,
            "goal control requires a session task controller",
        )?;
        let captured =
            anyhow::Context::context(turn_permit, "goal control has no admitted turn permit")?;
        let current = controller.admit_dispatched_goal(captured, self.name == "goal_complete")?;
        if self
            .task_permit
            .as_ref()
            .is_some_and(|permit| permit.revision() != current.revision())
        {
            return Err(crate::task::StaleTask.into());
        }
        self.task_permit = Some(current);
        Ok(())
    }

    /// Captures task authority before a turn-scoped control can wait for execution.
    pub(crate) fn admit_turn_control(
        &mut self,
        controller: Option<&crate::task::SessionTaskController>,
        turn_permit: Option<&crate::task::TaskPermit>,
        mode: crate::provider::PromptMode,
    ) -> anyhow::Result<()> {
        self.admit_task_control(controller, turn_permit)?;
        let Some(controller) = controller else {
            return Ok(());
        };
        let scoped = self.name.starts_with("plan_")
            || matches!(
                self.name.as_str(),
                "question_ask" | "question_reply" | "question_revise" | "question_withdraw"
            );
        let kind = match mode {
            crate::provider::PromptMode::Plan | crate::provider::PromptMode::PlanDiscussion
                if scoped =>
            {
                crate::task::TaskKind::Plan
            }
            crate::provider::PromptMode::ExecutePlan if scoped => crate::task::TaskKind::Execute,
            _ => return Ok(()),
        };
        let captured =
            anyhow::Context::context(turn_permit, "task control has no admitted turn permit")?;
        let current = controller.admit_dispatched(kind, captured)?;
        if captured.revision() != current.revision() {
            return Err(crate::task::StaleTask.into());
        }
        if self
            .task_permit
            .as_ref()
            .is_some_and(|permit| permit.revision() != captured.revision())
        {
            return Err(crate::task::StaleTask.into());
        }
        self.task_permit = Some(captured.clone());
        Ok(())
    }
}

struct ControlToolValidator {
    schema: Value,
    validator: jsonschema::Validator,
}

static CONTROL_TOOL_VALIDATOR_MAP: OnceLock<HashMap<&'static str, ControlToolValidator>> =
    OnceLock::new();

/// Apply one provider-neutral control invocation to the normalized turn result.
pub fn apply_invocation(
    invocation: &ControlToolInvocation,
    output: &mut ProviderCompletion,
) -> Result<()> {
    validate_arguments(invocation)?;
    match invocation.name.as_str() {
        name if super::agents::is_tool(name) => super::agents::record(invocation, output)?,
        name if super::repository::is_tool(name) => super::repository::record(invocation, output)?,
        name if super::plan::is_tool(name) => super::plan::record(invocation, output)?,
        name if super::questions::is_tool(name) => super::questions::record(invocation, output)?,
        name if super::goals::is_tool(name) => super::goals::record(invocation, output)?,
        name if super::checks::is_tool(name) => super::checks::record(invocation, output)?,
        name if super::lsp::is_tool(name) => super::lsp::record(invocation, output)?,
        name if super::session::is_tool(name) => super::session::record(invocation, output)?,
        super::config::NAME => {}
        name => crate::tools::error::bail!("unknown Harness control tool: {name}"),
    }
    Ok(())
}

/// Validate the advertised schema without applying legacy completion semantics.
pub(crate) fn validate_arguments(invocation: &ControlToolInvocation) -> Result<()> {
    let validator_map = CONTROL_TOOL_VALIDATOR_MAP.get_or_init(|| {
        ControlToolRegistry
            .definition_list()
            .into_iter()
            .map(|definition| {
                let schema = definition.input_schema;
                let validator = jsonschema::validator_for(&schema)
                    .expect("Harness control-tool schemas must compile");
                (definition.name, ControlToolValidator { schema, validator })
            })
            .collect()
    });
    let validation = validator_map
        .get(invocation.name.as_str())
        .with_context(|| format!("unknown Harness control tool: {}", invocation.name))?;
    let mut violation_list = validation
        .validator
        .iter_errors(&invocation.arguments)
        .flat_map(|error| schema_violation_list(&error, &validation.schema))
        .collect::<Vec<_>>();
    violation_list.sort_by(|left, right| {
        (&left.path, &left.code, &left.message).cmp(&(&right.path, &right.code, &right.message))
    });
    violation_list.dedup_by(|left, right| {
        left.path == right.path && left.code == right.code && left.message == right.message
    });
    if violation_list.is_empty() {
        return Ok(());
    }
    Err(ControlToolArgumentError {
        violation: violation_list,
    }
    .into())
}

pub(crate) fn json_pointer_to_path(pointer: &str) -> String {
    let mut path = String::new();
    for segment in pointer.split('/').skip(1) {
        let segment = segment.replace("~1", "/").replace("~0", "~");
        if segment.chars().all(|character| character.is_ascii_digit()) {
            path.push('[');
            path.push_str(&segment);
            path.push(']');
        } else {
            if !path.is_empty() {
                path.push('.');
            }
            path.push_str(&segment);
        }
    }
    path
}

pub(in crate::tools) fn decode_arguments<T>(invocation: &ControlToolInvocation) -> Result<T>
where
    T: for<'de> Deserialize<'de>,
{
    serde_path_to_error::deserialize(serde_stacker::Deserializer::new(&invocation.arguments))
        .map_err(|error| {
            crate::tools::error::rejected!("{} at JSON path {}", error.inner(), error.path())
        })
        .with_context(|| format!("decode {} arguments", invocation.name))
}
