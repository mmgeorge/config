use crate::tools::ControlToolDefinition;
use crate::tools::error::ToolContext;
use crate::tools::schema::strict_object_input_schema;
use crate::tools::schema::string_schema;
use serde_json::Value;
use serde_json::json;

pub(super) const NAME: &str = "question_withdraw";

/// Describe the question_withdraw tool and its accepted inputs.
pub(super) fn definition() -> ControlToolDefinition {
    ControlToolDefinition {
        name: NAME,
        description: "Withdraw currently pending Harness questions only when no material user decision remains. Never \
                call this while continuing a planning-feedback turn because Harness already resolved that \
                question set.",
        input_schema: question_withdraw_input_schema(),
    }
}

/// Build the pending-question withdrawal input schema.
pub fn question_withdraw_input_schema() -> Value {
    strict_object_input_schema(
        vec![
            ("question_id", string_schema()),
            ("revision", json!({"type":"integer","minimum":1})),
            ("reason", string_schema()),
        ],
        &["question_id", "revision", "reason"],
    )
}

/// Withdraw a pending question under its recorded revision.
pub(super) async fn execute(
    runtime: &mut crate::tools::runtime::ControlToolRuntime,
    invocation: crate::tools::ControlToolInvocation,
    _output: crate::provider::ProviderCompletion,
) -> crate::tools::ToolResult<crate::tools::ControlToolResult> {
    if runtime.context.questions.is_some() {
        let reason = invocation.arguments["reason"]
            .as_str()
            .context("withdrawal reason required")?
            .into();
        return super::state::apply(
            runtime,
            invocation,
            crate::question::QuestionAction::Withdraw { reason },
        )
        .await;
    }

    Err(crate::tools::ToolError::MissingState(
        "question mutation owner is missing".into(),
    ))
}
