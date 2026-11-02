use super::ask::plan_question_input_schema;
use crate::tools::ControlToolDefinition;
use crate::tools::error::ToolContext;
use crate::tools::schema::strict_object_input_schema;
use crate::tools::schema::string_schema;
use serde_json::json;

pub(super) const NAME: &str = "question_revise";

/// Describe the question_revise tool and its accepted inputs.
pub(super) fn definition() -> ControlToolDefinition {
    ControlToolDefinition {
        name: NAME,
        description: "Revise a pending question's wording or choices using its existing question_id and revision. \
                Preserve its waiting behavior and discussion. Do not revise an answered question or invent a new \
                ID.",
        input_schema: strict_object_input_schema(
            vec![
                ("question_id", string_schema()),
                ("revision", json!({"type":"integer","minimum":1})),
                (
                    "draft",
                    plan_question_input_schema()["properties"]["questions"]["items"].clone(),
                ),
            ],
            &["question_id", "revision", "draft"],
        ),
    }
}

/// Revise question text while retaining its answer-delivery behavior.
pub(super) async fn execute(
    runtime: &mut crate::tools::runtime::ControlToolRuntime,
    invocation: crate::tools::ControlToolInvocation,
    _output: crate::backend::BackendCompletion,
) -> crate::tools::ToolResult<crate::tools::ControlToolResult> {
    let controller = runtime
        .context
        .questions
        .as_ref()
        .context("question revision requires a session question controller")?;
    let mut draft: crate::question::QuestionDraft =
        serde_json::from_value(invocation.arguments["draft"].clone())?;
    draft.requirement = controller
        .snapshot()?
        .questions
        .iter()
        .find(|question| Some(question.id.as_str()) == invocation.arguments["question_id"].as_str())
        .context("question does not belong to this session")?
        .requirement;
    super::state::apply(
        runtime,
        invocation,
        crate::question::QuestionAction::Revise { draft },
    )
}
