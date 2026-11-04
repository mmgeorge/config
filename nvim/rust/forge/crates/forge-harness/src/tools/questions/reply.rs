use crate::tools::ControlToolDefinition;
use crate::tools::error::ToolContext;
use crate::tools::schema::strict_object_input_schema;
use crate::tools::schema::string_schema;
use serde_json::json;

pub(super) const NAME: &str = "question_reply";

/// Describe the question_reply tool and its accepted inputs.
pub(super) fn definition() -> ControlToolDefinition {
    ControlToolDefinition {
        name: NAME,
        description: "Reply to an explicit clarification message on a Harness question using its existing question_id, \
                revision, and message_id. This explains the question without resolving it. Only the user can \
                submit an answer.",
        input_schema: strict_object_input_schema(
            vec![
                ("question_id", string_schema()),
                ("revision", json!({"type":"integer","minimum":1})),
                ("message_id", string_schema()),
                ("text", string_schema()),
            ],
            &["question_id", "revision", "message_id", "text"],
        ),
    }
}

/// Reply to a clarification while leaving the question open.
pub(super) async fn execute(
    runtime: &mut crate::tools::runtime::ControlToolRuntime,
    invocation: crate::tools::ControlToolInvocation,
    _output: crate::provider::ProviderCompletion,
) -> crate::tools::ToolResult<crate::tools::ControlToolResult> {
    let action = crate::question::QuestionAction::Reply {
        message_id: invocation.arguments["message_id"]
            .as_str()
            .context("clarification message identity required")?
            .into(),
        text: invocation.arguments["text"]
            .as_str()
            .context("reply text required")?
            .into(),
    };
    super::state::apply(runtime, invocation, action).await
}
