use crate::provider::PromptMode;
use crate::tools::ControlToolDefinition;
use crate::tools::error::ToolContext;
use serde_json::Value;
use serde_json::json;

pub(super) const NAME: &str = "question_ask";

/// Describe the question_ask tool and its accepted inputs.
pub(super) fn definition() -> ControlToolDefinition {
    ControlToolDefinition {
        name: NAME,
        description: "Ask the user one or more concise, self-contained questions for missing information, preferences, \
                constraints, clarification, or approval. Respect requests not to ask questions. By default, \
                returns Harness-generated question_ids immediately without ending the turn; keep working while \
                answers arrive asynchronously as new user messages. Set wait:true only for important decisions \
                that block progress: the tool stays pending and returns the resolved answers instead of sending \
                them separately. Never invent IDs or treat recommended choices or assumptions as user answers. \
                New questions have revision 1. Use the returned question IDs with question_reply, \
                question_revise, or question_withdraw.",
        input_schema: plan_question_input_schema(),
    }
}

/// Build the waiting and asynchronous question input schema.
pub fn plan_question_input_schema() -> Value {
    json!({
        "type": "object",
        "properties": {
            "wait": { "type": "boolean", "default": false, "description": "Wait for answers only when an important decision blocks progress. Otherwise return IDs immediately and continue work." },
            "questions": {
                "type": "array",
                "minItems": 1,
                "items": {
                    "type": "object",
                    "properties": {
                        "header": { "type": "string" },
                        "question": { "type": "string" },
                        "options": {
                            "type": "array",
                                        "items": {
                                "type": "object",
                                "properties": {
                                    "label": { "type": "string" },
                                    "recommended": { "type": "boolean" },
                                    "description": { "type": "string" }
                                },
                                "required": ["label", "description"],
                                "additionalProperties": false
                            }
                        },
                        "allow_freeform": { "type": "boolean" }
                    },
                    "required": ["header", "question", "options"],
                    "additionalProperties": false
                }
            }
        },
        "required": ["questions"],
        "additionalProperties": false
    })
}

/// Decode the tool's replayable completion fields.
pub(super) fn record(
    invocation: &crate::tools::ControlToolInvocation,
    output: &mut crate::provider::ProviderCompletion,
) -> crate::tools::ToolResult<()> {
    output.plan_question = Some(
        crate::tools::invocation::decode_arguments::<crate::plan::PlanQuestionSet>(invocation)?
            .normalize()?,
    );
    output.plan_submit = None;
    Ok(())
}

/// Ask a persistent question or retain the legacy conversational result.
pub(super) async fn execute(
    runtime: &mut crate::tools::runtime::ControlToolRuntime,
    invocation: crate::tools::ControlToolInvocation,
    output: crate::provider::ProviderCompletion,
) -> crate::tools::ToolResult<crate::tools::ControlToolResult> {
    if let Some(controller) = runtime.context.questions.clone() {
        let task = super::state::current_task(runtime)?;

        let mut drafts: Vec<crate::question::QuestionDraft> =
            serde_json::from_value(invocation.arguments["questions"].clone())?;
        let wait = invocation.arguments["wait"].as_bool().unwrap_or(false);
        for draft in &mut drafts {
            draft.requirement = if wait {
                crate::question::QuestionRequirement::Required
            } else {
                crate::question::QuestionRequirement::Optional
            };
        }
        let operation_id = uuid::Uuid::new_v4().to_string();
        let authority = crate::tools::TaskAuthority::capture(&runtime.context);
        let exchange_id = runtime
            .context
            .question_exchange_id
            .as_deref()
            .context("question has no originating exchange")?;
        if wait {
            let answers = controller
                .ask_wait_authorized(
                    &operation_id,
                    exchange_id,
                    task.as_ref(),
                    drafts,
                    authority.as_ref(),
                )
                .await?;
            if let Some(authority) = &authority {
                authority.validate_scope()?;
            }
            return Ok(crate::tools::ControlToolResult {
                invocation: None,
                message: serde_json::json!({"answers": answers}).to_string(),
            });
        }
        let publish = || {
            controller.ask(
                &operation_id,
                exchange_id,
                if runtime.context.question_discussion {
                    crate::question::QuestionOrigin::Discussion(task.as_ref())
                } else {
                    task.as_ref().map_or(
                        crate::question::QuestionOrigin::Chat,
                        crate::question::QuestionOrigin::Task,
                    )
                },
                drafts,
            )
        };
        let ids = match authority {
            Some(authority) => authority.commit(publish)?,
            None => publish()?,
        };
        return Ok(crate::tools::ControlToolResult {
            invocation: None,
            message: serde_json::json!({"question_ids":ids}).to_string(),
        });
    }

    let question = output
        .plan_question
        .as_ref()
        .context("question ask did not produce questions")?;
    if question.questions.iter().all(|item| {
        runtime
            .context
            .resolved_question_digest_set
            .contains(&item.content_digest())
    }) {
        runtime.terminal = true;
        return Ok(crate::tools::ControlToolResult { invocation: None, message: "Those questions were already consumed. Continue planning without reopening feedback.".into() });
    }
    crate::tools::error::ensure!(
        !runtime.context.has_active_elicitation
            || (matches!(
                runtime.context.mode,
                PromptMode::Chat | PromptMode::PlanDiscussion
            ) && !runtime.context.planning_feedback),
        "a Harness question set is already pending"
    );
    runtime.terminal = true;
    Ok(crate::tools::ControlToolResult {
        invocation: Some(invocation),
        message: "Question set accepted".into(),
    })
}

/// Publish a required question synchronously so its answer wait can outlive the runtime lock.
pub(crate) fn begin_wait(
    runtime: &crate::tools::runtime::ControlToolRuntime,
    invocation: &crate::tools::ControlToolInvocation,
) -> crate::tools::ToolResult<crate::question::OwnedQuestionWait> {
    crate::tools::validate_arguments(invocation)?;
    let controller = runtime
        .context
        .questions
        .as_ref()
        .context("waiting questions require the session question controller")?;
    crate::tools::error::ensure!(
        runtime.context.question_discussion || !controller.snapshot()?.required_pending(),
        "required questions must be resolved before continuing task work"
    );
    let task = super::state::current_task(runtime)?;
    let mut drafts: Vec<crate::question::QuestionDraft> =
        serde_json::from_value(invocation.arguments["questions"].clone())?;
    for draft in &mut drafts {
        draft.requirement = crate::question::QuestionRequirement::Required;
    }
    let authority = crate::tools::TaskAuthority::capture(&runtime.context);
    let exchange = runtime
        .context
        .question_exchange_id
        .as_deref()
        .context("question has no originating exchange")?;
    Ok(controller.begin_owned_wait(
        &uuid::Uuid::new_v4().to_string(),
        exchange,
        task.as_ref(),
        drafts,
        authority.as_ref(),
    )?)
}
