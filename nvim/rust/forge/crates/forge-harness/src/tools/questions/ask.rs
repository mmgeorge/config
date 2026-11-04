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

/// Ask persistent questions through their owner before returning a result.
pub(super) async fn execute(
    runtime: &mut crate::tools::runtime::ControlToolRuntime,
    invocation: crate::tools::ControlToolInvocation,
    _output: crate::provider::ProviderCompletion,
) -> crate::tools::ToolResult<crate::tools::ControlToolResult> {
    if runtime.context.questions.is_some() {
        #[cfg(test)]
        let controller = runtime
            .context
            .questions
            .clone()
            .expect("the question controller was checked above");
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
        let call = super::state::tool_call(runtime, &invocation)?;
        #[cfg(test)]
        let operation_id = call
            .as_ref()
            .map(|call| call.operation_id().as_str().to_owned())
            .unwrap_or_else(|| uuid::Uuid::new_v4().to_string());
        let authority = crate::tools::TaskAuthority::capture(&runtime.context);
        let exchange_id = runtime
            .context
            .question_exchange_id
            .as_deref()
            .context("question has no originating exchange")?;
        if wait {
            let waiting = if let Some(events) = &runtime.context.question_events {
                let creation = crate::exchange::QuestionCreation::from_tool(
                    call.clone()
                        .context("question creation has no authenticated tool call")?,
                    exchange_id.into(),
                    task,
                    drafts,
                    if runtime.context.question_discussion {
                        crate::exchange::QuestionCreationOrigin::Discussion
                    } else {
                        crate::exchange::QuestionCreationOrigin::Workflow
                    },
                    authority
                        .as_ref()
                        .context("question creation has no captured tool authority")?
                        .clone(),
                )?;
                events.begin_wait(creation).await?
            } else {
                #[cfg(test)]
                {
                    controller
                        .prepare_tool_wait(
                            &operation_id,
                            exchange_id,
                            task.as_ref(),
                            drafts,
                            authority.as_ref(),
                        )?
                        .into_test_owned()
                }
                #[cfg(not(test))]
                {
                    return Err(crate::tools::ToolError::MissingState(
                        "question creation owner is missing".into(),
                    ));
                }
            };
            let answers = waiting.answers().await?;
            if let Some(authority) = &authority {
                authority.validate_scope()?;
            }
            return Ok(crate::tools::ControlToolResult {
                invocation: None,
                message: serde_json::json!({"answers":answers}).to_string(),
            });
        }
        let ids = if let Some(events) = &runtime.context.question_events {
            let creation = crate::exchange::QuestionCreation::from_tool(
                call.context("question creation has no authenticated tool call")?,
                exchange_id.into(),
                task,
                drafts,
                if runtime.context.question_discussion {
                    crate::exchange::QuestionCreationOrigin::Discussion
                } else {
                    crate::exchange::QuestionCreationOrigin::Workflow
                },
                authority.context("question creation has no captured tool authority")?,
            )?;
            events.ask(creation).await?
        } else {
            #[cfg(test)]
            {
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
                match authority {
                    Some(authority) => authority.commit(publish)?,
                    None => publish()?,
                }
            }
            #[cfg(not(test))]
            {
                return Err(crate::tools::ToolError::MissingState(
                    "question creation owner is missing".into(),
                ));
            }
        };
        return Ok(crate::tools::ControlToolResult {
            invocation: None,
            message: serde_json::json!({"question_ids":ids}).to_string(),
        });
    }

    Err(crate::tools::ToolError::MissingState(
        "question creation owner is missing".into(),
    ))
}

/// Retain a prepared wait without holding the tool runtime lock across owner intake or user input.
pub(in crate::tools) enum PreparedQuestionWait {
    Owner {
        events: crate::exchange::QuestionEvents,
        creation: crate::exchange::QuestionCreation,
    },
    #[cfg(test)]
    Local(crate::question::OwnedQuestionWait),
}

impl PreparedQuestionWait {
    /// Publish through the owner before releasing the ordered tool mutation barrier.
    pub(in crate::tools) async fn publish(
        self,
    ) -> crate::tools::ToolResult<crate::question::OwnedQuestionWait> {
        match self {
            Self::Owner { events, creation } => Ok(events.begin_wait(creation).await?),
            #[cfg(test)]
            Self::Local(wait) => Ok(wait),
        }
    }
}

/// Capture the exact question scope while the tool runtime is held; publication happens after release.
pub(in crate::tools) fn prepare_wait(
    runtime: &crate::tools::runtime::ControlToolRuntime,
    invocation: &crate::tools::ControlToolInvocation,
) -> crate::tools::ToolResult<PreparedQuestionWait> {
    crate::tools::validate_arguments(invocation)?;
    runtime
        .context
        .questions
        .as_ref()
        .context("waiting questions require the session question controller")?;
    #[cfg(test)]
    let controller = runtime
        .context
        .questions
        .as_ref()
        .expect("the question controller was validated above");
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
    if let Some(events) = &runtime.context.question_events {
        return Ok(PreparedQuestionWait::Owner {
            events: events.clone(),
            creation: crate::exchange::QuestionCreation::from_tool(
                super::state::tool_call(runtime, invocation)?
                    .context("question creation has no authenticated tool call")?,
                exchange.into(),
                task,
                drafts,
                if runtime.context.question_discussion {
                    crate::exchange::QuestionCreationOrigin::Discussion
                } else {
                    crate::exchange::QuestionCreationOrigin::Workflow
                },
                authority.context("question creation has no captured tool authority")?,
            )?,
        });
    }
    #[cfg(test)]
    {
        Ok(PreparedQuestionWait::Local(
            controller
                .prepare_tool_wait(
                    &uuid::Uuid::new_v4().to_string(),
                    exchange,
                    task.as_ref(),
                    drafts,
                    authority.as_ref(),
                )?
                .into_test_owned(),
        ))
    }
    #[cfg(not(test))]
    {
        Err(crate::tools::ToolError::MissingState(
            "question creation owner is missing".into(),
        ))
    }
}
