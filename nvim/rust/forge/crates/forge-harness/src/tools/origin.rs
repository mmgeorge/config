//! The `origin_message` tool, through which an aside session writes to the session it forked from.
//!
//! An aside reads its origin with the session-history tools and has no other way to affect it.
//! This tool is that one write: it queues a message in the origin's input, exactly where the
//! origin's user would have typed it. The origin's model receives it as a `<session_message>` that
//! names the aside, never as the user's own words, so another model's text cannot borrow the
//! origin user's authority.

use super::schema::{strict_object_input_schema, string_schema};
use super::{ControlToolDefinition, ControlToolInvocation, ControlToolResult, ToolResult};
use crate::broker::session_message::{
    SessionMessage, SessionMessageDelivery, SessionMessageRouter,
};
use crate::session::{AsideOrigin, HarnessSession};
use serde::Deserialize;
use serde_json::json;
use std::sync::Arc;

pub(in crate::tools) const NAME: &str = "origin_message";

/// Names an aside session's origin and carries its messages there.
pub(crate) struct AsideToolScope {
    origin: AsideOrigin,
    aside_session_id: String,
    aside_session_name: String,
    router: Arc<SessionMessageRouter>,
}

impl AsideToolScope {
    /// Build the scope for an aside session, or `None` for every other session.
    pub(crate) fn open(
        session: &HarnessSession,
        router: &Arc<SessionMessageRouter>,
    ) -> Option<Arc<Self>> {
        let origin = session.aside.clone()?;
        Some(Arc::new(Self {
            origin,
            aside_session_id: session.id.clone(),
            aside_session_name: session.name.clone(),
            router: Arc::clone(router),
        }))
    }

    /// Describe the origin as the standing `<aside>` section.
    pub(crate) fn section(&self) -> forge_provider_api::forge::Aside {
        forge_provider_api::forge::Aside {
            origin_session_id: self.origin.session_id.clone(),
            origin_session_name: self.origin.session_name.clone(),
        }
    }

    /// Name this aside as the sender of a message, so the origin's model and user can tell it
    /// from text typed in the origin's own composer.
    fn message(&self, text: &str) -> forge_provider_api::forge::SessionMessage {
        forge_provider_api::forge::SessionMessage {
            from_session_id: self.aside_session_id.clone(),
            from_session_name: self.aside_session_name.clone(),
            text: text.to_owned(),
        }
    }
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct OriginMessage {
    message: String,
    #[serde(default)]
    delivery: SessionMessageDelivery,
}

pub(in crate::tools) fn definitions() -> Vec<ControlToolDefinition> {
    vec![ControlToolDefinition {
        name: NAME,
        description: "Send a message to the origin session this aside was forked from. The message arrives there as a \
                message that names this session. delivery `queue`, the default, starts a turn when the \
                origin is idle and otherwise waits behind its running turn and queued input. delivery `steer` \
                enters the origin's running turn at once, as the user's own steering would. The call returns \
                once the origin has queued the message, not when it answers; read the answer later with \
                harness_session_exchange_list.",
        input_schema: strict_object_input_schema(
            vec![
                ("message", string_schema()),
                (
                    "delivery",
                    json!({ "type": "string", "enum": ["queue", "steer"] }),
                ),
            ],
            &["message"],
        ),
    }]
}

pub(in crate::tools) fn is_tool(name: &str) -> bool {
    name == NAME
}

/// Queue the message in the origin session's input.
pub(in crate::tools) async fn execute(
    runtime: &mut super::runtime::ControlToolRuntime,
    invocation: ControlToolInvocation,
    _output: crate::provider::ProviderCompletion,
) -> ToolResult<ControlToolResult> {
    let Some(scope) = runtime.context.aside.clone() else {
        return Err(super::ToolError::Rejected(super::unavailable_tool_reason(
            NAME,
        )));
    };
    let input: OriginMessage = super::invocation::decode_arguments(&invocation)?;
    let message = input.message.trim();
    crate::tools::error::ensure!(!message.is_empty(), "message must not be empty");
    let input_id = uuid::Uuid::new_v4().to_string();
    scope
        .router
        .deliver(SessionMessage {
            target_session_id: scope.origin.session_id.clone(),
            input_id: input_id.clone(),
            content: scope.message(message),
            delivery: input.delivery,
        })
        .await?;
    Ok(ControlToolResult {
        invocation: None,
        message: json!({
            "delivered": true,
            "origin_session_id": scope.origin.session_id,
            "input_id": input_id,
        })
        .to_string(),
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::provider::PromptMode;
    use crate::tools::{ControlToolRegistry, ControlTurnContext};

    fn aside_scope() -> Arc<AsideToolScope> {
        Arc::new(AsideToolScope {
            origin: AsideOrigin {
                session_id: "origin".into(),
                session_name: "main".into(),
            },
            aside_session_id: "aside-1".into(),
            aside_session_name: String::new(),
            router: Arc::default(),
        })
    }

    fn offered(context: &ControlTurnContext, root: bool) -> bool {
        ControlToolRegistry
            .definition_list_for_context(Some(context), root)
            .iter()
            .any(|definition| definition.name == NAME)
    }

    /// Offer the tool to an aside's primary agent only: not to its child agents, and not to a
    /// session that names no origin.
    #[test]
    fn only_the_primary_agent_of_an_aside_is_offered_the_tool() {
        let mut context = ControlTurnContext::inactive(PromptMode::Chat);
        assert!(!offered(&context, true));
        context.aside = Some(aside_scope());
        assert!(offered(&context, true));
        assert!(!offered(&context, false));
    }

    /// Reject a call from a session with no origin, and report a host that routes no messages
    /// instead of dropping the message.
    #[tokio::test]
    async fn calls_fail_without_an_origin_or_a_message_route() {
        let invocation = || ControlToolInvocation {
            call_id: None,
            name: NAME.into(),
            arguments: json!({"message": "The cache key is wrong."}),
            task_permit: None,
        };
        let mut ordinary = crate::tools::runtime::ControlToolRuntime::new(
            ControlTurnContext::inactive(PromptMode::Chat),
        );
        let rejection = ordinary.invoke(invocation()).await.unwrap_err();
        assert!(
            format!("{rejection:?}").contains("aside session"),
            "{rejection:?}"
        );

        let mut context = ControlTurnContext::inactive(PromptMode::Chat);
        context.aside = Some(aside_scope());
        let mut aside = crate::tools::runtime::ControlToolRuntime::new(context);
        let undelivered = aside.invoke(invocation()).await.unwrap_err();
        assert!(
            format!("{undelivered:?}").contains("cannot deliver messages between sessions"),
            "{undelivered:?}"
        );
    }

    /// Name the sending aside, by name when it has one, ahead of the message text.
    #[test]
    fn messages_name_the_sending_aside() {
        let scope = |name: &str| AsideToolScope {
            origin: AsideOrigin {
                session_id: "origin".into(),
                session_name: "main".into(),
            },
            aside_session_id: "aside-1".into(),
            aside_session_name: name.into(),
            router: Arc::default(),
        };
        assert_eq!(
            scope("review notes").message("The cache key is wrong."),
            forge_provider_api::forge::SessionMessage {
                from_session_id: "aside-1".into(),
                from_session_name: "review notes".into(),
                text: "The cache key is wrong.".into(),
            }
        );
        assert_eq!(scope("").message("Done.").from_session_name, "");
    }
}
