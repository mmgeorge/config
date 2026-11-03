mod collect;
mod fixture;
pub(crate) use collect::{ProviderTestExt, ProviderTestSession};
pub(crate) use fixture::{EventFixtureProvider, call_tool};

use std::sync::Arc;

use super::{
    ExecutionOwner, InvocationScope, MessageAddress, MessageClassification, MessageId, MessageRole,
    MessageUpdate, ProviderEvent, ProviderEventSink, ToolAddress, ToolInvocation, ToolUpdate,
    TurnAddress,
};

/// Build the normalized provider mock behind the production Harness bridge.
pub(crate) fn build_mock(
    root: std::path::PathBuf,
    permissions: Arc<super::approval::PermissionCoordinator>,
    trace: Arc<crate::trace::TraceStore>,
) -> anyhow::Result<Arc<super::Provider>> {
    let native = forge_providers::registry::nonselectable_id()
        .ok_or_else(|| anyhow::anyhow!("registry has no test provider"))?;
    super::build(
        super::ProviderLaunch {
            kind: native,
            command: Vec::new(),
        },
        permissions,
        trace,
        root,
    )
}

/// Use registry-advertised capabilities for normalized lifecycle fixtures.
pub(crate) fn fixture_capability() -> super::ProviderCapability {
    let id = forge_providers::registry::nonselectable_id().expect("registry has a test provider");
    forge_providers::registry::resolve(&id)
        .expect("test provider is registered")
        .capability
}

/// Use the registry's launch defaults in tests that exercise host selection without native traffic.
pub(crate) fn default_launch() -> super::ProviderLaunch {
    let registration = forge_providers::registry::resolve(&forge_providers::registry::default_id())
        .expect("registry default is registered");
    super::ProviderLaunch {
        kind: registration.id,
        command: registration
            .command
            .iter()
            .map(|part| (*part).to_owned())
            .collect(),
    }
}

/// Construct a normalized reply for tests whose assertion concerns host admission.
pub(crate) fn question_reply(
    question_id: &crate::question::QuestionId,
    response: forge_provider_api::QuestionReplyResponse,
) -> forge_provider_api::QuestionReply {
    forge_provider_api::QuestionReply {
        question_id: question_id.as_str().to_owned(),
        revision: 1,
        message_id: "fixture-reply".into(),
        question: "Fixture question".into(),
        response,
    }
}

/// Classify a fixture's host filesystem failure without leaking its raw payload into events.
pub(crate) fn io_failure(error: std::io::Error) -> super::ProviderError {
    super::lifecycle_error::classify_source(
        forge_provider_api::ProviderOperation::AdmitInput,
        &error,
    )
}

/// Consume one deterministic fixture gate without adding native error semantics.
pub(crate) async fn wait_gate(gate: &tokio::sync::Semaphore) -> Result<(), super::ProviderError> {
    gate.acquire()
        .await
        .map_err(|_| {
            super::ProviderError::Failure(forge_provider_api::ProviderFailure::new(
                forge_provider_api::ProviderOperation::AdmitInput,
                forge_provider_api::ProviderFailureKind::Rejected,
            ))
        })?
        .forget();
    Ok(())
}

pub(crate) fn scope() -> InvocationScope {
    InvocationScope {
        owner: ExecutionOwner::root("session"),
        exchange_id: "exchange".into(),
        invocation_id: "invocation".into(),
    }
}

pub(crate) fn address(
    scope: &InvocationScope,
    thread: impl Into<String>,
    turn: impl Into<String>,
) -> TurnAddress {
    TurnAddress {
        scope: scope.clone(),
        thread_id: thread.into(),
        turn_id: turn.into().into(),
    }
}

pub(crate) fn message(
    turn: &TurnAddress,
    id: impl Into<String>,
    role: Option<MessageRole>,
    text: impl Into<String>,
) -> Vec<ProviderEvent> {
    let text = text.into();
    if text.is_empty() {
        return Vec::new();
    }
    let address = MessageAddress {
        turn: turn.clone(),
        message_id: MessageId::Native(id.into()),
    };
    vec![
        ProviderEvent::Message {
            address: address.clone(),
            update: MessageUpdate::Started {
                classification: role.map_or(
                    MessageClassification::Unclassified,
                    MessageClassification::Known,
                ),
                text: text.clone(),
            },
        },
        ProviderEvent::Message {
            address,
            update: MessageUpdate::Completed { text },
        },
    ]
}

pub(crate) fn tool(
    turn: &TurnAddress,
    id: String,
    invocation: ToolInvocation,
    update: impl IntoIterator<Item = ToolUpdate>,
) -> Vec<ProviderEvent> {
    let address = ToolAddress {
        turn: turn.clone(),
        tool_id: id.into(),
    };
    std::iter::once(ToolUpdate::Requested { invocation })
        .chain(update)
        .map(|update| ProviderEvent::Tool {
            address: address.clone(),
            update,
        })
        .collect()
}

pub(crate) enum FixtureBatch {
    Event(Box<ProviderEvent>),
    Sequence(Vec<ProviderEvent>),
}

impl From<ProviderEvent> for FixtureBatch {
    fn from(event: ProviderEvent) -> Self {
        Self::Event(Box::new(event))
    }
}

impl From<Vec<ProviderEvent>> for FixtureBatch {
    fn from(event: Vec<ProviderEvent>) -> Self {
        Self::Sequence(event)
    }
}

/// Supply an explicit observation drain for retry-policy tests that inspect only completion.
pub(crate) async fn retry_prompt(
    provider: &ProviderTestSession,
    request: super::ProviderRequest,
    sink: Option<ProviderEventSink>,
    trace: &crate::trace::TraceStore,
    store: &Arc<super::submission::SubmissionStore>,
) -> anyhow::Result<super::ProviderCompletion> {
    Ok(provider
        .session_prompt_with_retry(request, sink, trace, store)
        .await?
        .completion)
}
