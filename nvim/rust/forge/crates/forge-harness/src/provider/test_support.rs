use crate::provider::ScopeOwner;
mod collect;
mod fixture;
pub(crate) use collect::{ProviderTestExt, ProviderTestSession};
pub(crate) use fixture::{EventFixtureProvider, call_tool};

use std::sync::Arc;

use super::{
    HarnessEventSink, InvocationScope, MessageClassification, ProviderEvent, TestMessageContent,
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
        forge_provider_api::ProviderOperation::TurnStart,
        &error,
    )
}

/// Consume one deterministic fixture gate without adding native error semantics.
pub(crate) async fn wait_gate(gate: &tokio::sync::Semaphore) -> Result<(), super::ProviderError> {
    gate.acquire()
        .await
        .map_err(|_| {
            super::ProviderError::Failure(forge_provider_api::ProviderFailure::new(
                forge_provider_api::ProviderOperation::TurnStart,
                forge_provider_api::ProviderFailureKind::Rejected,
            ))
        })?
        .forget();
    Ok(())
}

/// Resolve only the host identities registered before native fixture dispatch.
pub(crate) fn address(scope: &InvocationScope) -> TurnAddress {
    TurnAddress {
        scope: scope.clone(),
        thread_id: scope.owner().thread_id(),
        turn_id: scope
            .turn_id()
            .expect("fixture invocation identity is nonempty"),
    }
}

pub(crate) fn message(
    turn: &TurnAddress,
    id: impl Into<String>,
    role: Option<TestMessageContent>,
    text: impl Into<String>,
) -> Vec<ProviderEvent> {
    let text = text.into();
    if text.is_empty() {
        return Vec::new();
    }
    use forge_provider_api::message::{
        AgentMessageRole, AgentMessageStart, ItemCompletion, ItemEvent, ItemStart,
        MessageCompletion, MessageOutcome, ReasoningStart,
    };
    let item_id =
        forge_provider_api::ItemId::new(id.into()).expect("fixture item identity is nonempty");
    let start = match role {
        Some(TestMessageContent::Reasoning(kind)) => ItemStart::Reasoning(ReasoningStart {
            text: text.clone(),
            kind,
        }),
        Some(TestMessageContent::Commentary) => ItemStart::AgentMessage(AgentMessageStart {
            text: text.clone(),
            classification: MessageClassification::Known(AgentMessageRole::Commentary),
        }),
        Some(TestMessageContent::Response) => ItemStart::AgentMessage(AgentMessageStart {
            text: text.clone(),
            classification: MessageClassification::Known(AgentMessageRole::Response),
        }),
        None => ItemStart::AgentMessage(AgentMessageStart {
            text: text.clone(),
            classification: MessageClassification::Unclassified,
        }),
    };
    let completion = MessageCompletion {
        text,
        outcome: MessageOutcome::Completed,
    };
    let completion = match &start {
        ItemStart::Reasoning(_) => ItemCompletion::Reasoning(completion),
        ItemStart::AgentMessage(_) => ItemCompletion::AgentMessage(completion),
        ItemStart::UserMessage(_)
        | ItemStart::CommandExecution(_)
        | ItemStart::FileChange(_)
        | ItemStart::ToolCall(_) => {
            unreachable!("message fixture constructs assistant prose or reasoning only")
        }
    };
    [ItemEvent::Started(start), ItemEvent::Completed(completion)]
        .into_iter()
        .map(|event| ProviderEvent::Item {
            thread_id: turn.thread_id.clone(),
            turn_id: turn.turn_id.clone(),
            item_id: item_id.clone(),
            event,
        })
        .collect()
}

/// Build native tool item observations from the shared normalized value vocabulary.
pub(crate) fn tool(
    turn: &TurnAddress,
    id: String,
    start: forge_provider_api::message::ItemStart,
    update: impl IntoIterator<Item = forge_provider_api::message::ItemEvent>,
) -> Vec<ProviderEvent> {
    let item_id =
        forge_provider_api::ItemId::new(id).expect("fixture tool item identity is nonempty");
    std::iter::once(forge_provider_api::message::ItemEvent::Started(start))
        .chain(update)
        .map(|event| ProviderEvent::Item {
            thread_id: turn.thread_id.clone(),
            turn_id: turn.turn_id.clone(),
            item_id: item_id.clone(),
            event,
        })
        .collect()
}

pub(crate) enum FixtureBatch {
    Observation(Box<super::session_execution::OwnerObservation>),
    Observations(Vec<super::session_execution::OwnerObservation>),
    Event(Box<ProviderEvent>),
    Sequence(Vec<ProviderEvent>),
}

impl From<super::session_execution::OwnerObservation> for FixtureBatch {
    /// Retain an already scoped host owner observation without translating it into native activity.
    fn from(event: super::session_execution::OwnerObservation) -> Self {
        Self::Observation(Box::new(event))
    }
}
impl From<Vec<super::session_execution::OwnerObservation>> for FixtureBatch {
    /// Preserve ordered host owner observations in one reducer fixture batch.
    fn from(event: Vec<super::session_execution::OwnerObservation>) -> Self {
        Self::Observations(event)
    }
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
    sink: Option<HarnessEventSink>,
    trace: &crate::trace::TraceStore,
    store: &Arc<super::submission::SubmissionStore>,
) -> anyhow::Result<super::ProviderCompletion> {
    Ok(provider
        .session_prompt_with_retry(request, sink, trace, store)
        .await?
        .completion)
}

/// Collect a real retained turn for synchronous reducer fixtures without terminal publication access.
pub(crate) fn completed_turn(
    scope: InvocationScope,
    result: forge_provider_api::TurnResult,
) -> Result<forge_provider_api::CompletedTurn, super::ProviderError> {
    std::thread::spawn(move || {
        let runtime = tokio::runtime::Builder::new_current_thread()
            .enable_all()
            .build()
            .map_err(io_failure)?;
        runtime.block_on(async move {
            let binding = forge_provider_api::BindingId::new(uuid::Uuid::new_v4().to_string())
                .expect("generated fixture binding is nonempty");
            let (sink, mut stream) = forge_provider_api::events::channel(binding, scope.clone());
            let address = TurnAddress {
                thread_id: scope.owner().thread_id(),
                turn_id: scope.turn_id().map_err(|_| {
                    super::ProviderError::Observation(
                        forge_provider_api::ObservationFailure::OwnershipConflict,
                    )
                })?,
                scope,
            };
            let runner = forge_providers::testing::RetainedTurn::spawn(
                Arc::new(CompletedFixtureDriver { result }),
                sink,
                address,
            )?;
            let join = runner.join();
            tokio::pin!(join);
            loop {
                tokio::select! {
                    completed = &mut join => return completed,
                    event = stream.recv() => { event?; }
                }
            }
        })
    })
    .join()
    .map_err(|_| {
        super::ProviderError::Observation(
            forge_provider_api::ObservationFailure::LifecycleUnavailable,
        )
    })?
}

struct CompletedFixtureDriver {
    result: forge_provider_api::TurnResult,
}
#[async_trait::async_trait]
impl forge_providers::testing::NativeTurnDriver for CompletedFixtureDriver {
    /// Establish a real native start and return the fixture's one terminal result.
    async fn run(
        &self,
        start: forge_provider_api::NativeTurnStart,
    ) -> Result<forge_provider_api::TurnResult, super::ProviderError> {
        start.observed(self.result.completed_at_ms()).await?;
        Ok(self.result.clone())
    }
    /// Preserve the fixture's already established native outcome on interruption.
    async fn interrupt(&self) -> Result<(), super::ProviderError> {
        Ok(())
    }
    /// Certify this fixture owns no remaining native resources.
    async fn collect(&self) -> Result<(), super::ProviderError> {
        Ok(())
    }
}

/// Account for this fixture's exact accepted request in its native result.
pub(crate) fn native_result(
    request: &forge_provider_api::PromptRequest,
    outcome: forge_provider_api::turn::TurnOutcome,
) -> forge_provider_api::TurnResult {
    forge_provider_api::TurnResult::new(
        outcome,
        super::retry::now_ms(),
        None,
        request.input_coverage(),
        Vec::new(),
        None,
    )
    .expect("one accepted fixture input has nonconflicting coverage")
}

/// Restrict fixture observations to the same activity vocabulary available to real adapters.
pub(crate) fn activity(
    event: ProviderEvent,
) -> Result<forge_provider_api::AdapterActivity, super::ProviderError> {
    use forge_provider_api::{
        AdapterActivity, ProviderConnectionEvent, TurnEvent, TurnMetadataEvent,
    };
    match event {
        ProviderEvent::Thread { thread_id, event } => {
            Ok(AdapterActivity::Thread { thread_id, event })
        }
        ProviderEvent::Item {
            thread_id,
            turn_id,
            item_id,
            event,
        } => Ok(AdapterActivity::Item {
            thread_id,
            turn_id,
            item_id,
            event,
        }),
        ProviderEvent::Agent { agent_id, event } => Ok(AdapterActivity::Agent { agent_id, event }),
        ProviderEvent::Turn {
            thread_id,
            turn_id,
            event,
        } => {
            let event = match event {
                TurnEvent::Error { error } => TurnMetadataEvent::Error { error },
                TurnEvent::ModelResolved(model) => TurnMetadataEvent::ModelResolved(model),
                TurnEvent::Usage(usage) => TurnMetadataEvent::Usage(usage),
                TurnEvent::ChecklistUpdated(checklist) => {
                    TurnMetadataEvent::ChecklistUpdated(checklist)
                }
                TurnEvent::Started(_) | TurnEvent::Completed(_) => {
                    return Err(super::ProviderError::Observation(
                        forge_provider_api::ObservationFailure::Protocol,
                    ));
                }
            };
            Ok(AdapterActivity::Turn {
                thread_id,
                turn_id,
                event,
            })
        }
        ProviderEvent::Provider {
            event: ProviderConnectionEvent::Warning(warning),
        } => Ok(AdapterActivity::Warning(warning)),
        ProviderEvent::Provider {
            event:
                ProviderConnectionEvent::CapabilitiesChanged(_)
                | ProviderConnectionEvent::Unavailable(_),
        } => Err(super::ProviderError::Observation(
            forge_provider_api::ObservationFailure::Protocol,
        )),
    }
}

/// Supply explicit deterministic provenance for reducer fixtures without claiming live native delivery.
pub(crate) fn native_event(
    scope: impl Into<super::EventScope>,
    event: ProviderEvent,
) -> super::HarnessEvent {
    static FIXTURE_SEQUENCE: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(1);
    let sequence = FIXTURE_SEQUENCE.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
    super::HarnessEvent::Native {
        source: super::NativeEventSource {
            binding_id: forge_provider_api::BindingId::new("fixture-binding")
                .expect("static fixture identity is nonempty"),
            sequence: forge_provider_api::EventSequence::new(sequence),
        },
        scope: scope.into(),
        event: Box::new(event),
    }
}

/// Create observed-child ownership evidence through the production registry in fixtures.
pub(crate) use super::ownership::observed_agent_fixture;

/// Consume the host-owned manifest that a successful binding open publishes before native events.
pub(crate) async fn expect_negotiated_capabilities(
    stream: &mut super::bridge::HostEventStream,
) -> anyhow::Result<forge_provider_api::CapabilityManifest> {
    let observed = stream
        .recv()
        .await?
        .ok_or_else(|| anyhow::anyhow!("binding stream ended before negotiated capabilities"))?;
    anyhow::ensure!(
        observed.native.is_none(),
        "negotiated capabilities are a host publication without a native sequence"
    );
    match observed.event {
        super::session_execution::OwnerObservation::Event(super::HarnessEvent::Host {
            scope: forge_provider_api::EventScope::Binding { .. },
            event: super::HostEvent::CapabilitiesNegotiated { manifest, .. },
        }) => Ok(manifest),
        other => anyhow::bail!("expected negotiated capabilities, observed {other:?}"),
    }
}
