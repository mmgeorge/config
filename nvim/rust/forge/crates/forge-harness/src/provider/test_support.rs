mod collect;
pub(crate) use super::session_events::{SessionEvents, TerminalBoundary};
pub(crate) use collect::{ProviderTestExt, ProviderTestSession};

use std::collections::HashSet;
use std::sync::{Arc, Mutex};

use super::{
    ExecutionOwner, InvocationScope, MessageAddress, MessageClassification, MessageId, MessageRole,
    MessageUpdate, ProviderError, ProviderEvent, ProviderEventSink, ToolAddress, ToolInvocation,
    ToolUpdate, TurnAddress, TurnUpdate,
};

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

#[derive(Default)]
struct Admission {
    turn: HashSet<TurnAddress>,
    settled: HashSet<TurnAddress>,
    tool: HashSet<ToolAddress>,
}

#[derive(Clone)]
pub(crate) struct FixtureStream {
    sink: ProviderEventSink,
    admission: Arc<Mutex<Admission>>,
}

impl FixtureStream {
    pub(crate) fn new(sink: ProviderEventSink) -> Self {
        Self {
            sink,
            admission: Arc::new(Mutex::new(Admission::default())),
        }
    }

    fn prepare(&self, batch: FixtureBatch) -> Vec<ProviderEvent> {
        let source = match batch {
            FixtureBatch::Event(event) => vec![*event],
            FixtureBatch::Sequence(event) => event,
        };
        let mut admission = self.admission.lock().expect("fixture admission lock");
        let mut event = Vec::new();
        for update in source {
            if let Some(address) = update.turn_address()
                && admission.turn.insert(address.clone())
            {
                let observation = forge_provider_api::lifecycle::ExecutionObservation::new(
                    address.scope.clone(),
                    forge_provider_api::lifecycle::ExecutionUpdate::Started {
                        address: address.clone(),
                    },
                )
                .expect("fixture execution address carries its own invocation scope");
                event.push(ProviderEvent::Execution { observation });
                if !matches!(
                    &update,
                    ProviderEvent::Turn {
                        update: TurnUpdate::Started { .. },
                        ..
                    }
                ) {
                    event.push(ProviderEvent::Turn {
                        address: address.clone(),
                        update: TurnUpdate::Started { started_at_ms: 0 },
                    });
                }
            }
            if let ProviderEvent::Tool {
                address,
                update: ToolUpdate::Requested { .. },
            } = &update
                && !admission.tool.insert(address.clone())
            {
                continue;
            }
            if let ProviderEvent::Turn {
                address,
                update: TurnUpdate::Finished { .. },
            } = &update
            {
                admission.settled.insert(address.clone());
            }
            event.push(update);
        }
        event
    }

    pub(crate) async fn send_wait(
        &self,
        batch: impl Into<FixtureBatch>,
    ) -> Result<(), super::events::EventDeliveryError> {
        for event in self.prepare(batch.into()) {
            self.sink.send_wait(event).await?;
        }
        Ok(())
    }

    pub(crate) fn send(
        &self,
        batch: impl Into<FixtureBatch>,
    ) -> Result<(), super::events::EventDeliveryError> {
        for event in self.prepare(batch.into()) {
            self.sink.send(event)?;
        }
        Ok(())
    }

    pub(crate) async fn finish(
        &self,
        result: &Result<super::ProviderCompletion, ProviderError>,
    ) -> Result<(), ProviderError> {
        let outcome = match result {
            Ok(_) => crate::turn::TurnOutcome::Completed,
            Err(ProviderError::Cancelled) => crate::turn::TurnOutcome::Cancelled,
            Err(_) => crate::turn::TurnOutcome::Failed,
        };
        let addresses: Vec<_> = {
            let admission = self.admission.lock().expect("fixture admission lock");
            admission
                .turn
                .iter()
                .filter(|turn| !admission.settled.contains(*turn))
                .cloned()
                .collect()
        };
        for address in addresses {
            self.sink
                .send_wait(ProviderEvent::Turn {
                    address,
                    update: TurnUpdate::Finished {
                        outcome,
                        completed_at_ms: 100,
                    },
                })
                .await?;
        }
        Ok(())
    }
}

/// Supply an explicit observation drain for retry-policy tests that inspect only completion.
pub(crate) async fn retry_prompt<P>(
    provider: &ProviderTestSession<P>,
    mut request: super::ProviderRequest,
    sink: Option<ProviderEventSink>,
    trace: &crate::trace::TraceStore,
    store: &Arc<super::submission::SubmissionStore>,
) -> anyhow::Result<super::ProviderCompletion>
where
    P: super::ProviderAdapter<Host = super::HarnessProviderHost> + ?Sized,
{
    if request.submission.is_none() {
        request.submission = Some(store.prepare(&request, provider.descriptor().kind)?);
    }
    let (sink, mut drain) = match sink {
        Some(sink) => (sink, None),
        None => {
            let (sink, stream) = super::events::channel(InvocationScope::new(
                request.owner()?,
                uuid::Uuid::new_v4().to_string(),
            ));
            (sink, Some(stream))
        }
    };
    let pending =
        super::retry::run_policy(request, sink, trace, |mut request, sink, _| async move {
            if request.submission.is_none() {
                request.submission = Some(store.prepare(&request, provider.descriptor().kind)?);
            }
            provider.session_prompt_and_wait(request, sink).await
        });
    tokio::pin!(pending);
    loop {
        tokio::select! {
            result = &mut pending => return result,
            event = async { match &mut drain { Some(stream) => stream.recv().await, None => std::future::pending().await } } => { event?; },
        }
    }
}
