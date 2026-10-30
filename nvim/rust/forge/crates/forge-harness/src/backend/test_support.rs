use std::collections::HashSet;
use std::sync::{Arc, Mutex};

use super::{
    BackendError, BackendEvent, BackendEventSink, ExecutionOwner, InvocationScope, MessageAddress,
    MessageClassification, MessageId, MessageRole, MessageUpdate, ToolAddress, ToolInvocation,
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
) -> Vec<BackendEvent> {
    let text = text.into();
    if text.is_empty() {
        return Vec::new();
    }
    let address = MessageAddress {
        turn: turn.clone(),
        message_id: MessageId::Native(id.into()),
    };
    vec![
        BackendEvent::Message {
            address: address.clone(),
            update: MessageUpdate::Started {
                classification: role.map_or(
                    MessageClassification::Unclassified,
                    MessageClassification::Known,
                ),
                text: text.clone(),
            },
        },
        BackendEvent::Message {
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
) -> Vec<BackendEvent> {
    let address = ToolAddress {
        turn: turn.clone(),
        tool_id: id.into(),
    };
    std::iter::once(ToolUpdate::Requested { invocation })
        .chain(update)
        .map(|update| BackendEvent::Tool {
            address: address.clone(),
            update,
        })
        .collect()
}

pub(crate) enum FixtureBatch {
    Event(BackendEvent),
    Sequence(Vec<BackendEvent>),
}

impl From<BackendEvent> for FixtureBatch {
    fn from(event: BackendEvent) -> Self {
        Self::Event(event)
    }
}

impl From<Vec<BackendEvent>> for FixtureBatch {
    fn from(event: Vec<BackendEvent>) -> Self {
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
    sink: BackendEventSink,
    admission: Arc<Mutex<Admission>>,
}

impl FixtureStream {
    pub(crate) fn new(sink: BackendEventSink) -> Self {
        Self {
            sink,
            admission: Arc::new(Mutex::new(Admission::default())),
        }
    }

    fn prepare(&self, batch: FixtureBatch) -> Vec<BackendEvent> {
        let source = match batch {
            FixtureBatch::Event(event) => vec![event],
            FixtureBatch::Sequence(event) => event,
        };
        let mut admission = self.admission.lock().expect("fixture admission lock");
        let mut event = Vec::new();
        for update in source {
            if let Some(address) = update.turn_address()
                && admission.turn.insert(address.clone())
                && !matches!(
                    &update,
                    BackendEvent::Turn {
                        update: TurnUpdate::Started { .. },
                        ..
                    }
                )
            {
                event.push(BackendEvent::Turn {
                    address: address.clone(),
                    update: TurnUpdate::Started { started_at_ms: 0 },
                });
            }
            if let BackendEvent::Tool {
                address,
                update: ToolUpdate::Requested { .. },
            } = &update
                && !admission.tool.insert(address.clone())
            {
                continue;
            }
            if let BackendEvent::Turn {
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
        result: &Result<super::BackendCompletion, BackendError>,
    ) -> Result<(), BackendError> {
        let outcome = match result {
            Ok(_) => crate::turn::TurnOutcome::Completed,
            Err(BackendError::Cancelled) => crate::turn::TurnOutcome::Cancelled,
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
                .send_wait(BackendEvent::Turn {
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
