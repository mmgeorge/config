use std::{fmt, io, time::Duration};

use forge_protocol::outbound::{self, MessageReceiver, MessageSender};

use super::{
    BackendError, BackendEvent, EventScope, InvocationScope, MessageUpdate, TextUpdate, ToolUpdate,
};

#[derive(Clone)]
pub struct BackendEventSink {
    scope: EventScope,
    sender: MessageSender,
    delivery: std::sync::Arc<tokio::sync::Mutex<()>>,
    skill: Option<std::sync::Arc<std::sync::Mutex<crate::skill::SkillObserver>>>,
}

pub struct BackendEventStream {
    input: EventReceiver,
    pending: Option<BackendEvent>,
    queued: Option<BackendEvent>,
    deadline: tokio::time::Instant,
}

struct EventReceiver {
    receiver: MessageReceiver,
    transfer: Option<(usize, usize, usize, String)>,
}

#[derive(Clone, Debug)]
pub enum EventDeliveryError {
    Closed {
        message: String,
    },
    OwnershipMismatch {
        expected: Box<EventScope>,
        received: Box<EventScope>,
    },
    InvalidTransportData {
        message: String,
    },
    IncompleteTransfer {
        message: String,
    },
    Empty,
}

impl EventDeliveryError {
    fn transport(error: impl fmt::Display) -> Self {
        Self::InvalidTransportData {
            message: error.to_string(),
        }
    }

    fn queue(error: io::Error) -> Self {
        match error.kind() {
            io::ErrorKind::WouldBlock => Self::Empty,
            io::ErrorKind::UnexpectedEof | io::ErrorKind::BrokenPipe => Self::Closed {
                message: error.to_string(),
            },
            _ => Self::transport(error),
        }
    }
}

impl fmt::Display for EventDeliveryError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Closed { message } => {
                write!(formatter, "provider event delivery closed: {message}")
            }
            Self::OwnershipMismatch { expected, received } => write!(
                formatter,
                "provider event crosses execution ownership: expected {expected:?}, received {received:?}"
            ),
            Self::InvalidTransportData { message } => write!(
                formatter,
                "invalid provider event transport data: {message}"
            ),
            Self::IncompleteTransfer { message } => {
                write!(formatter, "incomplete provider event transfer: {message}")
            }
            Self::Empty => formatter.write_str("provider event queue is empty"),
        }
    }
}

impl std::error::Error for EventDeliveryError {}

pub fn channel(scope: impl Into<EventScope>) -> (BackendEventSink, BackendEventStream) {
    let (sender, receiver) = outbound::channel();
    (
        BackendEventSink {
            scope: scope.into(),
            sender,
            delivery: Default::default(),
            skill: None,
        },
        BackendEventStream {
            input: EventReceiver {
                receiver,
                transfer: None,
            },
            pending: None,
            queued: None,
            deadline: tokio::time::Instant::now(),
        },
    )
}

impl BackendEventSink {
    pub fn scope(&self) -> &EventScope {
        &self.scope
    }

    pub fn invocation_scope(&self) -> Result<&InvocationScope, BackendError> {
        self.scope
            .invocation()
            .ok_or_else(|| BackendError::Protocol {
                message: "provider prompt requires an admitted invocation scope".into(),
            })
    }

    pub(crate) fn skills(
        &self,
        observer: std::sync::Arc<std::sync::Mutex<crate::skill::SkillObserver>>,
    ) -> Self {
        Self {
            skill: Some(observer),
            ..self.clone()
        }
    }

    fn validate(&self, event: &BackendEvent) -> Result<(), EventDeliveryError> {
        let received = event.scope();
        if received != self.scope {
            let error = EventDeliveryError::OwnershipMismatch {
                expected: Box::new(self.scope.clone()),
                received: Box::new(received),
            };
            self.sender
                .record_failure(&io::Error::other(error.to_string()));
            return Err(error);
        }
        Ok(())
    }

    pub async fn send_wait(&self, event: BackendEvent) -> Result<(), EventDeliveryError> {
        self.validate(&event)?;
        let _delivery = self.delivery.lock().await;
        let read = if let Some(skill) = &self.skill {
            skill
                .lock()
                .map_err(|_| {
                    let error = EventDeliveryError::transport("skill observer lock poisoned");
                    self.sender
                        .record_failure(&io::Error::other(error.to_string()));
                    error
                })?
                .observe(&event)
        } else {
            Vec::new()
        };
        let skill_event = match &event {
            BackendEvent::Tool { address, .. } if !read.is_empty() => Some(BackendEvent::Tool {
                address: address.clone(),
                update: ToolUpdate::SkillRead { read },
            }),
            _ => None,
        };
        self.send_encoded(event).await?;
        if let Some(event) = skill_event {
            self.send_encoded(event).await?;
        }
        Ok(())
    }

    async fn send_encoded(&self, event: BackendEvent) -> Result<(), EventDeliveryError> {
        let transfer = forge_protocol::transfer::JsonTransfer::new(&event).map_err(|error| {
            self.sender.record_failure(&error);
            EventDeliveryError::transport(error)
        })?;
        if transfer.total_bytes() < forge_protocol::MAX_FRAME_BYTES / 2 {
            return self
                .sender
                .send_wait(event)
                .await
                .map_err(EventDeliveryError::queue);
        }
        for part in transfer {
            self.sender
                .send_wait(serde_json::json!({"forge_event_part": part}))
                .await
                .map_err(EventDeliveryError::queue)?;
        }
        Ok(())
    }

    #[cfg(test)]
    pub(crate) fn send(&self, event: BackendEvent) -> Result<(), EventDeliveryError> {
        self.validate(&event)?;
        self.sender.send(event).map_err(EventDeliveryError::queue)
    }

    pub async fn failed(&self) -> EventDeliveryError {
        EventDeliveryError::queue(self.sender.failed().await)
    }
}

impl EventReceiver {
    fn accept(&mut self, bytes: &[u8]) -> Result<Option<BackendEvent>, EventDeliveryError> {
        let value: serde_json::Value =
            serde_json::from_slice(bytes).map_err(EventDeliveryError::transport)?;
        let Some(part) = value.get("forge_event_part") else {
            if let Some((sequence, count, bytes, payload)) = &self.transfer {
                return Err(EventDeliveryError::IncompleteTransfer {
                    message: format!(
                        "interrupted after {sequence}/{count} parts and {}/{bytes} bytes",
                        payload.len()
                    ),
                });
            }
            return serde_json::from_value(value)
                .map(Some)
                .map_err(EventDeliveryError::transport);
        };
        let part: forge_protocol::transfer::JsonPart =
            serde_json::from_value(part.clone()).map_err(EventDeliveryError::transport)?;
        if part.part_count == 0
            || part.part_count > part.total_bytes
            || part.payload.is_empty()
            || part.payload.len() > forge_protocol::MAX_SNAPSHOT_PART_BYTES
        {
            return Err(EventDeliveryError::transport(format!(
                "invalid part: sequence {}, count {}, total bytes {}, payload bytes {}",
                part.sequence,
                part.part_count,
                part.total_bytes,
                part.payload.len()
            )));
        }
        let transfer = self
            .transfer
            .get_or_insert_with(|| (0, part.part_count, part.total_bytes, String::new()));
        if part.sequence != transfer.0
            || part.part_count != transfer.1
            || part.total_bytes != transfer.2
            || transfer
                .3
                .len()
                .checked_add(part.payload.len())
                .is_none_or(|length| length > transfer.2)
        {
            return Err(EventDeliveryError::transport(format!(
                "expected sequence {}, count {}, bytes {} with {} bytes retained; received sequence {}, count {}, bytes {}, payload bytes {}",
                transfer.0,
                transfer.1,
                transfer.2,
                transfer.3.len(),
                part.sequence,
                part.part_count,
                part.total_bytes,
                part.payload.len()
            )));
        }
        transfer.0 += 1;
        transfer.3.push_str(&part.payload);
        if transfer.0 != transfer.1 {
            return Ok(None);
        }
        let length = transfer.2;
        let payload = std::mem::take(&mut transfer.3);
        self.transfer = None;
        if payload.len() != length {
            return Err(EventDeliveryError::IncompleteTransfer {
                message: format!("completed with {} bytes, expected {length}", payload.len()),
            });
        }
        serde_json::from_str(&payload)
            .map(Some)
            .map_err(EventDeliveryError::transport)
    }

    fn closed(&self) -> Result<Option<BackendEvent>, EventDeliveryError> {
        if let Some((sequence, count, bytes, payload)) = &self.transfer {
            return Err(EventDeliveryError::IncompleteTransfer {
                message: format!(
                    "closed after {sequence}/{count} transfer parts and {}/{bytes} bytes",
                    payload.len()
                ),
            });
        }
        Ok(None)
    }

    async fn receive(&mut self) -> Result<Option<BackendEvent>, EventDeliveryError> {
        loop {
            match self
                .receiver
                .recv()
                .await
                .map_err(EventDeliveryError::queue)?
            {
                Some(frame) => {
                    if let Some(event) = self.accept(frame.bytes())? {
                        return Ok(Some(event));
                    }
                }
                None => return self.closed(),
            }
        }
    }

    fn try_receive(&mut self) -> Result<BackendEvent, EventDeliveryError> {
        loop {
            let frame = match self.receiver.try_recv() {
                Ok(frame) => frame,
                Err(error) if error.kind() == io::ErrorKind::UnexpectedEof => {
                    self.closed()?;
                    return Err(EventDeliveryError::queue(error));
                }
                Err(error) => return Err(EventDeliveryError::queue(error)),
            };
            if let Some(event) = self.accept(frame.bytes())? {
                return Ok(event);
            }
        }
    }
}

impl BackendEventStream {
    pub fn try_next(&mut self) -> Result<Option<BackendEvent>, EventDeliveryError> {
        match self.try_recv() {
            Ok(event) => Ok(Some(event)),
            Err(EventDeliveryError::Empty | EventDeliveryError::Closed { .. }) => Ok(None),
            Err(error) => Err(error),
        }
    }

    pub fn check(&self) -> Result<(), EventDeliveryError> {
        self.input
            .receiver
            .check()
            .map_err(EventDeliveryError::queue)
    }

    pub async fn recv(&mut self) -> Result<Option<BackendEvent>, EventDeliveryError> {
        if self.pending.is_none() {
            self.pending = match self.queued.take() {
                Some(event) => Some(event),
                None => self.input.receive().await?,
            };
            self.deadline = tokio::time::Instant::now() + Duration::from_millis(16);
        }
        while let Some(pending) = self.pending.as_mut() {
            if !batchable(pending) || output_bytes(pending) >= 64 * 1024 {
                break;
            }
            let event = match tokio::time::timeout_at(self.deadline, self.input.receive()).await {
                Err(_) | Ok(Ok(None)) => break,
                Ok(Err(error)) => return Err(error),
                Ok(Ok(Some(event))) => event,
            };
            if !merge_output(pending, &event) {
                self.queued = Some(event);
                break;
            }
        }
        Ok(self.pending.take())
    }

    pub fn try_recv(&mut self) -> Result<BackendEvent, EventDeliveryError> {
        self.check()?;
        let mut event = match self.pending.take().or_else(|| self.queued.take()) {
            Some(event) => event,
            None => self.input.try_receive()?,
        };
        while batchable(&event) && output_bytes(&event) < 64 * 1024 {
            let next = match self.input.try_receive() {
                Ok(event) => event,
                Err(EventDeliveryError::Empty | EventDeliveryError::Closed { .. }) => break,
                Err(error) => return Err(error),
            };
            if !merge_output(&mut event, &next) {
                self.queued = Some(next);
                break;
            }
        }
        Ok(event)
    }
}

fn batchable(event: &BackendEvent) -> bool {
    event.message_delta().is_some() || event.tool_output_delta().is_some()
}

fn output_bytes(event: &BackendEvent) -> usize {
    event
        .message_delta()
        .or_else(|| event.tool_output_delta())
        .map_or(0, str::len)
}

fn merge_output(pending: &mut BackendEvent, event: &BackendEvent) -> bool {
    if output_bytes(pending).saturating_add(output_bytes(event)) > 64 * 1024 {
        return false;
    }
    match (pending, event) {
        (
            BackendEvent::Message {
                address: previous_address,
                update: MessageUpdate::Delta { text: previous },
            },
            BackendEvent::Message {
                address,
                update: MessageUpdate::Delta { text },
            },
        ) if previous_address == address => {
            previous.push_str(text);
            true
        }
        (
            BackendEvent::Tool {
                address: previous_address,
                update:
                    ToolUpdate::Output {
                        update: TextUpdate::Append(previous),
                    },
            },
            BackendEvent::Tool {
                address,
                update:
                    ToolUpdate::Output {
                        update: TextUpdate::Append(text),
                    },
            },
        ) if previous_address == address => {
            previous.push_str(text);
            true
        }
        _ => false,
    }
}

pub async fn failed(sink: Option<&BackendEventSink>) -> EventDeliveryError {
    match sink {
        Some(sink) => sink.failed().await,
        None => std::future::pending().await,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::backend::{ExecutionOwner, MessageAddress, MessageId, TurnAddress};
    use serde_json::json;

    fn scope() -> InvocationScope {
        InvocationScope {
            owner: ExecutionOwner::root("session"),
            exchange_id: "exchange".into(),
            invocation_id: "invocation".into(),
        }
    }

    fn message(id: &str, update: MessageUpdate) -> BackendEvent {
        BackendEvent::Message {
            address: MessageAddress {
                turn: TurnAddress {
                    scope: scope(),
                    thread_id: "thread".into(),
                    turn_id: "turn".into(),
                },
                message_id: MessageId::Native(id.into()),
            },
            update,
        }
    }

    #[tokio::test]
    async fn malformed_transfer_reports_expected_and_received_sequence() {
        let (sink, mut stream) = channel(scope());
        sink.sender
            .send(json!({"forge_event_part": {
                "sequence": 1, "part_count": 2, "total_bytes": 4, "payload": "ab"
            }}))
            .unwrap();
        let error = stream.recv().await.unwrap_err().to_string();
        assert!(error.contains("expected sequence 0"), "{error}");
        assert!(error.contains("received sequence 1"), "{error}");
    }

    #[tokio::test]
    async fn closed_transfer_reports_progress_instead_of_generic_eof() {
        let (sink, mut stream) = channel(scope());
        sink.sender
            .send(json!({"forge_event_part": {
                "sequence": 0, "part_count": 2, "total_bytes": 4, "payload": "ab"
            }}))
            .unwrap();
        drop(sink);
        let error = stream.recv().await.unwrap_err().to_string();
        assert!(error.contains("1/2 transfer parts"), "{error}");
        assert!(error.contains("2/4 bytes"), "{error}");
    }

    #[tokio::test]
    async fn foreign_invocation_scope_is_rejected_before_delivery() {
        let (sink, mut stream) = channel(scope());
        let mut foreign = scope();
        foreign.invocation_id = "foreign".into();
        let error = sink
            .send_wait(BackendEvent::Runtime {
                scope: foreign,
                update: crate::backend::RuntimeUpdate::ForegroundIdle,
            })
            .await
            .unwrap_err();
        assert!(matches!(
            error,
            EventDeliveryError::OwnershipMismatch { .. }
        ));
        assert!(stream.recv().await.is_err());
    }

    #[tokio::test]
    async fn message_batches_preserve_message_phase_and_snapshot_barriers() {
        let (sink, mut stream) = channel(scope());
        let producer = async {
            for _ in 0..100 {
                sink.send_wait(message("one", MessageUpdate::Delta { text: "λ".into() }))
                    .await
                    .unwrap();
            }
            sink.send_wait(message(
                "one",
                MessageUpdate::Completed {
                    text: "complete".into(),
                },
            ))
            .await
            .unwrap();
            sink.send_wait(message(
                "two",
                MessageUpdate::Delta {
                    text: "second".into(),
                },
            ))
            .await
            .unwrap();
        };
        let consumer = async {
            assert_eq!(
                stream.recv().await.unwrap().unwrap().message_delta(),
                Some("λ".repeat(100).as_str())
            );
            assert!(matches!(
                stream.recv().await.unwrap(),
                Some(BackendEvent::Message {
                    update: MessageUpdate::Completed { .. },
                    ..
                })
            ));
            assert_eq!(
                stream.recv().await.unwrap().unwrap().message_delta(),
                Some("second")
            );
        };
        tokio::join!(producer, consumer);
    }

    #[tokio::test]
    async fn typed_events_preserve_barriers_and_full_content_under_backpressure() {
        let (sink, mut stream) = channel(scope());
        let text = "λ\n".repeat(forge_protocol::MAX_FRAME_BYTES);
        let expected = text.clone();
        let producer = async move {
            sink.send_wait(message("large", MessageUpdate::Completed { text }))
                .await
                .unwrap();
        };
        let consumer = async {
            match stream.recv().await.unwrap().unwrap() {
                BackendEvent::Message {
                    update: MessageUpdate::Completed { text },
                    ..
                } => assert_eq!(text, expected),
                event => panic!("unexpected event {event:?}"),
            }
        };
        tokio::join!(producer, consumer);
    }

    #[tokio::test]
    async fn cancelled_receive_retains_partial_batch_for_final_drain() {
        let (sink, mut stream) = channel(scope());
        sink.send_wait(message(
            "one",
            MessageUpdate::Delta {
                text: "partial".into(),
            },
        ))
        .await
        .unwrap();
        assert!(
            tokio::time::timeout(Duration::from_millis(1), stream.recv())
                .await
                .is_err()
        );
        sink.send_wait(message(
            "one",
            MessageUpdate::Delta {
                text: " tail".into(),
            },
        ))
        .await
        .unwrap();
        assert_eq!(
            stream.try_recv().unwrap().message_delta(),
            Some("partial tail")
        );
    }
}
