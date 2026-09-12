//! Bounded provider delivery reports saturation independently of callback return values.

use std::{fmt, io};

use forge_protocol::outbound::{self, MessageReceiver, MessageSender};

use super::BackendEvent;

#[derive(Clone)]
pub struct BackendEventSink {
    sender: MessageSender,
}

pub struct BackendEventStream {
    receiver: MessageReceiver,
}

#[derive(Debug)]
pub struct EventDeliveryFailure(io::Error);

impl fmt::Display for EventDeliveryFailure {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "provider event delivery failed: {}", self.0)
    }
}

impl std::error::Error for EventDeliveryFailure {}

pub fn channel() -> (BackendEventSink, BackendEventStream) {
    let (sender, receiver) = outbound::channel();
    (BackendEventSink { sender }, BackendEventStream { receiver })
}

impl BackendEventSink {
    pub fn send(&self, event: BackendEvent) -> Result<(), EventDeliveryFailure> {
        self.sender.send(event).map_err(EventDeliveryFailure)
    }

    pub async fn failed(&self) -> EventDeliveryFailure {
        EventDeliveryFailure(self.sender.failed().await)
    }
}

impl BackendEventStream {
    pub fn check(&self) -> Result<(), EventDeliveryFailure> {
        self.receiver.check().map_err(EventDeliveryFailure)
    }

    pub async fn recv(&mut self) -> Result<Option<BackendEvent>, EventDeliveryFailure> {
        self.receiver
            .recv()
            .await
            .map_err(EventDeliveryFailure)?
            .map(|frame| {
                serde_json::from_slice(frame.bytes())
                    .map_err(|error| EventDeliveryFailure(io::Error::other(error)))
            })
            .transpose()
    }

    pub fn try_recv(&mut self) -> Result<BackendEvent, EventDeliveryFailure> {
        let frame = self.receiver.try_recv().map_err(EventDeliveryFailure)?;
        serde_json::from_slice(frame.bytes())
            .map_err(|error| EventDeliveryFailure(io::Error::other(error)))
    }
}

pub async fn failed(sink: Option<&BackendEventSink>) -> EventDeliveryFailure {
    match sink {
        Some(sink) => sink.failed().await,
        None => std::future::pending().await,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    fn event(sequence: usize) -> BackendEvent {
        BackendEvent {
            kind: "delta".into(),
            text: Some(sequence.to_string()),
            data: json!(sequence),
            activity: None,
            summary: None,
            task_update: None,
        }
    }

    #[tokio::test]
    async fn preserves_order_and_drains_after_producer_exit() {
        let (sink, mut stream) = channel();
        for sequence in 0..96 {
            sink.send(event(sequence)).unwrap();
        }
        drop(sink);
        for sequence in 0..96 {
            assert_eq!(stream.recv().await.unwrap().unwrap().data, json!(sequence));
        }
        assert!(stream.recv().await.unwrap().is_none());
    }

    #[tokio::test]
    async fn ignored_callback_failure_still_reaches_consumer() {
        let (sink, mut stream) = channel();
        for sequence in 0..96 {
            sink.send(event(sequence)).unwrap();
        }
        let _ = sink.send(event(96));
        assert!(sink.failed().await.to_string().contains("budget exhausted"));
        assert!(stream.recv().await.is_err());
        assert!(stream.try_recv().is_err());
        assert!(sink.send(event(97)).is_err());
    }

    #[tokio::test]
    async fn oversized_payload_fails_before_retention() {
        let (sink, mut stream) = channel();
        let mut oversized = event(0);
        oversized.text = Some("x".repeat(forge_protocol::MAX_FRAME_BYTES));
        assert!(sink.send(oversized).is_err());
        assert!(stream.recv().await.is_err());
    }
}
