//! Bounded provider delivery waits for capacity and preserves event order.

use std::{fmt, io, time::Duration};

use forge_protocol::outbound::{self, MessageReceiver, MessageSender};

use super::BackendEvent;

#[derive(Clone)]
pub struct BackendEventSink {
    sender: MessageSender,
    delivery: std::sync::Arc<tokio::sync::Mutex<()>>,
}

pub struct BackendEventStream {
    receiver: MessageReceiver,
    transfer: Option<(usize, usize, usize, String)>,
    pending: Option<BackendEvent>,
    queued: Option<BackendEvent>,
    deadline: tokio::time::Instant,
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
    (BackendEventSink { sender, delivery: Default::default() }, BackendEventStream {
        receiver,
        transfer: None,
        pending: None,
        queued: None,
        deadline: tokio::time::Instant::now(),
    })
}

impl BackendEventSink {
    /// Waits for bounded queue capacity without treating a burst as delivery failure.
    pub async fn send_wait(&self, mut event: BackendEvent) -> Result<(), EventDeliveryFailure> {
        event.received_at_ms.get_or_insert_with(|| std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH).unwrap_or_default().as_millis() as i64);
        let _delivery = self.delivery.lock().await;
        let transfer = forge_protocol::transfer::JsonTransfer::new(&event).map_err(|error| {
            self.sender.record_failure(&error);
            EventDeliveryFailure(error)
        })?;
        if transfer.total_bytes() < forge_protocol::MAX_FRAME_BYTES / 2 {
            return self.sender.send_wait(event).await.map_err(EventDeliveryFailure);
        }
        for part in transfer {
            self.sender.send_wait(serde_json::json!({"forge_event_part": part}))
                .await.map_err(EventDeliveryFailure)?;
        }
        Ok(())
    }

    #[cfg(test)]
    pub(crate) fn send(&self, event: BackendEvent) -> Result<(), EventDeliveryFailure> {
        self.sender.send(event).map_err(EventDeliveryFailure)
    }

    pub async fn failed(&self) -> EventDeliveryFailure {
        EventDeliveryFailure(self.sender.failed().await)
    }
}

impl BackendEventStream {
    fn accept(&mut self, bytes: &[u8]) -> Result<Option<BackendEvent>, EventDeliveryFailure> {
        let value: serde_json::Value = serde_json::from_slice(bytes)
            .map_err(|error| EventDeliveryFailure(io::Error::other(error)))?;
        let Some(part) = value.get("forge_event_part") else {
            if self.transfer.is_some() { return Err(EventDeliveryFailure(io::Error::other("interrupted event transfer"))); }
            return serde_json::from_value(value).map(Some).map_err(|error| EventDeliveryFailure(io::Error::other(error)));
        };
        let part: forge_protocol::transfer::JsonPart = serde_json::from_value(part.clone())
            .map_err(|error| EventDeliveryFailure(io::Error::other(error)))?;
        if part.total_bytes > forge_protocol::MAX_SNAPSHOT_BYTES || part.part_count == 0
            || part.part_count > 512 || part.payload.is_empty()
            || part.payload.len() > forge_protocol::MAX_SNAPSHOT_PART_BYTES {
            return Err(EventDeliveryFailure(io::Error::other("event transfer exceeds its limit")));
        }
        let transfer = self.transfer.get_or_insert_with(|| (0, part.part_count, part.total_bytes, String::new()));
        if part.sequence != transfer.0 || part.part_count != transfer.1 || part.total_bytes != transfer.2
            || transfer.3.len() + part.payload.len() > transfer.2 {
            return Err(EventDeliveryFailure(io::Error::other("invalid event transfer sequence")));
        }
        transfer.0 += 1;
        transfer.3.push_str(&part.payload);
        if transfer.0 != transfer.1 { return Ok(None); }
        let (_, _, length, payload) = self.transfer.take().expect("active transfer");
        if payload.len() != length { return Err(EventDeliveryFailure(io::Error::other("incomplete event transfer"))); }
        decode(payload.as_bytes()).map(Some)
    }

    async fn receive(&mut self) -> Result<Option<BackendEvent>, EventDeliveryFailure> {
        loop {
            match self.receiver.recv().await.map_err(EventDeliveryFailure)? {
                Some(frame) => if let Some(event) = self.accept(frame.bytes())? { return Ok(Some(event)); },
                None if self.transfer.is_some() => return Err(EventDeliveryFailure(io::Error::other("event transfer ended early"))),
                None => return Ok(None),
            }
        }
    }

    fn try_receive(&mut self) -> Result<BackendEvent, EventDeliveryFailure> {
        loop {
            let frame = self.receiver.try_recv().map_err(EventDeliveryFailure)?;
            if let Some(event) = self.accept(frame.bytes())? { return Ok(event); }
        }
    }

    pub fn check(&self) -> Result<(), EventDeliveryFailure> {
        self.receiver.check().map_err(EventDeliveryFailure)
    }

    pub async fn recv(&mut self) -> Result<Option<BackendEvent>, EventDeliveryFailure> {
        if self.pending.is_none() {
            self.pending = match self.queued.take() {
                Some(event) => Some(event),
                None => self.receive().await?,
            };
            self.deadline = tokio::time::Instant::now() + Duration::from_millis(16);
        }
        while self.pending.as_ref().is_some_and(batchable) {
            if output_bytes(self.pending.as_ref().unwrap()) >= 64 * 1024 {
                break;
            }
            let received = tokio::time::timeout_at(self.deadline, self.receive()).await;
            let event = match received {
                Err(_) | Ok(Ok(None)) => break,
                Ok(Err(error)) => return Err(error),
                Ok(Ok(Some(event))) => event,
            };
            if !merge_output(self.pending.as_mut().unwrap(), &event) {
                self.queued = Some(event);
                break;
            }
        }
        Ok(self.pending.take())
    }

    pub fn try_recv(&mut self) -> Result<BackendEvent, EventDeliveryFailure> {
        self.check()?;
        let mut event = match self.pending.take().or_else(|| self.queued.take()) {
            Some(event) => event,
            None => self.try_receive()?,
        };
        while batchable(&event) && output_bytes(&event) < 64 * 1024 {
            let next = match self.try_receive() {
                Ok(event) => event,
                Err(EventDeliveryFailure(error)) if matches!(error.kind(), io::ErrorKind::WouldBlock | io::ErrorKind::UnexpectedEof) => break,
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

fn decode(bytes: &[u8]) -> Result<BackendEvent, EventDeliveryFailure> {
    serde_json::from_slice(bytes)
        .map_err(|error| EventDeliveryFailure(io::Error::other(error)))
}

fn batchable(event: &BackendEvent) -> bool {
    event.tool_output_delta().is_some() || event.message_delta().is_some()
}

fn output_bytes(event: &BackendEvent) -> usize {
    event.text.as_ref().or_else(||event.activity.as_ref().and_then(|activity|activity.output.as_ref()))
        .map_or(0,String::len)
}

fn merge_output(pending: &mut BackendEvent, event: &BackendEvent) -> bool {
    if !batchable(event) || pending.address != event.address || pending.kind != event.kind
        || output_bytes(pending) + output_bytes(event) > 64 * 1024 {
        return false;
    }
    if pending.text.is_some() {
        if pending.message_id() != event.message_id() || pending.message_phase() != event.message_phase() {
            return false;
        }
        pending.text.as_mut().unwrap().push_str(event.text.as_ref().expect("same message kind"));
        if let Some(delta) = pending.data.pointer_mut("/params/delta") {
            *delta = serde_json::Value::String(pending.text.as_ref().unwrap().clone());
        }
        return true;
    }
    let previous = pending.activity.as_mut().unwrap();
    let next = event.activity.as_ref().unwrap();
    if previous.id != next.id || previous.kind != next.kind || previous.title != next.title
        || previous.status != next.status {
        return false;
    }
    previous.output.as_mut().unwrap().push_str(next.output.as_ref().unwrap());
    if let Some(delta) = pending.data.pointer_mut("/params/delta") {
        *delta = serde_json::Value::String(previous.output.as_ref().unwrap().clone());
    }
    true
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

    #[tokio::test]
    async fn receipt_time_survives_forwarding_without_replacing_scalar_provider_data() {
        let (sink,mut stream) = channel();
        let mut source = event(42);
        source.data = serde_json::Value::Null;
        sink.send_wait(source).await.unwrap();
        let received = stream.recv().await.unwrap().unwrap();
        let timestamp = received.received_at_ms.expect("adapter receipt");
        assert_eq!(received.data,serde_json::Value::Null);
        assert_eq!(received.observed_at_ms(timestamp + 2000),timestamp);
        let (forward,mut forwarded) = channel();
        tokio::time::sleep(Duration::from_millis(20)).await;
        forward.send_wait(received).await.unwrap();
        let received = forwarded.recv().await.unwrap().unwrap();
        assert_eq!(received.received_at_ms,Some(timestamp));
        assert_eq!(received.data,serde_json::Value::Null);
    }

    fn event(sequence: usize) -> BackendEvent {
        BackendEvent {
            received_at_ms: None,
            address: None,
            turn_boundary: None,
            kind: "delta".into(),
            text: Some(sequence.to_string()),
            data: json!(sequence),
            activity: None,
            summary: None,
            task_update: None,
        }
    }

    fn output(id: &str, text: &str) -> BackendEvent {
        let mut event = event(0);
        event.text = None;
        event.kind = "tool-output".into();
        event.activity = Some(super::super::ToolActivity {
            id: id.into(),
            kind: super::super::ToolActivityKind::Command,
            title: "command".into(),
            output: Some(text.into()),
            status: Some("inProgress".into()),
            change: Default::default(),
            output_delta: true,
        });
        event
    }

    #[tokio::test]
    async fn message_batches_preserve_message_phase_and_snapshot_barriers() {
        let (sink, mut stream) = channel();
        let message = |id: &str, text: &str| BackendEvent {
            kind:"assistant_message".into(),text:Some(text.into()),
            data:json!({"provider_message_id":id,"provider_message_phase":"commentary"}),
            ..event(0)
        };
        let producer = async {
            for _ in 0..100 { sink.send_wait(message("one","λ")).await.unwrap(); }
            sink.send_wait(message("two","second")).await.unwrap();
            let mut snapshot = message("two","complete");
            snapshot.data["message_update"] = json!("snapshot");
            sink.send_wait(snapshot).await.unwrap();
            sink.send_wait(event(1)).await.unwrap();
        };
        let consumer = async {
            let first = stream.recv().await.unwrap().unwrap();
            assert_eq!(first.text.as_deref(),Some("λ".repeat(100).as_str()));
            assert_eq!(stream.recv().await.unwrap().unwrap().text.as_deref(),Some("second"));
            assert_eq!(stream.recv().await.unwrap().unwrap().text.as_deref(),Some("complete"));
            assert_eq!(stream.recv().await.unwrap().unwrap().data,json!(1));
        };
        tokio::join!(producer,consumer);
    }

    #[tokio::test]
    async fn output_burst_batches_without_crossing_lifecycle_or_owner_boundaries() {
        let (sink, mut stream) = channel();
        let producer = async move {
            for _ in 0..792 { sink.send_wait(output("first", "λ\n")).await.unwrap(); }
            sink.send_wait(event(1)).await.unwrap();
            sink.send_wait(output("second", "other")).await.unwrap();
        };
        let consumer = async move {
            let combined = stream.recv().await.unwrap().unwrap();
            assert_eq!(combined.activity.unwrap().output.unwrap(), "λ\n".repeat(792));
            assert_eq!(stream.recv().await.unwrap().unwrap().data, json!(1));
            assert_eq!(stream.recv().await.unwrap().unwrap().activity.unwrap().id, "second");
            assert!(stream.recv().await.unwrap().is_none());
        };
        tokio::join!(producer, consumer);
    }

    #[tokio::test]
    async fn cancelled_receive_retains_partial_batch_for_final_drain() {
        let (sink, mut stream) = channel();
        sink.send_wait(output("first", "partial")).await.unwrap();
        assert!(tokio::time::timeout(Duration::from_millis(1), stream.recv()).await.is_err());
        sink.send_wait(output("first", " tail")).await.unwrap();
        sink.send_wait(event(2)).await.unwrap();
        assert_eq!(stream.try_recv().unwrap().activity.unwrap().output.unwrap(), "partial tail");
        assert_eq!(stream.try_recv().unwrap().data, json!(2));
    }

    #[tokio::test]
    async fn output_batch_enforces_byte_limit_and_preserves_snapshot() {
        let (sink, mut stream) = channel();
        sink.send_wait(output("first", &"a".repeat(40000))).await.unwrap();
        sink.send_wait(output("first", &"b".repeat(40000))).await.unwrap();
        let mut completed = output("first", "authoritative");
        completed.activity.as_mut().unwrap().output_delta = false;
        sink.send_wait(completed).await.unwrap();
        assert_eq!(output_bytes(&stream.recv().await.unwrap().unwrap()), 40000);
        assert_eq!(output_bytes(&stream.recv().await.unwrap().unwrap()), 40000);
        assert_eq!(stream.recv().await.unwrap().unwrap().activity.unwrap().output.unwrap(), "authoritative");
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
    async fn oversized_payload_is_transferred_without_poisoning_delivery() {
        let (sink, mut stream) = channel();
        let mut oversized = event(0);
        oversized.text = Some("x".repeat(forge_protocol::MAX_FRAME_BYTES));
        let expected = oversized.text.clone();
        let producer = async { sink.send_wait(oversized).await.unwrap(); sink.send_wait(event(1)).await.unwrap(); };
        let consumer = async {
            assert_eq!(stream.recv().await.unwrap().unwrap().text, expected);
            assert_eq!(stream.recv().await.unwrap().unwrap().data, json!(1));
        };
        tokio::join!(producer, consumer);
    }

    #[tokio::test]
    async fn oversized_aggregate_reports_failure_even_when_the_producer_ignores_it() {
        let (sink, mut stream) = channel();
        let mut oversized = event(0);
        oversized.text = Some("x".repeat(forge_protocol::MAX_SNAPSHOT_BYTES));
        let _ = sink.send_wait(oversized).await;
        assert!(stream.recv().await.is_err());
        assert!(tokio::time::timeout(Duration::from_secs(1), sink.failed()).await.is_ok());
    }

    #[tokio::test]
    async fn cancelled_receive_preserves_an_incomplete_multipart_event() {
        let (sink, mut stream) = channel();
        let mut source = event(0);
        source.text = Some("λ".repeat(200000));
        let mut parts = forge_protocol::transfer::JsonTransfer::new(&source).unwrap();
        sink.sender.send_wait(serde_json::json!({"forge_event_part":parts.next().unwrap()})).await.unwrap();
        assert!(tokio::time::timeout(Duration::from_millis(1), stream.recv()).await.is_err());
        let producer = async {
            for part in parts { sink.sender.send_wait(serde_json::json!({"forge_event_part":part})).await.unwrap(); }
        };
        let consumer = async { assert_eq!(stream.recv().await.unwrap().unwrap().text, source.text); };
        tokio::join!(producer, consumer);
    }

    #[tokio::test]
    async fn burst_waits_for_capacity_and_preserves_every_event() {
        let (sink, mut stream) = channel();
        let producer = async move {
            for sequence in 0..384 {
                sink.send_wait(event(sequence)).await.unwrap();
            }
        };
        let consumer = async move {
            for sequence in 0..384 {
                assert_eq!(stream.recv().await.unwrap().unwrap().data, json!(sequence));
            }
            assert!(stream.recv().await.unwrap().is_none());
        };
        tokio::time::timeout(std::time::Duration::from_secs(5), async {
            tokio::join!(producer, consumer);
        })
        .await
        .unwrap();
    }

    #[tokio::test]
    async fn closing_consumer_releases_a_waiting_producer() {
        let (sink, stream) = channel();
        for sequence in 0..96 {
            sink.send_wait(event(sequence)).await.unwrap();
        }
        let waiting = sink.send_wait(event(96));
        tokio::pin!(waiting);
        assert!(
            std::future::poll_fn(|context| {
                std::task::Poll::Ready(std::future::Future::poll(waiting.as_mut(), context))
            })
            .await
            .is_pending()
        );
        drop(stream);
        assert!(
            tokio::time::timeout(std::time::Duration::from_secs(1), waiting)
                .await
                .unwrap()
                .is_err()
        );
    }
}
