use std::{fmt, io, time::Duration};

use forge_protocol::outbound::{self, MessageReceiver, MessageSender};

use super::{
    BindingId, BoundProviderEvent, EventScope, InvocationScope, MessageUpdate, ProviderError,
    ProviderEvent, TextUpdate, ToolUpdate,
};

#[derive(Clone)]
pub struct ProviderEventSink {
    binding_id: BindingId,
    scope: EventScope,
    sender: MessageSender,
    failure: std::sync::Arc<std::sync::Mutex<Option<EventDeliveryError>>>,
    delivery: std::sync::Arc<tokio::sync::Mutex<()>>,
    published: std::sync::Arc<std::sync::atomic::AtomicU64>,
    observer: Vec<std::sync::Arc<dyn EventObserver>>,
}

pub struct ProviderEventStream {
    binding_id: BindingId,
    input: EventReceiver,
    pending: Option<BoundProviderEvent>,
    queued: Option<BoundProviderEvent>,
    deadline: tokio::time::Instant,
    deferred_error: Option<EventDeliveryError>,
}

struct EventReceiver {
    receiver: MessageReceiver,
    failure: std::sync::Arc<std::sync::Mutex<Option<EventDeliveryError>>>,
    transfer: Option<(usize, usize, usize, String)>,
    binding_id: BindingId,
    received: crate::EventSequence,
}

#[derive(Clone, Debug)]
pub enum EventDeliveryError {
    /// Preserve a native observation failure across bounded host delivery.
    Observation(crate::lifecycle::ObservationFailure),
    Closed,
    OwnershipMismatch {
        expected: Box<EventScope>,
        received: Box<EventScope>,
    },
    InvalidTransportData,
    IncompleteTransfer {
        received_parts: usize,
        expected_parts: usize,
        received_bytes: usize,
        expected_bytes: usize,
    },
    Empty,
}

impl EventDeliveryError {
    pub fn transport(_error: impl fmt::Display) -> Self {
        Self::InvalidTransportData
    }

    fn queue(error: io::Error) -> Self {
        match error.kind() {
            io::ErrorKind::WouldBlock => Self::Empty,
            io::ErrorKind::UnexpectedEof | io::ErrorKind::BrokenPipe => Self::Closed,
            _ => Self::transport(error),
        }
    }
}

impl fmt::Display for EventDeliveryError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Observation(failure) => {
                write!(formatter, "provider observation failed: {failure}")
            }
            Self::Closed => formatter.write_str("provider event delivery closed"),
            Self::OwnershipMismatch { expected, received } => write!(
                formatter,
                "provider event crosses execution ownership: expected {expected:?}, received {received:?}"
            ),
            Self::InvalidTransportData => {
                formatter.write_str("invalid provider event transport data")
            }
            Self::IncompleteTransfer {
                received_parts,
                expected_parts,
                received_bytes,
                expected_bytes,
            } => write!(
                formatter,
                "incomplete provider event transfer: {received_parts}/{expected_parts} parts and {received_bytes}/{expected_bytes} bytes"
            ),
            Self::Empty => formatter.write_str("provider event queue is empty"),
        }
    }
}

impl std::error::Error for EventDeliveryError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::Observation(failure) => Some(failure),
            Self::Closed
            | Self::OwnershipMismatch { .. }
            | Self::InvalidTransportData
            | Self::IncompleteTransfer { .. }
            | Self::Empty => None,
        }
    }
}

pub fn channel(
    binding_id: BindingId,
    scope: impl Into<EventScope>,
) -> (ProviderEventSink, ProviderEventStream) {
    let (sender, receiver) = outbound::channel();
    let failure = std::sync::Arc::new(std::sync::Mutex::new(None));
    (
        ProviderEventSink {
            binding_id: binding_id.clone(),
            scope: scope.into(),
            sender,
            failure: failure.clone(),
            delivery: Default::default(),
            published: Default::default(),
            observer: Vec::new(),
        },
        ProviderEventStream {
            binding_id: binding_id.clone(),
            input: EventReceiver {
                receiver,
                failure,
                transfer: None,
                binding_id,
                received: crate::EventSequence::default(),
            },
            pending: None,
            queued: None,
            deadline: tokio::time::Instant::now(),
            deferred_error: None,
        },
    )
}

impl ProviderEventSink {
    /// Get the binding generation that owns every event sent through this sink.
    pub fn binding_id(&self) -> &BindingId {
        &self.binding_id
    }

    /// Get the final successfully published sequence, independently of owner consumption.
    pub fn watermark(&self) -> crate::EventSequence {
        crate::EventSequence::new(self.published.load(std::sync::atomic::Ordering::Acquire))
    }

    /// Create the next envelope while the shared delivery lock serializes publishers.
    fn envelope(&self, event: ProviderEvent) -> Result<BoundProviderEvent, EventDeliveryError> {
        let sequence = self
            .watermark()
            .successor()
            .ok_or(EventDeliveryError::Observation(
                crate::ObservationFailure::Protocol,
            ))?;
        Ok(BoundProviderEvent {
            binding_id: self.binding_id.clone(),
            sequence,
            event,
        })
    }

    /// Attach a host observer while retaining the same ordered delivery channel.
    pub fn with_observer(mut self, observer: std::sync::Arc<dyn EventObserver>) -> Self {
        self.observer.push(observer);
        self
    }

    pub fn scope(&self) -> &EventScope {
        &self.scope
    }

    pub fn invocation_scope(&self) -> Result<&InvocationScope, ProviderError> {
        self.scope.invocation().ok_or_else(|| {
            ProviderError::Failure(crate::error::ProviderFailure::new(
                crate::error::ProviderOperation::InputSubmit,
                crate::error::ProviderFailureKind::Rejected,
            ))
        })
    }

    /// Bind a session observer to one original invocation without widening prompt delivery.
    pub fn invocation_sink(
        &self,
        invocation: &InvocationScope,
    ) -> Result<Self, EventDeliveryError> {
        self.bind_scope(EventScope::from(invocation.clone()))
    }

    /// Route host output for a successor invocation within the same owner and exchange.
    /// This creates no dispatch or callback authority. Native invocation sinks remain exact;
    /// the host uses this only after it owns admission of a fresh ordinary retry input.
    pub fn successor_invocation_sink(
        &self,
        invocation: &InvocationScope,
    ) -> Result<Self, EventDeliveryError> {
        let original =
            self.scope
                .invocation()
                .ok_or_else(|| EventDeliveryError::OwnershipMismatch {
                    expected: Box::new(self.scope.clone()),
                    received: Box::new(invocation.clone().into()),
                })?;
        if original.owner != invocation.owner
            || original.exchange_id != invocation.exchange_id
            || original.invocation_id == invocation.invocation_id
        {
            return Err(EventDeliveryError::OwnershipMismatch {
                expected: Box::new(self.scope.clone()),
                received: Box::new(invocation.clone().into()),
            });
        }
        Ok(Self {
            scope: invocation.clone().into(),
            ..self.clone()
        })
    }

    /// Bind an owner-wide session publisher to an original event scope.
    pub fn bind_scope(&self, received: EventScope) -> Result<Self, EventDeliveryError> {
        if !matches!(self.scope, EventScope::Binding { .. })
            || self.scope.owner() != received.owner()
        {
            return Err(EventDeliveryError::OwnershipMismatch {
                expected: Box::new(self.scope.clone()),
                received: Box::new(received),
            });
        }
        Ok(Self {
            scope: received,
            ..self.clone()
        })
    }

    fn validate(&self, event: &ProviderEvent) -> Result<(), EventDeliveryError> {
        if let Some(error) = self
            .failure
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .clone()
        {
            return Err(error);
        }
        if let ProviderEvent::Settled { settlement } = event
            && settlement.binding_id != self.binding_id
        {
            let error =
                EventDeliveryError::Observation(crate::ObservationFailure::OwnershipConflict);
            self.fail(error.clone());
            return Err(error);
        }
        let received = event.scope();
        if received != self.scope {
            let error = EventDeliveryError::OwnershipMismatch {
                expected: Box::new(self.scope.clone()),
                received: Box::new(received),
            };
            self.fail(error.clone());
            return Err(error);
        }
        Ok(())
    }

    pub async fn send_wait(&self, event: ProviderEvent) -> Result<(), EventDeliveryError> {
        self.validate(&event)?;
        for observer in &self.observer {
            observer
                .before_delivery(&event)
                .inspect_err(|error| self.fail(error.clone()))?;
        }
        let _delivery = self.delivery.lock().await;
        let mut additional = Vec::new();
        for observer in &self.observer {
            additional.extend(observer.additional_events(&event).inspect_err(|error| {
                self.fail(error.clone());
            })?);
        }
        self.send_encoded(event).await?;
        for event in additional {
            self.send_encoded(event).await?;
        }
        Ok(())
    }

    /// Publish successful collection with its exact stream position under the shared delivery lock.
    /// The returned receipt and event carry the same watermark, even with concurrent publishers.
    pub async fn send_settlement(
        &self,
        mut settlement: crate::InvocationSettlement,
    ) -> Result<crate::InvocationSettlement, EventDeliveryError> {
        let _delivery = self.delivery.lock().await;
        settlement.watermark =
            self.watermark()
                .successor()
                .ok_or(EventDeliveryError::Observation(
                    crate::ObservationFailure::Protocol,
                ))?;
        let event = ProviderEvent::Settled {
            settlement: Box::new(settlement.clone()),
        };
        self.validate(&event)?;
        let mut additional = Vec::new();
        for observer in &self.observer {
            observer
                .before_delivery(&event)
                .inspect_err(|error| self.fail(error.clone()))?;
            additional.extend(
                observer
                    .additional_events(&event)
                    .inspect_err(|error| self.fail(error.clone()))?,
            );
        }
        self.send_encoded(event).await?;
        for event in additional {
            self.send_encoded(event).await?;
        }
        Ok(settlement)
    }

    /// Encode and publish an event under the shared delivery lock before advancing its watermark.
    async fn send_encoded(&self, event: ProviderEvent) -> Result<(), EventDeliveryError> {
        let event = self.envelope(event)?;
        let transfer = forge_protocol::transfer::JsonTransfer::new(&event).map_err(|error| {
            self.sender.record_failure(&error);
            EventDeliveryError::transport(error)
        })?;
        if transfer.total_bytes() < forge_protocol::MAX_FRAME_BYTES / 2 {
            self.sender
                .send_wait(&event)
                .await
                .map_err(EventDeliveryError::queue)?;
        } else {
            for part in transfer {
                self.sender
                    .send_wait(serde_json::json!({"forge_event_part": part}))
                    .await
                    .map_err(EventDeliveryError::queue)?;
            }
        }
        self.published
            .store(event.sequence.get(), std::sync::atomic::Ordering::Release);
        Ok(())
    }

    /// Publish a bounded envelope synchronously while preserving the same sequence protocol.
    fn send_immediate(&self, event: ProviderEvent) -> Result<(), EventDeliveryError> {
        let event = self.envelope(event)?;
        self.sender
            .send(&event)
            .map_err(EventDeliveryError::queue)?;
        self.published
            .store(event.sequence.get(), std::sync::atomic::Ordering::Release);
        Ok(())
    }

    /// Deliver a native event and its host observations without waiting for channel capacity.
    pub fn send(&self, event: ProviderEvent) -> Result<(), EventDeliveryError> {
        self.validate(&event)?;
        let _delivery = self.delivery.try_lock().map_err(|_| {
            let error =
                EventDeliveryError::Observation(crate::lifecycle::ObservationFailure::Overflow);
            self.fail(error.clone());
            error
        })?;
        for observer in &self.observer {
            observer
                .before_delivery(&event)
                .inspect_err(|error| self.fail(error.clone()))?;
        }
        let mut additional = Vec::new();
        for observer in &self.observer {
            additional.extend(observer.additional_events(&event).inspect_err(|error| {
                self.fail(error.clone());
            })?);
        }
        self.send_immediate(event)?;
        for event in additional {
            self.send_immediate(event)?;
        }
        Ok(())
    }

    /// Record irreversible observation loss independently of ordinary queue capacity.
    pub fn fail(&self, error: EventDeliveryError) {
        self.failure
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .get_or_insert(error.clone());
        self.sender
            .record_failure(&io::Error::other(error.to_string()));
    }

    pub async fn failed(&self) -> EventDeliveryError {
        let error = EventDeliveryError::queue(self.sender.failed().await);
        self.failure
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .clone()
            .unwrap_or(error)
    }
}

impl EventReceiver {
    fn accept(&mut self, bytes: &[u8]) -> Result<Option<BoundProviderEvent>, EventDeliveryError> {
        let value: serde_json::Value =
            serde_json::from_slice(bytes).map_err(EventDeliveryError::transport)?;
        let Some(part) = value.get("forge_event_part") else {
            if let Some((sequence, count, bytes, payload)) = &self.transfer {
                return Err(EventDeliveryError::IncompleteTransfer {
                    received_parts: *sequence,
                    expected_parts: *count,
                    received_bytes: payload.len(),
                    expected_bytes: *bytes,
                });
            }
            let event = serde_json::from_value(value).map_err(EventDeliveryError::transport)?;
            return self.accept_envelope(event).map(Some);
        };
        let part: forge_protocol::transfer::JsonPart =
            serde_json::from_value(part.clone()).map_err(EventDeliveryError::transport)?;
        if part.part_count == 0
            || part.part_count > part.total_bytes
            || part.payload.is_empty()
            || part.payload.len() > forge_protocol::MAX_SNAPSHOT_PART_BYTES
        {
            return Err(EventDeliveryError::transport("invalid part"));
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
            return Err(EventDeliveryError::transport("inconsistent transfer part"));
        }
        transfer.0 += 1;
        transfer.3.push_str(&part.payload);
        if transfer.0 != transfer.1 {
            return Ok(None);
        }
        let length = transfer.2;
        let received_parts = transfer.0;
        let expected_parts = transfer.1;
        let payload = std::mem::take(&mut transfer.3);
        self.transfer = None;
        if payload.len() != length {
            return Err(EventDeliveryError::IncompleteTransfer {
                received_parts,
                expected_parts,
                received_bytes: payload.len(),
                expected_bytes: length,
            });
        }
        let event = serde_json::from_str(&payload).map_err(EventDeliveryError::transport)?;
        self.accept_envelope(event).map(Some)
    }

    /// Reject foreign bindings and gaps before exposing an event to the owner.
    fn accept_envelope(
        &mut self,
        event: BoundProviderEvent,
    ) -> Result<BoundProviderEvent, EventDeliveryError> {
        if event.binding_id != self.binding_id || self.received.successor() != Some(event.sequence)
        {
            return Err(EventDeliveryError::Observation(
                crate::ObservationFailure::Protocol,
            ));
        }
        self.received = event.sequence;
        Ok(event)
    }

    fn closed(&self) -> Result<Option<BoundProviderEvent>, EventDeliveryError> {
        if let Some((sequence, count, bytes, payload)) = &self.transfer {
            return Err(EventDeliveryError::IncompleteTransfer {
                received_parts: *sequence,
                expected_parts: *count,
                received_bytes: payload.len(),
                expected_bytes: *bytes,
            });
        }
        Ok(None)
    }

    /// Restore the typed producer cause after the byte transport reports failure.
    fn failure(&self, error: EventDeliveryError) -> EventDeliveryError {
        self.failure
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .clone()
            .unwrap_or(error)
    }

    async fn receive(&mut self) -> Result<Option<BoundProviderEvent>, EventDeliveryError> {
        loop {
            let frame = match self.receiver.try_drain() {
                Ok(frame) => Some(frame),
                Err(_) => match self.receiver.recv().await {
                    Ok(frame) => frame,
                    Err(error) => match self.receiver.try_drain() {
                        Ok(frame) => Some(frame),
                        Err(_) => return Err(self.failure(EventDeliveryError::queue(error))),
                    },
                },
            };
            match frame {
                Some(frame) => {
                    if let Some(event) = self.accept(frame.bytes())? {
                        return Ok(Some(event));
                    }
                }
                None => return self.closed(),
            }
        }
    }

    fn try_receive(&mut self) -> Result<BoundProviderEvent, EventDeliveryError> {
        loop {
            let frame = match self.receiver.try_drain() {
                Ok(frame) => frame,
                Err(error) => {
                    if let Err(failure) = self.receiver.check() {
                        return Err(self.failure(EventDeliveryError::queue(failure)));
                    }
                    if error.kind() == io::ErrorKind::UnexpectedEof {
                        self.closed()?;
                    }
                    return Err(self.failure(EventDeliveryError::queue(error)));
                }
            };
            if let Some(event) = self.accept(frame.bytes())? {
                return Ok(event);
            }
        }
    }
}

impl ProviderEventStream {
    /// Get the exact live generation whose events this stream consumes.
    pub fn binding_id(&self) -> &BindingId {
        &self.binding_id
    }

    pub fn try_next(&mut self) -> Result<Option<BoundProviderEvent>, EventDeliveryError> {
        match self.try_recv() {
            Ok(event) => Ok(Some(event)),
            Err(EventDeliveryError::Empty | EventDeliveryError::Closed) => Ok(None),
            Err(error) => Err(error),
        }
    }

    pub fn check(&self) -> Result<(), EventDeliveryError> {
        self.input
            .receiver
            .check()
            .map_err(|error| self.input.failure(EventDeliveryError::queue(error)))
    }

    pub async fn recv(&mut self) -> Result<Option<BoundProviderEvent>, EventDeliveryError> {
        if self.pending.is_none()
            && self.queued.is_none()
            && let Some(error) = self.deferred_error.take()
        {
            return Err(error);
        }
        if self.pending.is_none() {
            self.pending = match self.queued.take() {
                Some(event) => Some(event),
                None => self.input.receive().await?,
            };
            self.deadline = tokio::time::Instant::now() + Duration::from_millis(16);
        }
        while let Some(pending) = self.pending.as_mut() {
            if !batchable(&pending.event) || output_bytes(&pending.event) >= 64 * 1024 {
                break;
            }
            let event = match tokio::time::timeout_at(self.deadline, self.input.receive()).await {
                Err(_) | Ok(Ok(None)) => break,
                Ok(Err(error)) => {
                    self.deferred_error = Some(error);
                    break;
                }
                Ok(Ok(Some(event))) => event,
            };
            if !merge_output(&mut pending.event, &event.event) {
                self.queued = Some(event);
                break;
            }
            pending.sequence = event.sequence;
        }
        Ok(self.pending.take())
    }

    pub fn try_recv(&mut self) -> Result<BoundProviderEvent, EventDeliveryError> {
        if self.pending.is_none()
            && self.queued.is_none()
            && let Some(error) = self.deferred_error.take()
        {
            return Err(error);
        }
        let mut event = match self.pending.take().or_else(|| self.queued.take()) {
            Some(event) => event,
            None => self.input.try_receive()?,
        };
        while batchable(&event.event) && output_bytes(&event.event) < 64 * 1024 {
            let next = match self.input.try_receive() {
                Ok(event) => event,
                Err(EventDeliveryError::Empty | EventDeliveryError::Closed) => break,
                Err(error) => {
                    self.deferred_error = Some(error);
                    break;
                }
            };
            if !merge_output(&mut event.event, &next.event) {
                self.queued = Some(next);
                break;
            }
            event.sequence = next.sequence;
        }
        Ok(event)
    }
}

fn batchable(event: &ProviderEvent) -> bool {
    event.message_delta().is_some() || event.tool_output_delta().is_some()
}

fn output_bytes(event: &ProviderEvent) -> usize {
    event
        .message_delta()
        .or_else(|| event.tool_output_delta())
        .map_or(0, str::len)
}

fn merge_output(pending: &mut ProviderEvent, event: &ProviderEvent) -> bool {
    if output_bytes(pending).saturating_add(output_bytes(event)) > 64 * 1024 {
        return false;
    }
    match (pending, event) {
        (
            ProviderEvent::Message {
                address: previous_address,
                update: MessageUpdate::Delta { text: previous },
            },
            ProviderEvent::Message {
                address,
                update: MessageUpdate::Delta { text },
            },
        ) if previous_address == address => {
            previous.push_str(text);
            true
        }
        (
            ProviderEvent::Tool {
                address: previous_address,
                update:
                    ToolUpdate::Output {
                        update: TextUpdate::Append(previous),
                    },
            },
            ProviderEvent::Tool {
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

pub async fn failed(sink: Option<&ProviderEventSink>) -> EventDeliveryError {
    match sink {
        Some(sink) => sink.failed().await,
        None => std::future::pending().await,
    }
}

/// Observe host policy without coupling provider delivery to host state owners.
pub trait EventObserver: Send + Sync {
    /// Observe execution state before waiting for ordered delivery.
    fn before_delivery(&self, _event: &ProviderEvent) -> Result<(), EventDeliveryError> {
        Ok(())
    }

    /// Produce host observations immediately after the event on the same delivery channel.
    fn additional_events(
        &self,
        _event: &ProviderEvent,
    ) -> Result<Vec<ProviderEvent>, EventDeliveryError> {
        Ok(Vec::new())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{BindingId, ExecutionOwner, MessageAddress, MessageId, TurnAddress};
    use serde_json::json;

    struct Observer {
        fail: bool,
    }

    impl EventObserver for Observer {
        fn additional_events(
            &self,
            event: &ProviderEvent,
        ) -> Result<Vec<ProviderEvent>, EventDeliveryError> {
            if self.fail {
                return Err(EventDeliveryError::transport("observer failed"));
            }
            Ok(vec![ProviderEvent::Runtime {
                scope: event.scope().invocation().unwrap().clone(),
                update: crate::RuntimeUpdate::Progress { received_bytes: 42 },
            }])
        }
    }

    /// A sticky native failure follows already accepted output and retains its classification.
    #[tokio::test]
    async fn queued_output_precedes_typed_observation_failure() -> anyhow::Result<()> {
        let (sink, mut stream) = channel(binding(), scope());
        sink.send_wait(message(
            "answer",
            MessageUpdate::Delta {
                text: "accepted".into(),
            },
        ))
        .await?;
        sink.fail(EventDeliveryError::Observation(
            crate::lifecycle::ObservationFailure::Overflow,
        ));
        sink.fail(EventDeliveryError::Observation(
            crate::lifecycle::ObservationFailure::Disconnected,
        ));
        assert!(
            matches!(stream.recv().await?, Some(super::BoundProviderEvent { event: ProviderEvent::Message {
            update: MessageUpdate::Delta { text }, ..
        }, .. }) if text == "accepted")
        );
        assert!(matches!(
            stream.recv().await,
            Err(EventDeliveryError::Observation(
                crate::lifecycle::ObservationFailure::Overflow
            ))
        ));
        assert!(matches!(
            sink.failed().await,
            EventDeliveryError::Observation(crate::lifecycle::ObservationFailure::Overflow)
        ));
        Ok(())
    }

    /// Preserve emitted watermark evidence when output batching combines many stream positions.
    #[tokio::test]
    async fn batched_events_preserve_published_watermark_and_lifecycle_barriers()
    -> anyhow::Result<()> {
        let (sink, mut stream) = channel(binding(), scope());
        assert_eq!(sink.watermark(), crate::EventSequence::new(0));
        sink.send_wait(message("one", MessageUpdate::Delta { text: "a".into() }))
            .await?;
        let sibling = sink.clone();
        sibling
            .send_wait(message("one", MessageUpdate::Delta { text: "b".into() }))
            .await?;
        sink.send_wait(ProviderEvent::Runtime {
            scope: scope(),
            update: crate::RuntimeUpdate::ForegroundIdle,
        })
        .await?;
        assert_eq!(sink.watermark(), crate::EventSequence::new(3));
        let output = stream.recv().await?.unwrap();
        assert_eq!(output.sequence, crate::EventSequence::new(2));
        assert_eq!(output.event.message_delta(), Some("ab"));
        let barrier = stream.recv().await?.unwrap();
        assert_eq!(barrier.sequence, sink.watermark());
        assert!(matches!(
            barrier.event,
            ProviderEvent::Runtime {
                update: crate::RuntimeUpdate::ForegroundIdle,
                ..
            }
        ));
        Ok(())
    }

    /// Correlate the settlement result with its actual envelope under concurrent publication.
    #[tokio::test]
    async fn settlement_receipt_matches_envelope_under_concurrent_publication() -> anyhow::Result<()>
    {
        let (sink, mut stream) = channel(binding(), scope());
        let publishing = async {
            for received_bytes in 0..8 {
                sink.send_wait(ProviderEvent::Runtime {
                    scope: scope(),
                    update: crate::RuntimeUpdate::Progress { received_bytes },
                })
                .await?;
            }
            Ok::<(), EventDeliveryError>(())
        };
        let settlement = crate::InvocationSettlement {
            binding_id: binding(),
            scope: scope(),
            result: crate::ExecutionResult::Completed {
                completion: crate::completion::ProviderCompletion::default().into(),
            },
            watermark: crate::EventSequence::default(),
        };
        let (published, receipt) = tokio::join!(publishing, sink.send_settlement(settlement));
        published?;
        let receipt = receipt?;
        let mut observed = None;
        for _ in 0..9 {
            let envelope = stream
                .recv()
                .await?
                .expect("all nine envelopes were published");
            if let ProviderEvent::Settled { settlement } = envelope.event {
                assert_eq!(settlement.watermark, envelope.sequence);
                observed = Some(settlement.watermark);
            }
        }
        assert_eq!(observed, Some(receipt.watermark));
        Ok(())
    }

    /// Rebind host output only within one exchange while keeping native invocation sinks immutable.
    #[test]
    fn successor_output_scope_rejects_same_invocation_and_other_exchanges() {
        let original = scope();
        let (sink, _stream) = channel(binding(), original.clone());
        assert!(sink.successor_invocation_sink(&original).is_err());
        let mut successor = original.clone();
        successor.invocation_id = "successor".into();
        assert!(sink.successor_invocation_sink(&successor).is_ok());
        assert!(sink.invocation_sink(&successor).is_err());
        successor.exchange_id = "unrelated".into();
        assert!(sink.successor_invocation_sink(&successor).is_err());
    }

    /// Reject a missing normalized event instead of satisfying a lifecycle watermark falsely.
    #[tokio::test]
    async fn sequence_gap_is_observation_failure_before_owner_consumption() {
        let (sink, mut stream) = channel(binding(), scope());
        sink.sender
            .send(BoundProviderEvent {
                binding_id: binding(),
                sequence: crate::EventSequence::new(2),
                event: message(
                    "missing-first",
                    MessageUpdate::Delta {
                        text: "unsafe".into(),
                    },
                ),
            })
            .unwrap();
        assert!(matches!(
            stream.recv().await,
            Err(EventDeliveryError::Observation(
                crate::ObservationFailure::Protocol
            ))
        ));
    }

    /// Preserve host observations immediately after their native event on the same channel.
    #[tokio::test]
    async fn host_observer_preserves_native_event_order() -> anyhow::Result<()> {
        for synchronous in [false, true] {
            let (sink, mut stream) = channel(binding(), scope());
            let sink = sink.with_observer(std::sync::Arc::new(Observer { fail: false }));
            let event = message(
                "observed",
                MessageUpdate::Delta {
                    text: "text".into(),
                },
            );
            if synchronous {
                sink.send(event)?;
            } else {
                sink.send_wait(event).await?;
            }
            assert!(matches!(
                stream.try_next()?,
                Some(super::BoundProviderEvent {
                    event: ProviderEvent::Message { .. },
                    ..
                })
            ));
            assert!(matches!(
                stream.try_next()?,
                Some(super::BoundProviderEvent {
                    event: ProviderEvent::Runtime {
                        update: crate::RuntimeUpdate::Progress { received_bytes: 42 },
                        ..
                    },
                    ..
                })
            ));
        }
        Ok(())
    }

    /// Fail the delivery channel when a host observer cannot produce required observations.
    #[tokio::test]
    async fn host_observer_failure_reaches_event_consumers() {
        for synchronous in [false, true] {
            let (sink, stream) = channel(binding(), scope());
            let sink = sink.with_observer(std::sync::Arc::new(Observer { fail: true }));
            let event = message(
                "observed",
                MessageUpdate::Delta {
                    text: "text".into(),
                },
            );
            let result = if synchronous {
                sink.send(event)
            } else {
                sink.send_wait(event).await
            };
            assert!(result.is_err());
            assert!(stream.check().is_err());
        }
    }

    /// Preserve the accepted prefix and wake consumers when synchronous delivery cannot remain ordered.
    #[tokio::test]
    async fn synchronous_send_contention_reports_typed_observation_loss() -> anyhow::Result<()> {
        let (sink, mut stream) = channel(binding(), scope());
        sink.send(message(
            "accepted",
            MessageUpdate::Delta {
                text: "prefix".into(),
            },
        ))?;
        let delivery = sink.delivery.lock().await;
        assert!(matches!(
            sink.send(message(
                "lost",
                MessageUpdate::Delta {
                    text: "lost".into()
                }
            )),
            Err(EventDeliveryError::Observation(
                crate::lifecycle::ObservationFailure::Overflow
            ))
        ));
        drop(delivery);
        assert!(
            matches!(stream.recv().await?, Some(super::BoundProviderEvent { event: ProviderEvent::Message {
            update: MessageUpdate::Delta { text }, ..
        }, .. }) if text == "prefix")
        );
        assert!(matches!(
            stream.recv().await,
            Err(EventDeliveryError::Observation(
                crate::lifecycle::ObservationFailure::Overflow
            ))
        ));
        assert!(matches!(
            sink.failed().await,
            EventDeliveryError::Observation(crate::lifecycle::ObservationFailure::Overflow)
        ));
        Ok(())
    }

    fn scope() -> InvocationScope {
        InvocationScope {
            owner: ExecutionOwner::root("session"),
            exchange_id: "exchange".into(),
            invocation_id: "invocation".into(),
        }
    }

    fn binding() -> BindingId {
        BindingId::new("binding").unwrap()
    }

    fn message(id: &str, update: MessageUpdate) -> ProviderEvent {
        ProviderEvent::Message {
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

    /// Keep retained delivery inside its session and preserve exact invocation validation.
    #[tokio::test]
    async fn session_observer_rebinds_only_its_owned_invocations() -> anyhow::Result<()> {
        let original = scope();
        let (session_sink, mut stream) = channel(
            binding(),
            EventScope::Binding {
                owner: original.owner.clone(),
            },
        );
        let retained_sink = session_sink.invocation_sink(&original)?;
        let update = message(
            "retained",
            MessageUpdate::Delta {
                text: "original invocation".into(),
            },
        );
        retained_sink.send_wait(update).await?;
        let received = stream.recv().await?.expect("retained event was sent");
        assert_eq!(received.event.scope(), EventScope::from(original.clone()));

        let mut later = original.clone();
        later.invocation_id = "later-invocation".into();
        later.exchange_id = "later-exchange".into();
        assert!(retained_sink.invocation_sink(&later).is_err());
        let later_sink = session_sink.invocation_sink(&later)?;
        assert!(later_sink.validate(&received.event).is_err());

        let mut sibling = original.clone();
        sibling.owner.agent_id = "another-agent".into();
        assert!(session_sink.invocation_sink(&sibling).is_err());
        let mut foreign = original;
        foreign.owner = ExecutionOwner::root("foreign-session");
        assert!(session_sink.invocation_sink(&foreign).is_err());
        Ok(())
    }

    #[tokio::test]
    async fn malformed_transfer_reports_normalized_transport_failure() {
        let (sink, mut stream) = channel(binding(), scope());
        sink.sender
            .send(json!({"forge_event_part": {
                "sequence": 1, "part_count": 2, "total_bytes": 4, "payload": "ab"
            }}))
            .unwrap();
        assert!(matches!(
            stream.recv().await,
            Err(EventDeliveryError::InvalidTransportData)
        ));
    }

    #[tokio::test]
    async fn closed_transfer_reports_progress_instead_of_generic_eof() {
        let (sink, mut stream) = channel(binding(), scope());
        sink.sender
            .send(json!({"forge_event_part": {
                "sequence": 0, "part_count": 2, "total_bytes": 4, "payload": "ab"
            }}))
            .unwrap();
        drop(sink);
        assert!(matches!(
            stream.recv().await,
            Err(EventDeliveryError::IncompleteTransfer {
                received_parts: 1,
                expected_parts: 2,
                received_bytes: 2,
                expected_bytes: 4,
            })
        ));
    }

    #[tokio::test]
    async fn foreign_invocation_scope_is_rejected_before_delivery() {
        let (sink, mut stream) = channel(binding(), scope());
        let mut foreign = scope();
        foreign.invocation_id = "foreign".into();
        let error = sink
            .send_wait(ProviderEvent::Runtime {
                scope: foreign,
                update: crate::RuntimeUpdate::ForegroundIdle,
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
        let (sink, mut stream) = channel(binding(), scope());
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
                stream.recv().await.unwrap().unwrap().event.message_delta(),
                Some("λ".repeat(100).as_str())
            );
            assert!(matches!(
                stream.recv().await.unwrap(),
                Some(super::BoundProviderEvent {
                    event: ProviderEvent::Message {
                        update: MessageUpdate::Completed { .. },
                        ..
                    },
                    ..
                })
            ));
            assert_eq!(
                stream.recv().await.unwrap().unwrap().event.message_delta(),
                Some("second")
            );
        };
        tokio::join!(producer, consumer);
    }

    #[tokio::test]
    async fn typed_events_preserve_barriers_and_full_content_under_backpressure() {
        let (sink, mut stream) = channel(binding(), scope());
        let text = "λ\n".repeat(forge_protocol::MAX_FRAME_BYTES);
        let expected = text.clone();
        let producer = async move {
            sink.send_wait(message("large", MessageUpdate::Completed { text }))
                .await
                .unwrap();
        };
        let consumer = async {
            match stream.recv().await.unwrap().unwrap() {
                super::BoundProviderEvent {
                    event:
                        ProviderEvent::Message {
                            update: MessageUpdate::Completed { text },
                            ..
                        },
                    ..
                } => assert_eq!(text, expected),
                event => panic!("unexpected event {event:?}"),
            }
        };
        tokio::join!(producer, consumer);
    }

    #[tokio::test]
    async fn cancelled_receive_retains_partial_batch_for_final_drain() {
        let (sink, mut stream) = channel(binding(), scope());
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
            stream.try_recv().unwrap().event.message_delta(),
            Some("partial tail")
        );
    }
}
