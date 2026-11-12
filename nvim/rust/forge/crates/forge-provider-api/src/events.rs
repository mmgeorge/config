use std::{fmt, io, time::Duration};

use forge_protocol::outbound::{self, MessageReceiver, MessageSender};

use super::{
    BindingId, BoundProviderEvent, EventScope, InvocationScope, ProviderError, ProviderEvent,
    TextUpdate,
};

/// Own connection delivery for one provider binding.
///
/// Terminal turn publication goes through a [`TurnPublisher`] claimed from this sink. The sink
/// grants at most one publisher per turn, so the retained runner in `forge-providers` that claims
/// it becomes the turn's only lifecycle authority and a second claim fails at runtime.
#[derive(Clone)]
pub struct ProviderEventSink {
    binding_id: BindingId,
    scope: EventScope,
    sender: MessageSender,
    failure: std::sync::Arc<std::sync::Mutex<Option<EventDeliveryError>>>,
    delivery: std::sync::Arc<tokio::sync::Mutex<()>>,
    published: std::sync::Arc<std::sync::atomic::AtomicU64>,
    observer: Vec<std::sync::Arc<dyn EventObserver>>,
    ownership: std::sync::Arc<std::sync::Mutex<ActivityOwnership>>,
}

#[derive(Default)]
struct ActivityOwnership {
    claimed_turns: std::collections::HashSet<(crate::ThreadId, crate::TurnId)>,
    registered_turns: std::collections::HashMap<crate::TurnId, crate::TurnAddress>,
    started_turns: std::collections::HashSet<(crate::ThreadId, crate::TurnId)>,
    completed_turns: std::collections::HashSet<(crate::ThreadId, crate::TurnId)>,
    ended_turns: std::collections::HashSet<(crate::ThreadId, crate::TurnId)>,
    not_started: std::collections::HashMap<crate::TurnId, ProviderError>,
    resolution_changed: std::sync::Arc<tokio::sync::Notify>,
    pending_models:
        std::collections::HashMap<(crate::ThreadId, crate::TurnId), Vec<crate::ResolvedModel>>,
    items: std::collections::HashMap<crate::ItemId, ItemOwner>,
    identical_replays: u64,
}

/// Choose whether a non-waiting delivery that loses the delivery lock fences the stream.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum DeliveryContention {
    /// The lost event is required observation, so contention records stream loss.
    Fence,
    /// The caller fences with its own known cause immediately afterward.
    Report,
}

/// Give publication a checked admission result rather than silently repeating replayed effects.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum ObservationAdmission {
    /// Apply a newly validated observation to its original owner.
    Apply,
    /// Acknowledge an exactly matching replay without publishing its effects again.
    IdenticalReplay,
}

/// Retain one item's ownership for the binding's life so late replays are still validated.
///
/// The record outlives its turn, because a provider may replay an item's start or completion
/// after the turn completed, and an identical replay must be acknowledged while a changed one is
/// rejected. Only fixed-size fingerprints of the start and completion are kept, not their
/// serialized bodies, so tool output and message text are not held for the binding's life.
#[derive(Clone, Debug, Eq, PartialEq)]
struct ItemOwner {
    thread_id: crate::ThreadId,
    turn_id: crate::TurnId,
    kind: ItemKind,
    initial: ContentFingerprint,
    completion: Option<ContentFingerprint>,
    background: bool,
    executing: bool,
    classification: Option<crate::MessageClassification>,
}

/// Identify serialized observation content by length and two independently seeded 64-bit
/// hashes, without retaining the content.
///
/// Fingerprints are compared only within one process, so the standard hasher's lack of a
/// stable cross-release algorithm does not matter. An accidental collision between a replay
/// and changed content requires both hashes and the length to agree.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct ContentFingerprint {
    length: usize,
    primary: u64,
    secondary: u64,
}

impl ContentFingerprint {
    fn of(encoded: &[u8]) -> Self {
        use std::hash::{BuildHasher, Hasher};
        let seeded = |seed: u8| {
            let mut hasher = FINGERPRINT_HASHER.build_hasher();
            hasher.write_u8(seed);
            hasher.write(encoded);
            hasher.finish()
        };
        Self {
            length: encoded.len(),
            primary: seeded(0),
            secondary: seeded(1),
        }
    }
}

/// One process-wide hasher state, so every fingerprint of the same content agrees.
static FINGERPRINT_HASHER: std::sync::LazyLock<std::collections::hash_map::RandomState> =
    std::sync::LazyLock::new(std::collections::hash_map::RandomState::new);

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum ItemKind {
    UserMessage,
    AgentMessage,
    Reasoning,
    CommandExecution,
    FileChange,
    ToolCall,
}

pub struct ProviderEventStream {
    binding_id: BindingId,
    input: EventReceiver,
    pending: Option<BoundProviderEvent>,
    queued: Option<BoundProviderEvent>,
    deadline: tokio::time::Instant,
    /// When the last output delta left the stream; the next one waits out the rest of the window.
    last_output_emission: Option<tokio::time::Instant>,
    deferred_error: Option<EventDeliveryError>,
    ownership: std::sync::Arc<std::sync::Mutex<ActivityOwnership>>,
    published: std::sync::Arc<std::sync::atomic::AtomicU64>,
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
    let ownership = Default::default();
    let published = Default::default();
    (
        ProviderEventSink {
            binding_id: binding_id.clone(),
            scope: scope.into(),
            sender,
            failure: failure.clone(),
            delivery: Default::default(),
            published: std::sync::Arc::clone(&published),
            observer: Vec::new(),
            ownership: std::sync::Arc::clone(&ownership),
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
            last_output_emission: None,
            deferred_error: None,
            ownership,
            published,
        },
    )
}

impl ProviderEventSink {
    /// Get the binding generation that owns every event sent through this sink.
    pub fn binding_id(&self) -> &BindingId {
        &self.binding_id
    }

    /// Reject native admission once this exact binding has failed or lost its sole receiver.
    pub fn check(&self) -> Result<(), EventDeliveryError> {
        if let Some(error) = self
            .failure
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .clone()
        {
            return Err(error);
        }
        if self.sender.is_closed() {
            return Err(EventDeliveryError::Closed);
        }
        Ok(())
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
                crate::error::ProviderOperation::TurnStart,
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
        if original.thread_id != invocation.thread_id
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
            || self.scope.thread_id() != received.thread_id()
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

    /// Fence connection-owned observation loss independently of any active turn or diagnostic capacity.
    pub async fn unavailable(&self, failure: crate::ProviderFailure) {
        self.fence_unavailable(failure, crate::ObservationFailure::LifecycleUnavailable);
    }

    /// Fence connection-owned observation loss with its known cause, so the first
    /// recorded failure reports what actually happened rather than a generic loss.
    pub async fn unavailable_because(
        &self,
        failure: crate::ProviderFailure,
        observation: crate::ObservationFailure,
    ) {
        self.fence_unavailable(failure, observation);
    }

    /// Fence idle native observation loss while preserving its exact first stream cause.
    pub async fn observation_unavailable(&self, observation: crate::ObservationFailure) {
        let kind = match observation {
            crate::ObservationFailure::Disconnected => crate::ProviderFailureKind::Transport,
            crate::ObservationFailure::Overflow => crate::ProviderFailureKind::Runtime,
            crate::ObservationFailure::DeliveryUncertain => crate::ProviderFailureKind::Unknown,
            crate::ObservationFailure::LifecycleUnavailable
            | crate::ObservationFailure::Protocol
            | crate::ObservationFailure::OwnershipConflict
            | crate::ObservationFailure::UnexpectedExecution => {
                crate::ProviderFailureKind::Protocol
            }
        };
        let failure = crate::ProviderFailure::new(crate::ProviderOperation::TurnStart, kind)
            .at_phase(
                crate::ProviderPhase::Observation,
                crate::FailureDisposition::Invalidated,
            );
        self.fence_unavailable(failure, observation);
    }

    /// Attempt the availability notification without depending on transport capacity for fencing.
    fn fence_unavailable(
        &self,
        failure: crate::ProviderFailure,
        observation: crate::ObservationFailure,
    ) {
        let message =
            crate::SanitizedMessage::new("Provider observation or foreground collection failed")
                .expect("static normalized failure text is sanitized");
        let _ = self.send_immediately(
            ProviderEvent::Provider {
                event: crate::ProviderConnectionEvent::Unavailable(crate::ProviderUnavailable {
                    failure,
                    message,
                }),
            },
            DeliveryContention::Report,
        );
        self.fail(EventDeliveryError::Observation(observation));
    }

    /// Publish effective capabilities through the owning connection runtime.
    pub async fn capabilities_changed(
        &self,
        capabilities: crate::CapabilityManifest,
    ) -> Result<(), EventDeliveryError> {
        self.send_wait(ProviderEvent::Provider {
            event: crate::ProviderConnectionEvent::CapabilitiesChanged(capabilities),
        })
        .await
    }

    /// Create a restricted writer sharing this connection's exact ownership and ordering.
    pub fn activity_writer(&self) -> AdapterActivityWriter {
        AdapterActivityWriter { sink: self.clone() }
    }

    /// Claim the sole lifecycle publication authority for an admitted host turn.
    ///
    /// Each turn can be claimed once, so the runtime's retained runner is its only publisher.
    pub fn turn_publisher(
        &self,
        address: crate::TurnAddress,
    ) -> Result<TurnPublisher, ProviderError> {
        if address.thread_id != address.scope.thread_id
            || address.scope.turn_id().as_ref() != Ok(&address.turn_id)
            || self.scope.thread_id() != &address.scope.thread_id
        {
            return Err(crate::ProviderFailure::new(
                crate::ProviderOperation::TurnStart,
                crate::ProviderFailureKind::WrongInvocation,
            )
            .into());
        }
        let sink = if self.scope.invocation() == Some(&address.scope) {
            self.clone()
        } else {
            self.invocation_sink(&address.scope)?
        };
        {
            let mut ownership = self
                .ownership
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            let key = (address.thread_id.clone(), address.turn_id.clone());
            if ownership.claimed_turns.contains(&key)
                || ownership
                    .registered_turns
                    .get(&address.turn_id)
                    .is_some_and(|registered| registered != &address)
            {
                return Err(crate::ProviderFailure::new(
                    crate::ProviderOperation::TurnStart,
                    crate::ProviderFailureKind::WrongInvocation,
                )
                .into());
            }
            ownership
                .registered_turns
                .insert(address.turn_id.clone(), address.clone());
            ownership.claimed_turns.insert(key);
        }
        Ok(TurnPublisher {
            sink,
            address,
            started: Default::default(),
            started_changed: Default::default(),
            observed: Default::default(),
        })
    }

    /// Create lifecycle authority for a positively correlated native child with independent collection ownership.
    pub fn observed_turn_publisher(
        &self,
        address: crate::TurnAddress,
    ) -> Result<TurnPublisher, ProviderError> {
        let writer = self
            .activity_writer()
            .register_observed_turn(address.clone())?;
        writer.sink.turn_publisher(address)
    }

    /// Retain an immutable typed ownership record before any native turn publication.
    fn register_turn(&self, address: &crate::TurnAddress) -> Result<(), EventDeliveryError> {
        let mut ownership = self
            .ownership
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        match ownership.registered_turns.entry(address.turn_id.clone()) {
            std::collections::hash_map::Entry::Occupied(entry) if entry.get() != address => Err(
                EventDeliveryError::Observation(crate::ObservationFailure::OwnershipConflict),
            ),
            std::collections::hash_map::Entry::Occupied(_) => Ok(()),
            std::collections::hash_map::Entry::Vacant(entry) => {
                entry.insert(address.clone());
                Ok(())
            }
        }
    }

    fn validate_scope(&self, event: &ProviderEvent) -> Result<(), EventDeliveryError> {
        if let Some(error) = self
            .failure
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .clone()
        {
            return Err(error);
        }
        if let ProviderEvent::Thread {
            thread_id,
            event: crate::ThreadEvent::CompactionUpdated(compaction),
        } = event
            && compaction.thread_id != thread_id.as_str()
        {
            return Err(EventDeliveryError::Observation(
                crate::ObservationFailure::OwnershipConflict,
            ));
        }
        let (thread, turn) = match event {
            ProviderEvent::Thread { thread_id, .. } => (Some(thread_id), None),
            ProviderEvent::Turn {
                thread_id, turn_id, ..
            }
            | ProviderEvent::Item {
                thread_id, turn_id, ..
            } => (Some(thread_id), Some(turn_id)),
            ProviderEvent::Provider { .. } | ProviderEvent::Agent { .. } => (None, None),
        };
        if turn.is_some_and(|turn| {
            self.ownership
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .not_started
                .contains_key(turn)
        }) {
            return Err(EventDeliveryError::Observation(
                crate::ObservationFailure::UnexpectedExecution,
            ));
        }
        if thread.is_some_and(|thread| thread != self.scope.thread_id())
            || turn.is_some_and(|turn| {
                self.scope
                    .invocation()
                    .is_none_or(|scope| scope.turn_id().as_ref() != Ok(turn))
            })
        {
            return Err(EventDeliveryError::Observation(
                crate::ObservationFailure::OwnershipConflict,
            ));
        }
        Ok(())
    }

    /// Count identical item replays acknowledged without publishing their effects again.
    pub fn identical_replay_count(&self) -> u64 {
        self.ownership
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .identical_replays
    }

    /// Validate and admit item state only while the shared delivery lock owns its publication order.
    ///
    /// An item start or completion whose identity, owner, type, and encoded contents exactly match
    /// the retained record is an identical replay. Any other repeated observation still fails.
    fn validate(&self, event: &ProviderEvent) -> Result<ObservationAdmission, EventDeliveryError> {
        let mut admission = ObservationAdmission::Apply;
        self.validate_scope(event)?;
        if let ProviderEvent::Turn {
            thread_id,
            turn_id,
            event: crate::TurnEvent::Started(_),
        } = event
            && self
                .ownership
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .ended_turns
                .contains(&(thread_id.clone(), turn_id.clone()))
        {
            return Err(EventDeliveryError::Observation(
                crate::ObservationFailure::UnexpectedExecution,
            ));
        }
        if let ProviderEvent::Item {
            thread_id,
            turn_id,
            item_id,
            event,
        } = event
        {
            let mut ownership = self
                .ownership
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            let invalid =
                || EventDeliveryError::Observation(crate::ObservationFailure::OwnershipConflict);
            let retired =
                || EventDeliveryError::Observation(crate::ObservationFailure::UnexpectedExecution);
            let key = (thread_id.clone(), turn_id.clone());
            let turn_completed =
                ownership.completed_turns.contains(&key) || ownership.ended_turns.contains(&key);
            match event {
                crate::ItemEvent::Started(start) => {
                    let kind = match start {
                        crate::ItemStart::UserMessage(_) => ItemKind::UserMessage,
                        crate::ItemStart::AgentMessage(_) => ItemKind::AgentMessage,
                        crate::ItemStart::Reasoning(_) => ItemKind::Reasoning,
                        crate::ItemStart::CommandExecution(_) => ItemKind::CommandExecution,
                        crate::ItemStart::FileChange(_) => ItemKind::FileChange,
                        crate::ItemStart::ToolCall(_) => ItemKind::ToolCall,
                    };
                    let initial = ContentFingerprint::of(
                        &serde_json::to_vec(start).map_err(EventDeliveryError::transport)?,
                    );
                    if let Some(previous) = ownership.items.get(item_id) {
                        if previous.thread_id != *thread_id
                            || previous.turn_id != *turn_id
                            || previous.kind != kind
                            || previous.initial != initial
                        {
                            return Err(invalid());
                        }
                        admission = ObservationAdmission::IdenticalReplay;
                    } else {
                        if turn_completed {
                            return Err(retired());
                        }
                        ownership.items.insert(
                            item_id.clone(),
                            ItemOwner {
                                thread_id: thread_id.clone(),
                                turn_id: turn_id.clone(),
                                kind,
                                initial,
                                completion: None,
                                background: false,
                                executing: false,
                                classification: match start {
                                    crate::ItemStart::AgentMessage(start) => {
                                        Some(start.classification)
                                    }
                                    crate::ItemStart::UserMessage(_)
                                    | crate::ItemStart::Reasoning(_)
                                    | crate::ItemStart::CommandExecution(_)
                                    | crate::ItemStart::FileChange(_)
                                    | crate::ItemStart::ToolCall(_) => None,
                                },
                            },
                        );
                    }
                }
                crate::ItemEvent::Message { event } => {
                    let owner = ownership.items.get_mut(item_id).ok_or_else(invalid)?;
                    if owner.thread_id != *thread_id
                        || owner.turn_id != *turn_id
                        || !matches!(
                            owner.kind,
                            ItemKind::UserMessage | ItemKind::AgentMessage | ItemKind::Reasoning
                        )
                    {
                        return Err(invalid());
                    }
                    match event {
                        crate::MessageItemEvent::Delta(_) => {
                            if turn_completed || owner.completion.is_some() {
                                return Err(retired());
                            }
                        }
                        crate::MessageItemEvent::Classified(incoming) => {
                            let current = owner.classification.as_mut().ok_or_else(invalid)?;
                            if turn_completed && current != incoming {
                                return Err(retired());
                            }
                            match (*current, *incoming) {
                                (_, crate::MessageClassification::Unclassified) => {}
                                (
                                    crate::MessageClassification::Unclassified,
                                    crate::MessageClassification::Known(_),
                                ) => *current = *incoming,
                                (
                                    crate::MessageClassification::Known(prior),
                                    crate::MessageClassification::Known(next),
                                ) if prior == next => {}
                                (
                                    crate::MessageClassification::Known(_),
                                    crate::MessageClassification::Known(_),
                                ) => return Err(invalid()),
                            }
                        }
                    }
                }
                crate::ItemEvent::Tool { event } => {
                    let owner = ownership.items.get_mut(item_id).ok_or_else(invalid)?;
                    if owner.thread_id != *thread_id
                        || owner.turn_id != *turn_id
                        || !matches!(
                            owner.kind,
                            ItemKind::CommandExecution | ItemKind::FileChange | ItemKind::ToolCall
                        )
                    {
                        return Err(invalid());
                    }
                    if turn_completed && (!owner.background || owner.completion.is_some()) {
                        return Err(retired());
                    }
                    match event {
                        crate::ToolItemEvent::ExecutionStarted => {
                            if owner.completion.is_some() {
                                return Err(retired());
                            }
                            owner.executing = true;
                        }
                        crate::ToolItemEvent::Backgrounded => {
                            if owner.kind != ItemKind::CommandExecution
                                || !owner.executing
                                || owner.completion.is_some()
                            {
                                return Err(invalid());
                            }
                            owner.background = true;
                        }
                        crate::ToolItemEvent::DescriptorUpdated(_)
                        | crate::ToolItemEvent::TitleChanged(_)
                        | crate::ToolItemEvent::Input(_)
                        | crate::ToolItemEvent::Output(_)
                        | crate::ToolItemEvent::Changes(_) => {}
                    }
                }
                crate::ItemEvent::Completed(completion) => {
                    let owner = ownership.items.get_mut(item_id).ok_or_else(invalid)?;
                    if owner.thread_id != *thread_id || owner.turn_id != *turn_id {
                        return Err(invalid());
                    }
                    let kind = match completion {
                        crate::ItemCompletion::UserMessage(_) => ItemKind::UserMessage,
                        crate::ItemCompletion::AgentMessage(_) => ItemKind::AgentMessage,
                        crate::ItemCompletion::Reasoning(_) => ItemKind::Reasoning,
                        crate::ItemCompletion::CommandExecution(_) => ItemKind::CommandExecution,
                        crate::ItemCompletion::FileChange(_) => ItemKind::FileChange,
                        crate::ItemCompletion::ToolCall(_) => ItemKind::ToolCall,
                    };
                    if owner.kind != kind {
                        return Err(invalid());
                    }
                    let encoded = ContentFingerprint::of(
                        &serde_json::to_vec(completion).map_err(EventDeliveryError::transport)?,
                    );
                    match &owner.completion {
                        Some(previous) if previous != &encoded => return Err(invalid()),
                        Some(_) => admission = ObservationAdmission::IdenticalReplay,
                        None => {
                            if turn_completed && !owner.background {
                                return Err(retired());
                            }
                            owner.completion = Some(encoded);
                        }
                    }
                }
            }
            if admission == ObservationAdmission::IdenticalReplay {
                ownership.identical_replays += 1;
            }
        }
        Ok(admission)
    }

    async fn send_wait(&self, event: ProviderEvent) -> Result<(), EventDeliveryError> {
        self.validate_scope(&event)
            .inspect_err(|error| self.fail(error.clone()))?;
        for observer in &self.observer {
            observer
                .before_delivery(&event)
                .inspect_err(|error| self.fail(error.clone()))?;
        }
        let _delivery = self.delivery.lock().await;
        let admission = self
            .validate(&event)
            .inspect_err(|error| self.fail(error.clone()))?;
        if admission == ObservationAdmission::IdenticalReplay {
            return Ok(());
        }
        self.send_encoded(event)
            .await
            .inspect_err(|error| self.fail(error.clone()))?;
        Ok(())
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
        if let ProviderEvent::Turn {
            thread_id,
            turn_id,
            event: crate::TurnEvent::Completed(_),
            ..
        } = &event.event
        {
            self.ownership
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .completed_turns
                .insert((thread_id.clone(), turn_id.clone()));
        }
        self.published
            .store(event.sequence.get(), std::sync::atomic::Ordering::Release);
        Ok(())
    }

    /// Publish a bounded envelope synchronously while preserving the same sequence protocol.
    fn send_immediate(&self, event: ProviderEvent) -> Result<(), EventDeliveryError> {
        let event = self.envelope(event)?;
        self.sender.send(&event).map_err(|error| {
            // A full outbound queue loses this event. `queue` maps `WouldBlock` to the
            // receiver's `Empty`, which readers treat as an idle stream, so the
            // producer classifies exhausted capacity as observation overflow.
            if error.kind() == io::ErrorKind::WouldBlock {
                EventDeliveryError::Observation(crate::lifecycle::ObservationFailure::Overflow)
            } else {
                EventDeliveryError::queue(error)
            }
        })?;
        self.published
            .store(event.sequence.get(), std::sync::atomic::Ordering::Release);
        Ok(())
    }

    /// Deliver a native event and its host observations without waiting for channel capacity.
    ///
    /// Losing the delivery lock to a concurrent waiting send loses this event, so it fences the
    /// stream.
    fn send(&self, event: ProviderEvent) -> Result<(), EventDeliveryError> {
        self.send_immediately(event, DeliveryContention::Fence)
    }

    /// Deliver without waiting; `contention` decides whether losing the delivery lock to a
    /// concurrent waiting send fences the stream.
    ///
    /// A fencing caller that already knows the real cause passes [`DeliveryContention::Report`],
    /// because the first recorded failure wins and a contended best-effort notice would
    /// otherwise record `Overflow` in place of that cause.
    fn send_immediately(
        &self,
        event: ProviderEvent,
        contention: DeliveryContention,
    ) -> Result<(), EventDeliveryError> {
        self.validate_scope(&event)
            .inspect_err(|error| self.fail(error.clone()))?;
        let _delivery = self.delivery.try_lock().map_err(|_| {
            let error =
                EventDeliveryError::Observation(crate::lifecycle::ObservationFailure::Overflow);
            if contention == DeliveryContention::Fence {
                self.fail(error.clone());
            }
            error
        })?;
        let admission = self
            .validate(&event)
            .inspect_err(|error| self.fail(error.clone()))?;
        if admission == ObservationAdmission::IdenticalReplay {
            return Ok(());
        }
        for observer in &self.observer {
            observer
                .before_delivery(&event)
                .inspect_err(|error| self.fail(error.clone()))?;
        }
        self.send_immediate(event)
            .inspect_err(|error| self.fail(error.clone()))?;
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
        let changed = self
            .ownership
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .resolution_changed
            .clone();
        changed.notify_waiters();
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
    /// Create a read-only view of original turn registrations that outlives this receiver.
    pub fn registrations(&self) -> TurnRegistrationReader {
        TurnRegistrationReader {
            ownership: self.ownership.clone(),
            published: self.published.clone(),
            failure: self.input.failure.clone(),
        }
    }
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
            // The first delta of a burst leaves at once. Later deltas merge until one window has
            // passed since the previous emission, so a burst still publishes at most once a window.
            let now = tokio::time::Instant::now();
            self.deadline = self
                .last_output_emission
                .map_or(now, |emitted| (emitted + OUTPUT_WINDOW).max(now));
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
        if self
            .pending
            .as_ref()
            .is_some_and(|pending| batchable(&pending.event))
        {
            self.last_output_emission = Some(tokio::time::Instant::now());
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

/// How long output deltas for one item merge before the stream emits them.
const OUTPUT_WINDOW: Duration = Duration::from_millis(16);

impl ProviderEvent {
    /// Report whether this event only appends message or tool output text.
    pub fn is_output_delta(&self) -> bool {
        batchable(self)
    }

    /// Report the UTF-8 length of this output delta's text, or `None` for any other event.
    ///
    /// Diagnostics record this length to measure delta batching without recording the text.
    pub fn output_delta_bytes(&self) -> Option<usize> {
        output_text(self).map(str::len)
    }

    /// Append `following` to this output delta when both extend the same item.
    ///
    /// Returns `false` and leaves both events unchanged when they are not adjacent output for one
    /// thread, turn, and item, or when the merged text would pass the 64 KiB merge boundary.
    pub fn coalesce_output(&mut self, following: &ProviderEvent) -> bool {
        merge_output(self, following)
    }
}

fn batchable(event: &ProviderEvent) -> bool {
    output_text(event).is_some()
}

fn output_text(event: &ProviderEvent) -> Option<&str> {
    match event {
        ProviderEvent::Item {
            event:
                crate::message::ItemEvent::Message {
                    event: crate::message::MessageItemEvent::Delta(text),
                },
            ..
        }
        | ProviderEvent::Item {
            event:
                crate::message::ItemEvent::Tool {
                    event: crate::tool::ToolItemEvent::Output(TextUpdate::Append(text)),
                },
            ..
        } => Some(text),
        ProviderEvent::Provider { .. }
        | ProviderEvent::Thread { .. }
        | ProviderEvent::Turn { .. }
        | ProviderEvent::Item { .. }
        | ProviderEvent::Agent { .. } => None,
    }
}

fn output_bytes(event: &ProviderEvent) -> usize {
    output_text(event).map_or(0, str::len)
}

fn merge_output(pending: &mut ProviderEvent, event: &ProviderEvent) -> bool {
    if output_bytes(pending).saturating_add(output_bytes(event)) > 64 * 1024 {
        return false;
    }
    match (pending, event) {
        (
            ProviderEvent::Item {
                thread_id: prior_thread,
                turn_id: prior_turn,
                item_id: prior_item,
                event:
                    crate::message::ItemEvent::Message {
                        event: crate::message::MessageItemEvent::Delta(previous),
                    },
            },
            ProviderEvent::Item {
                thread_id,
                turn_id,
                item_id,
                event:
                    crate::message::ItemEvent::Message {
                        event: crate::message::MessageItemEvent::Delta(text),
                    },
            },
        )
        | (
            ProviderEvent::Item {
                thread_id: prior_thread,
                turn_id: prior_turn,
                item_id: prior_item,
                event:
                    crate::message::ItemEvent::Tool {
                        event: crate::tool::ToolItemEvent::Output(TextUpdate::Append(previous)),
                    },
            },
            ProviderEvent::Item {
                thread_id,
                turn_id,
                item_id,
                event:
                    crate::message::ItemEvent::Tool {
                        event: crate::tool::ToolItemEvent::Output(TextUpdate::Append(text)),
                    },
            },
        ) if prior_thread == thread_id && prior_turn == turn_id && prior_item == item_id => {
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
}

/// Read immutable native turn registrations independently of the sole event receiver.
#[derive(Clone)]
pub struct TurnRegistrationReader {
    ownership: std::sync::Arc<std::sync::Mutex<ActivityOwnership>>,
    published: std::sync::Arc<std::sync::atomic::AtomicU64>,
    failure: std::sync::Arc<std::sync::Mutex<Option<EventDeliveryError>>>,
}

impl TurnRegistrationReader {
    /// Get the native publication boundary preceding an ownership callback's proof lookup.
    pub fn watermark(&self) -> crate::EventSequence {
        crate::EventSequence::new(self.published.load(std::sync::atomic::Ordering::Acquire))
    }

    /// Report whether a registered turn names this thread, without selecting any turn.
    pub fn thread_registered(&self, thread_id: &crate::ThreadId) -> bool {
        self.ownership
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .registered_turns
            .values()
            .any(|address| &address.thread_id == thread_id)
    }

    /// Get a definitive no-start operation error retained after successful native collection.
    pub fn not_started_error(&self, turn_id: &crate::TurnId) -> Option<ProviderError> {
        self.ownership
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .not_started
            .get(turn_id)
            .cloned()
    }

    /// Observe the original runner's start or collected no-start result after an operation waiter expires.
    pub async fn start_result(&self, turn_id: &crate::TurnId) -> Result<(), ProviderError> {
        let changed = {
            let ownership = self
                .ownership
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            if !ownership.registered_turns.contains_key(turn_id) {
                return Err(crate::ProviderFailure::new(
                    crate::ProviderOperation::TurnStart,
                    crate::ProviderFailureKind::WrongInvocation,
                )
                .into());
            }
            ownership.resolution_changed.clone()
        };
        loop {
            let notified = changed.notified();
            tokio::pin!(notified);
            notified.as_mut().enable();
            {
                let ownership = self
                    .ownership
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner);
                if let Some(error) = ownership.not_started.get(turn_id) {
                    return Err(error.clone());
                }
                if let Some(address) = ownership.registered_turns.get(turn_id)
                    && ownership
                        .started_turns
                        .contains(&(address.thread_id.clone(), address.turn_id.clone()))
                {
                    return Ok(());
                }
            }
            if let Some(error) = self
                .failure
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .clone()
            {
                return Err(error.into());
            }
            notified.await;
        }
    }

    /// Get the original registered owner without substituting a current or visible turn.
    pub fn registered_turn(&self, turn_id: &crate::TurnId) -> Option<crate::TurnAddress> {
        self.ownership
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .registered_turns
            .get(turn_id)
            .cloned()
    }
}

/// Publish native activity without access to turn completion or host notifications.
///
/// Completion cannot cross the activity writer's publication boundary:
/// ```compile_fail
/// use forge_provider_api::{AdapterActivityWriter, CompletedTurn, ProviderEvent, ThreadId, TurnId, TurnEvent};
/// fn finish(writer: &AdapterActivityWriter, thread_id: ThreadId, turn_id: TurnId, completed: CompletedTurn) {
///     writer.send(ProviderEvent::Turn { thread_id, turn_id, event: TurnEvent::Completed(completed) }).unwrap();
/// }
/// ```
/// Distinct ID categories cannot be exchanged:
/// ```compile_fail
/// use forge_provider_api::{AdapterActivity, ThreadId, TurnId, TurnMetadataEvent, ProviderFailure};
/// fn swapped(thread_id: ThreadId, turn_id: TurnId, error: ProviderFailure) {
///     let _ = AdapterActivity::Turn { thread_id: turn_id, turn_id: thread_id, event: TurnMetadataEvent::Error { error } };
/// }
/// ```
#[derive(Clone)]
pub struct AdapterActivityWriter {
    sink: ProviderEventSink,
}

impl AdapterActivityWriter {
    /// Get the original provider binding.
    pub fn binding_id(&self) -> &BindingId {
        self.sink.binding_id()
    }
    /// Get the retained host authority of this writer.
    pub fn scope(&self) -> &EventScope {
        self.sink.scope()
    }
    /// Reject callbacks after the original binding has failed or lost its event reader.
    pub fn check(&self) -> Result<(), EventDeliveryError> {
        self.sink.check()
    }

    /// Get the original admitted scope for turn-owned callbacks.
    pub fn invocation_scope(&self) -> Result<&InvocationScope, ProviderError> {
        self.sink.invocation_scope()
    }
    /// Get the final successfully published event position.
    pub fn watermark(&self) -> crate::EventSequence {
        self.sink.watermark()
    }
    /// Bind connection observations to one original admitted turn.
    pub fn invocation_writer(&self, scope: &InvocationScope) -> Result<Self, EventDeliveryError> {
        Ok(Self {
            sink: self.sink.invocation_sink(scope)?,
        })
    }
    /// Bind explicitly observed child activity to its immutable native ownership.
    pub fn register_observed_turn(
        &self,
        address: crate::TurnAddress,
    ) -> Result<Self, EventDeliveryError> {
        if !address
            .scope
            .thread_id
            .shares_namespace(self.sink.scope.thread_id())
            || address.thread_id != address.scope.thread_id
            || address.scope.turn_id().as_ref() != Ok(&address.turn_id)
        {
            return Err(EventDeliveryError::Observation(
                crate::ObservationFailure::OwnershipConflict,
            ));
        }
        self.sink.register_turn(&address)?;
        Ok(Self {
            sink: ProviderEventSink {
                scope: address.scope.into(),
                ..self.sink.clone()
            },
        })
    }
    /// Publish permitted activity while waiting for bounded transport capacity.
    pub async fn send_wait(
        &self,
        activity: crate::AdapterActivity,
    ) -> Result<(), EventDeliveryError> {
        self.sink
            .validate_scope(&ProviderEvent::from(activity.clone()))
            .inspect_err(|error| self.sink.fail(error.clone()))?;
        let activity = self.retain_prestart_model(activity);
        match activity {
            Some(activity) => self.sink.send_wait(activity.into()).await,
            None => Ok(()),
        }
    }
    /// Publish permitted activity without waiting for bounded capacity.
    pub fn send(&self, activity: crate::AdapterActivity) -> Result<(), EventDeliveryError> {
        self.sink
            .validate_scope(&ProviderEvent::from(activity.clone()))
            .inspect_err(|error| self.sink.fail(error.clone()))?;
        let activity = self.retain_prestart_model(activity);
        match activity {
            Some(activity) => self.sink.send(activity.into()),
            None => Ok(()),
        }
    }
    /// Retain only positively correlated model metadata until its native turn has started.
    fn retain_prestart_model(
        &self,
        activity: crate::AdapterActivity,
    ) -> Option<crate::AdapterActivity> {
        if let crate::AdapterActivity::Turn {
            thread_id,
            turn_id,
            event: crate::TurnMetadataEvent::ModelResolved(model),
        } = activity
        {
            let mut ownership = self
                .sink
                .ownership
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            let key = (thread_id.clone(), turn_id.clone());
            if !ownership.started_turns.contains(&key) {
                ownership.pending_models.entry(key).or_default().push(model);
                return None;
            }
            return Some(crate::AdapterActivity::Turn {
                thread_id,
                turn_id,
                event: crate::TurnMetadataEvent::ModelResolved(model),
            });
        }
        Some(activity)
    }
    /// Record the first observation failure independently of transport capacity.
    pub fn fail(&self, error: EventDeliveryError) {
        self.sink.fail(error);
    }
    /// Await the first retained observation failure.
    pub async fn failed(&self) -> EventDeliveryError {
        self.sink.failed().await
    }
}

/// Own lifecycle publication for one claimed turn on behalf of the provider runtime's runner.
pub struct TurnPublisher {
    sink: ProviderEventSink,
    address: crate::TurnAddress,
    started: std::sync::Arc<tokio::sync::Mutex<Option<i64>>>,
    started_changed: std::sync::Arc<tokio::sync::Notify>,
    observed: std::sync::Arc<std::sync::atomic::AtomicBool>,
}

/// Announce positively attributed native start without granting completion authority.
#[derive(Clone)]
pub struct NativeTurnStart {
    sink: ProviderEventSink,
    address: crate::TurnAddress,
    started: std::sync::Arc<tokio::sync::Mutex<Option<i64>>>,
    started_changed: std::sync::Arc<tokio::sync::Notify>,
    observed: std::sync::Arc<std::sync::atomic::AtomicBool>,
}

impl TurnPublisher {
    /// Create the adapter's restricted positive-start capability.
    pub fn start_handle(&self) -> NativeTurnStart {
        NativeTurnStart {
            sink: self.sink.clone(),
            address: self.address.clone(),
            started: self.started.clone(),
            started_changed: self.started_changed.clone(),
            observed: self.observed.clone(),
        }
    }
    /// Check whether positively attributed native work began.
    pub async fn has_started(&self) -> bool {
        self.observed.load(std::sync::atomic::Ordering::Acquire)
    }
    /// Seal foreground activity at the native boundary before resource collection can wait.
    pub async fn native_ended(&self) {
        let _delivery = self.sink.delivery.lock().await;
        self.sink
            .ownership
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .ended_turns
            .insert((self.address.thread_id.clone(), self.address.turn_id.clone()));
    }

    /// Publish one collected native result without changing its outcome.
    pub async fn completed(
        self,
        result: crate::TurnResult,
        collected_at_ms: i64,
    ) -> Result<crate::CompletedTurn, ProviderError> {
        let valid_timestamp = self
            .started
            .lock()
            .await
            .is_some_and(|started| result.completed_at_ms() >= started);
        if !self.has_started().await || !valid_timestamp {
            let failure = crate::ProviderFailure::new(
                crate::ProviderOperation::TurnStart,
                crate::ProviderFailureKind::Protocol,
            )
            .at_phase(
                crate::ProviderPhase::Observation,
                crate::FailureDisposition::Invalidated,
            );
            self.sink.unavailable(failure).await;
            return Err(failure.into());
        }
        let completed = crate::CompletedTurn::new(result, collected_at_ms);
        self.sink
            .send_wait(ProviderEvent::Turn {
                thread_id: self.address.thread_id,
                turn_id: self.address.turn_id,
                event: crate::TurnEvent::Completed(completed.clone()),
            })
            .await?;
        Ok(completed)
    }
    /// Retain a collected no-start operation error without constructing native turn completion.
    pub fn not_started(self, error: ProviderError) {
        if self.observed.load(std::sync::atomic::Ordering::Acquire) {
            return;
        }
        let changed = {
            let mut ownership = self
                .sink
                .ownership
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            ownership
                .pending_models
                .remove(&(self.address.thread_id, self.address.turn_id.clone()));
            ownership
                .not_started
                .entry(self.address.turn_id)
                .or_insert(error);
            ownership.resolution_changed.clone()
        };
        changed.notify_waiters();
    }

    /// Fence the original connection while preserving the operation's first failure.
    pub async fn unavailable(&self, failure: crate::ProviderFailure) {
        self.sink.unavailable(failure).await;
    }
}

impl NativeTurnStart {
    /// Get whether positive native start has crossed its ordered publication boundary.
    pub fn is_observed(&self) -> bool {
        self.observed.load(std::sync::atomic::Ordering::Acquire)
    }

    /// Wait for positive native start without changing input acceptance or owning cancellation.
    pub async fn wait_observed(&self) {
        loop {
            let changed = self.started_changed.notified();
            tokio::pin!(changed);
            changed.as_mut().enable();
            if self.is_observed() {
                return;
            }
            changed.await;
        }
    }

    /// Publish the first positive start once, preserving its original native timestamp.
    pub async fn observed(&self, started_at_ms: i64) -> Result<(), ProviderError> {
        let mut started = self.started.lock().await;
        if started.is_some() {
            return Ok(());
        }
        let _delivery = self.sink.delivery.lock().await;
        let event = ProviderEvent::Turn {
            thread_id: self.address.thread_id.clone(),
            turn_id: self.address.turn_id.clone(),
            event: crate::TurnEvent::Started(crate::turn::TurnStarted { started_at_ms }),
        };
        self.sink.validate(&event)?;
        for observer in &self.sink.observer {
            observer.before_delivery(&event)?;
        }
        self.sink
            .send_encoded(event)
            .await
            .inspect_err(|error| self.sink.fail(error.clone()))?;
        *started = Some(started_at_ms);
        self.observed
            .store(true, std::sync::atomic::Ordering::Release);
        self.started_changed.notify_waiters();
        let (pending_model, resolution_changed) = {
            let mut ownership = self
                .sink
                .ownership
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            let key = (self.address.thread_id.clone(), self.address.turn_id.clone());
            ownership.started_turns.insert(key.clone());
            let pending = ownership.pending_models.remove(&key);
            (pending, ownership.resolution_changed.clone())
        };
        resolution_changed.notify_waiters();
        for model in pending_model.unwrap_or_default() {
            let event = ProviderEvent::Turn {
                thread_id: self.address.thread_id.clone(),
                turn_id: self.address.turn_id.clone(),
                event: crate::TurnEvent::ModelResolved(model),
            };
            for observer in &self.sink.observer {
                observer.before_delivery(&event)?;
            }
            self.sink
                .send_encoded(event)
                .await
                .inspect_err(|error| self.sink.fail(error.clone()))?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{AdapterActivity, TurnAddress, TurnEvent};

    /// Create one exact host registration before native dispatch.
    fn fixture() -> (ProviderEventSink, ProviderEventStream, TurnAddress) {
        let scope = InvocationScope::new(crate::testing::root_thread("owner"), "exchange");
        let address = TurnAddress {
            thread_id: scope.thread_id.clone(),
            turn_id: scope.turn_id().unwrap(),
            scope: scope.clone(),
        };
        let binding = BindingId::new("binding").unwrap();
        let (sink, stream) = channel(binding, scope);
        (sink, stream, address)
    }

    /// Identical start and completion replays are acknowledged once; changed replays still fail.
    #[tokio::test]
    async fn identical_item_replay_is_acknowledged_without_republication() {
        let (sink, mut stream, address) = fixture();
        let item = |event| AdapterActivity::Item {
            thread_id: address.thread_id.clone(),
            turn_id: address.turn_id.clone(),
            item_id: crate::ItemId::new("replayed").unwrap(),
            event,
        };
        let start = || {
            crate::ItemEvent::Started(crate::ItemStart::Reasoning(crate::ReasoningStart {
                text: "partial".into(),
                kind: crate::ReasoningKind::Full,
            }))
        };
        let completion = |text: &str| {
            crate::ItemEvent::Completed(crate::ItemCompletion::Reasoning(
                crate::MessageCompletion {
                    text: text.into(),
                    outcome: crate::MessageOutcome::Completed,
                },
            ))
        };
        let writer = sink.activity_writer();
        for event in [start(), start(), completion("final"), completion("final")] {
            writer.send_wait(item(event)).await.unwrap();
        }
        assert_eq!(sink.identical_replay_count(), 2);
        assert_eq!(sink.watermark().get(), 2);
        assert!(matches!(
            stream.recv().await.unwrap().unwrap().event,
            ProviderEvent::Item {
                event: crate::ItemEvent::Started(_),
                ..
            }
        ));
        assert!(matches!(
            stream.recv().await.unwrap().unwrap().event,
            ProviderEvent::Item {
                event: crate::ItemEvent::Completed(_),
                ..
            }
        ));
        assert!(writer.send_wait(item(completion("changed"))).await.is_err());
    }

    /// The first delta of a burst leaves at once; later deltas merge for the rest of the window.
    #[tokio::test(start_paused = true)]
    async fn output_deltas_emit_on_the_leading_edge_and_merge_within_a_window() {
        let (sink, mut stream, address) = fixture();
        let item = |event| AdapterActivity::Item {
            thread_id: address.thread_id.clone(),
            turn_id: address.turn_id.clone(),
            item_id: crate::ItemId::new("streamed").unwrap(),
            event,
        };
        let delta = |text: &str| crate::ItemEvent::Message {
            event: crate::MessageItemEvent::Delta(text.into()),
        };
        let delta_text = |event: BoundProviderEvent| match event.event {
            ProviderEvent::Item {
                event:
                    crate::ItemEvent::Message {
                        event: crate::MessageItemEvent::Delta(text),
                    },
                ..
            } => text,
            other => panic!("expected an output delta, received {other:?}"),
        };
        let writer = sink.activity_writer();
        writer
            .send_wait(item(crate::ItemEvent::Started(
                crate::ItemStart::Reasoning(crate::ReasoningStart {
                    text: String::new(),
                    kind: crate::ReasoningKind::Full,
                }),
            )))
            .await
            .unwrap();
        stream.recv().await.unwrap().unwrap();

        let started = tokio::time::Instant::now();
        writer.send_wait(item(delta("first"))).await.unwrap();
        assert_eq!(delta_text(stream.recv().await.unwrap().unwrap()), "first");
        assert_eq!(started.elapsed(), Duration::ZERO);

        // Deltas that follow inside the window wait for its end and leave as one event.
        writer.send_wait(item(delta("second "))).await.unwrap();
        writer.send_wait(item(delta("third"))).await.unwrap();
        let merged = stream.recv().await.unwrap().unwrap();
        assert_eq!(
            merged.event.output_delta_bytes(),
            Some("second third".len())
        );
        assert_eq!(delta_text(merged), "second third");
        assert_eq!(started.elapsed(), OUTPUT_WINDOW);

        // After an idle window the next delta is a new leading edge.
        tokio::time::advance(OUTPUT_WINDOW).await;
        let resumed = tokio::time::Instant::now();
        writer.send_wait(item(delta("fourth"))).await.unwrap();
        assert_eq!(delta_text(stream.recv().await.unwrap().unwrap()), "fourth");
        assert_eq!(resumed.elapsed(), Duration::ZERO);
    }

    /// A fence whose best-effort notice loses the delivery lock to a waiting send still
    /// records its own cause rather than a spurious overflow.
    #[tokio::test]
    async fn contended_unavailable_notice_keeps_the_fence_cause() {
        let (sink, _stream, _address) = fixture();
        let held_delivery = sink.delivery.clone().lock_owned().await;
        sink.observation_unavailable(crate::ObservationFailure::Disconnected)
            .await;
        drop(held_delivery);
        assert!(matches!(
            sink.failed().await,
            EventDeliveryError::Observation(crate::ObservationFailure::Disconnected)
        ));
    }

    /// Drain accepted output before exposing the first retained stream failure.
    #[tokio::test]
    async fn unavailable_preserves_accepted_output_and_first_failure() {
        let (sink, mut stream, address) = fixture();
        let publisher = sink.turn_publisher(address).unwrap();
        publisher.start_handle().observed(1).await.unwrap();
        sink.fail(EventDeliveryError::Observation(
            crate::ObservationFailure::Disconnected,
        ));
        publisher
            .unavailable(crate::ProviderFailure::new(
                crate::ProviderOperation::TurnStart,
                crate::ProviderFailureKind::Runtime,
            ))
            .await;
        assert!(matches!(
            stream.recv().await.unwrap().unwrap().event,
            ProviderEvent::Turn {
                event: TurnEvent::Started(_),
                ..
            }
        ));
        assert!(matches!(
            stream.recv().await,
            Err(EventDeliveryError::Observation(
                crate::ObservationFailure::Disconnected
            ))
        ));
    }
}
