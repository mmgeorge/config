//! Host publications retain their resolved owner independently of native transport.
use crate::provider::ScopeOwner;

use super::session_execution::{InputPresentation, OwnerObservation};
use super::{EventScope, InvocationScope, ProviderEvent};
pub use forge_provider_api::events::EventDeliveryError;

/// Observe host state and derive host effects without granting native publication authority.
pub trait EventObserver: Send + Sync {
    /// Observe a publication before the host delivery boundary.
    fn before_delivery(&self, _event: &HarnessEvent) -> Result<(), EventDeliveryError> {
        Ok(())
    }
    /// Derive host observations from an already validated original native observation.
    fn additional_events(
        &self,
        _scope: &EventScope,
        _event: &ProviderEvent,
    ) -> Result<Vec<HostEvent>, EventDeliveryError> {
        Ok(Vec::new())
    }
}
pub use super::host_event::HostEvent;

/// Preserve the exact native generation and ordered publication position through host fanout.
#[derive(Clone, Debug, serde::Deserialize, serde::Serialize)]
pub struct NativeEventSource {
    /// Identify the original connection generation.
    pub binding_id: forge_provider_api::BindingId,
    /// Preserve the original ordered publication position.
    pub sequence: forge_provider_api::EventSequence,
}

/// Carry either a native observation or a host-owned effect with its original scope.
#[derive(Clone, Debug, serde::Deserialize, serde::Serialize)]
pub enum HarnessEvent {
    /// Preserve an actual native observation and its validated original provenance.
    Native {
        source: NativeEventSource,
        scope: EventScope,
        event: Box<ProviderEvent>,
    },
    /// Publish an effect committed by its original host owner.
    Host { scope: EventScope, event: HostEvent },
}
impl HarnessEvent {
    /// Get the original owner resolved before this publication entered delivery.
    pub fn scope(&self) -> &EventScope {
        match self {
            Self::Native { scope, .. } | Self::Host { scope, .. } => scope,
        }
    }
    /// Get the native observation timestamp where the event supplies one.
    pub fn observed_at_ms(&self, fallback: i64) -> i64 {
        match self {
            Self::Native { event, .. } => event.observed_at_ms(fallback),
            Self::Host { .. } => fallback,
        }
    }
}

/// Publish host events under one immutable scope capability.
#[derive(Clone)]
pub struct HarnessEventSink {
    scope: EventScope,
    sender: HostSender,
    observers: Vec<std::sync::Arc<dyn EventObserver>>,
    failure: std::sync::Arc<std::sync::Mutex<Option<EventDeliveryError>>>,
}
#[derive(Clone)]
enum HostSender {
    Channel(tokio::sync::mpsc::UnboundedSender<Result<OwnerObservation, EventDeliveryError>>),
    Binding(
        forge_provider_api::BindingId,
        tokio::sync::mpsc::UnboundedSender<
            Result<super::bridge::ProviderObservation, EventDeliveryError>,
        >,
    ),
}
impl HostSender {
    /// Forward owner observations through either the local channel or its original binding intake.
    fn send(
        &self,
        event: Result<OwnerObservation, EventDeliveryError>,
    ) -> Result<(), EventDeliveryError> {
        match self {
            Self::Channel(sender) => sender.send(event).map_err(|_| EventDeliveryError::Closed),
            Self::Binding(_, sender) => sender
                .send(event.map(|event| super::bridge::ProviderObservation {
                    event,
                    native: None,
                }))
                .map_err(|_| EventDeliveryError::Closed),
        }
    }
}

impl HarnessEventSink {
    /// Get the original scope authorized by this capability.
    pub fn scope(&self) -> &EventScope {
        &self.scope
    }
    /// Get the invocation without inventing one for binding metadata.
    pub fn invocation_scope(&self) -> Result<&InvocationScope, super::ProviderError> {
        self.scope.invocation().ok_or_else(|| {
            super::ProviderError::from(forge_provider_api::ProviderFailure::new(
                forge_provider_api::ProviderOperation::TurnStart,
                forge_provider_api::ProviderFailureKind::WrongInvocation,
            ))
        })
    }
    /// Restrict binding authority to a scope owned by the same conversation.
    pub fn bind_scope(&self, scope: EventScope) -> Result<Self, EventDeliveryError> {
        if !matches!(self.scope, EventScope::Binding { .. }) || self.scope.owner() != scope.owner()
        {
            return Err(self.mismatch(scope));
        }
        Ok(Self {
            scope,
            ..self.clone()
        })
    }
    /// Route registry-validated native child observations through their original owner scope.
    /// The binding consumer must validate the native source and relationship before calling.
    pub(super) fn registered_native_sink(
        &self,
        scope: &EventScope,
    ) -> Result<Self, EventDeliveryError> {
        if !matches!(self.scope, EventScope::Binding { .. })
            || self.scope.owner().session_id() != scope.owner().session_id()
        {
            return Err(self.mismatch(scope.clone()));
        }
        Ok(Self {
            scope: scope.clone(),
            ..self.clone()
        })
    }
    /// Route host child policy publications only under a registry-proven original child invocation.
    pub(super) fn registered_observed_sink(
        &self,
        association: &super::ObservedAgentAssociation,
    ) -> Result<Self, EventDeliveryError> {
        let scope = association
            .turn()
            .cloned()
            .ok_or(EventDeliveryError::InvalidTransportData)?;
        if !matches!(self.scope, EventScope::Binding { .. })
            || self.scope.owner().session_id() != scope.owner().session_id()
        {
            return Err(self.mismatch(scope.into()));
        }
        Ok(Self {
            scope: scope.into(),
            ..self.clone()
        })
    }
    /// Restrict binding authority to one admitted invocation.
    pub fn invocation_sink(&self, scope: &InvocationScope) -> Result<Self, EventDeliveryError> {
        self.bind_scope(scope.clone().into())
    }
    /// Route a fresh admitted retry within the same exchange without changing the old capability.
    pub fn successor_invocation_sink(
        &self,
        invocation: &InvocationScope,
    ) -> Result<Self, EventDeliveryError> {
        let original = self
            .scope
            .invocation()
            .ok_or_else(|| self.mismatch(invocation.clone().into()))?;
        if original.owner() != invocation.owner()
            || original.exchange_id != invocation.exchange_id
            || original.invocation_id == invocation.invocation_id
        {
            return Err(self.mismatch(invocation.clone().into()));
        }
        Ok(Self {
            scope: invocation.clone().into(),
            ..self.clone()
        })
    }
    /// Bind a child scope only after the bridge proves its original native registration and delegation.
    pub(super) fn observed_invocation_sink(
        &self,
        scope: &InvocationScope,
        binding: forge_provider_api::BindingId,
        sender: tokio::sync::mpsc::UnboundedSender<
            Result<super::bridge::ProviderObservation, EventDeliveryError>,
        >,
    ) -> Result<Self, EventDeliveryError> {
        let parent = self
            .invocation_scope()
            .map_err(|_| self.mismatch(scope.clone().into()))?;
        if parent.owner().session_id() != scope.owner().session_id()
            || parent.exchange_id != scope.exchange_id
        {
            return Err(self.mismatch(scope.clone().into()));
        }
        Ok(Self {
            scope: scope.clone().into(),
            sender: HostSender::Binding(binding, sender),
            failure: Default::default(),
            ..self.clone()
        })
    }
    /// Attach a host-only observer to this capability.
    pub fn with_observer(mut self, observer: std::sync::Arc<dyn EventObserver>) -> Self {
        self.observers.push(observer);
        self
    }
    /// Retain the first channel failure for every publisher and wake the receiver.
    pub fn fail(&self, error: EventDeliveryError) {
        let mut failure = self
            .failure
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        if failure.is_none() {
            *failure = Some(error.clone());
            let _ = self.sender.send(Err(error));
        }
    }
    /// Publish only after validating the already resolved original owner.
    pub fn send(&self, event: HarnessEvent) -> Result<(), EventDeliveryError> {
        if event.scope() != &self.scope {
            return Err(self.mismatch(event.scope().clone()));
        }
        if let Some(error) = self
            .failure
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .clone()
        {
            return Err(error);
        }
        for observer in &self.observers {
            observer.before_delivery(&event)?;
        }
        self.sender
            .send(Ok(OwnerObservation::Event(event)))
            .map_err(|_| EventDeliveryError::Closed)
    }
    /// Publish a resolved event and retain the existing asynchronous call contract.
    pub async fn send_wait(&self, event: HarnessEvent) -> Result<(), EventDeliveryError> {
        self.send(event)
    }
    /// Preserve direct acceptance receipts while forwarding owner publications.
    pub async fn send_observation(
        &self,
        observation: OwnerObservation,
    ) -> Result<(), EventDeliveryError> {
        match observation {
            OwnerObservation::Event(event) => self.send(event),
            OwnerObservation::HostFileChange {
                scope,
                binding_id,
                change,
            } => {
                if self.scope != EventScope::from(scope.clone()) {
                    return Err(self.mismatch(scope.into()));
                }
                self.sender.send(Ok(OwnerObservation::HostFileChange {
                    scope,
                    binding_id,
                    change,
                }))
            }
            OwnerObservation::InputAcceptance {
                scope,
                receipt,
                presentation,
            } => self.send_acceptance(scope, receipt, presentation).await,
        }
    }
    /// Publish a committed physical change under its checked original child generation.
    pub(crate) fn send_host_file_change(
        &self,
        change: crate::tools::lsp::HostFileChange,
    ) -> Result<(), EventDeliveryError> {
        let HostSender::Binding(binding_id, _) = &self.sender else {
            return Err(EventDeliveryError::Closed);
        };
        let scope = self.scope.invocation().ok_or(EventDeliveryError::Closed)?;
        if &change.scope != scope {
            return Err(self.mismatch(change.scope.clone().into()));
        }
        self.sender.send(Ok(OwnerObservation::HostFileChange {
            scope: scope.clone(),
            binding_id: binding_id.clone(),
            change,
        }))
    }
    /// Publish durable native acceptance directly to the session input owner.
    pub(crate) async fn send_acceptance(
        &self,
        scope: InvocationScope,
        receipt: std::sync::Arc<super::submission::Submission>,
        presentation: InputPresentation,
    ) -> Result<(), EventDeliveryError> {
        if self.scope != EventScope::from(scope.clone()) {
            return Err(self.mismatch(scope.into()));
        }
        if let Some(error) = self
            .failure
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .clone()
        {
            return Err(error);
        }
        self.sender
            .send(Ok(OwnerObservation::InputAcceptance {
                scope,
                receipt,
                presentation,
            }))
            .map_err(|_| EventDeliveryError::Closed)
    }
    /// Publish a host effect under this capability's immutable owner.
    pub async fn send_host(&self, event: HostEvent) -> Result<(), EventDeliveryError> {
        self.send(HarnessEvent::Host {
            scope: self.scope.clone(),
            event,
        })
    }
    /// Reject a publication that crosses its admitted ownership boundary.
    fn mismatch(&self, received: EventScope) -> EventDeliveryError {
        EventDeliveryError::OwnershipMismatch {
            expected: Box::new(self.scope.clone()),
            received: Box::new(received),
        }
    }
}

/// Receive host publications without fabricating native stream positions.
pub struct HarnessEventStream {
    receiver: tokio::sync::mpsc::UnboundedReceiver<Result<OwnerObservation, EventDeliveryError>>,
}
impl HarnessEventStream {
    /// Receive the next original-owner publication.
    pub async fn recv(&mut self) -> Result<Option<OwnerObservation>, EventDeliveryError> {
        self.receiver.recv().await.transpose()
    }
    /// Drain one available publication.
    pub fn try_recv(&mut self) -> Result<OwnerObservation, EventDeliveryError> {
        self.receiver.try_recv().map_err(|error| match error {
            tokio::sync::mpsc::error::TryRecvError::Empty => EventDeliveryError::Empty,
            tokio::sync::mpsc::error::TryRecvError::Disconnected => EventDeliveryError::Closed,
        })?
    }
    /// Distinguish no available publication from a delivery failure.
    pub fn try_next(&mut self) -> Result<Option<OwnerObservation>, EventDeliveryError> {
        match self.try_recv() {
            Ok(event) => Ok(Some(event)),
            Err(EventDeliveryError::Empty | EventDeliveryError::Closed) => Ok(None),
            Err(error) => Err(error),
        }
    }
}
/// Create an owner-bound host channel independent of native transport generations.
pub fn channel(scope: impl Into<EventScope>) -> (HarnessEventSink, HarnessEventStream) {
    let (sender, receiver) = tokio::sync::mpsc::unbounded_channel();
    (
        HarnessEventSink {
            scope: scope.into(),
            sender: HostSender::Channel(sender),
            observers: Vec::new(),
            failure: Default::default(),
        },
        HarnessEventStream { receiver },
    )
}

/// Build a direct native-child accounting intake from an actual ownership-registry proof.
#[cfg(test)]
pub(crate) fn observed_file_sink_fixture(
    association: &super::ObservedAgentAssociation,
) -> Result<
    (
        HarnessEventSink,
        tokio::sync::mpsc::UnboundedReceiver<
            Result<super::bridge::ProviderObservation, EventDeliveryError>,
        >,
    ),
    EventDeliveryError,
> {
    let scope = association
        .turn()
        .ok_or(EventDeliveryError::InvalidTransportData)?;
    let (parent, _) = channel(association.parent().clone());
    let (sender, receiver) = tokio::sync::mpsc::unbounded_channel();
    let sink = parent.observed_invocation_sink(scope, association.binding_id().clone(), sender)?;
    Ok((sink, receiver))
}
