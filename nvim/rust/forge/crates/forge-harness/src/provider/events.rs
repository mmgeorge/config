//! Host-local fanout of already normalized events.

pub use forge_provider_api::events::{EventDeliveryError, EventObserver, ProviderEventSink};

use super::{EventScope, ProviderEvent};

/// Keep transport envelopes at the host channel boundary rather than in presentation reducers.
pub struct ProviderEventStream {
    binding: forge_provider_api::BindingId,
    stream: forge_provider_api::events::ProviderEventStream,
}

/// Create an independent host publication channel with one immutable delivery generation.
pub fn channel(scope: impl Into<EventScope>) -> (ProviderEventSink, ProviderEventStream) {
    let binding = forge_provider_api::BindingId::new(uuid::Uuid::new_v4().to_string())
        .expect("generated host binding identity is nonempty");
    let (sink, stream) = forge_provider_api::events::channel(binding.clone(), scope);
    (sink, ProviderEventStream { binding, stream })
}

impl ProviderEventStream {
    /// Receive a normalized event after validating this host delivery generation.
    pub async fn recv(&mut self) -> Result<Option<ProviderEvent>, EventDeliveryError> {
        let event = self.stream.recv().await?;
        event.map(|event| self.validate(event)).transpose()
    }

    /// Drain an available event without waiting for provider activity.
    pub fn try_recv(&mut self) -> Result<ProviderEvent, EventDeliveryError> {
        let event = self.stream.try_recv()?;
        self.validate(event)
    }

    /// Distinguish an empty host publication queue from a failed delivery stream.
    pub fn try_next(&mut self) -> Result<Option<ProviderEvent>, EventDeliveryError> {
        match self.try_recv() {
            Ok(event) => Ok(Some(event)),
            Err(EventDeliveryError::Empty | EventDeliveryError::Closed) => Ok(None),
            Err(error) => Err(error),
        }
    }

    fn validate(
        &self,
        event: forge_provider_api::BoundProviderEvent,
    ) -> Result<ProviderEvent, EventDeliveryError> {
        if event.binding_id != self.binding {
            return Err(EventDeliveryError::Observation(
                forge_provider_api::lifecycle::ObservationFailure::OwnershipConflict,
            ));
        }
        Ok(event.event)
    }
}
