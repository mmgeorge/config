//! Prepare provider observation through a weak service callback and commit it in the owner.
//!
//! Startup never allocates a native conversation. An interactive session attaches its
//! observation listener and registers its root owner, then `ProviderBridge::turn_start` opens
//! or resumes the native conversation under admission when the first input needs it.

use std::{future::Future, pin::Pin, sync::Arc};

use crate::provider::Provider;

/// Retain a provider initialization future in the exchange owner's active stage.
pub(crate) type ProviderInitializationFuture =
    Pin<Box<dyn Future<Output = Result<ProviderInitialization, ProviderLifecycleFailure>> + Send>>;

/// Bridge the service-owned observation listener without retaining the session controller.
#[derive(Clone)]
pub(crate) struct ProviderLifecycle {
    prepare: Arc<dyn Fn(Arc<Provider>) -> ProviderInitializationFuture + Send + Sync>,
}

impl ProviderLifecycle {
    /// Create a callback whose controller capture must remain weak.
    pub(crate) fn new(
        prepare: impl Fn(Arc<Provider>) -> ProviderInitializationFuture + Send + Sync + 'static,
    ) -> Self {
        Self {
            prepare: Arc::new(prepare),
        }
    }

    /// Prepare the already published provider generation without owning its domain commit.
    pub(crate) fn prepare(&self, provider: Arc<Provider>) -> ProviderInitializationFuture {
        (self.prepare)(provider)
    }
}

/// Carry the prepared provider generation back to the owner for a guarded commit.
pub(crate) enum ProviderInitialization {
    /// This session purpose has no persistent interactive native conversation.
    NotRequired { provider: Arc<Provider> },
    /// Observation is attached; the native conversation opens on first provider work.
    Deferred { provider: Arc<Provider> },
}

impl ProviderInitialization {
    /// Accept the prepared generation only while it remains the broker's current provider.
    pub(crate) fn commit(
        self,
        broker: &crate::broker::HarnessBroker,
    ) -> Result<(), ProviderLifecycleFailure> {
        let (Self::NotRequired { provider } | Self::Deferred { provider }) = &self;
        if !Arc::ptr_eq(provider, &broker.provider_handle()) {
            return Err(ProviderLifecycleFailure::GenerationChanged);
        }
        Ok(())
    }
}

/// Preserve normalized lifecycle failure categories at the service-owner boundary.
#[derive(Clone, Debug, thiserror::Error)]
pub(crate) enum ProviderLifecycleFailure {
    /// The weak service owner disappeared while preparation was pending.
    #[error("the session controller closed during provider initialization")]
    ControllerClosed,
    /// A different provider generation replaced the captured preparation.
    #[error("the provider changed during initialization")]
    GenerationChanged,
    /// The prior listener has not finished its original event stream.
    #[error("the prior provider observation stream is still live")]
    PreviousObservationLive,
    /// Observation work failed or was lost before a safe handoff.
    #[error("provider observation could not be retained")]
    ObservationUnavailable,
    /// Persistent settings or initialized session metadata could not be read or committed.
    #[error("provider initialization state is unavailable")]
    StateUnavailable,
    /// A normalized adapter failure prevented native preparation.
    #[error(transparent)]
    Provider(#[from] crate::provider::ProviderError),
}
