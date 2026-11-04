//! Prepare native initialization through a weak service callback and commit it in the owner.

use std::{future::Future, pin::Pin, sync::Arc};

use crate::provider::{Provider, ProviderRequest};

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

/// Carry initialized native identity back to the owner for a guarded domain commit.
pub(crate) enum ProviderInitialization {
    /// This session purpose has no persistent interactive native initialization.
    NotRequired { provider: Arc<Provider> },
    /// Native initialization completed for the exact captured settings and provider.
    Ready {
        provider: Arc<Provider>,
        request: Box<ProviderRequest>,
        provider_session_id: Option<forge_provider_api::ProviderSessionId>,
    },
}

impl ProviderInitialization {
    /// Commit initialized metadata only while the captured provider remains current.
    pub(crate) fn commit(
        self,
        broker: &mut crate::broker::HarnessBroker,
    ) -> Result<Option<crate::broker::BrokerSnapshot>, ProviderLifecycleFailure> {
        let provider = match &self {
            Self::NotRequired { provider } | Self::Ready { provider, .. } => provider,
        };
        if !Arc::ptr_eq(provider, &broker.provider_handle()) {
            return Err(ProviderLifecycleFailure::GenerationChanged);
        }
        match self {
            Self::NotRequired { .. }
            | Self::Ready {
                provider_session_id: None,
                ..
            } => Ok(None),
            Self::Ready {
                request,
                provider_session_id: Some(id),
                ..
            } => broker
                .retain_initialized_session(&request, id.into())
                .map_err(|_| ProviderLifecycleFailure::StateUnavailable)?
                .map(Some)
                .ok_or(ProviderLifecycleFailure::GenerationChanged),
        }
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
