//! Retain domain services without exposing mutable exchange state to callers.

use crate::input::SessionInputQueue;
use crate::provider::submission::SubmissionStore;
use std::{path::PathBuf, sync::Arc};

/// Retain services for owner-issued preparation, local policy commits, and resource collection.
#[derive(Clone)]
pub(crate) struct OwnerServices {
    /// Reattach and initialize replacements through a weak service callback.
    pub(crate) provider_lifecycle: super::provider_lifecycle::ProviderLifecycle,
    /// Retain ordered session notifications independently of a transient transport sender.
    pub(crate) publications: Arc<super::publication::SessionPublications>,
    /// Session input journal; only the owner applies caller mutations.
    pub(crate) input: Arc<SessionInputQueue>,
    /// Independent native receipt journal used to prove safe retry.
    pub(crate) submissions: Arc<SubmissionStore>,
    /// Current provider generation, selected only while its retirement permit is held.
    pub(crate) provider: Arc<std::sync::RwLock<Arc<crate::provider::Provider>>>,
    /// Exclude binding replacement throughout native receipt reconciliation.
    pub(crate) provider_lane: Arc<tokio::sync::RwLock<()>>,
    /// Published effective configuration used by read-only discovery and future input preparation.
    pub(crate) catalog: Arc<tokio::sync::RwLock<crate::provider::ProviderCatalogRequest>>,
    /// Invalidate stale resource discovery when the owner publishes a committed configuration.
    pub(crate) resource_revision: Arc<std::sync::atomic::AtomicU64>,
    /// Root of retained attachments and command storage.
    pub(crate) root: PathBuf,
    /// Task authority affected by accepting explicit caller input.
    pub(crate) tasks: Arc<crate::task::SessionTaskController>,
    /// Revision-scoped question authority revalidated before input dispatch.
    pub(crate) questions: Arc<crate::question::QuestionController>,
    /// Published host state used to notify input scheduling and rendering.
    pub(crate) observer: Arc<crate::broker::BrokerObserver>,
    /// Project committed owner task changes into the existing session document lifetime.
    pub(crate) presentation: Arc<std::sync::Mutex<crate::buffer::session::SessionPresentation>>,
    /// Existing execution permit source retained during the remaining migration.
    pub(crate) execution: Arc<crate::command::ExecutionAdmission>,
    /// Legacy request admission, retained only until all callers enter the owner FIFO.
    pub(crate) control: Arc<tokio::sync::Semaphore>,
    /// Existing native wait signal, written by owner transitions during migration.
    pub(crate) cancellation: Arc<crate::broker::TurnCancellation>,
    /// Session approval resources collected by owner-issued cancellation work.
    pub(crate) permissions: Arc<crate::provider::approval::PermissionCoordinator>,
    /// Child resource ownership retained throughout collection.
    pub(crate) children: Arc<crate::agent::AgentCoordinator>,
    /// Identified project-check processes collected with current exchange work.
    pub(crate) checks: Arc<crate::checks::CheckSupervisor>,
    /// Session task budget used by owner-controlled task creation.
    pub(crate) goal_max_turns: u32,
}

impl OwnerServices {
    /// Capture the provider only after excluding retirement for all cloned users of this permit.
    pub(super) async fn observe_provider_generation(&self) -> ProviderGenerationPermit {
        let lease = self.provider_lane.clone().read_owned().await;
        let provider = self
            .provider
            .read()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .clone();
        ProviderGenerationPermit {
            provider,
            _lease: Arc::new(lease),
        }
    }
}

/// Share one admitted provider generation without recursively acquiring a fair read lock.
#[derive(Clone)]
pub(super) struct ProviderGenerationPermit {
    provider: Arc<crate::provider::Provider>,
    _lease: Arc<tokio::sync::OwnedRwLockReadGuard<()>>,
}

impl ProviderGenerationPermit {
    /// Get the provider selected under this still-live retirement exclusion.
    pub(super) fn provider(&self) -> &Arc<crate::provider::Provider> {
        &self.provider
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Keep retirement blocked until every user of the admitted generation releases its permit.
    #[tokio::test]
    async fn shared_generation_permit_outlives_each_observer_and_waiting_writer()
    -> anyhow::Result<()> {
        let root = tempfile::tempdir()?;
        let broker = crate::broker::HarnessBroker::initialize(serde_json::from_value(
            serde_json::json!({
                "workspace":root.path(),"data_root":root.path().join("data"),"client_id":"generation-permit",
                "provider":{"kind":"mock","command":[]},
            }),
        )?)?;
        let lane = Arc::new(tokio::sync::RwLock::new(()));
        let permit = ProviderGenerationPermit {
            provider: broker.provider_handle(),
            _lease: Arc::new(lane.clone().read_owned().await),
        };
        let mut retirement = Box::pin(lane.write_owned());
        assert!(futures_util::poll!(retirement.as_mut()).is_pending());
        let collection = permit.clone();
        assert!(Arc::ptr_eq(permit.provider(), collection.provider()));
        drop(permit);
        assert!(futures_util::poll!(retirement.as_mut()).is_pending());
        drop(collection);
        let _retirement =
            tokio::time::timeout(std::time::Duration::from_secs(1), retirement).await?;
        Ok(())
    }
}
