//! Retained cleanup graph for native provider processes, profiles, and callback leases.
//!
//! A native adapter registers every resource it creates before startup can suspend or fail, so a
//! cancelled or failed open still collects its process tree. Profiles and callback leases outlive
//! the processes that use them: a profile closes only after every native process has been
//! collected, and a lease is released only after its profiles closed, because a still-running
//! process can read the profile or call back through the lease.

use async_trait::async_trait;
use forge_provider_api::ProviderError;
use std::path::Path;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
use tokio::sync::Mutex;

/// Collect one native process and its reader tasks for a retained cleanup graph.
#[async_trait]
pub(crate) trait NativeResource: Send + Sync {
    /// Collect the process tree and reader tasks, retaining handles so a failed attempt can retry.
    async fn shutdown(&self) -> Result<(), ProviderError>;
    /// Report positive collection evidence without awaiting a collection in progress.
    fn is_collected(&self) -> bool;
}

/// Retain native resources from spawn through successful collection, including failed opens.
#[derive(Clone)]
pub(crate) struct ResourceCleanup {
    resources: Arc<std::sync::Mutex<Vec<RetainedResource>>>,
    parent: Option<Arc<std::sync::Mutex<Vec<RetainedResource>>>>,
    admission: Arc<AtomicBool>,
    parent_admission: Option<Arc<AtomicBool>>,
}

impl Default for ResourceCleanup {
    fn default() -> Self {
        Self {
            resources: Arc::new(std::sync::Mutex::new(Vec::new())),
            parent: None,
            admission: Arc::new(AtomicBool::new(true)),
            parent_admission: None,
        }
    }
}

impl ResourceCleanup {
    /// Create an operation-local cleanup scope while preserving provider shutdown ownership.
    pub(crate) fn scope(&self) -> Self {
        Self {
            resources: Arc::new(std::sync::Mutex::new(Vec::new())),
            parent: Some(self.parent.as_ref().unwrap_or(&self.resources).clone()),
            admission: Arc::new(AtomicBool::new(true)),
            parent_admission: Some(
                self.parent_admission
                    .as_ref()
                    .unwrap_or(&self.admission)
                    .clone(),
            ),
        }
    }

    /// Retain a native profile before a connection can load it or suspend startup.
    pub(crate) fn retain_profile(
        &self,
        profile: crate::profile::ResourceProfile,
    ) -> Arc<RetainedProfile> {
        let profile = Arc::new(RetainedProfile {
            path: profile.path().to_owned(),
            profile: Mutex::new(Some(profile)),
            collected: AtomicBool::new(false),
        });
        self.retain(RetainedResource::Profile(profile.clone()));
        profile
    }

    /// Retain callback/server authority until its native resources and profile have collected.
    pub(crate) fn retain_lease(
        &self,
        lease: Option<Arc<dyn forge_provider_api::ResourceLease>>,
    ) -> Arc<RetainedLease> {
        let collected = lease.is_none();
        let lease = Arc::new(RetainedLease {
            lease: Mutex::new(lease),
            collected: AtomicBool::new(collected),
        });
        self.retain(RetainedResource::Lease(lease.clone()));
        lease
    }

    /// Retain one native process before its startup can suspend or fail.
    pub(crate) fn retain_native(&self, native: Arc<dyn NativeResource>) {
        self.retain(RetainedResource::Native(native));
    }

    /// Get whether every resource registered to this operation completed collection.
    pub(crate) fn is_collected(&self) -> bool {
        self.resources
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .iter()
            .all(RetainedResource::is_collected)
    }

    /// Stop native launch admission before collecting this operation's pending resources.
    pub(crate) fn close_admission(&self) {
        self.admission.store(false, Ordering::Release);
    }

    /// Require both this operation and the provider shutdown owner to admit native launch.
    pub(crate) fn is_admitting(&self) -> bool {
        self.admission.load(Ordering::Acquire)
            && self
                .parent_admission
                .as_ref()
                .is_none_or(|parent| parent.load(Ordering::Acquire))
    }

    /// Register ownership before startup can suspend or fail.
    fn retain(&self, resource: RetainedResource) {
        if let Some(parent) = &self.parent {
            parent
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .push(resource.clone());
        }
        self.resources
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .push(resource);
    }

    /// Retry processes first, then profiles and callback leases only after native collection.
    pub(crate) async fn shutdown(&self) -> Result<(), ProviderError> {
        self.close_admission();
        let resources = self
            .resources
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .clone();
        let mut errors = Vec::new();
        for resource in &resources {
            if let RetainedResource::Native(native) = resource
                && let Err(error) = native.shutdown().await
            {
                errors.push(error);
            }
        }
        let native_collected = resources.iter().all(|resource| match resource {
            RetainedResource::Native(_) => resource.is_collected(),
            RetainedResource::Profile(_) | RetainedResource::Lease(_) => true,
        });
        if native_collected {
            for resource in &resources {
                if let RetainedResource::Profile(profile) = resource
                    && let Err(error) = profile
                        .close(forge_provider_api::ProviderOperation::Shutdown)
                        .await
                {
                    errors.push(error);
                }
            }
        }
        let prerequisites_collected = resources.iter().all(|resource| match resource {
            RetainedResource::Native(_) | RetainedResource::Profile(_) => resource.is_collected(),
            RetainedResource::Lease(_) => true,
        });
        if prerequisites_collected {
            for resource in &resources {
                if let RetainedResource::Lease(lease) = resource {
                    lease.release().await;
                }
            }
        }
        let mut errors = errors.into_iter();
        let Some(primary) = errors.next() else {
            return Ok(());
        };
        let secondary = errors.collect::<Vec<_>>();
        if secondary.is_empty() {
            Err(primary)
        } else {
            Err(ProviderError::Combined {
                primary: Box::new(primary),
                secondary,
            })
        }
    }
}

/// Keep native startup, profile, and callback ownership in one retained cleanup graph.
#[derive(Clone)]
enum RetainedResource {
    Native(Arc<dyn NativeResource>),
    Profile(Arc<RetainedProfile>),
    Lease(Arc<RetainedLease>),
}

impl RetainedResource {
    /// Get positive collection evidence without awaiting a resource currently being collected.
    fn is_collected(&self) -> bool {
        match self {
            Self::Native(native) => native.is_collected(),
            Self::Profile(profile) => profile.collected.load(Ordering::Acquire),
            Self::Lease(lease) => lease.collected.load(Ordering::Acquire),
        }
    }
}

/// Retain one profile across native load/configuration failures and cleanup retries.
pub(crate) struct RetainedProfile {
    path: std::path::PathBuf,
    profile: Mutex<Option<crate::profile::ResourceProfile>>,
    collected: AtomicBool,
}

impl RetainedProfile {
    /// Get the unchanged native profile path while its retained owner controls collection.
    pub(crate) fn path(&self) -> &Path {
        &self.path
    }

    /// Close the profile once and retain a failed close for the next cleanup attempt.
    pub(crate) async fn close(
        &self,
        operation: forge_provider_api::ProviderOperation,
    ) -> Result<(), ProviderError> {
        let mut profile = self.profile.lock().await;
        if let Some(profile) = profile.as_mut() {
            profile
                .close()
                .map_err(|error| match error.into_provider_error(operation) {
                    ProviderError::Failure(failure) => {
                        forge_provider_api::ProviderFailure::new(operation, failure.kind())
                            .at_phase(
                                forge_provider_api::ProviderPhase::Collection,
                                forge_provider_api::FailureDisposition::Invalidated,
                            )
                            .into()
                    }
                    error => error,
                })?;
        }
        profile.take();
        self.collected.store(true, Ordering::Release);
        Ok(())
    }
}

/// Retain host callback authority until all native users have been collected.
pub(crate) struct RetainedLease {
    lease: Mutex<Option<Arc<dyn forge_provider_api::ResourceLease>>>,
    collected: AtomicBool,
}

impl RetainedLease {
    /// Release callback/server ownership after native collection, idempotently across retries.
    pub(crate) async fn release(&self) {
        self.lease.lock().await.take();
        self.collected.store(true, Ordering::Release);
    }
}
