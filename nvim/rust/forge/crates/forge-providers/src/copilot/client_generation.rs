//! Scope native health and recovery proofs to one shared SDK client.

use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, OnceLock};
use std::time::Duration;

use forge_provider_api::ProviderError;
use forge_provider_api::error::{CleanupFailure, ObservationCleanup};
use forge_provider_api::lifecycle::ObservationFailure;
use github_copilot_sdk::Client;
use tokio::sync::{Mutex, watch};
use tokio::task::JoinHandle;

/// Identifies a failed shared client without granting access to a successor.
#[derive(Clone)]
pub struct FailedGeneration {
    generation: ClientGeneration,
}

impl FailedGeneration {
    /// Return the first failure shared by every session on this client.
    pub fn failure(&self) -> ObservationFailure {
        self.generation
            .failure()
            .expect("failed generation retains its first failure")
    }

    /// Compare recovery ownership without exposing either native client.
    pub fn same(&self, other: &Self) -> bool {
        self.generation.same(&other.generation)
    }

    /// Compare the exact client allocation, independently of session identifiers.
    pub(crate) fn matches(&self, generation: &ClientGeneration) -> bool {
        self.generation.same(generation)
    }
}

/// Proves the failed SDK client's retained stop task has been joined.
#[derive(Clone)]
pub struct CollectedClient {
    failed: FailedGeneration,
}

impl CollectedClient {
    /// Identify the generation whose local session tasks may now be collected.
    pub fn failed_generation(&self) -> &FailedGeneration {
        &self.failed
    }
}

/// Proves all registered session work for a stopped client has been collected.
pub struct CollectedSessions {
    failed: FailedGeneration,
}

impl CollectedSessions {
    /// Seal the sessions registry's successful collection of this stopped client.
    pub(crate) fn new(client: &CollectedClient) -> Self {
        Self {
            failed: client.failed.clone(),
        }
    }

    /// Identify the cache entry that this collection proof may retire.
    pub(crate) fn failed_generation(&self) -> &FailedGeneration {
        &self.failed
    }
}

enum Collection {
    Idle,
    Running(JoinHandle<Result<(), Arc<ProviderError>>>),
    Complete(Result<(), Arc<ProviderError>>),
}

/// Share one transport's sticky failure and retained collection ownership.
#[derive(Clone)]
pub(crate) struct ClientGeneration(Arc<GenerationState>);

struct GenerationState {
    client: Client,
    first_failure: OnceLock<ObservationFailure>,
    stopped: AtomicBool,
    failure: watch::Sender<Option<ObservationFailure>>,
    collection: Mutex<Collection>,
}

impl ClientGeneration {
    /// Bind fresh health to exactly one SDK client allocation.
    pub(crate) fn new(client: Client) -> Self {
        let (failure, _) = watch::channel(None);
        Self(Arc::new(GenerationState {
            client,
            first_failure: OnceLock::new(),
            stopped: AtomicBool::new(false),
            failure,
            collection: Mutex::new(Collection::Idle),
        }))
    }

    /// Get whether this generation is fenced by failure or its SDK stop has completed.
    pub(crate) fn is_stopped(&self) -> bool {
        self.failure().is_some() || self.0.stopped.load(Ordering::Acquire)
    }

    /// Publish successful SDK collection without inventing observation loss.
    pub(crate) fn mark_stopped(&self) {
        self.0.stopped.store(true, Ordering::Release);
    }

    /// Share the first failure directly with every native intake.
    pub(crate) fn failures(&self) -> watch::Receiver<Option<ObservationFailure>> {
        self.0.failure.subscribe()
    }

    /// Read sticky transport health before admitting new session work.
    pub(crate) fn failure(&self) -> Option<ObservationFailure> {
        self.0.first_failure.get().cloned()
    }

    /// Reject work on a stopped generation without implicitly replaying it elsewhere.
    pub(crate) fn ensure_healthy(&self) -> Result<(), ProviderError> {
        match self.failure() {
            Some(reason) => Err(ProviderError::Observation(reason)),
            None if self.0.stopped.load(Ordering::Acquire) => Err(native_failure(
                forge_provider_api::error::ProviderOperation::NativeTransport,
                forge_provider_api::error::ProviderFailureKind::SessionMissing,
            )),
            None => Ok(()),
        }
    }

    /// Fence this transport before publishing the token that permits SDK collection.
    pub(crate) fn fail(&self, reason: ObservationFailure) {
        self.fail_after_fencing(reason, || self.0.client.force_stop());
    }

    /// Reject new writes immediately, but expose recovery only after synchronous fencing completes.
    fn fail_after_fencing(&self, reason: ObservationFailure, fence: impl FnOnce()) {
        let first = self.0.first_failure.get_or_init(|| reason).clone();
        fence();
        self.0.failure.send_if_modified(|current| {
            if current.is_some() {
                return false;
            }
            *current = Some(first);
            true
        });
    }

    /// Obtain an opaque recovery token only after a failure has been published.
    pub(crate) fn failed(&self) -> Option<FailedGeneration> {
        self.0.failure.borrow().as_ref().map(|_| FailedGeneration {
            generation: self.clone(),
        })
    }

    /// Wait for an already known failure's physical fence before admitting recovery.
    pub(crate) async fn failed_after_fencing(&self) -> Option<FailedGeneration> {
        self.failure()?;
        let mut failure = self.failures();
        failure
            .wait_for(Option::is_some)
            .await
            .expect("generation owns the fence completion sender");
        self.failed()
    }

    /// Join SDK collection with bounded waiting while retaining ownership on cancellation.
    pub(crate) async fn collect(
        &self,
        failed: &FailedGeneration,
    ) -> Result<CollectedClient, ProviderError> {
        if !failed.matches(self) {
            return Err(native_failure(
                forge_provider_api::error::ProviderOperation::RecoverSession,
                forge_provider_api::error::ProviderFailureKind::Rejected,
            ));
        }
        let mut collection = self.0.collection.lock().await;
        if matches!(*collection, Collection::Idle) {
            let client = self.0.client.clone();
            *collection = Collection::Running(tokio::spawn(async move {
                client.stop().await.map_err(|_| {
                    Arc::new(native_failure(
                        forge_provider_api::error::ProviderOperation::CollectNativeRuntime,
                        forge_provider_api::error::ProviderFailureKind::Transport,
                    ))
                })
            }));
        }
        if let Collection::Running(task) = &mut *collection {
            let result = tokio::time::timeout(Duration::from_secs(30), task)
                .await
                .map_err(|_| {
                    ProviderError::Cleanup(Arc::new(ObservationCleanup::new(
                        failed.failure(),
                        None,
                        None,
                        Some(CleanupFailure::TimedOut),
                    )))
                })?;
            *collection = Collection::Complete(result.unwrap_or_else(|_| {
                Err(Arc::new(native_failure(
                    forge_provider_api::error::ProviderOperation::CollectNativeRuntime,
                    forge_provider_api::error::ProviderFailureKind::Runtime,
                )))
            }));
        }
        match &*collection {
            Collection::Complete(Ok(())) => {
                self.mark_stopped();
                Ok(CollectedClient {
                    failed: failed.clone(),
                })
            }
            Collection::Complete(Err(error)) => Err(ProviderError::PreparationCleanup {
                cause: Box::new(ProviderError::Observation(failed.failure())),
                cleanup: Box::new(error.as_ref().clone()),
            }),
            Collection::Idle | Collection::Running(_) => {
                unreachable!("collection is complete after joining its retained worker")
            }
        }
    }

    /// Compare identity without allowing old callbacks to target a new transport.
    pub(crate) fn same(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}

fn native_failure(
    operation: forge_provider_api::error::ProviderOperation,
    kind: forge_provider_api::error::ProviderFailureKind,
) -> ProviderError {
    ProviderError::Failure(forge_provider_api::error::ProviderFailure::new(
        operation, kind,
    ))
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A cancelled recovery waiter leaves the same SDK collection worker owned for a later join.
    #[tokio::test]
    async fn cancelled_collection_waiter_retains_worker_and_first_failure() {
        let (reader, _read_peer) = tokio::io::duplex(4096);
        let (writer, _write_peer) = tokio::io::duplex(4096);
        let generation = ClientGeneration::new(
            Client::from_streams(reader, writer, std::env::temp_dir()).unwrap(),
        );
        generation.fail(ObservationFailure::Disconnected);
        generation.fail(ObservationFailure::Overflow);
        let failed = generation.failed().unwrap();
        let (release, released) = tokio::sync::oneshot::channel();
        *generation.0.collection.lock().await = Collection::Running(tokio::spawn(async move {
            released.await.map_err(|_| {
                Arc::new(native_failure(
                    forge_provider_api::error::ProviderOperation::CollectNativeRuntime,
                    forge_provider_api::error::ProviderFailureKind::Runtime,
                ))
            })
        }));
        assert!(
            tokio::time::timeout(Duration::from_millis(1), generation.collect(&failed))
                .await
                .is_err()
        );
        assert!(matches!(
            *generation.0.collection.lock().await,
            Collection::Running(_)
        ));
        release.send(()).unwrap();
        generation.collect(&failed).await.unwrap();
        generation.collect(&failed).await.unwrap();
        assert!(matches!(
            *generation.0.collection.lock().await,
            Collection::Complete(Ok(()))
        ));
        assert_eq!(failed.failure(), ObservationFailure::Disconnected);
    }

    /// Failure rejects input immediately, while recovery waits until the SDK router is fenced.
    #[tokio::test]
    async fn recovery_token_waits_for_force_stop_before_sdk_collection() {
        use tokio::io::{AsyncReadExt, AsyncWriteExt};
        let runtime = super::super::runtime::CopilotRuntime::new(Vec::new());
        let (reader, mut peer_write) = tokio::io::duplex(4096);
        let (writer, mut peer_read) = tokio::io::duplex(4096);
        runtime
            .attach_streams(reader, writer, std::env::temp_dir())
            .await
            .unwrap();
        let peer = tokio::spawn(async move {
            let mut header = Vec::new();
            while !header.ends_with(b"\r\n\r\n") {
                header.push(peer_read.read_u8().await.unwrap());
            }
            let bytes: usize = std::str::from_utf8(&header)
                .unwrap()
                .trim()
                .strip_prefix("Content-Length: ")
                .unwrap()
                .parse()
                .unwrap();
            let mut body = vec![0; bytes];
            peer_read.read_exact(&mut body).await.unwrap();
            let request: serde_json::Value = serde_json::from_slice(&body).unwrap();
            assert_eq!(request["method"], "status.get");
            let response = serde_json::to_vec(&serde_json::json!({"jsonrpc":"2.0","id":request["id"],"result":{"version":"1.0.103"}})).unwrap();
            peer_write
                .write_all(format!("Content-Length: {}\r\n\r\n", response.len()).as_bytes())
                .await
                .unwrap();
            peer_write.write_all(&response).await.unwrap();
            let mut byte = [0];
            assert_eq!(
                peer_read.read(&mut byte).await.unwrap(),
                0,
                "recovery issued an RPC before the failed transport was fenced"
            );
        });
        let generation = runtime.client(".").await.unwrap().generation;
        let failures = generation.failures();
        let (entered, fencing) = tokio::sync::oneshot::channel();
        let (release, released) = std::sync::mpsc::sync_channel(0);
        let failing = generation.clone();
        let worker = tokio::task::spawn_blocking(move || {
            failing.fail_after_fencing(ObservationFailure::Overflow, || {
                entered.send(()).unwrap();
                released.recv().unwrap();
                failing.0.client.force_stop();
            });
        });
        fencing.await.unwrap();
        assert!(matches!(
            generation.ensure_healthy(),
            Err(ProviderError::Observation(ObservationFailure::Overflow))
        ));
        assert!(
            generation.failed().is_none(),
            "recovery raced a force-stop operation still clearing the SDK router"
        );
        assert!(
            failures.borrow().is_none(),
            "the observer started cleanup before transport fencing completed"
        );
        let mut lookup = Box::pin(runtime.failed_generation());
        assert!(
            futures_util::poll!(lookup.as_mut()).is_pending(),
            "a known failure was mistaken for a healthy client during its physical fence"
        );
        release.send(()).unwrap();
        worker.await.unwrap();
        assert_eq!(*failures.borrow(), Some(ObservationFailure::Overflow));
        let failed = lookup
            .await
            .expect("completed force-stop must release recovery");
        runtime.collect_failed_client(&failed).await.unwrap();
        runtime.shutdown().await.unwrap();
        peer.await.unwrap();
    }
}
