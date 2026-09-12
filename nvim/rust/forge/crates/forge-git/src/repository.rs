use std::{
    sync::{
        Arc, Mutex,
        atomic::{AtomicU64, Ordering},
    },
    time::{Duration, Instant},
};

use anyhow::{Context, Result, ensure};

use crate::{
    RepositoryIdentity,
    completion::RevisionState,
    snapshot::{ObservationId, ObservationTiming, RepositoryObservation, collect_observation},
    store::RepositoryStore,
};

const OBSERVATION_ATTEMPTS: usize = 3;

enum SnapshotKind {
    Display,
    Comparison,
}

/// Identifies concurrent repository changes that require collecting fresh read inputs.
pub fn transient_observation_failure(failure: &anyhow::Error) -> bool {
    failure.chain().any(|cause| {
        matches!(
            cause.to_string().as_str(),
            "status backend changed during observation"
                | "status changed during observation"
                | "worktree metadata changed during observation"
                | "rename origin changed during observation"
                | "index changed during observation"
                | "HEAD changed during observation"
                | "snapshot was invalidated"
                | "repository read was superseded by invalidation"
                | "content acquisition was invalidated"
                | "observation was invalidated"
                | "configuration changed during canonical conversion"
                | "index changed during canonical conversion"
                | "attributes changed during canonical conversion"
                | "worktree changed before content acquisition"
                | "worktree changed during content acquisition"
                | "worktree path changed during content acquisition"
                | "worktree changed during conversion"
        )
    })
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct RepositoryGeneration(u64);

/// Owns one shared Git handle and the generation used to reject superseded read results.
pub struct RepositoryState {
    pub identity: RepositoryIdentity,
    pub(crate) repository: gix::ThreadSafeRepository,
    generation: AtomicU64,
    observation: Mutex<ObservationState>,
    observation_collection: tokio::sync::Mutex<()>,
    pub(crate) revision: Mutex<RevisionState>,
    pub(crate) line_stats: crate::snapshot::LineStatsCache,
}

#[derive(Default)]
struct ObservationState {
    requested: u64,
    current: Option<Arc<RepositoryObservation>>,
}

#[derive(Clone, Copy)]
struct ObservationRequest {
    id: ObservationId,
    generation: RepositoryGeneration,
}

#[derive(Clone, Debug)]
pub struct RepositorySnapshot {
    pub generation: RepositoryGeneration,
    pub worktree: crate::WorktreeId,
    pub head: crate::snapshot::HeadState,
    pub index: crate::snapshot::IndexStamp,
    pub backend: crate::reader::StatusBackend,
    pub path: Vec<crate::snapshot::ObservedPath>,
    pub timing: ObservationTiming,
}

impl RepositoryState {
    /// Returns the repository's object format without reopening configuration or reading files.
    pub fn object_hash(&self) -> gix::hash::Kind {
        self.repository.to_thread_local().object_hash()
    }
    pub(crate) fn new(identity: RepositoryIdentity, repository: gix::ThreadSafeRepository) -> Self {
        Self {
            identity,
            repository,
            generation: AtomicU64::new(0),
            observation: Mutex::new(ObservationState::default()),
            observation_collection: tokio::sync::Mutex::new(()),
            revision: Mutex::new(RevisionState::default()),
            line_stats: crate::snapshot::LineStatsCache::default(),
        }
    }

    pub fn generation(&self) -> RepositoryGeneration {
        RepositoryGeneration(self.generation.load(Ordering::Acquire))
    }

    pub fn invalidate(&self) -> Result<RepositoryGeneration> {
        let mut observation = self
            .observation
            .lock()
            .expect("repository observation lock");
        let mut revision = self.revision.lock().expect("repository revision lock");
        let previous = self
            .generation
            .fetch_update(Ordering::AcqRel, Ordering::Acquire, |generation| {
                generation.checked_add(1)
            })
            .ok()
            .context("repository generation exhausted")?;
        let previous_observation = observation.current.take();
        let previous_revision = revision.current.take();
        self.line_stats.invalidate();
        drop(revision);
        drop(observation);
        drop(previous_observation);
        drop(previous_revision);
        Ok(RepositoryGeneration(previous + 1))
    }

    /// Collects and adopts a full status observation without holding the state lock during I/O.
    ///
    /// Retries detected HEAD, index, status, and changed-path metadata changes up to three times.
    /// Concurrent callers collect in sequence so independent documents cannot supersede each other.
    /// Invalidation prevents adoption of an older result. Native checks use a
    /// 30-second cooperative deadline. Filesystem waits can outlive that deadline. Accepted metadata
    /// is not an exact content precondition for a later mutation.
    pub async fn observe(
        self: &Arc<Self>,
        store: &RepositoryStore,
    ) -> Result<Arc<RepositoryObservation>> {
        let (observation, _) = self.observe_with_timing(store).await?;
        Ok(observation)
    }

    /// Collects and adopts an observation while reporting queue, retry, Git, and filesystem costs.
    pub async fn observe_with_timing(
        self: &Arc<Self>,
        store: &RepositoryStore,
    ) -> Result<(Arc<RepositoryObservation>, ObservationTiming)> {
        let started = Instant::now();
        let _collection = self.observation_collection.lock().await;
        let queue_wait_ms = started.elapsed().as_millis();
        let mut transient = None;
        let mut retry_ms = 0;
        for attempt in 0..OBSERVATION_ATTEMPTS {
            let attempt_started = Instant::now();
            let result = async {
                let request = self.begin_observation()?;
                let snapshot = self.snapshot(store).await?;
                ensure!(
                    snapshot.generation == request.generation,
                    "observation was invalidated"
                );
                let mut timing = snapshot.timing;
                let observation = self.adopt(RepositoryRead {
                    generation: snapshot.generation,
                    value: RepositoryObservation {
                        id: request.id,
                        generation: snapshot.generation,
                        worktree: snapshot.worktree,
                        head: snapshot.head,
                        index: snapshot.index,
                        backend: snapshot.backend,
                        path: snapshot.path,
                    },
                })?;
                timing.queue_wait_ms = queue_wait_ms;
                timing.attempt_count = attempt + 1;
                timing.retry_ms = retry_ms;
                timing.retry_reason = transient.as_ref().map(|failure: &anyhow::Error| {
                    failure
                        .chain()
                        .map(ToString::to_string)
                        .collect::<Vec<_>>()
                        .join(": ")
                });
                timing.overall_ms = started.elapsed().as_millis();
                Ok((observation, timing))
            }
            .await;
            match result {
                Ok(observation) => return Ok(observation),
                Err(failure)
                    if attempt + 1 < OBSERVATION_ATTEMPTS
                        && transient_observation_failure(&failure) =>
                {
                    retry_ms += attempt_started.elapsed().as_millis();
                    transient = Some(failure);
                    tokio::task::yield_now().await;
                }
                Err(failure) => return Err(failure),
            }
        }
        Err(transient
            .expect("transient observation failure recorded")
            .context("repository observation did not stabilize after three attempts"))
    }

    pub async fn snapshot(self: &Arc<Self>, store: &RepositoryStore) -> Result<RepositorySnapshot> {
        self.snapshot_for(store, SnapshotKind::Display).await
    }

    /// Collects a detached per-path comparison without rename coalescing or line statistics.
    ///
    /// Preserves the current display observation and rejects concurrent invalidation or
    /// collection beyond the shared 30-second snapshot deadline.
    pub async fn comparison_snapshot(
        self: &Arc<Self>,
        store: &RepositoryStore,
    ) -> Result<RepositorySnapshot> {
        self.snapshot_for(store, SnapshotKind::Comparison).await
    }

    async fn snapshot_for(
        self: &Arc<Self>,
        store: &RepositoryStore,
        kind: SnapshotKind,
    ) -> Result<RepositorySnapshot> {
        ensure!(
            self.identity.worktree.is_some(),
            "snapshot requires a worktree"
        );
        let worker = Arc::clone(self);
        let analysis = Arc::clone(&store.analysis);
        let skip_line_stats = store.diagnostic_skip_line_stats.load(Ordering::Relaxed);
        let generation = self.generation();
        let result = store
            .read(
                Arc::clone(self),
                size_of::<Arc<Self>>(),
                move |local, cancellation| {
                    let started = Instant::now();
                    let mut check = || {
                        cancellation.check()?;
                        ensure!(
                            worker.generation() == generation,
                            "snapshot was invalidated"
                        );
                        ensure!(
                            started.elapsed() < Duration::from_secs(30),
                            "snapshot exceeded 30-second deadline"
                        );
                        Ok(())
                    };
                    let mut collected = match kind {
                        SnapshotKind::Display => collect_observation(
                            &local,
                            &worker.identity,
                            &analysis,
                            &worker.line_stats,
                            skip_line_stats,
                            &mut check,
                        )?,
                        SnapshotKind::Comparison => crate::snapshot::collect_comparison(
                            &local,
                            &worker.identity,
                            &mut check,
                        )?,
                    };
                    for path in &mut collected.path {
                        for prepared in path.analysis.iter_mut().flatten() {
                            prepared.generation = Some(generation);
                        }
                    }
                    Ok(RepositorySnapshot {
                        generation,
                        worktree: worker
                            .identity
                            .worktree
                            .clone()
                            .context("snapshot lost worktree identity")?,
                        head: collected.head,
                        index: collected.index,
                        backend: collected.backend,
                        path: collected.path,
                        timing: collected.timing,
                    })
                },
            )
            .await?;
        Ok(result.value)
    }

    pub fn current_observation(&self) -> Option<Arc<RepositoryObservation>> {
        self.observation
            .lock()
            .expect("repository observation lock")
            .current
            .clone()
    }

    pub fn adopt(
        &self,
        result: RepositoryRead<RepositoryObservation>,
    ) -> Result<Arc<RepositoryObservation>> {
        let mut state = self
            .observation
            .lock()
            .expect("repository observation lock");
        ensure!(
            result.generation == result.value.generation && result.generation == self.generation(),
            "observation generation was superseded"
        );
        ensure!(
            Some(&result.value.worktree) == self.identity.worktree.as_ref(),
            "observation belongs to another worktree"
        );
        ensure!(
            result.value.id.0 == state.requested,
            "observation request was superseded"
        );
        ensure!(
            state
                .current
                .as_ref()
                .is_none_or(|current| current.id < result.value.id),
            "observation was already adopted"
        );
        let observation = Arc::new(result.value);
        let previous = state.current.replace(Arc::clone(&observation));
        drop(state);
        drop(previous);
        Ok(observation)
    }

    fn begin_observation(&self) -> Result<ObservationRequest> {
        let mut state = self
            .observation
            .lock()
            .expect("repository observation lock");
        state.requested = state
            .requested
            .checked_add(1)
            .context("observation sequence exhausted")?;
        Ok(ObservationRequest {
            id: ObservationId(state.requested),
            generation: self.generation(),
        })
    }
}

/// Tags a read with its repository generation for the feature owner's final adoption check.
pub struct RepositoryRead<Output> {
    pub generation: RepositoryGeneration,
    pub value: Output,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        reader::StatusBackend,
        snapshot::{HeadState, IndexStamp},
        test_support::git,
    };

    #[test]
    fn observation_retry_classifies_only_concurrent_repository_changes() {
        for message in [
            "status backend changed during observation",
            "status changed during observation",
            "worktree metadata changed during observation",
            "rename origin changed during observation",
            "index changed during observation",
            "HEAD changed during observation",
            "snapshot was invalidated",
            "repository read was superseded by invalidation",
            "content acquisition was invalidated",
            "observation was invalidated",
        ] {
            assert!(transient_observation_failure(&anyhow::anyhow!(message)));
        }
        assert!(!transient_observation_failure(&anyhow::anyhow!(
            "invalid index checksum"
        )));
    }

    #[test]
    fn newer_request_supersedes_older_completion_and_duplicate_adoption() {
        let fixture = tempfile::tempdir().unwrap();
        git(fixture.path(), &["init", "--quiet"]);
        let (identity, shared) = crate::identity::discover_repository(fixture.path())
            .unwrap()
            .unwrap();
        let repository = RepositoryState::new(identity, shared);
        let older = repository.begin_observation().unwrap();
        let newer = repository.begin_observation().unwrap();
        let error = repository.adopt(result(&repository, older)).unwrap_err();
        assert!(error.to_string().contains("request was superseded"));
        assert!(repository.current_observation().is_none());
        let current = repository.adopt(result(&repository, newer)).unwrap();
        assert_eq!(current.id, newer.id);
        assert!(
            repository
                .adopt(result(&repository, newer))
                .unwrap_err()
                .to_string()
                .contains("already adopted")
        );
        assert!(Arc::ptr_eq(
            &current,
            &repository.current_observation().unwrap()
        ));
    }

    #[test]
    fn invalidation_rejects_pending_results_and_clears_current_observation() {
        let fixture = tempfile::tempdir().unwrap();
        git(fixture.path(), &["init", "--quiet"]);
        let (identity, shared) = crate::identity::discover_repository(fixture.path())
            .unwrap()
            .unwrap();
        let repository = RepositoryState::new(identity, shared);
        let current = repository.begin_observation().unwrap();
        repository.adopt(result(&repository, current)).unwrap();
        let pending = repository.begin_observation().unwrap();
        repository.invalidate().unwrap();
        assert!(repository.current_observation().is_none());
        assert!(
            repository
                .adopt(result(&repository, pending))
                .unwrap_err()
                .to_string()
                .contains("generation was superseded")
        );
    }

    #[test]
    fn foreign_worktree_and_mismatched_read_generation_are_rejected() {
        let fixture = tempfile::tempdir().unwrap();
        git(fixture.path(), &["init", "--quiet"]);
        let (identity, shared) = crate::identity::discover_repository(fixture.path())
            .unwrap()
            .unwrap();
        let repository = RepositoryState::new(identity, shared);
        let request = repository.begin_observation().unwrap();
        let foreign = tempfile::tempdir().unwrap();
        git(foreign.path(), &["init", "--quiet"]);
        let foreign_identity = crate::discover_identity(foreign.path()).unwrap().unwrap();
        let mut incoming = result(&repository, request);
        incoming.value.worktree = foreign_identity.worktree.unwrap();
        assert!(
            repository
                .adopt(incoming)
                .unwrap_err()
                .to_string()
                .contains("another worktree")
        );
        let mut incoming = result(&repository, request);
        incoming.generation = RepositoryGeneration(request.generation.0 + 1);
        assert!(
            repository
                .adopt(incoming)
                .unwrap_err()
                .to_string()
                .contains("generation was superseded")
        );
        assert!(repository.current_observation().is_none());
    }

    #[tokio::test]
    async fn detached_snapshot_does_not_supersede_pending_observation() {
        let fixture = tempfile::tempdir().unwrap();
        git(fixture.path(), &["init", "--quiet"]);
        let store = RepositoryStore::default();
        let repository = store
            .open(fixture.path().to_owned())
            .await
            .unwrap()
            .unwrap();
        let request = repository.begin_observation().unwrap();
        let snapshot = repository.snapshot(&store).await.unwrap();
        assert_eq!(snapshot.generation, request.generation);
        assert!(repository.current_observation().is_none());
        assert_eq!(
            repository.observation.lock().unwrap().requested,
            request.id.0
        );
        repository.adopt(result(&repository, request)).unwrap();
        let current = repository.current_observation().unwrap();
        repository.snapshot(&store).await.unwrap();
        assert!(Arc::ptr_eq(
            &current,
            &repository.current_observation().unwrap()
        ));
    }

    fn result(
        repository: &RepositoryState,
        request: ObservationRequest,
    ) -> RepositoryRead<RepositoryObservation> {
        RepositoryRead {
            generation: request.generation,
            value: RepositoryObservation {
                id: request.id,
                generation: request.generation,
                worktree: repository.identity.worktree.clone().unwrap(),
                head: HeadState::Unborn {
                    reference: b"refs/heads/main".to_vec(),
                },
                index: IndexStamp {
                    index: None,
                    shared: Vec::new(),
                },
                backend: StatusBackend::Gix,
                path: Vec::new(),
            },
        }
    }
}
