use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::{Arc, Mutex, atomic::AtomicBool};
use std::time::Duration;

use anyhow::{Context, Result, ensure};

use crate::identity::{GitStorageId, RepositoryIdentity, WorktreeId, discover_repository};
use crate::read_pool::{BlockingReadPool, ReadCancellation, ReadShutdown};
use crate::repository::{RepositoryRead, RepositoryState};

#[derive(Clone, Eq, Hash, PartialEq)]
enum RepositoryKey {
    Worktree(WorktreeId),
    Bare(GitStorageId),
}

struct StoreEntry {
    repository: Arc<RepositoryState>,
    last_use: u64,
}

#[derive(Default)]
struct StoreState {
    entries: HashMap<RepositoryKey, StoreEntry>,
    sequence: u64,
    closed: bool,
}

/// Reuses bounded repository handles. Arc leases keep active clients and workers ineligible for eviction.
pub struct RepositoryStore {
    /// Diagnostic display-only bypass. Mutation settlement always collects normally.
    pub diagnostic_skip_line_stats: AtomicBool,
    pub analysis: Arc<forge_diff::cache::AnalysisStore>,
    pub writes: crate::coordinator::MutationCoordinator,
    /// Shared admission for repository reads and immutable, digest-validated checkpoint objects.
    pub reads: BlockingReadPool,
    state: Mutex<StoreState>,
    max_repositories: usize,
}

impl Default for RepositoryStore {
    fn default() -> Self {
        Self::new(32, 4, 32 * 1024 * 1024).expect("positive repository admission defaults")
    }
}

impl RepositoryStore {
    pub fn new(max_repositories: usize, read_jobs: usize, read_input_bytes: usize) -> Result<Self> {
        ensure!(
            max_repositories > 0,
            "repository handle limit must be positive"
        );
        Ok(Self {
            diagnostic_skip_line_stats: AtomicBool::new(false),
            analysis: Arc::new(forge_diff::cache::AnalysisStore::new(Default::default())),
            writes: crate::coordinator::MutationCoordinator::new(64, 16 * 1024 * 1024)?,
            state: Mutex::new(StoreState::default()),
            reads: BlockingReadPool::new(read_jobs, read_input_bytes)?,
            max_repositories,
        })
    }

    /// Discovers on the bounded blocking pool, then deduplicates the canonical worktree identity.
    pub async fn open(&self, path: PathBuf) -> Result<Option<Arc<RepositoryState>>> {
        let input_bytes = path.as_os_str().as_encoded_bytes().len();
        let discovered = self
            .reads
            .submit(input_bytes, move |cancellation| {
                cancellation.check()?;
                discover_repository(&path)
            })?
            .finish()
            .await?;
        let Some((identity, repository)) = discovered else {
            return Ok(None);
        };
        let key = repository_key(&identity);
        let mut state = self.state.lock().expect("repository store lock");
        ensure!(!state.closed, "repository store is closed");
        state.sequence = state
            .sequence
            .checked_add(1)
            .context("repository access sequence exhausted")?;
        let sequence = state.sequence;
        if let Some(entry) = state.entries.get_mut(&key) {
            entry.last_use = sequence;
            return Ok(Some(Arc::clone(&entry.repository)));
        }
        if state.entries.len() == self.max_repositories {
            let idle = state
                .entries
                .iter()
                .filter(|(_, entry)| Arc::strong_count(&entry.repository) == 1)
                .min_by_key(|(_, entry)| entry.last_use)
                .map(|(key, _)| key.clone());
            let idle = idle.context("repository handle capacity is full with active leases")?;
            state.entries.remove(&idle);
        }
        let repository = Arc::new(RepositoryState::new(identity, repository));
        state.entries.insert(
            key,
            StoreEntry {
                repository: Arc::clone(&repository),
                last_use: sequence,
            },
        );
        Ok(Some(repository))
    }

    /// Creates worker-local gix state while retaining a lease until the blocking operation exits.
    pub async fn read<Output: Send + 'static>(
        &self,
        repository: Arc<RepositoryState>,
        input_bytes: usize,
        work: impl FnOnce(gix::Repository, ReadCancellation) -> Result<Output> + Send + 'static,
    ) -> Result<RepositoryRead<Output>> {
        {
            let mut state = self.state.lock().expect("repository store lock");
            ensure!(!state.closed, "repository store is closed");
            state.sequence = state
                .sequence
                .checked_add(1)
                .context("repository access sequence exhausted")?;
            let sequence = state.sequence;
            let entry = state
                .entries
                .get_mut(&repository_key(&repository.identity))
                .context("repository is not owned by this store")?;
            ensure!(
                Arc::ptr_eq(&entry.repository, &repository),
                "repository lease belongs to another store"
            );
            entry.last_use = sequence;
        }
        let generation = repository.generation();
        let worker_repository = Arc::clone(&repository);
        let value = self
            .reads
            .submit(input_bytes, move |cancellation| {
                let local = worker_repository.repository.to_thread_local();
                let result = work(local, cancellation);
                drop(worker_repository);
                result
            })?
            .finish()
            .await?;
        ensure!(
            generation == repository.generation(),
            "repository read was superseded by invalidation"
        );
        Ok(RepositoryRead { generation, value })
    }

    pub fn invalidate_storage(&self, storage: &GitStorageId) -> Result<usize> {
        let state = self.state.lock().expect("repository store lock");
        let mut invalidated = 0;
        for entry in state
            .entries
            .values()
            .filter(|entry| &entry.repository.identity.storage == storage)
        {
            entry.repository.invalidate()?;
            invalidated += 1;
        }
        Ok(invalidated)
    }

    pub fn evict_idle(&self) -> usize {
        let mut state = self.state.lock().expect("repository store lock");
        let previous = state.entries.len();
        state
            .entries
            .retain(|_, entry| Arc::strong_count(&entry.repository) > 1);
        previous - state.entries.len()
    }

    pub async fn shutdown(&self, deadline: Duration) -> ReadShutdown {
        self.writes.close();
        self.state.lock().expect("repository store lock").closed = true;
        self.reads.shutdown(deadline).await
    }
}

fn repository_key(identity: &RepositoryIdentity) -> RepositoryKey {
    match &identity.worktree {
        Some(worktree) => RepositoryKey::Worktree(worktree.clone()),
        None => RepositoryKey::Bare(identity.storage.clone()),
    }
}

#[cfg(test)]
mod tests {
    use std::future::Future;
    use std::task::Poll;

    use super::*;

    #[test]
    fn uncollected_discovery_uses_shared_read_admission() {
        let runtime = tokio::runtime::Builder::new_current_thread()
            .enable_all()
            .max_blocking_threads(1)
            .build()
            .unwrap();
        runtime.block_on(async {
            let root = tempfile::tempdir().unwrap();
            let output = std::process::Command::new("git")
                .arg("-C")
                .arg(root.path())
                .args(["init", "--quiet"])
                .output()
                .unwrap();
            assert!(output.status.success());
            let store = RepositoryStore::new(1, 1, 4096).unwrap();
            let (release, release_receiver) = std::sync::mpsc::channel();
            let blocker = tokio::task::spawn_blocking(move || {
                release_receiver
                    .recv_timeout(Duration::from_secs(5))
                    .unwrap();
            });
            let mut opening = Box::pin(store.open(root.path().to_owned()));
            std::future::poll_fn(|context| {
                assert!(opening.as_mut().poll(context).is_pending());
                Poll::Ready(())
            })
            .await;
            release.send(()).unwrap();
            blocker.await.unwrap();
            assert_eq!(store.reads.status().active_jobs, 1);
            assert!(
                store
                    .open(root.path().to_owned())
                    .await
                    .err()
                    .unwrap()
                    .to_string()
                    .contains("capacity is full")
            );
            drop(opening);
            tokio::time::timeout(Duration::from_secs(5), async {
                while store.reads.status().active_jobs != 0 {
                    tokio::task::yield_now().await;
                }
            })
            .await
            .unwrap();
            assert_eq!(store.reads.status().reserved_input_bytes, 0);
        });
    }
}
