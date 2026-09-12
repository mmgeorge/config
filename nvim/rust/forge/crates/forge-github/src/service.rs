use std::collections::HashMap;
use std::future::{Future, poll_fn};
use std::panic::{AssertUnwindSafe, catch_unwind};
use std::path::PathBuf;
use std::sync::atomic::Ordering;
use std::sync::{Arc, Mutex};
use std::time::{Duration, SystemTime, UNIX_EPOCH};

use anyhow::{Context, Result, anyhow, bail, ensure};
use serde::{Deserialize, Serialize};
use tokio::sync::oneshot;
use tokio::task::JoinSet;

use crate::issue_store::IssueStore;
use crate::lease::RepositoryLease;
use crate::metadata::{MetadataRequest, MetadataStore, RepositoryMetadata};
use crate::model::{
    DetailOutput, DetailRecord, DetailsOutput, IssueDetail, PageInput, RepoSyncState,
    SnapshotState, SyncScope, UpsertOutput,
};
use crate::pull_request::{PullRequestOperation, PullRequestRequest, PullRequestResult};
use crate::queue::{
    MAX_REMOTE_OPERATIONS, RemoteIntent, RemoteOperation, RemoteQueue, RemoteResource, RemoteScope,
};
use crate::remote::{GithubRemote, IssueDetailRequest};
use crate::sync::{SyncOutcome, SyncProgress, SyncRequest};

/// Error evidence that the current operation never crossed its remote-write boundary.
/// Earlier uncertain operations on the same resource remain unresolved.
#[derive(Debug)]
pub struct MutationNotStarted {
    message: String,
}

impl std::fmt::Display for MutationNotStarted {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str(&self.message)
    }
}

impl std::error::Error for MutationNotStarted {}

/// Classifies a failure only when the caller proves no remote mutation was dispatched.
pub fn not_started(failure: anyhow::Error) -> anyhow::Error {
    let message = failure.to_string();
    failure.context(MutationNotStarted { message })
}

const MAX_STORAGE_JOBS: usize = 8;
const MAX_REMOTE_JOBS: usize = 2;

/// Issue storage operation with owned inputs retained for the blocking worker's lifetime.
#[derive(Debug, Deserialize)]
#[serde(tag = "operation", rename_all = "snake_case", deny_unknown_fields)]
pub enum IssueOperation {
    /// Commits one issue page and its synchronization state.
    UpsertPage { scope: SyncScope, page: PageInput },
    /// Reads repository synchronization state.
    State {},
    /// Removes current-format repository data under exclusive operation ownership.
    DeleteCache {},
    /// Reads one issue detail, including explicit cache misses.
    Detail { number: u64 },
    /// Reads issue details in request order.
    Details { number: Vec<u64> },
    /// Commits a remote issue detail.
    UpsertDetail { number: u64, detail: DetailRecord },
    /// Atomically publishes completion metadata to a local file.
    PublishSnapshot {
        state: SnapshotState,
        output: PathBuf,
    },
    /// Reconciles the completion file against the committed issue-state revision.
    ReconcileSnapshot {
        state: SnapshotState,
        output: PathBuf,
    },
}

/// Typed storage result serialized using the existing command payload shape.
#[derive(Debug, Serialize)]
#[serde(untagged)]
pub enum IssueResult {
    /// Issue state and whether its completion file was repaired before returning.
    Reconciled(Box<crate::model::SnapshotRecovery>),
    /// Committed page count and synchronization state.
    UpsertPage(Box<UpsertOutput>),
    /// Persisted repository synchronization state.
    State(Box<RepoSyncState>),
    /// Cached detail or explicit miss.
    Detail(DetailOutput),
    /// Ordered detail lookup results.
    Details(DetailsOutput),
    /// Normalized committed detail.
    UpsertDetail(DetailRecord),
    /// Number of completion records published to disk.
    Published { issue_count: usize },
    /// Whether a repository directory existed and was removed.
    Deleted { deleted: bool },
}

/// Retains issue storage work independently of callers and closes admission during shutdown.
///
/// Construction opens no files and needs no executor. At most eight admitted blocking storage jobs
/// and two async remote read jobs exist, plus 64 queued or running remote mutation jobs. Bounds
/// include completed-but-uncollected jobs. Dropping a
/// request receiver does not cancel its admitted work. Remote implementations retain native process
/// ownership independently of the read futures supplied to the sync lifecycle.
#[derive(Clone, Default)]
pub struct GithubService {
    storage: Arc<StorageTaskStore>,
    mutation: RemoteQueue,
    recovery: Arc<std::sync::OnceLock<crate::recovery::RecoveryStore>>,
}

#[derive(Default)]
struct StorageTaskStore {
    state: Mutex<StorageTaskState>,
    drainage: tokio::sync::Mutex<()>,
    closed: tokio::sync::Notify,
}

#[derive(Default)]
struct StorageTaskState {
    closed: bool,
    task: JoinSet<()>,
    remote_task: JoinSet<()>,
    mutation_task: JoinSet<()>,
    failed_jobs: usize,
    repository: HashMap<PathBuf, RepositoryActivity>,
}

enum RepositoryActivity {
    Operation(usize),
    Deletion,
}

struct RepositoryAdmission {
    storage: Arc<StorageTaskStore>,
    directory: PathBuf,
}

impl Drop for RepositoryAdmission {
    fn drop(&mut self) {
        let mut state = self
            .storage
            .state
            .lock()
            .expect("GitHub storage state poisoned");
        match state.repository.get_mut(&self.directory) {
            Some(RepositoryActivity::Operation(count)) if *count > 1 => *count -= 1,
            _ => {
                state.repository.remove(&self.directory);
            }
        }
    }
}

/// Native storage ownership remaining after a shutdown attempt.
#[derive(Debug, PartialEq, Eq)]
pub struct StorageShutdown {
    /// Admitted jobs whose completion has not been collected.
    pub unfinished_jobs: usize,
    /// Task join failures observed since service construction.
    pub failed_jobs: usize,
}

impl GithubService {
    pub fn recovery_configured(&self) -> bool {
        self.recovery.get().is_some()
    }

    /// Fixes the durable recovery root for this service lifetime without filesystem IO.
    pub fn configure_recovery(&self, directory: PathBuf) -> Result<()> {
        let configured = crate::recovery::RecoveryStore::new(directory)?;
        if let Some(current) = self.recovery.get() {
            ensure!(
                current.directory() == configured.directory(),
                "GitHub recovery directory cannot change during a service lifetime"
            );
            return Ok(());
        }
        self.recovery
            .set(configured)
            .map_err(|_| anyhow!("GitHub recovery directory was configured concurrently"))
    }

    pub(crate) fn recovery_store(&self) -> Result<crate::recovery::RecoveryStore> {
        self.recovery
            .get()
            .cloned()
            .context("GitHub writes require a configured durable recovery directory")
    }

    /// Reads durable uncertainty through the existing bounded storage owner without issuing a write.
    pub async fn recovery_inspect(
        &self,
        resource: crate::recovery::RecoveryResource,
    ) -> Result<Option<crate::recovery::RecoveryRecord>> {
        let store = self.recovery_store()?;
        self.submit(move || store.inspect(&resource))?
            .await
            .context("GitHub recovery read ended without collection")?
    }

    /// Retires a terminal operation only after its matching durable draft acknowledgement.
    pub async fn recovery_acknowledge(
        &self,
        resource: crate::recovery::RecoveryResource,
        operation_id: String,
    ) -> Result<()> {
        let store = self.recovery_store()?;
        self.submit(move || {
            let journal = store.resume(&resource, &operation_id)?;
            ensure!(
                journal.record().capture.edit_sequence.is_none(),
                "editable mutations require durable draft settlement before acknowledgement"
            );
            journal.acknowledge(&operation_id)
        })?
        .await
        .context("GitHub recovery acknowledgement ended without collection")?
    }

    pub async fn recovery_settle_draft(
        &self,
        resource: crate::recovery::RecoveryResource,
        operation_id: String,
        draft: serde_json::Value,
    ) -> Result<()> {
        let store = self.recovery_store()?;
        crate::draft::validate_patch(&resource, &draft)?;
        let publication = crate::draft::DraftStore::new(&store, &resource)?;
        self.submit(move || {
            let journal = store.resume(&resource, &operation_id)?;
            ensure!(
                matches!(
                    journal.record().state,
                    crate::recovery::RecoveryPhase::Confirmed { .. }
                        | crate::recovery::RecoveryPhase::Rejected { .. }
                        | crate::recovery::RecoveryPhase::UserLinked { .. }
                        | crate::recovery::RecoveryPhase::UserClosedUnknown { .. }
                ),
                "uncertain operation cannot settle a draft"
            );
            let mut draft = draft;
            if journal.record().resource.kind == crate::recovery::RecoveryResourceKind::Repository {
                draft["creation_receipt"] = serde_json::to_value(journal.record())?;
            }
            if matches!(
                journal.record().state,
                crate::recovery::RecoveryPhase::UserClosedUnknown { .. }
            ) {
                draft["retired_operation"] =
                    serde_json::json!({operation_id.clone(): journal.record()});
            }
            publication.merge(draft)?;
            journal.acknowledge(&operation_id)
        })?
        .await
        .context("durable draft settlement ended without collection")?
    }

    pub(crate) async fn recovery_prepare(
        &self,
        resource: crate::recovery::RecoveryResource,
        capture: crate::recovery::RecoveryCapture,
    ) -> Result<crate::recovery::RecoveryLease> {
        let store = self.recovery_store()?;
        self.submit(move || store.prepare(resource, capture))?
            .await
            .context("GitHub recovery preparation ended without collection")?
    }

    /// Retains one queued PR operation through remote completion, independently of its caller.
    ///
    /// At most 64 mutation jobs and 16 MiB of request input are admitted. Same-resource jobs execute in FIFO order, including
    /// reopen-then-draft or ready steps. Closing rejects queued work and drains running requests
    /// without dropping a possibly active native write. Unknown results require explicit reconcile.
    pub async fn pull_request(
        &self,
        remote: Arc<dyn GithubRemote>,
        request: PullRequestRequest,
    ) -> Result<PullRequestResult> {
        request.target.validate().map_err(not_started)?;
        let retained_bytes = request.target.node_id.capacity()
            + 1024
            + match &request.request {
                PullRequestOperation::Edit { edit }
                | PullRequestOperation::ReconcileEdit { edit } => {
                    edit.validate().map_err(not_started)?;
                    edit.retained_bytes()
                }
                _ => 0,
            };
        let resource =
            RemoteResource::new(request.target.repository.clone(), request.target.number)
                .map_err(not_started)?;
        let intent = match request.request {
            PullRequestOperation::Transition { .. } | PullRequestOperation::Edit { .. } => {
                RemoteIntent::Mutation
            }
            PullRequestOperation::Reconcile | PullRequestOperation::ReconcileEdit { .. } => {
                RemoteIntent::Reconcile
            }
        };
        self.run_mutation(
            resource,
            RemoteScope::PullRequest,
            intent,
            retained_bytes,
            move |operation| crate::pull_request::run_pull_request(remote, request, operation),
        )
        .await
    }

    /// Retains queued and active remote ownership independently of the response receiver.
    pub(crate) async fn run_mutation<Output, Run, Work>(
        &self,
        resource: RemoteResource,
        scope: RemoteScope,
        intent: RemoteIntent,
        retained_bytes: usize,
        run: Run,
    ) -> Result<Output>
    where
        Output: Send + 'static,
        Run: FnOnce(RemoteOperation) -> Work + Send + 'static,
        Work: Future<Output = Result<Output>> + Send + 'static,
    {
        let mut mutation_started = None;
        let result = async {
            tokio::runtime::Handle::try_current()
                .context("GitHub mutation requires a Tokio executor")?;
            let operation =
                self.mutation
                    .enqueue_scoped(resource, scope, intent, retained_bytes)?;
            mutation_started = Some(Arc::clone(&operation.mutation_started));
            let (sender, receiver) = oneshot::channel();
            {
                let mut state = self
                    .storage
                    .state
                    .lock()
                    .expect("GitHub storage state poisoned");
                ensure!(!state.closed, "GitHub mutation admission is closed");
                while let Some(outcome) = state.mutation_task.try_join_next() {
                    if outcome.is_err() {
                        state.failed_jobs = state.failed_jobs.saturating_add(1);
                    }
                }
                ensure!(
                    state.mutation_task.len() < MAX_REMOTE_OPERATIONS,
                    "GitHub mutation task admission is full"
                );
                let service = self.clone();
                state.mutation_task.spawn(async move {
                    let outcome = async {
                        let operation = tokio::select! {
                            biased;
                            _ = service.closed() => bail!("GitHub mutation admission is closed"),
                            ready = operation.ready() => ready?,
                        };
                        run(operation).await
                    }
                    .await;
                    let _ = sender.send(outcome);
                });
            }
            receiver
                .await
                .context("GitHub mutation ended without a result")?
        }
        .await;
        result.map_err(|failure| {
            if mutation_started
                .as_ref()
                .is_none_or(|started| !started.load(Ordering::Acquire))
            {
                not_started(failure)
            } else {
                failure
            }
        })
    }

    /// Reuses fresh metadata or retains remote refresh ownership through atomic publication.
    ///
    /// Dropping the caller does not cancel admitted work. Shutdown interrupts remote waits and
    /// drains any blocking publication before releasing repository ownership.
    pub async fn metadata(
        &self,
        remote: Arc<dyn GithubRemote>,
        directory: PathBuf,
        request: MetadataRequest,
    ) -> Result<RepositoryMetadata> {
        let store = MetadataStore::new(directory, request.repository.clone())?;
        self.run_remote(store.directory().to_owned(), "metadata", move |service| async move {
            let preparation = store.clone();
            let prepared = service.submit(move || {
                let lease = RepositoryLease::metadata(preparation.directory())?;
                Ok((lease, preparation.read()?))
            })?;
            let (lease, cached) = prepared.await.context("GitHub metadata preparation ended without a result")??;
            let now = SystemTime::now().duration_since(UNIX_EPOCH)?.as_secs();
            if let Some(cached) = cached.filter(|cached| cached.fetched_at <= now && now - cached.fetched_at < request.ttl_seconds) {
                return Ok(cached);
            }
            let users = tokio::select! {
                biased;
                _ = service.closed() => bail!("GitHub metadata admission is closed"),
                result = tokio::time::timeout(Duration::from_secs(120), remote.read_repository_users(request.repository.clone())) => result.context("GitHub repository metadata deadline exceeded")??,
            };
            let metadata = RepositoryMetadata {
                repo: request.repository.repository_name(), hostname: Some(request.repository.hostname().into()),
                fetched_at: SystemTime::now().duration_since(UNIX_EPOCH)?.as_secs(),
                contributors: users.contributors, failure: users.failure,
            };
            service.submit(move || {
                let _lease = lease;
                store.publish(&metadata)?;
                Ok(metadata)
            })?.await.context("GitHub metadata publication ended without a result")?
        }).await
    }

    /// Runs an owned issue refresh with at most two concurrent sync jobs.
    ///
    /// Remote waits retain the repository operation and sync leases but no database handle.
    /// Dropping the caller does not abort publication. Closing the service stops remote reads and
    /// retry timers, while admitted blocking storage retains its own ownership until completion.
    pub async fn sync(
        &self,
        remote: Arc<dyn GithubRemote>,
        store: IssueStore,
        request: SyncRequest,
        progress: Option<tokio::sync::watch::Sender<SyncProgress>>,
    ) -> Result<SyncOutcome> {
        store.validate_repository(&request.repository)?;
        let directory = store.repository_directory()?;
        self.run_remote(directory.clone(), "sync", move |service| async move {
            let prepared_store = store.clone();
            let snapshot = request.snapshot.clone();
            let prepared = service.submit(move || {
                let lease = RepositoryLease::sync(&directory)?;
                let recovery = prepared_store.reconcile_snapshot(SnapshotState::Open, &snapshot)?;
                Ok((lease, recovery.state, recovery.ready))
            })?;
            let (_lease, state, exists) = prepared
                .await
                .context("GitHub sync preparation ended without a result")??;
            crate::sync::run(service, remote, store, request, state, exists, progress).await
        })
        .await
    }

    /// Fetches and commits a normalized issue detail while excluding cache deletion.
    ///
    /// Shares the two-job remote admission bound with sync. Remote waits hold a repository lease
    /// but no database handle. Caller cancellation retains the owned job through persistence.
    pub async fn fetch_detail(
        &self,
        remote: Arc<dyn GithubRemote>,
        store: IssueStore,
        request: IssueDetailRequest,
    ) -> Result<DetailRecord> {
        self.fetch_detail_capture(remote, store, request)
            .await
            .map(|(_, record)| record)
    }

    /// Returns the typed remote capture only after its identical detail record commits to the cache.
    /// Shares admission, deletion exclusion, and cancellation ownership with `fetch_detail`.
    pub async fn fetch_issue_document(
        &self,
        remote: Arc<dyn GithubRemote>,
        store: IssueStore,
        request: IssueDetailRequest,
    ) -> Result<IssueDetail> {
        self.fetch_detail_capture(remote, store, request)
            .await
            .map(|(detail, _)| detail)
    }

    async fn fetch_detail_capture(
        &self,
        remote: Arc<dyn GithubRemote>,
        store: IssueStore,
        request: IssueDetailRequest,
    ) -> Result<(IssueDetail, DetailRecord)> {
        store.validate_repository(&request.repository)?;
        ensure!(
            request.number > 0 && request.number <= i32::MAX as u64,
            "invalid GitHub issue number"
        );
        let directory = store.repository_directory()?;
        self.run_remote(directory.clone(), "detail", move |service| async move {
            let lease = service.submit(move || RepositoryLease::operation(&directory))?;
            let _lease = lease.await.context("GitHub detail preparation ended without a result")??;
            let detail = tokio::select! {
                biased;
                _ = service.closed() => bail!("GitHub detail admission is closed"),
                result = tokio::time::timeout(Duration::from_secs(120), remote.read_issue_detail(request.clone())) => result.context("GitHub issue detail deadline exceeded")??,
            };
            ensure!(detail.repo == request.repository.repository_name() && detail.number == request.number,
                "GitHub remote returned a different issue detail");
            let record = DetailRecord {
                repo: detail.repo.clone(), number: detail.number,
                fetched_at: SystemTime::now().duration_since(UNIX_EPOCH)?.as_secs().try_into()?,
                item: serde_json::to_value(&detail)?,
            };
            match service.execute(store, IssueOperation::UpsertDetail { number: request.number, detail: record }).await? {
                IssueResult::UpsertDetail(record) => Ok((detail, record)),
                _ => bail!("GitHub detail persistence returned an invalid result"),
            }
        }).await
    }

    pub(crate) async fn run_remote<Response, Work>(
        &self,
        directory: PathBuf,
        operation: &'static str,
        work: impl FnOnce(GithubService) -> Work + Send + 'static,
    ) -> Result<Response>
    where
        Response: Send + 'static,
        Work: Future<Output = Result<Response>> + Send + 'static,
    {
        tokio::runtime::Handle::try_current()
            .context("GitHub remote work requires a Tokio executor")?;
        let admission = self.admit_repository(directory, false)?;
        let (sender, receiver) = oneshot::channel();
        {
            let mut state = self
                .storage
                .state
                .lock()
                .expect("GitHub storage state poisoned");
            ensure!(!state.closed, "GitHub {operation} admission is closed");
            while let Some(outcome) = state.remote_task.try_join_next() {
                if outcome.is_err() {
                    state.failed_jobs = state.failed_jobs.saturating_add(1);
                }
            }
            ensure!(
                state.remote_task.len() < MAX_REMOTE_JOBS,
                "GitHub {operation} admission is full"
            );
            let service = self.clone();
            state.remote_task.spawn(async move {
                let outcome = work(service).await;
                drop(admission);
                let _ = sender.send(outcome);
            });
        }
        receiver
            .await
            .with_context(|| format!("GitHub {operation} ended without a result"))?
    }

    /// Executes a typed operation on a retained blocking worker.
    ///
    /// Requires a Tokio executor. Pages and detail batches accept at most 100 issues. Closed,
    /// saturated, or oversized admission fails before spawning work. Store
    /// errors and panic payloads reach the caller. Cancellation of this future leaves admitted work
    /// owned until it completes, so callers must reload after an unobserved write result.
    pub async fn execute(
        &self,
        store: IssueStore,
        operation: IssueOperation,
    ) -> Result<IssueResult> {
        match &operation {
            IssueOperation::UpsertPage { page, .. } => ensure!(
                page.issues.len() <= 100,
                "GitHub storage page exceeds 100 issues"
            ),
            IssueOperation::Details { number } => ensure!(
                number.len() <= 100,
                "GitHub storage detail batch exceeds 100 issues"
            ),
            _ => {}
        }
        let directory = store.repository_directory()?;
        let admission = self.admit_repository(
            directory,
            matches!(operation, IssueOperation::DeleteCache {}),
        )?;
        let result = self.submit(move || {
            let _admission = admission;
            match operation {
                IssueOperation::UpsertPage { scope, page } => Ok(IssueResult::UpsertPage(
                    Box::new(store.upsert_page(scope, page)?),
                )),
                IssueOperation::State {} => Ok(IssueResult::State(Box::new(store.read_state()?))),
                IssueOperation::DeleteCache {} => Ok(IssueResult::Deleted {
                    deleted: store.delete_repository_cache()?,
                }),
                IssueOperation::Detail { number } => {
                    Ok(IssueResult::Detail(store.read_detail(number)?))
                }
                IssueOperation::Details { number } => {
                    Ok(IssueResult::Details(store.read_details(&number)?))
                }
                IssueOperation::UpsertDetail { number, detail } => Ok(IssueResult::UpsertDetail(
                    store.upsert_detail(number, detail)?,
                )),
                IssueOperation::PublishSnapshot { state, output } => Ok(IssueResult::Published {
                    issue_count: store.publish_snapshot(state, &output)?,
                }),
                IssueOperation::ReconcileSnapshot { state, output } => Ok(IssueResult::Reconciled(
                    Box::new(store.reconcile_snapshot(state, &output)?),
                )),
            }
        })?;
        result
            .await
            .context("GitHub storage worker ended without a result")?
    }

    fn admit_repository(&self, directory: PathBuf, deletion: bool) -> Result<RepositoryAdmission> {
        let mut state = self
            .storage
            .state
            .lock()
            .expect("GitHub storage state poisoned");
        ensure!(!state.closed, "GitHub storage admission is closed");
        match state.repository.get_mut(&directory) {
            Some(RepositoryActivity::Operation(count)) if !deletion => *count += 1,
            Some(_) => return Err(anyhow!("Busy: repository has admitted issue storage work")),
            None => {
                state.repository.insert(
                    directory.clone(),
                    if deletion {
                        RepositoryActivity::Deletion
                    } else {
                        RepositoryActivity::Operation(1)
                    },
                );
            }
        }
        Ok(RepositoryAdmission {
            storage: Arc::clone(&self.storage),
            directory,
        })
    }

    /// Rejects subsequent storage admission without aborting admitted work.
    pub fn close(&self) {
        self.storage
            .state
            .lock()
            .expect("GitHub storage state poisoned")
            .closed = true;
        self.mutation.close();
        self.storage.closed.notify_waiters();
    }

    pub(crate) async fn closed(&self) {
        let notification = self.storage.closed.notified();
        tokio::pin!(notification);
        notification.as_mut().enable();
        if self
            .storage
            .state
            .lock()
            .expect("GitHub storage state poisoned")
            .closed
        {
            return;
        }
        notification.await;
    }

    /// Closes admission and collects job completion through the supplied deadline.
    ///
    /// A timeout retains queued and running jobs for a later attempt. Concurrent shutdown calls
    /// serialize collection within their own deadlines. Native calls are not forcibly interrupted.
    pub async fn shutdown(&self, deadline: Duration) -> StorageShutdown {
        self.close();
        let _ = tokio::time::timeout(deadline, async {
            let _drainage = self.storage.drainage.lock().await;
            loop {
                let outcome = poll_fn(|context| {
                    let mut state = self
                        .storage
                        .state
                        .lock()
                        .expect("GitHub storage state poisoned");
                    let remote = state.remote_task.poll_join_next(context);
                    let mutation = state.mutation_task.poll_join_next(context);
                    let storage = state.task.poll_join_next(context);
                    let mut completed = false;
                    for outcome in [remote, storage, mutation] {
                        if let std::task::Poll::Ready(Some(outcome)) = outcome {
                            completed = true;
                            if outcome.is_err() {
                                state.failed_jobs = state.failed_jobs.saturating_add(1);
                            }
                        }
                    }
                    if completed {
                        std::task::Poll::Ready(Some(()))
                    } else if state.task.is_empty()
                        && state.remote_task.is_empty()
                        && state.mutation_task.is_empty()
                    {
                        std::task::Poll::Ready(None)
                    } else {
                        std::task::Poll::Pending
                    }
                })
                .await;
                if outcome.is_none() {
                    break;
                }
            }
        })
        .await;
        let state = self
            .storage
            .state
            .lock()
            .expect("GitHub storage state poisoned");
        StorageShutdown {
            unfinished_jobs: state.task.len() + state.remote_task.len() + state.mutation_task.len(),
            failed_jobs: state.failed_jobs,
        }
    }

    pub(crate) fn submit<Output: Send + 'static>(
        &self,
        operation: impl FnOnce() -> Result<Output> + Send + 'static,
    ) -> Result<oneshot::Receiver<Result<Output>>> {
        tokio::runtime::Handle::try_current()
            .context("GitHub storage requires a Tokio executor")?;
        let mut state = self
            .storage
            .state
            .lock()
            .expect("GitHub storage state poisoned");
        ensure!(!state.closed, "GitHub storage admission is closed");
        while let Some(outcome) = state.task.try_join_next() {
            if outcome.is_err() {
                state.failed_jobs = state.failed_jobs.saturating_add(1);
            }
        }
        ensure!(
            state.task.len() < MAX_STORAGE_JOBS,
            "GitHub storage admission is full"
        );
        let (sender, receiver) = oneshot::channel();
        let ownership = Arc::clone(&self.storage);
        state.task.spawn_blocking(move || {
            let _ownership = ownership;
            let result = catch_unwind(AssertUnwindSafe(operation)).unwrap_or_else(|payload| {
                let message = payload
                    .downcast_ref::<String>()
                    .map(String::as_str)
                    .or_else(|| payload.downcast_ref::<&str>().copied())
                    .unwrap_or("unknown panic payload");
                Err(anyhow!("GitHub storage operation panicked: {message}"))
            });
            let _ = sender.send(result);
        });
        Ok(receiver)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn repository_admission_retains_deletion_exclusion_after_receiver_drop() -> Result<()> {
        let root = tempfile::tempdir()?;
        let store = IssueStore::new(
            root.path().join("repo/issues/issues.redb"),
            "owner/repo",
            Duration::ZERO,
        )?;
        let service = GithubService::default();
        let admission = service.admit_repository(store.repository_directory()?, false)?;
        let (release, wait) = std::sync::mpsc::channel();
        let receiver = service.submit(move || {
            let _admission = admission;
            wait.recv_timeout(Duration::from_secs(2))?;
            Ok(())
        })?;
        drop(receiver);
        let failure = service
            .execute(store.clone(), IssueOperation::DeleteCache {})
            .await
            .unwrap_err();
        assert!(failure.to_string().contains("Busy"));
        release.send(())?;
        let deadline = tokio::time::Instant::now() + Duration::from_secs(2);
        loop {
            if service.storage.state.lock().unwrap().repository.is_empty() {
                break;
            }
            ensure!(
                tokio::time::Instant::now() < deadline,
                "repository admission was not released"
            );
            tokio::task::yield_now().await;
        }
        let deletion = service.admit_repository(store.repository_directory()?, true)?;
        assert!(
            service
                .execute(store.clone(), IssueOperation::State {})
                .await
                .unwrap_err()
                .to_string()
                .contains("Busy")
        );
        assert!(
            service
                .execute(store.clone(), IssueOperation::DeleteCache {})
                .await
                .is_err()
        );
        drop(deletion);
        assert!(matches!(
            service
                .execute(store.clone(), IssueOperation::State {})
                .await?,
            IssueResult::State(_)
        ));
        assert!(matches!(
            service
                .execute(store, IssueOperation::DeleteCache {})
                .await?,
            IssueResult::Deleted { deleted: true }
        ));
        service.close();
        assert_eq!(
            service
                .shutdown(Duration::from_secs(2))
                .await
                .unfinished_jobs,
            0
        );
        Ok(())
    }

    #[tokio::test]
    async fn dropping_the_service_retains_storage_ownership_until_work_finishes() {
        let service = GithubService::default();
        let ownership = Arc::downgrade(&service.storage);
        let (release, blocked) = std::sync::mpsc::channel();
        let result = service
            .submit(move || {
                blocked.recv().unwrap();
                Ok(())
            })
            .unwrap();
        drop(service);
        assert!(ownership.upgrade().is_some());
        release.send(()).unwrap();
        result.await.unwrap().unwrap();
        tokio::time::timeout(Duration::from_secs(2), async {
            while ownership.upgrade().is_some() {
                tokio::task::yield_now().await;
            }
        })
        .await
        .unwrap();
    }

    #[tokio::test]
    async fn abandoned_receiver_and_shutdown_timeout_retain_native_work() {
        let service = GithubService::default();
        let (release, blocked) = std::sync::mpsc::channel();
        let result = service
            .submit(move || {
                blocked.recv().unwrap();
                Ok(())
            })
            .unwrap();
        drop(result);
        let report = service.shutdown(Duration::from_millis(10)).await;
        assert_eq!(report.unfinished_jobs, 1);
        assert!(service.submit(|| Ok(())).is_err());
        release.send(()).unwrap();
        assert_eq!(
            service.shutdown(Duration::from_secs(2)).await,
            StorageShutdown {
                unfinished_jobs: 0,
                failed_jobs: 0
            }
        );
    }

    #[tokio::test]
    async fn saturation_rejects_work_before_its_closure_runs() {
        let service = GithubService::default();
        let mut release = Vec::new();
        for _ in 0..MAX_STORAGE_JOBS {
            let (sender, receiver) = std::sync::mpsc::channel();
            release.push(sender);
            service
                .submit(move || {
                    receiver.recv().unwrap();
                    Ok(())
                })
                .unwrap();
        }
        assert!(
            service
                .submit(|| -> Result<()> { panic!("rejected work ran") })
                .unwrap_err()
                .to_string()
                .contains("full")
        );
        for sender in release {
            sender.send(()).unwrap();
        }
        assert_eq!(
            service
                .shutdown(Duration::from_secs(2))
                .await
                .unfinished_jobs,
            0
        );
    }

    #[tokio::test]
    async fn panic_payload_reaches_the_caller_without_losing_task_ownership() {
        let service = GithubService::default();
        let result = service
            .submit(|| -> Result<()> { panic!("storage fixture failure") })
            .unwrap();
        assert!(
            result
                .await
                .unwrap()
                .unwrap_err()
                .to_string()
                .contains("storage fixture failure")
        );
        assert_eq!(
            service.shutdown(Duration::from_secs(2)).await,
            StorageShutdown {
                unfinished_jobs: 0,
                failed_jobs: 0
            }
        );
    }

    #[tokio::test]
    async fn typed_operations_commit_and_reload_real_storage() {
        let directory = tempfile::tempdir().unwrap();
        let store = IssueStore::new(
            directory.path().join("issues.redb"),
            "owner/repo",
            Duration::ZERO,
        )
        .unwrap();
        let service = GithubService::default();
        assert!(
            service
                .execute(
                    store.clone(),
                    IssueOperation::Details {
                        number: vec![7; 101]
                    }
                )
                .await
                .is_err()
        );
        assert!(!directory.path().join("issues.redb").exists());
        service
            .execute(
                store.clone(),
                IssueOperation::UpsertDetail {
                    number: 7,
                    detail: DetailRecord {
                        repo: String::new(),
                        number: 0,
                        fetched_at: 123,
                        item: serde_json::json!({"title":"Stored"}),
                    },
                },
            )
            .await
            .unwrap();
        let IssueResult::Detail(detail) = service
            .execute(store, IssueOperation::Detail { number: 7 })
            .await
            .unwrap()
        else {
            panic!("wrong storage result")
        };
        assert!(detail.found);
        assert_eq!(detail.item.unwrap()["title"], "Stored");
        assert_eq!(
            service
                .shutdown(Duration::from_secs(2))
                .await
                .unfinished_jobs,
            0
        );
    }
}

#[cfg(test)]
mod mutation_tests;
