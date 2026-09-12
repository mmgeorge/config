use crate::{
    edit::{EditBudget, RegionEdit, SaveOutcome},
    issue::{IssueDocument, IssueEditResult, IssueFieldState},
};
use anyhow::{Context, Result, ensure};
use forge_buffer::{
    admission::{DocumentAdmission, DocumentAdmissionStore},
    identity::DocumentId,
    patch::BufferSnapshot,
};
use forge_github::{
    issue_store::IssueStore,
    model::GithubRepositoryId,
    recovery::{RecoveryPhase, RecoveryRecord, RecoveryResource, RecoveryResourceKind},
    remote::IssueDetailRequest,
    review_mutation::{GithubReviewWriteRemote, ReviewMutationRequest},
    service::GithubService,
};
use serde::Serialize;
use std::{
    collections::{HashMap, HashSet},
    future::Future,
    sync::{
        Arc, Mutex, Weak,
        atomic::{AtomicBool, Ordering},
    },
};
use tokio::{
    sync::{OwnedSemaphorePermit, Semaphore, oneshot},
    task::JoinSet,
};

#[derive(Debug, Default)]
pub struct IssueShutdown {
    pub unfinished_jobs: usize,
    pub failed_jobs: usize,
}

const MAX_JOBS: usize = 32;
const MAX_INPUT: usize = 16 * 1024 * 1024;
#[derive(Clone)]
pub struct IssueDocumentService {
    github: GithubService,
    analysis: Arc<forge_diff::workers::AnalysisPool>,
    budget: EditBudget,
    admission: Arc<DocumentAdmissionStore>,
    state: Arc<Mutex<IssueServiceState>>,
    jobs: Arc<Semaphore>,
    bytes: Arc<Semaphore>,
}
#[derive(Default)]
struct IssueServiceState {
    closed: bool,
    document: HashMap<DocumentId, Arc<IssueOwner>>,
    resource: HashSet<(GithubRepositoryId, u64)>,
    task: JoinSet<()>,
    failure: Vec<String>,
}
struct IssueOwner {
    admission: Arc<DocumentAdmission>,
    document: tokio::sync::Mutex<IssueDocument>,
    remote: Arc<dyn GithubReviewWriteRemote>,
    resource: RecoveryResource,
    store: IssueStore,
    recovery: Mutex<Option<RecoveryRecord>>,
    saving: AtomicBool,
    layout: tokio::sync::Mutex<()>,
    _lease: IssueResourceLease,
}
struct IssueResourceLease {
    state: Weak<Mutex<IssueServiceState>>,
    key: (GithubRepositoryId, u64),
}
impl Drop for IssueResourceLease {
    fn drop(&mut self) {
        if let Some(state) = self.state.upgrade() {
            state
                .lock()
                .expect("issue registry")
                .resource
                .remove(&self.key);
        }
    }
}
struct IssueSaveGuard(Arc<IssueOwner>);
impl Drop for IssueSaveGuard {
    fn drop(&mut self) {
        self.0.saving.store(false, Ordering::Release);
    }
}
#[derive(Serialize)]
pub struct IssueOpen {
    pub fresh_required: bool,
    pub snapshot: BufferSnapshot,
    pub fields: Vec<IssueFieldState>,
    pub recovery: Option<RecoveryRecord>,
}
#[derive(Serialize)]
pub struct IssueSave {
    pub fresh_required: bool,
    pub fields: Vec<IssueFieldState>,
    pub recovery: Option<RecoveryRecord>,
    pub diagnostic: Option<String>,
}

#[derive(Serialize)]
pub struct IssueUpdate {
    pub patch: Option<forge_buffer::patch::BufferPatch>,
    pub fields: Vec<IssueFieldState>,
    pub recovery: Option<RecoveryRecord>,
    pub fresh_required: bool,
}

impl IssueDocumentService {
    pub fn new(
        github: GithubService,
        budget: EditBudget,
        analysis: Arc<forge_diff::workers::AnalysisPool>,
    ) -> Self {
        Self {
            github,
            analysis,
            budget,
            admission: Arc::default(),
            state: Arc::default(),
            jobs: Arc::new(Semaphore::new(MAX_JOBS)),
            bytes: Arc::new(Semaphore::new(MAX_INPUT)),
        }
    }
    pub async fn open(
        &self,
        id: DocumentId,
        store: IssueStore,
        repository: GithubRepositoryId,
        number: u64,
        remote: Arc<dyn GithubReviewWriteRemote>,
    ) -> Result<IssueOpen> {
        ensure!(
            number > 0 && number <= i32::MAX as u64,
            "invalid issue number"
        );
        let admission = Arc::new(self.admission.admit(id.clone())?);
        let job = self.admit_job(4096)?;
        let key = (repository.clone(), number);
        {
            let mut state = self.state.lock().expect("issue registry");
            ensure!(!state.closed, "issue service is closed");
            ensure!(
                state.resource.insert(key.clone()),
                "issue already has an open document"
            );
        }
        let lease = IssueResourceLease {
            state: Arc::downgrade(&self.state),
            key,
        };
        let service = self.clone();
        let (sender, receiver) = oneshot::channel();
        self.spawn(async move {
            let _job = job;
            let result = async {
                let detail = service
                    .github
                    .fetch_issue_document(
                        remote.clone(),
                        store.clone(),
                        IssueDetailRequest {
                            repository: repository.clone(),
                            number,
                        },
                    )
                    .await?;
                admission.check()?;
                let resource = RecoveryResource {
                    repository: repository.clone(),
                    kind: RecoveryResourceKind::Issue,
                    number,
                };
                let model_id = id.clone();
                let budget = service.budget.clone();
                let mut document = service
                    .analyze(admission.clone(), 8 * 1024 * 1024, move || {
                        IssueDocument::new(
                            model_id,
                            repository,
                            detail.node_id.clone(),
                            detail,
                            budget,
                        )
                    })
                    .await?;
                let mut recovery = None;
                if service.github.recovery_configured() {
                    if let Some(draft) = service.github.review_draft(resource.clone()).await? {
                        document.restore_draft(&draft)?;
                    }
                    recovery = service.github.recovery_inspect(resource.clone()).await?;
                    if let Some(operation) = document.pending_operation().map(str::to_owned) {
                        let terminal = recovery
                            .as_ref()
                            .filter(|record| record.capture.operation_id == operation)
                            .and_then(|record| terminal_outcome(&record.state));
                        if let Some(outcome) = terminal {
                            let fresh_required = recovery.as_ref().is_some_and(|record| {
                                matches!(record.state, RecoveryPhase::UserClosedUnknown { .. })
                            });
                            service
                                .github
                                .recovery_settle_draft(
                                    resource.clone(),
                                    operation,
                                    document.resolution_payload(outcome, fresh_required)?,
                                )
                                .await?;
                            document.resolve_pending(outcome, fresh_required)?;
                        }
                    }
                }
                admission.check()?;
                ensure!(!sender.is_closed(), "issue open receiver closed");
                let result = IssueOpen {
                    fresh_required: document.fresh_required(),
                    snapshot: document.snapshot(),
                    fields: document.fields()?,
                    recovery: recovery.clone(),
                };
                let owner = Arc::new(IssueOwner {
                    admission,
                    document: tokio::sync::Mutex::new(document),
                    remote,
                    resource,
                    store,
                    recovery: Mutex::new(recovery),
                    saving: AtomicBool::new(false),
                    layout: tokio::sync::Mutex::new(()),
                    _lease: lease,
                });
                {
                    let mut state = service.state.lock().expect("issue registry");
                    ensure!(!state.closed, "issue service closed during open");
                    owner.admission.check()?;
                    state.document.insert(id.clone(), owner);
                }
                Ok::<_, anyhow::Error>(result)
            }
            .await;
            if sender.send(result).is_err() {
                let removed = {
                    service
                        .state
                        .lock()
                        .expect("issue registry")
                        .document
                        .remove(&id)
                };
                drop(removed);
            }
        })?;
        receiver
            .await
            .context("issue open ended without collection")?
    }
    pub async fn snapshot(&self, id: &DocumentId) -> Result<IssueOpen> {
        let owner = self.get(id)?;
        owner.admission.check()?;
        let document = owner.document.lock().await;
        Ok(IssueOpen {
            fresh_required: document.fresh_required(),
            snapshot: document.snapshot(),
            fields: document.fields()?,
            recovery: owner.recovery.lock().expect("issue recovery").clone(),
        })
    }
    pub async fn edit(&self, edit: RegionEdit) -> Result<IssueEditResult> {
        let owner = self.get(&edit.document)?;
        owner.admission.check()?;
        let job = self.admit_job(edit.text.len().saturating_add(4096))?;
        let service = self.clone();
        let (sender, receiver) = oneshot::channel();
        self.spawn(async move {
            let _job = job;
            let result = async {
                let mut document = owner.document.lock().await;
                owner.admission.check()?;
                let mut prepared = document.prepare_edit(edit)?;
                service
                    .github
                    .review_draft_write(owner.resource.clone(), prepared.take_draft())
                    .await?;
                Ok::<_, anyhow::Error>(prepared.commit())
            }
            .await;
            let _ = sender.send(result);
        })?;
        receiver
            .await
            .context("issue edit ended without durable collection")?
    }
    pub async fn save(&self, id: &DocumentId) -> Result<IssueSave> {
        let owner = self.get(id)?;
        owner.admission.check()?;
        ensure!(
            owner
                .saving
                .compare_exchange(false, true, Ordering::AcqRel, Ordering::Acquire)
                .is_ok(),
            "issue save is already active"
        );
        let guard = IssueSaveGuard(owner.clone());
        let job = self.admit_job(4096)?;
        let service = self.clone();
        let (sender, receiver) = oneshot::channel();
        self.spawn(async move {
            let _job = job;
            let _guard = guard;
            let result = service.execute_save(owner).await;
            let _ = sender.send(result);
        })?;
        receiver
            .await
            .context("issue save ended without collection")?
    }
    async fn execute_save(&self, owner: Arc<IssueOwner>) -> Result<IssueSave> {
        let (mutation, operation, sequence, node_id) = {
            let mut document = owner.document.lock().await;
            owner.admission.check()?;
            let Some(mutation) = document.begin_save()? else {
                self.github
                    .review_draft_write(owner.resource.clone(), document.draft_payload()?)
                    .await?;
                return Ok(IssueSave {
                    fresh_required: document.fresh_required(),
                    fields: document.fields()?,
                    recovery: None,
                    diagnostic: None,
                });
            };
            let operation = document
                .pending_operation()
                .context("issue save capture omitted operation identity")?
                .to_owned();
            let sequence = document.pending_sequence();
            if let Err(failure) = self
                .github
                .review_draft_write(owner.resource.clone(), document.draft_payload()?)
                .await
            {
                document.complete_save(SaveOutcome::Rejected)?;
                return Err(failure);
            }
            (mutation, operation, sequence, document.node_id.clone())
        };
        let request = async {
            let actor = owner
                .remote
                .read_actor(owner.resource.repository.clone())
                .await?;
            self.github
                .review_mutation(
                    owner.remote.clone(),
                    ReviewMutationRequest {
                        parent_node_id: Some(node_id),
                        resource: owner.resource.clone(),
                        operation_id: operation.clone(),
                        actor_node_id: actor.node_id,
                        edit_sequence: Some(sequence),
                        draft_target: Some("issue_document".into()),
                        mutation,
                    },
                )
                .await
        }
        .await;
        let (recovery, diagnostic) = match request {
            Ok(record) => (Some(record), None),
            Err(failure) => (
                self.github.recovery_inspect(owner.resource.clone()).await?,
                Some(format!("{failure:#}").chars().take(8192).collect()),
            ),
        };
        let matching = recovery
            .as_ref()
            .filter(|record| record.capture.operation_id == operation);
        let outcome = matching
            .and_then(|record| terminal_outcome(&record.state))
            .unwrap_or_else(|| {
                if matching.is_some() {
                    SaveOutcome::Uncertain
                } else {
                    SaveOutcome::Rejected
                }
            });
        let mut document = owner.document.lock().await;
        let draft = document.settlement_payload(outcome)?;
        if matching.is_some() && outcome != SaveOutcome::Uncertain {
            self.github
                .recovery_settle_draft(owner.resource.clone(), operation, draft)
                .await?;
        } else {
            self.github
                .review_draft_write(owner.resource.clone(), draft)
                .await?;
        }
        document.complete_save(outcome)?;
        *owner.recovery.lock().expect("issue recovery") = recovery.clone();
        Ok(IssueSave {
            fresh_required: document.fresh_required(),
            fields: document.fields()?,
            recovery,
            diagnostic,
        })
    }
    pub async fn refresh(&self, id: &DocumentId) -> Result<IssueUpdate> {
        let owner = self.get(id)?;
        owner.admission.check()?;
        ensure!(
            owner
                .saving
                .compare_exchange(false, true, Ordering::AcqRel, Ordering::Acquire)
                .is_ok(),
            "issue remote operation is already active"
        );
        let guard = IssueSaveGuard(owner.clone());
        let job = self.admit_job(4096)?;
        let service = self.clone();
        let (sender, receiver) = oneshot::channel();
        self.spawn(async move {
            let _job = job;
            let _guard = guard;
            let result = async {
                let detail = service
                    .github
                    .fetch_issue_document(
                        owner.remote.clone(),
                        owner.store.clone(),
                        IssueDetailRequest {
                            repository: owner.resource.repository.clone(),
                            number: owner.resource.number,
                        },
                    )
                    .await?;
                let capture = {
                    let mut document = owner.document.lock().await;
                    owner.admission.check()?;
                    document.capture_refresh(&detail)?
                };
                let revision = capture.revision;
                let (detail, projection, width) = service
                    .analyze(owner.admission.clone(), 16 * 1024 * 1024, move || {
                        let projection = crate::issue_presentation::project(
                            &detail,
                            capture.fields,
                            &capture.width,
                        )?;
                        Ok((detail, projection, capture.width))
                    })
                    .await?;
                let mut document = owner.document.lock().await;
                owner.admission.check()?;
                let mut prepared = document.prepare_refresh(revision, width, detail, projection)?;
                service
                    .github
                    .review_draft_write(owner.resource.clone(), prepared.take_draft())
                    .await?;
                let patch = prepared.commit();
                Ok(IssueUpdate {
                    patch,
                    fields: document.fields()?,
                    recovery: owner.recovery.lock().expect("issue recovery").clone(),
                    fresh_required: document.fresh_required(),
                })
            }
            .await;
            let _ = sender.send(result);
        })?;
        receiver
            .await
            .context("issue refresh ended without collection")?
    }
    pub async fn resolve(
        &self,
        id: &DocumentId,
        operation_id: String,
        decision: forge_github::review_mutation::RecoveryResolution,
    ) -> Result<IssueSave> {
        use forge_github::review_mutation::RecoveryResolution;
        let owner = self.get(id)?;
        owner.admission.check()?;
        ensure!(
            owner
                .saving
                .compare_exchange(false, true, Ordering::AcqRel, Ordering::Acquire)
                .is_ok(),
            "issue remote operation is already active"
        );
        let guard = IssueSaveGuard(owner.clone());
        let job = self.admit_job(4096)?;
        {
            let document = owner.document.lock().await;
            ensure!(
                document.pending_operation() == Some(operation_id.as_str()),
                "issue recovery operation is stale"
            );
        }
        let service = self.clone();
        let (sender, receiver) = oneshot::channel();
        self.spawn(async move {
            let _job = job;
            let _guard = guard;
            let result = async {
                let record = service
                    .github
                    .recovery_inspect(owner.resource.clone())
                    .await?;
                let matching = record
                    .as_ref()
                    .filter(|record| record.capture.operation_id == operation_id);
                let recovery = if let Some(record) = matching {
                    if terminal_outcome(&record.state).is_some() {
                        Some(record.clone())
                    } else {
                        Some(
                            service
                                .github
                                .recovery_resolve(
                                    owner.remote.clone(),
                                    owner.resource.clone(),
                                    operation_id.clone(),
                                    decision.clone(),
                                )
                                .await?,
                        )
                    }
                } else {
                    ensure!(
                        matches!(decision, RecoveryResolution::CloseUnknown),
                        "missing issue journal requires explicit close unknown"
                    );
                    None
                };
                let outcome = recovery
                    .as_ref()
                    .and_then(|record| terminal_outcome(&record.state))
                    .unwrap_or(SaveOutcome::Rejected);
                let fresh_required = recovery.as_ref().is_none_or(|record| {
                    matches!(record.state, RecoveryPhase::UserClosedUnknown { .. })
                });
                let mut document = owner.document.lock().await;
                ensure!(
                    document.pending_operation() == Some(operation_id.as_str()),
                    "issue recovery capture changed"
                );
                let payload = document.resolution_payload(outcome, fresh_required)?;
                if recovery.is_some() {
                    service
                        .github
                        .recovery_settle_draft(owner.resource.clone(), operation_id, payload)
                        .await?;
                } else {
                    service
                        .github
                        .review_draft_write(owner.resource.clone(), payload)
                        .await?;
                }
                document.resolve_pending(outcome, fresh_required)?;
                *owner.recovery.lock().expect("issue recovery") = recovery.clone();
                Ok(IssueSave {
                    fields: document.fields()?,
                    recovery,
                    diagnostic: None,
                    fresh_required,
                })
            }
            .await;
            let _ = sender.send(result);
        })?;
        receiver
            .await
            .context("issue recovery ended without collection")?
    }
    pub async fn close_collected(&self, id: &DocumentId) -> bool {
        let existed = self.admission.cancel(id);
        let removed = {
            self.state
                .lock()
                .expect("issue registry")
                .document
                .remove(id)
        };
        drop(removed);
        self.admission.wait_closed(id).await;
        existed
    }
    pub fn close(&self) {
        self.admission.cancel_all();
        self.jobs.close();
        self.bytes.close();
        let document = {
            let mut state = self.state.lock().expect("issue registry");
            state.closed = true;
            std::mem::take(&mut state.document)
        };
        drop(document);
    }
    pub async fn shutdown(&self, timeout: std::time::Duration) -> IssueShutdown {
        self.close();
        let deadline = tokio::time::Instant::now() + timeout;
        loop {
            let report = {
                let mut state = self.state.lock().expect("issue registry");
                while let Some(result) = state.task.try_join_next() {
                    if let Err(error) = result {
                        if state.failure.len() < MAX_JOBS {
                            state.failure.push(error.to_string());
                        }
                    }
                }
                IssueShutdown {
                    unfinished_jobs: state.task.len(),
                    failed_jobs: state.failure.len(),
                }
            };
            if report.unfinished_jobs == 0 || tokio::time::Instant::now() >= deadline {
                return report;
            }
            tokio::time::sleep(std::time::Duration::from_millis(5)).await;
        }
    }
    async fn analyze<Output: Send + 'static>(
        &self,
        admission: Arc<DocumentAdmission>,
        bytes: usize,
        work: impl FnOnce() -> Result<Output> + Send + 'static,
    ) -> Result<Output> {
        use forge_diff::workers::{WorkBudget, WorkPriority};
        let (sender, receiver) = oneshot::channel();
        let ticket = self
            .analysis
            .submit(
                WorkPriority::Foreground,
                WorkBudget::new(
                    bytes,
                    Some(std::time::Instant::now() + std::time::Duration::from_secs(10)),
                ),
                move |budget| {
                    let result = (|| {
                        admission.check()?;
                        budget
                            .check()
                            .map_err(|stop| anyhow::anyhow!("issue analysis stopped: {stop:?}"))?;
                        let result = work()?;
                        admission.check()?;
                        budget
                            .check()
                            .map_err(|stop| anyhow::anyhow!("issue analysis stopped: {stop:?}"))?;
                        Ok(result)
                    })();
                    let _ = sender.send(result);
                },
            )
            .map_err(|failure| anyhow::anyhow!("issue analysis admission failed: {failure:?}"))?;
        let result = receiver
            .await
            .context("issue analysis ended without collection")?;
        ticket.completed().await;
        result
    }
    fn get(&self, id: &DocumentId) -> Result<Arc<IssueOwner>> {
        self.state
            .lock()
            .expect("issue registry")
            .document
            .get(id)
            .cloned()
            .context("unknown issue document")
    }
    fn admit_job(&self, bytes: usize) -> Result<(OwnedSemaphorePermit, OwnedSemaphorePermit)> {
        ensure!(
            bytes > 0 && bytes <= MAX_INPUT,
            "issue input exceeds 16 MiB"
        );
        Ok((
            self.jobs
                .clone()
                .try_acquire_owned()
                .context("issue job admission is full")?,
            self.bytes
                .clone()
                .try_acquire_many_owned(bytes as u32)
                .context("issue input admission is full")?,
        ))
    }
    fn spawn(&self, future: impl Future<Output = ()> + Send + 'static) -> Result<()> {
        let mut state = self.state.lock().expect("issue registry");
        ensure!(!state.closed, "issue service is closed");
        while let Some(result) = state.task.try_join_next() {
            if let Err(failure) = result {
                if state.failure.len() < MAX_JOBS {
                    state.failure.push(failure.to_string());
                }
            }
        }
        state.task.spawn(future);
        Ok(())
    }
}
fn terminal_outcome(phase: &RecoveryPhase) -> Option<SaveOutcome> {
    match phase {
        RecoveryPhase::Confirmed { .. } | RecoveryPhase::UserLinked { .. } => {
            Some(SaveOutcome::Confirmed)
        }
        RecoveryPhase::Rejected { .. } | RecoveryPhase::UserClosedUnknown { .. } => {
            Some(SaveOutcome::Rejected)
        }
        _ => None,
    }
}

#[cfg(test)]
mod tests;

mod view;
