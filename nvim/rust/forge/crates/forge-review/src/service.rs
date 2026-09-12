use std::collections::BTreeMap;
use std::future::Future;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};
use std::time::Duration;

use anyhow::{Context, Result, ensure};
use forge_buffer::identity::DocumentId;
use forge_github::comment::GithubCommentRemote;
use forge_github::pull_request::{
    DesiredPullRequestState, PullRequestLifecycle, PullRequestOperation, PullRequestOutcome,
    PullRequestRequest, PullRequestResult, PullRequestTarget,
};
use forge_github::remote::GithubRemote;

/// Capabilities retained by one review document for PR and comment operations.
pub trait ReviewRemote:
    GithubRemote
    + GithubCommentRemote
    + forge_github::review_mutation::GithubReviewWriteRemote
    + forge_github::review_api::GithubReviewRemote
{
}
impl<
    Remote: GithubRemote
        + GithubCommentRemote
        + forge_github::review_mutation::GithubReviewWriteRemote
        + forge_github::review_api::GithubReviewRemote,
> ReviewRemote for Remote
{
}

mod comment;
mod commit;
mod document;
pub(crate) mod file;
mod input;
pub(crate) mod thread_projection;
pub use file::{ReviewFileAnalysis, ReviewFileDelivery};
mod section;
mod thread;
pub use comment::ReviewCommentResult;
pub use document::{ReviewEditResult, ReviewMaterialization};
use forge_github::service::{GithubService, MutationNotStarted};
pub use input::{ReviewActionDelivery, ReviewActionEffect};
pub use section::{
    ReviewCommitPresentation, ReviewComparisonIdentity, ReviewFileSource, ReviewSectionItem,
    ReviewSectionKind, ReviewSectionState,
};
use serde::Serialize;
pub use thread::{ReviewThread, ReviewThreadComment, ReviewThreadDelivery};
use tokio::sync::{OwnedSemaphorePermit, Semaphore, oneshot};
use tokio::task::JoinSet;

use crate::edit::{EditBudget, RegionEdit};
use crate::review::{ReviewDocument, ReviewMode, ReviewSnapshot, ReviewVerdict};

const MAX_DOCUMENTS: usize = 64;
const MAX_JOBS: usize = 32;
const MAX_TEXT_BYTES: usize = 16 * 1024 * 1024;

#[derive(Debug, Serialize)]
pub struct ReviewSaveResult {
    pub snapshot: ReviewSnapshot,
    pub remote: Option<PullRequestResult>,
}

#[derive(Clone, Copy, Debug, Serialize, Eq, PartialEq)]
pub struct ReviewLifecycle {
    pub state: PullRequestLifecycle,
    pub is_draft: bool,
}

impl ReviewLifecycle {
    pub fn label(self) -> &'static str {
        match (self.state, self.is_draft) {
            (PullRequestLifecycle::Closed, _) => "CLOSED",
            (PullRequestLifecycle::Open, true) => "DRAFT",
            (PullRequestLifecycle::Open, false) => "OPEN",
            (PullRequestLifecycle::Merged, _) => "MERGED",
        }
    }
}

#[derive(Debug, Serialize)]
pub struct ReviewLifecycleDelivery {
    pub lifecycle: Option<ReviewLifecycle>,
    pub recovery: Option<forge_github::recovery::RecoveryRecord>,
    pub fresh_required: bool,
}

#[derive(Debug, Serialize)]
pub struct ReviewBatchedDelivery {
    pub snapshot: ReviewSnapshot,
    pub mode: ReviewMode,
    pub viewed_file: Vec<String>,
}

#[derive(Debug, Serialize)]
pub struct ReviewBatchedSubmissionDelivery {
    pub snapshot: ReviewSnapshot,
    pub mode: ReviewMode,
    pub viewed_file: Vec<String>,
    pub outcome: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub operation_id: Option<String>,
}

#[derive(Debug, Serialize)]
pub struct ReviewBatchedRecoveryDelivery {
    pub submission: ReviewBatchedSubmissionDelivery,
    pub fresh_required: bool,
}

#[derive(Debug, Default)]
pub struct ReviewShutdown {
    pub unfinished_jobs: usize,
    pub failed_jobs: usize,
}

/// One host's document registry. Remote work retains ownership after request or buffer closure.
/// All remote mutations and reconciliation use the shared GithubService resource queue.
#[derive(Clone)]
pub struct ReviewService {
    diff: Option<Arc<forge_diff::engine::DiffEngine>>,
    syntax: Option<Arc<forge_diff::syntax::SyntaxEngine>>,
    section_bytes: Arc<std::sync::atomic::AtomicUsize>,
    analysis: Option<Arc<forge_diff::workers::AnalysisPool>>,
    epoch: uuid::Uuid,
    state: Arc<Mutex<ServiceState>>,
    github: GithubService,
    budget: EditBudget,
    document_admission: Arc<Semaphore>,
    job_admission: Arc<Semaphore>,
}

#[derive(Default)]
struct ServiceState {
    closed: bool,
    next_document: u64,
    document: BTreeMap<DocumentId, Arc<DocumentOwner>>,
    task: JoinSet<()>,
    failed_jobs: usize,
}

struct DocumentOwner {
    workspace: Option<std::path::PathBuf>,
    document: Mutex<ReviewDocument>,
    publication: tokio::sync::Mutex<()>,
    remote: Arc<dyn ReviewRemote>,
    remote_active: AtomicBool,
    _admission: OwnedSemaphorePermit,
}

impl DocumentOwner {
    fn validate_directory(&self, directory: &std::path::Path) -> Result<()> {
        if let Some(workspace) = &self.workspace {
            validate_workspace(directory)?;
            ensure!(
                workspace == directory,
                "review workspace differs from its captured owner"
            );
        }
        Ok(())
    }
}

fn validate_workspace(directory: &std::path::Path) -> Result<()> {
    let bytes = directory.as_os_str().as_encoded_bytes();
    ensure!(
        directory.is_absolute()
            && bytes.len() <= 32_768
            && !bytes.contains(&0)
            && !directory
                .components()
                .any(|component| matches!(component, std::path::Component::ParentDir)),
        "review workspace must be an absolute captured path without parent traversal"
    );
    Ok(())
}

struct RemoteGuard {
    owner: Arc<DocumentOwner>,
}

impl Drop for RemoteGuard {
    fn drop(&mut self) {
        let mut document = self
            .owner
            .document
            .lock()
            .unwrap_or_else(|poison| poison.into_inner());
        document.abandon_save();
        self.owner.remote_active.store(false, Ordering::Release);
    }
}

impl ReviewService {
    pub async fn begin_batched(&self, id: &DocumentId) -> Result<ReviewBatchedDelivery> {
        let owner = self.owner(id)?;
        let (delivery, resource, draft) = {
            let mut document = owner.document.lock().expect("review document poisoned");
            document.begin_batched()?;
            let resource = forge_github::recovery::RecoveryResource {
                repository: document.target.repository.clone(),
                kind: forge_github::recovery::RecoveryResourceKind::PullRequest,
                number: document.target.number,
            };
            let delivery = ReviewBatchedDelivery {
                snapshot: document.snapshot()?,
                mode: document.mode,
                viewed_file: document.viewed_file.iter().cloned().collect(),
            };
            (delivery, resource, document.draft_payload()?)
        };
        self.github.review_draft_write(resource, draft).await?;
        Ok(delivery)
    }

    pub async fn set_viewed(
        &self,
        id: &DocumentId,
        path: String,
        viewed: bool,
    ) -> Result<ReviewBatchedDelivery> {
        let owner = self.owner(id)?;
        let (delivery, resource, draft) = {
            let mut document = owner.document.lock().expect("review document poisoned");
            document.set_viewed(path, viewed)?;
            let resource = forge_github::recovery::RecoveryResource {
                repository: document.target.repository.clone(),
                kind: forge_github::recovery::RecoveryResourceKind::PullRequest,
                number: document.target.number,
            };
            let delivery = ReviewBatchedDelivery {
                snapshot: document.snapshot()?,
                mode: document.mode,
                viewed_file: document.viewed_file.iter().cloned().collect(),
            };
            (delivery, resource, document.draft_payload()?)
        };
        self.github.review_draft_write(resource, draft).await?;
        Ok(delivery)
    }
    pub async fn submit_batched(
        &self,
        id: &DocumentId,
        verdict: ReviewVerdict,
    ) -> Result<ReviewBatchedSubmissionDelivery> {
        use forge_github::recovery::RecoveryPhase;
        use forge_github::review_mutation::ReviewMutationRequest;
        let job = Arc::clone(&self.job_admission)
            .try_acquire_owned()
            .context("review job admission is full")?;
        let guard = self.remote_guard(id)?;
        let operation_id = uuid::Uuid::new_v4().to_string();
        let (target, mutation, sequence, draft) = {
            let mut document = guard
                .owner
                .document
                .lock()
                .expect("review document poisoned");
            let (mutation, sequence) =
                document.begin_batched_submission(operation_id.clone(), verdict)?;
            (
                document.target.clone(),
                mutation,
                sequence,
                document.draft_payload()?,
            )
        };
        let resource = forge_github::recovery::RecoveryResource {
            repository: target.repository.clone(),
            kind: forge_github::recovery::RecoveryResourceKind::PullRequest,
            number: target.number,
        };
        self.github
            .review_draft_write(resource.clone(), draft)
            .await?;
        let service = self.clone();
        let (sender, receiver) = oneshot::channel();
        self.spawn(async move {
            let _job = job;
            let mut dispatch_attempted = false;
            let outcome = async {
                let actor = guard
                    .owner
                    .remote
                    .read_actor(target.repository.clone())
                    .await?;
                dispatch_attempted = true;
                service
                    .github
                    .review_mutation(
                        guard.owner.remote.clone(),
                        ReviewMutationRequest {
                            parent_node_id: Some(target.node_id.clone()),
                            resource: resource.clone(),
                            operation_id: operation_id.clone(),
                            actor_node_id: actor.node_id,
                            edit_sequence: Some(sequence),
                            draft_target: Some("review:submission".into()),
                            mutation,
                        },
                    )
                    .await
            }
            .await;
            let (outcome_name, confirmed, terminal) = match &outcome {
                Ok(record) => match &record.state {
                    RecoveryPhase::Confirmed { .. } => ("confirmed", true, true),
                    RecoveryPhase::Rejected { .. } => ("rejected", false, true),
                    _ => ("outcome_unknown", false, false),
                },
                Err(error)
                    if !dispatch_attempted
                        || error.is::<forge_github::service::MutationNotStarted>() =>
                {
                    ("rejected", false, true)
                }
                Err(_) => ("outcome_unknown", false, false),
            };
            let result = async {
                let _publication = guard.owner.publication.lock().await;
                let (snapshot, viewed_file, draft) = {
                    let mut document = guard
                        .owner
                        .document
                        .lock()
                        .expect("review document poisoned");
                    document.settle_batched_submission(confirmed, terminal)?;
                    (
                        document.snapshot()?,
                        document.viewed_file.iter().cloned().collect(),
                        document.draft_payload()?,
                    )
                };
                if terminal {
                    service
                        .github
                        .recovery_settle_draft(resource.clone(), operation_id.clone(), draft)
                        .await?;
                } else {
                    service.github.review_draft_write(resource, draft).await?;
                }
                Ok(ReviewBatchedSubmissionDelivery {
                    mode: ReviewMode::Batched,
                    viewed_file,
                    snapshot,
                    outcome: outcome_name.into(),
                    operation_id: (!terminal).then_some(operation_id),
                })
            }
            .await;
            drop(guard);
            let _ = sender.send(result);
        })?;
        receiver
            .await
            .context("review submission ended without a result")?
    }

    pub async fn recover_batched_submission(
        &self,
        id: &DocumentId,
        operation_id: String,
        resolution: forge_github::review_mutation::RecoveryResolution,
    ) -> Result<ReviewBatchedRecoveryDelivery> {
        use forge_github::recovery::RecoveryPhase;
        let owner = self.owner(id)?;
        let (target, expected_capture) = {
            let document = owner.document.lock().expect("review document poisoned");
            ensure!(
                document.batched_operation.as_deref() == Some(operation_id.as_str()),
                "review submission recovery does not match the captured operation"
            );
            (
                document.target.clone(),
                document
                    .batched_capture
                    .clone()
                    .context("review submission capture is unavailable")?,
            )
        };
        ensure!(
            matches!(
                expected_capture,
                forge_github::review_mutation::ReviewMutation::ReviewSubmit { .. }
            ),
            "review submission capture is invalid"
        );
        let resource = lifecycle_resource(&target);
        let retained = self.github.recovery_inspect(resource.clone()).await?;
        let has_retained = retained.is_some();
        let (confirmed, outcome, fresh_required) = if let Some(retained) = retained {
            ensure!(
                retained.capture.operation_id == operation_id
                    && retained.capture.operation
                        == forge_github::recovery::RecoveryOperation::ReviewSubmit
                    && retained
                        .capture
                        .submitted
                        .get("draft_target")
                        .and_then(serde_json::Value::as_str)
                        == Some("review:submission"),
                "review submission recovery belongs to another operation"
            );
            ensure!(
                retained.capture.submitted.get("mutation")
                    == Some(&serde_json::to_value(&expected_capture)?),
                "review submission recovery differs from the durable capture"
            );
            let record = self
                .github
                .recovery_resolve(
                    owner.remote.clone(),
                    resource.clone(),
                    operation_id.clone(),
                    resolution,
                )
                .await?;
            match record.state {
                RecoveryPhase::Confirmed { .. } => (true, "confirmed", false),
                RecoveryPhase::Rejected { .. } => (false, "rejected", false),
                RecoveryPhase::UserClosedUnknown { .. } => (false, "closed_unknown", true),
                _ => anyhow::bail!("review submission recovery remains unresolved"),
            }
        } else {
            ensure!(
                matches!(
                    resolution,
                    forge_github::review_mutation::RecoveryResolution::NotDispatched
                        | forge_github::review_mutation::RecoveryResolution::CloseUnknown
                ),
                "missing review submission journal requires an explicit rejected or close-unknown decision"
            );
            let closed = matches!(
                resolution,
                forge_github::review_mutation::RecoveryResolution::CloseUnknown
            );
            (
                false,
                if closed { "closed_unknown" } else { "rejected" },
                closed,
            )
        };
        let (snapshot, viewed_file, draft) = {
            let mut document = owner.document.lock().expect("review document poisoned");
            document.settle_batched_submission(confirmed, true)?;
            (
                document.snapshot()?,
                document.viewed_file.iter().cloned().collect(),
                document.draft_payload()?,
            )
        };
        if has_retained {
            self.github
                .recovery_settle_draft(resource, operation_id, draft)
                .await?;
        } else {
            self.github.review_draft_write(resource, draft).await?;
        }
        Ok(ReviewBatchedRecoveryDelivery {
            submission: ReviewBatchedSubmissionDelivery {
                snapshot,
                mode: ReviewMode::Batched,
                viewed_file,
                outcome: outcome.into(),
                operation_id: None,
            },
            fresh_required,
        })
    }

    pub fn new(github: GithubService) -> Self {
        Self {
            diff: None,
            syntax: None,
            section_bytes: Arc::new(std::sync::atomic::AtomicUsize::new(0)),
            analysis: None,
            epoch: uuid::Uuid::new_v4(),
            state: Arc::new(Mutex::new(ServiceState::default())),
            github,
            budget: EditBudget::new(MAX_TEXT_BYTES).expect("positive review text budget"),
            document_admission: Arc::new(Semaphore::new(MAX_DOCUMENTS)),
            job_admission: Arc::new(Semaphore::new(MAX_JOBS)),
        }
    }

    pub fn with_analysis(
        github: GithubService,
        analysis: Arc<forge_diff::workers::AnalysisPool>,
    ) -> Self {
        let mut service = Self::new(github);
        service.analysis = Some(analysis);
        service
    }

    pub fn with_engines(
        github: GithubService,
        diff: Arc<forge_diff::engine::DiffEngine>,
        syntax: Arc<forge_diff::syntax::SyntaxEngine>,
    ) -> Self {
        let mut service = Self::with_analysis(github, diff.analysis_pool());
        service.diff = Some(diff);
        service.syntax = Some(syntax);
        service
    }

    /// Reads remote text before publishing a document. Failed or abandoned opens release admission.
    pub async fn open_repository_pr(
        &self,
        directory: std::path::PathBuf,
        remote: Arc<dyn ReviewRemote>,
        repository: forge_github::model::GithubRepositoryId,
        number: u64,
    ) -> Result<ReviewSnapshot> {
        validate_workspace(&directory)?;
        let page = self
            .github
            .review_page(
                directory.clone(),
                remote.clone(),
                forge_github::review_api::ReviewReadRequest {
                    repository: repository.clone(),
                    number,
                    view: forge_github::review_api::ReviewSection::Overview,
                    cursor: None,
                },
            )
            .await?;
        ensure!(
            page.complete && page.records.len() == 1,
            "PR discovery omitted its complete summary"
        );
        let record = &page.records[0];
        ensure!(
            record.get("number").and_then(serde_json::Value::as_u64) == Some(number),
            "PR discovery returned another number"
        );
        let target = PullRequestTarget {
            repository,
            number,
            node_id: record
                .get("node_id")
                .and_then(serde_json::Value::as_str)
                .context("PR discovery omitted node identity")?
                .into(),
        };
        target.validate()?;
        self.open_captured_pr(remote, target, Some(directory)).await
    }

    pub async fn open_pr(
        &self,
        remote: Arc<dyn ReviewRemote>,
        target: PullRequestTarget,
    ) -> Result<ReviewSnapshot> {
        self.open_captured_pr(remote, target, None).await
    }

    pub async fn open_pr_in_directory(
        &self,
        directory: std::path::PathBuf,
        remote: Arc<dyn ReviewRemote>,
        target: PullRequestTarget,
    ) -> Result<ReviewSnapshot> {
        validate_workspace(&directory)?;
        self.open_captured_pr(remote, target, Some(directory)).await
    }

    async fn open_captured_pr(
        &self,
        remote: Arc<dyn ReviewRemote>,
        target: PullRequestTarget,
        workspace: Option<std::path::PathBuf>,
    ) -> Result<ReviewSnapshot> {
        target.validate()?;
        let admission = Arc::clone(&self.document_admission)
            .try_acquire_owned()
            .context("review document admission is full")?;
        let job = Arc::clone(&self.job_admission)
            .try_acquire_owned()
            .context("review job admission is full")?;
        let id = {
            let mut state = self.state.lock().expect("review registry poisoned");
            ensure!(!state.closed, "review service is closed");
            state.next_document = state
                .next_document
                .checked_add(1)
                .context("review document identity exhausted")?;
            DocumentId(format!("review-{}-{}", self.epoch, state.next_document))
        };
        let (sender, receiver) = oneshot::channel();
        let service = self.clone();
        self.spawn(async move {
            let _job = job;
            let result = async {
                let observation = service
                    .github
                    .pull_request(
                        remote.clone(),
                        PullRequestRequest {
                            target: target.clone(),
                            request: PullRequestOperation::Reconcile,
                        },
                    )
                    .await?;
                let text = observation
                    .text
                    .context("PR reconciliation omitted field text")?;
                let resource = forge_github::recovery::RecoveryResource {
                    repository: target.repository.clone(),
                    kind: forge_github::recovery::RecoveryResourceKind::PullRequest,
                    number: target.number,
                };
                let mut document =
                    ReviewDocument::open(id.clone(), target, text, service.budget.clone())?;
                if service.github.recovery_configured()
                    && let Some(draft) = service.github.review_draft(resource).await?
                {
                    document.restore_pr_fields(&draft)?;
                    document.restore_comments(&draft)?;
                    document.restore_batched(&draft)?;
                }
                let snapshot = document.snapshot()?;
                let owner = Arc::new(DocumentOwner {
                    workspace,
                    document: Mutex::new(document),
                    publication: tokio::sync::Mutex::new(()),
                    remote,
                    remote_active: AtomicBool::new(false),
                    _admission: admission,
                });
                let mut state = service.state.lock().expect("review registry poisoned");
                ensure!(
                    !state.closed,
                    "review service closed while opening document"
                );
                if sender.is_closed() {
                    anyhow::bail!("review open receiver closed");
                }
                state.document.insert(id.clone(), owner);
                Ok(snapshot)
            }
            .await;
            if sender.send(result).is_err() {
                service
                    .state
                    .lock()
                    .expect("review registry poisoned")
                    .document
                    .remove(&id);
            }
        })?;
        receiver
            .await
            .context("review open ended without a result")?
    }

    pub async fn region_edit(&self, edit: RegionEdit) -> Result<ReviewEditResult> {
        let owner = self.owner(&edit.document)?;
        let job = Arc::clone(&self.job_admission)
            .try_acquire_owned()
            .context("review edit admission is full")?;
        let service = self.clone();
        let (sender, receiver) = oneshot::channel();
        self.spawn(async move {
            let _job = job;
            let _publication = owner.publication.lock().await;
            let result = async {
                let (acknowledgement, target, draft) = {
                    let mut document = owner.document.lock().expect("review document poisoned");
                    let acknowledgement = document.projected_edit(edit)?;
                    (
                        acknowledgement,
                        document.target.clone(),
                        document.draft_payload()?,
                    )
                };
                let resource = forge_github::recovery::RecoveryResource {
                    repository: target.repository,
                    kind: forge_github::recovery::RecoveryResourceKind::PullRequest,
                    number: target.number,
                };
                service.github.review_draft_write(resource, draft).await?;
                Ok::<_, anyhow::Error>(acknowledgement)
            }
            .await;
            let _ = sender.send(result);
        })?;
        receiver
            .await
            .context("review edit publication ended without collection")?
    }

    pub fn snapshot(&self, id: &DocumentId) -> Result<ReviewSnapshot> {
        let owner = self.owner(id)?;
        owner
            .document
            .lock()
            .expect("review document poisoned")
            .snapshot()
    }

    /// Sends one captured title/body batch. Receiver cancellation never cancels an admitted write.
    pub async fn save(&self, id: &DocumentId) -> Result<ReviewSaveResult> {
        use forge_github::recovery::RecoveryPhase;
        use forge_github::review_mutation::{ReviewMutation, ReviewMutationRequest};
        let job = Arc::clone(&self.job_admission)
            .try_acquire_owned()
            .context("review job admission is full")?;
        let guard = self.remote_guard(id)?;
        let operation_id = uuid::Uuid::new_v4().to_string();
        let prepared = {
            let mut document = guard
                .owner
                .document
                .lock()
                .expect("review document poisoned");
            let snapshot = document.snapshot()?;
            let sequence = snapshot
                .field
                .iter()
                .map(|field| field.sequence.0)
                .max()
                .unwrap_or(0);
            document
                .begin_save(operation_id.clone())?
                .map(|edit| (document.target.clone(), edit, sequence))
        };
        let Some((target, edit, sequence)) = prepared else {
            return Ok(ReviewSaveResult {
                snapshot: guard
                    .owner
                    .document
                    .lock()
                    .expect("review document poisoned")
                    .snapshot()?,
                remote: None,
            });
        };
        let service = self.clone();
        let (sender, receiver) = oneshot::channel();
        self.spawn(async move {
            let _job = job;
            let resource = forge_github::recovery::RecoveryResource {
                repository: target.repository.clone(),
                kind: forge_github::recovery::RecoveryResourceKind::PullRequest,
                number: target.number,
            };
            let mut dispatch_attempted = false;
            let mut draft_published = false;
            let outcome = async {
                {
                    let _publication = guard.owner.publication.lock().await;
                    let draft = guard.owner.document.lock().expect("review document poisoned").draft_payload()?;
                    service.github.review_draft_write(resource.clone(), draft).await?;
                    draft_published = true;
                }
                let actor = guard.owner.remote.read_actor(target.repository.clone()).await?;
                dispatch_attempted = true;
                service.github.review_mutation(guard.owner.remote.clone(), ReviewMutationRequest {
                    parent_node_id: Some(target.node_id.clone()),
                    resource: resource.clone(), operation_id: operation_id.clone(), actor_node_id: actor.node_id,
                    edit_sequence: Some(sequence), draft_target: Some("pr:fields".into()), mutation: ReviewMutation::PullRequestEdit {
                        node_id: target.node_id, title: edit.title.clone(), body: edit.body.clone(),
                    },
                }).await
            }.await;
            let remote_result = match &outcome {
                Ok(record) => {
                    let (outcome, message, state) = match &record.state {
                        RecoveryPhase::Confirmed { result } => (PullRequestOutcome::Confirmed, None, Some(result)),
                        RecoveryPhase::Rejected { diagnostic } => (PullRequestOutcome::Rejected, Some(diagnostic.clone()), None),
                        RecoveryPhase::OutcomeUnknown { diagnostic } => (PullRequestOutcome::OutcomeUnknown, Some(diagnostic.clone()), None),
                        _ => (PullRequestOutcome::OutcomeUnknown, Some("remote settlement remains unresolved".into()), None),
                    };
                    PullRequestResult { ok: outcome == PullRequestOutcome::Confirmed, outcome,
                        state: state.and_then(|state| state.get("state")).and_then(|state| serde_json::from_value(state.clone()).ok()),
                        is_draft: state.and_then(|state| state.get("isDraft")).and_then(serde_json::Value::as_bool),
                        message, matches_submission: (outcome == PullRequestOutcome::Confirmed).then_some(true), text: None }
                }
                Err(failure) => PullRequestResult { ok: false,
                    outcome: if !dispatch_attempted || failure.is::<MutationNotStarted>() { PullRequestOutcome::Rejected } else { PullRequestOutcome::OutcomeUnknown },
                    state: None, is_draft: None, message: Some(failure.to_string()), matches_submission: None, text: None },
            };
            let settled = async {
                let _publication = guard.owner.publication.lock().await;
                if outcome.is_err() && remote_result.outcome == PullRequestOutcome::Rejected {
                    let draft = {
                        let mut document = guard.owner.document.lock().expect("review document poisoned");
                        document.complete_save(Some(&remote_result))?;
                        draft_published.then(|| document.draft_payload()).transpose()?
                    };
                    if let Some(draft) = draft {
                        service.github.review_draft_write(resource.clone(), draft).await
                            .map_err(forge_github::service::not_started)?;
                    }
                    return Err(forge_github::service::not_started(outcome.unwrap_err()));
                }
                if let Ok(record) = &outcome
                    && matches!(record.state, RecoveryPhase::Confirmed { .. } | RecoveryPhase::Rejected { .. }) {
                    let mut snapshot = guard.owner.document.lock().expect("review document poisoned").snapshot()?;
                    if matches!(record.state, RecoveryPhase::Confirmed { .. }) {
                        for field in &mut snapshot.field {
                            let submitted = if field.region.0 == "title" { edit.title.as_ref() } else { edit.body.as_ref() };
                            if let Some(submitted) = submitted {
                                field.baseline = submitted.clone();
                                field.dirty = field.text != *submitted;
                                field.uncertain = false;
                            }
                        }
                    }
                    snapshot.saving = false;
                    snapshot.uncertain = false;
                    service.github.recovery_settle_draft(resource.clone(), operation_id,
                        serde_json::json!({ "repo":resource.repository.repository_name(), "number":resource.number,
                            "pr_fields":snapshot, "pr_pending":[], "pr_operation":null })).await?;
                }
                let mut document = guard.owner.document.lock().expect("review document poisoned");
                document.complete_save(Some(&remote_result))?;
                outcome?;
                Ok::<_, anyhow::Error>(ReviewSaveResult { snapshot: document.snapshot()?, remote: Some(remote_result) })
            }.await;
            if settled.is_err() {
                let _ = guard.owner.document.lock().expect("review document poisoned").complete_save(None);
            }
            drop(guard);
            let _ = sender.send(settled);
        })?;
        receiver
            .await
            .context("review save ended without a result")?
    }
    /// Observes an unresolved write without reposting it. Local edits remain available throughout.
    pub async fn reconcile(&self, id: &DocumentId) -> Result<ReviewSnapshot> {
        let job = Arc::clone(&self.job_admission)
            .try_acquire_owned()
            .context("review job admission is full")?;
        let guard = self.remote_guard(id)?;
        let (target, operation_id) = {
            let document = guard
                .owner
                .document
                .lock()
                .expect("review document poisoned");
            let snapshot = document.snapshot()?;
            ensure!(snapshot.uncertain, "review document has no uncertain save");
            if document.pending_operation.is_none()
                && snapshot.field.iter().all(|field| !field.uncertain)
            {
                return Ok(snapshot);
            }
            (
                document.target.clone(),
                document
                    .pending_operation
                    .clone()
                    .context("uncertain PR capture lacks operation identity")?,
            )
        };
        let service = self.clone();
        let (sender, receiver) = oneshot::channel();
        self.spawn(async move {
            let _job = job;
            let result = async {
                let resource = forge_github::recovery::RecoveryResource {
                    repository: target.repository.clone(),
                    kind: forge_github::recovery::RecoveryResourceKind::PullRequest,
                    number: target.number,
                };
                let mut recovery = service
                    .github
                    .recovery_inspect(resource.clone())
                    .await?
                    .filter(|record| {
                        record.capture.operation
                            == forge_github::recovery::RecoveryOperation::PullRequestEdit
                    });
                ensure!(
                    recovery
                        .as_ref()
                        .is_none_or(|record| record.capture.operation_id == operation_id),
                    "PR recovery record belongs to another captured operation"
                );
                if let Some(record) = &recovery
                    && matches!(
                        record.state,
                        forge_github::recovery::RecoveryPhase::DispatchPossible
                            | forge_github::recovery::RecoveryPhase::OutcomeUnknown { .. }
                    )
                {
                    recovery = Some(
                        service
                            .github
                            .recovery_resolve(
                                guard.owner.remote.clone(),
                                resource.clone(),
                                record.capture.operation_id.clone(),
                                forge_github::review_mutation::RecoveryResolution::Link {
                                    remote_id: target.number,
                                },
                            )
                            .await?,
                    );
                }
                let observation = service
                    .github
                    .pull_request(
                        guard.owner.remote.clone(),
                        PullRequestRequest {
                            target,
                            request: PullRequestOperation::Reconcile,
                        },
                    )
                    .await?;
                let _publication = guard.owner.publication.lock().await;
                let record = recovery.context("captured PR recovery record is unavailable")?;
                ensure!(
                    matches!(
                        record.state,
                        forge_github::recovery::RecoveryPhase::Confirmed { .. }
                            | forge_github::recovery::RecoveryPhase::UserLinked { .. }
                            | forge_github::recovery::RecoveryPhase::UserClosedUnknown { .. }
                    ),
                    "captured PR operation has no verified terminal result"
                );
                let observed = observation
                    .text
                    .context("PR reconciliation omitted field text")?;
                let draft = guard
                    .owner
                    .document
                    .lock()
                    .expect("review document poisoned")
                    .reconciled_draft(
                        &observed,
                        !matches!(
                            record.state,
                            forge_github::recovery::RecoveryPhase::UserClosedUnknown { .. }
                        ),
                    )?;
                service
                    .github
                    .recovery_settle_draft(resource, record.capture.operation_id, draft)
                    .await?;
                let mut document = guard
                    .owner
                    .document
                    .lock()
                    .expect("review document poisoned");
                document.reconcile(observed)?;
                document.snapshot()
            }
            .await;
            drop(guard);
            let _ = sender.send(result);
        })?;
        receiver
            .await
            .context("review reconciliation ended without a result")?
    }

    /// Changes one retained pull request lifecycle through a durable mutation record.
    /// A nonterminal record blocks another transition until the caller resolves or reconciles it.
    pub async fn transition(
        &self,
        id: &DocumentId,
        desired: DesiredPullRequestState,
    ) -> Result<ReviewLifecycleDelivery> {
        use forge_github::recovery::RecoveryPhase;
        use forge_github::review_mutation::{ReviewMutation, ReviewMutationRequest};

        let job = Arc::clone(&self.job_admission)
            .try_acquire_owned()
            .context("review lifecycle admission is full")?;
        let guard = self.remote_guard(id)?;
        let target = guard
            .owner
            .document
            .lock()
            .expect("review document poisoned")
            .target
            .clone();
        let resource = lifecycle_resource(&target);
        if let Some(record) = self.github.recovery_inspect(resource.clone()).await? {
            ensure!(
                lifecycle_record_is_terminal(&record),
                "review lifecycle outcome requires explicit recovery"
            );
        }
        let service = self.clone();
        let (sender, receiver) = oneshot::channel();
        self.spawn(async move {
            let _job = job;
            let result = async {
                let operation_id = uuid::Uuid::new_v4().to_string();
                let actor = guard
                    .owner
                    .remote
                    .read_actor(target.repository.clone())
                    .await?;
                let record = service
                    .github
                    .review_mutation(
                        guard.owner.remote.clone(),
                        ReviewMutationRequest {
                            parent_node_id: Some(target.node_id.clone()),
                            resource: resource.clone(),
                            operation_id: operation_id.clone(),
                            actor_node_id: actor.node_id,
                            edit_sequence: None,
                            draft_target: None,
                            mutation: ReviewMutation::PullRequestTransition {
                                node_id: target.node_id.clone(),
                                desired,
                            },
                        },
                    )
                    .await?;
                let fresh_required =
                    matches!(record.state, RecoveryPhase::UserClosedUnknown { .. });
                let lifecycle = if lifecycle_record_is_terminal(&record) && !fresh_required {
                    let observed = service
                        .github
                        .pull_request(
                            guard.owner.remote.clone(),
                            PullRequestRequest {
                                target: target.clone(),
                                request: PullRequestOperation::Reconcile,
                            },
                        )
                        .await?;
                    let lifecycle = lifecycle_from_result(&observed)?;
                    service
                        .github
                        .recovery_acknowledge(resource.clone(), operation_id)
                        .await?;
                    lifecycle
                } else {
                    None
                };
                Ok(ReviewLifecycleDelivery {
                    lifecycle,
                    recovery: fresh_required.then_some(record),
                    fresh_required,
                })
            }
            .await;
            drop(guard);
            let _ = sender.send(result);
        })?;
        receiver
            .await
            .context("review lifecycle transition ended without a result")?
    }

    /// Re-reads remote lifecycle state and retires a terminal lifecycle record after that read.
    pub async fn lifecycle_reconcile(&self, id: &DocumentId) -> Result<ReviewLifecycleDelivery> {
        let job = Arc::clone(&self.job_admission)
            .try_acquire_owned()
            .context("review lifecycle reconciliation admission is full")?;
        let guard = self.remote_guard(id)?;
        let target = guard
            .owner
            .document
            .lock()
            .expect("review document poisoned")
            .target
            .clone();
        let resource = lifecycle_resource(&target);
        let service = self.clone();
        let (sender, receiver) = oneshot::channel();
        self.spawn(async move {
            let _job = job;
            let result = async {
                let recovery = service.github.recovery_inspect(resource.clone()).await?;
                if let Some(record) = &recovery {
                    ensure!(
                        lifecycle_record_is_terminal(record),
                        "review lifecycle outcome requires explicit recovery"
                    );
                }
                let observed = service
                    .github
                    .pull_request(
                        guard.owner.remote.clone(),
                        PullRequestRequest {
                            target,
                            request: PullRequestOperation::Reconcile,
                        },
                    )
                    .await?;
                let lifecycle = lifecycle_from_result(&observed)?;
                if let Some(record) = &recovery {
                    service
                        .github
                        .recovery_acknowledge(resource, record.capture.operation_id.clone())
                        .await?;
                }
                Ok(ReviewLifecycleDelivery {
                    lifecycle,
                    recovery: None,
                    fresh_required: false,
                })
            }
            .await;
            drop(guard);
            let _ = sender.send(result);
        })?;
        receiver
            .await
            .context("review lifecycle reconciliation ended without a result")?
    }

    /// Resolves a captured lifecycle record. Closing an unknown outcome requires a later read.
    pub async fn resolve_lifecycle(
        &self,
        id: &DocumentId,
        operation_id: String,
        resolution: forge_github::review_mutation::RecoveryResolution,
    ) -> Result<ReviewLifecycleDelivery> {
        use forge_github::recovery::RecoveryPhase;

        let owner = self.owner(id)?;
        let target = owner
            .document
            .lock()
            .expect("review document poisoned")
            .target
            .clone();
        let resource = lifecycle_resource(&target);
        let record = self
            .github
            .recovery_inspect(resource.clone())
            .await?
            .context("review lifecycle has no recovery record")?;
        ensure!(
            record.capture.operation_id == operation_id && lifecycle_record(&record),
            "review lifecycle recovery does not match the captured operation"
        );
        let close_unknown = matches!(
            resolution,
            forge_github::review_mutation::RecoveryResolution::CloseUnknown
        );
        let record = self
            .github
            .recovery_resolve(owner.remote.clone(), resource, operation_id, resolution)
            .await?;
        if close_unknown || matches!(record.state, RecoveryPhase::UserClosedUnknown { .. }) {
            return Ok(ReviewLifecycleDelivery {
                lifecycle: None,
                recovery: Some(record),
                fresh_required: true,
            });
        }
        self.lifecycle_reconcile(id).await
    }

    /// Removes the view's document. Admitted work keeps its owner until its remote result settles.
    pub fn close_document(&self, id: &DocumentId) -> Result<()> {
        self.state
            .lock()
            .expect("review registry poisoned")
            .document
            .remove(id)
            .context("unknown review document")?;
        Ok(())
    }

    pub fn close(&self) {
        let mut state = self.state.lock().expect("review registry poisoned");
        state.closed = true;
        state.document.clear();
        self.document_admission.close();
        self.job_admission.close();
    }

    /// Retains unfinished work at the deadline instead of aborting remote writes or their settlement.
    pub async fn shutdown(&self, timeout: Duration) -> ReviewShutdown {
        self.close();
        let deadline = tokio::time::Instant::now() + timeout;
        loop {
            let report = {
                let mut state = self.state.lock().expect("review registry poisoned");
                reap(&mut state);
                ReviewShutdown {
                    unfinished_jobs: state.task.len(),
                    failed_jobs: state.failed_jobs,
                }
            };
            if report.unfinished_jobs == 0 || tokio::time::Instant::now() >= deadline {
                return report;
            }
            tokio::time::sleep(Duration::from_millis(5)).await;
        }
    }

    fn owner(&self, id: &DocumentId) -> Result<Arc<DocumentOwner>> {
        let state = self.state.lock().expect("review registry poisoned");
        ensure!(!state.closed, "review service is closed");
        state
            .document
            .get(id)
            .cloned()
            .context("unknown review document")
    }

    fn remote_guard(&self, id: &DocumentId) -> Result<RemoteGuard> {
        let owner = self.owner(id)?;
        ensure!(
            owner
                .remote_active
                .compare_exchange(false, true, Ordering::AcqRel, Ordering::Acquire)
                .is_ok(),
            "review remote operation is already running"
        );
        Ok(RemoteGuard { owner })
    }

    fn spawn(&self, work: impl Future<Output = ()> + Send + 'static) -> Result<()> {
        tokio::runtime::Handle::try_current()
            .context("review remote operation requires a Tokio executor")?;
        let mut state = self.state.lock().expect("review registry poisoned");
        ensure!(!state.closed, "review service is closed");
        reap(&mut state);
        ensure!(state.task.len() < MAX_JOBS, "review task admission is full");
        state.task.spawn(work);
        Ok(())
    }
}

fn lifecycle_resource(target: &PullRequestTarget) -> forge_github::recovery::RecoveryResource {
    forge_github::recovery::RecoveryResource {
        repository: target.repository.clone(),
        kind: forge_github::recovery::RecoveryResourceKind::PullRequest,
        number: target.number,
    }
}

fn lifecycle_record(record: &forge_github::recovery::RecoveryRecord) -> bool {
    record.capture.operation == forge_github::recovery::RecoveryOperation::PullRequestTransition
}

fn lifecycle_record_is_terminal(record: &forge_github::recovery::RecoveryRecord) -> bool {
    lifecycle_record(record)
        && matches!(
            record.state,
            forge_github::recovery::RecoveryPhase::Confirmed { .. }
                | forge_github::recovery::RecoveryPhase::Rejected { .. }
                | forge_github::recovery::RecoveryPhase::UserLinked { .. }
                | forge_github::recovery::RecoveryPhase::UserClosedUnknown { .. }
        )
}

fn lifecycle_from_result(result: &PullRequestResult) -> Result<Option<ReviewLifecycle>> {
    match (result.state, result.is_draft) {
        (Some(state), Some(is_draft)) => Ok(Some(ReviewLifecycle { state, is_draft })),
        (None, None) => Ok(None),
        _ => anyhow::bail!("review lifecycle observation is incomplete"),
    }
}

fn reap(state: &mut ServiceState) {
    while let Some(result) = state.task.try_join_next() {
        if result.is_err() {
            state.failed_jobs = state.failed_jobs.saturating_add(1);
        }
    }
}
