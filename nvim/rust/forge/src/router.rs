use std::{path::PathBuf, sync::Arc};

use anyhow::{Context, Result, ensure};
use base64::{Engine, engine::general_purpose::STANDARD};
use forge_buffer::identity::DocumentId;
use forge_git::{completion::RevisionCandidates, repository::RepositoryState};
use forge_github::{
    comment::{CommentOperation, CommentTarget},
    issue_store::IssueStore,
    metadata::MetadataRequest,
    pull_request::PullRequestRequest,
    remote::IssueDetailRequest,
    service::IssueOperation,
    sync::{SyncProgress, SyncRequest},
};
use forge_harness::protocol::RuntimeMethod;
use forge_protocol::message::{DocumentEvent, Message, RequestEvent, Response};
use forge_protocol::outbound::MessageSender;
use forge_review::edit::RegionEdit;
use forge_review::review::ReviewCommentCommand;
use serde::{Deserialize, Serialize};
use serde_json::{Value, json};

use crate::{host::RoutedRequestEnvelope, runtime::ForgeRuntime};

#[path = "github_routes.rs"]
mod github_routes;
#[path = "issue_routes.rs"]
mod issue_routes;
#[path = "notification_routes.rs"]
mod notification_routes;
#[path = "walkthrough_routes.rs"]
mod walkthrough_routes;

const PAGE_BYTES: usize = 128 * 1024;

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct HostInitialize {
    protocol_version: u32,
    recovery_directory: Option<PathBuf>,
    status_ignored_directory: Option<PathBuf>,
    git_config_cache_path: Option<PathBuf>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct RevisionRequest {
    workspace: PathBuf,
    revision: Option<u64>,
    repository: Option<String>,
    reference_digest: Option<String>,
    #[serde(default)]
    offset: usize,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct IssueRequest {
    database: PathBuf,
    repo: String,
    request: IssueOperation,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct IssueSyncRequest {
    database: PathBuf,
    directory: PathBuf,
    request: SyncRequest,
    #[serde(default)]
    progress: bool,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct IssueDetailParams {
    database: PathBuf,
    directory: PathBuf,
    request: IssueDetailRequest,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct MetadataParams {
    cache_directory: PathBuf,
    directory: PathBuf,
    request: MetadataRequest,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct PullRequestParams {
    directory: PathBuf,
    request: PullRequestRequest,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct CommentParams {
    directory: PathBuf,
    target: CommentTarget,
    request: CommentOperation,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct ReviewOpenParams {
    initial: Option<serde_json::Value>,
    directory: PathBuf,
    target: Option<forge_github::pull_request::PullRequestTarget>,
    repository: Option<forge_github::model::GithubRepositoryId>,
    number: Option<u64>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct ReviewDocumentParams {
    document: DocumentId,
    #[serde(default)]
    capture: Vec<RegionEdit>,
    #[serde(default)]
    draft_comment: Vec<forge_review::comments::DraftComment>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct ReviewRenderParams {
    document: DocumentId,
    width: forge_buffer::width::WidthProfile,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct ReviewSectionParams {
    document: DocumentId,
    directory: PathBuf,
    section: forge_review::service::ReviewSectionKind,
    cursor: Option<String>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct ReviewLoadParams {
    document: DocumentId,
    directory: PathBuf,
    initial: Option<serde_json::Value>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct ReviewFileParams {
    document: DocumentId,
    directory: PathBuf,
    path: String,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct ReviewFilePageParams {
    document: DocumentId,
    path: String,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct ReviewThreadParams {
    document: DocumentId,
    directory: PathBuf,
    thread_node_id: String,
    cursor: Option<String>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct ReviewViewParams {
    document: DocumentId,
    view: forge_buffer::identity::ViewId,
    width: forge_buffer::width::WidthProfile,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct ReviewInputParams {
    input: forge_buffer::input::DocumentInput,
    directory: PathBuf,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct ReviewTransitionParams {
    document: DocumentId,
    desired: forge_github::pull_request::DesiredPullRequestState,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct ReviewLifecycleResolveParams {
    document: DocumentId,
    operation_id: String,
    resolution: forge_github::review_mutation::RecoveryResolution,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct ReviewSetViewedParams {
    document: DocumentId,
    path: String,
    viewed: bool,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct ReviewBatchSubmitParams {
    document: DocumentId,
    verdict: forge_review::review::ReviewVerdict,
    #[serde(default)]
    capture: Vec<RegionEdit>,
    #[serde(default)]
    draft_comment: Vec<forge_review::comments::DraftComment>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct ReviewBatchRecoverParams {
    document: DocumentId,
    operation_id: String,
    resolution: forge_github::review_mutation::RecoveryResolution,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct StatusNavigationParams {
    input: forge_status::StatusInput,
    forward: bool,
}

#[derive(Deserialize)]
#[serde(tag = "operation", rename_all = "snake_case", deny_unknown_fields)]
enum SessionSourceRequest {
    OpenDiff {
        document: DocumentId,
        input: forge_buffer::input::DocumentInput,
    },
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct ReviewCommentParams {
    document: DocumentId,
    command: ReviewCommentCommand,
    #[serde(default)]
    capture: Vec<RegionEdit>,
    #[serde(default)]
    draft_comment: Vec<forge_review::comments::DraftComment>,
}

#[derive(Deserialize)]
#[serde(tag = "operation", rename_all = "snake_case", deny_unknown_fields)]
enum StatusRequest {
    Local {
        document: DocumentId,
        filename: PathBuf,
    },
    Comparison {
        document: DocumentId,
        workspace: PathBuf,
        reference: String,
        worktree: bool,
        path: Option<String>,
        diff_view: Option<forge_diff::view::DiffViewOptions>,
    },
    BodySnapshot {
        document: DocumentId,
        file: u64,
        generation: u64,
    },
    Open {
        document: DocumentId,
        workspace: PathBuf,
    },
    Snapshot {
        document: DocumentId,
    },
    Refresh {
        document: DocumentId,
    },
    Demand {
        input: forge_status::StatusInput,
    },
    DiffView {
        input: forge_status::StatusInput,
        #[serde(default)]
        related: Vec<DocumentId>,
    },
    SourceSection {
        input: forge_status::StatusInput,
    },
    OpenTarget {
        input: forge_status::StatusInput,
    },
    Input {
        input: forge_status::StatusInput,
        selection: Option<forge_status::StatusSelection>,
    },
    CloseView {
        document: DocumentId,
        view: forge_buffer::identity::ViewId,
    },
    Close {
        document: DocumentId,
    },
}

#[derive(Deserialize)]
#[serde(tag = "operation", rename_all = "snake_case", deny_unknown_fields)]
enum SourceRequest {
    Open {
        document: DocumentId,
        workspace: PathBuf,
        revision: String,
        path: String,
    },
    Demand {
        input: forge_buffer::input::DocumentInput,
    },
    Snapshot {
        document: DocumentId,
    },
    CloseView {
        document: DocumentId,
        view: forge_buffer::identity::ViewId,
    },
    Close {
        document: DocumentId,
    },
}

#[derive(Deserialize)]
#[serde(tag = "operation", rename_all = "snake_case", deny_unknown_fields)]
enum StatusContextRequest {
    Action {
        input: forge_status::StatusInput,
    },
    Issues {
        input: forge_status::StatusInput,
        text: String,
    },
    CommitMessage {
        document: DocumentId,
        workspace: PathBuf,
        oid: String,
    },
    AboutMessage {
        document: DocumentId,
        text: String,
    },
}

pub(crate) enum RoutedMethod {
    Github(github_routes::GithubRoute),
    SessionInitialize,
    SessionDocument,
    RepositoryRevisionList,
    RepositoryWrite,
    RepositoryGeneration,
    BackgroundSettings,
    BackgroundSessionDocument,
    RepositoryStatusDocument,
    RepositoryStatusContext,
    RepositoryStatusNavigate,
    RepositoryWalkthroughDocument,
    GithubNotificationDocument,
    RepositorySourceDocument,
    GithubIssueList,
    GithubIssueSync,
    GithubIssueGet,
    GithubRepositoryMetadata,
    GithubPullRequest,
    GithubComment,
    GithubReviewDocumentOpen,
    GithubReviewDocumentRender,
    GithubReviewDocumentSectionGet,
    GithubReviewDocumentLoad,
    GithubReviewDocumentHeaderLoad,
    GithubReviewDocumentFileGet,
    GithubReviewDocumentFilePageGet,
    GithubReviewDocumentThreadGet,
    GithubReviewDocumentViewSet,
    GithubReviewDocumentInputApply,
    GithubReviewDocumentTransition,
    GithubReviewDocumentLifecycleReconcile,
    GithubReviewDocumentLifecycleResolve,
    GithubReviewDocumentBatchBegin,
    GithubReviewDocumentFileViewedSet,
    GithubReviewDocumentBatchSubmit,
    GithubReviewDocumentBatchRecover,
    GithubIssueDocument,
    GithubReviewDocumentSave,
    GithubReviewDocumentReconcile,
    GithubReviewDocumentClose,
    GithubReviewDocumentComment,
    HostShutdown,
    Runtime(RuntimeMethod),
}

fn decode_method(method: &str) -> Result<RoutedMethod> {
    match method {
        "github.actor.get" => Ok(RoutedMethod::Github(github_routes::GithubRoute::ActorGet)),
        "github.notification.list" => Ok(RoutedMethod::Github(
            github_routes::GithubRoute::NotificationList,
        )),
        "github.review.submission.context" => Ok(RoutedMethod::Github(
            github_routes::GithubRoute::ReviewSubmissionContext,
        )),
        "github.pull_request.creation.context" => Ok(RoutedMethod::Github(
            github_routes::GithubRoute::PullRequestCreationContext,
        )),
        "github.review.mutate" => Ok(RoutedMethod::Github(
            github_routes::GithubRoute::ReviewMutate,
        )),
        "github.recovery.inspect" => Ok(RoutedMethod::Github(
            github_routes::GithubRoute::RecoveryInspect,
        )),
        "github.recovery.resolve" => Ok(RoutedMethod::Github(
            github_routes::GithubRoute::RecoveryResolve,
        )),
        "github.recovery.acknowledge" => Ok(RoutedMethod::Github(
            github_routes::GithubRoute::RecoveryAcknowledge,
        )),
        "github.recovery.draft.settle" => Ok(RoutedMethod::Github(
            github_routes::GithubRoute::RecoveryDraftSettle,
        )),
        "github.review.draft.get" => Ok(RoutedMethod::Github(
            github_routes::GithubRoute::ReviewDraftGet,
        )),
        "github.review.draft.save" => Ok(RoutedMethod::Github(
            github_routes::GithubRoute::ReviewDraftSave,
        )),
        "session.initialize" => Ok(RoutedMethod::SessionInitialize),
        "session.document" => Ok(RoutedMethod::SessionDocument),
        "repository.revision.list" => Ok(RoutedMethod::RepositoryRevisionList),
        "repository.write" => Ok(RoutedMethod::RepositoryWrite),
        "repository.generation" => Ok(RoutedMethod::RepositoryGeneration),
        "background.settings" => Ok(RoutedMethod::BackgroundSettings),
        "background.session.document" => Ok(RoutedMethod::BackgroundSessionDocument),
        "repository.status.document" => Ok(RoutedMethod::RepositoryStatusDocument),
        "repository.status.context" => Ok(RoutedMethod::RepositoryStatusContext),
        "repository.status.navigate" => Ok(RoutedMethod::RepositoryStatusNavigate),
        "repository.walkthrough.document" => Ok(RoutedMethod::RepositoryWalkthroughDocument),
        "github.notification.document" => Ok(RoutedMethod::GithubNotificationDocument),
        "github.issue.document" => Ok(RoutedMethod::GithubIssueDocument),
        "repository.source.document" => Ok(RoutedMethod::RepositorySourceDocument),
        "github.issue.list" => Ok(RoutedMethod::GithubIssueList),
        "github.issue.sync" => Ok(RoutedMethod::GithubIssueSync),
        "github.issue.get" => Ok(RoutedMethod::GithubIssueGet),
        "github.repository.metadata" => Ok(RoutedMethod::GithubRepositoryMetadata),
        "github.pull_request" => Ok(RoutedMethod::GithubPullRequest),
        "github.comment" => Ok(RoutedMethod::GithubComment),
        "github.review.document.open" => Ok(RoutedMethod::GithubReviewDocumentOpen),
        "github.review.document.render" => Ok(RoutedMethod::GithubReviewDocumentRender),
        "github.review.document.section.get" => Ok(RoutedMethod::GithubReviewDocumentSectionGet),
        "github.review.document.load" => Ok(RoutedMethod::GithubReviewDocumentLoad),
        "github.review.document.header.load" => Ok(RoutedMethod::GithubReviewDocumentHeaderLoad),
        "github.review.document.file.get" => Ok(RoutedMethod::GithubReviewDocumentFileGet),
        "github.review.document.file.page.get" => Ok(RoutedMethod::GithubReviewDocumentFilePageGet),
        "github.review.document.thread.get" => Ok(RoutedMethod::GithubReviewDocumentThreadGet),
        "github.review.document.view.set" => Ok(RoutedMethod::GithubReviewDocumentViewSet),
        "github.review.document.input.apply" => Ok(RoutedMethod::GithubReviewDocumentInputApply),
        "github.review.document.transition" => Ok(RoutedMethod::GithubReviewDocumentTransition),
        "github.review.document.lifecycle.reconcile" => {
            Ok(RoutedMethod::GithubReviewDocumentLifecycleReconcile)
        }
        "github.review.document.lifecycle.resolve" => {
            Ok(RoutedMethod::GithubReviewDocumentLifecycleResolve)
        }
        "github.review.document.batch.begin" => Ok(RoutedMethod::GithubReviewDocumentBatchBegin),
        "github.review.document.file.viewed.set" => {
            Ok(RoutedMethod::GithubReviewDocumentFileViewedSet)
        }
        "github.review.document.batch.submit" => Ok(RoutedMethod::GithubReviewDocumentBatchSubmit),
        "github.review.document.batch.recover" => {
            Ok(RoutedMethod::GithubReviewDocumentBatchRecover)
        }
        "github.review.document.save" => Ok(RoutedMethod::GithubReviewDocumentSave),
        "github.review.document.reconcile" => Ok(RoutedMethod::GithubReviewDocumentReconcile),
        "github.review.document.close" => Ok(RoutedMethod::GithubReviewDocumentClose),
        "github.review.document.comment" => Ok(RoutedMethod::GithubReviewDocumentComment),
        "host.shutdown" => Ok(RoutedMethod::HostShutdown),
        "host.initialize" => anyhow::bail!("Forge host is already initialized"),
        method => RuntimeMethod::decode(method).map(RoutedMethod::Runtime),
    }
}

enum StatusSubscription {
    Idle,
    Active {
        stop: tokio::sync::oneshot::Sender<()>,
        task: tokio::task::JoinHandle<Result<()>>,
    },
    Closed,
}

/// Routes repository requests without creating a provider or durable Harness session.
pub(crate) struct HostRouter {
    host: Arc<ForgeRuntime>,
    status_events: std::sync::Mutex<StatusSubscription>,
    #[cfg(test)]
    pub(crate) preparation_hold: std::sync::Mutex<Option<PreparationHold>>,
}

#[cfg(test)]
pub(crate) struct PreparationHold {
    pub(crate) request_id: u64,
    pub(crate) arrived: tokio::sync::oneshot::Sender<()>,
    pub(crate) released: tokio::sync::oneshot::Receiver<()>,
}

impl HostRouter {
    pub(crate) fn new(host: Arc<ForgeRuntime>) -> Self {
        Self {
            host,
            status_events: std::sync::Mutex::new(StatusSubscription::Idle),
            #[cfg(test)]
            preparation_hold: std::sync::Mutex::new(None),
        }
    }

    pub(crate) async fn prepare(
        &self,
        envelope: &mut RoutedRequestEnvelope,
    ) -> Result<RoutedMethod> {
        #[cfg(test)]
        {
            let held = {
                let mut hold = self.preparation_hold.lock().unwrap();
                if hold
                    .as_ref()
                    .is_some_and(|hold| hold.request_id == envelope.request.id)
                {
                    hold.take()
                } else {
                    None
                }
            };
            if let Some(held) = held {
                let _ = held.arrived.send(());
                held.released.await?;
            }
        }
        envelope.validate_context()?;
        let mut method = decode_method(&envelope.request.method)?;
        if matches!(method, RoutedMethod::Runtime(_)) {
            self.host
                .harness
                .normalize_request(envelope.session_id.as_deref(), &mut envelope.request)
                .await?;
            method = decode_method(&envelope.request.method)?;
        }
        if let RoutedMethod::Runtime(harness_method) = method {
            // Queued prompts wait in their request task so the input lane can still cancel work.
            if harness_method == RuntimeMethod::SessionPromptSubmit
                && envelope.request.params["queued"].as_bool() == Some(true)
            {
                return Ok(method);
            }
            if harness_method == RuntimeMethod::SessionCommandApply {
                self.host
                    .harness
                    .prepare_task(envelope.session_id.as_deref(), &envelope.request)
                    .await?;
            }
            envelope.harness_admission = self
                .host
                .harness
                .prepare(envelope.session_id.as_deref(), harness_method)
                .await?;
        }
        Ok(method)
    }

    pub(crate) async fn route(
        &self,
        envelope: RoutedRequestEnvelope,
        method: RoutedMethod,
        sink: &MessageSender,
    ) -> Result<()> {
        {
            let mut subscription = self
                .status_events
                .lock()
                .map_err(|_| anyhow::anyhow!("Forge status subscription owner poisoned"))?;
            match &*subscription {
                StatusSubscription::Idle => {
                    let updates = self.host.status.subscribe_updates();
                    let (stop, stopped) = tokio::sync::oneshot::channel();
                    let sink = sink.clone();
                    let task = tokio::spawn(forward_status_updates(sink, updates, stopped));
                    *subscription = StatusSubscription::Active { stop, task };
                }
                StatusSubscription::Active { .. } => {}
                StatusSubscription::Closed => anyhow::bail!("Forge status subscription is closed"),
            }
        }
        let harness_admission = envelope.harness_admission;
        let request = envelope.request;
        match method {
            RoutedMethod::RepositoryStatusNavigate => {
                let params: StatusNavigationParams = serde_json::from_value(request.params)?;
                let result = self
                    .host
                    .status
                    .navigate(params.input, params.forward)
                    .await?;
                send_completed_result(sink, request.id, result).await?;
            }
            RoutedMethod::SessionDocument => {
                if request.params.get("operation").and_then(Value::as_str) == Some("open_diff") {
                    let SessionSourceRequest::OpenDiff { document, input } =
                        serde_json::from_value(request.params)?;
                    let text = self
                        .host
                        .harness
                        .saved_diff(envelope.session_id, input)
                        .await?;
                    let result = self
                        .host
                        .sources
                        .open_saved_diff(document, "Saved diff".into(), text)
                        .await?;
                    send_completed_result(sink, request.id, result).await?;
                    return Ok(());
                }
                let params = serde_json::from_value(request.params)?;
                let result = self
                    .host
                    .harness
                    .document(envelope.session_id, params)
                    .await?;
                send_completed_result(sink, request.id, result).await?;
            }
            RoutedMethod::RepositoryStatusContext => {
                let params: StatusContextRequest = serde_json::from_value(request.params)?;
                let result = match params {
                    StatusContextRequest::Action { input } => {
                        serde_json::to_value(self.host.status.context_action(input).await?)?
                    }
                    StatusContextRequest::Issues { input, text } => {
                        let ticket = self.host.status.context_issues(input, &text).await?;
                        let outcome = ticket.finish().await?;
                        crate::runtime::repository_write::outcome_value(&outcome)
                    }
                    StatusContextRequest::CommitMessage {
                        document,
                        workspace,
                        oid,
                    } => {
                        let title = format!("Commit {oid}");
                        let mut text = self
                            .host
                            .status
                            .context_commit_message(workspace, oid)
                            .await?;
                        text.push_str("\n\n");
                        serde_json::to_value(self.host.sources.open_text(
                            document,
                            title,
                            text.into_bytes(),
                        )?)?
                    }
                    StatusContextRequest::AboutMessage { document, text } => serde_json::to_value(
                        self.host
                            .sources
                            .open_text(document, "About".into(), text.into_bytes())?,
                    )?,
                };
                send_completed_result(sink, request.id, result).await?;
            }
            RoutedMethod::RepositorySourceDocument => {
                let params: SourceRequest = serde_json::from_value(request.params)?;
                let result = match params {
                    SourceRequest::Open {
                        document,
                        workspace,
                        revision,
                        path,
                    } => {
                        let path = STANDARD
                            .decode(path)
                            .context("source path is not valid base64")?;
                        serde_json::to_value(
                            self.host
                                .sources
                                .open(document, workspace, revision, path)
                                .await?,
                        )?
                    }
                    SourceRequest::Demand { input } => {
                        serde_json::to_value(self.host.sources.demand(input).await?)?
                    }
                    SourceRequest::Snapshot { document } => {
                        serde_json::to_value(self.host.sources.snapshot_state(&document).await?)?
                    }
                    SourceRequest::CloseView { document, view } => {
                        self.host.sources.close_view(&document, &view).await?;
                        json!({"closed":true})
                    }
                    SourceRequest::Close { document } => {
                        json!({"closed":self.host.sources.close_collected(&document).await})
                    }
                };
                send_completed_result(sink, request.id, result).await?;
            }
            RoutedMethod::Github(operation) => {
                let result = github_routes::route(&self.host, operation, request.params).await?;
                send_completed_result(sink, request.id, result).await?;
            }
            RoutedMethod::RepositoryGeneration => {
                let result = self
                    .host
                    .generation
                    .dispatch(
                        request.params,
                        &self.host.harness,
                        Some((sink.clone(), request.id)),
                        envelope.generation_lease,
                    )
                    .await?;
                send_completed_result(sink, request.id, result).await?;
            }
            RoutedMethod::BackgroundSessionDocument => {
                let result = self
                    .host
                    .harness
                    .background
                    .timeline(request.params)
                    .await?;
                send_completed_result(sink, request.id, result).await?;
            }
            RoutedMethod::BackgroundSettings => {
                let result = crate::runtime::commit_message::settings(request.params).await?;
                send_completed_result(sink, request.id, result).await?;
            }
            RoutedMethod::GithubNotificationDocument => {
                let result = notification_routes::route(&self.host, request.params).await?;
                send_completed_result(sink, request.id, result).await?;
            }
            RoutedMethod::GithubIssueDocument => {
                let result = issue_routes::route(&self.host, request.params).await?;
                send_completed_result(sink, request.id, result).await?;
            }
            RoutedMethod::RepositoryWalkthroughDocument => {
                let result = walkthrough_routes::route(&self.host, request.params).await?;
                send_completed_result(sink, request.id, result).await?;
            }
            RoutedMethod::RepositoryStatusDocument => {
                let params = serde_json::from_value(request.params)?;
                let result = match params {
                    StatusRequest::Local { document, filename } => {
                        ensure!(
                            filename.is_absolute(),
                            "local diff requires an absolute file path"
                        );
                        serde_json::to_value(
                            self.host.status.open_local(document, filename).await?,
                        )?
                    }
                    StatusRequest::Comparison {
                        document,
                        workspace,
                        reference,
                        worktree,
                        path,
                        diff_view,
                    } => {
                        let path = path
                            .map(|path| {
                                forge_git::identity::RepositoryPath::new(
                                    STANDARD
                                        .decode(path)
                                        .context("comparison path is not valid base64")?,
                                )
                            })
                            .transpose()?;
                        serde_json::to_value(
                            self.host
                                .status
                                .open_comparison(
                                    document,
                                    workspace,
                                    forge_git::revision::comparison::ComparisonRequest {
                                        reference,
                                        head: None,
                                        worktree,
                                        path,
                                    },
                                    diff_view.unwrap_or_default(),
                                )
                                .await?,
                        )?
                    }
                    StatusRequest::Open {
                        document,
                        workspace,
                    } => {
                        let (snapshot, timing) = self
                            .host
                            .status
                            .open_with_timing(document, workspace)
                            .await?;
                        let serialization_started = std::time::Instant::now();
                        let result = serde_json::to_value(snapshot)?;
                        sink.send(Message::RequestEvent(RequestEvent {
                            request_id: request.id,
                            event: forge_protocol::contract::RequestEventName::RepositoryStatusDocumentProgress.as_str().into(),
                            payload: json!({
                                "phase":"open",
                                "native":timing,
                                "router_serialization_ms":serialization_started.elapsed().as_millis(),
                            }),
                        }))?;
                        result
                    }
                    StatusRequest::Snapshot { document } => {
                        serde_json::to_value(self.host.status.snapshot(&document).await?)?
                    }
                    StatusRequest::Refresh { document } => {
                        serde_json::to_value(self.host.status.refresh(&document).await?)?
                    }
                    StatusRequest::Demand { input } => {
                        serde_json::to_value(self.host.status.demand(input).await?)?
                    }
                    StatusRequest::DiffView { input, related } => serde_json::to_value(
                        self.host.status.change_diff_views(input, related).await?,
                    )?,
                    StatusRequest::SourceSection { input } => {
                        serde_json::to_value(self.host.status.source_section(input).await?)?
                    }
                    StatusRequest::OpenTarget { input } => {
                        serde_json::to_value(self.host.status.open_target(input).await?)?
                    }
                    StatusRequest::Input { input, selection } => {
                        match self.host.status.act(input, selection).await? {
                            forge_status::StatusAction::Accepted(accepted, _) => {
                                serde_json::to_value(accepted)?
                            }
                            forge_status::StatusAction::Write(ticket) => {
                                let outcome = ticket.finish().await?;
                                crate::runtime::repository_write::outcome_value(&outcome)
                            }
                            forge_status::StatusAction::Projection(patch) => {
                                json!({"success":true,"patch":patch})
                            }
                        }
                    }
                    StatusRequest::CloseView { document, view } => {
                        serde_json::to_value(self.host.status.close_view(&document, &view).await?)?
                    }
                    StatusRequest::BodySnapshot {
                        document,
                        file,
                        generation,
                    } => serde_json::to_value(
                        self.host
                            .status
                            .body_snapshot(&document, file, generation)
                            .await?,
                    )?,
                    StatusRequest::Close { document } => {
                        json!({"closed":self.host.status.close_collected(&document).await})
                    }
                };
                send_completed_result(sink, request.id, result).await?;
            }
            RoutedMethod::RepositoryWrite => {
                let params = serde_json::from_value(request.params)?;
                let result = self.host.writes.request(params, request.id, sink).await?;
                send_completed_result(sink, request.id, result).await?;
            }
            RoutedMethod::GithubReviewDocumentComment => {
                let params: ReviewCommentParams = serde_json::from_value(request.params)?;
                let result = self
                    .host
                    .review
                    .comment(
                        &params.document,
                        params.capture,
                        params.draft_comment,
                        params.command,
                    )
                    .await?;
                send_completed_result(sink, request.id, result).await?;
            }
            RoutedMethod::GithubReviewDocumentOpen => {
                let params: ReviewOpenParams = serde_json::from_value(request.params)?;
                let remote = self
                    .host
                    .github_remote
                    .for_directory(params.directory.clone())?;
                let snapshot = match (params.target, params.repository, params.number) {
                    (Some(target), None, None) => {
                        self.host
                            .review
                            .open_pr_in_directory(params.directory, remote, target)
                            .await?
                    }
                    (None, Some(repository), Some(number)) => {
                        if let Some(initial) = params.initial {
                            self.host
                                .review
                                .open_pr_snapshot(
                                    params.directory,
                                    remote,
                                    repository,
                                    number,
                                    initial,
                                )
                                .await?
                        } else {
                            self.host
                                .review
                                .open_repository_pr(params.directory, remote, repository, number)
                                .await?
                        }
                    }
                    _ => anyhow::bail!(
                        "review open requires exactly one captured target or repository and number"
                    ),
                };
                send_completed_result(sink, request.id, snapshot).await?;
            }
            RoutedMethod::GithubReviewDocumentFileGet => {
                let params: ReviewFileParams = serde_json::from_value(request.params)?;
                let remote = self
                    .host
                    .github_remote
                    .for_directory(params.directory.clone())?;
                let result = self
                    .host
                    .review
                    .read_file(&params.document, params.directory, remote, params.path)
                    .await?;
                send_completed_result(sink, request.id, result).await?;
            }
            RoutedMethod::GithubReviewDocumentFilePageGet => {
                let params: ReviewFilePageParams = serde_json::from_value(request.params)?;
                let result = self
                    .host
                    .review
                    .read_file_more(&params.document, params.path)
                    .await?;
                send_completed_result(sink, request.id, result).await?;
            }
            RoutedMethod::GithubReviewDocumentThreadGet => {
                let params: ReviewThreadParams = serde_json::from_value(request.params)?;
                let remote = self
                    .host
                    .github_remote
                    .for_directory(params.directory.clone())?;
                let result = self
                    .host
                    .review
                    .read_thread(
                        &params.document,
                        params.directory,
                        remote,
                        params.thread_node_id,
                        params.cursor,
                    )
                    .await?;
                send_completed_result(sink, request.id, result).await?;
            }
            RoutedMethod::GithubReviewDocumentViewSet => {
                let params: ReviewViewParams = serde_json::from_value(request.params)?;
                let result = self
                    .host
                    .review
                    .view(&params.document, params.view, params.width)
                    .await?;
                send_completed_result(sink, request.id, result).await?;
            }
            RoutedMethod::GithubReviewDocumentInputApply => {
                let params: ReviewInputParams = serde_json::from_value(request.params)?;
                let result = self.host.review.act(params.input, params.directory).await?;
                send_completed_result(sink, request.id, result).await?;
            }
            RoutedMethod::GithubReviewDocumentTransition => {
                let params: ReviewTransitionParams = serde_json::from_value(request.params)?;
                let result = self
                    .host
                    .review
                    .transition(&params.document, params.desired)
                    .await?;
                send_completed_result(sink, request.id, result).await?;
            }
            RoutedMethod::GithubReviewDocumentLifecycleReconcile => {
                let params: ReviewDocumentParams = serde_json::from_value(request.params)?;
                let result = self
                    .host
                    .review
                    .lifecycle_reconcile(&params.document)
                    .await?;
                send_completed_result(sink, request.id, result).await?;
            }
            RoutedMethod::GithubReviewDocumentLifecycleResolve => {
                let params: ReviewLifecycleResolveParams = serde_json::from_value(request.params)?;
                let result = self
                    .host
                    .review
                    .resolve_lifecycle(&params.document, params.operation_id, params.resolution)
                    .await?;
                send_completed_result(sink, request.id, result).await?;
            }
            RoutedMethod::GithubReviewDocumentBatchBegin => {
                let params: ReviewDocumentParams = serde_json::from_value(request.params)?;
                let result = self.host.review.begin_batched(&params.document).await?;
                send_completed_result(sink, request.id, result).await?;
            }
            RoutedMethod::GithubReviewDocumentFileViewedSet => {
                let params: ReviewSetViewedParams = serde_json::from_value(request.params)?;
                let result = self
                    .host
                    .review
                    .set_viewed(&params.document, params.path, params.viewed)
                    .await?;
                send_completed_result(sink, request.id, result).await?;
            }
            RoutedMethod::GithubReviewDocumentBatchSubmit => {
                let params: ReviewBatchSubmitParams = serde_json::from_value(request.params)?;
                let result = self
                    .host
                    .review
                    .submit_batched(
                        &params.document,
                        params.capture,
                        params.draft_comment,
                        params.verdict,
                    )
                    .await?;
                send_completed_result(sink, request.id, result).await?;
            }
            RoutedMethod::GithubReviewDocumentBatchRecover => {
                let params: ReviewBatchRecoverParams = serde_json::from_value(request.params)?;
                let result = self
                    .host
                    .review
                    .recover_batched_submission(
                        &params.document,
                        params.operation_id,
                        params.resolution,
                    )
                    .await?;
                send_completed_result(sink, request.id, result).await?;
            }
            RoutedMethod::GithubReviewDocumentHeaderLoad => {
                let params: ReviewLoadParams = serde_json::from_value(request.params)?;
                if let Some(initial) = params.initial {
                    self.host.review.retain_pr_snapshot(
                        &params.document,
                        &params.directory,
                        initial,
                    )?;
                }
                let remote = self
                    .host
                    .github_remote
                    .for_directory(params.directory.clone())?;
                self.host
                    .review
                    .header(&params.document, params.directory, remote)
                    .await?;
                let timing = self.host.review.section_timings(&params.document)?;
                send_completed_result(
                    sink,
                    request.id,
                    serde_json::json!({ "ready": true, "timing": timing }),
                )
                .await?;
            }
            RoutedMethod::GithubReviewDocumentLoad => {
                let params: ReviewLoadParams = serde_json::from_value(request.params)?;
                let remote = self
                    .host
                    .github_remote
                    .for_directory(params.directory.clone())?;
                let diagnostic = self
                    .host
                    .review
                    .load(&params.document, params.directory, remote)
                    .await;
                let timing = self.host.review.section_timings(&params.document)?;
                send_completed_result(
                    sink,
                    request.id,
                    serde_json::json!({ "diagnostic": diagnostic, "timing": timing }),
                )
                .await?;
            }
            RoutedMethod::GithubReviewDocumentSectionGet => {
                let params: ReviewSectionParams = serde_json::from_value(request.params)?;
                let remote = self
                    .host
                    .github_remote
                    .for_directory(params.directory.clone())?;
                let result = self
                    .host
                    .review
                    .read_section(
                        &params.document,
                        params.directory,
                        remote,
                        params.section,
                        params.cursor,
                    )
                    .await?;
                send_completed_result(sink, request.id, result).await?;
            }
            RoutedMethod::GithubReviewDocumentRender => {
                let params: ReviewRenderParams = serde_json::from_value(request.params)?;
                let result = self
                    .host
                    .review
                    .materialize(&params.document, params.width)
                    .await?;
                send_completed_result(sink, request.id, result).await?;
            }
            RoutedMethod::GithubReviewDocumentSave => {
                let params: ReviewDocumentParams = serde_json::from_value(request.params)?;
                send_completed_result(
                    sink,
                    request.id,
                    self.host
                        .review
                        .save(&params.document, params.capture, params.draft_comment)
                        .await?,
                )
                .await?;
            }
            RoutedMethod::GithubReviewDocumentReconcile => {
                let params: ReviewDocumentParams = serde_json::from_value(request.params)?;
                send_completed_result(
                    sink,
                    request.id,
                    self.host.review.reconcile(&params.document).await?,
                )
                .await?;
            }
            RoutedMethod::GithubReviewDocumentClose => {
                let params: ReviewDocumentParams = serde_json::from_value(request.params)?;
                self.host.review.close_document(&params.document)?;
                sink.send(Message::Response(Response::success(
                    request.id,
                    json!({"closed":true}),
                )?))?;
            }
            RoutedMethod::GithubRepositoryMetadata => {
                let params: MetadataParams = serde_json::from_value(request.params)?;
                let remote = self.host.github_remote.for_directory(params.directory)?;
                let metadata = self
                    .host
                    .github
                    .metadata(remote, params.cache_directory, params.request)
                    .await?;
                send_completed_result(sink, request.id, metadata).await?;
            }
            RoutedMethod::GithubComment => {
                let params: CommentParams = serde_json::from_value(request.params)?;
                let remote = self.host.github_remote.for_directory(params.directory)?;
                let result = self
                    .host
                    .github
                    .comment(remote, params.target, params.request)
                    .await?;
                send_completed_result(sink, request.id, result).await?;
            }
            RoutedMethod::GithubPullRequest => {
                let params: PullRequestParams = serde_json::from_value(request.params)?;
                let remote = self.host.github_remote.for_directory(params.directory)?;
                let result = self
                    .host
                    .github
                    .pull_request(remote, params.request)
                    .await?;
                send_completed_result(sink, request.id, result).await?;
            }
            RoutedMethod::GithubIssueGet => {
                let params: IssueDetailParams = serde_json::from_value(request.params)?;
                let store = IssueStore::new(
                    params.database,
                    &params.request.repository.repository_name(),
                    std::time::Duration::from_secs(1),
                )?;
                let remote = self.host.github_remote.for_directory(params.directory)?;
                let detail = self
                    .host
                    .github
                    .fetch_detail(remote, store, params.request)
                    .await?;
                send_completed_result(sink, request.id, detail).await?;
            }
            RoutedMethod::GithubIssueSync => {
                let params: IssueSyncRequest = serde_json::from_value(request.params)?;
                let store = IssueStore::new(
                    params.database,
                    &params.request.repository.repository_name(),
                    std::time::Duration::from_secs(1),
                )?;
                let remote = self.host.github_remote.for_directory(params.directory)?;
                let (sender, mut progress) = tokio::sync::watch::channel(SyncProgress::default());
                let mut observing = params.progress;
                let sync = self.host.github.sync(
                    remote,
                    store,
                    params.request,
                    observing.then_some(sender),
                );
                tokio::pin!(sync);
                let result = loop {
                    tokio::select! {
                        biased;
                        result = &mut sync => break result?,
                        changed = progress.changed(), if observing => {
                            if changed.is_err() { observing = false; continue; }
                            let payload = serde_json::to_value(progress.borrow_and_update().clone())?;
                            sink.send(Message::RequestEvent(RequestEvent { request_id: request.id, event: forge_protocol::contract::RequestEventName::GithubIssueSyncProgress.as_str().into(), payload }))?;
                        }
                    }
                };
                sink.send(Message::Response(Response::success(request.id, result)?))?;
            }
            RoutedMethod::GithubIssueList => {
                let params: IssueRequest = serde_json::from_value(request.params)?;
                let store = IssueStore::new(
                    params.database,
                    &params.repo,
                    std::time::Duration::from_secs(1),
                )?;
                let result = self.host.github.execute(store, params.request).await?;
                send_completed_result(sink, request.id, result).await?;
            }
            RoutedMethod::SessionInitialize => {
                let services_only = request
                    .params
                    .get("services_only")
                    .map(|value| value.as_bool().context("services_only must be a boolean"))
                    .transpose()?
                    .unwrap_or(false);
                let initialize = serde_json::from_value(request.params)?;
                let response = if services_only {
                    self.host
                        .harness
                        .initialize_services(request.id, initialize)
                        .await?
                } else {
                    self.host
                        .harness
                        .open_session(request.id, initialize)
                        .await?
                };
                let opened_session = response
                    .result()
                    .and_then(|result| result.pointer("/session/id"))
                    .and_then(Value::as_str)
                    .map(str::to_owned);
                sink.send_response(response).await?;
                if let Some(session_id) = opened_session
                    && let Err(error) = self
                        .host
                        .harness
                        .start_provider_session(&session_id, sink)
                        .await
                {
                    sink.send_event(forge_protocol::message::SessionEvent {
                            session_id, event: "backend_event".into(),
                            payload: json!({"kind":"error","text":format!("Provider initialization failed: {error:#}")}),
                        }).await?;
                }
            }
            RoutedMethod::RepositoryRevisionList => {
                let params: RevisionRequest = serde_json::from_value(request.params)?;
                let repository = self
                    .host
                    .repositories
                    .open(params.workspace)
                    .await?
                    .context("revision completion requires a Git repository")?;
                let candidates = if let Some(revision) = params.revision {
                    let candidates = repository
                        .current_revisions()
                        .context("revision candidate snapshot was invalidated")?;
                    ensure!(
                        candidates.revision.0 == revision,
                        "revision candidate snapshot was superseded"
                    );
                    ensure!(
                        params.repository.as_deref()
                            == Some(
                                STANDARD
                                    .encode(
                                        repository
                                            .identity
                                            .git_directory
                                            .as_os_str()
                                            .as_encoded_bytes()
                                    )
                                    .as_str()
                            ),
                        "revision page repository identity changed"
                    );
                    ensure!(
                        params.reference_digest.as_deref()
                            == Some(candidates.reference_digest.to_string().as_str()),
                        "revision page source identity changed"
                    );
                    candidates
                } else {
                    ensure!(
                        params.offset == 0,
                        "initial revision request requires zero offset"
                    );
                    repository
                        .refresh_revisions(&self.host.repositories)
                        .await?
                };
                let page = revision_page(&repository, &candidates, params.offset)?;
                sink.send(Message::Response(Response::success(request.id, page)?))?;
            }
            RoutedMethod::HostShutdown => {
                anyhow::bail!("shutdown must route through the Forge connection host")
            }
            RoutedMethod::Runtime(_) => {
                self.host
                    .harness
                    .dispatch_admitted(envelope.session_id, request, sink, harness_admission)
                    .await?;
            }
        }
        Ok(())
    }

    /// Resolve status subscription ownership after request and feature producers quiesce.
    pub(crate) fn finish_status_subscription(
        &self,
    ) -> Result<Option<tokio::task::JoinHandle<Result<()>>>> {
        let mut subscription = self
            .status_events
            .lock()
            .map_err(|_| anyhow::anyhow!("Forge status subscription owner poisoned"))?;
        match std::mem::replace(&mut *subscription, StatusSubscription::Closed) {
            StatusSubscription::Active { stop, task } => {
                // Preserve task collection when it completed before the stop signal.
                let _ = stop.send(());
                Ok(Some(task))
            }
            StatusSubscription::Idle | StatusSubscription::Closed => Ok(None),
        }
    }
}

pub(crate) fn validate_initialize(params: &Value) -> Result<()> {
    let initialize: HostInitialize =
        serde_json::from_value(params.clone()).context("decode Forge host handshake")?;
    ensure!(
        initialize.protocol_version == forge_protocol::WIRE_VERSION,
        "Forge wire version mismatch"
    );
    if let Some(path) = initialize.git_config_cache_path {
        ensure!(
            path.is_absolute(),
            "Git configuration cache must be an absolute path"
        );
    }
    if let Some(directory) = initialize.recovery_directory {
        ensure!(
            directory.is_absolute(),
            "GitHub recovery directory must be an absolute path"
        );
    }
    if let Some(directory) = initialize.status_ignored_directory {
        ensure!(
            directory.is_absolute(),
            "Status ignored directory must be an absolute path"
        );
    }
    Ok(())
}

async fn forward_status_updates(
    sink: MessageSender,
    mut updates: tokio::sync::broadcast::Receiver<Arc<forge_status::StatusUpdate>>,
    mut stopped: tokio::sync::oneshot::Receiver<()>,
) -> Result<()> {
    use tokio::sync::broadcast::error::{RecvError, TryRecvError};

    let mut draining = false;
    loop {
        let update = if draining {
            match updates.try_recv() {
                Ok(update) => Ok(update),
                Err(TryRecvError::Closed | TryRecvError::Empty) => break,
                Err(TryRecvError::Lagged(count)) => Err(RecvError::Lagged(count)),
            }
        } else {
            tokio::select! {
                biased;
                result = &mut stopped => {
                    result.context("Forge status subscription stop owner disappeared")?;
                    draining = true;
                    continue;
                }
                update = updates.recv() => update,
            }
        };
        match update {
            Ok(update) => {
                if let Err(error) = send_status_update(&sink, &update).await {
                    sink.send_wait(Message::DocumentEvent(DocumentEvent {
                        document: update.document.0.clone(),
                        event: "status.resync".into(),
                        payload: json!({"diagnostic": [format!("Status update delivery requires recovery: {error:#}")]}),
                    })).await.with_context(|| format!("report status update delivery failure: {error:#}"))?;
                }
            }
            Err(RecvError::Closed) => break,
            Err(RecvError::Lagged(_)) => {
                sink.send_wait(Message::DocumentEvent(DocumentEvent {
                    document: String::new(),
                    event: "status.resync".into(),
                    payload: json!({}),
                }))
                .await
                .context("report lagged status subscription")?;
            }
        }
    }
    Ok(())
}

async fn send_status_update(
    sink: &MessageSender,
    update: &forge_status::StatusUpdate,
) -> Result<()> {
    let message = Message::DocumentEvent(DocumentEvent {
        document: update.document.0.clone(),
        event: "status.update".into(),
        payload: serde_json::to_value(update)?,
    });
    if forge_protocol::outbound::encode(&message, forge_protocol::MAX_FRAME_BYTES).is_ok() {
        sink.send_wait(message).await?;
        return Ok(());
    }
    let _permit = sink.begin_transfer().await?;
    let transfer = forge_protocol::transfer::JsonTransfer::new(&message)?;
    let complete =
        json!({"part_count": transfer.part_count(), "total_bytes": transfer.total_bytes()});
    for part in transfer {
        sink.send_wait(Message::DocumentEvent(DocumentEvent {
            document: update.document.0.clone(),
            event: "document.part".into(),
            payload: serde_json::to_value(part)?,
        }))
        .await?;
        tokio::task::yield_now().await;
    }
    sink.send_wait(Message::DocumentEvent(DocumentEvent {
        document: update.document.0.clone(),
        event: "document.complete".into(),
        payload: complete,
    }))
    .await?;
    Ok(())
}

async fn send_completed_result(
    sink: &MessageSender,
    request_id: u64,
    result: impl Serialize,
) -> Result<()> {
    Ok(sink
        .send_response(Response::success(request_id, result)?)
        .await?)
}

fn revision_page(
    repository: &RepositoryState,
    candidates: &RevisionCandidates,
    offset: usize,
) -> Result<Value> {
    ensure!(
        repository.generation() == candidates.generation,
        "revision candidate generation was invalidated"
    );
    ensure!(
        candidates.revision.0 <= 9_007_199_254_740_991,
        "revision candidate sequence exceeds wire range"
    );
    let count = candidates.values.len();
    let total = candidates.values.iter().try_fold(0usize, |total, value| {
        total
            .checked_add(value.len())
            .and_then(|bytes| bytes.checked_add(1))
            .context("revision candidate byte count overflow")
    })?;
    ensure!(
        offset <= total,
        "revision page offset exceeds snapshot size"
    );
    let mut payload = Vec::with_capacity(PAGE_BYTES.min(total - offset));
    let mut position = 0;
    for value in &candidates.values[..count] {
        for fragment in [value.as_slice(), b"\0".as_slice()] {
            let end = position + fragment.len();
            if end > offset && payload.len() < PAGE_BYTES {
                let start = offset.saturating_sub(position);
                let length = (fragment.len() - start).min(PAGE_BYTES - payload.len());
                payload.extend_from_slice(&fragment[start..start + length]);
            }
            position = end;
        }
        if payload.len() == PAGE_BYTES {
            break;
        }
    }
    Ok(json!({
        "revision": candidates.revision.0,
        "repository": STANDARD.encode(repository.identity.git_directory.as_os_str().as_encoded_bytes()),
        "reference_digest": candidates.reference_digest.to_string(),
        "offset": offset,
        "next_offset": offset + payload.len(),
        "total_bytes": total,
        "count": count,
        "data": STANDARD.encode(payload),
    }))
}

#[cfg(test)]
mod tests {
    use super::*;
    use forge_git::completion::{RevisionBackend, RevisionId};

    #[test]
    fn renamed_methods_preserve_dispatch() {
        for (method, _) in forge_protocol::contract::METHODS {
            if matches!(*method, "host.initialize" | "host.transport.consumed") {
                continue;
            }
            assert!(decode_method(method).is_ok(), "missing route for {method}");
        }
    }

    #[test]
    fn retired_method_names_are_rejected() {
        for method in [
            "initialize",
            "shutdown",
            "status",
            "walkthrough",
            "task.transition",
            "task.list",
            "task.operation",
            "goal.set",
            "review.open_pr",
            "queue.edit_last",
            "plan.deviation.resolve",
            "session.plan.deviation.resolve",
        ] {
            assert!(
                decode_method(method).is_err(),
                "retired route accepted: {method}"
            );
        }
    }

    #[tokio::test]
    async fn revision_pages_preserve_candidates_beyond_previous_cache_budget() {
        let directory = tempfile::Builder::new()
            .prefix(".forge-revision-")
            .tempdir_in(std::env::current_dir().unwrap())
            .unwrap();
        let initialized = std::process::Command::new("git")
            .arg("-C")
            .arg(directory.path())
            .args(["init", "--quiet"])
            .status()
            .unwrap();
        assert!(initialized.success());
        let store = forge_git::store::RepositoryStore::default();
        let repository = store
            .open(directory.path().to_path_buf())
            .await
            .unwrap()
            .unwrap();
        let values: Vec<Vec<u8>> = (0..20_001)
            .map(|index| format!("branch-{index}-{}", "x".repeat(100)).into_bytes())
            .collect();
        let expected: Vec<u8> = values
            .iter()
            .flat_map(|value| value.iter().copied().chain([0]))
            .collect();
        assert!(expected.len() > 2 * 1024 * 1024);
        let candidates = RevisionCandidates {
            revision: RevisionId(1),
            generation: repository.generation(),
            storage: repository.identity.storage.clone(),
            backend: RevisionBackend::Gix,
            reference_digest: "fixture".into(),
            retained_bytes: expected.len(),
            values,
        };
        let mut offset = 0;
        let mut actual = Vec::new();
        loop {
            let page = revision_page(&repository, &candidates, offset).unwrap();
            assert_eq!(page["count"], 20_001);
            assert_eq!(page["total_bytes"], expected.len());
            let data = STANDARD.decode(page["data"].as_str().unwrap()).unwrap();
            assert!(data.len() <= PAGE_BYTES);
            actual.extend(data);
            offset = page["next_offset"].as_u64().unwrap() as usize;
            if offset == expected.len() {
                break;
            }
        }
        assert_eq!(actual, expected);
        assert!(revision_page(&repository, &candidates, expected.len() + 1).is_err());
    }

    #[test]
    fn initialization_preserves_absolute_paths_without_byte_admission() {
        let path = std::env::current_dir()
            .unwrap()
            .join("x".repeat(16 * 1024 + 1));
        assert!(
            validate_initialize(&json!({
                "protocol_version": forge_protocol::WIRE_VERSION,
                "git_config_cache_path": path,
                "recovery_directory": path,
                "status_ignored_directory": path,
            }))
            .is_ok()
        );
        assert!(
            validate_initialize(&json!({
                "protocol_version": forge_protocol::WIRE_VERSION,
                "git_config_cache_path": "relative/path",
            }))
            .is_err()
        );
    }
}
