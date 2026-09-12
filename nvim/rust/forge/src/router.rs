use std::{path::PathBuf, sync::Arc};

use anyhow::{Context, Result, ensure};
use base64::{Engine, engine::general_purpose::STANDARD};
use forge_buffer::identity::DocumentId;
use forge_git::{
    completion::{CandidateLimits, RevisionCandidates},
    repository::RepositoryState,
};
use forge_github::{
    comment::{CommentOperation, CommentTarget},
    issue_store::IssueStore,
    metadata::MetadataRequest,
    pull_request::PullRequestRequest,
    remote::IssueDetailRequest,
    service::IssueOperation,
    sync::{SyncProgress, SyncRequest},
};
use forge_harness::protocol::HarnessMethod;
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
const CACHE_BYTES: usize = 2 * 1024 * 1024;
const SNAPSHOT_BYTES: usize = 4096;
const VALUE_BYTES: usize = 64;

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
    directory: PathBuf,
    target: Option<forge_github::pull_request::PullRequestTarget>,
    repository: Option<forge_github::model::GithubRepositoryId>,
    number: Option<u64>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct ReviewDocumentParams {
    document: DocumentId,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct ReviewMaterializeParams {
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
struct ReviewFileParams {
    document: DocumentId,
    directory: PathBuf,
    path: String,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct ReviewFileMoreParams {
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
struct ReviewCloseViewParams {
    document: DocumentId,
    view: forge_buffer::identity::ViewId,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct ReviewActionParams {
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
struct ReviewSubmitBatchedParams {
    document: DocumentId,
    verdict: forge_review::review::ReviewVerdict,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct ReviewSubmitBatchedRecoverParams {
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
enum HarnessSourceRequest {
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
    HarnessInitialize,
    HarnessDocument,
    Revisions,
    RepositoryWrite,
    RepositoryGenerate,
    Status,
    StatusContext,
    StatusNavigate,
    Walkthrough,
    Notifications,
    Source,
    Issues,
    IssueSync,
    IssueDetail,
    Metadata,
    PullRequest,
    Comment,
    ReviewOpen,
    ReviewEdit,
    ReviewSnapshot,
    ReviewMaterialize,
    ReviewSection,
    ReviewFile,
    ReviewFileMore,
    ReviewThread,
    ReviewView,
    ReviewCloseView,
    ReviewAction,
    ReviewTransition,
    ReviewLifecycleReconcile,
    ReviewLifecycleResolve,
    ReviewBeginBatched,
    ReviewSetViewed,
    ReviewSubmitBatched,
    ReviewSubmitBatchedRecover,
    IssueDocument,
    ReviewSave,
    ReviewReconcile,
    ReviewClose,
    ReviewComment,
    Shutdown,
    Harness(HarnessMethod),
}

fn decode_method(method: &str) -> Result<RoutedMethod> {
    match method {
        "github.actor" => Ok(RoutedMethod::Github(github_routes::GithubRoute::Actor)),
        "github.notifications.page" => Ok(RoutedMethod::Github(
            github_routes::GithubRoute::NotificationPage,
        )),
        "github.review.submission_context" => Ok(RoutedMethod::Github(
            github_routes::GithubRoute::ReviewSubmissionContext,
        )),
        "github.creation.context" => Ok(RoutedMethod::Github(
            github_routes::GithubRoute::CreationContext,
        )),
        "github.review.read" => Ok(RoutedMethod::Github(github_routes::GithubRoute::ReviewRead)),
        "github.review.mutate" => Ok(RoutedMethod::Github(
            github_routes::GithubRoute::ReviewMutate,
        )),
        "github.recovery.inspect" => Ok(RoutedMethod::Github(
            github_routes::GithubRoute::RecoveryInspect,
        )),
        "github.recovery.resolve" => Ok(RoutedMethod::Github(
            github_routes::GithubRoute::RecoveryResolve,
        )),
        "github.recovery.ack" => Ok(RoutedMethod::Github(
            github_routes::GithubRoute::RecoveryAcknowledge,
        )),
        "github.recovery.settle_draft" => Ok(RoutedMethod::Github(
            github_routes::GithubRoute::RecoverySettleDraft,
        )),
        "github.review.draft" => Ok(RoutedMethod::Github(
            github_routes::GithubRoute::ReviewDraft,
        )),
        "github.review.draft.write" => Ok(RoutedMethod::Github(
            github_routes::GithubRoute::ReviewDraftWrite,
        )),
        "harness.initialize" => Ok(RoutedMethod::HarnessInitialize),
        "harness.document" => Ok(RoutedMethod::HarnessDocument),
        "repository.revisions" => Ok(RoutedMethod::Revisions),
        "repository.write" => Ok(RoutedMethod::RepositoryWrite),
        "repository.generate" => Ok(RoutedMethod::RepositoryGenerate),
        "status" => Ok(RoutedMethod::Status),
        "status.context" => Ok(RoutedMethod::StatusContext),
        "status.navigate" => Ok(RoutedMethod::StatusNavigate),
        "walkthrough" => Ok(RoutedMethod::Walkthrough),
        "notifications.document" => Ok(RoutedMethod::Notifications),
        "issue.document" => Ok(RoutedMethod::IssueDocument),
        "source.document" => Ok(RoutedMethod::Source),
        "github.issues" => Ok(RoutedMethod::Issues),
        "github.sync" => Ok(RoutedMethod::IssueSync),
        "github.detail" => Ok(RoutedMethod::IssueDetail),
        "github.metadata" => Ok(RoutedMethod::Metadata),
        "github.pull_request" => Ok(RoutedMethod::PullRequest),
        "github.comment" => Ok(RoutedMethod::Comment),
        "review.open_pr" => Ok(RoutedMethod::ReviewOpen),
        "review.open" => Ok(RoutedMethod::ReviewOpen),
        "review.region_edit" => Ok(RoutedMethod::ReviewEdit),
        "review.snapshot" => Ok(RoutedMethod::ReviewSnapshot),
        "review.materialize" => Ok(RoutedMethod::ReviewMaterialize),
        "review.section" => Ok(RoutedMethod::ReviewSection),
        "review.file" => Ok(RoutedMethod::ReviewFile),
        "review.file.more" => Ok(RoutedMethod::ReviewFileMore),
        "review.thread" => Ok(RoutedMethod::ReviewThread),
        "review.view" => Ok(RoutedMethod::ReviewView),
        "review.view.close" => Ok(RoutedMethod::ReviewCloseView),
        "review.act" => Ok(RoutedMethod::ReviewAction),
        "review.transition" => Ok(RoutedMethod::ReviewTransition),
        "review.lifecycle_reconcile" => Ok(RoutedMethod::ReviewLifecycleReconcile),
        "review.lifecycle_resolve" => Ok(RoutedMethod::ReviewLifecycleResolve),
        "review.begin_batched" => Ok(RoutedMethod::ReviewBeginBatched),
        "review.set_viewed" => Ok(RoutedMethod::ReviewSetViewed),
        "review.submit_batched" => Ok(RoutedMethod::ReviewSubmitBatched),
        "review.submit_batched_recover" => Ok(RoutedMethod::ReviewSubmitBatchedRecover),
        "review.save" => Ok(RoutedMethod::ReviewSave),
        "review.reconcile" => Ok(RoutedMethod::ReviewReconcile),
        "review.close" => Ok(RoutedMethod::ReviewClose),
        "review.comment" => Ok(RoutedMethod::ReviewComment),
        "shutdown" => Ok(RoutedMethod::Shutdown),
        "initialize" => anyhow::bail!("Forge host is already initialized"),
        method => HarnessMethod::decode(method).map(RoutedMethod::Harness),
    }
}

/// Routes repository requests without creating a provider or durable Harness session.
pub(crate) struct HostRouter {
    host: Arc<ForgeRuntime>,
    status_events: std::sync::OnceLock<tokio::task::JoinHandle<()>>,
}

impl HostRouter {
    pub(crate) fn new(host: Arc<ForgeRuntime>) -> Self {
        Self { host, status_events: std::sync::OnceLock::new() }
    }

    pub(crate) async fn prepare(&self, envelope: &RoutedRequestEnvelope) -> Result<RoutedMethod> {
        let method = decode_method(&envelope.request.method)?;
        if let RoutedMethod::Harness(harness_method) = method {
            self.host
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
        self.status_events.get_or_init(|| {
            let mut updates = self.host.status.subscribe_updates();
            let sink = sink.clone();
            tokio::spawn(async move {
                loop {
                    match updates.recv().await {
                        Ok(update) => {
                            if let Err(error) = send_status_update(&sink, &update).await {
                                if sink.send_wait(Message::DocumentEvent(DocumentEvent { document: update.document.0.clone(), event: "status.resync".into(), payload: json!({"diagnostic": [format!("Status update delivery requires recovery: {error:#}")]}) })).await.is_err() { break; }
                            }
                        }
                        Err(tokio::sync::broadcast::error::RecvError::Closed) => break,
                        Err(tokio::sync::broadcast::error::RecvError::Lagged(_)) => {
                            if sink.send_wait(Message::DocumentEvent(DocumentEvent { document: String::new(), event: "status.resync".into(), payload: json!({}) })).await.is_err() { break; }
                        }
                    }
                }
            })
        });
        let request = envelope.request;
        match method {
            RoutedMethod::StatusNavigate => {
                let params: StatusNavigationParams = serde_json::from_value(request.params)?;
                let result = self
                    .host
                    .status
                    .navigate(params.input, params.forward)
                    .await?;
                send_completed_result(sink, request.id, result).await?;
            }
            RoutedMethod::HarnessDocument => {
                if request.params.get("operation").and_then(Value::as_str) == Some("open_diff") {
                    let HarnessSourceRequest::OpenDiff { document, input } =
                        serde_json::from_value(request.params)?;
                    let text = self
                        .host
                        .harness
                        .saved_diff(envelope.session_id, input)
                        .await?;
                    let result = self.host.sources.open_diff_text(
                        document,
                        "Saved diff".into(),
                        text.into_bytes(),
                    )?;
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
            RoutedMethod::StatusContext => {
                let params: StatusContextRequest = serde_json::from_value(request.params)?;
                let result = match params {
                    StatusContextRequest::Action { input } => {
                        serde_json::to_value(self.host.status.context_action(input).await?)?
                    }
                    StatusContextRequest::Issues { input, text } => {
                        ensure!(
                            text.len() <= 1024 * 1024,
                            "Status issues text exceeds request limit"
                        );
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
                    StatusContextRequest::AboutMessage { document, text } => {
                        ensure!(text.len() <= 1024 * 1024, "About message exceeds 1 MiB");
                        serde_json::to_value(self.host.sources.open_text(
                            document,
                            "About".into(),
                            text.into_bytes(),
                        )?)?
                    }
                };
                send_completed_result(sink, request.id, result).await?;
            }
            RoutedMethod::Source => {
                let params: SourceRequest = serde_json::from_value(request.params)?;
                let result = match params {
                    SourceRequest::Open {
                        document,
                        workspace,
                        revision,
                        path,
                    } => {
                        ensure!(path.len() <= 65536, "source path exceeds request limit");
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
                        serde_json::to_value(self.host.sources.snapshot(&document).await?)?
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
            RoutedMethod::RepositoryGenerate => {
                let result = self.host.generation.dispatch(request.params).await?;
                send_completed_result(sink, request.id, result).await?;
            }
            RoutedMethod::Notifications => {
                let result = notification_routes::route(&self.host, request.params).await?;
                send_completed_result(sink, request.id, result).await?;
            }
            RoutedMethod::IssueDocument => {
                let result = issue_routes::route(&self.host, request.params).await?;
                send_completed_result(sink, request.id, result).await?;
            }
            RoutedMethod::Walkthrough => {
                let result = walkthrough_routes::route(&self.host, request.params).await?;
                send_completed_result(sink, request.id, result).await?;
            }
            RoutedMethod::Status => {
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
                    } => {
                        let path = path
                            .map(|path| {
                                ensure!(
                                    path.len() <= 65536,
                                    "comparison path exceeds request limit"
                                );
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
                                        worktree,
                                        path,
                                    },
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
                            event: "status.progress".into(),
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
                    StatusRequest::OpenTarget { input } => {
                        serde_json::to_value(self.host.status.open_target(input).await?)?
                    }
                    StatusRequest::Input { input, selection } => {
                        match self.host.status.act(input, selection).await? {
                            forge_status::StatusAction::Accepted(accepted, _) => serde_json::to_value(accepted)?,
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
            RoutedMethod::ReviewComment => {
                let params: ReviewCommentParams = serde_json::from_value(request.params)?;
                let result = self
                    .host
                    .review
                    .comment(&params.document, params.command)
                    .await?;
                send_completed_result(sink, request.id, result).await?;
            }
            RoutedMethod::ReviewOpen => {
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
                        self.host
                            .review
                            .open_repository_pr(params.directory, remote, repository, number)
                            .await?
                    }
                    _ => anyhow::bail!(
                        "review open requires exactly one captured target or repository and number"
                    ),
                };
                send_completed_result(sink, request.id, snapshot).await?;
            }
            RoutedMethod::ReviewEdit => {
                let edit: RegionEdit = serde_json::from_value(request.params)?;
                let acknowledgement = self.host.review.region_edit(edit).await?;
                send_completed_result(sink, request.id, acknowledgement).await?;
            }
            RoutedMethod::ReviewFile => {
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
            RoutedMethod::ReviewFileMore => {
                let params: ReviewFileMoreParams = serde_json::from_value(request.params)?;
                let result = self
                    .host
                    .review
                    .read_file_more(&params.document, params.path)
                    .await?;
                send_completed_result(sink, request.id, result).await?;
            }
            RoutedMethod::ReviewThread => {
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
            RoutedMethod::ReviewView => {
                let params: ReviewViewParams = serde_json::from_value(request.params)?;
                let result = self
                    .host
                    .review
                    .view(&params.document, params.view, params.width)
                    .await?;
                send_completed_result(sink, request.id, result).await?;
            }
            RoutedMethod::ReviewCloseView => {
                let params: ReviewCloseViewParams = serde_json::from_value(request.params)?;
                let result = self
                    .host
                    .review
                    .close_view(&params.document, &params.view)
                    .await?;
                send_completed_result(sink, request.id, result).await?;
            }
            RoutedMethod::ReviewAction => {
                let params: ReviewActionParams = serde_json::from_value(request.params)?;
                let result = self.host.review.act(params.input, params.directory).await?;
                send_completed_result(sink, request.id, result).await?;
            }
            RoutedMethod::ReviewTransition => {
                let params: ReviewTransitionParams = serde_json::from_value(request.params)?;
                let result = self
                    .host
                    .review
                    .transition(&params.document, params.desired)
                    .await?;
                send_completed_result(sink, request.id, result).await?;
            }
            RoutedMethod::ReviewLifecycleReconcile => {
                let params: ReviewDocumentParams = serde_json::from_value(request.params)?;
                let result = self
                    .host
                    .review
                    .lifecycle_reconcile(&params.document)
                    .await?;
                send_completed_result(sink, request.id, result).await?;
            }
            RoutedMethod::ReviewLifecycleResolve => {
                let params: ReviewLifecycleResolveParams = serde_json::from_value(request.params)?;
                let result = self
                    .host
                    .review
                    .resolve_lifecycle(&params.document, params.operation_id, params.resolution)
                    .await?;
                send_completed_result(sink, request.id, result).await?;
            }
            RoutedMethod::ReviewBeginBatched => {
                let params: ReviewDocumentParams = serde_json::from_value(request.params)?;
                let result = self.host.review.begin_batched(&params.document).await?;
                send_completed_result(sink, request.id, result).await?;
            }
            RoutedMethod::ReviewSetViewed => {
                let params: ReviewSetViewedParams = serde_json::from_value(request.params)?;
                let result = self
                    .host
                    .review
                    .set_viewed(&params.document, params.path, params.viewed)
                    .await?;
                send_completed_result(sink, request.id, result).await?;
            }
            RoutedMethod::ReviewSubmitBatched => {
                let params: ReviewSubmitBatchedParams = serde_json::from_value(request.params)?;
                let result = self
                    .host
                    .review
                    .submit_batched(&params.document, params.verdict)
                    .await?;
                send_completed_result(sink, request.id, result).await?;
            }
            RoutedMethod::ReviewSubmitBatchedRecover => {
                let params: ReviewSubmitBatchedRecoverParams =
                    serde_json::from_value(request.params)?;
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
            RoutedMethod::ReviewSection => {
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
            RoutedMethod::ReviewMaterialize => {
                let params: ReviewMaterializeParams = serde_json::from_value(request.params)?;
                let result = self
                    .host
                    .review
                    .materialize(&params.document, params.width)
                    .await?;
                send_completed_result(sink, request.id, result).await?;
            }
            RoutedMethod::ReviewSnapshot
            | RoutedMethod::ReviewSave
            | RoutedMethod::ReviewReconcile
            | RoutedMethod::ReviewClose => {
                let params: ReviewDocumentParams = serde_json::from_value(request.params)?;
                match method {
                    RoutedMethod::ReviewSnapshot => {
                        send_completed_result(
                            sink,
                            request.id,
                            self.host.review.snapshot(&params.document)?,
                        )
                        .await?
                    }
                    RoutedMethod::ReviewSave => {
                        send_completed_result(
                            sink,
                            request.id,
                            self.host.review.save(&params.document).await?,
                        )
                        .await?
                    }
                    RoutedMethod::ReviewReconcile => {
                        send_completed_result(
                            sink,
                            request.id,
                            self.host.review.reconcile(&params.document).await?,
                        )
                        .await?
                    }
                    RoutedMethod::ReviewClose => {
                        self.host.review.close_document(&params.document)?;
                        sink.send(Message::Response(Response::success(
                            request.id,
                            json!({"closed":true}),
                        )?))?;
                    }
                    _ => unreachable!("review document route"),
                }
            }
            RoutedMethod::Metadata => {
                let params: MetadataParams = serde_json::from_value(request.params)?;
                let remote = self.host.github_remote.for_directory(params.directory)?;
                let metadata = self
                    .host
                    .github
                    .metadata(remote, params.cache_directory, params.request)
                    .await?;
                send_completed_result(sink, request.id, metadata).await?;
            }
            RoutedMethod::Comment => {
                let params: CommentParams = serde_json::from_value(request.params)?;
                let remote = self.host.github_remote.for_directory(params.directory)?;
                let result = self
                    .host
                    .github
                    .comment(remote, params.target, params.request)
                    .await?;
                send_completed_result(sink, request.id, result).await?;
            }
            RoutedMethod::PullRequest => {
                let params: PullRequestParams = serde_json::from_value(request.params)?;
                let remote = self.host.github_remote.for_directory(params.directory)?;
                let result = self
                    .host
                    .github
                    .pull_request(remote, params.request)
                    .await?;
                send_completed_result(sink, request.id, result).await?;
            }
            RoutedMethod::IssueDetail => {
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
            RoutedMethod::IssueSync => {
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
                            sink.send(Message::RequestEvent(RequestEvent { request_id: request.id, event: "github.sync.progress".into(), payload }))?;
                        }
                    }
                };
                sink.send(Message::Response(Response::success(request.id, result)?))?;
            }
            RoutedMethod::Issues => {
                let params: IssueRequest = serde_json::from_value(request.params)?;
                let store = IssueStore::new(
                    params.database,
                    &params.repo,
                    std::time::Duration::from_secs(1),
                )?;
                let result = self.host.github.execute(store, params.request).await?;
                send_completed_result(sink, request.id, result).await?;
            }
            RoutedMethod::HarnessInitialize => {
                let response = self
                    .host
                    .harness
                    .open_session(request.id, serde_json::from_value(request.params)?)
                    .await?;
                sink.send(Message::Response(response))?;
            }
            RoutedMethod::Revisions => {
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
                        .refresh_revisions(&self.host.repositories, CandidateLimits::default())
                        .await?
                };
                let page = revision_page(&repository, &candidates, params.offset)?;
                sink.send(Message::Response(Response::success(request.id, page)?))?;
            }
            RoutedMethod::Shutdown => {
                anyhow::bail!("shutdown must route through the Forge connection host")
            }
            RoutedMethod::Harness(_) => {
                self.host
                    .harness
                    .dispatch(envelope.session_id, request, sink)
                    .await?;
            }
        }
        Ok(())
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
            path.is_absolute() && path.as_os_str().len() <= 16 * 1024,
            "Git configuration cache must be an absolute bounded path"
        );
    }
    if let Some(directory) = initialize.recovery_directory {
        ensure!(
            directory.is_absolute() && directory.as_os_str().len() <= 16 * 1024,
            "GitHub recovery directory must be an absolute bounded path"
        );
    }
    if let Some(directory) = initialize.status_ignored_directory {
        ensure!(
            directory.is_absolute() && directory.as_os_str().len() <= 16 * 1024,
            "Status ignored directory must be an absolute bounded path"
        );
    }
    Ok(())
}

async fn send_status_update(sink: &MessageSender, update: &forge_status::StatusUpdate) -> Result<()> {
    let message = Message::DocumentEvent(DocumentEvent { document: update.document.0.clone(), event: "status.update".into(), payload: serde_json::to_value(update)? });
    if forge_protocol::outbound::encode(&message, forge_protocol::MAX_FRAME_BYTES).is_ok() { sink.send_wait(message).await?; return Ok(()); }
    let _permit = sink.begin_transfer()?;
    let transfer = forge_protocol::transfer::JsonTransfer::new(&message)?;
    let complete = json!({"part_count": transfer.part_count(), "total_bytes": transfer.total_bytes()});
    for part in transfer {
        sink.send_wait(Message::DocumentEvent(DocumentEvent { document: update.document.0.clone(), event: "document.part".into(), payload: serde_json::to_value(part)? })).await?;
        tokio::task::yield_now().await;
    }
    sink.send_wait(Message::DocumentEvent(DocumentEvent { document: update.document.0.clone(), event: "document.complete".into(), payload: complete })).await?;
    Ok(())
}

async fn send_completed_result(
    sink: &MessageSender,
    request_id: u64,
    result: impl Serialize,
) -> Result<()> {
    let response = Message::Response(Response::success(request_id, result)?);
    if forge_protocol::outbound::encode(&response, forge_protocol::MAX_FRAME_BYTES).is_ok() {
        sink.send_wait(response).await?;
        return Ok(());
    }
    let prepared = sink.begin_transfer().and_then(|permit| {
        forge_protocol::transfer::JsonTransfer::new(&response).map(|transfer| (permit, transfer))
    });
    let (_permit, transfer) = match prepared {
        Ok(prepared) => prepared,
        Err(error) => {
            sink.send_wait(Message::Response(Response::failure_with_data(
                request_id,
                if error.kind() == std::io::ErrorKind::WouldBlock {
                    "result_transfer_busy"
                } else {
                    "result_too_large"
                },
                format!("Forge operation completed, but its result cannot be transferred: {error}"),
                json!({"operation_completed": true}),
            )))
            .await?;
            return Ok(());
        }
    };
    let complete =
        json!({"part_count":transfer.part_count(), "total_bytes":transfer.total_bytes()});
    for part in transfer {
        sink.send_wait(Message::RequestEvent(RequestEvent {
            request_id,
            event: "result.part".into(),
            payload: serde_json::to_value(part)?,
        }))
        .await?;
        tokio::task::yield_now().await;
    }
    sink.send_wait(Message::RequestEvent(RequestEvent {
        request_id,
        event: "result.complete".into(),
        payload: complete,
    }))
    .await?;
    Ok(())
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
    let mut accounted = SNAPSHOT_BYTES;
    let mut count = 0;
    let mut total = 0;
    for value in &candidates.values {
        if value.len() + VALUE_BYTES > CACHE_BYTES.saturating_sub(accounted) {
            break;
        }
        accounted += value.len() + VALUE_BYTES;
        total += value.len() + 1;
        count += 1;
    }
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
        "truncated": candidates.truncated || count < candidates.values.len(),
        "data": STANDARD.encode(payload),
    }))
}
