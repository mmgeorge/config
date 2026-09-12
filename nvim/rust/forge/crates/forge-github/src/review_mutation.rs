use std::{future::Future, pin::Pin, sync::Arc};

use anyhow::{Context, Result, ensure};
use serde::{Deserialize, Serialize};
use serde_json::Value;

use crate::queue::{RemoteCompletion, RemoteIntent, RemoteResource, RemoteScope};
use crate::recovery::{
    RecoveryCapture, RecoveryOperation, RecoveryPhase, RecoveryRecord, RecoveryResource,
    RecoveryResourceKind,
};
use crate::remote::{GithubRemote, RemoteFailure};
use crate::service::GithubService;

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "operation", rename_all = "snake_case", deny_unknown_fields)]
pub enum ReviewMutation {
    PullRequestCreate {
        repository_node_id: String,
        title: String,
        body: String,
        base: String,
        head: String,
        head_commit: String,
        draft: bool,
    },
    PullRequestEdit {
        node_id: String,
        title: Option<String>,
        body: Option<String>,
    },
    PullRequestTransition {
        node_id: String,
        desired: crate::pull_request::DesiredPullRequestState,
    },
    PullRequestStep {
        node_id: String,
        action: PullRequestAction,
    },
    PullRequestObserve {
        node_id: String,
    },
    ConversationCreate {
        body: String,
    },
    InlineCreate {
        body: String,
        path: String,
        line: u32,
        side: DiffSide,
        start_line: Option<u32>,
        start_side: Option<DiffSide>,
        commit_id: String,
    },
    ReplyCreate {
        body: String,
        parent_comment_id: u64,
    },
    PendingReviewCreate {
        commit_id: String,
    },
    PendingReviewComment {
        body: String,
        review_node_id: String,
        path: String,
        position: u32,
        commit_id: String,
    },
    ReviewSubmit {
        body: String,
        event: ReviewEvent,
        pending_review_id: Option<u64>,
        commit_id: Option<String>,
        #[serde(default)]
        pending_comment: Option<Vec<PendingReviewMember>>,
        #[serde(default)]
        comment: Vec<ReviewSubmissionComment>,
    },
    ConversationEdit {
        comment_id: u64,
        body: String,
    },
    ConversationDelete {
        comment_id: u64,
    },
    InlineEdit {
        comment_id: u64,
        body: String,
    },
    InlineDelete {
        comment_id: u64,
    },
    ReviewerChange {
        add: ReviewerSelection,
        remove: ReviewerSelection,
    },
    ReviewerAdd {
        reviewer: Vec<String>,
        team: Vec<String>,
    },
    ReviewerRemove {
        reviewer: Vec<String>,
        team: Vec<String>,
    },
    MilestoneAssign {
        milestone_number: Option<u64>,
    },
    MilestoneCreate {
        title: String,
    },
    MilestoneCreateAndAssign {
        title: String,
    },
    MilestoneAssignCreated,
    IssueEdit {
        title: Option<String>,
        body: Option<String>,
        #[serde(default)]
        add_assignees: Vec<String>,
        #[serde(default)]
        remove_assignees: Vec<String>,
    },
    IssueAssigneeAdd {
        assignee: Vec<String>,
    },
    IssueAssigneeRemove {
        assignee: Vec<String>,
    },
    NotificationRead,
    NotificationDone,
}

#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
#[serde(deny_unknown_fields)]
pub struct ReviewSubmissionComment {
    pub body: String,
    pub path: String,
    pub line: u32,
    pub side: DiffSide,
    pub start_line: Option<u32>,
    pub start_side: Option<DiffSide>,
}

#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
#[serde(deny_unknown_fields)]
pub struct PendingReviewMember {
    pub id: u64,
    pub node_id: String,
    pub body: String,
    pub path: String,
    pub commit_id: String,
    pub position: Option<u32>,
    pub line: Option<u32>,
    pub side: Option<String>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct ReviewerSelection {
    pub reviewer: Vec<String>,
    pub team: Vec<String>,
}

impl ReviewerSelection {
    fn validate(&self) -> Result<()> {
        ensure!(
            self.reviewer.len() + self.team.len() <= 100
                && self
                    .reviewer
                    .iter()
                    .chain(&self.team)
                    .all(|name| !name.is_empty()
                        && name.len() <= 100
                        && name
                            .bytes()
                            .all(|byte| byte.is_ascii_alphanumeric() || byte == b'-')),
            "invalid reviewer set"
        );
        Ok(())
    }
}

#[derive(Clone, Copy, Debug, Deserialize, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum PullRequestAction {
    Close,
    Reopen,
    Draft,
    Ready,
}

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "UPPERCASE")]
pub enum DiffSide {
    Left,
    Right,
}

#[derive(Clone, Copy, Debug, Deserialize, Serialize)]
#[serde(rename_all = "SCREAMING_SNAKE_CASE")]
pub enum ReviewEvent {
    Approve,
    RequestChanges,
    Comment,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct ReviewMutationRequest {
    pub parent_node_id: Option<String>,
    pub resource: RecoveryResource,
    pub operation_id: String,
    pub actor_node_id: String,
    pub edit_sequence: Option<u64>,
    pub draft_target: Option<String>,
    pub mutation: ReviewMutation,
}

pub enum ReviewMutationOutcome {
    Confirmed(Value),
    Rejected(RemoteFailure),
    OutcomeUnknown(RemoteFailure),
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "resolution", rename_all = "snake_case", deny_unknown_fields)]
pub enum RecoveryResolution {
    Link { remote_id: u64 },
    CloseUnknown,
    NotDispatched,
}

pub trait GithubReviewWriteRemote: GithubRemote {
    fn plan_review_write(
        &self,
        request: ReviewMutationRequest,
    ) -> Pin<Box<dyn Future<Output = Result<Vec<ReviewMutation>, RemoteFailure>> + Send + '_>> {
        Box::pin(async move { Ok(vec![request.mutation]) })
    }
    fn read_review_write_result(
        &self,
        request: ReviewMutationRequest,
        remote_id: u64,
    ) -> Pin<Box<dyn Future<Output = Result<Value, RemoteFailure>> + Send + '_>>;
    fn validate_review_write(
        &self,
        request: ReviewMutationRequest,
    ) -> Pin<Box<dyn Future<Output = Result<(), RemoteFailure>> + Send + '_>>;
    fn mutate_review(
        &self,
        request: ReviewMutationRequest,
    ) -> Pin<Box<dyn Future<Output = ReviewMutationOutcome> + Send + '_>>;
}

impl ReviewMutationRequest {
    pub fn validate(&self) -> Result<()> {
        self.resource.validate()?;
        ensure!(
            self.parent_node_id
                .as_ref()
                .is_none_or(|identity| !identity.is_empty()
                    && identity.len() <= 256
                    && identity.bytes().all(|byte| byte.is_ascii_graphic())),
            "invalid captured parent identity"
        );
        ensure!(
            self.draft_target
                .as_ref()
                .is_none_or(|identity| !identity.is_empty()
                    && identity.len() <= 512
                    && !identity.contains(['\n', '\r', '\0'])),
            "invalid captured draft identity"
        );
        ensure!(
            self.edit_sequence.is_none() || self.draft_target.is_some(),
            "editable mutation omitted its captured draft identity"
        );
        ensure!(
            (self.resource.kind == RecoveryResourceKind::Repository && self.resource.number == 0)
                || (self.resource.number > 0
                    && (self.resource.kind == RecoveryResourceKind::Notification
                        || self.resource.number <= i32::MAX as u64)),
            "invalid remote resource number"
        );
        for identity in [&self.operation_id, &self.actor_node_id] {
            ensure!(
                !identity.is_empty()
                    && identity.len() <= 256
                    && identity.bytes().all(|byte| byte.is_ascii_graphic()),
                "invalid captured operation identity"
            );
        }
        let encoded = serde_json::to_vec(self)?;
        ensure!(
            encoded.len() <= 8 * 1024 * 1024,
            "remote mutation capture exceeds 8 MiB"
        );
        if let ReviewMutation::ReviewSubmit {
            pending_review_id,
            commit_id,
            pending_comment,
            comment,
            ..
        } = &self.mutation
        {
            ensure!(
                commit_id.is_some() && pending_review_id != &Some(0),
                "review submission requires a captured head and valid pending identity"
            );
            let member = pending_comment
                .as_ref()
                .context("review submission requires captured comment membership")?;
            ensure!(
                member.len() <= 10_000 && (pending_review_id.is_some() || member.is_empty()),
                "invalid review submission membership"
            );
            ensure!(
                member.windows(2).all(|pair| pair[0].id < pair[1].id),
                "review submission membership is not unique and ordered"
            );
            ensure!(
                comment.len() <= 10_000,
                "review submission comment count exceeds limit"
            );
            for comment in comment {
                ensure!(
                    !comment.body.trim().is_empty()
                        && !comment.body.contains('\0')
                        && comment.body.len() <= 4 * 1024 * 1024,
                    "invalid submitted review comment body"
                );
                validate_path(&comment.path)?;
                ensure!(
                    comment.line > 0
                        && comment
                            .start_line
                            .is_none_or(|start| start > 0 && start <= comment.line)
                        && (comment.start_line.is_some() == comment.start_side.is_some()),
                    "invalid submitted review comment coordinate"
                );
            }
            for member in member {
                ensure!(
                    member.id > 0
                        && !member.node_id.is_empty()
                        && member.node_id.len() <= 256
                        && !member.body.contains('\0')
                        && member.body.len() <= 4 * 1024 * 1024,
                    "invalid captured review comment"
                );
                validate_commit(&member.commit_id)?;
                validate_path(&member.path)?;
                ensure!(
                    member.position != Some(0)
                        && member.line != Some(0)
                        && member
                            .side
                            .as_deref()
                            .is_none_or(|side| matches!(side, "LEFT" | "RIGHT")),
                    "invalid captured review comment coordinate"
                );
            }
        }
        let body = match &self.mutation {
            ReviewMutation::PullRequestCreate { body, .. } => Some(body),
            ReviewMutation::ConversationCreate { body }
            | ReviewMutation::InlineCreate { body, .. }
            | ReviewMutation::ReplyCreate { body, .. }
            | ReviewMutation::PendingReviewComment { body, .. }
            | ReviewMutation::ReviewSubmit { body, .. }
            | ReviewMutation::ConversationEdit { body, .. }
            | ReviewMutation::InlineEdit { body, .. } => Some(body),
            ReviewMutation::IssueEdit { body, .. } => body.as_ref(),
            _ => None,
        };
        ensure!(
            body.is_none_or(|body| body.len() <= 4 * 1024 * 1024 && !body.contains('\0')),
            "invalid remote body"
        );
        match &self.mutation {
            ReviewMutation::PullRequestCreate {
                repository_node_id,
                title,
                body,
                base,
                head,
                head_commit,
                ..
            } => {
                ensure!(
                    !repository_node_id.is_empty()
                        && repository_node_id.len() <= 256
                        && repository_node_id
                            .bytes()
                            .all(|byte| byte.is_ascii_graphic()),
                    "invalid creation repository identity"
                );
                ensure!(
                    self.edit_sequence.is_some()
                        && self.draft_target.as_deref() == Some("pr:create"),
                    "PR creation requires a durable captured draft"
                );
                crate::pull_request::PullRequestEdit {
                    title: Some(title.clone()),
                    body: Some(body.clone()),
                }
                .validate()?;
                for branch in [base, head] {
                    ensure!(
                        !branch.is_empty()
                            && branch.len() <= 1024
                            && !branch.chars().any(char::is_control),
                        "invalid captured creation branch"
                    );
                }
                ensure!(
                    base != head,
                    "PR creation base and head branches are identical"
                );
                validate_commit(head_commit)?;
            }
            ReviewMutation::PullRequestEdit {
                node_id,
                title,
                body,
            } => {
                ensure!(
                    !node_id.is_empty() && node_id.len() <= 256,
                    "invalid PR node identity"
                );
                crate::pull_request::PullRequestEdit {
                    title: title.clone(),
                    body: body.clone(),
                }
                .validate()?;
            }
            ReviewMutation::PullRequestTransition { node_id, .. }
            | ReviewMutation::PullRequestStep { node_id, .. }
            | ReviewMutation::PullRequestObserve { node_id } => ensure!(
                !node_id.is_empty() && node_id.len() <= 256,
                "invalid PR node identity"
            ),
            ReviewMutation::InlineCreate {
                path,
                line,
                commit_id,
                start_line,
                start_side,
                ..
            } => {
                validate_path(path)?;
                validate_commit(commit_id)?;
                ensure!(*line > 0, "inline line must be positive");
                ensure!(
                    start_line.is_some() == start_side.is_some()
                        && start_line.is_none_or(|first| first > 0 && first <= *line),
                    "invalid multiline comment range"
                );
            }
            ReviewMutation::PendingReviewComment {
                path,
                position,
                commit_id,
                review_node_id,
                ..
            } => {
                validate_path(path)?;
                validate_commit(commit_id)?;
                ensure!(
                    *position > 0 && !review_node_id.is_empty() && review_node_id.len() <= 256,
                    "invalid pending review target"
                );
            }
            ReviewMutation::PendingReviewCreate { commit_id } => validate_commit(commit_id)?,
            ReviewMutation::ReviewSubmit {
                commit_id: Some(commit_id),
                ..
            } => validate_commit(commit_id)?,
            ReviewMutation::ReplyCreate {
                parent_comment_id, ..
            } => ensure!(*parent_comment_id > 0, "invalid reply parent"),
            ReviewMutation::ConversationEdit { comment_id, .. }
            | ReviewMutation::ConversationDelete { comment_id }
            | ReviewMutation::InlineEdit { comment_id, .. }
            | ReviewMutation::InlineDelete { comment_id } => {
                ensure!(*comment_id > 0, "invalid comment target")
            }
            ReviewMutation::ReviewerChange { add, remove } => {
                add.validate()?;
                remove.validate()?;
                ensure!(
                    !add.reviewer.is_empty()
                        || !add.team.is_empty()
                        || !remove.reviewer.is_empty()
                        || !remove.team.is_empty(),
                    "reviewer change is empty"
                );
            }
            ReviewMutation::ReviewerAdd { reviewer, team }
            | ReviewMutation::ReviewerRemove { reviewer, team } => ensure!(
                reviewer.len() + team.len() <= 100
                    && reviewer.iter().chain(team).all(|name| !name.is_empty()
                        && name.len() <= 100
                        && name
                            .bytes()
                            .all(|byte| byte.is_ascii_alphanumeric() || byte == b'-')),
                "invalid reviewer set"
            ),
            ReviewMutation::MilestoneCreate { title }
            | ReviewMutation::MilestoneCreateAndAssign { title } => ensure!(
                !title.trim().is_empty() && title.len() <= 1024 && !title.contains('\0'),
                "invalid milestone title"
            ),
            ReviewMutation::IssueEdit {
                title,
                body,
                add_assignees,
                remove_assignees,
            } => {
                validate_assignees(add_assignees)?;
                validate_assignees(remove_assignees)?;
                ensure!(
                    !add_assignees.iter().any(|add| remove_assignees
                        .iter()
                        .any(|remove| add.eq_ignore_ascii_case(remove))),
                    "issue assignee is both added and removed"
                );
                ensure!(
                    (title.is_some()
                        || body.is_some()
                        || !add_assignees.is_empty()
                        || !remove_assignees.is_empty())
                        && title.as_ref().is_none_or(|title| !title.trim().is_empty()
                            && title.len() <= 1024
                            && !title.contains('\0')),
                    "invalid issue edit"
                );
            }
            ReviewMutation::IssueAssigneeAdd { assignee }
            | ReviewMutation::IssueAssigneeRemove { assignee } => {
                ensure!(!assignee.is_empty(), "issue assignee step is empty");
                validate_assignees(assignee)?;
            }
            _ => {}
        }
        let kind = match self.mutation {
            ReviewMutation::PullRequestCreate { .. } => RecoveryResourceKind::Repository,
            ReviewMutation::NotificationRead | ReviewMutation::NotificationDone => {
                RecoveryResourceKind::Notification
            }
            ReviewMutation::IssueEdit { .. }
            | ReviewMutation::IssueAssigneeAdd { .. }
            | ReviewMutation::IssueAssigneeRemove { .. } => RecoveryResourceKind::Issue,
            _ => RecoveryResourceKind::PullRequest,
        };
        ensure!(
            self.resource.kind == kind,
            "mutation resource kind does not match its endpoint"
        );
        Ok(())
    }

    fn capture(&self) -> Result<RecoveryCapture> {
        let operation = match self.mutation {
            ReviewMutation::PullRequestCreate { .. } => RecoveryOperation::PullRequestCreate,
            ReviewMutation::PullRequestEdit { .. } => RecoveryOperation::PullRequestEdit,
            ReviewMutation::PullRequestTransition { .. }
            | ReviewMutation::PullRequestStep { .. }
            | ReviewMutation::PullRequestObserve { .. } => RecoveryOperation::PullRequestTransition,
            ReviewMutation::ConversationCreate { .. } => RecoveryOperation::ConversationCreate,
            ReviewMutation::InlineCreate { .. } | ReviewMutation::PendingReviewComment { .. } => {
                RecoveryOperation::InlineCreate
            }
            ReviewMutation::ReplyCreate { .. } => RecoveryOperation::ReplyCreate,
            ReviewMutation::PendingReviewCreate { .. } => RecoveryOperation::PendingReviewCreate,
            ReviewMutation::ReviewSubmit { .. } => RecoveryOperation::ReviewSubmit,
            ReviewMutation::ConversationEdit { .. } | ReviewMutation::InlineEdit { .. } => {
                RecoveryOperation::CommentEdit
            }
            ReviewMutation::ConversationDelete { .. } | ReviewMutation::InlineDelete { .. } => {
                RecoveryOperation::CommentDelete
            }
            ReviewMutation::ReviewerChange { .. }
            | ReviewMutation::ReviewerAdd { .. }
            | ReviewMutation::ReviewerRemove { .. } => RecoveryOperation::ReviewerSet,
            ReviewMutation::MilestoneAssign { .. }
            | ReviewMutation::MilestoneCreate { .. }
            | ReviewMutation::MilestoneCreateAndAssign { .. }
            | ReviewMutation::MilestoneAssignCreated => RecoveryOperation::MilestoneSet,
            ReviewMutation::IssueEdit { .. }
            | ReviewMutation::IssueAssigneeAdd { .. }
            | ReviewMutation::IssueAssigneeRemove { .. } => RecoveryOperation::IssueEdit,
            ReviewMutation::NotificationRead => RecoveryOperation::NotificationRead,
            ReviewMutation::NotificationDone => RecoveryOperation::NotificationDone,
        };
        Ok(RecoveryCapture {
            operation_id: self.operation_id.clone(),
            actor: self.actor_node_id.clone(),
            operation,
            edit_sequence: self.edit_sequence,
            submitted: serde_json::to_value(self)?,
        })
    }
}

impl GithubService {
    pub async fn recovery_resolve(
        &self,
        remote: Arc<dyn GithubReviewWriteRemote>,
        resource: RecoveryResource,
        operation_id: String,
        resolution: RecoveryResolution,
    ) -> Result<RecoveryRecord> {
        let store = self.recovery_store()?;
        resource.validate()?;
        let queue_resource = if resource.kind == RecoveryResourceKind::Repository {
            RemoteResource::repository(resource.repository.clone())
        } else if resource.kind == RecoveryResourceKind::Notification {
            RemoteResource::notification(resource.repository.clone(), resource.number)?
        } else {
            RemoteResource::new(resource.repository.clone(), resource.number)?
        };
        let service = self.clone();
        self.run_mutation(
            queue_resource,
            RemoteScope::PullRequest,
            RemoteIntent::Reconcile,
            4096,
            move |operation| async move {
                let mut journal = service
                    .submit(move || store.resume(&resource, &operation_id))?
                    .await
                    .context("recovery ownership was not collected")??;
                let linked = if let RecoveryResolution::Link { remote_id } = resolution {
                    ensure!(remote_id > 0, "invalid recovery remote identity");
                    let request: ReviewMutationRequest =
                        serde_json::from_value(journal.record().capture.submitted.clone())?;
                    Some(remote.read_review_write_result(request, remote_id).await?)
                } else {
                    None
                };
                let record = service
                    .submit(move || {
                        match resolution {
                            RecoveryResolution::Link { .. } => journal
                                .user_linked(linked.context("verified remote result missing")?)?,
                            RecoveryResolution::CloseUnknown => journal.user_closed_unknown()?,
                            RecoveryResolution::NotDispatched => {
                                ensure!(
                                    matches!(journal.record().state, RecoveryPhase::Prepared),
                                    "possible dispatch cannot be resolved as not dispatched"
                                );
                                journal.settle(RecoveryPhase::Rejected {
                                    diagnostic:
                                        "operation was prepared but never reached possible dispatch"
                                            .into(),
                                })?;
                            }
                        }
                        Ok(journal.record().clone())
                    })?
                    .await
                    .context("recovery resolution was not collected")??;
                operation.complete(RemoteCompletion::Reconciled)?;
                Ok(record)
            },
        )
        .await
    }

    /// Retains immutable intent and resource ownership through durable outcome publication.
    pub async fn review_mutation(
        &self,
        remote: Arc<dyn GithubReviewWriteRemote>,
        request: ReviewMutationRequest,
    ) -> Result<RecoveryRecord> {
        request.validate()?;
        self.recovery_store()?;
        let resource = if request.resource.kind == RecoveryResourceKind::Repository {
            RemoteResource::repository(request.resource.repository.clone())
        } else if request.resource.kind == RecoveryResourceKind::Notification {
            RemoteResource::notification(
                request.resource.repository.clone(),
                request.resource.number,
            )?
        } else {
            RemoteResource::new(request.resource.repository.clone(), request.resource.number)?
        };
        let retained_bytes = serde_json::to_vec(&request)?.len();
        let service = self.clone();
        self.run_mutation(
            resource,
            RemoteScope::PullRequest,
            RemoteIntent::Mutation,
            retained_bytes,
            move |mut operation| async move {
                let mut journal = service
                    .recovery_prepare(request.resource.clone(), request.capture()?)
                    .await?;
                let preflight = async {
                    let actor = remote
                        .read_actor(request.resource.repository.clone())
                        .await?;
                    ensure!(
                        actor.node_id == request.actor_node_id,
                        "authenticated actor changed after mutation capture"
                    );
                    remote.validate_review_write(request.clone()).await?;
                    let plan = remote.plan_review_write(request.clone()).await?;
                    ensure!(
                        !plan.is_empty() && plan.len() <= 4,
                        "remote write plan exceeds four steps"
                    );
                    Ok::<_, anyhow::Error>(plan)
                }
                .await;
                let plan = match preflight {
                    Ok(plan) => plan,
                    Err(failure) => {
                        let diagnostic = bounded_diagnostic(failure.to_string());
                        let record = service
                            .submit(move || {
                                journal.settle(RecoveryPhase::Rejected { diagnostic })?;
                                Ok(journal.record().clone())
                            })?
                            .await
                            .context("preflight rejection was not collected")??;
                        operation.complete(RemoteCompletion::Unchanged)?;
                        return Ok(record);
                    }
                };
                journal = service
                    .submit(move || {
                        journal.dispatch_possible()?;
                        Ok(journal)
                    })?
                    .await
                    .context("dispatch record was not collected")??;
                operation.begin_mutation()?;
                let mut phase = RecoveryPhase::Confirmed {
                    result: Value::Null,
                };
                let mut completion = RemoteCompletion::Confirmed;
                let step_count = plan.len();
                for (index, mutation) in plan.into_iter().enumerate() {
                    let mut step = request.clone();
                    step.mutation = mutation;
                    if matches!(step.mutation, ReviewMutation::MilestoneAssignCreated) {
                        let milestone_number = journal
                            .record()
                            .confirmed_steps
                            .last()
                            .and_then(|receipt| receipt.get("number"))
                            .and_then(Value::as_u64)
                            .filter(|number| *number > 0)
                            .context("created milestone has no durable numeric identity")?;
                        step.mutation = ReviewMutation::MilestoneAssign {
                            milestone_number: Some(milestone_number),
                        };
                    }
                    if index > 0 {
                        let checked = async {
                            let actor = remote.read_actor(step.resource.repository.clone()).await?;
                            ensure!(
                                actor.node_id == step.actor_node_id,
                                "authenticated actor changed between confirmed mutation steps"
                            );
                            remote.validate_review_write(step.clone()).await?;
                            Ok::<_, anyhow::Error>(())
                        }
                        .await;
                        if let Err(failure) = checked {
                            phase = RecoveryPhase::Rejected {
                                diagnostic: bounded_diagnostic(failure.to_string()),
                            };
                            break;
                        }
                    }
                    match remote.mutate_review(step).await {
                        ReviewMutationOutcome::Confirmed(result) => {
                            if index + 1 < step_count {
                                let receipt = result.clone();
                                journal = service
                                    .submit(move || {
                                        journal.confirmed_step(receipt)?;
                                        Ok(journal)
                                    })?
                                    .await
                                    .context("confirmed mutation step was not collected")??;
                            }
                            phase = RecoveryPhase::Confirmed { result };
                        }
                        ReviewMutationOutcome::Rejected(failure) => {
                            phase = RecoveryPhase::Rejected {
                                diagnostic: bounded_diagnostic(failure.to_string()),
                            };
                            break;
                        }
                        ReviewMutationOutcome::OutcomeUnknown(failure) => {
                            phase = RecoveryPhase::OutcomeUnknown {
                                diagnostic: bounded_diagnostic(failure.to_string()),
                            };
                            completion = RemoteCompletion::Uncertain;
                            break;
                        }
                    }
                }
                let record = service
                    .submit(move || {
                        journal.settle(phase)?;
                        Ok(journal.record().clone())
                    })?
                    .await
                    .context("remote outcome publication was not collected")??;
                operation.complete(completion)?;
                Ok(record)
            },
        )
        .await
    }
}

fn validate_assignees(assignee: &[String]) -> Result<()> {
    ensure!(
        assignee.len() <= 100,
        "issue assignee set exceeds 100 entries"
    );
    let mut seen = std::collections::HashSet::new();
    for login in assignee {
        ensure!(
            !login.is_empty()
                && login.len() <= 100
                && login.bytes().all(|byte| byte.is_ascii_alphanumeric()
                    || matches!(byte, b'-' | b'_' | b'[' | b']'))
                && seen.insert(login.to_ascii_lowercase()),
            "invalid or repeated issue assignee"
        );
    }
    Ok(())
}

fn validate_path(path: &str) -> Result<()> {
    ensure!(
        !path.is_empty()
            && path.len() <= 16 * 1024
            && !path.contains(['\n', '\r', '\0'])
            && !path.starts_with('/')
            && !path.split('/').any(|part| part == ".."),
        "invalid review path"
    );
    Ok(())
}

fn validate_commit(commit: &str) -> Result<()> {
    ensure!(
        matches!(commit.len(), 40 | 64) && commit.bytes().all(|byte| byte.is_ascii_hexdigit()),
        "invalid review commit identity"
    );
    Ok(())
}

fn bounded_diagnostic(mut diagnostic: String) -> String {
    let mut end = diagnostic.len().min(32 * 1024);
    while !diagnostic.is_char_boundary(end) {
        end -= 1;
    }
    diagnostic.truncate(end);
    diagnostic
}

#[cfg(test)]
mod tests;
