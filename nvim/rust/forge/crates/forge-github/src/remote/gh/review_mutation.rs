use std::{future::Future, path::PathBuf, pin::Pin};

use anyhow::Result;
use serde_json::{Value, json};

use super::{GhClient, GhDirectory, classify_failure, diagnostic, remote_result};
use crate::remote::{RemoteFailure, RemoteFailureKind};
use crate::review_mutation::{
    GithubReviewWriteRemote, ReviewMutation, ReviewMutationOutcome, ReviewMutationRequest,
};

impl GithubReviewWriteRemote for GhClient {
    fn plan_review_write(
        &self,
        request: ReviewMutationRequest,
    ) -> Pin<Box<dyn Future<Output = Result<Vec<ReviewMutation>, RemoteFailure>> + Send + '_>> {
        Box::pin(self.plan_review_write_at(self.directory.clone(), request))
    }
    fn read_review_write_result(
        &self,
        request: ReviewMutationRequest,
        remote_id: u64,
    ) -> Pin<Box<dyn Future<Output = Result<Value, RemoteFailure>> + Send + '_>> {
        Box::pin(self.read_review_write_result_at(self.directory.clone(), request, remote_id))
    }
    fn validate_review_write(
        &self,
        request: ReviewMutationRequest,
    ) -> Pin<Box<dyn Future<Output = Result<(), RemoteFailure>> + Send + '_>> {
        Box::pin(self.validate_review_write_at(self.directory.clone(), request))
    }
    fn mutate_review(
        &self,
        request: ReviewMutationRequest,
    ) -> Pin<Box<dyn Future<Output = ReviewMutationOutcome> + Send + '_>> {
        Box::pin(self.mutate_review_at(self.directory.clone(), request))
    }
}

impl GithubReviewWriteRemote for GhDirectory {
    fn plan_review_write(
        &self,
        request: ReviewMutationRequest,
    ) -> Pin<Box<dyn Future<Output = Result<Vec<ReviewMutation>, RemoteFailure>> + Send + '_>> {
        Box::pin(
            self.client
                .plan_review_write_at(self.directory.clone(), request),
        )
    }
    fn read_review_write_result(
        &self,
        request: ReviewMutationRequest,
        remote_id: u64,
    ) -> Pin<Box<dyn Future<Output = Result<Value, RemoteFailure>> + Send + '_>> {
        Box::pin(self.client.read_review_write_result_at(
            self.directory.clone(),
            request,
            remote_id,
        ))
    }
    fn validate_review_write(
        &self,
        request: ReviewMutationRequest,
    ) -> Pin<Box<dyn Future<Output = Result<(), RemoteFailure>> + Send + '_>> {
        Box::pin(
            self.client
                .validate_review_write_at(self.directory.clone(), request),
        )
    }
    fn mutate_review(
        &self,
        request: ReviewMutationRequest,
    ) -> Pin<Box<dyn Future<Output = ReviewMutationOutcome> + Send + '_>> {
        Box::pin(
            self.client
                .mutate_review_at(self.directory.clone(), request),
        )
    }
}

impl GhClient {
    async fn plan_review_write_at(
        &self,
        directory: PathBuf,
        request: ReviewMutationRequest,
    ) -> Result<Vec<ReviewMutation>, RemoteFailure> {
        use crate::pull_request::{
            DesiredPullRequestState, PullRequestLifecycle, PullRequestTarget,
        };
        use crate::review_mutation::PullRequestAction;
        if let ReviewMutation::IssueEdit {
            title,
            body,
            add_assignees,
            remove_assignees,
        } = &request.mutation
        {
            let mut plan = Vec::new();
            if title.is_some() || body.is_some() {
                plan.push(ReviewMutation::IssueEdit {
                    title: title.clone(),
                    body: body.clone(),
                    add_assignees: Vec::new(),
                    remove_assignees: Vec::new(),
                });
            }
            if !remove_assignees.is_empty() {
                plan.push(ReviewMutation::IssueAssigneeRemove {
                    assignee: remove_assignees.clone(),
                });
            }
            if !add_assignees.is_empty() {
                plan.push(ReviewMutation::IssueAssigneeAdd {
                    assignee: add_assignees.clone(),
                });
            }
            return Ok(plan);
        }
        if let ReviewMutation::MilestoneCreateAndAssign { title } = &request.mutation {
            return Ok(vec![
                ReviewMutation::MilestoneCreate {
                    title: title.clone(),
                },
                ReviewMutation::MilestoneAssignCreated,
            ]);
        }
        if let ReviewMutation::ReviewerChange { add, remove } = &request.mutation {
            let mut plan = Vec::new();
            if !remove.reviewer.is_empty() || !remove.team.is_empty() {
                plan.push(ReviewMutation::ReviewerRemove {
                    reviewer: remove.reviewer.clone(),
                    team: remove.team.clone(),
                });
            }
            if !add.reviewer.is_empty() || !add.team.is_empty() {
                plan.push(ReviewMutation::ReviewerAdd {
                    reviewer: add.reviewer.clone(),
                    team: add.team.clone(),
                });
            }
            return Ok(plan);
        }
        let ReviewMutation::PullRequestTransition { node_id, desired } = &request.mutation else {
            return Ok(vec![request.mutation]);
        };
        let observed = self
            .read_pr(
                directory,
                PullRequestTarget {
                    repository: request.resource.repository.clone(),
                    number: request.resource.number,
                    node_id: node_id.clone(),
                },
                false,
            )
            .await?;
        if observed.state == PullRequestLifecycle::Merged {
            return Err(invalid("merged pull request cannot change lifecycle"));
        }
        let mut plan = Vec::new();
        if *desired == DesiredPullRequestState::Closed {
            if observed.state != PullRequestLifecycle::Closed {
                plan.push(PullRequestAction::Close);
            }
        } else {
            if observed.state == PullRequestLifecycle::Closed {
                plan.push(PullRequestAction::Reopen);
            }
            if *desired == DesiredPullRequestState::Draft && !observed.is_draft {
                plan.push(PullRequestAction::Draft);
            }
            if *desired == DesiredPullRequestState::Open && observed.is_draft {
                plan.push(PullRequestAction::Ready);
            }
        }
        if plan.is_empty() {
            return Ok(vec![ReviewMutation::PullRequestObserve {
                node_id: node_id.clone(),
            }]);
        }
        Ok(plan
            .into_iter()
            .map(|action| ReviewMutation::PullRequestStep {
                node_id: node_id.clone(),
                action,
            })
            .collect())
    }

    async fn read_review_write_result_at(
        &self,
        directory: PathBuf,
        request: ReviewMutationRequest,
        remote_id: u64,
    ) -> Result<Value, RemoteFailure> {
        request
            .validate()
            .map_err(|error| invalid(error.to_string()))?;
        if matches!(request.mutation, ReviewMutation::PullRequestCreate { .. }) {
            let endpoint = format!(
                "repos/{}/pulls/{remote_id}",
                request.resource.repository.repository_name()
            );
            let value = remote_result(
                self.review_json_at(directory, &request, endpoint, "GET", None)
                    .await,
            )?;
            if value.get("number").and_then(Value::as_u64) != Some(remote_id) {
                return Err(invalid("selected PR creation identity changed"));
            }
            return validate_confirmation(value, &request);
        }
        if let ReviewMutation::PullRequestEdit { node_id, .. }
        | ReviewMutation::PullRequestTransition { node_id, .. } = &request.mutation
        {
            if remote_id != request.resource.number {
                return Err(invalid("selected PR recovery identity changed"));
            }
            let target = crate::pull_request::PullRequestTarget {
                repository: request.resource.repository.clone(),
                number: remote_id,
                node_id: node_id.clone(),
            };
            let observed = self.read_pr(directory, target, true).await?;
            return validate_confirmation(
                serde_json::to_value(observed).expect("serialize PR observation"),
                &request,
            );
        }
        let prefix = format!("repos/{}", request.resource.repository.repository_name());
        let number = request.resource.number;
        if matches!(
            request.mutation,
            ReviewMutation::IssueEdit { .. }
                | ReviewMutation::IssueAssigneeAdd { .. }
                | ReviewMutation::IssueAssigneeRemove { .. }
        ) {
            if remote_id != number {
                return Err(invalid("selected issue recovery identity changed"));
            }
            let value = remote_result(
                self.review_json_at(
                    directory,
                    &request,
                    format!("{prefix}/issues/{number}"),
                    "GET",
                    None,
                )
                .await,
            )?;
            if request.parent_node_id.as_ref().is_some_and(|identity| {
                value.get("node_id").and_then(Value::as_str) != Some(identity)
            }) {
                return Err(invalid("selected issue recovery node changed"));
            }
            return validate_confirmation(value, &request);
        }
        let (endpoint, parent_field, parent_kind) = match request.mutation {
            ReviewMutation::ConversationCreate { .. } | ReviewMutation::ConversationEdit { .. } => {
                (
                    format!("{prefix}/issues/comments/{remote_id}"),
                    "issue_url",
                    "issues",
                )
            }
            ReviewMutation::InlineCreate { .. }
            | ReviewMutation::InlineEdit { .. }
            | ReviewMutation::ReplyCreate { .. } => (
                format!("{prefix}/pulls/comments/{remote_id}"),
                "pull_request_url",
                "pulls",
            ),
            ReviewMutation::ReviewSubmit { .. } | ReviewMutation::PendingReviewCreate { .. } => (
                format!("{prefix}/pulls/{number}/reviews/{remote_id}"),
                "pull_request_url",
                "pulls",
            ),
            _ => {
                return Err(invalid(
                    "this operation requires explicit unknown closure or operation-specific reconciliation",
                ));
            }
        };
        let value = remote_result(
            self.review_json_at(directory.clone(), &request, endpoint, "GET", None)
                .await,
        )?;
        if value.get("id").and_then(Value::as_u64) != Some(remote_id)
            || value
                .get(parent_field)
                .and_then(Value::as_str)
                .is_none_or(|url| !url.ends_with(&format!("/{prefix}/{parent_kind}/{number}")))
            || value.pointer("/user/node_id").and_then(Value::as_str)
                != Some(request.actor_node_id.as_str())
        {
            return Err(invalid(
                "selected recovery result has a different identity, parent, or actor",
            ));
        }
        let value = validate_confirmation(value, &request)?;
        self.validate_submitted_members_at(directory, &request, &value)
            .await?;
        Ok(value)
    }

    async fn validate_submitted_members_at(
        &self,
        directory: PathBuf,
        request: &ReviewMutationRequest,
        value: &Value,
    ) -> Result<(), RemoteFailure> {
        if let ReviewMutation::ReviewSubmit {
            pending_comment: Some(captured),
            comment,
            pending_review_id,
            ..
        } = &request.mutation
        {
            let review = value
                .get("id")
                .and_then(Value::as_u64)
                .ok_or_else(|| invalid("submitted review omitted identity"))?;
            let observed = self
                .pending_members_at(
                    directory,
                    &request.resource.repository,
                    request.resource.number,
                    review,
                )
                .await?;
            if pending_review_id.is_some() && &observed != captured {
                return Err(invalid(
                    "submitted review membership or text differs from capture",
                ));
            }
            if pending_review_id.is_none()
                && (observed.len() != comment.len()
                    || observed.iter().zip(comment).any(|(observed, captured)| {
                        observed.body != captured.body
                            || observed.path != captured.path
                            || observed.line != Some(captured.line)
                            || observed.side.as_deref()
                                != Some(match captured.side {
                                    crate::review_mutation::DiffSide::Left => "LEFT",
                                    crate::review_mutation::DiffSide::Right => "RIGHT",
                                })
                            || observed.position.is_some()
                    }))
            {
                return Err(invalid("submitted review comments differ from capture"));
            }
        }
        Ok(())
    }

    async fn review_json_at(
        &self,
        directory: PathBuf,
        request: &ReviewMutationRequest,
        endpoint: String,
        method: &'static str,
        body: Option<Value>,
    ) -> Result<Result<Value, RemoteFailure>> {
        let encoded = body.map(|body| serde_json::to_vec(&body)).transpose()?;
        let argument = vec![
            "api".into(),
            "--hostname".into(),
            request.resource.repository.hostname().into(),
            "--method".into(),
            method.into(),
            endpoint.into(),
        ];
        self.run_native(directory, argument, encoded, 4096, |output| {
            if !output.status.success() {
                return Err(classify_failure(diagnostic(&String::from_utf8_lossy(
                    if output.stderr.is_empty() {
                        &output.stdout
                    } else {
                        &output.stderr
                    },
                ))));
            }
            if output.stdout.is_empty() {
                return Ok(Value::Null);
            }
            let value: Value = serde_json::from_slice(&output.stdout)
                .map_err(|error| invalid(error.to_string()))?;
            if value
                .get("errors")
                .and_then(Value::as_array)
                .is_some_and(|error| !error.is_empty())
            {
                return Err(invalid("GraphQL write returned errors"));
            }
            Ok(value)
        })
        .await
    }

    async fn validate_review_write_at(
        &self,
        directory: PathBuf,
        request: ReviewMutationRequest,
    ) -> Result<(), RemoteFailure> {
        request
            .validate()
            .map_err(|error| invalid(error.to_string()))?;
        let prefix = format!("repos/{}", request.resource.repository.repository_name());
        let number = request.resource.number;
        if let ReviewMutation::PullRequestCreate {
            repository_node_id,
            base,
            head,
            head_commit,
            ..
        } = &request.mutation
        {
            let repository = remote_result(
                self.review_json_at(directory.clone(), &request, prefix.clone(), "GET", None)
                    .await,
            )?;
            if repository.get("node_id").and_then(Value::as_str) != Some(repository_node_id) {
                return Err(invalid("creation repository identity changed"));
            }
            for (branch, expected) in [(base, None), (head, Some(head_commit))] {
                let endpoint = format!("{prefix}/git/ref/heads/{}", encode_component(branch));
                let observed = remote_result(
                    self.review_json_at(directory.clone(), &request, endpoint, "GET", None)
                        .await,
                )?;
                if observed.get("ref").and_then(Value::as_str)
                    != Some(format!("refs/heads/{branch}").as_str())
                    || observed.pointer("/object/type").and_then(Value::as_str) != Some("commit")
                    || expected.is_some_and(|expected| {
                        observed.pointer("/object/sha").and_then(Value::as_str) != Some(expected)
                    })
                {
                    return Err(invalid(
                        "creation branch or head commit changed after capture",
                    ));
                }
            }
            return Ok(());
        }
        if matches!(
            request.mutation,
            ReviewMutation::NotificationRead | ReviewMutation::NotificationDone
        ) {
            let thread = remote_result(
                self.review_json_at(
                    directory,
                    &request,
                    format!("notifications/threads/{number}"),
                    "GET",
                    None,
                )
                .await,
            )?;
            if thread
                .pointer("/repository/full_name")
                .and_then(Value::as_str)
                .is_none_or(|name| {
                    !name.eq_ignore_ascii_case(&request.resource.repository.repository_name())
                })
            {
                return Err(invalid("notification belongs to another repository"));
            }
            return Ok(());
        }
        let issue = matches!(
            request.mutation,
            ReviewMutation::IssueEdit { .. }
                | ReviewMutation::IssueAssigneeAdd { .. }
                | ReviewMutation::IssueAssigneeRemove { .. }
        );
        let parent = remote_result(
            self.review_json_at(
                directory.clone(),
                &request,
                format!(
                    "{prefix}/{}/{number}",
                    if issue { "issues" } else { "pulls" }
                ),
                "GET",
                None,
            )
            .await,
        )?;
        if parent.get("number").and_then(Value::as_u64) != Some(number) {
            return Err(invalid("remote parent identity changed"));
        }
        let expected_node = match &request.mutation {
            ReviewMutation::PullRequestEdit { node_id, .. }
            | ReviewMutation::PullRequestTransition { node_id, .. }
            | ReviewMutation::PullRequestStep { node_id, .. }
            | ReviewMutation::PullRequestObserve { node_id } => Some(node_id),
            _ => request.parent_node_id.as_ref(),
        };
        if expected_node
            .is_some_and(|node_id| parent.get("node_id").and_then(Value::as_str) != Some(node_id))
        {
            return Err(invalid("pull request node identity changed"));
        }
        let commit = match &request.mutation {
            ReviewMutation::InlineCreate { commit_id, .. }
            | ReviewMutation::PendingReviewCreate { commit_id }
            | ReviewMutation::PendingReviewComment { commit_id, .. } => Some(commit_id),
            ReviewMutation::ReviewSubmit {
                commit_id: Some(commit_id),
                ..
            } => Some(commit_id),
            _ => None,
        };
        if commit.is_some_and(|commit| {
            parent.pointer("/head/sha").and_then(Value::as_str) != Some(commit.as_str())
        }) {
            return Err(invalid("pull request head changed after capture"));
        }
        let comment = match request.mutation {
            ReviewMutation::ConversationEdit { comment_id, .. }
            | ReviewMutation::ConversationDelete { comment_id } => Some((comment_id, true, true)),
            ReviewMutation::InlineEdit { comment_id, .. }
            | ReviewMutation::InlineDelete { comment_id } => Some((comment_id, false, true)),
            ReviewMutation::ReplyCreate {
                parent_comment_id, ..
            } => Some((parent_comment_id, false, false)),
            _ => None,
        };
        if let Some((id, conversation, owned)) = comment {
            let observed = remote_result(
                self.review_json_at(
                    directory.clone(),
                    &request,
                    format!(
                        "{prefix}/{}/comments/{id}",
                        if conversation { "issues" } else { "pulls" }
                    ),
                    "GET",
                    None,
                )
                .await,
            )?;
            let parent_field = if conversation {
                "issue_url"
            } else {
                "pull_request_url"
            };
            let parent_kind = if conversation { "issues" } else { "pulls" };
            let expected_suffix = format!("/{prefix}/{parent_kind}/{number}");
            if observed.get("id").and_then(Value::as_u64) != Some(id)
                || observed
                    .get(parent_field)
                    .and_then(Value::as_str)
                    .is_none_or(|url| !url.ends_with(&expected_suffix))
            {
                return Err(invalid("comment identity or parent changed"));
            }
            if owned
                && observed.pointer("/user/node_id").and_then(Value::as_str)
                    != Some(request.actor_node_id.as_str())
            {
                return Err(invalid("comment is not authored by the captured actor"));
            }
        }
        if let ReviewMutation::ReviewSubmit {
            pending_review_id: Some(id),
            ..
        } = request.mutation
        {
            let observed = remote_result(
                self.review_json_at(
                    directory.clone(),
                    &request,
                    format!("{prefix}/pulls/{number}/reviews/{id}"),
                    "GET",
                    None,
                )
                .await,
            )?;
            if observed.get("id").and_then(Value::as_u64) != Some(id)
                || observed.get("state").and_then(Value::as_str) != Some("PENDING")
                || observed.pointer("/user/node_id").and_then(Value::as_str)
                    != Some(request.actor_node_id.as_str())
            {
                return Err(invalid("pending review identity, state, or actor changed"));
            }
            if let ReviewMutation::ReviewSubmit {
                pending_comment: Some(ref captured),
                ..
            } = request.mutation
            {
                let observed = self
                    .pending_members_at(directory.clone(), &request.resource.repository, number, id)
                    .await?;
                if &observed != captured {
                    return Err(invalid(
                        "pending review membership or text changed after capture",
                    ));
                }
            }
        }
        if let ReviewMutation::PendingReviewComment {
            ref review_node_id, ..
        } = request.mutation
        {
            let body = json!({"query":"query($id:ID!){node(id:$id){... on PullRequestReview{id state author{... on User{id}} pullRequest{number repository{nameWithOwner}}}}}","variables":{"id":review_node_id}});
            let observed = remote_result(
                self.review_json_at(directory, &request, "graphql".into(), "POST", Some(body))
                    .await,
            )?;
            let review = observed
                .pointer("/data/node")
                .ok_or_else(|| invalid("pending review is missing"))?;
            if review.get("id").and_then(Value::as_str) != Some(review_node_id)
                || review.get("state").and_then(Value::as_str) != Some("PENDING")
                || review.pointer("/author/id").and_then(Value::as_str)
                    != Some(&request.actor_node_id)
                || review
                    .pointer("/pullRequest/number")
                    .and_then(Value::as_u64)
                    != Some(number)
                || review
                    .pointer("/pullRequest/repository/nameWithOwner")
                    .and_then(Value::as_str)
                    .is_none_or(|name| {
                        !name.eq_ignore_ascii_case(&request.resource.repository.repository_name())
                    })
            {
                return Err(invalid(
                    "pending review identity, parent, state, or actor changed",
                ));
            }
        }
        Ok(())
    }

    async fn mutate_review_at(
        &self,
        directory: PathBuf,
        request: ReviewMutationRequest,
    ) -> ReviewMutationOutcome {
        use crate::pull_request::{
            PullRequestEdit, PullRequestMutation, PullRequestMutationOutcome, PullRequestTarget,
        };
        use crate::review_mutation::PullRequestAction;
        let native_pr = match &request.mutation {
            ReviewMutation::PullRequestEdit {
                node_id,
                title,
                body,
            } => Some((
                node_id,
                PullRequestMutation::Edit(Box::new(PullRequestEdit {
                    title: title.clone(),
                    body: body.clone(),
                })),
            )),
            ReviewMutation::PullRequestStep { node_id, action } => Some((
                node_id,
                match action {
                    PullRequestAction::Close => PullRequestMutation::Close,
                    PullRequestAction::Reopen => PullRequestMutation::Reopen,
                    PullRequestAction::Draft => PullRequestMutation::Draft,
                    PullRequestAction::Ready => PullRequestMutation::Ready,
                },
            )),
            _ => None,
        };
        if let Some((node_id, mutation)) = native_pr {
            let target = PullRequestTarget {
                repository: request.resource.repository.clone(),
                number: request.resource.number,
                node_id: node_id.clone(),
            };
            return match self.mutate_pr(directory, target, mutation).await {
                PullRequestMutationOutcome::Confirmed(state) => match validate_confirmation(
                    serde_json::to_value(state).expect("serialize PR state"),
                    &request,
                ) {
                    Ok(result) => ReviewMutationOutcome::Confirmed(result),
                    Err(failure) => ReviewMutationOutcome::OutcomeUnknown(failure),
                },
                PullRequestMutationOutcome::Rejected(failure) => {
                    ReviewMutationOutcome::Rejected(failure)
                }
                PullRequestMutationOutcome::Uncertain(failure) => {
                    ReviewMutationOutcome::OutcomeUnknown(failure)
                }
            };
        }
        if let ReviewMutation::PullRequestObserve { node_id } = &request.mutation {
            let target = PullRequestTarget {
                repository: request.resource.repository.clone(),
                number: request.resource.number,
                node_id: node_id.clone(),
            };
            return match self.read_pr(directory, target, false).await {
                Ok(result) => ReviewMutationOutcome::Confirmed(
                    serde_json::to_value(result).expect("serialize PR state"),
                ),
                Err(failure) => ReviewMutationOutcome::Rejected(failure),
            };
        }
        let prefix = format!("repos/{}", request.resource.repository.repository_name());
        let number = request.resource.number;
        let pull = format!("{prefix}/pulls/{number}");
        let (endpoint, method, body) = match &request.mutation {
            ReviewMutation::PullRequestCreate {
                title,
                body,
                base,
                head,
                draft,
                ..
            } => (
                format!("{prefix}/pulls"),
                "POST",
                json!({"title":title,"body":body,"base":base,"head":head,"draft":draft}),
            ),
            ReviewMutation::PullRequestTransition { .. }
            | ReviewMutation::ReviewerChange { .. }
            | ReviewMutation::MilestoneCreateAndAssign { .. }
            | ReviewMutation::MilestoneAssignCreated
            | ReviewMutation::PullRequestStep { .. }
            | ReviewMutation::PullRequestEdit { .. }
            | ReviewMutation::PullRequestObserve { .. } => {
                return ReviewMutationOutcome::Rejected(invalid(
                    "PR mutation requires its validated native plan",
                ));
            }
            ReviewMutation::ConversationCreate { body } => (
                format!("{prefix}/issues/{number}/comments"),
                "POST",
                json!({"body":body}),
            ),
            ReviewMutation::InlineCreate {
                body,
                path,
                line,
                side,
                commit_id,
                start_line,
                start_side,
            } => {
                let mut payload =
                    json!({"body":body,"path":path,"line":line,"side":side,"commit_id":commit_id});
                if let Some(start_line) = start_line {
                    payload["start_line"] = json!(start_line);
                }
                if let Some(start_side) = start_side {
                    payload["start_side"] = json!(start_side);
                }
                (format!("{pull}/comments"), "POST", payload)
            }
            ReviewMutation::ReplyCreate {
                body,
                parent_comment_id,
            } => (
                format!("{pull}/comments/{parent_comment_id}/replies"),
                "POST",
                json!({"body":body}),
            ),
            ReviewMutation::PendingReviewCreate { commit_id } => (
                format!("{pull}/reviews"),
                "POST",
                json!({"commit_id":commit_id}),
            ),
            ReviewMutation::PendingReviewComment {
                body,
                review_node_id,
                path,
                position,
                commit_id,
            } => (
                "graphql".into(),
                "POST",
                json!({"query":"mutation($input:AddPullRequestReviewCommentInput!){addPullRequestReviewComment(input:$input){comment{id databaseId body viewerDidAuthor path position author{login} pullRequestReview{id}}}}","variables":{"input":{"pullRequestReviewId":review_node_id,"body":body,"path":path,"position":position,"commitOID":commit_id}}}),
            ),
            ReviewMutation::ReviewSubmit {
                body,
                event,
                pending_review_id,
                commit_id,
                comment,
                ..
            } => {
                let mut payload = json!({"body":body,"event":event});
                if !comment.is_empty() {
                    payload["comments"] = json!(comment);
                }
                if pending_review_id.is_none() {
                    if let Some(commit_id) = commit_id {
                        payload["commit_id"] = json!(commit_id);
                    }
                }
                (
                    pending_review_id.map_or_else(
                        || format!("{pull}/reviews"),
                        |id| format!("{pull}/reviews/{id}/events"),
                    ),
                    "POST",
                    payload,
                )
            }
            ReviewMutation::ConversationEdit { comment_id, body } => (
                format!("{prefix}/issues/comments/{comment_id}"),
                "PATCH",
                json!({"body":body}),
            ),
            ReviewMutation::InlineEdit { comment_id, body } => (
                format!("{prefix}/pulls/comments/{comment_id}"),
                "PATCH",
                json!({"body":body}),
            ),
            ReviewMutation::ConversationDelete { comment_id } => (
                format!("{prefix}/issues/comments/{comment_id}"),
                "DELETE",
                Value::Null,
            ),
            ReviewMutation::InlineDelete { comment_id } => (
                format!("{prefix}/pulls/comments/{comment_id}"),
                "DELETE",
                Value::Null,
            ),
            ReviewMutation::ReviewerAdd { reviewer, team } => (
                format!("{pull}/requested_reviewers"),
                "POST",
                json!({"reviewers":reviewer,"team_reviewers":team}),
            ),
            ReviewMutation::ReviewerRemove { reviewer, team } => (
                format!("{pull}/requested_reviewers"),
                "DELETE",
                json!({"reviewers":reviewer,"team_reviewers":team}),
            ),
            ReviewMutation::MilestoneAssign { milestone_number } => (
                format!("{prefix}/issues/{number}"),
                "PATCH",
                json!({"milestone":milestone_number}),
            ),
            ReviewMutation::MilestoneCreate { title } => (
                format!("{prefix}/milestones"),
                "POST",
                json!({"title":title}),
            ),
            ReviewMutation::IssueEdit { title, body, .. } => {
                let mut fields = serde_json::Map::new();
                if let Some(title) = title {
                    fields.insert("title".into(), json!(title));
                }
                if let Some(body) = body {
                    fields.insert("body".into(), json!(body));
                }
                (
                    format!("{prefix}/issues/{number}"),
                    "PATCH",
                    Value::Object(fields),
                )
            }
            ReviewMutation::IssueAssigneeAdd { assignee } => (
                format!("{prefix}/issues/{number}/assignees"),
                "POST",
                json!({"assignees":assignee}),
            ),
            ReviewMutation::IssueAssigneeRemove { assignee } => (
                format!("{prefix}/issues/{number}/assignees"),
                "DELETE",
                json!({"assignees":assignee}),
            ),
            ReviewMutation::NotificationRead => (
                format!("notifications/threads/{number}"),
                "PATCH",
                Value::Null,
            ),
            ReviewMutation::NotificationDone => (
                format!("notifications/threads/{number}"),
                "DELETE",
                Value::Null,
            ),
        };
        let response = self
            .review_json_at(
                directory.clone(),
                &request,
                endpoint,
                method,
                (!body.is_null()).then_some(body),
            )
            .await;
        let not_started = response.as_ref().err().is_some_and(|error| {
            error
                .downcast_ref::<forge_git::read_pool::ReadAdmissionError>()
                .is_some()
        });
        let response = match remote_result(response) {
            Ok(_)
                if matches!(
                    request.mutation,
                    ReviewMutation::ReviewerAdd { .. } | ReviewMutation::ReviewerRemove { .. }
                ) =>
            {
                remote_result(
                    self.review_json_at(
                        directory.clone(),
                        &request,
                        format!("{pull}/requested_reviewers"),
                        "GET",
                        None,
                    )
                    .await,
                )
            }
            result => result,
        };
        match response {
            Ok(value) => match validate_confirmation(value, &request) {
                Ok(value) => match self
                    .validate_submitted_members_at(directory, &request, &value)
                    .await
                {
                    Ok(()) => ReviewMutationOutcome::Confirmed(value),
                    Err(failure) => ReviewMutationOutcome::OutcomeUnknown(failure),
                },
                Err(failure) => ReviewMutationOutcome::OutcomeUnknown(failure),
            },
            Err(failure) if not_started => ReviewMutationOutcome::Rejected(failure),
            Err(failure) => ReviewMutationOutcome::OutcomeUnknown(failure),
        }
    }
}

fn validate_confirmation(
    value: Value,
    request: &ReviewMutationRequest,
) -> Result<Value, RemoteFailure> {
    if let ReviewMutation::PullRequestCreate {
        repository_node_id,
        title,
        body,
        base,
        head,
        head_commit,
        draft,
    } = &request.mutation
    {
        if value
            .get("number")
            .and_then(Value::as_u64)
            .is_none_or(|number| number == 0 || number > i32::MAX as u64)
            || value
                .get("node_id")
                .and_then(Value::as_str)
                .is_none_or(str::is_empty)
            || value.get("title").and_then(Value::as_str) != Some(title)
            || value.get("body").and_then(Value::as_str) != Some(body)
            || value.get("draft").and_then(Value::as_bool) != Some(*draft)
            || value.pointer("/base/ref").and_then(Value::as_str) != Some(base)
            || value.pointer("/head/ref").and_then(Value::as_str) != Some(head)
            || value.pointer("/head/sha").and_then(Value::as_str) != Some(head_commit)
            || value.pointer("/base/repo/node_id").and_then(Value::as_str)
                != Some(repository_node_id)
            || value.pointer("/user/node_id").and_then(Value::as_str)
                != Some(&request.actor_node_id)
        {
            return Err(invalid(
                "PR creation confirmation changed captured identity or text",
            ));
        }
        return Ok(value);
    }
    let expected_body = match &request.mutation {
        ReviewMutation::ConversationCreate { body }
        | ReviewMutation::InlineCreate { body, .. }
        | ReviewMutation::ReplyCreate { body, .. }
        | ReviewMutation::ConversationEdit { body, .. }
        | ReviewMutation::InlineEdit { body, .. }
        | ReviewMutation::ReviewSubmit { body, .. } => Some(body),
        _ => None,
    };
    if let Some(body) = expected_body {
        if value.get("body").and_then(Value::as_str) != Some(body)
            || value.pointer("/user/node_id").and_then(Value::as_str)
                != Some(&request.actor_node_id)
            || value
                .get("id")
                .and_then(Value::as_u64)
                .is_none_or(|id| id == 0)
        {
            return Err(invalid(
                "remote write confirmation does not match captured body and actor",
            ));
        }
    }
    if let ReviewMutation::PendingReviewComment {
        body,
        review_node_id,
        ..
    } = &request.mutation
    {
        let comment = value
            .pointer("/data/addPullRequestReviewComment/comment")
            .ok_or_else(|| invalid("pending comment response is missing"))?;
        if comment.get("body").and_then(Value::as_str) != Some(body)
            || comment.get("viewerDidAuthor").and_then(Value::as_bool) != Some(true)
            || comment
                .pointer("/pullRequestReview/id")
                .and_then(Value::as_str)
                != Some(review_node_id)
        {
            return Err(invalid(
                "pending comment confirmation changed capture identity",
            ));
        }
    }
    let matches = match &request.mutation {
        ReviewMutation::PullRequestTransition { desired, .. } => match desired {
            crate::pull_request::DesiredPullRequestState::Closed => {
                value.get("state").and_then(Value::as_str) == Some("CLOSED")
            }
            crate::pull_request::DesiredPullRequestState::Open => {
                value.get("state").and_then(Value::as_str) == Some("OPEN")
                    && value.get("isDraft").and_then(Value::as_bool) == Some(false)
            }
            crate::pull_request::DesiredPullRequestState::Draft => {
                value.get("state").and_then(Value::as_str) == Some("OPEN")
                    && value.get("isDraft").and_then(Value::as_bool) == Some(true)
            }
        },
        ReviewMutation::PullRequestEdit { title, body, .. } => {
            title
                .as_ref()
                .is_none_or(|title| value.get("title").and_then(Value::as_str) == Some(title))
                && body
                    .as_ref()
                    .is_none_or(|body| value.get("body").and_then(Value::as_str) == Some(body))
        }
        ReviewMutation::PullRequestStep { action, .. } => match action {
            crate::review_mutation::PullRequestAction::Close => {
                value.get("state").and_then(Value::as_str) == Some("CLOSED")
            }
            crate::review_mutation::PullRequestAction::Reopen => {
                value.get("state").and_then(Value::as_str) == Some("OPEN")
            }
            crate::review_mutation::PullRequestAction::Draft => {
                value.get("state").and_then(Value::as_str) == Some("OPEN")
                    && value.get("isDraft").and_then(Value::as_bool) == Some(true)
            }
            crate::review_mutation::PullRequestAction::Ready => {
                value.get("state").and_then(Value::as_str) == Some("OPEN")
                    && value.get("isDraft").and_then(Value::as_bool) == Some(false)
            }
        },
        ReviewMutation::ReviewerAdd { reviewer, team }
        | ReviewMutation::ReviewerRemove { reviewer, team } => {
            let expected_present = matches!(request.mutation, ReviewMutation::ReviewerAdd { .. });
            let users = value.get("users").and_then(Value::as_array);
            let teams = value.get("teams").and_then(Value::as_array);
            users.zip(teams).is_some_and(|(users, teams)| {
                reviewer.iter().all(|name| {
                    users.iter().any(|user| {
                        user.get("login")
                            .and_then(Value::as_str)
                            .is_some_and(|login| login.eq_ignore_ascii_case(name))
                    }) == expected_present
                }) && team.iter().all(|name| {
                    teams.iter().any(|team| {
                        team.get("slug")
                            .and_then(Value::as_str)
                            .is_some_and(|slug| slug.eq_ignore_ascii_case(name))
                    }) == expected_present
                })
            })
        }
        ReviewMutation::InlineCreate {
            path,
            line,
            commit_id,
            side,
            start_line,
            start_side,
            ..
        } => {
            value.get("path").and_then(Value::as_str) == Some(path)
                && value.get("line").and_then(Value::as_u64) == Some(u64::from(*line))
                && value.get("commit_id").and_then(Value::as_str) == Some(commit_id)
                && value.get("side") == Some(&json!(side))
                && start_line.is_none_or(|line| {
                    value.get("start_line").and_then(Value::as_u64) == Some(u64::from(line))
                })
                && start_side
                    .as_ref()
                    .is_none_or(|side| value.get("start_side") == Some(&json!(side)))
        }
        ReviewMutation::ReplyCreate {
            parent_comment_id, ..
        } => value.get("in_reply_to_id").and_then(Value::as_u64) == Some(*parent_comment_id),
        ReviewMutation::ConversationEdit { comment_id, .. }
        | ReviewMutation::InlineEdit { comment_id, .. } => {
            value.get("id").and_then(Value::as_u64) == Some(*comment_id)
        }
        ReviewMutation::PendingReviewCreate { commit_id } => {
            value.get("state").and_then(Value::as_str) == Some("PENDING")
                && value.get("commit_id").and_then(Value::as_str) == Some(commit_id)
                && value.pointer("/user/node_id").and_then(Value::as_str)
                    == Some(&request.actor_node_id)
                && value
                    .get("id")
                    .and_then(Value::as_u64)
                    .is_some_and(|id| id > 0)
        }
        ReviewMutation::ReviewSubmit {
            event,
            pending_review_id,
            ..
        } => {
            let expected = match event {
                crate::review_mutation::ReviewEvent::Approve => "APPROVED",
                crate::review_mutation::ReviewEvent::RequestChanges => "CHANGES_REQUESTED",
                crate::review_mutation::ReviewEvent::Comment => "COMMENTED",
            };
            value.get("state").and_then(Value::as_str) == Some(expected)
                && pending_review_id
                    .is_none_or(|id| value.get("id").and_then(Value::as_u64) == Some(id))
        }
        ReviewMutation::MilestoneAssign { milestone_number } => {
            value.get("number").and_then(Value::as_u64) == Some(request.resource.number)
                && value.pointer("/milestone/number").and_then(Value::as_u64) == *milestone_number
        }
        ReviewMutation::MilestoneCreate { title } => {
            value.get("title").and_then(Value::as_str) == Some(title)
                && value
                    .get("number")
                    .and_then(Value::as_u64)
                    .is_some_and(|number| number > 0)
        }
        ReviewMutation::IssueEdit {
            title,
            body,
            add_assignees,
            remove_assignees,
        } => {
            value.get("number").and_then(Value::as_u64) == Some(request.resource.number)
                && title
                    .as_ref()
                    .is_none_or(|title| value.get("title").and_then(Value::as_str) == Some(title))
                && body
                    .as_ref()
                    .is_none_or(|body| value.get("body").and_then(Value::as_str) == Some(body))
                && confirm_assignees(&value, add_assignees, remove_assignees)
        }
        ReviewMutation::IssueAssigneeAdd { assignee } => {
            value.get("number").and_then(Value::as_u64) == Some(request.resource.number)
                && confirm_assignees(&value, assignee, &[])
        }
        ReviewMutation::IssueAssigneeRemove { assignee } => {
            value.get("number").and_then(Value::as_u64) == Some(request.resource.number)
                && confirm_assignees(&value, &[], assignee)
        }
        _ => true,
    };
    if !matches {
        return Err(invalid(
            "remote response does not confirm the captured mutation target",
        ));
    }
    Ok(value)
}

fn confirm_assignees(value: &Value, added: &[String], removed: &[String]) -> bool {
    if added.is_empty() && removed.is_empty() {
        return true;
    }
    let Some(assignee) = value.get("assignees").and_then(Value::as_array) else {
        return false;
    };
    let mut login = std::collections::HashSet::new();
    for entry in assignee {
        let Some(name) = entry.get("login").and_then(Value::as_str) else {
            return false;
        };
        login.insert(name.to_ascii_lowercase());
    }
    added
        .iter()
        .all(|name| login.contains(&name.to_ascii_lowercase()))
        && removed
            .iter()
            .all(|name| !login.contains(&name.to_ascii_lowercase()))
}

fn encode_component(value: &str) -> String {
    let mut encoded = String::with_capacity(value.len());
    for byte in value.bytes() {
        if byte.is_ascii_alphanumeric() || matches!(byte, b'-' | b'_' | b'.' | b'~') {
            encoded.push(char::from(byte));
        } else {
            use std::fmt::Write;
            write!(encoded, "%{byte:02X}").expect("string write");
        }
    }
    encoded
}

fn invalid(message: impl Into<String>) -> RemoteFailure {
    RemoteFailure {
        kind: RemoteFailureKind::InvalidResponse,
        message: message.into(),
    }
}
