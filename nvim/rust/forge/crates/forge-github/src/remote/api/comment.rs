use std::{future::Future, pin::Pin};

use serde::Deserialize;
use serde_json::{Value, json};

use super::{ApiRequest, ApiResponse, GithubClient, diagnostic, remote_result, request_not_sent};
use crate::comment::ConversationCommentCreation;
use crate::comment::{
    CommentKind, CommentMutationOutcome, CommentOperation, CommentState, CommentTarget,
    GithubCommentRemote,
};
use crate::remote::{RemoteFailure, RemoteFailureKind};

const FIELDS: &str = "__typename id databaseId: fullDatabaseId body url viewerDidAuthor";

#[derive(Deserialize)]
struct CommentResponse {
    data: Option<Value>,
}

impl GithubCommentRemote for GithubClient {
    fn create_conversation_comment(
        &self,
        request: ConversationCommentCreation,
    ) -> Pin<Box<dyn Future<Output = CommentMutationOutcome> + Send + '_>> {
        Box::pin(self.create_conversation_at(request))
    }

    fn read_comment(
        &self,
        target: CommentTarget,
    ) -> Pin<
        Box<dyn Future<Output = anyhow::Result<Option<CommentState>, RemoteFailure>> + Send + '_>,
    > {
        Box::pin(self.read_comment_at(target))
    }

    fn mutate_comment(
        &self,
        target: CommentTarget,
        operation: CommentOperation,
    ) -> Pin<Box<dyn Future<Output = CommentMutationOutcome> + Send + '_>> {
        Box::pin(self.mutate_comment_at(target, operation))
    }
}

impl GithubClient {
    async fn create_conversation_at(
        &self,
        request: ConversationCommentCreation,
    ) -> CommentMutationOutcome {
        if let Err(failure) = request.validate() {
            return CommentMutationOutcome::Rejected(invalid(failure));
        }
        let query = format!(
            "mutation($input:AddCommentInput!) {{ mutation: addComment(input:$input) {{ clientMutationId subject {{ id }} commentEdge {{ node {{ {FIELDS} }} }} }} }}"
        );
        let payload = json!({"query":query,"variables":{"input":{"subjectId":request.target.node_id,"body":request.body,"clientMutationId":request.receipt}}});
        let api_request = ApiRequest::graphql(request.target.repository.hostname(), &payload);
        let result = self
            .send(api_request, move |response| {
                let data = decode_data(response)?;
                let mutation = data
                    .get("mutation")
                    .ok_or_else(|| invalid("creation omitted its receipt"))?;
                if mutation.get("clientMutationId").and_then(Value::as_str)
                    != Some(request.receipt.as_str())
                    || mutation.pointer("/subject/id").and_then(Value::as_str)
                        != Some(request.target.node_id.as_str())
                {
                    return Err(invalid(
                        "creation receipt or PR identity does not match its request",
                    ));
                }
                let state: CommentState = serde_json::from_value(
                    mutation
                        .pointer("/commentEdge/node")
                        .cloned()
                        .ok_or_else(|| invalid("creation omitted its comment"))?,
                )
                .map_err(invalid)?;
                request.validate_state(&state).map_err(invalid)?;
                Ok(Some(state))
            })
            .await;
        mutation_result(result)
    }

    async fn read_comment_at(
        &self,
        target: CommentTarget,
    ) -> Result<Option<CommentState>, RemoteFailure> {
        target.validate().map_err(invalid)?;
        let query = format!(
            "query($id:ID!) {{ node(id:$id) {{ ... on IssueComment {{ {FIELDS} }} ... on PullRequestReviewComment {{ {FIELDS} }} }} }}"
        );
        let payload = json!({"query":query,"variables":{"id":target.node_id}});
        remote_result(
            self.send(
                ApiRequest::graphql(target.pull_request.repository.hostname(), &payload),
                move |response| {
                    let data = decode_data(response)?;
                    let node = data
                        .get("node")
                        .ok_or_else(|| invalid("comment read omitted node"))?;
                    if node.is_null() {
                        return Ok(None);
                    }
                    let state: CommentState =
                        serde_json::from_value(node.clone()).map_err(invalid)?;
                    state.validate(&target).map_err(invalid)?;
                    Ok(Some(state))
                },
            )
            .await,
        )
    }

    async fn mutate_comment_at(
        &self,
        target: CommentTarget,
        operation: CommentOperation,
    ) -> CommentMutationOutcome {
        if let Err(failure) = target.validate().and_then(|()| operation.validate()) {
            return CommentMutationOutcome::Rejected(invalid(failure));
        }
        let (field, input, body, receipt, returned) = match (&target.kind, &operation) {
            (CommentKind::IssueComment, CommentOperation::Edit { body, receipt }) => (
                "updateIssueComment",
                "UpdateIssueCommentInput",
                Some(body),
                receipt,
                Some("issueComment"),
            ),
            (CommentKind::PullRequestReviewComment, CommentOperation::Edit { body, receipt }) => (
                "updatePullRequestReviewComment",
                "UpdatePullRequestReviewCommentInput",
                Some(body),
                receipt,
                Some("pullRequestReviewComment"),
            ),
            (CommentKind::IssueComment, CommentOperation::Delete { receipt }) => (
                "deleteIssueComment",
                "DeleteIssueCommentInput",
                None,
                receipt,
                None,
            ),
            (CommentKind::PullRequestReviewComment, CommentOperation::Delete { receipt }) => (
                "deletePullRequestReviewComment",
                "DeletePullRequestReviewCommentInput",
                None,
                receipt,
                None,
            ),
            (_, CommentOperation::Reconcile) => {
                return CommentMutationOutcome::Rejected(invalid(
                    "reconciliation cannot issue a mutation",
                ));
            }
        };
        let selection = returned
            .map(|field| format!("comment: {field} {{ {FIELDS} }}"))
            .unwrap_or_default();
        let query = format!(
            "mutation($input:{input}!) {{ mutation: {field}(input:$input) {{ clientMutationId {selection} }} }}"
        );
        let identity_field =
            if target.kind == CommentKind::PullRequestReviewComment && body.is_some() {
                "pullRequestReviewCommentId"
            } else {
                "id"
            };
        let mut variables = json!({"clientMutationId":receipt});
        variables[identity_field] = json!(target.node_id);
        if let Some(body) = body {
            variables["body"] = json!(body);
        }
        let payload = json!({"query":query,"variables":{"input":variables}});
        drop(variables);
        let receipt = receipt.clone();
        let api_request = ApiRequest::graphql(target.pull_request.repository.hostname(), &payload);
        let result = self
            .send(api_request, move |response| {
                let data = decode_data(response)?;
                let mutation = data
                    .get("mutation")
                    .and_then(Value::as_object)
                    .ok_or_else(|| invalid("comment mutation omitted receipt"))?;
                if mutation.get("clientMutationId").and_then(Value::as_str) != Some(&receipt) {
                    return Err(invalid(
                        "comment mutation receipt does not match its operation",
                    ));
                }
                if returned.is_none() {
                    return Ok(None);
                }
                let state: CommentState = serde_json::from_value(
                    mutation
                        .get("comment")
                        .cloned()
                        .ok_or_else(|| invalid("comment mutation omitted body"))?,
                )
                .map_err(invalid)?;
                state.validate(&target).map_err(invalid)?;
                Ok(Some(state))
            })
            .await;
        mutation_result(result)
    }
}

fn decode_data(response: ApiResponse) -> Result<Value, RemoteFailure> {
    let response: CommentResponse = response.into_json()?;
    response
        .data
        .filter(Value::is_object)
        .ok_or_else(|| invalid("comment response omitted data"))
}

fn invalid(failure: impl std::fmt::Display) -> RemoteFailure {
    RemoteFailure {
        kind: RemoteFailureKind::InvalidResponse,
        message: diagnostic(&failure.to_string()),
    }
}

fn mutation_result(
    result: anyhow::Result<Result<Option<CommentState>, RemoteFailure>>,
) -> CommentMutationOutcome {
    let rejected = result.as_ref().err().is_some_and(request_not_sent);
    match remote_result(result) {
        Ok(state) => CommentMutationOutcome::Confirmed(state),
        Err(failure) if rejected => CommentMutationOutcome::Rejected(failure),
        Err(failure) => CommentMutationOutcome::Uncertain(failure),
    }
}
