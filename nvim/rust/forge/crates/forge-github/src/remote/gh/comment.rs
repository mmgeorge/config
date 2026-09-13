use std::{future::Future, path::PathBuf, pin::Pin, process::Output};

use serde::Deserialize;
use serde_json::{Value, json};

use super::{
    BoundedVec, GhClient, GhDirectory, GraphFailure, append_graph_failures, classify_failure,
    diagnostic, remote_result,
};
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
    #[serde(default)]
    errors: BoundedVec<GraphFailure, 32>,
}

impl GithubCommentRemote for GhClient {
    fn create_conversation_comment(
        &self,
        request: ConversationCommentCreation,
    ) -> Pin<Box<dyn Future<Output = CommentMutationOutcome> + Send + '_>> {
        Box::pin(self.create_conversation_at(self.directory.clone(), request))
    }

    fn read_comment(
        &self,
        target: CommentTarget,
    ) -> Pin<
        Box<dyn Future<Output = anyhow::Result<Option<CommentState>, RemoteFailure>> + Send + '_>,
    > {
        Box::pin(self.read_comment_at(self.directory.clone(), target))
    }

    fn mutate_comment(
        &self,
        target: CommentTarget,
        operation: CommentOperation,
    ) -> Pin<Box<dyn Future<Output = CommentMutationOutcome> + Send + '_>> {
        Box::pin(self.mutate_comment_at(self.directory.clone(), target, operation))
    }
}

impl GithubCommentRemote for GhDirectory {
    fn create_conversation_comment(
        &self,
        request: ConversationCommentCreation,
    ) -> Pin<Box<dyn Future<Output = CommentMutationOutcome> + Send + '_>> {
        Box::pin(
            self.client
                .create_conversation_at(self.directory.clone(), request),
        )
    }

    fn read_comment(
        &self,
        target: CommentTarget,
    ) -> Pin<
        Box<dyn Future<Output = anyhow::Result<Option<CommentState>, RemoteFailure>> + Send + '_>,
    > {
        Box::pin(self.client.read_comment_at(self.directory.clone(), target))
    }

    fn mutate_comment(
        &self,
        target: CommentTarget,
        operation: CommentOperation,
    ) -> Pin<Box<dyn Future<Output = CommentMutationOutcome> + Send + '_>> {
        Box::pin(
            self.client
                .mutate_comment_at(self.directory.clone(), target, operation),
        )
    }
}

impl GhClient {
    async fn create_conversation_at(
        &self,
        directory: PathBuf,
        request: ConversationCommentCreation,
    ) -> CommentMutationOutcome {
        if let Err(failure) = request.validate() {
            return CommentMutationOutcome::Rejected(invalid(failure));
        }
        let query = format!(
            "mutation($input:AddCommentInput!) {{ mutation: addComment(input:$input) {{ clientMutationId subject {{ id }} commentEdge {{ node {{ {FIELDS} }} }} }} }}"
        );
        let encoded = serde_json::to_vec(&json!({"query":query,"variables":{"input":{"subjectId":request.target.node_id,"body":request.body,"clientMutationId":request.receipt}}})).expect("serialize comment creation");
        let argument = vec![
            "api".into(),
            "--hostname".into(),
            request.target.repository.hostname().into(),
            "graphql".into(),
        ];
        let retained_bytes = request.target.node_id.capacity()
            + request.body.capacity()
            + request.receipt.capacity()
            + 1024;
        let result = self
            .run_native(
                directory,
                argument,
                Some(encoded),
                retained_bytes,
                move |output| {
                    let data = decode_data(output)?;
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
                },
            )
            .await;
        mutation_result(result)
    }

    async fn read_comment_at(
        &self,
        directory: PathBuf,
        target: CommentTarget,
    ) -> Result<Option<CommentState>, RemoteFailure> {
        target.validate().map_err(invalid)?;
        let query = format!(
            "query($id:ID!) {{ node(id:$id) {{ ... on IssueComment {{ {FIELDS} }} ... on PullRequestReviewComment {{ {FIELDS} }} }} }}"
        );
        let encoded = serde_json::to_vec(&json!({"query":query,"variables":{"id":target.node_id}}))
            .expect("serialize comment read");
        let argument = vec![
            "api".into(),
            "--hostname".into(),
            target.pull_request.repository.hostname().into(),
            "graphql".into(),
        ];
        remote_result(
            self.run_native(
                directory,
                argument,
                Some(encoded),
                target.node_id.capacity() + 1024,
                move |output| {
                    let data = decode_data(output)?;
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
        directory: PathBuf,
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
        let encoded = serde_json::to_vec(&json!({"query":query,"variables":{"input":variables}}))
            .expect("serialize comment mutation");
        drop(variables);
        let argument = vec![
            "api".into(),
            "--hostname".into(),
            target.pull_request.repository.hostname().into(),
            "graphql".into(),
        ];
        let receipt = receipt.clone();
        let result = self
            .run_native(
                directory,
                argument,
                Some(encoded),
                target.node_id.capacity() + receipt.capacity() + 1024,
                move |output| {
                    let data = decode_data(output)?;
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
                },
            )
            .await;
        mutation_result(result)
    }
}

fn decode_data(output: Output) -> Result<Value, RemoteFailure> {
    let response = serde_json::from_slice::<CommentResponse>(&output.stdout);
    if !output.status.success() {
        let mut message = diagnostic(&format!(
            "gh exited {}: {}",
            output.status,
            String::from_utf8_lossy(&output.stderr)
        ));
        if let Ok(response) = response {
            append_graph_failures(&mut message, &response.errors.0);
        }
        return Err(classify_failure(message));
    }
    let response = response.map_err(invalid)?;
    if !response.errors.0.is_empty() {
        let mut message = String::new();
        append_graph_failures(&mut message, &response.errors.0);
        return Err(classify_failure(message));
    }
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
    let rejected = result.as_ref().err().is_some_and(|failure| {
        failure
            .downcast_ref::<forge_git::read_pool::ReadAdmissionError>()
            .is_some()
    });
    match remote_result(result) {
        Ok(state) => CommentMutationOutcome::Confirmed(state),
        Err(failure) if rejected => CommentMutationOutcome::Rejected(failure),
        Err(failure) => CommentMutationOutcome::Uncertain(failure),
    }
}
