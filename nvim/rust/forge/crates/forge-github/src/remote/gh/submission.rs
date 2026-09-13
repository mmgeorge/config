use std::path::PathBuf;

use serde::{Deserialize, Serialize};
use serde_json::Value;

use super::{GhClient, GhDirectory, remote_result};
use crate::{
    model::GithubRepositoryId,
    remote::{GithubRemote, RemoteFailure, RemoteFailureKind},
    review_mutation::PendingReviewMember,
    review_source::valid_object_id,
};

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct PendingReviewExpected {
    pub id: u64,
    pub body: String,
}

#[derive(Clone, Debug, Serialize)]
pub struct ReviewSubmissionContext {
    pub actor_node_id: String,
    pub commit_id: String,
    pub pending_review_id: Option<u64>,
    pub pending_comment: Vec<PendingReviewMember>,
}

impl GhDirectory {
    pub async fn submission_context(
        &self,
        repository: GithubRepositoryId,
        number: u64,
        pending_review_id: Option<u64>,
        mut expected: Vec<PendingReviewExpected>,
    ) -> Result<ReviewSubmissionContext, RemoteFailure> {
        if number == 0
            || number > i32::MAX as u64
            || pending_review_id == Some(0)
            || expected.len() > 10_000
            || expected
                .iter()
                .map(|member| member.body.len())
                .sum::<usize>()
                > 4 * 1024 * 1024
        {
            return Err(invalid("invalid review submission capture"));
        }
        expected.sort_by_key(|member| member.id);
        if expected.iter().any(|member| member.id == 0)
            || expected.windows(2).any(|pair| pair[0].id == pair[1].id)
        {
            return Err(invalid("review submission repeats a comment identity"));
        }
        let actor = self.read_actor(repository.clone()).await?;
        let prefix = format!("repos/{}/pulls/{number}", repository.repository_name());
        let parent = self
            .client
            .submission_json_at(self.directory.clone(), &repository, prefix.clone())
            .await?;
        if parent.get("number").and_then(Value::as_u64) != Some(number) {
            return Err(invalid("review submission parent identity changed"));
        }
        let commit_id = parent
            .pointer("/head/sha")
            .and_then(Value::as_str)
            .filter(|identity| valid_object_id(identity))
            .ok_or_else(|| invalid("PR head identity is unavailable"))?
            .to_owned();
        drop(parent);
        let pending_comment = if let Some(review) = pending_review_id {
            let observed = self
                .client
                .submission_json_at(
                    self.directory.clone(),
                    &repository,
                    format!("{prefix}/reviews/{review}"),
                )
                .await?;
            if observed.get("id").and_then(Value::as_u64) != Some(review)
                || observed.get("state").and_then(Value::as_str) != Some("PENDING")
                || observed.pointer("/user/node_id").and_then(Value::as_str)
                    != Some(actor.node_id.as_str())
            {
                return Err(invalid("pending review identity, state, or actor changed"));
            }
            self.client
                .pending_members_at(self.directory.clone(), &repository, number, review)
                .await?
        } else {
            Vec::new()
        };
        if pending_comment.len() != expected.len()
            || pending_comment
                .iter()
                .zip(&expected)
                .any(|(observed, expected)| {
                    observed.id != expected.id || observed.body != expected.body
                })
        {
            return Err(invalid(
                "pending review membership or text changed after verdict capture",
            ));
        }
        Ok(ReviewSubmissionContext {
            actor_node_id: actor.node_id,
            commit_id,
            pending_review_id,
            pending_comment,
        })
    }
}

impl GhClient {
    async fn submission_json_at(
        &self,
        directory: PathBuf,
        repository: &GithubRepositoryId,
        endpoint: String,
    ) -> Result<Value, RemoteFailure> {
        let argument = vec![
            "api".into(),
            "--hostname".into(),
            repository.hostname().into(),
            endpoint.into(),
        ];
        remote_result(
            self.run_native(
                directory,
                argument,
                None,
                8 * 1024 * 1024,
                super::source::response,
            )
            .await,
        )
    }

    pub(super) async fn pending_members_at(
        &self,
        directory: PathBuf,
        repository: &GithubRepositoryId,
        number: u64,
        review: u64,
    ) -> Result<Vec<PendingReviewMember>, RemoteFailure> {
        let mut result = Vec::new();
        let mut bytes = 0;
        for page in 1..=101 {
            let argument = vec![
                "api".into(),
                "--hostname".into(),
                repository.hostname().into(),
                format!(
                    "repos/{}/pulls/{number}/reviews/{review}/comments?per_page=100&page={page}",
                    repository.repository_name()
                )
                .into(),
            ];
            let members = remote_result(
                self.run_native(
                    directory.clone(),
                    argument,
                    None,
                    8 * 1024 * 1024,
                    |output| {
                        let value = super::source::response(output)?;
                        let record = value.as_array().ok_or_else(|| {
                            invalid("pending review comments are not a collection")
                        })?;
                        if record.len() > 100 {
                            return Err(invalid("pending review page exceeds 100 records"));
                        }
                        record
                            .iter()
                            .map(decode_member)
                            .collect::<Result<Vec<_>, _>>()
                    },
                )
                .await,
            )?;
            let complete = members.len() < 100;
            bytes += members
                .iter()
                .map(|member| member.body.len() + member.path.len() + member.node_id.len() + 256)
                .sum::<usize>();
            if bytes > 8 * 1024 * 1024 || result.len() + members.len() > 10_000 {
                return Err(invalid("pending review membership exceeds capture limits"));
            }
            result.extend(members);
            if complete {
                result.sort_by_key(|member| member.id);
                if result.windows(2).any(|pair| pair[0].id == pair[1].id) {
                    return Err(invalid("pending review membership repeats an identity"));
                }
                return Ok(result);
            }
        }
        Err(invalid("pending review membership is incomplete"))
    }
}

fn decode_member(record: &Value) -> Result<PendingReviewMember, RemoteFailure> {
    let text = |field| {
        record
            .get(field)
            .and_then(Value::as_str)
            .map(str::to_owned)
            .ok_or_else(|| invalid(format!("pending review comment omitted {field}")))
    };
    let coordinate = |field| match record.get(field) {
        None | Some(Value::Null) => Ok(None),
        Some(value) => value
            .as_u64()
            .filter(|value| *value > 0 && *value <= u32::MAX as u64)
            .map(|value| Some(value as u32))
            .ok_or_else(|| invalid("invalid pending review coordinate")),
    };
    let id = record
        .get("id")
        .and_then(Value::as_u64)
        .filter(|id| *id > 0)
        .ok_or_else(|| invalid("pending review comment omitted identity"))?;
    let commit_id = text("commit_id")?;
    if !valid_object_id(&commit_id) {
        return Err(invalid("pending review comment omitted commit identity"));
    }
    Ok(PendingReviewMember {
        id,
        node_id: text("node_id")?,
        body: text("body")?,
        path: text("path")?,
        commit_id,
        position: coordinate("position")?,
        line: coordinate("line")?,
        side: record
            .get("side")
            .and_then(Value::as_str)
            .map(str::to_owned),
    })
}

fn invalid(message: impl Into<String>) -> RemoteFailure {
    RemoteFailure {
        kind: RemoteFailureKind::InvalidResponse,
        message: message.into(),
    }
}
