use crate::service::not_started;
use std::{future::Future, pin::Pin, sync::Arc};

use anyhow::{Result, ensure};
use serde::{Deserialize, Serialize};

use crate::pull_request::PullRequestTarget;
use crate::queue::{RemoteCompletion, RemoteIntent, RemoteResource, RemoteScope};
use crate::remote::GithubRemote;
use crate::remote::RemoteFailure;
use crate::service::GithubService;

#[derive(Clone, Copy, Debug, Deserialize, Serialize, Eq, PartialEq)]
pub enum CommentKind {
    IssueComment,
    PullRequestReviewComment,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct CommentTarget {
    pub pull_request: PullRequestTarget,
    pub kind: CommentKind,
    pub node_id: String,
    pub database_id: u64,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct CommentState {
    #[serde(rename = "__typename")]
    pub kind: CommentKind,
    pub id: String,
    #[serde(rename = "databaseId", deserialize_with = "database_identity")]
    pub database_id: u64,
    pub body: String,
    pub url: String,
    #[serde(rename = "viewerDidAuthor")]
    pub viewer_did_author: bool,
}

#[derive(Clone, Debug, Deserialize)]
#[serde(tag = "operation", rename_all = "snake_case", deny_unknown_fields)]
pub enum CommentOperation {
    Edit { body: String, receipt: String },
    Delete { receipt: String },
    Reconcile,
}

#[derive(Clone, Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct ConversationCommentCreation {
    pub target: PullRequestTarget,
    pub body: String,
    pub receipt: String,
}

#[derive(Debug)]
pub enum CommentMutationOutcome {
    Confirmed(Option<CommentState>),
    Rejected(RemoteFailure),
    Uncertain(RemoteFailure),
}

#[derive(Debug, Serialize)]
#[serde(tag = "outcome", rename_all = "snake_case")]
pub enum CommentResult {
    Confirmed { state: Option<CommentState> },
    Reconciled { state: Option<CommentState> },
    Rejected { message: String },
    OutcomeUnknown { message: String },
}

/// Native comment capability shares the GitHub client's existing process admission and reaping.
pub trait GithubCommentRemote: Send + Sync {
    fn create_conversation_comment(
        &self,
        request: ConversationCommentCreation,
    ) -> Pin<Box<dyn Future<Output = CommentMutationOutcome> + Send + '_>>;
    fn read_comment(
        &self,
        target: CommentTarget,
    ) -> Pin<Box<dyn Future<Output = Result<Option<CommentState>, RemoteFailure>> + Send + '_>>;
    fn mutate_comment(
        &self,
        target: CommentTarget,
        operation: CommentOperation,
    ) -> Pin<Box<dyn Future<Output = CommentMutationOutcome> + Send + '_>>;
}

impl ConversationCommentCreation {
    pub fn validate(&self) -> Result<()> {
        self.target.validate()?;
        validate_body(&self.body)?;
        validate_identity(&self.receipt)
    }

    pub fn validate_state(&self, state: &CommentState) -> Result<CommentTarget> {
        let target = CommentTarget {
            pull_request: self.target.clone(),
            kind: CommentKind::IssueComment,
            node_id: state.id.clone(),
            database_id: state.database_id,
        };
        target.validate()?;
        state.validate(&target)?;
        ensure!(
            state.viewer_did_author && state.body == self.body,
            "created comment differs from submitted content or viewer ownership"
        );
        Ok(target)
    }
}

impl CommentTarget {
    pub fn validate(&self) -> Result<()> {
        self.pull_request.validate()?;
        validate_identity(&self.node_id)?;
        ensure!(
            self.database_id > 0 && self.database_id <= 9_007_199_254_740_991,
            "invalid comment database identity"
        );
        Ok(())
    }
}

impl CommentState {
    pub fn validate(&self, target: &CommentTarget) -> Result<()> {
        ensure!(
            self.kind == target.kind
                && self.id == target.node_id
                && self.database_id == target.database_id,
            "comment observation belongs to another target"
        );
        let prefix = format!(
            "https://{}/{}/pull/{}#",
            target.pull_request.repository.hostname(),
            target.pull_request.repository.repository_name(),
            target.pull_request.number
        );
        ensure!(
            self.url.len() <= 4096
                && !self.url.chars().any(char::is_control)
                && self
                    .url
                    .to_ascii_lowercase()
                    .starts_with(&prefix.to_ascii_lowercase()),
            "comment observation belongs to another PR"
        );
        ensure!(
            self.body.len() <= 256 * 1024 && !self.body.contains('\0'),
            "comment body exceeds its text contract"
        );
        Ok(())
    }
}

impl CommentOperation {
    pub fn validate(&self) -> Result<()> {
        match self {
            Self::Edit { body, receipt } => {
                validate_body(body)?;
                validate_identity(receipt)?;
            }
            Self::Delete { receipt } => validate_identity(receipt)?,
            Self::Reconcile => {}
        }
        Ok(())
    }

    pub(crate) fn retained_bytes(&self) -> usize {
        match self {
            Self::Edit { body, receipt } => body.capacity() + receipt.capacity(),
            Self::Delete { receipt } => receipt.capacity(),
            Self::Reconcile => 0,
        }
    }
}

impl GithubService {
    /// Creates one conversation comment. Unknown creation remains scoped to its receipt without retry.
    pub async fn create_conversation_comment<
        Remote: GithubRemote + GithubCommentRemote + ?Sized + 'static,
    >(
        &self,
        remote: Arc<Remote>,
        request: ConversationCommentCreation,
    ) -> Result<CommentResult> {
        request.validate().map_err(not_started)?;
        let resource =
            RemoteResource::new(request.target.repository.clone(), request.target.number)
                .map_err(not_started)?;
        let scope = RemoteScope::Creation(request.receipt.clone().into_boxed_str());
        let retained_bytes = request.body.capacity()
            + request.receipt.capacity()
            + request.target.node_id.capacity()
            + 1024;
        self.run_mutation(
            resource,
            scope,
            RemoteIntent::Mutation,
            retained_bytes,
            move |mut operation| async move {
                let preflight = match remote
                    .read_pull_request(request.target.clone(), false)
                    .await
                {
                    Ok(parent) => parent.validate(&request.target),
                    Err(failure) => Err(failure.into()),
                };
                if let Err(failure) = preflight {
                    operation.complete(RemoteCompletion::Unchanged)?;
                    return Ok(CommentResult::Rejected {
                        message: failure.to_string(),
                    });
                }
                operation.begin_mutation()?;
                match remote.create_conversation_comment(request.clone()).await {
                    CommentMutationOutcome::Confirmed(Some(state))
                        if request.validate_state(&state).is_ok() =>
                    {
                        operation.complete(RemoteCompletion::Confirmed)?;
                        Ok(CommentResult::Confirmed { state: Some(state) })
                    }
                    CommentMutationOutcome::Rejected(failure) => {
                        operation.complete(RemoteCompletion::Confirmed)?;
                        Ok(CommentResult::Rejected {
                            message: failure.to_string(),
                        })
                    }
                    CommentMutationOutcome::Uncertain(failure) => {
                        operation.complete(RemoteCompletion::Uncertain)?;
                        Ok(CommentResult::OutcomeUnknown {
                            message: failure.to_string(),
                        })
                    }
                    _ => {
                        operation.complete(RemoteCompletion::Uncertain)?;
                        Ok(CommentResult::OutcomeUnknown {
                            message: "creation confirmation does not match its submission".into(),
                        })
                    }
                }
            },
        )
        .await
    }
    /// Validates the existing remote owner before one write attempt. Reconciliation never writes.
    pub async fn comment(
        &self,
        remote: Arc<dyn GithubCommentRemote>,
        target: CommentTarget,
        request: CommentOperation,
    ) -> Result<CommentResult> {
        target.validate().map_err(not_started)?;
        request.validate().map_err(not_started)?;
        let resource = RemoteResource::new(
            target.pull_request.repository.clone(),
            target.pull_request.number,
        )
        .map_err(not_started)?;
        let scope = RemoteScope::Comment(target.node_id.clone().into_boxed_str());
        let intent = if matches!(request, CommentOperation::Reconcile) {
            RemoteIntent::Reconcile
        } else {
            RemoteIntent::Mutation
        };
        let retained_bytes = request.retained_bytes()
            + target.node_id.capacity()
            + target.pull_request.node_id.capacity()
            + 1024;
        self.run_mutation(
            resource,
            scope,
            intent,
            retained_bytes,
            move |mut operation| async move {
                let observed = remote.read_comment(target.clone()).await?;
                if let Some(state) = &observed {
                    state.validate(&target)?;
                }
                if matches!(request, CommentOperation::Reconcile) {
                    operation.complete(RemoteCompletion::Reconciled)?;
                    return Ok(CommentResult::Reconciled { state: observed });
                }
                let Some(state) = observed else {
                    operation.complete(RemoteCompletion::Unchanged)?;
                    return Ok(CommentResult::Rejected {
                        message: "comment no longer exists".into(),
                    });
                };
                if !state.viewer_did_author {
                    operation.complete(RemoteCompletion::Unchanged)?;
                    return Ok(CommentResult::Rejected {
                        message: "comment is not viewer authored".into(),
                    });
                }
                if matches!(&request, CommentOperation::Edit { body, .. } if *body == state.body) {
                    operation.complete(RemoteCompletion::Confirmed)?;
                    return Ok(CommentResult::Confirmed { state: Some(state) });
                }
                operation.begin_mutation()?;
                match remote.mutate_comment(target.clone(), request.clone()).await {
                    CommentMutationOutcome::Confirmed(state) => {
                        let valid = match (&request, &state) {
                            (CommentOperation::Edit { body, .. }, Some(state)) => {
                                state.validate(&target).is_ok()
                                    && state.viewer_did_author
                                    && state.body == *body
                            }
                            (CommentOperation::Delete { .. }, None) => true,
                            _ => false,
                        };
                        if !valid {
                            operation.complete(RemoteCompletion::Uncertain)?;
                            return Ok(CommentResult::OutcomeUnknown {
                                message: "comment confirmation does not match its submission"
                                    .into(),
                            });
                        }
                        operation.complete(RemoteCompletion::Confirmed)?;
                        Ok(CommentResult::Confirmed { state })
                    }
                    CommentMutationOutcome::Rejected(failure) => {
                        operation.complete(RemoteCompletion::Confirmed)?;
                        Ok(CommentResult::Rejected {
                            message: failure.to_string(),
                        })
                    }
                    CommentMutationOutcome::Uncertain(failure) => {
                        operation.complete(RemoteCompletion::Uncertain)?;
                        Ok(CommentResult::OutcomeUnknown {
                            message: failure.to_string(),
                        })
                    }
                }
            },
        )
        .await
    }
}

fn validate_identity(identity: &str) -> Result<()> {
    ensure!(
        !identity.is_empty()
            && identity.len() <= 256
            && identity.bytes().all(|byte| byte.is_ascii_graphic()),
        "invalid comment identity or mutation receipt"
    );
    Ok(())
}

fn validate_body(body: &str) -> Result<()> {
    ensure!(
        !body.trim().is_empty() && body.len() <= 256 * 1024 && !body.contains('\0'),
        "invalid submitted comment body"
    );
    Ok(())
}

fn database_identity<'input, Input: serde::Deserializer<'input>>(
    input: Input,
) -> Result<u64, Input::Error> {
    #[derive(Deserialize)]
    #[serde(untagged)]
    enum DatabaseIdentity {
        Number(u64),
        Text(String),
    }
    match DatabaseIdentity::deserialize(input)? {
        DatabaseIdentity::Number(number) => Ok(number),
        DatabaseIdentity::Text(text)
            if !text.is_empty()
                && text.len() <= 16
                && text.bytes().all(|byte| byte.is_ascii_digit()) =>
        {
            text.parse().map_err(serde::de::Error::custom)
        }
        DatabaseIdentity::Text(_) => Err(serde::de::Error::custom(
            "invalid comment database identity",
        )),
    }
}
