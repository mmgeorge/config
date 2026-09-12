use std::path::PathBuf;

use anyhow::Result;
use forge_github::model::GithubRepositoryId;
use forge_github::recovery::RecoveryResource;
use forge_github::remote::{GithubRemote, PendingReviewExpected};
use forge_github::review_api::ReviewReadRequest;
use forge_github::review_mutation::{RecoveryResolution, ReviewMutationRequest};
use serde::Deserialize;
use serde_json::{Value, json};

use crate::runtime::ForgeRuntime;

pub(crate) enum GithubRoute {
    Actor,
    CreationContext,
    ReviewSubmissionContext,
    NotificationPage,
    ReviewRead,
    ReviewMutate,
    RecoveryInspect,
    RecoveryResolve,
    RecoveryAcknowledge,
    RecoverySettleDraft,
    ReviewDraft,
    ReviewDraftWrite,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct CreationContextParams {
    directory: PathBuf,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct ReviewSubmissionContextParams {
    directory: PathBuf,
    repository: GithubRepositoryId,
    number: u64,
    pending_review_id: Option<u64>,
    expected: Vec<PendingReviewExpected>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct NotificationPageParams {
    directory: PathBuf,
    request: forge_github::notification::NotificationReadRequest,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct ActorParams {
    directory: PathBuf,
    repository: GithubRepositoryId,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct ReviewReadParams {
    directory: PathBuf,
    request: ReviewReadRequest,
    #[serde(default)]
    collection: bool,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct ReviewMutateParams {
    directory: PathBuf,
    request: ReviewMutationRequest,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct RecoveryInspectParams {
    resource: RecoveryResource,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct RecoveryResolveParams {
    directory: PathBuf,
    resource: RecoveryResource,
    operation_id: String,
    resolution: RecoveryResolution,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct RecoveryAcknowledgeParams {
    resource: RecoveryResource,
    operation_id: String,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct RecoverySettleDraftParams {
    resource: RecoveryResource,
    operation_id: String,
    draft: Value,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct ReviewDraftWriteParams {
    resource: RecoveryResource,
    draft: Value,
}

pub(super) async fn route(
    host: &ForgeRuntime,
    operation: GithubRoute,
    params: Value,
) -> Result<Value> {
    Ok(match operation {
        GithubRoute::CreationContext => {
            let params: CreationContextParams = serde_json::from_value(params)?;
            let remote = host.github_remote.for_directory(params.directory)?;
            serde_json::to_value(remote.creation_context().await?)?
        }
        GithubRoute::NotificationPage => {
            let params: NotificationPageParams = serde_json::from_value(params)?;
            let remote = host.github_remote.for_directory(params.directory.clone())?;
            serde_json::to_value(
                host.github
                    .notification_page(params.directory, remote, params.request)
                    .await?,
            )?
        }
        GithubRoute::ReviewSubmissionContext => {
            let params: ReviewSubmissionContextParams = serde_json::from_value(params)?;
            let remote = host.github_remote.for_directory(params.directory)?;
            serde_json::to_value(
                remote
                    .submission_context(
                        params.repository,
                        params.number,
                        params.pending_review_id,
                        params.expected,
                    )
                    .await?,
            )?
        }
        GithubRoute::Actor => {
            let params: ActorParams = serde_json::from_value(params)?;
            let remote = host.github_remote.for_directory(params.directory)?;
            serde_json::to_value(remote.read_actor(params.repository).await?)?
        }
        GithubRoute::ReviewRead => {
            let params: ReviewReadParams = serde_json::from_value(params)?;
            let remote = host.github_remote.for_directory(params.directory.clone())?;
            if params.collection {
                serde_json::to_value(
                    host.github
                        .review_collection(params.directory, remote, params.request)
                        .await?,
                )?
            } else {
                serde_json::to_value(
                    host.github
                        .review_page(params.directory, remote, params.request)
                        .await?,
                )?
            }
        }
        GithubRoute::ReviewMutate => {
            let params: ReviewMutateParams = serde_json::from_value(params)?;
            let remote = host.github_remote.for_directory(params.directory)?;
            serde_json::to_value(host.github.review_mutation(remote, params.request).await?)?
        }
        GithubRoute::RecoveryInspect => {
            let params: RecoveryInspectParams = serde_json::from_value(params)?;
            serde_json::to_value(host.github.recovery_inspect(params.resource).await?)?
        }
        GithubRoute::RecoveryResolve => {
            let params: RecoveryResolveParams = serde_json::from_value(params)?;
            let remote = host.github_remote.for_directory(params.directory)?;
            serde_json::to_value(
                host.github
                    .recovery_resolve(
                        remote,
                        params.resource,
                        params.operation_id,
                        params.resolution,
                    )
                    .await?,
            )?
        }
        GithubRoute::RecoveryAcknowledge => {
            let params: RecoveryAcknowledgeParams = serde_json::from_value(params)?;
            host.github
                .recovery_acknowledge(params.resource, params.operation_id)
                .await?;
            json!({"acknowledged":true})
        }
        GithubRoute::RecoverySettleDraft => {
            let params: RecoverySettleDraftParams = serde_json::from_value(params)?;
            host.github
                .recovery_settle_draft(params.resource, params.operation_id, params.draft)
                .await?;
            json!({"acknowledged":true})
        }
        GithubRoute::ReviewDraft => {
            let params: RecoveryInspectParams = serde_json::from_value(params)?;
            serde_json::to_value(host.github.review_draft(params.resource).await?)?
        }
        GithubRoute::ReviewDraftWrite => {
            let params: ReviewDraftWriteParams = serde_json::from_value(params)?;
            host.github
                .review_draft_write(params.resource, params.draft)
                .await?;
            json!({"saved":true})
        }
    })
}
