use std::sync::Arc;

use anyhow::{Result, ensure};
use serde::{Deserialize, Serialize};

use crate::model::GithubRepositoryId;
use crate::review_api::GithubReviewRemote;
use crate::service::GithubService;

pub const MAX_SOURCE_BYTES: usize = 8 * 1024 * 1024;

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct ReviewCommitDetailRequest {
    pub repository: GithubRepositoryId,
    pub commit: String,
}

#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub struct ReviewCommitDetail {
    pub commit: String,
    pub parent: Option<String>,
    pub subject: String,
    pub file: Vec<ReviewCommitFile>,
}

#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub struct ReviewCommitFile {
    pub path: String,
    pub previous_path: Option<String>,
    pub status: String,
    pub patch: Option<String>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct ReviewSourceRequest {
    pub repository: GithubRepositoryId,
    pub commit: String,
    pub path: String,
}

#[derive(Debug)]
pub struct ReviewSource {
    pub request: ReviewSourceRequest,
    pub blob: Option<String>,
    pub bytes: Vec<u8>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct ReviewComparisonRequest {
    pub repository: GithubRepositoryId,
    pub base: String,
    pub head: String,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ReviewComparison {
    pub base: String,
    pub head: String,
    pub merge_base: String,
}

impl ReviewSourceRequest {
    pub fn validate(&self) -> Result<()> {
        ensure!(
            valid_object_id(&self.commit),
            "invalid immutable source commit"
        );
        ensure!(
            !self.path.is_empty()
                && self.path.len() <= 4096
                && !self.path.contains('\0')
                && !self.path.starts_with('/')
                && self
                    .path
                    .split('/')
                    .all(|part| !matches!(part, "" | "." | "..")),
            "invalid repository source path"
        );
        Ok(())
    }
}

impl ReviewCommitDetailRequest {
    pub fn validate(&self) -> Result<()> {
        ensure!(
            valid_object_id(&self.commit),
            "invalid immutable commit identity"
        );
        Ok(())
    }
}

impl ReviewComparisonRequest {
    pub fn validate(&self) -> Result<()> {
        ensure!(
            valid_object_id(&self.base) && valid_object_id(&self.head),
            "comparison requires immutable commit identities"
        );
        Ok(())
    }
}

pub(crate) fn valid_object_id(value: &str) -> bool {
    matches!(value.len(), 40 | 64) && value.bytes().all(|byte| byte.is_ascii_hexdigit())
}

impl GithubService {
    pub async fn review_commit_detail(
        &self,
        directory: std::path::PathBuf,
        remote: Arc<dyn GithubReviewRemote>,
        request: ReviewCommitDetailRequest,
    ) -> Result<ReviewCommitDetail> {
        request.validate()?;
        self.run_remote(directory, "review commit detail", move |_| async move {
            remote
                .read_commit_detail(request)
                .await
                .map_err(anyhow::Error::from)
        })
        .await
    }

    pub async fn review_source(
        &self,
        directory: std::path::PathBuf,
        remote: Arc<dyn GithubReviewRemote>,
        request: ReviewSourceRequest,
    ) -> Result<ReviewSource> {
        request.validate()?;
        self.run_remote(directory, "review source", move |_| async move {
            remote
                .read_source(request)
                .await
                .map_err(anyhow::Error::from)
        })
        .await
    }

    pub async fn review_comparison(
        &self,
        directory: std::path::PathBuf,
        remote: Arc<dyn GithubReviewRemote>,
        request: ReviewComparisonRequest,
    ) -> Result<ReviewComparison> {
        request.validate()?;
        self.run_remote(directory, "review comparison", move |_| async move {
            remote
                .read_comparison(request)
                .await
                .map_err(anyhow::Error::from)
        })
        .await
    }
}
