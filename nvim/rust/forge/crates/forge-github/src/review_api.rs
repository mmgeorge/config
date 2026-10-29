use std::future::Future;
use std::pin::Pin;
use std::sync::Arc;

use anyhow::{Result, ensure};
use serde::{Deserialize, Serialize};
use serde_json::Value;

use crate::model::GithubRepositoryId;
use crate::remote::{GithubRemote, RemoteFailure};
use crate::service::GithubService;

pub const REVIEW_PAGE_SIZE: usize = 100;

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "section", rename_all = "snake_case", deny_unknown_fields)]
pub enum ReviewSection {
    Overview,
    /// Initial editable PR fields queried by repository and number.
    EditableFields,
    Commits,
    Files,
    Reviews,
    ReviewComments,
    Conversation,
    Threads,
    ThreadComments {
        thread_node_id: String,
    },
    Checks,
    RequestedReviewers,
    Milestones,
    Notifications,
    Issue,
    Diff,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct ReviewReadRequest {
    pub repository: GithubRepositoryId,
    pub number: u64,
    pub view: ReviewSection,
    pub cursor: Option<String>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct ReviewPage {
    pub records: Vec<Value>,
    pub next_cursor: Option<String>,
    pub complete: bool,
}

pub trait GithubReviewRemote: GithubRemote {
    fn read_commit_detail(
        &self,
        _request: crate::review_source::ReviewCommitDetailRequest,
    ) -> Pin<
        Box<
            dyn Future<
                    Output = std::result::Result<
                        crate::review_source::ReviewCommitDetail,
                        RemoteFailure,
                    >,
                > + Send
                + '_,
        >,
    > {
        Box::pin(async {
            Err(RemoteFailure {
                kind: crate::remote::RemoteFailureKind::InvalidResponse,
                message: "commit detail reads are unavailable".into(),
            })
        })
    }

    fn read_source(
        &self,
        _request: crate::review_source::ReviewSourceRequest,
    ) -> Pin<
        Box<
            dyn Future<
                    Output = std::result::Result<crate::review_source::ReviewSource, RemoteFailure>,
                > + Send
                + '_,
        >,
    > {
        Box::pin(async {
            Err(RemoteFailure {
                kind: crate::remote::RemoteFailureKind::InvalidResponse,
                message: "immutable source reads are unavailable".into(),
            })
        })
    }

    fn read_comparison(
        &self,
        _request: crate::review_source::ReviewComparisonRequest,
    ) -> Pin<
        Box<
            dyn Future<
                    Output = std::result::Result<
                        crate::review_source::ReviewComparison,
                        RemoteFailure,
                    >,
                > + Send
                + '_,
        >,
    > {
        Box::pin(async {
            Err(RemoteFailure {
                kind: crate::remote::RemoteFailureKind::InvalidResponse,
                message: "immutable comparison reads are unavailable".into(),
            })
        })
    }

    fn read_review(
        &self,
        request: ReviewReadRequest,
    ) -> Pin<Box<dyn Future<Output = std::result::Result<ReviewPage, RemoteFailure>> + Send + '_>>;
}

impl ReviewReadRequest {
    pub fn validate(&self) -> Result<()> {
        ensure!(
            self.number > 0 && self.number <= i32::MAX as u64,
            "invalid GitHub document number"
        );
        ensure!(
            self.cursor
                .as_ref()
                .is_none_or(|cursor| !cursor.is_empty() && !cursor.contains(['\n', '\r', '\0'])),
            "invalid GitHub document cursor"
        );
        if let ReviewSection::ThreadComments { thread_node_id } = &self.view {
            ensure!(
                !thread_node_id.is_empty()
                    && thread_node_id.bytes().all(|byte| byte.is_ascii_graphic()),
                "invalid GitHub thread identity"
            );
        }
        Ok(())
    }
}

impl GithubService {
    /// Reads one bounded native page. Failed reads never become empty successful documents.
    pub async fn review_page(
        &self,
        directory: std::path::PathBuf,
        remote: Arc<dyn GithubReviewRemote>,
        request: ReviewReadRequest,
    ) -> Result<ReviewPage> {
        request.validate()?;
        self.run_remote(directory, "review read", move |_| async move {
            remote
                .read_review(request)
                .await
                .map_err(anyhow::Error::from)
        })
        .await
    }
}
