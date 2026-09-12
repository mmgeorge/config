use std::future::Future;
use std::pin::Pin;
use std::sync::Arc;

use anyhow::{Context, Result, ensure};
use serde::{Deserialize, Serialize};
use serde_json::Value;

use crate::model::GithubRepositoryId;
use crate::remote::{GithubRemote, RemoteFailure};
use crate::service::GithubService;

pub const REVIEW_PAGE_SIZE: usize = 100;
pub const MAX_REVIEW_RECORDS: usize = 10_000;
pub const MAX_REVIEW_DOCUMENT_BYTES: usize = 16 * 1024 * 1024;

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "section", rename_all = "snake_case", deny_unknown_fields)]
pub enum ReviewSection {
    Overview,
    Commits,
    Files,
    Reviews,
    ReviewComments,
    Conversation,
    Threads,
    ThreadComments { thread_node_id: String },
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

#[derive(Clone, Debug, Serialize)]
pub struct ReviewCollection {
    pub records: Vec<Value>,
    pub next_cursor: Option<String>,
    pub complete: bool,
    pub diagnostic: Option<String>,
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
            self.cursor.as_ref().is_none_or(|cursor| !cursor.is_empty()
                && cursor.len() <= 512
                && !cursor.contains(['\n', '\r', '\0'])),
            "invalid GitHub document cursor"
        );
        if let ReviewSection::ThreadComments { thread_node_id } = &self.view {
            ensure!(
                !thread_node_id.is_empty()
                    && thread_node_id.len() <= 256
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

    /// Collects metadata until complete or the explicit document limits require further demand.
    pub async fn review_collection(
        &self,
        directory: std::path::PathBuf,
        remote: Arc<dyn GithubReviewRemote>,
        mut request: ReviewReadRequest,
    ) -> Result<ReviewCollection> {
        request.validate()?;
        let mut collection = ReviewCollection {
            records: Vec::new(),
            next_cursor: request.cursor.clone(),
            complete: false,
            diagnostic: None,
        };
        let mut bytes = 0usize;
        let mut visited = std::collections::HashSet::new();
        loop {
            let page = self
                .review_page(directory.clone(), Arc::clone(&remote), request.clone())
                .await?;
            ensure!(
                page.records.len() <= REVIEW_PAGE_SIZE,
                "remote page exceeds 100 records"
            );
            ensure!(
                page.complete == page.next_cursor.is_none(),
                "remote pagination omitted completion identity"
            );
            let page_bytes = serde_json::to_vec(&page.records)?.len();
            if collection.records.len() + page.records.len() > MAX_REVIEW_RECORDS
                || bytes.saturating_add(page_bytes) > MAX_REVIEW_DOCUMENT_BYTES
            {
                collection.diagnostic = Some(
                    "document collection limit reached, additional records require explicit demand"
                        .into(),
                );
                return Ok(collection);
            }
            bytes += page_bytes;
            collection.records.extend(page.records);
            collection.next_cursor = page.next_cursor.clone();
            collection.complete = page.complete;
            if page.complete {
                return Ok(collection);
            }
            let cursor = page.next_cursor.context("incomplete page omitted cursor")?;
            ensure!(
                visited.insert(cursor.clone()),
                "remote document repeated its pagination cursor"
            );
            request.cursor = Some(cursor);
            if collection.records.len() == MAX_REVIEW_RECORDS {
                collection.diagnostic =
                    Some("document metadata limit reached at 10000 records".into());
                return Ok(collection);
            }
        }
    }
}
