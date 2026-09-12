use std::future::Future;
use std::pin::Pin;

use serde::{Deserialize, Serialize};

use crate::metadata::RepositoryUsers;
use crate::model::{GithubRepositoryId, IssueDetail, IssueRecord, SyncScope};
use crate::pull_request::{
    PullRequestMutation, PullRequestMutationOutcome, PullRequestState, PullRequestTarget,
};

mod gh;
pub use gh::{
    CreationContext, GhClient, GhDirectory, PendingReviewExpected, ReviewSubmissionContext,
};

/// Read-only issue page request. Incremental refresh includes closed issues in either scope.
#[derive(Clone, Debug)]
pub struct IssuePageRequest {
    pub repository: GithubRepositoryId,
    pub scope: SyncScope,
    pub incremental: bool,
    pub cursor: Option<String>,
}

/// One issue detail read with explicit remote identity.
#[derive(Clone, Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct IssueDetailRequest {
    pub repository: GithubRepositoryId,
    pub number: u64,
}

/// A decoded page with explicit pagination and rate-budget state.
#[derive(Clone, Debug)]
pub struct IssuePage {
    pub issues: Vec<IssueRecord>,
    pub next_cursor: Option<String>,
    pub total_count: Option<u64>,
    pub rate_remaining: Option<u64>,
}

/// Remote failure category controlling read retry policy.
#[derive(Clone, Copy, Debug, Deserialize, Serialize, Eq, PartialEq)]
#[serde(rename_all = "snake_case")]
pub enum RemoteFailureKind {
    Busy,
    RateLimited,
    Unauthorized,
    NotFound,
    InvalidResponse,
    Transport,
}

/// Preserves remote diagnostics without interpreting a failed read as an empty page.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct RemoteFailure {
    pub kind: RemoteFailureKind,
    pub message: String,
}

#[derive(Clone, Debug, Deserialize, Serialize, Eq, PartialEq)]
pub struct RemoteActor {
    pub login: String,
    pub node_id: String,
}

impl std::fmt::Display for RemoteFailure {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(formatter, "{:?}: {}", self.kind, self.message)
    }
}

impl std::error::Error for RemoteFailure {}

/// Injectable remote requests. Implementations retain subprocess cleanup until completion.
pub trait GithubRemote: Send + Sync {
    /// Reads the authenticated account identity used to validate a durable mutation capture.
    fn read_actor(
        &self,
        _repository: GithubRepositoryId,
    ) -> Pin<Box<dyn Future<Output = Result<RemoteActor, RemoteFailure>> + Send + '_>> {
        Box::pin(async {
            Err(RemoteFailure {
                kind: RemoteFailureKind::Unauthorized,
                message: "remote does not provide authenticated actor identity".into(),
            })
        })
    }

    /// Reads validated PR identity and lifecycle, selecting title and raw body when include_text is true.
    fn read_pull_request(
        &self,
        target: PullRequestTarget,
        include_text: bool,
    ) -> Pin<Box<dyn Future<Output = Result<PullRequestState, RemoteFailure>> + Send + '_>>;

    /// Performs exactly one write attempt and distinguishes proven rejection from uncertainty.
    /// The future must not complete before its native request has terminated and been collected.
    fn mutate_pull_request(
        &self,
        target: PullRequestTarget,
        mutation: PullRequestMutation,
    ) -> Pin<Box<dyn Future<Output = PullRequestMutationOutcome> + Send + '_>>;

    /// Merges repository user sources while retaining diagnostics from partially failed reads.
    fn read_repository_users(
        &self,
        repository: GithubRepositoryId,
    ) -> Pin<Box<dyn Future<Output = Result<RepositoryUsers, RemoteFailure>> + Send + '_>>;

    fn read_issues(
        &self,
        request: IssuePageRequest,
    ) -> Pin<Box<dyn Future<Output = Result<IssuePage, RemoteFailure>> + Send + '_>>;

    fn read_issue_detail(
        &self,
        request: IssueDetailRequest,
    ) -> Pin<Box<dyn Future<Output = Result<IssueDetail, RemoteFailure>> + Send + '_>>;
}
