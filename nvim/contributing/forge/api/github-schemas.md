# GitHub request, result, and recovery schemas

These declarations preserve the native field names, nested operation tags, and null/default behavior. Recovery identities name the captured resource and operation across host restarts. They do not authorize replay of an uncertain network write.

`RecoveryResource.number` is zero for repository-scoped operations and positive for issue, pull-request, and notification resources. The repository identity includes hostname, owner, and repository name. All counters are JSON integers, strings retain their full UTF-8 content, and optional fields accept omission or null. Read each Serde tag as the exact discriminator field for that enum.

## GithubRepositoryId

Source: [model.rs](../../../rust/forge/crates/forge-github/src/model.rs).

```rust
/// Validated remote identity independent of a local checkout or cache path.
#[derive(Clone, Debug, Eq, Hash, PartialEq, Deserialize, Serialize)]
#[serde(try_from = "RepositoryIdentityInput")]
pub struct GithubRepositoryId {
    hostname: String,
    owner: String,
    name: String,
}
```

## SyncScope

Source: [model.rs](../../../rust/forge/crates/forge-github/src/model.rs).

```rust
/// Remote issue history selected for page-level synchronization.
#[derive(Clone, Copy, Debug, Deserialize, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum SyncScope {
    /// Open issues only.
    Open,
    /// Open and closed issues.
    All,
}
```

## SnapshotState

Source: [model.rs](../../../rust/forge/crates/forge-github/src/model.rs).

```rust
/// Issue states included in a completion snapshot.
#[derive(Clone, Copy, Debug, Deserialize, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum SnapshotState {
    /// Open issues only.
    Open,
    /// Open and closed issues.
    All,
}
```

## LabelRecord

Source: [model.rs](../../../rust/forge/crates/forge-github/src/model.rs).

```rust
/// Remote label metadata stored with an issue.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct LabelRecord {
    /// Remote label name.
    pub name: String,
    #[serde(default)]
    /// Optional remote label color.
    pub color: Option<String>,
    #[serde(default)]
    /// Optional remote label description.
    pub description: Option<String>,
}
```

## IssueRecord

Source: [model.rs](../../../rust/forge/crates/forge-github/src/model.rs).

```rust
/// Cached issue metadata and optional body in the existing redb format.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct IssueRecord {
    /// Repository owner/name stored in lowercase by IssueStore.
    pub repo: String,
    /// Repository-local issue number supplied by GitHub.
    pub number: u64,
    /// Remote issue title at the last cache update.
    pub title: String,
    /// Remote issue state or snapshot state filter.
    pub state: String,
    /// Remote issue URL.
    pub url: String,
    #[serde(default)]
    /// Optional remote creation timestamp.
    pub created_at: Option<String>,
    #[serde(default)]
    /// Optional remote last-update timestamp used for completion ordering.
    pub updated_at: Option<String>,
    #[serde(default)]
    /// Optional remote issue body, excluded from completion snapshots.
    pub body: Option<String>,
    #[serde(default)]
    /// Remote labels attached to the cached issue.
    pub labels: Vec<LabelRecord>,
}
```

## SnapshotIssueRecord

Source: [model.rs](../../../rust/forge/crates/forge-github/src/model.rs).

```rust
/// Completion metadata that excludes issue bodies.
#[derive(Clone, Debug, Serialize)]
pub struct SnapshotIssueRecord {
    /// Repository owner/name stored in lowercase by IssueStore.
    pub repo: String,
    /// Repository-local issue number supplied by GitHub.
    pub number: u64,
    /// Remote issue title at the last cache update.
    pub title: String,
    /// Remote issue state or snapshot state filter.
    pub state: String,
    /// Remote issue URL.
    pub url: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    /// Optional remote creation timestamp.
    pub created_at: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    /// Optional remote last-update timestamp used for completion ordering.
    pub updated_at: Option<String>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    /// Remote labels attached to the cached issue.
    pub labels: Vec<LabelRecord>,
}
```

## PageInput

Source: [model.rs](../../../rust/forge/crates/forge-github/src/model.rs).

```rust
/// One remote page and its cursor and high-water update.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct PageInput {
    #[serde(default)]
    /// Issue records belonging to this page or snapshot.
    pub issues: Vec<IssueRecord>,
    #[serde(default)]
    /// Remote cursor for the next history page.
    pub cursor: Option<String>,
    #[serde(default)]
    /// Whether the remote page has a successor.
    pub has_next_page: bool,
    #[serde(default)]
    /// Optional remote count for the selected history scope.
    pub total_count: Option<u64>,
    #[serde(default)]
    /// Whether this page completes the selected history.
    pub completed: bool,
    #[serde(default)]
    /// Optional newest remote update timestamp observed in this page.
    pub high_water: Option<String>,
    #[serde(default)]
    /// Optional timestamp of the completed remote check.
    pub checked_at: Option<i64>,
}
```

## RepoSyncState

Source: [model.rs](../../../rust/forge/crates/forge-github/src/model.rs).

```rust
/// Persisted open and all synchronization progress for one repository.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct RepoSyncState {
    /// Repository owner/name stored in lowercase by IssueStore.
    pub repo: String,
    /// Revision of committed issue records and sync state. Detail-only writes do not advance it.
    #[serde(default)]
    pub revision: u64,
    #[serde(default)]
    /// Persisted cursor for open-issue history.
    pub open_cursor: Option<String>,
    #[serde(default)]
    /// Persisted cursor for all-issue history.
    pub all_cursor: Option<String>,
    #[serde(default)]
    /// Whether initial open-issue history completed.
    pub open_historical_complete: bool,
    #[serde(default)]
    /// Whether initial all-issue history completed.
    pub all_historical_complete: bool,
    #[serde(default)]
    /// Newest remote update timestamp observed in open-issue sync.
    pub open_high_water: Option<String>,
    #[serde(default)]
    /// Newest remote update timestamp observed in all-issue sync.
    pub all_high_water: Option<String>,
    #[serde(default)]
    /// Last remote open-issue count.
    pub open_total_count: Option<u64>,
    #[serde(default)]
    /// Last remote all-issue count.
    pub all_total_count: Option<u64>,
    #[serde(default)]
    /// Timestamp of the last open-issue check.
    pub last_open_checked_at: Option<i64>,
    #[serde(default)]
    /// Timestamp of the last all-issue check.
    pub last_all_checked_at: Option<i64>,
    #[serde(default)]
    /// Number of issue records in this state or snapshot.
    pub issue_count: u64,
}
```

## UpsertOutput

Source: [model.rs](../../../rust/forge/crates/forge-github/src/model.rs).

```rust
/// Committed page count and resulting repository synchronization state.
#[derive(Debug, Serialize)]
pub struct UpsertOutput {
    /// Repository owner/name stored in lowercase by IssueStore.
    pub repo: String,
    /// Number of issue records committed from the input page.
    pub upserted: usize,
    /// Remote issue state or snapshot state filter.
    pub state: RepoSyncState,
}
```

## SnapshotRecovery

Source: [model.rs](../../../rust/forge/crates/forge-github/src/model.rs).

```rust
/// State and publication outcome observed while one database handle excludes competing writers.
#[derive(Debug, Serialize)]
pub struct SnapshotRecovery {
    pub state: RepoSyncState,
    pub ready: bool,
    pub republished: bool,
}
```

## IssueDetail

Source: [model.rs](../../../rust/forge/crates/forge-github/src/model.rs).

```rust
/// Normalized issue content shared by remote reads and the detail cache.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct IssueDetail {
    pub kind: String,
    pub node_id: String,
    pub repo: String,
    pub number: u64,
    pub title: String,
    pub body: String,
    pub url: String,
    pub state: String,
    pub author: String,
    pub created_at: String,
    pub updated_at: String,
    pub labels: Vec<String>,
    pub assignees: Vec<String>,
    pub milestone: String,
    pub projects: Vec<String>,
    pub comments_count: usize,
    pub comments: Vec<ConversationComment>,
    pub subscription: String,
    pub is_draft: bool,
}
```

## ConversationComment

Source: [model.rs](../../../rust/forge/crates/forge-github/src/model.rs).

```rust
/// Remote issue conversation content with a source URL for navigation.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ConversationComment {
    pub body: String,
    pub author: String,
    pub created_at: String,
    pub updated_at: String,
    pub url: String,
}
```

## DetailRecord

Source: [model.rs](../../../rust/forge/crates/forge-github/src/model.rs).

```rust
/// Cached remote issue detail and its fetch timestamp.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct DetailRecord {
    /// Repository owner/name stored in lowercase by IssueStore.
    pub repo: String,
    /// Repository-local issue number supplied by GitHub.
    pub number: u64,
    /// Timestamp of the remote detail fetch.
    pub fetched_at: i64,
    /// Remote detail payload retained without completion truncation.
    pub item: serde_json::Value,
}
```

## DetailOutput

Source: [model.rs](../../../rust/forge/crates/forge-github/src/model.rs).

```rust
/// Lookup result that distinguishes absent details from cached details.
#[derive(Debug, Serialize)]
pub struct DetailOutput {
    /// Repository owner/name stored in lowercase by IssueStore.
    pub repo: String,
    /// Repository-local issue number supplied by GitHub.
    pub number: u64,
    /// Whether the requested detail exists in the cache.
    pub found: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    /// Timestamp of the remote detail fetch.
    pub fetched_at: Option<i64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    /// Remote detail payload retained without completion truncation.
    pub item: Option<serde_json::Value>,
}
```

## DetailsOutput

Source: [model.rs](../../../rust/forge/crates/forge-github/src/model.rs).

```rust
/// Detail lookup results in the caller-provided issue order.
#[derive(Debug, Serialize)]
pub struct DetailsOutput {
    /// Repository owner/name stored in lowercase by IssueStore.
    pub repo: String,
    /// Detail results in request order, including misses.
    pub details: Vec<DetailOutput>,
}
```

## RemoteActor

Source: [remote.rs](../../../rust/forge/crates/forge-github/src/remote.rs).

```rust
#[derive(Clone, Debug, Deserialize, Serialize, Eq, PartialEq)]
pub struct RemoteActor {
    pub login: String,
    pub node_id: String,
}
```

## RemoteFailureKind

Source: [remote.rs](../../../rust/forge/crates/forge-github/src/remote.rs).

```rust
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
```

## RemoteFailure

Source: [remote.rs](../../../rust/forge/crates/forge-github/src/remote.rs).

```rust
/// Preserves remote diagnostics without interpreting a failed read as an empty page.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct RemoteFailure {
    pub kind: RemoteFailureKind,
    pub message: String,
}
```

## IssueDetailRequest

Source: [remote.rs](../../../rust/forge/crates/forge-github/src/remote.rs).

```rust
/// One issue detail read with explicit remote identity.
#[derive(Clone, Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct IssueDetailRequest {
    pub repository: GithubRepositoryId,
    pub number: u64,
}
```

## CreationContext

Source: [remote/gh/creation.rs](../../../rust/forge/crates/forge-github/src/remote/gh/creation.rs).

```rust
#[derive(Debug, Serialize)]
pub struct CreationContext {
    pub repository: GithubRepositoryId,
    pub repository_node_id: String,
    pub branch: String,
    pub head_commit: String,
}
```

## PendingReviewExpected

Source: [remote/gh/submission.rs](../../../rust/forge/crates/forge-github/src/remote/gh/submission.rs).

```rust
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct PendingReviewExpected {
    pub id: u64,
    pub body: String,
}
```

## ReviewSubmissionContext

Source: [remote/gh/submission.rs](../../../rust/forge/crates/forge-github/src/remote/gh/submission.rs).

```rust
#[derive(Clone, Debug, Serialize)]
pub struct ReviewSubmissionContext {
    pub actor_node_id: String,
    pub commit_id: String,
    pub pending_review_id: Option<u64>,
    pub pending_comment: Vec<PendingReviewMember>,
}
```

## NotificationReadRequest

Source: [notification.rs](../../../rust/forge/crates/forge-github/src/notification.rs).

```rust
/// One participating-notification page across every repository on a selected host.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct NotificationReadRequest {
    pub hostname: String,
    pub cursor: Option<u32>,
}
```

## NotificationRecord

Source: [notification.rs](../../../rust/forge/crates/forge-github/src/notification.rs).

```rust
/// Captured notification identity and subject routes from one validated host response.
#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub struct NotificationRecord {
    pub id: String,
    pub repository: GithubRepositoryId,
    pub title: String,
    pub kind: String,
    pub unread: bool,
    pub reason: String,
    pub updated_at: String,
    pub subject_endpoint: Option<String>,
    pub comment_endpoint: Option<String>,
    pub number: Option<u64>,
    pub browser_url: Option<String>,
}
```

## NotificationPage

Source: [notification.rs](../../../rust/forge/crates/forge-github/src/notification.rs).

```rust
#[derive(Clone, Debug, Serialize)]
pub struct NotificationPage {
    pub record: Vec<NotificationRecord>,
    pub next_cursor: Option<u32>,
    pub complete: bool,
}
```

## MetadataRequest

Source: [metadata.rs](../../../rust/forge/crates/forge-github/src/metadata.rs).

```rust
#[derive(Clone, Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct MetadataRequest {
    pub repository: GithubRepositoryId,
    #[serde(default = "default_ttl")]
    pub ttl_seconds: u64,
}
```

## RepositoryMetadata

Source: [metadata.rs](../../../rust/forge/crates/forge-github/src/metadata.rs).

```rust
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct RepositoryMetadata {
    pub repo: String,
    /// Legacy records omit hostname and inherit identity from their validated cache path.
    #[serde(default)]
    pub hostname: Option<String>,
    pub fetched_at: u64,
    pub contributors: Vec<RepositoryUser>,
    /// Failed sources retained when another source completed successfully.
    #[serde(default)]
    pub failure: Vec<RemoteFailure>,
}
```

## RepositoryUser

Source: [metadata.rs](../../../rust/forge/crates/forge-github/src/metadata.rs).

```rust
#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub struct RepositoryUser {
    pub login: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub name: Option<String>,
}
```

## SyncRequest

Source: [sync.rs](../../../rust/forge/crates/forge-github/src/sync.rs).

```rust
/// One repository refresh using the existing scope and snapshot-path contract.
#[derive(Clone, Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct SyncRequest {
    pub repository: GithubRepositoryId,
    pub scope: SyncScope,
    #[serde(default)]
    pub manual: bool,
    pub snapshot: PathBuf,
}
```

## SyncOutcome

Source: [sync.rs](../../../rust/forge/crates/forge-github/src/sync.rs).

```rust
/// Completed refresh or a ten-minute freshness skip.
#[derive(Clone, Debug, Serialize)]
pub struct SyncOutcome {
    pub refreshed: bool,
    pub fetched: usize,
    pub pages: usize,
}
```

## SyncProgress

Source: [sync.rs](../../../rust/forge/crates/forge-github/src/sync.rs).

```rust
/// Current sync activity. Slow observers retain only the newest progress value.
#[derive(Clone, Debug, Default, Serialize)]
pub struct SyncProgress {
    pub phase: SyncPhase,
    pub fetched: usize,
    pub total: Option<u64>,
    pub pages: usize,
    pub retry_after_ms: Option<u64>,
}
```

## SyncPhase

Source: [sync.rs](../../../rust/forge/crates/forge-github/src/sync.rs).

```rust
#[derive(Clone, Copy, Debug, Default, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum SyncPhase {
    #[default]
    Starting,
    Reading,
    Indexing,
    Publishing,
    RateLimited,
    Complete,
    Fresh,
}
```

## IssueOperation

Source: [service.rs](../../../rust/forge/crates/forge-github/src/service.rs).

```rust
/// Issue storage operation with owned inputs retained for the blocking worker's lifetime.
#[derive(Debug, Deserialize)]
#[serde(tag = "operation", rename_all = "snake_case", deny_unknown_fields)]
pub enum IssueOperation {
    /// Commits one issue page and its synchronization state.
    UpsertPage { scope: SyncScope, page: PageInput },
    /// Reads repository synchronization state.
    State {},
    /// Removes current-format repository data under exclusive operation ownership.
    DeleteCache {},
    /// Reads one issue detail, including explicit cache misses.
    Detail { number: u64 },
    /// Reads issue details in request order.
    Details { number: Vec<u64> },
    /// Commits a remote issue detail.
    UpsertDetail { number: u64, detail: DetailRecord },
    /// Atomically publishes completion metadata to a local file.
    PublishSnapshot {
        state: SnapshotState,
        output: PathBuf,
    },
    /// Reconciles the completion file against the committed issue-state revision.
    ReconcileSnapshot {
        state: SnapshotState,
        output: PathBuf,
    },
}
```

## IssueResult

Source: [service.rs](../../../rust/forge/crates/forge-github/src/service.rs).

```rust
/// Typed storage result serialized using the existing command payload shape.
#[derive(Debug, Serialize)]
#[serde(untagged)]
pub enum IssueResult {
    /// Issue state and whether its completion file was repaired before returning.
    Reconciled(Box<crate::model::SnapshotRecovery>),
    /// Committed page count and synchronization state.
    UpsertPage(Box<UpsertOutput>),
    /// Persisted repository synchronization state.
    State(Box<RepoSyncState>),
    /// Cached detail or explicit miss.
    Detail(DetailOutput),
    /// Ordered detail lookup results.
    Details(DetailsOutput),
    /// Normalized committed detail.
    UpsertDetail(DetailRecord),
    /// Number of completion records published to disk.
    Published { issue_count: usize },
    /// Whether a repository directory existed and was removed.
    Deleted { deleted: bool },
}
```

## CommentKind

Source: [comment.rs](../../../rust/forge/crates/forge-github/src/comment.rs).

```rust
#[derive(Clone, Copy, Debug, Deserialize, Serialize, Eq, PartialEq)]
pub enum CommentKind {
    IssueComment,
    PullRequestReviewComment,
}
```

## CommentTarget

Source: [comment.rs](../../../rust/forge/crates/forge-github/src/comment.rs).

```rust
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct CommentTarget {
    pub pull_request: PullRequestTarget,
    pub kind: CommentKind,
    pub node_id: String,
    pub database_id: u64,
}
```

## CommentState

Source: [comment.rs](../../../rust/forge/crates/forge-github/src/comment.rs).

```rust
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
```

## CommentOperation

Source: [comment.rs](../../../rust/forge/crates/forge-github/src/comment.rs).

```rust
#[derive(Clone, Debug, Deserialize)]
#[serde(tag = "operation", rename_all = "snake_case", deny_unknown_fields)]
pub enum CommentOperation {
    Edit { body: String, receipt: String },
    Delete { receipt: String },
    Reconcile,
}
```

## CommentResult

Source: [comment.rs](../../../rust/forge/crates/forge-github/src/comment.rs).

```rust
#[derive(Debug, Serialize)]
#[serde(tag = "outcome", rename_all = "snake_case")]
pub enum CommentResult {
    Confirmed { state: Option<CommentState> },
    Reconciled { state: Option<CommentState> },
    Rejected { message: String },
    OutcomeUnknown { message: String },
}
```

## PullRequestTarget

Source: [pull_request.rs](../../../rust/forge/crates/forge-github/src/pull_request.rs).

```rust
/// Expected PR identity, validated again against remote truth before a transition.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct PullRequestTarget {
    pub repository: GithubRepositoryId,
    pub number: u64,
    pub node_id: String,
}
```

## PullRequestRequest

Source: [pull_request.rs](../../../rust/forge/crates/forge-github/src/pull_request.rs).

```rust
#[derive(Clone, Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct PullRequestRequest {
    pub target: PullRequestTarget,
    pub request: PullRequestOperation,
}
```

## PullRequestOperation

Source: [pull_request.rs](../../../rust/forge/crates/forge-github/src/pull_request.rs).

```rust
#[derive(Clone, Debug, Deserialize)]
#[serde(tag = "operation", rename_all = "snake_case", deny_unknown_fields)]
pub enum PullRequestOperation {
    Transition { desired: DesiredPullRequestState },
    Edit { edit: PullRequestEdit },
    Reconcile,
    ReconcileEdit { edit: PullRequestEdit },
}
```

## PullRequestEdit

Source: [pull_request.rs](../../../rust/forge/crates/forge-github/src/pull_request.rs).

```rust
/// Submitted fields. None leaves a field unchanged, while an empty body clears it.
#[derive(Clone, Debug, Deserialize, Serialize, Eq, PartialEq)]
#[serde(deny_unknown_fields)]
pub struct PullRequestEdit {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub title: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub body: Option<String>,
}
```

## PullRequestLifecycle

Source: [pull_request.rs](../../../rust/forge/crates/forge-github/src/pull_request.rs).

```rust
#[derive(Clone, Copy, Debug, Deserialize, Serialize, Eq, PartialEq)]
#[serde(rename_all = "UPPERCASE")]
pub enum PullRequestLifecycle {
    Open,
    Closed,
    Merged,
}
```

## DesiredPullRequestState

Source: [pull_request.rs](../../../rust/forge/crates/forge-github/src/pull_request.rs).

```rust
#[derive(Clone, Copy, Debug, Deserialize, Serialize, Eq, PartialEq)]
#[serde(rename_all = "UPPERCASE")]
pub enum DesiredPullRequestState {
    Open,
    Closed,
    Draft,
}
```

## PullRequestOutcome

Source: [pull_request.rs](../../../rust/forge/crates/forge-github/src/pull_request.rs).

```rust
#[derive(Clone, Copy, Debug, Serialize, Eq, PartialEq)]
#[serde(rename_all = "snake_case")]
pub enum PullRequestOutcome {
    Confirmed,
    Rejected,
    OutcomeUnknown,
    Reconciled,
}
```

## PullRequestResult

Source: [pull_request.rs](../../../rust/forge/crates/forge-github/src/pull_request.rs).

```rust
/// Final observation or an explicit unresolved outcome. Unknown writes expose no current state.
#[derive(Debug, Serialize)]
pub struct PullRequestResult {
    pub ok: bool,
    pub outcome: PullRequestOutcome,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub state: Option<PullRequestLifecycle>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub is_draft: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub message: Option<String>,
    /// Equality of every submitted field with observed values, absent for lifecycle requests.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub matches_submission: Option<bool>,
    /// Exact remote fields returned by reconciliation for feature-owned saved baselines.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub text: Option<PullRequestEdit>,
}
```

## RecoveryResourceKind

Source: [recovery.rs](../../../rust/forge/crates/forge-github/src/recovery.rs).

```rust
#[derive(Clone, Copy, Debug, Eq, PartialEq, Deserialize, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum RecoveryResourceKind {
    Repository,
    PullRequest,
    Issue,
    Notification,
}
```

## RecoveryResource

Source: [recovery.rs](../../../rust/forge/crates/forge-github/src/recovery.rs).

```rust
#[derive(Clone, Debug, Eq, PartialEq, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct RecoveryResource {
    pub repository: GithubRepositoryId,
    pub kind: RecoveryResourceKind,
    pub number: u64,
}
```

## RecoveryOperation

Source: [recovery.rs](../../../rust/forge/crates/forge-github/src/recovery.rs).

```rust
#[derive(Clone, Copy, Debug, Eq, PartialEq, Deserialize, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum RecoveryOperation {
    PullRequestCreate,
    ConversationCreate,
    InlineCreate,
    ReplyCreate,
    PendingReviewCreate,
    ReviewSubmit,
    CommentEdit,
    CommentDelete,
    PullRequestEdit,
    PullRequestTransition,
    ReviewerSet,
    MilestoneSet,
    IssueEdit,
    NotificationRead,
    NotificationDone,
}
```

## RecoveryCapture

Source: [recovery.rs](../../../rust/forge/crates/forge-github/src/recovery.rs).

```rust
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct RecoveryCapture {
    pub operation_id: String,
    pub actor: String,
    pub operation: RecoveryOperation,
    pub edit_sequence: Option<u64>,
    pub submitted: Value,
}
```

## RecoveryPhase

Source: [recovery.rs](../../../rust/forge/crates/forge-github/src/recovery.rs).

```rust
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "phase", rename_all = "snake_case", deny_unknown_fields)]
pub enum RecoveryPhase {
    Prepared,
    DispatchPossible,
    Confirmed { result: Value },
    Rejected { diagnostic: String },
    OutcomeUnknown { diagnostic: String },
    UserLinked { result: Value, diagnostic: String },
    UserClosedUnknown { diagnostic: String },
}
```

## RecoveryRecord

Source: [recovery.rs](../../../rust/forge/crates/forge-github/src/recovery.rs).

```rust
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct RecoveryRecord {
    pub version: u32,
    pub resource: RecoveryResource,
    pub capture: RecoveryCapture,
    pub reserved_bytes: usize,
    pub state: RecoveryPhase,
    pub confirmed_steps: Vec<Value>,
}
```

## ReviewMutationRequest

Source: [review_mutation.rs](../../../rust/forge/crates/forge-github/src/review_mutation.rs).

```rust
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct ReviewMutationRequest {
    pub parent_node_id: Option<String>,
    pub resource: RecoveryResource,
    pub operation_id: String,
    pub actor_node_id: String,
    pub edit_sequence: Option<u64>,
    pub draft_target: Option<String>,
    pub mutation: ReviewMutation,
}
```

## ReviewMutation

Source: [review_mutation.rs](../../../rust/forge/crates/forge-github/src/review_mutation.rs).

```rust
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "operation", rename_all = "snake_case", deny_unknown_fields)]
pub enum ReviewMutation {
    PullRequestCreate {
        repository_node_id: String,
        title: String,
        body: String,
        base: String,
        head: String,
        head_commit: String,
        draft: bool,
    },
    PullRequestEdit {
        node_id: String,
        title: Option<String>,
        body: Option<String>,
    },
    PullRequestTransition {
        node_id: String,
        desired: crate::pull_request::DesiredPullRequestState,
    },
    PullRequestStep {
        node_id: String,
        action: PullRequestAction,
    },
    PullRequestObserve {
        node_id: String,
    },
    ConversationCreate {
        body: String,
    },
    InlineCreate {
        body: String,
        path: String,
        line: u32,
        side: DiffSide,
        start_line: Option<u32>,
        start_side: Option<DiffSide>,
        commit_id: String,
    },
    ReplyCreate {
        body: String,
        parent_comment_id: u64,
    },
    PendingReviewCreate {
        commit_id: String,
    },
    PendingReviewComment {
        body: String,
        review_node_id: String,
        path: String,
        position: u32,
        commit_id: String,
    },
    ReviewSubmit {
        body: String,
        event: ReviewEvent,
        pending_review_id: Option<u64>,
        commit_id: Option<String>,
        #[serde(default)]
        pending_comment: Option<Vec<PendingReviewMember>>,
        #[serde(default)]
        comment: Vec<ReviewSubmissionComment>,
    },
    ConversationEdit {
        comment_id: u64,
        body: String,
    },
    ConversationDelete {
        comment_id: u64,
    },
    InlineEdit {
        comment_id: u64,
        body: String,
    },
    InlineDelete {
        comment_id: u64,
    },
    ReviewerChange {
        add: ReviewerSelection,
        remove: ReviewerSelection,
    },
    ReviewerAdd {
        reviewer: Vec<String>,
        team: Vec<String>,
    },
    ReviewerRemove {
        reviewer: Vec<String>,
        team: Vec<String>,
    },
    MilestoneAssign {
        milestone_number: Option<u64>,
    },
    MilestoneCreate {
        title: String,
    },
    MilestoneCreateAndAssign {
        title: String,
    },
    MilestoneAssignCreated,
    IssueEdit {
        title: Option<String>,
        body: Option<String>,
        #[serde(default)]
        add_assignees: Vec<String>,
        #[serde(default)]
        remove_assignees: Vec<String>,
    },
    IssueAssigneeAdd {
        assignee: Vec<String>,
    },
    IssueAssigneeRemove {
        assignee: Vec<String>,
    },
    NotificationRead,
    NotificationDone,
}
```

## RecoveryResolution

Source: [review_mutation.rs](../../../rust/forge/crates/forge-github/src/review_mutation.rs).

```rust
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "resolution", rename_all = "snake_case", deny_unknown_fields)]
pub enum RecoveryResolution {
    Link { remote_id: u64 },
    CloseUnknown,
    NotDispatched,
}
```

## PendingReviewMember

Source: [review_mutation.rs](../../../rust/forge/crates/forge-github/src/review_mutation.rs).

```rust
#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
#[serde(deny_unknown_fields)]
pub struct PendingReviewMember {
    pub id: u64,
    pub node_id: String,
    pub body: String,
    pub path: String,
    pub commit_id: String,
    pub position: Option<u32>,
    pub line: Option<u32>,
    pub side: Option<String>,
}
```

