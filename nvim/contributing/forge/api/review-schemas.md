# Native review document schemas

These declarations define the result objects and nested inputs used by [GitHub review methods](github.md). A Serde `flatten` attribute merges the referenced object's fields into its parent. `skip` fields are internal and absent on the wire. `skip_serializing_if` omits matching optional values; other optional result fields serialize as JSON null. Ownership IDs and revisions must be copied from the corresponding document result rather than reconstructed from display text.

`ReviewThreadDelivery.thread` uses a custom serializer. Its wire value contains only `node_id`, `loaded_comments`, `total_comments`, and `next_cursor`. Complete comment content belongs to the rendered document and retained section data. The complete `ReviewThread` declaration below also describes those retained section objects.

`NotificationUpdate.recovery` also uses a custom serializer: a present record follows the GitHub recovery schema except that `resource.number` is a decimal string, preserving notification IDs on clients that use floating-point numbers. Other recovery methods retain their own numeric schema.

Patch and snapshot fields use the shared [buffer schemas](repository-schemas.md). Temporary acquisition, publication, and recovery state can require another request; a partial page is not permission to discard the remaining content.

## ReviewMode

Source: [review.rs](../../../rust/forge/crates/forge-review/src/review.rs).

```rust
#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize, serde::Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ReviewMode {
    Overview,
    Batched,
}
```

## ReviewVerdict

Source: [review.rs](../../../rust/forge/crates/forge-review/src/review.rs).

```rust
#[derive(Clone, Copy, Debug, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ReviewVerdict {
    Approve,
    RequestChanges,
    Comment,
}
```

## ReviewField

Source: [review.rs](../../../rust/forge/crates/forge-review/src/review.rs).

```rust
#[derive(Debug, Serialize, serde::Deserialize)]
pub struct ReviewField {
    pub region: RegionId,
    pub revision: RegionRevision,
    pub sequence: EditSequence,
    pub text: String,
    pub baseline: String,
    pub dirty: bool,
    pub uncertain: bool,
}
```

## ReviewSnapshot

Source: [review.rs](../../../rust/forge/crates/forge-review/src/review.rs).

```rust
#[derive(Debug, Serialize)]
pub struct ReviewSnapshot {
    pub document: DocumentId,
    pub field: Vec<ReviewField>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub summary: Option<ReviewField>,
    pub saving: bool,
    pub uncertain: bool,
}
```

## ReviewSaveResult

Source: [service.rs](../../../rust/forge/crates/forge-review/src/service.rs).

```rust
#[derive(Debug, Serialize)]
pub struct ReviewSaveResult {
    pub snapshot: ReviewSnapshot,
    pub remote: Option<PullRequestResult>,
}
```

## ReviewLifecycle

Source: [service.rs](../../../rust/forge/crates/forge-review/src/service.rs).

```rust
#[derive(Clone, Copy, Debug, Serialize, Eq, PartialEq)]
pub struct ReviewLifecycle {
    pub state: PullRequestLifecycle,
    pub is_draft: bool,
}
```

## ReviewLifecycleDelivery

Source: [service.rs](../../../rust/forge/crates/forge-review/src/service.rs).

```rust
#[derive(Debug, Serialize)]
pub struct ReviewLifecycleDelivery {
    pub lifecycle: Option<ReviewLifecycle>,
    pub recovery: Option<forge_github::recovery::RecoveryRecord>,
    pub fresh_required: bool,
}
```

## ReviewBatchedDelivery

Source: [service.rs](../../../rust/forge/crates/forge-review/src/service.rs).

```rust
#[derive(Debug, Serialize)]
pub struct ReviewBatchedDelivery {
    pub snapshot: ReviewSnapshot,
    pub mode: ReviewMode,
    pub viewed_file: Vec<String>,
}
```

## ReviewBatchedSubmissionDelivery

Source: [service.rs](../../../rust/forge/crates/forge-review/src/service.rs).

```rust
#[derive(Debug, Serialize)]
pub struct ReviewBatchedSubmissionDelivery {
    pub snapshot: ReviewSnapshot,
    pub mode: ReviewMode,
    pub viewed_file: Vec<String>,
    pub outcome: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub operation_id: Option<String>,
}
```

## ReviewBatchedRecoveryDelivery

Source: [service.rs](../../../rust/forge/crates/forge-review/src/service.rs).

```rust
#[derive(Debug, Serialize)]
pub struct ReviewBatchedRecoveryDelivery {
    pub submission: ReviewBatchedSubmissionDelivery,
    pub fresh_required: bool,
}
```

## ReviewEditResult

Source: [service/document.rs](../../../rust/forge/crates/forge-review/src/service/document.rs).

```rust
#[derive(Debug, Serialize)]
pub struct ReviewEditResult {
    #[serde(flatten)]
    pub acknowledgement: EditAcknowledgement,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub patch: Option<BufferPatch>,
}
```

## ReviewMaterialization

Source: [service/document.rs](../../../rust/forge/crates/forge-review/src/service/document.rs).

```rust
#[derive(Debug, Serialize)]
pub struct ReviewMaterialization {
    pub diff_view: forge_diff::view::DiffViewOptions,
    pub field: Vec<crate::review::ReviewField>,
    pub comment: Vec<crate::review::ReviewCommentSnapshot>,
    pub inline_anchor: std::collections::BTreeMap<TargetId, crate::comments::CommentAnchor>,
    /// Durations in microseconds for the current presentation.
    pub timing: std::collections::BTreeMap<&'static str, u64>,
    pub snapshot: BufferSnapshot,
    pub patch: Option<BufferPatch>,
}
```

## ReviewFileAnalysis

Source: [service/file.rs](../../../rust/forge/crates/forge-review/src/service/file.rs).

```rust
#[derive(Clone, Debug, Serialize)]
pub struct ReviewFileAnalysis {
    pub path: String,
    pub base_commit: String,
    pub head_commit: String,
    pub merge_base: String,
    pub old_blob: Option<String>,
    pub new_blob: Option<String>,
    pub syntax_diagnostic: Option<String>,
    pub more: bool,
}
```

## ReviewFileDelivery

Source: [service/file.rs](../../../rust/forge/crates/forge-review/src/service/file.rs).

```rust
#[derive(Debug, Serialize)]
pub struct ReviewFileDelivery {
    #[serde(flatten)]
    pub file: ReviewFileAnalysis,
    pub patch: Option<forge_buffer::patch::BufferPatch>,
}
```

## ReviewActionDelivery

Source: [service/input.rs](../../../rust/forge/crates/forge-review/src/service/input.rs).

```rust
#[derive(Debug, serde::Serialize)]
pub struct ReviewActionDelivery {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub diff_view: Option<forge_diff::view::DiffViewOptions>,
    pub patch: Vec<forge_buffer::patch::BufferPatch>,
    pub effect: Option<ReviewActionEffect>,
    pub diagnostic: Option<String>,
    pub refresh: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub comment: Option<crate::review::ReviewCommentSnapshot>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub choice: Option<Vec<ReviewActionChoice>>,
}
```

## ReviewActionChoice

Source: [service/input.rs](../../../rust/forge/crates/forge-review/src/service/input.rs).

```rust
#[derive(Debug, serde::Serialize)]
pub struct ReviewActionChoice {
    pub key: &'static str,
    pub label: &'static str,
    pub value: &'static str,
}
```

## ReviewActionEffect

Source: [service/input.rs](../../../rust/forge/crates/forge-review/src/service/input.rs).

```rust
#[derive(Debug, serde::Serialize)]
pub struct ReviewActionEffect {
    pub id: String,
    #[serde(flatten)]
    pub input: DocumentInput,
    pub kind: &'static str,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub url: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub path: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub workspace: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub oid: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub message: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub level: Option<u8>,
}
```

## ReviewSectionKind

Source: [service/section.rs](../../../rust/forge/crates/forge-review/src/service/section.rs).

```rust
#[derive(Clone, Copy, Debug, Deserialize, Serialize, PartialEq, Eq, PartialOrd, Ord)]
#[serde(rename_all = "snake_case")]
pub enum ReviewSectionKind {
    Overview,
    Commits,
    Files,
    Reviews,
    Conversation,
    ReviewComments,
    Threads,
    Checks,
    RequestedReviewers,
}
```

## ReviewSectionItem

Source: [service/section.rs](../../../rust/forge/crates/forge-review/src/service/section.rs).

```rust
#[derive(Debug, Serialize)]
pub struct ReviewSectionItem {
    pub identity: String,
    pub title: String,
    pub body: Option<String>,
    pub url: Option<String>,
    pub detail: Vec<(String, String)>,
    pub source: Option<ReviewFileSource>,
    pub comparison: Option<ReviewComparisonIdentity>,
    pub commit: Option<ReviewCommitPresentation>,
    pub check: Option<ReviewCheckPresentation>,
    pub review: Option<ReviewSummaryPresentation>,
    pub thread: Option<Arc<super::thread::ReviewThread>>,
}
```

## ReviewCommitPresentation

Source: [service/section.rs](../../../rust/forge/crates/forge-review/src/service/section.rs).

```rust
#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub struct ReviewCommitPresentation {
    pub sha: String,
    pub headline: String,
    pub committed_at: String,
}
```

## ReviewCheckPresentation

Source: [service/section.rs](../../../rust/forge/crates/forge-review/src/service/section.rs).

```rust
#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct ReviewCheckPresentation {
    pub name: String,
    pub state: String,
    pub workflow: Option<String>,
}
```

## ReviewSummaryPresentation

Source: [service/section.rs](../../../rust/forge/crates/forge-review/src/service/section.rs).

```rust
#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct ReviewSummaryPresentation {
    pub author: String,
    pub state: String,
    pub submitted_at: String,
}
```

## ReviewComparisonIdentity

Source: [service/section.rs](../../../rust/forge/crates/forge-review/src/service/section.rs).

```rust
#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct ReviewComparisonIdentity {
    pub base_commit: String,
    pub head_commit: String,
    pub head_repository: String,
}
```

## ReviewFileSource

Source: [service/section.rs](../../../rust/forge/crates/forge-review/src/service/section.rs).

```rust
#[derive(Clone, Debug, Serialize)]
pub struct ReviewFileSource {
    pub path: String,
    pub status: String,
    pub previous_path: Option<String>,
    pub blob: String,
    pub patch: Option<String>,
}
```

## ReviewSectionState

Source: [service/section.rs](../../../rust/forge/crates/forge-review/src/service/section.rs).

```rust
#[derive(Clone, Debug, Default, Serialize)]
pub struct ReviewSectionState {
    /// Durations in microseconds for the latest section request.
    pub timing: std::collections::BTreeMap<&'static str, u64>,
    pub item: Vec<Arc<ReviewSectionItem>>,
    pub next_cursor: Option<String>,
    pub complete: bool,
    pub loading: bool,
    pub diagnostic: Option<String>,
    #[serde(skip)]
    pub(crate) bytes: usize,
    #[serde(skip)]
    pub(super) charge: Option<Arc<SectionCharge>>,
}
```

## ReviewThread

Source: [service/thread.rs](../../../rust/forge/crates/forge-review/src/service/thread.rs).

```rust
#[derive(Clone, Debug, Serialize)]
pub struct ReviewThread {
    pub node_id: String,
    pub path: String,
    pub resolved: bool,
    pub outdated: bool,
    pub side: crate::comments::CommentSide,
    pub line: Option<u64>,
    pub start_line: Option<u64>,
    pub original_line: Option<u64>,
    pub original_start_line: Option<u64>,
    pub can_reply: bool,
    pub can_resolve: bool,
    pub can_unresolve: bool,
    pub comment: Vec<Arc<ReviewThreadComment>>,
    pub next_cursor: Option<String>,
    pub total_comments: usize,
    #[serde(skip)]
    additional_bytes: usize,
}
```

## ReviewThreadDelivery

Source: [service/thread.rs](../../../rust/forge/crates/forge-review/src/service/thread.rs).

```rust
#[derive(Debug, Serialize)]
pub struct ReviewThreadDelivery {
    #[serde(serialize_with = "serialize_thread_summary")]
    pub thread: Arc<ReviewThread>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub patch: Option<forge_buffer::patch::BufferPatch>,
    #[serde(skip)]
    _charge: Arc<super::section::SectionCharge>,
}
```

## ReviewThreadComment

Source: [service/thread.rs](../../../rust/forge/crates/forge-review/src/service/thread.rs).

```rust
#[derive(Debug, Serialize)]
pub struct ReviewThreadComment {
    pub node_id: String,
    pub database_id: u64,
    pub author: Option<String>,
    pub viewer_did_author: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub created_at: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub updated_at: Option<String>,
    pub body: String,
    pub url: String,
    pub commit: Option<String>,
    pub original_commit: Option<String>,
    pub line: Option<u64>,
    pub original_line: Option<u64>,
    pub start_line: Option<u64>,
    pub original_start_line: Option<u64>,
    pub diff_hunk: String,
    pub review_node_id: Option<String>,
    pub review_state: Option<String>,
}
```

## ReviewCommentResult

Source: [service/comment.rs](../../../rust/forge/crates/forge-review/src/service/comment.rs).

```rust
#[derive(Debug, Serialize)]
pub struct ReviewCommentResult {
    pub snapshot: ReviewCommentSnapshot,
    pub remote: Option<CommentResult>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub patch: Option<forge_buffer::patch::BufferPatch>,
}
```

## ReviewCommentCommand

Source: [review/comment.rs](../../../rust/forge/crates/forge-review/src/review/comment.rs).

```rust
#[derive(Debug, Deserialize)]
#[serde(tag = "operation", rename_all = "snake_case", deny_unknown_fields)]
pub enum ReviewCommentCommand {
    Recover {
        comment: CommentId,
        resolution: Option<forge_github::review_mutation::RecoveryResolution>,
    },
    DraftConversation,
    DraftInline {
        anchor: CommentAnchor,
    },
    DraftReply {
        parent: CommentId,
    },
    LoadThreadComment {
        thread_node_id: String,
        comment_node_id: String,
    },
    Load {
        target: Box<CommentTarget>,
        anchor: Option<CommentAnchor>,
    },
    Save {
        comment: CommentId,
        action: CommentSaveAction,
    },
    SaveDraft {
        region: RegionId,
        action: CommentSaveAction,
    },
    Reconcile {
        comment: CommentId,
    },
    Snapshot {
        comment: CommentId,
    },
    Resolve {
        comment: CommentId,
        base: RegionRevision,
        choice: ConflictResolution,
    },
}
```

## ReviewCommentSnapshot

Source: [review/comment.rs](../../../rust/forge/crates/forge-review/src/review/comment.rs).

```rust
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ReviewCommentSnapshot {
    pub document: DocumentId,
    pub comment: CommentId,
    pub region: RegionId,
    pub revision: RegionRevision,
    pub sequence: EditSequence,
    pub text: String,
    pub baseline: String,
    pub conflict: Option<String>,
    pub dirty: bool,
    pub uncertain: bool,
    pub saving: bool,
    pub deleted: bool,
    pub viewer_did_author: bool,
    pub anchor: Option<CommentAnchor>,
    pub reply_to: Option<CommentId>,
}
```

## IssueFieldState

Source: [issue.rs](../../../rust/forge/crates/forge-review/src/issue.rs).

```rust
#[derive(Serialize)]
pub struct IssueFieldState {
    pub region: RegionId,
    pub baseline: String,
    pub revision: RegionRevision,
    pub sequence: forge_buffer::identity::EditSequence,
    pub dirty: bool,
    pub uncertain: bool,
    pub conflict: bool,
}
```

## IssueEffect

Source: [issue/view.rs](../../../rust/forge/crates/forge-review/src/issue/view.rs).

```rust
#[derive(Serialize)]
pub struct IssueEffect {
    pub id: String,
    #[serde(flatten)]
    pub input: DocumentInput,
    pub kind: &'static str,
    pub url: String,
}
```

## IssueOpen

Source: [issue_service.rs](../../../rust/forge/crates/forge-review/src/issue_service.rs).

```rust
const MAX_JOBS: usize = 32;
#[derive(Clone)]
pub struct IssueDocumentService {
    github: GithubService,
    analysis: Arc<forge_diff::workers::AnalysisPool>,
    budget: EditBudget,
    admission: Arc<DocumentAdmissionStore>,
    state: Arc<Mutex<IssueServiceState>>,
    jobs: Arc<Semaphore>,
}
#[derive(Default)]
struct IssueServiceState {
    closed: bool,
    document: HashMap<DocumentId, Arc<IssueOwner>>,
    resource: HashSet<(GithubRepositoryId, u64)>,
    task: JoinSet<()>,
    failure: Vec<String>,
}
struct IssueOwner {
    admission: Arc<DocumentAdmission>,
    document: tokio::sync::Mutex<IssueDocument>,
    remote: Arc<dyn GithubReviewWriteRemote>,
    resource: RecoveryResource,
    store: IssueStore,
    recovery: Mutex<Option<RecoveryRecord>>,
    saving: AtomicBool,
    layout: tokio::sync::Mutex<()>,
    _lease: IssueResourceLease,
}
struct IssueResourceLease {
    state: Weak<Mutex<IssueServiceState>>,
    key: (GithubRepositoryId, u64),
}
impl Drop for IssueResourceLease {
    fn drop(&mut self) {
        if let Some(state) = self.state.upgrade() {
            state
                .lock()
                .expect("issue registry")
                .resource
                .remove(&self.key);
        }
    }
}
struct IssueSaveGuard(Arc<IssueOwner>);
impl Drop for IssueSaveGuard {
    fn drop(&mut self) {
        self.0.saving.store(false, Ordering::Release);
    }
}
#[derive(Serialize)]
pub struct IssueOpen {
    pub fresh_required: bool,
    pub snapshot: BufferSnapshot,
    pub fields: Vec<IssueFieldState>,
    pub recovery: Option<RecoveryRecord>,
}
```

## IssueSave

Source: [issue_service.rs](../../../rust/forge/crates/forge-review/src/issue_service.rs).

```rust
const MAX_JOBS: usize = 32;
#[derive(Clone)]
pub struct IssueDocumentService {
    github: GithubService,
    analysis: Arc<forge_diff::workers::AnalysisPool>,
    budget: EditBudget,
    admission: Arc<DocumentAdmissionStore>,
    state: Arc<Mutex<IssueServiceState>>,
    jobs: Arc<Semaphore>,
}
#[derive(Default)]
struct IssueServiceState {
    closed: bool,
    document: HashMap<DocumentId, Arc<IssueOwner>>,
    resource: HashSet<(GithubRepositoryId, u64)>,
    task: JoinSet<()>,
    failure: Vec<String>,
}
struct IssueOwner {
    admission: Arc<DocumentAdmission>,
    document: tokio::sync::Mutex<IssueDocument>,
    remote: Arc<dyn GithubReviewWriteRemote>,
    resource: RecoveryResource,
    store: IssueStore,
    recovery: Mutex<Option<RecoveryRecord>>,
    saving: AtomicBool,
    layout: tokio::sync::Mutex<()>,
    _lease: IssueResourceLease,
}
struct IssueResourceLease {
    state: Weak<Mutex<IssueServiceState>>,
    key: (GithubRepositoryId, u64),
}
impl Drop for IssueResourceLease {
    fn drop(&mut self) {
        if let Some(state) = self.state.upgrade() {
            state
                .lock()
                .expect("issue registry")
                .resource
                .remove(&self.key);
        }
    }
}
struct IssueSaveGuard(Arc<IssueOwner>);
impl Drop for IssueSaveGuard {
    fn drop(&mut self) {
        self.0.saving.store(false, Ordering::Release);
    }
}
#[derive(Serialize)]
pub struct IssueOpen {
    pub fresh_required: bool,
    pub snapshot: BufferSnapshot,
    pub fields: Vec<IssueFieldState>,
    pub recovery: Option<RecoveryRecord>,
}
#[derive(Serialize)]
pub struct IssueSave {
    pub fresh_required: bool,
    pub fields: Vec<IssueFieldState>,
    pub recovery: Option<RecoveryRecord>,
    pub diagnostic: Option<String>,
}
```

## IssueUpdate

Source: [issue_service.rs](../../../rust/forge/crates/forge-review/src/issue_service.rs).

```rust
#[derive(Serialize)]
pub struct IssueUpdate {
    pub patch: Option<forge_buffer::patch::BufferPatch>,
    pub fields: Vec<IssueFieldState>,
    pub recovery: Option<RecoveryRecord>,
    pub fresh_required: bool,
}
```

## NotificationOpen

Source: [notification.rs](../../../rust/forge/crates/forge-review/src/notification.rs).

```rust
struct NotificationDocument {
    admission: Arc<DocumentAdmission>,
    workspace: PathBuf,
    hostname: String,
    remote: Arc<dyn NotificationRemote>,
    local: Arc<Mutex<NotificationLocal>>,
    buffer: BufferDocument,
    views: DocumentViews,
    input: HashMap<ViewId, InputSequence>,
    record: Vec<NotificationRecord>,
    target: HashMap<TargetId, String>,
    identity: HashMap<String, TargetId>,
    detail: HashMap<String, NotificationDetail>,
    diagnostic: HashMap<String, String>,
    expanded: HashSet<String>,
    cursor: Option<u32>,
    busy: bool,
}
#[derive(Serialize)]
pub struct NotificationOpen {
    pub snapshot: BufferSnapshot,
    pub more: bool,
}
```

## NotificationUpdate

Source: [notification.rs](../../../rust/forge/crates/forge-review/src/notification.rs).

```rust
struct NotificationDocument {
    admission: Arc<DocumentAdmission>,
    workspace: PathBuf,
    hostname: String,
    remote: Arc<dyn NotificationRemote>,
    local: Arc<Mutex<NotificationLocal>>,
    buffer: BufferDocument,
    views: DocumentViews,
    input: HashMap<ViewId, InputSequence>,
    record: Vec<NotificationRecord>,
    target: HashMap<TargetId, String>,
    identity: HashMap<String, TargetId>,
    detail: HashMap<String, NotificationDetail>,
    diagnostic: HashMap<String, String>,
    expanded: HashSet<String>,
    cursor: Option<u32>,
    busy: bool,
}
#[derive(Serialize)]
pub struct NotificationOpen {
    pub snapshot: BufferSnapshot,
    pub more: bool,
}
#[derive(Serialize)]
pub struct NotificationUpdate {
    pub patch: Option<BufferPatch>,
    pub more: bool,
    pub effect: Option<NotificationEffect>,
    #[serde(serialize_with = "serialize_recovery")]
    pub recovery: Option<RecoveryRecord>,
    pub diagnostic: Option<String>,
}
```

## NotificationEffect

Source: [notification.rs](../../../rust/forge/crates/forge-review/src/notification.rs).

```rust
struct NotificationDocument {
    admission: Arc<DocumentAdmission>,
    workspace: PathBuf,
    hostname: String,
    remote: Arc<dyn NotificationRemote>,
    local: Arc<Mutex<NotificationLocal>>,
    buffer: BufferDocument,
    views: DocumentViews,
    input: HashMap<ViewId, InputSequence>,
    record: Vec<NotificationRecord>,
    target: HashMap<TargetId, String>,
    identity: HashMap<String, TargetId>,
    detail: HashMap<String, NotificationDetail>,
    diagnostic: HashMap<String, String>,
    expanded: HashSet<String>,
    cursor: Option<u32>,
    busy: bool,
}
#[derive(Serialize)]
pub struct NotificationOpen {
    pub snapshot: BufferSnapshot,
    pub more: bool,
}
#[derive(Serialize)]
pub struct NotificationUpdate {
    pub patch: Option<BufferPatch>,
    pub more: bool,
    pub effect: Option<NotificationEffect>,
    #[serde(serialize_with = "serialize_recovery")]
    pub recovery: Option<RecoveryRecord>,
    pub diagnostic: Option<String>,
}
#[derive(Serialize)]
pub struct NotificationEffect {
    #[serde(flatten)]
    pub input: DocumentInput,
    pub id: String,
    pub kind: String,
    pub repository: forge_github::model::GithubRepositoryId,
    pub subject_kind: String,
    pub number: Option<u64>,
    pub url: Option<String>,
}
```

