# Native parameter schemas

These declarations specify the field names and types accepted by the native decoders. Method entries and operation schemas link here when several calls share a payload. The declarations below are copied from the current checked implementation, including Serde tags, defaults, and flattened fields.

`Option<T>` permits omitted or null fields. Fields marked `serde(default)` use the default of that field's type. `serde(flatten)` merges another object's fields into this object. `rename_all = "snake_case"` converts enum variant names to snake case. An explicit `serde(rename)` selects that exact wire value. `deny_unknown_fields` rejects additional fields.

Opaque ID types serialize as strings, revision and sequence types as integers, collections as JSON arrays or objects, and `PathBuf` as a path string. Paths are interpreted by the owning local service. Resource identity, freshness, and authorization checks follow decoding and remain part of the method contract.

The [local text-service contract](text.md) defines `text.initialize` and `text.correct`.
Their authoritative Serde definitions are in [forge-text/contract.rs](../../../rust/forge/crates/forge-text/src/contract.rs).
Text ranges use UTF-8 bytes, and correction results use the `decision` tag with
`unchanged` and `replace` variants.

## Document input lifecycle

The client captures a document ID, revision, view ID, sequence, block, and position before dispatch. The host validates that the view and revision still own the target before applying an action. A stale action cannot be retargeted to a different active buffer. `action` must be nonempty and contain no control characters. An optional target narrows the action to the supplied native target identity.

Width profiles describe the current view rather than persisted content. The caller captures current columns, tab width, variable tab widths, ambiguous-character policy, and explicit cell-width overrides. Resizing a view does not truncate its underlying document.

## Diff view controls

Every source diff starts with `diff_view = { "visibility": "all", "presentation": "full" }`.
Visibility accepts `all` or `public`. Presentation accepts `full` or `semantic`.
PlanReview starts Public and supports only Semantic. Its capability rejects a
presentation toggle and its client exposes only the visibility command.

The native document owns both controls across all attached windows. Clients send
`toggle_visibility` or `toggle_presentation` through the owning document input
lifecycle. Timeline uses `session.document` with `operation = "diff_input"` and
an `input: DocumentInput` payload. Status uses its existing native `input`
operation. PR, commit, walkthrough, and saved-source documents use their owning
review or source input operation. Lua maps visibility to `ov` and presentation
to `ox` through the shared diff keymap group.

Control deliveries publish committed `diff_view` together with the prepared
body patches. Recovery snapshots carry the same committed options. Clients retain
their previous usable body during preparation and adopt options only with its
matching body. Superseded requests cannot publish over a newer control request.
Non-diff source documents omit the optional `diff_view` field.

Canonical source coordinates remain authoritative for navigation, comments, and
repository mutations. A semantic heading includes only its selected declaration
and visibility scope, even when a raw hunk also contains excluded private edits.
Saved timeline comparisons use captured sources. Missing or inconsistent captured
evidence produces an explicit raw fallback without reading current worktree bytes.

Source: [forge-diff view state](../../../rust/forge/crates/forge-diff/src/view.rs).

Diff blocks can carry optional `source_anchor` metadata with an opaque `owner`
scope identifying its owner and exact source pair, with nullable `before` and
`after` line ranges. Each range is a zero-based,
half-open `{ "start": number, "end": number }` interval. At least one interval
must be nonempty. A source row represents one line and a semantic heading
represents its declaration's complete extent. The buffer restores a removed
row to the closest source range in the same scope, preferring the narrowest
containing declaration. These anchors affect cursor and selection restoration
and grant no repository or comment action.

## HostInitialize

Decoder source: [nvim/rust/forge/src/router.rs](../../../rust/forge/src/router.rs).

```rust
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct HostInitialize {
    protocol_version: u32,
    recovery_directory: Option<PathBuf>,
    status_ignored_directory: Option<PathBuf>,
    git_config_cache_path: Option<PathBuf>,
}
```

## RevisionRequest

Decoder source: [nvim/rust/forge/src/router.rs](../../../rust/forge/src/router.rs).

```rust
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct RevisionRequest {
    workspace: PathBuf,
    revision: Option<u64>,
    repository: Option<String>,
    reference_digest: Option<String>,
    #[serde(default)]
    offset: usize,
}
```

## IssueRequest

Decoder source: [nvim/rust/forge/src/router.rs](../../../rust/forge/src/router.rs).

```rust
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct IssueRequest {
    database: PathBuf,
    repo: String,
    request: IssueOperation,
}
```

## IssueSyncRequest

Decoder source: [nvim/rust/forge/src/router.rs](../../../rust/forge/src/router.rs).

```rust
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct IssueSyncRequest {
    database: PathBuf,
    directory: PathBuf,
    request: SyncRequest,
    #[serde(default)]
    progress: bool,
}
```

## IssueDetailParams

Decoder source: [nvim/rust/forge/src/router.rs](../../../rust/forge/src/router.rs).

```rust
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct IssueDetailParams {
    database: PathBuf,
    directory: PathBuf,
    request: IssueDetailRequest,
}
```

## MetadataParams

Decoder source: [nvim/rust/forge/src/router.rs](../../../rust/forge/src/router.rs).

```rust
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct MetadataParams {
    cache_directory: PathBuf,
    directory: PathBuf,
    request: MetadataRequest,
}
```

## PullRequestParams

Decoder source: [nvim/rust/forge/src/router.rs](../../../rust/forge/src/router.rs).

```rust
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct PullRequestParams {
    directory: PathBuf,
    request: PullRequestRequest,
}
```

## CommentParams

Decoder source: [nvim/rust/forge/src/router.rs](../../../rust/forge/src/router.rs).

```rust
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct CommentParams {
    directory: PathBuf,
    target: CommentTarget,
    request: CommentOperation,
}
```

## ReviewOpenParams

Decoder source: [nvim/rust/forge/src/router.rs](../../../rust/forge/src/router.rs).

```rust
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct ReviewOpenParams {
    initial: Option<serde_json::Value>,
    directory: PathBuf,
    target: Option<forge_github::pull_request::PullRequestTarget>,
    repository: Option<forge_github::model::GithubRepositoryId>,
    number: Option<u64>,
}
```

## ReviewDocumentParams

Decoder source: [nvim/rust/forge/src/router.rs](../../../rust/forge/src/router.rs).

```rust
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct ReviewDocumentParams {
    document: DocumentId,
    #[serde(default)]
    capture: Vec<RegionEdit>,
    #[serde(default)]
    draft_comment: Vec<forge_review::comments::DraftComment>,
}
```

## ReviewRenderParams

Decoder source: [nvim/rust/forge/src/router.rs](../../../rust/forge/src/router.rs).

```rust
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct ReviewRenderParams {
    document: DocumentId,
    width: forge_buffer::width::WidthProfile,
}
```

## ReviewSectionParams

Decoder source: [nvim/rust/forge/src/router.rs](../../../rust/forge/src/router.rs).

```rust
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct ReviewSectionParams {
    document: DocumentId,
    directory: PathBuf,
    section: forge_review::service::ReviewSectionKind,
    cursor: Option<String>,
}
```

## ReviewLoadParams

Decoder source: [nvim/rust/forge/src/router.rs](../../../rust/forge/src/router.rs).

```rust
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct ReviewLoadParams {
    document: DocumentId,
    directory: PathBuf,
    initial: Option<serde_json::Value>,
}
```

## ReviewFileParams

Decoder source: [nvim/rust/forge/src/router.rs](../../../rust/forge/src/router.rs).

```rust
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct ReviewFileParams {
    document: DocumentId,
    directory: PathBuf,
    path: String,
}
```

## ReviewFilePageParams

Decoder source: [nvim/rust/forge/src/router.rs](../../../rust/forge/src/router.rs).

```rust
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct ReviewFilePageParams {
    document: DocumentId,
    path: String,
}
```

## ReviewThreadParams

Decoder source: [nvim/rust/forge/src/router.rs](../../../rust/forge/src/router.rs).

```rust
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct ReviewThreadParams {
    document: DocumentId,
    directory: PathBuf,
    thread_node_id: String,
    cursor: Option<String>,
}
```

## ReviewViewParams

Decoder source: [nvim/rust/forge/src/router.rs](../../../rust/forge/src/router.rs).

```rust
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct ReviewViewParams {
    document: DocumentId,
    view: forge_buffer::identity::ViewId,
    width: forge_buffer::width::WidthProfile,
}
```

## ReviewInputParams

Decoder source: [nvim/rust/forge/src/router.rs](../../../rust/forge/src/router.rs).

```rust
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct ReviewInputParams {
    input: forge_buffer::input::DocumentInput,
    directory: PathBuf,
}
```

## ReviewTransitionParams

Decoder source: [nvim/rust/forge/src/router.rs](../../../rust/forge/src/router.rs).

```rust
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct ReviewTransitionParams {
    document: DocumentId,
    desired: forge_github::pull_request::DesiredPullRequestState,
}
```

## ReviewLifecycleResolveParams

Decoder source: [nvim/rust/forge/src/router.rs](../../../rust/forge/src/router.rs).

```rust
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct ReviewLifecycleResolveParams {
    document: DocumentId,
    operation_id: String,
    resolution: forge_github::review_mutation::RecoveryResolution,
}
```

## ReviewSetViewedParams

Decoder source: [nvim/rust/forge/src/router.rs](../../../rust/forge/src/router.rs).

```rust
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct ReviewSetViewedParams {
    document: DocumentId,
    path: String,
    viewed: bool,
}
```

## ReviewBatchSubmitParams

Decoder source: [nvim/rust/forge/src/router.rs](../../../rust/forge/src/router.rs).

```rust
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct ReviewBatchSubmitParams {
    document: DocumentId,
    verdict: forge_review::review::ReviewVerdict,
    #[serde(default)]
    capture: Vec<RegionEdit>,
    #[serde(default)]
    draft_comment: Vec<forge_review::comments::DraftComment>,
}
```

## ReviewBatchRecoverParams

Decoder source: [nvim/rust/forge/src/router.rs](../../../rust/forge/src/router.rs).

```rust
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct ReviewBatchRecoverParams {
    document: DocumentId,
    operation_id: String,
    resolution: forge_github::review_mutation::RecoveryResolution,
}
```

## StatusNavigationParams

Decoder source: [nvim/rust/forge/src/router.rs](../../../rust/forge/src/router.rs).

```rust
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct StatusNavigationParams {
    input: forge_status::StatusInput,
    forward: bool,
}
```

## SessionSourceRequest

Decoder source: [nvim/rust/forge/src/router.rs](../../../rust/forge/src/router.rs).

```rust
#[derive(Deserialize)]
#[serde(tag = "operation", rename_all = "snake_case", deny_unknown_fields)]
enum SessionSourceRequest {
    OpenDiff {
        document: DocumentId,
        input: forge_buffer::input::DocumentInput,
    },
}
```

## ReviewCommentParams

Decoder source: [nvim/rust/forge/src/router.rs](../../../rust/forge/src/router.rs).

```rust
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct ReviewCommentParams {
    document: DocumentId,
    command: ReviewCommentCommand,
    #[serde(default)]
    capture: Vec<RegionEdit>,
    #[serde(default)]
    draft_comment: Vec<forge_review::comments::DraftComment>,
}
```

## StatusRequest

Decoder source: [nvim/rust/forge/src/router.rs](../../../rust/forge/src/router.rs).

```rust
#[derive(Deserialize)]
#[serde(tag = "operation", rename_all = "snake_case", deny_unknown_fields)]
enum StatusRequest {
    Local {
        document: DocumentId,
        filename: PathBuf,
    },
    Comparison {
        document: DocumentId,
        workspace: PathBuf,
        reference: String,
        worktree: bool,
        path: Option<String>,
    },
    BodySnapshot {
        document: DocumentId,
        file: u64,
        generation: u64,
    },
    Open {
        document: DocumentId,
        workspace: PathBuf,
    },
    Snapshot {
        document: DocumentId,
    },
    Refresh {
        document: DocumentId,
    },
    Demand {
        input: forge_status::StatusInput,
    },
    OpenTarget {
        input: forge_status::StatusInput,
    },
    Input {
        input: forge_status::StatusInput,
        selection: Option<forge_status::StatusSelection>,
    },
    CloseView {
        document: DocumentId,
        view: forge_buffer::identity::ViewId,
    },
    Close {
        document: DocumentId,
    },
}
```

## SourceRequest

Decoder source: [nvim/rust/forge/src/router.rs](../../../rust/forge/src/router.rs).

```rust
#[derive(Deserialize)]
#[serde(tag = "operation", rename_all = "snake_case", deny_unknown_fields)]
enum SourceRequest {
    Open {
        document: DocumentId,
        workspace: PathBuf,
        revision: String,
        path: String,
    },
    Demand {
        input: forge_buffer::input::DocumentInput,
    },
    Snapshot {
        document: DocumentId,
    },
    CloseView {
        document: DocumentId,
        view: forge_buffer::identity::ViewId,
    },
    Close {
        document: DocumentId,
    },
}
```

## StatusContextRequest

Decoder source: [nvim/rust/forge/src/router.rs](../../../rust/forge/src/router.rs).

```rust
#[derive(Deserialize)]
#[serde(tag = "operation", rename_all = "snake_case", deny_unknown_fields)]
enum StatusContextRequest {
    Action {
        input: forge_status::StatusInput,
    },
    Issues {
        input: forge_status::StatusInput,
        text: String,
    },
    CommitMessage {
        document: DocumentId,
        workspace: PathBuf,
        oid: String,
    },
    AboutMessage {
        document: DocumentId,
        text: String,
    },
}
```

## CreationContextParams

Decoder source: [nvim/rust/forge/src/github_routes.rs](../../../rust/forge/src/github_routes.rs).

```rust
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct CreationContextParams {
    directory: PathBuf,
}
```

## ReviewSubmissionContextParams

Decoder source: [nvim/rust/forge/src/github_routes.rs](../../../rust/forge/src/github_routes.rs).

```rust
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct ReviewSubmissionContextParams {
    directory: PathBuf,
    repository: GithubRepositoryId,
    number: u64,
    pending_review_id: Option<u64>,
    expected: Vec<PendingReviewExpected>,
}
```

## NotificationPageParams

Decoder source: [nvim/rust/forge/src/github_routes.rs](../../../rust/forge/src/github_routes.rs).

```rust
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct NotificationPageParams {
    directory: PathBuf,
    request: forge_github::notification::NotificationReadRequest,
}
```

## ActorParams

Decoder source: [nvim/rust/forge/src/github_routes.rs](../../../rust/forge/src/github_routes.rs).

```rust
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct ActorParams {
    directory: PathBuf,
    repository: GithubRepositoryId,
}
```

## ReviewMutateParams

Decoder source: [nvim/rust/forge/src/github_routes.rs](../../../rust/forge/src/github_routes.rs).

```rust
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct ReviewMutateParams {
    directory: PathBuf,
    request: ReviewMutationRequest,
}
```

## RecoveryInspectParams

Decoder source: [nvim/rust/forge/src/github_routes.rs](../../../rust/forge/src/github_routes.rs).

```rust
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct RecoveryInspectParams {
    resource: RecoveryResource,
}
```

## RecoveryResolveParams

Decoder source: [nvim/rust/forge/src/github_routes.rs](../../../rust/forge/src/github_routes.rs).

```rust
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct RecoveryResolveParams {
    directory: PathBuf,
    resource: RecoveryResource,
    operation_id: String,
    resolution: RecoveryResolution,
}
```

## RecoveryAcknowledgeParams

Decoder source: [nvim/rust/forge/src/github_routes.rs](../../../rust/forge/src/github_routes.rs).

```rust
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct RecoveryAcknowledgeParams {
    resource: RecoveryResource,
    operation_id: String,
}
```

## RecoverySettleDraftParams

Decoder source: [nvim/rust/forge/src/github_routes.rs](../../../rust/forge/src/github_routes.rs).

```rust
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct RecoverySettleDraftParams {
    resource: RecoveryResource,
    operation_id: String,
    draft: Value,
}
```

## ReviewDraftWriteParams

Decoder source: [nvim/rust/forge/src/github_routes.rs](../../../rust/forge/src/github_routes.rs).

```rust
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct ReviewDraftWriteParams {
    resource: RecoveryResource,
    draft: Value,
}
```

## InitializeRequest

Decoder source: [nvim/rust/forge/crates/forge-harness/src/broker/mod.rs](../../../rust/forge/crates/forge-harness/src/broker/mod.rs).

```rust
/// Represents initialization state supplied by the Neovim client.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct InitializeRequest {
    /// Creates or resumes only a tool-free Background conversation when present.
    #[serde(default)]
    pub background: Option<crate::background::BackgroundIdentity>,
    #[serde(default)]
    /// Resolves the dotfiles-owned global Rulesync catalog independently of workspace.
    pub config_dir: Option<String>,
    pub data_root: String,
    #[serde(default)]
    pub permission_file: Option<String>,
    pub workspace: String,
    pub client_id: String,
    pub backend: BackendLaunch,
    #[serde(default)]
    /// Supplies explicit launch descriptors for cross-provider children.
    pub backend_launch_by_provider: HashMap<String, BackendLaunch>,
    #[serde(default = "default_model")]
    pub model: String,
    #[serde(default = "default_effort")]
    pub effort: String,
    pub session_id: Option<String>,
    #[serde(default)]
    pub new_session_name: Option<String>,
    #[serde(default = "default_goal_max_turns")]
    pub goal_max_turns: u32,
    #[serde(default)]
    pub lease_conflict_action: Option<String>,
}
```

## TextGenerationConfig

Decoder source: [nvim/rust/forge/crates/forge-harness/src/service.rs](../../../rust/forge/crates/forge-harness/src/service.rs).

```rust
/// Defines provider settings for one-shot generation without a conversation lease.
#[derive(Clone, Debug, serde::Deserialize)]
#[serde(deny_unknown_fields)]
pub struct TextGenerationConfig {
    pub backend: crate::backend::BackendLaunch,
    pub model: String,
    /// Explicit effort for the isolated request, independent of the active conversation.
    pub reasoning_effort: Option<String>,
    #[serde(default)]
    /// Provider speed for Background requests, independent of the active conversation.
    pub service_tier: crate::backend::ServiceTier,
    /// Background default, overridden by saved per-session policy.
    #[serde(default = "background_compaction")]
    pub auto_compaction: crate::session::AutoCompaction,
    pub data_root: PathBuf,
}
```

## Comparison

Decoder source: [nvim/rust/forge/src/commit_message/service.rs](../../../rust/forge/src/commit_message/service.rs).

```rust
#[derive(Clone, Copy, Deserialize, PartialEq)]
#[serde(rename_all = "snake_case")]
pub enum Comparison {
    Head,
    Staged,
    LatestCommit,
    /// Committed changes from the selected base's merge base to a pinned head.
    PullRequest,
}
```

## GenerationRequest

Decoder source: [nvim/rust/forge/src/commit_message/service.rs](../../../rust/forge/src/commit_message/service.rs).

```rust
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
pub struct GenerationRequest {
    pub operation: String,
    pub workspace: String,
    pub comparison: Comparison,
    /// Base reference required by provider-free pull request context requests.
    pub base: Option<String>,
    /// Head reference pinned by the pull request workflow before context collection.
    pub head: Option<String>,
    /// Context byte allowance after the caller reserves its own instruction text.
    pub max_bytes: Option<usize>,
    #[serde(default)]
    pub ignored_paths: Vec<String>,
    pub generation: Option<TextGenerationConfig>,
    /// Complete PR instructions and evidence for Harness-owned generation.
    pub input: Option<String>,
}
```

## PromptPreview

Decoder source: [nvim/rust/forge/src/commit_message/service.rs](../../../rust/forge/src/commit_message/service.rs).

```rust
#[derive(Clone, Serialize)]
/// Exact model input prepared without selecting or contacting a provider.
pub struct PromptPreview {
    pub system: &'static str,
    pub user: String,
    pub input_bytes: usize,
    pub limit_bytes: usize,
}
```

## GenerationResponse

Decoder source: [nvim/rust/forge/src/commit_message/service.rs](../../../rust/forge/src/commit_message/service.rs).

```rust
#[derive(Clone, Serialize)]
pub struct GenerationResponse {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub session_id: Option<String>,
    pub state: String,
    pub message: Option<String>,
    pub source_requests: usize,
    pub diff_pairs: usize,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub prompt: Option<PromptPreview>,
}
```

## CommandResponse

Decoder source: [nvim/rust/forge/crates/forge-harness/src/task/command.rs](../../../rust/forge/crates/forge-harness/src/task/command.rs).

```rust
#[derive(Clone, Copy, Debug, Default, Deserialize, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum CommandResponse {
    #[default]
    Receipt,
    Snapshot,
}
```

## SessionAction

Decoder source: [nvim/rust/forge/crates/forge-harness/src/task/command.rs](../../../rust/forge/crates/forge-harness/src/task/command.rs).

```rust
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "action")]
pub(crate) enum SessionAction {
    #[serde(rename = "goal")]
    NewGoal { text: String },
    #[serde(rename = "plan")]
    NewPlan { text: String },
    #[serde(rename = "execute")]
    Execute {
        #[serde(flatten)]
        selection: ExecuteSelection,
    },
    #[serde(rename = "attach_plan")]
    Attach { plan_id: String },
    #[serde(rename = "fork")]
    Fork { plan_id: String },
    #[serde(rename = "replan")]
    Replan { plan_id: String, revision: u32 },
    #[serde(rename = "pause")]
    Pause,
    #[serde(rename = "resume")]
    Resume {
        #[serde(flatten)]
        request: ResumeRequest,
    },
    #[serde(rename = "clear")]
    Clear,
    #[serde(rename = "continue")]
    ReserveContinuation {
        #[serde(
            default,
            alias = "completed_turn_id",
            alias = "source",
            skip_serializing_if = "Option::is_none"
        )]
        completed_turn: Option<crate::task::TaskTurnId>,
    },
    #[serde(rename = "begin_acceptance")]
    BeginAcceptance {
        #[serde(flatten)]
        review: PlanReviewRequest,
    },
    #[serde(rename = "cancel_acceptance")]
    CancelAcceptance {
        #[serde(flatten)]
        review: PlanReviewRequest,
    },
    #[serde(rename = "request_changes")]
    RequestChanges {
        #[serde(flatten)]
        review: PlanReviewRequest,
    },
    #[serde(rename = "cancel_plan")]
    CancelPlan {
        #[serde(flatten)]
        review: PlanReviewRequest,
    },
    #[serde(rename = "complete_acceptance")]
    CompleteAcceptance { plan_id: String, digest: String },
}
```

## ExecuteSelection

Decoder source: [nvim/rust/forge/crates/forge-harness/src/task/command.rs](../../../rust/forge/crates/forge-harness/src/task/command.rs).

```rust
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub(crate) struct ExecuteSelection {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub plan_id: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub digest: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub saved_source_digest: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub approval_mode: Option<crate::session::ApprovalMode>,
    #[serde(default, alias = "executor", skip_serializing_if = "Option::is_none")]
    pub plan_executor: Option<crate::session::PlanExecutor>,
    #[serde(default, alias = "compact", skip_serializing_if = "Option::is_none")]
    pub plan_compact: Option<bool>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub comment: Option<String>,
    #[serde(flatten)]
    pub metadata: BTreeMap<String, Value>,
}
```

## PlanReviewRequest

Decoder source: [nvim/rust/forge/crates/forge-harness/src/task/command.rs](../../../rust/forge/crates/forge-harness/src/task/command.rs).

```rust
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub(crate) struct PlanReviewRequest {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub plan_id: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub digest: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub saved_source_digest: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub approval_mode: Option<crate::session::ApprovalMode>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub comment: Option<String>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub annotations: Vec<crate::plan::PlanAnnotationInput>,
    #[serde(flatten)]
    pub metadata: BTreeMap<String, Value>,
}
```

## ResumeRequest

Decoder source: [nvim/rust/forge/crates/forge-harness/src/task/command.rs](../../../rust/forge/crates/forge-harness/src/task/command.rs).

```rust
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub(crate) struct ResumeRequest {
    #[serde(default, alias = "text", skip_serializing_if = "Option::is_none")]
    pub message: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub lifecycle: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub continuation: Option<String>,
    #[serde(flatten)]
    pub metadata: BTreeMap<String, Value>,
}
```

## PermissionChange

Decoder source: [nvim/rust/forge/crates/forge-harness/src/task.rs](../../../rust/forge/crates/forge-harness/src/task.rs).

```rust
#[derive(Clone, Copy, Debug, Deserialize, Serialize, PartialEq, Eq)]
#[serde(tag = "kind", rename_all = "snake_case", deny_unknown_fields)]
/// Captures a policy intent resolved once against authoritative session state.
pub enum PermissionChange {
    /// Toggle workspace and full reads without changing writes or approval.
    ToggleReadScope,
    /// Toggle writes between off and workspace without remembering full scope.
    ToggleWriteEnabled,
    /// Toggle enabled writes between workspace and full, enabling full from off.
    ToggleWriteScope,
    /// Change only the tool approval selection.
    SetApproval { mode: ApprovalMode },
    #[serde(rename = "enable_yolo")]
    /// Toggle full unsandboxed Allow to full-access Ask with supported sandboxing.
    ToggleYolo,
}
```

## PermissionSelection

Decoder source: [nvim/rust/forge/crates/forge-harness/src/task.rs](../../../rust/forge/crates/forge-harness/src/task.rs).

```rust
#[derive(Clone, Copy, Debug, Deserialize, Serialize, PartialEq, Eq)]
#[serde(deny_unknown_fields)]
/// Retains the cumulative policy selected by admitted permission intents.
pub struct PermissionSelection {
    /// Selected provider sandbox enablement.
    pub sandbox: bool,
    /// Selected read boundary, independent of directory grants.
    pub read_access: DirectoryScope,
    /// Selected write boundary, independent of directory grants.
    pub write_access: WriteScope,
    /// Selected approval behavior.
    pub approval_mode: ApprovalMode,
}
```

## AccessPolicy

Decoder source: [nvim/rust/forge/crates/forge-harness/src/session/access.rs](../../../rust/forge/crates/forge-harness/src/session/access.rs).

```rust
/// Controls file authorization and provider isolation independently of approval mode.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct AccessPolicy {
    pub sandbox: bool,
    #[serde(default)]
    pub read_access: DirectoryScope,
    pub write_access: WriteScope,
    #[serde(default)]
    pub readable_directory: Vec<String>,
    pub writable_directory: Vec<String>,
    pub windows_sandbox: WindowsSandbox,
}
```

## DirectoryScope

Decoder source: [nvim/rust/forge/crates/forge-harness/src/session/access.rs](../../../rust/forge/crates/forge-harness/src/session/access.rs).

```rust
#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum DirectoryScope {
    Workspace,
    #[default]
    Full,
}
```

## WriteScope

Decoder source: [nvim/rust/forge/crates/forge-harness/src/session/access.rs](../../../rust/forge/crates/forge-harness/src/session/access.rs).

```rust
/// Selects write authorization independently of read scope and approval handling.
#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum WriteScope {
    /// Reject reported writes, including writes covered by saved grants.
    Off,
    /// Authorize workspace writes and configured directory grants.
    #[default]
    Workspace,
    /// Authorize writes across the filesystem.
    Full,
}
```

## AutoCompaction

Decoder source: [nvim/rust/forge/crates/forge-harness/src/session/auto_compaction.rs](../../../rust/forge/crates/forge-harness/src/session/auto_compaction.rs).

```rust
#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
#[serde(deny_unknown_fields)]
/// Selects a provider-reported token threshold without changing model capacity.
pub struct AutoCompaction {
    /// Enables Forge-requested compaction at a completed turn boundary.
    pub enabled: bool,
    /// Minimum retained context tokens at which compaction becomes eligible.
    pub threshold_tokens: u64,
}
```

## SettingScope

Decoder source: [nvim/rust/forge/crates/forge-harness/src/settings/store.rs](../../../rust/forge/crates/forge-harness/src/settings/store.rs).

```rust
#[derive(Clone, Copy, Debug, Deserialize, Serialize)]
#[serde(rename_all = "snake_case")]
/// Selects preference inheritance without coupling preferences to conversation formats.
pub enum SettingScope {
    /// Applies to every workspace and provider unless overridden.
    Global,
    /// Overrides global values for one canonical workspace.
    Workspace,
    /// Overrides inherited values for one Harness conversation.
    Session,
}
```

## PortableSettings

Decoder source: [nvim/rust/forge/crates/forge-harness/src/settings/store.rs](../../../rust/forge/crates/forge-harness/src/settings/store.rs).

```rust
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
/// Retains portable choices without copying provider authentication or native files.
pub struct PortableSettings {
    /// Requests native compaction after completed turns reach this token threshold.
    pub auto_compaction: Option<crate::session::AutoCompaction>,
    /// Controls reported file authorization and requested OS isolation independently of approval mode.
    pub access: Option<crate::session::AccessPolicy>,
    /// Selects the check shell used by Harness execution.
    pub shell: Option<crate::session::CheckShell>,
    /// Automatic validation boundary for the editor's work queue.
    pub check_timing: Option<crate::checks::CheckTiming>,
    /// Enables automatic completion checks, defaulting to disabled when omitted.
    pub automatic_checks: Option<bool>,
    /// Allows the root agent to edit project checks in this conversation.
    pub check_editing: Option<bool>,
    /// Selects approval handling for ordinary work independently of filesystem access.
    pub default_approval: Option<crate::session::ApprovalMode>,
    /// Selects approval handling when planning.
    pub plan_approval: Option<crate::session::ApprovalMode>,
    /// Selects compaction before accepted plan execution.
    pub plan_compact: Option<bool>,
    /// Selects automatic review of execution plan revisions.
    pub plan_auto_approve_revisions: Option<bool>,
    /// Overrides the duration of future approval requests.
    pub approval_timeout: Option<crate::session::InputTimeout>,
    /// Overrides the duration of future question sets.
    pub answer_timeout: Option<crate::session::InputTimeout>,
    /// Enables the root's model-facing child tools without restricting manual admission.
    pub subagent_tools: Option<bool>,
    /// Enables read-only tools backed by the connected editor language servers.
    pub lsp_tools: Option<bool>,
    /// Exposes read-only session history tools independently of configuration inspection.
    pub session_tools: Option<bool>,
    /// Shows the latest thought beneath live status without changing timeline messages.
    pub status_thoughts: Option<bool>,
    #[serde(default)]
    /// Records selection by source identity, independent of provider display names.
    pub skill: BTreeMap<String, bool>,
    #[serde(default)]
    /// Records server selection by catalog identity.
    pub mcp: BTreeMap<String, bool>,
}
```

## DocumentInput

Decoder source: [nvim/rust/forge/crates/forge-buffer/src/input.rs](../../../rust/forge/crates/forge-buffer/src/input.rs).

```rust
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct DocumentInput {
    pub document: DocumentId,
    pub revision: DocumentRevision,
    pub view: ViewId,
    pub sequence: InputSequence,
    pub action: String,
    pub block: BlockId,
    pub position: TextPosition,
    pub target: Option<TargetId>,
}
```

## AmbiguousWidth

Decoder source: [nvim/rust/forge/crates/forge-buffer/src/width.rs](../../../rust/forge/crates/forge-buffer/src/width.rs).

```rust
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum AmbiguousWidth {
    #[default]
    Single,
    Double,
}
```

## CellWidthRange

Decoder source: [nvim/rust/forge/crates/forge-buffer/src/width.rs](../../../rust/forge/crates/forge-buffer/src/width.rs).

```rust
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct CellWidthRange {
    pub first: u32,
    pub last: u32,
    pub cells: usize,
}
```

## WidthProfile

Decoder source: [nvim/rust/forge/crates/forge-buffer/src/width.rs](../../../rust/forge/crates/forge-buffer/src/width.rs).

```rust
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct WidthProfile {
    pub columns: usize,
    pub tabstop: usize,
    #[serde(default)]
    pub variable_tabstop: Vec<usize>,
    #[serde(default)]
    pub ambiguous: AmbiguousWidth,
    #[serde(default)]
    pub cell_width: Vec<CellWidthRange>,
}
```

