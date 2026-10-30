# Session runtime schemas

These declarations define session-state results and input queue payloads. Types are copied from their owning source, including serialization attributes. `Box<T>` has the same wire representation as `T`. `Vec<T>` is an array and maps are objects. Transparent identity types retain their underlying representation: task revisions are strings, while document, queue, and snapshot revision counters are integers. Fields marked `serde(skip)` never appear on the wire.

Optional fields serialize as JSON null unless an explicit skip rule says otherwise. The Lua decoder removes null object members and retains null array entries as `vim.NIL`. A top-level null result reaches the callback as nil with no error. The examples in the method reference retain the original wire nulls.

Session and snapshot revisions belong to one `runtime_epoch`. An asynchronous callback must still own the captured session, host generation, and epoch before applying these values. A snapshot does not transfer ownership of an interactive session. Exchange and timeline content arrive through their document or exchange APIs, rather than the skipped native fields below.

`SessionPurpose` defaults to `conversation` for older records. Internal Plan Review answer forks use `plan_review_answer`, remain available through explicit session access, and do not compete with user conversations during automatic startup selection. This purpose is selected by the native fork caller, not by the public request payload.

## InitializeRequest

Source: [broker/mod.rs](../../../rust/forge/crates/forge-harness/src/broker/mod.rs).

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

## BrokerSnapshot

Source: [broker/mod.rs](../../../rust/forge/crates/forge-harness/src/broker/mod.rs).

```rust
/// Represents the complete client-visible state for one active session.
#[derive(Clone, Debug, Serialize)]
pub struct BrokerSnapshot {
    pub ask_model: crate::settings::AskModel,
    /// Confirmed access label computed by the authoritative filesystem policy.
    pub access_abbreviation: String,
    pub subagent_tools: bool,
    pub lsp_tools: bool,
    pub session_tools: bool,
    pub status_thoughts: bool,
    pub check_editing: bool,
    pub runtime_epoch: String,
    pub snapshot_revision: u64,
    pub checks_running: bool,
    pub check_timing: crate::checks::CheckTiming,
    /// Effective completion-check enablement, independent of manual and plan checks.
    pub automatic_checks: bool,
    pub checks_deferred: bool,
    pub active_check_run: Option<String>,
    pub task_operation: Option<Value>,
    pub session: HarnessSession,
    #[serde(skip)]
    pub exchange: Vec<Exchange>,
    pub capability: BackendCapability,
    pub no_checkpoint: bool,
    pub restore_recovery: Option<Value>,
    pub active_plan: Option<PlanRecord>,
    pub active_elicitation: Option<ActiveElicitation>,
    pub artifact: Vec<ArtifactSummary>,
    #[serde(skip)]
    pub timeline: Vec<TimelineEntry>,
    pub timeline_revision: u64,
    pub status: crate::session::state_machine::SessionPhase,
    pub active_wait: Option<ActiveWait>,
    pub prompt_history: Vec<crate::attachment::PromptDraft>,
    pub agent: AgentSnapshot,
    pub approval: Vec<ApprovalRequestView>,
}
```

## AgentSnapshot

Source: [broker/mod.rs](../../../rust/forge/crates/forge-harness/src/broker/mod.rs).

```rust
/// Retains reusable child identities and their durable timelines inside one root.
#[derive(Clone, Debug, Serialize)]
pub struct AgentSnapshot {
    pub run: Vec<Agent>,
    pub summary: std::collections::HashMap<String, Value>,
    #[serde(skip)]
    pub exchange: Vec<Exchange>,
}
```

## SessionPreview

Source: [broker/mod.rs](../../../rust/forge/crates/forge-harness/src/broker/mod.rs).

```rust
/// Represents a stored session timeline projected without acquiring its lease.
#[derive(Clone, Debug, Serialize)]
pub struct SessionPreview {
    pub lines: Vec<String>,
    pub session: HarnessSession,
    #[serde(skip)]
    pub exchange: Vec<Exchange>,
    #[serde(skip)]
    pub timeline: Vec<TimelineEntry>,
    pub agent: AgentSnapshot,
}
```

## ActiveElicitation

Source: [broker/mod.rs](../../../rust/forge/crates/forge-harness/src/broker/mod.rs).

```rust
/// Represents the durable owner and question state presented by the Harness question UI.
#[derive(Clone, Debug, Serialize)]
pub struct ActiveElicitation {
    pub owner: ElicitationOwner,
    pub plan_id: Option<String>,
    pub exchange_id: Option<String>,
    pub elicitation: PlanElicitation,
}
```

## PlanChoice

Source: [broker/observation.rs](../../../rust/forge/crates/forge-harness/src/broker/observation.rs).

```rust
#[derive(Serialize)]
struct PlanChoice {
    id: String,
    created_at_ms: i64,
    title: String,
    session_name: String,
    revision_count: u32,
    implemented: bool,
    state: PlanState,
    digest: Option<String>,
}
```

## ActiveWait

Source: [exchange/node.rs](../../../rust/forge/crates/forge-harness/src/exchange/node.rs).

```rust
/// Tracks transient waiting chrome without adding it to durable history.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ActiveWait {
    pub exchange_id: String,
    pub started_at_ms: i64,
    pub agent_count: usize,
}
```

## ExchangeComment

Source: [exchange/record.rs](../../../rust/forge/crates/forge-harness/src/exchange/record.rs).

```rust
/// Represents one review comment anchored to an interaction diff.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ExchangeComment {
    pub id: String,
    pub exchange_id: String,
    pub file_path: String,
    pub old_line: Option<u32>,
    pub new_line: Option<u32>,
    pub body: String,
    pub created_at_ms: i64,
}
```

## ActiveTask

Source: [task/active.rs](../../../rust/forge/crates/forge-harness/src/task/active.rs).

```rust
/// Carries the only selected workflow in a session. Transcript entries do not own this state.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ActiveTask {
    pub revision: TaskRevision,
    pub created_at_ms: i64,
    pub updated_at_ms: i64,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub entry: Option<TaskEntry>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub turn: Option<TaskTurn>,
    #[serde(flatten)]
    pub workflow: TaskWorkflow,
}
```

## HarnessSession

Source: [session/mod.rs](../../../rust/forge/crates/forge-harness/src/session/mod.rs).

```rust
/// Represents one durable conversation and its backend identity.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct HarnessSession {
    /// Controls automatic resumption while retaining explicit access to every session.
    #[serde(default)]
    pub purpose: SessionPurpose,
    /// Distinguishes tool-free Background conversations from interactive sessions.
    #[serde(default)]
    pub background: Option<crate::background::BackgroundIdentity>,
    /// Controls explicit compaction using reported context usage.
    #[serde(default)]
    pub auto_compaction: AutoCompaction,
    /// Prevents retry loops when compaction fails or leaves usage above the threshold.
    #[serde(default)]
    pub last_compaction_turn: Option<String>,
    pub id: String,
    /// Stable owner of the session's primary timeline and exchanges.
    pub primary_agent_id: String,
    pub name: String,
    pub workspace: String,
    pub backend: String,
    pub backend_session_id: Option<String>,
    pub provider_checkpoint_id: Option<String>,
    pub provider_fork_state: ProviderForkState,
    pub model: String,
    #[serde(default)]
    pub provider_label: String,
    #[serde(default)]
    pub resolved_model: Option<String>,
    pub effort: String,
    #[serde(default)]
    pub plan_executor: PlanExecutor,
    #[serde(default)]
    pub plan_compact: bool,
    #[serde(default = "auto_approve_revisions")]
    pub plan_auto_approve_revisions: bool,
    #[serde(default)]
    /// Limits each pending approval independently.
    pub approval_timeout: InputTimeout,
    #[serde(default)]
    /// Limits one complete question set.
    pub answer_timeout: InputTimeout,
    #[serde(default)]
    pub context_window: Option<String>,
    #[serde(default)]
    pub service_tier: crate::backend::ServiceTier,
    #[serde(default)]
    pub access: crate::session::AccessPolicy,
    #[serde(default)]
    pub shell: CheckShell,
    pub approval_mode: ApprovalMode,
    #[serde(default)]
    pub active_task: Option<crate::task::ActiveTask>,
    #[serde(default)]
    pub default_approval: ApprovalMode,
    #[serde(default)]
    pub plan_approval: Option<ApprovalMode>,
    pub created_at_ms: i64,
    pub updated_at_ms: i64,
    pub lease_owner: Option<String>,
    #[serde(default)]
    pub native_fork: bool,
    #[serde(default)]
    pub native_compact: bool,
    #[serde(default)]
    pub context_usage: Option<ContextUsage>,
}
```

## SessionPurpose

Source: [session/mod.rs](../../../rust/forge/crates/forge-harness/src/session/mod.rs).

```rust
/// Distinguishes resumable user conversations from internal answer forks.
#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum SessionPurpose {
    #[default]
    Conversation,
    PlanReviewAnswer,
}
```

## PlanExecutor

Source: [session/mod.rs](../../../rust/forge/crates/forge-harness/src/session/mod.rs).

```rust
/// Selects the model used after a plan is accepted.
#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub struct PlanExecutor {
    pub enabled: bool,
    pub model: Option<String>,
    pub effort: Option<String>,
}
```

## ModelSetting

Source: [session/mod.rs](../../../rust/forge/crates/forge-harness/src/session/mod.rs).

```rust
/// Stores durable controls selected for one provider model.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct ModelSetting {
    pub reasoning: Option<String>,
    pub context_window: Option<String>,
}
```

## SessionPhase

Source: [session/state_machine.rs](../../../rust/forge/crates/forge-harness/src/session/state_machine.rs).

```rust
/// Represents the authoritative workflow phase exposed to one session timeline.
#[derive(Clone, Debug, Default, Eq, PartialEq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum SessionPhase {
    #[default]
    Idle,
    Paused,
    Compacting {
        started_at_ms: i64,
    },
    Finalizing {
        exchange_id: String,
        outcome: Option<crate::exchange::ExchangeOutcome>,
        error: Option<String>,
    },
    Working {
        /// Accumulated execution intervals, excluding user waits.
        duration_ms: u64,
        /// Current execution interval, absent while processing a settled provider result.
        started_at_ms: Option<i64>,
        activity: WorkflowActivity,
        reasoning_summary: Option<String>,
        execution: Option<ExecutionStatus>,
    },
    AwaitingInput {
        owner: ElicitationOwner,
        plan_id: Option<String>,
        exchange_id: Option<String>,
    },
    AwaitingPlanReview {
        plan_id: String,
        revision: u32,
    },
    AwaitingApproval {
        /// Exchange whose permission request owns the wait.
        exchange_id: String,
    },
    RetryingPlanGeneration {
        plan_id: String,
        turn: u32,
        max_turn: u32,
        /// Accumulated execution intervals retained across planning continuations.
        duration_ms: u64,
        /// Current execution interval, excluding approval and question waits.
        started_at_ms: Option<i64>,
        activity: WorkflowActivity,
    },
    RetryingProvider {
        retry: crate::backend::retry::ProviderRetry,
        activity: WorkflowActivity,
    },
    PlanningFailed {
        plan_id: String,
        turn_count: u32,
        reason: String,
    },
    WaitingForAgent {
        agent_count: usize,
    },
}
```

## InputTimeout

Source: [session/timeout.rs](../../../rust/forge/crates/forge-harness/src/session/timeout.rs).

```rust
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
/// Configures one input deadline. Zero disables expiry.
pub struct InputTimeout {
    /// Holds a nonnegative whole count, with zero disabling expiry.
    pub amount: u32,
    /// Preserves the user's selected display unit.
    pub unit: TimeoutUnit,
}
```

## TimeoutUnit

Source: [session/timeout.rs](../../../rust/forge/crates/forge-harness/src/session/timeout.rs).

```rust
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "lowercase")]
/// Selects the display unit without changing the configured amount.
pub enum TimeoutUnit {
    /// Measures seconds.
    S,
    /// Measures minutes.
    M,
    /// Measures hours.
    H,
}
```

## AskModel

Source: [settings/ask_model.rs](../../../rust/forge/crates/forge-harness/src/settings/ask_model.rs).

```rust
/// Overrides the planning model only for forked Plan Review answers.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct AskModel {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub model: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub effort: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub service_tier: Option<ServiceTier>,
}
```

## InputIntent

Source: [input.rs](../../../rust/forge/crates/forge-harness/src/input.rs).

```rust
#[derive(Clone, Debug, Default, Deserialize, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum InputIntent {
    #[default]
    Auto,
    Queue,
}
```

## InputRoute

Source: [input.rs](../../../rust/forge/crates/forge-harness/src/input.rs).

```rust
/// Selects whether admission starts a queued turn or adds input to the active turn.
#[derive(Clone, Copy, Debug, Deserialize, Serialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub enum InputRoute {
    Turn,
    Steering,
}
```

## InputDelivery

Source: [input.rs](../../../rust/forge/crates/forge-harness/src/input.rs).

```rust
#[derive(Clone, Debug, Default, Deserialize, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum InputDelivery {
    #[default]
    Queued,
    Delivering,
    Accepted,
    Unknown,
    Failed,
}
```

## InputPurpose

Source: [input.rs](../../../rust/forge/crates/forge-harness/src/input.rs).

```rust
/// Keeps user input independent of task selection and fences generated task input by revision.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum InputPurpose {
    #[default]
    User,
    TaskEntry {
        revision: crate::task::TaskRevision,
        id: crate::task::TaskInputId,
        start: crate::exchange::TaskStart,
    },
    TaskContinuation {
        revision: crate::task::TaskRevision,
        id: crate::task::TaskInputId,
        completed_turn_id: crate::task::TaskTurnId,
        reason: String,
    },
}
```

## InputMessage

Source: [input.rs](../../../rust/forge/crates/forge-harness/src/input.rs).

```rust
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct InputMessage {
    pub id: String,
    #[serde(default)]
    pub revision: u64,
    pub text: String,
    #[serde(default)]
    pub purpose: InputPurpose,
    #[serde(default)]
    pub attachments: Vec<Value>,
    pub config: Option<Value>,
    #[serde(default)]
    pub intent: InputIntent,
    #[serde(default)]
    pub delivery: InputDelivery,
    #[serde(default)]
    pub route: Option<InputRoute>,
    pub reservation: Option<String>,
    pub submission_id: Option<String>,
    pub error: Option<String>,
}
```

## InputEdit

Source: [input.rs](../../../rust/forge/crates/forge-harness/src/input.rs).

```rust
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct InputEdit {
    pub id: String,
    pub revision: u64,
    pub text: String,
    #[serde(default)]
    pub attachments: Vec<Value>,
    pub config: Option<Value>,
}
```

## InputAction

Source: [input.rs](../../../rust/forge/crates/forge-harness/src/input.rs).

```rust
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum InputAction {
    Retry {
        id: String,
        revision: u64,
    },
    Resume,
    Submit {
        message: Box<InputMessage>,
    },
    Reserve {
        id: String,
        revision: u64,
        reservation: String,
    },
    Commit {
        reservation: String,
        edits: Vec<InputEdit>,
    },
    Release {
        reservation: String,
    },
    Remove {
        id: String,
        revision: u64,
    },
}
```

## InputOperation

Source: [input.rs](../../../rust/forge/crates/forge-harness/src/input.rs).

```rust
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct InputOperation {
    pub id: String,
    #[serde(default)]
    pub control_revision: u64,
    pub after_control: Option<String>,
    pub action: InputAction,
}
```

## InputUpdate

Source: [input.rs](../../../rust/forge/crates/forge-harness/src/input.rs).

```rust
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct InputUpdate {
    pub snapshot: Option<InputSnapshot>,
    pub operation_id: Option<String>,
    pub revision: u64,
    pub upsert: Vec<InputMessage>,
    pub removed: Vec<String>,
    pub suspended: bool,
    pub control_revision: u64,
    pub error: Option<String>,
    pub prepend: bool,
    pub retracted: Option<String>,
}
```

## InputSnapshot

Source: [input.rs](../../../rust/forge/crates/forge-harness/src/input.rs).

```rust
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct InputSnapshot {
    pub revision: u64,
    pub messages: Vec<InputMessage>,
    pub suspended: bool,
    pub control_revision: u64,
    pub control_operation_id: Option<String>,
}
```

## PromptDraft

Source: [attachment.rs](../../../rust/forge/crates/forge-harness/src/attachment.rs).

```rust
#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
/// Complete editable input with attachment ownership separate from literal text.
pub struct PromptDraft {
    /// Literal text, including visible attachment labels.
    pub text: String,
    /// Explicit file selections and their editor spans.
    #[serde(default)]
    pub attachments: Vec<AttachmentSelection>,
}
```

## Attachment

Source: [attachment.rs](../../../rust/forge/crates/forge-harness/src/attachment.rs).

```rust
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
/// Identifies a retained file independently of its source path and display label.
pub(crate) struct Attachment {
    object_id: String,
    name: String,
    media_type: String,
}
```

## AttachmentSelection

Source: [attachment.rs](../../../rust/forge/crates/forge-harness/src/attachment.rs).

```rust
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
/// A composer-owned attachment and its editable label span in UTF-8 bytes.
pub struct AttachmentSelection {
    /// Retained-file metadata identity.
    pub id: String,
    /// Visible reference, independent of the retained filename.
    pub label: String,
    /// Original filename from the import response.
    pub name: String,
    /// Detected content type from the import response.
    pub media_type: String,
    /// Inclusive byte offset of the label in the composer text.
    pub start: usize,
    /// Exclusive byte offset of the label in the composer text.
    pub end: usize,
}
```

## BackendLaunch

Source: [backend/contract.rs](../../../rust/forge/crates/forge-harness/src/backend/contract.rs).

```rust
/// Represents one executable backend launch descriptor.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct BackendLaunch {
    pub kind: String,
    pub command: Vec<String>,
}
```

## BackendCapability

Source: [backend/contract.rs](../../../rust/forge/crates/forge-harness/src/backend/contract.rs).

```rust
/// Represents backend features that control visible Harness actions.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct BackendCapability {
    #[serde(default)]
    /// Enables OS isolation controls only for providers on the sandbox allowlist.
    pub os_sandbox: bool,
    pub native_fork: bool,
    /// Supports an explicit provider compaction request.
    pub native_compact: bool,
    #[serde(default)]
    /// Describes observable compaction evidence independently of manual request support.
    pub compaction: CompactionCapability,
    pub native_steer: bool,
    pub native_turn_rollback: bool,
    pub native_goal: bool,
    pub model_selection: bool,
    pub effort_selection: bool,
    pub fast_mode: bool,
    /// Enables ultrafast tier selection for this provider.
    pub ultrafast_mode: bool,
    pub permission_control: bool,
    #[serde(default)]
    /// Exposes a provider-owned terminal inventory without running a model prompt.
    pub background_terminals: bool,
    #[serde(default)]
    pub approval_mode_list: Vec<ApprovalMode>,
    pub agent: AgentCapability,
    #[serde(default)]
    pub catalog: CatalogCapability,
}
```

## BackendContextWindow

Source: [backend/contract.rs](../../../rust/forge/crates/forge-harness/src/backend/contract.rs).

```rust
/// Represents one selectable context-window tier advertised by a provider.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct BackendContextWindow {
    pub id: String,
    pub token_limit: Option<u64>,
}
```

## BackendModel

Source: [backend/contract.rs](../../../rust/forge/crates/forge-harness/src/backend/contract.rs).

```rust
/// Represents one model and the controls advertised by its provider.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct BackendModel {
    pub id: String,
    #[serde(default)]
    /// Service tiers explicitly advertised for this model.
    pub service_tiers: Vec<ServiceTier>,
    pub reasoning: Vec<String>,
    pub default_reasoning: Option<String>,
    pub selected_reasoning: Option<String>,
    pub context_window: Vec<BackendContextWindow>,
    pub default_context_window: Option<String>,
    pub selected_context_window: Option<String>,
    pub vision: bool,
    pub description: Option<String>,
    #[serde(default)]
    pub is_default: bool,
}
```

## Support

Source: [backend/capability.rs](../../../rust/forge/crates/forge-harness/src/backend/capability.rs).

```rust
#[derive(Clone, Debug, Serialize)]
#[serde(rename_all = "snake_case")]
/// Distinguishes supported behavior from provider limitations.
pub enum Support {
    /// The operation preserves its full Harness contract.
    Supported,
    /// The operation works with the reported restriction.
    Limited,
    /// The adapter rejects the operation before dispatch.
    Unsupported,
}
```

## Capability

Source: [backend/capability.rs](../../../rust/forge/crates/forge-harness/src/backend/capability.rs).

```rust
#[derive(Clone, Debug, Serialize)]
/// Describes one user-visible operation and its application boundary.
pub struct Capability {
    /// Identifies the operation across providers and menu refreshes.
    pub id: &'static str,
    /// Names the menu group.
    pub group: &'static str,
    /// Names the operation in the menu.
    pub label: &'static str,
    /// Records whether every provider must implement this operation.
    pub required: bool,
    /// Identifies Forge or the provider as the implementation owner.
    pub implementation: &'static str,
    /// Reports support independently of discovery availability.
    pub support: Support,
    /// Describes the configuration application boundary.
    pub application: &'static str,
    /// Explains the observable restriction or guarantee.
    pub reason: &'static str,
}
```

## CapabilityManifest

Source: [backend/capability.rs](../../../rust/forge/crates/forge-harness/src/backend/capability.rs).

```rust
#[derive(Clone, Debug, Serialize)]
/// Captures one coherent provider capability revision for presentation and dispatch.
pub struct CapabilityManifest {
    /// Identifies the selected backend.
    pub backend: BackendKind,
    /// Identifies the provider in the menu.
    pub label: String,
    /// Changes when the capability schema changes.
    pub revision: u64,
    /// Retains the negotiated agent version when the transport exposes it.
    pub provider_version: Option<String>,
    /// Distinguishes startup discovery from ready and blocked runtime state.
    pub availability: &'static str,
    /// Retains a bounded runtime explanation independent of feature support.
    pub availability_reason: Option<String>,
    /// Reports unavailable default resources without exposing credential values.
    pub notice: Vec<String>,
    /// Contains supported and unsupported operations.
    pub capability: Vec<Capability>,
}
```

## SkillDefinition

Source: [backend/catalog.rs](../../../rust/forge/crates/forge-harness/src/backend/catalog.rs).

```rust
/// Describes one provider skill exposed to Harness callers.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct SkillDefinition {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    /// Identifies the shared definition when the skill has an explicit source binding.
    pub resource_id: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    /// Records the definition's scope when Forge owns its source.
    pub scope: Option<ResourceScope>,
    pub name: String,
    pub description: String,
    pub enabled: bool,
    pub user_invocable: bool,
    pub path: Option<String>,
    pub source: Option<String>,
    pub argument_hint: Option<String>,
}
```

## McpStatus

Source: [backend/catalog.rs](../../../rust/forge/crates/forge-harness/src/backend/catalog.rs).

```rust
/// Defines the normalized lifecycle state shown by the MCP picker.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum McpStatus {
    Disabled,
    Loading,
    Connected,
    NeedsAuthentication,
    Failed,
    Unavailable,
}
```

## McpToolDefinition

Source: [backend/catalog.rs](../../../rust/forge/crates/forge-harness/src/backend/catalog.rs).

```rust
/// Describes one cached tool nested under its owning MCP server.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct McpToolDefinition {
    pub name: String,
    pub description: Option<String>,
}
```

## McpDefinition

Source: [backend/catalog.rs](../../../rust/forge/crates/forge-harness/src/backend/catalog.rs).

```rust
/// Describes one complete provider MCP row and its cached tool inventory.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct McpDefinition {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    /// Identifies the shared definition independently of its provider-visible name.
    pub resource_id: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    /// Records the definition's scope when Forge owns its source.
    pub scope: Option<ResourceScope>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    /// Identifies the source configuration without exposing launch credentials.
    pub source_path: Option<String>,
    pub name: String,
    pub transport: String,
    pub enabled: bool,
    pub status: McpStatus,
    pub status_detail: Option<String>,
    pub token_count: Option<u64>,
    pub token_estimated: bool,
    #[serde(default)]
    pub tools: Vec<McpToolDefinition>,
    pub tool_error: Option<String>,
}
```

## ApprovalRequestView

Source: [backend/approval.rs](../../../rust/forge/crates/forge-harness/src/backend/approval.rs).

```rust
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ApprovalRequestView {
    pub id: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub execution_owner: Option<crate::backend::ExecutionOwner>,
    pub exchange_id: Option<String>,
    pub turn_id: Option<String>,
    pub provider: String,
    pub reason: Option<String>,
    pub item_list: Vec<ApprovalItem>,
    /// Captures the original deadline independently of the active window.
    #[serde(default)]
    pub deadline: Option<InputDeadline>,
    /// Distinguishes automatic rejection from an explicit user decision.
    #[serde(default)]
    pub resolution_source: ResolutionSource,
}
```

## ApprovalAnswer

Source: [backend/approval.rs](../../../rust/forge/crates/forge-harness/src/backend/approval.rs).

```rust
#[derive(Clone, Debug, Deserialize, Serialize)]
/// An explicit choice for one item in a pending approval request.
pub struct ApprovalAnswer {
    pub item_id: String,
    pub choice_id: String,
}
```

## ApprovalChoice

Source: [backend/approval.rs](../../../rust/forge/crates/forge-harness/src/backend/approval.rs).

```rust
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ApprovalChoice {
    pub id: String,
    pub label: String,
    pub resolution: ApprovalResolution,
    #[serde(default)]
    pub rule_list: Vec<(String, String)>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub directory: Option<DirectoryGrant>,
}
```

## ApprovalItem

Source: [backend/approval.rs](../../../rust/forge/crates/forge-harness/src/backend/approval.rs).

```rust
#[derive(Clone, Debug, Deserialize, Serialize)]
/// One unapproved target with choices scoped to that target.
pub struct ApprovalItem {
    pub id: String,
    pub title: String,
    pub detail: String,
    /// Full source commands containing this target, with all matching executable occurrences.
    pub command_list: Vec<ApprovalCommand>,
    pub choice_list: Vec<ApprovalChoice>,
}
```

## ApprovalResolution

Source: [backend/approval.rs](../../../rust/forge/crates/forge-harness/src/backend/approval.rs).

```rust
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum ApprovalResolution {
    AllowOnce,
    AllowExact,
    AllowBroad,
    DenyOnce,
    DenyExact,
    DenyBroad,
    /// Automatic denial by saved permissions or filesystem access policy.
    DenyPolicy,
    /// Automatic denial because approval prompts are disabled.
    DenyMode,
    /// Rejects an expired request without recording a persistent rule.
    TimedOut,
    Cancel,
}
```

## CheckRunRequest

Source: [checks/selection.rs](../../../rust/forge/crates/forge-harness/src/checks/selection.rs).

```rust
#[derive(Clone, Debug, Deserialize, JsonSchema, Serialize)]
#[serde(deny_unknown_fields)]
/// Runs one saved check, with at most one explicit coverage override.
pub struct CheckRunRequest {
    pub check: CheckNames,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub inputs: Option<super::CheckInputValues>,
    #[serde(default)]
    pub all: bool,
}
```

## CheckNames

Source: [checks/selection.rs](../../../rust/forge/crates/forge-harness/src/checks/selection.rs).

```rust
#[derive(Clone, Debug, Deserialize, JsonSchema, Serialize)]
#[serde(untagged)]
pub enum CheckNames {
    One(String),
    Many(Vec<String>),
}
```

## CheckSelection

Source: [checks/selection.rs](../../../rust/forge/crates/forge-harness/src/checks/selection.rs).

```rust
#[derive(Clone, Debug, Default, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case", deny_unknown_fields)]
/// Selects coverage without allowing the caller to replace configured commands.
pub enum CheckSelection {
    #[default]
    All,
    Inputs {
        inputs: super::CheckInputValues,
    },
    Files {
        files: Vec<String>,
    },
    Cases {
        cases: Vec<CheckCase>,
    },
}
```

## CheckCase

Source: [checks/selection.rs](../../../rust/forge/crates/forge-harness/src/checks/selection.rs).

```rust
#[derive(Clone, Debug, Deserialize, Eq, JsonSchema, Ord, PartialEq, PartialOrd, Serialize)]
#[serde(deny_unknown_fields)]
/// Identifies a runner-resolvable test case and its project-relative source file.
pub struct CheckCase {
    pub file: String,
    pub name: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    /// Optional executable target, such as lib, bin:cli, or test:gameplay.
    pub target: Option<String>,
}
```

## CheckScope

Source: [checks/config.rs](../../../rust/forge/crates/forge-harness/src/checks/config.rs).

```rust
#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
/// Selects project-wide or task-derived coverage without changing command text.
pub enum CheckScope {
    #[default]
    All,
    ChangedFiles,
    ChangedCases,
    /// Runs only tests marked new in the accepted plan.
    NewCases,
}
```

## CheckOverrides

Source: [checks/tools.rs](../../../rust/forge/crates/forge-harness/src/checks/tools.rs).

```rust
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
/// Retains user-selected check states independently of the project configuration.
pub(crate) struct CheckOverrides {
    #[serde(default)]
    pub enabled: BTreeMap<String, bool>,
    #[serde(default)]
    /// Task-local automatic selection preferences.
    pub auto: BTreeMap<String, bool>,
    #[serde(default)]
    pub scopes: BTreeMap<String, super::CheckScope>,
}
```

## CheckState

Source: [checks/model.rs](../../../rust/forge/crates/forge-harness/src/checks/model.rs).

```rust
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
/// Separates command completion from interruption and execution prerequisites.
pub enum CheckState {
    Pending,
    Running,
    Passed,
    Failed,
    Blocked,
    Interrupted,
    Skipped,
    ConfigurationError,
    ExecutionError,
    Stale,
    Stopped,
}
```

## CommandCheck

Source: [checks/model.rs](../../../rust/forge/crates/forge-harness/src/checks/model.rs).

```rust
#[derive(Clone, Debug, Deserialize, Serialize)]
/// Owns one exact accepted command and its durable output identities.
pub struct CommandCheck<CommandContext = Option<CheckCommand>> {
    #[serde(default)]
    /// Membership in this run's selection, independent of execution outcome.
    pub selected: bool,
    #[serde(default)]
    /// Stable inputs and selection certified by this attempt.
    pub evidence: Option<super::evidence::CheckEvidence>,
    #[serde(default)]
    /// Original run and command when an automatic run reused recorded evidence.
    pub reused_from: Option<(String, String)>,
    pub id: String,
    pub command: String,
    #[serde(default)]
    pub context: CommandContext,
    pub state: CheckState,
    pub started_at_ms: Option<i64>,
    pub completed_at_ms: Option<i64>,
    pub exit_code: Option<i32>,
    pub error: Option<String>,
    pub stdout: PathBuf,
    pub stderr: PathBuf,
    pub output: PathBuf,
    pub output_bytes: u64,
    pub preview: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    /// Validated file-level outcomes from a structured runner, separate from this command's status.
    pub report: Option<super::CheckReport>,
}
```

## CheckRun

Source: [checks/model.rs](../../../rust/forge/crates/forge-harness/src/checks/model.rs).

```rust
#[derive(Clone, Debug, Deserialize, Serialize)]
/// Retains a Harness-owned gate without creating a provider exchange.
#[serde(bound(
    deserialize = "RunContext: Deserialize<'de> + Default, CommandContext: Deserialize<'de> + Default"
))]
pub struct CheckRun<RunContext = Option<CheckRunContext>, CommandContext = Option<CheckCommand>> {
    pub id: String,
    /// Destination and source position captured before check execution begins.
    pub owner: Box<crate::timeline::TimelineOwner>,
    pub revision: u32,
    #[serde(default)]
    pub context: RunContext,
    #[serde(default)]
    pub lifecycle: Vec<CheckEvent>,
    pub completed_plan: Option<String>,
    pub shell: crate::session::CheckShell,
    pub executable: Option<PathBuf>,
    pub workspace: String,
    pub environment_digest: String,
    pub started_at_ms: i64,
    pub completed_at_ms: Option<i64>,
    #[serde(default)]
    /// Inputs of the cancelled attempt, used to prevent an automatic retry of unchanged work.
    pub cancelled_inputs: Option<crate::checkpoint::CheckpointRecord>,
    #[serde(default)]
    /// Interrupted run that prevents automatic execution of unchanged inputs.
    pub cancelled_run_id: Option<String>,
    pub state: CheckState,
    pub progress: SemanticProgress,
    pub workspace_identity: Option<String>,
    pub workspace_digest: BTreeMap<String, String>,
    pub commands: Vec<CommandCheck<CommandContext>>,
}
```

## CheckCommand

Source: [checks/model.rs](../../../rust/forge/crates/forge-harness/src/checks/model.rs).

```rust
#[derive(Clone, Debug, Deserialize, Serialize)]
/// Retains configured identity, requested coverage, and the handler's resolved coverage.
pub struct CheckCommand {
    #[serde(default)]
    /// Configured operation before named inputs resolve.
    pub operation: Option<super::CheckOperation>,
    #[serde(default)]
    /// Frozen direct invocation executed without shell interpolation.
    pub invocation: Option<super::invocation::CheckInvocation>,
    pub definition_id: String,
    pub name: String,
    /// Functional area retained with the run's frozen configuration.
    pub area: String,
    /// Reason this check was selected or excluded for this change set.
    pub reason: String,
    /// Trigger-matching paths before the handler resolves executable coverage.
    pub matched_files: Vec<String>,
    pub category: super::CheckCategory,
    pub directory: String,
    pub selection: super::CheckSelection,
    pub resolved: Option<super::CheckSelection>,
    pub depends_on: Vec<String>,
    pub receipt: PathBuf,
}
```

## CheckRunContext

Source: [checks/model.rs](../../../rust/forge/crates/forge-harness/src/checks/model.rs).

```rust
#[derive(Clone, Debug, Deserialize, Serialize)]
/// Freezes project configuration and user overrides independently from the design document.
pub struct CheckRunContext {
    #[serde(default)]
    /// Permits automatic execution to reuse current recorded coverage.
    pub automatic: bool,
    pub trigger: String,
    pub task_id: Option<String>,
    pub snapshot: super::ConfigSnapshot,
    pub overrides: BTreeMap<String, bool>,
    /// Complete task-relative path changes used to evaluate all check triggers.
    pub changed_files: Vec<String>,
    #[serde(default)]
    /// Accepted plan cases available independently of explicit input overrides.
    pub planned_cases: Vec<super::CheckCase>,
}
```

## CheckEvent

Source: [checks/model.rs](../../../rust/forge/crates/forge-harness/src/checks/model.rs).

```rust
#[derive(Clone, Debug, Deserialize, Serialize)]
/// Anchors a check transition at a stable chronological timeline position.
pub struct CheckEvent {
    pub id: String,
    pub created_at_ms: i64,
    pub kind: CheckEventKind,
    pub check_id: Option<String>,
    pub text: String,
}
```

## CheckEventKind

Source: [checks/model.rs](../../../rust/forge/crates/forge-harness/src/checks/model.rs).

```rust
#[derive(Clone, Copy, Debug, Deserialize, Serialize)]
#[serde(rename_all = "snake_case")]
/// Records lifecycle transitions once, independently of timer or output refreshes.
pub enum CheckEventKind {
    Started,
    CheckStarted,
    CheckFinished,
    CheckSkipped,
    ConfigurationError,
    ExecutionError,
    Interrupted,
    Resumed,
    Finished,
    ConfigurationSaved,
}
```

## TraceStatus

Source: [trace/mod.rs](../../../rust/forge/crates/forge-harness/src/trace/mod.rs).

```rust
#[derive(Clone, Debug, Serialize)]
pub struct TraceStatus {
    pub enabled: bool,
    pub path: String,
    pub configured: bool,
}
```

## SpawnAgentRequest

Source: [agent/coordinator.rs](../../../rust/forge/crates/forge-harness/src/agent/coordinator.rs).

```rust
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
/// Describes a fresh child without inheriting parent transcript or workflow state.
pub struct SpawnAgentRequest {
    pub profile: Option<String>,
    pub text: String,
}
```

## Agent

Source: [agent/identity.rs](../../../rust/forge/crates/forge-harness/src/agent/identity.rs).

```rust
/// Stores one reusable provider-backed agent identity and lifecycle.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Agent {
    pub id: String,
    pub session_id: String,
    pub provider_thread_id: Option<String>,
    pub definition: String,
    pub nickname: Option<String>,
    /// Retains the unique generic task name independently of native provider roles.
    pub task_name: String,
    /// Resolves stable tool targets inside this root session.
    pub task_path: String,
    /// Distinguishes model delegation from manual admission and root ownership.
    pub source: super::AgentSource,
    /// Retains independent child tuning and the effective access policy.
    pub selection: Option<super::AgentSelection>,
    /// Preserves informational messages across interruption and process restart.
    pub inbox: Vec<super::AgentMessage>,
    pub state: AgentState,
    pub created_at_ms: i64,
    pub updated_at_ms: i64,
}
```

## PlanRecord

Source: [plan/mod.rs](../../../rust/forge/crates/forge-harness/src/plan/mod.rs).

```rust
/// Represents one durable plan and the exact digest under review.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct PlanRecord {
    pub id: String,
    pub session_id: String,
    pub request: String,
    #[serde(default)]
    pub title: String,
    pub state: PlanState,
    pub working_path: String,
    #[serde(default)]
    pub document_version: u64,
    pub model_revision: u32,
    #[serde(default)]
    pub submitted_version: Option<u64>,
    #[serde(default)]
    pub accepted_revision: Option<u32>,
    pub user_revision: u32,
    #[serde(default)]
    /// Retains ordered user edits across reviewed revisions and final decisions.
    pub manual_changes: Vec<String>,
    pub review_digest: Option<String>,
    pub accepted_digest: Option<String>,
    #[serde(default)]
    pub elicitation: Option<PlanElicitation>,
    #[serde(default)]
    pub acceptance: Option<PlanAcceptance>,
    #[serde(default)]
    pub question_ledger: PlanQuestionLedger,
    #[serde(default)]
    pub generation: PlanGeneration,
    #[serde(default)]
    pub validation_warning: Vec<PlanViolation>,
    pub created_at_ms: i64,
    pub updated_at_ms: i64,
}
```

## PlanQuestionResponse

Source: [plan/mod.rs](../../../rust/forge/crates/forge-harness/src/plan/mod.rs).

```rust
/// Defines one committed response to a planning question.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum PlanQuestionResponse {
    Selected {
        option: String,
        feedback: Option<String>,
    },
    Other {
        text: String,
    },
    Skipped,
}
```

## DesignTestSelection

Source: [plan/design_tests.rs](../../../rust/forge/crates/forge-harness/src/plan/design_tests.rs).

```rust
#[derive(Clone, Debug, Deserialize, Eq, Ord, PartialEq, PartialOrd, Serialize)]
#[serde(deny_unknown_fields)]
/// Identifies one inventory entry independently of its rendered row or array index.
pub(crate) struct DesignTestSelection {
    pub file: String,
    pub name: String,
}
```

## QuestionOwner

Source: [storage/question.rs](../../../rust/forge/crates/forge-harness/src/storage/question.rs).

```rust
#[derive(Deserialize)]
#[serde(rename_all = "snake_case")]
enum QuestionOwner {
    Interaction,
    Plan,
    PlanAcceptance,
}
```

## QuestionTarget

Source: [storage/question.rs](../../../rust/forge/crates/forge-harness/src/storage/question.rs).

```rust
#[derive(Deserialize)]
struct QuestionTarget {
    owner: QuestionOwner,
    id: String,
    set_id: String,
    revision: u32,
}
```

## PresentationOpen

Source: [buffer/session.rs](../../../rust/forge/crates/forge-harness/src/buffer/session.rs).

```rust
#[derive(Serialize)]
pub struct PresentationOpen {
    pub diff_view: forge_diff::view::DiffViewOptions,
    pub status: BufferSnapshot,
    pub syntax_pending: bool,
    pub transcript: BufferSnapshot,
    pub startup_timing: std::collections::BTreeMap<&'static str, f64>,
}
```

## PresentationSync

Source: [buffer/session.rs](../../../rust/forge/crates/forge-harness/src/buffer/session.rs).

```rust
#[derive(Serialize)]
pub struct PresentationSync {
    pub diff_view: forge_diff::view::DiffViewOptions,
    pub status: BufferSnapshot,
    pub syntax_pending: bool,
    pub patch: Vec<BufferPatch>,
    pub snapshot: Option<BufferSnapshot>,
}
```

## OutputDelivery

Source: [buffer/output.rs](../../../rust/forge/crates/forge-harness/src/buffer/output.rs).

```rust
#[derive(Serialize)]
pub struct OutputDelivery {
    pub patch: Option<BufferPatch>,
    pub more: bool,
}
```

## TranscriptAction

Source: [buffer/projection.rs](../../../rust/forge/crates/forge-harness/src/buffer/projection.rs).

```rust
#[derive(Clone, Debug, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum TranscriptAction {
    Question {
        question_set_id: String,
    },
    Tool {
        call_id: super::identity::DisplayId,
    },
    Diff {
        text: String,
        #[serde(skip)]
        syntax: bool,
    },
    File {
        path: String,
        line: usize,
    },
    Declaration {
        plan_id: String,
        revision: u32,
        path: String,
        baseline: bool,
        document: bool,
        line: usize,
    },
    Plan {
        plan_id: String,
        revision: Option<u32>,
    },
    Agent {
        run_id: String,
    },
    Session {
        session_id: String,
    },
    Url {
        url: String,
    },
}
```

