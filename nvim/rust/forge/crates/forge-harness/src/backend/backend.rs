use super::*;
use crate::agent::AgentCapability;
use crate::goal::TurnEvidence;
use crate::plan::{PlanQuestionAnswer, PlanQuestionSet, PlanQuestionWithdrawal};
use crate::session::{ContextUsage, PermissionMode};
use anyhow::{Context, Result};
use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::time::Duration;

pub(crate) const HARNESS_SYSTEM_MESSAGE: &str = include_str!("../plan/prompts/system.md");

/// Identifies one supported provider implementation without leaking launch strings across consumers.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum BackendKind {
    Codex,
    Copilot,
    /// Uses the published Claude Code ACP adapter.
    Claude,
    /// Uses Google's standalone Antigravity ACP server.
    Antigravity,
    Mock,
}

impl BackendKind {
    /// Report OS sandbox support from the provider allowlist.
    pub fn supports_sandbox(self) -> bool {
        matches!(self, Self::Codex)
    }

    /// Resolve requested isolation for the selected provider without changing shared preferences.
    pub(crate) fn resolve_access(
        self,
        mut access: crate::session::AccessPolicy,
    ) -> crate::session::AccessPolicy {
        access.sandbox &= self.supports_sandbox();
        access
    }

    /// Reject OS isolation requests that this provider cannot enforce.
    pub(crate) fn validate_access(self, access: &crate::session::AccessPolicy) -> Result<()> {
        anyhow::ensure!(
            !access.sandbox || self.supports_sandbox(),
            "This provider cannot enforce Forge's OS sandbox"
        );
        Ok(())
    }

    /// Parse one persisted backend name into the supported provider set.
    pub fn parse(value: &str) -> Result<Self> {
        match value {
            "codex" => Ok(Self::Codex),
            "copilot" => Ok(Self::Copilot),
            "claude" => Ok(Self::Claude),
            "antigravity" | "agy" => Ok(Self::Antigravity),
            "mock" => Ok(Self::Mock),
            _ => anyhow::bail!("unsupported Harness backend: {value}"),
        }
    }
}

/// Describes provider identity and capability for broker and editor consumers.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct BackendDescriptor {
    pub kind: BackendKind,
    pub label: String,
    pub capability: BackendCapability,
}

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
    pub execution_mode_list: Vec<PermissionMode>,
    pub agent: AgentCapability,
    #[serde(default)]
    pub catalog: CatalogCapability,
}

/// Represents one executable backend launch descriptor.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct BackendLaunch {
    pub kind: String,
    pub command: Vec<String>,
}

/// Represents the broker intent for one admitted prompt.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum PromptMode {
    /// Run a persistent tool-free generation request.
    Background,
    Chat,
    Plan,
    PlanDiscussion,
    /// Answer an anchored review question in the main conversation without plan mutations.
    PlanQuestion,
    ExecutePlan,
    GoalContinuation,
    RequestChanges,
}

/// Represents one prompt sent across a backend boundary.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct BackendRequest {
    /// Separates the agent runtime from its visible root session and settings scope.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub execution_owner: Option<ExecutionOwner>,
    pub harness_session_id: String,
    pub workspace: String,
    pub input: BackendInput,
    pub mode: PromptMode,
    pub model: String,
    pub effort: String,
    pub context_window: Option<String>,
    /// Carries the session tier through explicit and continuation turns.
    pub service_tier: ServiceTier,
    pub access: crate::session::AccessPolicy,
    pub execution_mode: PermissionMode,
    pub backend_session_id: Option<String>,
    #[serde(skip, default)]
    /// Durable acknowledgment capability attached before dispatch.
    pub submission: Option<std::sync::Arc<super::submission::Submission>>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    /// Earlier definitely undelivered input, ordered before the current instruction.
    pub recovered_input: Vec<BackendInput>,
    #[serde(skip, default)]
    pub control_context: Option<crate::control_tools::ControlTurnContext>,
}

impl BackendRequest {
    /// Renders missing user input ahead of the newest instruction without re-executing a turn.
    pub(crate) fn recovered_text(&self, current: &str, previous: &[String]) -> String {
        if previous.is_empty() {
            return current.to_owned();
        }
        let mut text = String::from(
            "Earlier user input was saved locally but never accepted by the provider. Treat it as context for the newest instruction, which may amend or cancel it.\n",
        );
        for (index, input) in previous.iter().enumerate() {
            text.push_str(&format!(
                "\nEarlier undelivered input {}:\n{}\n",
                index + 1,
                input
            ));
        }
        text.push_str(&format!("\nNewest user input:\n{current}"));
        text
    }

    /// Retains native attachment references from every recovered input and the newest input.
    pub(crate) fn input_attachments(&self) -> Vec<crate::attachment::InputAttachment> {
        self.recovered_input
            .iter()
            .chain(std::iter::once(&self.input))
            .flat_map(|input| input.attachments().iter().cloned())
            .collect()
    }

    /// Resolve the independent runtime without changing the visible settings scope.
    pub fn runtime_key(&self) -> Result<String> {
        Ok(self.owner()?.runtime_key())
    }
    /// Validate the provider runtime key before admission or cancellation routing.
    pub fn owner(&self) -> Result<ExecutionOwner> {
        ExecutionOwner::resolve(&self.execution_owner, &self.harness_session_id)
    }
}

/// Represents a streamed backend update normalized for the interaction reducer.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct BackendEvent {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    /// First adapter receipt time, retained through queueing and forwarding.
    pub received_at_ms: Option<i64>,
    #[serde(default)]
    /// Provider execution that owns this event, when exposed by the adapter.
    pub address: Option<ProviderAddress>,
    #[serde(default)]
    /// Explicit execution admission or completion.
    pub turn_boundary: Option<TurnBoundary>,
    pub kind: String,
    pub text: Option<String>,
    pub data: Value,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub activity: Option<ToolActivity>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub summary: Option<TurnSummary>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub task_update: Option<ProviderTaskUpdate>,
}

impl BackendEvent {
    /// Decode the provider-neutral compaction lifecycle payload.
    pub(crate) fn compaction(&self) -> Result<Option<Compaction>> {
        if self.kind != "compaction" {
            return Ok(None);
        }
        Ok(Some(serde_json::from_value(self.data.clone())?))
    }

    /// Publish one typed compaction update through the shared event transport.
    pub(crate) fn from_compaction(
        compaction: Compaction,
        address: Option<ProviderAddress>,
    ) -> Self {
        Self {
            received_at_ms: None,
            address,
            turn_boundary: None,
            kind: "compaction".into(),
            text: None,
            data: serde_json::to_value(compaction).expect("compaction is JSON serializable"),
            activity: None,
            summary: None,
            task_update: None,
        }
    }
    pub(crate) fn tool_output_delta(&self) -> Option<&str> {
        if self.turn_boundary.is_some()
            || self.summary.is_some()
            || self.task_update.is_some()
            || self.text.is_some()
        {
            return None;
        }
        self.activity
            .as_ref()
            .filter(|activity| {
                activity.output_delta
                    && activity.change.is_empty()
                    && activity
                        .status
                        .as_deref()
                        .is_none_or(|status| status == "inProgress")
            })
            .and_then(|activity| activity.output.as_deref())
    }

    pub(crate) fn message_delta(&self) -> Option<&str> {
        if self.turn_boundary.is_some()
            || self.summary.is_some()
            || self.task_update.is_some()
            || self.activity.is_some()
            || self.message_snapshot()
            || !matches!(
                self.kind.as_str(),
                "assistant_message" | "reasoning" | "reasoning_summary"
            )
        {
            return None;
        }
        self.text.as_deref()
    }

    pub(crate) fn observed_at_ms(&self, fallback: i64) -> i64 {
        [
            "/params/completedAtMs",
            "/params/startedAtMs",
            "/emittedAtMs",
            "/forge_received_at_ms",
        ]
        .into_iter()
        .find_map(|path| self.data.pointer(path).and_then(Value::as_i64))
        .or(self.received_at_ms)
        .unwrap_or(fallback)
    }

    /// Borrow the provider message identity shared by lifecycle and delta events.
    pub(crate) fn message_id(&self) -> Option<&str> {
        [
            "/provider_message_id",
            "/params/item/id",
            "/params/itemId",
            "/params/item_id",
            "/params/messageId",
            "/params/message_id",
            "/data/messageId",
            "/data/message_id",
            "/data/id",
            "/id",
        ]
        .into_iter()
        .find_map(|path| self.data.pointer(path).and_then(Value::as_str))
    }

    /// Borrow the provider delivery phase when the adapter has identified it.
    pub(crate) fn message_phase(&self) -> Option<&str> {
        [
            "/params/item/phase",
            "/params/phase",
            "/data/phase",
            "/phase",
        ]
        .into_iter()
        .find_map(|path| self.data.pointer(path).and_then(Value::as_str))
    }

    /// Distinguish authoritative message text from incremental text delivery.
    pub(crate) fn message_snapshot(&self) -> bool {
        self.data.get("message_update").and_then(Value::as_str) == Some("snapshot")
    }

    /// Build the canonical event for provider-acknowledged active-turn input.
    pub(crate) fn steering_input(text: String) -> Self {
        Self {
            received_at_ms: None,
            address: None,
            turn_boundary: None,
            kind: "steering_input".into(),
            text: Some(text),
            data: Value::Null,
            activity: None,
            summary: None,
            task_update: None,
        }
    }
}

/// Represents one complete provider task replacement.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ProviderTaskUpdate {
    pub scope_id: String,
    pub name: Option<String>,
    pub complete: bool,
    #[serde(default = "default_true")]
    pub replace_entries: bool,
    pub entry_list: Vec<ProviderTaskEntry>,
}

fn default_true() -> bool {
    true
}

/// Represents one task within a provider replacement.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ProviderTaskEntry {
    pub provider_id: Option<String>,
    pub content: String,
    pub priority: Option<String>,
    pub status: TaskStatus,
    pub provider_ordinal: usize,
}

/// Defines provider task state plus retained superseded history.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum TaskStatus {
    Pending,
    InProgress,
    Completed,
    Superseded,
}

/// Represents final timing and usage metadata for one assistant response.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct TurnSummary {
    pub duration_ms: u64,
    pub usage: Option<usage::TokenUsage>,
}

/// Represents one provider tool invocation across its streamed lifecycle.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ToolActivity {
    pub id: String,
    pub kind: ToolActivityKind,
    pub title: String,
    pub output: Option<String>,
    pub status: Option<String>,
    #[serde(default)]
    pub change: ProviderChangeSet,
    #[serde(default)]
    pub output_delta: bool,
}

impl ToolActivity {
    /// Formats a call with sorted, credential-redacted arguments, decoding JSON strings first.
    /// Returns `None` for an empty name or invalid encoded JSON.
    pub(super) fn call_title(name: &str, arguments: &Value) -> Option<String> {
        let name = name.split_whitespace().collect::<Vec<_>>().join(" ");
        if name.is_empty() {
            return None;
        }
        let arguments = match arguments {
            Value::String(encoded) => serde_json::from_str(encoded).ok()?,
            arguments => arguments.clone(),
        };
        let mut arguments = Self::redact_arguments(&arguments);
        arguments.sort_all_objects();
        Some(format!(
            "{name}({})",
            serde_json::to_string(&arguments).ok()?
        ))
    }

    fn redact_arguments(value: &Value) -> Value {
        match value {
            Value::Array(values) => {
                Value::Array(values.iter().map(Self::redact_arguments).collect())
            }
            Value::Object(values) => Value::Object(
                values
                    .iter()
                    .map(|(key, value)| {
                        let value = if Self::sensitive_argument(key) {
                            Value::String("[REDACTED]".into())
                        } else {
                            Self::redact_arguments(value)
                        };
                        (key.clone(), value)
                    })
                    .collect(),
            ),
            _ => value.clone(),
        }
    }

    fn sensitive_argument(key: &str) -> bool {
        matches!(
            key.to_ascii_lowercase().replace(['-', '_'], "").as_str(),
            "token"
                | "apikey"
                | "key"
                | "secret"
                | "password"
                | "passphrase"
                | "authorization"
                | "bearer"
                | "cookie"
                | "session"
                | "credential"
                | "accesstoken"
                | "refreshtoken"
                | "clientsecret"
                | "privatekey"
        )
    }
}

/// Defines one provider-reported file operation without consulting workspace state.
#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum ProviderChangeKind {
    Add,
    Delete,
    #[default]
    Update,
    Move,
}

/// Represents one provider-reported file edit and its textual patch.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct ProviderFileChange {
    pub path: String,
    #[serde(default)]
    pub move_path: Option<String>,
    pub kind: ProviderChangeKind,
    pub diff: String,
}

impl ProviderFileChange {
    /// Separates a provider's trailing move annotation from complete patch content.
    pub(crate) fn split_move_report(kind: ProviderChangeKind, diff: &str) -> (&str, Option<&str>) {
        if matches!(kind, ProviderChangeKind::Add | ProviderChangeKind::Delete) {
            return (diff, None);
        }
        let Some((patch, destination)) = diff.rsplit_once("\nMoved to: ") else {
            return (diff, None);
        };
        let destination = destination.trim_end_matches(['\r', '\n']);
        if destination.is_empty() || destination.contains(['\r', '\n']) {
            return (diff, None);
        }
        (patch, Some(destination))
    }
}

/// Stores provider-reported file edits for one tool lifecycle item.
#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub struct ProviderChangeSet {
    #[serde(default)]
    pub file: Vec<ProviderFileChange>,
}

impl ProviderChangeSet {
    /// Return whether this provider item carries any structured file edits.
    pub fn is_empty(&self) -> bool {
        self.file.is_empty()
    }
}

/// Defines the visible action verb for one normalized tool invocation.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum ToolActivityKind {
    Command,
    FileChange,
    ToolCall,
}

/// Routes normalized provider updates to the live interaction reducer.
pub use events::BackendEventSink;

/// Represents the normalized result of one complete backend turn.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct BackendOutput {
    pub backend_session_id: Option<String>,
    pub provider_checkpoint_id: Option<String>,
    pub event: Vec<BackendEvent>,
    #[serde(default)]
    pub plan_edit: Vec<crate::plan::PlanEditRequest>,
    #[serde(default)]
    pub design_patch: Vec<crate::plan::DesignPatchRequest>,
    pub plan_submit: Option<PlanSubmitRequest>,
    pub plan_read: Option<String>,
    #[serde(default)]
    pub plan_deviation: Vec<crate::plan::PlanDeviationRequest>,
    #[serde(default)]
    pub plan_task_report: Vec<crate::plan::PlanTaskReport>,
    pub plan_question: Option<PlanQuestionSet>,
    pub question_answer: Option<PlanQuestionAnswer>,
    pub question_withdrawal: Option<PlanQuestionWithdrawal>,
    pub control_error: Option<String>,
    pub evidence: TurnEvidence,
    pub capability: BackendCapability,
    pub runtime: BackendRuntime,
    #[serde(skip)]
    pub structured_plan: bool,
    #[serde(skip)]
    pub metrics: TurnMetrics,
}

/// Identifies the exact canonical plan version entering review.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct PlanSubmitRequest {
    pub plan_id: String,
    pub expected_version: u64,
}

/// Describes a provider fork after the Harness child identity is allocated.
#[derive(Clone, Debug)]
pub struct BackendForkRequest {
    pub source: BackendRequest,
    pub target_harness_session_id: String,
    pub checkpoint_id: Option<String>,
}

/// Tracks provider-reported metrics while one backend turn streams.
#[derive(Clone, Debug, Default)]
pub struct TurnMetrics {
    pub context_usage: Option<ContextUsage>,
    pub native_compact_update: Option<bool>,
}

/// Represents the provider identity and resolved model shown by Harness.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct BackendRuntime {
    pub provider: String,
    pub model: Option<String>,
}

impl BackendRuntime {
    /// Publishes resolved provider identity independently of turn content and completion.
    pub fn resolved_event(&self) -> BackendEvent {
        BackendEvent {
            received_at_ms: None,
            address: None,
            turn_boundary: None,
            kind: "runtime_resolved".into(),
            text: None,
            data: serde_json::json!(self),
            activity: None,
            summary: None,
            task_update: None,
        }
    }
}

/// Represents one measured provider phase returned to broker diagnostics.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct BackendTimingRecord {
    pub phase: String,
    pub duration_ms: f64,
}

/// Represents a native provider fork and its provider-owned timing phases.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct BackendForkResult {
    pub backend_session_id: String,
    pub timing: Vec<BackendTimingRecord>,
}

impl BackendForkResult {
    /// Build an unprofiled native fork result from its provider session identity.
    pub fn unprofiled(backend_session_id: impl Into<String>) -> Self {
        Self {
            backend_session_id: backend_session_id.into(),
            timing: Vec::new(),
        }
    }
}

/// Represents one selectable context-window tier advertised by a provider.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct BackendContextWindow {
    pub id: String,
    pub token_limit: Option<u64>,
}

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

/// Requires session lifecycle ownership and provides explicit failures for optional operations.
#[async_trait]
pub trait Backend: Send + Sync {
    /// Reports whether every local tool waits on the request's checkpoint barrier.
    fn checkpoint_tool_gate(&self) -> bool {
        false
    }

    /// Inspect or explicitly install the selected provider runtime without creating a conversation.
    async fn setup(&self, _request: BackendCatalogRequest, _install: bool) -> Result<Value> {
        Err(UnsupportedCapability("runtime_setup").into())
    }

    /// Authenticate explicitly without admitting a model prompt or creating provider history.
    async fn authenticate(&self, _request: BackendCatalogRequest, _method: String) -> Result<()> {
        Err(UnsupportedCapability("authentication").into())
    }

    /// Return support and discovery state for the selected session without launching provider work.
    async fn capabilities(&self, _request: BackendCatalogRequest) -> Result<CapabilityManifest> {
        Ok(CapabilityManifest::from_descriptor(self.descriptor()))
    }

    /// Reconcile portable resource selection before reporting configuration as applied.
    async fn reconcile_settings(
        &self,
        _request: BackendCatalogRequest,
        setting: &crate::settings::PortableSettings,
    ) -> Result<()> {
        if !setting.skill.is_empty() || !setting.mcp.is_empty() {
            return Err(UnsupportedCapability("resource_selection").into());
        }
        Ok(())
    }
    /// Collect provider process ownership after admitted session work has drained.
    async fn shutdown(&self) -> Result<()>;

    /// Return stable provider identity and capability without starting a provider process.
    fn descriptor(&self) -> BackendDescriptor;

    /// Returns a native receipt only when history proves this exact submission was accepted.
    /// Missing history and unsupported reconciliation return None and never authorize replay.
    async fn reconcile_submission(
        &self,
        _submission: &super::submission::SubmissionRecord,
        _known_receipts: &std::collections::HashSet<String>,
    ) -> Result<Option<String>> {
        Ok(None)
    }

    /// Run one prompt and normalize provider events into Harness events.
    async fn prompt(&self, request: BackendRequest) -> Result<BackendOutput> {
        self.prompt_stream(request, None).await
    }

    /// Run one prompt while publishing provider events before the turn completes.
    async fn prompt_stream(
        &self,
        request: BackendRequest,
        event_sink: Option<BackendEventSink>,
    ) -> Result<BackendOutput>;

    /// Fork a provider session only when the provider advertises native support.
    async fn fork(&self, _request: BackendForkRequest) -> Result<BackendForkResult> {
        Err(UnsupportedCapability("native_fork").into())
    }

    /// Query live provider shells independently of prompt execution and exchange completion.
    async fn background_terminals(
        &self,
        _request: BackendCatalogRequest,
    ) -> Result<terminal::TerminalSnapshot> {
        Err(UnsupportedCapability("background_terminals").into())
    }

    /// Terminate one provider-owned background shell without interrupting its exchange.
    async fn terminate_terminal(&self, _request: BackendCatalogRequest, _id: &str) -> Result<()> {
        Err(UnsupportedCapability("background_termination").into())
    }

    /// Generate text independently of the main provider conversation.
    async fn generate_text(
        &self,
        _request: BackendCatalogRequest,
        _purpose: TextGeneration,
        _model: &str,
        _reasoning_effort: Option<&str>,
        _history: &str,
    ) -> Result<String> {
        Err(UnsupportedCapability("text_generation").into())
    }

    /// List provider models for the Harness model picker.
    async fn model_list(&self, _request: BackendRequest) -> Result<Vec<BackendModel>> {
        Err(UnsupportedCapability("model_selection").into())
    }

    /// List user-invocable provider skills with their effective enabled state.
    async fn skill_list(&self, _request: BackendCatalogRequest) -> Result<Vec<SkillDefinition>> {
        Err(UnsupportedCapability("skill_catalog").into())
    }

    /// Persist one provider skill's enabled state for future turns.
    async fn set_skill_enabled(
        &self,
        _request: BackendCatalogRequest,
        _name: &str,
        _enabled: bool,
    ) -> Result<CatalogMutation> {
        Err(UnsupportedCapability("skill_mutation").into())
    }

    /// List configured servers without waiting for their startup handshakes.
    async fn mcp_configuration(
        &self,
        _request: BackendCatalogRequest,
    ) -> Result<Vec<McpDefinition>> {
        Err(UnsupportedCapability("mcp_catalog").into())
    }

    /// Query server readiness and tool definitions when the provider exposes them.
    async fn mcp_list(&self, _request: BackendCatalogRequest) -> Result<Vec<McpDefinition>> {
        Err(UnsupportedCapability("mcp_catalog").into())
    }

    /// Persist one MCP server's enabled state and report whether its turn must resume.
    async fn set_mcp_enabled(
        &self,
        _request: BackendCatalogRequest,
        _name: &str,
        _enabled: bool,
    ) -> Result<CatalogMutation> {
        Err(UnsupportedCapability("mcp_mutation").into())
    }

    /// Return whether one Harness session currently owns an in-flight provider turn.
    async fn has_active_turn(&self, session_id: &str) -> bool;

    /// Clear provisional native turn identity before admitting the next broker invocation.
    async fn prepare_turn(&self, _session_id: &str) -> Result<()> {
        Ok(())
    }

    /// Stop native continuation through the active reader before acquiring the broker lock.
    /// The caller races reader readiness against broker availability and may cancel this wait.
    async fn stop_goal_session(&self, _session_id: &str, _clear: bool) -> Result<()> {
        Ok(())
    }

    /// Update a native provider goal when that backend owns goal persistence.
    async fn goal_status(
        &self,
        _request: BackendRequest,
        _objective: Option<String>,
        _status: &str,
    ) -> Result<()> {
        Ok(())
    }

    /// Compact provider-owned conversation history when the backend advertises support.
    async fn compact(&self, _request: BackendCompactionRequest) -> Result<BackendOutput> {
        Err(UnsupportedCapability("native_compact").into())
    }

    /// Subscribe once to session compaction that can outlive an active provider turn.
    async fn compaction_stream(
        &self,
        _session_id: &str,
    ) -> Result<Option<events::BackendEventStream>> {
        Ok(None)
    }

    /// Appends input to an active turn without starting an idle provider session.
    /// Implementations render user additions only and keep agent results out of user-prompt rows.
    async fn steer_session(&self, _session_id: &str, _input: SteeringInput) -> Result<()> {
        Err(UnsupportedCapability("native_steer").into())
    }

    /// Interrupt the parent and descendants for one session and await terminal evidence.
    async fn cleanup_execution(&self, session_id: &str) -> Result<()>;

    /// Stop and collect one session through the same execution cleanup contract.
    async fn cancel_session(&self, session_id: &str) -> Result<()> {
        self.cleanup_execution(session_id).await
    }

    /// Return one Harness session's provider conversation after interruption.
    async fn active_session_id_for(&self, session_id: &str) -> Option<String>;

    /// Roll back a cancelled turn after the broker proves it produced no visible output or changes.
    async fn rollback_cancelled_turn(&self) -> Result<bool> {
        Ok(false)
    }

    /// Roll back one Harness session's output-free cancelled turn.
    async fn rollback_cancelled_turn_for(&self, _session_id: &str) -> Result<bool> {
        self.rollback_cancelled_turn().await
    }
}

/// Build a backend strategy from an explicit launch descriptor.
pub fn build(
    launch: BackendLaunch,
    permission_coordinator: std::sync::Arc<approval::PermissionCoordinator>,
    trace: std::sync::Arc<crate::trace::TraceStore>,
    settings_root: std::path::PathBuf,
) -> Result<Box<dyn Backend>> {
    match BackendKind::parse(&launch.kind)? {
        BackendKind::Codex => Ok(Box::new(
            codex::CodexBackend::new_with_permission_coordinator(
                launch.command,
                permission_coordinator,
                trace,
                Some(settings_root),
            )?,
        )),
        BackendKind::Copilot => Ok(Box::new(
            copilot::CopilotBackend::new_with_permission_coordinator(
                launch.command,
                permission_coordinator,
                trace,
                Some(settings_root),
            )?,
        )),
        kind @ (BackendKind::Claude | BackendKind::Antigravity) => Ok(Box::new(
            acp::AcpBackend::new(kind, launch.command, permission_coordinator, settings_root)?,
        )),
        BackendKind::Mock => Ok(Box::new(MockBackend {
            lsp_probe: launch
                .command
                .first()
                .is_some_and(|value| value == "lsp-tools"),
            delay: match launch.command.first().map(String::as_str) {
                Some("blocking" | "visible-blocking" | "writing-blocking") => {
                    Some(Duration::from_secs(60))
                }
                Some("session-blocking" | "approval-blocking") => Some(Duration::from_secs(3)),
                Some("steering") => Some(Duration::from_millis(500)),
                _ => None,
            },
            emit_before_delay: launch
                .command
                .first()
                .is_some_and(|value| value == "visible-blocking"),
            write_before_delay: launch
                .command
                .first()
                .is_some_and(|value| value == "writing-blocking"),
            fork_delay: launch
                .command
                .first()
                .is_some_and(|value| value == "fork-blocking")
                .then_some(Duration::from_secs(3)),
            steering_by_session: Default::default(),
            permission: launch
                .command
                .first()
                .is_some_and(|value| value == "approval-blocking")
                .then_some(permission_coordinator),
            retained_control: launch
                .command
                .first()
                .is_some_and(|value| value == "retained-control")
                .then(Default::default),
        })),
    }
}

struct MockBackend {
    lsp_probe: bool,
    retained_control: Option<
        std::sync::Mutex<
            std::collections::HashMap<String, crate::control_tools::ControlTurnContext>,
        >,
    >,
    permission: Option<std::sync::Arc<approval::PermissionCoordinator>>,
    delay: Option<Duration>,
    emit_before_delay: bool,
    write_before_delay: bool,
    fork_delay: Option<Duration>,
    steering_by_session: std::sync::Mutex<
        std::collections::HashMap<String, std::sync::Weak<steering::SteeringLane>>,
    >,
}

impl MockBackend {
    fn steering_lane(&self, session_id: &str) -> std::sync::Arc<steering::SteeringLane> {
        let mut lane = self
            .steering_by_session
            .lock()
            .expect("mock steering map poisoned");
        lane.retain(|_, lane| lane.strong_count() > 0);
        if let Some(active) = lane.get(session_id).and_then(std::sync::Weak::upgrade) {
            return active;
        }
        let active = std::sync::Arc::new(steering::SteeringLane::default());
        lane.insert(session_id.to_owned(), std::sync::Arc::downgrade(&active));
        active
    }
}

#[async_trait]
impl Backend for MockBackend {
    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn cancel_session(&self, _session_id: &str) -> Result<()> {
        Ok(())
    }

    async fn has_active_turn(&self, _session_id: &str) -> bool {
        false
    }

    async fn active_session_id_for(&self, _session_id: &str) -> Option<String> {
        None
    }

    async fn background_terminals(
        &self,
        _request: BackendCatalogRequest,
    ) -> Result<terminal::TerminalSnapshot> {
        Ok(terminal::TerminalSnapshot::default())
    }

    async fn cleanup_execution(&self, _session_id: &str) -> Result<()> {
        Ok(())
    }
    async fn generate_text(
        &self,
        _request: BackendCatalogRequest,
        purpose: TextGeneration,
        _model: &str,
        _reasoning_effort: Option<&str>,
        _history: &str,
    ) -> Result<String> {
        if let Some(delay) = self.delay {
            tokio::time::sleep(delay).await;
        }
        purpose.validate(match purpose {
            TextGeneration::Recap => {
                "The current request is recorded. Review the next proposed change."
            }
            TextGeneration::SessionName => "Review proposed changes",
            TextGeneration::CommitMessage => "feat: describe repository changes",
            TextGeneration::PullRequestDescription => {
                "Describes the repository changes for reviewers."
            }
        })
    }
    fn descriptor(&self) -> BackendDescriptor {
        BackendDescriptor {
            kind: BackendKind::Mock,
            label: "Mock CLI".into(),
            capability: mock_capability(),
        }
    }

    async fn prompt_stream(
        &self,
        request: BackendRequest,
        event_sink: Option<BackendEventSink>,
    ) -> Result<BackendOutput> {
        if let Some(retained) = &self.retained_control
            && let Some(context) = &request.control_context
        {
            retained
                .lock()
                .expect("mock control context poisoned")
                .insert(request.runtime_key()?, context.clone());
        }
        if self.lsp_probe
            && let Some(arguments) = request.input.text().strip_prefix("LSP ")
        {
            let context = request
                .control_context
                .clone()
                .context("fixture has no control context")?;
            let mut arguments: Value = serde_json::from_str(arguments)?;
            let name = arguments
                .as_object_mut()
                .and_then(|value| value.remove("tool"))
                .and_then(|value| value.as_str().map(str::to_owned))
                .context("LSP fixture requires tool name")?;
            let invocation = crate::control_tools::ControlToolInvocation { name, arguments };
            let text = match crate::control_tools::ControlToolExecutor::new(context)
                .execute(invocation)
                .await
                .and_then(|outcome| outcome.result)
            {
                Ok(result) => result.message,
                Err(error) => format!("LSP error: {error:#}"),
            };
            let event = BackendEvent {
                received_at_ms: None,
                address: None,
                turn_boundary: None,
                kind: "assistant_message".into(),
                text: Some(text),
                data: Value::Null,
                activity: None,
                summary: None,
                task_update: None,
            };
            if let Some(sink) = event_sink {
                sink.send_wait(event.clone()).await?;
            }
            return Ok(BackendOutput {
                event: vec![event],
                capability: mock_capability(),
                ..Default::default()
            });
        }
        let steering = self.steering_lane(&request.runtime_key()?);
        let mut active_steering = steering.activate(event_sink.clone())?;
        let mut steering_text_list = Vec::new();
        if let Some(context) = request.control_context.as_ref()
            && context
                .checks
                .as_ref()
                .is_some_and(|scope| scope.setup_reviewed().is_some())
        {
            let advertised = crate::control_tools::ControlToolRegistry
                .definition_list_for_context(Some(context), true);
            anyhow::ensure!(
                advertised.iter().any(|tool| tool.name == "checks_list")
                    && advertised
                        .iter()
                        .any(|tool| tool.name == "checks_configure"),
                "check setup requires advertised list and configuration tools"
            );
        }
        if let Some(scope) = request
            .control_context
            .as_ref()
            .and_then(|context| context.checks.as_ref())
            && scope.setup_reviewed() == Some(false)
        {
            let catalog: Value =
                serde_json::from_str(&scope.invoke("checks_list", serde_json::json!({})).await?)?;
            let rejected = scope.invoke("checks_configure", serde_json::json!({
                "expected_revision": catalog["revision"], "config": {"version":2,"definitions":[]}
            })).await;
            anyhow::ensure!(
                rejected.is_err(),
                "setup published before the user answered"
            );
            return Ok(BackendOutput { plan_question: Some(crate::plan::PlanQuestionSet::freeform(
                "Which functional areas and suites should run automatically, and should routine coverage be targeted or full?".into())), ..Default::default() });
        }
        if let Some(scope) = request
            .control_context
            .as_ref()
            .and_then(|context| context.checks.as_ref())
            && scope.setup_reviewed() == Some(true)
        {
            let catalog: Value =
                serde_json::from_str(&scope.invoke("checks_list", serde_json::json!({})).await?)?;
            let config = if catalog["snapshot"]["config"].is_object() {
                catalog["snapshot"]["config"].clone()
            } else {
                serde_json::json!({"version":2,"definitions":[]})
            };
            scope
                .invoke(
                    "checks_configure",
                    serde_json::json!({
                        "expected_revision": catalog["revision"], "config": config,
                    }),
                )
                .await?;
        }
        if let Some(permission) = &self.permission {
            let resolution = permission
                .authorize_callback(
                    request.execution_mode,
                    true,
                    crate::permissions::matcher::PermissionRequest {
                        id: "mock-approval".into(),
                        provider: "Mock".into(),
                        reason: None,
                        target_list: vec![crate::permissions::matcher::PermissionTarget::Command {
                            command: "mock-probe".into(),
                            shell: None,
                        }],
                    },
                    Some(ProviderAddress {
                        owner: Some(request.owner()?),
                        thread_id: request.harness_session_id.clone(),
                        turn_id: "mock-turn".into(),
                    }),
                    event_sink.as_ref(),
                )
                .await?;
            anyhow::ensure!(
                resolution == approval::ApprovalResolution::AllowOnce,
                "mock approval was not allowed"
            );
        }
        let event = BackendEvent {
            received_at_ms: None,
            address: None,
            turn_boundary: None,
            kind: "assistant_message".into(),
            text: Some(if request.mode == PromptMode::PlanQuestion {
                "This declaration defines the proposed interface and separates its responsibility from the surrounding code. Implementation details remain outside the declaration plan.".into()
            } else {
                format!("Mock response: {}", request.input.text())
            }),
            data: Value::Null,
            activity: None,
            summary: None,
            task_update: None,
        };
        if self.emit_before_delay
            && let Some(event_sink) = event_sink.as_ref()
        {
            let _ = event_sink.send_wait(event.clone()).await;
        }
        if self.write_before_delay {
            std::fs::write(
                std::path::Path::new(&request.workspace).join("mock-provider-change.txt"),
                "changed before cancellation\n",
            )?;
        }
        if let Some(delay) = self.delay {
            let delay = tokio::time::sleep(delay);
            tokio::pin!(delay);
            loop {
                tokio::select! {
                    () = &mut delay => break,
                    Some(command) = active_steering.receive() => {
                        if let Some(submission) = &command.submission {
                            let admitted = submission.sending(super::submission::SubmissionTarget::Untracked)
                                .and_then(|()| submission.accepted("local mock steering admission".into()));
                            if let Err(error) = admitted {
                                command.complete(Err(error)).await;
                                continue;
                            }
                        }
                        steering_text_list.push(command.text.clone());
                        command.complete(Ok(())).await;
                    }
                }
            }
        }
        if request.mode == PromptMode::Plan {
            if let Some(document) = request
                .control_context
                .as_ref()
                .and_then(|context| context.plan_document.as_ref())
                .filter(|document| document.design.is_some())
            {
                let path = "src/change.rs";
                let design = document.design.as_ref().unwrap();
                let mut source_digests = std::collections::BTreeMap::new();
                let patch = if design.proposed.contains_key(path)
                    || std::path::Path::new(&request.workspace)
                        .join(path)
                        .is_file()
                {
                    let inspected = design.inspect(
                        std::path::Path::new(&request.workspace),
                        Some(path),
                        false,
                    )?;
                    if let Some(digest) = inspected["source_digest"].as_str() {
                        source_digests.insert(path.into(), digest.into());
                    }
                    let text = inspected["text"]
                        .as_str()
                        .ok_or_else(|| anyhow::anyhow!("mock declaration read has no text"))?;
                    let removed = text
                        .lines()
                        .map(|line| format!("-{line}\n"))
                        .collect::<String>();
                    let original = design
                        .baseline
                        .get(path)
                        .map(|file| file.text.as_str())
                        .unwrap_or(text);
                    let behavior = if original.contains("pub fn reviewed_change(") {
                        "Change\n  Preserve the requested behavior."
                    } else {
                        "Algorithm\n  return"
                    };
                    let behavior = behavior
                        .lines()
                        .map(|line| format!("+{line}\n"))
                        .collect::<String>();
                    format!(
                        "*** Begin Patch\n*** Update File: {path}\n@@\n{removed}+pub fn reviewed_change();\n{behavior}*** End Patch"
                    )
                } else {
                    format!(
                        "*** Begin Patch\n*** Add File: {path}\n+pub fn requested_change();\n+Algorithm\n+  return\n*** End Patch"
                    )
                };
                let mut patch = patch;
                let mut configuration = design.proposed.clone();
                for path in [
                    "Cargo.toml",
                    "package.json",
                    "tsconfig.json",
                    "ci.yaml",
                    "App.csproj",
                    ".gitignore",
                ] {
                    if !configuration.contains_key(path)
                        && !design.baseline.contains_key(path)
                        && std::path::Path::new(&request.workspace)
                            .join(path)
                            .is_file()
                    {
                        let inspected = design.inspect(
                            std::path::Path::new(&request.workspace),
                            Some(path),
                            false,
                        )?;
                        configuration
                            .insert(path.into(), inspected["text"].as_str().unwrap().into());
                        source_digests.insert(
                            path.into(),
                            inspected["source_digest"].as_str().unwrap().into(),
                        );
                    }
                }
                for (path, configuration) in &configuration {
                    if forge_diff::syntax::ConfigurationFormat::for_path(path).is_none() {
                        continue;
                    }
                    let updated = configuration.replacen("\"0.1.0\"", "\"0.2.0\"", 1);
                    if updated != *configuration {
                        let removed = configuration
                            .lines()
                            .map(|line| format!("-{line}\n"))
                            .collect::<String>();
                        let added = updated
                            .lines()
                            .map(|line| format!("+{line}\n"))
                            .collect::<String>();
                        patch = patch.replace(
                            "*** End Patch",
                            &format!("*** Update File: {path}\n@@\n{removed}{added}*** End Patch"),
                        );
                    }
                }
                let metadata = serde_json::to_string_pretty(&design.document)?
                    .lines()
                    .map(|line| format!("-{line}\n"))
                    .collect::<String>();
                let mut revised_metadata = design.document.clone();
                revised_metadata.objective = "Revise the registry interface.".into();
                revised_metadata.background =
                    "The registry owns published handles used by its callers.".into();
                revised_metadata.requirements =
                    vec!["Preserve the registry ownership boundary.".into()];
                revised_metadata.design =
                    "Revise the registry interface while preserving its ownership boundary.".into();
                revised_metadata.verification =
                    vec!["Confirm the requested interface behaves as designed.".into()];
                let added = serde_json::to_string_pretty(&revised_metadata)?
                    .lines()
                    .map(|line| format!("+{line}\n"))
                    .collect::<String>();
                let patch = patch.replace(
                    "*** End Patch",
                    &format!("*** Update File: plan.json\n@@\n{metadata}{added}*** End Patch"),
                );
                let change = crate::plan::DesignPatchRequest {
                    plan_id: document.plan_id.clone(),
                    expected_version: document.version,
                    patch,
                    title: Some("Design the requested change".into()),
                    source_digests,
                };
                let proposed = document
                    .patch_design(std::path::Path::new(&request.workspace), change.clone())?;
                return Ok(BackendOutput {
                    backend_session_id: request.backend_session_id.or(Some("mock-session".into())),
                    design_patch: vec![change],
                    plan_submit: Some(PlanSubmitRequest {
                        plan_id: document.plan_id.clone(),
                        expected_version: proposed.version,
                    }),
                    structured_plan: true,
                    capability: mock_capability(),
                    ..Default::default()
                });
            }
        }
        let planning_note = (request.mode == PromptMode::Plan).then(|| {
            let steering = steering_text_list
                .iter()
                .map(|text| format!("- {text}"))
                .collect::<Vec<_>>()
                .join("\n");
            if steering.is_empty() {
                request.input.text().to_owned()
            } else {
                format!(
                    "{}\n\nSteering constraints:\n{steering}",
                    request.input.text()
                )
            }
        });
        let plan_document = planning_note
            .as_deref()
            .and_then(mock_plan_document_from_prompt);
        let mut plan_edit = Vec::new();
        let plan_submit = plan_document.as_ref().map(|document| {
            let mut mutation = crate::plan::PlanMutation {
                plan: Some(crate::plan::PlanFieldPatch {
                    title: Some("Implement the requested change".into()),
                    overview: Some(format!(
                        "{}\n{}",
                        if document.prompt.is_empty() {
                            &document.title
                        } else {
                            &document.prompt
                        },
                        steering_text_list.join("\n")
                    )),
                    ..Default::default()
                }),
                ..Default::default()
            };
            if document.stages.is_empty() {
                mutation.set = Some(crate::plan::PlanResourceSet {
                    entity_changes: Some(vec![crate::plan::ProgramEntityChange {
                            action: crate::plan::EntityChangeAction::Add,
                            kind: crate::plan::EntityKind::Function,
                            renamed_from: None,
                            name: "requested_change".into(),
                            description: "Implement the requested behavior.".into(),
                            path: "src/change.rs".into(),
                            members: Vec::new(),
                            variants: Vec::new(),
                            extends: None,
                            conforms_to: Vec::new(),
                    }]),
                    flows: Some(vec![crate::plan::PlanFlow {
                            title: "Requested change".into(),
                            description: "Start from the requested change and produce its observable result. Keep implementation ownership explicit at the changed entity boundary.".into(),
                            source: crate::plan::EntityReference::PlannedEntity {
                                entity: "requested_change".into(),
                            },
                            edges: vec![crate::plan::PlanFlowEdge {
                                    relation: crate::plan::PlanFlowRelation::Emit,
                                    target: crate::plan::EntityReference::ExternalEntity {
                                        entity_kind: crate::plan::ReferencedEntityKind::Endpoint,
                                        name: "requested outcome".into(),
                                        dependency: None,
                                    },
                                    callable: None,
                                    payload_type: Some("completed change".into()),
                                    return_type: None,
                                    expansion: Vec::new(),
                                    branches: Vec::new(),
                            }],
                    }]),
                    ..Default::default()
                });
                mutation.stages = Some(vec![crate::plan::PlanStage { id: "implementation".into(), title: "Implement the change".into(), tasks: vec![crate::plan::PlanTask { id: "requested-change".into(), requires: Vec::new(),
                            title: "Implement the requested change".into(),
                            description: "Connect the requested behavior to its source boundary."
                                .into(),
                            files: vec![crate::plan::PlanFile {
                                change: crate::plan::PlanFileChange::Add {
                                    path: "src/change.rs".into(),
                                },
                                subtasks: vec![
                                    crate::plan::PlanSubtask::Work(
                                        crate::plan::PlanWorkSubtask {
                                            action: crate::plan::SubtaskAction::Create,
                                            description: "Apply the planned behavior at its owner."
                                                .into(),
                                            entities: vec!["requested_change".into()],
                                        },
                                    ),
                                    crate::plan::PlanSubtask::Test(
                                        crate::plan::PlanTestSubtask {
                                            operation: crate::plan::TestSubtaskOperation::Test,
                                            action: crate::plan::ChangeAction::Add,
                                            renamed_from: None,
                                            name: "verify_requested_change".into(),
                                            category: crate::plan::TestCategory::Unit,
                                            behavior:
                                                "The implementation produces the requested result."
                                                    .into(),
                                            covers_entities: vec!["requested_change".into()],
                                        },
                                    ),
                                ],
                            }],
                    }] }]);
            }
            plan_edit.push(crate::plan::PlanEditRequest {
                plan_id: document.plan_id.clone(),
                expected_version: document.version,
                mutation,
            });
            PlanSubmitRequest {
                plan_id: document.plan_id.clone(),
                expected_version: document.version.saturating_add(1),
            }
        });
        let execution_document = matches!(
            request.mode,
            PromptMode::ExecutePlan | PromptMode::GoalContinuation
        )
        .then(|| mock_plan_document_from_prompt(&request.input.text()))
        .flatten();
        let execution_id = request
            .input
            .text()
            .split("Execution ID: ")
            .nth(1)
            .and_then(|tail| tail.split(['.', '\n']).next())
            .map(str::to_owned);
        let plan_task_report = execution_document
            .as_ref()
            .zip(execution_id)
            .and_then(|(document, execution_id)| {
                request
                    .input
                    .text()
                    .split("Active task:\n```json\n")
                    .nth(1)
                    .and_then(|tail| tail.split("\n```").next())
                    .and_then(|value| serde_json::from_str::<serde_json::Value>(value).ok())
                    .and_then(|active| {
                        active["task_id"]
                            .as_str()
                            .and_then(|id| document.task_by_id(id).map(|(_, task)| task))
                    })
                    .map(|task| (document, task, execution_id))
            })
            .map(
                |(document, task, execution_id)| crate::plan::PlanTaskReport {
                    task_id: task.id.clone(),
                    plan_version: document.version,
                    execution_id,
                    task_path: document.task_by_id(&task.id).unwrap().0,
                    state: crate::plan::PlanTaskState::Complete,
                    completed_subtask_paths: task
                        .files
                        .iter()
                        .enumerate()
                        .flat_map(|(file_index, file)| {
                            file.subtasks
                                .iter()
                                .enumerate()
                                .map(move |(subtask_index, _)| {
                                    format!(
                                        "{}/files/{file_index}/subtasks/{subtask_index}",
                                        document.task_by_id(&task.id).unwrap().0
                                    )
                                })
                        })
                        .collect(),
                    completed_entity_paths: task
                        .files
                        .iter()
                        .flat_map(|file| file.subtasks.iter())
                        .flat_map(|subtask| subtask.owned_entities().iter())
                        .filter_map(|entity_name| {
                            document
                                .entity_changes
                                .iter()
                                .position(|entity| entity.name == *entity_name)
                        })
                        .map(|entity_index| format!("/entity_changes/{entity_index}"))
                        .collect(),
                    test_results: task
                        .files
                        .iter()
                        .enumerate()
                        .flat_map(|(file_index, file)| {
                            file.subtasks.iter().enumerate().filter_map(
                                move |(subtask_index, subtask)| {
                                    subtask.test().map(|_| crate::plan::PlanTestResult {
                                        test_subtask_path: Some(format!(
                                            "{}/files/{file_index}/subtasks/{subtask_index}",
                                            document.task_by_id(&task.id).unwrap().0
                                        )),
                                        status: crate::plan::PlanTestStatus::Passed,
                                        command: Some("mock test".into()),
                                        detail: None,
                                    })
                                },
                            )
                        })
                        .collect(),
                    changed_paths: task
                        .files
                        .iter()
                        .flat_map(|file| {
                            file.change
                                .source_path()
                                .into_iter()
                                .chain(std::iter::once(file.change.path()))
                                .map(str::to_owned)
                        })
                        .collect(),
                    summary: Some("Completed the mock task.".into()),
                    blocking_reason: None,
                },
            );
        let execution_complete = plan_task_report.as_ref().is_some_and(|report| {
            execution_document
                .as_ref()
                .and_then(|document| document.tasks().last())
                .is_some_and(|task| task.id == report.task_id)
        });
        if !self.emit_before_delay
            && let Some(event_sink) = event_sink
        {
            let _ = event_sink.send_wait(event.clone()).await;
        }
        Ok(BackendOutput {
            backend_session_id: request
                .backend_session_id
                .or_else(|| Some("mock-session".into())),
            provider_checkpoint_id: Some("mock-checkpoint".into()),
            event: vec![event],
            plan_edit,
            design_patch: Vec::new(),
            plan_submit,
            plan_read: None,
            plan_deviation: Vec::new(),
            plan_task_report: plan_task_report.into_iter().collect(),
            plan_question: None,
            question_answer: None,
            question_withdrawal: None,
            control_error: None,
            evidence: TurnEvidence {
                tool_called: request.execution_mode.is_write_default(),
                structured_complete: execution_complete,
                ..TurnEvidence::default()
            },
            capability: mock_capability(),
            runtime: BackendRuntime {
                provider: "Mock backend".into(),
                model: Some(request.model),
            },
            structured_plan: request.mode == PromptMode::Plan,
            metrics: TurnMetrics::default(),
        })
    }

    async fn fork(&self, request: BackendForkRequest) -> Result<BackendForkResult> {
        if let Some(delay) = self.fork_delay {
            tokio::time::sleep(delay).await;
        }
        Ok(BackendForkResult::unprofiled(format!(
            "{}-fork",
            request
                .source
                .backend_session_id
                .unwrap_or_else(|| "mock-session".into())
        )))
    }

    async fn steer_session(&self, session_id: &str, input: SteeringInput) -> Result<()> {
        self.steering_lane(session_id).steer(input).await
    }

    async fn rollback_cancelled_turn(&self) -> Result<bool> {
        Ok(true)
    }

    async fn compact(&self, request: BackendCompactionRequest) -> Result<BackendOutput> {
        let compaction = Compaction {
            id: request.id,
            thread_id: request
                .request
                .backend_session_id
                .clone()
                .unwrap_or_default(),
            state: CompactionState::Completed,
            manual: true,
            started_at_ms: None,
            completed_at_ms: None,
            result: CompactionResult {
                summary_content: Some(
                    "Mock compacted context preserves the conversation constraints.".into(),
                ),
                pre_compaction_tokens: Some(80_000),
                post_compaction_tokens: Some(20_000),
                tokens_removed: Some(60_000),
                ..Default::default()
            },
        };
        let request = request.request;
        Ok(BackendOutput {
            event: vec![BackendEvent::from_compaction(compaction, None)],
            backend_session_id: request.backend_session_id,
            capability: mock_capability(),
            runtime: BackendRuntime {
                provider: "Mock backend".into(),
                model: Some(request.model),
            },
            metrics: TurnMetrics {
                context_usage: ContextUsage::reported(20_000, 100_000),
                ..TurnMetrics::default()
            },
            ..BackendOutput::default()
        })
    }

    async fn model_list(&self, _request: BackendRequest) -> Result<Vec<BackendModel>> {
        Ok(vec![BackendModel {
            service_tiers: vec![
                ServiceTier::Standard,
                ServiceTier::Fast,
                ServiceTier::Ultrafast,
            ],
            id: "mock-model".into(),
            reasoning: vec!["low".into(), "medium".into(), "high".into()],
            default_reasoning: Some("medium".into()),
            selected_reasoning: None,
            context_window: vec![BackendContextWindow {
                id: "default".into(),
                token_limit: Some(100_000),
            }],
            default_context_window: Some("default".into()),
            selected_context_window: None,
            vision: false,
            description: Some("Deterministic Harness test model.".into()),
            is_default: true,
        }])
    }
}

pub(crate) fn mock_plan_document_from_prompt(prompt: &str) -> Option<crate::plan::PlanDocument> {
    let mut remaining = prompt;
    while let Some(offset) = remaining.find("```json\n") {
        let json_start = offset + "```json\n".len();
        let relative_end = remaining[json_start..].find("\n```")?;
        let json_end = json_start + relative_end;
        if let Ok(document) = serde_json::from_str(&remaining[json_start..json_end]) {
            return Some(document);
        }
        remaining = &remaining[json_end + "\n```".len()..];
    }
    None
}

/// Describe the synthetic backend used by isolated Harness workflow fixtures.
pub(crate) fn mock_capability() -> BackendCapability {
    BackendCapability {
        os_sandbox: BackendKind::Mock.supports_sandbox(),
        native_fork: true,
        native_compact: true,
        compaction: CompactionCapability {
            lifecycle: true,
            background: false,
            summary: true,
            metrics: true,
        },
        native_steer: true,
        native_turn_rollback: true,
        native_goal: true,
        model_selection: true,
        effort_selection: true,
        fast_mode: true,
        ultrafast_mode: true,
        permission_control: true,
        background_terminals: true,
        execution_mode_list: vec![
            PermissionMode::Read,
            PermissionMode::Write,
            PermissionMode::Yolo,
        ],
        agent: AgentCapability::default(),
        catalog: CatalogCapability::default(),
    }
}

#[cfg(test)]
mod test {
    #[test]
    fn sandbox_allowlist_preserves_codex_preferences_and_disables_other_providers() {
        for provider in [
            super::BackendKind::Codex,
            super::BackendKind::Copilot,
            super::BackendKind::Claude,
            super::BackendKind::Antigravity,
            super::BackendKind::Mock,
        ] {
            let requested = crate::session::AccessPolicy::default();
            let effective = provider.resolve_access(requested.clone());
            assert_eq!(
                provider.supports_sandbox(),
                provider == super::BackendKind::Codex
            );
            assert_eq!(effective.sandbox, provider.supports_sandbox());
            assert!(requested.sandbox);
            assert_eq!(effective.write_access, requested.write_access);
            assert_eq!(effective.writable_directory, requested.writable_directory);
            assert_eq!(effective.windows_sandbox, requested.windows_sandbox);
            provider.validate_access(&effective).unwrap();
            let disabled = crate::session::AccessPolicy {
                sandbox: false,
                ..requested
            };
            assert!(!provider.resolve_access(disabled).sandbox);
        }
    }

    #[test]
    fn provider_instructions_describe_staged_edits_and_revision_scoped_reports() {
        let prompt = super::HARNESS_SYSTEM_MESSAGE;
        assert!(prompt.contains("design_patch"));
        assert!(!prompt.contains("harness_plan_task_report"));
        assert!(!prompt.contains("harness_plan_edit"));
    }
    use super::{Backend, BackendRequest, MockBackend, PromptMode};
    use crate::session::PermissionMode;
    use std::sync::Arc;
    use std::time::Duration;

    #[tokio::test]
    async fn applies_steering_to_the_same_planning_turn() {
        let directory = tempfile::tempdir().unwrap();
        let store = crate::backend::submission::SubmissionStore::open(
            directory.path(),
            Arc::new(crate::trace::TraceStore::open(directory.path()).unwrap()),
        )
        .unwrap();
        let submission = store
            .prepare_steering(
                &super::BackendCatalogRequest {
                    execution_owner: None,
                    harness_session_id: "harness-session".into(),
                    workspace: ".".into(),
                    access: Default::default(),
                    execution_mode: PermissionMode::Read,
                    backend_session_id: None,
                },
                super::BackendInput::from_text("And be sure to modify Y"),
                super::BackendKind::Mock,
            )
            .unwrap();
        let input = super::SteeringInput::User {
            text: "And be sure to modify Y".into(),
            attachments: Vec::new(),
            submission: Some(submission.clone()),
        };
        let backend = Arc::new(MockBackend {
            lsp_probe: false,
            retained_control: None,
            permission: None,
            delay: Some(Duration::from_millis(50)),
            emit_before_delay: false,
            write_before_delay: false,
            fork_delay: None,
            steering_by_session: Default::default(),
        });
        let prompt_backend = Arc::clone(&backend);
        let prompt = tokio::spawn(async move {
            prompt_backend
                .prompt_stream(
                    BackendRequest {
                        execution_owner: None,
                        harness_session_id: "harness-session".into(),
                        workspace: ".".into(),
                        input: super::BackendInput::from_text(
                            "Active canonical PlanDocument:\n```json\n{\"schema_version\":5,\"version\":1,\"plan_id\":\"plan\",\"title\":\"Refactor X\",\"overview\":\"Planning\",\"usage\":null,\"entity_changes\":[],\"dependencies\":[],\"flows\":[],\"stages\":[],\"assumptions\":[]}\n```\n\nRefactor X",
                        ),
                        mode: PromptMode::Plan,
                        model: "mock-model".into(),
                        effort: "low".into(),
                        context_window: None,
                        service_tier: crate::backend::ServiceTier::Standard,
                        access: Default::default(),
                        execution_mode: PermissionMode::Read,
                        backend_session_id: None,
                        submission: None,
                        recovered_input: Vec::new(),
                        control_context: None,
                    },
                    None,
                )
                .await
        });
        loop {
            match backend
                .steer_session("harness-session", input.clone())
                .await
            {
                Ok(()) => break,
                Err(error) if format!("{error:#}").contains("no active turn") => {
                    tokio::task::yield_now().await;
                }
                Err(error) => panic!("unexpected steering failure: {error:#}"),
            }
        }
        let output = prompt.await.unwrap().unwrap();
        assert!(matches!(
            submission.record().unwrap().delivery,
            crate::backend::submission::SubmissionState::Accepted { .. }
        ));
        let text = output.plan_edit[0]
            .mutation
            .plan
            .as_ref()
            .and_then(|plan| plan.overview.as_deref())
            .expect("mock planning should update the overview");
        assert!(text.contains("Refactor X"));
        assert!(text.contains("And be sure to modify Y"));
        assert!(!text.contains("Active canonical PlanDocument"));
        assert!(!text.contains("schema_version"));
    }
}
