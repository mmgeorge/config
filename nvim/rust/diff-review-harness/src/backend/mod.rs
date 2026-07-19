pub mod approval;
mod catalog;
pub mod codex;
pub mod copilot;
mod steering;
pub use catalog::{
    BackendCatalogRequest, BackendInput, CatalogCapability, CatalogMutation, McpDefinition,
    McpStatus, McpToolDefinition, SkillDefinition,
};
pub use steering::SteerTarget;

use crate::agent::AgentCapability;
use crate::goal::TurnEvidence;
use crate::plan::{PlanQuestionAnswer, PlanQuestionSet, PlanQuestionWithdrawal};
use crate::session::{ContextUsage, ExecutionMode};
use anyhow::Result;
use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::time::Duration;
use tokio::sync::mpsc::UnboundedSender;

/// Identifies one supported provider implementation without leaking launch strings across consumers.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum BackendKind {
    Codex,
    Copilot,
    Mock,
}

impl BackendKind {
    /// Parse one persisted backend name into the supported provider set.
    pub fn parse(value: &str) -> Result<Self> {
        match value {
            "codex" => Ok(Self::Codex),
            "copilot" => Ok(Self::Copilot),
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
    pub native_fork: bool,
    pub native_compact: bool,
    pub native_steer: bool,
    pub native_turn_rollback: bool,
    pub native_goal: bool,
    pub model_selection: bool,
    pub effort_selection: bool,
    pub fast_mode: bool,
    pub permission_control: bool,
    #[serde(default)]
    pub execution_mode_list: Vec<ExecutionMode>,
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
    Chat,
    Plan,
    ExecutePlan,
    GoalContinuation,
    RequestChanges,
}

/// Represents one prompt sent across a backend boundary.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct BackendRequest {
    pub harness_session_id: String,
    pub workspace: String,
    pub input: BackendInput,
    pub mode: PromptMode,
    pub model: String,
    pub effort: String,
    pub context_window: Option<String>,
    pub fast_mode: bool,
    pub execution_mode: ExecutionMode,
    pub backend_session_id: Option<String>,
}

/// Represents a streamed backend update normalized for the interaction reducer.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct BackendEvent {
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
    /// Build the canonical event for provider-acknowledged active-turn input.
    pub(crate) fn steering_input(text: String) -> Self {
        Self {
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
    pub token_count: Option<u64>,
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
pub type BackendEventSink = UnboundedSender<BackendEvent>;

/// Represents the normalized result of one complete backend turn.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct BackendOutput {
    pub backend_session_id: Option<String>,
    pub event: Vec<BackendEvent>,
    pub plan_markdown: Option<String>,
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

/// Tracks provider-reported metrics while one backend turn streams.
#[derive(Clone, Debug, Default)]
pub struct TurnMetrics {
    pub token_count: Option<u64>,
    pub context_usage: Option<ContextUsage>,
    pub native_compact_update: Option<bool>,
}

/// Represents the provider identity and resolved model shown by Harness.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct BackendRuntime {
    pub provider: String,
    pub model: Option<String>,
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

/// Defines the prompt, fork, and capability operations consumed by the broker.
#[async_trait]
pub trait Backend: Send + Sync {
    /// Return stable provider identity and capability without starting a provider process.
    fn descriptor(&self) -> BackendDescriptor {
        BackendDescriptor {
            kind: BackendKind::Mock,
            label: "Mock CLI".into(),
            capability: mock_capability(),
        }
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
    async fn fork(&self, _request: BackendRequest) -> Result<BackendForkResult> {
        anyhow::bail!("backend does not support session fork")
    }

    /// List provider models for the Harness model picker.
    async fn model_list(&self, _request: BackendRequest) -> Result<Vec<BackendModel>> {
        Ok(Vec::new())
    }

    /// List user-invocable provider skills with their effective enabled state.
    async fn skill_list(&self, _request: BackendCatalogRequest) -> Result<Vec<SkillDefinition>> {
        Ok(Vec::new())
    }

    /// Persist one provider skill's enabled state for future turns.
    async fn set_skill_enabled(
        &self,
        _request: BackendCatalogRequest,
        _name: &str,
        _enabled: bool,
    ) -> Result<CatalogMutation> {
        anyhow::bail!("backend does not support skill configuration")
    }

    /// List complete provider MCP definitions including cached tool metadata.
    async fn mcp_list(&self, _request: BackendCatalogRequest) -> Result<Vec<McpDefinition>> {
        Ok(Vec::new())
    }

    /// Persist one MCP server's enabled state and report whether its turn must resume.
    async fn set_mcp_enabled(
        &self,
        _request: BackendCatalogRequest,
        _name: &str,
        _enabled: bool,
    ) -> Result<CatalogMutation> {
        anyhow::bail!("backend does not support MCP configuration")
    }

    /// Return whether one Harness session currently owns an in-flight provider turn.
    async fn has_active_turn(&self, _session_id: &str) -> bool {
        false
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
    async fn compact(&self, _request: BackendRequest) -> Result<BackendOutput> {
        anyhow::bail!("backend does not support context compaction")
    }

    /// Append user input to the active provider turn when native steering is available.
    async fn steer(&self, _text: String) -> Result<()> {
        anyhow::bail!("backend does not support active-turn steering")
    }

    /// Append user input to one Harness session's active provider turn.
    async fn steer_session(&self, _session_id: &str, text: String) -> Result<()> {
        self.steer(text).await
    }

    /// Append user input to one specific provider child turn.
    async fn steer_target(&self, text: String, _target: SteerTarget) -> Result<()> {
        self.steer(text).await
    }

    /// Interrupt one specific provider child turn without cancelling its parent.
    async fn interrupt_target(&self, _target: SteerTarget) -> Result<()> {
        anyhow::bail!("backend does not support targeted child interruption")
    }

    /// Stop the active provider transport after its prompt future is cancelled.
    async fn cancel(&self) -> Result<()> {
        Ok(())
    }

    /// Stop one Harness session's active provider transport.
    async fn cancel_session(&self, _session_id: &str) -> Result<()> {
        self.cancel().await
    }

    /// Return the provider conversation that can continue after an interrupted turn.
    async fn active_session_id(&self) -> Option<String> {
        None
    }

    /// Return one Harness session's provider conversation after interruption.
    async fn active_session_id_for(&self, _session_id: &str) -> Option<String> {
        self.active_session_id().await
    }

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
) -> Result<Box<dyn Backend>> {
    match BackendKind::parse(&launch.kind)? {
        BackendKind::Codex => Ok(Box::new(
            codex::CodexBackend::new_with_permission_coordinator(
                launch.command,
                permission_coordinator,
            )?,
        )),
        BackendKind::Copilot => Ok(Box::new(
            copilot::CopilotBackend::new_with_permission_coordinator(
                launch.command,
                permission_coordinator,
            )?,
        )),
        BackendKind::Mock => Ok(Box::new(MockBackend {
            delay: match launch.command.first().map(String::as_str) {
                Some("blocking" | "visible-blocking" | "writing-blocking") => {
                    Some(Duration::from_secs(60))
                }
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
            steering: steering::SteeringLane::default(),
        })),
    }
}

struct MockBackend {
    delay: Option<Duration>,
    emit_before_delay: bool,
    write_before_delay: bool,
    steering: steering::SteeringLane,
}

#[async_trait]
impl Backend for MockBackend {
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
        let mut active_steering = self.steering.activate(event_sink.clone())?;
        let mut steering_text_list = Vec::new();
        let event = BackendEvent {
            kind: "assistant_message".into(),
            text: Some(format!("Mock response: {}", request.input.text())),
            data: Value::Null,
            activity: None,
            summary: None,
            task_update: None,
        };
        if self.emit_before_delay
            && let Some(event_sink) = event_sink.as_ref()
        {
            let _ = event_sink.send(event.clone());
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
                        steering_text_list.push(command.text.clone());
                        command.complete(Ok(()));
                    }
                }
            }
        }
        let plan_markdown = (request.mode == PromptMode::Plan).then(|| {
            let steering = steering_text_list
                .iter()
                .map(|text| format!("- {text}"))
                .collect::<Vec<_>>()
                .join("\n");
            if steering.is_empty() {
                format!("# Plan\n\n1. {}\n", request.input.text())
            } else {
                format!(
                    "# Plan\n\n1. {}\n\n## Steering constraints\n\n{steering}\n",
                    request.input.text()
                )
            }
        });
        if !self.emit_before_delay
            && let Some(event_sink) = event_sink
        {
            let _ = event_sink.send(event.clone());
        }
        Ok(BackendOutput {
            backend_session_id: request
                .backend_session_id
                .or_else(|| Some("mock-session".into())),
            event: vec![event],
            plan_markdown,
            plan_question: None,
            question_answer: None,
            question_withdrawal: None,
            control_error: None,
            evidence: TurnEvidence {
                tool_called: request.execution_mode.permits_workspace_write(),
                structured_complete: matches!(request.mode, PromptMode::GoalContinuation),
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

    async fn fork(&self, request: BackendRequest) -> Result<BackendForkResult> {
        Ok(BackendForkResult::unprofiled(format!(
            "{}-fork",
            request
                .backend_session_id
                .unwrap_or_else(|| "mock-session".into())
        )))
    }

    async fn steer(&self, text: String) -> Result<()> {
        self.steering.steer(text).await
    }

    async fn rollback_cancelled_turn(&self) -> Result<bool> {
        Ok(true)
    }

    async fn compact(&self, request: BackendRequest) -> Result<BackendOutput> {
        Ok(BackendOutput {
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

fn mock_capability() -> BackendCapability {
    BackendCapability {
        native_fork: true,
        native_compact: true,
        native_steer: true,
        native_turn_rollback: true,
        native_goal: true,
        model_selection: true,
        effort_selection: true,
        fast_mode: true,
        permission_control: true,
        execution_mode_list: vec![
            ExecutionMode::Read,
            ExecutionMode::Write,
            ExecutionMode::Full,
            ExecutionMode::Yolo,
        ],
        agent: AgentCapability::default(),
        catalog: CatalogCapability::default(),
    }
}

#[cfg(test)]
mod test {
    use super::{Backend, BackendRequest, MockBackend, PromptMode, steering};
    use crate::session::ExecutionMode;
    use std::sync::Arc;
    use std::time::Duration;

    #[tokio::test]
    async fn applies_steering_to_the_same_planning_turn() {
        let backend = Arc::new(MockBackend {
            delay: Some(Duration::from_millis(50)),
            emit_before_delay: false,
            write_before_delay: false,
            steering: steering::SteeringLane::default(),
        });
        let prompt_backend = Arc::clone(&backend);
        let prompt = tokio::spawn(async move {
            prompt_backend
                .prompt_stream(
                    BackendRequest {
                        harness_session_id: "harness-session".into(),
                        workspace: ".".into(),
                        input: super::BackendInput::from_text("Refactor X"),
                        mode: PromptMode::Plan,
                        model: "mock-model".into(),
                        effort: "low".into(),
                        context_window: None,
                        fast_mode: false,
                        execution_mode: ExecutionMode::Read,
                        backend_session_id: None,
                    },
                    None,
                )
                .await
        });
        loop {
            match backend.steer("And be sure to modify Y".into()).await {
                Ok(()) => break,
                Err(error) if format!("{error:#}").contains("no active turn") => {
                    tokio::task::yield_now().await;
                }
                Err(error) => panic!("unexpected steering failure: {error:#}"),
            }
        }
        let output = prompt.await.unwrap().unwrap();
        let plan = output.plan_markdown.unwrap();
        assert!(plan.contains("Refactor X"));
        assert!(plan.contains("And be sure to modify Y"));
    }
}
