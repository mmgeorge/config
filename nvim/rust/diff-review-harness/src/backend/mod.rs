pub mod acp;
pub mod approval;
pub mod codex;
mod json_rpc;
mod process;
mod steering;
pub use steering::SteerTarget;

use crate::agent::AgentCapability;
use crate::goal::TurnEvidence;
use crate::plan::PlanQuestionSet;
use crate::session::{ContextUsage, ExecutionMode};
use anyhow::Result;
use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::time::Duration;
use tokio::sync::mpsc::UnboundedSender;

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
    pub permission_control: bool,
    #[serde(default)]
    pub execution_mode_list: Vec<ExecutionMode>,
    pub agent: AgentCapability,
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
    pub workspace: String,
    pub text: String,
    pub mode: PromptMode,
    pub model: String,
    pub effort: String,
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

/// Represents one model and its provider-advertised reasoning efforts.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct BackendModel {
    pub id: String,
    pub label: String,
    pub effort: Vec<String>,
    #[serde(default)]
    pub is_default: bool,
}

/// Defines the prompt, fork, and capability operations consumed by the broker.
#[async_trait]
pub trait Backend: Send + Sync {
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
    async fn fork(&self, request: BackendRequest) -> Result<String>;

    /// List provider models for the Harness model picker.
    async fn model_list(&self, _request: BackendRequest) -> Result<Vec<BackendModel>> {
        Ok(Vec::new())
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

    /// Roll back a cancelled turn after the broker proves it produced no visible output or changes.
    async fn rollback_cancelled_turn(&self) -> Result<bool> {
        Ok(false)
    }
}

/// Build a backend strategy from an explicit launch descriptor.
pub fn build(
    launch: BackendLaunch,
    permission_coordinator: std::sync::Arc<approval::PermissionCoordinator>,
) -> Result<Box<dyn Backend>> {
    match launch.kind.as_str() {
        "acp" => Ok(Box::new(acp::AcpBackend::new_with_permission_coordinator(
            launch.command,
            permission_coordinator,
        )?)),
        "codex" => Ok(Box::new(
            codex::CodexBackend::new_with_permission_coordinator(
                launch.command,
                permission_coordinator,
            )?,
        )),
        "mock" => Ok(Box::new(MockBackend {
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
        kind => anyhow::bail!("unsupported Harness backend: {kind}"),
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
    async fn prompt_stream(
        &self,
        request: BackendRequest,
        event_sink: Option<BackendEventSink>,
    ) -> Result<BackendOutput> {
        let mut active_steering = self.steering.activate(event_sink.clone())?;
        let mut steering_text_list = Vec::new();
        let event = BackendEvent {
            kind: "assistant_message".into(),
            text: Some(format!("Mock response: {}", request.text)),
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
                format!("# Plan\n\n1. {}\n", request.text)
            } else {
                format!(
                    "# Plan\n\n1. {}\n\n## Steering constraints\n\n{steering}\n",
                    request.text
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
            evidence: TurnEvidence {
                tool_called: request.execution_mode.permits_workspace_write(),
                structured_complete: matches!(request.mode, PromptMode::GoalContinuation),
                ..TurnEvidence::default()
            },
            capability: BackendCapability {
                native_fork: true,
                native_compact: true,
                native_steer: true,
                native_turn_rollback: true,
                native_goal: true,
                model_selection: true,
                effort_selection: true,
                permission_control: true,
                execution_mode_list: vec![
                    ExecutionMode::Read,
                    ExecutionMode::Write,
                    ExecutionMode::Full,
                    ExecutionMode::Yolo,
                ],
                agent: AgentCapability::default(),
            },
            runtime: BackendRuntime {
                provider: "Mock backend".into(),
                model: Some(request.model),
            },
            structured_plan: request.mode == PromptMode::Plan,
            metrics: TurnMetrics::default(),
        })
    }

    async fn fork(&self, request: BackendRequest) -> Result<String> {
        Ok(format!(
            "{}-fork",
            request
                .backend_session_id
                .unwrap_or_else(|| "mock-session".into())
        ))
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
            capability: BackendCapability {
                native_fork: true,
                native_compact: true,
                native_steer: true,
                native_turn_rollback: true,
                native_goal: true,
                model_selection: true,
                effort_selection: true,
                permission_control: true,
                execution_mode_list: vec![
                    ExecutionMode::Read,
                    ExecutionMode::Write,
                    ExecutionMode::Full,
                    ExecutionMode::Yolo,
                ],
                agent: AgentCapability::default(),
            },
            runtime: BackendRuntime {
                provider: "Mock backend".into(),
                model: Some(request.model),
            },
            metrics: TurnMetrics {
                context_usage: ContextUsage::acp(20_000, 100_000),
                ..TurnMetrics::default()
            },
            ..BackendOutput::default()
        })
    }

    async fn model_list(&self, _request: BackendRequest) -> Result<Vec<BackendModel>> {
        Ok(vec![BackendModel {
            id: "mock-model".into(),
            label: "Mock model".into(),
            effort: vec!["low".into(), "medium".into(), "high".into()],
            is_default: true,
        }])
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
                        workspace: ".".into(),
                        text: "Refactor X".into(),
                        mode: PromptMode::Plan,
                        model: "mock-model".into(),
                        effort: "low".into(),
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
