pub mod acp;
pub mod codex;
mod json_rpc;
mod process;

use crate::goal::TurnEvidence;
use crate::session::WriteMode;
use anyhow::Result;
use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use tokio::sync::mpsc::UnboundedSender;

/// Represents backend features that control visible Harness actions.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct BackendCapability {
    pub native_fork: bool,
    pub native_goal: bool,
    pub native_plan: bool,
    pub model_selection: bool,
    pub effort_selection: bool,
    pub permission_control: bool,
}

/// Represents one executable backend launch descriptor.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct BackendLaunch {
    pub kind: String,
    pub command: Vec<String>,
}

/// Represents the write operations automatically approved in WRITE mode.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct TrustPolicy {
    pub allow_workspace_write: bool,
    pub allow_command: bool,
    pub allow_network: bool,
    pub allow_outside_workspace: bool,
    pub allow_git_index: bool,
    pub allow_git_history: bool,
    pub allow_elevation: bool,
}

impl Default for TrustPolicy {
    fn default() -> Self {
        Self {
            allow_workspace_write: true,
            allow_command: true,
            allow_network: false,
            allow_outside_workspace: false,
            allow_git_index: false,
            allow_git_history: false,
            allow_elevation: false,
        }
    }
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
    pub write_mode: WriteMode,
    pub trust_profile: String,
    pub trust_policy: TrustPolicy,
    pub backend_session_id: Option<String>,
}

/// Represents a streamed backend update normalized for the Neovim transcript.
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
    pub plan_progress: Option<PlanProgress>,
}

/// Represents one provider plan as stable progress state for transcript consumers.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct PlanProgress {
    pub id: String,
    pub name: Option<String>,
    pub status: String,
    pub step_list: Vec<PlanStep>,
}

/// Represents one checklist step within normalized provider plan progress.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct PlanStep {
    pub text: String,
    pub status: String,
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
    pub output_delta: bool,
}

/// Defines the visible action verb for one normalized tool invocation.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum ToolActivityKind {
    Command,
    FileChange,
    ToolCall,
}

/// Routes normalized provider updates to the live Neovim transcript.
pub type BackendEventSink = UnboundedSender<BackendEvent>;

/// Represents the normalized result of one complete backend turn.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct BackendOutput {
    pub backend_session_id: Option<String>,
    pub event: Vec<BackendEvent>,
    pub plan_markdown: Option<String>,
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
}

/// Build a backend strategy from an explicit launch descriptor.
pub fn build(launch: BackendLaunch) -> Result<Box<dyn Backend>> {
    match launch.kind.as_str() {
        "acp" => Ok(Box::new(acp::AcpBackend::new(launch.command)?)),
        "codex" => Ok(Box::new(codex::CodexBackend::new(launch.command)?)),
        "mock" => Ok(Box::new(MockBackend)),
        kind => anyhow::bail!("unsupported Harness backend: {kind}"),
    }
}

struct MockBackend;

#[async_trait]
impl Backend for MockBackend {
    async fn prompt_stream(
        &self,
        request: BackendRequest,
        event_sink: Option<BackendEventSink>,
    ) -> Result<BackendOutput> {
        let plan_markdown =
            (request.mode == PromptMode::Plan).then(|| format!("# Plan\n\n1. {}\n", request.text));
        let event = BackendEvent {
            kind: "assistant_message".into(),
            text: Some(format!("Mock response: {}", request.text)),
            data: Value::Null,
            activity: None,
            summary: None,
            plan_progress: None,
        };
        if let Some(event_sink) = event_sink {
            let _ = event_sink.send(event.clone());
        }
        Ok(BackendOutput {
            backend_session_id: request
                .backend_session_id
                .or_else(|| Some("mock-session".into())),
            event: vec![event],
            plan_markdown,
            evidence: TurnEvidence {
                tool_called: request.write_mode == WriteMode::Write,
                structured_complete: matches!(request.mode, PromptMode::GoalContinuation),
                ..TurnEvidence::default()
            },
            capability: BackendCapability {
                native_fork: true,
                native_goal: true,
                native_plan: true,
                model_selection: true,
                effort_selection: true,
                permission_control: true,
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

    async fn model_list(&self, _request: BackendRequest) -> Result<Vec<BackendModel>> {
        Ok(vec![BackendModel {
            id: "mock-model".into(),
            label: "Mock model".into(),
            effort: vec!["low".into(), "medium".into(), "high".into()],
            is_default: true,
        }])
    }
}
