use crate::{ProviderCapability, ProviderEvent, ProviderRuntime, context::ContextUsage};
use serde::{Deserialize, Serialize};

/// Represents the lifecycle state of one persistent harness goal.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum GoalState {
    Active,
    Paused,
    Complete,
    Blocked,
    UsageLimited,
    BudgetLimited,
    Stalled,
    Cleared,
}

/// Represents observable evidence produced by one provider turn.
#[derive(Clone, Copy, Debug, Default, Deserialize, Serialize)]
pub struct TurnEvidence {
    pub workspace_changed: bool,
    pub tool_called: bool,
    #[serde(default)]
    pub native_state: Option<GoalState>,
    pub structured_complete: bool,
    pub structured_blocked: bool,
}

/// Tracks provider-reported metrics while one provider turn streams.
#[derive(Clone, Debug, Default)]
pub struct TurnMetrics {
    pub context_usage: Option<ContextUsage>,
    pub native_compact_update: Option<bool>,
}

/// Collect native observations without host plan documents, permits, or tool implementation state.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct ProviderCompletion {
    #[serde(alias = "backend_session_id")]
    pub provider_session_id: Option<String>,
    pub provider_checkpoint_id: Option<String>,
    pub evidence: TurnEvidence,
    pub capability: ProviderCapability,
    pub runtime: ProviderRuntime,
    #[serde(skip)]
    pub metrics: TurnMetrics,
}

/// Collect normalized provider events and their settled native observations.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct ProviderOutput {
    pub event: Vec<ProviderEvent>,
    #[serde(flatten)]
    pub completion: ProviderCompletion,
}

impl crate::progress::TurnProgress for ProviderCompletion {
    fn session_id(&self) -> Option<&str> {
        self.provider_session_id.as_deref()
    }
    fn runtime(&self) -> &ProviderRuntime {
        &self.runtime
    }
    fn set_model(&mut self, model: String) {
        self.runtime.model = Some(model);
    }
    fn set_context_usage(&mut self, usage: ContextUsage) {
        self.metrics.context_usage = Some(usage);
    }
    fn record_tool_call(&mut self) {
        self.evidence.tool_called = true;
    }
    fn record_workspace_change(&mut self, changed: bool) {
        self.evidence.workspace_changed |= changed;
    }
}
