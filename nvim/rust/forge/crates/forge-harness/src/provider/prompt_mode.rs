//! Harness workflow intent for one admitted prompt.

use serde::{Deserialize, Serialize};

/// Represents the broker intent for one admitted prompt.
///
/// The mode selects Harness instructions, tool authority, and workflow transitions. Providers
/// never see it; [`PromptMode::execution`] projects the one posture a native runtime can apply.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum PromptMode {
    /// Run a persistent tool-free generation request.
    Background,
    Chat,
    Plan,
    PlanDiscussion,
    /// Answer an anchored review question in a fork without plan mutations.
    PlanQuestion,
    ExecutePlan,
    GoalContinuation,
    RequestChanges,
}

impl PromptMode {
    /// Project the workflow onto the provider-neutral execution posture.
    ///
    /// Draft authoring needs ordinary file tools; only review questions request native read-only planning.
    pub fn execution(self) -> forge_provider_api::ExecutionMode {
        match self {
            Self::PlanQuestion => forge_provider_api::ExecutionMode::Planning,
            Self::Background
            | Self::Plan
            | Self::PlanDiscussion
            | Self::Chat
            | Self::ExecutePlan
            | Self::GoalContinuation
            | Self::RequestChanges => forge_provider_api::ExecutionMode::Default,
        }
    }
}
