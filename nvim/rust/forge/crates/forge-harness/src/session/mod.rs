mod auto_compaction;
use anyhow::Result;
pub use auto_compaction::AutoCompaction;
use serde::{Deserialize, Serialize};

mod shell;
mod timeout;
pub use forge_provider_api::access::{AccessPolicy, DirectoryScope, WriteScope};
pub use shell::CheckShell;
pub(crate) use timeout::InputSchedule;
pub use timeout::{InputDeadline, InputTimeout, InputTimeoutError, ResolutionSource, TimeoutUnit};
pub mod continuation;
pub mod state_machine;
use std::collections::BTreeMap;

/// Captures the provider-owned boundary used to materialize one fork.
#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub struct ProviderForkPoint {
    #[serde(alias = "backend_session_id")]
    pub provider_session_id: Option<String>,
    pub checkpoint_id: Option<String>,
}

/// Tracks whether one Harness session can accept provider-bound work.
#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "state", rename_all = "snake_case")]
pub enum ProviderForkState {
    #[default]
    Ready,
    Preparing {
        source_session_id: String,
        point: ProviderForkPoint,
    },
    Failed {
        source_session_id: String,
        point: ProviderForkPoint,
        message: String,
    },
}

use std::fmt;

/// Represents an active session lease held by another Neovim client.
#[derive(Clone, Debug)]
pub struct SessionLeaseConflict {
    pub session_id: String,
    pub native_fork: bool,
}

impl fmt::Display for SessionLeaseConflict {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str("session is controlled by another Neovim instance")
    }
}

impl std::error::Error for SessionLeaseConflict {}

/// Distinguishes resumable user conversations from internal answer forks.
#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum SessionPurpose {
    #[default]
    Conversation,
    PlanReviewAnswer,
}

/// Represents one durable conversation and its provider identity.
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
    #[serde(alias = "backend")]
    pub provider: String,
    #[serde(alias = "backend_session_id")]
    pub provider_session_id: Option<String>,
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
    #[serde(default)]
    /// Limits each pending approval independently.
    pub approval_timeout: InputTimeout,
    /// Limits each tool invocation and shell command independently.
    #[serde(default)]
    pub tool_timeout: InputTimeout,
    #[serde(default)]
    pub context_window: Option<String>,
    #[serde(default)]
    pub service_tier: crate::provider::ServiceTier,
    #[serde(default)]
    pub access: crate::session::AccessPolicy,
    #[serde(default)]
    pub shell: CheckShell,
    pub approval_mode: ApprovalMode,
    #[serde(default)]
    pub active_task: Option<crate::task::Task>,
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

/// Stores the last model controls selected for one provider and workspace.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct HarnessPreference {
    #[serde(default)]
    pub access: crate::session::AccessPolicy,
    #[serde(default)]
    pub shell: CheckShell,
    pub model: String,
    pub effort: String,
    #[serde(default)]
    pub model_setting: BTreeMap<String, ModelSetting>,
    #[serde(default)]
    pub service_tier: crate::provider::ServiceTier,
    #[serde(default)]
    pub plan_executor: PlanExecutor,
    #[serde(default)]
    pub plan_compact: bool,
    #[serde(default)]
    /// Limits each pending approval independently.
    pub approval_timeout: InputTimeout,
    /// Limits each tool invocation and shell command independently.
    #[serde(default)]
    pub tool_timeout: InputTimeout,
}

/// Selects the model used after a plan is accepted.
#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub struct PlanExecutor {
    pub enabled: bool,
    pub model: Option<String>,
    pub effort: Option<String>,
}

/// Stores durable controls selected for one provider model.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct ModelSetting {
    pub reasoning: Option<String>,
    pub context_window: Option<String>,
}

impl HarnessSession {
    /// Derive the persistent primary-agent identity from a session identity.
    pub fn primary_agent_id(session_id: &str) -> String {
        format!("{session_id}:agent:primary")
    }

    /// Claims an unowned session or confirms its existing owner without clock-based expiry.
    pub fn acquire_lease(&mut self, client_id: &str) -> Result<()> {
        let owned_elsewhere = self
            .lease_owner
            .as_deref()
            .is_some_and(|owner| owner != client_id);
        if owned_elsewhere {
            return Err(SessionLeaseConflict {
                session_id: self.id.clone(),
                native_fork: self.native_fork,
            }
            .into());
        }
        self.lease_owner = Some(client_id.into());
        Ok(())
    }
}

/// Defines durable session operations consumed by the broker.
pub trait SessionStore {
    /// Write a new or changed session into durable storage.
    fn save_session(&mut self, session: &HarnessSession) -> Result<()>;

    /// Load one session by its stable harness identifier.
    fn load_session(&self, session_id: &str) -> Result<Option<HarnessSession>>;

    /// Load sessions in reverse activity order for repository and global browsing.
    fn list_session(&self, workspace: Option<&str>) -> Result<Vec<HarnessSession>>;

    /// Delete Harness-owned state without deleting the provider conversation.
    fn delete_session(&mut self, session_id: &str) -> Result<()>;
}

pub use forge_provider_api::approval::ApprovalMode;

/// Retain the session's latest context window with Harness's derived remaining capacity.
#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub struct ContextUsage {
    pub used: u64,
    pub size: u64,
    /// Percent of the window beyond the provider's reserved overhead that remains unused.
    pub remaining_percent: u8,
}

impl ContextUsage {
    /// Derive the remaining capacity from one provider context report.
    pub fn from_report(report: &forge_provider_api::context::ContextUsage) -> Option<Self> {
        let effective_size = report.size.checked_sub(report.reserved)?;
        if effective_size == 0 {
            return None;
        }
        let effective_used = report.used.saturating_sub(report.reserved);
        let remaining = effective_size.saturating_sub(effective_used);
        let remaining_percent = ((remaining as f64 / effective_size as f64) * 100.0)
            .clamp(0.0, 100.0)
            .round() as u8;
        Some(Self {
            used: report.used,
            size: report.size,
            remaining_percent,
        })
    }

    /// Derive usage for a window that reserves no native overhead.
    pub fn reported(used: u64, size: u64) -> Option<Self> {
        Self::from_report(&forge_provider_api::context::ContextUsage::reported(
            used, size,
        )?)
    }
}
