mod auto_compaction;
use anyhow::Result;
pub use auto_compaction::AutoCompaction;
use serde::{Deserialize, Serialize};

mod access;
mod shell;
mod timeout;
pub use access::{AccessPolicy, DirectoryScope, WindowsSandbox, WriteScope};
pub use shell::CheckShell;
pub(crate) use timeout::InputSchedule;
pub use timeout::{InputDeadline, InputTimeout, ResolutionSource, TimeoutUnit};
pub mod continuation;
pub mod state_machine;
use std::collections::BTreeMap;

/// Defines the approval policy selected for one Harness session.
#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum ApprovalMode {
    #[default]
    /// Apply rules and request a decision for remaining tool requests.
    Ask,
    /// Reject tool requests that still require a decision.
    Deny,
    /// Approve remaining tool requests once, preserving explicit denials.
    Allow,
}

impl ApprovalMode {
    /// Return the stable user-facing label for this approval policy.
    pub const fn label(self) -> &'static str {
        match self {
            Self::Ask => "ask",
            Self::Deny => "deny",
            Self::Allow => "allow",
        }
    }
}

#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub struct ContextUsage {
    pub used: u64,
    pub size: u64,
    pub remaining_percent: u8,
}

/// Captures the provider-owned boundary used to materialize one fork.
#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub struct ProviderForkPoint {
    pub backend_session_id: Option<String>,
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

impl ContextUsage {
    const CODEX_BASELINE_TOKENS: u64 = 12_000;

    pub fn codex(used: u64, size: u64) -> Option<Self> {
        Self::with_baseline(used, size, Self::CODEX_BASELINE_TOKENS)
    }

    pub fn reported(used: u64, size: u64) -> Option<Self> {
        Self::with_baseline(used, size, 0)
    }

    fn with_baseline(used: u64, size: u64, baseline: u64) -> Option<Self> {
        let effective_size = size.checked_sub(baseline)?;
        if effective_size == 0 {
            return None;
        }
        let effective_used = used.saturating_sub(baseline);
        let remaining = effective_size.saturating_sub(effective_used);
        let remaining_percent = ((remaining as f64 / effective_size as f64) * 100.0)
            .clamp(0.0, 100.0)
            .round() as u8;
        Some(Self {
            used,
            size,
            remaining_percent,
        })
    }
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

/// Represents one durable conversation and its backend identity.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct HarnessSession {
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
    pub current_task_id: Option<String>,
    #[serde(default)]
    pub default_approval: ApprovalMode,
    #[serde(default)]
    pub plan_approval: Option<ApprovalMode>,
    pub created_at_ms: i64,
    pub updated_at_ms: i64,
    pub goal_id: Option<String>,
    pub lease_owner: Option<String>,
    #[serde(default)]
    pub native_fork: bool,
    #[serde(default)]
    pub native_compact: bool,
    #[serde(default)]
    pub context_usage: Option<ContextUsage>,
}

/// Stores the last model controls selected for one backend and workspace.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct HarnessPreference {
    #[serde(default)]
    pub access: crate::session::AccessPolicy,
    #[serde(default)]
    pub shell: CheckShell,
    #[serde(default)]
    pub default_approval: ApprovalMode,
    #[serde(default)]
    pub plan_approval: Option<ApprovalMode>,
    pub model: String,
    pub effort: String,
    #[serde(default)]
    pub model_setting: BTreeMap<String, ModelSetting>,
    #[serde(default)]
    pub service_tier: crate::backend::ServiceTier,
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
}

/// Selects the model used after a plan is accepted.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
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

#[cfg(test)]
mod context_usage_test {
    use super::ContextUsage;

    #[test]
    fn normalizes_codex_context_against_the_reserved_baseline() {
        let usage = ContextUsage::codex(275_000, 353_000).expect("valid Codex usage");
        assert_eq!(usage.remaining_percent, 23);
        assert_eq!(usage.size, 353_000);
        assert!(ContextUsage::codex(1, 12_000).is_none());
    }

    #[test]
    fn preserves_provider_context_as_reported_by_the_backend() {
        let usage = ContextUsage::reported(156_000, 200_000).expect("valid reported usage");
        assert_eq!(usage.remaining_percent, 22);
    }
}

fn auto_approve_revisions() -> bool {
    true
}
