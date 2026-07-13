use anyhow::Result;
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub struct ContextUsage {
    pub used: u64,
    pub size: u64,
    pub remaining_percent: u8,
}

impl ContextUsage {
    const CODEX_BASELINE_TOKENS: u64 = 12_000;

    pub fn codex(used: u64, size: u64) -> Option<Self> {
        Self::with_baseline(used, size, Self::CODEX_BASELINE_TOKENS)
    }

    pub fn acp(used: u64, size: u64) -> Option<Self> {
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

/// Represents the write authority enforced for one harness session.
#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum WriteMode {
    #[default]
    Read,
    Write,
}

/// Represents one durable conversation and its backend identity.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct HarnessSession {
    pub id: String,
    pub name: String,
    pub workspace: String,
    pub backend: String,
    pub backend_session_id: Option<String>,
    pub model: String,
    #[serde(default)]
    pub provider_label: String,
    #[serde(default)]
    pub resolved_model: Option<String>,
    pub effort: String,
    #[serde(default)]
    pub fast_mode: bool,
    pub trust_profile: String,
    pub write_mode: WriteMode,
    pub created_at_ms: i64,
    pub updated_at_ms: i64,
    pub active_plan_id: Option<String>,
    pub goal_id: Option<String>,
    pub lease_owner: Option<String>,
    pub lease_expires_at_ms: Option<i64>,
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
    pub model: String,
    pub effort: String,
    #[serde(default)]
    pub fast_mode: bool,
}

impl HarnessSession {
    /// Acquire or renew this session lease for one Neovim client.
    pub fn acquire_lease(&mut self, client_id: &str, now_ms: i64) -> Result<()> {
        let lease_active = self.lease_expires_at_ms.unwrap_or(0) > now_ms;
        let owned_elsewhere = self
            .lease_owner
            .as_deref()
            .is_some_and(|owner| owner != client_id);
        if lease_active && owned_elsewhere {
            return Err(SessionLeaseConflict {
                session_id: self.id.clone(),
                native_fork: self.native_fork,
            }
            .into());
        }
        self.lease_owner = Some(client_id.into());
        self.lease_expires_at_ms = Some(now_ms + 30_000);
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
    fn preserves_acp_context_as_reported_by_the_agent() {
        let usage = ContextUsage::acp(156_000, 200_000).expect("valid ACP usage");
        assert_eq!(usage.remaining_percent, 22);
    }
}
