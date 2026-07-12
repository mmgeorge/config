use anyhow::Result;
use serde::{Deserialize, Serialize};
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
