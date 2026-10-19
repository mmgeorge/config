use serde::{Deserialize, Serialize};

use crate::turn::TurnOutcome;

#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
/// Identifies the root session and the single agent that owns a provider runtime.
pub struct ExecutionOwner {
    /// Identifies the visible Harness session shared by the root and children.
    pub session_id: String,
    /// Identifies the independent conversation owner within that session.
    pub agent_id: String,
}

impl ExecutionOwner {
    /// Derive an adapter lookup key while preserving legacy root-session controls.
    pub fn runtime_key(&self) -> String {
        if self.is_root() {
            self.session_id.clone()
        } else {
            format!("{}:agent:{}", self.session_id, self.agent_id)
        }
    }
    /// Resolve the primary conversation without selecting any child runtime.
    pub fn root(session_id: impl Into<String>) -> Self {
        let session_id = session_id.into();
        Self {
            agent_id: crate::session::HarnessSession::primary_agent_id(&session_id),
            session_id,
        }
    }

    /// Validate an explicit owner before using it as a provider connection key.
    pub(crate) fn resolve(owner: &Option<Self>, session_id: &str) -> anyhow::Result<Self> {
        let owner = owner.clone().unwrap_or_else(|| Self::root(session_id));
        anyhow::ensure!(
            owner.session_id == session_id && !owner.agent_id.is_empty(),
            "provider execution owner does not belong to the Harness session"
        );
        Ok(owner)
    }

    /// Distinguish child authority from the root's workflow authority.
    pub fn is_root(&self) -> bool {
        self.agent_id == crate::session::HarnessSession::primary_agent_id(&self.session_id)
    }
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
/// Identifies exactly one execution in one provider conversation.
pub struct ProviderAddress {
    /// Retains exact agent ownership when multiple conversations share a provider.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub owner: Option<ExecutionOwner>,
    /// Provider conversation containing the turn.
    pub thread_id: String,
    /// Provider execution identifier, never inferred from a UI segment.
    pub turn_id: String,
}

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
/// A provider lifecycle boundary normalized before broker routing.
pub enum TurnBoundary {
    /// The provider admitted a new execution.
    Started,
    /// The provider permanently settled an execution.
    Finished {
        /// Execution outcome reported by the adapter.
        outcome: TurnOutcome,
    },
}
