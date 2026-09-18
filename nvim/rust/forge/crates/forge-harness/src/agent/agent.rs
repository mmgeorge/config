use serde::{Deserialize, Serialize};
use uuid::Uuid;

/// Defines the lifecycle of one concrete child-agent run.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum AgentExecutionState {
    Starting,
    Running,
    Waiting,
    Completed,
    Failed,
    Interrupted,
    Closed,
}

/// Defines the lifecycle of a persistent provider-backed agent identity.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum AgentState {
    Starting,
    Ready,
    Closing,
    Closed,
}

impl AgentState {
    /// Return whether the identity can own another exchange.
    pub fn is_open(self) -> bool {
        matches!(self, Self::Starting | Self::Ready)
    }
}

impl AgentExecutionState {
    /// Return whether the run can still accept input or interruption.
    pub fn is_active(self) -> bool {
        matches!(self, Self::Starting | Self::Running | Self::Waiting)
    }
}

/// Stores one reusable provider-backed agent identity and lifecycle.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Agent {
    pub id: String,
    pub session_id: String,
    pub provider_thread_id: Option<String>,
    pub definition: String,
    pub nickname: Option<String>,
    pub state: AgentState,
    pub created_at_ms: i64,
    pub updated_at_ms: i64,
}

impl Agent {
    /// Create the persistent primary identity that owns a session timeline.
    pub fn primary(session_id: &str, now_ms: i64) -> Self {
        Self {
            id: crate::session::HarnessSession::primary_agent_id(session_id),
            session_id: session_id.into(),
            provider_thread_id: None,
            definition: "primary".into(),
            nickname: None,
            state: AgentState::Ready,
            created_at_ms: now_ms,
            updated_at_ms: now_ms,
        }
    }

    /// Create one pending run before the provider reports its child thread.
    pub fn pending(session_id: &str, definition: &str, _task: &str, now_ms: i64) -> Self {
        Self {
            id: Uuid::new_v4().to_string(),
            session_id: session_id.into(),
            provider_thread_id: None,
            definition: definition.into(),
            nickname: None,
            state: AgentState::Starting,
            created_at_ms: now_ms,
            updated_at_ms: now_ms,
        }
    }

    /// Return the best stable label for winbar and picker presentation.
    pub fn label(&self) -> &str {
        self.nickname.as_deref().unwrap_or(&self.definition)
    }

    /// Return whether this identity owns the session's primary timeline.
    pub fn is_primary(&self) -> bool {
        self.id == crate::session::HarnessSession::primary_agent_id(&self.session_id)
    }
}
