//! Harness session and agent ownership of provider conversations.
//!
//! Providers address conversations only by [`ThreadId`]. Harness owns the pair that gives a
//! thread its meaning: the visible Harness session and the agent within it. Every root
//! thread identity is derived from its owner, and an adapter derives an observed child's
//! thread from its parent with [`ThreadId::child`], so Harness resolves the owner of any
//! admitted thread from the identity alone, before routing, without a mutable table that
//! could disagree with the adapter.

use forge_provider_api::{AgentId, EventScope, InvocationScope, ThreadId, TurnAddress};
use serde::{Deserialize, Serialize};

/// Identify one visible Harness session shared by its root and child agents.
#[derive(Clone, Debug, Deserialize, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize)]
#[serde(transparent)]
pub struct SessionId(String);

impl SessionId {
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl From<String> for SessionId {
    fn from(value: String) -> Self {
        Self(value)
    }
}

impl From<&str> for SessionId {
    fn from(value: &str) -> Self {
        Self(value.to_owned())
    }
}

impl std::fmt::Display for SessionId {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(formatter)
    }
}

/// Identifies the root session and the single agent that owns a provider runtime.
#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
#[serde(try_from = "RawThreadOwner")]
pub struct ThreadOwner {
    /// Identifies the visible Harness session shared by the root and children.
    session_id: SessionId,
    /// Identifies the independent conversation owner within that session.
    agent_id: AgentId,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, thiserror::Error)]
pub enum ThreadOwnerError {
    #[error("provider execution owner has an empty session identity")]
    EmptySession,
    #[error("provider execution owner has an empty agent identity")]
    EmptyAgent,
    #[error("provider execution owner belongs to another session")]
    SessionMismatch,
    #[error("provider thread identity was not issued by Harness")]
    ForeignThread,
}

impl From<ThreadOwnerError> for forge_provider_api::ProviderError {
    fn from(_error: ThreadOwnerError) -> Self {
        Self::Contract(forge_provider_api::lifecycle::LifecycleError::OwnershipMismatch)
    }
}

#[derive(Deserialize)]
struct RawThreadOwner {
    session_id: SessionId,
    agent_id: AgentId,
}

impl TryFrom<RawThreadOwner> for ThreadOwner {
    type Error = ThreadOwnerError;

    /// Validate identities loaded from storage or transport.
    fn try_from(raw: RawThreadOwner) -> Result<Self, Self::Error> {
        let RawThreadOwner {
            session_id,
            agent_id,
        } = raw;
        Self::new(session_id, agent_id)
    }
}

impl ThreadOwner {
    /// Create an owner from nonempty session and agent identities.
    pub fn new(session_id: SessionId, agent_id: AgentId) -> Result<Self, ThreadOwnerError> {
        if session_id.as_str().trim().is_empty() {
            return Err(ThreadOwnerError::EmptySession);
        }
        if agent_id.as_str().trim().is_empty() {
            return Err(ThreadOwnerError::EmptyAgent);
        }
        Ok(Self {
            session_id,
            agent_id,
        })
    }

    /// Resolve the owner of a thread identity that Harness or an adapter's child derivation issued.
    pub fn from_thread_id(thread_id: &ThreadId) -> Result<Self, ThreadOwnerError> {
        let (session_id, agent_id) = thread_id
            .host_parts()
            .map_err(|_| ThreadOwnerError::ForeignThread)?;
        Self::new(
            session_id.into(),
            AgentId::new(agent_id).map_err(|_| ThreadOwnerError::EmptyAgent)?,
        )
    }

    /// Get the Harness session identity.
    pub fn session_id(&self) -> &SessionId {
        &self.session_id
    }

    /// Get the independent conversation owner identity.
    pub fn agent_id(&self) -> &AgentId {
        &self.agent_id
    }

    /// Get the stable provider conversation identity of this owner.
    pub fn thread_id(&self) -> ThreadId {
        ThreadId::host(self.session_id.as_str(), self.agent_id.as_str())
            .expect("validated owner identity produces a nonempty thread identity")
    }

    /// Encode the complete owner for storage boundaries; runtime maps use `ThreadOwner` directly.
    pub fn runtime_key(&self) -> String {
        let Self {
            session_id,
            agent_id,
        } = self;
        serde_json::to_string(&(session_id, agent_id))
            .expect("two string identities always have a JSON representation")
    }

    /// Resolve the primary conversation without selecting any child runtime.
    pub fn root(session_id: impl Into<String>) -> Self {
        let session_id = session_id.into();
        Self::new(
            session_id.clone().into(),
            AgentId::new(format!("{session_id}:agent:primary"))
                .expect("registered root owner creates a nonempty agent identity"),
        )
        .expect("root owners require a registered nonempty session identity")
    }

    /// Validate an explicit owner before using it as a provider connection key.
    pub fn resolve(owner: &Option<Self>, session_id: &str) -> Result<Self, ThreadOwnerError> {
        if session_id.trim().is_empty() {
            return Err(ThreadOwnerError::EmptySession);
        }
        let owner = owner.clone().unwrap_or_else(|| Self::root(session_id));
        if owner.session_id != SessionId::from(session_id) {
            return Err(ThreadOwnerError::SessionMismatch);
        }
        if owner.agent_id.as_str().is_empty() {
            return Err(ThreadOwnerError::EmptyAgent);
        }
        Ok(owner)
    }

    /// Distinguish child authority from the root's workflow authority.
    pub fn is_root(&self) -> bool {
        self.agent_id
            == AgentId::new(format!("{}:agent:primary", self.session_id))
                .expect("validated owner has a nonempty primary agent identity")
    }
}

impl From<ThreadOwner> for ThreadId {
    fn from(owner: ThreadOwner) -> Self {
        owner.thread_id()
    }
}

impl From<&ThreadOwner> for ThreadId {
    fn from(owner: &ThreadOwner) -> Self {
        owner.thread_id()
    }
}

/// Resolve the Harness owner of a provider-facing address.
///
/// Harness admits only addresses whose thread it can attribute, rejecting foreign identities at
/// the provider event boundary, so resolution after admission cannot fail.
pub trait ScopeOwner {
    /// Get the thread identity that names the address's conversation.
    fn owner_thread(&self) -> &ThreadId;

    /// Get the Harness owner of the address's conversation.
    fn owner(&self) -> ThreadOwner {
        ThreadOwner::from_thread_id(self.owner_thread())
            .expect("admitted provider addresses carry Harness-attributable thread identities")
    }
}

impl ScopeOwner for InvocationScope {
    fn owner_thread(&self) -> &ThreadId {
        &self.thread_id
    }
}

impl ScopeOwner for EventScope {
    fn owner_thread(&self) -> &ThreadId {
        self.thread_id()
    }
}

impl ScopeOwner for forge_provider_api::SessionRequest {
    fn owner_thread(&self) -> &ThreadId {
        &self.thread_id
    }
}

impl ScopeOwner for forge_provider_api::ForkTarget {
    fn owner_thread(&self) -> &ThreadId {
        &self.thread_id
    }
}

impl ScopeOwner for forge_provider_api::testing::PendingTestResponse {
    fn owner_thread(&self) -> &ThreadId {
        &self.thread_id
    }
}

impl ScopeOwner for TurnAddress {
    fn owner_thread(&self) -> &ThreadId {
        &self.scope.thread_id
    }
}
