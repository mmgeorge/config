use serde::{Deserialize, Serialize};

macro_rules! execution_identity {
    ($name:ident) => {
        #[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
        #[serde(transparent)]
        pub struct $name(String);
        impl $name {
            pub fn as_str(&self) -> &str {
                &self.0
            }
        }
        impl From<String> for $name {
            fn from(value: String) -> Self {
                Self(value)
            }
        }
        impl From<&str> for $name {
            fn from(value: &str) -> Self {
                Self(value.to_owned())
            }
        }
        impl std::fmt::Display for $name {
            fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                self.0.fmt(formatter)
            }
        }
    };
}

execution_identity!(SessionId);
pub use crate::agent::AgentId;
execution_identity!(ExchangeId);
execution_identity!(TaskId);
execution_identity!(InvocationId);

#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
/// Identifies the root session and the single agent that owns a provider runtime.
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
}

#[derive(Deserialize)]
struct RawThreadOwner {
    session_id: SessionId,
    agent_id: AgentId,
}

impl TryFrom<RawThreadOwner> for ThreadOwner {
    type Error = ThreadOwnerError;

    /// Validate identities loaded from storage or provider transport.
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

    /// Get the Harness session identity.
    pub fn session_id(&self) -> &SessionId {
        &self.session_id
    }

    /// Get the independent conversation owner identity.
    pub fn agent_id(&self) -> &AgentId {
        &self.agent_id
    }

    /// Get the stable host conversation identity allocated from this registered owner.
    pub fn thread_id(&self) -> crate::ThreadId {
        crate::ThreadId::new(format!("thread:{}", self.runtime_key()))
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

#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct InvocationScope {
    pub owner: ThreadOwner,
    pub exchange_id: ExchangeId,
    pub invocation_id: InvocationId,
}

impl InvocationScope {
    /// Resolve the host response-cycle identity independently of native turn numbering.
    pub fn turn_id(&self) -> Result<crate::TurnId, crate::identity::IdentityError> {
        crate::TurnId::new(self.invocation_id.as_str())
    }

    pub fn new(owner: ThreadOwner, exchange_id: impl Into<ExchangeId>) -> Self {
        Self {
            owner,
            exchange_id: exchange_id.into(),
            invocation_id: uuid::Uuid::new_v4().to_string().into(),
        }
    }
}

#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum EventScope {
    /// Observe a retained live binding without inventing an invocation or mutation operation.
    Binding {
        /// Identify the host conversation which owns this stream; the envelope identifies its generation.
        owner: ThreadOwner,
    },
    Invocation {
        invocation: InvocationScope,
    },
    SessionOperation {
        owner: ThreadOwner,
        operation_id: crate::OperationId,
    },
}

impl From<InvocationScope> for EventScope {
    fn from(invocation: InvocationScope) -> Self {
        Self::Invocation { invocation }
    }
}

impl EventScope {
    pub fn owner(&self) -> &ThreadOwner {
        match self {
            Self::Invocation { invocation } => &invocation.owner,
            Self::Binding { owner } | Self::SessionOperation { owner, .. } => owner,
        }
    }

    pub fn invocation(&self) -> Option<&InvocationScope> {
        match self {
            Self::Invocation { invocation } => Some(invocation),
            Self::Binding { .. } | Self::SessionOperation { .. } => None,
        }
    }
}

#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct TurnAddress {
    pub scope: InvocationScope,
    pub thread_id: crate::ThreadId,
    pub turn_id: crate::TurnId,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum TurnEvent {
    /// Publish the retained result after provider-owned foreground cleanup succeeds.
    Completed(crate::CompletedTurn),
    /// Observe native failure explicitly correlated to this turn without completing it.
    Error {
        error: crate::ProviderFailure,
    },
    /// Apply a native plan observation to its owning turn.
    ChecklistUpdated(crate::ProviderChecklistUpdate),
    Started(crate::turn::TurnStarted),
    /// Report identified accounting without ending the turn.
    Usage(crate::TurnUsageReport),
    /// Preserve the positively confirmed model for this response cycle.
    ModelResolved(crate::model::ResolvedModel),
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Reject malformed owner identities before transport can admit them.
    #[test]
    fn deserialization_rejects_empty_owner_identities() {
        for raw in [
            serde_json::json!({"session_id":"", "agent_id":"agent"}),
            serde_json::json!({"session_id":"session", "agent_id":""}),
            serde_json::json!({"session_id":" ", "agent_id":"agent"}),
            serde_json::json!({"session_id":"session", "agent_id":"\t"}),
        ] {
            assert!(serde_json::from_value::<ThreadOwner>(raw).is_err());
        }
        let owner = ThreadOwner::new("session".into(), AgentId::new("agent").unwrap()).unwrap();
        let encoded = serde_json::to_value(&owner).unwrap();
        assert_eq!(
            encoded,
            serde_json::json!({"session_id":"session", "agent_id":"agent"})
        );
        assert_eq!(
            serde_json::from_value::<ThreadOwner>(encoded).unwrap(),
            owner
        );
    }

    /// Owner serialization preserves separators, escaping, and the root/child distinction.
    #[test]
    fn storage_keys_preserve_complete_owner_identity() {
        let owners = [
            ThreadOwner::root("a:agent:b"),
            ThreadOwner {
                session_id: "a".into(),
                agent_id: AgentId::new("b").unwrap(),
            },
            ThreadOwner {
                session_id: "a".into(),
                agent_id: AgentId::new("b:agent:c").unwrap(),
            },
            ThreadOwner {
                session_id: "a:agent:b".into(),
                agent_id: AgentId::new("c").unwrap(),
            },
            ThreadOwner {
                session_id: "a\"\n".into(),
                agent_id: AgentId::new("b:c").unwrap(),
            },
        ];
        let keys = owners
            .iter()
            .map(ThreadOwner::runtime_key)
            .collect::<std::collections::HashSet<_>>();
        assert_eq!(keys.len(), owners.len());
        for owner in owners {
            let pair: (SessionId, AgentId) = serde_json::from_str(&owner.runtime_key()).unwrap();
            assert_eq!(pair, (owner.session_id, owner.agent_id));
        }
    }
    /// Reattachment preserves host identity while sibling agents retain distinct threads.
    #[test]
    fn host_thread_identity_survives_owner_reload_and_separates_agents() {
        let owner = ThreadOwner::root("session");
        let restored: ThreadOwner =
            serde_json::from_value(serde_json::to_value(&owner).unwrap()).unwrap();
        assert_eq!(owner.thread_id(), restored.thread_id());
        let child = ThreadOwner::new("session".into(), AgentId::new("child").unwrap()).unwrap();
        assert_ne!(owner.thread_id(), child.thread_id());
        assert_ne!(
            owner.thread_id(),
            ThreadOwner::root("other-session").thread_id()
        );
    }
}
