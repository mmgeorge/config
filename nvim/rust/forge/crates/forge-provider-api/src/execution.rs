use serde::{Deserialize, Serialize};

use crate::turn::TurnOutcome;

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
execution_identity!(AgentId);
execution_identity!(ExchangeId);
execution_identity!(TaskId);
execution_identity!(InvocationId);
execution_identity!(ProviderTurnId);
execution_identity!(ToolId);

#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
/// Identifies the root session and the single agent that owns a provider runtime.
pub struct ExecutionOwner {
    /// Identifies the visible Harness session shared by the root and children.
    pub session_id: SessionId,
    /// Identifies the independent conversation owner within that session.
    pub agent_id: AgentId,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, thiserror::Error)]
pub enum ExecutionOwnerError {
    #[error("provider execution owner has an empty session identity")]
    EmptySession,
    #[error("provider execution owner has an empty agent identity")]
    EmptyAgent,
    #[error("provider execution owner belongs to another session")]
    SessionMismatch,
}

impl ExecutionOwner {
    /// Encode the complete owner for storage boundaries; runtime maps use `ExecutionOwner` directly.
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
        Self {
            agent_id: format!("{session_id}:agent:primary").into(),
            session_id: session_id.into(),
        }
    }

    /// Validate an explicit owner before using it as a provider connection key.
    pub fn resolve(owner: &Option<Self>, session_id: &str) -> Result<Self, ExecutionOwnerError> {
        if session_id.is_empty() {
            return Err(ExecutionOwnerError::EmptySession);
        }
        let owner = owner.clone().unwrap_or_else(|| Self::root(session_id));
        if owner.session_id != SessionId::from(session_id) {
            return Err(ExecutionOwnerError::SessionMismatch);
        }
        if owner.agent_id.as_str().is_empty() {
            return Err(ExecutionOwnerError::EmptyAgent);
        }
        Ok(owner)
    }

    /// Distinguish child authority from the root's workflow authority.
    pub fn is_root(&self) -> bool {
        self.agent_id == AgentId::from(format!("{}:agent:primary", self.session_id))
    }
}

#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct InvocationScope {
    pub owner: ExecutionOwner,
    pub exchange_id: ExchangeId,
    pub invocation_id: InvocationId,
}

impl InvocationScope {
    pub fn new(owner: ExecutionOwner, exchange_id: impl Into<ExchangeId>) -> Self {
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
        owner: ExecutionOwner,
    },
    Invocation {
        invocation: InvocationScope,
    },
    SessionOperation {
        owner: ExecutionOwner,
        operation_id: crate::OperationId,
    },
}

impl From<InvocationScope> for EventScope {
    fn from(invocation: InvocationScope) -> Self {
        Self::Invocation { invocation }
    }
}

impl EventScope {
    pub fn owner(&self) -> &ExecutionOwner {
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
    pub thread_id: String,
    pub turn_id: ProviderTurnId,
}

#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct MessageAddress {
    pub turn: TurnAddress,
    pub message_id: super::MessageId,
}

#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct ToolAddress {
    pub turn: TurnAddress,
    pub tool_id: ToolId,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum TurnUpdate {
    Started {
        started_at_ms: i64,
    },
    CallFinished {
        completed_at_ms: i64,
    },
    Summary {
        summary: super::TurnSummary,
    },
    Finished {
        outcome: TurnOutcome,
        completed_at_ms: i64,
    },
}

/// Represents final timing and usage metadata for one assistant response.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct TurnSummary {
    pub duration_ms: u64,
    pub usage: Option<crate::usage::TokenUsage>,
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Owner serialization preserves separators, escaping, and the root/child distinction.
    #[test]
    fn storage_keys_preserve_complete_owner_identity() {
        let owners = [
            ExecutionOwner::root("a:agent:b"),
            ExecutionOwner {
                session_id: "a".into(),
                agent_id: "b".into(),
            },
            ExecutionOwner {
                session_id: "a".into(),
                agent_id: "b:agent:c".into(),
            },
            ExecutionOwner {
                session_id: "a:agent:b".into(),
                agent_id: "c".into(),
            },
            ExecutionOwner {
                session_id: "a\"\n".into(),
                agent_id: "b:c".into(),
            },
        ];
        let keys = owners
            .iter()
            .map(ExecutionOwner::runtime_key)
            .collect::<std::collections::HashSet<_>>();
        assert_eq!(keys.len(), owners.len());
        for owner in owners {
            let pair: (SessionId, AgentId) = serde_json::from_str(&owner.runtime_key()).unwrap();
            assert_eq!(pair, (owner.session_id, owner.agent_id));
        }
    }
}
