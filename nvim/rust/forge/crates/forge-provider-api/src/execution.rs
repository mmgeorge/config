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

pub use crate::agent::AgentId;
execution_identity!(ExchangeId);
execution_identity!(TaskId);
execution_identity!(InvocationId);

#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct InvocationScope {
    /// Identify the host conversation that owns this invocation.
    pub thread_id: crate::ThreadId,
    pub exchange_id: ExchangeId,
    pub invocation_id: InvocationId,
}

impl InvocationScope {
    /// Resolve the host response-cycle identity independently of native turn numbering.
    pub fn turn_id(&self) -> Result<crate::TurnId, crate::identity::IdentityError> {
        crate::TurnId::new(self.invocation_id.as_str())
    }

    pub fn new(thread_id: impl Into<crate::ThreadId>, exchange_id: impl Into<ExchangeId>) -> Self {
        Self {
            thread_id: thread_id.into(),
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
        thread_id: crate::ThreadId,
    },
    Invocation {
        invocation: InvocationScope,
    },
    SessionOperation {
        thread_id: crate::ThreadId,
        operation_id: crate::OperationId,
    },
}

impl From<InvocationScope> for EventScope {
    fn from(invocation: InvocationScope) -> Self {
        Self::Invocation { invocation }
    }
}

impl EventScope {
    /// Get the host conversation that owns this scope.
    pub fn thread_id(&self) -> &crate::ThreadId {
        match self {
            Self::Invocation { invocation } => &invocation.thread_id,
            Self::Binding { thread_id } | Self::SessionOperation { thread_id, .. } => thread_id,
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
