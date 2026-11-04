//! Describe caller-requested context compaction without exposing native orchestration.

use serde::{Deserialize, Serialize};

/// Compact this session's retained model context; the owner selects collection and continuation.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct ContextCompactionRequest {}

/// Report confirmed compaction separately from the handoff of any replacement invocation.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub(crate) struct ContextCompactionOutcome {
    /// Host-created durable timeline identity of the completed compaction attempt.
    pub(crate) compaction: super::ContextCompactionId,
    /// Historical handoff evidence; this never authorizes a future invocation.
    pub(crate) disposition: super::InvocationDisposition,
}

/// Preserve normalized compaction failure categories without native diagnostic strings.
#[derive(Clone, Copy, Debug, Deserialize, Serialize, thiserror::Error)]
#[serde(tag = "compaction_failure", rename_all = "snake_case")]
pub(crate) enum ContextCompactionFailure {
    /// The normalized capability does not support an explicit compaction operation.
    #[error("context compaction is unavailable for this session")]
    Unsupported,
    /// No native conversation exists yet; there is no context operation to target.
    #[error("the session has no conversation to compact yet")]
    NoConversation,
    /// Native return or worker observation did not establish completed context replacement.
    #[error("context compaction completion was not confirmed")]
    Unconfirmed,
    /// The result cannot mutate a different conversation or task revision.
    #[error("compaction authority changed before the result could apply")]
    AuthorityChanged,
    /// The attempt remains retained while cleanup establishes quiescence.
    #[error("context compaction exceeded its deadline; its work remains owned for collection")]
    TimedOut,
    /// The adapter supplied a normalized failure category.
    #[error("context compaction failed ({kind:?})")]
    Provider {
        kind: forge_provider_api::ProviderFailureKind,
    },
}

impl ContextCompactionFailure {
    /// Get the stable host failure code without parsing provider messages.
    pub(crate) fn code(self) -> &'static str {
        match self {
            Self::Unsupported => "compaction_unsupported",
            Self::NoConversation => "compaction_no_conversation",
            Self::Unconfirmed => "compaction_unconfirmed",
            Self::AuthorityChanged => "compaction_authority_changed",
            Self::TimedOut => "compaction_timeout",
            Self::Provider { .. } => "compaction_provider_failed",
        }
    }
}
