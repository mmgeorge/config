//! Host policy notifications kept outside native provider observations.
use super::approval_view::ApprovalCommand;
use crate::permissions::directory::DirectoryGrant;
use crate::session::InputDeadline;
use forge_provider_api::permissions::PermissionTarget;
use forge_provider_api::{
    ItemId, OperationId, ProviderFailure, SanitizedMessage, ThreadId, TurnId,
};
use serde::{Deserialize, Serialize};

/// Publish committed host state under the transport's original invocation scope.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub enum HostEvent {
    /// Present a host-owned permission lifecycle transition.
    PermissionApproval {
        execution_kind: ApprovalExecutionKind,
        event: PermissionApprovalEvent,
    },
    /// Announce a persisted directory grant and its origin.
    DirectoryAccessGranted(DirectoryAccessGrant),
    /// Change retry presentation for the originally failed turn.
    RetryUpdated {
        thread_id: ThreadId,
        turn_id: TurnId,
        event: RetryEvent,
    },
    /// Change a parent turn's wait for independent children.
    ChildWait {
        thread_id: ThreadId,
        turn_id: TurnId,
        event: ChildWaitEvent,
    },
    /// Attribute verified skill reads to their original item.
    SkillRead {
        thread_id: ThreadId,
        turn_id: TurnId,
        item_id: ItemId,
        read: Vec<crate::skill::SkillRead>,
    },
    /// Publish the manifest negotiated by a successful binding open or configuration.
    ///
    /// Adapters report these manifests in operation results rather than as native stream
    /// events, so the bridge republishes each accepted result in binding order for the
    /// broker's monotonic capability adoption. The binding identity lets the broker refuse a
    /// queued manifest from a generation that retired before the broker observed it.
    ///
    /// `history` carries the native history identity that a binding open reported in its
    /// result. Adapters do not restate that identity as `Thread.IdentityResolved`, so this
    /// publication is the only ordered point at which the broker can retain it. Configuration
    /// results leave it empty because a replacement history arrives as `IdentityResolved`.
    CapabilitiesNegotiated {
        binding_id: forge_provider_api::BindingId,
        manifest: forge_provider_api::CapabilityManifest,
        history: Option<forge_provider_api::ProviderSessionId>,
    },
    /// Report a host failure independently of native turn completion.
    Failure {
        failure: ProviderFailure,
        message: SanitizedMessage,
    },
}

/// Distinguish managed conversation controls from independently observed native work.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Deserialize, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum ApprovalExecutionKind {
    Managed,
    NativeObserved,
}

/// Identify one host approval request independently of its native RPC.
#[derive(Clone, Debug, Eq, PartialEq, Deserialize, Serialize)]
#[serde(try_from = "String", into = "String")]
pub struct ApprovalRequestId(String);
impl ApprovalRequestId {
    /// Create a nonempty request identity.
    pub fn new(
        value: impl Into<String>,
    ) -> Result<Self, forge_provider_api::identity::IdentityError> {
        let value = value.into();
        if value.trim().is_empty() {
            return Err(forge_provider_api::identity::IdentityError);
        }
        Ok(Self(value))
    }
    /// Get the opaque request identity.
    pub fn as_str(&self) -> &str {
        &self.0
    }
}
impl TryFrom<String> for ApprovalRequestId {
    type Error = forge_provider_api::identity::IdentityError;
    /// Validate a serialized identity.
    fn try_from(value: String) -> Result<Self, Self::Error> {
        Self::new(value)
    }
}
impl From<ApprovalRequestId> for String {
    /// Encode the opaque identity.
    fn from(value: ApprovalRequestId) -> Self {
        value.0
    }
}

macro_rules! approval_identity {
    ($name:ident, $description:literal) => {
        #[doc = $description]
        #[derive(Clone, Debug, Eq, PartialEq, Deserialize, Serialize)]
        #[serde(try_from = "String", into = "String")]
        pub struct $name(String);
        impl $name {
            /// Create a checked opaque identity.
            pub fn new(
                value: impl Into<String>,
            ) -> Result<Self, forge_provider_api::IdentityError> {
                let value = value.into();
                if value.trim().is_empty() {
                    return Err(forge_provider_api::IdentityError);
                }
                Ok(Self(value))
            }
            /// Get the original opaque identity.
            pub fn as_str(&self) -> &str {
                &self.0
            }
        }
        impl TryFrom<String> for $name {
            type Error = forge_provider_api::IdentityError;
            /// Validate a deserialized identity.
            fn try_from(value: String) -> Result<Self, Self::Error> {
                Self::new(value)
            }
        }
        impl From<$name> for String {
            /// Encode the opaque identity at a transport boundary.
            fn from(value: $name) -> Self {
                value.0
            }
        }
    };
}
approval_identity!(
    ApprovalTargetId,
    "Identify one independently resolved approval target."
);
approval_identity!(
    ApprovalChoiceId,
    "Identify one concrete offered permission choice."
);

/// Identify the exact owner addressed by a permission request.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub enum ApprovalScope {
    /// Authorize one identified item.
    Item {
        thread_id: ThreadId,
        turn_id: TurnId,
        item_id: ItemId,
    },
    /// Authorize a turn without a native item identity.
    Turn {
        thread_id: ThreadId,
        turn_id: TurnId,
    },
    /// Authorize conversation-level preparation.
    Thread { thread_id: ThreadId },
    /// Authorize one conversation operation.
    Operation {
        thread_id: ThreadId,
        operation_id: OperationId,
    },
}
/// Preserve a target decision separately from its policy source.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub enum PermissionDecision {
    /// Permit the displayed authority scope.
    Allow { scope: PermissionRuleScope },
    /// Reject the displayed authority scope.
    Deny {
        scope: PermissionRuleScope,
        reason: SanitizedMessage,
    },
    /// Reject because the deadline expired.
    TimedOut,
}
/// Describe the persistence authority explicitly offered to the user.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Deserialize, Serialize)]
pub enum PermissionRuleScope {
    Once,
    ExactRule,
    BroadRule,
}
/// Select an existing permission rule namespace.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Deserialize, Serialize)]
pub enum ApprovalRuleCategory {
    Command,
    Read,
    Write,
    Network,
    Mcp,
    Elevation,
    Tool,
}
impl ApprovalRuleCategory {
    /// Get the established persistent category spelling.
    pub fn saved_category(self) -> &'static str {
        use crate::permissions::document::*;
        match self {
            Self::Command => CATEGORY_BASH,
            Self::Read => CATEGORY_READ,
            Self::Write => CATEGORY_EDIT,
            Self::Network => CATEGORY_WEBFETCH,
            Self::Mcp => CATEGORY_MCP,
            Self::Elevation => CATEGORY_ELEVATE,
            Self::Tool => CATEGORY_TOOL,
        }
    }
    /// Parse an existing category without inventing a storage vocabulary.
    pub fn from_saved_category(category: &str) -> Option<Self> {
        use crate::permissions::document::*;
        match category {
            CATEGORY_BASH => Some(Self::Command),
            CATEGORY_READ => Some(Self::Read),
            CATEGORY_EDIT => Some(Self::Write),
            CATEGORY_WEBFETCH => Some(Self::Network),
            CATEGORY_MCP => Some(Self::Mcp),
            CATEGORY_ELEVATE => Some(Self::Elevation),
            CATEGORY_TOOL => Some(Self::Tool),
            _ => None,
        }
    }
}
/// Identify the authority responsible for a committed decision.
#[derive(Clone, Copy, Debug, Deserialize, Serialize)]
pub enum ApprovalDecisionSource {
    User,
    SavedPolicy,
    ApprovalMode,
    Timeout,
}
/// Explain why no permission decision committed.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub enum ApprovalCancellationReason {
    UserCancelled,
    TurnInterrupted,
    ThreadClosed,
    RequesterGone,
    PersistenceFailed(ProviderFailure),
}
/// Record a concrete validated saved rule.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ApprovalRulePattern {
    pub category: ApprovalRuleCategory,
    pub pattern: String,
}
/// Offer a concrete permission decision and exactly its disclosed effects.
#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(try_from = "RawApprovalChoice")]
pub struct PermissionApprovalChoice {
    choice_id: ApprovalChoiceId,
    label: String,
    decision: PermissionDecision,
    rules: Vec<ApprovalRulePattern>,
    directory_grants: Vec<DirectoryGrant>,
}
impl PermissionApprovalChoice {
    /// Create an offered decision, rejecting timeout and undisclosed persistence.
    pub fn new(
        choice_id: String,
        label: String,
        decision: PermissionDecision,
        rules: Vec<ApprovalRulePattern>,
        directory_grants: Vec<DirectoryGrant>,
    ) -> Result<Self, ApprovalChoiceError> {
        if choice_id.trim().is_empty() {
            return Err(ApprovalChoiceError::EmptyIdentity);
        }
        let scope = match &decision {
            PermissionDecision::TimedOut => return Err(ApprovalChoiceError::TimeoutOffered),
            PermissionDecision::Allow { scope } | PermissionDecision::Deny { scope, .. } => *scope,
        };
        if scope == PermissionRuleScope::Once && (!rules.is_empty() || !directory_grants.is_empty())
        {
            return Err(ApprovalChoiceError::UndisclosedPersistence);
        }
        if scope != PermissionRuleScope::Once && rules.is_empty() && directory_grants.is_empty() {
            return Err(ApprovalChoiceError::MissingPersistence);
        }
        if rules.iter().any(|rule| rule.pattern.trim().is_empty()) {
            return Err(ApprovalChoiceError::EmptyPattern);
        }
        Ok(Self {
            choice_id: ApprovalChoiceId::new(choice_id)
                .map_err(|_| ApprovalChoiceError::EmptyIdentity)?,
            label,
            decision,
            rules,
            directory_grants,
        })
    }
    /// Get the offered choice identity.
    pub fn choice_id(&self) -> &str {
        self.choice_id.as_str()
    }
    /// Get the displayed label.
    pub fn label(&self) -> &str {
        &self.label
    }
    /// Get the offered decision.
    pub fn decision(&self) -> &PermissionDecision {
        &self.decision
    }
    /// Get the exact disclosed rules.
    pub fn rules(&self) -> &[ApprovalRulePattern] {
        &self.rules
    }
    /// Get the exact disclosed directory grants.
    pub fn directory_grants(&self) -> &[DirectoryGrant] {
        &self.directory_grants
    }
}
/// Reject an invalid offered permission choice.
#[derive(Debug, thiserror::Error)]
pub enum ApprovalChoiceError {
    #[error("approval choice identity is empty")]
    EmptyIdentity,
    #[error("timeout cannot be offered as a user choice")]
    TimeoutOffered,
    #[error("once-only choice contains persistent effects")]
    UndisclosedPersistence,
    #[error("persistent choice has no disclosed effects")]
    MissingPersistence,
    #[error("permission rule pattern is empty")]
    EmptyPattern,
}
#[derive(Deserialize)]
struct RawApprovalChoice {
    choice_id: String,
    label: String,
    decision: PermissionDecision,
    rules: Vec<ApprovalRulePattern>,
    directory_grants: Vec<DirectoryGrant>,
}
impl TryFrom<RawApprovalChoice> for PermissionApprovalChoice {
    type Error = ApprovalChoiceError;
    /// Validate every deserialized choice through its constructor.
    fn try_from(raw: RawApprovalChoice) -> Result<Self, Self::Error> {
        let RawApprovalChoice {
            choice_id,
            label,
            decision,
            rules,
            directory_grants,
        } = raw;
        Self::new(choice_id, label, decision, rules, directory_grants)
    }
}
/// Preserve one independently identified target and its concrete choices.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct PermissionApprovalTarget {
    pub target_id: ApprovalTargetId,
    pub target: PermissionTarget,
    pub title: String,
    pub detail: String,
    pub commands: Vec<ApprovalCommand>,
    pub choices: Vec<PermissionApprovalChoice>,
}
/// Present the original permission request without native event authority.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct PermissionApprovalRequest {
    pub request_id: ApprovalRequestId,
    pub scope: ApprovalScope,
    pub reason: Option<SanitizedMessage>,
    pub targets: Vec<PermissionApprovalTarget>,
    pub deadline: Option<InputDeadline>,
}
/// Present a pending request with its original host owner for editor control routing.
#[derive(Clone, Debug, Serialize)]
pub struct ApprovalRequestPresentation {
    #[serde(flatten)]
    pub request: PermissionApprovalRequest,
    pub execution_owner: crate::provider::ThreadOwner,
    pub execution_kind: ApprovalExecutionKind,
    pub exchange_id: Option<String>,
}

/// Record the committed decision and effects for one target.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ResolvedPermissionTarget {
    pub target_id: ApprovalTargetId,
    pub decision: PermissionDecision,
    pub source: ApprovalDecisionSource,
    pub rules: Vec<ApprovalRulePattern>,
    pub directory_grants: Vec<DirectoryGrant>,
}
/// Resolve one approval without implying every target was allowed.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct PermissionApprovalResolution {
    pub request_id: ApprovalRequestId,
    pub scope: ApprovalScope,
    pub targets: Vec<ResolvedPermissionTarget>,
    pub resolved_at_ms: i64,
}
/// End a pending request without fabricating a denied user decision.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct PermissionApprovalCancellation {
    pub request_id: ApprovalRequestId,
    pub scope: ApprovalScope,
    pub reason: ApprovalCancellationReason,
    pub cancelled_at_ms: i64,
}
/// Announce a successfully persisted grant with its original ownership.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct DirectoryAccessGrant {
    pub request_id: ApprovalRequestId,
    pub scope: ApprovalScope,
    pub grant: DirectoryGrant,
    pub granted_at_ms: i64,
}
/// Publish exactly one host permission lifecycle transition.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub enum PermissionApprovalEvent {
    Requested(PermissionApprovalRequest),
    Resolved(PermissionApprovalResolution),
    Cancelled(PermissionApprovalCancellation),
}
/// Replace optional retry state with explicit transitions.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub enum RetryEvent {
    Scheduled(crate::provider::retry::ProviderRetry),
    Cleared,
}
/// Track the parent wait independently of child turn outcomes.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub enum ChildWaitEvent {
    Started { agent_count: usize },
    Updated { agent_count: usize },
    Finished,
}
