//! Provider-neutral requests and identities for retained conversations.

use serde::{Deserialize, Serialize};

use crate::{ExecutionOwner, InvocationScope, ProviderSessionId, SubmissionId};

/// Identify one live provider binding independently of persisted conversation identity.
#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
#[serde(try_from = "String", into = "String")]
pub struct BindingId(String);

impl BindingId {
    /// Create a binding identity from a nonempty host or provider generated token.
    pub fn new(value: impl Into<String>) -> Result<Self, BindingIdError> {
        let value = value.into();
        if value.trim().is_empty() {
            return Err(BindingIdError);
        }
        Ok(Self(value))
    }

    /// Get the opaque binding token for equality, routing, or persistence.
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl TryFrom<String> for BindingId {
    type Error = BindingIdError;

    /// Validate deserialized binding identities using the constructor invariant.
    fn try_from(value: String) -> Result<Self, Self::Error> {
        Self::new(value)
    }
}

impl From<BindingId> for String {
    /// Encode the opaque token without assigning it provider-specific meaning.
    fn from(value: BindingId) -> Self {
        value.0
    }
}

/// Report an invalid empty binding identity.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct BindingIdError;

impl std::fmt::Display for BindingIdError {
    /// Describe the validation failure at a diagnostic boundary.
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str("provider binding identity is empty")
    }
}

impl std::error::Error for BindingIdError {}

/// Identify a provider-issued history position without exposing its native format.
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
#[serde(try_from = "String", into = "String")]
pub struct CheckpointId(String);

impl CheckpointId {
    /// Create a checkpoint identity after rejecting an empty token.
    pub fn new(value: impl Into<String>) -> Result<Self, CheckpointIdError> {
        let value = value.into();
        if value.trim().is_empty() {
            return Err(CheckpointIdError);
        }
        Ok(Self(value))
    }

    /// Borrow the opaque token when returning it to the issuing provider.
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl TryFrom<String> for CheckpointId {
    type Error = CheckpointIdError;
    fn try_from(value: String) -> Result<Self, Self::Error> {
        Self::new(value)
    }
}

impl From<CheckpointId> for String {
    fn from(value: CheckpointId) -> Self {
        value.0
    }
}

/// Reject an empty history position before native dispatch.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct CheckpointIdError;

impl std::fmt::Display for CheckpointIdError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str("provider checkpoint identity is empty")
    }
}

impl std::error::Error for CheckpointIdError {}

/// Fork a provider conversation while retaining the host's independent target owner.
#[derive(Clone, Debug)]
pub struct ConversationForkRequest {
    pub source: crate::catalog::ProviderCatalogRequest,
    pub target: ForkTarget,
    pub checkpoint: Option<CheckpointId>,
}

/// Prepare durable target history without creating a live host callback binding.
#[derive(Clone, Debug)]
pub struct ForkTarget {
    pub owner: ExecutionOwner,
    pub workspace: String,
    pub configuration: InvocationConfiguration,
    pub resources: ResourceSelection,
    pub system_instructions: String,
}

/// Describe the conversation and host capabilities needed to initialize a session.
#[derive(Clone)]
pub struct SessionRequest {
    /// Own the live conversation and its callbacks.
    pub owner: ExecutionOwner,
    /// Distinguish persistent interactive, Background, and delegated conversations.
    pub mode: SessionMode,
    /// Select the workspace used to resolve admitted resources.
    pub workspace: String,
    /// Resume this persisted conversation when supported.
    pub provider_session_id: Option<ProviderSessionId>,
    /// Select the complete initial model configuration, including explicit standard service.
    pub model: ModelSelection,
    /// Apply the host's normalized access policy.
    pub access: crate::access::AccessPolicy,
    /// Apply the host's normalized approval policy.
    pub approval: crate::approval::ApprovalMode,
    /// Retain already-admitted resources for the binding lifetime.
    pub resources: ResourceSelection,
    /// Keep ephemeral host-owned resources alive for the binding lifetime.
    pub resource_lease: Option<std::sync::Arc<dyn ResourceLease>>,
    /// Advertise host-owned tools using normalized definitions.
    pub tools: Vec<crate::control::ControlToolDefinition>,
    /// Connect to the host's retained canonical tool service when the transport requires MCP.
    pub tool_endpoint: Option<HostToolEndpoint>,
    /// Supply host-prepared base instructions for this session.
    pub system_instructions: String,
    /// Record sanitized provider diagnostics.
    pub trace: std::sync::Arc<dyn crate::trace::ProviderTrace>,
}

/// Describe the normalized endpoint of a retained host-owned tool service.
#[derive(Clone, Eq, PartialEq)]
pub struct HostToolEndpoint {
    /// Address of the host service; native server names remain provider-owned.
    pub url: String,
    /// Ephemeral transport headers required to authenticate to the host service.
    pub headers: std::collections::BTreeMap<String, String>,
}

impl std::fmt::Debug for HostToolEndpoint {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter
            .debug_struct("HostToolEndpoint")
            .field("url", &"[redacted]")
            .field("header_names", &self.headers.keys().collect::<Vec<_>>())
            .finish()
    }
}

/// Identify the persistent conversation role independently of provider naming.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum SessionMode {
    Interactive,
    Background,
    Child,
}

/// Select model behavior using normalized catalog identifiers and typed service policy.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct ModelSelection {
    /// None explicitly selects the provider's default model.
    pub model: Option<String>,
    pub reasoning: crate::generation::ReasoningSelection,
    /// None explicitly selects the model's default context-window tier.
    pub context_window: Option<String>,
    pub service_tier: crate::ServiceTier,
}

/// Snapshot the model and permission configuration for one admitted invocation.
#[derive(Clone, Debug)]
pub struct InvocationConfiguration {
    pub model: ModelSelection,
    pub access: crate::access::AccessPolicy,
    pub approval: crate::approval::ApprovalMode,
}

/// Retain one definitely-undelivered input without flattening its selected skill or question action.
#[derive(Clone, Debug)]
pub struct RecoveredInput {
    pub input: crate::input::PromptInput,
    pub attachments: Vec<crate::input::FileAttachment>,
    pub instructions: String,
}

/// Configure normalized model and access choices for retained settings changes.
#[derive(Clone, Debug, Default)]
pub struct SessionSettings {
    /// Replace all model choices; None preserves the retained selection.
    pub model: Option<ModelSelection>,
    /// Apply normalized access policy.
    pub access: Option<crate::access::AccessPolicy>,
    /// Apply normalized approval policy.
    pub approval: Option<crate::approval::ApprovalMode>,
}

/// Select the complete resource set for one retained provider session.
/// Empty selections exclude inherited resources rather than restoring native defaults.
#[derive(Clone, Debug, Default)]
pub struct ResourceSelection {
    /// Select portable skills by normalized definitions.
    pub skills: Vec<crate::catalog::SkillDefinition>,
    /// Configure MCP launches without exposing provider-specific resource objects.
    pub mcp: Vec<McpLaunch>,
}

/// Keep host-owned temporary resources alive without exposing their storage layout.
pub trait ResourceLease: Send + Sync {}

/// Describe a normalized MCP launch configuration with ephemeral credentials.
#[derive(Clone, Debug)]
pub struct McpLaunch {
    /// Stable normalized resource name.
    pub name: String,
    /// Select the normalized transport.
    pub transport: McpTransport,
    /// Limit startup and request duration when configured.
    pub timeout: Option<std::time::Duration>,
    /// Resolve relative launch paths against this admitted directory.
    pub working_directory: Option<std::path::PathBuf>,
    /// Restrict the advertised tool set when configured.
    pub tools: Option<McpToolSelection>,
}

/// Select all MCP tools or an explicitly named subset without a wildcard sentinel.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum McpToolSelection {
    All,
    Only(Vec<McpToolName>),
}

impl McpToolSelection {
    /// Normalize a portable configuration's wildcard or validate each selected tool name.
    pub fn from_names(names: Vec<String>) -> Result<Self, McpToolNameError> {
        if names.len() == 1 && names[0] == "*" {
            return Ok(Self::All);
        }
        names
            .into_iter()
            .map(McpToolName::try_from)
            .collect::<Result<Vec<_>, _>>()
            .map(Self::Only)
    }
}

/// Identify one named MCP tool independently of provider wire aliases.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct McpToolName(String);

impl McpToolName {
    /// Get the validated name to encode in a provider-owned request.
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl TryFrom<String> for McpToolName {
    type Error = McpToolNameError;

    fn try_from(name: String) -> Result<Self, Self::Error> {
        if name.trim().is_empty() {
            return Err(McpToolNameError::Empty);
        }
        if name == "*" {
            return Err(McpToolNameError::Wildcard);
        }
        Ok(Self(name))
    }
}

/// Reject malformed names without retaining external configuration text.
#[derive(Clone, Copy, Debug, Eq, PartialEq, thiserror::Error)]
pub enum McpToolNameError {
    #[error("MCP tool name cannot be empty")]
    Empty,
    #[error("MCP wildcard must be the entire tool selection")]
    Wildcard,
}

/// Select the normalized connection shape for one MCP resource.
#[derive(Clone)]
pub enum McpTransport {
    Stdio {
        command: String,
        arguments: Vec<String>,
        environment: std::collections::BTreeMap<String, String>,
    },
    Http {
        url: String,
        headers: std::collections::BTreeMap<String, String>,
    },
    Sse {
        url: String,
        headers: std::collections::BTreeMap<String, String>,
    },
}

impl std::fmt::Debug for McpTransport {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Stdio {
                command,
                environment,
                ..
            } => formatter
                .debug_struct("Stdio")
                .field("command", command)
                .field("arguments", &"[redacted]")
                .field("environment_names", &environment.keys().collect::<Vec<_>>())
                .finish(),
            Self::Http { headers, .. } | Self::Sse { headers, .. } => {
                let kind = if matches!(self, Self::Http { .. }) {
                    "Http"
                } else {
                    "Sse"
                };
                formatter
                    .debug_struct(kind)
                    .field("url", &"[redacted]")
                    .field("header_names", &headers.keys().collect::<Vec<_>>())
                    .finish()
            }
        }
    }
}

/// Authorize exactly one safe continuation after the provider proves non-consumption.
#[derive(Clone, Eq, PartialEq, Serialize, Deserialize)]
#[serde(try_from = "Vec<u8>", into = "Vec<u8>")]
pub struct ContinuationToken(Vec<u8>);

impl std::fmt::Debug for ContinuationToken {
    /// Hide native continuation state from generic diagnostic formatting.
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str("ContinuationToken([opaque])")
    }
}

impl ContinuationToken {
    /// Create an opaque, nonempty authorization token owned by the provider binding.
    pub fn new(bytes: Vec<u8>) -> Result<Self, RecoveryTokenError> {
        if bytes.is_empty() {
            return Err(RecoveryTokenError);
        }
        Ok(Self(bytes))
    }

    /// Borrow the token for one operation routed to its issuing provider.
    pub fn as_bytes(&self) -> &[u8] {
        &self.0
    }
}

impl TryFrom<Vec<u8>> for ContinuationToken {
    type Error = RecoveryTokenError;

    /// Validate deserialized continuation authorization using the constructor invariant.
    fn try_from(bytes: Vec<u8>) -> Result<Self, Self::Error> {
        Self::new(bytes)
    }
}

impl From<ContinuationToken> for Vec<u8> {
    /// Encode the opaque continuation authorization for the internal event transport.
    fn from(token: ContinuationToken) -> Self {
        token.0
    }
}

/// Authorize one provider-owned continuation without exposing native retry state.
#[derive(Clone)]
pub enum ContinuationRequest {
    /// Resubmit a request only after the provider proves its prior input was not consumed.
    NotConsumed {
        request: Box<PromptRequest>,
        authorization: ContinuationToken,
    },
    /// Resume settled accepted history after a replay-safe transient execution failure.
    RetryableExecution {
        scope: InvocationScope,
        authorization: ContinuationToken,
        callbacks: Box<InvocationCallbacks>,
    },
}

/// Supply fresh host authority when resuming accepted history under a new invocation.
#[derive(Clone)]
pub struct InvocationCallbacks {
    /// Bind MCP callbacks to this invocation instead of a mutable session endpoint.
    pub tool_endpoint: Option<HostToolEndpoint>,
    /// Retain the endpoint and its transport resources for this invocation.
    pub resource_lease: Option<std::sync::Arc<dyn ResourceLease>>,
    pub permissions: std::sync::Arc<dyn crate::permissions::ProviderPermissions>,
    pub provider_tools: std::sync::Arc<dyn crate::control::ProviderTools>,
    pub turn_control: std::sync::Arc<dyn crate::turn_control::TurnControl>,
    pub host_effects: std::sync::Arc<dyn ProviderHostEffects>,
}

/// Report the normalized result of a safe continuation operation.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ContinuationOutcome {
    InputAccepted(InputAcceptance),
    ExecutionResumed,
}

/// Carry one invocation's explicit workflow behavior and admitted inputs.
#[derive(Clone)]
pub struct PromptRequest {
    /// Identify the admitted execution scope.
    pub scope: InvocationScope,
    /// Preserve the user-visible execution mode for this invocation.
    pub mode: crate::input::PromptMode,
    /// Apply the current choices even when a live native binding is reused.
    pub configuration: InvocationConfiguration,
    /// Preserve normalized task intent, when this invocation starts a task.
    pub task: Option<crate::input::TaskStart>,
    /// Authorize exactly these tools for this invocation, independently of cached native schemas.
    /// Providers with mutable catalogs advertise this subset; immutable catalogs may retain the
    /// session's registration superset but must reject every call absent from this list.
    pub tools: Vec<crate::control::ControlToolDefinition>,
    /// Bind MCP tool calls to this invocation's immutable host endpoint before dispatch.
    /// An adapter must not reuse a previous invocation's endpoint for new input.
    pub tool_endpoint: Option<HostToolEndpoint>,
    /// Keep invocation-specific transport resources alive until callbacks are collected.
    pub resource_lease: Option<std::sync::Arc<dyn ResourceLease>>,
    /// Request native goal behavior explicitly rather than encoding a control command in text.
    pub goal: Option<crate::native_goal::NativeGoalCommand>,
    /// Identify the durable input journal entry.
    pub submission_id: SubmissionId,
    /// Carry prepared user input.
    pub input: crate::input::PromptInput,
    /// Carry previously saved input proven not to have reached the provider.
    pub recovered_inputs: Vec<RecoveredInput>,
    /// Carry prepared instructions.
    pub instructions: String,
    /// Carry files admitted and resolved by the host input owner.
    pub attachments: Vec<crate::input::FileAttachment>,
    /// Commit native delivery transitions through the host-owned journal.
    pub receipt: std::sync::Arc<dyn crate::submission::InputReceipt>,
    /// Authorize callbacks against this invocation's immutable scope.
    pub permissions: std::sync::Arc<dyn crate::permissions::ProviderPermissions>,
    /// Execute host-owned tools against this invocation's immutable scope.
    pub provider_tools: std::sync::Arc<dyn crate::control::ProviderTools>,
    /// Coordinate turn controls for this invocation only.
    pub turn_control: std::sync::Arc<dyn crate::turn_control::TurnControl>,
    /// Settle required foreground effects before terminal publication.
    pub host_effects: std::sync::Arc<dyn ProviderHostEffects>,
}

/// Identify one live binding and its optional durable conversation identity.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SessionBinding {
    /// Identify this exact live reader and callback generation.
    pub id: BindingId,
    /// Identify persisted conversation history, when available.
    pub provider_session_id: Option<ProviderSessionId>,
}

/// Carry live steering input without exposing native turn placement.
#[derive(Clone)]
pub struct SteeringRequest {
    /// Identify the original execution owner and invocation.
    pub scope: InvocationScope,
    /// Distinguish durable user input from a host-generated contextual notification.
    pub delivery: SteeringDelivery,
    /// Carry normalized text or a selected skill.
    pub input: crate::input::PromptInput,
    /// Preserve files admitted with this independently journaled input.
    pub attachments: Vec<crate::input::FileAttachment>,
    /// Preserve task entry semantics independently of its rendered text.
    pub task: Option<crate::input::TaskStart>,
    /// Preserve explicit native goal changes when a new task enters an active invocation.
    pub goal: Option<crate::NativeGoalCommand>,
}

/// Preserve input accountability without inventing journal identities for host notifications.
#[derive(Clone)]
pub enum SteeringDelivery {
    Journaled {
        receipt: std::sync::Arc<dyn crate::submission::InputReceipt>,
    },
    Context,
}

/// Confirm the exact class of input incorporated by a steering operation.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum SteeringAcceptance {
    Journaled { submission_id: SubmissionId },
    Context,
}

impl SteeringDelivery {
    pub fn receipt(&self) -> Option<&std::sync::Arc<dyn crate::submission::InputReceipt>> {
        match self {
            Self::Journaled { receipt } => Some(receipt),
            Self::Context => None,
        }
    }

    pub fn submission_id(&self) -> Option<&SubmissionId> {
        self.receipt().map(|receipt| receipt.id())
    }
}

/// Acknowledge durable native acceptance without exposing a native message identifier.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct InputAcceptance {
    /// Identify the host journal entry accepted by the provider.
    pub submission_id: SubmissionId,
}

/// Describe a failure while publishing required host effects.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum HostEffectFailure {
    /// The host rejected an effect before applying it.
    Rejected,
    /// The host could not establish whether the effect was applied.
    Uncertain,
    /// The callback owner was already closed or retired.
    Retired,
}

impl std::fmt::Display for HostEffectFailure {
    /// Describe the host publication failure without exposing host internals.
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str(match self {
            Self::Rejected => "host rejected a required provider effect",
            Self::Uncertain => "host effect publication is uncertain",
            Self::Retired => "host effect owner has been retired",
        })
    }
}

impl std::error::Error for HostEffectFailure {}

/// Close foreground callback admission and settle effects already admitted for an invocation.
#[async_trait::async_trait]
pub trait ProviderHostEffects: Send + Sync {
    /// Wait for required foreground effects and receipt writes before terminal publication.
    async fn settle_foreground(
        &self,
        binding: &BindingId,
        scope: &InvocationScope,
    ) -> Result<(), HostEffectFailure>;
}

/// Store adapter-owned recovery bytes without exposing or decoding their format.
#[derive(Clone, Eq, PartialEq, Serialize, Deserialize)]
#[serde(try_from = "Vec<u8>", into = "Vec<u8>")]
pub struct RecoveryToken(Vec<u8>);

impl std::fmt::Debug for RecoveryToken {
    /// Hide native recovery state from generic diagnostic formatting.
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str("RecoveryToken([opaque])")
    }
}

impl RecoveryToken {
    /// Capture an opaque checkpoint produced by its owning provider implementation.
    pub fn new(bytes: Vec<u8>) -> Result<Self, RecoveryTokenError> {
        if bytes.is_empty() {
            return Err(RecoveryTokenError);
        }
        Ok(Self(bytes))
    }

    /// Borrow checkpoint bytes for persistence or return to the owning implementation.
    pub fn as_bytes(&self) -> &[u8] {
        &self.0
    }
}

impl TryFrom<Vec<u8>> for RecoveryToken {
    type Error = RecoveryTokenError;

    /// Validate deserialized checkpoint data using the same invariant as `new`.
    fn try_from(bytes: Vec<u8>) -> Result<Self, Self::Error> {
        Self::new(bytes)
    }
}

impl From<RecoveryToken> for Vec<u8> {
    /// Encode the opaque checkpoint for host persistence.
    fn from(token: RecoveryToken) -> Self {
        token.0
    }
}

/// Report an invalid empty recovery token.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct RecoveryTokenError;

impl std::fmt::Display for RecoveryTokenError {
    /// Describe the invalid checkpoint at a diagnostic boundary.
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str("provider recovery token is empty")
    }
}

impl std::error::Error for RecoveryTokenError {}

/// Report whether a particular submission is proven present in recovered history.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum SubmissionEvidence {
    Accepted {
        receipt: Option<RecoveryToken>,
        provider_session_id: Option<ProviderSessionId>,
    },
    /// The adapter found positive, protocol-supported evidence of non-consumption.
    NotAccepted,
    /// Available evidence cannot prove acceptance or rejection and forbids replay.
    Unknown,
}

/// Keep scope validation local to normalized request construction.
pub fn validate_scope_owner(
    owner: &ExecutionOwner,
    scope: &InvocationScope,
) -> Result<(), crate::lifecycle::LifecycleError> {
    if &scope.owner == owner {
        Ok(())
    } else {
        Err(crate::lifecycle::LifecycleError::OwnershipMismatch)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Normalize wildcards once and prevent them from masquerading as named selections.
    #[test]
    fn mcp_tool_selection_distinguishes_all_from_validated_names() {
        assert_eq!(
            McpToolSelection::from_names(vec!["*".into()]).unwrap(),
            McpToolSelection::All
        );
        let selection = McpToolSelection::from_names(vec!["read".into(), "search".into()]).unwrap();
        let McpToolSelection::Only(names) = selection else {
            panic!("named selection became a wildcard");
        };
        assert_eq!(
            names.iter().map(McpToolName::as_str).collect::<Vec<_>>(),
            ["read", "search"]
        );
        assert_eq!(
            McpToolSelection::from_names(vec!["*".into(), "read".into()]),
            Err(McpToolNameError::Wildcard)
        );
        assert_eq!(
            McpToolSelection::from_names(vec![" ".into()]),
            Err(McpToolNameError::Empty)
        );
        assert_eq!(
            McpToolSelection::from_names(Vec::new()).unwrap(),
            McpToolSelection::Only(Vec::new())
        );
    }

    #[test]
    fn transport_debug_does_not_reveal_credentials_or_arguments() {
        let remote = McpTransport::Http {
            url: "https://fixture.invalid/?token=secret-url".into(),
            headers: [("Authorization".into(), "secret-header".into())].into(),
        };
        let local = McpTransport::Stdio {
            command: "fixture".into(),
            arguments: vec!["secret-argument".into()],
            environment: [("TOKEN".into(), "secret-environment".into())].into(),
        };
        let endpoint = HostToolEndpoint {
            url: "http://localhost/secret-endpoint".into(),
            headers: Default::default(),
        };
        let output = format!("{remote:?} {local:?} {endpoint:?}");
        for secret in [
            "secret-url",
            "secret-header",
            "secret-argument",
            "secret-environment",
            "secret-endpoint",
        ] {
            assert!(!output.contains(secret));
        }
    }
}
