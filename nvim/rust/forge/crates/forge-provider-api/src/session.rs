//! Provider-neutral requests and identities for retained conversations.

use serde::{Deserialize, Serialize};

use crate::{InvocationScope, ProviderSessionId, SubmissionId, ThreadOwner};

/// Identify one adapter-issued live binding generation independently of durable history.
/// Each open, including resume, allocates a fresh ID. Retired IDs never authorize work on
/// replacements and cannot be used as a durable session ID or a resume credential.
#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
#[serde(try_from = "String", into = "String")]
pub struct BindingId(String);

impl BindingId {
    /// Create a validated nonempty opaque token without granting authority to any binding.
    pub fn new(value: impl Into<String>) -> Result<Self, BindingIdError> {
        let value = value.into();
        if value.trim().is_empty() {
            return Err(BindingIdError);
        }
        Ok(Self(value))
    }

    /// Get the opaque token for exact routing and diagnostics, never for durable resume.
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

/// Identify a provider-issued immutable history position within its owning durable session.
/// The issuing adapter resolves the token; hosts must not interpret it or substitute the latest
/// history item. It remains usable only while the adapter can prove that position's identity.
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
    /// Validate deserialized history positions through the nonempty-token constructor.
    fn try_from(value: String) -> Result<Self, Self::Error> {
        Self::new(value)
    }
}

impl From<CheckpointId> for String {
    /// Encode the opaque position for persistence or its issuing adapter.
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

/// Fork stable provider history with a host-issued retry identity.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SessionForkRequest {
    /// Select a settled binding or immutable provider checkpoint.
    pub source: ForkSource,
    /// Configure the separately retained target history.
    pub target: ForkTarget,
}

/// Select a positively stable source boundary before creating fork history.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ForkSource {
    /// Read the exact settled live generation.
    Binding(BindingId),
    /// Read an immutable history position independently of a live binding.
    Checkpoint {
        /// Durable provider-issued history which owns the immutable position.
        provider_session_id: ProviderSessionId,
        /// Provider-issued position whose contents must remain stable during fork.
        checkpoint: CheckpointId,
    },
}

/// Prepare durable target history without creating a live host callback binding.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ForkTarget {
    /// Independent host owner reserved for the staged target history.
    pub owner: ThreadOwner,
    /// Workspace resolving the target's admitted resources.
    pub workspace: String,
    /// Complete resolved defaults for the target; never a patch to the source.
    pub configuration: SessionConfiguration,
    /// Explicit host-prepared base instructions for the target history.
    pub system_instructions: String,
}

/// Own complete open inputs and binding-lifetime host resource authority.
/// Transfer this request to the retained open operation. Its identity snapshot retains values
/// and weak authority identity only, so cached replay metadata cannot prolong live leases.
/// Isolated history excludes resume by construction; adapters also reject incompatible tools,
/// inherited resources, or instructions before allocating native isolated work.
#[derive(Clone)]
pub struct SessionRequest {
    /// Identify the host conversation before any native history is opened.
    pub thread_id: crate::ThreadId,
    /// Own the live conversation and its callbacks.
    pub owner: ThreadOwner,
    /// Distinguish persistent interactive, Background, and delegated conversations.
    pub mode: SessionMode,
    /// Select the workspace used to resolve admitted resources.
    pub workspace: String,
    /// Choose durable history or enforce fresh isolated temporary history.
    pub history: SessionHistory,
    /// Apply one complete initial configuration.
    pub configuration: SessionConfiguration,
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

impl SessionRequest {
    /// Reject a host thread identity that belongs to another registered owner.
    pub fn validate_thread_owner(&self) -> Result<(), crate::ProviderError> {
        if self.thread_id != self.owner.thread_id() {
            return Err(crate::ProviderFailure::new(
                crate::ProviderOperation::ThreadOpen,
                crate::ProviderFailureKind::WrongInvocation,
            )
            .into());
        }
        Ok(())
    }

    /// Snapshot complete replay identity without retaining callback or resource ownership.
    pub fn identity(&self) -> crate::SessionRequestIdentity {
        self.into()
    }
}

impl PartialEq for SessionRequest {
    /// Compare the complete open request including immutable resource and diagnostic authority identities.
    fn eq(&self, other: &Self) -> bool {
        self.owner == other.owner
            && self.mode == other.mode
            && self.workspace == other.workspace
            && self.history == other.history
            && self.configuration == other.configuration
            && same_lease(&self.resource_lease, &other.resource_lease)
            && self.tools == other.tools
            && self.tool_endpoint == other.tool_endpoint
            && self.system_instructions == other.system_instructions
            && std::sync::Arc::ptr_eq(&self.trace, &other.trace)
    }
}
impl Eq for SessionRequest {}

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
    /// The user's retained foreground conversation.
    Interactive,
    /// A retained background conversation with its own host owner; not isolated temporary text work.
    Background,
    /// A delegated conversation with independent host ownership and callback authority.
    Child,
}

/// Select model behavior using normalized catalog identifiers and typed service policy.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct ModelSelection {
    /// None explicitly selects the provider's default model.
    pub model: Option<String>,
    /// Explicit provider-default or selected normalized reasoning policy.
    pub reasoning: crate::generation::ReasoningSelection,
    /// None explicitly selects the model's default context-window tier.
    pub context_window: Option<String>,
    /// Explicit normalized service policy, including standard service.
    pub service_tier: crate::ServiceTier,
}

/// Snapshot the model and permission configuration for one admitted invocation.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct InvocationConfiguration {
    /// Complete effective model controls for this invocation only.
    pub model: ModelSelection,
    /// Filesystem and sandbox authority captured for this invocation.
    pub access: crate::access::AccessPolicy,
    /// Approval policy captured for this invocation's callbacks.
    pub approval: crate::approval::ApprovalMode,
}

/// Retain one definitely-undelivered input without flattening its selected skill or question action.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct RecoveredInput {
    /// Original logical input and delivery attempt accepted with this grouped prompt.
    pub covered_input: crate::CoveredInput,
    /// Earlier host input backed by positive proof that its prior attempt was not consumed.
    pub input: crate::input::PromptInput,
    /// Files admitted with that exact earlier input.
    pub attachments: Vec<crate::input::FileAttachment>,
    /// Host-prepared workflow instructions retained with that input.
    pub instructions: String,
}

/// Own complete resolved session defaults independently of per-invocation overrides.
/// Resolve persisted preferences in Harness before calling configure. Every field replaces
/// its previous value; provider defaults are explicit typed choices rather than missing patches.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SessionConfiguration {
    /// Resolve model selection, including provider-default choices.
    pub model: ModelSelection,
    /// Enforce normalized filesystem and sandbox authority.
    pub access: crate::access::AccessPolicy,
    /// Enforce normalized permission approval policy.
    pub approval: crate::approval::ApprovalMode,
    /// Select the entire admitted skill and MCP resource set.
    pub resources: ResourceSelection,
}

/// Select history lifetime independently of interactive, child, or background role.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum SessionHistory {
    /// Retain durable history; resuming always allocates a fresh binding generation.
    Durable {
        /// Durable provider-issued history to resume, or None for new retained history.
        resume: Option<ProviderSessionId>,
    },
    /// Create fresh temporary history with the complete isolation guarantee.
    IsolatedText(IsolatedTextPolicy),
}

impl SessionHistory {
    /// Get the durable resume identity without confusing temporary history with a saved session.
    pub fn resume_id(&self) -> Option<&ProviderSessionId> {
        match self {
            Self::Durable { resume } => resume.as_ref(),
            Self::IsolatedText(_) => None,
        }
    }
}

/// Request fresh temporary history with only explicitly supplied instructions.
/// Exclude inherited instructions, hooks, skills, and MCP; disable native and host tools,
/// permission prompts, and autonomous goal work. This policy cannot contain a resume ID and
/// is independent of session role. Adapters must enforce every guarantee or reject before
/// dispatch; an empty host tool list alone is insufficient. Successful close removes the history.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct IsolatedTextPolicy {
    _private: (),
}

impl IsolatedTextPolicy {
    /// Create the complete isolated-text policy; no partial isolation mode is available.
    pub fn new() -> Self {
        Self { _private: () }
    }
}

/// Select the complete resource set for one retained provider session.
/// Empty selections exclude inherited resources rather than restoring native defaults.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct ResourceSelection {
    /// Select portable skills by normalized definitions.
    pub skills: Vec<crate::catalog::SkillDefinition>,
    /// Configure MCP launches without exposing provider-specific resource objects.
    pub mcp: Vec<McpLaunch>,
}

/// Keep host-owned temporary resources alive for the binding or invocation which received the lease.
/// Native owners release strong lease copies after that scope is collected. Replay keys retain
/// weak identity only; cached operation metadata must not keep temporary resources alive.
pub trait ResourceLease: Send + Sync {}

/// Describe a normalized MCP launch configuration with ephemeral credentials.
#[derive(Clone, Debug, Eq, PartialEq)]
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
    /// Admit all tools exposed by this explicitly selected MCP resource.
    All,
    /// Admit exactly these validated names; an empty list admits none.
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

    /// Reject empty names and the all-tools wildcard before adapter transport encoding.
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
    /// A selected tool name is empty or whitespace-only.
    #[error("MCP tool name cannot be empty")]
    Empty,
    /// The all-tools wildcard was mixed with named selections.
    #[error("MCP wildcard must be the entire tool selection")]
    Wildcard,
}

/// Select the normalized connection shape for one MCP resource.
#[derive(Clone, Eq, PartialEq)]
pub enum McpTransport {
    /// Launch an explicitly configured server over standard input and output.
    Stdio {
        /// Host-resolved executable or command name.
        command: String,
        /// Adapter arguments, potentially containing credentials; never log these values.
        arguments: Vec<String>,
        /// Ephemeral environment values supplied only to the selected server.
        environment: std::collections::BTreeMap<String, String>,
    },
    /// Connect to an explicitly selected HTTP server.
    Http {
        /// Server address, potentially credential-bearing.
        url: String,
        /// Ephemeral authentication and transport headers.
        headers: std::collections::BTreeMap<String, String>,
    },
    /// Connect using the selected server-sent events transport.
    Sse {
        /// Server address, potentially credential-bearing.
        url: String,
        /// Ephemeral authentication and transport headers.
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

/// Own one journaled delivery attempt and its immutable invocation authority.
/// The host issues scope, submission, and attempt IDs. The adapter validates them together
/// with the dispatch reporter and receipt before sending. All callbacks use this request's
/// endpoints and authority; none may fall back to a previous invocation. Release strong
/// callback/resource copies when their native scope is collected, keeping only weak replay identity.
#[derive(Clone)]
pub struct PromptRequest {
    /// Address the registered host thread before native dispatch.
    pub thread_id: crate::ThreadId,
    /// Address this host response cycle before a native turn exists.
    pub turn_id: crate::TurnId,
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
    /// Identify the durable input journal entry.
    pub submission_id: SubmissionId,
    /// Identify this immutable delivery attempt independently of logical input.
    pub attempt_id: crate::DeliveryAttemptId,
    /// Carry prepared user input.
    pub input: crate::input::PromptInput,
    /// Confirm tracked transport handoff independently of eventual native acceptance.
    /// Validate binding, scope, submission, and attempt before admission; confirm only after
    /// immutable configuration and cancellation/collection ownership are installed.
    pub dispatch: crate::submission::DispatchReporter,
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
    /// Retain separately host-owned publication authority; native settlement must not await host exchange jobs.
    pub host_effects: std::sync::Arc<dyn ProviderHostEffects>,
}

impl PromptRequest {
    /// Get every original journal attempt accepted by this grouped native input.
    pub fn input_coverage(&self) -> Vec<crate::CoveredInput> {
        self.recovered_inputs
            .iter()
            .map(|input| input.covered_input.clone())
            .chain(std::iter::once(crate::CoveredInput::new(
                self.submission_id.clone(),
                self.attempt_id.clone(),
            )))
            .collect()
    }

    /// Reject normalized turn identities that do not belong to the admitted host scope.
    pub fn validate_turn_identity(&self) -> Result<(), crate::ProviderError> {
        if self.thread_id != self.scope.owner.thread_id()
            || self.scope.turn_id().as_ref() != Ok(&self.turn_id)
        {
            return Err(crate::ProviderFailure::new(
                crate::ProviderOperation::TurnStart,
                crate::ProviderFailureKind::WrongInvocation,
            )
            .into());
        }
        Ok(())
    }

    /// Snapshot complete replay identity without retaining callback or resource ownership.
    pub fn identity(&self) -> crate::PromptRequestIdentity {
        self.into()
    }
}

impl PartialEq for PromptRequest {
    /// Compare every immutable input value, attempt, dispatch channel, and callback authority.
    fn eq(&self, other: &Self) -> bool {
        self.thread_id == other.thread_id
            && self.turn_id == other.turn_id
            && self.scope == other.scope
            && self.mode == other.mode
            && self.configuration == other.configuration
            && self.task == other.task
            && self.tools == other.tools
            && self.tool_endpoint == other.tool_endpoint
            && same_lease(&self.resource_lease, &other.resource_lease)
            && self.submission_id == other.submission_id
            && self.attempt_id == other.attempt_id
            && self.input == other.input
            && self.dispatch == other.dispatch
            && self.recovered_inputs == other.recovered_inputs
            && self.instructions == other.instructions
            && self.attachments == other.attachments
            && std::sync::Arc::ptr_eq(&self.receipt, &other.receipt)
            && std::sync::Arc::ptr_eq(&self.permissions, &other.permissions)
            && std::sync::Arc::ptr_eq(&self.provider_tools, &other.provider_tools)
            && std::sync::Arc::ptr_eq(&self.turn_control, &other.turn_control)
            && std::sync::Arc::ptr_eq(&self.host_effects, &other.host_effects)
    }
}
impl Eq for PromptRequest {}

/// Own the adapter-issued live generation and its separately provider-issued durable history ID.
/// A resumed history receives a new live ID. If durable identity resolves later, publish a
/// binding-scoped SessionIdentity observation before host persistence relies on that identity.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SessionBinding {
    /// Retain the host conversation identity independently of native history resolution.
    pub thread_id: crate::ThreadId,
    /// Identify this exact live reader and callback generation.
    pub id: BindingId,
    /// Identify persisted conversation history, when available.
    pub provider_session_id: Option<ProviderSessionId>,
}

/// Own steering input targeted at one exact active host invocation.
/// Journaled delivery has a fixed receipt/attempt identity; contextual notifications remain
/// unjournaled. Neither form authorizes fallback to a later invocation or a new prompt.
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
}

impl SteeringRequest {
    /// Snapshot complete replay identity without retaining callback or resource ownership.
    pub fn identity(&self) -> crate::SteeringRequestIdentity {
        self.into()
    }
}

impl PartialEq for SteeringRequest {
    /// Compare the exact invocation, journal authority, and independently delivered input.
    fn eq(&self, other: &Self) -> bool {
        self.scope == other.scope
            && self.delivery == other.delivery
            && self.input == other.input
            && self.attachments == other.attachments
            && self.task == other.task
    }
}
impl Eq for SteeringRequest {}

/// Preserve input accountability without inventing journal identities for host notifications.
#[derive(Clone)]
pub enum SteeringDelivery {
    /// Deliver independently journaled user input under its immutable attempt receipt.
    Journaled {
        /// Host journal authority bound to one immutable submission and delivery attempt.
        receipt: std::sync::Arc<dyn crate::submission::InputReceipt>,
    },
    /// Deliver a host contextual notification without fabricating a submission identity.
    Context,
}

impl PartialEq for SteeringDelivery {
    /// Compare journal authority identity while preserving explicitly unjournaled notifications.
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Journaled { receipt: left }, Self::Journaled { receipt: right }) => {
                std::sync::Arc::ptr_eq(left, right)
            }
            (Self::Context, Self::Context) => true,
            (Self::Journaled { .. }, Self::Context) | (Self::Context, Self::Journaled { .. }) => {
                false
            }
        }
    }
}
impl Eq for SteeringDelivery {}

/// Confirm the exact class of input incorporated by a steering operation.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum SteeringAcceptance {
    /// Positive native acceptance of the independently journaled input.
    Journaled {
        /// Host-issued logical input accepted by the targeted invocation.
        submission_id: SubmissionId,
    },
    /// Positive native delivery of an explicitly unjournaled contextual notification.
    Context,
}

impl SteeringDelivery {
    /// Get journal authority only for independently accountable user input.
    pub fn receipt(&self) -> Option<&std::sync::Arc<dyn crate::submission::InputReceipt>> {
        match self {
            Self::Journaled { receipt } => Some(receipt),
            Self::Context => None,
        }
    }

    /// Get the host-issued logical input ID without inventing one for contextual notifications.
    pub fn submission_id(&self) -> Option<&SubmissionId> {
        self.receipt().map(|receipt| receipt.id())
    }
}

/// Prove acceptance of the request's exact journaled attempt without native message identifiers.
/// The retained operation context and request bind this logical ID to its attempt. This value
/// does not prove terminal execution or foreground collection; those arrive through observations.
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

/// Collect host-owned effects independently of native provider settlement.
/// Harness invokes this barrier while it continues consuming native observations; adapters
/// must not await it to prove their native worker and callback resources were collected.
#[async_trait::async_trait]
pub trait ProviderHostEffects: Send + Sync {
    /// Wait for host-owned foreground effects and receipt writes at the host exchange barrier.
    async fn settle_foreground(
        &self,
        binding: &BindingId,
        scope: &InvocationScope,
    ) -> Result<(), HostEffectFailure>;
}

/// Own opaque adapter-issued evidence scoped to its history, logical input, and delivery attempt.
/// Persist required evidence for post-binding recovery and return it only to its issuing adapter.
/// A token is not replay permission, a continuation capability, or proof of successful retraction;
/// its issuer determines whether the retained history still makes the evidence authoritative.
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
    /// Create nonempty opaque delivery evidence issued by its owning adapter.
    pub fn new(bytes: Vec<u8>) -> Result<Self, RecoveryTokenError> {
        if bytes.is_empty() {
            return Err(RecoveryTokenError);
        }
        Ok(Self(bytes))
    }

    /// Get opaque evidence bytes for persistence or return to the issuing adapter.
    pub fn as_bytes(&self) -> &[u8] {
        &self.0
    }
}

impl TryFrom<Vec<u8>> for RecoveryToken {
    type Error = RecoveryTokenError;

    /// Validate deserialized delivery evidence using the same invariant as `new`.
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
    /// Positive native evidence establishes acceptance of the queried delivery attempt.
    Accepted {
        /// Adapter-issued accepted-input evidence available for durable recovery.
        receipt: Option<RecoveryToken>,
        /// Durable history positively correlated with that acceptance, when resolved.
        provider_session_id: Option<ProviderSessionId>,
    },
    /// The adapter found positive, protocol-supported evidence of non-consumption.
    NotAccepted,
    /// Available evidence cannot prove acceptance or rejection and forbids replay.
    Unknown,
}

/// Compare optional immutable resource ownership without interpreting an external lease.
fn same_lease(
    left: &Option<std::sync::Arc<dyn ResourceLease>>,
    right: &Option<std::sync::Arc<dyn ResourceLease>>,
) -> bool {
    match (left, right) {
        (Some(left), Some(right)) => std::sync::Arc::ptr_eq(left, right),
        (None, None) => true,
        (Some(_), None) | (None, Some(_)) => false,
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
