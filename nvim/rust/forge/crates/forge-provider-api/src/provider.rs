//! Provider-neutral operations implemented by native provider owners.

use async_trait::async_trait;
use serde::{Deserialize, Serialize};

use crate::{
    capability::{CapabilityManifest, ProviderFeature, UnsupportedCapability},
    catalog::{CatalogMutation, McpDefinition, ProviderCatalogRequest, SkillDefinition},
    events::ProviderEventStream,
    session::{
        BindingId, ContinuationOutcome, ContinuationRequest, InputAcceptance, PromptRequest,
        RecoveryToken, ResourceSelection, SessionBinding, SessionRequest, SessionSettings,
        SteeringRequest, SubmissionEvidence,
    },
    terminal::TerminalSnapshot,
    *,
};

/// Own a normalized persistent session and every native lifecycle behind its binding.
#[async_trait]
pub trait Provider: Send + Sync + 'static {
    /// Create or resume a conversation and retain its authoritative event reader.
    async fn initialize_session(
        &self,
        request: SessionRequest,
    ) -> Result<SessionBinding, ProviderError>;

    /// Take the session's one normalized stream before admitting input.
    async fn take_session_events(
        &self,
        binding: &BindingId,
    ) -> Result<ProviderEventStream, ProviderError>;

    /// Submit input to the retained session; success records acceptance, not completion.
    async fn session_prompt(
        &self,
        binding: &BindingId,
        request: PromptRequest,
    ) -> Result<InputAcceptance, ProviderError>;

    /// Continue the same invocation only with provider proof that its prior input was not consumed.
    async fn continue_session(
        &self,
        binding: &BindingId,
        request: ContinuationRequest,
    ) -> Result<ContinuationOutcome, ProviderError>;

    /// Submit steering to the original execution owner without exposing native turn placement.
    async fn session_steer(
        &self,
        binding: &BindingId,
        request: SteeringRequest,
    ) -> Result<SteeringAcceptance, ProviderError>;

    /// Revoke callback authority and collect a failed binding before replacement.
    async fn retire_session(&self, binding: &BindingId) -> Result<(), ProviderError>;

    /// Fence a binding whose event observation is no longer reliable.
    async fn fence_session(&self, binding: &BindingId) -> Result<(), ProviderError>;

    /// Resume a persisted conversation using opaque provider-owned recovery evidence.
    async fn recover_session(
        &self,
        request: SessionRequest,
        token: Option<RecoveryToken>,
    ) -> Result<SessionBinding, ProviderError>;

    /// Inspect installed runtime state and normalized authentication choices.
    async fn setup(&self, request: ProviderCatalogRequest) -> Result<ProviderSetup, ProviderError> {
        let _ = request;
        Err(UnsupportedCapability(ProviderFeature::RuntimeSetup).into())
    }

    /// Authenticate with one advertised method without admitting model input.
    async fn authenticate(
        &self,
        request: ProviderCatalogRequest,
        method: AuthenticationMethodId,
    ) -> Result<(), ProviderError> {
        let _ = (request, method);
        Err(UnsupportedCapability(ProviderFeature::Authentication).into())
    }

    /// Return normalized support and discovery state for the selected session.
    async fn capabilities(
        &self,
        request: ProviderCatalogRequest,
    ) -> Result<CapabilityManifest, ProviderError> {
        let _ = request;
        Ok(CapabilityManifest::from_descriptor(self.descriptor()))
    }

    /// Reconcile normalized model, access, and admitted resource choices.
    /// Preserve the existing binding and its one event stream while replacing native resources.
    /// Fence the binding and report observation loss if replacement cannot be completed safely.
    async fn reconcile_settings(
        &self,
        request: ProviderCatalogRequest,
        settings: SessionSettings,
        resources: ResourceSelection,
    ) -> Result<(), ProviderError> {
        let _ = (request, settings, resources);
        Err(UnsupportedCapability(ProviderFeature::ResourceSelection).into())
    }

    /// Collect all provider-owned sessions and runtimes after their readers are drained.
    async fn shutdown(&self) -> Result<(), ProviderError>;

    /// Expose normalized deterministic controls when this provider is a test backend.
    fn test_control(&self) -> Option<std::sync::Arc<dyn crate::testing::ProviderTestControl>> {
        None
    }

    /// Return stable provider identity and capability without starting provider work.
    fn descriptor(&self) -> ProviderDescriptor;

    /// Report task steering support through normalized task semantics.
    fn supports_task_steering(&self, kind: input::TaskKind) -> bool {
        kind == input::TaskKind::Goal
    }

    /// Reconcile one journal entry using adapter-owned history and opaque recovery evidence.
    async fn reconcile_submission(
        &self,
        submission_id: &SubmissionId,
        token: Option<&RecoveryToken>,
        accepted_receipts: &[RecoveryToken],
    ) -> Result<SubmissionEvidence, ProviderError> {
        let _ = (submission_id, token, accepted_receipts);
        Ok(SubmissionEvidence::Unknown)
    }

    /// Create durable target history when the capability declares native support.
    /// The host subsequently initializes its live binding with the returned history identity.
    async fn fork(
        &self,
        request: ConversationForkRequest,
    ) -> Result<ProviderForkResult, ProviderError> {
        let _ = request;
        Err(UnsupportedCapability(ProviderFeature::NativeFork).into())
    }

    /// Query live provider shells independently of prompt execution and completion.
    async fn background_terminals(
        &self,
        request: ProviderCatalogRequest,
    ) -> Result<TerminalSnapshot, ProviderError> {
        let _ = request;
        Err(UnsupportedCapability(ProviderFeature::BackgroundTerminals).into())
    }

    /// Terminate one provider-owned shell without interrupting its exchange.
    async fn terminate_terminal(
        &self,
        request: ProviderCatalogRequest,
        terminal_id: &str,
    ) -> Result<(), ProviderError> {
        let _ = (request, terminal_id);
        Err(UnsupportedCapability(ProviderFeature::BackgroundTermination).into())
    }

    /// Generate text in an isolated temporary operation independent of persistent sessions.
    async fn generate_text(&self, request: TextGenerationRequest) -> Result<String, ProviderError> {
        let _ = request;
        Err(UnsupportedCapability(ProviderFeature::TextGeneration).into())
    }

    /// List models in normalized preference order for the model picker.
    async fn model_list(
        &self,
        request: ProviderCatalogRequest,
    ) -> Result<Vec<ProviderModel>, ProviderError> {
        let _ = request;
        Err(UnsupportedCapability(ProviderFeature::ModelSelection).into())
    }

    /// List skills using normalized state and stable identity.
    async fn skill_list(
        &self,
        request: ProviderCatalogRequest,
    ) -> Result<Vec<SkillDefinition>, ProviderError> {
        let _ = request;
        Err(UnsupportedCapability(ProviderFeature::SkillCatalog).into())
    }

    /// Persist one skill's enabled state for future turns.
    async fn set_skill_enabled(
        &self,
        request: ProviderCatalogRequest,
        name: &str,
        enabled: bool,
    ) -> Result<CatalogMutation, ProviderError> {
        let _ = (request, name, enabled);
        Err(UnsupportedCapability(ProviderFeature::SkillMutation).into())
    }

    /// List configured MCP resources without exposing native launch representation.
    async fn mcp_configuration(
        &self,
        request: ProviderCatalogRequest,
    ) -> Result<Vec<McpDefinition>, ProviderError> {
        let _ = request;
        Err(UnsupportedCapability(ProviderFeature::McpCatalog).into())
    }

    /// Query normalized MCP readiness and available tools.
    async fn mcp_list(
        &self,
        request: ProviderCatalogRequest,
    ) -> Result<Vec<McpDefinition>, ProviderError> {
        let _ = request;
        Err(UnsupportedCapability(ProviderFeature::McpCatalog).into())
    }

    /// Persist one MCP resource's enabled state and report whether a turn must resume.
    async fn set_mcp_enabled(
        &self,
        request: ProviderCatalogRequest,
        name: &str,
        enabled: bool,
    ) -> Result<CatalogMutation, ProviderError> {
        let _ = (request, name, enabled);
        Err(UnsupportedCapability(ProviderFeature::McpMutation).into())
    }

    /// Report whether one execution owner currently has active provider work.
    async fn has_active_turn(&self, owner: &ExecutionOwner) -> bool;

    /// Clear provisional turn identity before admitting another invocation.
    async fn prepare_turn(&self, owner: &ExecutionOwner) -> Result<(), ProviderError> {
        let _ = owner;
        Ok(())
    }

    /// Stop native continuation through the active reader before host coordination locks.
    async fn stop_goal_session(
        &self,
        owner: &ExecutionOwner,
        mode: GoalStopMode,
    ) -> Result<(), ProviderError> {
        let _ = (owner, mode);
        Err(UnsupportedCapability(ProviderFeature::NativeGoal).into())
    }

    /// Update a provider-owned goal using normalized status.
    async fn goal_status(
        &self,
        owner: &ExecutionOwner,
        update: NativeGoalUpdate,
    ) -> Result<(), ProviderError> {
        let _ = (owner, update);
        Err(UnsupportedCapability(ProviderFeature::NativeGoal).into())
    }

    /// Compact provider-owned history when the normalized capability supports it.
    async fn compact(
        &self,
        binding: &BindingId,
        request: CompactionRequest,
    ) -> Result<Compaction, ProviderError> {
        let _ = (binding, request);
        Err(UnsupportedCapability(ProviderFeature::NativeCompact).into())
    }

    /// Cancel one binding and wait for its normalized terminal evidence and collection.
    async fn cancel_session(&self, binding: &BindingId) -> Result<(), ProviderError>;

    /// Resolve one execution's foreground native work while retaining completed background tools.
    async fn cleanup_execution(&self, binding: &BindingId) -> Result<(), ProviderError>;

    /// Return a conversation identity after interruption for persistence.
    async fn active_session_id_for(
        &self,
        owner: &ExecutionOwner,
    ) -> Result<Option<ProviderSessionId>, ProviderError>;

    /// Roll back output-free cancellation only after the host confirms it is safe.
    async fn rollback_cancelled_turn(&self, binding: &BindingId) -> Result<bool, ProviderError> {
        let _ = binding;
        Ok(false)
    }
}

/// Select an authentication method advertised by normalized setup discovery.
#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
#[serde(try_from = "String", into = "String")]
pub struct AuthenticationMethodId(String);

impl AuthenticationMethodId {
    /// Create an opaque nonempty method identity.
    pub fn new(value: impl Into<String>) -> Result<Self, AuthenticationMethodIdError> {
        let value = value.into();
        if value.trim().is_empty() {
            return Err(AuthenticationMethodIdError);
        }
        Ok(Self(value))
    }

    /// Get the opaque method identity for round-tripping to its owning provider.
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl TryFrom<String> for AuthenticationMethodId {
    type Error = AuthenticationMethodIdError;

    /// Validate deserialized authentication identities through the constructor.
    fn try_from(value: String) -> Result<Self, Self::Error> {
        Self::new(value)
    }
}

impl From<AuthenticationMethodId> for String {
    /// Encode the opaque method identity for host transport.
    fn from(value: AuthenticationMethodId) -> Self {
        value.0
    }
}

/// Reject an empty authentication method identity.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct AuthenticationMethodIdError;

impl std::fmt::Display for AuthenticationMethodIdError {
    /// Describe the invalid authentication identity at the boundary.
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str("authentication method identity is empty")
    }
}

impl std::error::Error for AuthenticationMethodIdError {}

/// Describe typed runtime setup and advertised authentication choices.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ProviderSetup {
    /// Report whether a compatible runtime is installed and available.
    pub runtime: RuntimeAvailability,
    /// Identify how the inspected runtime was selected.
    pub origin: RuntimeOrigin,
    /// Report executable and adapter versions independently of runtime readiness.
    pub versions: RuntimeVersions,
    /// Advertise supported authentication methods.
    pub authentication: Vec<AuthenticationMethod>,
}

/// Report whether the selected provider runtime can be used.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum RuntimeAvailability {
    Available,
    Missing,
    Incompatible,
    Unavailable,
}

/// Distinguish an explicitly configured runtime from a retained managed installation.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum RuntimeOrigin {
    Configured,
    Managed,
}

/// Preserve independently observed runtime components without exposing probe output.
#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub struct RuntimeVersions {
    /// Installed model-provider executable, when the adapter can inspect it.
    pub provider: VersionObservation,
    /// Separately installed protocol adapter, when one is required.
    pub adapter: VersionObservation,
}

/// Describe a version probe without carrying native error text or process output.
#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum VersionObservation {
    Known {
        version: String,
    },
    #[default]
    NotReported,
    Failed {
        failure: crate::ProviderFailureKind,
    },
}

/// Describe one opaque authentication method to a setup caller.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct AuthenticationMethod {
    /// Opaque method identity passed back to `authenticate`.
    pub id: AuthenticationMethodId,
    /// User-facing method label.
    pub label: String,
    /// User-facing explanation of the interaction.
    pub description: String,
    /// Identify whether setup opens a terminal or delegates interaction to the provider.
    pub interaction: AuthenticationInteraction,
}

/// Identify the normalized authentication interaction surface.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum AuthenticationInteraction {
    ProviderManaged,
    Terminal { launch: TerminalLaunch },
}

/// Carry an opaque native launch specification for a terminal authentication action.
#[derive(Clone, Deserialize, Eq, PartialEq, Serialize)]
pub struct TerminalLaunch {
    /// Select the executable already resolved by the owning provider.
    pub program: std::path::PathBuf,
    /// Preserve provider-authored arguments without asking the host to parse them.
    pub arguments: Vec<String>,
    /// Carry ephemeral environment values needed for the selected interaction.
    pub environment: std::collections::BTreeMap<String, String>,
    /// Run the command in the workspace selected for setup.
    pub working_directory: Option<std::path::PathBuf>,
}

impl std::fmt::Debug for TerminalLaunch {
    /// Hide ephemeral authentication arguments and environment values from diagnostics.
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter
            .debug_struct("TerminalLaunch")
            .field("program", &self.program)
            .field("arguments", &"[redacted]")
            .field("environment", &self.environment.keys().collect::<Vec<_>>())
            .field("working_directory", &self.working_directory)
            .finish()
    }
}

/// Carry a normalized compaction operation identity and scope.
#[derive(Clone, Debug)]
pub struct CompactionRequest {
    /// Identify this session operation independently of prompt invocations.
    pub operation_id: String,
    /// Own its event stream observations.
    pub scope: EventScope,
}

/// Choose whether stopping a goal retains its provider conversation identity.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum GoalStopMode {
    RetainConversation,
    ClearConversation,
}
