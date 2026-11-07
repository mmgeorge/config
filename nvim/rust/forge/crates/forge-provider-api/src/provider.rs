//! Provider-neutral operations implemented by native provider owners.

use async_trait::async_trait;
use serde::{Deserialize, Serialize};

use crate::{
    capability::{ProviderFeature, UnsupportedCapability},
    catalog::{McpDefinition, McpQuery, QueryContext, SkillDefinition, WorkspaceContext},
    terminal::{TerminalId, TerminalInventory},
    *,
};

/// Own native resources behind exactly scoped provider-neutral operations.
///
/// Harness issues one immutable [`OperationContext`] per retained operation and reuses
/// its clone when observing that operation again. Validate the complete context and typed
/// request before effects. Replay recorded outcomes before checking expiry, reject changed
/// tuples, and never resolve unknown or stale identities to current work. These in-process
/// records do not promise automatic mutation recovery after a crash.
///
/// A dropped awaiter or cancellation source does not cancel work. An explicit cancellation
/// request or deadline can end observation while native work and cleanup remain owned.
/// Preserve the actual phase and effect disposition; timeout never proves non-consumption.
/// Later exact observation may obtain authoritative evidence which supersedes provisional
/// uncertainty. Every returned error path owns its required revocation and cleanup; a native
/// future returning cancellation may complete before an observation interruption hook runs.
/// Open transfers its sole stream once; a second completed transfer reports
/// [`ProviderFailureKind::OperationResultTaken`] without allocating another binding.
///
/// An operation future establishes its documented completion boundary. Stream observations
/// establish execution history; native settlement additionally proves foreground collection.
/// Publish required observations before resolving watermark-bearing results. The owner must
/// consume through each returned watermark; early stream closure is observation failure.
/// Native settlement does not await Harness-owned exchange jobs. Background terminals have
/// separate binding ownership until explicit termination or full close.
///
/// Fresh open, submit, cancel, automatic settlement, close, shutdown, and inspect are core
/// obligations. Optional operations or modes may reject as unsupported before mutation.
/// Resume, isolated text, configuration changes, fork, compact, steering, and retraction
/// require their full advertised contracts. Exact no-op configuration may succeed without
/// claiming unsupported changes. Catalog reads never become configuration writes.
#[async_trait]
pub trait Provider: Send + Sync + 'static {
    /// Open fresh or resumed history together with a fresh binding, negotiated support, and its sole reader.
    /// Create the reader before returning and retain cleanup across dropped awaits. The caller
    /// installs stream consumption before submission. Resume and isolated modes may be unsupported;
    /// reject them before allocation or dispatch. Successful close deletes isolated temporary history.
    async fn thread_open(
        &self,
        context: OperationContext,
        request: SessionRequest,
    ) -> Result<OpenedSession, ProviderError>;

    /// Apply complete defaults to the exact settled binding without interrupting execution.
    /// Preserve binding identity, its stream, and surviving resource ownership. Success contains
    /// the confirmed configuration, matching capabilities, and observation watermark. Reject busy
    /// or unsupported changes before writes; uncertain partial replacement invalidates the binding.
    /// Invocation overrides remain local to their invocation and do not replace these defaults.
    async fn thread_configure(
        &self,
        context: OperationContext,
        binding: &BindingId,
        configuration: SessionConfiguration,
    ) -> Result<ConfiguredSession, ProviderError>;

    /// Create separate durable target history from a settled source or immutable checkpoint.
    /// Establish the stable source before target mutation; leave the source unchanged and do not
    /// open the target's live binding. Retain disposition under the context ID. An ambiguous target
    /// creation does not authorize a fresh fork; unsupported sources reject before target creation.
    async fn thread_fork(
        &self,
        context: OperationContext,
        request: SessionForkRequest,
    ) -> Result<ProviderForkResult, ProviderError> {
        context.check_admission(ProviderOperation::ThreadFork)?;
        let _ = request;
        Err(UnsupportedCapability(ProviderFeature::NativeFork).into())
    }

    /// Compact settled history and return correlated completion evidence and its event watermark.
    /// Require SessionOperation scope with the same ID as the context; reject a mismatched scope,
    /// active execution, or unsupported compaction before effects. An RPC acknowledgement alone
    /// does not establish completion. Retain uncertain disposition so retry cannot compact twice.
    async fn thread_compact(
        &self,
        context: OperationContext,
        binding: &BindingId,
        request: CompactionRequest,
    ) -> Result<CompactionResult, ProviderError> {
        context.check_admission(ProviderOperation::ThreadCompact)?;
        let _ = (binding, request);
        Err(UnsupportedCapability(ProviderFeature::NativeCompact).into())
    }

    /// Revoke admission and callback authority before transport waits, then collect the entire binding.
    /// Collect foreground workers, callbacks, readers, profiles, and owned terminals while retaining
    /// durable history. Success also deletes temporary history. Preserve the first failure and all
    /// unfinished ownership for later collection; failure never restores admission or touches a
    /// replacement generation. No caller fence, cancel, or cleanup preamble is required.
    async fn thread_close(
        &self,
        context: OperationContext,
        binding: &BindingId,
    ) -> Result<(), ProviderError>;

    /// Interrupt the exact invocation and await its retained native foreground settlement.
    /// Leave the binding reusable only after successful collection, preserving independent background
    /// terminals. A completed target keeps its true terminal outcome; repeats observe that same
    /// invocation and cannot stop a successor. A stop acknowledgement alone is insufficient, and
    /// failed collection retains ownership for close rather than fabricating successful settlement.
    /// Serialize with queued input preparation, including cancellation before its first poll.
    /// WrongInvocation proves no matching preparation can still install native work: retain exact
    /// scope prevention and join its registration handoff before reporting absence. This never
    /// authorizes stopping another scope or treating unknown input delivery as not consumed.
    async fn turn_interrupt(
        &self,
        context: OperationContext,
        binding: &BindingId,
        request: CancelRequest,
    ) -> Result<TurnInterruptResult, ProviderError>;

    /// Admit journaled input into an idle execution slot with exact attempt and invocation authority.
    /// Install cancellation and collection ownership plus immutable configuration before dispatch;
    /// confirm transport handoff independently from acceptance, which may arrive only at turn end.
    /// Persist required receipts before acknowledging acceptance. Success proves delivery, not
    /// execution completion. Never steer, queue behind active work, or cancel another invocation.
    /// Admission/Preparation with Unchanged disposition is positive non-consumption evidence.
    /// After possible dispatch, preserve uncertainty unless native rejection proves non-consumption;
    /// represent that proven rejection as PreparationFailed with its original typed cause.
    async fn turn_start(
        &self,
        context: OperationContext,
        binding: &BindingId,
        request: PromptRequest,
    ) -> Result<InputAcceptance, ProviderError>;

    /// Deliver new input only to the identified active invocation.
    /// Preserve journaled receipts and explicitly unjournaled contextual notifications. Success
    /// identifies the delivered class; positive NotConsumed evidence and uncertain delivery remain
    /// distinct errors. A late request never becomes a new prompt or targets a successor. Unsupported
    /// steering rejects before sending or changing provider history.
    /// A completed target rejected before delivery is not uncertain delivery. Preserve its
    /// Admission/Unchanged cause, or use PreparationFailed for a positively rejected native send;
    /// malformed acknowledgement after dispatch remains Observation/Uncertain.
    async fn turn_steer(
        &self,
        context: OperationContext,
        binding: &BindingId,
        request: SteeringRequest,
    ) -> Result<SteeringAcceptance, ProviderError> {
        context.check_admission(ProviderOperation::TurnSteer)?;
        let _ = (binding, request);
        Err(UnsupportedCapability(ProviderFeature::NativeSteer).into())
    }

    /// Observe native evidence for one stored input attempt without sending or changing history.
    /// Remain callable after the original binding is lost. Report acceptance, positive final proof
    /// of non-consumption, or Unknown; absent or incomplete history alone is not negative proof.
    /// Unknown never authorizes replay. The host revalidates and durably commits the evidence.
    async fn input_reconcile(
        &self,
        context: OperationContext,
        request: InputReconcileRequest,
    ) -> Result<SubmissionEvidence, ProviderError> {
        context.check_admission(ProviderOperation::InputReconcile)?;
        let _ = request;
        Ok(SubmissionEvidence::Unknown)
    }

    /// Retract only the authorized cancelled submission at its checked settled history boundary.
    /// Require exact invocation, logical input, attempt, boundary, and host eligibility authorization.
    /// Never remove a newer history item or claim to undo filesystem/tool effects. Replay the same
    /// context's recorded outcome; after a possible mutation retain uncertainty and block unsafe reuse
    /// until disposition is established. Only proven retraction or non-consumption permits draft restoration.
    async fn input_retract(
        &self,
        context: OperationContext,
        binding: &BindingId,
        request: InputRetractRequest,
    ) -> Result<InputRetractOutcome, ProviderError> {
        context.check_admission(ProviderOperation::InputRetract)?;
        let _ = (binding, request);
        Err(UnsupportedCapability(ProviderFeature::NativeTurnRollback).into())
    }

    /// Read installation, versions, advertised authentication methods, and discoverable runtime support.
    /// Do not log in, change credentials, configure an existing binding, or start model execution.
    /// Runtime discovery is independent of capabilities negotiated for a particular live binding.
    async fn inspect(
        &self,
        context: OperationContext,
        workspace: WorkspaceContext,
    ) -> Result<ProviderSetup, ProviderError>;

    /// Execute one explicitly selected advertised method in the supplied workspace/runtime context.
    /// Credential changes may require interaction. Invalidate stale discovery and inspect again;
    /// do not replace live binding capabilities with unnegotiated discovery flags. Unsupported
    /// methods reject before credential mutation or launching an interaction.
    async fn authenticate(
        &self,
        context: OperationContext,
        workspace: WorkspaceContext,
        method: AuthenticationMethodId,
    ) -> Result<(), ProviderError> {
        context.check_admission(ProviderOperation::Authenticate)?;
        let _ = (workspace, method);
        Err(UnsupportedCapability(ProviderFeature::Authentication).into())
    }

    /// Close provider admission and collect every binding, pending open, utility operation, and shared runtime.
    /// Use the same binding-close semantics, including temporary-history removal and independent
    /// phase attempts. Success requires actual native/resource collection, not merely joined wrapper
    /// futures. Retain failures and unfinished ownership when interruption or cleanup cannot complete.
    async fn shutdown(&self, context: OperationContext) -> Result<(), ProviderError>;

    /// Read normalized opaque model choices for workspace discovery or the exact existing binding.
    /// Do not write configuration or implicitly open/restart a binding. Unsupported discovery
    /// rejects explicitly; an empty successful catalog means the provider reported no choices.
    async fn model_list(
        &self,
        context: OperationContext,
        query: QueryContext,
    ) -> Result<Vec<ProviderModel>, ProviderError> {
        context.check_admission(ProviderOperation::ModelList)?;
        let _ = query;
        Err(UnsupportedCapability(ProviderFeature::ModelSelection).into())
    }

    /// Read normalized native skill definitions and availability for the explicit query target.
    /// Keep enablement writes exclusively in thread_configure; reading cannot mutate an active
    /// binding. Unsupported discovery rejects rather than claiming an empty successful catalog.
    async fn skill_list(
        &self,
        context: OperationContext,
        query: QueryContext,
    ) -> Result<Vec<SkillDefinition>, ProviderError> {
        context.check_admission(ProviderOperation::SkillList)?;
        let _ = query;
        Err(UnsupportedCapability(ProviderFeature::SkillCatalog).into())
    }

    /// Read configured definitions promptly or observe readiness and tools on the explicit query target.
    /// Configured mode never starts servers or waits for readiness. Observed mode may inspect an
    /// existing runtime but cannot reconfigure active bindings. Unsupported modes reject before effects.
    async fn mcp_list(
        &self,
        context: OperationContext,
        query: McpQuery,
    ) -> Result<Vec<McpDefinition>, ProviderError> {
        context.check_admission(ProviderOperation::McpList)?;
        let _ = query;
        Err(UnsupportedCapability(ProviderFeature::McpCatalog).into())
    }

    /// Read shells owned by the exact binding independently of foreground execution.
    /// Normal settlement and cancellation leave surviving terminals listable. A missing binding
    /// rejects instead of implicitly opening or restarting a session; unsupported inventory is explicit.
    async fn terminal_list(
        &self,
        context: OperationContext,
        binding: &BindingId,
    ) -> Result<TerminalInventory, ProviderError> {
        context.check_admission(ProviderOperation::TerminalList)?;
        let _ = binding;
        Err(UnsupportedCapability(ProviderFeature::BackgroundTerminals).into())
    }

    /// Stop the typed terminal belonging to the exact binding without cancelling unrelated work.
    /// Reject stale or unsupported identities before native effects. A terminal identity from a
    /// retired generation cannot target a shell in a replacement binding.
    async fn terminal_terminate(
        &self,
        context: OperationContext,
        binding: &BindingId,
        terminal: &TerminalId,
    ) -> Result<(), ProviderError> {
        context.check_admission(ProviderOperation::TerminalTerminate)?;
        let _ = (binding, terminal);
        Err(UnsupportedCapability(ProviderFeature::BackgroundTermination).into())
    }
}

/// Identify a provider-issued authentication method advertised by runtime inspection.
/// Hosts round-trip this opaque value with the matching workspace/runtime selection. A later
/// inspection may invalidate stale choices; constructing a token does not authorize login.
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

/// Own one read-only installation and discovery snapshot.
/// Authentication methods are advertisements, not permission to execute them. Runtime support
/// remains distinct from the effective capabilities returned by an opened binding.
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
    /// Discoverable baseline support; this is not negotiated live-binding state.
    pub capabilities: crate::capability::CapabilityManifest,
}

/// Report whether the selected provider runtime can be used.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum RuntimeAvailability {
    /// A compatible selected runtime installation was positively found.
    Available,
    /// No selected runtime installation was found.
    Missing,
    /// The installed runtime does not meet the adapter's protocol/version requirements.
    Incompatible,
    /// Inspection could not establish a usable runtime for a reason other than absence or compatibility.
    Unavailable,
}

/// Distinguish an explicitly configured runtime from a retained managed installation.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum RuntimeOrigin {
    /// The host explicitly supplied the selected executable or adapter command.
    Configured,
    /// The integration selected its retained managed installation.
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
    /// A normalized installed component version was positively observed.
    Known {
        /// Provider-issued version text for display; callers do not parse protocol behavior from it.
        version: String,
    },
    /// The component does not expose an independently discoverable version.
    #[default]
    NotReported,
    /// Version inspection failed without exposing native process output.
    Failed {
        /// Normalized cause of the failed probe.
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
    /// The adapter owns any interaction required by the explicitly selected method.
    ProviderManaged,
    /// The host presents an adapter-authored terminal action after explicit authentication selection.
    Terminal {
        /// Credential-bearing launch values for the selected interaction; never a command to parse.
        launch: TerminalLaunch,
    },
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

/// Carry correlated compaction scope; the enclosing OperationContext owns its sole operation ID.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct CompactionRequest {
    /// Scope correlated observations to the matching operation context identity.
    pub scope: EventScope,
}

impl CompactionRequest {
    /// Validate the sole compaction identity before binding lookup, observation handoff, or native mutation.
    /// Binding and invocation scopes cannot identify a compaction operation. This check does
    /// not admit the operation or replace adapter validation of binding ownership and settlement.
    pub fn validate_context(&self, context: &OperationContext) -> Result<(), ProviderError> {
        match &self.scope {
            EventScope::SessionOperation { operation_id, .. } if operation_id == context.id() => {
                Ok(())
            }
            EventScope::SessionOperation { .. }
            | EventScope::Binding { .. }
            | EventScope::Invocation { .. } => Err(ProviderFailure::new(
                ProviderOperation::ThreadCompact,
                ProviderFailureKind::OperationConflict,
            )
            .into()),
        }
    }
}
