use crate::{ProviderDescriptor, ProviderId};
use serde::{Deserialize, Serialize};

/// Distinguishes supported behavior from provider limitations.
#[derive(Clone, Debug, Deserialize, Serialize, Eq, PartialEq)]
#[serde(rename_all = "snake_case")]
pub enum Support {
    /// The operation preserves its full Harness contract.
    Supported,
    /// The operation works with the reported restriction.
    Limited {
        /// Actionable typed restrictions; producers include at least one restriction for Limited support.
        restrictions: Vec<CapabilityRestriction>,
    },
    /// The adapter rejects the operation before dispatch.
    Unsupported,
}

/// Encode actionable limits independently of user-facing explanatory text.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum CapabilityRestriction {
    /// Enforce filesystem scopes only for operations which use normalized permission callbacks.
    PermissionCallbacksOnly,
    /// The selected read scope is not enforced as operating-system read confinement.
    UnconfinedReads,
    /// Native edit tools may omit structured diff output; host checkpoint review remains available.
    UnstructuredDiffs,
    /// Provider enterprise terminal policy can execute without normalized approval callbacks.
    EnterpriseTerminalPolicy,
    /// Inherited project-native skills remain available despite the explicit portable selection.
    InheritedSkills,
    /// Require the exact source checkpoint for history mutation.
    CheckpointRequired,
    /// Require settled foreground execution before applying configuration or history mutation.
    SettledOnly,
}

/// Identify a provider-implemented capability independently of provider names or native
/// protocol methods. Harness-owned workflows are host capabilities, not provider capabilities.
#[derive(Clone, Copy, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum CapabilityId {
    /// Journaled model input admission.
    Prompt,
    /// Exact invocation cancellation and settlement.
    Cancel,
    /// Durable history resume through a fresh binding.
    Resume,
    /// Native foreground collection, automatic on completion and part of cancel/close.
    Cleanup,
    /// Invocation-scoped host control tools.
    Tools,
    /// Separate native retained-history fork.
    NativeFork,
    /// Explicit correlated native history compaction.
    NativeCompact,
    /// Exact active-invocation input delivery.
    NativeSteer,
    /// Checked cancelled-input history retraction.
    NativeTurnRollback,
    /// Native compaction lifecycle observations.
    CompactionLifecycle,
    /// Compaction observations outside active input execution.
    BackgroundCompaction,
    /// Provider-exposed compaction summary evidence.
    CompactionSummary,
    /// Provider-exposed compaction usage evidence.
    CompactionMetrics,
    /// Opaque normalized model selection.
    ModelSelection,
    /// Normalized reasoning effort selection.
    EffortSelection,
    /// Fast service policy selection.
    FastMode,
    /// Ultrafast service policy selection.
    UltrafastMode,
    /// Native skill definition discovery and enforceable selection.
    SkillCatalog,
    /// Configured and observed MCP discovery.
    McpCatalog,
    /// Normalized permission callbacks for native tool requests.
    PermissionCallbacks,
    /// Enforceable operating-system isolation policy.
    OsSandbox,
    /// Binding-owned shell inventory.
    BackgroundTerminals,
    /// Structured provider file-change observations.
    FileDiff,
    /// Independent binding-owned shell termination.
    BackgroundTermination,
    /// Text generation in an isolated history that never appends to the main conversation.
    IsolatedGeneration,
    /// Runtime installation/version inspection.
    RuntimeSetup,
    /// Explicit provider authentication interaction.
    Authentication,
    /// Enforcement of host-owned delegation authority.
    NativeAgentSuppression,
    /// Skill enablement applied through complete session configuration.
    SkillMutation,
    /// MCP enablement applied through complete session configuration.
    McpMutation,
    /// Read-only observed MCP readiness.
    McpRuntimeStatus,
    /// Read-only observed MCP tools.
    McpToolCatalog,
    /// Normalized filesystem authority and its typed restrictions.
    FilesystemAccess,
}

/// Specify when a changed setting takes effect.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum CapabilityApplication {
    /// No additional deferred application phase is advertised; operation preconditions still apply.
    Immediate,
    /// Host scheduling waits for no active execution and still honors operation-specific collection barriers.
    Idle,
    /// Apply complete configuration after invocation settlement.
    Settled,
}

/// Report one provider capability's support and when a change to it takes effect.
#[derive(Clone, Debug, Deserialize, Serialize, Eq, PartialEq)]
pub struct Capability {
    /// Identifies the operation across providers and refreshes.
    pub id: CapabilityId,
    /// Reports support independently of discovery availability.
    pub support: Support,
    /// Describes the configuration application boundary.
    pub application: CapabilityApplication,
}

/// Own one coherent baseline or negotiated support snapshot for admission.
/// Registration supplies baseline support; open/configure return effective binding snapshots
/// and ordered capability observations publish later changes. Loading/failure state belongs
/// to host discovery, not placeholder availability fields in this manifest. The host composes
/// menus and Harness-owned workflow availability from these facts.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct CapabilityManifest {
    /// Identifies the selected provider.
    pub provider: ProviderId,
    /// Capability schema version, independent of event sequencing and negotiated support changes.
    pub revision: u64,
    /// Binding-local support revision, advanced by the adapter when negotiated support changes.
    pub effective_revision: u64,
    /// Retains the negotiated agent version when the transport exposes it.
    pub provider_version: Option<String>,
    /// Reports unavailable default resources without exposing credential values.
    pub notice: Vec<String>,
    /// Contains supported and unsupported operations.
    pub capability: Vec<Capability>,
}

impl CapabilityManifest {
    /// Create a baseline inventory from the supplied descriptor with effective revision zero.
    /// Adapters refine binding-specific support and its revision before publishing effective snapshots;
    /// registry metadata alone does not prove that a runtime negotiated optional support.
    pub fn from_descriptor(descriptor: ProviderDescriptor) -> Self {
        use CapabilityApplication::{Idle, Immediate, Settled};
        let flags = &descriptor.capability;
        let integration = &flags.integration;
        let mut capability: Vec<Capability> = [
            (CapabilityId::Prompt, true, Immediate),
            (CapabilityId::Cancel, true, Immediate),
            (CapabilityId::Resume, true, Immediate),
            (CapabilityId::Cleanup, true, Immediate),
            (CapabilityId::Tools, true, Immediate),
            (CapabilityId::NativeFork, flags.native_fork, Immediate),
            (CapabilityId::NativeCompact, flags.native_compact, Immediate),
            (CapabilityId::NativeSteer, flags.native_steer, Immediate),
            (
                CapabilityId::NativeTurnRollback,
                flags.native_turn_rollback,
                Immediate,
            ),
            (
                CapabilityId::CompactionLifecycle,
                flags.compaction.lifecycle,
                Immediate,
            ),
            (
                CapabilityId::BackgroundCompaction,
                flags.compaction.background,
                Immediate,
            ),
            (
                CapabilityId::CompactionSummary,
                flags.compaction.summary,
                Immediate,
            ),
            (
                CapabilityId::CompactionMetrics,
                flags.compaction.metrics,
                Immediate,
            ),
            (
                CapabilityId::ModelSelection,
                flags.model_selection,
                Immediate,
            ),
            (
                CapabilityId::EffortSelection,
                flags.effort_selection,
                Immediate,
            ),
            (CapabilityId::FastMode, flags.fast_mode, Immediate),
            (CapabilityId::UltrafastMode, flags.ultrafast_mode, Immediate),
            (CapabilityId::SkillCatalog, flags.catalog.skill, Immediate),
            (CapabilityId::McpCatalog, flags.catalog.mcp, Immediate),
            (
                CapabilityId::PermissionCallbacks,
                flags.permission_control,
                Immediate,
            ),
            (CapabilityId::OsSandbox, flags.os_sandbox, Settled),
            (
                CapabilityId::BackgroundTerminals,
                flags.background_terminals,
                Immediate,
            ),
            (CapabilityId::FileDiff, true, Immediate),
            (
                CapabilityId::BackgroundTermination,
                integration.terminal_termination,
                Immediate,
            ),
            (
                CapabilityId::IsolatedGeneration,
                integration.isolated_generation,
                Immediate,
            ),
            (CapabilityId::RuntimeSetup, integration.runtime_setup, Idle),
            (
                CapabilityId::Authentication,
                integration.authentication,
                Idle,
            ),
            (
                CapabilityId::NativeAgentSuppression,
                integration.native_agent_suppression,
                Settled,
            ),
            (CapabilityId::SkillMutation, flags.catalog.skill, Settled),
            (CapabilityId::McpMutation, flags.catalog.mcp, Settled),
            (
                CapabilityId::McpRuntimeStatus,
                flags.catalog.mcp && integration.mcp_runtime_status,
                Immediate,
            ),
            (
                CapabilityId::McpToolCatalog,
                flags.catalog.mcp && integration.mcp_tool_catalog,
                Immediate,
            ),
        ]
        .into_iter()
        .map(|(id, supported, application)| Capability {
            id,
            support: if supported {
                Support::Supported
            } else {
                Support::Unsupported
            },
            application,
        })
        .collect();
        capability.push(Capability {
            id: CapabilityId::FilesystemAccess,
            support: Support::Limited {
                restrictions: vec![CapabilityRestriction::PermissionCallbacksOnly],
            },
            application: Settled,
        });
        for limitation in &integration.limitations {
            if let Some(entry) = capability
                .iter_mut()
                .find(|entry| entry.id == limitation.id)
                && !matches!(entry.support, Support::Unsupported)
            {
                entry.support = limitation.support.clone();
            }
        }
        Self {
            provider: descriptor.kind,
            revision: 8,
            effective_revision: 0,
            capability,
            provider_version: None,
            notice: Vec::new(),
        }
    }

    /// Get the reported support for one capability.
    pub fn support(&self, id: CapabilityId) -> Option<&Support> {
        self.capability
            .iter()
            .find(|capability| capability.id == id)
            .map(|capability| &capability.support)
    }

    /// Report whether one capability is available, fully or with typed restrictions.
    pub fn supports(&self, id: CapabilityId) -> bool {
        matches!(
            self.support(id),
            Some(Support::Supported | Support::Limited { .. })
        )
    }

    /// Replace the reported support for one capability.
    pub fn set_support(&mut self, id: CapabilityId, support: Support) {
        if let Some(capability) = self
            .capability
            .iter_mut()
            .find(|capability| capability.id == id)
        {
            capability.support = support;
        }
    }
}

/// Rejects an optional operation without reporting empty discovery as success.
#[derive(Clone, Debug)]
pub struct UnsupportedCapability(pub ProviderFeature);

/// Identify an unsupported normalized feature without interpreting arbitrary strings.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum ProviderFeature {
    /// Runtime installation and version inspection.
    RuntimeSetup,
    /// Explicit provider credential setup.
    Authentication,
    /// Terminal-based authentication interaction.
    TerminalAuthentication,
    /// Complete normalized resource selection.
    ResourceSelection,
    /// Separate durable native history fork.
    NativeFork,
    /// Forking from an immutable provider checkpoint.
    CheckpointFork,
    /// Binding-owned terminal inventory.
    BackgroundTerminals,
    /// Independent terminal termination.
    BackgroundTermination,
    /// The complete isolated-text session mode.
    TextGeneration,
    /// Opaque normalized model selection.
    ModelSelection,
    /// Explicit provider-default model selection.
    ModelDefaultReset,
    /// Normalized reasoning effort selection.
    EffortSelection,
    /// Explicit provider-default reasoning selection.
    ReasoningDefaultReset,
    /// Native operating-system sandbox enforcement.
    OsSandbox,
    /// An explicitly selected native isolation mode.
    IsolationMode,
    /// Normalized context-window tier selection.
    ContextWindowSelection,
    /// Normalized service-tier selection.
    ServiceTier,
    /// Native skill definition discovery.
    SkillCatalog,
    /// Skill enablement through complete session configuration.
    SkillMutation,
    /// Configured and observed MCP discovery.
    McpCatalog,
    /// MCP enablement through complete session configuration.
    McpMutation,
    /// Enforcement of normalized MCP launch authority.
    McpLaunchRequirements,
    /// Correlated native history compaction.
    NativeCompact,
    /// Exact active-invocation input delivery.
    NativeSteer,
    /// Checked exact cancelled-input history retraction.
    NativeTurnRollback,
    /// Admitted image attachment input.
    ImageInput,
}

impl std::fmt::Display for UnsupportedCapability {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(formatter, "unsupported provider capability: {:?}", self.0)
    }
}

impl std::error::Error for UnsupportedCapability {}

/// Records a restriction supplied by the integration that owns the operation.
///
/// Restrictions are typed facts; the host derives any explanation shown to the user.
#[derive(Clone, Debug, Deserialize, Serialize, Eq, PartialEq)]
pub struct FeatureLimitation {
    /// Normalized feature whose baseline support is restricted.
    pub id: CapabilityId,
    /// Typed supported subset; this cannot upgrade an unsupported baseline feature.
    pub support: Support,
}
