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
    /// Native edit tools may omit structured diff output; host checkpoint review remains available.
    UnstructuredDiffs,
    /// Provider enterprise terminal policy can execute without normalized approval callbacks.
    EnterpriseTerminalPolicy,
    /// Inherited project-native skills remain available despite the explicit portable selection.
    InheritedSkills,
    /// Permit only the listed normalized task kinds when steering an active invocation.
    TaskKinds(Vec<crate::input::TaskKind>),
    /// Require the exact source checkpoint for history mutation.
    CheckpointRequired,
    /// Require settled foreground execution before applying configuration or history mutation.
    SettledOnly,
}

/// Identify a normalized capability independently of provider names or native protocol methods.
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
    /// Harness-owned objective and continuation workflow.
    Goals,
    /// Harness-owned durable input queuing.
    Queue,
    /// Host workspace checkpoint restoration.
    Undo,
    /// Harness-owned plan review and execution.
    Plans,
    /// Host-correlated question forms and replies.
    Questions,
    /// Host-owned saved-session deletion.
    SessionDelete,
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
    /// Saved host permission/approval rules.
    ApprovalRules,
    /// Enforceable operating-system isolation policy.
    OsSandbox,
    /// Binding-owned shell inventory.
    BackgroundTerminals,
    /// Structured provider file-change observations.
    FileDiff,
    /// Independent binding-owned shell termination.
    BackgroundTermination,
    /// Host summary/recap generation workflow.
    Recap,
    /// Runtime installation/version inspection.
    RuntimeSetup,
    /// Explicit provider authentication interaction.
    Authentication,
    /// Host observation of delegated agents.
    AgentObserve,
    /// Harness-owned delegated-agent creation.
    AgentSpawn,
    /// Harness-owned follow-up input to an agent.
    AgentFollowup,
    /// Harness-owned inter-agent messaging.
    AgentMessage,
    /// Harness-owned agent interruption.
    AgentInterrupt,
    /// Host selection of provider/model per delegated owner.
    AgentCrossProvider,
    /// Host editor language-server queries.
    LspTools,
    /// Host model-facing delegation tools.
    SubagentTools,
    /// Host session discovery/history tools.
    SessionTools,
    /// Enforcement of host-owned delegation authority.
    NativeAgentSuppression,
    /// Skill enablement applied through complete session configuration.
    SkillMutation,
    /// Explicit host import of native resource definitions.
    NativeImport,
    /// MCP enablement applied through complete session configuration.
    McpMutation,
    /// Read-only observed MCP readiness.
    McpRuntimeStatus,
    /// Read-only observed MCP tools.
    McpToolCatalog,
    /// Normalized filesystem authority and its typed restrictions.
    FilesystemAccess,
}

/// Identify the layer that implements a normalized capability.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum CapabilityOwner {
    /// Harness owns the workflow and its scheduling decisions.
    Forge,
    /// The native adapter implements the documented provider boundary.
    Provider,
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

/// Describes one user-visible operation and its application boundary.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Capability {
    /// Identifies the operation across providers and menu refreshes.
    pub id: CapabilityId,
    /// Names the menu group.
    pub group: String,
    /// Names the operation in the menu.
    pub label: String,
    /// Records whether every provider must implement this operation.
    pub required: bool,
    /// Identifies Forge or the provider as the implementation owner.
    pub implementation: CapabilityOwner,
    /// Reports support independently of discovery availability.
    pub support: Support,
    /// Describes the configuration application boundary.
    pub application: CapabilityApplication,
    /// Explains the observable restriction or guarantee.
    pub reason: String,
}

/// Own one coherent baseline or negotiated support snapshot for presentation and admission.
/// Registration supplies baseline support; open/configure return effective binding snapshots
/// and ordered capability observations publish later changes. Loading/failure state belongs
/// to host discovery, not placeholder availability fields in this manifest.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct CapabilityManifest {
    /// Identifies the selected provider.
    pub provider: ProviderId,
    /// Identifies the provider in the menu.
    pub label: String,
    /// Capability schema version, independent of event sequencing and negotiated support changes.
    pub revision: u64,
    /// Binding-local support revision, advanced by the adapter when negotiated support changes.
    pub effective_revision: u64,
    /// Task kinds eligible for active steering; an empty list permits no task steering.
    pub task_steering: Vec<crate::input::TaskKind>,
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
        let flags = &descriptor.capability;
        let mut capability = Vec::new();
        let mut add =
            |id, group, label, required, implementation, supported, application, reason: &str| {
                capability.push(Capability {
                    id,
                    group: String::from(group),
                    label: String::from(label),
                    required,
                    implementation,
                    support: if supported {
                        Support::Supported
                    } else {
                        Support::Unsupported
                    },
                    application,
                    reason: reason.to_owned(),
                });
            };
        for (id, label) in [
            (CapabilityId::Prompt, "Streaming prompts"),
            (CapabilityId::Cancel, "Cancel active work"),
            (CapabilityId::Resume, "Resume conversation"),
            (CapabilityId::Cleanup, "Collect provider execution"),
            (CapabilityId::Tools, "Harness control tools"),
        ] {
            add(
                id,
                "Conversation",
                label,
                true,
                CapabilityOwner::Provider,
                true,
                CapabilityApplication::Immediate,
                "Required provider contract",
            );
        }
        for (id, label) in [
            (CapabilityId::Goals, "Set, pause, resume, and clear goals"),
            (CapabilityId::Queue, "Queue prompts"),
            (CapabilityId::Undo, "Restore workspace checkpoint"),
            (CapabilityId::Plans, "Plan review and execution"),
            (CapabilityId::Questions, "Harness question forms"),
            (CapabilityId::SessionDelete, "Delete Forge history"),
        ] {
            add(
                id,
                "Harness",
                label,
                false,
                CapabilityOwner::Forge,
                true,
                CapabilityApplication::Immediate,
                "Forge owns this workflow",
            );
        }
        for (id, label, supported, reason) in [
            (
                CapabilityId::NativeFork,
                "Fork provider conversation",
                flags.native_fork,
                "Requires native provider history fork",
            ),
            (
                CapabilityId::NativeCompact,
                "Compact context manually",
                flags.native_compact,
                "Requires an explicit provider compaction operation",
            ),
            (
                CapabilityId::NativeSteer,
                "Steer active work",
                flags.native_steer,
                "Queued prompts remain available when steering is unsupported",
            ),
            (
                CapabilityId::NativeTurnRollback,
                "Retract output-free cancelled prompt",
                flags.native_turn_rollback,
                "Cancellation still stops work without history retraction",
            ),
            (
                CapabilityId::CompactionLifecycle,
                "Compaction timeline events",
                flags.compaction.lifecycle,
                "Shows provider evidence only",
            ),
            (
                CapabilityId::BackgroundCompaction,
                "Observe background compaction",
                flags.compaction.background,
                "Requires compaction events outside an active prompt",
            ),
            (
                CapabilityId::CompactionSummary,
                "Compaction summary",
                flags.compaction.summary,
                "Requires a provider-exposed summary",
            ),
            (
                CapabilityId::CompactionMetrics,
                "Compaction metrics",
                flags.compaction.metrics,
                "Requires provider-exposed usage evidence",
            ),
            (
                CapabilityId::ModelSelection,
                "Select model",
                flags.model_selection,
                "Model preferences remain specific to each provider",
            ),
            (
                CapabilityId::EffortSelection,
                "Select reasoning effort",
                flags.effort_selection,
                "Choices come from the selected provider",
            ),
            (
                CapabilityId::FastMode,
                "Fast service tier",
                flags.fast_mode,
                "Requires provider service-tier support",
            ),
            (
                CapabilityId::UltrafastMode,
                "Ultrafast service tier",
                flags.ultrafast_mode,
                "Requires provider service-tier support",
            ),
            (
                CapabilityId::SkillCatalog,
                "Select skills",
                flags.catalog.skill,
                "Requires an enforceable skill selection",
            ),
            (
                CapabilityId::McpCatalog,
                "Select MCP servers",
                flags.catalog.mcp,
                "Requires enforceable server selection",
            ),
        ] {
            add(
                id,
                "Provider",
                label,
                false,
                CapabilityOwner::Provider,
                supported,
                CapabilityApplication::Immediate,
                reason,
            );
        }
        add(
            CapabilityId::ApprovalRules,
            "Security",
            "Saved approval rules",
            true,
            CapabilityOwner::Forge,
            flags.permission_control,
            CapabilityApplication::Immediate,
            "Forge persists rules and answers individual provider requests",
        );
        add(
            CapabilityId::OsSandbox,
            "Security",
            "Enforce OS sandbox",
            false,
            CapabilityOwner::Provider,
            flags.os_sandbox,
            CapabilityApplication::Settled,
            "Only integrations that declare enforced OS isolation enable this control",
        );
        add(
            CapabilityId::BackgroundTerminals,
            "Provider",
            "Inspect background terminals",
            false,
            CapabilityOwner::Provider,
            flags.background_terminals,
            CapabilityApplication::Immediate,
            "Requires an explicit provider terminal inventory",
        );
        add(
            CapabilityId::FileDiff,
            "Provider",
            "Provider file diffs",
            false,
            CapabilityOwner::Provider,
            true,
            CapabilityApplication::Immediate,
            "Projects structured provider changes into tool history and the exchange timeline",
        );
        add(
            CapabilityId::BackgroundTermination,
            "Provider",
            "Terminate one background terminal",
            false,
            CapabilityOwner::Provider,
            flags.integration.terminal_termination,
            CapabilityApplication::Immediate,
            "Root cancellation remains available without independent terminal controls",
        );
        add(
            CapabilityId::Recap,
            "Harness",
            "Generate isolated recap and session name",
            false,
            CapabilityOwner::Provider,
            flags.integration.isolated_generation,
            CapabilityApplication::Immediate,
            "Generation must not append to the main provider conversation",
        );
        for (id, label, supported) in [
            (
                CapabilityId::RuntimeSetup,
                "Install supported provider runtime",
                flags.integration.runtime_setup,
            ),
            (
                CapabilityId::Authentication,
                "Sign in from Forge",
                flags.integration.authentication,
            ),
        ] {
            add(
                id,
                "Setup",
                label,
                false,
                CapabilityOwner::Forge,
                supported,
                CapabilityApplication::Idle,
                "Uses provider-advertised setup and sign-in methods, preserving native credentials",
            );
        }
        for (id, label) in [
            (CapabilityId::AgentObserve, "Inspect child timelines"),
            (
                CapabilityId::AgentSpawn,
                "Spawn independent child conversations",
            ),
            (CapabilityId::AgentFollowup, "Queue child follow-up tasks"),
            (
                CapabilityId::AgentMessage,
                "Persist child informational messages",
            ),
            (
                CapabilityId::AgentInterrupt,
                "Interrupt one child or all children",
            ),
            (
                CapabilityId::AgentCrossProvider,
                "Select each child's provider and model",
            ),
        ] {
            add(
                id,
                "Harness",
                label,
                false,
                CapabilityOwner::Forge,
                true,
                CapabilityApplication::Immediate,
                "Harness owns child identity, fresh history, task queues, cancellation, and durable outcomes",
            );
        }
        add(
            CapabilityId::LspTools,
            "Settings",
            "Query running editor language servers",
            false,
            CapabilityOwner::Forge,
            true,
            CapabilityApplication::Settled,
            "Off by default. Read-only agent queries; interactive plan navigation stays available",
        );
        add(
            CapabilityId::SubagentTools,
            "Settings",
            "Enable root model delegation tools",
            false,
            CapabilityOwner::Forge,
            true,
            CapabilityApplication::Settled,
            "Off by default. Manual child spawning remains available and children cannot delegate",
        );
        add(
            CapabilityId::SessionTools,
            "Settings",
            "Inspect and search Harness sessions",
            false,
            CapabilityOwner::Forge,
            true,
            CapabilityApplication::Settled,
            "On by default. Disabling hides history tools but retains configuration inspection and the Harness skill",
        );
        add(
            CapabilityId::NativeAgentSuppression,
            "Provider",
            "Disable provider-native agent tools",
            false,
            CapabilityOwner::Provider,
            flags.integration.native_agent_suppression,
            CapabilityApplication::Settled,
            "Requires an integration that can disable provider-native delegation",
        );
        add(
            CapabilityId::SkillMutation,
            "Settings",
            "Apply skill selection",
            false,
            CapabilityOwner::Provider,
            flags.catalog.skill,
            CapabilityApplication::Settled,
            "Forge persists desired selection and verifies the provider result",
        );
        add(
            CapabilityId::NativeImport,
            "Settings",
            "Import native skills and MCP definitions",
            false,
            CapabilityOwner::Forge,
            true,
            CapabilityApplication::Settled,
            "Explicit source preview and selection. Imported entries start disabled and preserve native value references",
        );
        add(
            CapabilityId::McpMutation,
            "Settings",
            "Apply MCP selection",
            false,
            CapabilityOwner::Provider,
            flags.catalog.mcp,
            CapabilityApplication::Settled,
            "Forge persists desired selection independently of native provider configuration",
        );
        for (id, label, supported) in [
            (
                CapabilityId::McpRuntimeStatus,
                "Inspect MCP connection status",
                flags.integration.mcp_runtime_status,
            ),
            (
                CapabilityId::McpToolCatalog,
                "Browse MCP tool definitions",
                flags.integration.mcp_tool_catalog,
            ),
        ] {
            add(
                id,
                "Provider",
                label,
                false,
                CapabilityOwner::Provider,
                flags.catalog.mcp && supported,
                CapabilityApplication::Immediate,
                "Requires provider observation of server readiness or tool inventory",
            );
        }
        capability.push(Capability {
            id: CapabilityId::FilesystemAccess,
            group: "Security".into(),
            label: "Filesystem access scopes".into(),
            required: false,
            implementation: CapabilityOwner::Forge,
            support: Support::Limited { restrictions: vec![CapabilityRestriction::PermissionCallbacksOnly] },
            application: CapabilityApplication::Settled,
            reason: "Scopes and write-off apply to reported file operations. Shell commands, MCP internals, and operations without permission callbacks retain provider-native restrictions".into(),
        });
        for limitation in &flags.integration.limitations {
            if let Some(entry) = capability
                .iter_mut()
                .find(|entry| entry.id == limitation.id)
            {
                if matches!(entry.support, Support::Unsupported) {
                    continue;
                }
                entry.support = limitation.support.clone();
                entry.reason = limitation.reason.clone();
            }
        }
        Self {
            provider: descriptor.kind,
            label: descriptor.label,
            revision: 7,
            effective_revision: 0,
            task_steering: if flags.native_steer {
                vec![
                    crate::input::TaskKind::Goal,
                    crate::input::TaskKind::Plan,
                    crate::input::TaskKind::Execute,
                ]
            } else {
                Vec::new()
            },
            capability,
            provider_version: None,
            notice: Vec::new(),
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
#[derive(Clone, Debug, Deserialize, Serialize, Eq, PartialEq)]
pub struct FeatureLimitation {
    /// Normalized feature whose baseline support is restricted.
    pub id: CapabilityId,
    /// Typed supported subset; this cannot upgrade an unsupported baseline feature.
    pub support: Support,
    /// User-facing explanation; admission uses typed support and restrictions.
    pub reason: String,
}

#[cfg(test)]
mod test {
    use super::*;

    /// Apply persisted capability IDs with the same outward spelling after typed decoding.
    #[test]
    fn capability_wire_identifiers_apply_the_expected_restrictions() {
        let restriction = serde_json::json!({
            "id": "file_diff", "support": {"limited": {"restrictions": ["permission_callbacks_only"]}}, "reason": "fixture restriction"
        });
        let limitation: FeatureLimitation = serde_json::from_value(restriction.clone()).unwrap();
        let manifest = CapabilityManifest::from_descriptor(ProviderDescriptor {
            kind: ProviderId::new("test-provider").unwrap(),
            label: "Test provider".into(),
            capability: crate::ProviderCapability {
                integration: crate::model::ProviderIntegration {
                    limitations: vec![limitation],
                    ..Default::default()
                },
                ..Default::default()
            },
        });
        let row = manifest
            .capability
            .iter()
            .find(|row| row.id == CapabilityId::FileDiff)
            .unwrap();
        assert!(matches!(row.support, Support::Limited { .. }));
        let encoded = serde_json::to_value(&manifest).unwrap();
        let row = encoded["capability"]
            .as_array()
            .unwrap()
            .iter()
            .find(|row| row["id"] == "file_diff")
            .unwrap();
        assert_eq!(row["implementation"], "provider");
        assert_eq!(row["application"], "immediate");
        assert!(
            serde_json::from_value::<FeatureLimitation>(serde_json::json!({
                "id": "unrecognized_native_method", "support": {"limited": {"restrictions": ["permission_callbacks_only"]}}, "reason": "unmapped"
            }))
            .is_err()
        );
    }

    #[test]
    fn declarations_control_support_without_provider_identity() {
        for provider in ["one", "two", "three", "four", "five"] {
            let manifest = CapabilityManifest::from_descriptor(ProviderDescriptor {
                kind: ProviderId::new(format!("test-{provider}")).unwrap(),
                label: "test".into(),
                capability: crate::ProviderCapability {
                    integration: crate::model::ProviderIntegration {
                        runtime_setup: true,
                        limitations: vec![FeatureLimitation {
                            id: CapabilityId::FileDiff,
                            support: Support::Limited {
                                restrictions: vec![CapabilityRestriction::PermissionCallbacksOnly],
                            },
                            reason: "fixture restriction".into(),
                        }],
                        ..Default::default()
                    },
                    ..Default::default()
                },
            });
            let find = |id| {
                manifest
                    .capability
                    .iter()
                    .find(|entry| entry.id == id)
                    .unwrap()
            };
            assert!(matches!(
                find(CapabilityId::RuntimeSetup).support,
                Support::Supported
            ));
            assert!(matches!(
                find(CapabilityId::OsSandbox).support,
                Support::Unsupported
            ));
            assert!(matches!(
                find(CapabilityId::Recap).support,
                Support::Unsupported
            ));
            assert!(matches!(
                find(CapabilityId::FileDiff).support,
                Support::Limited { .. }
            ));
            assert_eq!(find(CapabilityId::FileDiff).reason, "fixture restriction");
        }
    }
}
