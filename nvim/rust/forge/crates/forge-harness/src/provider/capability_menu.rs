//! Compose the `/capabilities` menu from provider facts and Harness-owned workflows.
//!
//! The provider API reports only which provider operations are supported and when a change to
//! them applies. Menu grouping, labels, required-contract markers, implementation ownership, and
//! explanations are presentation policy, so Harness derives them here. Harness-owned workflows
//! such as goals, queues, and child agents never depend on a provider and appear as Forge rows.

use forge_provider_api::capability::{
    Capability, CapabilityApplication, CapabilityId, CapabilityManifest, Support,
};
use serde::Serialize;

/// Identify the component that implements one menu row.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum CapabilityImplementation {
    /// Harness implements the behavior, independently of or on top of provider facts.
    Forge,
    /// The provider implements the behavior and Harness forwards or observes it.
    Provider,
}

/// Identify a workflow that Harness implements without provider capability negotiation.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum HostCapabilityId {
    Goals,
    Queue,
    Undo,
    Plans,
    Questions,
    SessionDelete,
    AgentObserve,
    AgentSpawn,
    AgentFollowup,
    AgentMessage,
    AgentInterrupt,
    AgentCrossProvider,
    LspTools,
    SubagentTools,
    SessionTools,
    NativeImport,
}

/// Identify one menu row by its provider or host capability; both serialize as one flat string.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize)]
#[serde(untagged)]
pub enum CapabilityRowId {
    Provider(CapabilityId),
    Host(HostCapabilityId),
}

/// Present one capability with its grouping, ownership, support, and explanation.
#[derive(Clone, Debug, Serialize)]
pub struct CapabilityRow {
    pub id: CapabilityRowId,
    pub group: &'static str,
    pub label: &'static str,
    pub required: bool,
    pub implementation: CapabilityImplementation,
    pub support: Support,
    pub application: CapabilityApplication,
    pub reason: &'static str,
}

/// Present the provider manifest and Harness workflows as one `/capabilities` response.
#[derive(Clone, Debug, Serialize)]
pub struct CapabilityMenu {
    pub provider: forge_provider_api::ProviderId,
    pub label: String,
    pub revision: u64,
    pub effective_revision: u64,
    pub provider_version: Option<String>,
    pub notice: Vec<String>,
    pub capability: Vec<CapabilityRow>,
}

/// Describe a provider capability row; `unsupported_reason` replaces `reason` when the
/// provider reports the capability as unsupported.
struct ProviderRowText {
    group: &'static str,
    label: &'static str,
    required: bool,
    implementation: CapabilityImplementation,
    reason: &'static str,
    unsupported_reason: Option<&'static str>,
}

const fn provider_row(
    group: &'static str,
    label: &'static str,
    implementation: CapabilityImplementation,
    reason: &'static str,
) -> ProviderRowText {
    ProviderRowText {
        group,
        label,
        required: false,
        implementation,
        reason,
        unsupported_reason: None,
    }
}

fn provider_row_text(id: CapabilityId) -> ProviderRowText {
    use CapabilityImplementation::{Forge, Provider};
    const REQUIRED: &str = "Required provider contract";
    let required = |label| ProviderRowText {
        required: true,
        ..provider_row("Conversation", label, Provider, REQUIRED)
    };
    match id {
        CapabilityId::Prompt => required("Streaming prompts"),
        CapabilityId::Cancel => required("Cancel active work"),
        CapabilityId::Resume => required("Resume conversation"),
        CapabilityId::Cleanup => required("Collect provider execution"),
        CapabilityId::Tools => required("Harness control tools"),
        CapabilityId::NativeFork => provider_row(
            "Provider",
            "Fork provider conversation",
            Provider,
            "Requires native provider history fork",
        ),
        CapabilityId::NativeCompact => provider_row(
            "Provider",
            "Compact context manually",
            Provider,
            "Requires an explicit provider compaction operation",
        ),
        CapabilityId::NativeSteer => provider_row(
            "Provider",
            "Steer active work",
            Provider,
            "Queued prompts remain available when steering is unsupported",
        ),
        CapabilityId::NativeTurnRollback => provider_row(
            "Provider",
            "Retract output-free cancelled prompt",
            Provider,
            "Cancellation still stops work without history retraction",
        ),
        CapabilityId::CompactionLifecycle => provider_row(
            "Provider",
            "Compaction timeline events",
            Provider,
            "Shows provider evidence only",
        ),
        CapabilityId::BackgroundCompaction => provider_row(
            "Provider",
            "Observe background compaction",
            Provider,
            "Requires compaction events outside an active prompt",
        ),
        CapabilityId::CompactionSummary => provider_row(
            "Provider",
            "Compaction summary",
            Provider,
            "Requires a provider-exposed summary",
        ),
        CapabilityId::CompactionMetrics => provider_row(
            "Provider",
            "Compaction metrics",
            Provider,
            "Requires provider-exposed usage evidence",
        ),
        CapabilityId::ModelSelection => provider_row(
            "Provider",
            "Select model",
            Provider,
            "Model preferences remain specific to each provider",
        ),
        CapabilityId::EffortSelection => provider_row(
            "Provider",
            "Select reasoning effort",
            Provider,
            "Choices come from the selected provider",
        ),
        CapabilityId::FastMode => provider_row(
            "Provider",
            "Fast service tier",
            Provider,
            "Requires provider service-tier support",
        ),
        CapabilityId::UltrafastMode => provider_row(
            "Provider",
            "Ultrafast service tier",
            Provider,
            "Requires provider service-tier support",
        ),
        CapabilityId::SkillCatalog => provider_row(
            "Provider",
            "Select skills",
            Provider,
            "Requires an enforceable skill selection",
        ),
        CapabilityId::McpCatalog => provider_row(
            "Provider",
            "Select MCP servers",
            Provider,
            "Requires enforceable server selection",
        ),
        CapabilityId::PermissionCallbacks => ProviderRowText {
            required: true,
            ..provider_row(
                "Security",
                "Saved approval rules",
                Forge,
                "Forge persists rules and answers individual provider requests",
            )
        },
        CapabilityId::OsSandbox => provider_row(
            "Security",
            "Enforce OS sandbox",
            Provider,
            "Only integrations that declare enforced OS isolation enable this control",
        ),
        CapabilityId::BackgroundTerminals => provider_row(
            "Provider",
            "Inspect background terminals",
            Provider,
            "Requires an explicit provider terminal inventory",
        ),
        CapabilityId::FileDiff => provider_row(
            "Provider",
            "Provider file diffs",
            Provider,
            "Projects structured provider changes into tool history and the exchange timeline",
        ),
        CapabilityId::BackgroundTermination => provider_row(
            "Provider",
            "Terminate one background terminal",
            Provider,
            "Root cancellation remains available without independent terminal controls",
        ),
        CapabilityId::IsolatedGeneration => ProviderRowText {
            unsupported_reason: Some(
                "Requires a provider runtime that generates text outside the main conversation",
            ),
            ..provider_row(
                "Harness",
                "Generate isolated recap and session name",
                Provider,
                "Generation must not append to the main provider conversation",
            )
        },
        CapabilityId::RuntimeSetup => provider_row(
            "Setup",
            "Install supported provider runtime",
            Forge,
            "Uses provider-advertised setup and sign-in methods, preserving native credentials",
        ),
        CapabilityId::Authentication => provider_row(
            "Setup",
            "Sign in from Forge",
            Forge,
            "Uses provider-advertised setup and sign-in methods, preserving native credentials",
        ),
        CapabilityId::NativeAgentSuppression => provider_row(
            "Provider",
            "Disable provider-native agent tools",
            Provider,
            "Requires an integration that can disable provider-native delegation",
        ),
        CapabilityId::SkillMutation => ProviderRowText {
            unsupported_reason: Some(
                "This integration cannot replace skill selections on an existing binding",
            ),
            ..provider_row(
                "Settings",
                "Apply skill selection",
                Provider,
                "Forge persists desired selection and verifies the provider result",
            )
        },
        CapabilityId::McpMutation => ProviderRowText {
            unsupported_reason: Some(
                "This integration cannot replace MCP selections on an existing binding",
            ),
            ..provider_row(
                "Settings",
                "Apply MCP selection",
                Provider,
                "Forge persists desired selection independently of native provider configuration",
            )
        },
        CapabilityId::McpRuntimeStatus => provider_row(
            "Provider",
            "Inspect MCP connection status",
            Provider,
            "Requires provider observation of server readiness or tool inventory",
        ),
        CapabilityId::McpToolCatalog => provider_row(
            "Provider",
            "Browse MCP tool definitions",
            Provider,
            "Requires provider observation of server readiness or tool inventory",
        ),
        CapabilityId::FilesystemAccess => provider_row(
            "Security",
            "Filesystem access scopes",
            Forge,
            "Scopes and write-off apply to reported file operations. Shell commands, MCP internals, and operations without permission callbacks retain provider-native restrictions",
        ),
    }
}

/// Harness-owned workflows, grouped as the menu presents them. Every entry is supported for
/// every provider because Harness implements it.
const HOST_WORKFLOW: &[(HostCapabilityId, &str, &str, CapabilityApplication, &str)] = &[
    (
        HostCapabilityId::Goals,
        "Harness",
        "Set, pause, resume, and clear goals",
        CapabilityApplication::Immediate,
        "Forge owns this workflow",
    ),
    (
        HostCapabilityId::Queue,
        "Harness",
        "Queue prompts",
        CapabilityApplication::Immediate,
        "Forge owns this workflow",
    ),
    (
        HostCapabilityId::Undo,
        "Harness",
        "Restore workspace checkpoint",
        CapabilityApplication::Immediate,
        "Forge owns this workflow",
    ),
    (
        HostCapabilityId::Plans,
        "Harness",
        "Plan review and execution",
        CapabilityApplication::Immediate,
        "Forge owns this workflow",
    ),
    (
        HostCapabilityId::Questions,
        "Harness",
        "Harness question forms",
        CapabilityApplication::Immediate,
        "Forge owns this workflow",
    ),
    (
        HostCapabilityId::SessionDelete,
        "Harness",
        "Delete Forge history",
        CapabilityApplication::Immediate,
        "Forge owns this workflow",
    ),
    (
        HostCapabilityId::AgentObserve,
        "Harness",
        "Inspect child timelines",
        CapabilityApplication::Immediate,
        CHILD_AGENT_REASON,
    ),
    (
        HostCapabilityId::AgentSpawn,
        "Harness",
        "Spawn independent child conversations",
        CapabilityApplication::Immediate,
        CHILD_AGENT_REASON,
    ),
    (
        HostCapabilityId::AgentFollowup,
        "Harness",
        "Queue child follow-up tasks",
        CapabilityApplication::Immediate,
        CHILD_AGENT_REASON,
    ),
    (
        HostCapabilityId::AgentMessage,
        "Harness",
        "Persist child informational messages",
        CapabilityApplication::Immediate,
        CHILD_AGENT_REASON,
    ),
    (
        HostCapabilityId::AgentInterrupt,
        "Harness",
        "Interrupt one child or all children",
        CapabilityApplication::Immediate,
        CHILD_AGENT_REASON,
    ),
    (
        HostCapabilityId::AgentCrossProvider,
        "Harness",
        "Select each child's provider and model",
        CapabilityApplication::Immediate,
        CHILD_AGENT_REASON,
    ),
    (
        HostCapabilityId::LspTools,
        "Settings",
        "Query running editor language servers",
        CapabilityApplication::Settled,
        "Off by default. Read-only agent queries; interactive plan navigation stays available",
    ),
    (
        HostCapabilityId::SubagentTools,
        "Settings",
        "Enable root model delegation tools",
        CapabilityApplication::Settled,
        "Off by default. Manual child spawning remains available and children cannot delegate",
    ),
    (
        HostCapabilityId::SessionTools,
        "Settings",
        "Inspect and search Harness sessions",
        CapabilityApplication::Settled,
        "On by default. Disabling hides history tools but retains configuration inspection and the Harness skill",
    ),
    (
        HostCapabilityId::NativeImport,
        "Settings",
        "Import native skills and MCP definitions",
        CapabilityApplication::Settled,
        "Explicit source preview and selection. Imported entries start disabled and preserve native value references",
    ),
];

const CHILD_AGENT_REASON: &str =
    "Harness owns child identity, fresh history, task queues, cancellation, and durable outcomes";

fn provider_capability_row(capability: &Capability) -> CapabilityRow {
    let text = provider_row_text(capability.id);
    let reason = match (&capability.support, text.unsupported_reason) {
        (Support::Unsupported, Some(unsupported_reason)) => unsupported_reason,
        _ => text.reason,
    };
    CapabilityRow {
        id: CapabilityRowId::Provider(capability.id),
        group: text.group,
        label: text.label,
        required: text.required,
        implementation: text.implementation,
        support: capability.support.clone(),
        application: capability.application,
        reason,
    }
}

impl CapabilityMenu {
    /// Compose the menu for `provider_label` from the provider's effective manifest.
    ///
    /// Provider rows keep the manifest order; Harness workflow rows follow the required
    /// conversation contract so the menu lists Forge-owned behavior before optional provider
    /// features.
    pub fn compose(provider_label: String, manifest: CapabilityManifest) -> Self {
        let (required, optional): (Vec<_>, Vec<_>) = manifest
            .capability
            .iter()
            .map(provider_capability_row)
            .partition(|row| row.group == "Conversation");
        let host = HOST_WORKFLOW
            .iter()
            .map(|&(id, group, label, application, reason)| CapabilityRow {
                id: CapabilityRowId::Host(id),
                group,
                label,
                required: false,
                implementation: CapabilityImplementation::Forge,
                support: Support::Supported,
                application,
                reason,
            });
        Self {
            provider: manifest.provider,
            label: provider_label,
            revision: manifest.revision,
            effective_revision: manifest.effective_revision,
            provider_version: manifest.provider_version,
            notice: manifest.notice,
            capability: required.into_iter().chain(host).chain(optional).collect(),
        }
    }
}
