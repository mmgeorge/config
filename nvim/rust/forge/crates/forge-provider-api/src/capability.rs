use crate::{ProviderDescriptor, ProviderKind};
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(rename_all = "snake_case")]
/// Distinguishes supported behavior from provider limitations.
pub enum Support {
    /// The operation preserves its full Harness contract.
    Supported,
    /// The operation works with the reported restriction.
    Limited,
    /// The adapter rejects the operation before dispatch.
    Unsupported,
}

#[derive(Clone, Debug, Serialize)]
/// Describes one user-visible operation and its application boundary.
pub struct Capability {
    /// Identifies the operation across providers and menu refreshes.
    pub id: &'static str,
    /// Names the menu group.
    pub group: &'static str,
    /// Names the operation in the menu.
    pub label: &'static str,
    /// Records whether every provider must implement this operation.
    pub required: bool,
    /// Identifies Forge or the provider as the implementation owner.
    pub implementation: &'static str,
    /// Reports support independently of discovery availability.
    pub support: Support,
    /// Describes the configuration application boundary.
    pub application: &'static str,
    /// Explains the observable restriction or guarantee.
    pub reason: String,
}

#[derive(Clone, Debug, Serialize)]
/// Captures one coherent provider capability revision for presentation and dispatch.
pub struct CapabilityManifest {
    /// Identifies the selected provider.
    pub provider: ProviderKind,
    /// Identifies the provider in the menu.
    pub label: String,
    /// Changes when the capability schema changes.
    pub revision: u64,
    /// Retains the negotiated agent version when the transport exposes it.
    pub provider_version: Option<String>,
    /// Distinguishes startup discovery from ready and blocked runtime state.
    pub availability: &'static str,
    /// Retains a bounded runtime explanation independent of feature support.
    pub availability_reason: Option<String>,
    /// Reports unavailable default resources without exposing credential values.
    pub notice: Vec<String>,
    /// Contains supported and unsupported operations.
    pub capability: Vec<Capability>,
}

impl CapabilityManifest {
    /// Build the capability inventory from the adapter's effective descriptor.
    pub fn from_descriptor(descriptor: ProviderDescriptor) -> Self {
        let flags = &descriptor.capability;
        let mut capability = Vec::new();
        let mut add =
            |id, group, label, required, implementation, supported, application, reason: &str| {
                capability.push(Capability {
                    id,
                    group,
                    label,
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
            ("prompt", "Streaming prompts"),
            ("cancel", "Cancel active work"),
            ("resume", "Resume conversation"),
            ("cleanup", "Collect provider execution"),
            ("tools", "Harness control tools"),
        ] {
            add(
                id,
                "Conversation",
                label,
                true,
                "provider",
                true,
                "immediate",
                "Required provider contract",
            );
        }
        for (id, label) in [
            ("goals", "Set, pause, resume, and clear goals"),
            ("queue", "Queue prompts"),
            ("undo", "Restore workspace checkpoint"),
            ("plans", "Plan review and execution"),
            ("questions", "Harness question forms"),
            ("session_delete", "Delete Forge history"),
        ] {
            add(
                id,
                "Harness",
                label,
                false,
                "forge",
                true,
                "immediate",
                "Forge owns this workflow",
            );
        }
        for (id, label, supported, reason) in [
            (
                "native_fork",
                "Fork provider conversation",
                flags.native_fork,
                "Requires native provider history fork",
            ),
            (
                "native_compact",
                "Compact context manually",
                flags.native_compact,
                "Requires an explicit provider compaction operation",
            ),
            (
                "native_steer",
                "Steer active work",
                flags.native_steer,
                "Queued prompts remain available when steering is unsupported",
            ),
            (
                "native_turn_rollback",
                "Retract output-free cancelled prompt",
                flags.native_turn_rollback,
                "Cancellation still stops work without history retraction",
            ),
            (
                "compaction_lifecycle",
                "Compaction timeline events",
                flags.compaction.lifecycle,
                "Shows provider evidence only",
            ),
            (
                "background_compaction",
                "Observe background compaction",
                flags.compaction.background,
                "Requires compaction events outside an active prompt",
            ),
            (
                "compaction_summary",
                "Compaction summary",
                flags.compaction.summary,
                "Requires a provider-exposed summary",
            ),
            (
                "compaction_metrics",
                "Compaction metrics",
                flags.compaction.metrics,
                "Requires provider-exposed usage evidence",
            ),
            (
                "model_selection",
                "Select model",
                flags.model_selection,
                "Model preferences remain specific to each provider",
            ),
            (
                "effort_selection",
                "Select reasoning effort",
                flags.effort_selection,
                "Choices come from the selected provider",
            ),
            (
                "fast_mode",
                "Fast service tier",
                flags.fast_mode,
                "Requires provider service-tier support",
            ),
            (
                "ultrafast_mode",
                "Ultrafast service tier",
                flags.ultrafast_mode,
                "Requires provider service-tier support",
            ),
            (
                "skill_catalog",
                "Select skills",
                flags.catalog.skill,
                "Requires an enforceable skill selection",
            ),
            (
                "mcp_catalog",
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
                "provider",
                supported,
                "immediate",
                reason,
            );
        }
        add(
            "approval_rules",
            "Security",
            "Saved approval rules",
            true,
            "forge",
            flags.permission_control,
            "immediate",
            "Forge persists rules and answers individual provider requests",
        );
        add(
            "os_sandbox",
            "Security",
            "Enforce OS sandbox",
            false,
            "provider",
            flags.os_sandbox,
            "session_restart",
            "Only integrations that declare enforced OS isolation enable this control",
        );
        add(
            "background_terminals",
            "Provider",
            "Inspect background terminals",
            false,
            "provider",
            flags.background_terminals,
            "immediate",
            "Requires an explicit provider terminal inventory",
        );
        add(
            "file_diff",
            "Provider",
            "Provider file diffs",
            false,
            "provider",
            true,
            "immediate",
            "Projects structured provider changes into tool history and the exchange timeline",
        );
        add(
            "background_termination",
            "Provider",
            "Terminate one background terminal",
            false,
            "provider",
            flags.integration.terminal_termination,
            "immediate",
            "Root cancellation remains available without independent terminal controls",
        );
        add(
            "recap",
            "Harness",
            "Generate isolated recap and session name",
            false,
            "provider",
            flags.integration.isolated_generation,
            "immediate",
            "Generation must not append to the main provider conversation",
        );
        for (id, label, supported) in [
            (
                "runtime_setup",
                "Install supported provider runtime",
                flags.integration.runtime_setup,
            ),
            (
                "authentication",
                "Sign in from Forge",
                flags.integration.authentication,
            ),
        ] {
            add(
                id,
                "Setup",
                label,
                false,
                "forge",
                supported,
                "idle",
                "Uses provider-advertised setup and sign-in methods, preserving native credentials",
            );
        }
        for (id, label) in [
            ("agent_observe", "Inspect child timelines"),
            ("agent_spawn", "Spawn independent child conversations"),
            ("agent_followup", "Queue child follow-up tasks"),
            ("agent_message", "Persist child informational messages"),
            ("agent_interrupt", "Interrupt one child or all children"),
            (
                "agent_cross_provider",
                "Select each child's provider and model",
            ),
        ] {
            add(
                id,
                "Harness",
                label,
                false,
                "forge",
                true,
                "immediate",
                "Harness owns child identity, fresh history, task queues, cancellation, and durable outcomes",
            );
        }
        add(
            "lsp_tools",
            "Settings",
            "Query running editor language servers",
            false,
            "forge",
            true,
            "server_restart",
            "Off by default. Read-only agent queries; interactive plan navigation stays available",
        );
        add(
            "subagent_tools",
            "Settings",
            "Enable root model delegation tools",
            false,
            "forge",
            true,
            "server_restart",
            "Off by default. Manual child spawning remains available and children cannot delegate",
        );
        add(
            "session_tools",
            "Settings",
            "Inspect and search Harness sessions",
            false,
            "forge",
            true,
            "server_restart",
            "On by default. Disabling hides history tools but retains configuration inspection and the Harness skill",
        );
        add(
            "native_agent_suppression",
            "Provider",
            "Disable provider-native agent tools",
            false,
            "provider",
            flags.integration.native_agent_suppression,
            "session_restart",
            "Requires an integration that can disable provider-native delegation",
        );
        add(
            "skill_mutation",
            "Settings",
            "Apply skill selection",
            false,
            "provider",
            flags.catalog.skill,
            "session_restart",
            "Forge persists desired selection and verifies the provider result",
        );
        add(
            "native_import",
            "Settings",
            "Import native skills and MCP definitions",
            false,
            "forge",
            true,
            "server_restart",
            "Explicit source preview and selection. Imported entries start disabled and preserve native value references",
        );
        add(
            "mcp_mutation",
            "Settings",
            "Apply MCP selection",
            false,
            "provider",
            flags.catalog.mcp,
            if flags.catalog.live_mcp_mutation {
                "immediate"
            } else {
                "server_restart"
            },
            "Forge persists desired selection independently of native provider configuration",
        );
        for (id, label, supported) in [
            (
                "mcp_runtime_status",
                "Inspect MCP connection status",
                flags.integration.mcp_runtime_status,
            ),
            (
                "mcp_tool_catalog",
                "Browse MCP tool definitions",
                flags.integration.mcp_tool_catalog,
            ),
        ] {
            add(
                id,
                "Provider",
                label,
                false,
                "provider",
                flags.catalog.mcp && supported,
                "immediate",
                "Requires provider observation of server readiness or tool inventory",
            );
        }
        capability.push(Capability {
            id: "filesystem_access",
            group: "Security",
            label: "Filesystem access scopes",
            required: false,
            implementation: "forge",
            support: Support::Limited,
            application: "execution_restart",
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
            revision: 6,
            capability,
            provider_version: None,
            availability: "ready",
            availability_reason: None,
            notice: Vec::new(),
        }
    }
}

#[derive(Debug)]
/// Rejects an optional operation without reporting empty discovery as success.
pub struct UnsupportedCapability(pub &'static str);

impl std::fmt::Display for UnsupportedCapability {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(formatter, "unsupported provider capability: {}", self.0)
    }
}

impl std::error::Error for UnsupportedCapability {}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn declarations_control_support_without_provider_identity() {
        for provider in [
            ProviderKind::Codex,
            ProviderKind::Copilot,
            ProviderKind::Claude,
            ProviderKind::Antigravity,
            ProviderKind::Mock,
        ] {
            let manifest = CapabilityManifest::from_descriptor(ProviderDescriptor {
                kind: provider,
                label: "test".into(),
                capability: crate::ProviderCapability {
                    integration: crate::model::ProviderIntegration {
                        runtime_setup: true,
                        limitations: vec![FeatureLimitation {
                            id: "file_diff".into(),
                            support: Support::Limited,
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
            assert!(matches!(find("runtime_setup").support, Support::Supported));
            assert!(matches!(find("os_sandbox").support, Support::Unsupported));
            assert!(matches!(find("recap").support, Support::Unsupported));
            assert!(matches!(find("file_diff").support, Support::Limited));
            assert_eq!(find("file_diff").reason, "fixture restriction");
        }
    }
}

/// Records a restriction supplied by the integration that owns the operation.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct FeatureLimitation {
    pub id: String,
    pub support: Support,
    pub reason: String,
}
