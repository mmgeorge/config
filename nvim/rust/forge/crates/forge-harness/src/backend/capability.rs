use super::{BackendDescriptor, BackendKind};
use serde::Serialize;

#[derive(Clone, Debug, Serialize)]
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
    pub reason: &'static str,
}

#[derive(Clone, Debug, Serialize)]
/// Captures one coherent provider capability revision for presentation and dispatch.
pub struct CapabilityManifest {
    /// Identifies the selected backend.
    pub backend: BackendKind,
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
    pub fn from_descriptor(descriptor: BackendDescriptor) -> Self {
        let flags = &descriptor.capability;
        let mut capability = Vec::new();
        let mut add = |id, group, label, required, implementation, supported, application, reason| {
            capability.push(Capability { id, group, label, required, implementation,
                support: if supported { Support::Supported } else { Support::Unsupported }, application, reason });
        };
        for (id, label) in [("prompt", "Streaming prompts"), ("cancel", "Cancel active work"),
            ("resume", "Resume conversation"), ("cleanup", "Collect provider execution"),
            ("control_tools", "Harness control tools")] {
            add(id, "Conversation", label, true, "provider", true, "immediate", "Required provider contract");
        }
        for (id, label) in [("goals", "Set, pause, resume, and clear goals"), ("queue", "Queue prompts"),
            ("undo", "Restore workspace checkpoint"), ("plans", "Plan review and execution"),
            ("questions", "Harness question forms"), ("session_delete", "Delete Forge history")] {
            add(id, "Harness", label, false, "forge", true, "immediate", "Forge owns this workflow");
        }
        for (id, label, supported, reason) in [
            ("native_fork", "Fork provider conversation", flags.native_fork, "Requires native provider history fork"),
            ("native_compact", "Compact context manually", flags.native_compact, "Requires an explicit provider compaction operation"),
            ("native_steer", "Steer active work", flags.native_steer, "Queued prompts remain available when steering is unsupported"),
            ("native_turn_rollback", "Retract output-free cancelled prompt", flags.native_turn_rollback, "Cancellation still stops work without history retraction"),
            ("compaction_lifecycle", "Compaction timeline events", flags.compaction.lifecycle, "Shows provider evidence only"),
            ("background_compaction", "Observe background compaction", flags.compaction.background, "Requires compaction events outside an active prompt"),
            ("compaction_summary", "Compaction summary", flags.compaction.summary, "Requires a provider-exposed summary"),
            ("compaction_metrics", "Compaction metrics", flags.compaction.metrics, "Requires provider-exposed usage evidence"),
            ("model_selection", "Select model", flags.model_selection, "Model preferences remain specific to each provider"),
            ("effort_selection", "Select reasoning effort", flags.effort_selection, "Choices come from the selected provider"),
            ("fast_mode", "Fast service tier", flags.fast_mode, "Requires provider service-tier support"),
            ("ultrafast_mode", "Ultrafast service tier", flags.ultrafast_mode, "Requires provider service-tier support"),
            ("agent_observe", "Observe child agents", flags.agent.observe, "Requires structured provider child events"),
            ("agent_catalog", "Select child agent", flags.agent.catalog, "Requires provider agent discovery"),
            ("skill_catalog", "Select skills", flags.catalog.skill, "Requires an enforceable skill selection"),
            ("mcp_catalog", "Select MCP servers", flags.catalog.mcp, "Requires enforceable server selection"),
        ] {
            add(id, "Provider", label, false, "provider", supported, "immediate", reason);
        }
        add("approval_rules", "Security", "Saved approval rules", true, "forge", flags.permission_control,
            "immediate", "Forge persists rules and answers individual provider requests");
        add("os_sandbox", "Security", "Enforce OS sandbox", false, "provider",
            matches!(descriptor.kind, BackendKind::Codex | BackendKind::Mock), "session_restart",
            "Unsupported isolation requirements block execution rather than silently disabling isolation");
        add("background_terminals", "Provider", "Inspect background terminals", false, "provider",
            flags.background_terminals, "immediate",
            "Requires an explicit provider terminal inventory");
        add("file_diff", "Provider", "Provider file diffs", false, "provider", true, "immediate",
            "Projects structured provider changes into tool history and the exchange timeline");
        add("background_termination", "Provider", "Terminate one background terminal", false, "provider",
            matches!(descriptor.kind, BackendKind::Codex | BackendKind::Copilot | BackendKind::Mock), "immediate",
            "Root cancellation remains available without independent terminal controls");
        add("recap", "Harness", "Generate isolated recap and session name", false, "provider",
            matches!(descriptor.kind, BackendKind::Codex | BackendKind::Copilot | BackendKind::Mock), "immediate",
            "Generation must not append to the main provider conversation");
        for (id, label) in [("runtime_setup", "Install supported provider runtime"),
            ("authentication", "Sign in from Forge")] {
            add(id, "Setup", label, false, "forge",
                matches!(descriptor.kind, BackendKind::Claude | BackendKind::Antigravity), "idle",
                "ACP setup uses provider-advertised sign-in methods and preserves native credentials");
        }
        add("native_child_input", "Provider", "Deliver child-agent input", false, "provider",
            !matches!(flags.agent.input,crate::agent::AgentControlMode::Unsupported), "immediate",
            "Direct or parent-mediated control depends on the adapter");
        add("native_child_spawn", "Provider", "Start a selected child agent", false, "provider",
            !matches!(flags.agent.spawn,crate::agent::AgentControlMode::Unsupported), "immediate",
            "Provider-created children remain observable independently of an explicit spawn API");
        add("native_child_interrupt", "Provider", "Interrupt one child agent", false, "provider",
            !matches!(flags.agent.interrupt,crate::agent::AgentControlMode::Unsupported), "immediate",
            "Root cancellation still collects descendants");
        add("skill_mutation", "Settings", "Apply skill selection", false, "provider", flags.catalog.skill,
            "session_restart", "Forge persists desired selection and verifies the provider result");
        add("native_import", "Settings", "Import native skills and MCP definitions", false, "forge", true,
            "server_restart", "Explicit source preview and selection. Imported entries start disabled and preserve native value references");
        add("mcp_mutation", "Settings", "Apply MCP selection", false, "provider", flags.catalog.mcp,
            if flags.catalog.live_mcp_mutation { "immediate" } else { "server_restart" },
            "Forge persists desired selection independently of native provider configuration");
        for (id, label) in [("mcp_runtime_status", "Inspect MCP connection status"),
            ("mcp_tool_catalog", "Browse MCP tool definitions")] {
            add(id, "Provider", label, false, "provider", flags.catalog.mcp
                && matches!(descriptor.kind, BackendKind::Codex | BackendKind::Copilot | BackendKind::Mock),
                "immediate", "ACP accepts configured servers but does not expose their readiness or tool inventory");
        }
        if matches!(descriptor.kind, BackendKind::Claude | BackendKind::Antigravity) {
            for entry in &mut capability {
                if entry.id == "agent_observe" && flags.agent.observe {
                    entry.support = Support::Limited;
                    entry.reason = "Shows advertised child session lifecycles and separate timelines during active prompts. Independent child controls and background observation are unavailable";
                }
            }
        }
        if descriptor.kind == BackendKind::Antigravity {
            for entry in &mut capability {
                match entry.id {
                    "file_diff" => {
                        entry.support = Support::Limited;
                        entry.reason = "Antigravity edit tools can complete without structured diff content. Forge checkpoint review remains available";
                    }
                    "approval_rules" if flags.permission_control => {
                        entry.support = Support::Limited;
                        entry.reason = "Forge evaluates received approval requests. Antigravity enterprise terminal policy can execute without an ACP callback";
                    }
                    "skill_catalog" | "skill_mutation" if flags.catalog.skill => {
                        entry.support = Support::Limited;
                        entry.reason = "Forge controls profile skills. Excluding a project-native skill is rejected because ACP does not expose that control";
                    }
                    _ => {}
                }
            }
        }
        Self { backend: descriptor.kind, label: descriptor.label, revision: 4, capability,
            provider_version:None,availability:"ready",availability_reason:None,notice:Vec::new() }
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
    fn antigravity_reports_policy_and_skill_limits_before_runtime_discovery() {
        for backend in [BackendKind::Claude, BackendKind::Antigravity] {
            let manifest = CapabilityManifest::from_descriptor(BackendDescriptor {
                kind: backend,
                label: "test".into(),
                capability: super::super::BackendCapability {
                    permission_control: true,
                    catalog: super::super::catalog::CatalogCapability {
                        skill: true,
                        ..Default::default()
                    },
                    ..Default::default()
                },
            });
            for id in ["approval_rules", "skill_catalog", "skill_mutation", "file_diff"] {
                let entry = manifest.capability.iter().find(|entry| entry.id == id).unwrap();
                assert!(matches!((&entry.support, backend),
                    (Support::Limited, BackendKind::Antigravity) | (Support::Supported, BackendKind::Claude)));
            }
        }
    }
}
