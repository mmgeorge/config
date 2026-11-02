use crate::{
    ServiceTier, access::AccessPolicy, agent::AgentCapability, approval::ApprovalMode,
    catalog::CatalogCapability, compaction::CompactionCapability,
};
use anyhow::Result;
use serde::{Deserialize, Serialize};

/// Identifies one supported provider implementation without leaking launch strings across consumers.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum ProviderKind {
    Codex,
    Copilot,
    /// Uses the published Claude Code ACP adapter.
    Claude,
    /// Uses Google's standalone Antigravity ACP server.
    Antigravity,
    Mock,
}

impl ProviderKind {
    /// Parse one persisted provider name into the supported provider set.
    pub fn parse(value: &str) -> Result<Self> {
        match value {
            "codex" => Ok(Self::Codex),
            "copilot" => Ok(Self::Copilot),
            "claude" => Ok(Self::Claude),
            "antigravity" | "agy" => Ok(Self::Antigravity),
            "mock" => Ok(Self::Mock),
            _ => anyhow::bail!("unsupported Harness provider: {value}"),
        }
    }
}

/// Describes provider identity and capability for broker and editor consumers.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ProviderDescriptor {
    pub kind: ProviderKind,
    pub label: String,
    pub capability: ProviderCapability,
}

/// Represents provider features that control visible Harness actions.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct ProviderCapability {
    /// Integration-owned support declarations consumed without provider identity switches.
    #[serde(default)]
    pub integration: ProviderIntegration,
    #[serde(default)]
    /// Enables OS isolation controls only for providers on the sandbox allowlist.
    pub os_sandbox: bool,
    pub native_fork: bool,
    /// Supports an explicit provider compaction request.
    pub native_compact: bool,
    #[serde(default)]
    /// Describes observable compaction evidence independently of manual request support.
    pub compaction: CompactionCapability,
    pub native_steer: bool,
    pub native_turn_rollback: bool,
    pub native_goal: bool,
    pub model_selection: bool,
    pub effort_selection: bool,
    pub fast_mode: bool,
    /// Enables ultrafast tier selection for this provider.
    pub ultrafast_mode: bool,
    pub permission_control: bool,
    #[serde(default)]
    /// Exposes a provider-owned terminal inventory without running a model prompt.
    pub background_terminals: bool,
    #[serde(default)]
    pub approval_mode_list: Vec<ApprovalMode>,
    pub agent: AgentCapability,
    #[serde(default)]
    pub catalog: CatalogCapability,
}

/// Represents one executable provider launch descriptor.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ProviderLaunch {
    pub kind: String,
    pub command: Vec<String>,
}

/// Represents one measured provider phase returned to broker diagnostics.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ProviderTimingRecord {
    pub phase: String,
    pub duration_ms: f64,
}

/// Represents a native provider fork and its provider-owned timing phases.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ProviderForkResult {
    #[serde(alias = "backend_session_id")]
    pub provider_session_id: String,
    pub timing: Vec<ProviderTimingRecord>,
}

impl ProviderForkResult {
    /// Build an unprofiled native fork result from its provider session identity.
    pub fn unprofiled(provider_session_id: impl Into<String>) -> Self {
        Self {
            provider_session_id: provider_session_id.into(),
            timing: Vec::new(),
        }
    }
}

/// Represents one selectable context-window tier advertised by a provider.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct ProviderContextWindow {
    pub id: String,
    pub token_limit: Option<u64>,
}

/// Represents one model and the controls advertised by its provider.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ProviderModel {
    pub id: String,
    #[serde(default)]
    /// Service tiers explicitly advertised for this model.
    pub service_tiers: Vec<ServiceTier>,
    /// Reasoning choices in ascending effort order, normalized by the adapter.
    pub reasoning: Vec<String>,
    pub default_reasoning: Option<String>,
    pub selected_reasoning: Option<String>,
    pub context_window: Vec<ProviderContextWindow>,
    pub default_context_window: Option<String>,
    pub selected_context_window: Option<String>,
    pub vision: bool,
    pub description: Option<String>,
    #[serde(default)]
    pub is_default: bool,
}

/// Declares optional operations and implementation-specific explanations.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct ProviderIntegration {
    pub background_generation: bool,
    pub isolated_generation: bool,
    pub terminal_termination: bool,
    pub runtime_setup: bool,
    pub authentication: bool,
    pub native_agent_suppression: bool,
    pub mcp_runtime_status: bool,
    pub mcp_tool_catalog: bool,
    pub tracked_submission: bool,
    pub local_acceptance: bool,
    pub resource_target: Option<String>,
    pub isolation_modes: Vec<IsolationMode>,
    pub limitations: Vec<crate::capability::FeatureLimitation>,
}

impl ProviderCapability {
    /// Report the integration's declared OS isolation support.
    pub fn supports_sandbox(&self) -> bool {
        self.os_sandbox
    }

    /// Resolve requested isolation for the selected provider without changing shared preferences.
    pub fn resolve_access(&self, mut access: AccessPolicy) -> AccessPolicy {
        access.sandbox &= self.supports_sandbox();
        access
    }

    /// Reject OS isolation requests that this provider cannot enforce.
    pub fn validate_access(&self, access: &AccessPolicy) -> Result<()> {
        anyhow::ensure!(
            !access.sandbox || self.supports_sandbox(),
            "This provider cannot enforce Forge's OS sandbox"
        );
        if access.sandbox
            && let Some(mode) = &access.isolation_mode
        {
            anyhow::ensure!(
                self.integration
                    .isolation_modes
                    .iter()
                    .any(|choice| &choice.id == mode),
                "Unsupported isolation mode: {mode}"
            );
        }
        Ok(())
    }
}

/// Describes an implementation-owned isolation choice and its actual guarantees.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct IsolationMode {
    pub id: String,
    pub label: String,
    pub description: String,
}
