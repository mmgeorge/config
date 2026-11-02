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
    /// Report OS sandbox support from the provider allowlist.
    pub fn supports_sandbox(self) -> bool {
        matches!(self, Self::Codex)
    }

    /// Resolve requested isolation for the selected provider without changing shared preferences.
    pub fn resolve_access(self, mut access: AccessPolicy) -> AccessPolicy {
        access.sandbox &= self.supports_sandbox();
        access
    }

    /// Reject OS isolation requests that this provider cannot enforce.
    pub fn validate_access(self, access: &AccessPolicy) -> Result<()> {
        anyhow::ensure!(
            !access.sandbox || self.supports_sandbox(),
            "This provider cannot enforce Forge's OS sandbox"
        );
        Ok(())
    }

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
