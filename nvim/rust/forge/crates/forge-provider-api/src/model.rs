use crate::{
    ServiceTier, access::AccessPolicy, agent::AgentCapability, approval::ApprovalMode,
    catalog::CatalogCapability, compaction::CompactionCapability,
};
use serde::{Deserialize, Serialize};
use std::fmt;
use thiserror::Error;

/// Identifies a provider without exposing its native launch or protocol details.
#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize)]
pub struct ProviderId(String);

/// Reject invalid serialized or constructed provider identities.
#[derive(Clone, Copy, Debug, Eq, Error, PartialEq)]
pub enum ProviderIdError {
    /// Provider identities cannot be empty or whitespace-only.
    #[error("provider identity cannot be empty")]
    Empty,
    /// Provider identities cannot contain surrounding whitespace.
    #[error("provider identity cannot have surrounding whitespace")]
    SurroundingWhitespace,
}

impl ProviderId {
    /// Create an opaque provider identity from a nonempty canonical identifier.
    pub fn new(value: impl Into<String>) -> std::result::Result<Self, ProviderIdError> {
        let value = value.into();
        if value.trim().is_empty() {
            return Err(ProviderIdError::Empty);
        }
        if value != value.trim() {
            return Err(ProviderIdError::SurroundingWhitespace);
        }
        Ok(Self(value))
    }

    /// Get the serialized identity used by persistence and host protocols.
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl TryFrom<String> for ProviderId {
    type Error = ProviderIdError;

    fn try_from(value: String) -> std::result::Result<Self, Self::Error> {
        Self::new(value)
    }
}

impl<'de> Deserialize<'de> for ProviderId {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let value = String::deserialize(deserializer)?;
        Self::try_from(value).map_err(serde::de::Error::custom)
    }
}

impl fmt::Display for ProviderId {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(&self.0)
    }
}

/// Describes provider identity and capability for broker and editor consumers.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ProviderDescriptor {
    pub kind: ProviderId,
    pub label: String,
    pub capability: ProviderCapability,
}

/// Represents provider features that control visible Harness actions.
#[derive(Clone, Debug, Default, Deserialize, Serialize, Eq, PartialEq)]
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

#[derive(Clone, Copy, Debug, Eq, PartialEq, Error)]
pub enum AccessCapabilityError {
    #[error("This provider cannot enforce Forge's OS sandbox")]
    SandboxUnsupported,
    #[error("Unsupported isolation mode")]
    IsolationModeUnsupported,
}

/// Represents one executable provider launch descriptor.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ProviderLaunch {
    pub kind: ProviderId,
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
    pub provider_session_id: crate::ProviderSessionId,
    pub timing: Vec<ProviderTimingRecord>,
}

impl ProviderForkResult {
    /// Build an unprofiled native fork result from its provider session identity.
    pub fn unprofiled(provider_session_id: crate::ProviderSessionId) -> Self {
        Self {
            provider_session_id,
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
#[derive(Clone, Debug, Default, Deserialize, Serialize, Eq, PartialEq)]
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
        if access.isolation_mode.as_ref().is_some_and(|mode| {
            !self
                .integration
                .isolation_modes
                .iter()
                .any(|choice| &choice.id == mode)
        }) {
            access.isolation_mode = None;
        }
        access
    }

    /// Reject OS isolation requests that this provider cannot enforce.
    pub fn validate_access(&self, access: &AccessPolicy) -> Result<(), AccessCapabilityError> {
        if access.sandbox && !self.supports_sandbox() {
            return Err(AccessCapabilityError::SandboxUnsupported);
        }
        if let Some(mode) = &access.isolation_mode
            && !self
                .integration
                .isolation_modes
                .iter()
                .any(|choice| &choice.id == mode)
        {
            return Err(AccessCapabilityError::IsolationModeUnsupported);
        }
        Ok(())
    }
}

/// Describes an implementation-owned isolation choice and its actual guarantees.
#[derive(Clone, Debug, Deserialize, Serialize, Eq, PartialEq)]
pub struct IsolationMode {
    pub id: String,
    pub label: String,
    pub description: String,
}

/// Identify a model within the owning provider's catalog.
#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
#[serde(try_from = "String", into = "String")]
pub struct ModelId(String);

impl ModelId {
    /// Create an opaque model identity from observed native evidence.
    pub fn new(value: impl Into<String>) -> Result<Self, crate::identity::IdentityError> {
        let value = value.into();
        if value.trim().is_empty() {
            return Err(crate::identity::IdentityError);
        }
        Ok(Self(value))
    }

    /// Get the native model identifier for display or transport.
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl TryFrom<String> for ModelId {
    type Error = crate::identity::IdentityError;
    /// Validate a serialized model identity.
    fn try_from(value: String) -> Result<Self, Self::Error> {
        Self::new(value)
    }
}

impl From<ModelId> for String {
    /// Encode the model identity without inventing an alias.
    fn from(value: ModelId) -> Self {
        value.0
    }
}

impl std::fmt::Display for ModelId {
    /// Render the observed model identifier at a presentation boundary.
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(formatter)
    }
}
/// Record a model positively confirmed for the addressed turn.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct ResolvedModel {
    /// Identify the actual native model, independently of requested defaults.
    pub model_id: ModelId,
    /// Preserve when native evidence confirmed this model.
    pub observed_at_ms: i64,
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Provider changes clear unavailable inherited isolation, while explicit choices reject.
    #[test]
    fn unsupported_isolation_is_cleared_on_resolution_and_rejected_on_admission() {
        let capability = ProviderCapability::default();
        let access = AccessPolicy {
            sandbox: false,
            isolation_mode: Some("unavailable".into()),
            ..Default::default()
        };
        assert_eq!(
            capability.validate_access(&access),
            Err(AccessCapabilityError::IsolationModeUnsupported)
        );
        let resolved = capability.resolve_access(access);
        assert!(resolved.isolation_mode.is_none());
        assert!(capability.validate_access(&resolved).is_ok());
    }
}
