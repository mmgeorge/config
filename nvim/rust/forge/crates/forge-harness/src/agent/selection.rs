use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Deserialize, Serialize)]
/// Persists a child's resolved provider selection and inherited execution authority.
pub struct AgentSelection {
    /// Chooses the adapter independently of the root provider.
    pub provider: crate::backend::BackendKind,
    /// Retains the advertised model selected before task admission.
    pub model: String,
    /// Retains reasoning validated against that model's supported choices.
    pub reasoning_effort: String,
    /// Retains the selected provider context tier.
    pub context_window: Option<String>,
    /// Retains a compatible provider service tier.
    pub service_tier: crate::backend::ServiceTier,
    /// Prevents cross-provider creation from weakening the root's access policy.
    pub access: crate::session::AccessPolicy,
    /// Retains the root's effective approval mode.
    pub execution_mode: crate::session::PermissionMode,
}
