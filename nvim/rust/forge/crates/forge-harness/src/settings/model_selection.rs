//! Validate executable model settings before a workflow can commit or dispatch them.

use crate::provider::{ProviderCapability, ProviderModel, ServiceTier};
use crate::session::{HarnessSession, PlanExecutor};

/// Retain a complete catalog-validated selection for a tool-free or executor invocation.
pub(crate) struct ValidatedModelSelection {
    model: String,
    effort: String,
    service_tier: ServiceTier,
}

/// Distinguish unsupported requested settings from unavailable model catalog entries.
#[derive(Debug, thiserror::Error)]
pub(crate) enum ModelSelectionError {
    /// An executor must select its model explicitly.
    #[error("executor model is not configured")]
    ModelRequired,
    /// An executor must select its reasoning policy explicitly.
    #[error("executor thinking is not configured")]
    EffortRequired,
    /// Explicit model selection requires an advertised capability.
    #[error("this provider does not support model selection")]
    ModelSelectionUnsupported,
    /// The model is absent from the current normalized catalog.
    #[error("the selected model is unavailable")]
    ModelUnavailable,
    /// The model or provider cannot enforce the explicit reasoning choice.
    #[error("the selected reasoning effort is unavailable")]
    EffortUnavailable,
    /// The explicit service tier is unsupported by the provider or selected model.
    #[error(transparent)]
    ServiceTier(#[from] forge_provider_api::service_tier::ServiceTierError),
}

enum TierSelection {
    Explicit(ServiceTier),
    Inherited(ServiceTier),
}

impl ValidatedModelSelection {
    /// Validate an explicit Background request without changing saved conversation state.
    pub(crate) fn background(
        config: &crate::service::TextGenerationConfig,
        capability: &ProviderCapability,
        catalog: &[ProviderModel],
    ) -> Result<Self, ModelSelectionError> {
        Self::resolve(
            &config.model,
            config.reasoning_effort.as_deref().unwrap_or("default"),
            TierSelection::Explicit(config.service_tier),
            capability,
            catalog,
        )
    }

    /// Validate executor tuning and retain the source tier only when the target supports it.
    pub(crate) fn executor(
        executor: &PlanExecutor,
        source_tier: ServiceTier,
        capability: &ProviderCapability,
        catalog: &[ProviderModel],
    ) -> Result<Self, ModelSelectionError> {
        Self::resolve(
            executor
                .model
                .as_deref()
                .ok_or(ModelSelectionError::ModelRequired)?,
            executor
                .effort
                .as_deref()
                .ok_or(ModelSelectionError::EffortRequired)?,
            TierSelection::Inherited(source_tier),
            capability,
            catalog,
        )
    }

    /// Commit a validated selection and remove observations tied to the previous model controls.
    pub(crate) fn apply(&self, session: &mut HarnessSession) {
        if session.model != self.model
            || session.effort != self.effort
            || session.service_tier != self.service_tier
            || session.context_window.is_some()
        {
            session.context_usage = None;
        }
        session.model = self.model.clone();
        session.resolved_model = None;
        session.effort = self.effort.clone();
        session.context_window = None;
        session.service_tier = self.service_tier;
    }

    /// Resolve requested values once; only inherited tiers may fall back to standard processing.
    fn resolve(
        model: &str,
        effort: &str,
        tier: TierSelection,
        capability: &ProviderCapability,
        catalog: &[ProviderModel],
    ) -> Result<Self, ModelSelectionError> {
        if model != "default" && !capability.model_selection {
            return Err(ModelSelectionError::ModelSelectionUnsupported);
        }
        let selected = catalog
            .iter()
            .find(|candidate| {
                if model == "default" {
                    candidate.is_default
                } else {
                    candidate.id == model
                }
            })
            .ok_or(ModelSelectionError::ModelUnavailable)?;
        if effort != "default"
            && (!capability.effort_selection
                || !selected.reasoning.iter().any(|choice| choice == effort))
        {
            return Err(ModelSelectionError::EffortUnavailable);
        }
        let validate_tier = |tier: ServiceTier| {
            tier.validate(capability)?;
            tier.validate_model(selected)
        };
        let service_tier = match tier {
            TierSelection::Explicit(tier) => {
                validate_tier(tier)?;
                tier
            }
            TierSelection::Inherited(tier) => {
                if validate_tier(tier).is_ok() {
                    tier
                } else {
                    ServiceTier::Standard
                }
            }
        };
        Ok(Self {
            model: model.into(),
            effort: effort.into(),
            service_tier,
        })
    }
}
