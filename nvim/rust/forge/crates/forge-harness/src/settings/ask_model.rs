use anyhow::Context;
use serde::{Deserialize, Serialize};

use crate::provider::{ProviderModel, ServiceTier};
use crate::session::HarnessSession;

/// Overrides the planning model only for forked Plan Review answers.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct AskModel {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub model: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub effort: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub service_tier: Option<ServiceTier>,
}

impl AskModel {
    /// Resolve an override against the current provider without changing the source session.
    pub(crate) fn resolve(
        &self,
        source: &HarnessSession,
        catalog: &[ProviderModel],
    ) -> Result<Self, AskModelError> {
        let Some(model) = &self.model else {
            if self.effort.is_some() || self.service_tier.is_some() {
                return Err(AskModelError::ModelRequired);
            }
            return Ok(Self {
                model: Some(
                    source
                        .resolved_model
                        .as_ref()
                        .unwrap_or(&source.model)
                        .clone(),
                ),
                effort: Some(source.effort.clone()),
                service_tier: Some(source.service_tier),
            });
        };
        let selected = catalog
            .iter()
            .find(|candidate| candidate.id == *model)
            .ok_or(AskModelError::ModelUnavailable)?;
        let effort = self
            .effort
            .clone()
            .or_else(|| selected.default_reasoning.clone());
        if let Some(effort) = &effort
            && !selected.reasoning.is_empty()
            && !selected.reasoning.contains(effort)
        {
            return Err(AskModelError::EffortUnavailable);
        }
        let service_tier = self.service_tier.unwrap_or_default();
        service_tier.validate_model(selected)?;
        Ok(Self {
            model: Some(model.clone()),
            effort,
            service_tier: Some(service_tier),
        })
    }
}

/// Distinguish normalized question-model validation failures without retaining native messages.
#[derive(Debug, thiserror::Error)]
pub(crate) enum AskModelError {
    /// An override must identify its model before selecting model-specific tuning.
    #[error("question-model tuning requires an explicit model")]
    ModelRequired,
    /// The normalized catalog does not contain the selected model.
    #[error("the selected question model is unavailable")]
    ModelUnavailable,
    /// The selected model does not support the requested reasoning choice.
    #[error("the selected question-model effort is unavailable")]
    EffortUnavailable,
    /// The selected model does not support the requested processing tier.
    #[error(transparent)]
    ServiceTier(#[from] forge_provider_api::service_tier::ServiceTierError),
}

impl super::SettingsSnapshot {
    /// Read provider-local Ask settings, defaulting older records to the planning model.
    pub(crate) fn ask_model(&self) -> anyhow::Result<AskModel> {
        self.provider
            .get("ask_model")
            .cloned()
            .map(serde_json::from_value)
            .transpose()
            .map(|value| value.unwrap_or_default())
            .context("read Ask model settings")
    }
}
