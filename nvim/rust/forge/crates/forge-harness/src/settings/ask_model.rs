use anyhow::{Context, Result, ensure};
use serde::{Deserialize, Serialize};

use crate::backend::{BackendModel, ServiceTier};
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
        catalog: &[BackendModel],
    ) -> Result<Self> {
        let Some(model) = &self.model else {
            ensure!(
                self.effort.is_none() && self.service_tier.is_none(),
                "Ask model tuning requires a model"
            );
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
            .with_context(|| format!("Ask model {model} is unavailable for this provider"))?;
        let effort = self
            .effort
            .clone()
            .or_else(|| selected.default_reasoning.clone());
        if let Some(effort) = &effort {
            ensure!(
                selected.reasoning.is_empty() || selected.reasoning.contains(effort),
                "Ask model reasoning {effort} is unavailable for {model}"
            );
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

impl super::SettingsSnapshot {
    /// Read provider-local Ask settings, defaulting older records to the planning model.
    pub(crate) fn ask_model(&self) -> Result<AskModel> {
        self.provider
            .get("ask_model")
            .cloned()
            .map(serde_json::from_value)
            .transpose()
            .map(|value| value.unwrap_or_default())
            .context("read Ask model settings")
    }
}
