use serde::{Deserialize, Serialize};

#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
/// Selects one provider service tier for a session and its requests.
pub enum ServiceTier {
    #[default]
    #[serde(rename = "default")]
    /// Explicitly selects standard processing instead of inheriting a prior accelerated tier.
    Standard,
    /// Requests the provider's fast processing tier.
    Fast,
    /// Requests the provider's ultrafast processing tier.
    Ultrafast,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, thiserror::Error)]
pub enum ServiceTierError {
    #[error("current provider does not support the requested service tier")]
    UnsupportedByProvider,
    #[error("selected model does not support the requested service tier")]
    UnsupportedByModel,
}

impl ServiceTier {
    /// Rejects accelerated tiers absent from the selected model's catalog entry.
    pub fn validate_model(self, model: &super::ProviderModel) -> Result<(), ServiceTierError> {
        if self != Self::Standard && !model.service_tiers.contains(&self) {
            return Err(ServiceTierError::UnsupportedByModel);
        }
        Ok(())
    }

    /// Rejects accelerated tiers the selected provider does not advertise.
    pub fn validate(self, capability: &super::ProviderCapability) -> Result<(), ServiceTierError> {
        let supported = match self {
            Self::Standard => true,
            Self::Fast => capability.fast_mode,
            Self::Ultrafast => capability.ultrafast_mode,
        };
        if !supported {
            return Err(ServiceTierError::UnsupportedByProvider);
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn speed_selection_requires_the_advertised_provider_capability() {
        let mut capability = crate::ProviderCapability::default();
        assert!(ServiceTier::Standard.validate(&capability).is_ok());
        assert!(ServiceTier::Fast.validate(&capability).is_err());
        assert!(ServiceTier::Ultrafast.validate(&capability).is_err());
        capability.fast_mode = true;
        assert!(ServiceTier::Fast.validate(&capability).is_ok());
        assert!(ServiceTier::Ultrafast.validate(&capability).is_err());
        capability.ultrafast_mode = true;
        assert!(ServiceTier::Ultrafast.validate(&capability).is_ok());
    }
}
