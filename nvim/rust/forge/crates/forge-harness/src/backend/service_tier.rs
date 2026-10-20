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

impl ServiceTier {
    /// Rejects accelerated tiers the selected provider does not advertise.
    pub fn validate(self, capability: &super::BackendCapability) -> anyhow::Result<()> {
        let supported = match self {
            Self::Standard => true,
            Self::Fast => capability.fast_mode,
            Self::Ultrafast => capability.ultrafast_mode,
        };
        anyhow::ensure!(
            supported,
            "current backend does not support the requested service tier"
        );
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn speed_selection_requires_the_advertised_provider_capability() {
        let mut capability = crate::backend::BackendCapability::default();
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
