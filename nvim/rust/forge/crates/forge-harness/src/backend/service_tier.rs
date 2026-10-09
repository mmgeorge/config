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
