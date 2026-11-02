//! Validated identity of a native provider session.

use std::fmt;

use serde::{Deserialize, Serialize};

/// Reject an empty provider session identity.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ProviderSessionIdError;

impl fmt::Display for ProviderSessionIdError {
    /// Describe the invalid identity at a diagnostic boundary.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("provider session identity is empty")
    }
}

impl std::error::Error for ProviderSessionIdError {}

/// Identify a session in a provider's native history.
#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
#[serde(try_from = "String", into = "String")]
pub struct ProviderSessionId(String);

impl ProviderSessionId {
    /// Get the original native identity.
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl TryFrom<String> for ProviderSessionId {
    type Error = ProviderSessionIdError;

    /// Validate a native identity at the boundary.
    fn try_from(value: String) -> Result<Self, Self::Error> {
        if value.trim().is_empty() {
            return Err(ProviderSessionIdError);
        }
        Ok(Self(value))
    }
}

impl From<ProviderSessionId> for String {
    /// Encode the original native identity without changing its representation.
    fn from(value: ProviderSessionId) -> Self {
        value.0
    }
}

#[cfg(test)]
mod tests {
    use super::{ProviderSessionId, ProviderSessionIdError};

    #[test]
    fn provider_session_identity_validates_and_preserves_native_value() {
        assert_eq!(
            ProviderSessionId::try_from(String::new()),
            Err(ProviderSessionIdError)
        );
        assert_eq!(
            ProviderSessionId::try_from(" \t".to_owned()),
            Err(ProviderSessionIdError)
        );
        let id = ProviderSessionId::try_from(" native-id ".to_owned()).unwrap();
        assert_eq!(id.as_str(), " native-id ");
        assert_eq!(String::from(id), " native-id ");
    }

    #[test]
    fn provider_session_identity_json_round_trip_and_rejects_empty() {
        let id = ProviderSessionId::try_from("native-id".to_owned()).unwrap();
        let json = serde_json::to_string(&id).unwrap();
        assert_eq!(json, "\"native-id\"");
        assert_eq!(
            serde_json::from_str::<ProviderSessionId>(&json).unwrap(),
            id
        );
        assert!(serde_json::from_str::<ProviderSessionId>("\"  \"").is_err());
        assert!(serde_json::from_str::<ProviderSessionId>("null").is_err());
    }
}
