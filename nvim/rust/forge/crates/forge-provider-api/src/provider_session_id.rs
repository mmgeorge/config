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

impl TryFrom<&str> for ProviderSessionId {
    type Error = ProviderSessionIdError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        value.parse()
    }
}

impl std::str::FromStr for ProviderSessionId {
    type Err = ProviderSessionIdError;

    /// Validate a borrowed identity without interpreting its opaque contents.
    fn from_str(value: &str) -> Result<Self, Self::Err> {
        value.to_owned().try_into()
    }
}

impl From<ProviderSessionId> for String {
    /// Encode the original native identity without changing its representation.
    fn from(value: ProviderSessionId) -> Self {
        value.0
    }
}

impl std::fmt::Display for ProviderSessionId {
    /// Render only the opaque identity; callers must not interpret its representation.
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str(self.as_str())
    }
}
