//! Sanitized display text shared by normalized diagnostics.

use serde::{Deserialize, Serialize};

/// Carry display text explicitly sanitized by the producer for the host boundary.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(try_from = "String", into = "String")]
pub struct SanitizedMessage(String);

impl SanitizedMessage {
    /// Create a nonempty display message after removing provider codes, payloads, and secrets.
    pub fn new(value: impl Into<String>) -> Result<Self, SanitizedMessageError> {
        let value = value.into();
        if value.trim().is_empty() {
            return Err(SanitizedMessageError);
        }
        Ok(Self(value))
    }

    /// Get the sanitized display text without parsing it as machine-readable state.
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl TryFrom<String> for SanitizedMessage {
    type Error = SanitizedMessageError;

    /// Validate deserialized messages using the constructor invariant.
    fn try_from(value: String) -> Result<Self, Self::Error> {
        Self::new(value)
    }
}

impl From<SanitizedMessage> for String {
    /// Encode sanitized display text for the host transport.
    fn from(value: SanitizedMessage) -> Self {
        value.0
    }
}

impl std::fmt::Display for SanitizedMessage {
    /// Render the already-sanitized display text.
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str(&self.0)
    }
}

/// Reject an empty display message.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct SanitizedMessageError;

impl std::fmt::Display for SanitizedMessageError {
    /// Describe the invalid display value.
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str("diagnostic message is empty")
    }
}

impl std::error::Error for SanitizedMessageError {}
