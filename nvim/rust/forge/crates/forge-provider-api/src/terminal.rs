use serde::{Deserialize, Serialize};

/// Provider-owned shell processes whose lifetime is independent of a Harness exchange.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct TerminalSnapshot {
    /// Whether this adapter implements terminal inventory for the exact binding.
    pub supported: bool,
    /// The provider session is initializing or applying resource configuration.
    pub starting: bool,
    /// Currently observed independently owned shells; an empty list contains no live shells.
    pub terminal: Vec<BackgroundTerminal>,
}

/// A live shell identity and its display command, scoped to one provider session.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct BackgroundTerminal {
    /// Adapter-issued identity resolved only within the binding supplied to terminal operations.
    pub id: TerminalId,
    /// User-visible command label, never parsed as a terminal identity.
    pub command: String,
}

/// Identify one adapter-issued terminal within the binding supplied to terminal operations.
/// A replacement binding cannot resolve this identity to a newer shell.
#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
#[serde(try_from = "String", into = "String")]
pub struct TerminalId(String);
impl TerminalId {
    /// Create an opaque nonempty terminal identity.
    pub fn new(value: impl Into<String>) -> Result<Self, crate::lifecycle::LifecycleError> {
        let value = value.into();
        if value.trim().is_empty() {
            return Err(crate::lifecycle::LifecycleError::EmptyTerminal);
        }
        Ok(Self(value))
    }
    /// Get the opaque token for presentation or its owning adapter's native lookup.
    pub fn as_str(&self) -> &str {
        &self.0
    }
}
impl TryFrom<String> for TerminalId {
    type Error = crate::lifecycle::LifecycleError;
    /// Validate deserialized identities before resolving any native terminal.
    fn try_from(value: String) -> Result<Self, Self::Error> {
        Self::new(value)
    }
}
impl From<TerminalId> for String {
    /// Encode the opaque validated identity.
    fn from(value: TerminalId) -> Self {
        value.0
    }
}
