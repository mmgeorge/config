use serde::{Deserialize, Serialize};

/// Identify a malformed terminal inventory without retaining native values or command text.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum TerminalInventoryError {
    /// The response has no terminal array.
    MissingInventory,
    /// A live terminal has no string identity.
    MissingIdentity,
    /// A live terminal identity is empty or whitespace.
    InvalidIdentity,
    /// A terminal requested for display has no command label.
    MissingCommand,
}

impl std::fmt::Display for TerminalInventoryError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str(match self {
            Self::MissingInventory => "background terminal inventory is missing",
            Self::MissingIdentity => "background terminal identity is missing",
            Self::InvalidIdentity => "background terminal identity is invalid",
            Self::MissingCommand => "background terminal command is missing",
        })
    }
}

impl std::error::Error for TerminalInventoryError {}

/// Provider-owned shells whose lifetime is independent of a Harness exchange.
///
/// A provider without terminal inventory reports `UnsupportedCapability` instead of an
/// inventory, so unsupported, opening, empty, and failed reads stay distinguishable.
#[derive(Clone, Debug, Deserialize, Serialize, Eq, PartialEq)]
pub struct TerminalInventory {
    /// Shells observed so far; an empty complete inventory holds no live shells.
    pub terminal: Vec<BackgroundTerminal>,
    /// Whether every native inventory source for the binding has reported.
    pub complete: bool,
}

impl TerminalInventory {
    /// Report a complete inventory of the observed shells.
    pub fn observed(terminal: Vec<BackgroundTerminal>) -> Self {
        Self {
            terminal,
            complete: true,
        }
    }

    /// Report an inventory whose native sources have not all reported yet.
    pub fn opening() -> Self {
        Self {
            terminal: Vec::new(),
            complete: false,
        }
    }
}

/// A live shell identity and its display command, scoped to one provider session.
#[derive(Clone, Debug, Deserialize, Serialize, Eq, PartialEq)]
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
