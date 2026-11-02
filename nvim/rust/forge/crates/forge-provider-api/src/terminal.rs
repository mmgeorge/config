use serde::Serialize;

/// Provider-owned shell processes whose lifetime is independent of a Harness exchange.
#[derive(Clone, Debug, Default, Serialize)]
pub struct TerminalSnapshot {
    pub supported: bool,
    /// The provider session is initializing or applying resource configuration.
    pub starting: bool,
    pub terminal: Vec<BackgroundTerminal>,
}

/// A live shell identity and its display command, scoped to one provider session.
#[derive(Clone, Debug, Serialize)]
pub struct BackgroundTerminal {
    pub id: String,
    pub command: String,
}
