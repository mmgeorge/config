//! Present provider terminal inventory together with Harness availability state.

use forge_provider_api::terminal::{BackgroundTerminal, TerminalInventory};
use serde::Serialize;

/// Describe background terminals for the editor, distinguishing unsupported, starting, empty,
/// and populated inventories.
#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub struct BackgroundTerminalStatus {
    /// Whether the provider exposes terminal inventory at all.
    pub supported: bool,
    /// Whether provider startup or a native inventory source has not reported yet.
    pub starting: bool,
    /// Shells observed so far.
    pub terminal: Vec<BackgroundTerminal>,
}

impl BackgroundTerminalStatus {
    /// Report an inventory that waits on provider startup before any native read.
    pub fn pending_startup(starting: bool) -> Self {
        Self {
            supported: true,
            starting,
            terminal: Vec::new(),
        }
    }

    /// Present one provider inventory; an incomplete inventory is still starting.
    pub fn from_inventory(inventory: TerminalInventory) -> Self {
        Self {
            supported: true,
            starting: !inventory.complete,
            terminal: inventory.terminal,
        }
    }
}
