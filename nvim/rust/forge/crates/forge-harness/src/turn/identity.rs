use serde::{Deserialize, Serialize};

/// Identify a tool item under its original normalized turn without concatenating IDs.
#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct ToolAddress {
    /// Preserve the original host-resolved turn owner.
    pub turn: crate::provider::TurnAddress,
    /// Preserve the normalized item identity.
    pub item_id: forge_provider_api::ItemId,
}
