//! Retain committed host file changes independently of native item snapshots.
use serde::{Deserialize, Serialize};

/// Attribute a committed host mutation to its original authenticated host callback.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct HostFileChange {
    /// Preserve the invocation that authorized and performed the mutation.
    pub(crate) scope: crate::provider::InvocationScope,
    /// Preserve host callback identity independently of any native item ID.
    pub(crate) call_id: forge_provider_api::control::ToolCallId,
    /// Attach a native item only after a positive adapter correlation exists.
    pub(crate) native_item_id: Option<forge_provider_api::ItemId>,
    /// Preserve committed file operations without modifying native final snapshots.
    pub(crate) changes: forge_provider_api::ProviderChangeSet,
}
