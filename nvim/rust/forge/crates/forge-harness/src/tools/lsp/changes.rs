//! Retain committed host file changes independently of native item snapshots.
use serde::{Deserialize, Serialize};

/// Attribute a committed host mutation to its original authenticated host callback.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct HostFileChange {
    /// Retain live writer exclusion through durable recording; restored records need no live lease.
    #[serde(skip)]
    pub(crate) reservation: Option<crate::checks::MutationReservation>,
    /// Preserve the invocation that authorized and performed the mutation.
    pub(crate) scope: crate::provider::InvocationScope,
    /// Preserve host callback identity independently of any native item ID.
    pub(crate) call_id: forge_provider_api::control::ToolCallId,
    /// Attach a native item only after a positive adapter correlation exists.
    pub(crate) native_item_id: Option<forge_provider_api::ItemId>,
    /// Preserve committed file operations without modifying native final snapshots.
    pub(crate) changes: forge_provider_api::ProviderChangeSet,
}

impl HostFileChange {
    /// Acknowledge durable physical-change storage without depending on a surviving caller.
    pub(crate) fn recorded(&self) -> anyhow::Result<()> {
        if let Some(reservation) = &self.reservation {
            reservation.recorded()?;
        }
        Ok(())
    }
}

impl PartialEq for HostFileChange {
    /// Compare recorded facts independently of transient reservation ownership.
    fn eq(&self, other: &Self) -> bool {
        let Self {
            reservation: _,
            scope,
            call_id,
            native_item_id,
            changes,
        } = self;
        let Self {
            reservation: _,
            scope: other_scope,
            call_id: other_call,
            native_item_id: other_item,
            changes: other_changes,
        } = other;
        scope == other_scope
            && call_id == other_call
            && native_item_id == other_item
            && changes == other_changes
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Preserve the lease across archive clones while omitting it from persisted facts.
    #[test]
    fn archived_changes_hold_reservation_until_durable_acknowledgment() -> anyhow::Result<()> {
        let root = tempfile::tempdir()?;
        let change = HostFileChange {
            reservation: Some(crate::checks::MutationReservation::acquire(root.path())?),
            scope: crate::provider::InvocationScope::new(
                crate::provider::ThreadOwner::root("session"),
                "turn",
            ),
            call_id: forge_provider_api::control::ToolCallId::new("rename")?,
            native_item_id: None,
            changes: Default::default(),
        };
        let slot = crate::provider::CompletionSlot::default();
        slot.publish(crate::provider::ProviderCompletion {
            host_file_changes: vec![change.clone()],
            ..Default::default()
        })?;
        let restored: HostFileChange = serde_json::from_slice(&serde_json::to_vec(&change)?)?;
        assert_eq!(restored, change);
        assert!(restored.reservation.is_none());
        drop(change);
        assert!(
            crate::checks::CheckReservation::acquire(
                root.path(),
                crate::checks::CheckPurpose::Verify
            )
            .is_err()
        );
        let archived = slot.collected_file_changes();
        archived[0].recorded()?;
        archived[0].recorded()?;
        assert!(
            crate::checks::CheckReservation::acquire(
                root.path(),
                crate::checks::CheckPurpose::Verify
            )
            .is_ok()
        );
        Ok(())
    }
}
