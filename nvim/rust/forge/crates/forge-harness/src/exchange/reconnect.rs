//! Capture a coherent owner snapshot while a bounded lease pauses outbound publication.

use std::num::NonZeroU16;

use super::publication::{
    PublicationError, PublicationLease, PublicationReconciliation, PublicationSequence,
};
use super::services::OwnerServices;

/// Keep response content and its output barrier together until transport reports complete admission.
pub(crate) struct ReconnectSnapshot {
    response: serde_json::Value,
    confirmation: ReconnectConfirmation,
}

impl ReconnectSnapshot {
    /// Separate the complete RPC payload from its confirmation, which must survive the entire send.
    pub(crate) fn into_parts(self) -> (serde_json::Value, ReconnectConfirmation) {
        (self.response, self.confirmation)
    }
}

/// Prove which exact retained page was included in a successfully admitted owner snapshot.
pub(crate) struct ReconnectConfirmation {
    lease: PublicationLease,
    receipt: PublicationReconciliation,
}

impl ReconnectConfirmation {
    /// Record the admitted page before releasing the matching outbound barrier.
    pub(super) fn confirm(self, services: &OwnerServices) -> Result<(), ReconnectError> {
        services
            .publications
            .confirm_reconciliation(&self.lease, &self.receipt)?;
        Ok(())
    }
}

/// Distinguish bounded reconnect admission from snapshot, storage, and transport-observation failures.
#[derive(Debug, thiserror::Error)]
pub(crate) enum ReconnectError {
    #[error("exchange owner is unavailable for reconnect")]
    OwnerClosed,
    #[error("exchange owner reconnect intake is full")]
    QueueFull,
    #[error("reconnect owner response was not observed")]
    ObservationLost,
    #[error("reconnect snapshot could not be captured")]
    Snapshot(#[source] anyhow::Error),
    #[error(transparent)]
    Publication(#[from] PublicationError),
}

impl ReconnectError {
    /// Get a stable transport code without exposing native or storage details.
    pub(crate) fn code(&self) -> &'static str {
        match self {
            Self::OwnerClosed => "reconnect_owner_closed",
            Self::QueueFull => "reconnect_queue_full",
            Self::ObservationLost => "reconnect_observation_lost",
            Self::Snapshot(_) => "reconnect_snapshot_unavailable",
            Self::Publication(PublicationError::DeliveryInProgress) => "reconnect_output_busy",
            Self::Publication(PublicationError::ReconciliationExpired) => "reconnect_expired",
            Self::Publication(PublicationError::InvalidCursor) => "reconnect_invalid_cursor",
            Self::Publication(
                PublicationError::IdentityConflict
                | PublicationError::SessionMismatch
                | PublicationError::InvalidEvent
                | PublicationError::InvalidSequence
                | PublicationError::SequenceExhausted
                | PublicationError::Unavailable
                | PublicationError::Storage(_)
                | PublicationError::Codec(_)
                | PublicationError::DeliveryState { .. },
            ) => "reconnect_publication_unavailable",
        }
    }
}

/// Capture state and its bounded retained-publication prefix in one owner update.
pub(super) fn capture(
    broker: &crate::broker::HarnessBroker,
    services: &OwnerServices,
    cursor: Option<PublicationSequence>,
    limit: NonZeroU16,
    update_step: u64,
    execution_available: bool,
) -> Result<ReconnectSnapshot, ReconnectError> {
    let lease = services.publications.begin_reconciliation()?;
    let snapshot = broker.snapshot().map_err(ReconnectError::Snapshot)?;
    let input = services
        .input
        .snapshot()
        .map_err(ReconnectError::Snapshot)?;
    let page = services.publications.snapshot(cursor, limit)?;
    let high_water = page.high_water();
    let (records, receipt) = page.into_parts();
    let through = records.last().map(|record| record.sequence());
    let events: Vec<_> = records
        .iter()
        .map(|record| {
            serde_json::json!({
                "sequence": record.sequence(), "event": record.delivery_event(), "delivery": record.state(),
            })
        })
        .collect();
    let mut response =
        serde_json::to_value(snapshot).map_err(|error| ReconnectError::Snapshot(error.into()))?;
    // Reconciliation may acknowledge input deltas carried only in this snapshot response.
    // Include their authoritative queue state before that page can be confirmed.
    response["input"] =
        serde_json::to_value(input).map_err(|error| ReconnectError::Snapshot(error.into()))?;
    response["owner_publication"] = serde_json::json!({
        "receipt_id": uuid::Uuid::new_v4().to_string(),
        "after": cursor, "through": through, "high_water": high_water,
        "more": through.or(cursor) != high_water, "events": events,
        "update_step": update_step, "execution_available": execution_available,
    });
    Ok(ReconnectSnapshot {
        response,
        confirmation: ReconnectConfirmation { lease, receipt },
    })
}
