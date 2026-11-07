//! Serialize host settlement and revalidate only its original durable exchange.

use std::collections::HashSet;

use anyhow::{Context, Result};

use super::HarnessBroker;
use crate::provider::{InvocationId, InvocationScope};

/// Serialize host checkpoint/finalization work under its exact original invocation.
#[derive(Default)]
pub(super) struct HostSettlementFence {
    settling: HashSet<InvocationId>,
    settled: std::sync::Arc<tokio::sync::Notify>,
}
impl HostSettlementFence {
    /// Mark an independently owned host prerequisite before releasing the broker lock.
    pub(super) fn begin_settlement(&mut self, scope: &InvocationScope) {
        self.settling.insert(scope.invocation_id.clone());
    }
    /// Release the exact original host prerequisite and wake competing finalizers.
    pub(super) fn end_settlement(&mut self, scope: &InvocationScope) {
        self.settling.remove(&scope.invocation_id);
        self.settled.notify_one();
    }
    /// Get the host-prerequisite completion notification.
    pub(super) fn settlement_notification(&self) -> std::sync::Arc<tokio::sync::Notify> {
        self.settled.clone()
    }
    /// Test whether this original invocation owns a pending host prerequisite.
    pub(super) fn is_settling(&self, scope: &InvocationScope) -> bool {
        self.settling.contains(&scope.invocation_id)
    }
}

impl HarnessBroker {
    /// Apply accepted observations before revalidating an original host checkpoint.
    pub(super) async fn refresh_settlement(
        &mut self,
        interaction: &mut crate::exchange::Exchange,
        scope: &InvocationScope,
    ) -> Result<bool> {
        let outcome = interaction.finalization_outcome();
        let host_change_count = interaction.host_file_changes.len();
        for observation in self.observer.session_events().drain()? {
            let events = self.observe_session_event(observation)?;
            self.settlement_events.extend(events);
        }
        let current = self
            .store
            .load_exchange(&interaction.id)?
            .context("original exchange disappeared during host settlement")?;
        anyhow::ensure!(
            current
                .turn
                .last()
                .is_none_or(|turn| &turn.provider().scope == scope),
            "host settlement cannot collect a successor invocation"
        );
        let changed = outcome != current.finalization_outcome()
            || host_change_count != current.host_file_changes.len();
        *interaction = current;
        Ok(changed)
    }
}
