use super::{Exchange, ExchangeState};

/// Borrows a saved comparison with both checkpoint identities present.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) struct CheckpointComparison<'exchange>(&'exchange str);

impl<'exchange> CheckpointComparison<'exchange> {
    pub(crate) fn text(self) -> &'exchange str {
        self.0
    }
}

/// Classifies committed workspace evidence without reading the current workspace.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum ExchangeChangeResult<'exchange> {
    Pending {
        previous: Option<CheckpointComparison<'exchange>>,
    },
    Captured(CheckpointComparison<'exchange>),
    Unavailable {
        previous: Option<CheckpointComparison<'exchange>>,
        reason: &'exchange str,
    },
}

impl Exchange {
    /// Select saved evidence, retaining earlier captures while work is unsettled or failed.
    pub(crate) fn change_result(&self) -> ExchangeChangeResult<'_> {
        let previous = match (
            &self.checkpoint_before,
            &self.checkpoint_after,
            self.checkpoint_diff_text.as_deref(),
        ) {
            (Some(_), Some(_), Some(text)) => Some(CheckpointComparison(text)),
            _ => None,
        };
        if let Some(reason) = self.finalization_error.as_deref() {
            return ExchangeChangeResult::Unavailable { previous, reason };
        }
        if matches!(
            self.state(),
            ExchangeState::Queued | ExchangeState::Running | ExchangeState::Finalizing
        ) || self.completed_at_ms().is_none()
        {
            return ExchangeChangeResult::Pending { previous };
        }
        match previous {
            Some(comparison) => ExchangeChangeResult::Captured(comparison),
            None => ExchangeChangeResult::Unavailable {
                previous: None,
                reason: if self.checkpoint_before.is_none() || self.checkpoint_after.is_none() {
                    "checkpoint identities are missing"
                } else {
                    "saved checkpoint comparison is missing"
                },
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    fn exchange() -> Exchange {
        serde_json::from_value(json!({
            "id":"exchange", "session_id":"session", "agent_id":"primary", "ordinal":1,
            "prompt":"Edit", "state":"complete", "created_at_ms":0, "completed_at_ms":10,
            "attributed_matches_checkpoint":false, "node_list":[],
            "checkpoint_before":"before", "checkpoint_after":"after", "checkpoint_diff_text":"patch"
        }))
        .unwrap()
    }

    #[test]
    fn change_result_requires_complete_checkpoint_evidence() {
        for mask in 0..8 {
            let mut exchange = exchange();
            if mask & 1 == 0 {
                exchange.checkpoint_before = None;
            }
            if mask & 2 == 0 {
                exchange.checkpoint_after = None;
            }
            if mask & 4 == 0 {
                exchange.checkpoint_diff_text = None;
            }
            assert_eq!(
                matches!(exchange.change_result(), ExchangeChangeResult::Captured(_)),
                mask == 7
            );
        }
    }

    #[test]
    fn change_result_preserves_previous_capture_during_pending_and_failure() {
        for state in [
            ExchangeState::Queued,
            ExchangeState::Running,
            ExchangeState::Finalizing,
            ExchangeState::Complete,
        ] {
            let mut exchange = exchange();
            exchange.phase = super::super::ExchangePhase::fixture(state, None, None);
            assert!(
                matches!(exchange.change_result(), ExchangeChangeResult::Pending { previous: Some(comparison) } if comparison.text() == "patch")
            );
            exchange.finalization_error = Some("capture failed".into());
            assert!(
                matches!(exchange.change_result(), ExchangeChangeResult::Unavailable { previous: Some(comparison), reason: "capture failed" } if comparison.text() == "patch")
            );
            exchange.checkpoint_after = None;
            assert!(matches!(
                exchange.change_result(),
                ExchangeChangeResult::Unavailable { previous: None, .. }
            ));
        }
    }

    #[test]
    fn change_result_accepts_empty_committed_comparison() {
        for state in [
            ExchangeState::Complete,
            ExchangeState::Failed,
            ExchangeState::Cancelled,
            ExchangeState::Interrupted,
        ] {
            let mut exchange = exchange();
            exchange.phase = super::super::ExchangePhase::fixture(state, Some(10), None);
            exchange.checkpoint_diff_text = Some(String::new());
            assert!(
                matches!(exchange.change_result(), ExchangeChangeResult::Captured(comparison) if comparison.text().is_empty())
            );
            let reopened: Exchange =
                serde_json::from_value(serde_json::to_value(&exchange).unwrap()).unwrap();
            assert_eq!(exchange.change_result(), reopened.change_result());
        }
    }
}
