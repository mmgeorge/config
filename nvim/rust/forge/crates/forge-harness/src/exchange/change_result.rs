use super::{Exchange, ExchangeState};

/// Borrows a saved comparison with both checkpoint identities present.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) struct CheckpointComparison<'exchange> {
    text: &'exchange str,
    pub session_id: &'exchange str,
    pub before: &'exchange str,
    pub after: &'exchange str,
}

impl<'exchange> CheckpointComparison<'exchange> {
    pub(crate) fn text(self) -> &'exchange str {
        self.text
    }
}

/// Classifies committed workspace evidence without reading the current workspace.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum ExchangeChangeResult<'exchange> {
    /// The exchange ended before its initial workspace checkpoint was captured.
    NotStarted,
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
            (Some(before), Some(after), Some(text)) => Some(CheckpointComparison {
                text,
                session_id: &self.session_id,
                before,
                after,
            }),
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
        if self.state() == ExchangeState::Cancelled
            && self.checkpoint_before.is_none()
            && self.checkpoint_after.is_none()
            && self.turn.is_empty()
        {
            return ExchangeChangeResult::NotStarted;
        }
        match (previous, &self.checkpoint_before, &self.checkpoint_after) {
            (Some(comparison), _, _) => ExchangeChangeResult::Captured(comparison),
            // A baseline obliges a closing checkpoint, which only a live owner captures: the
            // session's next owner retries the finalization of an exchange its killed owner left,
            // and the session preview shows that exchange settled before any owner has. A capture
            // that fails records `finalization_error`, handled above, so this one is still due.
            (None, Some(_), None) => ExchangeChangeResult::Pending { previous: None },
            (None, Some(_), Some(_)) => ExchangeChangeResult::Unavailable {
                previous: None,
                reason: "saved checkpoint comparison is missing",
            },
            (None, None, _) => ExchangeChangeResult::Unavailable {
                previous: None,
                reason: "checkpoint identities are missing",
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::exchange::ExchangeOutcome;

    /// A finished record with a baseline and no closing checkpoint is pending: only the
    /// session's next owner captures that checkpoint, and a capture that fails records its own
    /// error. A record that never held checkpoints keeps the general reason.
    #[test]
    fn finished_exchange_without_closing_checkpoint_is_pending() {
        let finished = |baseline: Option<&str>, error: Option<&str>| {
            let mut exchange = crate::storage::test::running_exchange("interrupted");
            exchange.checkpoint_before = baseline.map(str::to_owned);
            exchange.finish(ExchangeOutcome::Interrupted, 40).unwrap();
            exchange.finalization_error = error.map(str::to_owned);
            exchange
        };
        assert_eq!(
            finished(Some("baseline"), None).change_result(),
            ExchangeChangeResult::Pending { previous: None }
        );
        assert_eq!(
            finished(Some("baseline"), Some("capture failed")).change_result(),
            ExchangeChangeResult::Unavailable {
                previous: None,
                reason: "capture failed"
            }
        );
        assert_eq!(
            finished(None, None).change_result(),
            ExchangeChangeResult::Unavailable {
                previous: None,
                reason: "checkpoint identities are missing"
            }
        );
    }
}
