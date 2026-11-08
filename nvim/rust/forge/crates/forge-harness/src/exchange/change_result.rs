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
