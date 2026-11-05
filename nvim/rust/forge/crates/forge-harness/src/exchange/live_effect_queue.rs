//! Stage callback requests until the owner has applied command admission for this update.
use std::collections::VecDeque;

pub(super) enum LiveEffectAdmission {
    Open,
    Closed,
}

#[derive(Default)]
pub(super) struct LiveEffectQueue {
    pending: VecDeque<Box<crate::provider::LiveHostEffect>>,
}

impl LiveEffectQueue {
    pub(super) fn stage(&mut self, request: Box<crate::provider::LiveHostEffect>) {
        self.pending.push_back(request);
    }

    pub(super) fn take_authorized(
        &mut self,
        admission: LiveEffectAdmission,
    ) -> Option<Box<crate::provider::LiveHostEffect>> {
        match admission {
            LiveEffectAdmission::Open => self.pending.pop_front(),
            LiveEffectAdmission::Closed => {
                self.reject_pending();
                None
            }
        }
    }

    /// Resolve unlaunched callbacks without executing their logical mutation.
    pub(super) fn reject_pending(&mut self) {
        for request in self.pending.drain(..) {
            request.reject(crate::provider::ProviderError::Cancelled);
        }
    }
}

#[cfg(test)]
#[path = "live_effect_queue_test.rs"]
mod tests;
