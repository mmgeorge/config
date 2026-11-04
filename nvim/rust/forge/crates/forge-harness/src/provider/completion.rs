//! Retain host workflow effects independently of provider-owned lifecycle evidence.

use std::sync::{
    Arc, Mutex,
    atomic::{AtomicBool, Ordering},
};

use forge_provider_api::lifecycle::ExecutionResult;

use super::{ProviderCompletion, ProviderError};

#[derive(Default, Debug)]
pub(crate) struct CompletionSlot {
    state: Mutex<CompletionState>,
}

#[derive(Default, Debug)]
struct CompletionState {
    host_outcome: Option<ExecutionResult>,
    batches: Vec<HostEffectBatch>,
    revision: u64,
}

/// Preserve one host effect batch and its stable publication revision.
#[derive(Clone, Debug)]
pub(crate) struct HostEffectBatch {
    pub(crate) revision: u64,
    pub(crate) completion: ProviderCompletion,
    rejected: Arc<AtomicBool>,
}

impl HostEffectBatch {
    pub(crate) fn new(revision: u64, completion: ProviderCompletion) -> Self {
        Self {
            revision,
            completion,
            rejected: Arc::new(AtomicBool::new(false)),
        }
    }

    /// Retire an unlaunched effect in every retained native snapshot before releasing its waiter.
    pub(crate) fn reject(&self) {
        self.rejected.store(true, Ordering::Release);
    }

    pub(crate) fn is_rejected(&self) -> bool {
        self.rejected.load(Ordering::Acquire)
    }

    pub(crate) fn active_completion(batches: &[Self]) -> Option<ProviderCompletion> {
        let mut completion: Option<ProviderCompletion> = None;
        for batch in batches.iter().filter(|batch| !batch.is_rejected()) {
            match &mut completion {
                Some(previous) => previous.merge_execution(batch.completion.clone()),
                None => completion = Some(batch.completion.clone()),
            }
        }
        completion
    }
}

pub(crate) struct CompletionPreview {
    pub revision: u64,
    pub completion: Option<ProviderCompletion>,
    pub host_outcome: Option<ExecutionResult>,
    pub batches: Vec<HostEffectBatch>,
}

impl CompletionSlot {
    /// Publish host results atomically before acknowledging the callback that produced them.
    pub(crate) fn publish(&self, completion: ProviderCompletion) -> Result<(), ProviderError> {
        self.publish_batch(completion).map(drop)
    }

    /// Publish and return the exact immutable batch identity committed by a live callback.
    pub(crate) fn publish_batch(
        &self,
        completion: ProviderCompletion,
    ) -> Result<HostEffectBatch, ProviderError> {
        let mut state = self.state.lock().map_err(|_| publication_failure())?;
        let revision = state
            .revision
            .checked_add(1)
            .ok_or_else(publication_failure)?;
        state
            .batches
            .push(HostEffectBatch::new(revision, completion));
        state.revision = revision;
        Ok(state
            .batches
            .last()
            .expect("published batch was appended under the same lock")
            .clone())
    }

    /// Preserve a host workflow outcome without changing native terminal evidence.
    #[cfg(test)]
    pub(crate) fn publish_host_outcome(
        &self,
        outcome: ExecutionResult,
    ) -> Result<(), ProviderError> {
        let mut state = self.state.lock().map_err(|_| publication_failure())?;
        let revision = state
            .revision
            .checked_add(1)
            .ok_or_else(publication_failure)?;
        state.host_outcome = Some(outcome);
        state.revision = revision;
        Ok(())
    }

    /// Retain accumulated effects while starting another provider-authorized attempt.
    pub(crate) fn begin_attempt(&self) -> Result<(), ProviderError> {
        let mut state = self.state.lock().map_err(|_| publication_failure())?;
        let revision = state
            .revision
            .checked_add(1)
            .ok_or_else(publication_failure)?;
        state.host_outcome = None;
        state.revision = revision;
        Ok(())
    }

    /// Inspect the effect revision that the host is about to settle.
    pub(crate) fn preview(&self) -> Result<CompletionPreview, ProviderError> {
        let state = self.state.lock().map_err(|_| publication_failure())?;
        Ok(CompletionPreview {
            revision: state.revision,
            completion: HostEffectBatch::active_completion(&state.batches),
            host_outcome: state.host_outcome.clone(),
            batches: state.batches.clone(),
        })
    }

    /// Consume effects and their host workflow outcome together.
    pub(crate) fn take_with_outcome(
        &self,
    ) -> Result<(Option<ProviderCompletion>, Option<ExecutionResult>), ProviderError> {
        let mut state = self.state.lock().map_err(|_| publication_failure())?;
        let completion = HostEffectBatch::active_completion(&state.batches);
        state.batches.clear();
        Ok((completion, state.host_outcome.take()))
    }

    /// Commit only the exact effect revision previously inspected by the host.
    pub(crate) fn take_revision(&self, revision: u64) -> Result<bool, ProviderError> {
        let mut state = self.state.lock().map_err(|_| publication_failure())?;
        if state.revision != revision {
            return Ok(false);
        }
        state.host_outcome.take();
        state.batches.clear();
        Ok(true)
    }
}

fn publication_failure() -> ProviderError {
    ProviderError::Failure(forge_provider_api::ProviderFailure::new(
        forge_provider_api::ProviderOperation::HostEffectPublication,
        forge_provider_api::ProviderFailureKind::Runtime,
    ))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rejected_batch_disappears_from_captured_terminal_snapshot() -> Result<(), ProviderError> {
        let slot = CompletionSlot::default();
        let batch = slot.publish_batch(ProviderCompletion::default())?;
        let captured = slot.preview()?;
        assert!(HostEffectBatch::active_completion(&captured.batches).is_some());
        batch.reject();
        assert!(captured.batches[0].is_rejected());
        assert!(HostEffectBatch::active_completion(&captured.batches).is_none());
        assert!(slot.preview()?.completion.is_none());
        assert!(slot.take_with_outcome()?.0.is_none());
        Ok(())
    }
}
