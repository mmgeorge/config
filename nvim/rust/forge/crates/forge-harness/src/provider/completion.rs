//! Retain host workflow effects independently of provider-owned lifecycle evidence.

use std::sync::Mutex;

use forge_provider_api::lifecycle::ExecutionResult;

use super::{ProviderCompletion, ProviderError};

#[derive(Default, Debug)]
pub(crate) struct CompletionSlot {
    state: Mutex<CompletionState>,
}

#[derive(Default, Debug)]
struct CompletionState {
    completion: Option<ProviderCompletion>,
    host_outcome: Option<ExecutionResult>,
    batches: Vec<HostEffectBatch>,
    revision: u64,
}

/// Preserve one host effect batch and its stable publication revision.
#[derive(Clone, Debug)]
pub(crate) struct HostEffectBatch {
    pub(crate) revision: u64,
    pub(crate) completion: ProviderCompletion,
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
        let mut state = self.state.lock().map_err(|_| publication_failure())?;
        let revision = state
            .revision
            .checked_add(1)
            .ok_or_else(publication_failure)?;
        state.batches.push(HostEffectBatch {
            revision,
            completion: completion.clone(),
        });
        match state.completion.as_mut() {
            Some(previous) => previous.merge_execution(completion),
            None => state.completion = Some(completion),
        }
        state.revision = revision;
        Ok(())
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
            completion: state.completion.clone(),
            host_outcome: state.host_outcome.clone(),
            batches: state.batches.clone(),
        })
    }

    /// Consume effects and their host workflow outcome together.
    pub(crate) fn take_with_outcome(
        &self,
    ) -> Result<(Option<ProviderCompletion>, Option<ExecutionResult>), ProviderError> {
        let mut state = self.state.lock().map_err(|_| publication_failure())?;
        state.batches.clear();
        Ok((state.completion.take(), state.host_outcome.take()))
    }

    /// Commit only the exact effect revision previously inspected by the host.
    pub(crate) fn take_revision(&self, revision: u64) -> Result<bool, ProviderError> {
        let mut state = self.state.lock().map_err(|_| publication_failure())?;
        if state.revision != revision {
            return Ok(false);
        }
        state.completion.take();
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
