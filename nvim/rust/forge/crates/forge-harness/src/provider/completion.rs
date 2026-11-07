//! Retain host workflow effects independently of provider-owned lifecycle evidence.

use std::sync::{
    Arc, Mutex,
    atomic::{AtomicBool, Ordering},
};

use super::{ProviderCompletion, ProviderError};

#[derive(Default, Debug)]
pub(crate) struct CompletionSlot {
    state: Mutex<CompletionState>,
}

#[derive(Default, Debug)]
struct CompletionState {
    batches: Vec<HostEffectBatch>,
    file_changes: Vec<crate::tools::lsp::HostFileChange>,
    revision: u64,
    descendant_work_admitted: bool,
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
        let mut file_changes = state.file_changes.clone();
        for change in &completion.host_file_changes {
            if let Some(previous) = file_changes.iter().find(|previous| {
                previous.scope == change.scope && previous.call_id == change.call_id
            }) {
                if previous != change {
                    return Err(ProviderError::Failure(
                        forge_provider_api::ProviderFailure::new(
                            forge_provider_api::ProviderOperation::TurnStart,
                            forge_provider_api::ProviderFailureKind::Protocol,
                        ),
                    ));
                }
            } else {
                file_changes.push(change.clone());
            }
        }
        let revision = state
            .revision
            .checked_add(1)
            .ok_or_else(publication_failure)?;
        state
            .batches
            .push(HostEffectBatch::new(revision, completion));
        state.file_changes = file_changes;
        state.revision = revision;
        Ok(state
            .batches
            .last()
            .expect("published batch was appended under the same lock")
            .clone())
    }

    /// Snapshot original committed physical facts after collection, independently of runnable batch draining.
    pub(crate) fn collected_file_changes(&self) -> Vec<crate::tools::lsp::HostFileChange> {
        // A poisoned effect queue cannot revoke physical writes already retained before publication.
        self.state
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .file_changes
            .clone()
    }

    /// Preserve replay ambiguity once trusted independently owned descendant work is admitted.
    pub(crate) fn record_descendant_work(&self) -> Result<(), ProviderError> {
        let mut state = self.state.lock().map_err(|_| publication_failure())?;
        if state.descendant_work_admitted {
            return Ok(());
        }
        let revision = state
            .revision
            .checked_add(1)
            .ok_or_else(publication_failure)?;
        state.descendant_work_admitted = true;
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
        state.revision = revision;
        Ok(())
    }

    /// Inspect the effect revision that the host is about to settle.
    pub(crate) fn preview(&self) -> Result<CompletionPreview, ProviderError> {
        let state = self.state.lock().map_err(|_| publication_failure())?;
        let mut completion = HostEffectBatch::active_completion(&state.batches);
        if state.descendant_work_admitted {
            completion
                .get_or_insert_with(ProviderCompletion::default)
                .descendant_work_admitted = true;
        }
        Ok(CompletionPreview {
            revision: state.revision,
            completion,
            batches: state.batches.clone(),
        })
    }

    /// Commit only the exact effect revision previously inspected by the host.
    pub(crate) fn take_revision(&self, revision: u64) -> Result<bool, ProviderError> {
        let mut state = self.state.lock().map_err(|_| publication_failure())?;
        if state.revision != revision {
            return Ok(false);
        }
        state.batches.clear();
        Ok(true)
    }
}

fn publication_failure() -> ProviderError {
    ProviderError::Failure(forge_provider_api::ProviderFailure::new(
        forge_provider_api::ProviderOperation::TurnStart,
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
        assert!(slot.take_revision(captured.revision)?);
        assert!(slot.preview()?.completion.is_none());
        Ok(())
    }
}
