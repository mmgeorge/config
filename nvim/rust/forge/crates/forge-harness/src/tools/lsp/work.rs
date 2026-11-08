//! Retain each invocation's blocking mutations independently of their result waiters.
use super::mutation_error::{MutationError, Result};
use std::sync::{Arc, Mutex};
use tokio::sync::{Notify, oneshot};

/// Own mutation admission, worker collection, and any unsafe retained failure.
#[derive(Default)]
pub(crate) struct MutationWork {
    state: Mutex<State>,
    changed: Notify,
}

#[derive(Default)]
struct State {
    closed: bool,
    active: usize,
    failure: Option<Arc<MutationError>>,
}

impl MutationWork {
    /// Get whether collection retained an unsafe or undelivered mutation failure.
    pub(crate) fn failed(&self) -> bool {
        self.state
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .failure
            .is_some()
    }

    /// Retain the blocking join and its failure even if the invoking tool is cancelled.
    pub(super) async fn run<T: Clone + Send + 'static>(
        self: &Arc<Self>,
        work: impl FnOnce() -> Result<T> + Send + 'static,
    ) -> Result<T> {
        {
            let mut state = self.state.lock().map_err(|_| MutationError::Poisoned)?;
            if state.closed {
                return Err(MutationError::Access("Mutation admission is closed."));
            }
            state.active += 1;
        }
        let (sender, receiver) = oneshot::channel();
        let owner = self.clone();
        let worker = tokio::task::spawn_blocking(work);
        tokio::spawn(async move {
            let result = worker
                .await
                .unwrap_or_else(|error| Err(MutationError::Worker(error)));
            let result = result.map_err(Arc::new);
            let uncertain = result
                .as_ref()
                .err()
                .filter(|error| {
                    matches!(
                        error.as_ref(),
                        MutationError::Worker(_)
                            | MutationError::Rollback { .. }
                            | MutationError::Publication(_)
                            | MutationError::Poisoned
                    )
                })
                .cloned();
            let lost = sender.send(result).err().and_then(|result| result.err());
            {
                let mut state = owner
                    .state
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner);
                state.failure = state.failure.take().or(uncertain).or(lost);
                state.active -= 1;
            }
            owner.changed.notify_waiters();
        });
        receiver
            .await
            .map_err(|_| MutationError::Access("Mutation result channel closed."))?
            .map_err(MutationError::Retained)
    }

    /// Close admission and wait for all original workers before exposing their retained failure.
    pub(crate) async fn finish(&self) -> Option<Arc<MutationError>> {
        loop {
            let changed = self.changed.notified();
            tokio::pin!(changed);
            changed.as_mut().enable();
            {
                let mut state = self
                    .state
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner);
                state.closed = true;
                if state.active == 0 {
                    return state.failure.clone();
                }
            }
            changed.await;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Dropping the response waiter cannot release collection ahead of a committed physical change.
    #[tokio::test]
    async fn cancelled_waiter_retains_physical_commit_and_original_archive() -> anyhow::Result<()> {
        let directory = tempfile::tempdir()?;
        let source = directory.path().join("before.txt");
        let destination = directory.path().join("after.txt");
        std::fs::write(&source, "retained")?;
        let owner = Arc::new(MutationWork::default());
        let archive = Arc::new(crate::provider::CompletionSlot::default());
        let scope = crate::provider::InvocationScope::new(
            crate::provider::ThreadOwner::root("session"),
            "original",
        );
        let change = crate::tools::lsp::HostFileChange {
            scope,
            call_id: forge_provider_api::control::ToolCallId::new("rename")?,
            native_item_id: None,
            changes: forge_provider_api::ProviderChangeSet {
                file: vec![forge_provider_api::ProviderFileChange {
                    path: "before.txt".into(),
                    move_path: Some("after.txt".into()),
                    kind: forge_provider_api::ProviderChangeKind::Move,
                    diff: String::new(),
                }],
            },
        };
        let (entered, started) = oneshot::channel();
        let (release, held) = std::sync::mpsc::channel();
        let worker_owner = owner.clone();
        let worker_archive = archive.clone();
        let expected = change.clone();
        let waiter = tokio::spawn(async move {
            worker_owner
                .run(move || {
                    std::fs::rename(source, destination)?;
                    entered.send(()).expect("test observes physical commit");
                    held.recv().expect("test releases worker");
                    worker_archive
                        .publish(crate::provider::ProviderCompletion {
                            host_file_changes: vec![change],
                            ..Default::default()
                        })
                        .map_err(MutationError::Publication)?;
                    Ok(())
                })
                .await
        });
        started.await?;
        waiter.abort();
        assert!(waiter.await.unwrap_err().is_cancelled());
        let collection = owner.finish();
        tokio::pin!(collection);
        assert!(futures_util::poll!(collection.as_mut()).is_pending());
        assert!(archive.collected_file_changes().is_empty());
        release.send(())?;
        assert!(collection.await.is_none());
        assert_eq!(
            std::fs::read_to_string(directory.path().join("after.txt"))?,
            "retained"
        );
        assert_eq!(archive.collected_file_changes(), vec![expected]);
        assert!(owner.finish().await.is_none());
        assert!(owner.run(|| Ok(())).await.is_err());
        Ok(())
    }
}
