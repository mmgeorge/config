use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use std::sync::Mutex;

#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
/// Selects the automatic validation boundary for queued source work.
pub enum CheckTiming {
    /// Validates after all currently queued work finishes.
    #[default]
    AfterQueue,
    /// Validates each completed work item before admitting the next.
    BetweenWork,
}

#[derive(Default)]
/// Shares editor queue admission with the completion gate without the broker lock.
pub(crate) struct CompletionQueue {
    state: Mutex<QueueState>,
}

#[derive(Default)]
struct QueueState {
    revision: u64,
    pending: bool,
    deferred: bool,
    owner: Option<String>,
}

impl CompletionQueue {
    /// Adopts a newer editor queue snapshot and ignores reordered publications.
    pub(crate) fn update(&self, revision: u64, pending: bool) -> Result<()> {
        let mut state = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("completion queue lock poisoned"))?;
        if revision > state.revision {
            state.revision = revision;
            state.pending = pending;
        }
        Ok(())
    }

    /// Retains the completion obligation while later queued work can change its inputs.
    pub(crate) fn defer(&self, timing: CheckTiming, owner: &str) -> Result<bool> {
        let mut state = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("completion queue lock poisoned"))?;
        if timing == CheckTiming::AfterQueue && state.pending {
            state.deferred = true;
            state.owner.get_or_insert_with(|| owner.to_owned());
            return Ok(true);
        }
        state.deferred = false;
        Ok(false)
    }

    /// Gets the outstanding queue completion obligation for the editor snapshot.
    pub(crate) fn deferred(&self) -> Result<bool> {
        Ok(self
            .state
            .lock()
            .ok()
            .context("completion queue lock poisoned")?
            .deferred)
    }

    /// Gets the first queued change owner until the batch completes validation.
    pub(crate) fn owner(&self) -> Result<Option<String>> {
        Ok(self
            .state
            .lock()
            .ok()
            .context("completion queue lock poisoned")?
            .owner
            .clone())
    }

    /// Releases a validated batch without dropping subsequently queued work.
    pub(crate) fn finish(&self) -> Result<()> {
        let mut state = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("completion queue lock poisoned"))?;
        state.deferred = false;
        state.owner = None;
        Ok(())
    }
}
