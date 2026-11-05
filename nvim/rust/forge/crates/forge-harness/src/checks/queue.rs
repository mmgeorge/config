use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use std::sync::{Arc, Mutex};

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

/// Retain a check obligation while reading pending input from its canonical queue.
pub(crate) struct CompletionQueue {
    input: Arc<crate::input::SessionInputQueue>,
    state: Mutex<CompletionObligation>,
}

#[derive(Default)]
enum CompletionObligation {
    #[default]
    None,
    Deferred(String),
    Ready(String),
}

impl CompletionQueue {
    /// Read the same queue used by input admission; no mirrored pending flag is retained.
    pub(crate) fn new(input: Arc<crate::input::SessionInputQueue>) -> Self {
        Self {
            input,
            state: Mutex::new(CompletionObligation::None),
        }
    }

    /// Retains the completion obligation while later queued work can change its inputs.
    pub(crate) fn defer(&self, timing: CheckTiming, owner: &str) -> Result<bool> {
        let pending = self.input.has_pending()?.1;
        let mut state = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("completion queue lock poisoned"))?;
        if timing == CheckTiming::AfterQueue && pending {
            let owner = match &*state {
                CompletionObligation::None => owner.to_owned(),
                CompletionObligation::Deferred(owner) | CompletionObligation::Ready(owner) => {
                    owner.clone()
                }
            };
            *state = CompletionObligation::Deferred(owner);
            return Ok(true);
        }
        if let CompletionObligation::Deferred(owner) = &*state {
            *state = CompletionObligation::Ready(owner.clone());
        }
        Ok(false)
    }

    /// Gets the outstanding queue completion obligation for the editor snapshot.
    pub(crate) fn deferred(&self) -> Result<bool> {
        Ok(matches!(
            &*self
                .state
                .lock()
                .ok()
                .context("completion queue lock poisoned")?,
            CompletionObligation::Deferred(_)
        ))
    }

    /// Gets the first queued change owner until the batch completes validation.
    pub(crate) fn owner(&self) -> Result<Option<String>> {
        let state = self
            .state
            .lock()
            .ok()
            .context("completion queue lock poisoned")?;
        Ok(match &*state {
            CompletionObligation::None => None,
            CompletionObligation::Deferred(owner) | CompletionObligation::Ready(owner) => {
                Some(owner.clone())
            }
        })
    }

    /// Releases a validated batch without dropping subsequently queued work.
    pub(crate) fn finish(&self) -> Result<()> {
        let mut state = self
            .state
            .lock()
            .map_err(|_| anyhow::anyhow!("completion queue lock poisoned"))?;
        *state = CompletionObligation::None;
        Ok(())
    }
}
