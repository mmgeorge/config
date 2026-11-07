//! Session command ownership, idempotent receipts, and execution capabilities.

use std::{
    collections::HashMap,
    sync::{Arc, Mutex},
};

use anyhow::{Context, Result, ensure};
use forge_protocol::message::Response;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use tokio::sync::{OwnedSemaphorePermit, Semaphore, TryAcquireError};

/// Proves that a caller owns this session's provider execution slot.
#[derive(Debug)]
pub struct ExecutionPermit {
    _permit: OwnedSemaphorePermit,
}

/// Describe whether execution admission is available to new owners.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum ExecutionFence {
    /// Owners may acquire the execution slot.
    Open,
    /// Native collection has not proven quiescence; admission resumes when collection completes.
    CollectionPending,
    /// An unrecoverable failure permanently closed admission for this session.
    Fatal,
}

/// Report that execution admission was permanently closed.
#[derive(Debug, thiserror::Error)]
#[error("execution admission is permanently closed")]
pub(crate) struct ExecutionClosed;

/// Grant the single execution slot, withholding it while collection is pending or after fatal failure.
pub(crate) struct ExecutionAdmission {
    permits: Arc<Semaphore>,
    fence: tokio::sync::watch::Sender<ExecutionFence>,
}

impl ExecutionAdmission {
    fn new() -> Self {
        Self {
            permits: Arc::new(Semaphore::new(1)),
            fence: tokio::sync::watch::channel(ExecutionFence::Open).0,
        }
    }

    /// Wait for the slot, suspending while collection is pending and failing once fatally closed.
    pub(crate) async fn acquire(&self) -> Result<ExecutionPermit, ExecutionClosed> {
        let mut fence = self.fence.subscribe();
        loop {
            let current = *fence.borrow_and_update();
            match current {
                ExecutionFence::Fatal => return Err(ExecutionClosed),
                ExecutionFence::Open => {
                    let permit = tokio::select! {
                        biased;
                        changed = fence.changed() => {
                            changed.map_err(|_| ExecutionClosed)?;
                            continue;
                        }
                        permit = self.permits.clone().acquire_owned() => {
                            permit.map_err(|_| ExecutionClosed)?
                        }
                    };
                    // Linearize the grant against fence mutation after the asynchronous wait.
                    let current = self.fence.borrow();
                    match *current {
                        ExecutionFence::Open => return Ok(ExecutionPermit { _permit: permit }),
                        ExecutionFence::Fatal => return Err(ExecutionClosed),
                        ExecutionFence::CollectionPending => continue,
                    }
                }
                ExecutionFence::CollectionPending => {}
            }
            if fence.changed().await.is_err() {
                return Err(ExecutionClosed);
            }
        }
    }

    pub(crate) async fn acquire_owned(self: Arc<Self>) -> Result<ExecutionPermit, ExecutionClosed> {
        self.acquire().await
    }

    pub(crate) fn try_acquire(&self) -> Result<ExecutionPermit, TryAcquireError> {
        let fence = self.fence.borrow();
        if *fence != ExecutionFence::Open {
            return Err(TryAcquireError::Closed);
        }
        self.permits
            .clone()
            .try_acquire_owned()
            .map(|permit| ExecutionPermit { _permit: permit })
    }

    pub(crate) fn try_acquire_owned(self: Arc<Self>) -> Result<ExecutionPermit, TryAcquireError> {
        self.try_acquire()
    }

    /// Get execution availability after applying the collection and fatal-admission fences.
    pub(crate) fn available_permits(&self) -> usize {
        if *self.fence.borrow() == ExecutionFence::Open {
            self.permits.available_permits()
        } else {
            0
        }
    }

    /// Get whether an unrecoverable failure permanently closed this session's execution admission.
    pub(crate) fn is_closed(&self) -> bool {
        *self.fence.borrow() == ExecutionFence::Fatal
    }

    /// Withhold admission until native collection completes; a fatal closure is never downgraded.
    pub(crate) fn fence_collection_pending(&self) {
        self.fence.send_if_modified(|fence| {
            let changed = *fence == ExecutionFence::Open;
            if changed {
                *fence = ExecutionFence::CollectionPending;
            }
            changed
        });
    }

    /// Resume admission after collection proved quiescence, unless a fatal failure closed it.
    pub(crate) fn lift_collection_pending(&self) {
        self.fence.send_if_modified(|fence| {
            let changed = *fence == ExecutionFence::CollectionPending;
            if changed {
                *fence = ExecutionFence::Open;
            }
            changed
        });
    }

    /// Permanently close admission after an unrecoverable failure.
    pub(crate) fn close(&self) {
        self.fence.send_replace(ExecutionFence::Fatal);
        self.permits.close();
    }
}

/// Identifies one immutable client intent within a host lifetime.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct CommandIdentity {
    pub id: String,
    pub runtime_epoch: Option<String>,
}

struct CommandRecord {
    attempt: uuid::Uuid,
    method: String,
    params: Value,
    response: Option<Response>,
    running: bool,
}

/// Coordinates one session independently of every other session in the host.
pub(crate) struct SessionCoordinator {
    pub(crate) execution: Arc<ExecutionAdmission>,
    pub(crate) control: Arc<Semaphore>,
    pub(crate) submissions: Arc<crate::input::SessionInputQueue>,
    record: Mutex<HashMap<String, CommandRecord>>,
}

pub(crate) enum CommandAdmission {
    Execute(CommandLease),
    Recorded(Response),
    Pending,
}

pub(crate) struct CommandLease {
    pub(crate) attempt: uuid::Uuid,
    owner: Arc<SessionCoordinator>,
    id: String,
}

impl SessionCoordinator {
    pub(crate) fn new(submissions: Arc<crate::input::SessionInputQueue>) -> Arc<Self> {
        Arc::new(Self {
            execution: Arc::new(ExecutionAdmission::new()),
            control: Arc::new(Semaphore::new(1)),
            submissions,
            record: Mutex::new(HashMap::new()),
        })
    }

    pub(crate) fn admit(
        self: &Arc<Self>,
        identity: &CommandIdentity,
        method: &str,
        params: &Value,
    ) -> Result<CommandAdmission> {
        ensure!(!identity.id.is_empty(), "command identity is empty");
        let mut record = self
            .record
            .lock()
            .map_err(|_| anyhow::anyhow!("command receipt lock poisoned"))?;
        if let Some(existing) = record.get(&identity.id) {
            ensure!(
                existing.method == method && existing.params == *params,
                "command identity was reused for a different intent"
            );
            if existing
                .response
                .as_ref()
                .and_then(Response::error)
                .is_none_or(|error| error.code != "not_admitted")
            {
                return Ok(match &existing.response {
                    Some(response) => CommandAdmission::Recorded(response.clone()),
                    None => CommandAdmission::Pending,
                });
            }
        }
        let attempt = uuid::Uuid::new_v4();
        record.insert(
            identity.id.clone(),
            CommandRecord {
                attempt,
                method: method.into(),
                params: params.clone(),
                response: None,
                running: true,
            },
        );
        Ok(CommandAdmission::Execute(CommandLease {
            attempt,
            owner: self.clone(),
            id: identity.id.clone(),
        }))
    }

    pub(crate) fn complete(
        &self,
        id: &str,
        attempt: uuid::Uuid,
        response: &Response,
    ) -> Result<()> {
        let mut record = self
            .record
            .lock()
            .map_err(|_| anyhow::anyhow!("command receipt lock poisoned"))?;
        let entry = record.get_mut(id).context("command receipt is missing")?;
        if entry.attempt != attempt {
            return Ok(());
        }
        if entry.response.is_none() {
            entry.response = Some(response.clone());
        }
        Ok(())
    }

    pub(crate) fn status(&self, id: &str) -> Result<Value> {
        let record = self
            .record
            .lock()
            .map_err(|_| anyhow::anyhow!("command receipt lock poisoned"))?;
        Ok(match record.get(id) {
            None => serde_json::json!({"state":"not_admitted"}),
            Some(entry) => serde_json::json!({
                "state": if entry.response.is_some() { "completed" } else if entry.running { "running" } else { "outcome_unknown" },
                "response": entry.response,
            }),
        })
    }
}

impl Drop for CommandLease {
    fn drop(&mut self) {
        if let Ok(mut record) = self.owner.record.lock()
            && let Some(entry) = record.get_mut(&self.id)
            && entry.attempt == self.attempt
        {
            entry.running = false;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Keep admission blocked after a terminal worker releases its permit but cleanup is still owned.
    #[tokio::test]
    async fn collection_fence_outlives_native_execution_permit() -> Result<()> {
        let coordinator = SessionCoordinator::new(Arc::new(Default::default()));
        let permit = coordinator.execution.acquire().await?;
        coordinator.execution.fence_collection_pending();
        drop(permit);
        assert_eq!(coordinator.execution.available_permits(), 0);
        assert!(coordinator.execution.try_acquire().is_err());
        let pending = coordinator.execution.acquire();
        tokio::pin!(pending);
        assert!(futures_util::poll!(&mut pending).is_pending());
        coordinator.execution.lift_collection_pending();
        let permit = pending.await?;
        drop(permit);
        assert_eq!(coordinator.execution.available_permits(), 1);
        Ok(())
    }

    /// Recheck a waiter admitted before fencing when the original execution releases its slot.
    #[tokio::test]
    async fn collection_fence_blocks_an_already_waiting_acquisition() -> Result<()> {
        let coordinator = SessionCoordinator::new(Arc::new(Default::default()));
        let original = coordinator.execution.acquire().await?;
        let pending = coordinator.execution.acquire();
        tokio::pin!(pending);
        assert!(futures_util::poll!(&mut pending).is_pending());
        coordinator.execution.fence_collection_pending();
        drop(original);
        assert!(futures_util::poll!(&mut pending).is_pending());
        assert_eq!(coordinator.execution.available_permits(), 0);
        coordinator.execution.lift_collection_pending();
        drop(pending.await?);
        assert_eq!(coordinator.execution.available_permits(), 1);
        Ok(())
    }
}
