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

pub(crate) struct ExecutionAdmission(Arc<Semaphore>);

impl ExecutionAdmission {
    fn new() -> Self {
        Self(Arc::new(Semaphore::new(1)))
    }

    pub(crate) async fn acquire(&self) -> Result<ExecutionPermit, tokio::sync::AcquireError> {
        self.0
            .clone()
            .acquire_owned()
            .await
            .map(|permit| ExecutionPermit { _permit: permit })
    }

    pub(crate) async fn acquire_owned(
        self: Arc<Self>,
    ) -> Result<ExecutionPermit, tokio::sync::AcquireError> {
        self.acquire().await
    }

    pub(crate) fn try_acquire(&self) -> Result<ExecutionPermit, TryAcquireError> {
        self.0
            .clone()
            .try_acquire_owned()
            .map(|permit| ExecutionPermit { _permit: permit })
    }

    pub(crate) fn try_acquire_owned(self: Arc<Self>) -> Result<ExecutionPermit, TryAcquireError> {
        self.try_acquire()
    }

    pub(crate) fn available_permits(&self) -> usize {
        self.0.available_permits()
    }
    pub(crate) fn close(&self) {
        self.0.close();
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
            if !existing
                .response
                .as_ref()
                .and_then(Response::error)
                .is_some_and(|error| error.code == "not_admitted")
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
    use serde_json::json;

    #[tokio::test]
    async fn command_receipts_isolate_attempts_and_sessions() -> Result<()> {
        let first = SessionCoordinator::new(Arc::new(Default::default()));
        let second = SessionCoordinator::new(Arc::new(Default::default()));
        let identity = CommandIdentity {
            id: "command".into(),
            runtime_epoch: None,
        };
        let CommandAdmission::Execute(original) = first.admit(&identity, "test", &json!({}))?
        else {
            panic!()
        };
        assert!(matches!(
            first.admit(&identity, "test", &json!({}))?,
            CommandAdmission::Pending
        ));
        assert!(first.admit(&identity, "different", &json!({})).is_err());
        first.complete(
            &identity.id,
            original.attempt,
            &Response::failure(1, "not_admitted", "retry"),
        )?;
        let CommandAdmission::Execute(retry) = first.admit(&identity, "test", &json!({}))? else {
            panic!()
        };
        drop(original);
        assert_eq!(first.status(&identity.id)?["state"], "running");
        first.complete(
            &identity.id,
            retry.attempt,
            &Response::success(2, json!({"committed":true}))?,
        )?;
        drop(retry);
        assert!(matches!(
            first.admit(&identity, "test", &json!({}))?,
            CommandAdmission::Recorded(_)
        ));
        let permit = first.execution.acquire().await?;
        assert!(first.execution.try_acquire().is_err());
        assert!(second.execution.try_acquire().is_ok());
        drop(permit);
        assert!(first.execution.try_acquire().is_ok());
        Ok(())
    }
}
