/// Explain why a checkpoint barrier did not release native execution.
#[derive(Clone, Copy, Debug, Eq, PartialEq, thiserror::Error)]
pub enum CheckpointGateError {
    #[error("checkpoint baseline was not persisted")]
    Persistence,
    #[error("execution ended before checkpoint admission completed")]
    OwnerEnded,
    #[error("checkpoint barrier exceeded 25 seconds")]
    TimedOut,
}

/// Share the durable baseline barrier with normalized provider callbacks.
#[derive(Clone)]
pub struct ToolGate {
    state: tokio::sync::watch::Sender<Option<std::result::Result<(), CheckpointGateError>>>,
}

impl ToolGate {
    /// Create a closed barrier that releases only after durable baseline persistence.
    pub(crate) fn new() -> Self {
        Self {
            state: tokio::sync::watch::channel(None).0,
        }
    }

    /// Publish the first terminal result atomically and ignore late replacement attempts.
    pub(crate) fn complete(&self, result: std::result::Result<(), CheckpointGateError>) {
        self.state.send_if_modified(|state| {
            if state.is_some() {
                return false;
            }
            *state = Some(result);
            true
        });
    }

    /// Report whether baseline persistence released the barrier.
    pub(crate) fn ready(&self) -> bool {
        matches!(self.state.borrow().as_ref(), Some(Ok(())))
    }

    /// Wait for baseline persistence without accepting a later result after failure.
    pub async fn wait(&self) -> std::result::Result<(), CheckpointGateError> {
        let mut state = self.state.subscribe();
        tokio::time::timeout(std::time::Duration::from_secs(25), async {
            loop {
                if let Some(result) = *state.borrow_and_update() {
                    return result;
                }
                state
                    .changed()
                    .await
                    .map_err(|_| CheckpointGateError::OwnerEnded)?;
            }
        })
        .await
        .map_err(|_| CheckpointGateError::TimedOut)?
    }
}

/// Retain and cancel asynchronous baseline acquisition with its exchange.
pub(crate) struct CheckpointTask(
    pub(crate)  tokio::task::JoinHandle<
        std::result::Result<super::CheckpointRecord, super::CheckpointFailure>,
    >,
);

impl Drop for CheckpointTask {
    fn drop(&mut self) {
        self.0.abort();
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[tokio::test]
    async fn tools_wait_for_persistence_and_failure_remains_denied() {
        let gate = ToolGate::new();
        let mut waiting = Box::pin(gate.wait());
        assert!(futures_util::poll!(&mut waiting).is_pending());
        gate.complete(Err(CheckpointGateError::Persistence));
        gate.complete(Ok(()));
        assert_eq!(waiting.await, Err(CheckpointGateError::Persistence));
        assert!(!gate.ready());
        let released = ToolGate::new();
        released.complete(Ok(()));
        released.complete(Err(CheckpointGateError::OwnerEnded));
        assert_eq!(released.wait().await, Ok(()));
        assert!(released.ready());
    }
}
