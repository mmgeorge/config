use std::collections::{HashMap, VecDeque};
use std::sync::{Arc, Mutex};

use anyhow::{Context, Result};
use forge_harness::protocol::RuntimeMethod;
use tokio::sync::Notify;

use super::RoutedRequestEnvelope;

#[derive(Clone, Copy, Eq, PartialEq)]
struct PreparationSequence(u64);

#[derive(Default)]
struct PreparationState {
    next_sequence: u64,
    queue_by_session: HashMap<String, VecDeque<PreparationSequence>>,
}

/// Orders mutation preparation without serializing request execution or unrelated reads.
#[derive(Default)]
pub(super) struct PreparationOrder {
    state: Mutex<PreparationState>,
    changed: Notify,
}

pub(super) struct PreparationTurn {
    owner: Arc<PreparationOrder>,
    session_id: String,
    sequence: PreparationSequence,
}

impl PreparationOrder {
    pub(super) fn reserve(
        self: &Arc<Self>,
        envelope: &RoutedRequestEnvelope,
    ) -> Result<Option<PreparationTurn>> {
        let Ok(method) = RuntimeMethod::decode(&envelope.request.method) else {
            return Ok(None);
        };
        if !method.requires_provider_fork()
            && !matches!(
                method,
                RuntimeMethod::SessionCommandApply | RuntimeMethod::SessionPermissionSet
            )
        {
            return Ok(None);
        }
        let Some(session_id) = &envelope.session_id else {
            return Ok(None);
        };
        let mut state = self
            .state
            .lock()
            .expect("request preparation order poisoned");
        state.next_sequence = state
            .next_sequence
            .checked_add(1)
            .context("request preparation sequence exhausted")?;
        let sequence = PreparationSequence(state.next_sequence);
        state
            .queue_by_session
            .entry(session_id.clone())
            .or_default()
            .push_back(sequence);
        Ok(Some(PreparationTurn {
            owner: Arc::clone(self),
            session_id: session_id.clone(),
            sequence,
        }))
    }
}

impl PreparationTurn {
    pub(super) async fn wait(&self) {
        loop {
            let changed = self.owner.changed.notified();
            tokio::pin!(changed);
            changed.as_mut().enable();
            let first = self
                .owner
                .state
                .lock()
                .expect("request preparation order poisoned")
                .queue_by_session
                .get(&self.session_id)
                .and_then(|queue| queue.front().copied());
            if first == Some(self.sequence) {
                return;
            }
            changed.await;
        }
    }
}

impl Drop for PreparationTurn {
    fn drop(&mut self) {
        let mut state = self
            .owner
            .state
            .lock()
            .expect("request preparation order poisoned");
        if let Some(queue) = state.queue_by_session.get_mut(&self.session_id) {
            queue.retain(|sequence| *sequence != self.sequence);
            if queue.is_empty() {
                state.queue_by_session.remove(&self.session_id);
            }
        }
        drop(state);
        self.owner.changed.notify_waiters();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;
    use std::time::Duration;

    fn request(session_id: &str, method: &str) -> RoutedRequestEnvelope {
        serde_json::from_value(json!({"id":1,"method":method,"params":{},"session_id":session_id}))
            .unwrap()
    }

    #[tokio::test]
    async fn configuration_order_survives_cancellation_without_blocking_other_sessions()
    -> Result<()> {
        let order = Arc::new(PreparationOrder::default());
        let first = order
            .reserve(&request("source", "session.command.apply"))?
            .unwrap();
        let middle = order
            .reserve(&request("source", "session.question.ask"))?
            .unwrap();
        let last = order
            .reserve(&request("source", "session.command.apply"))?
            .unwrap();
        let independent = order
            .reserve(&request("other", "session.command.apply"))?
            .unwrap();
        assert!(
            order
                .reserve(&request("source", "session.state.get"))?
                .is_none()
        );
        first.wait().await;
        independent.wait().await;
        drop(middle);
        assert!(
            tokio::time::timeout(Duration::from_millis(10), last.wait())
                .await
                .is_err()
        );
        drop(first);
        tokio::time::timeout(Duration::from_secs(1), last.wait()).await?;
        drop(last);
        drop(independent);
        assert!(order.state.lock().unwrap().queue_by_session.is_empty());
        Ok(())
    }
}
