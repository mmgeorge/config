//! Retain a scoped host-effect commit request independently of native terminal evidence.

use super::{HostEffectBatch, InvocationScope, ProviderError};
use forge_provider_api::control::ToolCallId;
use tokio::sync::{mpsc, oneshot};

/// Submit one already-published callback batch to its foreground exchange owner.
pub(crate) struct LiveHostEffect {
    pub(crate) scope: InvocationScope,
    pub(crate) call_id: ToolCallId,
    pub(crate) batch: HostEffectBatch,
    pub(crate) reply: oneshot::Sender<Result<LiveHostCommit, ProviderError>>,
}

impl LiveHostEffect {
    pub(crate) fn reject(self, error: ProviderError) {
        self.batch.reject();
        let _ = self.reply.send(Err(error));
    }
}

/// Carry only task authority explicitly advanced by the same live owner commit.
pub(crate) struct LiveHostCommit {
    pub(crate) task_permit: Option<crate::task::TaskPermit>,
}

/// Apply bounded backpressure while the owner commits a callback's logical effects.
#[derive(Clone)]
pub(crate) struct LiveHostEffectSender {
    sender: mpsc::Sender<LiveHostEffect>,
}

impl LiveHostEffectSender {
    /// Create one foreground invocation's host-effect route before native dispatch.
    pub(crate) fn channel() -> (Self, mpsc::Receiver<LiveHostEffect>) {
        let (sender, receiver) = mpsc::channel(32);
        (Self { sender }, receiver)
    }

    /// Await the exact logical commit without waiting for provider acceptance or terminal completion.
    pub(crate) async fn commit(
        &self,
        scope: InvocationScope,
        call_id: ToolCallId,
        batch: HostEffectBatch,
    ) -> Result<LiveHostCommit, ProviderError> {
        let (reply, response) = oneshot::channel();
        if let Err(failed) = self
            .sender
            .send(LiveHostEffect {
                scope,
                call_id,
                batch,
                reply,
            })
            .await
        {
            // Nothing was admitted, so terminal settlement must never replay this batch.
            failed.0.reject(ProviderError::Cancelled);
            return Err(ProviderError::Cancelled);
        }
        response
            .await
            .map_err(|_| ProviderError::DeliveryUncertain)?
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn closed_owner_channel_rejects_batch_in_retained_terminal_snapshot()
    -> Result<(), Box<dyn std::error::Error>> {
        let (sender, requests) = LiveHostEffectSender::channel();
        drop(requests);
        let scope = InvocationScope::new(
            forge_provider_api::ExecutionOwner::root("session"),
            "exchange",
        );
        let batch = HostEffectBatch::new(7, Default::default());
        let snapshot = vec![batch.clone()];
        let result = sender
            .commit(scope, ToolCallId::new("closed-owner")?, batch)
            .await;
        assert!(matches!(result, Err(ProviderError::Cancelled)));
        assert!(snapshot[0].is_rejected());
        assert!(HostEffectBatch::active_completion(&snapshot).is_none());
        Ok(())
    }

    /// A live callback retains its exact effect identity and waits for the owner's commit decision.
    #[tokio::test]
    async fn live_effect_reply_requires_correlated_owner_commit()
    -> Result<(), Box<dyn std::error::Error>> {
        let (sender, mut requests) = LiveHostEffectSender::channel();
        let scope = InvocationScope::new(
            forge_provider_api::ExecutionOwner::root("session"),
            "exchange",
        );
        let call = ToolCallId::new("call")?;
        let batch = HostEffectBatch::new(7, Default::default());
        let sent = sender.commit(scope.clone(), call.clone(), batch);
        tokio::pin!(sent);
        assert!(futures_util::poll!(&mut sent).is_pending());
        let request = requests.recv().await.ok_or("request channel closed")?;
        assert_eq!(request.scope, scope);
        assert_eq!(request.call_id, call);
        assert_eq!(request.batch.revision, 7);
        assert!(futures_util::poll!(&mut sent).is_pending());
        request
            .reply
            .send(Ok(LiveHostCommit { task_permit: None }))
            .map_err(|_| "reply receiver closed")?;
        sent.await?;
        Ok(())
    }
}
