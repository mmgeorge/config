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
    /// Reject unapplied logical intent while retaining evidence of committed filesystem changes.
    pub(crate) fn reject(self, error: ProviderError) {
        if self.batch.completion.host_file_changes.is_empty() {
            self.batch.reject();
        }
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
            // Failed transport cannot undo a physical mutation already retained in this batch.
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

    /// Retain committed mutation evidence after lost delivery or rejection without replaying the write.
    #[tokio::test]
    async fn lost_live_delivery_preserves_committed_file_change()
    -> Result<(), Box<dyn std::error::Error>> {
        for owner_rejects in [false, true] {
            let directory = tempfile::tempdir()?;
            let path = directory.path().join("changed.rs");
            std::fs::write(&path, "old")?;
            std::fs::write(&path, "new")?;
            let scope =
                InvocationScope::new(crate::provider::ThreadOwner::root("session"), "exchange");
            let call = ToolCallId::new("committed-rename")?;
            let change = crate::tools::lsp::HostFileChange {
                scope: scope.clone(),
                call_id: call.clone(),
                native_item_id: None,
                changes: forge_provider_api::ProviderChangeSet {
                    file: vec![forge_provider_api::ProviderFileChange {
                        path: path.to_string_lossy().into_owned(),
                        move_path: None,
                        kind: forge_provider_api::ProviderChangeKind::Update,
                        diff: "@@ -1 +1 @@\n-old\n+new\n".into(),
                    }],
                },
            };
            let slot = super::super::CompletionSlot::default();
            let mut completion = super::super::ProviderCompletion::default();
            completion.host_file_changes.push(change.clone());
            let batch = slot.publish_batch(completion)?;
            let (sender, mut requests) = LiveHostEffectSender::channel();
            if owner_rejects {
                let sent = sender.commit(scope, call, batch);
                tokio::pin!(sent);
                assert!(futures_util::poll!(&mut sent).is_pending());
                requests
                    .recv()
                    .await
                    .ok_or("live request missing")?
                    .reject(ProviderError::Cancelled);
                assert!(sent.await.is_err());
            } else {
                drop(requests);
                assert!(sender.commit(scope, call, batch).await.is_err());
            }
            let preview = slot.preview()?;
            assert!(!preview.batches[0].is_rejected());
            assert_eq!(
                preview
                    .completion
                    .ok_or("physical evidence lost")?
                    .host_file_changes,
                vec![change]
            );
            assert_eq!(std::fs::read_to_string(path)?, "new");
        }
        Ok(())
    }

    /// A live callback retains its exact effect identity and waits for the owner's commit decision.
    #[tokio::test]
    async fn live_effect_reply_requires_correlated_owner_commit()
    -> Result<(), Box<dyn std::error::Error>> {
        let (sender, mut requests) = LiveHostEffectSender::channel();
        let scope = InvocationScope::new(crate::provider::ThreadOwner::root("session"), "exchange");
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
