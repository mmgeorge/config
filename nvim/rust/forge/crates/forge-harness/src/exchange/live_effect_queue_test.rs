use super::*;
use crate::provider::ThreadOwner;
use crate::provider::{HostEffectBatch, InvocationScope, LiveHostEffectSender, ProviderError};
use forge_provider_api::control::ToolCallId;

/// Closing admission resolves queued callbacks without handing any mutation to the owner.
#[tokio::test]
async fn closed_admission_rejects_queued_callback_without_commit()
-> Result<(), Box<dyn std::error::Error>> {
    let (sender, mut requests) = LiveHostEffectSender::channel();
    let scope = InvocationScope::new(ThreadOwner::root("session"), "exchange");
    let callback = sender.commit(
        scope,
        ToolCallId::new("queued-call")?,
        HostEffectBatch::new(7, Default::default()),
    );
    tokio::pin!(callback);
    assert!(futures_util::poll!(&mut callback).is_pending());
    let mut queue = LiveEffectQueue::default();
    queue.stage(Box::new(
        requests.recv().await.ok_or("callback not queued")?,
    ));
    assert!(queue.take_authorized(LiveEffectAdmission::Closed).is_none());
    assert!(matches!(callback.await, Err(ProviderError::Cancelled)));
    assert!(
        queue.take_authorized(LiveEffectAdmission::Open).is_none(),
        "reopening admission must not revive the cancelled mutation"
    );
    Ok(())
}

/// A close fences later work while the already handed-off callback retains its own settlement.
#[tokio::test]
async fn closing_admission_preserves_started_callback_settlement()
-> Result<(), Box<dyn std::error::Error>> {
    let (sender, mut requests) = LiveHostEffectSender::channel();
    let scope = InvocationScope::new(ThreadOwner::root("session"), "exchange");
    let first_id = ToolCallId::new("started-call")?;
    let first = sender.commit(
        scope.clone(),
        first_id.clone(),
        HostEffectBatch::new(8, Default::default()),
    );
    let second = sender.commit(
        scope.clone(),
        ToolCallId::new("queued-successor")?,
        HostEffectBatch::new(9, Default::default()),
    );
    tokio::pin!(first, second);
    let mut queue = LiveEffectQueue::default();
    assert!(futures_util::poll!(&mut first).is_pending());
    queue.stage(Box::new(
        requests.recv().await.ok_or("first callback missing")?,
    ));
    let started = queue
        .take_authorized(LiveEffectAdmission::Open)
        .ok_or("first callback not admitted")?;
    assert_eq!(started.scope, scope);
    assert_eq!(started.call_id, first_id);
    assert_eq!(started.batch.revision, 8);
    assert!(futures_util::poll!(&mut second).is_pending());
    queue.stage(Box::new(
        requests.recv().await.ok_or("successor callback missing")?,
    ));
    assert!(queue.take_authorized(LiveEffectAdmission::Closed).is_none());
    assert!(matches!(second.await, Err(ProviderError::Cancelled)));
    assert!(
        futures_util::poll!(&mut first).is_pending(),
        "closing admission cannot manufacture a result for already started work"
    );
    started
        .reply
        .send(Ok(crate::provider::LiveHostCommit { task_permit: None }))
        .map_err(|_| "started observer lost")?;
    first.await?;
    assert!(queue.take_authorized(LiveEffectAdmission::Open).is_none());
    Ok(())
}
