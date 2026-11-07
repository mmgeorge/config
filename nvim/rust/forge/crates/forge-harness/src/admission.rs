//! Release transport ordering when caller intent reaches the session owner.

use std::cell::RefCell;
use std::future::Future;

use tokio::sync::oneshot;

tokio::task_local! {
    static ADMITTED: RefCell<Option<oneshot::Sender<()>>>;
}

/// Observe owner admission without retaining transport ordering through command completion.
pub async fn observe<F: Future>(notification: Option<oneshot::Sender<()>>, work: F) -> F::Output {
    ADMITTED.scope(RefCell::new(notification), work).await
}

/// Transfer the notification when a caller command moves into a retained worker.
pub(crate) fn take_notification() -> Option<oneshot::Sender<()>> {
    ADMITTED
        .try_with(|notification| notification.borrow_mut().take())
        .ok()
        .flatten()
}

/// Release caller ordering after enqueue, or before routing a request without an owner command.
pub(crate) fn complete() {
    if let Some(notification) = take_notification() {
        let _ = notification.send(());
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Preserve one notification when a retained worker owns the eventual enqueue.
    #[allow(
        clippy::async_yields_async,
        reason = "Admission returns the retained worker without waiting for its completion"
    )]
    #[tokio::test]
    async fn admission_transfers_to_retained_worker() {
        let (admitted, admission) = oneshot::channel();
        let (enqueue, enqueued) = oneshot::channel();
        let worker = observe(Some(admitted), async {
            let notification = take_notification();
            complete();
            tokio::spawn(observe(notification, async move {
                enqueued.await.unwrap();
                complete();
            }))
        })
        .await;
        let mut admission = admission;
        assert!(matches!(
            admission.try_recv(),
            Err(oneshot::error::TryRecvError::Empty)
        ));
        enqueue.send(()).unwrap();
        admission.await.unwrap();
        worker.await.unwrap();
    }
}
