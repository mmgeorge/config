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
