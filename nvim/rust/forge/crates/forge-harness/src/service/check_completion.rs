//! Forward automatic completion-check intent to the session's exchange owner.

use super::*;

/// Wait for owner-controlled completion checks without retaining the broker or transition authority.
pub(super) async fn drive(
    controller: &SessionController,
    broker: MutexGuard<'_, HarnessBroker>,
    request: Request,
    sink: crate::broker::publication::SessionUpdateSink,
    _messages: &MessageSender,
) -> crate::broker::DispatchResult {
    let sender = controller.observer.check_owner_sender();
    drop(broker);
    let result = async {
        let outcome = sender?.complete().await?;
        sink.flush().await?;
        Ok::<_, anyhow::Error>(crate::broker::DispatchResult {
            response: Response::success(request.id, outcome.value)?,
            event: outcome.events,
        })
    }
    .await;
    let _ = controller;
    result.unwrap_or_else(|error| crate::broker::DispatchResult {
        response: Response::failure(
            request.id,
            "request_failed",
            crate::storage::error_message(&error),
        ),
        event: Vec::new(),
    })
}
