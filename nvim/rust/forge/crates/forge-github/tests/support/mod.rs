use std::future::{Future, poll_fn};
use std::pin::Pin;
use std::task::Poll;

/// Polls a future exactly once and reports whether it stayed pending.
///
/// Callers first establish the blocking state through a fixture marker or a synchronous
/// enqueue, so the single poll reaches the future's park point deterministically.
pub async fn is_pending_after_one_poll<Waiting: Future>(mut waiting: Pin<&mut Waiting>) -> bool {
    poll_fn(|context| Poll::Ready(waiting.as_mut().poll(context).is_pending())).await
}
