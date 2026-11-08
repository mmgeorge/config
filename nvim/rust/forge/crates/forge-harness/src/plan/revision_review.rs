use std::sync::{Arc, Mutex};

use anyhow::{Context, Result, ensure};
use tokio::sync::{mpsc, oneshot};

/// Routes review reads and decisions to the broker retaining a provider tool call.
#[derive(Default)]
pub(crate) struct RevisionReview {
    sender: Mutex<Option<mpsc::Sender<RevisionReviewRequest>>>,
}

impl RevisionReview {
    /// Open one review wait, closed when its owning provider attempt is collected.
    pub(crate) fn open(self: &Arc<Self>) -> Result<RevisionReviewWait> {
        let mut active = self
            .sender
            .lock()
            .map_err(|_| anyhow::anyhow!("revision review lock poisoned"))?;
        ensure!(active.is_none(), "a revision review is already waiting");
        let (sender, receiver) = mpsc::channel(8);
        *active = Some(sender.clone());
        Ok(RevisionReviewWait {
            owner: Arc::clone(self),
            sender,
            receiver,
        })
    }

    fn sender(&self) -> Result<Option<mpsc::Sender<RevisionReviewRequest>>> {
        Ok(self
            .sender
            .lock()
            .map_err(|_| anyhow::anyhow!("revision review lock poisoned"))?
            .clone())
    }

    /// Read a review source without acquiring the broker held by the waiting turn.
    pub(crate) async fn capture(
        &self,
        plan_id: &str,
        digest: &str,
        revision: Option<u32>,
    ) -> Result<Option<super::review_source::PlanReviewSource>> {
        let Some(sender) = self.sender()? else {
            return Ok(None);
        };
        let (response, receive) = oneshot::channel();
        sender
            .send(RevisionReviewRequest::Capture {
                plan_id: plan_id.into(),
                digest: digest.into(),
                revision,
                response,
            })
            .await
            .context("revision review closed before source capture")?;
        receive
            .await
            .context("revision review source capture was interrupted")?
            .map(Some)
    }
}

/// Keeps review admission alive only while its provider tool response is pending.
pub(crate) struct RevisionReviewWait {
    owner: Arc<RevisionReview>,
    sender: mpsc::Sender<RevisionReviewRequest>,
    pub(crate) receiver: mpsc::Receiver<RevisionReviewRequest>,
}

impl Drop for RevisionReviewWait {
    fn drop(&mut self) {
        if let Ok(mut active) = self.owner.sender.lock()
            && active
                .as_ref()
                .is_some_and(|sender| sender.same_channel(&self.sender))
        {
            *active = None;
        }
    }
}

/// A bounded request serviced by the broker without ending its provider attempt.
pub(crate) enum RevisionReviewRequest {
    Capture {
        plan_id: String,
        digest: String,
        revision: Option<u32>,
        response: oneshot::Sender<Result<super::review_source::PlanReviewSource>>,
    },
}
