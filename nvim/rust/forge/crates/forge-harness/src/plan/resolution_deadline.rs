use anyhow::Result;
use std::future::Future;
use std::time::Duration;
use tokio::time::Instant;

/// Bound automated resolution across the tool and the broker that prepares the revision.
pub(crate) const LIMIT: Duration = Duration::from_secs(10);

/// Identify an internal processing failure separately from an invalid authored plan.
#[derive(Debug, thiserror::Error)]
#[error(
    "Harness encountered a bug: plan resolution exceeded 10 seconds. Stop working and report this error to the user."
)]
pub(crate) struct ResolutionTimeout;

tokio::task_local! {
    static PROCESSING: ResolutionDeadline;
}

/// Carry a monotonic deadline through read-only analysis, including synchronous resolver calls.
#[derive(Clone, Copy)]
pub(crate) struct ResolutionDeadline(Instant);

impl ResolutionDeadline {
    /// Start with the portion of the processing allowance still available to this owner.
    pub(crate) fn new(remaining: Duration) -> Self {
        Self(Instant::now() + remaining.min(LIMIT))
    }

    /// Share the caller's budget when validation is already inside submission processing.
    pub(crate) fn current() -> Self {
        PROCESSING
            .try_with(|deadline| *deadline)
            .unwrap_or_else(|_| Self::new(LIMIT))
    }

    /// Hand unused processing time to the next owner without charging human review.
    pub(crate) fn remaining(self) -> Result<Duration> {
        self.0
            .checked_duration_since(Instant::now())
            .filter(|time| !time.is_zero())
            .ok_or_else(|| ResolutionTimeout.into())
    }

    /// Restrict a subordinate owner's own deadline to this submission's expiry.
    pub(crate) fn restrict(deadline: Instant) -> Instant {
        PROCESSING
            .try_with(|processing| processing.0.min(deadline))
            .unwrap_or(deadline)
    }

    /// Stop synchronous traversal when its enclosing submission expires.
    pub(crate) fn check() -> Result<()> {
        PROCESSING
            .try_with(|deadline| deadline.remaining().map(drop))
            .unwrap_or(Ok(()))
    }

    /// Bound a read-only asynchronous stage and reject results that crossed the deadline.
    pub(crate) async fn wait<T, E: From<anyhow::Error>>(
        self,
        work: impl Future<Output = Result<T, E>>,
    ) -> Result<T, E> {
        self.remaining()?;
        let result = tokio::time::timeout_at(self.0, PROCESSING.scope(self, work)).await;
        self.remaining()?;
        result.map_err(|_| anyhow::Error::from(ResolutionTimeout))?
    }

    /// Run CPU-bound preparation off the async runtime. Only the caller can commit its output.
    pub(crate) async fn process<T: Send + 'static>(
        self,
        work: impl FnOnce() -> Result<T> + Send + 'static,
    ) -> Result<T> {
        self.wait(async {
            tokio::task::spawn_blocking(move || {
                PROCESSING.sync_scope(self, || {
                    Self::check()?;
                    work()
                })
            })
            .await?
        })
        .await
    }

    /// Bind synchronous broker preparation to its remaining budget.
    pub(crate) fn scope<T>(self, work: impl FnOnce() -> Result<T>) -> Result<T> {
        self.remaining()?;
        PROCESSING.sync_scope(self, work)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A timed-out read-only worker observes the same deadline before returning its result.
    #[tokio::test(start_paused = true)]
    async fn expired_worker_cannot_supply_a_result_to_commit() -> Result<()> {
        let deadline = ResolutionDeadline::new(LIMIT);
        let (started, ready) = tokio::sync::oneshot::channel();
        let (release, resume) = std::sync::mpsc::channel();
        let (stopped, observed) = tokio::sync::oneshot::channel();
        let processing = tokio::spawn(deadline.process(move || {
            let _ = started.send(());
            resume.recv()?;
            let expired = ResolutionDeadline::check();
            let _ = stopped.send(expired.is_err());
            expired
        }));
        ready.await?;
        tokio::time::advance(LIMIT).await;
        let result = processing.await?;
        release.send(())?;
        assert!(result.unwrap_err().is::<ResolutionTimeout>());
        assert!(observed.await?);
        Ok(())
    }

    /// Expiry at the owner uses the unused portion of the original allowance and gives the
    /// agent a Harness diagnostic without the authored-plan retry instruction.
    #[tokio::test(start_paused = true)]
    async fn resolution_timeout_uses_the_remaining_budget_and_requests_a_bug_report() -> Result<()>
    {
        let tool = ResolutionDeadline::new(LIMIT);
        tokio::time::advance(Duration::from_secs(7)).await;
        let remaining = tool.remaining()?;
        tokio::time::advance(Duration::from_secs(20)).await;
        let owner = ResolutionDeadline::new(remaining);
        let start = Instant::now();
        let error = owner
            .wait(std::future::pending::<Result<()>>())
            .await
            .unwrap_err();
        assert_eq!(start.elapsed(), Duration::from_secs(3));
        let invocation = crate::tools::ControlToolInvocation {
            call_id: None,
            task_permit: None,
            name: "plan_submit".into(),
            arguments: serde_json::json!({}),
        };
        let text = crate::tools::control_tool_failure_text(&invocation, error.as_ref(), None, None);
        assert_eq!(
            text,
            format!("plan_submit: error[harness_plan_resolution_timeout]: {ResolutionTimeout}")
        );
        Ok(())
    }
}
