//! Report when the async runtime runs a ready timer late.
//!
//! A worker thread that blocks on a disk, a lock, or a long computation delays every task
//! queued behind it. This canary measures that delay from inside the runtime. It is a coarse
//! alarm: a multi-thread runtime can steal the timer onto a free worker, so short stalls of a
//! single worker pass unseen.

use std::time::Duration;
use tokio::time::Instant;

/// How often the canary asks the runtime to wake it.
const PROBE_INTERVAL: Duration = Duration::from_millis(100);
/// The smallest overshoot worth reporting.
const REPORT_THRESHOLD: Duration = Duration::from_millis(50);
/// The shortest gap between two reports.
const REPORT_INTERVAL: Duration = Duration::from_secs(1);

/// Decide which late wakeups to report, without touching a clock.
#[derive(Default)]
pub(crate) struct SchedulingLagProbe {
    last_report: Option<Instant>,
}

impl SchedulingLagProbe {
    /// Return the overshoot of a wakeup expected at `expected` and observed at `now`, when it
    /// reaches the threshold and no report was made in the last interval.
    pub(crate) fn observe(&mut self, expected: Instant, now: Instant) -> Option<Duration> {
        let lag = now.saturating_duration_since(expected);
        if lag < REPORT_THRESHOLD {
            return None;
        }
        if self
            .last_report
            .is_some_and(|last| now.saturating_duration_since(last) < REPORT_INTERVAL)
        {
            return None;
        }
        self.last_report = Some(now);
        Some(lag)
    }
}

/// Stop the canary when the connection that started it ends.
pub(crate) struct SchedulingLagCanary {
    task: tokio::task::JoinHandle<()>,
}

impl Drop for SchedulingLagCanary {
    fn drop(&mut self) {
        self.task.abort();
    }
}

/// Start the canary on the current runtime.
pub(crate) fn spawn() -> SchedulingLagCanary {
    SchedulingLagCanary {
        task: tokio::spawn(async {
            let mut probe = SchedulingLagProbe::default();
            loop {
                let expected = Instant::now() + PROBE_INTERVAL;
                tokio::time::sleep_until(expected).await;
                if let Some(lag) = probe.observe(expected, Instant::now()) {
                    tracing::warn!(
                        name: "runtime.scheduling_lag",
                        lag_ms = u64::try_from(lag.as_millis()).unwrap_or(u64::MAX),
                        "the async runtime ran a ready timer late"
                    );
                }
            }
        }),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Small overshoots stay silent and a sustained stall reports once per interval.
    #[tokio::test(start_paused = true)]
    async fn reports_large_overshoot_at_most_once_per_interval() {
        let mut probe = SchedulingLagProbe::default();
        let start = Instant::now();
        let after = |milliseconds| start + Duration::from_millis(milliseconds);

        assert_eq!(probe.observe(start, after(49)), None);
        assert_eq!(
            probe.observe(start, after(80)),
            Some(Duration::from_millis(80))
        );
        assert_eq!(probe.observe(after(100), after(400)), None);
        assert_eq!(
            probe.observe(after(1000), after(1090)),
            Some(Duration::from_millis(90))
        );
        assert_eq!(probe.observe(after(1200), after(1100)), None);
    }
}
