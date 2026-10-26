use anyhow::{Context, Result, ensure};
use serde::{Deserialize, Serialize};
use std::time::Duration;
use tokio::time::Instant;

#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
/// Identifies whether a response came from the user or an elapsed deadline.
pub enum ResolutionSource {
    #[default]
    /// Records an explicitly confirmed response.
    User,
    /// Records an automatically selected or rejected response.
    Timeout,
}

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "lowercase")]
/// Selects the display unit without changing the configured amount.
pub enum TimeoutUnit {
    /// Measures seconds.
    S,
    /// Measures minutes.
    M,
    /// Measures hours.
    H,
}

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
/// Configures one input deadline. Zero disables expiry.
pub struct InputTimeout {
    /// Holds a nonnegative whole count, with zero disabling expiry.
    pub amount: u32,
    /// Preserves the user's selected display unit.
    pub unit: TimeoutUnit,
}

impl InputTimeout {
    /// Validate the duration before saving it or creating a runtime timer.
    pub fn validate(self) -> Result<Self> {
        let duration = self.duration();
        ensure!(
            Instant::now().checked_add(duration).is_some(),
            "input timeout exceeds the timer range"
        );
        Ok(self)
    }

    /// Return the configured interval, including zero for disabled deadlines.
    pub fn duration(self) -> Duration {
        let multiplier = match self.unit {
            TimeoutUnit::S => 1,
            TimeoutUnit::M => 60,
            TimeoutUnit::H => 3600,
        };
        Duration::from_secs(u64::from(self.amount) * multiplier)
    }

    /// Capture one deadline without retaining mutable configuration.
    pub fn deadline(self, now_ms: i64) -> Result<Option<InputDeadline>> {
        self.validate()?;
        if self.amount == 0 {
            return Ok(None);
        }
        let duration = self.duration();
        let expires_at_ms = now_ms
            .checked_add(i64::try_from(duration.as_millis())?)
            .context("input timeout exceeds the timestamp range")?;
        Ok(Some(InputDeadline {
            expires_at_ms,
            instant: Instant::now().checked_add(duration),
        }))
    }
}

impl Default for InputTimeout {
    fn default() -> Self {
        Self {
            amount: 10,
            unit: TimeoutUnit::M,
        }
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
/// Retains an absolute recovery deadline and a monotonic deadline while the host lives.
pub struct InputDeadline {
    /// Stores the UTC expiry for presentation and recovery.
    pub expires_at_ms: i64,
    #[serde(skip)]
    instant: Option<Instant>,
}

impl InputDeadline {
    /// Return the captured monotonic deadline after restoring durable timing once.
    pub(crate) fn instant(&mut self, now_ms: i64) -> Instant {
        self.restore(now_ms);
        self.instant.expect("validated input deadline")
    }
    /// Restore monotonic timing once after loading durable state.
    pub fn restore(&mut self, now_ms: i64) {
        if self.instant.is_none() {
            self.instant = Instant::now().checked_add(Duration::from_millis(
                self.expires_at_ms.saturating_sub(now_ms).max(0) as u64,
            ));
        }
    }

    /// Test the running monotonic timer, or the recovery timestamp before restoration.
    pub fn expired(&self, now_ms: i64) -> bool {
        self.instant
            .map_or(now_ms >= self.expires_at_ms, |instant| {
                Instant::now() >= instant
            })
    }
}

impl PartialEq for InputDeadline {
    fn eq(&self, other: &Self) -> bool {
        self.expires_at_ms == other.expires_at_ms
    }
}
impl Eq for InputDeadline {}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
/// Publishes the current input owner independently of execution admission.
pub(crate) struct InputSchedule {
    /// Changes whenever input ownership, timing, or continuation eligibility changes.
    pub revision: u64,
    /// Identifies the owning plan or exchange and its question set.
    pub identity: Option<String>,
    /// Invalidates pending expiry work when answers or question content change.
    pub question_revision: Option<u32>,
    /// Retains one monotonic timer for the current durable deadline.
    pub deadline: Option<InputDeadline>,
    /// Allows approval feedback to continue only outside question and acceptance waits.
    pub approval_allowed: bool,
}

impl InputSchedule {
    /// Adopt durable input state without extending an unchanged live deadline.
    pub fn update(&mut self, mut next: Self, now_ms: i64) -> bool {
        next.revision = self.revision;
        if *self == next {
            return false;
        }
        if self.identity == next.identity && self.deadline == next.deadline {
            next.deadline = self.deadline.clone();
        } else if let Some(deadline) = &mut next.deadline {
            deadline.restore(now_ms);
        }
        next.revision += 1;
        *self = next;
        true
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[tokio::test(start_paused = true)]
    async fn input_schedule_keeps_monotonic_deadlines_and_invalidates_replaced_owners() {
        let mut schedule = InputSchedule::default();
        let durable = || InputSchedule {
            identity: Some("exchange:first:question".into()),
            deadline: Some(
                serde_json::from_value(serde_json::json!({"expires_at_ms": 11_000})).unwrap(),
            ),
            ..Default::default()
        };
        assert!(schedule.update(durable(), 10_000));
        let revision = schedule.revision;
        tokio::time::advance(Duration::from_millis(999)).await;
        assert!(!schedule.update(durable(), 0));
        assert_eq!(schedule.revision, revision);
        let mut revised = durable();
        revised.question_revision = Some(2);
        assert!(schedule.update(revised, 0));
        assert!(!schedule.deadline.as_ref().unwrap().expired(i64::MAX));
        tokio::time::advance(Duration::from_millis(1)).await;
        assert!(schedule.deadline.as_ref().unwrap().expired(0));
        assert!(schedule.update(InputSchedule::default(), 11_000));
        assert!(schedule.update(durable(), 11_000));
        assert!(schedule.revision > revision);
        assert!(schedule.deadline.as_ref().unwrap().expired(0));
    }

    #[test]
    fn input_timeout_recovery_and_clock_changes_do_not_extend_live_deadlines() {
        let timeout = InputTimeout {
            amount: 1,
            unit: TimeoutUnit::S,
        };
        let deadline = timeout.deadline(10_000).unwrap().unwrap();
        assert!(!deadline.expired(i64::MAX));
        let mut recovered: InputDeadline =
            serde_json::from_value(serde_json::json!({"expires_at_ms": 10_000})).unwrap();
        assert!(recovered.expired(10_001));
        recovered.restore(10_001);
        assert!(recovered.expired(0));
        assert!(
            InputTimeout {
                amount: 0,
                unit: TimeoutUnit::H
            }
            .deadline(0)
            .unwrap()
            .is_none()
        );
        assert!(timeout.deadline(i64::MAX).is_err());
    }
}
