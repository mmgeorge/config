pub use forge_provider_api::deadline::{
    InputDeadline, InputTimeout, ResolutionSource, TimeoutUnit,
};

#[cfg(test)]
use std::time::Duration;

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

    /// Keep tool deadlines enabled even though approval waits support disabling expiry.
    #[test]
    fn tool_timeout_requires_expiry_and_defaults_to_ten_minutes() {
        assert_eq!(
            InputTimeout::default().require_expiry().unwrap().duration(),
            Duration::from_secs(600)
        );
        assert!(
            InputTimeout {
                amount: 0,
                unit: TimeoutUnit::S
            }
            .require_expiry()
            .is_err()
        );
    }

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
