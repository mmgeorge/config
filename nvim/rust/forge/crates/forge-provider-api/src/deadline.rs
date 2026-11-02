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
/// Configures an operation deadline. Input waits may disable expiry with zero.
pub struct InputTimeout {
    /// Holds a nonnegative whole count, with zero disabling expiry.
    pub amount: u32,
    /// Preserves the user's selected display unit.
    pub unit: TimeoutUnit,
}

impl InputTimeout {
    /// Reject disabled expiry for tool and shell execution.
    pub fn require_expiry(self) -> Result<Self> {
        self.validate()?;
        ensure!(
            self.amount > 0,
            "tool and shell timeout must be greater than zero"
        );
        Ok(self)
    }

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
    pub fn instant(&mut self, now_ms: i64) -> Instant {
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
