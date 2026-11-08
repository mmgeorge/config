use anyhow::{Result, ensure};
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
#[serde(deny_unknown_fields)]
/// Selects a provider-reported token threshold without changing model capacity.
pub struct AutoCompaction {
    /// Enables Forge-requested compaction at a completed turn boundary.
    pub enabled: bool,
    /// Minimum retained context tokens at which compaction becomes eligible.
    pub threshold_tokens: u64,
}

impl AutoCompaction {
    /// Validate settings independently of whether a provider has reported capacity yet.
    pub fn validate(&self, usage: Option<&super::ContextUsage>) -> Result<()> {
        ensure!(
            self.threshold_tokens > 0,
            "Compaction threshold must be greater than zero"
        );
        if self.enabled
            && let Some(usage) = usage
        {
            ensure!(
                self.threshold_tokens < usage.size,
                "Compaction threshold {} must be below the model context capacity {}",
                self.threshold_tokens,
                usage.size
            );
        }
        Ok(())
    }

    /// Report eligibility from current context rather than cumulative billing tokens.
    pub fn due(&self, usage: Option<&super::ContextUsage>, supported: bool) -> bool {
        supported
            && self.enabled
            && self.validate(usage).is_ok()
            && usage.is_some_and(|usage| usage.used >= self.threshold_tokens)
    }
}

impl Default for AutoCompaction {
    fn default() -> Self {
        Self {
            enabled: false,
            threshold_tokens: 128_000,
        }
    }
}
