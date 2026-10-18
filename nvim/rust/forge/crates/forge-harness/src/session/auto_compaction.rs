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
        ensure!(self.threshold_tokens > 0, "Compaction threshold must be greater than zero");
        if self.enabled && let Some(usage) = usage {
            ensure!(self.threshold_tokens < usage.size,
                "Compaction threshold {} must be below the model context capacity {}", self.threshold_tokens, usage.size);
        }
        Ok(())
    }

    /// Report eligibility from current context rather than cumulative billing tokens.
    pub fn due(&self, usage: Option<&super::ContextUsage>, supported: bool) -> bool {
        supported && self.enabled && self.validate(usage).is_ok()
            && usage.is_some_and(|usage| usage.used >= self.threshold_tokens)
    }
}

impl Default for AutoCompaction {
    fn default() -> Self { Self { enabled: false, threshold_tokens: 128_000 } }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn threshold_uses_retained_context_and_known_capacity() {
        let mut policy = AutoCompaction { enabled:true, ..Default::default() };
        assert!(!policy.due(None,true));
        assert!(!policy.due(super::super::ContextUsage::reported(127_999,256_000).as_ref(),true));
        assert!(policy.due(super::super::ContextUsage::reported(128_000,256_000).as_ref(),true));
        assert!(!policy.due(super::super::ContextUsage::reported(128_000,256_000).as_ref(),false));
        assert!(policy.validate(super::super::ContextUsage::reported(64_000,128_000).as_ref()).is_err());
        policy.enabled=false;
        assert!(!policy.due(super::super::ContextUsage::reported(180_000,256_000).as_ref(),true));
        policy.threshold_tokens=0;
        assert!(policy.validate(None).is_err());
    }
}
