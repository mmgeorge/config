use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
/// Stores inclusive provider token counts. Absent fields remain unavailable.
pub struct TokenUsage {
    /// All submitted input tokens, including cache hits.
    pub input: Option<u64>,
    /// Input tokens served from the prompt cache.
    pub cached_input: Option<u64>,
    /// Generated tokens attributed to internal reasoning.
    pub reasoning: Option<u64>,
    /// All generated tokens, including reasoning and tool arguments.
    pub output: Option<u64>,
}

impl TokenUsage {
    /// Sum matching reported fields without treating unavailable counts as zero.
    pub(crate) fn accumulate(&mut self, incoming: &Self) {
        self.input = self
            .input
            .zip(incoming.input)
            .and_then(|(current, added)| current.checked_add(added));
        self.cached_input = self
            .cached_input
            .zip(incoming.cached_input)
            .and_then(|(current, added)| current.checked_add(added));
        self.reasoning = self
            .reasoning
            .zip(incoming.reasoning)
            .and_then(|(current, added)| current.checked_add(added));
        self.output = self
            .output
            .zip(incoming.output)
            .and_then(|(current, added)| current.checked_add(added));
    }

    /// Return a cumulative increment only when every supplied counter remains monotonic.
    pub(crate) fn difference(&self, previous: &Self) -> Option<Self> {
        fn difference(current: Option<u64>, previous: Option<u64>) -> Option<Option<u64>> {
            match (current, previous) {
                (Some(current), Some(previous)) => current.checked_sub(previous).map(Some),
                (None, None) => Some(None),
                _ => None,
            }
        }
        Some(Self {
            input: difference(self.input, previous.input)?,
            cached_input: difference(self.cached_input, previous.cached_input)?,
            reasoning: difference(self.reasoning, previous.reasoning)?,
            output: difference(self.output, previous.output)?,
        })
    }

    /// Return generated output excluding reasoning, without inventing missing breakdowns.
    pub fn non_reasoning_output(&self) -> Option<u64> {
        self.output
            .zip(self.reasoning)
            .and_then(|(output, reasoning)| output.checked_sub(reasoning))
    }

    /// Return the weighted cache-hit percentage when input accounting is complete and valid.
    pub fn cached_percent(&self) -> Option<u64> {
        let (input, cached) = self.input.zip(self.cached_input)?;
        (input > 0 && cached <= input)
            .then(|| ((cached as f64 / input as f64) * 100.0).round() as u64)
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
/// Identifies one usage report independently of text and tool lifecycle replay.
pub(crate) struct UsageUpdate {
    /// Provider call identity or cumulative snapshot identity.
    pub id: String,
    /// Usage reported for the last completed model call.
    pub usage: TokenUsage,
    /// Native thread totals used to recover increments and deduplicate refreshes.
    pub cumulative: Option<TokenUsage>,
    /// Native total, retained separately from the token category breakdown.
    pub cumulative_total: Option<u64>,
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn weighted_cache_percentage_and_output_keep_missing_categories_unavailable() {
        let mut usage = TokenUsage {
            input: Some(100),
            cached_input: Some(90),
            reasoning: Some(10),
            output: Some(30),
        };
        usage.accumulate(&TokenUsage {
            input: Some(900),
            cached_input: Some(0),
            reasoning: Some(20),
            output: Some(40),
        });
        assert_eq!(usage.cached_percent(), Some(9));
        assert_eq!(usage.non_reasoning_output(), Some(40));
        usage.accumulate(&TokenUsage {
            input: Some(100),
            cached_input: None,
            reasoning: None,
            output: Some(30),
        });
        assert_eq!(usage.input, Some(1100));
        assert_eq!(usage.cached_percent(), None);
        assert_eq!(usage.reasoning, None);
        assert_eq!(usage.non_reasoning_output(), None);
        let invalid = TokenUsage {
            input: Some(0),
            cached_input: Some(1),
            reasoning: Some(30),
            output: Some(10),
        };
        assert_eq!(invalid.cached_percent(), None);
        assert_eq!(invalid.non_reasoning_output(), None);
    }
}
