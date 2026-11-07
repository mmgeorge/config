use serde_json::Value;

/// Classify a schema-valid ACP prompt terminal response.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum PromptOutcome {
    /// The agent reported a non-cancelled terminal reason.
    Completed(forge_provider_api::turn::TurnCompletionReason),
    /// The agent acknowledged native cancellation.
    Cancelled,
}

/// Retain a parsed native terminal fact before host receipt persistence or collection waits.
#[derive(Clone, Copy, Debug)]
pub(super) struct NativePromptEnd {
    outcome: PromptOutcome,
    completed_at_ms: i64,
}

impl NativePromptEnd {
    /// Decode exactly one native stop reason at its response boundary.
    pub(super) fn parse(value: &Value, completed_at_ms: i64) -> Result<Self, serde_json::Error> {
        Ok(Self {
            outcome: prompt_outcome(value)?,
            completed_at_ms,
        })
    }
    /// Get the once-parsed native terminal outcome.
    pub(super) fn outcome(self) -> PromptOutcome {
        self.outcome
    }
    /// Get native response time independently of later receipt persistence and collection.
    pub(super) fn completed_at_ms(self) -> i64 {
        self.completed_at_ms
    }
}

/// Validate the native completed-turn response before acknowledging delivery.
pub(crate) fn validate_prompt_response(value: Value) -> Result<(), serde_json::Error> {
    serde_json::from_value::<agent_client_protocol::schema::v1::PromptResponse>(value).map(|_| ())
}

/// Decode known ACP stop reasons without treating missing or unknown values as success.
pub(crate) fn prompt_outcome(value: &Value) -> Result<PromptOutcome, serde_json::Error> {
    use agent_client_protocol::schema::v1::StopReason;
    use serde::de::Error as _;

    let response =
        serde_json::from_value::<agent_client_protocol::schema::v1::PromptResponse>(value.clone())?;
    match response.stop_reason {
        StopReason::Cancelled => Ok(PromptOutcome::Cancelled),
        StopReason::EndTurn => Ok(PromptOutcome::Completed(
            forge_provider_api::turn::TurnCompletionReason::Normal,
        )),
        StopReason::MaxTokens => Ok(PromptOutcome::Completed(
            forge_provider_api::turn::TurnCompletionReason::TokenLimit,
        )),
        StopReason::MaxTurnRequests => Ok(PromptOutcome::Completed(
            forge_provider_api::turn::TurnCompletionReason::RequestLimit,
        )),
        StopReason::Refusal => Ok(PromptOutcome::Completed(
            forge_provider_api::turn::TurnCompletionReason::Refusal,
        )),
        _ => Err(serde_json::Error::custom(
            "unsupported ACP prompt stop reason",
        )),
    }
}

#[cfg(test)]
mod tests {
    use super::{PromptOutcome, prompt_outcome};
    use serde_json::json;

    #[test]
    fn prompt_terminal_requires_a_known_stop_reason() {
        assert!(matches!(
            prompt_outcome(&json!({"stopReason":"end_turn"})),
            Ok(PromptOutcome::Completed(
                forge_provider_api::turn::TurnCompletionReason::Normal
            ))
        ));
        assert!(matches!(
            prompt_outcome(&json!({"stopReason":"cancelled"})),
            Ok(PromptOutcome::Cancelled)
        ));
        for (native, reason) in [
            (
                "max_tokens",
                forge_provider_api::turn::TurnCompletionReason::TokenLimit,
            ),
            (
                "max_turn_requests",
                forge_provider_api::turn::TurnCompletionReason::RequestLimit,
            ),
            (
                "refusal",
                forge_provider_api::turn::TurnCompletionReason::Refusal,
            ),
        ] {
            assert_eq!(
                prompt_outcome(&json!({"stopReason":native})).unwrap(),
                PromptOutcome::Completed(reason)
            );
        }
        assert!(prompt_outcome(&json!({})).is_err());
        assert!(prompt_outcome(&json!({"stopReason":"future_reason"})).is_err());
    }
}
