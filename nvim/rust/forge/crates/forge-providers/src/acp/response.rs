use serde_json::Value;

/// Classify a schema-valid ACP prompt terminal response.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum PromptOutcome {
    /// The agent reported a non-cancelled terminal reason.
    Completed,
    /// The agent acknowledged native cancellation.
    Cancelled,
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
        StopReason::EndTurn
        | StopReason::MaxTokens
        | StopReason::MaxTurnRequests
        | StopReason::Refusal => Ok(PromptOutcome::Completed),
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
            Ok(PromptOutcome::Completed)
        ));
        assert!(matches!(
            prompt_outcome(&json!({"stopReason":"cancelled"})),
            Ok(PromptOutcome::Cancelled)
        ));
        assert!(prompt_outcome(&json!({})).is_err());
        assert!(prompt_outcome(&json!({"stopReason":"future_reason"})).is_err());
    }
}
