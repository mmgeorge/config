use serde_json::Value;

/// Validate the native completed-turn response before acknowledging delivery.
pub fn validate_prompt_response(value: Value) -> Result<(), serde_json::Error> {
    serde_json::from_value::<agent_client_protocol::schema::v1::PromptResponse>(value).map(|_| ())
}
