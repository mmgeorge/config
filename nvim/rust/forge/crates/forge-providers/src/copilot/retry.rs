use forge_provider_api::retry::RetryCategory;
use serde_json::Value;

/// Accepts explicit upstream connection/server status on a terminal query failure.
pub fn transient_category(data: &Value) -> Option<RetryCategory> {
    if data.get("errorType").and_then(Value::as_str) != Some("query") {
        return None;
    }
    match data.get("statusCode").and_then(Value::as_u64) {
        Some(503) => Some(RetryCategory::TemporaryCapacity),
        Some(408 | 500 | 502 | 504) => Some(RetryCategory::TemporaryTransport),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn unknown_categories_and_runtime_retries_are_not_replayed() {
        assert_eq!(
            transient_category(&json!({"errorType":"query","statusCode":503})),
            Some(RetryCategory::TemporaryCapacity)
        );
        for category in [
            "model_call",
            "authentication",
            "authorization",
            "policy",
            "quota",
            "rate_limit",
            "context_limit",
            "unknown",
        ] {
            assert!(transient_category(&json!({"errorType":category,"statusCode":503})).is_none());
        }
        for status in [400, 401, 403, 429] {
            assert!(
                transient_category(
                    &json!({"errorType":"query","statusCode":status,"message":"network failure"})
                )
                .is_none()
            );
        }
        assert!(
            transient_category(&json!({"errorType":"query","message":"model at capacity"}))
                .is_none()
        );
    }
}
