use forge_provider_api::retry::RetryCategory;
use serde_json::Value;

/// Convert native failure metadata to the shared diagnostic category without message matching.
pub fn failure_kind(error: &Value) -> forge_provider_api::ProviderFailureKind {
    use forge_provider_api::ProviderFailureKind;
    let Some(info) = error.get("codexErrorInfo") else {
        return ProviderFailureKind::Unknown;
    };
    match info.as_str() {
        Some("contextWindowExceeded") => return ProviderFailureKind::ContextLimit,
        Some("usageLimitExceeded") => return ProviderFailureKind::Quota,
        Some("rateLimitExceeded") => return ProviderFailureKind::RateLimited,
        Some("unauthorized") => return ProviderFailureKind::Authentication,
        Some("badRequest") => return ProviderFailureKind::Rejected,
        Some("sandboxError") => return ProviderFailureKind::Policy,
        Some("serverOverloaded" | "internalServerError") => return ProviderFailureKind::Transport,
        _ => {}
    }
    let Some(variant) = info.as_object().filter(|variant| variant.len() == 1) else {
        return ProviderFailureKind::Unknown;
    };
    let Some((code, detail)) = variant.iter().next() else {
        return ProviderFailureKind::Unknown;
    };
    if !matches!(
        code.as_str(),
        "httpConnectionFailed" | "responseStreamConnectionFailed" | "responseStreamDisconnected"
    ) {
        return ProviderFailureKind::Unknown;
    }
    match detail.get("httpStatusCode") {
        Some(Value::Null) => ProviderFailureKind::Transport,
        Some(status) => match status.as_u64() {
            Some(401) => ProviderFailureKind::Authentication,
            Some(403) => ProviderFailureKind::Authorization,
            Some(408 | 504) => ProviderFailureKind::Timeout,
            Some(429) => ProviderFailureKind::RateLimited,
            Some(400 | 422) => ProviderFailureKind::Rejected,
            Some(500 | 502 | 503) => ProviderFailureKind::Transport,
            _ => ProviderFailureKind::Unknown,
        },
        None => ProviderFailureKind::Unknown,
    }
}

/// Classifies only documented connection and capacity codes, never message text.
pub fn transient_category(error: &Value) -> Option<RetryCategory> {
    let info = error.get("codexErrorInfo")?;
    if info.as_str() == Some("serverOverloaded") {
        return Some(RetryCategory::TemporaryCapacity);
    }
    let variant = info.as_object()?;
    if variant.len() != 1 {
        return None;
    }
    let (code, detail) = variant.iter().next()?;
    if !matches!(
        code.as_str(),
        "httpConnectionFailed" | "responseStreamConnectionFailed" | "responseStreamDisconnected"
    ) {
        return None;
    }
    let status = detail.get("httpStatusCode");
    if status.is_some_and(|status| {
        status.is_null() || matches!(status.as_u64(), Some(408 | 500 | 502 | 503 | 504))
    }) {
        Some(RetryCategory::TemporaryTransport)
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    /// Keep permanent failures distinguishable without granting them automatic replay.
    #[test]
    fn permanent_failures_keep_their_normalized_category() {
        use forge_provider_api::ProviderFailureKind;
        for (code, expected) in [
            ("contextWindowExceeded", ProviderFailureKind::ContextLimit),
            ("usageLimitExceeded", ProviderFailureKind::Quota),
            ("rateLimitExceeded", ProviderFailureKind::RateLimited),
            ("unauthorized", ProviderFailureKind::Authentication),
            ("sandboxError", ProviderFailureKind::Policy),
        ] {
            let error = json!({"codexErrorInfo":code,"message":"private-native-diagnostic"});
            assert_eq!(failure_kind(&error), expected);
            assert!(transient_category(&error).is_none());
        }
        assert_eq!(
            failure_kind(&json!({"message":"unauthorized"})),
            ProviderFailureKind::Unknown
        );
    }

    /// Do not turn malformed or future native status fields into a transport classification.
    #[test]
    fn unknown_status_shapes_remain_unknown_and_not_retryable() {
        use forge_provider_api::ProviderFailureKind;
        for detail in [
            json!({}),
            json!(null),
            json!("503"),
            json!({"httpStatusCode":"503"}),
            json!({"httpStatusCode":599}),
            json!({"httpStatusCode":false}),
        ] {
            let error = json!({"codexErrorInfo":{"httpConnectionFailed":detail}, "message":"network failure"});
            assert_eq!(failure_kind(&error), ProviderFailureKind::Unknown);
            assert!(transient_category(&error).is_none());
        }
        let no_response =
            json!({"codexErrorInfo":{"httpConnectionFailed":{"httpStatusCode":null}}});
        assert_eq!(failure_kind(&no_response), ProviderFailureKind::Transport);
        assert_eq!(
            transient_category(&no_response),
            Some(RetryCategory::TemporaryTransport)
        );
        for info in [
            json!({"httpConnectionFailed":{"httpStatusCode":503},"unauthorized":{}}),
            json!(["serverOverloaded"]),
            json!(true),
        ] {
            let error = json!({"codexErrorInfo": info});
            assert_eq!(failure_kind(&error), ProviderFailureKind::Unknown);
            assert!(transient_category(&error).is_none());
        }
    }

    #[test]
    fn allowlist_rejects_permanent_and_unknown_failures() {
        assert!(transient_category(&json!({"codexErrorInfo":"serverOverloaded"})).is_some());
        for code in [
            "unauthorized",
            "badRequest",
            "usageLimitExceeded",
            "rateLimitExceeded",
            "sandboxError",
            "other",
            "internalServerError",
        ] {
            assert!(
                transient_category(
                    &json!({"codexErrorInfo":code,"message":"network error at capacity"})
                )
                .is_none()
            );
        }
        for status in [
            Value::Null,
            json!(408),
            json!(500),
            json!(502),
            json!(503),
            json!(504),
        ] {
            assert!(
                transient_category(
                    &json!({"codexErrorInfo":{"httpConnectionFailed":{"httpStatusCode":status}}})
                )
                .is_some()
            );
        }
        for status in [json!(400), json!(401), json!(403), json!(429), json!("503")] {
            assert!(
                transient_category(
                    &json!({"codexErrorInfo":{"httpConnectionFailed":{"httpStatusCode":status}}})
                )
                .is_none()
            );
        }
        assert!(transient_category(&json!({"message":"Selected model is at capacity"})).is_none());
    }
}
