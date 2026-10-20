use serde_json::Value;

/// Classifies only documented connection and capacity codes, never message text.
pub(super) fn transient_code(error: &Value) -> Option<String> {
    let info = error.get("codexErrorInfo")?;
    if info.as_str() == Some("serverOverloaded") {
        return Some("serverOverloaded".into());
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
        Some(code.clone())
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn allowlist_rejects_permanent_and_unknown_failures() {
        assert!(transient_code(&json!({"codexErrorInfo":"serverOverloaded"})).is_some());
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
                transient_code(
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
                transient_code(
                    &json!({"codexErrorInfo":{"httpConnectionFailed":{"httpStatusCode":status}}})
                )
                .is_some()
            );
        }
        for status in [json!(400), json!(401), json!(403), json!(429), json!("503")] {
            assert!(
                transient_code(
                    &json!({"codexErrorInfo":{"httpConnectionFailed":{"httpStatusCode":status}}})
                )
                .is_none()
            );
        }
        assert!(transient_code(&json!({"message":"Selected model is at capacity"})).is_none());
    }
}
