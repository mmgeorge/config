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
