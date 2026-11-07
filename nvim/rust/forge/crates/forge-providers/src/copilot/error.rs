use forge_provider_api::{ProviderError, ProviderFailure, ProviderFailureKind, ProviderOperation};

/// Classify a native session failure without retaining its message, event, or payload.
pub(crate) fn session_failure_kind(data: &serde_json::Value) -> ProviderFailureKind {
    match data.get("errorType").and_then(serde_json::Value::as_str) {
        Some("authentication") => ProviderFailureKind::Authentication,
        Some("authorization") => ProviderFailureKind::Authorization,
        Some("policy") => ProviderFailureKind::Policy,
        Some("quota") => ProviderFailureKind::Quota,
        Some("rate_limit") => ProviderFailureKind::RateLimited,
        Some("context_limit") => ProviderFailureKind::ContextLimit,
        Some("query") => match data.get("statusCode").and_then(serde_json::Value::as_u64) {
            Some(401) => ProviderFailureKind::Authentication,
            Some(403) => ProviderFailureKind::Authorization,
            Some(408 | 504) => ProviderFailureKind::Timeout,
            Some(429) => ProviderFailureKind::RateLimited,
            Some(400 | 422) => ProviderFailureKind::Rejected,
            Some(500 | 502 | 503) => ProviderFailureKind::Transport,
            _ => ProviderFailureKind::Unknown,
        },
        _ => ProviderFailureKind::Unknown,
    }
}

/// Preserve native session failures as nonterminal observations of their mapped conversation.
pub(crate) fn session_error(
    thread_id: forge_provider_api::ThreadId,
    contents: &serde_json::Value,
) -> forge_provider_api::AdapterActivity {
    forge_provider_api::AdapterActivity::Thread {
        thread_id,
        event: forge_provider_api::ThreadEvent::Error {
            error: ProviderFailure::new(
                ProviderOperation::TurnStart,
                session_failure_kind(contents),
            )
            .at_phase(
                forge_provider_api::ProviderPhase::Observation,
                forge_provider_api::FailureDisposition::Uncertain,
            ),
        },
    }
}

/// Classify native tool diagnostics independently of the tool's actual output.
pub(crate) fn tool_failure_kind(data: &serde_json::Value) -> ProviderFailureKind {
    match data
        .pointer("/error/code")
        .and_then(serde_json::Value::as_str)
    {
        Some("denied" | "rejected") => ProviderFailureKind::Authorization,
        Some("timeout") => ProviderFailureKind::Timeout,
        Some("execution") => ProviderFailureKind::Runtime,
        _ => ProviderFailureKind::Unknown,
    }
}

/// Convert one native outcome into its normalized operation and failure category.
pub(crate) fn native_failure(
    operation: ProviderOperation,
    kind: ProviderFailureKind,
) -> ProviderError {
    ProviderError::Failure(ProviderFailure::new(operation, kind))
}

/// A committed input frame prevents a generic native failure from proving nonconsumption.
/// Keep explicit no-consumption evidence and independent cleanup causes intact.
pub(crate) fn after_input_dispatch(
    error: ProviderError,
    operation: ProviderOperation,
) -> ProviderError {
    match error {
        ProviderError::Failure(failure)
            if failure.disposition() == forge_provider_api::FailureDisposition::Unchanged
                && matches!(
                    failure.phase(),
                    forge_provider_api::ProviderPhase::Admission
                        | forge_provider_api::ProviderPhase::Preparation
                ) =>
        {
            ProviderError::Failure(failure.for_operation(operation).at_phase(
                forge_provider_api::ProviderPhase::Observation,
                forge_provider_api::FailureDisposition::Uncertain,
            ))
        }
        ProviderError::Combined { primary, secondary } => ProviderError::Combined {
            primary: Box::new(after_input_dispatch(*primary, operation)),
            secondary,
        },
        retained => retained,
    }
}

/// Preserve an existing normalized category when recording nested adapter failures.
pub(crate) fn trace_failure_kind(error: &ProviderError) -> ProviderFailureKind {
    match error {
        ProviderError::Failure(failure) => failure.kind(),
        ProviderError::Combined { primary, .. } | ProviderError::PreparationFailed(primary) => {
            trace_failure_kind(primary)
        }
        ProviderError::PreparationCleanup { cause, .. } => trace_failure_kind(cause),
        ProviderError::Contract(_)
        | ProviderError::Cleanup(_)
        | ProviderError::Unsupported(_)
        | ProviderError::Observation(_)
        | ProviderError::RuntimeVersion(_)
        | ProviderError::NotConsumed
        | ProviderError::DeliveryUncertain
        | ProviderError::Cancelled
        | ProviderError::Retryable(_)
        | ProviderError::Delivery(_) => ProviderFailureKind::Unknown,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    /// Preserve machine-readable failure semantics while discarding native diagnostics.
    #[test]
    fn native_codes_and_statuses_have_normalized_categories() {
        let nested = ProviderError::Combined {
            primary: Box::new(native_failure(
                ProviderOperation::TurnInterrupt,
                ProviderFailureKind::Timeout,
            )),
            secondary: vec![native_failure(
                ProviderOperation::TurnInterrupt,
                ProviderFailureKind::Rejected,
            )],
        };
        assert_eq!(trace_failure_kind(&nested), ProviderFailureKind::Timeout);
        for (status, expected) in [
            (401, ProviderFailureKind::Authentication),
            (403, ProviderFailureKind::Authorization),
            (408, ProviderFailureKind::Timeout),
            (429, ProviderFailureKind::RateLimited),
            (503, ProviderFailureKind::Transport),
            (599, ProviderFailureKind::Unknown),
        ] {
            assert_eq!(
                session_failure_kind(&json!({"errorType":"query","statusCode":status,
                "message":"private-native-diagnostic"})),
                expected
            );
        }
        assert_eq!(
            session_failure_kind(&json!({"errorType":"future","message":"authentication"})),
            ProviderFailureKind::Unknown
        );
        assert_eq!(
            tool_failure_kind(
                &json!({"error":{"code":"timeout","message":"private-native-diagnostic"}})
            ),
            ProviderFailureKind::Timeout
        );
        assert_eq!(
            tool_failure_kind(&json!({"error":{"message":"timeout"}})),
            ProviderFailureKind::Unknown
        );
    }
}
