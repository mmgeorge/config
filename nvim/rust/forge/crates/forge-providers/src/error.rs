use crate::acp::{AcpConnectionError, AcpRemoteFailure};
use crate::codex::CodexConnectionError;
use forge_provider_api::ProviderError;
use forge_provider_api::error::{ProviderFailureKind, ProviderOperation};

/// Normalize a Codex transport failure at its requested operation boundary.
pub(crate) fn codex(error: CodexConnectionError, operation: ProviderOperation) -> ProviderError {
    let kind = match error {
        CodexConnectionError::Overflow => {
            return ProviderError::Observation(
                forge_provider_api::lifecycle::ObservationFailure::Overflow,
            );
        }
        CodexConnectionError::Encoding(_) => ProviderFailureKind::Protocol,
        CodexConnectionError::Transport { .. } | CodexConnectionError::Closed(_) => {
            ProviderFailureKind::Transport
        }
    };
    ProviderError::Failure(
        forge_provider_api::ProviderFailure::new(operation, kind).at_phase(
            forge_provider_api::ProviderPhase::Observation,
            forge_provider_api::FailureDisposition::Uncertain,
        ),
    )
}

/// Normalize an ACP transport failure without retaining native diagnostics or inventing delivery proof.
pub(crate) fn acp(error: AcpConnectionError, operation: ProviderOperation) -> ProviderError {
    let (kind, phase, disposition) = match error {
        AcpConnectionError::Protocol { .. } => (
            ProviderFailureKind::Protocol,
            forge_provider_api::ProviderPhase::Observation,
            forge_provider_api::FailureDisposition::Uncertain,
        ),
        AcpConnectionError::Remote { failure, .. } => {
            let kind = match failure {
                Some(AcpRemoteFailure::AuthenticationRequired) => {
                    ProviderFailureKind::Authentication
                }
                Some(AcpRemoteFailure::RequestCancelled | AcpRemoteFailure::Rejected) => {
                    ProviderFailureKind::Rejected
                }
                Some(AcpRemoteFailure::ResourceNotFound) => ProviderFailureKind::SessionMissing,
                Some(AcpRemoteFailure::Protocol) => ProviderFailureKind::Protocol,
                None => ProviderFailureKind::Unknown,
            };
            (
                kind,
                forge_provider_api::ProviderPhase::Observation,
                forge_provider_api::FailureDisposition::Uncertain,
            )
        }
        AcpConnectionError::Callback(_) => (
            ProviderFailureKind::Unknown,
            forge_provider_api::ProviderPhase::Observation,
            forge_provider_api::FailureDisposition::Uncertain,
        ),
        AcpConnectionError::Configuration(_) => (
            ProviderFailureKind::Rejected,
            forge_provider_api::ProviderPhase::Admission,
            forge_provider_api::FailureDisposition::Unchanged,
        ),
        AcpConnectionError::Process { .. } | AcpConnectionError::Startup { .. } => (
            ProviderFailureKind::Runtime,
            forge_provider_api::ProviderPhase::Preparation,
            forge_provider_api::FailureDisposition::Unchanged,
        ),
        AcpConnectionError::Collection { process, reader } => {
            let normalized = |disposition| {
                ProviderError::Failure(
                    forge_provider_api::ProviderFailure::new(
                        operation,
                        ProviderFailureKind::Runtime,
                    )
                    .at_phase(forge_provider_api::ProviderPhase::Collection, disposition),
                )
            };
            return match (process, reader) {
                (Some(_), Some(_)) => ProviderError::Combined {
                    primary: Box::new(normalized(
                        forge_provider_api::FailureDisposition::Invalidated,
                    )),
                    secondary: vec![normalized(forge_provider_api::FailureDisposition::Closed)],
                },
                (Some(_), None) => normalized(forge_provider_api::FailureDisposition::Invalidated),
                (None, Some(_)) | (None, None) => {
                    normalized(forge_provider_api::FailureDisposition::Closed)
                }
            };
        }
    };
    ProviderError::Failure(
        forge_provider_api::ProviderFailure::new(operation, kind).at_phase(phase, disposition),
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn native_error_conversion_drops_payloads_and_source_chains() {
        let failures = [
            (
                codex(
                    CodexConnectionError::Closed("native-secret-code-payload".into()),
                    ProviderOperation::TurnStart,
                ),
                ProviderFailureKind::Transport,
            ),
            (
                acp(
                    AcpConnectionError::Remote {
                        method: "native-secret-method".into(),
                        failure: None,
                        source: Box::new(std::io::Error::other("native-secret-code-payload")),
                    },
                    ProviderOperation::TurnStart,
                ),
                ProviderFailureKind::Unknown,
            ),
            (
                acp(
                    AcpConnectionError::Remote {
                        method: "authenticate".into(),
                        failure: Some(AcpRemoteFailure::AuthenticationRequired),
                        source: Box::new(std::io::Error::other("native auth detail")),
                    },
                    ProviderOperation::TurnStart,
                ),
                ProviderFailureKind::Authentication,
            ),
            (
                acp(
                    AcpConnectionError::Remote {
                        method: "session/prompt".into(),
                        failure: Some(AcpRemoteFailure::Rejected),
                        source: Box::new(std::io::Error::other("native rejection detail")),
                    },
                    ProviderOperation::TurnStart,
                ),
                ProviderFailureKind::Rejected,
            ),
        ];
        for (error, expected) in failures {
            assert!(
                matches!(&error, ProviderError::Failure(failure) if failure.kind() == expected && failure.operation() == ProviderOperation::TurnStart)
            );
            let mut source: Option<&(dyn std::error::Error + 'static)> = Some(&error);
            while let Some(cause) = source {
                assert!(!cause.to_string().contains("native-secret"));
                assert!(!cause.is::<std::io::Error>());
                source = cause.source();
            }
            assert!(!format!("{error:?}").contains("native-secret"));
        }
        assert!(matches!(
            acp(AcpConnectionError::Remote {
                method: "session/prompt".into(),
                failure: Some(AcpRemoteFailure::RequestCancelled),
                source: Box::new(std::io::Error::other("native cancellation detail")),
            }, ProviderOperation::TurnStart),
            ProviderError::Failure(failure) if failure.disposition() == forge_provider_api::FailureDisposition::Uncertain
        ));
    }
}
