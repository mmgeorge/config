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
