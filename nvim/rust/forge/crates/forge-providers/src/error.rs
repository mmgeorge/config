use crate::acp::{AcpConnectionError, AcpRemoteFailure};
use crate::codex::CodexConnectionError;
use forge_provider_api::{ProviderError, SanitizedMessage};
use forge_provider_api::error::{ProviderFailureKind, ProviderOperation};

/// Render a native cause chain only at the diagnostic boundary, without debug payloads.
pub(crate) struct Diagnostic<'cause>(pub(crate) &'cause (dyn std::error::Error + 'static));

impl std::fmt::Display for Diagnostic<'_> {
    /// Preserve each source before provider normalization discards native details.
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.0, formatter)?;
        let mut cause = self.0.source();
        while let Some(error) = cause {
            write!(formatter, ": {error}")?;
            cause = error.source();
        }
        Ok(())
    }
}

/// Normalize a Codex transport failure at its requested operation boundary.
///
/// A connection this side released is the expected end of every binding's reader, so it is
/// recorded at debug level rather than as a provider failure; its waiters still fail.
pub(crate) fn codex(error: CodexConnectionError, operation: ProviderOperation) -> ProviderError {
    if matches!(error, CodexConnectionError::Released) {
        tracing::debug!(target: "forge::provider", event_name = "provider.connection_released",
            provider = "codex", operation = ?operation);
    } else {
        tracing::error!(target: "forge::provider", event_name = "provider.failure",
            provider = "codex", operation = ?operation, error = %Diagnostic(&error));
    }
    let kind = match error {
        CodexConnectionError::Overflow => {
            return ProviderError::Observation(
                forge_provider_api::lifecycle::ObservationFailure::Overflow,
            );
        }
        CodexConnectionError::Encoding(_) => ProviderFailureKind::Protocol,
        CodexConnectionError::Transport { .. }
        | CodexConnectionError::Closed(_)
        | CodexConnectionError::Released => ProviderFailureKind::Transport,
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
    let (summary, remote_failure, os_error) = match &error {
        AcpConnectionError::Configuration(message) | AcpConnectionError::Startup { message, .. } =>
            (*message, None, None),
        AcpConnectionError::Process { operation, source } =>
            (*operation, None, source.raw_os_error()),
        AcpConnectionError::StartupExit { .. } => ("ACP process exited during initialization", None, None),
        AcpConnectionError::Protocol { .. } => ("could not encode ACP request", None, None),
        AcpConnectionError::Remote { failure, .. } => ("ACP request failed", *failure, None),
        AcpConnectionError::Collection { .. } => ("ACP collection failed", None, None),
        AcpConnectionError::Callback(_) => ("ACP callback failed", None, None),
    };
    let explanation = match &error {
        AcpConnectionError::Remote {
            failure: Some(AcpRemoteFailure::AuthenticationRequired),
            ..
        } => Some(SanitizedMessage::new("Antigravity is not signed in. Run `agy acp --login` and retry.")),
        AcpConnectionError::StartupExit { reason } => Some(SanitizedMessage::new(reason.to_string())),
        _ => None,
    }.map(|explanation| explanation.expect("normalized ACP explanation is nonempty"));
    tracing::error!(target: "forge::provider", event_name = "provider.failure",
        provider = "antigravity", operation = ?operation, summary, remote_failure = ?remote_failure,
        os_error, explanation = explanation.as_ref().map(SanitizedMessage::as_str));
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
        AcpConnectionError::Process { .. }
        | AcpConnectionError::Startup { .. }
        | AcpConnectionError::StartupExit { .. } => (
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
    let cause = ProviderError::Failure(
        forge_provider_api::ProviderFailure::new(operation, kind).at_phase(phase, disposition),
    );
    match explanation {
        Some(explanation) => ProviderError::Explained {
            cause: Box::new(cause),
            explanation,
        },
        None => cause,
    }
}
