use crate::acp::{AcpConnectionError, AcpRemoteFailure};
use crate::codex::CodexConnectionError;
use forge_provider_api::ProviderError;
use forge_provider_api::error::{ProviderFailureKind, ProviderOperation};

impl From<CodexConnectionError> for ProviderError {
    /// Classify native failures without retaining a provider error or source chain.
    fn from(error: CodexConnectionError) -> Self {
        let kind = match &error {
            CodexConnectionError::Overflow => {
                return Self::Observation(
                    forge_provider_api::lifecycle::ObservationFailure::Overflow,
                );
            }
            CodexConnectionError::Encoding(_) => ProviderFailureKind::Protocol,
            CodexConnectionError::Transport { .. } | CodexConnectionError::Closed(_) => {
                ProviderFailureKind::Transport
            }
        };
        Self::Failure(forge_provider_api::ProviderFailure::new(
            ProviderOperation::NativeTransport,
            kind,
        ))
    }
}
impl From<AcpConnectionError> for ProviderError {
    /// Normalize ACP failures without exposing provider text, codes, payloads, or source chains.
    fn from(error: AcpConnectionError) -> Self {
        let kind = match error {
            AcpConnectionError::Protocol { .. } => ProviderFailureKind::Protocol,
            AcpConnectionError::Remote { failure, .. } => match failure {
                Some(AcpRemoteFailure::AuthenticationRequired) => {
                    ProviderFailureKind::Authentication
                }
                Some(AcpRemoteFailure::RequestCancelled) => return Self::Cancelled,
                Some(AcpRemoteFailure::ResourceNotFound) => ProviderFailureKind::SessionMissing,
                Some(AcpRemoteFailure::Rejected) => ProviderFailureKind::Rejected,
                Some(AcpRemoteFailure::Protocol) => ProviderFailureKind::Protocol,
                None => ProviderFailureKind::Unknown,
            },
            AcpConnectionError::Callback(_) => ProviderFailureKind::Unknown,
            AcpConnectionError::Configuration(_) => ProviderFailureKind::Rejected,
            AcpConnectionError::Process { .. } | AcpConnectionError::Startup { .. } => {
                ProviderFailureKind::Runtime
            }
            AcpConnectionError::Timeout { .. } => ProviderFailureKind::Timeout,
        };
        Self::Failure(forge_provider_api::ProviderFailure::new(
            ProviderOperation::NativeTransport,
            kind,
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn native_error_conversion_drops_payloads_and_source_chains() {
        let timeout_source =
            tokio::time::timeout(std::time::Duration::ZERO, std::future::pending::<()>())
                .await
                .expect_err("a zero-duration timeout must expire");
        let failures = [
            (
                ProviderError::from(CodexConnectionError::Closed(
                    "native-secret-code-payload".into(),
                )),
                ProviderFailureKind::Transport,
            ),
            (
                ProviderError::from(AcpConnectionError::Remote {
                    method: "native-secret-method".into(),
                    failure: None,
                    source: Box::new(std::io::Error::other("native-secret-code-payload")),
                }),
                ProviderFailureKind::Unknown,
            ),
            (
                ProviderError::from(AcpConnectionError::Timeout {
                    method: "native-secret-method".into(),
                    seconds: 1,
                    source: timeout_source,
                }),
                ProviderFailureKind::Timeout,
            ),
            (
                ProviderError::from(AcpConnectionError::Remote {
                    method: "authenticate".into(),
                    failure: Some(AcpRemoteFailure::AuthenticationRequired),
                    source: Box::new(std::io::Error::other("native auth detail")),
                }),
                ProviderFailureKind::Authentication,
            ),
            (
                ProviderError::from(AcpConnectionError::Remote {
                    method: "session/prompt".into(),
                    failure: Some(AcpRemoteFailure::Rejected),
                    source: Box::new(std::io::Error::other("native rejection detail")),
                }),
                ProviderFailureKind::Rejected,
            ),
        ];
        for (error, expected) in failures {
            assert!(
                matches!(&error, ProviderError::Failure(failure) if failure.kind() == expected)
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
            ProviderError::from(AcpConnectionError::Remote {
                method: "session/prompt".into(),
                failure: Some(AcpRemoteFailure::RequestCancelled),
                source: Box::new(std::io::Error::other("native cancellation detail")),
            }),
            ProviderError::Cancelled
        ));
    }
}
