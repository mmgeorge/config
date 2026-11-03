use crate::acp::AcpConnectionError;
use crate::codex::CodexConnectionError;
use forge_provider_api::ProviderError;
use forge_provider_api::error::{ProviderFailureKind, ProviderOperation, ProviderOperationFailure};

impl From<crate::copilot::decode::DecodeError> for ProviderError {
    /// Preserve malformed native input as a classified protocol source.
    fn from(error: crate::copilot::decode::DecodeError) -> Self {
        Self::Operation(ProviderOperationFailure::native(
            ProviderOperation::NativeTransport,
            ProviderFailureKind::Protocol,
            std::sync::Arc::new(error),
        ))
    }
}
impl From<CodexConnectionError> for ProviderError {
    /// Preserve transport and decode causes while keeping overflow sticky and typed.
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
        Self::Operation(ProviderOperationFailure::native(
            ProviderOperation::NativeTransport,
            kind,
            std::sync::Arc::new(error),
        ))
    }
}
impl From<AcpConnectionError> for ProviderError {
    /// Preserve ACP protocol, peer, startup, process, and deadline causes.
    fn from(error: AcpConnectionError) -> Self {
        let kind = match &error {
            AcpConnectionError::Protocol { .. } => ProviderFailureKind::Protocol,
            AcpConnectionError::Remote { .. } | AcpConnectionError::Callback(_) => {
                ProviderFailureKind::Provider
            }
            AcpConnectionError::Configuration(_)
            | AcpConnectionError::Process { .. }
            | AcpConnectionError::Startup { .. }
            | AcpConnectionError::Timeout { .. } => ProviderFailureKind::Transport,
        };
        Self::Operation(ProviderOperationFailure::native(
            ProviderOperation::NativeTransport,
            kind,
            std::sync::Arc::new(error),
        ))
    }
}
