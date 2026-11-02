use crate::acp::AcpConnectionError;
use crate::codex::CodexConnectionError;
use forge_provider_api::ProviderError;

impl From<crate::copilot::decode::DecodeError> for ProviderError {
    fn from(error: crate::copilot::decode::DecodeError) -> Self {
        Self::Protocol {
            message: error.to_string(),
        }
    }
}

impl From<CodexConnectionError> for ProviderError {
    fn from(error: CodexConnectionError) -> Self {
        match error {
            error @ CodexConnectionError::Encoding(_) => Self::Protocol {
                message: error.to_string(),
            },
            error @ (CodexConnectionError::Transport { .. } | CodexConnectionError::Closed(_)) => {
                Self::Transport {
                    message: error.to_string(),
                }
            }
        }
    }
}

impl From<AcpConnectionError> for ProviderError {
    fn from(error: AcpConnectionError) -> Self {
        match error {
            error @ AcpConnectionError::Protocol { .. } => Self::Protocol {
                message: error.to_string(),
            },
            error @ AcpConnectionError::Remote { .. } | error @ AcpConnectionError::Callback(_) => {
                Self::Provider {
                    code: None,
                    message: error.to_string(),
                }
            }
            error @ (AcpConnectionError::Configuration(_)
            | AcpConnectionError::Process { .. }
            | AcpConnectionError::Startup { .. }
            | AcpConnectionError::Timeout { .. }) => Self::Transport {
                message: error.to_string(),
            },
        }
    }
}
