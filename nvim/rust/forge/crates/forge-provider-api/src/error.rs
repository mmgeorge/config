use crate::{events, retry};

#[derive(Debug)]
/// Identifies user cancellation across broker and task-coordinator error boundaries.
pub struct TurnCancelled;

impl std::fmt::Display for TurnCancelled {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str("Turn cancelled by user")
    }
}

impl std::error::Error for TurnCancelled {}

#[derive(Debug)]
pub enum ProviderError {
    Cancelled,
    Transport {
        message: String,
    },
    Protocol {
        message: String,
    },
    Provider {
        code: Option<String>,
        message: String,
    },
    /// The provider cannot find the conversation requested for resumption.
    SessionMissing {
        message: String,
    },
    Rejected {
        message: String,
    },
    Retryable(Box<retry::RetryableFailure>),
    Delivery(events::EventDeliveryError),
}

impl std::fmt::Display for ProviderError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Cancelled => formatter.write_str("provider execution cancelled"),
            Self::Transport { message }
            | Self::Protocol { message }
            | Self::Provider { message, .. }
            | Self::SessionMissing { message }
            | Self::Rejected { message } => formatter.write_str(message),
            Self::Retryable(error) => std::fmt::Display::fmt(error, formatter),
            Self::Delivery(error) => std::fmt::Display::fmt(error, formatter),
        }
    }
}

impl std::error::Error for ProviderError {}

impl From<events::EventDeliveryError> for ProviderError {
    fn from(error: events::EventDeliveryError) -> Self {
        Self::Delivery(error)
    }
}

impl From<anyhow::Error> for ProviderError {
    fn from(error: anyhow::Error) -> Self {
        let error = match error.downcast::<Self>() {
            Ok(error) => return error,
            Err(error) => error,
        };
        let error = match error.downcast::<events::EventDeliveryError>() {
            Ok(error) => return Self::Delivery(error),
            Err(error) => error,
        };
        let error = match error.downcast::<retry::RetryableFailure>() {
            Ok(error) => return Self::Retryable(Box::new(error)),
            Err(error) => error,
        };
        if error.is::<TurnCancelled>() {
            return Self::Cancelled;
        }
        Self::Provider {
            code: None,
            message: format!("{error:#}"),
        }
    }
}
