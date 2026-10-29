use std::fmt;

/// Stops automatic retries after a transcript cannot produce a valid document.
///
/// The presentation must be reopened before publication can resume. The owned
/// diagnostic remains available through error context at the host boundary.
#[derive(Debug)]
pub struct TranscriptProjectionFailure {
    message: String,
}

impl TranscriptProjectionFailure {
    /// Retains the diagnostic for a presentation that requires reopening.
    pub fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
        }
    }
}

impl fmt::Display for TranscriptProjectionFailure {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            formatter,
            "transcript projection requires reopening: {}",
            self.message
        )
    }
}

impl std::error::Error for TranscriptProjectionFailure {}
