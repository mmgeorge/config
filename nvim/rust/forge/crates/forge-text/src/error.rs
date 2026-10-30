/// Distinguish unavailable profiles, malformed inputs, and failed local model work.
#[derive(Debug, thiserror::Error)]
pub enum TextError {
    #[error("invalid text correction input: {0}")]
    InvalidInput(String),
    #[error("invalid dictionary row {line}: {message}")]
    Dictionary { line: usize, message: String },
    #[error("text profile is unavailable: {0}")]
    Profile(String),
    #[error("text model I/O failed: {0}")]
    Io(#[from] std::io::Error),
    #[error("text model checksum does not match its manifest")]
    Checksum,
    #[error("text correction service is closed")]
    Closed,
    #[error("text worker failed: {0}")]
    Worker(String),
}
