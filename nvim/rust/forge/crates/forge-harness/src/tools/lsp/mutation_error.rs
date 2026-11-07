use std::path::PathBuf;

/// Distinguish rename admission, preparation, commit, and recovery failures.
#[derive(Debug, thiserror::Error)]
pub(crate) enum MutationError {
    #[error("{0}")]
    Retained(#[source] std::sync::Arc<MutationError>),
    #[error("invalid_argument: {0}")]
    Argument(&'static str),
    #[error("access_denied: {0}")]
    Access(&'static str),
    #[error("access_denied: {}", .0.display())]
    PathDenied(PathBuf),
    #[error("invalid_edit: {0}")]
    InvalidEdit(&'static str),
    #[error("source_changed: {}", .0.display())]
    SourceChanged(PathBuf),
    #[error("destination_exists: {}", .0.display())]
    DestinationExists(PathBuf),
    #[error("path_not_found: {}", .0.display())]
    PathMissing(PathBuf),
    #[error("unsupported: {}", .0.display())]
    UnsupportedPath(PathBuf),
    #[error("rename I/O failed: {0}")]
    Io(#[from] std::io::Error),
    #[error("invalid rename payload: {0}")]
    Json(#[from] serde_json::Error),
    #[error("editor or permission service failed: {0}")]
    Service(#[source] anyhow::Error),
    #[error("rename worker failed: {0}")]
    Worker(#[from] tokio::task::JoinError),
    #[error("rollback_failed: {path}: {rollback}; original failure: {cause}")]
    Rollback {
        path: String,
        rollback: std::io::Error,
        cause: Box<MutationError>,
    },
    #[error("rename committed but change publication failed: {0}")]
    Publication(#[source] crate::provider::ProviderError),
    #[error("rename permission lock poisoned")]
    Poisoned,
}

impl From<MutationError> for crate::tools::error::ToolError {
    fn from(error: MutationError) -> Self {
        Self::Operation(Box::new(error))
    }
}

pub(super) type Result<T> = std::result::Result<T, MutationError>;
