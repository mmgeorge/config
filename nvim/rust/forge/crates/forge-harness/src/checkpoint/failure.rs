use std::error::Error;
use std::fmt;

/// Preserve concrete checkpoint ownership failures at the engine cleanup boundary.
#[derive(Debug)]
pub(crate) enum CheckpointFailure {
    /// The existing host snapshot operation failed before producing a publishable record.
    ///
    /// Git discovery and immutable-object capture remain legacy host orchestration.
    /// Their original source chain crosses this adapter boundary without entering
    /// the provider or engine contracts as an anyhow value or a rendered string.
    SnapshotCapture {
        source: Box<dyn Error + Send + Sync>,
    },
    /// The owned checkpoint task panicked or ended unexpectedly while being collected.
    WorkerJoin { source: tokio::task::JoinError },
    /// The bounded foreground wait elapsed; its owner still retains the task handle.
    CollectionDeadline { source: tokio::time::error::Elapsed },
}

impl fmt::Display for CheckpointFailure {
    /// Describe the checkpoint phase while preserving its actual source error.
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::SnapshotCapture { source } => {
                write!(formatter, "checkpoint snapshot capture failed: {source}")
            }
            Self::WorkerJoin { source } => {
                write!(formatter, "checkpoint worker collection failed: {source}")
            }
            Self::CollectionDeadline { source } => write!(
                formatter,
                "checkpoint baseline collection timed out: {source}"
            ),
        }
    }
}

impl Error for CheckpointFailure {
    /// Keep legacy capture causes and native join/deadline errors inspectable.
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::SnapshotCapture { source } => Some(source.as_ref()),
            Self::WorkerJoin { source } => Some(source),
            Self::CollectionDeadline { source } => Some(source),
        }
    }
}
