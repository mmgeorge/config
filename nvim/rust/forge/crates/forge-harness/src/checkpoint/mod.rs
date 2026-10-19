mod checkpoint;
mod gate;
mod manifest;
mod restore;

pub use checkpoint::{GitCheckpoint, checkpoint_diff, checkpoint_diff_for_paths};
pub(crate) use gate::CheckpointTask;
pub use gate::{ToolGate, wait_hook};
pub use manifest::{CheckpointFile, CheckpointRecord};

pub(crate) use restore::{RestoreJournal, RestorePreview, RestoreState};
