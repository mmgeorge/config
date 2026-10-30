use serde::{Deserialize, Serialize};

/// Authorize only the next submitted revision produced from accepted review comments.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum ReviewRevisionApproval {
    /// Retain execution choices while the planning agent incorporates review feedback.
    StartExecution {
        reviewed_revision: u32,
        execution: crate::task::ExecutionEntry,
        comment: Option<String>,
    },
    /// Resume the existing execution after its pending revision incorporates feedback.
    ResumeExecution { reviewed_revision: u32 },
}

impl ReviewRevisionApproval {
    /// Check that submission advances exactly the revision authorized by this review.
    pub(crate) fn authorizes(&self, revision: u32) -> bool {
        let reviewed_revision = match self {
            Self::StartExecution {
                reviewed_revision, ..
            }
            | Self::ResumeExecution { reviewed_revision } => *reviewed_revision,
        };
        reviewed_revision.checked_add(1) == Some(revision)
    }
}
