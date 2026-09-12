use crate::identity::{GitStorageId, WorktreeId};

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct OperationId(pub(crate) u64);

impl OperationId {
    pub fn from_value(value: u64) -> anyhow::Result<Self> {
        anyhow::ensure!(
            value <= 9_007_199_254_740_991,
            "invalid operation identifier"
        );
        Ok(Self(value))
    }

    pub fn value(self) -> u64 {
        self.0
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum MutationScope {
    Index(WorktreeId),
    WorktreeFiles(WorktreeId),
    SharedRefs(GitStorageId),
}

/// Records operation termination. Per-target write facts belong to the writer's outcome.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum OperationCompletion {
    Completed,
    Failed,
    CancelledBeforeStart,
    Uncertain,
}
