//! Preview availability and speculative admission do not change exact raw action targets.

use crate::source::{MAX_SOURCE_BYTES, SourceCoordinate};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum BodyKind {
    Modified,
    Added,
    Deleted,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum UnavailableReason {
    FullFileLines {
        lines: usize,
        limit: usize,
    },
    LineBytes {
        coordinate: SourceCoordinate,
        line_bytes: usize,
        limit: usize,
    },
}

#[derive(Clone, Copy, Debug)]
pub struct BodyPolicy {
    pub full_file_lines: usize,
    pub prewarm_delta_exclusive: usize,
}

impl Default for BodyPolicy {
    fn default() -> Self {
        Self {
            full_file_lines: 1000,
            prewarm_delta_exclusive: 100,
        }
    }
}

impl BodyPolicy {
    /// Explicit modified-file expansion is independent of the full added/deleted preview limit.
    pub fn may_preview_full_file(
        &self,
        kind: BodyKind,
        lines: usize,
    ) -> Result<(), UnavailableReason> {
        let limit = self.full_file_lines.min(1000);
        if kind != BodyKind::Modified && lines > limit {
            Err(UnavailableReason::FullFileLines { lines, limit })
        } else {
            Ok(())
        }
    }

    /// Unknown observations never authorize speculative acquisition or count computation.
    /// This eligibility decision does not reserve worker capacity or admit source ownership.
    pub fn may_prewarm(
        &self,
        kind: BodyKind,
        known_delta: Option<usize>,
        known_source_bytes: Option<(usize, usize)>,
        speculative_capacity: bool,
    ) -> bool {
        kind != BodyKind::Deleted
            && speculative_capacity
            && known_delta.is_some_and(|delta| delta < self.prewarm_delta_exclusive.min(100))
            && known_source_bytes
                .is_some_and(|(old, new)| old <= MAX_SOURCE_BYTES && new <= MAX_SOURCE_BYTES)
    }
}
