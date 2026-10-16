//! Preview availability and speculative admission do not change exact raw action targets.

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum BodyKind {
    Modified,
    Added,
    Deleted,
}

#[derive(Clone, Copy, Debug)]
pub struct BodyPolicy {
    pub prewarm_delta_exclusive: usize,
}

impl Default for BodyPolicy {
    fn default() -> Self {
        Self {
            prewarm_delta_exclusive: 100,
        }
    }
}

impl BodyPolicy {
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
            && known_source_bytes.is_some()
    }
}
