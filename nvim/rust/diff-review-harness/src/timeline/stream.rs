use anyhow::{Result, ensure};
use serde::Serialize;
use serde_json::Value;

use super::TimelineEntry;

/// Represents one atomic change to a session's projected timeline.
#[derive(Clone, Debug, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum TimelineOperation {
    Insert { index: usize, entry: TimelineEntry },
    Replace { index: usize, entry: TimelineEntry },
    Remove { index: usize, id: String },
}

/// Represents one causally ordered timeline revision for a single session.
#[derive(Clone, Debug, Serialize)]
pub struct TimelinePatch {
    pub session_id: String,
    pub base_revision: u64,
    pub revision: u64,
    pub operation: Vec<TimelineOperation>,
}

impl TimelinePatch {
    /// Report whether this revision carries any visible timeline mutation.
    pub fn is_empty(&self) -> bool {
        self.operation.is_empty()
    }
}

/// Owns the last projected timeline and its monotonically increasing revision.
pub struct TimelineStream {
    session_id: String,
    revision: u64,
    entry_list: Vec<TimelineEntry>,
    value_list: Vec<Value>,
}

impl TimelineStream {
    /// Create an empty revision stream for one durable Harness session.
    pub fn new(session_id: String) -> Self {
        Self {
            session_id,
            revision: 0,
            entry_list: Vec::new(),
            value_list: Vec::new(),
        }
    }

    /// Initialize the stream from the full snapshot returned to a new client.
    pub fn initialize(&mut self, entry_list: Vec<TimelineEntry>) -> Result<()> {
        ensure!(self.revision == 0, "timeline stream is already initialized");
        self.value_list = serialize_entry_list(&entry_list)?;
        self.entry_list = entry_list;
        self.revision = 1;
        Ok(())
    }

    /// Return the current session-local timeline revision.
    pub fn revision(&self) -> u64 {
        self.revision
    }

    /// Resolve the current canonical entry list for targeted Rust projections.
    pub fn entry_list(&self) -> &[TimelineEntry] {
        &self.entry_list
    }

    /// Reconcile one canonical projection into ordered top-level operations.
    pub fn reconcile(&mut self, next_entry_list: Vec<TimelineEntry>) -> Result<TimelinePatch> {
        let base_revision = self.revision;
        let next_value_list = serialize_entry_list(&next_entry_list)?;
        let mut working_entry_list = self.entry_list.clone();
        let mut working_value_list = self.value_list.clone();
        let mut operation = Vec::new();

        for index in (0..working_entry_list.len()).rev() {
            let id = working_entry_list[index].id();
            if !next_entry_list.iter().any(|entry| entry.id() == id) {
                working_entry_list.remove(index);
                working_value_list.remove(index);
                operation.push(TimelineOperation::Remove { index, id });
            }
        }

        for (index, next_entry) in next_entry_list.iter().enumerate() {
            let next_id = next_entry.id();
            if working_entry_list
                .get(index)
                .is_some_and(|entry| entry.id() == next_id)
            {
                if working_value_list[index] != next_value_list[index] {
                    working_entry_list[index] = next_entry.clone();
                    working_value_list[index] = next_value_list[index].clone();
                    operation.push(TimelineOperation::Replace {
                        index,
                        entry: next_entry.clone(),
                    });
                }
                continue;
            }

            if let Some(previous_index) = working_entry_list
                .iter()
                .position(|entry| entry.id() == next_id)
            {
                let removed_id = working_entry_list[previous_index].id();
                working_entry_list.remove(previous_index);
                working_value_list.remove(previous_index);
                operation.push(TimelineOperation::Remove {
                    index: previous_index,
                    id: removed_id,
                });
            }
            working_entry_list.insert(index, next_entry.clone());
            working_value_list.insert(index, next_value_list[index].clone());
            operation.push(TimelineOperation::Insert {
                index,
                entry: next_entry.clone(),
            });
        }

        ensure!(
            working_value_list == next_value_list,
            "timeline reconciliation did not converge"
        );
        if !operation.is_empty() {
            self.revision += 1;
            self.entry_list = next_entry_list;
            self.value_list = next_value_list;
        }
        Ok(TimelinePatch {
            session_id: self.session_id.clone(),
            base_revision,
            revision: self.revision,
            operation,
        })
    }
}

fn serialize_entry_list(entry_list: &[TimelineEntry]) -> Result<Vec<Value>> {
    entry_list
        .iter()
        .map(serde_json::to_value)
        .collect::<Result<Vec<_>, _>>()
        .map_err(Into::into)
}

#[cfg(test)]
mod test {
    use super::{TimelineOperation, TimelineStream};
    use crate::{
        session::state_machine::{SessionPhase, WorkflowActivity},
        timeline::TimelineEntry,
    };

    fn status(kind: SessionPhase) -> TimelineEntry {
        TimelineEntry::Status {
            id: "session:status".into(),
            created_at_ms: 0,
            status: kind,
        }
    }

    #[test]
    fn replaces_one_stable_status_entry_without_resending_the_frame() {
        let mut stream = TimelineStream::new("session".into());
        stream
            .initialize(vec![status(SessionPhase::Working {
                started_at_ms: 10,
                activity: WorkflowActivity::Working,
            })])
            .unwrap();

        let patch = stream
            .reconcile(vec![status(SessionPhase::AwaitingPlanReview {
                plan_id: "plan".into(),
                revision: 2,
            })])
            .unwrap();

        assert_eq!(patch.base_revision, 1);
        assert_eq!(patch.revision, 2);
        assert!(matches!(
            patch.operation.as_slice(),
            [TimelineOperation::Replace { index: 0, .. }]
        ));
    }

    #[test]
    fn unchanged_projection_does_not_advance_the_revision() {
        let mut stream = TimelineStream::new("session".into());
        stream.initialize(vec![status(SessionPhase::Idle)]).unwrap();
        let patch = stream.reconcile(vec![status(SessionPhase::Idle)]).unwrap();
        assert!(patch.is_empty());
        assert_eq!(patch.base_revision, patch.revision);
    }

    #[test]
    fn sessions_advance_independent_revision_sequences() {
        let mut first = TimelineStream::new("first".into());
        let mut second = TimelineStream::new("second".into());
        first.initialize(vec![status(SessionPhase::Idle)]).unwrap();
        second.initialize(vec![status(SessionPhase::Idle)]).unwrap();

        let first_patch = first
            .reconcile(vec![status(SessionPhase::Working {
                started_at_ms: 5,
                activity: WorkflowActivity::Working,
            })])
            .unwrap();

        assert_eq!(first_patch.session_id, "first");
        assert_eq!(first.revision(), 2);
        assert_eq!(second.revision(), 1);
    }
}
