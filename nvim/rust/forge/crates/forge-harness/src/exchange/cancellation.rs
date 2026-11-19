//! Describe durable cancellation progress without carrying prompt or tool content.

use serde::{Deserialize, Serialize};

/// Identify the owner boundary that still prevents cancellation from settling.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum CancellationPhase {
    Acquiring,
    Collecting,
    Draining,
    Settling,
    Recovering,
    Settled,
    Failed,
}

impl CancellationPhase {
    /// Get whether this operation still owns cancellation or recovery work.
    pub fn active(self) -> bool {
        !matches!(self, Self::Settled | Self::Failed)
    }
}

/// Preserve safe blocker facts independently of optional tracing.
#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub struct CancellationBlockers {
    pub foreground: bool,
    pub running_jobs: usize,
    pub staged_effects: Vec<String>,
    pub queued_publications: usize,
    pub publication_active: bool,
    pub checks_running: bool,
    pub children: usize,
    pub input_delivery: bool,
    pub recovery_jobs: bool,
}

impl CancellationBlockers {
    /// Get whether any blocker is work that a cancellation stops or waits for.
    ///
    /// Queued and active publications are excluded: they deliver output already committed, and an
    /// idle owner flushes them whether or not anyone cancels.
    pub(crate) fn owns_work(&self) -> bool {
        let Self {
            foreground,
            running_jobs,
            staged_effects,
            queued_publications: _,
            publication_active: _,
            checks_running,
            children,
            input_delivery,
            recovery_jobs,
        } = self;
        *foreground
            || *running_jobs > 0
            || !staged_effects.is_empty()
            || *checks_running
            || *children > 0
            || *input_delivery
            || *recovery_jobs
    }
}

/// Keep operation progress separate from the immutable response to each cancel command.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct CancellationProgress {
    pub(crate) operation_id: super::command::CommandId,
    pub exchange_id: Option<crate::provider::ExchangeId>,
    pub revision: u64,
    pub phase: CancellationPhase,
    pub response_timed_out: bool,
    pub started_at_ms: i64,
    pub updated_at_ms: i64,
    pub blockers: CancellationBlockers,
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Progress survives disabled tracing and recovery retires an interrupted owner explicitly.
    #[test]
    fn cancellation_progress_is_durable_and_recorded() -> anyhow::Result<()> {
        let root = tempfile::tempdir()?;
        super::super::core_log::persist_test_session(root.path(), "session")?;
        let mut journal = super::super::journal::CommandJournal::open(root.path(), "session")?;
        let progress = CancellationProgress {
            operation_id: super::super::command::CommandId::new("cancel-one")?,
            exchange_id: Some("exchange-one".into()),
            revision: 3,
            phase: CancellationPhase::Draining,
            response_timed_out: true,
            started_at_ms: 100,
            updated_at_ms: 200,
            blockers: CancellationBlockers {
                foreground: true,
                running_jobs: 1,
                staged_effects: vec!["FinalCheckpoint".into()],
                ..Default::default()
            },
        };
        journal.append_core(&super::super::core_log::CoreRecord::CancellationProgress {
            progress: progress.clone(),
        })?;
        let reader = crate::storage::HistoryReader::current(root.path())?;
        assert_eq!(
            reader.cancellation_progress("session")?,
            Some(progress.clone())
        );
        forge_recorded_fixtures::assert_matches_recording(
            "cancellation-progress.json",
            &serde_json::to_value(progress)?,
        );
        journal.recover()?;
        let recovered = reader.cancellation_progress("session")?.unwrap();
        assert_eq!(recovered.phase, CancellationPhase::Failed);
        assert_eq!(recovered.revision, 4);
        assert!(recovered.response_timed_out);
        Ok(())
    }
}
