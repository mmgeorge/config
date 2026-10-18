use super::{CheckRun, CheckState};
use serde::Serialize;

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
/// Active check progress independent of the provider or plan phase.
pub struct CheckActivity {
    /// Durable run whose progress is currently visible.
    pub run_id: String,
    /// Execution time accumulated by completed commands.
    pub duration_ms: u64,
    /// Current execution interval, absent while an interrupted run is suspended.
    pub started_at_ms: Option<i64>,
    /// Commands with a recorded terminal state, including skips and failures.
    pub completed_count: usize,
    /// Commands selected for this run.
    pub total_count: usize,
    /// Running command labels and timers, without retained output.
    pub command_list: Vec<CheckCommandActivity>,
}

impl CheckActivity {
    /// Capture live progress without changing the main workflow phase or reading output files.
    pub(crate) fn from_run(run: &CheckRun) -> Option<Self> {
        if run.state != CheckState::Running {
            return None;
        }
        let duration_ms = run
            .commands
            .iter()
            .filter(|command| {
                matches!(
                    command.state,
                    CheckState::Passed | CheckState::Failed | CheckState::Blocked
                )
            })
            .filter_map(|command| command.elapsed_ms(0))
            .fold(0_u64, u64::saturating_add);
        let started_at_ms = if let Some(command) = run
            .commands
            .iter()
            .find(|command| command.state == CheckState::Running)
        {
            command.started_at_ms
        } else if run
            .commands
            .iter()
            .any(|command| command.state == CheckState::Interrupted)
        {
            None
        } else {
            run.commands
                .iter()
                .filter_map(|command| command.completed_at_ms)
                .max()
                .or(Some(run.started_at_ms))
        };
        let command_list = run
            .commands
            .iter()
            .filter(|command| command.state == CheckState::Running)
            .map(|command| CheckCommandActivity {
                title: command.context.as_ref().map_or_else(
                    || {
                        crate::permissions::command::display_command(
                            &command.command,
                            crate::permissions::shell::CommandShell::default(),
                        )
                        .source
                    },
                    |context| context.name.clone(),
                ),
                started_at_ms: command.started_at_ms,
            })
            .collect();
        Some(Self {
            run_id: run.id.clone(),
            duration_ms,
            started_at_ms,
            completed_count: run
                .commands
                .iter()
                .filter(|command| {
                    !matches!(command.state, CheckState::Pending | CheckState::Running)
                })
                .count(),
            total_count: run.commands.len(),
            command_list,
        })
    }

    /// Advance the current execution interval while retaining accumulated work.
    pub(crate) fn elapsed_ms(&self, now_ms: i64) -> u64 {
        self.duration_ms.saturating_add(
            self.started_at_ms
                .map_or(0, |started| now_ms.saturating_sub(started).max(0) as u64),
        )
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
/// One live command within the current check run.
pub struct CheckCommandActivity {
    /// Complete configured check name or normalized command text.
    pub title: String,
    /// Command execution interval used by the status timer.
    pub started_at_ms: Option<i64>,
}

impl CheckCommandActivity {
    /// Return elapsed execution time when the command has started.
    pub(crate) fn elapsed_ms(&self, now_ms: i64) -> Option<u64> {
        self.started_at_ms
            .map(|started| now_ms.saturating_sub(started).max(0) as u64)
    }
}
