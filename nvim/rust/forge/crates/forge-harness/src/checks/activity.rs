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
    /// Selected checks whose commands all reached a terminal state, including skips and failures.
    pub completed_count: usize,
    /// Check definitions selected for this run, including required prerequisites.
    pub total_count: usize,
    /// Selected command states, timers, and latest output lines in execution order.
    pub command_list: Vec<CheckCommandActivity>,
}

impl CheckActivity {
    /// Capture live progress without changing the main workflow phase or reading output files.
    pub(crate) fn from_run(run: &CheckRun) -> Option<Self> {
        if run.state != CheckState::Running {
            return None;
        }
        let mut selected = std::collections::BTreeMap::new();
        for command in run.commands.iter().filter(|command| command.selected) {
            let id = command
                .context
                .as_ref()
                .map_or(command.id.as_str(), |context| {
                    context.definition_id.as_str()
                });
            let completed = !matches!(command.state, CheckState::Pending | CheckState::Running);
            selected
                .entry(id)
                .and_modify(|finished| *finished &= completed)
                .or_insert(completed);
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
            .filter(|command| command.selected)
            .map(|command| CheckCommandActivity {
                title: command.context.as_ref().map_or_else(
                    || {
                        crate::permissions::command::display_command(
                            &command.command,
                            crate::permissions::shell::CommandShell::default(),
                        )
                        .source
                    },
                    |context| match &context.selection {
                        super::CheckSelection::Tests { .. } => context.name.clone(),
                        _ => context.label(),
                    },
                ),
                selection: command.context.as_ref().and_then(|context| {
                    let super::CheckSelection::Tests { tests } = &context.selection else {
                        return None;
                    };
                    match tests.ids.as_slice() {
                        [] => None,
                        [test] => Some(test.display_name().to_owned()),
                        selected => Some(format!("{} tests selected", selected.len())),
                    }
                }),
                started_at_ms: command.started_at_ms,
                completed_at_ms: command.completed_at_ms,
                state: command.state,
                progress: command.adapter_progress.clone(),
                output_line: command
                    .error
                    .clone()
                    .or_else(|| super::output::latest_line(&command.preview)),
            })
            .collect();
        Some(Self {
            run_id: run.id.clone(),
            duration_ms,
            started_at_ms,
            completed_count: selected.values().filter(|completed| **completed).count(),
            total_count: selected.len(),
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
/// One selected command within the current check run.
pub struct CheckCommandActivity {
    /// Configured check name, file selection, or normalized command text.
    pub title: String,
    /// Compact selected test name or count, independent of its execution identity.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub selection: Option<String>,
    /// Command execution interval used by the status timer.
    pub started_at_ms: Option<i64>,
    /// Terminal execution boundary, which freezes the displayed duration.
    pub completed_at_ms: Option<i64>,
    /// Recorded lifecycle, including queued, completed, and unsuccessful commands.
    pub state: CheckState,
    /// Latest message with terminal escapes, diagnostic borders, and source excerpts removed.
    pub output_line: Option<String>,
    /// Structured progress from the active adapter contract.
    pub progress: Option<super::adapter::AdapterProgress>,
}

impl CheckCommandActivity {
    /// Return elapsed execution time when the command has started.
    pub(crate) fn elapsed_ms(&self, now_ms: i64) -> Option<u64> {
        self.started_at_ms.map(|started| {
            self.completed_at_ms
                .unwrap_or(now_ms)
                .saturating_sub(started)
                .max(0) as u64
        })
    }
}
