use super::{CheckRun, CheckState};
use serde::Serialize;

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
/// Active check progress independent of the provider or plan phase.
pub struct CheckActivity {
    /// Durable run whose progress is currently visible.
    pub run_id: String,
    /// Run time already elapsed when the run was suspended; zero while it advances.
    pub duration_ms: u64,
    /// Start of the run's execution interval, absent while an interrupted run is suspended.
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
        // The run is one execution interval from its start: discovery, scans, and the gaps
        // between commands are check work, so the timer never restarts when a command starts.
        // An interrupted command suspends the run at its last recorded boundary.
        let (duration_ms, started_at_ms) = if run
            .commands
            .iter()
            .any(|command| command.state == CheckState::Interrupted)
        {
            let suspended_at_ms = run
                .commands
                .iter()
                .filter_map(|command| command.completed_at_ms.or(command.started_at_ms))
                .max()
                .unwrap_or(run.started_at_ms);
            (
                suspended_at_ms.saturating_sub(run.started_at_ms).max(0) as u64,
                None,
            )
        } else {
            (0, Some(run.started_at_ms))
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

#[cfg(test)]
mod tests {
    use super::*;

    /// Build a running run that started at zero whose first command started at `started_at_ms`.
    fn running_run(started_at_ms: Option<i64>) -> CheckRun {
        let state = if started_at_ms.is_some() {
            "running"
        } else {
            "pending"
        };
        serde_json::from_value(serde_json::json!({
            "id": "run", "owner": {"kind": "session", "session_id": "session"}, "revision": 0,
            "shell": "system", "executable": null, "workspace": ".", "environment_digest": "fixture",
            "started_at_ms": 0, "completed_at_ms": null, "state": "running",
            "progress": crate::plan::execution::SemanticProgress::default(),
            "workspace_identity": null, "workspace_digest": {},
            "commands": [
                {"id":"typecheck", "command":"typecheck", "state":state, "selected":true,
                 "started_at_ms":started_at_ms, "completed_at_ms":null, "exit_code":null,
                 "error":null, "stdout":"unused", "stderr":"unused", "output":"unused",
                 "output_bytes":0, "preview":""}
            ]
        }))
        .unwrap()
    }

    /// The run timer counts from the run's start, so the preparation before the first command
    /// is not discarded when that command starts.
    #[test]
    fn the_run_timer_continues_when_the_first_command_starts() {
        let before = CheckActivity::from_run(&running_run(None)).unwrap();
        assert_eq!(before.elapsed_ms(1_400), 1_400);
        let after = CheckActivity::from_run(&running_run(Some(1_200))).unwrap();
        assert_eq!(after.elapsed_ms(1_400), 1_400);
    }
}
