use serde::{Deserialize, Serialize};

/// Identifies who requested an exchange without retaining task ownership.
#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case", deny_unknown_fields)]
pub enum ExchangeOrigin {
    #[default]
    User,
    /// Observe provider-native child work without claiming a Harness task or user submission.
    NativeAgent,
    TaskStarted {
        start: TaskStart,
    },
    TaskContinuation {
        reason: String,
    },
}

impl ExchangeOrigin {
    pub fn task_continuation(reason: impl Into<String>) -> Self {
        Self::TaskContinuation {
            reason: reason.into(),
        }
    }
}

/// Record the task an exchange starts, independently of the text that announces it.
#[derive(Clone, Debug, serde::Deserialize, Eq, PartialEq, serde::Serialize)]
#[serde(deny_unknown_fields)]
pub struct TaskStart {
    pub kind: crate::task::TaskKind,
    pub title: String,
}

impl TaskStart {
    /// Name the task start in the form every provider receives ahead of the task's input.
    pub fn heading(&self) -> String {
        let label = match self.kind {
            crate::task::TaskKind::Goal => "Goal",
            crate::task::TaskKind::Plan => "Plan",
            crate::task::TaskKind::Execute => "Execution",
        };
        format!("{label} started: {}", self.title)
    }

    /// Announce the task start as a notice on the task's first input.
    ///
    /// Providers receive no task record, so every adapter delivers the same announcement inside
    /// the turn's `<instructions>`. An untitled task adds nothing.
    pub fn announce(start: Option<&Self>, input: &mut forge_provider_api::forge::TurnInput) {
        let Some(start) = start.filter(|start| !start.title.is_empty()) else {
            return;
        };
        let label = match start.kind {
            crate::task::TaskKind::Goal => "Goal",
            crate::task::TaskKind::Plan => "Plan",
            crate::task::TaskKind::Execute => "Execution",
        };
        input.push_notice(forge_provider_api::forge::Notice::TaskStarted {
            label: label.into(),
            title: start.title.clone(),
        });
    }
}
