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

    /// Render the task start into the provider input as host-prepared text.
    ///
    /// Providers receive no task record; the heading precedes the task's own input so every
    /// adapter delivers the same announcement. Structured inputs keep their native shape and
    /// carry no heading. An untitled task adds nothing.
    pub fn render_input(
        start: Option<&Self>,
        input: forge_provider_api::PromptInput,
    ) -> forge_provider_api::PromptInput {
        match (start, input) {
            (Some(start), forge_provider_api::PromptInput::Text(text))
                if !start.title.is_empty() =>
            {
                forge_provider_api::PromptInput::Text(format!("{}\n\n{text}", start.heading()))
            }
            (_, input) => input,
        }
    }
}
