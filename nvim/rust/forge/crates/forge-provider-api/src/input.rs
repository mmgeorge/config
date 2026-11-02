use serde::{Deserialize, Serialize};

/// The text or explicit skill selected by the caller, before native skill resolution.
#[derive(Clone, Debug)]
pub enum PromptInput {
    Text(String),
    Skill { name: String, arguments: String },
}

/// Render admitted but definitely undelivered input ahead of the newest instruction.
pub fn recovered_text(current: &str, previous: &[String]) -> String {
    if previous.is_empty() {
        return current.to_owned();
    }
    let mut text = String::from(
        "Earlier user input was saved locally but never accepted by the provider. Treat it as context for the newest instruction, which may amend or cancel it.\n",
    );
    for (index, input) in previous.iter().enumerate() {
        text.push_str(&format!(
            "\nEarlier undelivered input {}:\n{}\n",
            index + 1,
            input
        ));
    }
    text.push_str(&format!("\nNewest user input:\n{current}"));
    text
}

/// Carry prepared user text and explicitly selected files into a native provider adapter.
#[derive(Clone, Debug)]
pub struct MessageInput {
    /// User text and labels prepared by the input owner.
    pub prompt: String,
    /// Files already admitted and resolved by the input owner.
    pub attachments: Vec<FileAttachment>,
}

/// Identify one admitted file independently of a provider's attachment wire format.
#[derive(Clone, Debug)]
pub struct FileAttachment {
    /// Canonical imported file retained for the provider invocation.
    pub path: std::path::PathBuf,
    /// Label shared by the prompt and the native attachment.
    pub display_name: String,
}

#[derive(Clone, Copy, Debug, Deserialize, Serialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
/// Identifies the workflow independently of its permission boundary.
pub enum TaskKind {
    /// Creates a reviewable design.
    Plan,
    /// Implements an accepted design through its verification gates.
    Execute,
    /// Continues toward an explicit objective.
    Goal,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct TaskStart {
    pub kind: TaskKind,
    pub title: String,
}

impl TaskStart {
    pub fn heading(&self) -> String {
        let label = match self.kind {
            TaskKind::Goal => "Goal",
            TaskKind::Plan => "Plan",
            TaskKind::Execute => "Execution",
        };
        format!("{label} started: {}", self.title)
    }
}

/// Represents the broker intent for one admitted prompt.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum PromptMode {
    /// Run a persistent tool-free generation request.
    Background,
    Chat,
    Plan,
    PlanDiscussion,
    /// Answer an anchored review question in a fork without plan mutations.
    PlanQuestion,
    ExecutePlan,
    GoalContinuation,
    RequestChanges,
}
