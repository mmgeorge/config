use crate::backend::{ProviderChangeSet, ToolActivity};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Execution state independent of provider-specific status spelling.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ToolState {
    Running,
    Completed,
    Failed,
    Cancelled,
    Interrupted,
}

/// Owns one provider tool call across progress, output, and completion events.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ToolCall {
    #[serde(default)]
    pub task_id: Option<String>,
    pub id: String,
    pub kind: String,
    pub title: String,
    pub output: String,
    pub status: String,
    pub failed: bool,
    #[serde(default)]
    pub change: ProviderChangeSet,
}

impl ToolCall {
    /// Resolve the provider status into a stable execution state.
    pub fn state(&self) -> ToolState {
        match self.status.to_ascii_lowercase().as_str() {
            "completed" | "complete" | "success" | "succeeded" => ToolState::Completed,
            "failed" | "error" | "denied" | "declined" | "rejected" => ToolState::Failed,
            "cancelled" | "canceled" => ToolState::Cancelled,
            "interrupted" => ToolState::Interrupted,
            _ => ToolState::Running,
        }
    }
}

/// Stores tool lifecycle records in first-seen provider order.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct ToolStore {
    order: Vec<String>,
    item: HashMap<String, ToolCall>,
}

impl ToolStore {
    /// Visit calls in first-seen provider order.
    pub(crate) fn iter(&self) -> impl Iterator<Item = &ToolCall> {
        self.order.iter().filter_map(|id| self.item.get(id))
    }

    /// Settle unfinished calls without replacing provider-reported terminal outcomes.
    pub(crate) fn finish(&mut self, outcome: super::TurnOutcome) {
        let status = match outcome {
            super::TurnOutcome::Failed => "failed",
            super::TurnOutcome::Cancelled => "cancelled",
            // A completed turn does not prove an unreported tool succeeded.
            super::TurnOutcome::Completed | super::TurnOutcome::Interrupted => "interrupted",
        };
        for tool in self.item.values_mut() {
            if tool.state() == ToolState::Running {
                tool.status = status.into();
                tool.failed = outcome == super::TurnOutcome::Failed;
            }
        }
    }

    /// Assign a call to its first observed task without changing historical attribution.
    pub(crate) fn attribute(&mut self, id: &str, task_id: &str) -> bool {
        let Some(tool) = self.item.get_mut(id) else {
            return false;
        };
        if tool.task_id.is_some() {
            return false;
        }
        tool.task_id = Some(task_id.to_owned());
        true
    }

    pub(crate) fn merge(&mut self, activity: &ToolActivity) {
        if !self.item.contains_key(&activity.id) {
            self.order.push(activity.id.clone());
        }
        let tool = self
            .item
            .entry(activity.id.clone())
            .or_insert_with(|| ToolCall {
                task_id: None,
                id: activity.id.clone(),
                kind: tool_kind(activity),
                title: activity.title.clone(),
                output: String::new(),
                status: activity
                    .status
                    .clone()
                    .unwrap_or_else(|| "inProgress".into()),
                failed: false,
                change: ProviderChangeSet::default(),
            });
        if !activity.title.is_empty()
            && !matches!(activity.title.as_str(), "command" | "file changes" | "tool")
        {
            tool.title.clone_from(&activity.title);
        }
        if tool.state() == ToolState::Running
            && let Some(status) = activity.status.as_ref()
        {
            tool.status.clone_from(status);
        }
        if let Some(output) = activity.output.as_ref() {
            if activity.output_delta {
                tool.output.push_str(output);
            } else {
                tool.output.clone_from(output);
            }
        }
        if !activity.change.is_empty() {
            tool.change.clone_from(&activity.change);
        }
        tool.failed = tool_failed(&tool.status, &tool.output);
    }

    pub(crate) fn get(&self, id: &str) -> Option<&ToolCall> {
        self.item.get(id)
    }
}

fn tool_kind(activity: &ToolActivity) -> String {
    serde_json::to_value(activity.kind)
        .ok()
        .and_then(|value| value.as_str().map(str::to_owned))
        .unwrap_or_else(|| "tool_call".into())
}

fn tool_failed(status: &str, output: &str) -> bool {
    matches!(
        status.to_ascii_lowercase().as_str(),
        "failed" | "error" | "denied" | "declined" | "rejected" | "cancelled" | "canceled"
    ) || output.to_ascii_lowercase().contains(" rejected:")
}
