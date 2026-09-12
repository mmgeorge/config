use crate::backend::{BackendEvent, ProviderChangeSet, ToolActivity};
use crate::interaction::change::ProviderDiffBuilder;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Stores tool lifecycle records in first-seen provider order.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct ToolStore {
    order: Vec<String>,
    item: HashMap<String, CompletedTool>,
}

impl ToolStore {
    fn merge(&mut self, activity: &ToolActivity) {
        if !self.item.contains_key(&activity.id) {
            self.order.push(activity.id.clone());
        }
        let tool = self
            .item
            .entry(activity.id.clone())
            .or_insert_with(|| CompletedTool {
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
        if let Some(status) = activity.status.as_ref() {
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

    fn len(&self) -> usize {
        self.order.len()
    }

    fn is_empty(&self) -> bool {
        self.order.is_empty()
    }

    fn failed_count(&self) -> usize {
        self.item.values().filter(|tool| tool.failed).count()
    }

    fn get(&self, id: &str) -> Option<&CompletedTool> {
        self.item.get(id)
    }

    fn into_list(mut self) -> Vec<CompletedTool> {
        self.order
            .into_iter()
            .filter_map(|id| self.item.remove(&id))
            .collect()
    }
}

/// Represents the one non-expandable thought currently receiving streamed activity.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ActiveThought {
    pub id: String,
    pub text: String,
    pub synthetic: bool,
    pub started_at_ms: i64,
    pub tool: ToolStore,
    pub latest_tool_id: Option<String>,
}

/// Represents the compact state rendered while one thought remains active.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ActiveThoughtUpdate {
    pub interaction_id: String,
    pub thought_id: String,
    pub text: String,
    pub synthetic: bool,
    pub tool_count: usize,
    pub failed_count: usize,
    pub latest_tool: Option<CompletedTool>,
    pub revision: u64,
}

/// Represents one immutable tool after its owning thought reaches a boundary.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct CompletedTool {
    pub id: String,
    pub kind: String,
    pub title: String,
    pub output: String,
    pub status: String,
    pub failed: bool,
    #[serde(default)]
    pub change: ProviderChangeSet,
}

/// Represents one immutable thought and the workspace delta attributed to it.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct CompletedThought {
    pub id: String,
    pub text: String,
    pub synthetic: bool,
    pub tool: Vec<CompletedTool>,
    pub started_at_ms: i64,
    pub completed_at_ms: i64,
    pub diff_text: Option<String>,
    #[serde(default)]
    pub task_id: Option<String>,
}

/// Carries every visible state transition caused by one normalized backend event.
#[derive(Clone, Debug, Default)]
pub struct TimelineTransition {
    pub completed: Option<CompletedThought>,
    pub active: Option<ActiveThoughtUpdate>,
}

/// Reduces provider events into one active thought and immutable completed thoughts.
pub struct TimelineReducer {
    interaction_id: String,
    active: Option<ActiveThought>,
    revision: u64,
    next_thought: u64,
}

impl TimelineReducer {
    /// Build an empty reducer for one admitted user interaction.
    pub fn new(interaction_id: impl Into<String>) -> Self {
        Self {
            interaction_id: interaction_id.into(),
            active: None,
            revision: 0,
            next_thought: 0,
        }
    }

    /// Apply one normalized provider event without exposing expandable active detail.
    pub fn apply(&mut self, event: &BackendEvent, now_ms: i64) -> TimelineTransition {
        if let Some(activity) = event.activity.as_ref() {
            self.ensure_active(now_ms);
            self.merge_tool(activity);
            return TimelineTransition {
                active: Some(self.active_update()),
                ..TimelineTransition::default()
            };
        }
        if event.kind != "assistant_message" {
            return TimelineTransition::default();
        }
        let text = event.text.as_deref().unwrap_or_default();
        if text.is_empty() {
            return TimelineTransition::default();
        }
        let completed = self
            .active
            .as_ref()
            .is_some_and(|active| !active.tool.is_empty())
            .then(|| self.take_active(now_ms))
            .flatten();
        match self.active.as_mut() {
            Some(active) => active.text.push_str(text),
            None => {
                let id = self.next_id();
                self.active = Some(ActiveThought {
                    id,
                    text: text.to_owned(),
                    synthetic: false,
                    started_at_ms: now_ms,
                    tool: ToolStore::default(),
                    latest_tool_id: None,
                });
            }
        }
        TimelineTransition {
            completed,
            active: Some(self.active_update()),
        }
    }

    /// Finish the backend turn and return its final response or final completed thought.
    pub fn finish_turn(
        &mut self,
        now_ms: i64,
        continuing: bool,
    ) -> (Option<CompletedThought>, Option<String>) {
        let Some(active) = self.active.take() else {
            return (None, None);
        };
        if continuing || active.synthetic || !active.tool.is_empty() {
            return (Some(complete(active, now_ms)), None);
        }
        (None, Some(active.text))
    }

    /// Complete the active thought before an acknowledged user steering boundary.
    pub fn complete_active(&mut self, now_ms: i64) -> Option<CompletedThought> {
        self.take_active(now_ms)
    }

    fn ensure_active(&mut self, now_ms: i64) {
        if self.active.is_some() {
            return;
        }
        let id = self.next_id();
        self.active = Some(ActiveThought {
            id,
            text: "Working".into(),
            synthetic: true,
            started_at_ms: now_ms,
            tool: ToolStore::default(),
            latest_tool_id: None,
        });
    }

    fn merge_tool(&mut self, activity: &ToolActivity) {
        let active = self.active.as_mut().expect("active thought is initialized");
        active.tool.merge(activity);
        active.latest_tool_id = Some(activity.id.clone());
        self.revision += 1;
    }

    fn active_update(&mut self) -> ActiveThoughtUpdate {
        self.revision += 1;
        let active = self.active.as_ref().expect("active thought is initialized");
        ActiveThoughtUpdate {
            interaction_id: self.interaction_id.clone(),
            thought_id: active.id.clone(),
            text: active.text.clone(),
            synthetic: active.synthetic,
            tool_count: active.tool.len(),
            failed_count: active.tool.failed_count(),
            latest_tool: active
                .latest_tool_id
                .as_ref()
                .and_then(|id| active.tool.get(id))
                .cloned(),
            revision: self.revision,
        }
    }

    fn take_active(&mut self, now_ms: i64) -> Option<CompletedThought> {
        self.active.take().map(|active| complete(active, now_ms))
    }

    fn next_id(&mut self) -> String {
        self.next_thought += 1;
        format!("{}:thought:{}", self.interaction_id, self.next_thought)
    }
}

fn complete(active: ActiveThought, now_ms: i64) -> CompletedThought {
    let tool_list = active.tool.into_list();
    let diff_text = ProviderDiffBuilder::build(&tool_list);
    CompletedThought {
        id: active.id,
        text: active.text,
        synthetic: active.synthetic,
        tool: tool_list,
        started_at_ms: active.started_at_ms,
        completed_at_ms: now_ms,
        diff_text,
        task_id: None,
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

#[cfg(test)]
mod test {
    use super::TimelineReducer;
    use crate::backend::{BackendEvent, ProviderChangeSet, ToolActivity, ToolActivityKind};
    use serde_json::Value;

    fn assistant(text: &str) -> BackendEvent {
        BackendEvent {
            kind: "assistant_message".into(),
            text: Some(text.into()),
            data: Value::Null,
            activity: None,
            summary: None,
            task_update: None,
        }
    }

    fn tool(id: &str, status: &str) -> BackendEvent {
        BackendEvent {
            kind: "tool".into(),
            text: None,
            data: Value::Null,
            activity: Some(ToolActivity {
                id: id.into(),
                kind: ToolActivityKind::Command,
                title: format!("command {id}"),
                output: Some(format!("output {id}")),
                status: Some(status.into()),
                change: ProviderChangeSet::default(),
                output_delta: false,
            }),
            summary: None,
            task_update: None,
        }
    }

    #[test]
    fn tools_before_commentary_create_a_synthetic_working_thought() {
        let mut reducer = TimelineReducer::new("interaction");
        let transition = reducer.apply(&tool("one", "inProgress"), 10);
        let active = transition.active.expect("active update");
        assert_eq!(active.text, "Working");
        assert!(active.synthetic);
        assert_eq!(active.tool_count, 1);
    }

    #[test]
    fn a_message_after_tools_completes_the_previous_thought() {
        let mut reducer = TimelineReducer::new("interaction");
        reducer.apply(&assistant("Investigating."), 10);
        reducer.apply(&tool("one", "completed"), 20);
        let transition = reducer.apply(&assistant("Implementing."), 30);
        let completed = transition.completed.expect("completed thought");
        assert_eq!(completed.text, "Investigating.");
        assert_eq!(completed.tool.len(), 1);
        assert_eq!(
            transition.active.expect("active thought").text,
            "Implementing."
        );
    }

    #[test]
    fn active_tool_lifecycle_updates_one_counted_tool() {
        let mut reducer = TimelineReducer::new("interaction");
        reducer.apply(&tool("one", "inProgress"), 10);
        let transition = reducer.apply(&tool("one", "failed"), 20);
        let active = transition.active.expect("active update");
        assert_eq!(active.tool_count, 1);
        assert_eq!(active.failed_count, 1);
        let latest = active.latest_tool.expect("latest tool preview");
        assert_eq!(latest.id, "one");
        assert_eq!(latest.output, "output one");
    }

    #[test]
    fn active_update_previews_only_the_most_recent_tool() {
        let mut reducer = TimelineReducer::new("interaction");
        reducer.apply(&tool("one", "completed"), 10);
        let transition = reducer.apply(&tool("two", "inProgress"), 20);
        let active = transition.active.expect("active update");
        assert_eq!(active.tool_count, 2);
        let latest = active.latest_tool.expect("latest tool preview");
        assert_eq!(latest.id, "two");
        assert_eq!(latest.output, "output two");
    }

    #[test]
    fn final_untooled_message_becomes_the_response() {
        let mut reducer = TimelineReducer::new("interaction");
        reducer.apply(&assistant("Final response"), 10);
        let (thought, response) = reducer.finish_turn(20, false);
        assert!(thought.is_none());
        assert_eq!(response.as_deref(), Some("Final response"));
    }

    #[test]
    fn goal_continuation_promotes_the_turn_response_to_a_thought() {
        let mut reducer = TimelineReducer::new("interaction");
        reducer.apply(&assistant("Intermediate result"), 10);
        let (thought, response) = reducer.finish_turn(20, true);
        assert_eq!(
            thought.expect("completed thought").text,
            "Intermediate result"
        );
        assert!(response.is_none());
    }

    #[test]
    fn steering_boundary_completes_untooled_commentary() {
        let mut reducer = TimelineReducer::new("interaction");
        reducer.apply(&assistant("Researching."), 10);

        let thought = reducer.complete_active(20).expect("completed thought");

        assert_eq!(thought.text, "Researching.");
        let (remaining, response) = reducer.finish_turn(30, false);
        assert!(remaining.is_none());
        assert!(response.is_none());
    }
}
