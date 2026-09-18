use crate::backend::{ProviderTaskEntry, ProviderTaskUpdate, TaskStatus};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};

/// Represents one provider task with a stable Harness identity.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct TaskItem {
    pub id: String,
    pub provider_id: Option<String>,
    pub title: String,
    pub priority: Option<String>,
    pub status: TaskStatus,
    pub provider_ordinal: usize,
    #[serde(default)]
    pub attributed: bool,
}

/// Stores the current provider presentation and superseded attributed history.
#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub struct TaskSnapshot {
    pub scope_id: String,
    pub name: Option<String>,
    pub revision: u64,
    pub complete: bool,
    pub current: Vec<TaskItem>,
    pub superseded: Vec<TaskItem>,
}

/// Reconciles complete provider task replacements without losing attributed history.
#[derive(Clone, Debug, Default)]
pub struct TaskTracker {
    snapshot: TaskSnapshot,
    next_id: u64,
}

impl TaskTracker {
    /// Restore task reconciliation from one durable snapshot.
    pub fn from_snapshot(snapshot: TaskSnapshot) -> Self {
        let next_id = snapshot
            .current
            .iter()
            .chain(&snapshot.superseded)
            .filter_map(|task| task.id.rsplit(':').next()?.parse::<u64>().ok())
            .max()
            .unwrap_or_default();
        Self { snapshot, next_id }
    }

    /// Replace the provider presentation while preserving stable attributed tasks.
    pub fn replace(&mut self, update: &ProviderTaskUpdate) -> &TaskSnapshot {
        if !update.replace_entries {
            self.snapshot.scope_id.clone_from(&update.scope_id);
            if update.name.is_some() {
                self.snapshot.name.clone_from(&update.name);
            }
            self.snapshot.revision = self.snapshot.revision.saturating_add(1);
            self.snapshot.complete = update.complete;
            return &self.snapshot;
        }
        let previous = std::mem::take(&mut self.snapshot.current);
        let mut unmatched_previous = (0..previous.len()).collect::<HashSet<_>>();
        let mut matched = vec![None; update.entry_list.len()];

        let mut by_provider_id = HashMap::new();
        for (index, task) in previous.iter().enumerate() {
            if let Some(provider_id) = task.provider_id.as_ref() {
                by_provider_id.insert(provider_id.as_str(), index);
            }
        }
        for (index, entry) in update.entry_list.iter().enumerate() {
            let Some(provider_id) = entry.provider_id.as_deref() else {
                continue;
            };
            if let Some(previous_index) = by_provider_id.get(provider_id).copied()
                && unmatched_previous.remove(&previous_index)
            {
                matched[index] = Some(previous_index);
            }
        }

        for (index, entry) in update.entry_list.iter().enumerate() {
            if matched[index].is_some() {
                continue;
            }
            let normalized = normalize_title(&entry.content);
            let candidate = unmatched_previous.iter().copied().find(|previous_index| {
                normalize_title(&previous[*previous_index].title) == normalized
            });
            if let Some(previous_index) = candidate {
                unmatched_previous.remove(&previous_index);
                matched[index] = Some(previous_index);
            }
        }

        let unmatched_new = matched
            .iter()
            .enumerate()
            .filter_map(|(index, slot)| slot.is_none().then_some(index))
            .collect::<Vec<_>>();
        if unmatched_new.len() == unmatched_previous.len() {
            let mut previous_index_list = unmatched_previous.iter().copied().collect::<Vec<_>>();
            previous_index_list.sort_unstable();
            for (new_index, previous_index) in unmatched_new.into_iter().zip(previous_index_list) {
                matched[new_index] = Some(previous_index);
                unmatched_previous.remove(&previous_index);
            }
        }

        let mut current = Vec::with_capacity(update.entry_list.len());
        for (index, entry) in update.entry_list.iter().enumerate() {
            let task = match matched[index] {
                Some(previous_index) => merge_task(previous[previous_index].clone(), entry, index),
                None => {
                    self.next_id += 1;
                    TaskItem {
                        id: format!("{}:task:{}", update.scope_id, self.next_id),
                        provider_id: entry.provider_id.clone(),
                        title: entry.content.clone(),
                        priority: entry.priority.clone(),
                        status: entry.status,
                        provider_ordinal: index,
                        attributed: false,
                    }
                }
            };
            current.push(task);
        }

        for previous_index in unmatched_previous {
            let mut task = previous[previous_index].clone();
            if task.attributed {
                task.status = TaskStatus::Superseded;
                self.snapshot.superseded.push(task);
            }
        }
        self.snapshot.scope_id.clone_from(&update.scope_id);
        self.snapshot.name.clone_from(&update.name);
        self.snapshot.revision = self.snapshot.revision.saturating_add(1);
        self.snapshot.complete = update.complete;
        self.snapshot.current = current;
        &self.snapshot
    }

    /// Return the only current in-progress task when attribution is unambiguous.
    pub fn attribution_target(&self) -> Option<&str> {
        let mut matching = self
            .snapshot
            .current
            .iter()
            .filter(|task| task.status == TaskStatus::InProgress);
        let task = matching.next()?;
        matching.next().is_none().then_some(task.id.as_str())
    }

    /// Mark a task as historically referenced so replacement cannot discard it.
    pub fn mark_attributed(&mut self, task_id: &str) {
        if let Some(task) = self
            .snapshot
            .current
            .iter_mut()
            .chain(&mut self.snapshot.superseded)
            .find(|task| task.id == task_id)
        {
            task.attributed = true;
        }
    }

    /// Borrow the durable task presentation.
    pub fn snapshot(&self) -> &TaskSnapshot {
        &self.snapshot
    }
}

fn normalize_title(title: &str) -> String {
    title
        .split_whitespace()
        .collect::<Vec<_>>()
        .join(" ")
        .to_lowercase()
}

fn merge_task(mut task: TaskItem, entry: &ProviderTaskEntry, provider_ordinal: usize) -> TaskItem {
    task.provider_id.clone_from(&entry.provider_id);
    task.title.clone_from(&entry.content);
    task.priority.clone_from(&entry.priority);
    task.status = entry.status;
    task.provider_ordinal = provider_ordinal;
    task
}

#[cfg(test)]
mod test {
    use super::*;

    fn update(entry_list: Vec<(&str, TaskStatus)>) -> ProviderTaskUpdate {
        ProviderTaskUpdate {
            scope_id: "turn".into(),
            name: None,
            complete: false,
            replace_entries: true,
            entry_list: entry_list
                .into_iter()
                .enumerate()
                .map(|(ordinal, (content, status))| ProviderTaskEntry {
                    provider_id: None,
                    content: content.into(),
                    priority: None,
                    status,
                    provider_ordinal: ordinal,
                })
                .collect(),
        }
    }

    #[test]
    fn replaces_titles_without_changing_identity() {
        let mut tracker = TaskTracker::default();
        let original = tracker
            .replace(&update(vec![("Build renderer", TaskStatus::InProgress)]))
            .current[0]
            .id
            .clone();
        let snapshot = tracker.replace(&update(vec![("Refactor renderer", TaskStatus::Completed)]));
        assert_eq!(snapshot.current[0].id, original);
        assert_eq!(snapshot.current[0].title, "Refactor renderer");
    }

    #[test]
    fn preserves_only_removed_tasks_with_attributed_history() {
        let mut tracker = TaskTracker::default();
        let snapshot = tracker.replace(&update(vec![
            ("Keep history", TaskStatus::InProgress),
            ("Discard me", TaskStatus::Pending),
        ]));
        let retained_id = snapshot.current[0].id.clone();
        tracker.mark_attributed(&retained_id);
        let snapshot = tracker.replace(&update(Vec::new()));
        assert!(snapshot.current.is_empty());
        assert_eq!(snapshot.superseded.len(), 1);
        assert_eq!(snapshot.superseded[0].id, retained_id);
        assert_eq!(snapshot.superseded[0].status, TaskStatus::Superseded);
    }

    #[test]
    fn attribution_requires_exactly_one_running_task() {
        let mut tracker = TaskTracker::default();
        tracker.replace(&update(vec![("One", TaskStatus::InProgress)]));
        assert!(tracker.attribution_target().is_some());
        tracker.replace(&update(vec![
            ("One", TaskStatus::InProgress),
            ("Two", TaskStatus::InProgress),
        ]));
        assert!(tracker.attribution_target().is_none());
    }
}
