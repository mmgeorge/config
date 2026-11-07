use std::collections::{HashMap, HashSet};

use serde_json::Value;

/// Track child ownership only after Codex reports a trusted parent relationship.
#[derive(Default)]
pub(super) struct DescendantTracker {
    active_turn_by_thread: HashMap<String, Option<String>>,
    owned_thread: HashSet<String>,
    observed_turn: HashMap<String, ObservedTurn>,
}

enum ObservedTurn {
    Active(String),
    Completed(String),
}

impl ObservedTurn {
    fn id(&self) -> &str {
        match self {
            Self::Active(id) | Self::Completed(id) => id,
        }
    }
}

impl DescendantTracker {
    /// Retain early turns but admit a child only through an explicit trusted relationship.
    pub(super) fn observe(&mut self, message: &Value, parent_thread: &str) {
        if let (Some(thread), Some(turn)) = (thread_id(message), turn_id(message))
            && thread != parent_thread
        {
            match message.get("method").and_then(Value::as_str) {
                Some("turn/started") => {
                    self.observed_turn
                        .insert(thread.into(), ObservedTurn::Active(turn.into()));
                }
                Some("turn/completed")
                    if self
                        .observed_turn
                        .get(thread)
                        .is_none_or(|old| old.id() == turn) =>
                {
                    self.observed_turn
                        .insert(thread.into(), ObservedTurn::Completed(turn.into()));
                }
                _ => {}
            }
        }

        // Normalization rejects a malformed collaboration item from this same message as a
        // protocol failure, so ownership tracking only admits validated records.
        let lifecycle_list = super::decode::agent_lifecycle_list(message).unwrap_or_default();
        for lifecycle in lifecycle_list {
            if !lifecycle
                .parent_thread_id
                .as_deref()
                .is_some_and(|parent| parent == parent_thread || self.owned_thread.contains(parent))
            {
                continue;
            }
            let Some(thread) = lifecycle.provider_thread_id else {
                continue;
            };
            if thread == parent_thread {
                continue;
            }
            let resumes = lifecycle.status.is_active()
                && lifecycle
                    .tool
                    .is_some_and(super::decode::CollabAgentTool::resumes_child);
            if lifecycle.starts_child || resumes {
                self.owned_thread.insert(thread.clone());
            }
            if !self.owned_thread.contains(&thread) {
                continue;
            }
            if lifecycle.status.is_active() {
                match self.observed_turn.get(&thread) {
                    Some(ObservedTurn::Active(turn)) => {
                        self.active_turn_by_thread
                            .insert(thread, Some(turn.clone()));
                    }
                    Some(ObservedTurn::Completed(_)) if resumes => {
                        self.active_turn_by_thread.insert(thread, None);
                    }
                    Some(ObservedTurn::Completed(_)) => {
                        self.active_turn_by_thread.remove(&thread);
                    }
                    None => {
                        self.active_turn_by_thread.entry(thread).or_default();
                    }
                }
            } else {
                self.active_turn_by_thread.remove(&thread);
            }
        }

        if message.get("method").and_then(Value::as_str) == Some("turn/started")
            && let (Some(thread), Some(turn)) = (thread_id(message), turn_id(message))
            && thread != parent_thread
            && self.owned_thread.contains(thread)
        {
            self.active_turn_by_thread
                .insert(thread.into(), Some(turn.into()));
        }
        if message.get("method").and_then(Value::as_str) == Some("turn/completed")
            && let Some(thread) = thread_id(message).filter(|thread| *thread != parent_thread)
            && self
                .active_turn_by_thread
                .get(thread)
                .is_some_and(|active| {
                    active
                        .as_deref()
                        .is_none_or(|turn| Some(turn) == turn_id(message))
                })
        {
            self.active_turn_by_thread.remove(thread);
        }
    }

    #[cfg(test)]
    pub(super) fn owns(&self, thread: &str) -> bool {
        self.owned_thread.contains(thread)
    }

    pub(super) fn active_turns(&self) -> Vec<(String, Option<String>)> {
        self.active_turn_by_thread
            .iter()
            .map(|(thread, turn)| (thread.clone(), turn.clone()))
            .collect()
    }

    #[cfg(test)]
    pub(super) fn active_count(&self) -> usize {
        self.active_turn_by_thread.len()
    }
}

fn thread_id(message: &Value) -> Option<&str> {
    message
        .pointer("/params/threadId")
        .or_else(|| message.pointer("/params/thread_id"))
        .or_else(|| message.pointer("/params/turn/threadId"))
        .or_else(|| message.pointer("/params/turn/thread_id"))
        .and_then(Value::as_str)
}

fn turn_id(message: &Value) -> Option<&str> {
    message
        .pointer("/params/turn/id")
        .or_else(|| message.pointer("/params/turnId"))
        .or_else(|| message.pointer("/params/turn_id"))
        .and_then(Value::as_str)
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn foreign_child_turns_do_not_establish_ownership() {
        let mut tracker = DescendantTracker::default();
        tracker.observe(
            &json!({"method":"turn/started","params":{"threadId":"foreign","turn":{"id":"turn"}}}),
            "parent",
        );
        tracker.observe(&json!({"method":"item/completed","params":{"threadId":"foreign","item":{
            "type":"collabAgentToolCall","id":"spawn","tool":"spawnAgent","senderThreadId":"foreign","receiverThreadIds":["foreign-child"],"status":"completed"
        }}}), "parent");
        assert!(!tracker.owns("foreign"));
        assert!(!tracker.owns("foreign-child"));
        assert_eq!(tracker.active_count(), 0);
    }

    #[test]
    fn trusted_child_turn_is_retained_until_its_exact_completion() {
        let mut tracker = DescendantTracker::default();
        tracker.observe(&json!({"method":"item/completed","params":{"threadId":"parent","item":{
            "type":"collabAgentToolCall","id":"spawn","tool":"spawnAgent","senderThreadId":"parent","receiverThreadIds":["child"],"status":"completed"
        }}}), "parent");
        assert!(tracker.owns("child"));
        assert_eq!(tracker.active_count(), 1);
        tracker.observe(&json!({"method":"turn/started","params":{"threadId":"child","turn":{"id":"child-turn"}}}), "parent");
        assert_eq!(
            tracker.active_turns(),
            vec![("child".into(), Some("child-turn".into()))]
        );
        tracker.observe(&json!({"method":"turn/completed","params":{"threadId":"child","turn":{"id":"stale-turn"}}}), "parent");
        assert_eq!(tracker.active_count(), 1);
        tracker.observe(&json!({"method":"turn/completed","params":{"threadId":"child","turn":{"id":"child-turn"}}}), "parent");
        assert_eq!(tracker.active_count(), 0);
        assert!(tracker.owns("child"));
    }

    #[test]
    fn retained_child_resumption_waits_for_a_new_turn_identity() {
        let mut tracker = DescendantTracker::default();
        tracker.observe(&json!({"method":"item/completed","params":{"threadId":"parent","item":{
            "type":"collabAgentToolCall","id":"spawn","tool":"spawnAgent","senderThreadId":"parent","receiverThreadIds":["child"],"status":"completed"
        }}}), "parent");
        tracker.observe(
            &json!({"method":"turn/started","params":{"threadId":"child","turn":{"id":"old"}}}),
            "parent",
        );
        tracker.observe(
            &json!({"method":"turn/completed","params":{"threadId":"child","turn":{"id":"old"}}}),
            "parent",
        );
        tracker.observe(
            &json!({"method":"item/completed","params":{"threadId":"parent","item":{
                "type":"collabAgentToolCall","id":"wait","tool":"wait","senderThreadId":"parent","receiverThreadIds":["child"],
                "agentsStates":{"child":{"status":"running"}},"status":"completed"
            }}}),
            "parent",
        );
        assert_eq!(tracker.active_turns(), vec![("child".into(), None)]);
        tracker.observe(
            &json!({"method":"turn/started","params":{"threadId":"child","turn":{"id":"new"}}}),
            "parent",
        );
        assert_eq!(
            tracker.active_turns(),
            vec![("child".into(), Some("new".into()))]
        );
    }
}
