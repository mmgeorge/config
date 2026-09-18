use super::{Message, MessageDelivery, MessageKind, ToolCall, ToolStore, TurnItem};
use anyhow::{Result, ensure};
use serde::{Deserialize, Serialize};

use crate::backend::ProviderAddress;

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
/// The immutable outcome of one provider execution.
pub enum TurnOutcome {
    /// The provider finished its execution normally.
    Completed,
    /// The provider reported an unrecoverable execution error.
    Failed,
    /// The user cancelled execution.
    Cancelled,
    /// Execution stopped without a normal provider completion.
    Interrupted,
}

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
/// Separates an active provider execution from its permanent outcome.
pub enum TurnState {
    /// Provider events may still extend the execution.
    Running,
    /// Provider events can no longer change this execution.
    Finished {
        /// The provider or cancellation boundary that settled execution.
        outcome: TurnOutcome,
    },
}

#[derive(Clone, Debug, Deserialize, Serialize)]
/// Owns the identity and lifecycle of one provider turn within an exchange.
pub struct Turn {
    id: String,
    provider: ProviderAddress,
    state: TurnState,
    started_at_ms: i64,
    completed_at_ms: Option<i64>,
    #[serde(default)]
    token_count: Option<u64>,
    tool: ToolStore,
    message: Vec<Message>,
    item: Vec<TurnItem>,
    current_message: Option<usize>,
}

impl Turn {
    /// Admit one provider start event into the owning exchange.
    pub(crate) fn new(id: String, provider: ProviderAddress, now_ms: i64) -> Self {
        Self {
            id,
            provider,
            state: TurnState::Running,
            started_at_ms: now_ms,
            completed_at_ms: None,
            token_count: None,
            tool: ToolStore::default(),
            message: Vec::new(),
            item: Vec::new(),
            current_message: None,
        }
    }

    /// Return the local identity independently of provider naming.
    pub fn id(&self) -> &str {
        &self.id
    }

    /// Return the provider address used for event and control admission.
    pub fn provider(&self) -> &ProviderAddress {
        &self.provider
    }

    /// Return the current execution state.
    pub fn state(&self) -> TurnState {
        self.state
    }

    /// Return usage attributed to this provider execution.
    pub fn token_count(&self) -> Option<u64> {
        self.token_count
    }

    /// Return the frozen provider execution duration after completion.
    pub fn duration_ms(&self) -> u64 {
        self.completed_at_ms.map_or(0, |completed| {
            completed.saturating_sub(self.started_at_ms) as u64
        })
    }

    /// Measure provider execution inside a bounded task interval.
    pub(crate) fn duration_between(&self, start_ms: i64, end_ms: i64) -> i64 {
        self.completed_at_ms
            .unwrap_or(end_ms)
            .min(end_ms)
            .saturating_sub(self.started_at_ms.max(start_ms))
            .max(0)
    }

    /// Attach terminal usage without reopening the settled execution.
    pub(crate) fn set_token_count(&mut self, token_count: Option<u64>) {
        if token_count.is_some() {
            self.token_count = token_count;
        }
    }

    /// Visit the canonical tool calls owned by this provider execution.
    pub fn tools(&self) -> impl Iterator<Item = &ToolCall> {
        self.tool.iter()
    }

    /// Merge tool progress only while its provider turn remains active.
    pub(crate) fn record_tool(&mut self, activity: &crate::backend::ToolActivity) -> Result<()> {
        ensure!(
            self.state == TurnState::Running,
            "completed turn rejects tool events"
        );
        self.current_message = None;
        if self.tool.get(&activity.id).is_none() {
            self.item.push(TurnItem::Tool {
                id: activity.id.clone(),
            });
        }
        self.tool.merge(activity);
        Ok(())
    }

    /// Attach task provenance before execution settles, retaining the first attribution.
    pub(crate) fn attribute_tool(&mut self, id: &str, task_id: &str) -> Result<bool> {
        ensure!(
            self.state == TurnState::Running,
            "completed turn rejects attribution changes"
        );
        Ok(self.tool.attribute(id, task_id))
    }

    /// Borrow the ordered content positions owned by this execution.
    pub fn items(&self) -> &[TurnItem] {
        &self.item
    }

    /// Borrow messages without exposing mutation of terminal history.
    pub fn messages(&self) -> &[Message] {
        &self.message
    }

    /// Start a new presentation-independent message at an acknowledged input boundary.
    pub(crate) fn close_message(&mut self) {
        self.current_message = None;
    }

    /// Append one normalized text delta to its provider execution.
    #[cfg(test)]
    pub(crate) fn record_text(&mut self, kind: MessageKind, text: &str) -> Result<()> {
        self.record_provider_text(kind, text, None, default_delivery(kind), false)
    }

    /// Reconcile provider text while retaining its message identity and original timeline position.
    pub(crate) fn record_provider_text(
        &mut self,
        kind: MessageKind,
        text: &str,
        provider_id: Option<String>,
        delivery: MessageDelivery,
        snapshot: bool,
    ) -> Result<()> {
        ensure!(
            self.state == TurnState::Running,
            "completed turn rejects message events"
        );
        if let Some(provider_id) = provider_id.as_deref()
            && let Some(message) = self.message.iter_mut().find(|message| {
                message.kind == kind && message.provider_id.as_deref() == Some(provider_id)
            })
        {
            message.delivery = delivery;
            if snapshot {
                message.text = text.to_owned();
            } else {
                message.text.push_str(text);
            }
            return Ok(());
        }
        if text.is_empty() {
            return Ok(());
        }
        if let Some(index) = self.current_message {
            if self.message[index].kind == kind
                && self.message[index].delivery == delivery
                && provider_id.is_none()
                && self.message[index].provider_id.is_none()
                && !snapshot
            {
                self.message[index].text.push_str(text);
                return Ok(());
            }
        }
        let index = self.message.len();
        let id = format!("{}:message:{}", self.id, index + 1);
        self.message.push(Message {
            id: id.clone(),
            provider_id,
            kind,
            delivery,
            text: text.to_owned(),
        });
        self.item.push(TurnItem::Message { id });
        self.current_message = Some(index);
        Ok(())
    }

    /// Permanently settle execution while accepting an identical completion replay.
    pub(crate) fn finish(&mut self, outcome: TurnOutcome, now_ms: i64) -> Result<()> {
        match self.state {
            TurnState::Finished { outcome: previous } => {
                ensure!(previous == outcome, "completed turn outcome cannot change");
            }
            TurnState::Running => {
                self.tool.finish(outcome);
                self.current_message = None;
                self.state = TurnState::Finished { outcome };
                self.completed_at_ms = Some(now_ms.max(self.started_at_ms));
            }
        }
        Ok(())
    }
}

#[cfg(test)]
fn default_delivery(kind: MessageKind) -> MessageDelivery {
    match kind {
        MessageKind::Assistant => MessageDelivery::Final,
        MessageKind::Reasoning => MessageDelivery::Commentary,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn activity(id: &str) -> crate::backend::ToolActivity {
        crate::backend::ToolActivity {
            id: id.into(),
            kind: crate::backend::ToolActivityKind::Command,
            title: "command".into(),
            output: Some("result".into()),
            status: Some("completed".into()),
            change: Default::default(),
            output_delta: false,
        }
    }

    #[test]
    fn messages_and_tools_keep_order_across_boundaries_and_freeze_at_completion() {
        let mut turn = Turn::new(
            "local".into(),
            ProviderAddress {
                thread_id: "thread".into(),
                turn_id: "provider".into(),
            },
            10,
        );
        turn.record_text(MessageKind::Reasoning, "Inspecting ")
            .unwrap();
        turn.record_text(MessageKind::Reasoning, "ownership")
            .unwrap();
        turn.record_tool(&activity("tool")).unwrap();
        turn.record_text(MessageKind::Assistant, "First response")
            .unwrap();
        turn.close_message();
        turn.record_text(MessageKind::Assistant, "After steering")
            .unwrap();
        assert_eq!(turn.messages().len(), 3);
        assert_eq!(turn.messages()[0].text(), "Inspecting ownership");
        assert_eq!(
            turn.items(),
            &[
                TurnItem::Message {
                    id: "local:message:1".into()
                },
                TurnItem::Tool { id: "tool".into() },
                TurnItem::Message {
                    id: "local:message:2".into()
                },
                TurnItem::Message {
                    id: "local:message:3".into()
                },
            ]
        );
        turn.finish(TurnOutcome::Completed, 20).unwrap();
        let encoded = serde_json::to_string(&turn).unwrap();
        assert!(
            turn.record_text(MessageKind::Assistant, "late delta")
                .is_err()
        );
        assert_eq!(serde_json::to_string(&turn).unwrap(), encoded);
        let restored: Turn = serde_json::from_str(&encoded).unwrap();
        assert_eq!(restored.messages()[2].text(), "After steering");
        assert_eq!(restored.items(), turn.items());
    }

    #[test]
    fn tool_records_survive_serialization_without_accepting_late_progress() {
        let mut turn = Turn::new(
            "local".into(),
            ProviderAddress {
                thread_id: "thread".into(),
                turn_id: "execution".into(),
            },
            10,
        );
        let mut tool = activity("call");
        turn.record_tool(&tool).unwrap();
        tool.status = Some("inProgress".into());
        tool.output = Some("late output".into());
        turn.record_tool(&tool).unwrap();
        assert_eq!(turn.tools().count(), 1);
        assert_eq!(
            turn.tools().next().unwrap().state(),
            crate::turn::ToolState::Completed
        );
        turn.finish(TurnOutcome::Completed, 20).unwrap();
        let restored: Turn = serde_json::from_str(&serde_json::to_string(&turn).unwrap()).unwrap();
        assert_eq!(restored.tools().next().unwrap().output, "late output");
        assert_eq!(
            restored.tools().next().unwrap().state(),
            crate::turn::ToolState::Completed
        );
    }

    #[test]
    fn finishing_turn_settles_unfinished_tools_without_rewriting_completed_calls() {
        for (outcome, expected) in [
            (TurnOutcome::Completed, crate::turn::ToolState::Interrupted),
            (
                TurnOutcome::Interrupted,
                crate::turn::ToolState::Interrupted,
            ),
            (TurnOutcome::Cancelled, crate::turn::ToolState::Cancelled),
            (TurnOutcome::Failed, crate::turn::ToolState::Failed),
        ] {
            let mut turn = Turn::new(
                "local".into(),
                ProviderAddress {
                    thread_id: "thread".into(),
                    turn_id: "execution".into(),
                },
                10,
            );
            turn.record_tool(&activity("completed")).unwrap();
            let mut running = activity("running");
            running.status = Some("inProgress".into());
            turn.record_tool(&running).unwrap();
            turn.finish(outcome, 20).unwrap();
            let state: Vec<_> = turn.tools().map(|tool| tool.state()).collect();
            assert_eq!(state, [crate::turn::ToolState::Completed, expected]);
            let encoded = serde_json::to_string(&turn).unwrap();
            turn.finish(outcome, 90).unwrap();
            assert!(turn.record_tool(&running).is_err());
            assert_eq!(serde_json::to_string(&turn).unwrap(), encoded);
        }
    }

    #[test]
    fn recovered_turn_cannot_finish_before_its_start() {
        let mut turn = Turn::new(
            "local".into(),
            ProviderAddress {
                thread_id: "thread".into(),
                turn_id: "continuation".into(),
            },
            200,
        );
        turn.finish(TurnOutcome::Interrupted, 100).unwrap();
        assert_eq!(turn.completed_at_ms, Some(200));
        turn.finish(TurnOutcome::Interrupted, 90_000).unwrap();
        assert_eq!(turn.completed_at_ms, Some(200));
    }

    #[test]
    fn completed_turn_rejects_late_content_and_freezes_completion() {
        let mut turn = Turn::new(
            "local".into(),
            ProviderAddress {
                thread_id: "thread".into(),
                turn_id: "provider-turn".into(),
            },
            10,
        );
        turn.record_tool(&activity("tool")).unwrap();
        turn.record_tool(&activity("tool")).unwrap();
        assert!(turn.attribute_tool("tool", "original-task").unwrap());
        assert!(!turn.attribute_tool("tool", "another-task").unwrap());
        assert_eq!(
            turn.tools().next().unwrap().task_id.as_deref(),
            Some("original-task")
        );

        assert_eq!(turn.tools().count(), 1);
        turn.finish(TurnOutcome::Completed, 20).unwrap();
        turn.set_token_count(Some(128));
        assert_eq!(turn.token_count(), Some(128));
        turn.finish(TurnOutcome::Completed, 90).unwrap();
        assert_eq!(turn.completed_at_ms, Some(20));
        assert!(turn.record_tool(&activity("late")).is_err());
        assert!(turn.attribute_tool("tool", "late-task").is_err());
        assert!(turn.finish(TurnOutcome::Failed, 90).is_err());
    }
}
