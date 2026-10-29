#[cfg(test)]
use super::MessageDelivery;
use super::{Message, MessageKind, MessageState, TurnItem};
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
    usage: Option<crate::backend::usage::TokenUsage>,
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
            usage: None,
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
    pub fn usage(&self) -> Option<&crate::backend::usage::TokenUsage> {
        self.usage.as_ref()
    }

    /// Return the frozen provider execution duration after completion.
    pub fn duration_ms(&self) -> u64 {
        self.completed_at_ms.map_or(0, |completed| {
            completed.saturating_sub(self.started_at_ms) as u64
        })
    }

    /// Attach terminal usage without reopening the settled execution.
    pub(crate) fn record_usage(&mut self, usage: crate::backend::usage::TokenUsage) {
        if let Some(current) = &mut self.usage {
            current.accumulate(&usage);
        } else {
            self.usage = Some(usage);
        }
    }

    pub(crate) fn admit_tool(&mut self, id: &str) -> Result<()> {
        ensure!(
            self.state == TurnState::Running,
            "completed turn rejects tool events"
        );
        self.current_message = None;
        if !self
            .item
            .iter()
            .any(|item| matches!(item, TurnItem::Tool { id: existing } if existing == id))
        {
            self.item.push(TurnItem::Tool { id: id.to_owned() });
        }
        Ok(())
    }

    /// Borrow the ordered content positions owned by this execution.
    pub fn items(&self) -> &[TurnItem] {
        &self.item
    }

    /// Borrow messages without exposing mutation of terminal history.
    pub fn messages(&self) -> &[Message] {
        &self.message
    }

    /// Return the latest readable reasoning summary section for this execution.
    pub fn latest_reasoning_summary(&self) -> Option<&str> {
        self.message
            .iter()
            .rev()
            .find(|message| message.kind() == MessageKind::ReasoningSummary)
            .map(Message::text)
            .filter(|text| !text.trim().is_empty())
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

    #[cfg(test)]
    pub(crate) fn record_provider_text(
        &mut self,
        kind: MessageKind,
        text: &str,
        provider_id: Option<String>,
        delivery: MessageDelivery,
        snapshot: bool,
    ) -> Result<()> {
        use crate::backend::{MessageId, MessageRole, MessageUpdate, ProviderMessage};
        let provider_id = provider_id.unwrap_or_else(|| {
            self.current_message
                .filter(|index| self.message[*index].kind() == kind && !snapshot)
                .and_then(|index| self.message[index].provider_id.clone())
                .unwrap_or_else(|| format!("test-message:{}", self.message.len() + 1))
        });
        let role = match kind {
            MessageKind::Unknown => None,
            MessageKind::Reasoning => Some(MessageRole::Thought),
            MessageKind::ReasoningSummary => Some(MessageRole::ThoughtSummary),
            MessageKind::Assistant => match delivery {
                MessageDelivery::Unknown => None,
                MessageDelivery::Commentary => Some(MessageRole::Commentary),
                MessageDelivery::Final => Some(MessageRole::Response),
            },
        };
        self.record_message(&ProviderMessage {
            id: MessageId(provider_id),
            role,
            update: if snapshot {
                MessageUpdate::Completed { text: text.into() }
            } else {
                MessageUpdate::Delta { text: text.into() }
            },
        })
    }

    pub(crate) fn record_message(
        &mut self,
        update: &crate::backend::ProviderMessage,
    ) -> Result<()> {
        use crate::backend::MessageUpdate;
        ensure!(
            self.state == TurnState::Running,
            "completed turn rejects message events"
        );
        ensure!(
            !update.id.0.is_empty(),
            "provider message identity is empty"
        );
        ensure!(
            !matches!(update.update, MessageUpdate::Classified) || update.role.is_some(),
            "provider message classification omitted its role"
        );
        let index = self
            .message
            .iter()
            .position(|message| message.provider_id.as_deref() == Some(update.id.0.as_str()))
            .unwrap_or_else(|| {
                let index = self.message.len();
                let id = format!("{}:message:{}", self.id, index + 1);
                self.message.push(Message {
                    id: id.clone(),
                    provider_id: Some(update.id.0.clone()),
                    role: None,
                    state: MessageState::Streaming,
                    text: String::new(),
                });
                self.item.push(TurnItem::Message { id });
                index
            });
        let message = &mut self.message[index];
        if let Some(role) = update.role {
            message.role = Some(role);
        }
        match &update.update {
            MessageUpdate::Started | MessageUpdate::Classified => {}
            MessageUpdate::Delta { text } if message.state == MessageState::Streaming => {
                message.text.push_str(text);
            }
            MessageUpdate::Completed { text } => {
                ensure!(
                    message.state != MessageState::Interrupted,
                    "interrupted message rejects completion"
                );
                message.text = text.clone();
                message.state = MessageState::Completed;
            }
            MessageUpdate::Interrupted if message.state == MessageState::Streaming => {
                message.state = MessageState::Interrupted;
            }
            MessageUpdate::Delta { .. } | MessageUpdate::Interrupted => {}
        }
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
                for message in &mut self.message {
                    if message.state == MessageState::Streaming {
                        message.state = MessageState::Interrupted;
                    }
                }
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
        MessageKind::Unknown => MessageDelivery::Unknown,
        MessageKind::Assistant => MessageDelivery::Final,
        MessageKind::Reasoning | MessageKind::ReasoningSummary => MessageDelivery::Commentary,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::turn::test_support::TestTurn as Turn;

    #[test]
    fn typed_message_retains_identity_role_and_terminal_lifecycle() -> Result<()> {
        use crate::backend::{MessageId, MessageRole, MessageUpdate, ProviderMessage};
        let mut turn = super::Turn::new(
            "local".into(),
            ProviderAddress {
                owner: None,
                thread_id: "thread".into(),
                turn_id: "provider".into(),
            },
            0,
        );
        let mut update = ProviderMessage {
            id: MessageId("message".into()),
            role: None,
            update: MessageUpdate::Started,
        };
        turn.record_message(&update)?;
        assert_eq!(turn.messages()[0].kind(), MessageKind::Unknown);
        assert_eq!(turn.messages()[0].delivery(), MessageDelivery::Unknown);
        update.update = MessageUpdate::Delta {
            text: "Draft".into(),
        };
        turn.record_message(&update)?;
        let mut turn: super::Turn = serde_json::from_value(serde_json::to_value(turn)?)?;
        assert_eq!(turn.messages()[0].state(), MessageState::Streaming);
        assert_eq!(turn.messages()[0].role(), None);
        update.update = MessageUpdate::Completed {
            text: "Final answer".into(),
        };
        turn.record_message(&update)?;
        update.role = Some(MessageRole::Response);
        update.update = MessageUpdate::Classified;
        turn.record_message(&update)?;
        update.role = None;
        update.update = MessageUpdate::Delta {
            text: "late delta".into(),
        };
        turn.record_message(&update)?;
        update.update = MessageUpdate::Interrupted;
        turn.record_message(&update)?;
        assert_eq!(turn.items().len(), 1);
        assert_eq!(turn.messages().len(), 1);
        assert_eq!(turn.messages()[0].id(), "local:message:1");
        assert_eq!(turn.messages()[0].role(), Some(MessageRole::Response));
        assert_eq!(turn.messages()[0].state(), MessageState::Completed);
        assert_eq!(turn.messages()[0].text(), "Final answer");
        update.id = MessageId("unfinished".into());
        update.role = Some(MessageRole::Commentary);
        update.update = MessageUpdate::Delta {
            text: "Partial".into(),
        };
        turn.record_message(&update)?;
        turn.finish(TurnOutcome::Interrupted, 10)?;
        let restored: super::Turn = serde_json::from_value(serde_json::to_value(turn)?)?;
        assert_eq!(restored.messages()[0].state(), MessageState::Completed);
        assert_eq!(restored.messages()[1].state(), MessageState::Interrupted);
        assert_eq!(restored.messages()[1].text(), "Partial");
        Ok(())
    }

    #[test]
    fn provider_duration_excludes_delivery_delay_and_stays_frozen() {
        let mut turn = Turn::new(
            "turn".into(),
            ProviderAddress {
                owner: None,
                thread_id: "thread".into(),
                turn_id: "turn".into(),
            },
            1000,
        );
        let event = crate::backend::BackendEvent {
            message: None,
            received_at_ms: None,
            address: Some(turn.provider().clone()),
            turn_boundary: None,
            kind: "tool".into(),
            text: None,
            data: serde_json::json!({"emittedAtMs":1383,"params":{
                "item":{"durationMs":383},"completedAtMs":1383 }}),
            activity: Some(activity("call")),
            summary: None,
            checklist_update: None,
        };
        turn.record_tool_event(event.tool_event().unwrap(), 10000)
            .unwrap();
        assert_eq!(turn.tools().next().unwrap().elapsed_ms(20000), Some(383));
        let mut late = event;
        late.data = serde_json::json!({"params":{"startedAtMs":1,"completedAtMs":20000,"item":{"durationMs":19999}}});
        turn.record_tool_event(late.tool_event().unwrap(), 20000)
            .unwrap();
        assert_eq!(turn.tools().next().unwrap().elapsed_ms(30000), Some(383));
    }

    fn activity(id: &str) -> crate::backend::ToolActivity {
        crate::backend::ToolActivity {
            id: id.into(),
            kind: crate::backend::ToolActivityKind::Command,
            title: "command".into(),
            output: Some("result".into()),
            status: Some(crate::backend::ToolStatus::Succeeded),
            change: Default::default(),
            output_delta: false,
        }
    }

    #[test]
    fn tool_runtime_ticks_independently_and_freezes_through_replay_and_reopening() {
        let mut turn = Turn::new(
            "turn".into(),
            ProviderAddress {
                owner: None,
                thread_id: "thread".into(),
                turn_id: "turn".into(),
            },
            1000,
        );
        let mut first = activity("first");
        first.status = Some(crate::backend::ToolStatus::Running);
        let mut second = first.clone();
        second.id = "second".into();
        turn.record_tool(&first, 1000).unwrap();
        turn.record_tool(&second, 2000).unwrap();
        turn.record_tool(&first, 3000).unwrap();
        assert_eq!(
            turn.tools()
                .map(|tool| tool.elapsed_ms(3500))
                .collect::<Vec<_>>(),
            vec![Some(2500), Some(1500)]
        );
        first.status = Some(crate::backend::ToolStatus::Failed);
        turn.record_tool(&first, 3500).unwrap();
        turn.record_tool(&first, 6000).unwrap();
        turn.record_tool(&activity("missing-start"), 6000).unwrap();
        turn.finish(TurnOutcome::Interrupted, 7000).unwrap();
        let restored: Turn = serde_json::from_value(serde_json::to_value(&turn).unwrap()).unwrap();
        assert_eq!(
            restored
                .tools()
                .map(|tool| tool.elapsed_ms(90_000))
                .collect::<Vec<_>>(),
            vec![Some(2500), Some(5000), None]
        );
        assert_eq!(restored.tools().next().unwrap().started_at_ms, Some(1000));
    }

    #[test]
    fn messages_and_tools_keep_order_across_boundaries_and_freeze_at_completion() {
        let mut turn = Turn::new(
            "local".into(),
            ProviderAddress {
                owner: None,
                thread_id: "thread".into(),
                turn_id: "provider".into(),
            },
            10,
        );
        turn.record_text(MessageKind::Reasoning, "Inspecting ")
            .unwrap();
        turn.record_text(MessageKind::Reasoning, "ownership")
            .unwrap();
        turn.record_tool(&activity("tool"), 1000).unwrap();
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
    fn latest_reasoning_summary_uses_the_newest_readable_section() {
        let mut turn = Turn::new(
            "local".into(),
            ProviderAddress {
                owner: None,
                thread_id: "thread".into(),
                turn_id: "provider".into(),
            },
            10,
        );
        turn.record_provider_text(
            MessageKind::ReasoningSummary,
            "Inspecting ownership",
            Some("reasoning:summary:0".into()),
            MessageDelivery::Commentary,
            false,
        )
        .unwrap();
        turn.record_provider_text(
            MessageKind::Reasoning,
            "private chain of thought",
            Some("reasoning".into()),
            MessageDelivery::Commentary,
            false,
        )
        .unwrap();
        turn.record_provider_text(
            MessageKind::ReasoningSummary,
            "Implementing the data path",
            Some("reasoning:summary:1".into()),
            MessageDelivery::Commentary,
            false,
        )
        .unwrap();

        assert_eq!(
            turn.latest_reasoning_summary(),
            Some("Implementing the data path")
        );
    }

    #[test]
    fn tool_records_survive_serialization_without_accepting_late_progress() {
        let mut turn = Turn::new(
            "local".into(),
            ProviderAddress {
                owner: None,
                thread_id: "thread".into(),
                turn_id: "execution".into(),
            },
            10,
        );
        let mut tool = activity("call");
        turn.record_tool(&tool, 1000).unwrap();
        tool.status = Some(crate::backend::ToolStatus::Running);
        tool.output = Some("late output".into());
        turn.record_tool(&tool, 1000).unwrap();
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
                    owner: None,
                    thread_id: "thread".into(),
                    turn_id: "execution".into(),
                },
                10,
            );
            turn.record_tool(&activity("completed"), 1000).unwrap();
            let mut running = activity("running");
            running.status = Some(crate::backend::ToolStatus::Running);
            turn.record_tool(&running, 1000).unwrap();
            turn.finish(outcome, 20).unwrap();
            let state: Vec<_> = turn.tools().map(|tool| tool.state()).collect();
            assert_eq!(state, [crate::turn::ToolState::Completed, expected]);
            let encoded = serde_json::to_string(&turn).unwrap();
            turn.finish(outcome, 90).unwrap();
            assert!(turn.record_tool(&running, 1000).is_err());
            assert_eq!(serde_json::to_string(&turn).unwrap(), encoded);
        }
    }

    #[test]
    fn recovered_turn_cannot_finish_before_its_start() {
        let mut turn = Turn::new(
            "local".into(),
            ProviderAddress {
                owner: None,
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
                owner: None,
                thread_id: "thread".into(),
                turn_id: "provider-turn".into(),
            },
            10,
        );
        turn.record_tool(&activity("tool"), 1000).unwrap();
        turn.record_tool(&activity("tool"), 1000).unwrap();
        assert!(turn.attribute_tool("tool", "original-task").unwrap());
        assert!(!turn.attribute_tool("tool", "another-task").unwrap());
        assert_eq!(
            turn.tools().next().unwrap().checklist_item_id.as_deref(),
            Some("original-task")
        );

        assert_eq!(turn.tools().count(), 1);
        turn.finish(TurnOutcome::Completed, 20).unwrap();
        turn.record_usage(crate::backend::usage::TokenUsage {
            output: Some(128),
            ..Default::default()
        });
        assert_eq!(turn.usage().unwrap().output, Some(128));
        turn.finish(TurnOutcome::Completed, 90).unwrap();
        assert_eq!(turn.completed_at_ms, Some(20));
        assert!(turn.record_tool(&activity("late"), 1000).is_err());
        assert!(turn.attribute_tool("tool", "late-task").is_err());
        assert!(turn.finish(TurnOutcome::Failed, 90).is_err());
    }
}
