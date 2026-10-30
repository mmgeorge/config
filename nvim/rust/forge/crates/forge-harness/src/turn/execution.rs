#[cfg(test)]
use super::MessageDelivery;
use super::{Message, MessageKind, MessageState, TurnItem};
use anyhow::{Result, ensure};
use serde::{Deserialize, Serialize};

use crate::backend::TurnAddress;

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
    provider: TurnAddress,
    state: TurnState,
    started_at_ms: i64,
    completed_at_ms: Option<i64>,
    reported_duration_ms: Option<u64>,
    #[serde(default)]
    usage: Option<crate::backend::usage::TokenUsage>,
    message: Vec<Message>,
    item: Vec<TurnItem>,
    current_message: Option<usize>,
}

impl Turn {
    /// Admit one provider start event into the owning exchange.
    pub(crate) fn new(id: String, provider: TurnAddress, now_ms: i64) -> Self {
        Self {
            id,
            provider,
            state: TurnState::Running,
            started_at_ms: now_ms,
            completed_at_ms: None,
            reported_duration_ms: None,
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
    pub fn provider(&self) -> &TurnAddress {
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
        self.reported_duration_ms.unwrap_or_else(|| {
            self.completed_at_ms.map_or(0, |completed| {
                completed.saturating_sub(self.started_at_ms) as u64
            })
        })
    }

    pub(crate) fn record_summary(&mut self, summary: &crate::backend::TurnSummary) {
        self.reported_duration_ms = Some(summary.duration_ms);
        if let Some(usage) = &summary.usage {
            self.record_usage(usage.clone());
        }
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
        use crate::backend::{
            MessageAddress, MessageClassification, MessageId, MessageRole, MessageUpdate,
        };
        let provider_id = provider_id.unwrap_or_else(|| {
            self.current_message
                .filter(|index| self.message[*index].kind() == kind && !snapshot)
                .map(|index| self.message[index].provider_id.as_str().to_owned())
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
        let address = MessageAddress {
            turn: self.provider.clone(),
            message_id: MessageId::Native(provider_id),
        };
        if !self
            .message
            .iter()
            .any(|message| message.provider_id == address.message_id)
        {
            self.record_message(
                &address,
                &MessageUpdate::Started {
                    classification: role.map_or(
                        MessageClassification::Unclassified,
                        MessageClassification::Known,
                    ),
                    text: text.into(),
                },
            )?;
            if !snapshot {
                return Ok(());
            }
        } else if let Some(role) = role {
            self.record_message(&address, &MessageUpdate::Classified { role })?;
        }
        Ok(self.record_message(
            &address,
            &if snapshot {
                MessageUpdate::Completed { text: text.into() }
            } else {
                MessageUpdate::Delta { text: text.into() }
            },
        )?)
    }

    pub(crate) fn record_message(
        &mut self,
        address: &crate::backend::MessageAddress,
        update: &crate::backend::MessageUpdate,
    ) -> Result<(), crate::backend::BackendError> {
        use crate::backend::{BackendError, MessageClassification, MessageUpdate};
        let protocol = |message: &str| BackendError::Protocol {
            message: message.into(),
        };
        if address.turn != self.provider || address.message_id.as_str().is_empty() {
            return Err(protocol(
                "provider message identity does not belong to this execution",
            ));
        }
        let existing = self
            .message
            .iter()
            .position(|message| message.provider_id == address.message_id);
        if let MessageUpdate::Started {
            classification,
            text,
        } = update
        {
            if text.is_empty() {
                return Err(protocol("provider message start requires actual text"));
            }
            if let Some(index) = existing {
                let start = &self.message[index].start;
                return if start.classification == *classification && start.text == *text {
                    Ok(())
                } else {
                    Err(protocol(
                        "provider message start replay changed its original content or classification",
                    ))
                };
            }
            if self.state != TurnState::Running {
                return Err(protocol("completed turn rejects new messages"));
            }
            let index = self.message.len();
            let id = format!("{}:message:{}", self.id, index + 1);
            self.message.push(Message {
                id: id.clone(),
                provider_id: address.message_id.clone(),
                classification: *classification,
                start: super::message::MessageStart {
                    classification: *classification,
                    text: text.clone(),
                },
                state: MessageState::Streaming,
                text: text.clone(),
            });
            self.item.push(TurnItem::Message { id });
            self.current_message = Some(index);
            return Ok(());
        }
        let index = existing
            .ok_or_else(|| protocol("provider message update arrived before its text start"))?;
        let message = &mut self.message[index];
        match update {
            MessageUpdate::Started { .. } => unreachable!(),
            MessageUpdate::Classified { role } => match message.classification {
                MessageClassification::Unclassified => {
                    message.classification = MessageClassification::Known(*role)
                }
                MessageClassification::Known(previous) if previous == *role => {}
                MessageClassification::Known(_) => {
                    return Err(protocol("provider message classification changed"));
                }
            },
            MessageUpdate::Delta { text } if message.state == MessageState::Streaming => {
                if self.state != TurnState::Running {
                    return Err(protocol("completed turn rejects message deltas"));
                }
                message.text.push_str(text);
                if self.current_message.is_none_or(|current| index >= current) {
                    self.current_message = Some(index);
                }
            }
            MessageUpdate::Completed { text } if message.state == MessageState::Streaming => {
                message.text.clone_from(text);
                message.state = MessageState::Completed;
                if self.current_message == Some(index) {
                    self.current_message = None;
                }
            }
            MessageUpdate::Completed { text }
                if message.state == MessageState::Completed && message.text == *text => {}
            MessageUpdate::Completed { .. } => {
                return Err(protocol(
                    "provider message terminal snapshot contradicts its settled state",
                ));
            }
            MessageUpdate::Interrupted if message.state == MessageState::Streaming => {
                message.state = MessageState::Interrupted;
                if self.current_message == Some(index) {
                    self.current_message = None;
                }
            }
            MessageUpdate::Interrupted if message.state == MessageState::Interrupted => {}
            MessageUpdate::Interrupted => {
                return Err(protocol("completed provider message rejects interruption"));
            }
            MessageUpdate::Delta { .. } => {
                return Err(protocol("terminal provider message rejects text deltas"));
            }
        }
        Ok(())
    }

    pub(crate) fn finish_call(&mut self, completed_at_ms: i64) {
        if self.completed_at_ms.is_none() {
            self.completed_at_ms = Some(completed_at_ms.max(self.started_at_ms));
        }
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
                self.finish_call(now_ms);
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
    use crate::backend::{TextUpdate, ToolOutcome, ToolUpdate};
    use crate::turn::test_support::TestTurn as Turn;

    fn scope() -> crate::backend::InvocationScope {
        crate::backend::InvocationScope {
            owner: crate::backend::ExecutionOwner::root("session"),
            exchange_id: "exchange".into(),
            invocation_id: "invocation".into(),
        }
    }

    #[test]
    fn call_completion_freezes_time_without_closing_message_admission() -> Result<()> {
        let address = TurnAddress {
            scope: scope(),
            thread_id: "thread".into(),
            turn_id: "turn".into(),
        };
        let mut turn = super::Turn::new("local".into(), address.clone(), 10);
        turn.finish_call(20);
        turn.finish_call(90);
        assert_eq!(turn.duration_ms(), 10);
        assert_eq!(turn.state(), TurnState::Running);
        let message = crate::backend::MessageAddress {
            turn: address,
            message_id: crate::backend::MessageId::Native("late".into()),
        };
        turn.record_message(
            &message,
            &crate::backend::MessageUpdate::Started {
                classification: crate::backend::MessageClassification::Known(
                    crate::backend::MessageRole::Response,
                ),
                text: "drained response".into(),
            },
        )?;
        turn.record_message(
            &message,
            &crate::backend::MessageUpdate::Completed {
                text: "drained response".into(),
            },
        )?;
        turn.finish(TurnOutcome::Completed, 100)?;
        assert_eq!(turn.duration_ms(), 10);
        assert_eq!(turn.messages()[0].text(), "drained response");
        Ok(())
    }

    #[test]
    fn native_and_synthetic_messages_with_equal_text_identity_remain_distinct() -> Result<()> {
        let address = TurnAddress {
            scope: scope(),
            thread_id: "thread".into(),
            turn_id: "turn".into(),
        };
        let mut turn = super::Turn::new("local".into(), address.clone(), 10);
        for id in [
            crate::backend::MessageId::Native("same".into()),
            crate::backend::MessageId::Synthetic("same".into()),
        ] {
            let message = crate::backend::MessageAddress {
                turn: address.clone(),
                message_id: id,
            };
            turn.record_message(
                &message,
                &crate::backend::MessageUpdate::Started {
                    classification: crate::backend::MessageClassification::Known(
                        crate::backend::MessageRole::Response,
                    ),
                    text: "answer".into(),
                },
            )?;
            turn.record_message(
                &message,
                &crate::backend::MessageUpdate::Completed {
                    text: "answer".into(),
                },
            )?;
        }
        let restored: super::Turn = serde_json::from_value(serde_json::to_value(turn)?)?;
        assert_eq!(restored.messages().len(), 2);
        assert_ne!(
            restored.messages()[0].provider_id(),
            restored.messages()[1].provider_id()
        );
        assert_eq!(restored.provider().scope, address.scope);
        Ok(())
    }

    #[test]
    fn late_reasoning_completion_does_not_replace_the_active_message() -> Result<()> {
        use crate::backend::{
            MessageAddress, MessageClassification, MessageId, MessageRole, MessageUpdate,
        };
        let address = TurnAddress {
            scope: scope(),
            thread_id: "thread".into(),
            turn_id: "turn".into(),
        };
        let mut turn = super::Turn::new("local".into(), address.clone(), 0);
        let reasoning = MessageAddress {
            turn: address.clone(),
            message_id: MessageId::Native("reasoning".into()),
        };
        let response = MessageAddress {
            turn: address,
            message_id: MessageId::Native("response".into()),
        };
        turn.record_message(
            &reasoning,
            &MessageUpdate::Started {
                classification: MessageClassification::Known(MessageRole::ThoughtSummary),
                text: "Inspecting".into(),
            },
        )?;
        turn.record_message(
            &response,
            &MessageUpdate::Started {
                classification: MessageClassification::Known(MessageRole::Response),
                text: "Answer".into(),
            },
        )?;
        turn.record_message(
            &reasoning,
            &MessageUpdate::Delta {
                text: " the source".into(),
            },
        )?;
        assert_eq!(turn.current_message, Some(1));
        turn.record_message(
            &response,
            &MessageUpdate::Completed {
                text: "Final answer".into(),
            },
        )?;
        assert_eq!(turn.current_message, None);
        turn.record_message(
            &reasoning,
            &MessageUpdate::Completed {
                text: "Reconciled summary".into(),
            },
        )?;
        assert_eq!(turn.current_message, None);
        assert_eq!(turn.latest_reasoning_summary(), Some("Reconciled summary"));
        Ok(())
    }

    #[test]
    fn message_replays_are_idempotent_and_invalid_updates_preserve_the_record() -> Result<()> {
        use crate::backend::{
            BackendError, MessageAddress, MessageClassification, MessageId, MessageRole,
            MessageUpdate,
        };
        let address = TurnAddress {
            scope: scope(),
            thread_id: "thread".into(),
            turn_id: "turn".into(),
        };
        let mut turn = super::Turn::new("local".into(), address.clone(), 0);
        let message = MessageAddress {
            turn: address,
            message_id: MessageId::Native("message".into()),
        };
        assert!(matches!(
            turn.record_message(
                &message,
                &MessageUpdate::Delta {
                    text: "orphan".into()
                }
            ),
            Err(BackendError::Protocol { .. })
        ));
        assert!(turn.messages().is_empty());
        let started = MessageUpdate::Started {
            classification: MessageClassification::Unclassified,
            text: "first".into(),
        };
        turn.record_message(&message, &started)?;
        turn.record_message(
            &message,
            &MessageUpdate::Delta {
                text: " second".into(),
            },
        )?;
        turn.record_message(&message, &started)?;
        assert_eq!(turn.messages()[0].text(), "first second");
        turn.record_message(
            &message,
            &MessageUpdate::Classified {
                role: MessageRole::Commentary,
            },
        )?;
        let completed = MessageUpdate::Completed {
            text: "authoritative".into(),
        };
        turn.record_message(&message, &completed)?;
        turn.record_message(&message, &completed)?;
        assert!(matches!(
            turn.record_message(
                &message,
                &MessageUpdate::Completed {
                    text: "contradictory".into()
                }
            ),
            Err(BackendError::Protocol { .. })
        ));
        assert_eq!(turn.messages()[0].text(), "authoritative");
        let restored: super::Turn = serde_json::from_value(serde_json::to_value(turn)?)?;
        assert_eq!(
            restored.messages()[0].classification(),
            MessageClassification::Known(MessageRole::Commentary)
        );
        Ok(())
    }

    #[test]
    fn typed_message_retains_identity_role_and_terminal_lifecycle() -> Result<()> {
        use crate::backend::{
            MessageAddress, MessageClassification, MessageId, MessageRole, MessageUpdate,
        };
        let mut turn = super::Turn::new(
            "local".into(),
            TurnAddress {
                scope: scope(),
                thread_id: "thread".into(),
                turn_id: "provider".into(),
            },
            0,
        );
        let mut address = MessageAddress {
            turn: turn.provider().clone(),
            message_id: MessageId::Native("message".into()),
        };
        let mut update = MessageUpdate::Started {
            classification: MessageClassification::Unclassified,
            text: "Draft".into(),
        };
        turn.record_message(&address, &update)?;
        assert_eq!(turn.messages()[0].kind(), MessageKind::Unknown);
        assert_eq!(turn.messages()[0].delivery(), MessageDelivery::Unknown);
        update = MessageUpdate::Delta {
            text: " outline".into(),
        };
        turn.record_message(&address, &update)?;
        assert_eq!(turn.messages()[0].text(), "Draft outline");
        let mut turn: super::Turn = serde_json::from_value(serde_json::to_value(turn)?)?;
        assert_eq!(turn.messages()[0].state(), MessageState::Streaming);
        assert_eq!(turn.messages()[0].role(), None);
        update = MessageUpdate::Completed {
            text: "Final answer".into(),
        };
        turn.record_message(&address, &update)?;
        update = MessageUpdate::Classified {
            role: MessageRole::Response,
        };
        turn.record_message(&address, &update)?;
        update = MessageUpdate::Delta {
            text: "late delta".into(),
        };
        assert!(turn.record_message(&address, &update).is_err());
        update = MessageUpdate::Interrupted;
        assert!(turn.record_message(&address, &update).is_err());
        assert_eq!(turn.items().len(), 1);
        assert_eq!(turn.messages().len(), 1);
        assert_eq!(turn.messages()[0].id(), "local:message:1");
        assert_eq!(turn.messages()[0].role(), Some(MessageRole::Response));
        assert_eq!(turn.messages()[0].state(), MessageState::Completed);
        assert_eq!(turn.messages()[0].text(), "Final answer");
        address.message_id = MessageId::Native("unfinished".into());
        update = MessageUpdate::Started {
            classification: MessageClassification::Known(MessageRole::Commentary),
            text: "Partial".into(),
        };
        turn.record_message(&address, &update)?;
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
            TurnAddress {
                scope: scope(),
                thread_id: "thread".into(),
                turn_id: "turn".into(),
            },
            1000,
        );
        turn.record_tool("call", ToolUpdate::Started, 1000).unwrap();
        turn.record_tool("call", succeeded(), 1383).unwrap();
        assert_eq!(turn.tools().next().unwrap().elapsed_ms(20000), Some(383));
        turn.record_tool("call", ToolUpdate::Started, 20000)
            .unwrap();
        turn.record_tool("call", succeeded(), 20000).unwrap();
        assert_eq!(turn.tools().next().unwrap().elapsed_ms(30000), Some(383));
    }

    fn succeeded() -> ToolUpdate {
        ToolUpdate::Finished {
            outcome: ToolOutcome::Succeeded,
        }
    }

    #[test]
    fn tool_runtime_ticks_independently_and_freezes_through_replay_and_reopening() {
        let mut turn = Turn::new(
            "turn".into(),
            TurnAddress {
                scope: scope(),
                thread_id: "thread".into(),
                turn_id: "turn".into(),
            },
            1000,
        );
        turn.record_tool("first", ToolUpdate::Started, 1000)
            .unwrap();
        turn.record_tool("second", ToolUpdate::Started, 2000)
            .unwrap();
        turn.record_tool("first", ToolUpdate::Started, 3000)
            .unwrap();
        assert_eq!(
            turn.tools()
                .map(|tool| tool.elapsed_ms(3500))
                .collect::<Vec<_>>(),
            vec![Some(2500), Some(1500)]
        );
        let failed = ToolUpdate::Finished {
            outcome: ToolOutcome::Failed {
                diagnostic: crate::backend::ToolDiagnostic {
                    code: None,
                    message: "failed".into(),
                },
            },
        };
        turn.record_tool("first", failed.clone(), 3500).unwrap();
        turn.record_tool("first", failed, 6000).unwrap();
        turn.record_tool("missing-start", succeeded(), 6000)
            .unwrap();
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
            TurnAddress {
                scope: scope(),
                thread_id: "thread".into(),
                turn_id: "provider".into(),
            },
            10,
        );
        turn.record_text(MessageKind::Reasoning, "Inspecting ")
            .unwrap();
        turn.record_text(MessageKind::Reasoning, "ownership")
            .unwrap();
        turn.record_tool("tool", succeeded(), 1000).unwrap();
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
            TurnAddress {
                scope: scope(),
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
            TurnAddress {
                scope: scope(),
                thread_id: "thread".into(),
                turn_id: "execution".into(),
            },
            10,
        );
        turn.record_tool("call", succeeded(), 1000).unwrap();
        turn.record_tool("call", ToolUpdate::Started, 1000).unwrap();
        turn.record_tool(
            "call",
            ToolUpdate::Output {
                update: TextUpdate::Replace("late output".into()),
            },
            1000,
        )
        .unwrap();
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
                TurnAddress {
                    scope: scope(),
                    thread_id: "thread".into(),
                    turn_id: "execution".into(),
                },
                10,
            );
            turn.record_tool("completed", succeeded(), 1000).unwrap();
            turn.record_tool("running", ToolUpdate::Started, 1000)
                .unwrap();
            turn.finish(outcome, 20).unwrap();
            let state: Vec<_> = turn.tools().map(|tool| tool.state()).collect();
            assert_eq!(state, [crate::turn::ToolState::Completed, expected]);
            let encoded = serde_json::to_string(&turn).unwrap();
            turn.finish(outcome, 90).unwrap();
            assert!(
                turn.record_tool("running", ToolUpdate::Started, 1000)
                    .is_err()
            );
            assert_eq!(serde_json::to_string(&turn).unwrap(), encoded);
        }
    }

    #[test]
    fn recovered_turn_cannot_finish_before_its_start() {
        let mut turn = Turn::new(
            "local".into(),
            TurnAddress {
                scope: scope(),
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
            TurnAddress {
                scope: scope(),
                thread_id: "thread".into(),
                turn_id: "provider-turn".into(),
            },
            10,
        );
        turn.record_tool("tool", succeeded(), 1000).unwrap();
        turn.record_tool("tool", succeeded(), 1000).unwrap();
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
        assert!(turn.record_tool("late", succeeded(), 1000).is_err());
        assert!(turn.attribute_tool("tool", "late-task").is_err());
        assert!(turn.finish(TurnOutcome::Failed, 90).is_err());
    }
}
