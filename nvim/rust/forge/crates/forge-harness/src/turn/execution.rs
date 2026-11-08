use super::{Message, MessageKind, MessageState, TurnItem};
use crate::provider::ScopeOwner;
use anyhow::{Result, ensure};
use serde::{Deserialize, Serialize};

use crate::provider::TurnAddress;

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
/// Separates an active provider execution from its permanent outcome.
pub enum TurnState {
    /// Provider events may still extend the execution.
    Running,
    /// Execution timing and outcome are fixed; correlated late output may still arrive.
    Finished {
        /// The provider or cancellation boundary that settled execution.
        outcome: TurnOutcome,
    },
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(try_from = "TurnWire")]
/// Owns the identity and lifecycle of one provider turn within an exchange.
pub struct Turn {
    id: String,
    provider: TurnAddress,
    state: TurnState,
    #[serde(default)]
    completed: Option<forge_provider_api::CompletedTurn>,
    #[serde(default)]
    resolved_model: Option<forge_provider_api::ResolvedModel>,
    started_at_ms: i64,
    completed_at_ms: Option<i64>,
    reported_duration_ms: Option<u64>,
    #[serde(default)]
    errors: Vec<forge_provider_api::ProviderFailure>,
    #[serde(default)]
    usage_reports: std::collections::BTreeMap<
        forge_provider_api::identity::UsageReportId,
        forge_provider_api::TurnUsageReport,
    >,
    #[serde(default)]
    usage: Option<crate::provider::usage::TokenUsage>,
    message: Vec<Message>,
    item: Vec<TurnItem>,
    current_message: Option<usize>,
}

#[derive(Deserialize)]
struct TurnWire {
    id: String,
    provider: TurnAddress,
    state: TurnState,
    #[serde(default)]
    completed: Option<forge_provider_api::CompletedTurn>,
    #[serde(default)]
    resolved_model: Option<forge_provider_api::ResolvedModel>,
    started_at_ms: i64,
    completed_at_ms: Option<i64>,
    reported_duration_ms: Option<u64>,
    #[serde(default)]
    errors: Vec<forge_provider_api::ProviderFailure>,
    #[serde(default)]
    usage_reports: std::collections::BTreeMap<
        forge_provider_api::identity::UsageReportId,
        forge_provider_api::TurnUsageReport,
    >,
    #[serde(default)]
    usage: Option<crate::provider::usage::TokenUsage>,
    message: Vec<Message>,
    item: Vec<TurnItem>,
    current_message: Option<usize>,
}

/// Reject persisted state that contradicts canonical native completion evidence.
#[derive(Debug, thiserror::Error)]
enum TurnLoadError {
    #[error("persisted turn identity disagrees with its original invocation owner")]
    Owner,
    #[error("persisted turn contradicts its admitted item identities or content families")]
    Content,
    /// The retained outcome or native completion timestamp differs from the turn state.
    #[error("persisted turn contradicts its collected native result")]
    Completion,
    /// Retained accounting omits or misidentifies an observed final report.
    #[error("persisted turn contradicts its final native accounting")]
    Accounting,
    /// Reported measurements changed across persistence.
    #[error(transparent)]
    Usage(#[from] forge_provider_api::UsageReportError),
}

impl Turn {
    /// Admit one provider start event into the owning exchange.
    pub(crate) fn new(id: String, provider: TurnAddress, now_ms: i64) -> Self {
        Self {
            id,
            provider,
            state: TurnState::Running,
            completed: None,
            resolved_model: None,
            started_at_ms: now_ms,
            completed_at_ms: None,
            reported_duration_ms: None,
            errors: Vec::new(),
            usage_reports: Default::default(),
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

    /// Return the immutable native start evidence.
    pub fn started_at_ms(&self) -> i64 {
        self.started_at_ms
    }

    /// Return the current execution state.
    pub fn state(&self) -> TurnState {
        self.state
    }

    /// Get the immutable native result and its successful provider collection boundary.
    pub fn completed(&self) -> Option<&forge_provider_api::CompletedTurn> {
        self.completed.as_ref()
    }

    /// Return only native confirmation for this original turn.
    pub fn resolved_model(&self) -> Option<&forge_provider_api::ResolvedModel> {
        self.resolved_model.as_ref()
    }

    /// Retain native confirmation, allowing first late evidence while freezing ended known models.
    pub(crate) fn record_model(
        &mut self,
        model: &forge_provider_api::ResolvedModel,
    ) -> Result<bool> {
        if self
            .resolved_model
            .as_ref()
            .is_some_and(|previous| previous.model_id == model.model_id)
        {
            return Ok(false);
        }
        ensure!(
            self.state == TurnState::Running || self.resolved_model.is_none(),
            "completed turn rejects model changes"
        );
        self.resolved_model = Some(model.clone());
        Ok(true)
    }

    /// Apply one canonical completion atomically while preserving idempotent replay.
    pub(crate) fn complete(
        &mut self,
        completed: &forge_provider_api::CompletedTurn,
    ) -> Result<bool> {
        if let Some(retained) = &self.completed {
            ensure!(retained == completed, "collected turn result cannot change");
            return Ok(false);
        }
        ensure!(
            completed.result().completed_at_ms() >= self.started_at_ms,
            "native completion precedes its start"
        );
        let mut next = self.clone();
        for report in completed.result().final_usage() {
            next.record_usage_report(report)?;
        }
        if let Some(duration) = completed.result().reported_duration_ms() {
            next.reported_duration_ms = Some(duration);
        }
        next.finish(
            completed.result().outcome(),
            completed.result().completed_at_ms(),
        )?;
        next.completed_at_ms = Some(completed.result().completed_at_ms());
        next.completed = Some(completed.clone());
        *self = next;
        Ok(true)
    }

    /// Get failures natively attributed to this turn without changing its outcome.
    pub fn errors(&self) -> &[forge_provider_api::ProviderFailure] {
        &self.errors
    }

    /// Retain one correlated error observation without assigning terminal lifecycle authority.
    pub(crate) fn record_error(&mut self, error: forge_provider_api::ProviderFailure) -> bool {
        if self.errors.contains(&error) {
            return false;
        }
        self.errors.push(error);
        true
    }

    /// Block automatic follow-up when a native terminal reason requires explicit user action.
    pub(crate) fn requires_explicit_continuation(&self) -> bool {
        match self.state {
            TurnState::Finished {
                outcome: outcome @ TurnOutcome::Completed { .. },
            } => !outcome.permits_automatic_continuation(),
            TurnState::Running
            | TurnState::Finished {
                outcome: TurnOutcome::Failed { .. } | TurnOutcome::Interrupted,
            } => false,
        }
    }

    /// Return usage attributed to this provider execution.
    pub fn usage(&self) -> Option<&crate::provider::usage::TokenUsage> {
        self.usage.as_ref()
    }

    /// Return reported native duration without substituting host or collection elapsed time.
    pub fn duration_ms(&self) -> Option<u64> {
        self.reported_duration_ms
    }

    /// Merge accounting under its original report identity before updating turn timing.
    pub(crate) fn record_usage_report(
        &mut self,
        report: &forge_provider_api::TurnUsageReport,
    ) -> Result<bool, forge_provider_api::UsageReportError> {
        let mut retained = self
            .usage_reports
            .get(report.report_id())
            .cloned()
            .unwrap_or_else(|| report.clone());
        if self.usage_reports.contains_key(report.report_id()) && !retained.merge(report)? {
            return Ok(false);
        }
        self.usage_reports
            .insert(report.report_id().clone(), retained);
        if self
            .usage_reports
            .values()
            .all(|report| report.cumulative().is_none())
        {
            let mut reports = self
                .usage_reports
                .values()
                .filter_map(|report| report.usage().as_ref())
                .filter(|usage| {
                    usage.input.is_some()
                        || usage.cached_input.is_some()
                        || usage.reasoning.is_some()
                        || usage.output.is_some()
                });
            if let Some(mut total) = reports.next().cloned() {
                for report in reports {
                    total.accumulate(report);
                }
                self.usage = Some(total);
            }
        }
        let mut durations = self
            .usage_reports
            .values()
            .filter_map(|report| *report.reported_duration_ms())
            .peekable();
        if let Some(duration) = self
            .completed
            .as_ref()
            .and_then(|completed| completed.result().reported_duration_ms())
        {
            self.reported_duration_ms = Some(duration);
        } else if durations.peek().is_some() {
            self.reported_duration_ms =
                durations.try_fold(0_u64, |sum, duration| sum.checked_add(duration));
        }
        Ok(true)
    }

    /// Replace derived totals from the single canonical accounting ledger.
    pub(crate) fn set_usage(&mut self, usage: Option<crate::provider::usage::TokenUsage>) {
        self.usage = usage;
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

    /// Close presentation grouping without altering any item's native lifecycle.
    pub(crate) fn close_message(&mut self) {
        self.current_message = None;
    }

    /// Apply typed message or reasoning activity to its original item atomically.
    pub(crate) fn record_item_message(
        &mut self,
        item_id: &forge_provider_api::ItemId,
        event: &forge_provider_api::ItemEvent,
    ) -> Result<(), crate::provider::ProviderError> {
        use super::message::{MessageContent, MessageStart};
        use forge_provider_api::{
            ItemCompletion, ItemEvent, ItemStart, MessageClassification, MessageItemEvent,
            MessageOutcome, ProviderError, ProviderFailure, ProviderFailureKind, ProviderOperation,
        };
        let protocol = || {
            ProviderError::Failure(ProviderFailure::new(
                ProviderOperation::TurnStart,
                ProviderFailureKind::Protocol,
            ))
        };
        let existing = self
            .message
            .iter()
            .position(|message| &message.provider_id == item_id);
        if let ItemEvent::Started(start) = event {
            let (content, text) = match start {
                ItemStart::UserMessage(start) => (
                    MessageContent::User {
                        submission_id: start.submission_id.clone(),
                    },
                    &start.text,
                ),
                ItemStart::AgentMessage(start) => (
                    MessageContent::Agent {
                        classification: start.classification,
                    },
                    &start.text,
                ),
                ItemStart::Reasoning(start) => {
                    (MessageContent::Reasoning { kind: start.kind }, &start.text)
                }
                ItemStart::CommandExecution(_)
                | ItemStart::FileChange(_)
                | ItemStart::ToolCall(_) => return Err(protocol()),
            };
            let start = MessageStart {
                content: content.clone(),
                text: text.clone(),
            };
            if let Some(index) = existing {
                return if self.message[index].start == start {
                    Ok(())
                } else {
                    Err(protocol())
                };
            }
            if self.state != TurnState::Running
                || self
                    .item
                    .iter()
                    .any(|item| matches!(item, TurnItem::Tool { id } if id == item_id.as_str()))
            {
                return Err(protocol());
            }
            let index = self.message.len();
            let id = format!("{}:message:{}", self.id, index + 1);
            self.message.push(Message {
                id: id.clone(),
                provider_id: item_id.clone(),
                content,
                start,
                state: MessageState::Streaming,
                text: text.clone(),
            });
            self.item.push(TurnItem::Message { id });
            self.current_message = Some(index);
            return Ok(());
        }
        let index = existing.ok_or_else(protocol)?;
        let mut message = self.message[index].clone();
        if self.state != TurnState::Running {
            match event {
                ItemEvent::Completed(_) if message.state != MessageState::Streaming => {}
                ItemEvent::Message {
                    event: MessageItemEvent::Classified(classification),
                } if message.classification() == *classification => {}
                _ => return Err(protocol()),
            }
        }
        match event {
            ItemEvent::Message {
                event: MessageItemEvent::Classified(classification),
            } => {
                let MessageContent::Agent {
                    classification: current,
                } = &mut message.content
                else {
                    return Err(protocol());
                };
                match (*current, classification) {
                    (_, MessageClassification::Unclassified) => {}
                    (MessageClassification::Unclassified, MessageClassification::Known(role)) => {
                        *current = MessageClassification::Known(*role)
                    }
                    (
                        MessageClassification::Known(previous),
                        MessageClassification::Known(incoming),
                    ) if previous == *incoming => {}
                    (MessageClassification::Known(_), MessageClassification::Known(_)) => {
                        return Err(protocol());
                    }
                }
            }
            ItemEvent::Message {
                event: MessageItemEvent::Delta(text),
            } => {
                if message.state != MessageState::Streaming {
                    return Err(protocol());
                }
                message.text.push_str(text);
            }
            ItemEvent::Completed(completion) => {
                let completed = match (&message.content, completion) {
                    (MessageContent::User { .. }, ItemCompletion::UserMessage(value))
                    | (MessageContent::Agent { .. }, ItemCompletion::AgentMessage(value))
                    | (MessageContent::Reasoning { .. }, ItemCompletion::Reasoning(value)) => value,
                    (
                        MessageContent::User { .. }
                        | MessageContent::Agent { .. }
                        | MessageContent::Reasoning { .. },
                        ItemCompletion::UserMessage(_)
                        | ItemCompletion::AgentMessage(_)
                        | ItemCompletion::Reasoning(_)
                        | ItemCompletion::CommandExecution(_)
                        | ItemCompletion::FileChange(_)
                        | ItemCompletion::ToolCall(_),
                    ) => return Err(protocol()),
                };
                let state = match completed.outcome {
                    MessageOutcome::Completed => MessageState::Completed,
                    MessageOutcome::Interrupted => MessageState::Interrupted,
                };
                if message.state != MessageState::Streaming
                    && (message.state != state || message.text != completed.text)
                {
                    return Err(protocol());
                }
                message.state = state;
                message.text.clone_from(&completed.text);
            }
            ItemEvent::Started(_) | ItemEvent::Tool { .. } => return Err(protocol()),
        }
        if message.state != MessageState::Streaming && self.current_message == Some(index) {
            self.current_message = None;
        }
        self.message[index] = message;
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

impl TryFrom<TurnWire> for Turn {
    type Error = TurnLoadError;

    /// Validate canonical completion and report identities after loading durable history.
    fn try_from(wire: TurnWire) -> Result<Self, Self::Error> {
        let TurnWire {
            id,
            provider,
            state,
            completed,
            resolved_model,
            started_at_ms,
            completed_at_ms,
            reported_duration_ms,
            errors,
            usage_reports,
            usage,
            message,
            item,
            current_message,
        } = wire;
        if provider.thread_id != provider.scope.owner().thread_id()
            || provider.scope.turn_id().ok().as_ref() != Some(&provider.turn_id)
        {
            return Err(TurnLoadError::Owner);
        }
        match state {
            TurnState::Running if completed.is_some() || completed_at_ms.is_some() => {
                return Err(TurnLoadError::Completion);
            }
            TurnState::Finished { .. }
                if completed_at_ms.is_none_or(|at| at < started_at_ms)
                    || message
                        .iter()
                        .any(|entry| entry.state == MessageState::Streaming) =>
            {
                return Err(TurnLoadError::Completion);
            }
            _ => {}
        }
        use super::message::MessageContent;
        let mut local_ids = std::collections::HashSet::new();
        let mut native_ids = std::collections::HashSet::new();
        for entry in &message {
            if entry.id.trim().is_empty()
                || !local_ids.insert(entry.id.as_str())
                || !native_ids.insert(entry.provider_id.as_str())
            {
                return Err(TurnLoadError::Content);
            }
            let consistent = match (&entry.start.content, &entry.content) {
                (
                    MessageContent::User {
                        submission_id: before,
                    },
                    MessageContent::User {
                        submission_id: after,
                    },
                ) => before == after,
                (
                    MessageContent::Reasoning { kind: before },
                    MessageContent::Reasoning { kind: after },
                ) => before == after,
                (
                    MessageContent::Agent {
                        classification: before,
                    },
                    MessageContent::Agent {
                        classification: after,
                    },
                ) => {
                    *before == forge_provider_api::MessageClassification::Unclassified
                        || before == after
                }
                _ => false,
            };
            if !consistent {
                return Err(TurnLoadError::Content);
            }
        }
        let mut referenced = std::collections::HashSet::new();
        for entry in &item {
            match entry {
                TurnItem::Message { id } => {
                    if !local_ids.contains(id.as_str()) || !referenced.insert(id.as_str()) {
                        return Err(TurnLoadError::Content);
                    }
                }
                TurnItem::Tool { id } => {
                    if id.trim().is_empty() || !native_ids.insert(id.as_str()) {
                        return Err(TurnLoadError::Content);
                    }
                }
            }
        }
        if referenced.len() != message.len()
            || current_message.is_some_and(|index| {
                state != TurnState::Running
                    || message
                        .get(index)
                        .is_none_or(|entry| entry.state != MessageState::Streaming)
            })
        {
            return Err(TurnLoadError::Content);
        }
        if usage_reports
            .iter()
            .any(|(id, report)| id != report.report_id())
        {
            return Err(TurnLoadError::Accounting);
        }
        if let Some(completed) = &completed {
            if state
                != (TurnState::Finished {
                    outcome: completed.result().outcome(),
                })
                || completed_at_ms != Some(completed.result().completed_at_ms())
                || completed
                    .result()
                    .reported_duration_ms()
                    .is_some_and(|duration| reported_duration_ms != Some(duration))
            {
                return Err(TurnLoadError::Completion);
            }
            for report in completed.result().final_usage() {
                let mut retained = usage_reports
                    .get(report.report_id())
                    .cloned()
                    .ok_or(TurnLoadError::Accounting)?;
                if retained.merge(report)? {
                    return Err(TurnLoadError::Accounting);
                }
            }
        }
        Ok(Self {
            id,
            provider,
            state,
            completed,
            resolved_model,
            started_at_ms,
            completed_at_ms,
            reported_duration_ms,
            errors,
            usage_reports,
            usage,
            message,
            item,
            current_message,
        })
    }
}

pub use forge_provider_api::turn::TurnOutcome;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::turn::test_support::TestTurn;
    use forge_provider_api::{
        AgentMessageRole, AgentMessageStart, InspectableArguments, ItemCompletion, ItemEvent,
        ItemId, ItemStart, MessageClassification, MessageCompletion, MessageItemEvent,
        MessageOutcome, ToolCompletion, ToolDescriptor, ToolOutcome, TurnUsageReport,
    };

    /// Allocate normalized host identities before each native fixture begins.
    fn address() -> TurnAddress {
        let scope = crate::provider::InvocationScope {
            thread_id: forge_provider_api::ThreadId::from(crate::provider::ThreadOwner::root(
                "session",
            )),
            exchange_id: "exchange".into(),
            invocation_id: "invocation".into(),
        };
        TurnAddress {
            thread_id: scope.owner().thread_id(),
            turn_id: scope.turn_id().unwrap(),
            scope,
        }
    }
    /// Create assistant prose with independently observed classification.
    fn message(text: &str, role: Option<AgentMessageRole>) -> ItemEvent {
        ItemEvent::Started(ItemStart::AgentMessage(AgentMessageStart {
            text: text.into(),
            classification: role.map_or(
                MessageClassification::Unclassified,
                MessageClassification::Known,
            ),
        }))
    }
    /// Freeze assistant prose using its native final snapshot.
    fn message_end(text: &str) -> ItemEvent {
        ItemEvent::Completed(ItemCompletion::AgentMessage(MessageCompletion {
            text: text.into(),
            outcome: MessageOutcome::Completed,
        }))
    }
    /// Append content without reopening a native item.
    fn delta(text: &str) -> ItemEvent {
        ItemEvent::Message {
            event: MessageItemEvent::Delta(text.into()),
        }
    }
    /// Admit one native tool without claiming execution has started.
    fn tool_start() -> ItemEvent {
        ItemEvent::Started(ItemStart::ToolCall(ToolDescriptor {
            name: "fixture".into(),
            title: "Fixture".into(),
            semantics: Default::default(),
            arguments: InspectableArguments::new(serde_json::json!({})),
        }))
    }
    /// Freeze one native tool's exact outcome without changing reported timing.
    fn tool_end(outcome: ToolOutcome) -> ItemEvent {
        ItemEvent::Completed(ItemCompletion::ToolCall(ToolCompletion {
            outcome,
            descriptor: None,
            output: None,
            changes: Default::default(),
        }))
    }
    /// Select a normal native end without implying that a Harness goal succeeded.
    fn normal() -> TurnOutcome {
        TurnOutcome::Completed {
            stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
        }
    }

    #[test]
    fn message_replays_are_idempotent_and_invalid_updates_preserve_the_record() -> Result<()> {
        let mut turn = super::Turn::new("local".into(), address(), 0);
        let id = ItemId::new("message")?;
        assert!(turn.record_item_message(&id, &delta("orphan")).is_err());
        assert!(turn.messages().is_empty());
        let started = message("first", None);
        turn.record_item_message(&id, &started)?;
        turn.record_item_message(&id, &delta(" second"))?;
        turn.record_item_message(&id, &started)?;
        assert_eq!(turn.messages()[0].text(), "first second");
        turn.record_item_message(
            &id,
            &ItemEvent::Message {
                event: MessageItemEvent::Classified(MessageClassification::Known(
                    AgentMessageRole::Commentary,
                )),
            },
        )?;
        let completed = message_end("authoritative");
        turn.record_item_message(&id, &completed)?;
        turn.record_item_message(&id, &completed)?;
        let before = serde_json::to_value(&turn)?;
        assert!(
            turn.record_item_message(&id, &message_end("contradictory"))
                .is_err()
        );
        assert_eq!(serde_json::to_value(&turn)?, before);
        let restored: super::Turn = serde_json::from_value(before)?;
        assert_eq!(
            restored.messages()[0].classification(),
            MessageClassification::Known(AgentMessageRole::Commentary)
        );
        Ok(())
    }

    #[test]
    fn completed_turn_rejects_late_content_and_freezes_completion() -> Result<()> {
        let mut turn = TestTurn::new("local".into(), address(), 10);
        turn.record_tool("tool", &tool_start(), 11)?;
        let completed = tool_end(ToolOutcome::Succeeded);
        turn.record_tool("tool", &completed, 12)?;
        turn.record_tool("tool", &completed, 12)?;
        assert!(turn.attribute_tool("tool", "original-task")?);
        assert!(!turn.attribute_tool("tool", "another-task")?);
        assert_eq!(
            turn.tools().next().unwrap().checklist_item_id.as_deref(),
            Some("original-task")
        );
        turn.finish(normal(), 20)?;
        turn.record_usage_report(&TurnUsageReport::accounting(
            forge_provider_api::identity::UsageReportId::new("late-accounting")?,
            21,
            Some(forge_provider_api::usage::TokenUsage {
                input: None,
                cached_input: None,
                reasoning: None,
                output: Some(128),
            }),
            None,
        )?)?;
        assert_eq!(turn.usage().unwrap().output, Some(128));
        turn.finish(normal(), 90)?;
        assert_eq!(turn.completed_at_ms, Some(20));
        assert!(turn.attribute_tool("tool", "late-task").is_err());
        assert!(
            turn.finish(
                TurnOutcome::Failed {
                    error: forge_provider_api::ProviderFailure::new(
                        forge_provider_api::ProviderOperation::TurnStart,
                        forge_provider_api::ProviderFailureKind::Unknown
                    ),
                    retry: forge_provider_api::turn::RetryDecision::NotAllowed
                },
                90
            )
            .is_err()
        );
        let original = turn.provider().clone();
        let mut exchange = turn.into_exchange();
        let before = serde_json::to_value(&exchange)?;
        let late = crate::turn::test_support::item_event(&original, "late", tool_start());
        assert!(!exchange.observe_event(&late, 100)?);
        assert_eq!(serde_json::to_value(&exchange)?, before);
        Ok(())
    }
}
