use super::*;
use crate::agent::Delegation;
use crate::plan::PlanElicitation;
use crate::provider::ScopeOwner;
use serde::{Deserialize, Serialize};

#[derive(Debug)]
enum InputAppend {
    User,
    Acknowledged,
}

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum ExchangeOutcome {
    Complete,
    Failed,
    Cancelled,
    Interrupted,
}

impl ExchangeOutcome {
    pub fn state(self) -> ExchangeState {
        match self {
            Self::Complete => ExchangeState::Complete,
            Self::Failed => ExchangeState::Failed,
            Self::Cancelled => ExchangeState::Cancelled,
            Self::Interrupted => ExchangeState::Interrupted,
        }
    }
}

impl TryFrom<ExchangeState> for ExchangeOutcome {
    type Error = anyhow::Error;

    fn try_from(state: ExchangeState) -> Result<Self, Self::Error> {
        match state {
            ExchangeState::Complete => Ok(Self::Complete),
            ExchangeState::Failed => Ok(Self::Failed),
            ExchangeState::Cancelled => Ok(Self::Cancelled),
            ExchangeState::Interrupted => Ok(Self::Interrupted),
            _ => anyhow::bail!("exchange state has no terminal outcome"),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
/// Resolves work and waits independently from the elapsed-time clock.
pub(crate) enum ExchangeActivity {
    /// The open exchange owns provider or Harness processing.
    Active,
    /// Permission admission waits for a user decision.
    AwaitingApproval,
    /// A question waits without an active clarification execution.
    AwaitingInput,
    /// Delegated execution owns the parent's pending result.
    WaitingForAgent,
    /// An interrupted provider execution awaits explicit resumption.
    Paused,
    /// Provider context compaction owns the live status.
    Compacting {
        /// Provider receipt time that starts the current compaction interval.
        started_at_ms: i64,
    },
    /// The exchange has no active execution ownership.
    Settled,
}

/// Owns one conversation exchange and its provider turns.
#[derive(Clone, Debug, Serialize)]
pub struct Exchange {
    #[serde(default)]
    pub origin: super::ExchangeOrigin,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub input_message_id: Option<String>,
    /// Runtime-only source for deferred tool output. Durable records keep output in their chunk table.
    #[serde(skip)]
    pub(crate) output_store: Option<std::path::PathBuf>,
    /// Immutable output identities retained by fork snapshots independently of source sessions.
    #[serde(default, skip_serializing_if = "std::collections::HashMap::is_empty")]
    pub(crate) archived_output: std::collections::HashMap<String, String>,
    #[serde(default)]
    /// Current provider retry, cleared when execution settles.
    pub provider_retry: Option<crate::provider::retry::ProviderRetry>,
    #[serde(default)]
    /// Retains one check baseline across queued work and automatic repair turns.
    pub check_owner: Option<String>,
    /// Durable failure that keeps cleanup retryable without replaying execution.
    #[serde(default)]
    pub finalization_error: Option<String>,
    /// Persistent agent identity that owns this exchange.
    pub agent_id: String,
    pub id: String,
    pub session_id: String,
    pub ordinal: u64,
    pub prompt: String,
    #[serde(default)]
    /// Approval selection captured when this exchange starts.
    pub approval_mode: Option<crate::session::ApprovalMode>,
    #[serde(flatten)]
    pub(crate) phase: super::ExchangePhase,
    pub checkpoint_before: Option<String>,
    pub checkpoint_after: Option<String>,
    pub attributed_diff_text: Option<String>,
    pub checkpoint_diff_text: Option<String>,
    pub attributed_matches_checkpoint: bool,
    /// Retains terminal approvals and their response source after the picker closes.
    #[serde(default)]
    pub approval_resolution: Vec<crate::provider::PermissionApprovalEvent>,
    /// Committed host file operations retain their own callback identity.
    #[serde(default)]
    pub(crate) host_file_changes: Vec<crate::tools::lsp::HostFileChange>,
    /// Owns the setup question boundary independently of the active plan.
    pub check_setup: Option<crate::checks::CheckSetupState>,
    pub created_at_ms: i64,
    pub node_list: Vec<ExchangeNode>,
    #[serde(default)]
    pub entry: Vec<super::ExchangeEntry>,
    #[serde(skip)]
    pub tool_counter: super::ToolCounter,
    #[serde(default)]
    /// Provider executions owned by this request, independently of UI grouping.
    pub turn: Vec<crate::turn::Turn>,
    #[serde(flatten)]
    pub(crate) question: super::ExchangeQuestion,
    #[serde(flatten)]
    pub(crate) clock: super::ExchangeClock,
    #[serde(default)]
    /// Retains usage identities and observed tool, approval, and delegated wait occupancy.
    pub metrics: ExchangeMetrics,
    #[serde(default)]
    pub comment: Vec<ExchangeComment>,
    #[serde(default)]
    pub checklist: Option<ChecklistSnapshot>,
}

#[derive(Deserialize)]
#[serde(remote = "Exchange")]
struct ExchangeRecord {
    #[serde(default)]
    pub origin: super::ExchangeOrigin,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub input_message_id: Option<String>,
    /// Runtime-only source for deferred tool output. Durable records keep output in their chunk table.
    #[serde(skip)]
    pub(crate) output_store: Option<std::path::PathBuf>,
    /// Immutable output identities retained by fork snapshots independently of source sessions.
    #[serde(default, skip_serializing_if = "std::collections::HashMap::is_empty")]
    pub(crate) archived_output: std::collections::HashMap<String, String>,
    #[serde(default)]
    /// Current provider retry, cleared when execution settles.
    pub provider_retry: Option<crate::provider::retry::ProviderRetry>,
    #[serde(default)]
    /// Retains one check baseline across queued work and automatic repair turns.
    pub check_owner: Option<String>,
    /// Durable failure that keeps cleanup retryable without replaying execution.
    #[serde(default)]
    pub finalization_error: Option<String>,
    /// Persistent agent identity that owns this exchange.
    pub agent_id: String,
    pub id: String,
    pub session_id: String,
    pub ordinal: u64,
    pub prompt: String,
    #[serde(default)]
    /// Approval selection captured when this exchange starts.
    pub approval_mode: Option<crate::session::ApprovalMode>,
    #[serde(flatten)]
    pub(crate) phase: super::ExchangePhase,
    pub checkpoint_before: Option<String>,
    pub checkpoint_after: Option<String>,
    pub attributed_diff_text: Option<String>,
    pub checkpoint_diff_text: Option<String>,
    pub attributed_matches_checkpoint: bool,
    /// Retains terminal approvals and their response source after the picker closes.
    #[serde(default)]
    pub approval_resolution: Vec<crate::provider::PermissionApprovalEvent>,
    /// Committed host file operations retain their own callback identity.
    #[serde(default)]
    pub(crate) host_file_changes: Vec<crate::tools::lsp::HostFileChange>,
    /// Owns the setup question boundary independently of the active plan.
    pub check_setup: Option<crate::checks::CheckSetupState>,
    pub created_at_ms: i64,
    pub node_list: Vec<ExchangeNode>,
    #[serde(default)]
    pub entry: Vec<super::ExchangeEntry>,
    #[serde(skip)]
    pub tool_counter: super::ToolCounter,
    #[serde(default)]
    /// Provider executions owned by this request, independently of UI grouping.
    pub turn: Vec<crate::turn::Turn>,
    #[serde(flatten)]
    pub(crate) question: super::ExchangeQuestion,
    #[serde(flatten)]
    pub(crate) clock: super::ExchangeClock,
    #[serde(default)]
    /// Retains usage identities and observed tool, approval, and delegated wait occupancy.
    pub metrics: ExchangeMetrics,
    #[serde(default)]
    pub comment: Vec<ExchangeComment>,
    #[serde(default)]
    pub checklist: Option<ChecklistSnapshot>,
}

impl<'de> Deserialize<'de> for Exchange {
    fn deserialize<Deserializer: serde::Deserializer<'de>>(
        deserializer: Deserializer,
    ) -> Result<Self, Deserializer::Error> {
        let mut exchange = ExchangeRecord::deserialize(deserializer)?;
        let mut retained_tools = std::collections::HashSet::new();
        for entry in &exchange.entry {
            if let super::ExchangeEntry::Commentary { commentary } = entry {
                for tools in &commentary.tool {
                    let turn = exchange
                        .turn
                        .iter()
                        .find(|turn| turn.id() == tools.turn_id)
                        .ok_or_else(|| {
                            serde::de::Error::custom("tool store has no original turn")
                        })?;
                    if !tools.store.belongs_to(turn.provider()) {
                        return Err(serde::de::Error::custom(
                            "tool store disagrees with its original turn",
                        ));
                    }
                    for tool in tools.store.iter() {
                        if !retained_tools.insert((turn.id().to_owned(), tool.id.clone())) || !turn.items().iter().any(|item| matches!(item, crate::turn::TurnItem::Tool { id } if id == &tool.id)) {
                            return Err(serde::de::Error::custom("tool has duplicate or unadmitted transcript ownership"));
                        }
                    }
                }
            }
        }
        for turn in &exchange.turn {
            for item in turn.items() {
                if let crate::turn::TurnItem::Tool { id } = item
                    && !retained_tools.contains(&(turn.id().to_owned(), id.clone()))
                {
                    return Err(serde::de::Error::custom(
                        "admitted tool has no retained original store",
                    ));
                }
            }
        }
        let changes = std::mem::take(&mut exchange.host_file_changes);
        for change in changes {
            if !exchange
                .record_host_file_change(&change)
                .map_err(serde::de::Error::custom)?
            {
                return Err(serde::de::Error::custom(
                    "duplicate committed host file operation",
                ));
            }
        }
        exchange
            .reconcile_entries()
            .map_err(serde::de::Error::custom)?;
        exchange.rebuild_tool_counter();
        Ok(exchange)
    }
}

impl Exchange {
    pub fn awaiting_input(&self) -> bool {
        self.question.waiting()
    }
    pub fn elicitation(&self) -> Option<&PlanElicitation> {
        self.question.elicitation()
    }
    pub fn elicitation_mut(&mut self) -> Option<&mut PlanElicitation> {
        self.question.elicitation_mut()
    }
    pub fn take_elicitation(&mut self) -> Option<PlanElicitation> {
        self.question.take_elicitation()
    }
    pub fn set_awaiting_input(&mut self, waiting: bool) {
        self.question.set_waiting(waiting);
    }
    pub fn set_elicitation(&mut self, elicitation: Option<PlanElicitation>) {
        self.question.set_elicitation(elicitation);
    }
    pub fn duration_ms(&self) -> u64 {
        self.clock.duration_ms()
    }
    pub fn execution_started_at_ms(&self) -> Option<i64> {
        self.clock.started_at_ms()
    }
    pub fn state(&self) -> ExchangeState {
        self.phase.state()
    }
    pub fn is_finished(&self) -> bool {
        matches!(self.phase, super::ExchangePhase::Finished(_))
    }
    pub fn completed_at_ms(&self) -> Option<i64> {
        self.phase.completed_at_ms()
    }
    pub fn disposition(&self) -> HistoryDisposition {
        self.phase.disposition()
    }
    pub fn finalization_outcome(&self) -> Option<ExchangeOutcome> {
        self.phase.outcome()
    }
    /// Create a causal root record for manual child admission without a root model turn.
    pub(crate) fn agent_admission(
        session_id: &str,
        ordinal: u64,
        prompt: String,
        now_ms: i64,
    ) -> Self {
        let delegation = Delegation {
            id: uuid::Uuid::new_v4().to_string(),
            child_agent_id: crate::session::HarnessSession::primary_agent_id(session_id),
            child_exchange_id: uuid::Uuid::new_v4().to_string(),
            parent_exchange_id: String::new(),
            parent_turn_id: None,
            task: prompt,
            created_at_ms: now_ms,
        };
        let mut exchange = Self::delegated(session_id, &delegation, ordinal);
        exchange.phase = super::ExchangePhase::Running;
        exchange
    }

    /// Append the reserved task identity without allocating a second child exchange.
    pub(crate) fn append_agent_task(
        &mut self,
        task: &crate::agent::AgentTask,
    ) -> anyhow::Result<bool> {
        anyhow::ensure!(
            task.session_id == self.session_id && task.parent_exchange_id == self.id,
            "child admission crosses causal ownership"
        );
        let appended = self.append_delegation(
            &task.agent_id,
            &task.operation_id,
            &task.message,
            task.created_at_ms,
        )?;
        if appended && let Some(ExchangeNode::AgentReference { agent }) = self.node_list.last_mut()
        {
            agent.child_exchange_id = task.id.clone();
        }
        Ok(appended)
    }
    /// Return the latest readable reasoning summary from the active provider turn.
    pub fn latest_reasoning_summary(&self) -> Option<&str> {
        self.turn
            .iter()
            .rev()
            .find(|turn| turn.state() == crate::turn::TurnState::Running)
            .and_then(crate::turn::Turn::latest_reasoning_summary)
    }

    /// Admit a delegated request before its first provider turn arrives.
    pub(crate) fn delegated(session_id: &str, delegation: &Delegation, ordinal: u64) -> Self {
        Self {
            id: delegation.child_exchange_id.clone(),
            session_id: session_id.to_owned(),
            agent_id: delegation.child_agent_id.clone(),
            ordinal,
            prompt: delegation.task.clone(),
            origin: crate::exchange::ExchangeOrigin::User,
            approval_mode: None,
            phase: crate::exchange::ExchangePhase::Queued,
            finalization_error: None,
            check_owner: None,
            output_store: None,
            archived_output: Default::default(),
            input_message_id: None,
            provider_retry: None,
            checkpoint_before: None,
            checkpoint_after: None,
            attributed_diff_text: None,
            checkpoint_diff_text: None,
            attributed_matches_checkpoint: false,
            approval_resolution: Vec::new(),
            host_file_changes: Vec::new(),
            check_setup: None,
            created_at_ms: delegation.created_at_ms,
            tool_counter: Default::default(),
            entry: Vec::new(),
            node_list: Vec::new(),
            turn: Vec::new(),
            metrics: ExchangeMetrics::default(),
            comment: Vec::new(),
            checklist: None,

            question: crate::exchange::ExchangeQuestion::None,
            clock: crate::exchange::ExchangeClock::Paused { elapsed_ms: 0 },
        }
    }

    /// Admit a registry-proven native child without manufacturing a submitted host input.
    pub(crate) fn observed(
        id: String,
        ordinal: u64,
        address: crate::provider::TurnAddress,
        started_at_ms: i64,
    ) -> anyhow::Result<Self> {
        anyhow::ensure!(
            !id.trim().is_empty() && ordinal > 0,
            "observed exchange identity is incomplete"
        );
        anyhow::ensure!(
            address.thread_id == address.scope.owner().thread_id()
                && address.turn_id == address.scope.turn_id()?,
            "observed turn identity disagrees with original owner"
        );
        let mut exchange = Self {
            id: id.clone(),
            session_id: address.scope.owner().session_id().as_str().to_owned(),
            agent_id: address.scope.owner().agent_id().as_str().to_owned(),
            ordinal,
            prompt: String::new(),
            origin: crate::exchange::ExchangeOrigin::NativeAgent,
            approval_mode: None,
            phase: crate::exchange::ExchangePhase::Running,
            finalization_error: None,
            check_owner: None,
            output_store: None,
            archived_output: Default::default(),
            input_message_id: None,
            provider_retry: None,
            checkpoint_before: None,
            checkpoint_after: None,
            attributed_diff_text: None,
            checkpoint_diff_text: None,
            attributed_matches_checkpoint: false,
            approval_resolution: Vec::new(),
            host_file_changes: Vec::new(),
            check_setup: None,
            created_at_ms: started_at_ms,
            tool_counter: Default::default(),
            entry: Vec::new(),
            node_list: Vec::new(),
            turn: vec![crate::turn::Turn::new(
                format!("{}:turn:1", id),
                address,
                started_at_ms,
            )],
            metrics: ExchangeMetrics::default(),
            comment: Vec::new(),
            checklist: None,

            question: crate::exchange::ExchangeQuestion::None,
            clock: crate::exchange::ExchangeClock::Paused { elapsed_ms: 0 },
        };
        exchange.clock.resume(started_at_ms);
        Ok(exchange)
    }

    /// Freeze execution for persistence, allowing later cleanup or capture failure to downgrade the outcome.
    pub fn begin_finalization(
        &mut self,
        outcome: ExchangeOutcome,
        now_ms: i64,
    ) -> anyhow::Result<()> {
        anyhow::ensure!(
            outcome == ExchangeOutcome::Failed
                || self
                    .finalization_outcome()
                    .is_none_or(|previous| previous == outcome),
            "finalization cannot change the recorded outcome"
        );
        anyhow::ensure!(
            matches!(
                self.state(),
                ExchangeState::Running | ExchangeState::Finalizing
            ),
            "only an open exchange can finalize"
        );
        self.settle_tools(now_ms);
        self.pause(now_ms);
        self.metrics.settle(self.elapsed(now_ms));
        self.set_awaiting_input(false);
        self.phase = super::ExchangePhase::Finalizing(outcome);
        self.provider_retry = None;
        Ok(())
    }

    /// Retain one compaction at its first observed position and merge terminal evidence.
    pub(crate) fn observe_compaction(
        &mut self,
        compaction: crate::provider::Compaction,
        now_ms: i64,
    ) {
        if let Some(ExchangeNode::Compaction {
            compaction: existing,
            ..
        }) = self.node_list.iter_mut().find(|node| {
            matches!(node, ExchangeNode::Compaction { compaction: existing, .. }
                if existing.id == compaction.id && existing.thread_id == compaction.thread_id)
        }) {
            existing.observe(compaction, now_ms);
        } else {
            self.close_running_messages();
            self.node_list.push(ExchangeNode::Compaction {
                id: format!(
                    "{}:compaction:{}:{}",
                    self.id, compaction.thread_id, compaction.id
                ),
                compaction: (compaction.observed(now_ms)).into(),
            });
        }
    }

    /// Stop active compaction presentation when its provider execution ends without confirmation.
    pub(crate) fn unconfirm_compaction(&mut self, thread_id: &str, now_ms: i64) {
        for node in &mut self.node_list {
            if let ExchangeNode::Compaction { compaction, .. } = node
                && compaction.thread_id == thread_id
                && compaction.state == crate::provider::CompactionState::Running
            {
                compaction.state = crate::provider::CompactionState::Unconfirmed;
                compaction.completed_at_ms = Some(now_ms);
            }
        }
    }

    /// Retain a committed host effect without rewriting native item snapshots.
    pub(crate) fn record_host_file_change(
        &mut self,
        change: &crate::tools::lsp::HostFileChange,
    ) -> anyhow::Result<bool> {
        anyhow::ensure!(
            self.owns_scope(&crate::provider::EventScope::Invocation {
                invocation: change.scope.clone()
            }),
            "host file operation crosses invocation ownership"
        );
        if let Some(item_id) = &change.native_item_id {
            anyhow::ensure!(
                self.turn
                    .iter()
                    .any(|turn| turn.provider().scope == change.scope
                        && self
                            .tools_for_turn(turn.id())
                            .any(|tool| tool.id == item_id.as_str())),
                "host file operation has no positively correlated native item"
            );
        }
        if let Some(previous) = self
            .host_file_changes
            .iter_mut()
            .find(|previous| previous.scope == change.scope && previous.call_id == change.call_id)
        {
            anyhow::ensure!(
                previous.changes == change.changes,
                "committed host file operation cannot change"
            );
            match (&previous.native_item_id, &change.native_item_id) {
                (Some(before), Some(after)) => anyhow::ensure!(
                    before == after,
                    "host callback cannot move to another native item"
                ),
                (None, Some(item)) => {
                    previous.native_item_id = Some(item.clone());
                    return Ok(true);
                }
                _ => {}
            }
            return Ok(false);
        }
        self.host_file_changes.push(change.clone());
        Ok(true)
    }

    /// Apply fixture observations through the same original-owner reducers.
    #[cfg(test)]
    pub(crate) fn observe_event(
        &mut self,
        event: &crate::provider::HarnessEvent,
        now_ms: i64,
    ) -> anyhow::Result<bool> {
        match event {
            crate::provider::HarnessEvent::Native { scope, event, .. } => {
                let event = event.as_ref();
                self.observe_provider_event(scope, event, now_ms)
            }
            crate::provider::HarnessEvent::Host { scope, event } => {
                self.observe_host_event(scope, event, now_ms)
            }
        }
    }

    fn owns_scope(&self, scope: &crate::provider::EventScope) -> bool {
        scope.invocation().is_some_and(|scope| {
            (scope.exchange_id.as_str() == self.id
                || self.turn.iter().any(|turn| &turn.provider().scope == scope))
                && scope.owner().session_id().as_str() == self.session_id
                && scope.owner().agent_id().as_str() == self.agent_id
        })
    }

    /// Reduce native evidence without deriving ownership from the currently visible turn.
    pub(crate) fn observe_provider_event(
        &mut self,
        scope: &crate::provider::EventScope,
        event: &forge_provider_api::ProviderEvent,
        now_ms: i64,
    ) -> anyhow::Result<bool> {
        let mut candidate = self.clone();
        let changed = candidate.reduce_provider_event(scope, event, now_ms)?;
        if changed {
            *self = candidate;
        }
        Ok(changed)
    }

    fn reduce_provider_event(
        &mut self,
        scope: &crate::provider::EventScope,
        event: &forge_provider_api::ProviderEvent,
        now_ms: i64,
    ) -> anyhow::Result<bool> {
        use forge_provider_api::{ItemEvent, ItemStart, ProviderEvent, TurnEvent};
        if !self.owns_scope(scope) {
            return Ok(false);
        }
        let (thread_id, turn_id) = match event {
            ProviderEvent::Turn {
                thread_id, turn_id, ..
            }
            | ProviderEvent::Item {
                thread_id, turn_id, ..
            } => (thread_id, turn_id),
            _ => return Ok(false),
        };
        let address = crate::provider::TurnAddress {
            thread_id: thread_id.clone(),
            turn_id: turn_id.clone(),
            scope: scope.invocation().expect("validated invocation").clone(),
        };
        if let ProviderEvent::Turn {
            event: TurnEvent::Started(start),
            ..
        } = event
        {
            if let Some(existing) = self.turn.iter().find(|turn| turn.provider() == &address) {
                anyhow::ensure!(
                    existing.started_at_ms() == start.started_at_ms,
                    "native start timestamp cannot change"
                );
                return Ok(false);
            }
            self.start_turn(address, start.started_at_ms)?;
            return Ok(true);
        }
        let Some(index) = self
            .turn
            .iter()
            .position(|turn| turn.provider() == &address)
        else {
            return Ok(false);
        };
        match event {
            ProviderEvent::Turn {
                event: TurnEvent::Completed(completed),
                ..
            } => {
                let mut candidate = self.turn[index].clone();
                if !candidate.complete(completed)? {
                    return Ok(false);
                }
                self.metrics.complete_turn(&address, completed.result())?;
                candidate.set_usage(self.metrics.turn_usage(&address));
                self.turn[index] = candidate;
                let turn_id = self.turn[index].id().to_owned();
                self.finish_tools(
                    &turn_id,
                    completed.result().outcome(),
                    completed.result().completed_at_ms(),
                );
                self.rebuild_tool_counter();
                let ids = self
                    .tools_for_turn(&turn_id)
                    .map(|tool| format!("tool:{turn_id}:{}", tool.id))
                    .collect::<Vec<_>>();
                let elapsed = self.elapsed(now_ms);
                for id in ids {
                    self.metrics.block(id, false, elapsed);
                }
            }
            ProviderEvent::Turn {
                event: TurnEvent::Usage(report),
                ..
            } => {
                let mut candidate = self.turn[index].clone();
                if !candidate.record_usage_report(report)? {
                    return Ok(false);
                }
                self.metrics.usage_report(&address, report)?;
                candidate.set_usage(self.metrics.turn_usage(&address));
                self.turn[index] = candidate;
            }
            ProviderEvent::Turn {
                event: TurnEvent::ChecklistUpdated(_),
                ..
            } => {
                anyhow::ensure!(
                    self.turn[index].completed().is_none(),
                    "completed turn rejects checklist updates"
                );
                // The broker owns checklist aggregation after this original-turn admission check.
            }
            ProviderEvent::Turn {
                event: TurnEvent::ModelResolved(model),
                ..
            } => {
                if !self.turn[index].record_model(model)? {
                    return Ok(false);
                }
            }
            ProviderEvent::Turn {
                event: TurnEvent::Error { error },
                ..
            } => {
                if !self.turn[index].record_error(*error) {
                    return Ok(false);
                }
            }
            ProviderEvent::Item { item_id, event, .. } => {
                let tool = matches!(
                    event,
                    ItemEvent::Started(
                        ItemStart::CommandExecution(_)
                            | ItemStart::FileChange(_)
                            | ItemStart::ToolCall(_)
                    ) | ItemEvent::Tool { .. }
                        | ItemEvent::Completed(
                            forge_provider_api::ItemCompletion::CommandExecution(_)
                                | forge_provider_api::ItemCompletion::FileChange(_)
                                | forge_provider_api::ItemCompletion::ToolCall(_)
                        )
                );
                // A finished turn ignores late messages and tools it never admitted. Late updates to
                // an admitted tool continue to record_tool_event, which accepts certified background
                // completion and rejects contradictory foreground output.
                let admitted_tool = tool
                    && self.turn[index].items().iter().any(|item| {
                        matches!(item, crate::turn::TurnItem::Tool { id } if id == item_id.as_str())
                    });
                if self.turn[index].state() != crate::turn::TurnState::Running && !admitted_tool {
                    return Ok(false);
                }
                let turn_id = self.turn[index].id().to_owned();
                let item_count = self.turn[index].items().len();
                if tool {
                    if !self.turn[index].items().iter().any(|item| matches!(item, crate::turn::TurnItem::Tool { id } if id == item_id.as_str())) {
                        anyhow::ensure!(!self.turn[index].messages().iter().any(|message| message.provider_id() == item_id), "item cannot change content family");
                        self.turn[index].admit_tool(item_id.as_str())?;
                    }
                } else {
                    self.turn[index].record_item_message(item_id, event)?;
                }
                // Harness authors task-start and continuation input, and its notice row stands in
                // for that text; the provider's echo of it must not render as a user prompt.
                let host_input = matches!(
                    self.origin,
                    super::ExchangeOrigin::TaskStarted { .. }
                        | super::ExchangeOrigin::TaskContinuation { .. }
                )
                .then(|| self.prompt.trim().to_owned())
                .filter(|prompt| !prompt.is_empty());
                let admitted_items = self.turn[index].items()[item_count..].to_vec();
                for item in admitted_items {
                    let item_id = match &item {
                        crate::turn::TurnItem::Message { id }
                        | crate::turn::TurnItem::Tool { id } => id,
                    };
                    // A native user message that echoes a Forge submission repeats the prompt the
                    // broker already rendered for that admitted action, so it stays turn evidence
                    // without a transcript node. Harness-authored child reports carry no submission, so
                    // their echo is recognized by its shared preamble instead, and task input by the
                    // exchange prompt it repeats. Other native input, such as text from another
                    // client, keeps its row.
                    let echo = match &item {
                        crate::turn::TurnItem::Message { id } => self.turn[index]
                            .messages()
                            .iter()
                            .find(|message| {
                                message.id() == id
                                    && message.kind() == crate::turn::MessageKind::User
                                    && (message.submission_id().is_some()
                                        || crate::agent::report::is_child_report_echo(
                                            message.text(),
                                        )
                                        || host_input
                                            .as_deref()
                                            .is_some_and(|prompt| message.text().contains(prompt)))
                            })
                            .map(|message| message.submission_id().cloned()),
                        crate::turn::TurnItem::Tool { .. } => None,
                    };
                    if let Some(submission) = echo {
                        // A provider may consume steering after acknowledgement, for example once
                        // its current model call ends, so the echo marks where the input applies.
                        if let Some(submission) = submission {
                            self.move_input_to_tail(&submission);
                        }
                        continue;
                    }
                    self.node_list.push(ExchangeNode::TurnContent {
                        id: format!("{turn_id}:content:{item_id}"),
                        turn_id: turn_id.clone(),
                        item,
                    });
                }
                if tool {
                    self.record_tool_event(&turn_id, &address, item_id, event, now_ms)?;
                    let tool = self
                        .tools_for_turn(&turn_id)
                        .find(|tool| tool.id == item_id.as_str())
                        .expect("recorded tool");
                    let running = tool.state() == crate::turn::ToolState::Running
                        && tool.started_at_ms.is_some();
                    if tool.state() != crate::turn::ToolState::Running
                        && tool.started_at_ms.is_none()
                    {
                        self.metrics.timing_complete = false;
                    }
                    let elapsed = self.elapsed(now_ms);
                    self.metrics
                        .block(format!("tool:{turn_id}:{item_id}"), running, elapsed);
                }
            }
            _ => return Ok(false),
        }
        for turn in &mut self.turn {
            turn.set_usage(self.metrics.turn_usage(turn.provider()));
        }
        self.reconcile_entries()?;
        self.metrics.observe_elapsed(self.elapsed(now_ms));
        Ok(true)
    }

    /// Keep host policy transitions separate from native turn lifecycle evidence.
    pub(crate) fn observe_host_event(
        &mut self,
        scope: &crate::provider::EventScope,
        event: &crate::provider::HostEvent,
        now_ms: i64,
    ) -> anyhow::Result<bool> {
        use crate::provider::{HostEvent, PermissionApprovalEvent, RetryEvent};
        if !self.owns_scope(scope) {
            return Ok(false);
        }
        match event {
            HostEvent::RetryUpdated {
                thread_id,
                turn_id,
                event,
            } => {
                if self.state() != ExchangeState::Running
                    || self.is_finished()
                    || !self.turn.iter().any(|turn| {
                        turn.provider().thread_id == *thread_id
                            && turn.provider().turn_id == *turn_id
                            && &turn.provider().scope
                                == scope.invocation().expect("validated scope")
                    })
                {
                    return Ok(false);
                }
                self.provider_retry = match event {
                    RetryEvent::Scheduled(retry) => Some(retry.clone()),
                    RetryEvent::Cleared => None,
                };
            }
            HostEvent::PermissionApproval {
                event:
                    event @ (PermissionApprovalEvent::Resolved(_)
                    | PermissionApprovalEvent::Cancelled(_)),
                ..
            } => {
                let request_id = match event {
                    PermissionApprovalEvent::Resolved(value) => &value.request_id,
                    PermissionApprovalEvent::Cancelled(value) => &value.request_id,
                    PermissionApprovalEvent::Requested(_) => {
                        unreachable!("terminal transition matched")
                    }
                };
                if let Some(previous) =
                    self.approval_resolution
                        .iter()
                        .find(|previous| match previous {
                            PermissionApprovalEvent::Resolved(value) => {
                                &value.request_id == request_id
                            }
                            PermissionApprovalEvent::Cancelled(value) => {
                                &value.request_id == request_id
                            }
                            PermissionApprovalEvent::Requested(_) => false,
                        })
                {
                    anyhow::ensure!(
                        serde_json::to_value(previous)? == serde_json::to_value(event)?,
                        "permission terminal resolution cannot change"
                    );
                    return Ok(false);
                }
                self.approval_resolution.push(event.clone());
            }
            HostEvent::SkillRead {
                thread_id,
                turn_id,
                read,
                ..
            } => {
                let Some(turn) = self.turn.iter().find(|turn| {
                    turn.provider().thread_id == *thread_id
                        && turn.provider().turn_id == *turn_id
                        && &turn.provider().scope == scope.invocation().expect("validated scope")
                }) else {
                    return Ok(false);
                };
                let turn_id = turn.id().to_owned();
                for read in read {
                    let id = format!("{turn_id}:skill:{}", read.resource_id);
                    if !self.node_list.iter().any(|node| node.id() == id) {
                        self.node_list.push(ExchangeNode::SkillRead {
                            id,
                            turn_id: turn_id.clone(),
                            read: read.clone(),
                        });
                    }
                }
            }
            // The broker owns pending approvals, grants, child waits, capability adoption, and
            // host failure presentation; none of them changes persisted exchange content.
            HostEvent::PermissionApproval {
                event: PermissionApprovalEvent::Requested(_),
                ..
            }
            | HostEvent::DirectoryAccessGranted(_)
            | HostEvent::ChildWait { .. }
            | HostEvent::CapabilitiesNegotiated { .. }
            | HostEvent::Failure { .. } => return Ok(false),
        }
        self.metrics.observe_elapsed(self.elapsed(now_ms));
        Ok(true)
    }
    /// Record provider execution, including late start evidence received during cleanup.
    pub fn start_turn(
        &mut self,
        address: crate::provider::TurnAddress,
        now_ms: i64,
    ) -> anyhow::Result<()> {
        anyhow::ensure!(
            matches!(
                self.state(),
                ExchangeState::Running | ExchangeState::Finalizing
            ),
            "completed exchange rejects new turns"
        );
        anyhow::ensure!(
            !address.thread_id.as_str().is_empty() && !address.turn_id.as_str().is_empty(),
            "provider turn address is incomplete"
        );
        anyhow::ensure!(
            (address.scope.exchange_id == forge_provider_api::ExchangeId::from(self.id.as_str())
                || self
                    .turn
                    .iter()
                    .any(|turn| turn.provider().scope == address.scope))
                && *address.scope.owner().session_id()
                    == crate::provider::SessionId::from(self.session_id.as_str())
                && address.scope.owner().agent_id().as_str() == self.agent_id
                && !address.scope.invocation_id.as_str().is_empty(),
            "provider invocation does not belong to this exchange"
        );
        anyhow::ensure!(
            address.thread_id == address.scope.owner().thread_id()
                && address.turn_id == address.scope.turn_id()?,
            "native turn identity disagrees with original owner"
        );
        if let Some(index) = self
            .turn
            .iter()
            .position(|turn| turn.provider() == &address)
        {
            anyhow::ensure!(
                self.turn[index].started_at_ms() == now_ms,
                "native start timestamp cannot change"
            );
            return Ok(());
        }
        anyhow::ensure!(
            self.turn
                .iter()
                .all(|turn| turn.state() != crate::turn::TurnState::Running),
            "exchange already has a running provider turn"
        );
        let id = format!("{}:turn:{}", self.id, self.turn.len() + 1);
        self.turn.push(crate::turn::Turn::new(id, address, now_ms));
        Ok(())
    }

    /// Resolve an execution address without creating work from a late content event.
    pub fn turn_mut(
        &mut self,
        address: &crate::provider::TurnAddress,
    ) -> anyhow::Result<&mut crate::turn::Turn> {
        self.turn
            .iter_mut()
            .find(|turn| turn.provider() == address)
            .ok_or_else(|| anyhow::anyhow!("provider event has no admitted turn"))
    }

    /// Revalidate host prerequisites for the same retained invocation after a stale ticket.
    pub(crate) fn revalidate_host_settlement(
        &mut self,
        scope: &crate::provider::InvocationScope,
    ) -> anyhow::Result<()> {
        anyhow::ensure!(
            self.state() == ExchangeState::Complete
                && self.finalization_outcome() == Some(ExchangeOutcome::Complete)
                && self.disposition() == HistoryDisposition::Current,
            "only a current completed exchange can revalidate host settlement"
        );
        anyhow::ensure!(
            scope.exchange_id == forge_provider_api::ExchangeId::from(self.id.as_str())
                && *scope.owner().session_id()
                    == crate::provider::SessionId::from(self.session_id.as_str())
                && scope.owner().agent_id().as_str() == self.agent_id
                && self
                    .turn
                    .last()
                    .is_none_or(|turn| &turn.provider().scope == scope),
            "host settlement revalidation crosses invocation ownership"
        );
        self.phase = super::ExchangePhase::Finalizing(ExchangeOutcome::Complete);
        self.finalization_error = None;
        self.checkpoint_after = None;
        self.checkpoint_diff_text = None;
        self.attributed_matches_checkpoint = false;
        Ok(())
    }

    /// Reject continuation after the request has reached a terminal outcome.
    pub fn resume(&mut self, now_ms: i64) -> anyhow::Result<()> {
        anyhow::ensure!(
            self.state() == ExchangeState::Running,
            "completed exchange cannot resume"
        );
        self.set_awaiting_input(self.elicitation().is_some());
        if self.metrics.awaiting_approval() {
            self.metrics.approval_resume = true;
        } else {
            self.clock.resume(now_ms);
        }
        Ok(())
    }

    /// Resume a recorded required tool answer, rejecting foreign, unresolved, or interrupted work.
    pub(crate) fn continue_required_question(
        &mut self,
        question: &crate::question::QuestionRecord,
        now_ms: i64,
    ) -> anyhow::Result<()> {
        anyhow::ensure!(
            question.exchange_id == self.id
                && question.requirement == crate::question::QuestionRequirement::Required
                && question.delivery == crate::question::QuestionDelivery::ToolResult
                && matches!(
                    question.resolution,
                    crate::question::QuestionResolution::Answered { .. }
                ),
            "required answer does not authorize this exchange"
        );
        anyhow::ensure!(
            matches!(
                self.state(),
                ExchangeState::Running | ExchangeState::Complete
            ) && self
                .turn
                .iter()
                .all(|turn| turn.state() != crate::turn::TurnState::Running),
            "required answer cannot resume interrupted or active native work"
        );
        self.phase = super::ExchangePhase::Running;
        self.finalization_error = None;
        self.resume(now_ms)
    }

    /// Continue explicit review feedback under the original exchange and checkpoint.
    pub(crate) fn revise_plan(&mut self, now_ms: i64) -> anyhow::Result<()> {
        anyhow::ensure!(
            matches!(
                self.state(),
                ExchangeState::Complete | ExchangeState::Running
            ) && !self.awaiting_input()
                && self.elicitation().is_none()
                && self
                    .turn
                    .iter()
                    .all(|turn| turn.state() != crate::turn::TurnState::Running),
            "plan revision requires a settled review exchange"
        );
        self.phase = super::ExchangePhase::Running;
        self.finalization_error = None;
        self.resume(now_ms)
    }

    /// Reopen completed source work for its automatic check repair without replacing its identity.
    pub(crate) fn repair_checks(&mut self, now_ms: i64) -> anyhow::Result<()> {
        anyhow::ensure!(
            self.state() == ExchangeState::Complete
                && !self.awaiting_input()
                && self.elicitation().is_none()
                && self.check_owner.is_some(),
            "automatic repair requires a completed exchange with check ownership"
        );
        self.phase = super::ExchangePhase::Running;
        self.finalization_error = None;
        self.resume(now_ms)
    }

    /// Preserve the exchange and settle interrupted turns before a policy restart.
    pub(crate) fn pause_for_restart(&mut self, now_ms: i64) -> anyhow::Result<()> {
        anyhow::ensure!(
            self.state() == ExchangeState::Running,
            "only an open exchange can pause for restart"
        );
        for turn in &mut self.turn {
            if turn.state() == crate::turn::TurnState::Running {
                turn.finish(crate::turn::TurnOutcome::Interrupted, now_ms)?;
            }
        }
        self.settle_tools(now_ms);
        self.pause(now_ms);
        self.metrics.settle(self.elapsed(now_ms));
        Ok(())
    }

    /// Report an open exchange whose execution is paused with no provider turn still running.
    ///
    /// Collection leaves this state after [`Self::pause_for_restart`]. Only a continuation
    /// selected for that exact exchange or a later admission closes it.
    pub(crate) fn is_parked(&self) -> bool {
        self.state() == ExchangeState::Running
            && self.execution_started_at_ms().is_none()
            && self
                .turn
                .iter()
                .all(|turn| turn.state() != crate::turn::TurnState::Running)
    }

    /// Return accumulated execution time without allowing terminal history to tick.
    pub fn elapsed(&self, now_ms: i64) -> u64 {
        if self.state() == ExchangeState::Running {
            self.clock.elapsed(now_ms)
        } else {
            self.clock.duration_ms()
        }
    }

    /// Read the clock that tool timers share with the Working timer.
    ///
    /// While the exchange runs this is its active execution clock, so approval and input
    /// waits pause tool time exactly as they pause Working. After settlement it continues
    /// in wall time from the frozen duration, so background commands keep counting.
    pub(crate) fn tool_instant(&self, now_ms: i64) -> crate::turn::ToolInstant {
        let active_ms = if self.state() == ExchangeState::Running {
            self.clock.elapsed(now_ms)
        } else {
            let since_settled = self.completed_at_ms().map_or(0, |settled_at_ms| {
                u64::try_from(now_ms.saturating_sub(settled_at_ms)).unwrap_or(0)
            });
            self.clock.duration_ms().saturating_add(since_settled)
        };
        crate::turn::ToolInstant {
            wall_ms: now_ms,
            active_ms: Some(active_ms),
        }
    }

    /// Resolves lifecycle ownership without treating a stopped clock as a task pause.
    pub(crate) fn activity(&self) -> ExchangeActivity {
        if self.state() != ExchangeState::Running {
            return ExchangeActivity::Settled;
        }
        if self.execution_started_at_ms().is_none()
            && !self.metrics.approval_resume
            && self.turn.last().is_some_and(|turn| {
                matches!(
                    turn.state(),
                    crate::turn::TurnState::Finished {
                        outcome: crate::turn::TurnOutcome::Interrupted
                    }
                )
            })
        {
            return ExchangeActivity::Paused;
        }
        if self.metrics.awaiting_approval() {
            return ExchangeActivity::AwaitingApproval;
        }
        if self.execution_started_at_ms().is_none()
            && (self.awaiting_input() || self.elicitation().is_some())
        {
            return ExchangeActivity::AwaitingInput;
        }
        if let Some(started_at_ms) = self.compaction_started_at_ms() {
            return ExchangeActivity::Compacting { started_at_ms };
        }
        if self.metrics.waiting_for_agent() {
            return ExchangeActivity::WaitingForAgent;
        }
        ExchangeActivity::Active
    }

    /// Returns the independently active provider compaction, including after exchange completion.
    pub(crate) fn compaction_started_at_ms(&self) -> Option<i64> {
        self.node_list.iter().rev().find_map(|node| match node {
            ExchangeNode::Compaction { compaction, .. }
                if compaction.state == crate::provider::CompactionState::Running =>
            {
                Some(compaction.started_at_ms.unwrap_or(self.created_at_ms))
            }
            _ => None,
        })
    }

    /// Suspends execution for permissions and accounts for other non-model activity.
    pub(crate) fn observe_blocker(&mut self, id: String, running: bool, now_ms: i64) {
        let approval = id.starts_with("approval:");
        if approval && running && !self.metrics.awaiting_approval() {
            let resume = self.execution_started_at_ms().is_some();
            self.pause(now_ms);
            self.metrics.approval_resume = resume;
        }
        self.metrics.block(id, running, self.elapsed(now_ms));
        if approval && !running && !self.metrics.awaiting_approval() {
            let resume = std::mem::take(&mut self.metrics.approval_resume);
            if resume
                && self.state() == ExchangeState::Running
                && !self.awaiting_input()
                && self.elicitation().is_none()
            {
                self.clock = crate::exchange::ExchangeClock::Running {
                    elapsed_ms: self.duration_ms(),
                    started_at_ms: now_ms,
                };
            }
        }
    }

    /// Freeze the active interval before waiting for user input or finalization.
    pub fn pause(&mut self, now_ms: i64) {
        self.metrics.approval_resume = false;
        self.clock.pause(now_ms);
    }

    /// Mark restored history without erasing the original execution outcome.
    pub fn set_disposition(&mut self, disposition: HistoryDisposition) -> anyhow::Result<()> {
        self.phase.set_disposition(disposition)
    }

    /// Settle provider turns whose native terminal will not arrive with the exchange outcome.
    ///
    /// Collection that closes the observation stream can drop a turn's terminal event. Failure
    /// finalization settles those turns before its checkpoint, which requires terminal native work.
    pub(crate) fn settle_running_turns(
        &mut self,
        outcome: ExchangeOutcome,
        now_ms: i64,
    ) -> anyhow::Result<()> {
        for turn in &mut self.turn {
            if turn.state() == crate::turn::TurnState::Running {
                anyhow::ensure!(
                    outcome != ExchangeOutcome::Complete,
                    "exchange completion requires settled provider turns"
                );
                turn.finish(
                    match outcome {
                        ExchangeOutcome::Cancelled => crate::turn::TurnOutcome::Interrupted,
                        ExchangeOutcome::Interrupted => crate::turn::TurnOutcome::Interrupted,
                        ExchangeOutcome::Failed => crate::turn::TurnOutcome::Failed {
                            error: forge_provider_api::ProviderFailure::new(
                                forge_provider_api::ProviderOperation::TurnStart,
                                forge_provider_api::ProviderFailureKind::Unknown,
                            )
                            .at_phase(
                                forge_provider_api::ProviderPhase::Observation,
                                forge_provider_api::FailureDisposition::Uncertain,
                            ),
                            retry: forge_provider_api::turn::RetryDecision::NotAllowed,
                        },
                        ExchangeOutcome::Complete => crate::turn::TurnOutcome::Completed {
                            stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
                        },
                    },
                    now_ms,
                )?;
            }
        }
        Ok(())
    }

    /// Settle every execution segment before publishing the terminal outcome.
    pub fn finish(&mut self, outcome: ExchangeOutcome, now_ms: i64) -> anyhow::Result<()> {
        self.provider_retry = None;
        if matches!(self.phase, super::ExchangePhase::Finished(_)) {
            anyhow::ensure!(
                self.state() == outcome.state(),
                "completed exchange outcome cannot change"
            );
            return Ok(());
        }
        self.settle_running_turns(outcome, now_ms)?;
        self.settle_tools(now_ms);
        self.pause(now_ms);
        self.metrics.settle(self.elapsed(now_ms));
        self.phase.complete(outcome, now_ms);
        self.set_awaiting_input(false);
        Ok(())
    }

    /// Preserve provider delegations, including in-flight work observed during cleanup.
    pub fn append_delegation(
        &mut self,
        run_id: &str,
        invocation_id: &str,
        task: &str,
        now_ms: i64,
    ) -> anyhow::Result<bool> {
        anyhow::ensure!(
            matches!(
                self.state(),
                ExchangeState::Running | ExchangeState::Finalizing
            ),
            "delegation requires a current running exchange"
        );
        let delegation_id = format!("{}:agent:{run_id}:{invocation_id}", self.id);
        if self.node_list.iter().any(|node| {
            matches!(
                node,
                ExchangeNode::AgentReference { agent }
                    if agent.id == delegation_id
            )
        }) {
            return Ok(false);
        }
        for turn in self
            .turn
            .iter_mut()
            .filter(|turn| turn.state() == crate::turn::TurnState::Running)
        {
            turn.close_message();
        }
        self.node_list.push(ExchangeNode::AgentReference {
            agent: Delegation {
                id: delegation_id,
                child_agent_id: run_id.to_owned(),
                child_exchange_id: uuid::Uuid::new_v4().to_string(),
                parent_exchange_id: self.id.clone(),
                parent_turn_id: self
                    .turn
                    .iter()
                    .rev()
                    .find(|turn| turn.state() == crate::turn::TurnState::Running)
                    .map(|turn| turn.id().to_owned()),
                task: task.to_owned(),
                created_at_ms: now_ms,
            },
        });
        Ok(true)
    }

    /// Append one acknowledged steering prompt at the current timeline tail.
    pub fn append_input(
        &mut self,
        intent: InputIntent,
        text: String,
        now_ms: i64,
    ) -> anyhow::Result<ExchangeInput> {
        self.record_input(InputAppend::User, None, None, intent, text, None, now_ms)
    }

    /// Retain accepted native input while its original exchange is still settling.
    ///
    /// The submission identifies the input's native echo, which moves the row to the point where
    /// the provider consumed it when that differs from acknowledgement.
    pub(crate) fn append_acknowledged_input(
        &mut self,
        id: Option<String>,
        submission_id: Option<forge_provider_api::SubmissionId>,
        intent: InputIntent,
        text: String,
        now_ms: i64,
    ) -> anyhow::Result<ExchangeInput> {
        self.record_input(
            InputAppend::Acknowledged,
            id,
            submission_id,
            intent,
            text,
            None,
            now_ms,
        )
    }

    #[allow(clippy::too_many_arguments)]
    fn record_input(
        &mut self,
        admission: InputAppend,
        id: Option<String>,
        submission_id: Option<forge_provider_api::SubmissionId>,
        intent: InputIntent,
        text: String,
        question: Option<QuestionInput>,
        now_ms: i64,
    ) -> anyhow::Result<ExchangeInput> {
        let accepts_input = match admission {
            InputAppend::User => self.state() == ExchangeState::Running,
            InputAppend::Acknowledged => matches!(
                self.state(),
                ExchangeState::Running | ExchangeState::Finalizing
            ),
        };
        if let Some(id) = &id
            && let Some(prompt) = self.node_list.iter().find_map(|node| match node {
                ExchangeNode::ExchangeInput { prompt } if &prompt.id == id => Some(prompt),
                _ => None,
            })
        {
            return Ok(prompt.clone());
        }
        anyhow::ensure!(
            accepts_input,
            "input append rejected: exchange={}, state={:?}, admission={admission:?}, input_id={id:?}, question={}",
            self.id,
            self.state(),
            question.is_some()
        );
        for turn in self
            .turn
            .iter_mut()
            .filter(|turn| turn.state() == crate::turn::TurnState::Running)
        {
            turn.close_message();
        }

        let prompt_ordinal = self
            .node_list
            .iter()
            .filter(|node| matches!(node, ExchangeNode::ExchangeInput { .. }))
            .count()
            + 1;
        let prompt = ExchangeInput {
            question,
            intent,
            id: id.unwrap_or_else(|| format!("{}:input:{prompt_ordinal}", self.id)),
            text,
            created_at_ms: now_ms,
            submission_id,
        };
        self.node_list.push(ExchangeNode::ExchangeInput {
            prompt: prompt.clone(),
        });
        self.reconcile_entries()?;
        Ok(prompt)
    }

    /// Move the acknowledged input a native echo identifies to the current timeline tail.
    fn move_input_to_tail(&mut self, submission: &forge_provider_api::SubmissionId) {
        let Some(position) = self.node_list.iter().position(|node| {
            matches!(node, ExchangeNode::ExchangeInput { prompt }
                if prompt.submission_id.as_ref() == Some(submission))
        }) else {
            return;
        };
        let node = self.node_list.remove(position);
        self.node_list.push(node);
    }

    /// Admit question feedback with a durable target before the provider can produce a reply.
    pub(crate) fn append_question_input(
        &mut self,
        intent: InputIntent,
        text: String,
        question: QuestionInput,
        now_ms: i64,
    ) -> anyhow::Result<()> {
        anyhow::ensure!(
            matches!(intent, InputIntent::Clarification | InputIntent::Answer),
            "question feedback requires clarification or answer intent"
        );
        anyhow::ensure!(
            !question.set_id.is_empty()
                && (intent != InputIntent::Clarification || question.question_id.is_some()),
            "question clarification requires a durable question target"
        );
        self.record_input(
            InputAppend::User,
            None,
            None,
            intent,
            text,
            Some(question),
            now_ms,
        )?;
        Ok(())
    }

    /// Sum usage across owned turns, retaining unavailable categories.
    pub fn usage(&self) -> crate::provider::usage::TokenUsage {
        self.metrics.usage()
    }

    /// End the current streamed message at a provider wait or input boundary.
    pub fn close_running_messages(&mut self) {
        for turn in self
            .turn
            .iter_mut()
            .filter(|turn| turn.state() == crate::turn::TurnState::Running)
        {
            turn.close_message();
        }
    }
}

/// Represents whether an interaction can still receive provider turns.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum ExchangeState {
    Queued,
    Running,
    Finalizing,
    Complete,
    Failed,
    Cancelled,
    Interrupted,
}

#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
/// Records rollback disposition independently from execution completion.
pub enum HistoryDisposition {
    #[default]
    /// The exchange still belongs to the current workspace history.
    Current,
    /// The workspace was restored to the beginning of this exchange.
    RolledBack,
    /// A preceding rollback replaced this exchange's workspace history.
    Superseded,
}

/// Represents one review comment anchored to an interaction diff.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ExchangeComment {
    pub id: String,
    pub exchange_id: String,
    pub file_path: String,
    pub old_line: Option<u32>,
    pub new_line: Option<u32>,
    pub body: String,
    pub created_at_ms: i64,
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::provider::HarnessEvent;
    use forge_provider_api::{
        AgentMessageRole, InspectableArguments, ItemCompletion, ItemEvent, ItemStart,
        ToolCompletion, ToolDescriptor, ToolItemEvent,
    };

    /// Preserve the original admitted owner for native turn fixtures.
    fn turn_event(
        address: &crate::provider::TurnAddress,
        event: forge_provider_api::TurnEvent,
    ) -> HarnessEvent {
        crate::provider::test_support::native_event(
            address.scope.clone(),
            forge_provider_api::ProviderEvent::Turn {
                thread_id: address.thread_id.clone(),
                turn_id: address.turn_id.clone(),
                event,
            },
        )
    }

    /// Build one inspectable native tool descriptor for lifecycle fixtures.
    fn descriptor() -> ToolDescriptor {
        ToolDescriptor {
            semantics: Default::default(),
            name: "probe".into(),
            title: "Probe".into(),
            arguments: InspectableArguments::new(serde_json::json!({})),
        }
    }

    /// Preserve immutable item ownership for native activity fixtures.
    fn item_event(
        address: &crate::provider::TurnAddress,
        id: &str,
        event: ItemEvent,
    ) -> HarnessEvent {
        crate::turn::test_support::item_event(address, id, event)
    }

    fn interaction() -> Exchange {
        Exchange {
            finalization_error: None,
            check_owner: None,
            output_store: None,
            archived_output: Default::default(),
            input_message_id: None,
            provider_retry: None,
            agent_id: "primary".into(),
            id: "interaction-one".into(),
            session_id: "session-one".into(),
            ordinal: 1,
            prompt: "inspect the repository".into(),
            origin: crate::exchange::ExchangeOrigin::User,
            approval_mode: None,
            phase: crate::exchange::ExchangePhase::Running,
            checkpoint_before: None,
            checkpoint_after: None,
            attributed_diff_text: None,
            checkpoint_diff_text: None,
            attributed_matches_checkpoint: false,
            approval_resolution: Vec::new(),
            host_file_changes: Vec::new(),
            check_setup: None,
            turn: Vec::new(),
            created_at_ms: 1,
            tool_counter: Default::default(),
            entry: Vec::new(),
            node_list: Vec::new(),
            metrics: ExchangeMetrics::default(),
            comment: Vec::new(),
            checklist: None,

            question: crate::exchange::ExchangeQuestion::None,
            clock: crate::exchange::ExchangeClock::Paused { elapsed_ms: 0 },
        }
    }

    #[test]
    fn provider_turn_identity_survives_steering_and_rejects_late_tools() {
        use crate::provider::TurnEvent;
        use crate::turn::{TurnOutcome, TurnState};
        let mut exchange = interaction();
        let address = crate::turn::test_support::address(&exchange, "first");
        let mut event = turn_event(
            &address.clone(),
            TurnEvent::Started(forge_provider_api::turn::TurnStarted { started_at_ms: 10 }),
        );
        assert!(exchange.observe_event(&event, 10).unwrap());
        event = crate::turn::test_support::message(
            &address,
            "message-one",
            Some(AgentMessageRole::Commentary),
            "Before steering",
        );
        assert!(exchange.observe_event(&event, 10).unwrap());
        exchange
            .append_input(
                crate::exchange::InputIntent::Steering,
                "include enterprise URLs".into(),
                11,
            )
            .unwrap();
        event = crate::turn::test_support::message(
            &address,
            "message-two",
            Some(AgentMessageRole::Response),
            "After steering",
        );
        assert!(exchange.observe_event(&event, 11).unwrap());
        assert_eq!(exchange.turn[0].messages().len(), 2);
        assert_eq!(
            exchange.turn[0].messages()[0].provider_id(),
            &forge_provider_api::ItemId::new("message-one").unwrap()
        );
        assert_eq!(
            exchange.turn[0].messages()[0].delivery(),
            crate::turn::MessageDelivery::Commentary
        );
        assert_eq!(exchange.turn[0].messages()[1].text(), "After steering");
        assert_eq!(
            exchange.turn[0].messages()[1].provider_id(),
            &forge_provider_api::ItemId::new("message-two").unwrap()
        );
        assert_eq!(
            exchange.turn[0].messages()[1].delivery(),
            crate::turn::MessageDelivery::Final
        );
        assert_eq!(exchange.turn.len(), 1);
        let first_id = exchange.turn[0].id().to_owned();
        crate::turn::test_support::tool(
            &mut exchange,
            &address,
            "change",
            ItemStart::FileChange(ToolDescriptor {
                name: "edit".into(),
                title: "edit".into(),
                ..descriptor()
            }),
            [
                ItemEvent::Tool {
                    event: ToolItemEvent::ExecutionStarted,
                },
                ItemEvent::Tool {
                    event: ToolItemEvent::Changes(crate::provider::ProviderChangeSet {
                        file: vec![crate::provider::ProviderFileChange {
                            path: "owned.rs".into(),
                            move_path: None,
                            kind: crate::provider::ProviderChangeKind::Add,
                            diff: "+owned".into(),
                        }],
                    }),
                },
                ItemEvent::Completed(ItemCompletion::FileChange(ToolCompletion {
                    outcome: forge_provider_api::ToolOutcome::Succeeded,
                    descriptor: None,
                    output: None,
                    changes: Default::default(),
                })),
            ],
            12,
        )
        .unwrap();
        let mut changes = ProviderChangeIndex::default();
        changes.record(&exchange);
        assert!(
            changes.paths().contains("owned.rs"),
            "attribution depended on presentation segments"
        );
        assert_eq!(exchange.tools_for_turn(exchange.turn[0].id()).count(), 1);
        event = crate::turn::test_support::completed_event(
            &address.clone(),
            TurnOutcome::Completed {
                stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
            },
            20,
        );
        assert!(exchange.observe_event(&event, 20).unwrap());
        event = item_event(
            &address,
            "late",
            ItemEvent::Tool {
                event: ToolItemEvent::ExecutionStarted,
            },
        );
        assert!(!exchange.observe_event(&event, 30).unwrap());
        assert_eq!(
            exchange.turn[0].state(),
            TurnState::Finished {
                outcome: TurnOutcome::Completed {
                    stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal
                }
            }
        );
        assert!(exchange.start_turn(address, 40).is_err());
        exchange
            .start_turn(crate::turn::test_support::address(&exchange, "second"), 50)
            .unwrap();
        assert_eq!(exchange.turn.len(), 2);
        assert_ne!(exchange.turn[1].id(), first_id);
        assert!(
            exchange
                .finish(crate::exchange::ExchangeOutcome::Complete, 60)
                .is_err()
        );
        exchange
            .finish(crate::exchange::ExchangeOutcome::Cancelled, 60)
            .unwrap();
        assert_eq!(
            exchange.turn[1].state(),
            TurnState::Finished {
                outcome: TurnOutcome::Interrupted
            }
        );
    }

    /// Preserve native steering evidence arriving after completion without reopening input admission.
    #[test]
    fn finalizing_exchange_retains_acknowledged_input_without_reopening() {
        let mut exchange = interaction();
        exchange.resume(10).unwrap();
        exchange
            .begin_finalization(ExchangeOutcome::Complete, 30)
            .unwrap();
        let prompt = exchange
            .append_acknowledged_input(
                Some("late-steering".into()),
                None,
                InputIntent::Steering,
                "accepted".into(),
                40,
            )
            .unwrap();
        assert_eq!(prompt.id, "late-steering");
        assert_eq!(exchange.state(), ExchangeState::Finalizing);
        assert_eq!(exchange.elapsed(100), 20);
        assert!(
            exchange
                .append_input(InputIntent::Steering, "new".into(), 50)
                .is_err()
        );
        exchange.finish(ExchangeOutcome::Complete, 60).unwrap();
        let node_count = exchange.node_list.len();
        assert!(
            exchange
                .append_acknowledged_input(
                    Some("late-steering".into()),
                    None,
                    InputIntent::Steering,
                    "accepted".into(),
                    70,
                )
                .is_ok()
        );
        assert_eq!(exchange.node_list.len(), node_count);
        assert!(
            exchange
                .append_acknowledged_input(
                    Some("after-terminal".into()),
                    None,
                    InputIntent::Steering,
                    "late".into(),
                    70,
                )
                .is_err()
        );
        assert_eq!(exchange.state(), ExchangeState::Complete);
    }
}
