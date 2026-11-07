use super::*;
use crate::agent::Delegation;
use crate::plan::PlanElicitation;
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
            address.thread_id == address.scope.owner.thread_id()
                && address.turn_id == address.scope.turn_id()?,
            "observed turn identity disagrees with original owner"
        );
        let mut exchange = Self {
            id: id.clone(),
            session_id: address.scope.owner.session_id().as_str().to_owned(),
            agent_id: address.scope.owner.agent_id().as_str().to_owned(),
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
                && scope.owner.session_id().as_str() == self.session_id
                && scope.owner.agent_id().as_str() == self.agent_id
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
                for item in self.turn[index].items()[item_count..].iter().cloned() {
                    let item_id = match &item {
                        crate::turn::TurnItem::Message { id }
                        | crate::turn::TurnItem::Tool { id } => id,
                    };
                    // A native user message that echoes a Forge submission repeats the prompt the
                    // broker already rendered for that admitted action, so it stays turn evidence
                    // without a transcript node. Native input from another client keeps its row.
                    if let crate::turn::TurnItem::Message { id } = &item
                        && self.turn[index]
                            .messages()
                            .iter()
                            .any(|message| message.id() == id && message.submission_id().is_some())
                    {
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
                && *address.scope.owner.session_id()
                    == forge_provider_api::SessionId::from(self.session_id.as_str())
                && address.scope.owner.agent_id().as_str() == self.agent_id
                && !address.scope.invocation_id.as_str().is_empty(),
            "provider invocation does not belong to this exchange"
        );
        anyhow::ensure!(
            address.thread_id == address.scope.owner.thread_id()
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
                && *scope.owner.session_id()
                    == forge_provider_api::SessionId::from(self.session_id.as_str())
                && scope.owner.agent_id().as_str() == self.agent_id
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
        self.record_input(InputAppend::User, None, intent, text, None, now_ms)
    }

    /// Retain accepted native input while its original exchange is still settling.
    pub(crate) fn append_acknowledged_input(
        &mut self,
        id: Option<String>,
        intent: InputIntent,
        text: String,
        now_ms: i64,
    ) -> anyhow::Result<ExchangeInput> {
        self.record_input(InputAppend::Acknowledged, id, intent, text, None, now_ms)
    }

    fn record_input(
        &mut self,
        admission: InputAppend,
        id: Option<String>,
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
        };
        self.node_list.push(ExchangeNode::ExchangeInput {
            prompt: prompt.clone(),
        });
        self.reconcile_entries()?;
        Ok(prompt)
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
    use crate::provider::{HarnessEvent, HostEvent, RetryEvent};
    use forge_provider_api::{
        AgentMessageRole, InspectableArguments, ItemCompletion, ItemEvent, ItemStart,
        MessageCompletion, MessageOutcome, ToolCompletion, ToolDescriptor, ToolItemEvent,
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

    #[test]
    fn input_timeout_approval_provenance_survives_exchange_serialization() {
        use crate::provider::{
            ApprovalDecisionSource, ApprovalRequestId, ApprovalScope, ApprovalTargetId,
            PermissionApprovalEvent, PermissionApprovalResolution, PermissionDecision,
            ResolvedPermissionTarget,
        };
        let mut exchange = interaction();
        let address = crate::turn::test_support::address(&exchange, "approval");
        let resolution = PermissionApprovalEvent::Resolved(PermissionApprovalResolution {
            request_id: ApprovalRequestId::new("timed-out").unwrap(),
            scope: ApprovalScope::Turn {
                thread_id: address.thread_id.clone(),
                turn_id: address.turn_id.clone(),
            },
            targets: vec![ResolvedPermissionTarget {
                target_id: ApprovalTargetId::new("source").unwrap(),
                decision: PermissionDecision::TimedOut,
                source: ApprovalDecisionSource::Timeout,
                rules: vec![],
                directory_grants: vec![],
            }],
            resolved_at_ms: 1000,
        });
        let event = HarnessEvent::Host {
            scope: address.scope.into(),
            event: HostEvent::PermissionApproval {
                execution_kind: crate::provider::ApprovalExecutionKind::Managed,
                event: resolution,
            },
        };
        assert!(exchange.observe_event(&event, 1000).unwrap());
        let mut restored: Exchange =
            serde_json::from_value(serde_json::to_value(&exchange).unwrap()).unwrap();
        assert!(!restored.observe_event(&event, 1001).unwrap());
        let PermissionApprovalEvent::Resolved(resolved) = &restored.approval_resolution[0] else {
            panic!("timeout resolution must remain terminal");
        };
        assert!(matches!(
            resolved.targets[0].source,
            ApprovalDecisionSource::Timeout
        ));
        assert!(matches!(
            resolved.targets[0].decision,
            PermissionDecision::TimedOut
        ));
        assert_eq!(resolved.resolved_at_ms, 1000);
        let mut conflict = event;
        if let HarnessEvent::Host {
            event:
                HostEvent::PermissionApproval {
                    event: PermissionApprovalEvent::Resolved(resolved),
                    ..
                },
            ..
        } = &mut conflict
        {
            resolved.resolved_at_ms = 1002;
        }
        assert!(restored.observe_event(&conflict, 1002).is_err());
        assert_eq!(restored.approval_resolution.len(), 1);
    }

    #[test]
    fn legacy_completion_without_timestamp_stays_terminal_after_roundtrip() {
        let mut record = serde_json::to_value(interaction()).unwrap();
        record["state"] = "complete".into();
        record["completed_at_ms"] = serde_json::Value::Null;
        record["duration_ms"] = 25.into();
        record["execution_started_at_ms"] = 1.into();
        let mut restored: Exchange = serde_json::from_value(record).unwrap();
        assert!(restored.is_finished());
        assert_eq!(restored.elapsed(100_000), 25);
        assert!(restored.resume(100_000).is_err());
        restored
            .set_disposition(HistoryDisposition::RolledBack)
            .unwrap();
        let encoded = serde_json::to_value(&restored).unwrap();
        assert_eq!(encoded["state"], "complete");
        assert!(encoded["completed_at_ms"].is_null());
        assert_eq!(encoded["duration_ms"], 25);
        assert!(encoded.get("phase").is_none());
        assert!(encoded.get("clock").is_none());
        let reopened: Exchange = serde_json::from_value(encoded).unwrap();
        assert_eq!(reopened.disposition(), HistoryDisposition::RolledBack);
        assert_eq!(reopened.elapsed(200_000), 25);
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
    fn retry_state_requires_known_turn_and_clears_after_recovery() -> anyhow::Result<()> {
        use crate::session::state_machine::SessionPhase;
        let mut exchange = interaction();
        exchange.resume(1000)?;
        let address = crate::turn::test_support::address(&exchange, "failed-turn");
        let retry = crate::provider::retry::ProviderRetry {
            attempt: 1,
            max_attempts: 5,
            retry_at_ms: Some(2000),
        };
        let retry_event = |event| HarnessEvent::Host {
            scope: address.scope.clone().into(),
            event: HostEvent::RetryUpdated {
                thread_id: address.thread_id.clone(),
                turn_id: address.turn_id.clone(),
                event,
            },
        };
        let mut event = retry_event(RetryEvent::Scheduled(retry.clone()));
        assert!(!exchange.observe_event(&event, 1000)?);
        exchange.start_turn(address.clone(), 1000)?;
        exchange.turn_mut(&address)?.finish(
            crate::turn::TurnOutcome::Failed {
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
            1001,
        )?;
        assert!(exchange.observe_event(&event, 1001)?);
        assert!(matches!(
            SessionPhase::resolve(None, None, Some(&exchange)),
            SessionPhase::RetryingProvider { .. }
        ));
        event = retry_event(RetryEvent::Cleared);
        assert!(exchange.observe_event(&event, 2000)?);
        assert!(matches!(
            SessionPhase::resolve(None, None, Some(&exchange)),
            SessionPhase::Working { .. }
        ));
        event = retry_event(RetryEvent::Scheduled(retry));
        exchange.finish(crate::exchange::ExchangeOutcome::Failed, 2001)?;
        assert!(!exchange.observe_event(&event, 2002)?);
        assert!(exchange.provider_retry.is_none());
        Ok(())
    }

    #[test]
    fn tool_timing_excludes_user_input_pauses_and_rejects_missing_start_evidence() {
        let mut exchange = interaction();
        exchange.resume(1000).unwrap();
        let address = crate::turn::test_support::address(&exchange, "one");
        exchange.start_turn(address.clone(), 1000).unwrap();
        exchange
            .observe_event(
                &item_event(
                    &address,
                    "call",
                    ItemEvent::Started(ItemStart::CommandExecution(descriptor())),
                ),
                2000,
            )
            .unwrap();
        exchange
            .observe_event(
                &item_event(
                    &address,
                    "call",
                    ItemEvent::Tool {
                        event: ToolItemEvent::ExecutionStarted,
                    },
                ),
                2000,
            )
            .unwrap();
        exchange.pause(3000);
        assert_eq!(exchange.elapsed(90000), 2000);
        assert_eq!(
            exchange.metrics.response_ms(exchange.elapsed(90000)),
            Some(1000)
        );
        exchange.resume(90000).unwrap();
        let completed = || {
            ItemEvent::Completed(ItemCompletion::CommandExecution(ToolCompletion {
                outcome: forge_provider_api::ToolOutcome::Succeeded,
                descriptor: None,
                output: None,
                changes: Default::default(),
            }))
        };
        exchange
            .observe_event(&item_event(&address, "call", completed()), 92000)
            .unwrap();
        assert_eq!(exchange.elapsed(93000), 5000);
        assert_eq!(exchange.metrics.response_ms(5000), Some(2000));
        exchange
            .observe_event(
                &item_event(
                    &address,
                    "second",
                    ItemEvent::Started(ItemStart::CommandExecution(descriptor())),
                ),
                93000,
            )
            .unwrap();
        exchange
            .observe_event(
                &item_event(
                    &address,
                    "second",
                    ItemEvent::Tool {
                        event: ToolItemEvent::ExecutionStarted,
                    },
                ),
                93000,
            )
            .unwrap();
        assert_eq!(exchange.metrics.response_ms(6000), Some(2000));
        assert!(
            exchange
                .observe_event(&item_event(&address, "missing-start", completed()), 94000)
                .is_err()
        );
        assert_eq!(exchange.metrics.response_ms(6000), Some(2000));
    }

    #[test]
    fn message_snapshots_reconcile_without_duplicating_or_reordering_content() {
        let mut exchange = interaction();
        let address = crate::turn::test_support::address(&exchange, "turn");
        exchange.start_turn(address.clone(), 10).unwrap();
        let mut event = crate::turn::test_support::message(
            &address,
            "one",
            Some(AgentMessageRole::Commentary),
            "Checking",
        );
        exchange.observe_event(&event, 11).unwrap();
        exchange
            .append_input(InputIntent::Steering, "Also tests".into(), 12)
            .unwrap();
        exchange
            .observe_event(
                &crate::turn::test_support::message(
                    &address,
                    "two",
                    Some(AgentMessageRole::Response),
                    "Done",
                ),
                13,
            )
            .unwrap();
        event = item_event(
            &address,
            "one",
            ItemEvent::Completed(ItemCompletion::AgentMessage(MessageCompletion {
                text: "Checking files.".into(),
                outcome: MessageOutcome::Completed,
            })),
        );
        exchange.observe_event(&event, 14).unwrap();
        exchange.observe_event(&event, 15).unwrap();
        assert_eq!(exchange.node_list.len(), 3);
        assert_eq!(exchange.turn[0].messages().len(), 2);
        assert_eq!(exchange.turn[0].messages()[0].text(), "Checking files.");
        assert_eq!(
            exchange.turn[0].messages()[0].delivery(),
            crate::turn::MessageDelivery::Commentary
        );
        assert_eq!(exchange.turn[0].messages()[1].text(), "Done");
        assert!(matches!(
            exchange.node_list[1],
            ExchangeNode::ExchangeInput { .. }
        ));
        exchange.turn[0]
            .finish(
                crate::turn::TurnOutcome::Completed {
                    stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
                },
                16,
            )
            .unwrap();
        event = item_event(
            &address,
            "one",
            ItemEvent::Completed(ItemCompletion::AgentMessage(MessageCompletion {
                text: "Late correction".into(),
                outcome: MessageOutcome::Completed,
            })),
        );
        assert!(!exchange.observe_event(&event, 17).unwrap());
        assert_eq!(exchange.turn[0].messages()[0].text(), "Checking files.");
    }

    #[test]
    fn active_turn_exposes_only_its_latest_readable_reasoning_summary() {
        let mut exchange = interaction();
        let address = crate::turn::test_support::address(&exchange, "turn");
        exchange.start_turn(address.clone(), 10).unwrap();
        let event = crate::turn::test_support::reasoning(
            &address,
            "reasoning:summary:0",
            forge_provider_api::ReasoningKind::Summary,
            "Inspecting the repository",
        );
        exchange.observe_event(&event, 11).unwrap();
        let event = crate::turn::test_support::reasoning(
            &address,
            "reasoning",
            forge_provider_api::ReasoningKind::Full,
            "private chain of thought",
        );
        exchange.observe_event(&event, 12).unwrap();

        assert_eq!(
            exchange.latest_reasoning_summary(),
            Some("Inspecting the repository")
        );
        exchange.turn[0]
            .finish(
                crate::turn::TurnOutcome::Completed {
                    stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
                },
                13,
            )
            .unwrap();
        assert_eq!(exchange.latest_reasoning_summary(), None);
    }

    #[test]
    fn preserves_delegation_and_steering_order_without_duplicate_agents() {
        let mut interaction = interaction();
        assert!(
            interaction
                .append_delegation("agent-one", "spawn", "original task", 21)
                .unwrap()
        );
        assert!(
            !interaction
                .append_delegation("agent-one", "spawn", "changed task", 22)
                .unwrap()
        );
        let ExchangeNode::AgentReference { agent } = interaction.node_list.last().unwrap() else {
            panic!("delegation should be retained");
        };
        assert_eq!(agent.task, "original task");
        assert_eq!(agent.parent_exchange_id, interaction.id);
        assert_eq!(agent.child_agent_id, "agent-one");
        let restored: Exchange =
            serde_json::from_str(&serde_json::to_string(&interaction).unwrap()).unwrap();
        let ExchangeNode::AgentReference { agent } = restored.node_list.last().unwrap() else {
            panic!("durable delegation should retain its task");
        };
        assert_eq!(agent.task, "original task");

        interaction
            .append_input(
                crate::exchange::InputIntent::Steering,
                "also inspect tests".into(),
                23,
            )
            .unwrap();
        let kind_list = interaction
            .node_list
            .iter()
            .map(|node| match node {
                ExchangeNode::TimelineEvent { .. } => "timeline-event",
                ExchangeNode::SkillRead { .. } => "skill-read",
                ExchangeNode::Compaction { .. } => "compaction",
                ExchangeNode::TurnContent { .. } => "content",
                ExchangeNode::AgentReference { .. } => "agent",
                ExchangeNode::ExchangeInput { .. } => "steering",
                ExchangeNode::PlanCommentResolution { .. } => "resolution",
                ExchangeNode::ArtifactChange { .. } => "artifact",
                ExchangeNode::VerificationResult { .. } => "verification",
                ExchangeNode::QuestionPresented { .. } => "question",
                ExchangeNode::QuestionWithdrawn { .. } => "question_withdrawn",
                ExchangeNode::PlanEvent { .. } => "plan",
                ExchangeNode::ImplementationReport { .. } => "implementation-report",
            })
            .collect::<Vec<_>>();
        assert_eq!(kind_list, ["agent", "steering"]);
    }

    /// Canonical completion commits final accounting once and preserves independently reported timing.
    #[test]
    fn canonical_completion_retains_exact_result_and_final_usage_after_reload() {
        use forge_provider_api::TurnEvent;
        let mut exchange = interaction();
        let address = crate::turn::test_support::address(&exchange, "canonical");
        exchange
            .observe_event(
                &turn_event(
                    &address.clone(),
                    TurnEvent::Started(forge_provider_api::turn::TurnStarted { started_at_ms: 10 }),
                ),
                10,
            )
            .unwrap();
        let report_id = forge_provider_api::identity::UsageReportId::new("report").unwrap();
        let initial = forge_provider_api::TurnUsageReport::accounting(
            report_id.clone(),
            11,
            Some(crate::provider::usage::TokenUsage {
                input: Some(2),
                output: None,
                cached_input: None,
                reasoning: None,
            }),
            None,
        )
        .unwrap();
        exchange
            .observe_event(&turn_event(&address.clone(), TurnEvent::Usage(initial)), 11)
            .unwrap();
        let final_usage = forge_provider_api::TurnUsageReport::accounting(
            report_id,
            19,
            Some(crate::provider::usage::TokenUsage {
                input: None,
                output: Some(3),
                cached_input: None,
                reasoning: None,
            }),
            Some(5),
        )
        .unwrap();
        let native = forge_provider_api::TurnResult::new(
            crate::turn::TurnOutcome::Completed {
                stop_reason: forge_provider_api::turn::TurnCompletionReason::Refusal,
            },
            20,
            Some(12),
            vec![forge_provider_api::CoveredInput::new(
                "input".try_into().unwrap(),
                forge_provider_api::DeliveryAttemptId::new("attempt").unwrap(),
            )],
            vec![final_usage],
            Some(forge_provider_api::CheckpointId::new("checkpoint").unwrap()),
        )
        .unwrap();
        let completed =
            crate::provider::test_support::completed_turn(address.scope.clone(), native).unwrap();
        let event = turn_event(&address.clone(), TurnEvent::Completed(completed.clone()));
        assert!(exchange.observe_event(&event, 30).unwrap());
        assert!(!exchange.observe_event(&event, 31).unwrap());
        let mut exchange: Exchange =
            serde_json::from_str(&serde_json::to_string(&exchange).unwrap()).unwrap();
        assert!(!exchange.observe_event(&event, 32).unwrap());
        assert_eq!(exchange.turn[0].completed(), Some(&completed));
        let timing = forge_provider_api::TurnUsageReport::timing(
            forge_provider_api::identity::UsageReportId::new("late-timing").unwrap(),
            33,
            7,
        );
        exchange
            .observe_event(&turn_event(&address, TurnEvent::Usage(timing)), 33)
            .unwrap();
        let original_start = turn_event(
            &exchange.turn[0].provider().clone(),
            forge_provider_api::TurnEvent::Started(forge_provider_api::turn::TurnStarted {
                started_at_ms: 10,
            }),
        );
        assert!(!exchange.observe_event(&original_start, 34).unwrap());
        let before = serde_json::to_value(&exchange).unwrap();
        let conflicting_start = turn_event(
            &exchange.turn[0].provider().clone(),
            forge_provider_api::TurnEvent::Started(forge_provider_api::turn::TurnStarted {
                started_at_ms: 35,
            }),
        );
        assert!(exchange.observe_event(&conflicting_start, 35).is_err());
        assert_eq!(serde_json::to_value(&exchange).unwrap(), before);
        let turn = &exchange.turn[0];
        assert_eq!(turn.duration_ms(), Some(12));
        assert_eq!(turn.usage().unwrap().input, Some(2));
        assert_eq!(turn.usage().unwrap().output, Some(3));
        let valid = serde_json::to_value(turn).unwrap();
        let _: crate::turn::Turn = serde_json::from_value(valid.clone()).unwrap();
        let mut invalid = valid.clone();
        invalid["completed"]["result"]["outcome"] = serde_json::json!("interrupted");
        assert!(serde_json::from_value::<crate::turn::Turn>(invalid).is_err());
        let mut invalid = valid.clone();
        invalid["reported_duration_ms"] = serde_json::json!(13);
        assert!(serde_json::from_value::<crate::turn::Turn>(invalid).is_err());
        let mut invalid = valid;
        invalid["usage_reports"]["report"]["usage"]["output"] = serde_json::json!(4);
        assert!(serde_json::from_value::<crate::turn::Turn>(invalid).is_err());
    }

    /// Correlated errors retain their original turn without establishing terminal or retry evidence.
    #[test]
    fn turn_error_observation_is_idempotent_and_nonterminal_after_reload() {
        use forge_provider_api::TurnEvent;
        let mut exchange = interaction();
        let address = crate::turn::test_support::address(&exchange, "original");
        exchange
            .observe_event(
                &turn_event(
                    &address.clone(),
                    TurnEvent::Started(forge_provider_api::turn::TurnStarted { started_at_ms: 10 }),
                ),
                10,
            )
            .unwrap();
        let failure = forge_provider_api::ProviderFailure::new(
            forge_provider_api::ProviderOperation::TurnStart,
            forge_provider_api::ProviderFailureKind::RateLimited,
        );
        let error = turn_event(&address.clone(), TurnEvent::Error { error: failure });
        assert!(exchange.observe_event(&error, 11).unwrap());
        let mut exchange: Exchange =
            serde_json::from_str(&serde_json::to_string(&exchange).unwrap()).unwrap();
        assert!(!exchange.observe_event(&error, 12).unwrap());
        assert_eq!(exchange.turn[0].state(), crate::turn::TurnState::Running);
        assert_eq!(exchange.turn[0].errors(), &[failure]);
        let unknown = turn_event(
            &crate::turn::test_support::address(&exchange, "unknown"),
            TurnEvent::Error { error: failure },
        );
        assert!(!exchange.observe_event(&unknown, 13).unwrap());
        assert_eq!(exchange.turn.len(), 1);
        exchange
            .observe_event(
                &crate::turn::test_support::completed_event(
                    &address,
                    crate::turn::TurnOutcome::Completed {
                        stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
                    },
                    20,
                ),
                20,
            )
            .unwrap();
        assert!(matches!(
            exchange.turn[0].state(),
            crate::turn::TurnState::Finished {
                outcome: crate::turn::TurnOutcome::Completed { .. }
            }
        ));
        let completed = exchange.turn[0].completed().cloned();
        let late_failure = forge_provider_api::ProviderFailure::new(
            forge_provider_api::ProviderOperation::TurnStart,
            forge_provider_api::ProviderFailureKind::Runtime,
        );
        let late_error = turn_event(
            &address,
            TurnEvent::Error {
                error: late_failure,
            },
        );
        assert!(exchange.observe_event(&late_error, 21).unwrap());
        assert!(!exchange.observe_event(&late_error, 22).unwrap());
        assert_eq!(exchange.turn[0].completed(), completed.as_ref());
        assert_eq!(exchange.turn[0].errors(), &[failure, late_failure]);
        let mut exchange: Exchange =
            serde_json::from_str(&serde_json::to_string(&exchange).unwrap()).unwrap();
        assert!(!exchange.observe_event(&late_error, 23).unwrap());
        assert_eq!(exchange.turn[0].completed(), completed.as_ref());
        assert_eq!(exchange.turn[0].provider(), &address);
        assert_eq!(exchange.turn[0].errors(), &[failure, late_failure]);
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

    #[test]
    fn interrupted_exchange_settles_provider_turns_permanently() {
        let mut exchange = interaction();
        exchange.resume(100).unwrap();
        let address = crate::turn::test_support::address(&exchange, "turn");
        exchange.start_turn(address, 100).unwrap();
        exchange
            .finish(crate::exchange::ExchangeOutcome::Interrupted, 150)
            .unwrap();
        assert_eq!(exchange.elapsed(90_000), 50);
        assert_eq!(
            exchange.turn[0].state(),
            crate::turn::TurnState::Finished {
                outcome: crate::turn::TurnOutcome::Interrupted,
            }
        );
        assert!(exchange.resume(90_000).is_err());
        exchange
            .finish(crate::exchange::ExchangeOutcome::Interrupted, 90_000)
            .unwrap();
        assert_eq!(exchange.completed_at_ms(), Some(150));
        assert!(
            exchange
                .finish(crate::exchange::ExchangeOutcome::Cancelled, 90_000)
                .is_err()
        );
    }

    #[test]
    fn terminal_exchanges_reject_every_input_intent_without_mutation() {
        for outcome in [
            ExchangeOutcome::Complete,
            ExchangeOutcome::Cancelled,
            ExchangeOutcome::Failed,
            ExchangeOutcome::Interrupted,
        ] {
            let mut exchange = interaction();
            exchange.finish(outcome, 30).unwrap();
            let original = serde_json::to_value(&exchange).unwrap();
            assert!(
                exchange
                    .append_delegation("child", "late", "task", 90)
                    .is_err()
            );
            assert_eq!(serde_json::to_value(&exchange).unwrap(), original);
            for intent in [InputIntent::Steering, InputIntent::Clarification] {
                assert!(
                    exchange
                        .append_input(intent, "late input".into(), 90)
                        .is_err()
                );
                assert_eq!(serde_json::to_value(&exchange).unwrap(), original);
            }
        }
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
                    InputIntent::Steering,
                    "late".into(),
                    70,
                )
                .is_err()
        );
        assert_eq!(exchange.state(), ExchangeState::Complete);
    }

    #[test]
    fn finalization_freezes_time_and_rejects_input_but_records_inflight_evidence() {
        let mut exchange = interaction();
        exchange.resume(10).unwrap();
        exchange
            .begin_finalization(crate::exchange::ExchangeOutcome::Complete, 30)
            .unwrap();
        exchange
            .begin_finalization(crate::exchange::ExchangeOutcome::Complete, 90)
            .unwrap();
        assert!(
            exchange
                .begin_finalization(crate::exchange::ExchangeOutcome::Cancelled, 90)
                .is_err()
        );
        assert_eq!(exchange.elapsed(1000), 20);
        assert!(exchange.resume(1000).is_err());
        assert!(
            exchange
                .append_input(InputIntent::Steering, "late".into(), 1000)
                .is_err()
        );
        assert!(
            exchange
                .append_delegation("child", "late", "task", 1000)
                .is_ok()
        );
        exchange
            .finish(crate::exchange::ExchangeOutcome::Complete, 1000)
            .unwrap();
        assert!(
            exchange
                .append_delegation("child", "after-terminal", "task", 1100)
                .is_err()
        );
        assert_eq!(exchange.elapsed(2000), 20);
    }

    #[test]
    fn terminal_exchange_cannot_restart() {
        let mut exchange = interaction();
        let address = crate::turn::test_support::address(&exchange, "turn");
        exchange.start_turn(address.clone(), 10).unwrap();
        exchange
            .turn_mut(&address)
            .unwrap()
            .finish(
                crate::turn::TurnOutcome::Completed {
                    stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
                },
                30,
            )
            .unwrap();
        exchange
            .finish(crate::exchange::ExchangeOutcome::Complete, 30)
            .unwrap();
        assert!(exchange.resume(900).is_err());
        assert!(exchange.start_turn(address, 900).is_err());
        exchange
            .finish(crate::exchange::ExchangeOutcome::Complete, 900)
            .unwrap();
        assert_eq!(exchange.completed_at_ms(), Some(30));
        assert_eq!(exchange.turn[0].duration_ms(), None);
        assert!(
            exchange
                .finish(crate::exchange::ExchangeOutcome::Failed, 901)
                .is_err()
        );
    }

    #[test]
    fn finalization_settles_running_turn_without_changing_completed_turns() {
        let mut exchange = interaction();
        let first = crate::turn::test_support::address(&exchange, "first");
        exchange.start_turn(first.clone(), 10).unwrap();
        exchange
            .turn_mut(&first)
            .unwrap()
            .finish(
                crate::turn::TurnOutcome::Completed {
                    stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
                },
                20,
            )
            .unwrap();
        exchange
            .start_turn(crate::turn::test_support::address(&exchange, "second"), 30)
            .unwrap();
        exchange
            .finish(crate::exchange::ExchangeOutcome::Cancelled, 50)
            .unwrap();
        assert_eq!(exchange.turn[0].duration_ms(), None);
        assert_eq!(exchange.turn[1].duration_ms(), None);
        assert_eq!(
            exchange.turn[1].state(),
            crate::turn::TurnState::Finished {
                outcome: crate::turn::TurnOutcome::Interrupted
            }
        );
    }

    #[test]
    fn approval_wait_freezes_exchange_and_overlapping_tool_time_until_last_decision() {
        let mut exchange = interaction();
        exchange.resume(1000).unwrap();
        exchange.observe_blocker("tool:first".into(), true, 1500);
        exchange.observe_blocker("tool:second".into(), true, 2000);
        exchange.observe_blocker("approval:first".into(), true, 3000);
        exchange.observe_blocker("approval:first".into(), true, 9000);
        exchange.observe_blocker("approval:second".into(), true, 10_000);
        for now_ms in [10_000, 90_000] {
            assert_eq!(exchange.elapsed(now_ms), 2000);
            assert_eq!(
                exchange.metrics.tool_ms(exchange.elapsed(now_ms)),
                Some(1500)
            );
        }
        exchange.observe_blocker("approval:first".into(), false, 90_000);
        exchange.observe_blocker("approval:unknown".into(), false, 95_000);
        assert_eq!(exchange.elapsed(100_000), 2000);
        exchange.resume(100_000).unwrap();
        assert_eq!(exchange.elapsed(110_000), 2000);
        exchange.observe_blocker("approval:second".into(), false, 120_000);
        assert_eq!(exchange.elapsed(121_000), 3000);
        assert_eq!(
            exchange.metrics.tool_ms(exchange.elapsed(121_000)),
            Some(2500)
        );
        exchange.observe_blocker("tool:first".into(), false, 121_000);
        exchange.observe_blocker("tool:second".into(), false, 122_000);
        exchange.observe_blocker("approval:second".into(), false, 125_000);
        assert_eq!(exchange.elapsed(126_000), 8000);
        assert_eq!(
            exchange.metrics.tool_ms(exchange.elapsed(126_000)),
            Some(3500)
        );
    }

    #[test]
    fn approval_resolution_preserves_other_pause_and_terminal_boundaries() {
        for boundary in [
            "already_paused",
            "user_input",
            "pause",
            "restart",
            "finalization",
            "cancelled",
        ] {
            let mut exchange = interaction();
            exchange.resume(1000).unwrap();
            if boundary == "already_paused" {
                exchange.pause(2000);
            }
            exchange.observe_blocker("approval:request".into(), true, 3000);
            match boundary {
                "user_input" => exchange.set_awaiting_input(true),
                "pause" => exchange.pause(4000),
                "restart" => exchange.pause_for_restart(4000).unwrap(),
                "finalization" => {
                    exchange
                        .begin_finalization(crate::exchange::ExchangeOutcome::Complete, 4000)
                        .unwrap();
                }
                "cancelled" => exchange
                    .finish(crate::exchange::ExchangeOutcome::Cancelled, 4000)
                    .unwrap(),
                _ => {}
            }
            let elapsed = exchange.elapsed(5000);
            exchange.observe_blocker("approval:request".into(), false, 90_000);
            assert_eq!(exchange.elapsed(100_000), elapsed, "{boundary}");
            assert!(exchange.execution_started_at_ms().is_none(), "{boundary}");
        }
    }

    #[test]
    fn approval_resume_intent_does_not_survive_coordinator_restart() {
        let mut exchange = interaction();
        exchange.resume(1000).unwrap();
        exchange.observe_blocker("approval:request".into(), true, 3000);
        let mut recovered: Exchange =
            serde_json::from_value(serde_json::to_value(exchange).unwrap()).unwrap();
        recovered.observe_blocker("approval:request".into(), false, 90_000);
        assert_eq!(recovered.elapsed(100_000), 2000);
        assert!(recovered.execution_started_at_ms().is_none());
    }

    #[test]
    fn elapsed_time_excludes_user_wait_and_freezes_at_completion() {
        let mut exchange = interaction();
        exchange.resume(100).unwrap();
        assert_eq!(exchange.elapsed(250), 150);
        exchange.pause(300);
        exchange.set_awaiting_input(true);
        assert_eq!(exchange.elapsed(10_000), 200);
        exchange.resume(10_000).unwrap();
        assert_eq!(exchange.elapsed(10_050), 250);
        exchange
            .finish(crate::exchange::ExchangeOutcome::Complete, 10_100)
            .unwrap();
        assert_eq!(exchange.elapsed(90_000), 300);
    }

    #[test]
    fn rollback_disposition_preserves_execution_outcome() {
        let mut exchange = interaction();
        assert!(
            exchange
                .set_disposition(HistoryDisposition::RolledBack)
                .is_err()
        );
        exchange
            .finish(crate::exchange::ExchangeOutcome::Failed, 30)
            .unwrap();
        exchange
            .set_disposition(HistoryDisposition::RolledBack)
            .unwrap();
        assert_eq!(exchange.state(), ExchangeState::Failed);
        assert_eq!(exchange.completed_at_ms(), Some(30));
        assert!(exchange.resume(40).is_err());
    }

    #[test]
    fn plan_artifact_does_not_affect_provider_turn_lifecycle() {
        let mut interaction = interaction();
        let address = crate::turn::test_support::address(&interaction, "turn");
        interaction.start_turn(address.clone(), 10).unwrap();
        interaction.node_list.push(ExchangeNode::ArtifactChange {
            change: ArtifactChange {
                revision_request: false,
                id: "artifact-one".into(),
                path: "plan.md".into(),
                diff_text: "plan".into(),
                created_at_ms: 20,
                declaration: None,
            },
        });

        interaction
            .turn_mut(&address)
            .unwrap()
            .finish(
                crate::turn::TurnOutcome::Completed {
                    stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
                },
                210,
            )
            .unwrap();

        assert_eq!(interaction.node_list.len(), 1);
        assert!(matches!(
            interaction.node_list[0],
            ExchangeNode::ArtifactChange { .. }
        ));
        assert_eq!(interaction.turn[0].duration_ms(), None);
    }
    /// Exact native replay stays terminal while unregistered new foreground ownership is rejected.
    #[test]
    fn native_reopening_preserves_original_history_and_rejects_replayed_start() {
        let mut exchange = interaction();
        let first = crate::turn::test_support::address(&exchange, "first");
        let start = turn_event(
            &first,
            forge_provider_api::TurnEvent::Started(forge_provider_api::turn::TurnStarted {
                started_at_ms: 10,
            }),
        );
        assert!(exchange.observe_event(&start, 10).unwrap());
        exchange
            .observe_event(
                &crate::turn::test_support::completed_event(
                    &first,
                    crate::turn::TurnOutcome::Completed {
                        stop_reason: forge_provider_api::turn::TurnCompletionReason::Normal,
                    },
                    20,
                ),
                20,
            )
            .unwrap();
        exchange.finish(ExchangeOutcome::Complete, 20).unwrap();
        let before = serde_json::to_value(&exchange).unwrap();
        assert!(!exchange.observe_event(&start, 30).unwrap());
        let unknown = crate::provider::TurnAddress {
            turn_id: forge_provider_api::TurnId::new("unregistered").unwrap(),
            ..first.clone()
        };
        let new_work = turn_event(
            &unknown,
            forge_provider_api::TurnEvent::Started(forge_provider_api::turn::TurnStarted {
                started_at_ms: 30,
            }),
        );
        assert!(exchange.observe_event(&new_work, 30).is_err());
        let foreign = crate::provider::TurnAddress {
            thread_id: forge_provider_api::ThreadId::new("foreign").unwrap(),
            ..first
        };
        let wrong_source = turn_event(
            &foreign,
            forge_provider_api::TurnEvent::Started(forge_provider_api::turn::TurnStarted {
                started_at_ms: 10,
            }),
        );
        assert!(exchange.observe_event(&wrong_source, 31).is_err());
        assert_eq!(serde_json::to_value(&exchange).unwrap(), before);
        assert_eq!(exchange.state(), ExchangeState::Complete);
        assert_eq!(exchange.turn.len(), 1);
    }

    /// Explicit cancellation cannot admit a new unregistered native response cycle.
    #[test]
    fn native_reopening_rejects_cancelled_exchange() {
        let mut exchange = interaction();
        let first = crate::turn::test_support::address(&exchange, "first");
        exchange.start_turn(first.clone(), 10).unwrap();
        exchange.finish(ExchangeOutcome::Cancelled, 20).unwrap();
        let before = serde_json::to_value(&exchange).unwrap();
        let unknown = crate::provider::TurnAddress {
            turn_id: forge_provider_api::TurnId::new("unregistered").unwrap(),
            ..first
        };
        let new_work = turn_event(
            &unknown,
            forge_provider_api::TurnEvent::Started(forge_provider_api::turn::TurnStarted {
                started_at_ms: 30,
            }),
        );
        assert!(exchange.observe_event(&new_work, 30).is_err());
        assert_eq!(serde_json::to_value(&exchange).unwrap(), before);
        assert_eq!(exchange.state(), ExchangeState::Cancelled);
        assert_eq!(exchange.turn.len(), 1);
    }

    /// Identified accounting replay cannot charge tokens or requests twice after history reload.
    #[test]
    fn canonical_usage_report_charges_once_after_reload() -> anyhow::Result<()> {
        let mut exchange = interaction();
        let address = crate::turn::test_support::address(&exchange, "turn");
        exchange.start_turn(address.clone(), 1)?;
        let report = forge_provider_api::TurnUsageReport::accounting(
            forge_provider_api::identity::UsageReportId::new("report")?,
            2,
            Some(forge_provider_api::usage::TokenUsage {
                input: Some(3),
                cached_input: Some(0),
                reasoning: Some(0),
                output: Some(4),
            }),
            Some(15),
        )?;
        let event = turn_event(&address, crate::provider::TurnEvent::Usage(report));
        assert!(exchange.observe_event(&event, 2)?);
        assert_eq!(exchange.usage().input, Some(3));
        assert_eq!(exchange.usage().output, Some(4));
        assert_eq!(exchange.metrics.request_count, 1);
        let mut restored: Exchange = serde_json::from_value(serde_json::to_value(&exchange)?)?;
        assert!(!restored.observe_event(&event, 3)?);
        assert_eq!(restored.usage().input, Some(3));
        assert_eq!(restored.usage().output, Some(4));
        assert_eq!(restored.metrics.request_count, 1);
        Ok(())
    }

    /// Complementary measurements complete one retained charge instead of becoming a second request.
    #[test]
    fn complementary_usage_reports_fill_missing_counts_without_recharging() -> anyhow::Result<()> {
        let mut exchange = interaction();
        let address = crate::turn::test_support::address(&exchange, "turn");
        exchange.start_turn(address.clone(), 1)?;
        for usage in [
            forge_provider_api::usage::TokenUsage {
                input: Some(3),
                cached_input: Some(0),
                reasoning: Some(0),
                output: None,
            },
            forge_provider_api::usage::TokenUsage {
                input: None,
                cached_input: None,
                reasoning: None,
                output: Some(4),
            },
        ] {
            let report = forge_provider_api::TurnUsageReport::accounting(
                forge_provider_api::identity::UsageReportId::new("report")?,
                2,
                Some(usage),
                None,
            )?;
            exchange.observe_event(
                &turn_event(&address.clone(), crate::provider::TurnEvent::Usage(report)),
                2,
            )?;
            exchange = serde_json::from_value(serde_json::to_value(&exchange)?)?;
        }
        assert_eq!(exchange.usage().input, Some(3));
        assert_eq!(exchange.usage().output, Some(4));
        assert_eq!(exchange.metrics.request_count, 1);
        Ok(())
    }
}

#[cfg(test)]
mod observed_tests {
    use super::*;
    use forge_provider_api::{
        AgentId, AgentMessageStart, EventScope, InvocationScope, ItemEvent, ItemId, ItemStart,
        MessageClassification, ProviderEvent, ThreadOwner, TurnAddress,
    };

    #[test]
    fn observed_child_retains_original_scope_without_a_user_submission() -> anyhow::Result<()> {
        let scope = InvocationScope {
            owner: ThreadOwner::new("session".into(), AgentId::new("native-child")?)?,
            exchange_id: "parent-exchange".into(),
            invocation_id: "child-invocation".into(),
        };
        let address = TurnAddress {
            thread_id: scope.owner.thread_id(),
            turn_id: scope.turn_id()?,
            scope: scope.clone(),
        };
        let mut exchange =
            Exchange::observed("observed-child-exchange".into(), 1, address.clone(), 1)?;
        assert!(exchange.prompt.is_empty());
        assert!(exchange.input_message_id.is_none());
        assert_eq!(
            exchange.origin,
            crate::exchange::ExchangeOrigin::NativeAgent
        );
        let event = ProviderEvent::Item {
            thread_id: address.thread_id.clone(),
            turn_id: address.turn_id.clone(),
            item_id: ItemId::new("child-message")?,
            event: ItemEvent::Started(ItemStart::AgentMessage(AgentMessageStart {
                text: "Child work".into(),
                classification: MessageClassification::Unclassified,
            })),
        };
        let original = EventScope::Invocation {
            invocation: scope.clone(),
        };
        assert!(exchange.observe_provider_event(&original, &event, 2)?);
        let snapshot = serde_json::to_value(&exchange)?;
        let mut foreign = scope.clone();
        foreign.invocation_id = "successor-invocation".into();
        assert!(!exchange.observe_provider_event(
            &EventScope::Invocation {
                invocation: foreign
            },
            &event,
            3
        )?);
        assert_eq!(serde_json::to_value(&exchange)?, snapshot);
        let restored: Exchange = serde_json::from_value(snapshot)?;
        assert_eq!(restored.turn[0].provider(), &address);
        assert_eq!(restored.turn[0].messages()[0].text(), "Child work");
        assert_eq!(restored.agent_id, "native-child");
        Ok(())
    }
}
