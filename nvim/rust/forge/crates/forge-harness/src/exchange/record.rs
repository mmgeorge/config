use super::*;
use crate::agent::Delegation;
use crate::plan::PlanElicitation;
use serde::{Deserialize, Serialize};

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
    pub provider_retry: Option<crate::backend::retry::ProviderRetry>,
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
    pub approval_resolution: Vec<crate::backend::approval::ApprovalRequestView>,
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
    pub provider_retry: Option<crate::backend::retry::ProviderRetry>,
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
    pub approval_resolution: Vec<crate::backend::approval::ApprovalRequestView>,
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
        let mut value = serde_json::Value::deserialize(deserializer)?;
        let mut legacy = std::collections::HashMap::new();
        for turn in value
            .get_mut("turn")
            .and_then(serde_json::Value::as_array_mut)
            .into_iter()
            .flatten()
        {
            let id = turn["id"].as_str().unwrap_or_default().to_owned();
            if let Some(tool) = turn
                .as_object_mut()
                .and_then(|turn| turn.remove("tool"))
                .filter(|tool| !tool.is_null())
            {
                legacy.insert(
                    id,
                    serde_json::from_value::<crate::turn::ToolStore>(tool)
                        .map_err(serde::de::Error::custom)?,
                );
            }
        }
        let mut exchange = ExchangeRecord::deserialize(value).map_err(serde::de::Error::custom)?;
        for turn in &exchange.turn {
            if let Some(store) = legacy.get(turn.id()) {
                for tool in store.iter() {
                    let present = exchange.node_list.iter().any(|node| matches!(node,
                        ExchangeNode::TurnContent { turn_id, item: crate::turn::TurnItem::Tool { id }, .. }
                        if turn_id == turn.id() && id == &tool.id));
                    if !present {
                        exchange.node_list.push(ExchangeNode::TurnContent {
                            id: format!("{}:content:{}", turn.id(), tool.id),
                            turn_id: turn.id().to_owned(),
                            item: crate::turn::TurnItem::Tool {
                                id: tool.id.clone(),
                            },
                        });
                    }
                }
            }
        }
        exchange
            .reconcile_entries()
            .map_err(serde::de::Error::custom)?;
        let positions: Vec<_> = exchange
            .node_list
            .iter()
            .filter_map(|node| match node {
                ExchangeNode::TurnContent {
                    turn_id,
                    item: crate::turn::TurnItem::Tool { id },
                    ..
                } => Some((turn_id.clone(), id.clone())),
                _ => None,
            })
            .collect();
        for (turn_id, id) in positions {
            if let Some(tool) = legacy.get_mut(&turn_id).and_then(|store| store.take(&id)) {
                exchange
                    .tool_store_mut(&turn_id, &id)
                    .map_err(serde::de::Error::custom)?
                    .insert(tool);
            }
        }
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

    /// Freeze execution before cleanup and checkpoint persistence begin.
    pub fn begin_finalization(
        &mut self,
        outcome: ExchangeOutcome,
        now_ms: i64,
    ) -> anyhow::Result<()> {
        anyhow::ensure!(
            self.finalization_outcome()
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
        compaction: crate::backend::Compaction,
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

    /// Retain fresh usage on the latest compaction without changing settled history.
    pub(crate) fn observe_compaction_usage(&mut self, thread_id: &str, used: u64) -> bool {
        let Some(compaction) = self.node_list.iter_mut().rev().find_map(|node| match node {
            ExchangeNode::Compaction { compaction, .. } if compaction.thread_id == thread_id => {
                Some(compaction)
            }
            _ => None,
        }) else {
            return false;
        };
        compaction.observe_context_usage(used)
    }

    /// Stop active compaction presentation when its provider execution ends without confirmation.
    pub(crate) fn unconfirm_compaction(&mut self, thread_id: &str, now_ms: i64) {
        for node in &mut self.node_list {
            if let ExchangeNode::Compaction { compaction, .. } = node
                && compaction.thread_id == thread_id
                && compaction.state == crate::backend::CompactionState::Running
            {
                compaction.state = crate::backend::CompactionState::Unconfirmed;
                compaction.completed_at_ms = Some(now_ms);
            }
        }
    }

    /// Route provider lifecycle and tool provenance before accepting presentation content.
    pub(crate) fn observe_turn(
        &mut self,
        event: &crate::backend::BackendEvent,
        now_ms: i64,
    ) -> anyhow::Result<bool> {
        use crate::backend::TurnBoundary;
        use crate::turn::TurnState;
        if event.kind == "provider_retry" {
            if self.state() != ExchangeState::Running
                || self.is_finished()
                || !event
                    .address
                    .as_ref()
                    .is_some_and(|address| self.turn.iter().any(|turn| turn.provider() == address))
            {
                return Ok(false);
            }
            self.provider_retry = serde_json::from_value(event.data.clone())?;
            return Ok(true);
        }
        let Some(address) = &event.address else {
            anyhow::ensure!(
                event.turn_boundary.is_none(),
                "provider turn boundary has no execution address"
            );
            self.metrics.observe_elapsed(self.elapsed(now_ms));
            return Ok(true);
        };
        if event.kind == "usage" {
            let Some(index) = self.turn.iter().position(|turn| turn.provider() == address) else {
                return Ok(false);
            };
            let update = serde_json::from_value(event.data.clone())?;
            if let Some(usage) = self
                .metrics
                .usage(&address.thread_id, &address.turn_id, update)
            {
                self.turn[index].record_usage(usage);
                self.metrics
                    .record_throughput(self.usage().output, self.elapsed(now_ms));
            }
            self.metrics.observe_elapsed(self.elapsed(now_ms));
            return Ok(true);
        }
        match event.turn_boundary {
            Some(TurnBoundary::Started) => {
                self.start_turn(address.clone(), now_ms)?;
            }
            Some(TurnBoundary::Finished { outcome }) => {
                let Some(turn) = self.turn.iter_mut().find(|turn| turn.provider() == address)
                else {
                    return Ok(false);
                };
                let turn_id = turn.id().to_owned();
                turn.finish(outcome, now_ms)?;
                self.finish_tools(&turn_id, outcome, now_ms);
                self.rebuild_tool_counter();
                let prefix = format!("tool:{}:{}:", address.thread_id, address.turn_id);
                let elapsed = self.elapsed(now_ms);
                for id in self
                    .turn
                    .iter()
                    .find(|turn| turn.provider() == address)
                    .into_iter()
                    .flat_map(|turn| self.tools_for_turn(turn.id()))
                    .map(|tool| format!("{prefix}{}", tool.id))
                    .collect::<Vec<_>>()
                {
                    self.metrics.block(id, false, elapsed);
                }
            }
            None => {
                let Some(turn) = self.turn.iter_mut().find(|turn| turn.provider() == address)
                else {
                    return Ok(false);
                };
                if turn.state() != TurnState::Running {
                    return Ok(false);
                }
                let item_count = turn.items().len();
                let mut new_tool = false;
                if let Some(tool_event) = event.tool_event() {
                    let tool = tool_event.activity();
                    new_tool = !turn.items().iter().any(|item| {
                        matches!(item,
                        crate::turn::TurnItem::Tool { id } if id == &tool.id)
                    });
                    turn.admit_tool(&tool.id)?;
                }
                if let Some(message) = &event.message {
                    turn.record_message(message)?;
                }
                let turn_id = turn.id().to_owned();
                for item in turn.items()[item_count..].iter().cloned() {
                    let item_id = match &item {
                        crate::turn::TurnItem::Message { id }
                        | crate::turn::TurnItem::Tool { id } => id,
                    };
                    self.node_list.push(ExchangeNode::TurnContent {
                        id: format!("{turn_id}:content:{item_id}"),
                        turn_id: turn_id.clone(),
                        item,
                    });
                }
                self.reconcile_entries()?;
                if let Some(tool_event) = event.tool_event() {
                    self.record_tool_event(&turn_id, tool_event, now_ms)?;
                    let activity = tool_event.activity();
                    let running = self
                        .tools_for_turn(&turn_id)
                        .find(|tool| tool.id == activity.id)
                        .is_some_and(|tool| tool.state() == crate::turn::ToolState::Running);
                    if new_tool && !running {
                        self.metrics.timing_complete = false;
                    }
                    let id = format!(
                        "tool:{}:{}:{}",
                        address.thread_id, address.turn_id, activity.id
                    );
                    let elapsed = self.clock.elapsed(event.observed_at_ms(now_ms));
                    self.metrics.block(id, running, elapsed);
                }
                if let Some(reads) = event.data.get("skill_reads") {
                    let reads: Vec<crate::skill::SkillRead> =
                        serde_json::from_value(reads.clone())?;
                    for read in reads {
                        let successful = self
                            .turn
                            .iter()
                            .find(|turn| turn.id() == turn_id)
                            .and_then(|turn| {
                                self.tools_for_turn(turn.id())
                                    .find(|tool| tool.id == read.tool_id)
                            })
                            .is_some_and(|tool| {
                                tool.state() == crate::turn::ToolState::Completed && !tool.failed
                            });
                        if !successful
                            || self.node_list.iter().any(|node| {
                                matches!(node,
                            ExchangeNode::SkillRead {turn_id:owner,read:existing,..}
                                if owner == &turn_id && existing.resource_id == read.resource_id)
                            })
                        {
                            continue;
                        }
                        self.node_list.push(ExchangeNode::SkillRead {
                            id: format!("{turn_id}:skill:{}", read.resource_id),
                            turn_id: turn_id.clone(),
                            read,
                        });
                    }
                }
            }
        }
        self.reconcile_entries()?;
        self.metrics.observe_elapsed(self.elapsed(now_ms));
        Ok(true)
    }
    /// Record provider execution, including late start evidence received during cleanup.
    pub fn start_turn(
        &mut self,
        address: crate::backend::ProviderAddress,
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
            !address.thread_id.is_empty() && !address.turn_id.is_empty(),
            "provider turn address is incomplete"
        );
        if let Some(index) = self
            .turn
            .iter()
            .position(|turn| turn.provider() == &address)
        {
            anyhow::ensure!(
                self.turn[index].state() == crate::turn::TurnState::Running,
                "completed provider turn cannot restart"
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
        address: &crate::backend::ProviderAddress,
    ) -> anyhow::Result<&mut crate::turn::Turn> {
        self.turn
            .iter_mut()
            .find(|turn| turn.provider() == address)
            .ok_or_else(|| anyhow::anyhow!("provider event has no admitted turn"))
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

    /// Settle interrupted provider turns while retaining the request and its original checkpoint.
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

    /// Return accumulated execution time without allowing terminal history to tick.
    pub fn elapsed(&self, now_ms: i64) -> u64 {
        if self.state() == ExchangeState::Running {
            self.clock.elapsed(now_ms)
        } else {
            self.clock.duration_ms()
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
                            | crate::turn::TurnOutcome::Cancelled
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
                if compaction.state == crate::backend::CompactionState::Running =>
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
        for turn in &mut self.turn {
            if turn.state() == crate::turn::TurnState::Running {
                anyhow::ensure!(
                    outcome != ExchangeOutcome::Complete,
                    "exchange completion requires settled provider turns"
                );
                turn.finish(
                    match outcome {
                        ExchangeOutcome::Cancelled => crate::turn::TurnOutcome::Cancelled,
                        ExchangeOutcome::Interrupted => crate::turn::TurnOutcome::Interrupted,
                        ExchangeOutcome::Failed => crate::turn::TurnOutcome::Failed,
                        ExchangeOutcome::Complete => crate::turn::TurnOutcome::Completed,
                    },
                    now_ms,
                )?;
            }
        }
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
        self.append_identified_input(None, intent, text, now_ms)
    }

    pub(crate) fn append_identified_input(
        &mut self,
        id: Option<String>,
        intent: InputIntent,
        text: String,
        now_ms: i64,
    ) -> anyhow::Result<ExchangeInput> {
        self.record_input(id, intent, text, None, now_ms)
    }

    fn record_input(
        &mut self,
        id: Option<String>,
        intent: InputIntent,
        text: String,
        question: Option<QuestionInput>,
        now_ms: i64,
    ) -> anyhow::Result<ExchangeInput> {
        if let Some(id) = &id
            && let Some(prompt) = self.node_list.iter().find_map(|node| match node {
                ExchangeNode::ExchangeInput { prompt } if &prompt.id == id => Some(prompt),
                _ => None,
            })
        {
            return Ok(prompt.clone());
        }
        anyhow::ensure!(
            self.state() == ExchangeState::Running,
            "user input requires a current running exchange"
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
        self.record_input(None, intent, text, Some(question), now_ms)?;
        Ok(())
    }

    /// Sum usage across owned turns, retaining unavailable categories.
    pub fn usage(&self) -> crate::backend::usage::TokenUsage {
        let mut turn = self.turn.iter().filter_map(|turn| turn.usage());
        let Some(first) = turn.next() else {
            return Default::default();
        };
        let mut usage = first.clone();
        for turn in turn {
            usage.accumulate(turn);
        }
        usage
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

/// Represents whether an interaction can still receive backend turns.
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

    #[test]
    fn input_timeout_approval_provenance_survives_exchange_serialization() {
        let mut exchange = interaction();
        exchange.approval_resolution.push(
            serde_json::from_value(serde_json::json!({
                "id":"timed-out", "provider":"test", "exchange_id":exchange.id, "turn_id":null,
                "reason":"Inspect source", "item_list":[], "deadline":{"expires_at_ms":1000},
                "resolution_source":"timeout"
            }))
            .unwrap(),
        );
        let encoded = serde_json::to_value(&exchange).unwrap();
        let restored: Exchange = serde_json::from_value(encoded.clone()).unwrap();
        assert_eq!(
            restored.approval_resolution[0].resolution_source,
            crate::session::ResolutionSource::Timeout
        );
        assert_eq!(
            restored.approval_resolution[0]
                .deadline
                .as_ref()
                .unwrap()
                .expires_at_ms,
            1000
        );
        let mut legacy = encoded;
        legacy
            .as_object_mut()
            .unwrap()
            .remove("approval_resolution");
        assert!(
            serde_json::from_value::<Exchange>(legacy)
                .unwrap()
                .approval_resolution
                .is_empty()
        );
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
        use crate::backend::{BackendEvent, ProviderAddress};
        use crate::session::state_machine::SessionPhase;
        let mut exchange = interaction();
        exchange.resume(1000)?;
        let address = ProviderAddress {
            owner: None,
            thread_id: "thread".into(),
            turn_id: "failed-turn".into(),
        };
        let retry = crate::backend::retry::ProviderRetry {
            attempt: 1,
            max_attempts: 5,
            retry_at_ms: Some(2000),
        };
        let mut event = BackendEvent {
            message: None,
            received_at_ms: None,
            address: Some(address.clone()),
            turn_boundary: None,
            kind: "provider_retry".into(),
            text: None,
            data: serde_json::to_value(&retry)?,
            activity: None,
            summary: None,
            checklist_update: None,
        };
        assert!(!exchange.observe_turn(&event, 1000)?);
        exchange.start_turn(address.clone(), 1000)?;
        exchange
            .turn_mut(&address)?
            .finish(crate::turn::TurnOutcome::Failed, 1001)?;
        assert!(exchange.observe_turn(&event, 1001)?);
        assert!(matches!(
            SessionPhase::resolve(None, None, Some(&exchange)),
            SessionPhase::RetryingProvider { .. }
        ));
        event.data = serde_json::Value::Null;
        assert!(exchange.observe_turn(&event, 2000)?);
        assert!(matches!(
            SessionPhase::resolve(None, None, Some(&exchange)),
            SessionPhase::Working { .. }
        ));
        event.data = serde_json::to_value(&retry)?;
        exchange.finish(crate::exchange::ExchangeOutcome::Failed, 2001)?;
        assert!(!exchange.observe_turn(&event, 2002)?);
        assert!(exchange.provider_retry.is_none());
        Ok(())
    }

    #[test]
    fn tool_timing_excludes_user_input_pauses_and_rejects_missing_start_evidence() {
        use crate::backend::{BackendEvent, ProviderAddress, ToolActivity, ToolActivityKind};
        let mut exchange = interaction();
        exchange.resume(1000).unwrap();
        let address = ProviderAddress {
            owner: None,
            thread_id: "parent".into(),
            turn_id: "one".into(),
        };
        exchange.start_turn(address.clone(), 1000).unwrap();
        let mut event = BackendEvent {
            message: None,
            received_at_ms: None,
            address: Some(address),
            turn_boundary: None,
            kind: "tool".into(),
            text: None,
            data: serde_json::json!({}),
            activity: Some(ToolActivity {
                id: "call".into(),
                kind: ToolActivityKind::ToolCall,
                title: "command".into(),
                output: None,
                status: Some(crate::backend::ToolStatus::Running),
                change: Default::default(),
                output_delta: false,
            }),
            summary: None,
            checklist_update: None,
        };
        exchange.observe_turn(&event, 2000).unwrap();
        exchange.pause(3000);
        assert_eq!(exchange.elapsed(90000), 2000);
        assert_eq!(
            exchange.metrics.response_ms(exchange.elapsed(90000)),
            Some(1000)
        );
        exchange.resume(90000).unwrap();
        event.activity.as_mut().unwrap().status = Some(crate::backend::ToolStatus::Succeeded);
        exchange.observe_turn(&event, 92000).unwrap();
        assert_eq!(exchange.elapsed(93000), 5000);
        assert_eq!(exchange.metrics.response_ms(5000), Some(2000));
        event.activity.as_mut().unwrap().status = Some(crate::backend::ToolStatus::Running);
        exchange.observe_turn(&event, 93000).unwrap();
        assert_eq!(exchange.metrics.response_ms(6000), Some(3000));
        event.activity.as_mut().unwrap().id = "missing-start".into();
        event.activity.as_mut().unwrap().status = Some(crate::backend::ToolStatus::Succeeded);
        exchange.observe_turn(&event, 94000).unwrap();
        assert_eq!(exchange.metrics.response_ms(6000), None);
    }

    #[test]
    fn message_snapshots_reconcile_without_duplicating_or_reordering_content() {
        use crate::backend::{BackendEvent, ProviderAddress};
        let mut exchange = interaction();
        let address = ProviderAddress {
            owner: None,
            thread_id: "parent".into(),
            turn_id: "turn".into(),
        };
        exchange.start_turn(address.clone(), 10).unwrap();
        let mut event = BackendEvent {
            message: None,
            received_at_ms: None,
            address: Some(address),
            turn_boundary: None,
            kind: "assistant_message".into(),
            text: Some("Checking".into()),
            data: serde_json::json!({"params":{"itemId":"one"}}),
            activity: None,
            summary: None,
            checklist_update: None,
        };
        crate::turn::test_support::set_message(
            &mut event,
            "one",
            crate::backend::MessageRole::Commentary,
        );
        exchange.observe_turn(&event, 11).unwrap();
        exchange
            .append_input(InputIntent::Steering, "Also tests".into(), 12)
            .unwrap();
        event.text = Some("Done".into());
        event.data = serde_json::json!({"params":{"itemId":"two","phase":"final_answer"}});
        crate::turn::test_support::set_message(
            &mut event,
            "two",
            crate::backend::MessageRole::Response,
        );
        exchange.observe_turn(&event, 13).unwrap();
        event.text = Some("Checking files.".into());
        event.data = serde_json::json!({"message_update":"snapshot", "params":{
            "item":{"id":"one","phase":"commentary"}
        }});
        event.message = Some(crate::backend::ProviderMessage {
            id: crate::backend::MessageId("one".into()),
            role: Some(crate::backend::MessageRole::Commentary),
            update: crate::backend::MessageUpdate::Completed {
                text: "Checking files.".into(),
            },
        });
        exchange.observe_turn(&event, 14).unwrap();
        exchange.observe_turn(&event, 15).unwrap();
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
            .finish(crate::turn::TurnOutcome::Completed, 16)
            .unwrap();
        event.text = Some("Late correction".into());
        event.message.as_mut().unwrap().update = crate::backend::MessageUpdate::Completed {
            text: "Late correction".into(),
        };
        assert!(!exchange.observe_turn(&event, 17).unwrap());
        assert_eq!(exchange.turn[0].messages()[0].text(), "Checking files.");
    }

    #[test]
    fn active_turn_exposes_only_its_latest_readable_reasoning_summary() {
        use crate::backend::{BackendEvent, ProviderAddress};
        let mut exchange = interaction();
        let address = ProviderAddress {
            owner: None,
            thread_id: "parent".into(),
            turn_id: "turn".into(),
        };
        exchange.start_turn(address.clone(), 10).unwrap();
        let mut event = BackendEvent {
            message: None,
            received_at_ms: None,
            address: Some(address),
            turn_boundary: None,
            kind: "reasoning_summary".into(),
            text: Some("Inspecting the repository".into()),
            data: serde_json::json!({"provider_message_id":"reasoning:summary:0"}),
            activity: None,
            summary: None,
            checklist_update: None,
        };
        crate::turn::test_support::set_message(
            &mut event,
            "reasoning:summary:0",
            crate::backend::MessageRole::Thought,
        );
        exchange.observe_turn(&event, 11).unwrap();
        event.kind = "reasoning".into();
        event.text = Some("private chain of thought".into());
        event.data = serde_json::json!({"params":{"itemId":"reasoning"}});
        crate::turn::test_support::set_message(
            &mut event,
            "reasoning",
            crate::backend::MessageRole::Thought,
        );
        exchange.observe_turn(&event, 12).unwrap();

        assert_eq!(
            exchange.latest_reasoning_summary(),
            Some("Inspecting the repository")
        );
        exchange.turn[0]
            .finish(crate::turn::TurnOutcome::Completed, 13)
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

    #[test]
    fn provider_turn_identity_survives_steering_and_rejects_late_tools() {
        use crate::backend::{BackendEvent, ProviderAddress, TurnBoundary};
        use crate::turn::{TurnOutcome, TurnState};
        let mut exchange = interaction();
        let address = ProviderAddress {
            owner: None,
            thread_id: "parent".into(),
            turn_id: "first".into(),
        };
        let mut event = BackendEvent {
            message: None,
            received_at_ms: None,
            address: Some(address.clone()),
            turn_boundary: Some(TurnBoundary::Started),
            kind: "turn_started".into(),
            text: None,
            data: serde_json::Value::Null,
            activity: None,
            summary: None,
            checklist_update: None,
        };
        assert!(exchange.observe_turn(&event, 10).unwrap());
        event.turn_boundary = None;
        event.kind = "assistant_message".into();
        event.text = Some("Before steering".into());
        event.data = serde_json::json!({
            "params": { "item": { "id": "message-one", "phase": "commentary" } }
        });
        crate::turn::test_support::set_message(
            &mut event,
            "message-one",
            crate::backend::MessageRole::Commentary,
        );
        assert!(exchange.observe_turn(&event, 10).unwrap());
        exchange
            .append_input(
                crate::exchange::InputIntent::Steering,
                "include enterprise URLs".into(),
                11,
            )
            .unwrap();
        event.text = Some("After steering".into());
        event.data = serde_json::json!({
            "params": { "item": { "id": "message-two", "phase": "final_answer" } }
        });
        crate::turn::test_support::set_message(
            &mut event,
            "message-two",
            crate::backend::MessageRole::Response,
        );
        assert!(exchange.observe_turn(&event, 11).unwrap());
        assert_eq!(exchange.turn[0].messages().len(), 2);
        assert_eq!(
            exchange.turn[0].messages()[0].provider_id(),
            Some("message-one")
        );
        assert_eq!(
            exchange.turn[0].messages()[0].delivery(),
            crate::turn::MessageDelivery::Commentary
        );
        assert_eq!(exchange.turn[0].messages()[1].text(), "After steering");
        assert_eq!(
            exchange.turn[0].messages()[1].provider_id(),
            Some("message-two")
        );
        assert_eq!(
            exchange.turn[0].messages()[1].delivery(),
            crate::turn::MessageDelivery::Final
        );
        event.text = None;
        assert_eq!(exchange.turn.len(), 1);
        event.message = None;
        let first_id = exchange.turn[0].id().to_owned();
        event.turn_boundary = None;
        event.activity = Some(crate::backend::ToolActivity {
            id: "change".into(),
            kind: crate::backend::ToolActivityKind::FileChange,
            title: "edit".into(),
            output: None,
            output_delta: false,
            status: Some(crate::backend::ToolStatus::Succeeded),
            change: crate::backend::ProviderChangeSet {
                file: vec![crate::backend::ProviderFileChange {
                    path: "owned.rs".into(),
                    move_path: None,
                    kind: crate::backend::ProviderChangeKind::Add,
                    diff: "+owned".into(),
                }],
            },
        });
        assert!(exchange.observe_turn(&event, 12).unwrap());
        let mut changes = ProviderChangeIndex::default();
        changes.record(&exchange);
        assert!(
            changes.paths().contains("owned.rs"),
            "attribution depended on presentation segments"
        );
        assert_eq!(exchange.tools_for_turn(exchange.turn[0].id()).count(), 1);
        event.activity = None;

        event.turn_boundary = Some(TurnBoundary::Finished {
            outcome: TurnOutcome::Completed,
        });
        assert!(exchange.observe_turn(&event, 20).unwrap());
        event.turn_boundary = None;
        event.kind = "tool".into();
        assert!(!exchange.observe_turn(&event, 30).unwrap());
        assert_eq!(
            exchange.turn[0].state(),
            TurnState::Finished {
                outcome: TurnOutcome::Completed
            }
        );
        assert!(exchange.start_turn(address, 40).is_err());
        exchange
            .start_turn(
                ProviderAddress {
                    owner: None,
                    thread_id: "parent".into(),
                    turn_id: "second".into(),
                },
                50,
            )
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
                outcome: TurnOutcome::Cancelled
            }
        );
    }

    #[test]
    fn interrupted_exchange_settles_provider_turns_permanently() {
        let mut exchange = interaction();
        exchange.resume(100).unwrap();
        let address = crate::backend::ProviderAddress {
            owner: None,
            thread_id: "thread".into(),
            turn_id: "turn".into(),
        };
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
        let address = crate::backend::ProviderAddress {
            owner: None,
            thread_id: "thread".into(),
            turn_id: "turn".into(),
        };
        exchange.start_turn(address.clone(), 10).unwrap();
        exchange
            .turn_mut(&address)
            .unwrap()
            .finish(crate::turn::TurnOutcome::Completed, 30)
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
        assert_eq!(exchange.turn[0].duration_ms(), 20);
        assert!(
            exchange
                .finish(crate::exchange::ExchangeOutcome::Failed, 901)
                .is_err()
        );
    }

    #[test]
    fn finalization_settles_running_turn_without_changing_completed_turns() {
        let mut exchange = interaction();
        let first = crate::backend::ProviderAddress {
            owner: None,
            thread_id: "thread".into(),
            turn_id: "first".into(),
        };
        exchange.start_turn(first.clone(), 10).unwrap();
        exchange
            .turn_mut(&first)
            .unwrap()
            .finish(crate::turn::TurnOutcome::Completed, 20)
            .unwrap();
        exchange
            .start_turn(
                crate::backend::ProviderAddress {
                    owner: None,
                    thread_id: "thread".into(),
                    turn_id: "second".into(),
                },
                30,
            )
            .unwrap();
        exchange
            .finish(crate::exchange::ExchangeOutcome::Cancelled, 50)
            .unwrap();
        assert_eq!(exchange.turn[0].duration_ms(), 10);
        assert_eq!(exchange.turn[1].duration_ms(), 20);
        assert_eq!(
            exchange.turn[1].state(),
            crate::turn::TurnState::Finished {
                outcome: crate::turn::TurnOutcome::Cancelled
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
        let address = crate::backend::ProviderAddress {
            owner: None,
            thread_id: "thread".into(),
            turn_id: "turn".into(),
        };
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
            .finish(crate::turn::TurnOutcome::Completed, 210)
            .unwrap();

        assert_eq!(interaction.node_list.len(), 1);
        assert!(matches!(
            interaction.node_list[0],
            ExchangeNode::ArtifactChange { .. }
        ));
        assert_eq!(interaction.turn[0].duration_ms(), 200);
    }
}
