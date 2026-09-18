use super::*;
use crate::agent::Delegation;
use crate::plan::PlanElicitation;
use serde::{Deserialize, Serialize};

/// Defines the timeline role of one admitted interaction.
#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum ExchangeKind {
    #[default]
    Chat,
    PlanDraft,
    PlanRevision,
    PlanExecution,
}

/// Represents one admitted user action and every backend turn it caused.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Exchange {
    /// Durable failure that keeps cleanup retryable without replaying execution.
    #[serde(default)]
    pub finalization_error: Option<String>,
    /// Terminal outcome retained while checkpoint persistence remains retryable.
    #[serde(default)]
    pub finalization_outcome: Option<ExchangeState>,
    /// Persistent agent identity that owns this exchange.
    pub agent_id: String,
    pub id: String,
    pub session_id: String,
    pub ordinal: u64,
    pub prompt: String,
    #[serde(default)]
    pub kind: ExchangeKind,
    #[serde(default)]
    pub plan_id: Option<String>,
    #[serde(default)]
    pub execution_id: Option<String>,
    #[serde(default)]
    /// Goal whose lifecycle caused this exchange.
    pub goal_id: Option<String>,
    pub state: ExchangeState,
    #[serde(default)]
    /// Position of the exchange in the current workspace history.
    pub disposition: HistoryDisposition,
    pub checkpoint_before: Option<String>,
    pub checkpoint_after: Option<String>,
    pub attributed_diff_text: Option<String>,
    pub checkpoint_diff_text: Option<String>,
    pub attributed_matches_checkpoint: bool,
    pub created_at_ms: i64,
    pub completed_at_ms: Option<i64>,
    pub node_list: Vec<ExchangeNode>,
    #[serde(default)]
    /// Provider executions owned by this request, independently of UI grouping.
    pub turn: Vec<crate::turn::Turn>,
    #[serde(default)]
    pub awaiting_input: bool,
    #[serde(default)]
    pub elicitation: Option<PlanElicitation>,
    #[serde(default)]
    pub duration_ms: u64,
    /// Start of the current execution interval, excluding time awaiting user input.
    #[serde(default)]
    pub execution_started_at_ms: Option<i64>,
    #[serde(default)]
    pub token_count: Option<u64>,
    #[serde(default)]
    pub comment: Vec<ExchangeComment>,
    #[serde(default)]
    pub task: Option<TaskSnapshot>,
}

impl Exchange {
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
            kind: ExchangeKind::Chat,
            plan_id: None,
            execution_id: None,
            goal_id: None,
            state: ExchangeState::Queued,
            disposition: HistoryDisposition::Current,
            finalization_error: None,
            finalization_outcome: None,
            checkpoint_before: None,
            checkpoint_after: None,
            attributed_diff_text: None,
            checkpoint_diff_text: None,
            attributed_matches_checkpoint: false,
            created_at_ms: delegation.created_at_ms,
            completed_at_ms: None,
            node_list: Vec::new(),
            turn: Vec::new(),
            awaiting_input: false,
            elicitation: None,
            duration_ms: 0,
            execution_started_at_ms: None,
            token_count: None,
            comment: Vec::new(),
            task: None,
        }
    }

    /// Freeze execution before cleanup and checkpoint persistence begin.
    pub fn begin_finalization(
        &mut self,
        outcome: ExchangeState,
        now_ms: i64,
    ) -> anyhow::Result<()> {
        anyhow::ensure!(
            matches!(
                outcome,
                ExchangeState::Complete
                    | ExchangeState::Failed
                    | ExchangeState::Cancelled
                    | ExchangeState::Interrupted
            ),
            "finalization requires a terminal outcome"
        );
        anyhow::ensure!(
            self.finalization_outcome
                .is_none_or(|previous| previous == outcome),
            "finalization cannot change the recorded outcome"
        );
        anyhow::ensure!(
            matches!(
                self.state,
                ExchangeState::Running | ExchangeState::Finalizing
            ) && self.completed_at_ms.is_none(),
            "only an open exchange can finalize"
        );
        self.pause(now_ms);
        self.awaiting_input = false;
        self.state = ExchangeState::Finalizing;
        self.finalization_outcome = Some(outcome);
        Ok(())
    }

    /// Route provider lifecycle and tool provenance before accepting presentation content.
    pub(crate) fn observe_turn(
        &mut self,
        event: &crate::backend::BackendEvent,
        now_ms: i64,
    ) -> anyhow::Result<bool> {
        use crate::backend::TurnBoundary;
        use crate::turn::TurnState;
        let Some(address) = &event.address else {
            anyhow::ensure!(
                event.turn_boundary.is_none(),
                "provider turn boundary has no execution address"
            );
            return Ok(true);
        };
        match event.turn_boundary {
            Some(TurnBoundary::Started) => {
                self.start_turn(address.clone(), now_ms)?;
            }
            Some(TurnBoundary::Finished { outcome }) => {
                let Some(turn) = self.turn.iter_mut().find(|turn| turn.provider() == address)
                else {
                    return Ok(false);
                };
                turn.finish(outcome, now_ms)?;
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
                if let Some(tool) = &event.activity {
                    turn.record_tool(tool)?;
                }
                if let Some(text) = event.text.as_deref() {
                    let kind = match event.kind.as_str() {
                        "assistant_message" => Some(crate::turn::MessageKind::Assistant),
                        "reasoning" => Some(crate::turn::MessageKind::Reasoning),
                        "reasoning_summary" => Some(crate::turn::MessageKind::ReasoningSummary),
                        _ => None,
                    };
                    if let Some(kind) = kind {
                        let provider_id = event.message_id().map(str::to_owned);
                        let delivery = backend_message_delivery(event.message_phase(), kind);
                        turn.record_provider_text(
                            kind,
                            text,
                            provider_id,
                            delivery,
                            event.message_snapshot(),
                        )?;
                    }
                }
                let turn_id = turn.id().to_owned();
                for item in turn.items()[item_count..].to_vec() {
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
            }
        }
        Ok(true)
    }
    /// Record provider execution, including late start evidence received during cleanup.
    pub fn start_turn(
        &mut self,
        address: crate::backend::ProviderAddress,
        now_ms: i64,
    ) -> anyhow::Result<&mut crate::turn::Turn> {
        anyhow::ensure!(
            matches!(
                self.state,
                ExchangeState::Running | ExchangeState::Finalizing
            ) && self.completed_at_ms.is_none()
                && self.disposition == HistoryDisposition::Current,
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
            return Ok(&mut self.turn[index]);
        }
        anyhow::ensure!(
            self.turn
                .iter()
                .all(|turn| turn.state() != crate::turn::TurnState::Running),
            "exchange already has a running provider turn"
        );
        let id = format!("{}:turn:{}", self.id, self.turn.len() + 1);
        self.turn.push(crate::turn::Turn::new(id, address, now_ms));
        Ok(self.turn.last_mut().expect("admitted provider turn"))
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
            self.state == ExchangeState::Running && self.completed_at_ms.is_none(),
            "completed exchange cannot resume"
        );
        self.awaiting_input = self.elicitation.is_some();
        self.execution_started_at_ms.get_or_insert(now_ms);
        Ok(())
    }

    /// Settle interrupted provider turns while retaining the request and its original checkpoint.
    pub(crate) fn pause_for_restart(&mut self, now_ms: i64) -> anyhow::Result<()> {
        anyhow::ensure!(
            self.state == ExchangeState::Running && self.completed_at_ms.is_none(),
            "only an open exchange can pause for restart"
        );
        for turn in &mut self.turn {
            if turn.state() == crate::turn::TurnState::Running {
                turn.finish(crate::turn::TurnOutcome::Interrupted, now_ms)?;
            }
        }
        self.pause(now_ms);
        Ok(())
    }

    /// Return accumulated execution time without allowing terminal history to tick.
    pub fn elapsed(&self, now_ms: i64) -> u64 {
        let active = if self.completed_at_ms.is_none() {
            self.execution_started_at_ms
                .map_or(0, |start| now_ms.saturating_sub(start).max(0) as u64)
        } else {
            0
        };
        self.duration_ms.saturating_add(active)
    }

    /// Freeze the active interval before waiting for user input or finalization.
    pub fn pause(&mut self, now_ms: i64) {
        if let Some(start) = self.execution_started_at_ms.take() {
            self.duration_ms = self
                .duration_ms
                .saturating_add(now_ms.saturating_sub(start).max(0) as u64);
        }
    }

    /// Mark restored history without erasing the original execution outcome.
    pub fn set_disposition(&mut self, disposition: HistoryDisposition) -> anyhow::Result<()> {
        anyhow::ensure!(
            self.completed_at_ms.is_some(),
            "running exchange cannot leave current history"
        );
        self.disposition = disposition;
        Ok(())
    }

    /// Settle every execution segment before publishing the terminal outcome.
    pub fn finish(&mut self, outcome: ExchangeState, now_ms: i64) -> anyhow::Result<()> {
        anyhow::ensure!(
            matches!(
                outcome,
                ExchangeState::Complete
                    | ExchangeState::Failed
                    | ExchangeState::Cancelled
                    | ExchangeState::Interrupted
            ),
            "exchange finish requires an execution outcome"
        );
        if self.completed_at_ms.is_some() {
            anyhow::ensure!(
                self.state == outcome,
                "completed exchange outcome cannot change"
            );
            return Ok(());
        }
        for turn in &mut self.turn {
            if turn.state() == crate::turn::TurnState::Running {
                anyhow::ensure!(
                    outcome != ExchangeState::Complete,
                    "exchange completion requires settled provider turns"
                );
                turn.finish(
                    match outcome {
                        ExchangeState::Cancelled => crate::turn::TurnOutcome::Cancelled,
                        ExchangeState::Interrupted => crate::turn::TurnOutcome::Interrupted,
                        _ => crate::turn::TurnOutcome::Failed,
                    },
                    now_ms,
                )?;
            }
        }
        self.pause(now_ms);
        self.state = outcome;
        self.completed_at_ms = Some(now_ms);
        self.awaiting_input = false;
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
                self.state,
                ExchangeState::Running | ExchangeState::Finalizing
            ) && self.completed_at_ms.is_none()
                && self.disposition == HistoryDisposition::Current,
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
        anyhow::ensure!(
            self.state == ExchangeState::Running
                && self.completed_at_ms.is_none()
                && self.disposition == HistoryDisposition::Current,
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
            intent,
            id: format!("{}:input:{prompt_ordinal}", self.id),
            text,
            created_at_ms: now_ms,
        };
        self.node_list.push(ExchangeNode::ExchangeInput {
            prompt: prompt.clone(),
        });
        Ok(prompt)
    }

    /// Attribute provider usage to the most recently admitted turn.
    pub fn record_latest_turn_usage(&mut self, token_count: Option<u64>) {
        if let Some(turn) = self.turn.last_mut() {
            turn.set_token_count(token_count);
        }
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

fn backend_message_delivery(
    phase: Option<&str>,
    kind: crate::turn::MessageKind,
) -> crate::turn::MessageDelivery {
    if phase.is_some_and(|phase| phase.eq_ignore_ascii_case("commentary")) {
        crate::turn::MessageDelivery::Commentary
    } else if phase.is_some_and(|phase| {
        phase.eq_ignore_ascii_case("final") || phase.eq_ignore_ascii_case("final_answer")
    }) || kind == crate::turn::MessageKind::Assistant
    {
        crate::turn::MessageDelivery::Final
    } else {
        crate::turn::MessageDelivery::Commentary
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

    fn interaction() -> Exchange {
        Exchange {
            finalization_error: None,
            finalization_outcome: None,
            agent_id: "primary".into(),
            id: "interaction-one".into(),
            session_id: "session-one".into(),
            ordinal: 1,
            prompt: "inspect the repository".into(),
            kind: ExchangeKind::Chat,
            plan_id: None,
            execution_id: None,
            goal_id: None,
            state: ExchangeState::Running,
            checkpoint_before: None,
            checkpoint_after: None,
            attributed_diff_text: None,
            checkpoint_diff_text: None,
            attributed_matches_checkpoint: false,
            disposition: crate::exchange::HistoryDisposition::Current,
            turn: Vec::new(),
            created_at_ms: 1,
            completed_at_ms: None,
            node_list: Vec::new(),
            awaiting_input: false,
            elicitation: None,
            duration_ms: 0,
            execution_started_at_ms: None,
            token_count: None,
            comment: Vec::new(),
            task: None,
        }
    }

    #[test]
    fn message_snapshots_reconcile_without_duplicating_or_reordering_content() {
        use crate::backend::{BackendEvent, ProviderAddress};
        let mut exchange = interaction();
        let address = ProviderAddress {
            thread_id: "parent".into(),
            turn_id: "turn".into(),
        };
        exchange.start_turn(address.clone(), 10).unwrap();
        let mut event = BackendEvent {
            address: Some(address),
            turn_boundary: None,
            kind: "assistant_message".into(),
            text: Some("Checking".into()),
            data: serde_json::json!({"params":{"itemId":"one"}}),
            activity: None,
            summary: None,
            task_update: None,
        };
        exchange.observe_turn(&event, 11).unwrap();
        exchange
            .append_input(InputIntent::Steering, "Also tests".into(), 12)
            .unwrap();
        event.text = Some("Done".into());
        event.data = serde_json::json!({"params":{"itemId":"two","phase":"final_answer"}});
        exchange.observe_turn(&event, 13).unwrap();
        event.text = Some("Checking files.".into());
        event.data = serde_json::json!({"message_update":"snapshot", "params":{
            "item":{"id":"one","phase":"commentary"}
        }});
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
        assert!(!exchange.observe_turn(&event, 17).unwrap());
        assert_eq!(exchange.turn[0].messages()[0].text(), "Checking files.");
    }

    #[test]
    fn active_turn_exposes_only_its_latest_readable_reasoning_summary() {
        use crate::backend::{BackendEvent, ProviderAddress};
        let mut exchange = interaction();
        let address = ProviderAddress {
            thread_id: "parent".into(),
            turn_id: "turn".into(),
        };
        exchange.start_turn(address.clone(), 10).unwrap();
        let mut event = BackendEvent {
            address: Some(address),
            turn_boundary: None,
            kind: "reasoning_summary".into(),
            text: Some("Inspecting the repository".into()),
            data: serde_json::json!({"provider_message_id":"reasoning:summary:0"}),
            activity: None,
            summary: None,
            task_update: None,
        };
        exchange.observe_turn(&event, 11).unwrap();
        event.kind = "reasoning".into();
        event.text = Some("private chain of thought".into());
        event.data = serde_json::json!({"params":{"itemId":"reasoning"}});
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
                ExchangeNode::TurnContent { .. } => "content",
                ExchangeNode::AgentReference { .. } => "agent",
                ExchangeNode::ExchangeInput { .. } => "steering",
                ExchangeNode::PlanCommentResolution { .. } => "resolution",
                ExchangeNode::ArtifactChange { .. } => "artifact",
                ExchangeNode::PlanEvent { .. } => "plan",
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
            thread_id: "parent".into(),
            turn_id: "first".into(),
        };
        let mut event = BackendEvent {
            address: Some(address.clone()),
            turn_boundary: Some(TurnBoundary::Started),
            kind: "turn_started".into(),
            text: None,
            data: serde_json::Value::Null,
            activity: None,
            summary: None,
            task_update: None,
        };
        assert!(exchange.observe_turn(&event, 10).unwrap());
        event.turn_boundary = None;
        event.kind = "assistant_message".into();
        event.text = Some("Before steering".into());
        event.data = serde_json::json!({
            "params": { "item": { "id": "message-one", "phase": "commentary" } }
        });
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
        let first_id = exchange.turn[0].id().to_owned();
        event.turn_boundary = None;
        event.activity = Some(crate::backend::ToolActivity {
            id: "change".into(),
            kind: crate::backend::ToolActivityKind::FileChange,
            title: "edit".into(),
            output: None,
            output_delta: false,
            status: Some("completed".into()),
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
        assert_eq!(exchange.turn[0].tools().count(), 1);
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
                    thread_id: "parent".into(),
                    turn_id: "second".into(),
                },
                50,
            )
            .unwrap();
        assert_eq!(exchange.turn.len(), 2);
        assert_ne!(exchange.turn[1].id(), first_id);
        assert!(exchange.finish(ExchangeState::Complete, 60).is_err());
        exchange.finish(ExchangeState::Cancelled, 60).unwrap();
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
            thread_id: "thread".into(),
            turn_id: "turn".into(),
        };
        exchange.start_turn(address, 100).unwrap();
        exchange.finish(ExchangeState::Interrupted, 150).unwrap();
        assert_eq!(exchange.elapsed(90_000), 50);
        assert_eq!(
            exchange.turn[0].state(),
            crate::turn::TurnState::Finished {
                outcome: crate::turn::TurnOutcome::Interrupted,
            }
        );
        assert!(exchange.resume(90_000).is_err());
        exchange.finish(ExchangeState::Interrupted, 90_000).unwrap();
        assert_eq!(exchange.completed_at_ms, Some(150));
        assert!(exchange.finish(ExchangeState::Cancelled, 90_000).is_err());
    }

    #[test]
    fn terminal_exchanges_reject_every_input_intent_without_mutation() {
        for outcome in [
            ExchangeState::Complete,
            ExchangeState::Cancelled,
            ExchangeState::Failed,
            ExchangeState::Interrupted,
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
            .begin_finalization(ExchangeState::Complete, 30)
            .unwrap();
        exchange
            .begin_finalization(ExchangeState::Complete, 90)
            .unwrap();
        assert!(
            exchange
                .begin_finalization(ExchangeState::Cancelled, 90)
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
        exchange.finish(ExchangeState::Complete, 1000).unwrap();
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
            thread_id: "thread".into(),
            turn_id: "turn".into(),
        };
        exchange.start_turn(address.clone(), 10).unwrap();
        exchange
            .turn_mut(&address)
            .unwrap()
            .finish(crate::turn::TurnOutcome::Completed, 30)
            .unwrap();
        exchange.finish(ExchangeState::Complete, 30).unwrap();
        assert!(exchange.resume(900).is_err());
        assert!(exchange.start_turn(address, 900).is_err());
        exchange.finish(ExchangeState::Complete, 900).unwrap();
        assert_eq!(exchange.completed_at_ms, Some(30));
        assert_eq!(exchange.turn[0].duration_ms(), 20);
        assert!(exchange.finish(ExchangeState::Failed, 901).is_err());
    }

    #[test]
    fn finalization_settles_running_turn_without_changing_completed_turns() {
        let mut exchange = interaction();
        let first = crate::backend::ProviderAddress {
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
                    thread_id: "thread".into(),
                    turn_id: "second".into(),
                },
                30,
            )
            .unwrap();
        exchange.finish(ExchangeState::Cancelled, 50).unwrap();
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
    fn elapsed_time_excludes_user_wait_and_freezes_at_completion() {
        let mut exchange = interaction();
        exchange.resume(100).unwrap();
        assert_eq!(exchange.elapsed(250), 150);
        exchange.pause(300);
        exchange.awaiting_input = true;
        assert_eq!(exchange.elapsed(10_000), 200);
        exchange.resume(10_000).unwrap();
        assert_eq!(exchange.elapsed(10_050), 250);
        exchange.finish(ExchangeState::Complete, 10_100).unwrap();
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
        exchange.finish(ExchangeState::Failed, 30).unwrap();
        exchange
            .set_disposition(HistoryDisposition::RolledBack)
            .unwrap();
        assert_eq!(exchange.state, ExchangeState::Failed);
        assert_eq!(exchange.completed_at_ms, Some(30));
        assert!(exchange.resume(40).is_err());
    }

    #[test]
    fn plan_artifact_does_not_affect_provider_turn_lifecycle() {
        let mut interaction = interaction();
        let address = crate::backend::ProviderAddress {
            thread_id: "thread".into(),
            turn_id: "turn".into(),
        };
        interaction.start_turn(address.clone(), 10).unwrap();
        interaction.node_list.push(ExchangeNode::ArtifactChange {
            change: ArtifactChange {
                id: "artifact-one".into(),
                path: "plan.md".into(),
                diff_text: "plan".into(),
                created_at_ms: 20,
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
