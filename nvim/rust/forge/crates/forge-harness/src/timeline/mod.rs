use crate::agent::Agent;
use crate::exchange::Exchange;
use crate::plan::{
    PlanAudit, PlanDeviation, PlanExecutionRecord, PlanFileStore, PlanLifecycleRecord, PlanRecord,
    PlanResolutionRecord,
};
use crate::session::state_machine::SessionPhase;
use anyhow::Result;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

pub(crate) mod ownership;
mod planning;
pub mod stream;
pub use ownership::{TimelineEvent, TimelineOwner};

/// Records a durable session action at its captured session or exchange boundary.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum SessionEventKind {
    TaskFinished {
        outcome: crate::task::TaskOutcome,
    },
    TaskStarted {
        start: crate::exchange::TaskStart,
        prompt: String,
    },
    /// Retains one configuration operation at a stable position throughout restart and resumption.
    Configuration {
        /// Identifies the durable task transition that owns cleanup and application.
        operation_id: String,
        /// Records application and resumption outcomes without creating model prompts.
        state: String,
        /// Names changed fields without retaining credentials or raw configuration.
        field: Vec<String>,
        /// Explains a failure or interruption when present.
        reason: Option<String>,
    },
    /// Retains an explicitly requested compaction without creating a user exchange.
    Compaction {
        compaction: Box<crate::provider::Compaction>,
    },
    Renamed {
        name: String,
    },
    Forked {
        source_session_id: String,
        source_session_name: String,
    },
}

/// Retains a session action and the timeline boundary that owns its presentation.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct SessionEventRecord {
    pub id: String,
    pub session_id: String,
    pub owner: TimelineOwner,
    pub created_at_ms: i64,
    #[serde(flatten)]
    pub detail: SessionEventKind,
}

/// Represents one fully resolved top-level Harness timeline entry.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum TimelineEntry {
    /// Retains a fork-time view without admitting its source exchanges as child execution state.
    Inherited {
        captured_at_ms: i64,
        entry: Box<TimelineEntry>,
    },
    Exchange {
        id: String,
        created_at_ms: i64,
        exchange: Box<Exchange>,
        agent_by_id: HashMap<String, TimelineEntry>,
    },
    AgentLifecycle {
        id: String,
        created_at_ms: i64,
        run: Box<Agent>,
        exchange: Vec<Exchange>,
        agent: Vec<TimelineEntry>,
    },
    /// Present provider-observed work without managed AgentTask controls or ownership.
    ObservedAgent {
        id: String,
        created_at_ms: i64,
        agent: Box<crate::agent::ObservedAgent>,
        children: Vec<TimelineEntry>,
    },
    SessionEvent {
        id: String,
        created_at_ms: i64,
        event: SessionEventRecord,
    },
    Checks {
        id: String,
        created_at_ms: i64,
        run: Box<crate::checks::CheckRun>,
    },
    #[serde(skip_deserializing)]
    Status {
        id: String,
        created_at_ms: i64,
        status: SessionPhase,
        /// Controls the status preview independently of retained thought messages.
        status_thoughts: bool,
        /// Active child executions across the root session, independent of its phase.
        agent_count: usize,
        /// Active checks independent of the main workflow phase.
        checks: Option<crate::checks::CheckActivity>,
    },
}

impl TimelineEntry {
    pub(crate) fn restore_reports(&mut self, plan_file: &PlanFileStore) {
        match self {
            Self::Inherited { entry, .. } => entry.restore_reports(plan_file),
            Self::Exchange {
                exchange,
                agent_by_id,
                ..
            } => {
                restore_exchange_reports(exchange, plan_file);
                for agent in agent_by_id.values_mut() {
                    agent.restore_reports(plan_file);
                }
            }
            Self::AgentLifecycle {
                exchange, agent, ..
            } => {
                for exchange in exchange {
                    restore_exchange_reports(exchange, plan_file);
                }
                for agent in agent {
                    agent.restore_reports(plan_file);
                }
            }
            _ => {}
        }
    }

    fn attach_event(&mut self, event: &TimelineEvent) -> Result<bool> {
        let Some(owner) = event.owner().exchange_id() else {
            return Ok(false);
        };
        let Some(exchange) = self.exchange_mut(owner) else {
            return Ok(false);
        };
        ownership::insert_event(exchange, event.clone())?;
        Ok(true)
    }

    fn exchange_mut(&mut self, id: &str) -> Option<&mut Exchange> {
        match self {
            Self::Exchange {
                exchange,
                agent_by_id,
                ..
            } => {
                if exchange.id == id {
                    return Some(exchange);
                }
                agent_by_id
                    .values_mut()
                    .find_map(|agent| agent.exchange_mut(id))
            }
            Self::AgentLifecycle {
                exchange, agent, ..
            } => {
                if let Some(exchange) = exchange.iter_mut().find(|exchange| exchange.id == id) {
                    return Some(exchange);
                }
                agent.iter_mut().find_map(|agent| agent.exchange_mut(id))
            }
            _ => None,
        }
    }

    /// Return the stable identity used by incremental timeline reconciliation.
    pub fn id(&self) -> String {
        match self {
            Self::Inherited { entry, .. } => entry.id(),
            Self::Exchange { id, .. }
            | Self::AgentLifecycle { id, .. }
            | Self::ObservedAgent { id, .. }
            | Self::SessionEvent { id, .. }
            | Self::Checks { id, .. }
            | Self::Status { id, .. } => id.clone(),
        }
    }

    /// Index each contained exchange by its retained top-level presentation owner.
    fn index_exchanges(&self, owner: usize, index: &mut HashMap<String, usize>) {
        match self {
            Self::Exchange {
                exchange,
                agent_by_id,
                ..
            } => {
                index.insert(exchange.id.clone(), owner);
                for agent in agent_by_id.values() {
                    agent.index_exchanges(owner, index);
                }
            }
            Self::AgentLifecycle {
                exchange, agent, ..
            } => {
                for exchange in exchange {
                    index.insert(exchange.id.clone(), owner);
                }
                for agent in agent {
                    agent.index_exchanges(owner, index);
                }
            }
            Self::ObservedAgent {
                agent, children, ..
            } => {
                for exchange in agent.exchanges() {
                    index.insert(exchange.id.clone(), owner);
                }
                for child in children {
                    child.index_exchanges(owner, index);
                }
            }
            _ => {}
        }
    }

    /// Replace a contained exchange without changing its planning events or agent ownership.
    fn replace_exchange(&mut self, replacement: &Exchange) -> Result<bool> {
        match self {
            Self::Exchange {
                exchange,
                agent_by_id,
                ..
            } => {
                if exchange.id == replacement.id {
                    **exchange = crate::plan::event::replacement_exchange(exchange, replacement)?;
                    Ok(true)
                } else {
                    for agent in agent_by_id.values_mut() {
                        if agent.replace_exchange(replacement)? {
                            return Ok(true);
                        }
                    }
                    Ok(false)
                }
            }
            Self::AgentLifecycle {
                exchange, agent, ..
            } => {
                if let Some(exchange) = exchange
                    .iter_mut()
                    .find(|exchange| exchange.id == replacement.id)
                {
                    *exchange = crate::plan::event::replacement_exchange(exchange, replacement)?;
                    Ok(true)
                } else {
                    for agent in agent.iter_mut() {
                        if agent.replace_exchange(replacement)? {
                            return Ok(true);
                        }
                    }
                    Ok(false)
                }
            }
            Self::ObservedAgent {
                agent, children, ..
            } => {
                if agent.replace_exchange(replacement)? {
                    return Ok(true);
                }
                for child in children {
                    if child.replace_exchange(replacement)? {
                        return Ok(true);
                    }
                }
                Ok(false)
            }
            _ => Ok(false),
        }
    }

    /// Remove a nested exchange while retaining its agent container.
    fn remove_exchange(&mut self, id: &str) {
        match self {
            Self::Exchange { agent_by_id, .. } => {
                for agent in agent_by_id.values_mut() {
                    agent.remove_exchange(id);
                }
            }
            Self::AgentLifecycle {
                exchange, agent, ..
            } => {
                exchange.retain(|exchange| exchange.id != id);
                for agent in agent {
                    agent.remove_exchange(id);
                }
            }
            _ => {}
        }
    }

    fn created_at_ms(&self) -> i64 {
        match self {
            Self::Inherited { entry, .. } => entry.created_at_ms(),
            Self::Exchange { created_at_ms, .. }
            | Self::AgentLifecycle { created_at_ms, .. }
            | Self::ObservedAgent { created_at_ms, .. }
            | Self::SessionEvent { created_at_ms, .. }
            | Self::Checks { created_at_ms, .. }
            | Self::Status { created_at_ms, .. } => *created_at_ms,
        }
    }

    fn order_key(&self) -> (bool, i64, String) {
        (
            matches!(self, Self::Status { .. }),
            self.created_at_ms(),
            self.id(),
        )
    }
}

/// Build one native child subtree without rebinding unresolved ancestors to the current root.
fn observed_entry(
    index: usize,
    agents: &[crate::agent::ObservedAgent],
    parent: &[Option<usize>],
    visited: &mut std::collections::HashSet<usize>,
) -> Result<TimelineEntry> {
    anyhow::ensure!(
        visited.insert(index),
        "native child hierarchy contains a cycle"
    );
    let mut children = Vec::new();
    for child in 0..agents.len() {
        if parent[child] == Some(index) {
            children.push(observed_entry(child, agents, parent, visited)?);
        }
    }
    let agent = &agents[index];
    Ok(TimelineEntry::ObservedAgent {
        id: agent.storage_id(),
        created_at_ms: agent.created_at_ms(),
        agent: Box::new(agent.clone()),
        children,
    })
}

/// Resolves durable records into one ordered session presentation.
pub struct TimelineProjector;

/// Owns one complete set of durable inputs for canonical timeline projection.
pub struct TimelineProjection<'a> {
    pub interaction_list: Vec<Exchange>,
    pub plan_list: &'a [PlanRecord],
    pub lifecycle_list: Vec<PlanLifecycleRecord>,
    pub execution_list: Vec<PlanExecutionRecord>,
    pub deviation_list: Vec<PlanDeviation>,
    pub audit_list: Vec<PlanAudit>,
    pub resolution_list: Vec<PlanResolutionRecord>,
    pub agent_run_list: Vec<Agent>,
    pub agent_exchange_list: Vec<Exchange>,
    pub session_event_list: Vec<SessionEventRecord>,
    pub plan_file: &'a PlanFileStore,
}

impl TimelineProjector {
    /// Attach native observed children by their original generation and trusted ancestry.
    pub(crate) fn with_observed_agents(
        mut timeline: Vec<TimelineEntry>,
        observed: Vec<crate::agent::ObservedAgent>,
    ) -> Result<Vec<TimelineEntry>> {
        timeline.retain(|entry| !matches!(entry, TimelineEntry::ObservedAgent { .. }));
        let parent = crate::agent::ObservedAgent::hierarchy(&observed)?;
        let mut visited = std::collections::HashSet::new();
        for index in 0..observed.len() {
            if parent[index].is_none() {
                timeline.push(observed_entry(index, &observed, &parent, &mut visited)?);
            }
        }
        anyhow::ensure!(
            visited.len() == observed.len(),
            "native child hierarchy contains a cycle"
        );
        timeline.sort_by_key(TimelineEntry::order_key);
        Ok(timeline)
    }

    /// Merges durable check groups with plan-owned groups while preserving one identity per run.
    pub(crate) fn with_checks(
        mut timeline: Vec<TimelineEntry>,
        runs: Vec<crate::checks::CheckRun>,
    ) -> Result<Vec<TimelineEntry>> {
        for run in runs
            .into_iter()
            .filter(|run| run.state != crate::checks::CheckState::Running)
        {
            if run.owner.exchange_id().is_some() {
                let event = TimelineEvent::Check { run: Box::new(run) };
                let mut attached = false;
                for entry in &mut timeline {
                    if entry.attach_event(&event)? {
                        attached = true;
                        break;
                    }
                }
                anyhow::ensure!(
                    attached,
                    "check event owner is absent from the timeline: {}",
                    event.id()
                );
                continue;
            }
            if !timeline.iter().any(|entry| entry.id() == run.id) {
                timeline.push(TimelineEntry::Checks {
                    id: run.id.clone(),

                    created_at_ms: run.started_at_ms,
                    run: run.into(),
                });
            }
        }
        timeline.sort_by_key(TimelineEntry::order_key);
        Ok(timeline)
    }

    pub(crate) fn with_history<'exchange>(
        timeline: Vec<TimelineEntry>,
        runs: Vec<crate::checks::CheckRun>,
        exchanges: impl IntoIterator<Item = &'exchange Exchange>,
    ) -> Result<Vec<TimelineEntry>> {
        let retired: std::collections::HashSet<_> = exchanges
            .into_iter()
            .filter(|exchange| {
                exchange.disposition() != crate::exchange::HistoryDisposition::Current
            })
            .map(|exchange| exchange.id.as_str())
            .collect();
        Self::with_checks(
            timeline,
            runs.into_iter()
                .filter(|run| {
                    run.owner
                        .exchange_id()
                        .is_none_or(|owner| !retired.contains(owner))
                })
                .collect(),
        )
    }
    /// Build one stable session timeline from interactions and plan lifecycle records.
    pub fn build(projection: TimelineProjection<'_>) -> Result<Vec<TimelineEntry>> {
        let TimelineProjection {
            interaction_list,
            plan_list,
            lifecycle_list,
            execution_list,
            deviation_list,
            audit_list,
            resolution_list,
            agent_run_list,
            agent_exchange_list,
            session_event_list,
            plan_file,
        } = projection;
        let retired: std::collections::HashSet<_> = interaction_list
            .iter()
            .chain(&agent_exchange_list)
            .filter(|exchange| {
                exchange.disposition() != crate::exchange::HistoryDisposition::Current
            })
            .map(|exchange| exchange.id.clone())
            .collect();
        let mut interaction_list = interaction_list;
        for exchange in &mut interaction_list {
            restore_exchange_reports(exchange, plan_file);
        }

        let checks = execution_list
            .iter()
            .flat_map(|execution| execution.check_runs.clone())
            .collect::<Vec<_>>();
        planning::attach(
            &mut interaction_list,
            plan_list,
            lifecycle_list,
            execution_list,
            resolution_list,
            &deviation_list,
            &audit_list,
        );
        let mut result = interaction_list
            .into_iter()
            .map(|exchange| TimelineEntry::Exchange {
                id: exchange.id.clone(),

                created_at_ms: exchange.created_at_ms,
                exchange: exchange.into(),

                agent_by_id: HashMap::new(),
            })
            .collect::<Vec<_>>();
        project_agent_tree(&mut result, agent_run_list, agent_exchange_list);
        for event in session_event_list {
            if event
                .owner
                .exchange_id()
                .is_some_and(|owner| retired.contains(owner))
            {
                continue;
            }
            if matches!(&event.detail, SessionEventKind::Configuration { state, .. }
                if !matches!(state.as_str(), "failed" | "cancelled" | "interrupted"))
            {
                continue;
            }
            if matches!(&event.detail, SessionEventKind::Renamed { .. }) {
                continue;
            }
            if matches!(&event.detail, SessionEventKind::Compaction { compaction }
                if compaction.state == crate::provider::CompactionState::Running)
            {
                continue;
            }
            if event.owner.exchange_id().is_some() {
                let event = TimelineEvent::Session {
                    event: Box::new(event),
                };
                let mut attached = false;
                for entry in &mut result {
                    if entry.attach_event(&event)? {
                        attached = true;
                        break;
                    }
                }
                anyhow::ensure!(
                    attached,
                    "session event owner is absent from the timeline: {}",
                    event.id()
                );
                continue;
            }
            result.push(TimelineEntry::SessionEvent {
                id: event.id.clone(),
                created_at_ms: event.created_at_ms,
                event,
            });
        }
        let mut result = Self::with_checks(
            result,
            checks
                .into_iter()
                .filter(|run| {
                    run.owner
                        .exchange_id()
                        .is_none_or(|owner| !retired.contains(owner))
                })
                .collect(),
        )?;
        result.retain(|entry| {
            !matches!(entry, TimelineEntry::Exchange { exchange, .. }
            if exchange.disposition() != crate::exchange::HistoryDisposition::Current)
        });
        result.sort_by_key(TimelineEntry::order_key);
        Ok(result)
    }
}

fn restore_exchange_reports(exchange: &mut Exchange, plan_file: &PlanFileStore) {
    for node in &mut exchange.node_list {
        if let crate::exchange::ExchangeNode::ImplementationReport {
            reference, report, ..
        } = node
        {
            // The reference remains renderable if historical report storage is unavailable.
            *report = plan_file.implementation_report(reference).ok();
        }
    }
}

fn project_agent_tree(
    result: &mut Vec<TimelineEntry>,
    agent_run_list: Vec<Agent>,
    agent_exchange_list: Vec<Exchange>,
) {
    let run_by_id = agent_run_list
        .iter()
        .map(|run| (run.id.clone(), run.clone()))
        .collect::<HashMap<_, _>>();
    let mut interaction_by_run_id = HashMap::<String, Vec<Exchange>>::new();
    for turn in agent_exchange_list {
        interaction_by_run_id
            .entry(turn.agent_id.clone())
            .or_default()
            .push(turn);
    }
    let delegated: std::collections::HashSet<_> = interaction_by_run_id
        .values()
        .flatten()
        .flat_map(|exchange| &exchange.node_list)
        .filter_map(|node| match node {
            crate::exchange::ExchangeNode::AgentReference { agent } => {
                Some(agent.child_agent_id.clone())
            }
            _ => None,
        })
        .collect();
    let primary_child: std::collections::HashSet<_> = result
        .iter()
        .filter_map(|entry| match entry {
            TimelineEntry::Exchange { exchange, .. } => Some(exchange),
            _ => None,
        })
        .flat_map(|exchange| &exchange.node_list)
        .filter_map(|node| match node {
            crate::exchange::ExchangeNode::AgentReference { agent } => {
                Some(agent.child_agent_id.clone())
            }
            _ => None,
        })
        .collect();
    let root_id_list: Vec<_> = agent_run_list
        .iter()
        .filter(|agent| primary_child.contains(&agent.id) || !delegated.contains(&agent.id))
        .map(|agent| agent.id.clone())
        .collect();

    for run_id in root_id_list {
        let entry = build_agent_entry(
            &run_id,
            &run_by_id,
            &interaction_by_run_id,
            None,
            &mut std::collections::HashSet::new(),
        );
        let mut attached = false;
        for timeline_entry in result.iter_mut() {
            if let TimelineEntry::Exchange {
                exchange: interaction,
                agent_by_id,
                ..
            } = timeline_entry
                && interaction.node_list.iter().any(|node| matches!(node,
                    crate::exchange::ExchangeNode::AgentReference { agent } if agent.child_agent_id == run_id)) {
                    agent_by_id.insert(run_id.clone(), entry.clone());
                    attached = true;
                }
        }
        if !attached {
            result.push(entry);
        }
    }
}

/// Project delegation edges by exchange identity rather than agent lifetime.
fn build_agent_entry(
    run_id: &str,
    run_by_id: &HashMap<String, Agent>,
    interaction_by_run_id: &HashMap<String, Vec<Exchange>>,
    exchange_id: Option<&str>,
    visiting: &mut std::collections::HashSet<String>,
) -> TimelineEntry {
    let run = run_by_id
        .get(run_id)
        .expect("agent tree references a known agent")
        .clone();
    let interaction: Vec<_> = interaction_by_run_id
        .get(run_id)
        .into_iter()
        .flatten()
        .filter(|exchange| {
            exchange.disposition() == crate::exchange::HistoryDisposition::Current
                && exchange_id.is_none_or(|id| exchange.id == id)
        })
        .cloned()
        .collect();
    let mut agent = Vec::new();
    for exchange in &interaction {
        if !visiting.insert(exchange.id.clone()) {
            continue;
        }
        for node in &exchange.node_list {
            let crate::exchange::ExchangeNode::AgentReference { agent: delegation } = node else {
                continue;
            };
            if visiting.contains(&delegation.child_exchange_id)
                || !run_by_id.contains_key(&delegation.child_agent_id)
            {
                continue;
            }
            let mut child = build_agent_entry(
                &delegation.child_agent_id,
                run_by_id,
                interaction_by_run_id,
                Some(&delegation.child_exchange_id),
                visiting,
            );
            if let TimelineEntry::AgentLifecycle { id, .. } = &mut child {
                *id = delegation.id.clone();
            }
            agent.push(child);
        }
        visiting.remove(&exchange.id);
    }
    TimelineEntry::AgentLifecycle {
        id: run.id.clone(),

        created_at_ms: run.created_at_ms,

        exchange: interaction,
        run: run.into(),

        agent,
    }
}
