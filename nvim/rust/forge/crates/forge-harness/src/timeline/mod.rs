use crate::agent::Agent;
use crate::exchange::Exchange;
use crate::plan::{
    PlanAudit, PlanDeviation, PlanExecutionRecord, PlanFileStore, PlanLifecycleRecord, PlanRecord,
    PlanResolutionRecord,
};
use crate::session::state_machine::SessionPhase;
use anyhow::Result;
use serde::{Deserialize, Serialize};
use std::collections::{HashMap};

pub mod stream;
mod planning;

/// Represents one durable session action projected outside model interactions.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum SessionEventKind {
    Renamed {
        name: String,
    },
    Forked {
        source_session_id: String,
        source_session_name: String,
    },
}

/// Represents one durable session-level action shown outside model interactions.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct SessionEventRecord {
    pub id: String,
    pub session_id: String,
    pub created_at_ms: i64,
    #[serde(flatten)]
    pub detail: SessionEventKind,
}

/// Represents one fully resolved top-level Harness timeline entry.
#[derive(Clone, Debug, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum TimelineEntry {
    Exchange {
        id: String,
        created_at_ms: i64,
        exchange: Exchange,
        agent_by_id: HashMap<String, TimelineEntry>,
    },
    AgentLifecycle {
        id: String,
        created_at_ms: i64,
        run: Agent,
        exchange: Vec<Exchange>,
        agent: Vec<TimelineEntry>,
    },
    SessionEvent {
        id: String,
        created_at_ms: i64,
        event: SessionEventRecord,
    },
    Status {
        id: String,
        created_at_ms: i64,
        status: SessionPhase,
    },
}

impl TimelineEntry {
    /// Return the stable identity used by incremental timeline reconciliation.
    pub fn id(&self) -> String {
        match self {
            Self::Exchange { id, .. }
            | Self::AgentLifecycle { id, .. }
            | Self::SessionEvent { id, .. }
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
            _ => {}
        }
    }

    /// Replace a contained exchange without changing its planning events or agent ownership.
    fn replace_exchange(&mut self, replacement: &Exchange) -> bool {
        match self {
            Self::Exchange {
                exchange,
                agent_by_id,
                ..
            } => {
                if exchange.id == replacement.id {
                    crate::plan::event::replace_exchange(exchange, replacement);
                    true
                } else {
                    agent_by_id
                        .values_mut()
                        .any(|agent| agent.replace_exchange(replacement))
                }
            }
            Self::AgentLifecycle {
                exchange, agent, ..
            } => {
                if let Some(exchange) = exchange
                    .iter_mut()
                    .find(|exchange| exchange.id == replacement.id)
                {
                    crate::plan::event::replace_exchange(exchange, replacement);
                    true
                } else {
                    agent
                        .iter_mut()
                        .any(|agent| agent.replace_exchange(replacement))
                }
            }
            _ => false,
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
            Self::Exchange { created_at_ms, .. }
            | Self::AgentLifecycle { created_at_ms, .. }
            | Self::SessionEvent { created_at_ms, .. }
            | Self::Status { created_at_ms, .. } => *created_at_ms,
        }
    }
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
            plan_file: _,
        } = projection;
        let mut interaction_list = interaction_list;
        planning::attach(&mut interaction_list, plan_list, lifecycle_list, execution_list,
            resolution_list, &deviation_list, &audit_list);
        let mut result = interaction_list.into_iter().map(|exchange| TimelineEntry::Exchange {
            id: exchange.id.clone(), created_at_ms: exchange.created_at_ms,
            exchange, agent_by_id: HashMap::new(),
        }).collect::<Vec<_>>();
        project_agent_tree(&mut result, agent_run_list, agent_exchange_list);
        for event in session_event_list {
            result.push(TimelineEntry::SessionEvent {
                id: event.id.clone(),
                created_at_ms: event.created_at_ms,
                event,
            });
        }
        result.sort_by_key(TimelineEntry::created_at_ms);
        Ok(result)
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
            {
                if interaction.node_list.iter().any(|node| matches!(node,
                    crate::exchange::ExchangeNode::AgentReference { agent } if agent.child_agent_id == run_id)) {
                    agent_by_id.insert(run_id.clone(), entry.clone());
                    attached = true;
                }
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
        .filter(|exchange| exchange_id.is_none_or(|id| exchange.id == id))
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
        run,
        agent,
    }
}

#[cfg(test)]
mod test {
    use super::{
        TimelineEntry, TimelineProjection, TimelineProjector,
    };
    use crate::{
        agent::{Agent, AgentState},
        exchange::{Exchange, ExchangeKind, ExchangeState},
        plan::{
            PlanExecutionLifecycleEvent, PlanExecutionLifecycleRecord, PlanExecutionRecord,
            PlanExecutionState, PlanFileStore, PlanScheduler,
        },
    };

    fn interaction(id: &str) -> Exchange {
        Exchange {
            finalization_error: None,
            finalization_outcome: None,
            agent_id: "primary".into(),
            id: id.into(),
            session_id: "session".into(),
            ordinal: 1,
            prompt: "inspect".into(),
            kind: ExchangeKind::Chat,
            mode: None,
            plan_id: None,
            execution_id: None,
            goal_id: None,
            state: ExchangeState::Complete,
            checkpoint_before: None,
            checkpoint_after: None,
            attributed_diff_text: None,
            checkpoint_diff_text: None,
            attributed_matches_checkpoint: false,
            disposition: crate::exchange::HistoryDisposition::Current,
            turn: Vec::new(),
            created_at_ms: 1,
            completed_at_ms: Some(2),
            node_list: Vec::new(),
            awaiting_input: false,
            elicitation: None,
            duration_ms: 1,
            execution_started_at_ms: None,
            token_count: None,
            comment: Vec::new(),
            task: None,
        }
    }

    fn run(
        id: &str,
        _parent_exchange_id: Option<&str>,
        _parent_thread_id: Option<&str>,
        provider_thread_id: &str,
    ) -> Agent {
        Agent {
            id: id.into(),
            session_id: "session".into(),
            provider_thread_id: Some(provider_thread_id.into()),
            definition: "explorer".into(),
            nickname: None,
            state: AgentState::Ready,
            created_at_ms: 2,
            updated_at_ms: 3,
        }
    }

    #[test]
    fn reused_agent_projects_later_exchange_without_recursing_into_earlier_exchange() {
        use crate::exchange::{ExchangeNode, ExchangeState};
        let first_agent = run("first-agent", None, None, "first-thread");
        let second_agent = run("second-agent", None, None, "second-thread");
        let mut first = interaction("first-exchange");
        let mut second = interaction("second-exchange");
        let mut later = interaction("later-exchange");
        first.agent_id = first_agent.id.clone();
        second.agent_id = second_agent.id.clone();
        later.agent_id = first_agent.id.clone();
        for (exchange, child, target) in [
            (&mut first, "second-agent", "second-exchange"),
            (&mut second, "first-agent", "later-exchange"),
            (&mut later, "first-agent", "first-exchange"),
        ] {
            exchange.state = ExchangeState::Running;
            exchange.completed_at_ms = None;
            exchange
                .append_delegation(child, "spawn", "task", 1)
                .unwrap();
            let ExchangeNode::AgentReference { agent } = &mut exchange.node_list[0] else {
                unreachable!()
            };
            agent.child_exchange_id = target.into();
            exchange.finish(ExchangeState::Complete, 2).unwrap();
        }
        let owners = std::collections::HashMap::from([
            (first_agent.id.clone(), first_agent),
            (second_agent.id.clone(), second_agent),
        ]);
        let records = std::collections::HashMap::from([
            ("first-agent".into(), vec![first, later]),
            ("second-agent".into(), vec![second]),
        ]);
        let entry = super::build_agent_entry(
            "first-agent",
            &owners,
            &records,
            Some("first-exchange"),
            &mut std::collections::HashSet::new(),
        );
        let TimelineEntry::AgentLifecycle {
            exchange: interaction,
            agent,
            ..
        } = entry
        else {
            unreachable!()
        };
        assert_eq!(interaction.len(), 1);
        assert_eq!(interaction[0].id, "first-exchange");
        let TimelineEntry::AgentLifecycle {
            exchange: interaction,
            agent,
            id,
            ..
        } = &agent[0]
        else {
            unreachable!()
        };
        assert_eq!(id, "first-exchange:agent:second-agent:spawn");
        assert_eq!(interaction.len(), 1);
        assert_eq!(interaction[0].id, "second-exchange");
        let TimelineEntry::AgentLifecycle {
            exchange: interaction,
            agent,
            ..
        } = &agent[0]
        else {
            unreachable!()
        };
        assert_eq!(interaction.len(), 1);
        assert_eq!(interaction[0].id, "later-exchange");
        assert!(
            agent.is_empty(),
            "the repeated exchange must terminate projection"
        );
    }

    #[test]
    fn projects_nested_agent_turns_under_their_spawning_interaction() {
        let mut parent = interaction("parent");
        parent.state = crate::exchange::ExchangeState::Running;
        parent.completed_at_ms = None;
        parent
            .append_delegation("root-run", "spawn", "inspect", 1)
            .unwrap();
        parent
            .finish(crate::exchange::ExchangeState::Complete, 3)
            .unwrap();
        let child_interaction = interaction("child-turn");
        let root_run = run("root-run", Some("parent"), None, "root-thread");
        let mut root_exchange = interaction("root-exchange");
        root_exchange.agent_id = root_run.id.clone();
        root_exchange.state = crate::exchange::ExchangeState::Running;
        root_exchange.completed_at_ms = None;
        root_exchange
            .append_delegation("child-run", "spawn", "nested task", 2)
            .unwrap();
        root_exchange
            .finish(crate::exchange::ExchangeState::Complete, 3)
            .unwrap();
        let child_run = run(
            "child-run",
            Some("parent"),
            Some("stale-provider-parent"),
            "child-thread",
        );
        let mut child_turn = child_interaction;
        if let crate::exchange::ExchangeNode::AgentReference { agent } =
            &mut root_exchange.node_list[0]
        {
            agent.child_exchange_id = child_turn.id.clone();
        }
        child_turn.agent_id = child_run.id.clone();
        let directory = tempfile::tempdir().unwrap();
        let timeline = TimelineProjector::build(TimelineProjection {
            interaction_list: vec![parent],
            plan_list: &[],
            lifecycle_list: Vec::new(),
            execution_list: Vec::new(),
            deviation_list: Vec::new(),
            audit_list: Vec::new(),
            resolution_list: Vec::new(),
            agent_run_list: vec![root_run, child_run],
            agent_exchange_list: vec![root_exchange, child_turn],
            session_event_list: Vec::new(),
            plan_file: &PlanFileStore::new(directory.path(), directory.path()),
        })
        .unwrap();

        let TimelineEntry::Exchange { agent_by_id, .. } = &timeline[0] else {
            panic!("parent interaction should remain the root timeline entry");
        };
        let TimelineEntry::AgentLifecycle { agent, .. } =
            agent_by_id.get("root-run").expect("attached root agent")
        else {
            panic!("attached entry should be an agent lifecycle");
        };
        let TimelineEntry::AgentLifecycle {
            exchange: interaction,
            ..
        } = &agent[0]
        else {
            panic!("nested entry should be an agent lifecycle");
        };
        assert_eq!(interaction[0].id, "child-turn");
    }

    #[test]
    fn delegation_links_attach_one_identity_to_each_causal_exchange() {
        let mut first = interaction("first");
        let mut second = interaction("second");
        for exchange in [&mut first, &mut second] {
            exchange.state = crate::exchange::ExchangeState::Running;
            exchange.completed_at_ms = None;
        }
        first
            .append_delegation("child", "spawn", "first task", 1)
            .unwrap();
        second
            .append_delegation("child", "followup", "second task", 2)
            .unwrap();
        for exchange in [&mut first, &mut second] {
            exchange
                .finish(crate::exchange::ExchangeState::Complete, 3)
                .unwrap();
        }
        let mut entries = vec![
            TimelineEntry::Exchange {
                id: first.id.clone(),
                created_at_ms: 1,
                exchange: first,
                agent_by_id: std::collections::HashMap::new(),
            },
            TimelineEntry::Exchange {
                id: second.id.clone(),
                created_at_ms: 2,
                exchange: second,
                agent_by_id: std::collections::HashMap::new(),
            },
        ];
        super::project_agent_tree(
            &mut entries,
            vec![run("child", Some("unrelated-parent"), None, "thread")],
            Vec::new(),
        );
        assert_eq!(entries.len(), 2);
        for entry in entries {
            let TimelineEntry::Exchange { agent_by_id, .. } = entry else {
                panic!("unexpected standalone agent")
            };
            assert!(agent_by_id.contains_key("child"));
        }
    }

    #[test]
    fn projects_task_duration_from_provider_intervals_without_waiting() {
        let document = crate::plan::test_fixture("plan", "Overview");
        let mut scheduler = PlanScheduler::activate(&document);
        scheduler.next_task(&document, 20).unwrap();
        let execution = PlanExecutionRecord {
            id: "execution".into(),
            session_id: "session".into(),
            plan_id: "plan".into(),
            goal_id: "goal".into(),
            state: PlanExecutionState::Complete,
            planning_backend_session_id: None,
            execution_backend_session_id: None,
            scheduler,
            lifecycle: vec![PlanExecutionLifecycleRecord {
                    anchor: None,
                sequence: 1,
                after_exchange_id: Some("second".into()),
                occurred_at_ms: 120,
                event: PlanExecutionLifecycleEvent::TaskCompleted {
                    task_path: "/tasks/0".into(),
                    ordinal: 1,
                    total: 1,
                    title: "Task".into(),
                    elapsed_ms: 100,
                },
            }],
            created_at_ms: 20,
            completed_at_ms: Some(120),
        };
        let mut exchanges = Vec::new();
        for (id, owner, start, end) in [
            ("first", "execution", 10, 30),
            ("second", "execution", 100, 130),
            ("unrelated", "other", 20, 120),
        ] {
            let mut exchange = interaction(id);
            exchange.execution_id = Some(owner.into());
            let mut turn = crate::turn::Turn::new(
                id.into(),
                crate::backend::ProviderAddress {
                    thread_id: "thread".into(),
                    turn_id: id.into(),
                },
                start,
            );
            turn.finish(crate::turn::TurnOutcome::Completed, end)
                .unwrap();
            exchange.turn.push(turn);
            exchanges.push(exchange);
        }
        assert_eq!(
            execution.task_duration_ms("/tasks/0", exchanges.iter(), 120),
            30
        );
        assert_eq!(
            execution.task_duration_ms("/tasks/0", exchanges.iter(), 5),
            0
        );
        super::planning::attach(&mut exchanges, &[], Vec::new(), vec![execution], Vec::new(), &[], &[]);
        assert!(exchanges.iter().flat_map(|exchange| &exchange.node_list).any(|node| matches!(node,
            crate::exchange::ExchangeNode::PlanEvent { event }
            if matches!(&event.content, crate::plan::PlanEventContent::Execution {
                event: PlanExecutionLifecycleEvent::TaskCompleted { elapsed_ms: 30, .. }
            }))));
    }

}
