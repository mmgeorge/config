use crate::agent::{AgentRun, AgentTurnRecord};
use crate::interaction::InteractionRecord;
use crate::plan::{
    PlanAudit, PlanDeviation, PlanExecutionLifecycleEvent, PlanExecutionLifecycleRecord,
    PlanExecutionRecord, PlanFileStore, PlanLifecycleKind, PlanLifecycleRecord, PlanRecord,
    PlanResolutionRecord,
};
use crate::session::state_machine::SessionPhase;
use anyhow::Result;
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};

pub mod stream;

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

/// Represents one causally ordered row inside an accepted plan execution.
#[derive(Clone, Debug, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum PlanExecutionTimelineItem {
    Interaction {
        interaction: InteractionRecord,
    },
    TaskStarted {
        task_id: String,
        ordinal: usize,
        total: usize,
        title: String,
    },
    TaskCompleted {
        task_id: String,
        ordinal: usize,
        total: usize,
        title: String,
        elapsed_ms: i64,
    },
    DeviationRecorded {
        deviation_id: String,
        summary: String,
    },
}

/// Represents one fully resolved top-level Harness timeline entry.
#[derive(Clone, Debug, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum TimelineEntry {
    Interaction {
        id: String,
        created_at_ms: i64,
        interaction: InteractionRecord,
        agent_by_id: HashMap<String, TimelineEntry>,
    },
    PlanLifecycle {
        id: String,
        created_at_ms: i64,
        plan: PlanRecord,
        lifecycle: PlanLifecycleRecord,
    },
    PlanExecution {
        id: String,
        created_at_ms: i64,
        plan: PlanRecord,
        execution: PlanExecutionRecord,
        item: Vec<PlanExecutionTimelineItem>,
    },
    PlanResolution {
        id: String,
        created_at_ms: i64,
        resolution: PlanResolutionRecord,
        deviation: Vec<PlanDeviation>,
        audit: Option<PlanAudit>,
    },
    AgentLifecycle {
        id: String,
        created_at_ms: i64,
        run: AgentRun,
        interaction: Vec<InteractionRecord>,
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
            Self::Interaction { id, .. }
            | Self::PlanLifecycle { id, .. }
            | Self::PlanExecution { id, .. }
            | Self::PlanResolution { id, .. }
            | Self::AgentLifecycle { id, .. }
            | Self::SessionEvent { id, .. }
            | Self::Status { id, .. } => id.clone(),
        }
    }

    fn created_at_ms(&self) -> i64 {
        match self {
            Self::Interaction { created_at_ms, .. }
            | Self::PlanLifecycle { created_at_ms, .. }
            | Self::PlanExecution { created_at_ms, .. }
            | Self::PlanResolution { created_at_ms, .. }
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
    pub interaction_list: Vec<InteractionRecord>,
    pub plan_list: &'a [PlanRecord],
    pub lifecycle_list: Vec<PlanLifecycleRecord>,
    pub execution_list: Vec<PlanExecutionRecord>,
    pub deviation_list: Vec<PlanDeviation>,
    pub audit_list: Vec<PlanAudit>,
    pub resolution_list: Vec<PlanResolutionRecord>,
    pub agent_run_list: Vec<AgentRun>,
    pub agent_turn_list: Vec<AgentTurnRecord>,
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
            agent_turn_list,
            session_event_list,
            plan_file: _,
        } = projection;
        let plan_by_id = plan_list
            .iter()
            .map(|plan| (plan.id.as_str(), plan))
            .collect::<HashMap<_, _>>();
        let mut interaction_by_execution = HashMap::<String, Vec<InteractionRecord>>::new();
        let mut result = Vec::new();
        for interaction in interaction_list {
            if let Some(execution_id) = interaction.execution_id.as_ref() {
                interaction_by_execution
                    .entry(execution_id.clone())
                    .or_default()
                    .push(interaction);
            } else {
                result.push(TimelineEntry::Interaction {
                    id: interaction.id.clone(),
                    created_at_ms: interaction.created_at_ms,
                    interaction,
                    agent_by_id: HashMap::new(),
                });
            }
        }
        for lifecycle in lifecycle_list {
            if matches!(
                lifecycle.kind,
                PlanLifecycleKind::QuestionAnswered
                    | PlanLifecycleKind::Created
                    | PlanLifecycleKind::RevisionCreated
                    | PlanLifecycleKind::ChangesRequested
            ) {
                continue;
            }
            let Some(plan) = plan_by_id.get(lifecycle.plan_id.as_str()) else {
                continue;
            };
            result.push(TimelineEntry::PlanLifecycle {
                id: lifecycle.id.clone(),
                created_at_ms: lifecycle.created_at_ms,
                plan: (*plan).clone(),
                lifecycle,
            });
        }
        for execution in execution_list {
            let Some(plan) = plan_by_id.get(execution.plan_id.as_str()) else {
                continue;
            };
            let interaction_list = interaction_by_execution
                .remove(&execution.id)
                .unwrap_or_default();
            let item = project_plan_execution_item(&execution, interaction_list);
            result.push(TimelineEntry::PlanExecution {
                id: execution.id.clone(),
                created_at_ms: execution.created_at_ms,
                plan: (*plan).clone(),
                item,
                execution,
            });
        }
        for resolution in resolution_list {
            result.push(TimelineEntry::PlanResolution {
                id: resolution.id.clone(),
                created_at_ms: resolution.resolved_at_ms,
                deviation: deviation_list
                    .iter()
                    .filter(|deviation| deviation.execution_id == resolution.execution_id)
                    .cloned()
                    .collect(),
                audit: audit_list
                    .iter()
                    .find(|audit| audit.id == resolution.audit_id)
                    .cloned(),
                resolution,
            });
        }
        project_agent_tree(&mut result, agent_run_list, agent_turn_list);
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

fn project_plan_execution_item(
    execution: &PlanExecutionRecord,
    mut interaction_list: Vec<InteractionRecord>,
) -> Vec<PlanExecutionTimelineItem> {
    interaction_list.sort_by_key(|interaction| interaction.ordinal);
    let interaction_id_set = interaction_list
        .iter()
        .map(|interaction| interaction.id.clone())
        .collect::<HashSet<_>>();
    let mut lifecycle_list = execution.lifecycle.clone();
    lifecycle_list.sort_by_key(|record| record.sequence);
    let mut item_list = Vec::new();
    append_plan_execution_lifecycle(
        &mut item_list,
        lifecycle_list
            .iter()
            .filter(|record| record.after_interaction_id.is_none()),
    );
    for interaction in interaction_list {
        let interaction_id = interaction.id.clone();
        item_list.push(PlanExecutionTimelineItem::Interaction { interaction });
        append_plan_execution_lifecycle(
            &mut item_list,
            lifecycle_list.iter().filter(|record| {
                record.after_interaction_id.as_deref() == Some(interaction_id.as_str())
            }),
        );
    }
    append_plan_execution_lifecycle(
        &mut item_list,
        lifecycle_list.iter().filter(|record| {
            record
                .after_interaction_id
                .as_deref()
                .is_some_and(|interaction_id| !interaction_id_set.contains(interaction_id))
        }),
    );
    item_list
}

fn append_plan_execution_lifecycle<'a>(
    item_list: &mut Vec<PlanExecutionTimelineItem>,
    lifecycle_list: impl Iterator<Item = &'a PlanExecutionLifecycleRecord>,
) {
    item_list.extend(lifecycle_list.map(|record| match &record.event {
        PlanExecutionLifecycleEvent::TaskStarted {
            task_id,
            ordinal,
            total,
            title,
        } => PlanExecutionTimelineItem::TaskStarted {
            task_id: task_id.clone(),
            ordinal: *ordinal,
            total: *total,
            title: title.clone(),
        },
        PlanExecutionLifecycleEvent::TaskCompleted {
            task_id,
            ordinal,
            total,
            title,
            elapsed_ms,
        } => PlanExecutionTimelineItem::TaskCompleted {
            task_id: task_id.clone(),
            ordinal: *ordinal,
            total: *total,
            title: title.clone(),
            elapsed_ms: *elapsed_ms,
        },
        PlanExecutionLifecycleEvent::DeviationRecorded {
            deviation_id,
            summary,
        } => PlanExecutionTimelineItem::DeviationRecorded {
            deviation_id: deviation_id.clone(),
            summary: summary.clone(),
        },
    }));
}

fn project_agent_tree(
    result: &mut Vec<TimelineEntry>,
    agent_run_list: Vec<AgentRun>,
    agent_turn_list: Vec<AgentTurnRecord>,
) {
    let run_by_id = agent_run_list
        .iter()
        .map(|run| (run.id.clone(), run.clone()))
        .collect::<HashMap<_, _>>();
    let run_id_by_thread = agent_run_list
        .iter()
        .filter_map(|run| {
            run.provider_thread_id
                .as_ref()
                .map(|thread_id| (thread_id.clone(), run.id.clone()))
        })
        .collect::<HashMap<_, _>>();
    let mut interaction_by_run_id = HashMap::<String, Vec<InteractionRecord>>::new();
    for turn in agent_turn_list {
        interaction_by_run_id
            .entry(turn.agent_run_id)
            .or_default()
            .push(turn.interaction);
    }
    let mut child_id_by_run_id = HashMap::<String, Vec<String>>::new();
    let mut root_id_list = Vec::new();
    for run in &agent_run_list {
        if let Some(parent_id) = run
            .parent_thread_id
            .as_ref()
            .and_then(|thread_id| run_id_by_thread.get(thread_id))
        {
            child_id_by_run_id
                .entry(parent_id.clone())
                .or_default()
                .push(run.id.clone());
        } else {
            root_id_list.push(run.id.clone());
        }
    }

    for run_id in root_id_list {
        let Some(run) = run_by_id.get(&run_id) else {
            continue;
        };
        let entry = build_agent_entry(
            &run_id,
            &run_by_id,
            &interaction_by_run_id,
            &child_id_by_run_id,
        );
        let mut attached = false;
        if let Some(parent_interaction_id) = run.parent_interaction_id.as_deref() {
            for timeline_entry in result.iter_mut() {
                if let TimelineEntry::Interaction {
                    interaction,
                    agent_by_id,
                    ..
                } = timeline_entry
                    && interaction.id == parent_interaction_id
                {
                    agent_by_id.insert(run_id.clone(), entry.clone());
                    attached = true;
                    break;
                }
            }
        }
        if !attached {
            result.push(entry);
        }
    }
}

fn build_agent_entry(
    run_id: &str,
    run_by_id: &HashMap<String, AgentRun>,
    interaction_by_run_id: &HashMap<String, Vec<InteractionRecord>>,
    child_id_by_run_id: &HashMap<String, Vec<String>>,
) -> TimelineEntry {
    let run = run_by_id
        .get(run_id)
        .expect("agent tree references a known run")
        .clone();
    let agent = child_id_by_run_id
        .get(run_id)
        .into_iter()
        .flatten()
        .map(|child_id| {
            build_agent_entry(
                child_id,
                run_by_id,
                interaction_by_run_id,
                child_id_by_run_id,
            )
        })
        .collect();
    TimelineEntry::AgentLifecycle {
        id: run.id.clone(),
        created_at_ms: run.created_at_ms,
        interaction: interaction_by_run_id
            .get(run_id)
            .cloned()
            .unwrap_or_default(),
        run,
        agent,
    }
}

#[cfg(test)]
mod test {
    use super::{
        PlanExecutionTimelineItem, TimelineEntry, TimelineProjection, TimelineProjector,
        project_plan_execution_item,
    };
    use crate::{
        agent::{AgentRun, AgentRunStatus, AgentTurnRecord},
        interaction::{InteractionKind, InteractionRecord, InteractionState},
        plan::{
            PlanExecutionLifecycleEvent, PlanExecutionLifecycleRecord, PlanExecutionRecord,
            PlanExecutionState, PlanFileStore, PlanScheduler,
        },
    };

    fn interaction(id: &str) -> InteractionRecord {
        InteractionRecord {
            id: id.into(),
            session_id: "session".into(),
            ordinal: 1,
            prompt: "inspect".into(),
            kind: InteractionKind::Chat,
            plan_id: None,
            execution_id: None,
            state: InteractionState::Complete,
            checkpoint_before: None,
            checkpoint_after: None,
            attributed_diff_text: None,
            checkpoint_diff_text: None,
            attributed_matches_checkpoint: false,
            created_at_ms: 1,
            completed_at_ms: Some(2),
            node_list: Vec::new(),
            awaiting_input: false,
            elicitation: None,
            duration_ms: 1,
            token_count: None,
            comment: Vec::new(),
            task: None,
        }
    }

    fn run(
        id: &str,
        parent_interaction_id: Option<&str>,
        parent_thread_id: Option<&str>,
        provider_thread_id: &str,
    ) -> AgentRun {
        AgentRun {
            id: id.into(),
            session_id: "session".into(),
            parent_interaction_id: parent_interaction_id.map(str::to_owned),
            parent_thread_id: parent_thread_id.map(str::to_owned),
            provider_thread_id: Some(provider_thread_id.into()),
            active_turn_id: None,
            definition: "explorer".into(),
            nickname: None,
            task: "inspect".into(),
            status: AgentRunStatus::Completed,
            created_at_ms: 2,
            updated_at_ms: 3,
        }
    }

    #[test]
    fn projects_nested_agent_turns_under_their_spawning_interaction() {
        let parent = interaction("parent");
        let child_interaction = interaction("child-turn");
        let root_run = run("root-run", Some("parent"), None, "root-thread");
        let child_run = run(
            "child-run",
            Some("parent"),
            Some("root-thread"),
            "child-thread",
        );
        let child_turn = AgentTurnRecord {
            id: "turn".into(),
            session_id: "session".into(),
            agent_run_id: child_run.id.clone(),
            ordinal: 1,
            interaction: child_interaction,
        };
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
            agent_turn_list: vec![child_turn],
            session_event_list: Vec::new(),
            plan_file: &PlanFileStore::new(directory.path()),
        })
        .unwrap();

        let TimelineEntry::Interaction { agent_by_id, .. } = &timeline[0] else {
            panic!("parent interaction should remain the root timeline entry");
        };
        let TimelineEntry::AgentLifecycle { agent, .. } =
            agent_by_id.get("root-run").expect("attached root agent")
        else {
            panic!("attached entry should be an agent lifecycle");
        };
        let TimelineEntry::AgentLifecycle { interaction, .. } = &agent[0] else {
            panic!("nested entry should be an agent lifecycle");
        };
        assert_eq!(interaction[0].id, "child-turn");
    }

    #[test]
    fn projects_scheduler_lifecycle_around_its_causal_interaction() {
        let mut first = interaction("first");
        first.execution_id = Some("execution".into());
        let mut second = interaction("second");
        second.ordinal = 2;
        second.execution_id = Some("execution".into());
        let execution = PlanExecutionRecord {
            id: "execution".into(),
            session_id: "session".into(),
            plan_id: "plan".into(),
            goal_id: "goal".into(),
            state: PlanExecutionState::Active,
            planning_backend_session_id: None,
            execution_backend_session_id: None,
            scheduler: PlanScheduler::default(),
            lifecycle: vec![
                PlanExecutionLifecycleRecord {
                    sequence: 1,
                    after_interaction_id: None,
                    occurred_at_ms: 1,
                    event: PlanExecutionLifecycleEvent::TaskStarted {
                        task_id: "task-one".into(),
                        ordinal: 1,
                        total: 2,
                        title: "First task".into(),
                    },
                },
                PlanExecutionLifecycleRecord {
                    sequence: 2,
                    after_interaction_id: Some("first".into()),
                    occurred_at_ms: 2,
                    event: PlanExecutionLifecycleEvent::TaskCompleted {
                        task_id: "task-one".into(),
                        ordinal: 1,
                        total: 2,
                        title: "First task".into(),
                        elapsed_ms: 1000,
                    },
                },
                PlanExecutionLifecycleRecord {
                    sequence: 3,
                    after_interaction_id: Some("first".into()),
                    occurred_at_ms: 2,
                    event: PlanExecutionLifecycleEvent::TaskStarted {
                        task_id: "task-two".into(),
                        ordinal: 2,
                        total: 2,
                        title: "Second task".into(),
                    },
                },
                PlanExecutionLifecycleRecord {
                    sequence: 4,
                    after_interaction_id: Some("second".into()),
                    occurred_at_ms: 3,
                    event: PlanExecutionLifecycleEvent::DeviationRecorded {
                        deviation_id: "deviation".into(),
                        summary: "Narrow correction".into(),
                    },
                },
            ],
            created_at_ms: 1,
            completed_at_ms: None,
        };

        let item_list = project_plan_execution_item(&execution, vec![second, first]);
        assert!(matches!(
            &item_list[0],
            PlanExecutionTimelineItem::TaskStarted { task_id, .. } if task_id == "task-one"
        ));
        assert!(matches!(
            &item_list[1],
            PlanExecutionTimelineItem::Interaction { interaction } if interaction.id == "first"
        ));
        assert!(matches!(
            &item_list[2],
            PlanExecutionTimelineItem::TaskCompleted { task_id, .. } if task_id == "task-one"
        ));
        assert!(matches!(
            &item_list[3],
            PlanExecutionTimelineItem::TaskStarted { task_id, .. } if task_id == "task-two"
        ));
        assert!(matches!(
            &item_list[4],
            PlanExecutionTimelineItem::Interaction { interaction } if interaction.id == "second"
        ));
        assert!(matches!(
            &item_list[5],
            PlanExecutionTimelineItem::DeviationRecorded { deviation_id, .. }
                if deviation_id == "deviation"
        ));
    }
}
