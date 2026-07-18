use crate::agent::AgentRun;
use crate::interaction::InteractionRecord;
use crate::plan::{
    PlanExecutionRecord, PlanFileStore, PlanLifecycleKind, PlanLifecycleRecord, PlanRecord,
};
use anyhow::Result;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

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
    Interaction {
        id: String,
        created_at_ms: i64,
        interaction: InteractionRecord,
    },
    PlanLifecycle {
        id: String,
        created_at_ms: i64,
        plan: PlanRecord,
        lifecycle: PlanLifecycleRecord,
        content: Option<String>,
    },
    PlanExecution {
        id: String,
        created_at_ms: i64,
        plan: PlanRecord,
        execution: PlanExecutionRecord,
        interaction: Vec<InteractionRecord>,
    },
    AgentLifecycle {
        id: String,
        created_at_ms: i64,
        run: AgentRun,
    },
    SessionEvent {
        id: String,
        created_at_ms: i64,
        event: SessionEventRecord,
    },
}

impl TimelineEntry {
    fn created_at_ms(&self) -> i64 {
        match self {
            Self::Interaction { created_at_ms, .. }
            | Self::PlanLifecycle { created_at_ms, .. }
            | Self::PlanExecution { created_at_ms, .. }
            | Self::AgentLifecycle { created_at_ms, .. }
            | Self::SessionEvent { created_at_ms, .. } => *created_at_ms,
        }
    }
}

/// Resolves durable records into one ordered session presentation.
pub struct TimelineProjector;

impl TimelineProjector {
    /// Build one stable session timeline from interactions and plan lifecycle records.
    pub fn build(
        interaction_list: Vec<InteractionRecord>,
        plan_list: &[PlanRecord],
        lifecycle_list: Vec<PlanLifecycleRecord>,
        execution_list: Vec<PlanExecutionRecord>,
        agent_run_list: Vec<AgentRun>,
        session_event_list: Vec<SessionEventRecord>,
        plan_file: &PlanFileStore,
    ) -> Result<Vec<TimelineEntry>> {
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
                });
            }
        }
        for lifecycle in lifecycle_list {
            let Some(plan) = plan_by_id.get(lifecycle.plan_id.as_str()) else {
                continue;
            };
            let content = match lifecycle.kind {
                PlanLifecycleKind::Created | PlanLifecycleKind::RevisionCreated => {
                    Some(plan_file.read_model_revision(
                        &lifecycle.session_id,
                        &lifecycle.plan_id,
                        lifecycle.model_revision,
                    )?)
                }
                PlanLifecycleKind::ChangesRequested | PlanLifecycleKind::Accepted
                    if lifecycle.user_revision > 0 =>
                {
                    Some(plan_file.read_user_revision(
                        &lifecycle.session_id,
                        &lifecycle.plan_id,
                        lifecycle.user_revision,
                    )?)
                }
                _ => None,
            };
            result.push(TimelineEntry::PlanLifecycle {
                id: lifecycle.id.clone(),
                created_at_ms: lifecycle.created_at_ms,
                plan: (*plan).clone(),
                lifecycle,
                content,
            });
        }
        for execution in execution_list {
            let Some(plan) = plan_by_id.get(execution.plan_id.as_str()) else {
                continue;
            };
            result.push(TimelineEntry::PlanExecution {
                id: execution.id.clone(),
                created_at_ms: execution.created_at_ms,
                plan: (*plan).clone(),
                interaction: interaction_by_execution
                    .remove(&execution.id)
                    .unwrap_or_default(),
                execution,
            });
        }
        for run in agent_run_list {
            result.push(TimelineEntry::AgentLifecycle {
                id: run.id.clone(),
                created_at_ms: run.created_at_ms,
                run,
            });
        }
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
