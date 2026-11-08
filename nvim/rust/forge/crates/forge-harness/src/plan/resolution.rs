use super::audit::PlanAudit;
use super::deviation::{PlanDeviation, PlanDeviationDisposition};
use super::scheduler::{PlanScheduler, PlanTaskState};
use anyhow::Result;
use serde::{Deserialize, Serialize};

/// Defines one durable terminal outcome for an accepted plan execution.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum PlanResolutionKind {
    Completed,
    Blocked,
    Cancelled,
}

/// Summarizes task completion for one terminal timeline event.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct PlanTaskSummary {
    pub completed: u32,
    pub blocked: u32,
    pub total: u32,
}

/// Summarizes verification outcomes for one terminal timeline event.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct PlanTestSummary {
    pub passed: u32,
    pub failed: u32,
    pub skipped: u32,
    pub not_run: u32,
}

/// Represents one chronological terminal plan event and its audit references.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct PlanResolutionRecord {
    #[serde(default)]
    pub anchor: Option<super::ExchangeAnchor>,
    pub id: String,
    pub session_id: String,
    pub plan_id: String,
    pub execution_id: String,
    pub accepted_revision: u32,
    pub kind: PlanResolutionKind,
    pub task_summary: PlanTaskSummary,
    pub test_summary: PlanTestSummary,
    pub deviation_ids: Vec<String>,
    pub audit_id: String,
    pub resolved_at_ms: i64,
}

/// Borrows the task, deviation, and audit evidence used to resolve one plan execution.
pub struct PlanResolutionEvidence<'a> {
    pub scheduler: &'a PlanScheduler,
    pub deviation_list: &'a [PlanDeviation],
    pub audit: &'a PlanAudit,
}

/// Build one outcome-specific terminal event after its audit exists.
pub fn build_plan_resolution(
    id: String,
    session_id: String,
    accepted_revision: u32,
    kind: PlanResolutionKind,
    evidence: PlanResolutionEvidence<'_>,
    resolved_at_ms: i64,
) -> Result<PlanResolutionRecord> {
    let PlanResolutionEvidence {
        scheduler,
        deviation_list,
        audit,
    } = evidence;
    if kind == PlanResolutionKind::Completed {
        anyhow::ensure!(
            scheduler.is_complete(),
            "completed resolution requires every task"
        );
        anyhow::ensure!(
            !deviation_list
                .iter()
                .any(|deviation| deviation.disposition == PlanDeviationDisposition::Pending),
            "completed resolution cannot contain pending deviations"
        );
    }
    Ok(PlanResolutionRecord {
        anchor: None,
        id,
        session_id,
        plan_id: scheduler.plan_id.clone(),
        execution_id: audit.execution_id.clone(),
        accepted_revision,
        kind,
        task_summary: PlanTaskSummary {
            completed: scheduler
                .task
                .iter()
                .filter(|task| task.state == PlanTaskState::Complete)
                .count() as u32,
            blocked: scheduler
                .task
                .iter()
                .filter(|task| task.state == PlanTaskState::Blocked)
                .count() as u32,
            total: scheduler.task.len() as u32,
        },
        test_summary: PlanTestSummary {
            passed: audit.passed_tests,
            failed: audit.failed_tests,
            skipped: audit.skipped_tests,
            not_run: audit.unrun_tests,
        },
        deviation_ids: deviation_list
            .iter()
            .map(|deviation| deviation.id.clone())
            .collect(),
        audit_id: audit.id.clone(),
        resolved_at_ms,
    })
}
