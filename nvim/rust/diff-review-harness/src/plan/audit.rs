use super::deviation::PlanDeviation;
use super::document::PlanDocument;
use super::scheduler::{PlanScheduler, PlanTaskState, PlanTestStatus};
use serde::{Deserialize, Serialize};
use std::collections::BTreeSet;

/// Reports one planned or changed source path missing from its opposite set.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct PlanAuditPathDifference {
    pub path: String,
    pub reason: String,
}

/// Reports the final execution state and evidence of one planned task.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct PlanAuditTask {
    pub task_id: String,
    pub state: PlanTaskState,
    pub completed_subtask_ids: Vec<String>,
    pub completed_code_edit_ids: Vec<String>,
    pub changed_paths: Vec<String>,
}

/// Owns the final comparison between accepted intent and execution evidence.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct PlanAudit {
    pub id: String,
    pub plan_id: String,
    pub execution_id: String,
    pub task: Vec<PlanAuditTask>,
    pub deviation_ids: Vec<String>,
    pub unplanned_paths: Vec<PlanAuditPathDifference>,
    pub unchanged_planned_paths: Vec<PlanAuditPathDifference>,
    pub passed_tests: u32,
    pub failed_tests: u32,
    pub skipped_tests: u32,
    pub unrun_tests: u32,
    pub created_at_ms: i64,
}

/// Build one deterministic audit from effective intent and task reports.
pub fn build_plan_audit(
    id: String,
    execution_id: String,
    document: &PlanDocument,
    scheduler: &PlanScheduler,
    deviation_list: &[PlanDeviation],
    created_at_ms: i64,
) -> PlanAudit {
    let planned_paths = document
        .tasks
        .iter()
        .flat_map(|task| task.files.iter().map(|file| file.path.clone()))
        .collect::<BTreeSet<_>>();
    let changed_paths = scheduler
        .task
        .iter()
        .flat_map(|task| task.changed_paths.iter().cloned())
        .collect::<BTreeSet<_>>();
    let unplanned_paths = changed_paths
        .difference(&planned_paths)
        .map(|path| PlanAuditPathDifference {
            path: path.clone(),
            reason: "changed path is absent from the effective plan".into(),
        })
        .collect();
    let unchanged_planned_paths = planned_paths
        .difference(&changed_paths)
        .map(|path| PlanAuditPathDifference {
            path: path.clone(),
            reason: "planned path has no reported change".into(),
        })
        .collect();
    let mut passed_tests = 0;
    let mut failed_tests = 0;
    let mut skipped_tests = 0;
    let mut unrun_tests = 0;
    for result in scheduler.task.iter().flat_map(|task| &task.test_results) {
        match result.status {
            PlanTestStatus::Passed => passed_tests += 1,
            PlanTestStatus::Failed => failed_tests += 1,
            PlanTestStatus::Skipped => skipped_tests += 1,
            PlanTestStatus::NotRun => unrun_tests += 1,
        }
    }
    PlanAudit {
        id,
        plan_id: document.plan_id.clone(),
        execution_id,
        task: scheduler
            .task
            .iter()
            .map(|task| PlanAuditTask {
                task_id: task.task_id.clone(),
                state: task.state,
                completed_subtask_ids: task.completed_subtask_ids.clone(),
                completed_code_edit_ids: task.completed_code_edit_ids.clone(),
                changed_paths: task.changed_paths.clone(),
            })
            .collect(),
        deviation_ids: deviation_list
            .iter()
            .map(|deviation| deviation.id.clone())
            .collect(),
        unplanned_paths,
        unchanged_planned_paths,
        passed_tests,
        failed_tests,
        skipped_tests,
        unrun_tests,
        created_at_ms,
    }
}

/// Render the final plan audit for expansion beneath the timeline resolution event.
pub fn render_plan_audit(audit: &PlanAudit) -> String {
    let mut line = vec!["# Plan audit".to_owned(), String::new()];
    line.push(format!("- Tasks: {}", audit.task.len()));
    line.push(format!(
        "- Tests: {} passed, {} failed, {} skipped, {} not run",
        audit.passed_tests, audit.failed_tests, audit.skipped_tests, audit.unrun_tests
    ));
    line.push(format!("- Deviations: {}", audit.deviation_ids.len()));
    line.push(format!(
        "- Unplanned paths: {}",
        audit.unplanned_paths.len()
    ));
    line.push(format!(
        "- Planned paths without changes: {}",
        audit.unchanged_planned_paths.len()
    ));
    line.join("\n") + "\n"
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn compares_effective_paths_with_reported_changes() {
        let document = super::super::document::test_fixture("plan", "Overview");
        let mut scheduler = PlanScheduler::activate(&document);
        scheduler.task[0].changed_paths = vec!["src/unplanned.rs".into()];
        let audit = build_plan_audit(
            "audit".into(),
            "execution".into(),
            &document,
            &scheduler,
            &[],
            1,
        );
        assert_eq!(audit.unplanned_paths[0].path, "src/unplanned.rs");
        assert_eq!(audit.unchanged_planned_paths[0].path, "src/plan.rs");
    }
}
