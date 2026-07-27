use super::document::{PlanDocument, PlanSubtask, PlanTask};
use serde::{Deserialize, Serialize};
use std::collections::HashSet;

/// Defines the execution state of one complete plan task.
#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum PlanTaskState {
    #[default]
    Pending,
    Active,
    Complete,
    Blocked,
}

/// Defines the reported outcome of one plan-linked test case.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum PlanTestStatus {
    Passed,
    Failed,
    Skipped,
    NotRun,
}

/// Represents one test result attached to task completion evidence.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct PlanTestResult {
    pub test_subtask_id: Option<String>,
    pub status: PlanTestStatus,
    pub command: Option<String>,
    pub detail: Option<String>,
}

/// Tracks granular evidence while one complete task remains the scheduling unit.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct PlanTaskExecution {
    pub task_id: String,
    pub state: PlanTaskState,
    #[serde(default)]
    pub started_at_ms: Option<i64>,
    #[serde(default)]
    pub completed_at_ms: Option<i64>,
    #[serde(default)]
    pub completed_subtask_ids: Vec<String>,
    #[serde(default)]
    pub completed_entity_ids: Vec<String>,
    #[serde(default)]
    pub test_results: Vec<PlanTestResult>,
    #[serde(default)]
    pub changed_paths: Vec<String>,
    pub summary: Option<String>,
    pub blocking_reason: Option<String>,
}

/// Carries completion or blocking evidence for one whole scheduled task.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct PlanTaskReport {
    pub execution_id: String,
    pub task_id: String,
    pub state: PlanTaskState,
    #[serde(default)]
    pub completed_subtask_ids: Vec<String>,
    #[serde(default)]
    pub completed_entity_ids: Vec<String>,
    #[serde(default)]
    pub test_results: Vec<PlanTestResult>,
    #[serde(default)]
    pub changed_paths: Vec<String>,
    pub summary: Option<String>,
    pub blocking_reason: Option<String>,
}

/// Owns ordered task selection without promoting subtasks into goals.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct PlanScheduler {
    pub plan_id: String,
    pub task: Vec<PlanTaskExecution>,
}

impl Default for PlanScheduler {
    fn default() -> Self {
        Self {
            plan_id: String::new(),
            task: Vec::new(),
        }
    }
}

impl PlanScheduler {
    /// Build pending execution state in canonical task order.
    pub fn activate(document: &PlanDocument) -> Self {
        Self {
            plan_id: document.plan_id.clone(),
            task: document
                .tasks
                .iter()
                .map(|task| PlanTaskExecution {
                    task_id: task.task_id.clone(),
                    state: PlanTaskState::Pending,
                    started_at_ms: None,
                    completed_at_ms: None,
                    completed_subtask_ids: Vec::new(),
                    completed_entity_ids: Vec::new(),
                    test_results: Vec::new(),
                    changed_paths: Vec::new(),
                    summary: None,
                    blocking_reason: None,
                })
                .collect(),
        }
    }

    /// Select the next incomplete task and activate its complete subtree.
    pub fn next_task<'a>(
        &mut self,
        document: &'a PlanDocument,
        now_ms: i64,
    ) -> Option<&'a PlanTask> {
        let execution = self
            .task
            .iter_mut()
            .find(|task| task.state == PlanTaskState::Pending)?;
        execution.state = PlanTaskState::Active;
        execution.started_at_ms = Some(now_ms);
        document
            .tasks
            .iter()
            .find(|task| task.task_id == execution.task_id)
    }

    /// Return whether every required task completed successfully.
    pub fn is_complete(&self) -> bool {
        !self.task.is_empty()
            && self
                .task
                .iter()
                .all(|task| task.state == PlanTaskState::Complete)
    }

    /// Apply evidence to the active task and activate the next complete task subtree.
    pub fn apply_report<'a>(
        &mut self,
        document: &'a PlanDocument,
        report: PlanTaskReport,
        now_ms: i64,
    ) -> anyhow::Result<Option<&'a PlanTask>> {
        anyhow::ensure!(
            matches!(
                report.state,
                PlanTaskState::Complete | PlanTaskState::Blocked
            ),
            "task report must complete or block a task"
        );
        let planned_task = document
            .tasks
            .iter()
            .find(|task| task.task_id == report.task_id)
            .ok_or_else(|| anyhow::anyhow!("canonical task not found"))?;
        validate_task_evidence(document, planned_task, &report)?;
        let task = self
            .task
            .iter_mut()
            .find(|task| task.task_id == report.task_id)
            .ok_or_else(|| anyhow::anyhow!("scheduled task not found"))?;
        anyhow::ensure!(task.state == PlanTaskState::Active, "task is not active");
        task.state = report.state;
        task.completed_at_ms = Some(now_ms);
        task.completed_subtask_ids = report.completed_subtask_ids;
        task.completed_entity_ids = report.completed_entity_ids;
        task.test_results = report.test_results;
        task.changed_paths = report.changed_paths;
        task.summary = report.summary;
        task.blocking_reason = report.blocking_reason;
        if task.state == PlanTaskState::Blocked {
            return Ok(None);
        }
        Ok(self.next_task(document, now_ms))
    }
}

fn validate_task_evidence(
    _document: &PlanDocument,
    task: &PlanTask,
    report: &PlanTaskReport,
) -> anyhow::Result<()> {
    let planned_subtask_id = task
        .files
        .iter()
        .flat_map(|file| &file.subtasks)
        .map(PlanSubtask::subtask_id)
        .collect::<HashSet<_>>();
    let planned_entity_id = task
        .files
        .iter()
        .flat_map(|file| &file.subtasks)
        .flat_map(PlanSubtask::owned_entity_ids)
        .map(String::as_str)
        .collect::<HashSet<_>>();
    let planned_path = task
        .files
        .iter()
        .flat_map(|file| {
            file.change
                .source_path()
                .into_iter()
                .chain(std::iter::once(file.change.path()))
        })
        .collect::<HashSet<_>>();
    let planned_test_subtask_id = task
        .files
        .iter()
        .flat_map(|file| &file.subtasks)
        .filter_map(PlanSubtask::test)
        .map(|test| test.subtask_id.as_str())
        .collect::<HashSet<_>>();

    anyhow::ensure!(
        report
            .completed_subtask_ids
            .iter()
            .all(|id| planned_subtask_id.contains(id.as_str())),
        "task report contains an unknown subtask"
    );
    anyhow::ensure!(
        report
            .completed_entity_ids
            .iter()
            .all(|id| planned_entity_id.contains(id.as_str())),
        "task report contains an unknown entity"
    );
    anyhow::ensure!(
        report
            .changed_paths
            .iter()
            .all(|path| planned_path.contains(path.as_str())),
        "task report contains a path outside the active task"
    );
    anyhow::ensure!(
        report.test_results.iter().all(|result| {
            result
                .test_subtask_id
                .as_deref()
                .is_none_or(|id| planned_test_subtask_id.contains(id))
        }),
        "task report contains a test outside the active task"
    );
    if report.state == PlanTaskState::Complete {
        let completed_subtask_id = report
            .completed_subtask_ids
            .iter()
            .map(String::as_str)
            .collect::<HashSet<_>>();
        let completed_entity_id = report
            .completed_entity_ids
            .iter()
            .map(String::as_str)
            .collect::<HashSet<_>>();
        anyhow::ensure!(
            planned_subtask_id == completed_subtask_id,
            "complete task report must account for every subtask"
        );
        anyhow::ensure!(
            planned_entity_id == completed_entity_id,
            "complete task report must account for every entity"
        );
    }
    Ok(())
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn schedules_complete_tasks_while_retaining_leaf_evidence() {
        let document = super::super::document::test_fixture("plan", "Overview");
        let mut scheduler = PlanScheduler::activate(&document);
        assert_eq!(
            scheduler.next_task(&document, 10).unwrap().task_id,
            "create_plan_state"
        );
        scheduler
            .apply_report(
                &document,
                PlanTaskReport {
                    execution_id: "execution".into(),
                    task_id: "create_plan_state".into(),
                    state: PlanTaskState::Complete,
                    completed_subtask_ids: vec!["create_owner".into()],
                    completed_entity_ids: vec!["plan_document".into()],
                    test_results: Vec::new(),
                    changed_paths: vec!["src/plan.rs".into()],
                    summary: Some("Complete".into()),
                    blocking_reason: None,
                },
                20,
            )
            .unwrap();
        assert!(scheduler.is_complete());
        assert_eq!(scheduler.task[0].completed_subtask_ids, ["create_owner"]);
        assert_eq!(scheduler.task[0].started_at_ms, Some(10));
        assert_eq!(scheduler.task[0].completed_at_ms, Some(20));
    }

    #[test]
    fn accepts_test_evidence_only_from_the_active_task_test_subtasks() {
        let mut document = super::super::document::test_fixture("plan", "Overview");
        super::super::document::attach_test_fixture(&mut document);
        let mut scheduler = PlanScheduler::activate(&document);
        scheduler.next_task(&document, 10).unwrap();

        scheduler
            .apply_report(
                &document,
                PlanTaskReport {
                    execution_id: "execution".into(),
                    task_id: "create_plan_state".into(),
                    state: PlanTaskState::Complete,
                    completed_subtask_ids: vec!["create_owner".into(), "validates_plans".into()],
                    completed_entity_ids: vec!["plan_document".into()],
                    test_results: vec![PlanTestResult {
                        test_subtask_id: Some("validates_plans".into()),
                        status: PlanTestStatus::Passed,
                        command: Some("cargo test validates_plans".into()),
                        detail: None,
                    }],
                    changed_paths: vec!["src/plan.rs".into()],
                    summary: Some("Complete".into()),
                    blocking_reason: None,
                },
                20,
            )
            .unwrap();

        assert_eq!(
            scheduler.task[0].test_results[0].test_subtask_id.as_deref(),
            Some("validates_plans")
        );
    }

    #[test]
    fn timestamps_each_whole_task_once_across_scheduler_transitions() {
        let mut document = super::super::document::test_fixture("plan", "Overview");
        let mut second_task = document.tasks[0].clone();
        second_task.task_id = "second_task".into();
        second_task.title = "Second task".into();
        document.tasks.push(second_task);
        let mut scheduler = PlanScheduler::activate(&document);
        scheduler.next_task(&document, 10).unwrap();
        let next_task = scheduler
            .apply_report(
                &document,
                PlanTaskReport {
                    execution_id: "execution".into(),
                    task_id: "create_plan_state".into(),
                    state: PlanTaskState::Complete,
                    completed_subtask_ids: vec!["create_owner".into()],
                    completed_entity_ids: vec!["plan_document".into()],
                    test_results: Vec::new(),
                    changed_paths: Vec::new(),
                    summary: Some("Complete".into()),
                    blocking_reason: None,
                },
                20,
            )
            .unwrap()
            .unwrap();

        assert_eq!(next_task.task_id, "second_task");
        assert_eq!(scheduler.task[0].started_at_ms, Some(10));
        assert_eq!(scheduler.task[0].completed_at_ms, Some(20));
        assert_eq!(scheduler.task[1].started_at_ms, Some(20));
        assert_eq!(scheduler.task[1].completed_at_ms, None);
        assert_eq!(scheduler.task[1].state, PlanTaskState::Active);
    }
}
