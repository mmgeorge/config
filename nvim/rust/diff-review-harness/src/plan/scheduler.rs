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
    pub test_subtask_path: Option<String>,
    pub status: PlanTestStatus,
    pub command: Option<String>,
    pub detail: Option<String>,
}

/// Tracks granular evidence while one complete task remains the scheduling unit.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct PlanTaskExecution {
    pub task_path: String,
    pub state: PlanTaskState,
    #[serde(default)]
    pub started_at_ms: Option<i64>,
    #[serde(default)]
    pub completed_at_ms: Option<i64>,
    #[serde(default)]
    pub completed_subtask_paths: Vec<String>,
    #[serde(default)]
    pub completed_entity_paths: Vec<String>,
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
    pub task_path: String,
    pub state: PlanTaskState,
    #[serde(default)]
    pub completed_subtask_paths: Vec<String>,
    #[serde(default)]
    pub completed_entity_paths: Vec<String>,
    #[serde(default)]
    pub test_results: Vec<PlanTestResult>,
    #[serde(default)]
    pub changed_paths: Vec<String>,
    pub summary: Option<String>,
    pub blocking_reason: Option<String>,
}

/// Owns ordered task selection without promoting subtasks into goals.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct PlanScheduler {
    pub plan_id: String,
    pub plan_version: u64,
    pub task: Vec<PlanTaskExecution>,
}

impl PlanScheduler {
    /// Build pending execution state in canonical task order.
    pub fn activate(document: &PlanDocument) -> Self {
        Self {
            plan_id: document.plan_id.clone(),
            plan_version: document.version,
            task: document
                .tasks
                .iter()
                .enumerate()
                .map(|(task_index, _)| PlanTaskExecution {
                    task_path: task_pointer(task_index),
                    state: PlanTaskState::Pending,
                    started_at_ms: None,
                    completed_at_ms: None,
                    completed_subtask_paths: Vec::new(),
                    completed_entity_paths: Vec::new(),
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
        let (task_index, execution) = self
            .task
            .iter_mut()
            .enumerate()
            .find(|(_, task)| task.state == PlanTaskState::Pending)?;
        execution.state = PlanTaskState::Active;
        execution.started_at_ms = Some(now_ms);
        document.tasks.get(task_index)
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
        anyhow::ensure!(
            self.plan_id == document.plan_id && self.plan_version == document.version,
            "scheduler does not target this plan revision"
        );
        let task_index = task_index_from_pointer(&report.task_path)
            .ok_or_else(|| anyhow::anyhow!("canonical task path is invalid"))?;
        let planned_task = document
            .tasks
            .get(task_index)
            .ok_or_else(|| anyhow::anyhow!("canonical task not found"))?;
        validate_task_evidence(document, task_index, planned_task, &report)?;
        let task = self
            .task
            .get_mut(task_index)
            .ok_or_else(|| anyhow::anyhow!("scheduled task not found"))?;
        anyhow::ensure!(
            task.task_path == report.task_path,
            "task report path does not identify the scheduled task"
        );
        anyhow::ensure!(task.state == PlanTaskState::Active, "task is not active");
        task.state = report.state;
        task.completed_at_ms = Some(now_ms);
        task.completed_subtask_paths = report.completed_subtask_paths;
        task.completed_entity_paths = report.completed_entity_paths;
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
    document: &PlanDocument,
    task_index: usize,
    task: &PlanTask,
    report: &PlanTaskReport,
) -> anyhow::Result<()> {
    let planned_subtask_path = task
        .files
        .iter()
        .enumerate()
        .flat_map(|(file_index, file)| {
            file.subtasks
                .iter()
                .enumerate()
                .map(move |(subtask_index, _)| {
                    subtask_pointer(task_index, file_index, subtask_index)
                })
        })
        .collect::<HashSet<_>>();
    let entity_index_by_name = document
        .entity_changes
        .iter()
        .enumerate()
        .map(|(entity_index, entity)| (entity.name.as_str(), entity_index))
        .collect::<std::collections::HashMap<_, _>>();
    let planned_entity_path = task
        .files
        .iter()
        .flat_map(|file| &file.subtasks)
        .flat_map(PlanSubtask::owned_entities)
        .filter_map(|entity_name| entity_index_by_name.get(entity_name.as_str()).copied())
        .map(entity_pointer)
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
    let planned_test_subtask_path = task
        .files
        .iter()
        .enumerate()
        .flat_map(|(file_index, file)| {
            file.subtasks
                .iter()
                .enumerate()
                .filter(|(_, subtask)| subtask.test().is_some())
                .map(move |(subtask_index, _)| {
                    subtask_pointer(task_index, file_index, subtask_index)
                })
        })
        .collect::<HashSet<_>>();

    anyhow::ensure!(
        report
            .completed_subtask_paths
            .iter()
            .all(|path| planned_subtask_path.contains(path)),
        "task report contains an unknown subtask"
    );
    anyhow::ensure!(
        report
            .completed_entity_paths
            .iter()
            .all(|path| planned_entity_path.contains(path)),
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
                .test_subtask_path
                .as_deref()
                .is_none_or(|path| planned_test_subtask_path.contains(path))
        }),
        "task report contains a test outside the active task"
    );
    if report.state == PlanTaskState::Complete {
        let completed_subtask_path = report
            .completed_subtask_paths
            .iter()
            .map(String::as_str)
            .collect::<HashSet<_>>();
        let completed_entity_path = report
            .completed_entity_paths
            .iter()
            .map(String::as_str)
            .collect::<HashSet<_>>();
        anyhow::ensure!(
            planned_subtask_path
                .iter()
                .map(String::as_str)
                .collect::<HashSet<_>>()
                == completed_subtask_path,
            "complete task report must account for every subtask"
        );
        anyhow::ensure!(
            planned_entity_path
                .iter()
                .map(String::as_str)
                .collect::<HashSet<_>>()
                == completed_entity_path,
            "complete task report must account for every entity"
        );
    }
    Ok(())
}

fn task_pointer(task_index: usize) -> String {
    format!("/tasks/{task_index}")
}

fn task_index_from_pointer(pointer: &str) -> Option<usize> {
    pointer.strip_prefix("/tasks/")?.parse().ok()
}

fn subtask_pointer(task_index: usize, file_index: usize, subtask_index: usize) -> String {
    format!("/tasks/{task_index}/files/{file_index}/subtasks/{subtask_index}")
}

fn entity_pointer(entity_index: usize) -> String {
    format!("/entity_changes/{entity_index}")
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn schedules_complete_tasks_while_retaining_leaf_evidence() {
        let document = super::super::document::test_fixture("plan", "Overview");
        let mut scheduler = PlanScheduler::activate(&document);
        assert_eq!(
            scheduler.next_task(&document, 10).unwrap().title,
            "Create plan state"
        );
        scheduler
            .apply_report(
                &document,
                PlanTaskReport {
                    execution_id: "execution".into(),
                    task_path: "/tasks/0".into(),
                    state: PlanTaskState::Complete,
                    completed_subtask_paths: vec!["/tasks/0/files/0/subtasks/0".into()],
                    completed_entity_paths: vec!["/entity_changes/0".into()],
                    test_results: Vec::new(),
                    changed_paths: vec!["src/plan.rs".into()],
                    summary: Some("Complete".into()),
                    blocking_reason: None,
                },
                20,
            )
            .unwrap();
        assert!(scheduler.is_complete());
        assert_eq!(
            scheduler.task[0].completed_subtask_paths,
            ["/tasks/0/files/0/subtasks/0"]
        );
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
                    task_path: "/tasks/0".into(),
                    state: PlanTaskState::Complete,
                    completed_subtask_paths: vec![
                        "/tasks/0/files/0/subtasks/0".into(),
                        "/tasks/0/files/0/subtasks/1".into(),
                    ],
                    completed_entity_paths: vec!["/entity_changes/0".into()],
                    test_results: vec![PlanTestResult {
                        test_subtask_path: Some("/tasks/0/files/0/subtasks/1".into()),
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
            scheduler.task[0].test_results[0]
                .test_subtask_path
                .as_deref(),
            Some("/tasks/0/files/0/subtasks/1")
        );
    }

    #[test]
    fn timestamps_each_whole_task_once_across_scheduler_transitions() {
        let mut document = super::super::document::test_fixture("plan", "Overview");
        let mut second_task = document.tasks[0].clone();
        second_task.title = "Second task".into();
        document.tasks.push(second_task);
        let mut scheduler = PlanScheduler::activate(&document);
        scheduler.next_task(&document, 10).unwrap();
        let next_task = scheduler
            .apply_report(
                &document,
                PlanTaskReport {
                    execution_id: "execution".into(),
                    task_path: "/tasks/0".into(),
                    state: PlanTaskState::Complete,
                    completed_subtask_paths: vec!["/tasks/0/files/0/subtasks/0".into()],
                    completed_entity_paths: vec!["/entity_changes/0".into()],
                    test_results: Vec::new(),
                    changed_paths: Vec::new(),
                    summary: Some("Complete".into()),
                    blocking_reason: None,
                },
                20,
            )
            .unwrap()
            .unwrap();

        assert_eq!(next_task.title, "Second task");
        assert_eq!(scheduler.task[0].started_at_ms, Some(10));
        assert_eq!(scheduler.task[0].completed_at_ms, Some(20));
        assert_eq!(scheduler.task[1].started_at_ms, Some(20));
        assert_eq!(scheduler.task[1].completed_at_ms, None);
        assert_eq!(scheduler.task[1].state, PlanTaskState::Active);
    }
}
