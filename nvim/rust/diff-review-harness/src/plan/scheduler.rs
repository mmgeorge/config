use super::document::{PlanDocument, PlanTask};
use serde::{Deserialize, Serialize};

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
    pub test_case_id: Option<String>,
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
    pub completed_subtask_ids: Vec<String>,
    #[serde(default)]
    pub completed_code_edit_ids: Vec<String>,
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
    pub completed_code_edit_ids: Vec<String>,
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
        let mut task_list = document.tasks.iter().collect::<Vec<_>>();
        task_list.sort_by_key(|task| task.order);
        Self {
            plan_id: document.plan_id.clone(),
            task: task_list
                .into_iter()
                .map(|task| PlanTaskExecution {
                    task_id: task.id.clone(),
                    state: PlanTaskState::Pending,
                    completed_subtask_ids: Vec::new(),
                    completed_code_edit_ids: Vec::new(),
                    test_results: Vec::new(),
                    changed_paths: Vec::new(),
                    summary: None,
                    blocking_reason: None,
                })
                .collect(),
        }
    }

    /// Select the next incomplete task and activate its complete subtree.
    pub fn next_task<'a>(&mut self, document: &'a PlanDocument) -> Option<&'a PlanTask> {
        let execution = self
            .task
            .iter_mut()
            .find(|task| task.state == PlanTaskState::Pending)?;
        execution.state = PlanTaskState::Active;
        document
            .tasks
            .iter()
            .find(|task| task.id == execution.task_id)
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
    ) -> anyhow::Result<Option<&'a PlanTask>> {
        anyhow::ensure!(
            matches!(
                report.state,
                PlanTaskState::Complete | PlanTaskState::Blocked
            ),
            "task report must complete or block a task"
        );
        let task = self
            .task
            .iter_mut()
            .find(|task| task.task_id == report.task_id)
            .ok_or_else(|| anyhow::anyhow!("scheduled task not found"))?;
        anyhow::ensure!(task.state == PlanTaskState::Active, "task is not active");
        task.state = report.state;
        task.completed_subtask_ids = report.completed_subtask_ids;
        task.completed_code_edit_ids = report.completed_code_edit_ids;
        task.test_results = report.test_results;
        task.changed_paths = report.changed_paths;
        task.summary = report.summary;
        task.blocking_reason = report.blocking_reason;
        if task.state == PlanTaskState::Blocked {
            return Ok(None);
        }
        Ok(self.next_task(document))
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn schedules_complete_tasks_while_retaining_leaf_evidence() {
        let document = super::super::document::test_fixture("plan", "Overview");
        let mut scheduler = PlanScheduler::activate(&document);
        assert_eq!(scheduler.next_task(&document).unwrap().id, "task");
        scheduler
            .apply_report(
                &document,
                PlanTaskReport {
                    execution_id: "execution".into(),
                    task_id: "task".into(),
                    state: PlanTaskState::Complete,
                    completed_subtask_ids: vec!["subtask".into()],
                    completed_code_edit_ids: vec!["edit".into()],
                    test_results: Vec::new(),
                    changed_paths: vec!["src/plan.rs".into()],
                    summary: Some("Complete".into()),
                    blocking_reason: None,
                },
            )
            .unwrap();
        assert!(scheduler.is_complete());
        assert_eq!(scheduler.task[0].completed_subtask_ids, ["subtask"]);
    }
}
