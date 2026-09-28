use super::document::{PlanDocument, PlanSubtask, PlanTask, ProgramEntityChange};
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
    pub task_id: String,
    pub stage_id: String,
    pub plan_version: u64,
    pub definition: PlanTask,
    pub entity_changes: Vec<ProgramEntityChange>,
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
    pub task_id: String,
    pub plan_version: u64,
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
                .tasks()
                .enumerate()
                .map(|(task_index, task)| PlanTaskExecution {
                    task_id: task.id.clone(),
                    stage_id: document
                        .stages
                        .iter()
                        .find(|stage| stage.tasks.iter().any(|candidate| candidate.id == task.id))
                        .expect("task belongs to stage")
                        .id
                        .clone(),
                    plan_version: document.version,
                    definition: task.clone(),
                    entity_changes: document
                        .entity_changes
                        .iter()
                        .filter(|entity| {
                            task.files
                                .iter()
                                .flat_map(|file| &file.subtasks)
                                .flat_map(PlanSubtask::owned_entities)
                                .any(|name| name == &entity.name)
                        })
                        .cloned()
                        .collect(),
                    task_path: document.task_path(task_index),
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
        if self
            .task
            .iter()
            .any(|task| matches!(task.state, PlanTaskState::Active | PlanTaskState::Blocked))
        {
            return None;
        }
        let (task_index, execution) = self
            .task
            .iter_mut()
            .enumerate()
            .find(|(_, task)| task.state == PlanTaskState::Pending)?;
        execution.state = PlanTaskState::Active;
        execution.started_at_ms = Some(now_ms);
        document.tasks().nth(task_index)
    }

    /// Adopt validated remaining work without reassigning persisted completion.
    pub fn reconcile(&mut self, document: &PlanDocument) -> anyhow::Result<()> {
        document.validate_for_submission()?;
        anyhow::ensure!(
            self.plan_id == document.plan_id,
            "cannot reconcile a different plan"
        );
        let mut updated = Self::activate(document);
        for stage in &document.stages {
            let previous = self
                .task
                .iter()
                .filter(|task| task.stage_id == stage.id)
                .collect::<Vec<_>>();
            if !previous.is_empty()
                && previous
                    .iter()
                    .all(|task| task.state == PlanTaskState::Complete)
            {
                anyhow::ensure!(
                    stage.tasks.len() == previous.len(),
                    "cannot add work to a completed stage"
                );
            }
        }
        for (ordinal, prior) in self.task.iter().enumerate() {
            let next = updated
                .task
                .iter_mut()
                .find(|task| task.task_id == prior.task_id);
            if prior.state == PlanTaskState::Complete {
                let next = next.ok_or_else(|| {
                    anyhow::anyhow!("cannot remove completed task {}", prior.task_id)
                })?;
                anyhow::ensure!(
                    next.definition == prior.definition
                        && next.entity_changes == prior.entity_changes
                        && next.task_path == prior.task_path
                        && next.stage_id == prior.stage_id,
                    "cannot redefine or move completed task {}",
                    prior.task_id
                );
                anyhow::ensure!(
                    document
                        .tasks()
                        .nth(ordinal)
                        .is_some_and(|task| task.id == prior.task_id),
                    "cannot insert unfinished work before completed tasks"
                );
                *next = prior.clone();
            } else if let Some(next) = next {
                let path = next.task_path.clone();
                if matches!(prior.state, PlanTaskState::Active | PlanTaskState::Blocked) {
                    anyhow::ensure!(path == prior.task_path, "cannot move the active task");
                }
                if next.definition == prior.definition
                    && next.entity_changes == prior.entity_changes
                    && path == prior.task_path
                {
                    *next = prior.clone();
                    next.plan_version = document.version;
                } else if matches!(prior.state, PlanTaskState::Active | PlanTaskState::Blocked) {
                    next.state = prior.state;
                    next.started_at_ms = prior.started_at_ms;
                }
            } else {
                anyhow::ensure!(
                    prior.state == PlanTaskState::Pending,
                    "cannot remove the active task"
                );
            }
        }
        *self = updated;
        Ok(())
    }

    /// Reactivate the blocked task while retaining its accumulated execution evidence.
    pub fn resume_blocked_task(&mut self) {
        if self
            .task
            .iter()
            .any(|task| task.state == PlanTaskState::Active)
        {
            return;
        }
        let Some(task) = self
            .task
            .iter_mut()
            .find(|task| task.state == PlanTaskState::Blocked)
        else {
            return;
        };
        task.state = PlanTaskState::Active;
        task.completed_at_ms = None;
        task.blocking_reason = None;
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
        anyhow::ensure!(
            report.plan_version == document.version,
            "task report plan version is stale"
        );
        let task_index = self
            .task
            .iter()
            .position(|task| task.task_id == report.task_id)
            .ok_or_else(|| anyhow::anyhow!("canonical task id is invalid"))?;
        let planned_task = document
            .task_at(&report.task_path)
            .ok_or_else(|| anyhow::anyhow!("canonical task not found"))?;
        anyhow::ensure!(
            planned_task.id == report.task_id,
            "task id and path disagree"
        );
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
                    format!(
                        "{}/files/{file_index}/subtasks/{subtask_index}",
                        document.task_path(task_index)
                    )
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
                    format!(
                        "{}/files/{file_index}/subtasks/{subtask_index}",
                        document.task_path(task_index)
                    )
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

fn entity_pointer(entity_index: usize) -> String {
    format!("/entity_changes/{entity_index}")
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn staged_execution_preserves_identity_and_rejects_stale_evidence() {
        use super::super::document::*;
        let mut document = test_fixture("plan", "Overview");
        let mut stage = PlanStage {
            id: "implementation".into(),
            title: "Implement consumers".into(),
            tasks: Vec::new(),
        };
        for id in ["Reader", "Writer"] {
            let mut entity = document.entity_changes[0].clone();
            entity.name = id.into();
            entity.path = format!("src/{id}.rs");
            let mut task = document.stages[0].tasks[0].clone();
            task.id = id.into();
            task.title = format!("Implement {id}");
            task.requires = vec!["plan-state".into()];
            task.files[0].change = PlanFileChange::Add {
                path: entity.path.clone(),
            };
            *task.files[0].subtasks[0].owned_entities_mut().unwrap() = vec![id.into()];
            stage.tasks.push(task);
            document.entity_changes.push(entity);
        }
        document.stages.push(stage);
        document.validate_for_submission().unwrap();
        let mut scheduler = PlanScheduler::activate(&document);
        scheduler.next_task(&document, 1).unwrap();
        assert!(scheduler.next_task(&document, 2).is_none());
        for ordinal in 0..3 {
            let task = document.tasks().nth(ordinal).unwrap();
            let report = PlanTaskReport {
                execution_id: "execution".into(),
                task_id: task.id.clone(),
                plan_version: document.version,
                task_path: document.task_path(ordinal),
                state: PlanTaskState::Complete,
                completed_subtask_paths: vec![format!(
                    "{}/files/0/subtasks/0",
                    document.task_path(ordinal)
                )],
                completed_entity_paths: vec![format!("/entity_changes/{ordinal}")],
                test_results: Vec::new(),
                changed_paths: vec![task.files[0].change.path().into()],
                summary: Some("Complete".into()),
                blocking_reason: None,
            };
            let mut stale = report.clone();
            stale.plan_version = 0;
            assert!(scheduler.apply_report(&document, stale, 3).is_err());
            if ordinal == 1 {
                assert_eq!(document.task_label(&task.id).as_deref(), Some("2a"));
                let mut changed = document.clone();
                changed.version += 1;
                changed.stages[1].tasks[0].description =
                    "Implement the revised reader contract.".into();
                scheduler.reconcile(&changed).unwrap();
                assert!(
                    scheduler
                        .apply_report(&document, report.clone(), 3)
                        .is_err()
                );
                scheduler.reconcile(&document).unwrap();
            }
            scheduler
                .apply_report(&document, report, ordinal as i64 + 3)
                .unwrap();
            scheduler = serde_json::from_value(serde_json::to_value(&scheduler).unwrap()).unwrap();
        }
        assert!(scheduler.is_complete());
        let mut altered = document.clone();
        altered.stages[0].tasks[0].description = "Different completed work".into();
        assert!(scheduler.reconcile(&altered).is_err());
        assert_eq!(scheduler.task[0].definition, document.stages[0].tasks[0]);
    }

    #[test]
    fn resumption_reactivates_the_blocked_task_without_skipping_or_erasing_evidence() {
        let mut document = super::super::document::test_fixture("plan", "Overview");
        let mut second = document.stages[0].tasks[0].clone();
        second.id = "second".into();
        document.stages[0].tasks.push(second);
        let mut scheduler = PlanScheduler::activate(&document);
        scheduler.next_task(&document, 10).unwrap();
        let mut report = PlanTaskReport {
            task_id: "plan-state".into(),
            plan_version: 1,
            execution_id: "execution".into(),
            task_path: "/stages/0/tasks/0".into(),
            state: PlanTaskState::Blocked,
            completed_subtask_paths: vec!["/stages/0/tasks/0/files/0/subtasks/0".into()],
            completed_entity_paths: vec!["/entity_changes/0".into()],
            test_results: Vec::new(),
            changed_paths: vec!["src/plan.rs".into()],
            summary: Some("partial work".into()),
            blocking_reason: Some("awaiting input".into()),
        };
        scheduler
            .apply_report(&document, report.clone(), 20)
            .unwrap();
        scheduler.resume_blocked_task();
        assert_eq!(scheduler.task[0].state, PlanTaskState::Active);
        assert_eq!(scheduler.task[0].started_at_ms, Some(10));
        assert_eq!(scheduler.task[0].completed_at_ms, None);
        assert_eq!(scheduler.task[0].blocking_reason, None);
        assert_eq!(
            scheduler.task[0].completed_entity_paths,
            report.completed_entity_paths
        );
        assert_eq!(scheduler.task[0].changed_paths, report.changed_paths);
        assert_eq!(scheduler.task[0].summary, report.summary);
        assert_eq!(scheduler.task[1].state, PlanTaskState::Pending);
        report.state = PlanTaskState::Complete;
        report.blocking_reason = None;
        scheduler.apply_report(&document, report, 30).unwrap();
        let settled = serde_json::to_value(&scheduler).unwrap();
        scheduler.resume_blocked_task();
        assert_eq!(serde_json::to_value(&scheduler).unwrap(), settled);
        assert_eq!(scheduler.task[0].state, PlanTaskState::Complete);
        assert_eq!(scheduler.task[1].state, PlanTaskState::Active);
    }

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
                    task_id: "plan-state".into(),
                    plan_version: 1,
                    execution_id: "execution".into(),
                    task_path: "/stages/0/tasks/0".into(),
                    state: PlanTaskState::Complete,
                    completed_subtask_paths: vec!["/stages/0/tasks/0/files/0/subtasks/0".into()],
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
            ["/stages/0/tasks/0/files/0/subtasks/0"]
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
                    task_id: "plan-state".into(),
                    plan_version: 1,
                    execution_id: "execution".into(),
                    task_path: "/stages/0/tasks/0".into(),
                    state: PlanTaskState::Complete,
                    completed_subtask_paths: vec![
                        "/stages/0/tasks/0/files/0/subtasks/0".into(),
                        "/stages/0/tasks/0/files/0/subtasks/1".into(),
                    ],
                    completed_entity_paths: vec!["/entity_changes/0".into()],
                    test_results: vec![PlanTestResult {
                        test_subtask_path: Some("/stages/0/tasks/0/files/0/subtasks/1".into()),
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
            Some("/stages/0/tasks/0/files/0/subtasks/1")
        );
    }

    #[test]
    fn timestamps_each_whole_task_once_across_scheduler_transitions() {
        let mut document = super::super::document::test_fixture("plan", "Overview");
        let mut second_task = document.stages[0].tasks[0].clone();
        second_task.title = "Second task".into();
        second_task.id = "second".into();
        document.stages[0].tasks.push(second_task);
        let mut scheduler = PlanScheduler::activate(&document);
        scheduler.next_task(&document, 10).unwrap();
        let next_task = scheduler
            .apply_report(
                &document,
                PlanTaskReport {
                    task_id: "plan-state".into(),
                    plan_version: 1,
                    execution_id: "execution".into(),
                    task_path: "/stages/0/tasks/0".into(),
                    state: PlanTaskState::Complete,
                    completed_subtask_paths: vec!["/stages/0/tasks/0/files/0/subtasks/0".into()],
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
