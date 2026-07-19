use super::document::*;
use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};

/// Selects the verification collection changed by one test operation.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum TestCategory {
    Unit,
    Integration,
}

/// Carries editable definition fields while preserving its stable identifier.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct DefinitionChanges {
    pub action: Option<PlanAction>,
    pub kind: Option<DefinitionKind>,
    pub name: Option<String>,
    pub path: Option<String>,
    pub conforms_to: Option<Vec<String>>,
    pub extends: Option<Option<String>>,
    pub exclusive_parent_id: Option<Option<String>>,
    pub members: Option<Vec<TypeMember>>,
    pub variants: Option<Vec<EnumVariant>>,
}

/// Carries editable flow fields while preserving its stable identifier.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct FlowChanges {
    pub title: Option<String>,
    pub rationale: Option<String>,
    pub order: Option<u32>,
}

/// Carries editable flow-step fields while preserving its stable identifier.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct FlowStepChanges {
    pub action: Option<String>,
    pub location: Option<String>,
    pub value_to_next: Option<Option<String>>,
    pub order: Option<u32>,
}

/// Carries editable task fields while preserving its stable identifier.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct TaskChanges {
    pub title: Option<String>,
    pub rationale: Option<String>,
    pub order: Option<u32>,
}

/// Carries an optional replacement path for one source boundary.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct FileChanges {
    pub path: Option<String>,
}

/// Carries editable subtask fields while preserving its stable identifier.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct SubtaskChanges {
    pub title: Option<String>,
    pub detail: Option<String>,
    pub order: Option<u32>,
}

/// Carries editable code-edit fields while preserving its stable identifier.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct CodeEditChanges {
    pub action: Option<PlanAction>,
    pub kind: Option<CodeKind>,
    pub target: Option<String>,
    pub description: Option<String>,
    pub definition_id: Option<Option<String>>,
    pub member_id: Option<Option<String>>,
}

/// Carries editable test fields while preserving its stable identifier.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct TestCaseChanges {
    pub title: Option<String>,
    pub behavior: Option<String>,
    pub mocks: Option<Vec<String>>,
    pub task_ids: Option<Vec<String>>,
    pub flow_ids: Option<Vec<String>>,
}

/// Defines one semantic mutation against the canonical plan tree.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(tag = "operation", rename_all = "snake_case")]
pub enum PlanEditOperation {
    #[serde(rename = "document.replace")]
    DocumentReplace { document: PlanDocument },
    #[serde(rename = "overview.update")]
    OverviewUpdate { text: String },
    #[serde(rename = "usage.update")]
    UsageUpdate { usage: Option<PlanUsage> },
    #[serde(rename = "definition.add")]
    DefinitionAdd { definition: TypeDefinition },
    #[serde(rename = "definition.update")]
    DefinitionUpdate {
        definition_id: String,
        changes: DefinitionChanges,
    },
    #[serde(rename = "definition.remove")]
    DefinitionRemove { definition_id: String },
    #[serde(rename = "flow.add")]
    FlowAdd { flow: PlanFlow },
    #[serde(rename = "flow.update")]
    FlowUpdate {
        flow_id: String,
        changes: FlowChanges,
    },
    #[serde(rename = "flow.remove")]
    FlowRemove { flow_id: String },
    #[serde(rename = "flow_step.add")]
    FlowStepAdd { flow_id: String, step: PlanFlowStep },
    #[serde(rename = "flow_step.update")]
    FlowStepUpdate {
        flow_id: String,
        step_id: String,
        changes: FlowStepChanges,
    },
    #[serde(rename = "flow_step.remove")]
    FlowStepRemove { flow_id: String, step_id: String },
    #[serde(rename = "task.add")]
    TaskAdd { task: PlanTask },
    #[serde(rename = "task.update")]
    TaskUpdate {
        task_id: String,
        changes: TaskChanges,
    },
    #[serde(rename = "task.remove")]
    TaskRemove { task_id: String },
    #[serde(rename = "file.add")]
    FileAdd { task_id: String, file: PlanFile },
    #[serde(rename = "file.update")]
    FileUpdate {
        task_id: String,
        path: String,
        changes: FileChanges,
    },
    #[serde(rename = "file.remove")]
    FileRemove { task_id: String, path: String },
    #[serde(rename = "subtask.add")]
    SubtaskAdd {
        task_id: String,
        path: String,
        subtask: PlanSubtask,
    },
    #[serde(rename = "subtask.update")]
    SubtaskUpdate {
        task_id: String,
        path: String,
        subtask_id: String,
        changes: SubtaskChanges,
    },
    #[serde(rename = "subtask.remove")]
    SubtaskRemove {
        task_id: String,
        path: String,
        subtask_id: String,
    },
    #[serde(rename = "code_edit.add")]
    CodeEditAdd {
        task_id: String,
        path: String,
        subtask_id: String,
        code_edit: CodeEdit,
    },
    #[serde(rename = "code_edit.update")]
    CodeEditUpdate {
        task_id: String,
        path: String,
        subtask_id: String,
        code_edit_id: String,
        changes: CodeEditChanges,
    },
    #[serde(rename = "code_edit.remove")]
    CodeEditRemove {
        task_id: String,
        path: String,
        subtask_id: String,
        code_edit_id: String,
    },
    #[serde(rename = "test_case.add")]
    TestCaseAdd {
        category: TestCategory,
        test_case: PlanTestCase,
    },
    #[serde(rename = "test_case.update")]
    TestCaseUpdate {
        category: TestCategory,
        test_case_id: String,
        changes: TestCaseChanges,
    },
    #[serde(rename = "test_case.remove")]
    TestCaseRemove {
        category: TestCategory,
        test_case_id: String,
    },
    #[serde(rename = "assumption.add")]
    AssumptionAdd { assumption: PlanAssumption },
    #[serde(rename = "assumption.update")]
    AssumptionUpdate { assumption_id: String, text: String },
    #[serde(rename = "assumption.remove")]
    AssumptionRemove { assumption_id: String },
}

/// Represents one optimistic atomic edit request.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct PlanEditRequest {
    pub plan_id: String,
    pub expected_version: u64,
    pub operations: Vec<PlanEditOperation>,
}

/// Reports the canonical document after one committed edit batch.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct PlanEditResult {
    pub plan_id: String,
    pub version: u64,
    pub document: PlanDocument,
}

/// Apply one semantic operation batch without exposing partial mutation.
pub fn apply_plan_edit(
    document: &PlanDocument,
    request: PlanEditRequest,
) -> Result<PlanEditResult> {
    anyhow::ensure!(
        request.plan_id == document.plan_id,
        "plan id does not match active document"
    );
    anyhow::ensure!(
        request.expected_version == document.version,
        "plan version conflict: expected {}, current {}",
        request.expected_version,
        document.version
    );
    anyhow::ensure!(
        !request.operations.is_empty(),
        "plan edit requires an operation"
    );
    let mut updated = document.clone();
    for operation in request.operations {
        apply_operation(&mut updated, operation)?;
    }
    updated.plan_id.clone_from(&document.plan_id);
    updated.version = document.version.saturating_add(1);
    updated.validate()?;
    Ok(PlanEditResult {
        plan_id: updated.plan_id.clone(),
        version: updated.version,
        document: updated,
    })
}

fn apply_operation(document: &mut PlanDocument, operation: PlanEditOperation) -> Result<()> {
    match operation {
        PlanEditOperation::DocumentReplace {
            document: mut replacement,
        } => {
            anyhow::ensure!(
                replacement.plan_id == document.plan_id,
                "document replacement cannot change plan id"
            );
            replacement.version = document.version;
            *document = replacement;
        }
        PlanEditOperation::OverviewUpdate { text } => document.overview = text,
        PlanEditOperation::UsageUpdate { usage } => document.usage = usage,
        PlanEditOperation::DefinitionAdd { definition } => document.definitions.push(definition),
        PlanEditOperation::DefinitionUpdate {
            definition_id,
            changes,
        } => {
            apply_definition_changes(find_definition(document, &definition_id)?, changes);
        }
        PlanEditOperation::DefinitionRemove { definition_id } => {
            remove_by_id(
                &mut document.definitions,
                &definition_id,
                |value| &value.id,
                "definition",
            )?;
        }
        PlanEditOperation::FlowAdd { flow } => document.flows.push(flow),
        PlanEditOperation::FlowUpdate { flow_id, changes } => {
            apply_flow_changes(find_flow(document, &flow_id)?, changes)
        }
        PlanEditOperation::FlowRemove { flow_id } => {
            remove_by_id(&mut document.flows, &flow_id, |value| &value.id, "flow")?
        }
        PlanEditOperation::FlowStepAdd { flow_id, step } => {
            find_flow(document, &flow_id)?.steps.push(step)
        }
        PlanEditOperation::FlowStepUpdate {
            flow_id,
            step_id,
            changes,
        } => {
            let step = find_flow(document, &flow_id)?
                .steps
                .iter_mut()
                .find(|step| step.id == step_id)
                .context("flow step not found")?;
            apply_flow_step_changes(step, changes);
        }
        PlanEditOperation::FlowStepRemove { flow_id, step_id } => {
            remove_by_id(
                &mut find_flow(document, &flow_id)?.steps,
                &step_id,
                |value| &value.id,
                "flow step",
            )?;
        }
        PlanEditOperation::TaskAdd { task } => document.tasks.push(task),
        PlanEditOperation::TaskUpdate { task_id, changes } => {
            apply_task_changes(find_task(document, &task_id)?, changes)
        }
        PlanEditOperation::TaskRemove { task_id } => {
            remove_by_id(&mut document.tasks, &task_id, |value| &value.id, "task")?
        }
        PlanEditOperation::FileAdd { task_id, file } => {
            find_task(document, &task_id)?.files.push(file)
        }
        PlanEditOperation::FileUpdate {
            task_id,
            path,
            changes,
        } => {
            let file = find_file(document, &task_id, &path)?;
            if let Some(path) = changes.path {
                file.path = path;
            }
        }
        PlanEditOperation::FileRemove { task_id, path } => {
            let file_list = &mut find_task(document, &task_id)?.files;
            remove_by_id(file_list, &path, |value| &value.path, "file")?;
        }
        PlanEditOperation::SubtaskAdd {
            task_id,
            path,
            subtask,
        } => find_file(document, &task_id, &path)?.subtasks.push(subtask),
        PlanEditOperation::SubtaskUpdate {
            task_id,
            path,
            subtask_id,
            changes,
        } => {
            apply_subtask_changes(
                find_subtask(document, &task_id, &path, &subtask_id)?,
                changes,
            );
        }
        PlanEditOperation::SubtaskRemove {
            task_id,
            path,
            subtask_id,
        } => {
            remove_by_id(
                &mut find_file(document, &task_id, &path)?.subtasks,
                &subtask_id,
                |value| &value.id,
                "subtask",
            )?;
        }
        PlanEditOperation::CodeEditAdd {
            task_id,
            path,
            subtask_id,
            code_edit,
        } => {
            find_subtask(document, &task_id, &path, &subtask_id)?
                .code_edits
                .push(code_edit);
        }
        PlanEditOperation::CodeEditUpdate {
            task_id,
            path,
            subtask_id,
            code_edit_id,
            changes,
        } => {
            let edit = find_subtask(document, &task_id, &path, &subtask_id)?
                .code_edits
                .iter_mut()
                .find(|edit| edit.id == code_edit_id)
                .context("code edit not found")?;
            apply_code_edit_changes(edit, changes);
        }
        PlanEditOperation::CodeEditRemove {
            task_id,
            path,
            subtask_id,
            code_edit_id,
        } => {
            remove_by_id(
                &mut find_subtask(document, &task_id, &path, &subtask_id)?.code_edits,
                &code_edit_id,
                |value| &value.id,
                "code edit",
            )?;
        }
        PlanEditOperation::TestCaseAdd {
            category,
            test_case,
        } => test_case_list_mut(document, category).push(test_case),
        PlanEditOperation::TestCaseUpdate {
            category,
            test_case_id,
            changes,
        } => {
            let test = test_case_list_mut(document, category)
                .iter_mut()
                .find(|test| test.id == test_case_id)
                .context("test case not found")?;
            apply_test_case_changes(test, changes);
        }
        PlanEditOperation::TestCaseRemove {
            category,
            test_case_id,
        } => {
            remove_by_id(
                test_case_list_mut(document, category),
                &test_case_id,
                |value| &value.id,
                "test case",
            )?;
        }
        PlanEditOperation::AssumptionAdd { assumption } => document.assumptions.push(assumption),
        PlanEditOperation::AssumptionUpdate {
            assumption_id,
            text,
        } => {
            document
                .assumptions
                .iter_mut()
                .find(|assumption| assumption.id == assumption_id)
                .context("assumption not found")?
                .text = text;
        }
        PlanEditOperation::AssumptionRemove { assumption_id } => {
            remove_by_id(
                &mut document.assumptions,
                &assumption_id,
                |value| &value.id,
                "assumption",
            )?;
        }
    }
    Ok(())
}

fn find_definition<'a>(document: &'a mut PlanDocument, id: &str) -> Result<&'a mut TypeDefinition> {
    document
        .definitions
        .iter_mut()
        .find(|value| value.id == id)
        .context("definition not found")
}

fn find_flow<'a>(document: &'a mut PlanDocument, id: &str) -> Result<&'a mut PlanFlow> {
    document
        .flows
        .iter_mut()
        .find(|value| value.id == id)
        .context("flow not found")
}

fn find_task<'a>(document: &'a mut PlanDocument, id: &str) -> Result<&'a mut PlanTask> {
    document
        .tasks
        .iter_mut()
        .find(|value| value.id == id)
        .context("task not found")
}

fn find_file<'a>(
    document: &'a mut PlanDocument,
    task_id: &str,
    path: &str,
) -> Result<&'a mut PlanFile> {
    find_task(document, task_id)?
        .files
        .iter_mut()
        .find(|value| value.path == path)
        .context("plan file not found")
}

fn find_subtask<'a>(
    document: &'a mut PlanDocument,
    task_id: &str,
    path: &str,
    subtask_id: &str,
) -> Result<&'a mut PlanSubtask> {
    find_file(document, task_id, path)?
        .subtasks
        .iter_mut()
        .find(|value| value.id == subtask_id)
        .context("subtask not found")
}

fn remove_by_id<T>(
    values: &mut Vec<T>,
    id: &str,
    resolve: impl Fn(&T) -> &String,
    kind: &str,
) -> Result<()> {
    let index = values
        .iter()
        .position(|value| resolve(value) == id)
        .with_context(|| format!("{kind} not found"))?;
    values.remove(index);
    Ok(())
}

fn apply_definition_changes(value: &mut TypeDefinition, changes: DefinitionChanges) {
    if let Some(action) = changes.action {
        value.action = action;
    }
    if let Some(kind) = changes.kind {
        value.kind = kind;
    }
    if let Some(name) = changes.name {
        value.name = name;
    }
    if let Some(path) = changes.path {
        value.path = path;
    }
    if let Some(conforms_to) = changes.conforms_to {
        value.conforms_to = conforms_to;
    }
    if let Some(extends) = changes.extends {
        value.extends = extends;
    }
    if let Some(parent) = changes.exclusive_parent_id {
        value.exclusive_parent_id = parent;
    }
    if let Some(members) = changes.members {
        value.members = members;
    }
    if let Some(variants) = changes.variants {
        value.variants = variants;
    }
}

fn apply_flow_changes(value: &mut PlanFlow, changes: FlowChanges) {
    if let Some(title) = changes.title {
        value.title = title;
    }
    if let Some(rationale) = changes.rationale {
        value.rationale = rationale;
    }
    if let Some(order) = changes.order {
        value.order = order;
    }
}

fn apply_flow_step_changes(value: &mut PlanFlowStep, changes: FlowStepChanges) {
    if let Some(action) = changes.action {
        value.action = action;
    }
    if let Some(location) = changes.location {
        value.location = location;
    }
    if let Some(output) = changes.value_to_next {
        value.value_to_next = output;
    }
    if let Some(order) = changes.order {
        value.order = order;
    }
}

fn apply_task_changes(value: &mut PlanTask, changes: TaskChanges) {
    if let Some(title) = changes.title {
        value.title = title;
    }
    if let Some(rationale) = changes.rationale {
        value.rationale = rationale;
    }
    if let Some(order) = changes.order {
        value.order = order;
    }
}

fn apply_subtask_changes(value: &mut PlanSubtask, changes: SubtaskChanges) {
    if let Some(title) = changes.title {
        value.title = title;
    }
    if let Some(detail) = changes.detail {
        value.detail = detail;
    }
    if let Some(order) = changes.order {
        value.order = order;
    }
}

fn apply_code_edit_changes(value: &mut CodeEdit, changes: CodeEditChanges) {
    if let Some(action) = changes.action {
        value.action = action;
    }
    if let Some(kind) = changes.kind {
        value.kind = kind;
    }
    if let Some(target) = changes.target {
        value.target = target;
    }
    if let Some(description) = changes.description {
        value.description = description;
    }
    if let Some(definition_id) = changes.definition_id {
        value.definition_id = definition_id;
    }
    if let Some(member_id) = changes.member_id {
        value.member_id = member_id;
    }
}

fn apply_test_case_changes(value: &mut PlanTestCase, changes: TestCaseChanges) {
    if let Some(title) = changes.title {
        value.title = title;
    }
    if let Some(behavior) = changes.behavior {
        value.behavior = behavior;
    }
    if let Some(mocks) = changes.mocks {
        value.mocks = mocks;
    }
    if let Some(task_ids) = changes.task_ids {
        value.task_ids = task_ids;
    }
    if let Some(flow_ids) = changes.flow_ids {
        value.flow_ids = flow_ids;
    }
}

fn test_case_list_mut(
    document: &mut PlanDocument,
    category: TestCategory,
) -> &mut Vec<PlanTestCase> {
    match category {
        TestCategory::Unit => &mut document.test_plan.unit,
        TestCategory::Integration => &mut document.test_plan.integration,
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn empty_document() -> PlanDocument {
        PlanDocument {
            version: 4,
            plan_id: "plan".into(),
            title: "Plan".into(),
            overview: "Initial".into(),
            usage: None,
            definitions: Vec::new(),
            flows: Vec::new(),
            tasks: Vec::new(),
            test_plan: PlanTestPlan::default(),
            assumptions: Vec::new(),
        }
    }

    #[test]
    fn applies_atomic_semantic_edits_and_increments_once() {
        let result = apply_plan_edit(
            &empty_document(),
            PlanEditRequest {
                plan_id: "plan".into(),
                expected_version: 4,
                operations: vec![
                    PlanEditOperation::OverviewUpdate {
                        text: "Changed".into(),
                    },
                    PlanEditOperation::AssumptionAdd {
                        assumption: PlanAssumption {
                            id: "a".into(),
                            text: "Stable".into(),
                        },
                    },
                ],
            },
        )
        .unwrap();
        assert_eq!(result.version, 5);
        assert_eq!(result.document.overview, "Changed");
        assert_eq!(result.document.assumptions.len(), 1);
    }

    #[test]
    fn rejects_stale_versions_before_mutating() {
        let error = apply_plan_edit(
            &empty_document(),
            PlanEditRequest {
                plan_id: "plan".into(),
                expected_version: 3,
                operations: vec![PlanEditOperation::OverviewUpdate {
                    text: "Changed".into(),
                }],
            },
        )
        .unwrap_err();
        assert!(error.to_string().contains("version conflict"));
    }
}
