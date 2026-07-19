use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use std::collections::HashSet;
use std::path::{Component, Path};

/// Defines whether one planned construct enters, changes, or leaves the codebase.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum PlanAction {
    Add,
    Modify,
    Remove,
}

/// Defines the caller-facing example represented by one plan.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum PlanUsageKind {
    Command,
    Code,
    Interaction,
    Omitted,
}

/// Represents one concrete caller interaction and its observable result.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct PlanUsage {
    pub kind: PlanUsageKind,
    pub input: String,
    pub expected_result: String,
}

/// Defines one object-model construct displayed in the plan UML.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum DefinitionKind {
    Trait,
    Interface,
    AbstractClass,
    Class,
    Struct,
    Enum,
    Config,
    Resource,
    TypeAlias,
}

/// Defines one member role inside an object-model definition.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum MemberKind {
    Field,
    Method,
    Function,
    Constant,
    Property,
}

/// Defines the reviewer-visible visibility of one planned member.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Ord, PartialOrd, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum Visibility {
    Public,
    Protected,
    Internal,
    Private,
}

/// Represents one ordered function or method parameter.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct FunctionParameter {
    pub name: String,
    pub type_name: String,
}

/// Represents one field exposed by an enum variant.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct VariantField {
    pub name: String,
    pub type_name: String,
}

/// Represents one named enum state and its contract data.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct EnumVariant {
    pub id: String,
    pub name: String,
    #[serde(default)]
    pub fields: Vec<VariantField>,
}

/// Represents one field or operation nested inside a planned definition.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct TypeMember {
    pub id: String,
    pub action: PlanAction,
    pub kind: MemberKind,
    pub name: String,
    pub visibility: Visibility,
    pub type_name: Option<String>,
    #[serde(default)]
    pub parameters: Vec<FunctionParameter>,
    pub return_type: Option<String>,
}

/// Represents one contract or concrete type in the plan object model.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct TypeDefinition {
    pub id: String,
    pub action: PlanAction,
    pub kind: DefinitionKind,
    pub name: String,
    pub path: String,
    #[serde(default)]
    pub conforms_to: Vec<String>,
    pub extends: Option<String>,
    pub exclusive_parent_id: Option<String>,
    #[serde(default)]
    pub members: Vec<TypeMember>,
    #[serde(default)]
    pub variants: Vec<EnumVariant>,
}

/// Represents one boundary in an affected runtime or data flow.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct PlanFlowStep {
    pub id: String,
    pub action: String,
    pub location: String,
    pub value_to_next: Option<String>,
    pub order: u32,
}

/// Represents one independent runtime, data, request, or recovery flow.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct PlanFlow {
    pub id: String,
    pub title: String,
    pub rationale: String,
    pub order: u32,
    #[serde(default)]
    pub steps: Vec<PlanFlowStep>,
}

/// Defines the concrete code role changed by one plan edit.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum CodeKind {
    Class,
    Struct,
    Enum,
    Trait,
    Interface,
    Test,
    App,
    Config,
    Function,
    Method,
    Constant,
    Field,
    Resource,
    Cache,
    Adapter,
}

/// Represents one concrete construct edit beneath a source-file boundary.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct CodeEdit {
    pub id: String,
    pub action: PlanAction,
    pub kind: CodeKind,
    pub target: String,
    pub description: String,
    pub definition_id: Option<String>,
    pub member_id: Option<String>,
}

/// Represents one local design move inside a source file.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct PlanSubtask {
    pub id: String,
    pub title: String,
    pub detail: String,
    pub order: u32,
    #[serde(default)]
    pub code_edits: Vec<CodeEdit>,
}

/// Represents one concrete source-file boundary owned by a task.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct PlanFile {
    pub path: String,
    #[serde(default)]
    pub subtasks: Vec<PlanSubtask>,
}

/// Represents one architectural execution unit and its complete source subtree.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct PlanTask {
    pub id: String,
    pub title: String,
    pub rationale: String,
    pub order: u32,
    #[serde(default)]
    pub files: Vec<PlanFile>,
}

/// Represents one verification case linked to affected tasks and flows.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct PlanTestCase {
    pub id: String,
    pub title: String,
    pub behavior: String,
    #[serde(default)]
    pub mocks: Vec<String>,
    #[serde(default)]
    pub task_ids: Vec<String>,
    #[serde(default)]
    pub flow_ids: Vec<String>,
}

/// Owns the unit and integration verification cases for one plan.
#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub struct PlanTestPlan {
    #[serde(default)]
    pub unit: Vec<PlanTestCase>,
    #[serde(default)]
    pub integration: Vec<PlanTestCase>,
}

/// Represents one explicit premise that implementation treats as true.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct PlanAssumption {
    pub id: String,
    pub text: String,
}

/// Owns the complete canonical plan consumed by review and execution.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct PlanDocument {
    pub version: u64,
    pub plan_id: String,
    pub title: String,
    pub overview: String,
    pub usage: Option<PlanUsage>,
    #[serde(default)]
    pub definitions: Vec<TypeDefinition>,
    #[serde(default)]
    pub flows: Vec<PlanFlow>,
    #[serde(default)]
    pub tasks: Vec<PlanTask>,
    #[serde(default)]
    pub test_plan: PlanTestPlan,
    #[serde(default)]
    pub assumptions: Vec<PlanAssumption>,
}

impl PlanDocument {
    /// Validate references and stable identifiers after every semantic edit.
    pub fn validate(&self) -> Result<()> {
        required(&self.plan_id, "plan_id")?;
        required(&self.title, "title")?;
        required(&self.overview, "overview")?;

        let definition_ids = unique_ids(
            self.definitions
                .iter()
                .map(|definition| definition.id.as_str()),
            "definition",
        )?;
        for definition in &self.definitions {
            required(&definition.name, "definition name")?;
            validate_repository_path(&definition.path)?;
            unique_ids(
                definition.members.iter().map(|member| member.id.as_str()),
                "definition member",
            )?;
            unique_ids(
                definition
                    .variants
                    .iter()
                    .map(|variant| variant.id.as_str()),
                "enum variant",
            )?;
            if let Some(parent_id) = definition.exclusive_parent_id.as_deref() {
                anyhow::ensure!(parent_id != definition.id, "definition cannot own itself");
                anyhow::ensure!(
                    definition_ids.contains(parent_id),
                    "exclusive parent {parent_id} does not exist"
                );
            }
        }
        validate_exclusive_parent_cycles(&self.definitions)?;

        let flow_ids = unique_ids(self.flows.iter().map(|flow| flow.id.as_str()), "flow")?;
        unique_order(self.flows.iter().map(|flow| flow.order), "flow")?;
        for flow in &self.flows {
            required(&flow.title, "flow title")?;
            unique_ids(flow.steps.iter().map(|step| step.id.as_str()), "flow step")?;
            unique_order(flow.steps.iter().map(|step| step.order), "flow step")?;
        }

        let task_ids = unique_ids(self.tasks.iter().map(|task| task.id.as_str()), "task")?;
        unique_order(self.tasks.iter().map(|task| task.order), "task")?;
        for task in &self.tasks {
            required(&task.title, "task title")?;
            required(&task.rationale, "task rationale")?;
            let mut path_set = HashSet::new();
            for file in &task.files {
                validate_repository_path(&file.path)?;
                anyhow::ensure!(
                    path_set.insert(file.path.as_str()),
                    "duplicate task file path {}",
                    file.path
                );
                unique_ids(
                    file.subtasks.iter().map(|subtask| subtask.id.as_str()),
                    "subtask",
                )?;
                unique_order(file.subtasks.iter().map(|subtask| subtask.order), "subtask")?;
                for subtask in &file.subtasks {
                    required(&subtask.title, "subtask title")?;
                    required(&subtask.detail, "subtask detail")?;
                    unique_ids(
                        subtask.code_edits.iter().map(|edit| edit.id.as_str()),
                        "code edit",
                    )?;
                    for edit in &subtask.code_edits {
                        required(&edit.target, "code edit target")?;
                        required(&edit.description, "code edit description")?;
                        if let Some(definition_id) = edit.definition_id.as_deref() {
                            anyhow::ensure!(
                                definition_ids.contains(definition_id),
                                "code edit definition {definition_id} does not exist"
                            );
                            if let Some(member_id) = edit.member_id.as_deref() {
                                let definition = self
                                    .definitions
                                    .iter()
                                    .find(|definition| definition.id == definition_id)
                                    .context("code edit definition disappeared")?;
                                anyhow::ensure!(
                                    definition
                                        .members
                                        .iter()
                                        .any(|member| member.id == member_id),
                                    "code edit member {member_id} does not exist"
                                );
                            }
                        } else {
                            anyhow::ensure!(
                                edit.member_id.is_none(),
                                "code edit member requires a definition"
                            );
                        }
                    }
                }
            }
        }

        let test_case_list = self
            .test_plan
            .unit
            .iter()
            .chain(self.test_plan.integration.iter());
        unique_ids(
            test_case_list.clone().map(|test| test.id.as_str()),
            "test case",
        )?;
        for test in test_case_list {
            required(&test.title, "test title")?;
            required(&test.behavior, "test behavior")?;
            for task_id in &test.task_ids {
                anyhow::ensure!(
                    task_ids.contains(task_id.as_str()),
                    "test task {task_id} does not exist"
                );
            }
            for flow_id in &test.flow_ids {
                anyhow::ensure!(
                    flow_ids.contains(flow_id.as_str()),
                    "test flow {flow_id} does not exist"
                );
            }
        }
        unique_ids(
            self.assumptions
                .iter()
                .map(|assumption| assumption.id.as_str()),
            "assumption",
        )?;
        for assumption in &self.assumptions {
            required(&assumption.text, "assumption text")?;
        }
        Ok(())
    }

    /// Validate that one working document can enter mandatory review.
    pub fn validate_for_submission(&self) -> Result<()> {
        self.validate()?;
        anyhow::ensure!(
            !self.tasks.is_empty(),
            "submitted plan requires at least one task"
        );
        anyhow::ensure!(
            !self.test_plan.unit.is_empty() || !self.test_plan.integration.is_empty(),
            "submitted plan requires at least one test case"
        );
        for task in &self.tasks {
            anyhow::ensure!(!task.files.is_empty(), "task {} requires a file", task.id);
            for file in &task.files {
                anyhow::ensure!(
                    !file.subtasks.is_empty(),
                    "task file {} requires a subtask",
                    file.path
                );
                for subtask in &file.subtasks {
                    anyhow::ensure!(
                        !subtask.code_edits.is_empty(),
                        "subtask {} requires a code edit",
                        subtask.id
                    );
                }
            }
        }
        Ok(())
    }
}

fn required(value: &str, field: &str) -> Result<()> {
    anyhow::ensure!(!value.trim().is_empty(), "{field} cannot be empty");
    Ok(())
}

fn unique_ids<'a>(values: impl Iterator<Item = &'a str>, kind: &str) -> Result<HashSet<&'a str>> {
    let mut result = HashSet::new();
    for value in values {
        required(value, &format!("{kind} id"))?;
        anyhow::ensure!(result.insert(value), "duplicate {kind} id {value}");
    }
    Ok(result)
}

fn unique_order(values: impl Iterator<Item = u32>, kind: &str) -> Result<()> {
    let mut result = HashSet::new();
    for value in values {
        anyhow::ensure!(result.insert(value), "duplicate {kind} order {value}");
    }
    Ok(())
}

fn validate_repository_path(value: &str) -> Result<()> {
    required(value, "repository path")?;
    let path = Path::new(value);
    anyhow::ensure!(
        !path.is_absolute(),
        "repository path must be relative: {value}"
    );
    anyhow::ensure!(
        path.components()
            .all(|component| matches!(component, Component::Normal(_))),
        "repository path cannot escape the workspace: {value}"
    );
    Ok(())
}

fn validate_exclusive_parent_cycles(definitions: &[TypeDefinition]) -> Result<()> {
    for definition in definitions {
        let mut visited = HashSet::new();
        let mut current = definition;
        while let Some(parent_id) = current.exclusive_parent_id.as_deref() {
            anyhow::ensure!(
                visited.insert(parent_id),
                "exclusive parent cycle includes {parent_id}"
            );
            current = definitions
                .iter()
                .find(|candidate| candidate.id == parent_id)
                .context("exclusive parent disappeared")?;
        }
    }
    Ok(())
}

#[cfg(test)]
pub(crate) fn test_fixture(plan_id: &str, overview: &str) -> PlanDocument {
    PlanDocument {
        version: 1,
        plan_id: plan_id.into(),
        title: "Structured plan".into(),
        overview: overview.into(),
        usage: None,
        definitions: Vec::new(),
        flows: vec![PlanFlow {
            id: "flow".into(),
            title: "Execution".into(),
            rationale: "Connect intent to work.".into(),
            order: 1,
            steps: Vec::new(),
        }],
        tasks: vec![PlanTask {
            id: "task".into(),
            title: "Create plan state".into(),
            rationale: "Give planning one owner.".into(),
            order: 1,
            files: vec![PlanFile {
                path: "src/plan.rs".into(),
                subtasks: vec![PlanSubtask {
                    id: "subtask".into(),
                    title: "Create the owner".into(),
                    detail: "Keep state durable.".into(),
                    order: 1,
                    code_edits: vec![CodeEdit {
                        id: "edit".into(),
                        action: PlanAction::Add,
                        kind: CodeKind::Struct,
                        target: "PlanDocument".into(),
                        description: "Own canonical planning data.".into(),
                        definition_id: None,
                        member_id: None,
                    }],
                }],
            }],
        }],
        test_plan: PlanTestPlan {
            unit: vec![PlanTestCase {
                id: "test".into(),
                title: "Validates plans".into(),
                behavior: "Reject malformed plans.".into(),
                mocks: Vec::new(),
                task_ids: vec!["task".into()],
                flow_ids: vec!["flow".into()],
            }],
            integration: Vec::new(),
        },
        assumptions: Vec::new(),
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn document() -> PlanDocument {
        test_fixture("plan", "Build structured planning.")
    }

    #[test]
    fn validates_complete_submission() {
        document().validate_for_submission().unwrap();
    }

    #[test]
    fn rejects_parent_paths_and_duplicate_order() {
        let mut value = document();
        value.tasks[0].files[0].path = "../outside.rs".into();
        assert!(value.validate().unwrap_err().to_string().contains("escape"));
        value.tasks[0].files[0].path = "src/plan.rs".into();
        value.flows.push(value.flows[0].clone());
        value.flows[1].id = "other".into();
        assert!(value.validate().unwrap_err().to_string().contains("order"));
    }
}
