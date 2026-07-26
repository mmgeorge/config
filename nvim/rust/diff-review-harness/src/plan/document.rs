use anyhow::Result;
use serde::{Deserialize, Serialize};

use super::validation::{validate_plan_edit, validate_plan_submission};

/// Defines whether one planned entity enters, changes, or leaves the codebase.
#[derive(Clone, Copy, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum ChangeAction {
    Add,
    Modify,
    Remove,
}

/// Represents one concrete caller interaction and its observable result.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct PlanUsage {
    pub command: String,
    pub expected_result: String,
}

/// Defines the semantic role of one named program entity.
#[derive(Clone, Copy, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum EntityKind {
    Class,
    AbstractClass,
    Struct,
    Enum,
    Trait,
    Interface,
    App,
    Config,
    #[serde(alias = "fn")]
    Function,
    Method,
    Constant,
    Field,
    Resource,
    Cache,
    Adapter,
}

/// Defines one member role inside a program entity.
#[derive(Clone, Copy, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum MemberKind {
    Field,
    Method,
    #[serde(alias = "fn")]
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
    #[serde(rename = "type")]
    pub type_name: String,
}

/// Represents one changed field exposed by an enum variant.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct EnumVariantFieldChange {
    #[serde(default)]
    pub field_id: String,
    pub action: ChangeAction,
    pub name: String,
    #[serde(rename = "type")]
    pub type_name: String,
}

/// Represents one changed enum case and its optional variant fields.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct EnumVariantChange {
    #[serde(default)]
    pub variant_id: String,
    pub action: ChangeAction,
    pub name: String,
    pub description: String,
    #[serde(default)]
    pub fields: Vec<EnumVariantFieldChange>,
}

/// Represents one changed field or operation nested inside a program entity.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct ProgramEntityMemberChange {
    #[serde(default)]
    pub member_id: String,
    pub action: ChangeAction,
    pub kind: MemberKind,
    pub name: String,
    pub description: String,
    pub visibility: Option<Visibility>,
    #[serde(rename = "type")]
    pub type_name: Option<String>,
    #[serde(default)]
    pub parameters: Vec<FunctionParameter>,
    pub return_type: Option<String>,
}

/// Identifies one planned entity or one named external architectural boundary.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case", deny_unknown_fields)]
pub enum EntityReference {
    PlannedEntity { entity: String },
    ExternalEntity { entity: String },
}

/// Represents one named program entity and its planned implementation lifecycle.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct ProgramEntityChange {
    #[serde(default)]
    pub entity_id: String,
    pub action: ChangeAction,
    pub kind: EntityKind,
    pub name: String,
    pub description: String,
    pub path: String,
    #[serde(default)]
    pub members: Vec<ProgramEntityMemberChange>,
    #[serde(default)]
    pub variants: Vec<EnumVariantChange>,
    pub extends: Option<EntityReference>,
    #[serde(default)]
    pub conforms_to: Vec<EntityReference>,
    #[serde(rename = "exclusive_owner_entity")]
    pub exclusive_owner_entity_id: Option<String>,
}

/// Represents one auditable package dependency and its manifest declaration.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct PlanDependencyChange {
    #[serde(default)]
    pub dependency_id: String,
    pub action: ChangeAction,
    pub name: String,
    pub version: String,
    pub manifest: String,
    pub license: Option<String>,
    pub justification: String,
}

/// Defines the verification boundary exercised by one test subtask.
#[derive(Clone, Copy, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum TestCategory {
    Unit,
    Integration,
}

/// Describes independent child work and its result inside one composite flow step.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct PlanFlowOperation {
    #[serde(default)]
    pub operation_id: String,
    pub action: String,
    pub target: EntityReference,
    pub result: Option<String>,
}

/// Defines how one flow value should render and participate in review.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum PlanFlowValueKind {
    Type,
    Text,
}

/// Defines the semantic value crossing from one flow step to its consumer.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case", deny_unknown_fields)]
pub enum PlanFlowValue {
    Type { name: String },
    Text { text: String },
}

impl PlanFlowValue {
    /// Resolve the stable semantic kind used by navigation and presentation.
    pub fn kind(&self) -> PlanFlowValueKind {
        match self {
            Self::Type { .. } => PlanFlowValueKind::Type,
            Self::Text { .. } => PlanFlowValueKind::Text,
        }
    }

    /// Resolve the reviewer-visible value without discarding its semantic kind.
    pub fn text(&self) -> &str {
        match self {
            Self::Type { name } => name,
            Self::Text { text } => text,
        }
    }
}

/// Represents one boundary in an affected runtime or data flow.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct PlanFlowStep {
    #[serde(default)]
    pub step_id: String,
    pub action: String,
    pub target: EntityReference,
    #[serde(default)]
    pub operations: Vec<PlanFlowOperation>,
    pub value_to_next: Option<PlanFlowValue>,
}

/// Represents one independent runtime, data, request, or recovery flow.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct PlanFlow {
    #[serde(default)]
    pub flow_id: String,
    pub title: String,
    pub description: String,
    #[serde(default)]
    pub steps: Vec<PlanFlowStep>,
}

/// Defines one local architectural move inside a source file.
#[derive(Clone, Copy, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum SubtaskAction {
    Expose,
    Encapsulate,
    Move,
    Centralize,
    Distribute,
    Extract,
    Inline,
    Split,
    Merge,
    Compose,
    Embed,
    Create,
    Destroy,
    Register,
    Unregister,
    Attach,
    Detach,
    Start,
    Stop,
    Route,
    Resolve,
    Defer,
    Configure,
    Relax,
    Enable,
    Disable,
    Reuse,
    Generalize,
    Specialize,
}

impl SubtaskAction {
    /// Resolve the reviewer-visible imperative that prefixes a subtask description.
    pub(crate) const fn label(self) -> &'static str {
        match self {
            Self::Expose => "Expose",
            Self::Encapsulate => "Encapsulate",
            Self::Move => "Move",
            Self::Centralize => "Centralize",
            Self::Distribute => "Distribute",
            Self::Extract => "Extract",
            Self::Inline => "Inline",
            Self::Split => "Split",
            Self::Merge => "Merge",
            Self::Compose => "Compose",
            Self::Embed => "Embed",
            Self::Create => "Create",
            Self::Destroy => "Destroy",
            Self::Register => "Register",
            Self::Unregister => "Unregister",
            Self::Attach => "Attach",
            Self::Detach => "Detach",
            Self::Start => "Start",
            Self::Stop => "Stop",
            Self::Route => "Route",
            Self::Resolve => "Resolve",
            Self::Defer => "Defer",
            Self::Configure => "Configure",
            Self::Relax => "Relax",
            Self::Enable => "Enable",
            Self::Disable => "Disable",
            Self::Reuse => "Reuse",
            Self::Generalize => "Generalize",
            Self::Specialize => "Specialize",
        }
    }
}

/// Represents one local architectural move inside a source file.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct PlanWorkSubtask {
    #[serde(default)]
    pub subtask_id: String,
    #[serde(rename = "operation")]
    pub action: SubtaskAction,
    pub description: String,
    #[serde(default)]
    #[serde(rename = "entities")]
    pub entity_ids: Vec<String>,
}

/// Defines the sole operation accepted by a test subtask.
#[derive(Clone, Copy, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum TestSubtaskOperation {
    Test,
}

/// Represents one concrete test edit inside its owning source file.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct PlanTestSubtask {
    #[serde(default)]
    pub subtask_id: String,
    #[serde(rename = "operation")]
    pub operation: TestSubtaskOperation,
    pub action: ChangeAction,
    pub name: String,
    pub category: TestCategory,
    pub behavior: String,
    #[serde(default)]
    #[serde(rename = "covers_entities")]
    pub covered_entity_ids: Vec<String>,
}

/// Represents one implementation or test responsibility inside a source file.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(untagged)]
pub enum PlanSubtask {
    Test(PlanTestSubtask),
    Work(PlanWorkSubtask),
}

impl PlanSubtask {
    /// Resolve the Harness-owned identity shared by every subtask role.
    pub fn subtask_id(&self) -> &str {
        match self {
            Self::Test(subtask) => &subtask.subtask_id,
            Self::Work(subtask) => &subtask.subtask_id,
        }
    }

    /// Resolve the mutable Harness-owned identity shared by every subtask role.
    pub fn subtask_id_mut(&mut self) -> &mut String {
        match self {
            Self::Test(subtask) => &mut subtask.subtask_id,
            Self::Work(subtask) => &mut subtask.subtask_id,
        }
    }

    /// Resolve program entities owned by an implementation subtask.
    pub fn owned_entity_ids(&self) -> &[String] {
        match self {
            Self::Test(_) => &[],
            Self::Work(subtask) => &subtask.entity_ids,
        }
    }

    /// Resolve mutable program-entity ownership for an implementation subtask.
    pub fn owned_entity_ids_mut(&mut self) -> Option<&mut Vec<String>> {
        match self {
            Self::Test(_) => None,
            Self::Work(subtask) => Some(&mut subtask.entity_ids),
        }
    }

    /// Resolve optional production-entity traceability for a test subtask.
    pub fn covered_entity_ids(&self) -> &[String] {
        match self {
            Self::Test(subtask) => &subtask.covered_entity_ids,
            Self::Work(_) => &[],
        }
    }

    /// Resolve the nested test when this subtask authors verification code.
    pub fn test(&self) -> Option<&PlanTestSubtask> {
        match self {
            Self::Test(subtask) => Some(subtask),
            Self::Work(_) => None,
        }
    }
}

/// Represents one concrete source-file boundary owned by a task.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct PlanFile {
    pub path: String,
    #[serde(default = "default_change_action")]
    pub action: ChangeAction,
    #[serde(default)]
    pub subtasks: Vec<PlanSubtask>,
}

/// Represents one architectural execution unit and its complete source subtree.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct PlanTask {
    #[serde(default)]
    pub task_id: String,
    pub title: String,
    pub description: String,
    #[serde(default)]
    pub files: Vec<PlanFile>,
}

fn default_change_action() -> ChangeAction {
    ChangeAction::Modify
}

pub const PROVISIONAL_PLAN_TITLE: &str = "Planning in progress";

/// Owns the complete canonical plan consumed by review and execution.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct PlanDocument {
    pub version: u64,
    pub plan_id: String,
    pub title: String,
    #[serde(default)]
    pub prompt: String,
    pub overview: String,
    pub usage: Option<PlanUsage>,
    #[serde(default)]
    pub entity_changes: Vec<ProgramEntityChange>,
    #[serde(default)]
    pub dependencies: Vec<PlanDependencyChange>,
    #[serde(default)]
    pub flows: Vec<PlanFlow>,
    #[serde(default)]
    pub tasks: Vec<PlanTask>,
    #[serde(default)]
    pub assumptions: Vec<String>,
}

impl PlanDocument {
    /// Validate references and stable identifiers after every semantic edit.
    pub fn validate(&self) -> Result<()> {
        validate_plan_edit(self)
    }

    /// Validate that one working document can enter mandatory review.
    pub fn validate_for_submission(&self) -> Result<()> {
        validate_plan_submission(self)
    }

    /// Serialize the semantic planning surface without Harness-owned identities.
    pub fn model_json(&self) -> Result<String> {
        let mut value = serde_json::to_value(self)?;
        value
            .as_object_mut()
            .expect("PlanDocument serializes as an object")
            .remove("prompt");
        hide_internal_identity(&mut value);
        Ok(serde_json::to_string_pretty(&value)?)
    }
}

fn hide_internal_identity(value: &mut serde_json::Value) {
    match value {
        serde_json::Value::Array(item_list) => {
            for item in item_list {
                hide_internal_identity(item);
            }
        }
        serde_json::Value::Object(object) => {
            for key in [
                "entity_id",
                "dependency_id",
                "member_id",
                "variant_id",
                "field_id",
                "flow_id",
                "step_id",
                "operation_id",
                "task_id",
                "subtask_id",
            ] {
                object.remove(key);
            }
            for child in object.values_mut() {
                hide_internal_identity(child);
            }
        }
        _ => {}
    }
}

#[cfg(test)]
pub(crate) fn test_fixture(plan_id: &str, overview: &str) -> PlanDocument {
    PlanDocument {
        version: 1,
        plan_id: plan_id.into(),
        title: "Structured plan".into(),
        prompt: "Create a structured plan.".into(),
        overview: overview.into(),
        usage: None,
        entity_changes: vec![ProgramEntityChange {
            entity_id: "plan_document".into(),
            action: ChangeAction::Add,
            kind: EntityKind::Struct,
            name: "PlanDocument".into(),
            description: "Own canonical planning data.".into(),
            path: "src/plan.rs".into(),
            members: Vec::new(),
            variants: Vec::new(),
            extends: None,
            conforms_to: Vec::new(),
            exclusive_owner_entity_id: None,
        }],
        dependencies: Vec::new(),
        flows: vec![PlanFlow {
            flow_id: "execution".into(),
            title: "Execution".into(),
            description: "Start from the accepted plan and produce executable work. Keep planning ownership distinct from execution state.".into(),
            steps: vec![PlanFlowStep {
                step_id: "read_plan".into(),
                action: "Read plan".into(),
                target: EntityReference::PlannedEntity {
                    entity: "plan_document".into(),
                },
                operations: Vec::new(),
                value_to_next: None,
            }],
        }],
        tasks: vec![PlanTask {
            task_id: "create_plan_state".into(),
            title: "Create plan state".into(),
            description: "Give planning one owner.".into(),
            files: vec![PlanFile {
                path: "src/plan.rs".into(),
                action: ChangeAction::Add,
                subtasks: vec![PlanSubtask::Work(PlanWorkSubtask {
                    subtask_id: "create_owner".into(),
                    action: SubtaskAction::Create,
                    description: "Keep state durable.".into(),
                    entity_ids: vec!["plan_document".into()],
                })],
            }],
        }],
        assumptions: Vec::new(),
    }
}

#[cfg(test)]
pub(crate) fn test_subtask_fixture() -> PlanSubtask {
    PlanSubtask::Test(PlanTestSubtask {
        subtask_id: "validates_plans".into(),
        operation: TestSubtaskOperation::Test,
        action: ChangeAction::Add,
        name: "validates_plans".into(),
        category: TestCategory::Unit,
        behavior: "Reject malformed plans.".into(),
        covered_entity_ids: vec!["plan_document".into()],
    })
}

#[cfg(test)]
pub(crate) fn integration_test_subtask_fixture() -> PlanSubtask {
    PlanSubtask::Test(PlanTestSubtask {
        subtask_id: "submits_complete_plan".into(),
        operation: TestSubtaskOperation::Test,
        action: ChangeAction::Add,
        name: "submits_complete_plan".into(),
        category: TestCategory::Integration,
        behavior: "Submit one complete plan through the real broker boundary.".into(),
        covered_entity_ids: vec!["plan_document".into()],
    })
}

#[cfg(test)]
pub(crate) fn attach_test_fixture(document: &mut PlanDocument) {
    document.tasks[0].files[0]
        .subtasks
        .push(test_subtask_fixture());
}

#[cfg(test)]
pub(crate) fn attach_integration_test_fixture(document: &mut PlanDocument) {
    document.tasks[0].files.push(PlanFile {
        path: "tests/plan_submission.rs".into(),
        action: ChangeAction::Add,
        subtasks: vec![integration_test_subtask_fixture()],
    });
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn deserializes_only_structured_flow_values() {
        assert_eq!(
            serde_json::from_value::<PlanFlowValue>(serde_json::json!({
                "kind": "type",
                "name": "PathBuf"
            }))
            .unwrap(),
            PlanFlowValue::Type {
                name: "PathBuf".into()
            }
        );
        assert_eq!(
            serde_json::from_value::<PlanFlowValue>(serde_json::json!({
                "kind": "text",
                "text": "registered table"
            }))
            .unwrap(),
            PlanFlowValue::Text {
                text: "registered table".into()
            }
        );
        assert!(serde_json::from_value::<PlanFlowValue>(serde_json::json!("PathBuf")).is_err());
    }

    #[test]
    fn validates_complete_submission() {
        test_fixture("plan", "Build structured planning.")
            .validate_for_submission()
            .unwrap();
    }

    #[test]
    fn accepts_fn_as_a_function_kind_alias() {
        assert_eq!(
            serde_json::from_str::<EntityKind>("\"fn\"").unwrap(),
            EntityKind::Function
        );
        assert_eq!(
            serde_json::to_string(&EntityKind::Function).unwrap(),
            "\"function\""
        );
    }

    #[test]
    fn serializes_each_test_as_one_flat_subtask() {
        let value = serde_json::to_value(test_subtask_fixture()).unwrap();

        assert_eq!(
            value.pointer("/operation"),
            Some(&serde_json::json!("test"))
        );
        assert_eq!(value.pointer("/action"), Some(&serde_json::json!("add")));
        assert_eq!(
            value.pointer("/name"),
            Some(&serde_json::json!("validates_plans"))
        );
        assert_eq!(value.pointer("/category"), Some(&serde_json::json!("unit")));
        assert_eq!(
            value.pointer("/covers_entities/0"),
            Some(&serde_json::json!("plan_document"))
        );
        assert!(value.get("tests").is_none());
        assert!(value.get("entities").is_none());
        assert!(value.get("description").is_none());
    }

    #[test]
    fn serializes_tagged_entity_references() {
        let mut document = test_fixture("plan", "Build structured planning.");
        document.flows[0].steps.push(PlanFlowStep {
            step_id: "external_output".into(),
            action: "Print result".into(),
            target: EntityReference::ExternalEntity {
                entity: "terminal stdout".into(),
            },
            operations: Vec::new(),
            value_to_next: None,
        });
        let value = serde_json::to_value(document).unwrap();

        assert_eq!(
            value.pointer("/flows/0/steps/0/target/kind"),
            Some(&serde_json::json!("planned_entity"))
        );
        assert_eq!(
            value.pointer("/flows/0/steps/0/target/entity"),
            Some(&serde_json::json!("plan_document"))
        );
        assert_eq!(
            value.pointer("/flows/0/steps/1/target/kind"),
            Some(&serde_json::json!("external_entity"))
        );
        assert_eq!(
            value.pointer("/flows/0/steps/1/target/entity"),
            Some(&serde_json::json!("terminal stdout"))
        );
    }

    #[test]
    fn rejects_ambiguous_entity_references() {
        let result = serde_json::from_value::<EntityReference>(serde_json::json!({
            "kind": "planned_entity",
            "entity": "plan_document",
            "external_entity_name": "CLI"
        }));

        assert!(result.is_err());
    }

    #[test]
    fn model_json_hides_internal_identity() {
        let mut document = test_fixture("plan", "Build structured planning.");
        document.flows[0].steps[0]
            .operations
            .push(PlanFlowOperation {
                operation_id: "read_plan_operation_validate".into(),
                action: "validate()".into(),
                target: EntityReference::ExternalEntity {
                    entity: "PlanValidator".into(),
                },
                result: Some("ValidatedPlan".into()),
            });
        document.dependencies.push(PlanDependencyChange {
            dependency_id: "dependency_tokio".into(),
            action: ChangeAction::Add,
            name: "tokio".into(),
            version: "1".into(),
            manifest: "Cargo.toml".into(),
            license: Some("MIT".into()),
            justification: "Run asynchronous work.".into(),
        });
        let model_json = document.model_json().unwrap();

        assert!(model_json.contains(r#""plan_id": "plan""#));
        assert!(!model_json.contains(r#""prompt""#));
        assert!(!model_json.contains("entity_id"));
        assert!(!model_json.contains("dependency_id"));
        assert!(!model_json.contains("member_id"));
        assert!(!model_json.contains("flow_id"));
        assert!(!model_json.contains("operation_id"));
        assert!(!model_json.contains("task_id"));
        assert!(!model_json.contains("subtask_id"));
        assert!(model_json.contains(r#""action": "validate()""#));
        assert!(model_json.contains(r#""result": "ValidatedPlan""#));
    }
}
