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

/// Defines whether one top-level program entity enters, changes, leaves, or changes its name.
#[derive(Clone, Copy, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum EntityChangeAction {
    Add,
    Modify,
    Remove,
    Rename,
}

impl EntityChangeAction {
    /// Map entity-specific lifecycle semantics onto the shared add/modify/remove graph behavior.
    pub fn base_action(self) -> ChangeAction {
        match self {
            Self::Add => ChangeAction::Add,
            Self::Modify | Self::Rename => ChangeAction::Modify,
            Self::Remove => ChangeAction::Remove,
        }
    }
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

/// Defines whether one referenced flow participant names a type or an endpoint.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum ReferencedEntityKind {
    Type,
    Endpoint,
}

/// Identifies one planned, workspace-owned, or external architectural boundary.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case", deny_unknown_fields)]
pub enum EntityReference {
    PlannedEntity {
        entity: String,
    },
    WorkspaceEntity {
        entity_kind: ReferencedEntityKind,
        name: String,
        path: String,
        line: usize,
    },
    ExternalEntity {
        entity_kind: ReferencedEntityKind,
        name: String,
        dependency: Option<String>,
    },
}

/// Represents one named program entity and its planned implementation lifecycle.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct ProgramEntityChange {
    #[serde(default)]
    pub entity_id: String,
    pub action: EntityChangeAction,
    pub kind: EntityKind,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub renamed_from: Option<String>,
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
}

/// Represents one auditable package dependency and its manifest declaration.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct PlanDependencyChange {
    #[serde(default)]
    pub dependency_id: String,
    pub action: ChangeAction,
    pub name: String,
    pub version: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub resolved_version: Option<String>,
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

/// Defines whether one invoked callable is a free function or a method.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum PlanCallableKind {
    Function,
    Method,
}

/// Identifies one callable without embedding presentation punctuation in its name.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct PlanCallable {
    pub kind: PlanCallableKind,
    pub name: String,
}

/// Defines one typed runtime relationship between two flow participants.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case", deny_unknown_fields)]
pub enum PlanFlowRelation {
    Construct,
    Call { callable: PlanCallable },
    Read { callable: PlanCallable },
    Write { callable: PlanCallable },
    Send { event: String },
    Emit,
    Return,
}

impl PlanFlowRelation {
    /// Render the runtime relationship without inferring semantics from prose.
    pub fn label(&self) -> String {
        match self {
            Self::Construct => "Construct".into(),
            Self::Call { callable } => format!("Call {}()", callable.name),
            Self::Read { callable } => format!("Read {}()", callable.name),
            Self::Write { callable } => format!("Write {}()", callable.name),
            Self::Send { event } => format!("Send {event}"),
            Self::Emit => "Emit".into(),
            Self::Return => "Return".into(),
        }
    }
}

/// Connects one flow step owner to a concrete runtime receiver.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct PlanFlowEdge {
    #[serde(default)]
    pub edge_id: String,
    pub relation: PlanFlowRelation,
    pub target: EntityReference,
    pub expansion: Vec<PlanFlowStep>,
    pub result: Option<PlanFlowValue>,
}

/// Represents one labeled continuation from an acting flow step.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct PlanFlowBranch {
    #[serde(default)]
    pub branch_id: String,
    pub condition: String,
    pub steps: Vec<PlanFlowStep>,
}

/// Represents one boundary in an affected runtime or data flow.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct PlanFlowStep {
    #[serde(default)]
    pub step_id: String,
    pub action: String,
    pub target: EntityReference,
    pub edges: Vec<PlanFlowEdge>,
    pub branches: Vec<PlanFlowBranch>,
}

/// Represents one independent runtime, data, request, or recovery flow.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct PlanFlow {
    #[serde(default)]
    pub flow_id: String,
    pub title: String,
    pub description: String,
    pub steps: Vec<PlanFlowStep>,
}

impl PlanFlow {
    /// Resolve one step from any depth in this flow's execution tree.
    pub fn step(&self, step_id: &str) -> Option<&PlanFlowStep> {
        find_flow_step(&self.steps, step_id)
    }

    /// Resolve one mutable step from any depth in this flow's execution tree.
    pub fn step_mut(&mut self, step_id: &str) -> Option<&mut PlanFlowStep> {
        find_flow_step_mut(&mut self.steps, step_id)
    }

    /// Resolve one edge through its owning step from any depth in the execution tree.
    pub fn edge(&self, step_id: &str, edge_id: &str) -> Option<&PlanFlowEdge> {
        self.step(step_id)?
            .edges
            .iter()
            .find(|edge| edge.edge_id == edge_id)
    }
}

fn find_flow_step<'a>(step_list: &'a [PlanFlowStep], step_id: &str) -> Option<&'a PlanFlowStep> {
    for step in step_list {
        if step.step_id == step_id {
            return Some(step);
        }
        for edge in &step.edges {
            if let Some(found) = find_flow_step(&edge.expansion, step_id) {
                return Some(found);
            }
        }
        for branch in &step.branches {
            if let Some(found) = find_flow_step(&branch.steps, step_id) {
                return Some(found);
            }
        }
    }
    None
}

fn find_flow_step_mut<'a>(
    step_list: &'a mut [PlanFlowStep],
    step_id: &str,
) -> Option<&'a mut PlanFlowStep> {
    for step in step_list {
        if step.step_id == step_id {
            return Some(step);
        }
        for edge in &mut step.edges {
            if let Some(found) = find_flow_step_mut(&mut edge.expansion, step_id) {
                return Some(found);
            }
        }
        for branch in &mut step.branches {
            if let Some(found) = find_flow_step_mut(&mut branch.steps, step_id) {
                return Some(found);
            }
        }
    }
    None
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

/// Defines how one task changes a concrete source-file boundary.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "action", rename_all = "snake_case")]
pub enum PlanFileChange {
    Add { path: String },
    Modify { path: String },
    Remove { path: String },
    Rename { from: String, to: String },
}

impl PlanFileChange {
    /// Resolve the file path that owns planned subtasks after this change.
    pub fn path(&self) -> &str {
        match self {
            Self::Add { path } | Self::Modify { path } | Self::Remove { path } => path,
            Self::Rename { to, .. } => to,
        }
    }

    /// Resolve the source path removed by a rename.
    pub fn source_path(&self) -> Option<&str> {
        match self {
            Self::Rename { from, .. } => Some(from),
            _ => None,
        }
    }

    /// Resolve the entity lifecycle constraint imposed by this file change.
    pub fn entity_action(&self) -> ChangeAction {
        match self {
            Self::Add { .. } => ChangeAction::Add,
            Self::Modify { .. } | Self::Rename { .. } => ChangeAction::Modify,
            Self::Remove { .. } => ChangeAction::Remove,
        }
    }
}

/// Represents one concrete source-file boundary owned by a task.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct PlanFile {
    #[serde(flatten)]
    pub change: PlanFileChange,
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
                "resolved_version",
                "member_id",
                "variant_id",
                "field_id",
                "flow_id",
                "step_id",
                "edge_id",
                "branch_id",
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
            action: EntityChangeAction::Add,
            kind: EntityKind::Struct,
            renamed_from: None,
            name: "PlanDocument".into(),
            description: "Own canonical planning data.".into(),
            path: "src/plan.rs".into(),
            members: Vec::new(),
            variants: Vec::new(),
            extends: None,
            conforms_to: Vec::new(),
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
                edges: vec![PlanFlowEdge {
                    edge_id: "read_plan_edge_return".into(),
                    relation: PlanFlowRelation::Return,
                    target: EntityReference::ExternalEntity {
                        entity_kind: ReferencedEntityKind::Endpoint,
                        name: "execution scheduler".into(),
                        dependency: None,
                    },
                    expansion: Vec::new(),
                    result: Some(PlanFlowValue::Type {
                        name: "ExecutablePlan".into(),
                    }),
                }],
                branches: Vec::new(),
            }],
        }],
        tasks: vec![PlanTask {
            task_id: "create_plan_state".into(),
            title: "Create plan state".into(),
            description: "Give planning one owner.".into(),
            files: vec![PlanFile {
                change: PlanFileChange::Add {
                    path: "src/plan.rs".into(),
                },
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
        change: PlanFileChange::Add {
            path: "tests/plan_submission.rs".into(),
        },
        subtasks: vec![integration_test_subtask_fixture()],
    });
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn deserializes_tagged_file_changes_without_ambiguous_rename_paths() {
        let renamed: PlanFile = serde_json::from_value(serde_json::json!({
            "action": "rename",
            "from": "src/old.rs",
            "to": "src/new.rs",
            "subtasks": []
        }))
        .unwrap();
        assert_eq!(renamed.change.path(), "src/new.rs");
        assert_eq!(renamed.change.source_path(), Some("src/old.rs"));

        assert!(
            serde_json::from_value::<PlanFile>(serde_json::json!({
                "action": "rename",
                "path": "src/new.rs",
                "subtasks": []
            }))
            .is_err()
        );
        assert!(
            serde_json::from_value::<PlanFile>(serde_json::json!({
                "path": "src/new.rs",
                "subtasks": []
            }))
            .is_err()
        );
    }

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
    fn deserializes_typed_runtime_edges_and_rejects_legacy_step_transitions() {
        let edge = serde_json::from_value::<PlanFlowEdge>(serde_json::json!({
            "relation": {
                "kind": "call",
                "callable": {"kind": "method", "name": "inspect"}
            },
            "target": {"kind": "planned_entity", "entity": "GeoParquetInspector"},
            "expansion": [],
            "result": {"kind": "type", "name": "InspectionReport"}
        }))
        .unwrap();
        assert!(matches!(
            edge.relation,
            PlanFlowRelation::Call { callable }
                if callable.kind == PlanCallableKind::Method && callable.name == "inspect"
        ));

        assert!(
            serde_json::from_value::<PlanFlowStep>(serde_json::json!({
                "action": "Inspect dataset",
                "target": {"kind": "planned_entity", "entity": "GeoParquetInspector"},
                "operations": [],
                "value_to_next": {"kind": "type", "name": "InspectionReport"}
            }))
            .is_err()
        );
    }

    #[test]
    fn deserializes_recursive_flow_trees_and_requires_explicit_child_arrays() {
        let flow = serde_json::from_value::<PlanFlow>(serde_json::json!({
            "title": "Inspection",
            "description": "Inspect one file and return a report. Keep file interpretation inside the inspector.",
            "steps": [{
                "action": "Parse local file path",
                "target": {"kind": "planned_entity", "entity": "main"},
                "edges": [{
                    "relation": {
                        "kind": "call",
                        "callable": {"kind": "method", "name": "inspect"}
                    },
                    "target": {
                        "kind": "planned_entity",
                        "entity": "GeoParquetInspector"
                    },
                    "expansion": [{
                        "action": "Read metadata",
                        "target": {
                            "kind": "planned_entity",
                            "entity": "GeoParquetInspector"
                        },
                        "edges": [],
                        "branches": []
                    }],
                    "result": {"kind": "type", "name": "InspectionReport"}
                }],
                "branches": [{
                    "condition": "failure",
                    "steps": [{
                        "action": "Emit failure",
                        "target": {"kind": "planned_entity", "entity": "main"},
                        "edges": [],
                        "branches": []
                    }]
                }]
            }]
        }))
        .unwrap();

        assert_eq!(flow.steps[0].edges[0].expansion[0].action, "Read metadata");
        assert_eq!(flow.steps[0].branches[0].condition, "failure");
        assert!(flow.step("missing").is_none());

        assert!(
            serde_json::from_value::<PlanFlowStep>(serde_json::json!({
                "action": "Inspect",
                "target": {"kind": "planned_entity", "entity": "Inspector"},
                "edges": []
            }))
            .is_err()
        );
        assert!(
            serde_json::from_value::<PlanFlowEdge>(serde_json::json!({
                "relation": {"kind": "construct"},
                "target": {"kind": "planned_entity", "entity": "Inspector"},
                "result": null
            }))
            .is_err()
        );
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
            step_id: "workspace_validation".into(),
            action: "Validate plan".into(),
            target: EntityReference::WorkspaceEntity {
                entity_kind: ReferencedEntityKind::Type,
                name: "PlanValidator".into(),
                path: "src/plan/validation.rs".into(),
                line: 76,
            },
            edges: Vec::new(),
            branches: Vec::new(),
        });
        document.flows[0].steps.push(PlanFlowStep {
            step_id: "external_output".into(),
            action: "Print result".into(),
            target: EntityReference::ExternalEntity {
                entity_kind: ReferencedEntityKind::Endpoint,
                name: "terminal stdout".into(),
                dependency: None,
            },
            edges: Vec::new(),
            branches: Vec::new(),
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
            Some(&serde_json::json!("workspace_entity"))
        );
        assert_eq!(
            value.pointer("/flows/0/steps/1/target/path"),
            Some(&serde_json::json!("src/plan/validation.rs"))
        );
        assert_eq!(
            value.pointer("/flows/0/steps/1/target/line"),
            Some(&serde_json::json!(76))
        );
        assert_eq!(
            value.pointer("/flows/0/steps/2/target/kind"),
            Some(&serde_json::json!("external_entity"))
        );
        assert_eq!(
            value.pointer("/flows/0/steps/2/target/name"),
            Some(&serde_json::json!("terminal stdout"))
        );
        assert_eq!(
            value.pointer("/flows/0/steps/2/target/entity_kind"),
            Some(&serde_json::json!("endpoint"))
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
        document.flows[0].steps[0].edges.push(PlanFlowEdge {
            edge_id: "read_plan_edge_validate".into(),
            relation: PlanFlowRelation::Call {
                callable: PlanCallable {
                    kind: PlanCallableKind::Method,
                    name: "validate".into(),
                },
            },
            target: EntityReference::ExternalEntity {
                entity_kind: ReferencedEntityKind::Type,
                name: "PlanValidator".into(),
                dependency: None,
            },
            expansion: Vec::new(),
            result: Some(PlanFlowValue::Type {
                name: "ValidatedPlan".into(),
            }),
        });
        document.dependencies.push(PlanDependencyChange {
            dependency_id: "dependency_tokio".into(),
            action: ChangeAction::Add,
            name: "tokio".into(),
            version: "1".into(),
            resolved_version: None,
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
        assert!(!model_json.contains("edge_id"));
        assert!(!model_json.contains("task_id"));
        assert!(!model_json.contains("subtask_id"));
        assert!(model_json.contains(r#""kind": "method""#));
        assert!(model_json.contains(r#""name": "validate""#));
        assert!(model_json.contains(r#""name": "ValidatedPlan""#));
    }
}
