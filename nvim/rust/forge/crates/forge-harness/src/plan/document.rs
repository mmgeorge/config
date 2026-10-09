use anyhow::{Context, Result};
use schemars::JsonSchema;
use serde::{Deserialize, Deserializer, Serialize};
use std::borrow::Cow;

use super::validation::{validate_plan_edit, validate_plan_submission};

/// Defines whether one planned entity enters, changes, or leaves the codebase.
#[derive(Clone, Copy, Debug, Deserialize, Eq, Hash, JsonSchema, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum ChangeAction {
    Add,
    Modify,
    Remove,
    Rename,
}

#[derive(JsonSchema)]
#[schemars(rename = "DependencyChangeAction", rename_all = "snake_case")]
pub enum DependencyChangeAction {
    Add,
    Modify,
    Remove,
}

impl ChangeAction {
    /// Map nested declaration renames onto modification behavior.
    pub fn base_action(self) -> Self {
        match self {
            Self::Rename => Self::Modify,
            action => action,
        }
    }
}

/// Defines whether one top-level program entity enters, changes, leaves, or changes its name.
#[derive(Clone, Copy, Debug, Deserialize, Eq, Hash, JsonSchema, PartialEq, Serialize)]
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
#[derive(Clone, Debug, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct PlanUsage {
    pub command: String,
    pub expected_result: String,
}

/// Defines the semantic role of one named program entity.
#[derive(Clone, Copy, Debug, Deserialize, Eq, Hash, JsonSchema, PartialEq, Serialize)]
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
    Constant,
    Resource,
    Cache,
    Adapter,
}

/// Defines one member role inside a program entity.
#[derive(Clone, Copy, Debug, Deserialize, Eq, Hash, JsonSchema, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum MemberKind {
    Field,
    Method,
    #[serde(alias = "fn")]
    Function,
    Constant,
    Property,
}

/// Accepts the optional redundant discriminator shared by field-like declarations.
#[derive(Clone, Copy, Debug, Deserialize, Eq, Hash, JsonSchema, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum FieldKind {
    Field,
}

/// Defines the reviewer-visible visibility of one planned member.
#[derive(
    Clone, Copy, Debug, Deserialize, Eq, JsonSchema, PartialEq, Ord, PartialOrd, Serialize,
)]
#[serde(rename_all = "snake_case")]
pub enum Visibility {
    Public,
    Protected,
    Internal,
    Private,
}

/// Represents one ordered function or method parameter.
#[derive(Clone, Debug, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct FunctionParameter {
    pub name: String,
    #[serde(rename = "type")]
    pub type_name: String,
}

/// Represents one changed field exposed by an enum variant.
#[derive(Clone, Debug, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct EnumVariantFieldChange {
    pub action: ChangeAction,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub renamed_from: Option<String>,
    pub name: String,
    #[serde(rename = "type")]
    pub type_name: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub kind: Option<FieldKind>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub visibility: Option<Visibility>,
}

/// Represents one changed enum case and its optional variant fields.
#[derive(Clone, Debug, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct EnumVariantChange {
    pub action: ChangeAction,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub renamed_from: Option<String>,
    pub name: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    pub fields: Vec<EnumVariantFieldChange>,
}

/// Represents one changed field or operation nested inside a program entity.
#[derive(Clone, Debug, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct ProgramEntityMemberChange {
    pub action: ChangeAction,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub renamed_from: Option<String>,
    pub kind: MemberKind,
    pub name: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    pub visibility: Option<Visibility>,
    #[serde(rename = "type")]
    pub type_name: Option<String>,
    #[serde(default)]
    pub parameters: Vec<FunctionParameter>,
    pub return_type: Option<String>,
}

/// Defines whether one referenced flow participant names a type or an endpoint.
#[derive(Clone, Copy, Debug, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum ReferencedEntityKind {
    Type,
    Endpoint,
}

/// Identifies one planned, workspace-owned, or external architectural boundary.
#[derive(Clone, Debug, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
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
#[derive(Clone, Debug, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct ProgramEntityChange {
    pub action: EntityChangeAction,
    pub kind: EntityKind,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub renamed_from: Option<String>,
    pub name: String,
    pub description: String,
    pub path: String,
    pub members: Vec<ProgramEntityMemberChange>,
    pub variants: Vec<EnumVariantChange>,
    pub extends: Option<EntityReference>,
    pub conforms_to: Vec<EntityReference>,
}

/// Represents one auditable package dependency and its manifest declaration.
#[derive(Clone, Debug, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct PlanDependencyChange {
    #[schemars(with = "DependencyChangeAction")]
    pub action: ChangeAction,
    pub name: String,
    pub version: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    #[schemars(skip)]
    pub resolved_version: Option<String>,
    pub manifest: String,
    pub license: Option<String>,
    pub justification: String,
}

/// Defines the verification boundary exercised by one test subtask.
#[derive(Clone, Copy, Debug, Deserialize, Eq, Hash, JsonSchema, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum TestCategory {
    Unit,
    Integration,
}

/// Defines whether one invoked callable is a free function or a method.
#[derive(Clone, Copy, Debug, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum PlanCallableKind {
    Function,
    Method,
}

/// Identifies one callable without embedding presentation punctuation in its name.
#[derive(Clone, Debug, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct PlanCallable {
    pub kind: PlanCallableKind,
    pub name: String,
}

/// Defines one runtime relationship between two flow participants.
#[derive(Clone, Debug, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum PlanFlowRelation {
    /// Creates one instance of the target type.
    Construct,
    /// Invokes a callable for its returned value or behavior.
    Call,
    /// Obtains data through a callable without assigning ownership semantics from prose.
    Read,
    /// Mutates or persists state through a callable.
    Write,
    /// Transfers one typed payload to the target.
    Send,
    /// Produces one typed payload at the target.
    Emit,
    /// Transfers one typed payload back to the target.
    Return,
}

/// Describes the success and failure types returned by one callable.
#[derive(Clone, Debug, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct PlanFlowReturnType {
    pub value_type: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub error_type: Option<String>,
}

/// Connects one flow participant to a concrete runtime receiver.
#[derive(Clone, Debug, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct PlanFlowEdge {
    pub relation: PlanFlowRelation,
    pub target: EntityReference,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub callable: Option<PlanCallable>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    /// Transferred payload, or callable parameters excluding self (a tuple for multiple inputs).
    pub payload_type: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub return_type: Option<PlanFlowReturnType>,
    pub expansion: Vec<PlanFlowEdge>,
    pub branches: Vec<PlanFlowBranch>,
}

/// Represents one conditional continuation from a runtime relationship.
#[derive(Clone, Debug, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct PlanFlowBranch {
    pub condition: String,
    pub edges: Vec<PlanFlowEdge>,
}

/// Represents one independent runtime, data, request, or recovery flow.
#[derive(Clone, Debug, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct PlanFlow {
    pub title: String,
    pub description: String,
    pub source: EntityReference,
    pub edges: Vec<PlanFlowEdge>,
}

/// Defines one local architectural move inside a source file.
#[derive(Clone, Copy, Debug, Deserialize, Eq, Hash, JsonSchema, PartialEq, Serialize)]
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
#[derive(Clone, Debug, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct PlanWorkSubtask {
    #[serde(rename = "operation")]
    pub action: SubtaskAction,
    pub description: String,
    pub entities: Vec<String>,
}

/// Defines the sole operation accepted by a test subtask.
#[derive(Clone, Copy, Debug, Deserialize, Eq, Hash, JsonSchema, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum TestSubtaskOperation {
    Test,
}

/// Represents one concrete test edit inside its owning source file.
#[derive(Clone, Debug, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct PlanTestSubtask {
    #[serde(rename = "operation")]
    pub operation: TestSubtaskOperation,
    pub action: ChangeAction,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub renamed_from: Option<String>,
    pub name: String,
    pub category: TestCategory,
    pub behavior: String,
    #[serde(default)]
    pub covers_entities: Vec<String>,
}

/// Represents one implementation or test responsibility inside a source file.
#[derive(Clone, Debug, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
#[serde(untagged)]
pub enum PlanSubtask {
    Test(PlanTestSubtask),
    Work(PlanWorkSubtask),
}

impl PlanSubtask {
    /// Resolve program entities owned by an implementation subtask.
    pub fn owned_entities(&self) -> &[String] {
        match self {
            Self::Test(_) => &[],
            Self::Work(subtask) => &subtask.entities,
        }
    }

    /// Resolve mutable program-entity ownership for an implementation subtask.
    pub fn owned_entities_mut(&mut self) -> Option<&mut Vec<String>> {
        match self {
            Self::Test(_) => None,
            Self::Work(subtask) => Some(&mut subtask.entities),
        }
    }

    /// Resolve optional production-entity traceability for a test subtask.
    pub fn covered_entities(&self) -> &[String] {
        match self {
            Self::Test(subtask) => &subtask.covers_entities,
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
#[derive(Clone, Debug, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
#[serde(tag = "action", rename_all = "snake_case")]
pub enum PlanFileChange {
    Add { path: String },
    Modify { path: String },
    Remove { path: String },
    Rename { from: String, to: String },
}

#[derive(Deserialize, JsonSchema)]
#[serde(tag = "action", rename_all = "snake_case", deny_unknown_fields)]
enum PlanFileWire {
    Add {
        path: String,
        subtasks: Vec<PlanSubtask>,
    },
    Modify {
        path: String,
        subtasks: Vec<PlanSubtask>,
    },
    Remove {
        path: String,
        subtasks: Vec<PlanSubtask>,
    },
    Rename {
        from: String,
        to: String,
        subtasks: Vec<PlanSubtask>,
    },
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
#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub struct PlanFile {
    #[serde(flatten)]
    pub change: PlanFileChange,
    pub subtasks: Vec<PlanSubtask>,
}

impl<'de> Deserialize<'de> for PlanFile {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        Ok(match PlanFileWire::deserialize(deserializer)? {
            PlanFileWire::Add { path, subtasks } => Self {
                change: PlanFileChange::Add { path },
                subtasks,
            },
            PlanFileWire::Modify { path, subtasks } => Self {
                change: PlanFileChange::Modify { path },
                subtasks,
            },
            PlanFileWire::Remove { path, subtasks } => Self {
                change: PlanFileChange::Remove { path },
                subtasks,
            },
            PlanFileWire::Rename { from, to, subtasks } => Self {
                change: PlanFileChange::Rename { from, to },
                subtasks,
            },
        })
    }
}

impl JsonSchema for PlanFile {
    fn schema_name() -> Cow<'static, str> {
        "PlanFile".into()
    }

    fn schema_id() -> Cow<'static, str> {
        concat!(module_path!(), "::PlanFile").into()
    }

    fn json_schema(generator: &mut schemars::SchemaGenerator) -> schemars::Schema {
        generator.subschema_for::<PlanFileWire>()
    }
}

/// Represents one architectural execution unit and its complete source subtree.
#[derive(Clone, Debug, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct PlanTask {
    pub id: String,
    pub requires: Vec<String>,
    pub title: String,
    pub description: String,
    pub files: Vec<PlanFile>,
}

pub const PROVISIONAL_PLAN_TITLE: &str = "Planning in progress";
pub const PLAN_SCHEMA_VERSION: u32 = 7;

#[derive(Clone, Debug, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
/// Owns independent tasks behind an ordered execution barrier.
pub struct PlanStage {
    pub id: String,
    pub title: String,
    pub tasks: Vec<PlanTask>,
}

/// Owns the complete canonical plan consumed by review and execution.
#[derive(Clone, Debug, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct PlanDocument {
    pub schema_version: u32,
    pub version: u64,
    pub plan_id: String,
    pub title: String,
    #[serde(default)]
    pub prompt: String,
    pub overview: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub design: Option<super::DeclarationDesign>,
    pub usage: Option<PlanUsage>,
    #[serde(default)]
    pub entity_changes: Vec<ProgramEntityChange>,
    #[serde(default)]
    pub dependencies: Vec<PlanDependencyChange>,
    #[serde(default)]
    pub flows: Vec<PlanFlow>,
    #[serde(default)]
    pub stages: Vec<PlanStage>,
    #[serde(default)]
    pub assumptions: Vec<String>,
}

impl PlanDocument {
    /// Apply one optimistic declaration patch without exposing the immutable baseline to edits.
    pub fn patch_design(&self, workspace: &std::path::Path, request: super::DesignPatchRequest) -> Result<Self> {
        anyhow::ensure!(request.plan_id == self.plan_id, "design plan id does not match the active plan");
        anyhow::ensure!(request.expected_version == self.version, "design version conflict: expected_version must be {}", self.version);
        let mut candidate = self.clone();
        candidate.design = Some(self.design.as_ref().context("obsolete plan format. Create a new declaration design.")?.patch(workspace, &request.source_digests, &request.patch)?);
        if let Some(title) = request.title {
            anyhow::ensure!(!title.trim().is_empty() && title.len() <= 1024, "design title must contain 1 to 1024 bytes");
            candidate.title = title.trim().into();
        }
        if candidate.design == self.design && candidate.title == self.title { return Ok(self.clone()); }
        candidate.version = self.version.checked_add(1).context("design version overflow")?;
        candidate.validate()?;
        Ok(candidate)
    }
    /// Visit executable tasks in stage order.
    pub fn tasks(&self) -> impl Iterator<Item = &PlanTask> {
        self.stages.iter().flat_map(|stage| &stage.tasks)
    }

    /// Resolve an execution ordinal into its canonical address.
    pub fn task_path(&self, ordinal: usize) -> String {
        let mut remaining = ordinal;
        for (stage_index, stage) in self.stages.iter().enumerate() {
            if remaining < stage.tasks.len() {
                return format!("/stages/{stage_index}/tasks/{remaining}");
            }
            remaining -= stage.tasks.len();
        }
        panic!("canonical task ordinal is out of bounds");
    }

    /// Resolve a stable task identity into its current address and value.
    pub fn task_by_id(&self, id: &str) -> Option<(String, &PlanTask)> {
        self.tasks()
            .enumerate()
            .find(|(_, task)| task.id == id)
            .map(|(ordinal, task)| (self.task_path(ordinal), task))
    }

    /// Resolve a canonical task pointer without accepting descendant paths.
    pub fn task_at(&self, pointer: &str) -> Option<&PlanTask> {
        let parts = pointer.split('/').collect::<Vec<_>>();
        if parts.len() != 5 || !parts[0].is_empty() || parts[1] != "stages" || parts[3] != "tasks" {
            return None;
        }
        let stage: usize = parts[2].parse().ok()?;
        let task: usize = parts[4].parse().ok()?;
        if pointer != format!("/stages/{stage}/tasks/{task}") {
            return None;
        }
        self.stages.get(stage)?.tasks.get(task)
    }

    /// Produce a positional reviewer label independent of stable identity.
    pub fn task_label(&self, id: &str) -> Option<String> {
        self.task_by_id(id)
            .and_then(|(path, _)| Self::label_for_path(&path))
    }

    /// Format a revision-scoped task address for execution history.
    pub fn label_for_path(path: &str) -> Option<String> {
        let parts = path.split('/').collect::<Vec<_>>();
        if parts.len() == 5 && parts[0].is_empty() && parts[1] == "stages" && parts[3] == "tasks" {
            let stage_index: usize = parts[2].parse().ok()?;
            let task_index: usize = parts[4].parse().ok()?;
            let mut number = task_index.checked_add(1)?;
            let mut suffix = Vec::new();
            while number > 0 {
                number -= 1;
                suffix.push((b'a' + (number % 26) as u8) as char);
                number /= 26;
            }
            return Some(format!(
                "{}{}",
                stage_index.checked_add(1)?,
                suffix.into_iter().rev().collect::<String>()
            ));
        }
        None
    }
    /// Validate references and structural invariants after every semantic edit.
    pub fn validate(&self) -> Result<()> {
        if let Some(design) = &self.design {
            anyhow::ensure!(self.schema_version == PLAN_SCHEMA_VERSION, "obsolete plan format. Create a new declaration design.");
            anyhow::ensure!(self.version > 0 && !self.plan_id.is_empty() && !self.title.trim().is_empty(),"design identity, title, and version are required");
            anyhow::ensure!(serde_json::to_vec(self)?.len() <= 8 * 1024 * 1024,"declaration design exceeds 8 MiB");
            return design.validate();
        }
        validate_plan_edit(self)
    }

    /// Validate that one working document can enter mandatory review.
    pub fn validate_for_submission(&self) -> Result<()> {
        if let Some(design) = &self.design {
            self.validate()?;
            design.document.validate_for_submission()?;
            return Ok(());
        }
        validate_plan_submission(self)
    }

    /// Serialize the semantic planning surface without Harness-derived state.
    pub fn model_json(&self) -> Result<String> {
        if let Some(design) = &self.design {
            return Ok(serde_json::to_string_pretty(&serde_json::json!({"plan_id":self.plan_id,"version":self.version,"title":self.title,"request":self.prompt,"document":design.document,"declaration_files":design.read(None,false)?}))?);
        }
        let mut value = serde_json::to_value(self)?;
        value
            .as_object_mut()
            .expect("PlanDocument serializes as an object")
            .remove("prompt");
        hide_derived_state(&mut value);
        Ok(serde_json::to_string_pretty(&value)?)
    }
}

fn hide_derived_state(value: &mut serde_json::Value) {
    match value {
        serde_json::Value::Array(item_list) => {
            for item in item_list {
                hide_derived_state(item);
            }
        }
        serde_json::Value::Object(object) => {
            object.remove("resolved_version");
            for child in object.values_mut() {
                hide_derived_state(child);
            }
        }
        _ => {}
    }
}

#[cfg(test)]
pub(crate) fn test_fixture(plan_id: &str, overview: &str) -> PlanDocument {
    PlanDocument {
        schema_version: PLAN_SCHEMA_VERSION,
        version: 1,
        plan_id: plan_id.into(),
        title: "Structured plan".into(),
        prompt: "Create a structured plan.".into(),
        overview: overview.into(),
        design: None,
        usage: None,
        entity_changes: vec![ProgramEntityChange {
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
            title: "Execution".into(),
            description: "Start from the accepted plan and produce executable work. Keep planning ownership distinct from execution state.".into(),
            source: EntityReference::PlannedEntity {
                entity: "PlanDocument".into(),
            },
            edges: vec![PlanFlowEdge {
                relation: PlanFlowRelation::Return,
                target: EntityReference::ExternalEntity {
                    entity_kind: ReferencedEntityKind::Endpoint,
                    name: "execution scheduler".into(),
                    dependency: None,
                },
                callable: None,
                payload_type: Some("ExecutablePlan".into()),
                return_type: None,
                expansion: Vec::new(),
                branches: Vec::new(),
            }],
        }],
        stages: vec![PlanStage { id: "foundation".into(), title: "Establish plan state".into(), tasks: vec![PlanTask {
            id: "plan-state".into(), requires: Vec::new(),
            title: "Create plan state".into(),
            description: "Give planning one owner.".into(),
            files: vec![PlanFile {
                change: PlanFileChange::Add {
                    path: "src/plan.rs".into(),
                },
                subtasks: vec![PlanSubtask::Work(PlanWorkSubtask {
                    action: SubtaskAction::Create,
                    description: "Keep state durable.".into(),
                    entities: vec!["PlanDocument".into()],
                })],
            }],
        }] }],
        assumptions: Vec::new(),
    }
}

#[cfg(test)]
pub(crate) fn test_subtask_fixture() -> PlanSubtask {
    PlanSubtask::Test(PlanTestSubtask {
        operation: TestSubtaskOperation::Test,
        action: ChangeAction::Add,
        renamed_from: None,
        name: "validates_plans".into(),
        category: TestCategory::Unit,
        behavior: "Reject malformed plans.".into(),
        covers_entities: vec!["PlanDocument".into()],
    })
}

#[cfg(test)]
pub(crate) fn integration_test_subtask_fixture() -> PlanSubtask {
    PlanSubtask::Test(PlanTestSubtask {
        operation: TestSubtaskOperation::Test,
        action: ChangeAction::Add,
        renamed_from: None,
        name: "submits_complete_plan".into(),
        category: TestCategory::Integration,
        behavior: "Submit one complete plan through the real broker boundary.".into(),
        covers_entities: vec!["PlanDocument".into()],
    })
}

#[cfg(test)]
pub(crate) fn attach_test_fixture(document: &mut PlanDocument) {
    document.stages[0].tasks[0].files[0]
        .subtasks
        .push(test_subtask_fixture());
}

#[cfg(test)]
pub(crate) fn attach_integration_test_fixture(document: &mut PlanDocument) {
    document.stages[0].tasks[0].files.push(PlanFile {
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
        assert!(
            serde_json::from_value::<PlanFile>(serde_json::json!({
                "action": "add",
                "path": "src/new.rs",
                "subtasks": [],
                "legacy_operation": "create"
            }))
            .is_err()
        );
    }

    #[test]
    fn deserializes_structured_callable_return_types() {
        let return_type = serde_json::from_value::<PlanFlowReturnType>(serde_json::json!({
            "value_type": "InspectionReport",
            "error_type": "InspectError"
        }))
        .unwrap();
        assert_eq!(return_type.value_type, "InspectionReport");
        assert_eq!(return_type.error_type.as_deref(), Some("InspectError"));
    }

    #[test]
    fn deserializes_typed_runtime_edges_and_rejects_legacy_step_transitions() {
        let edge = serde_json::from_value::<PlanFlowEdge>(serde_json::json!({
            "relation": "call",
            "target": {"kind": "planned_entity", "entity": "GeoParquetInspector"},
            "callable": {"kind": "method", "name": "inspect"},
            "expansion": [],
            "branches": [],
            "return_type": {"value_type": "InspectionReport"}
        }))
        .unwrap();
        assert_eq!(edge.relation, PlanFlowRelation::Call);
        assert_eq!(edge.callable.unwrap().name, "inspect");
    }

    #[test]
    fn deserializes_recursive_flow_trees_and_requires_explicit_child_arrays() {
        let flow = serde_json::from_value::<PlanFlow>(serde_json::json!({
            "title": "Inspection",
            "description": "Inspect one file and return a report. Keep file interpretation inside the inspector.",
            "source": {"kind": "planned_entity", "entity": "main"},
            "edges": [{
                    "relation": "call",
                    "target": {
                        "kind": "planned_entity",
                        "entity": "GeoParquetInspector"
                    },
                    "callable": {"kind": "method", "name": "inspect"},
                    "expansion": [],
                    "branches": [{
                    "condition": "failure",
                    "edges": [{
                        "relation": "emit",
                        "target": {"kind": "external_entity", "entity_kind":"endpoint", "name": "stderr", "dependency": null},
                        "payload_type": "InspectError",
                        "expansion": [],
                        "branches": []
                    }]
                }]
            }]
        }))
        .unwrap();

        assert_eq!(flow.edges[0].branches[0].condition, "failure");
        assert!(
            serde_json::from_value::<PlanFlowEdge>(serde_json::json!({
                "relation": "construct",
                "target": {"kind": "planned_entity", "entity": "Inspector"},
                "expansion": []
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
            Some(&serde_json::json!("PlanDocument"))
        );
        assert!(value.get("tests").is_none());
        assert!(value.get("entities").is_none());
        assert!(value.get("description").is_none());
    }

    #[test]
    fn serializes_tagged_entity_references() {
        let mut document = test_fixture("plan", "Build structured planning.");
        document.flows[0].source = EntityReference::WorkspaceEntity {
            entity_kind: ReferencedEntityKind::Type,
            name: "PlanValidator".into(),
            path: "src/plan/validation.rs".into(),
            line: 76,
        };
        document.flows[0].edges[0].target = EntityReference::ExternalEntity {
            entity_kind: ReferencedEntityKind::Endpoint,
            name: "terminal stdout".into(),
            dependency: None,
        };
        let value = serde_json::to_value(document).unwrap();

        assert_eq!(
            value.pointer("/flows/0/source/kind"),
            Some(&serde_json::json!("workspace_entity"))
        );
        assert_eq!(
            value.pointer("/flows/0/source/path"),
            Some(&serde_json::json!("src/plan/validation.rs"))
        );
        assert_eq!(
            value.pointer("/flows/0/source/line"),
            Some(&serde_json::json!(76))
        );
        assert_eq!(
            value.pointer("/flows/0/edges/0/target/kind"),
            Some(&serde_json::json!("external_entity"))
        );
        assert_eq!(
            value.pointer("/flows/0/edges/0/target/name"),
            Some(&serde_json::json!("terminal stdout"))
        );
        assert_eq!(
            value.pointer("/flows/0/edges/0/target/entity_kind"),
            Some(&serde_json::json!("endpoint"))
        );
    }

    #[test]
    fn rejects_ambiguous_entity_references() {
        let result = serde_json::from_value::<EntityReference>(serde_json::json!({
            "kind": "planned_entity",
            "entity": "PlanDocument",
            "external_entity_name": "CLI"
        }));

        assert!(result.is_err());
    }

    #[test]
    fn model_json_omits_generated_node_ids() {
        let mut document = test_fixture("plan", "Build structured planning.");
        document.flows[0].edges.push(PlanFlowEdge {
            relation: PlanFlowRelation::Call,
            target: EntityReference::ExternalEntity {
                entity_kind: ReferencedEntityKind::Type,
                name: "PlanValidator".into(),
                dependency: None,
            },
            callable: Some(PlanCallable {
                kind: PlanCallableKind::Method,
                name: "validate".into(),
            }),
            payload_type: None,
            return_type: Some(PlanFlowReturnType {
                value_type: "ValidatedPlan".into(),
                error_type: None,
            }),
            expansion: Vec::new(),
            branches: Vec::new(),
        });
        document.dependencies.push(PlanDependencyChange {
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
        assert!(model_json.contains(r#""value_type": "ValidatedPlan""#));
    }

    #[test]
    fn requires_the_explicit_plan_document_schema_version() {
        let mut value = serde_json::to_value(test_fixture("plan", "Version plans.")).unwrap();
        value.as_object_mut().unwrap().remove("schema_version");
        let missing = serde_json::from_value::<PlanDocument>(value).unwrap_err();
        assert!(missing.to_string().contains("schema_version"));

        let mut document = test_fixture("plan", "Version plans.");
        document.schema_version = PLAN_SCHEMA_VERSION + 1;
        let unsupported = document.validate().unwrap_err().to_string();
        assert!(unsupported.contains("supported PlanDocument schema version 7"));
    }
}
