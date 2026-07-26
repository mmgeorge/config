use std::collections::{HashMap, HashSet};

use anyhow::{Context, Result};
use serde::{Deserialize, Deserializer, Serialize};

use super::document::*;

/// Represents an omitted, cleared, or replaced nullable patch field.
#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
#[serde(untagged)]
pub enum PatchField<T> {
    Missing,
    Null,
    Value(T),
}

impl<T> Default for PatchField<T> {
    fn default() -> Self {
        Self::Missing
    }
}

impl<'de, T> Deserialize<'de> for PatchField<T>
where
    T: Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        Option::<T>::deserialize(deserializer).map(|value| match value {
            Some(value) => Self::Value(value),
            None => Self::Null,
        })
    }
}

impl<T> PatchField<T> {
    fn apply(self, target: &mut Option<T>) {
        match self {
            Self::Missing => {}
            Self::Null => *target = None,
            Self::Value(value) => *target = Some(value),
        }
    }
}

/// Applies the same add, modify, and remove vocabulary to one typed collection.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(default, deny_unknown_fields)]
pub struct CollectionMutation<T, P> {
    pub add: Vec<T>,
    pub modify: Vec<P>,
    pub remove: Vec<String>,
}

impl<T, P> Default for CollectionMutation<T, P> {
    fn default() -> Self {
        Self {
            add: Vec::new(),
            modify: Vec::new(),
            remove: Vec::new(),
        }
    }
}

impl<T, P> CollectionMutation<T, P> {
    fn is_empty(&self) -> bool {
        self.add.is_empty() && self.modify.is_empty() && self.remove.is_empty()
    }
}

/// Modifies scalar plan fields without replacing the canonical document.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(default, deny_unknown_fields)]
pub struct PlanFieldPatch {
    pub title: Option<String>,
    pub overview: Option<String>,
    pub usage: PatchField<PlanUsage>,
}

/// Owns scalar plan-field mutation.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(default, deny_unknown_fields)]
pub struct PlanFieldMutation {
    pub modify: PlanFieldPatch,
}

/// Modifies one enum-variant payload field.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct EnumVariantFieldPatch {
    #[serde(rename = "field")]
    pub field_id: String,
    pub action: Option<ChangeAction>,
    pub name: Option<String>,
    #[serde(rename = "type")]
    pub type_name: Option<String>,
}

/// Modifies one enum variant without replacing its owning enum.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct EnumVariantPatch {
    #[serde(rename = "variant")]
    pub variant_id: String,
    pub action: Option<ChangeAction>,
    pub name: Option<String>,
    pub description: Option<String>,
    pub fields: Option<CollectionMutation<EnumVariantFieldChange, EnumVariantFieldPatch>>,
}

/// Modifies one member without replacing its owning program entity.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct ProgramEntityMemberPatch {
    #[serde(rename = "member")]
    pub member_id: String,
    pub action: Option<ChangeAction>,
    pub kind: Option<MemberKind>,
    pub name: Option<String>,
    pub description: Option<String>,
    #[serde(default)]
    pub visibility: PatchField<Visibility>,
    #[serde(default, rename = "type")]
    pub type_name: PatchField<String>,
    pub parameters: Option<Vec<FunctionParameter>>,
    #[serde(default)]
    pub return_type: PatchField<String>,
}

/// Modifies one program entity and optionally its nested members.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct ProgramEntityPatch {
    #[serde(rename = "entity")]
    pub entity_id: String,
    pub action: Option<ChangeAction>,
    pub kind: Option<EntityKind>,
    pub name: Option<String>,
    pub description: Option<String>,
    pub path: Option<String>,
    pub members: Option<CollectionMutation<ProgramEntityMemberChange, ProgramEntityMemberPatch>>,
    pub variants: Option<CollectionMutation<EnumVariantChange, EnumVariantPatch>>,
    #[serde(default)]
    pub extends: PatchField<EntityReference>,
    pub conforms_to: Option<Vec<EntityReference>>,
    #[serde(default)]
    #[serde(rename = "exclusive_owner_entity")]
    pub exclusive_owner_entity_id: PatchField<String>,
}

/// Modifies one package dependency without replacing its durable identity.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct PlanDependencyPatch {
    pub dependency: String,
    pub action: Option<ChangeAction>,
    pub name: Option<String>,
    pub version: Option<String>,
    pub manifest: Option<String>,
    #[serde(default)]
    pub license: PatchField<String>,
    pub justification: Option<String>,
}

/// Modifies one flow step without replacing its flow.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct PlanFlowStepPatch {
    #[serde(rename = "step")]
    pub step_id: String,
    pub action: Option<String>,
    pub target: Option<EntityReference>,
    pub operations: Option<Vec<PlanFlowOperation>>,
    #[serde(default)]
    pub value_to_next: PatchField<PlanFlowValue>,
}

/// Modifies one flow and optionally its nested steps.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct PlanFlowPatch {
    #[serde(rename = "flow")]
    pub flow_id: String,
    pub title: Option<String>,
    pub description: Option<String>,
    pub steps: Option<CollectionMutation<PlanFlowStep, PlanFlowStepPatch>>,
}

/// Modifies one implementation subtask without replacing its file.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct PlanWorkSubtaskPatch {
    #[serde(rename = "subtask")]
    pub subtask_id: String,
    #[serde(rename = "operation")]
    pub action: Option<SubtaskAction>,
    pub description: Option<String>,
    #[serde(rename = "entities")]
    pub entity_ids: Option<Vec<String>>,
}

/// Modifies one test subtask without replacing its file.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct PlanTestSubtaskPatch {
    #[serde(rename = "subtask")]
    pub subtask_id: String,
    #[serde(rename = "operation")]
    pub operation: TestSubtaskOperation,
    pub action: Option<ChangeAction>,
    pub name: Option<String>,
    pub category: Option<TestCategory>,
    pub behavior: Option<String>,
    #[serde(rename = "covers_entities")]
    pub covered_entity_ids: Option<Vec<String>>,
}

/// Modifies one implementation or test subtask through its exact role.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(untagged)]
pub enum PlanSubtaskPatch {
    Test(PlanTestSubtaskPatch),
    Work(PlanWorkSubtaskPatch),
}

impl PlanSubtaskPatch {
    /// Resolve the existing subtask selected by this patch.
    fn subtask_id(&self) -> &str {
        match self {
            Self::Test(patch) => &patch.subtask_id,
            Self::Work(patch) => &patch.subtask_id,
        }
    }
}

/// Modifies one file and optionally its nested subtasks.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct PlanFilePatch {
    pub path: String,
    pub action: Option<ChangeAction>,
    pub subtasks: Option<CollectionMutation<PlanSubtask, PlanSubtaskPatch>>,
}

/// Modifies one task and optionally its nested files.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct PlanTaskPatch {
    #[serde(rename = "task")]
    pub task_id: String,
    pub title: Option<String>,
    pub description: Option<String>,
    pub files: Option<CollectionMutation<PlanFile, PlanFilePatch>>,
}

/// Modifies one explicit plan assumption.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct PlanAssumptionPatch {
    pub assumption: String,
    pub text: String,
}

/// Owns every typed mutation supported by one plan edit.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(default, deny_unknown_fields)]
pub struct PlanMutation {
    pub plan: Option<PlanFieldMutation>,
    pub entity_changes: Option<CollectionMutation<ProgramEntityChange, ProgramEntityPatch>>,
    pub dependencies: Option<CollectionMutation<PlanDependencyChange, PlanDependencyPatch>>,
    pub flows: Option<CollectionMutation<PlanFlow, PlanFlowPatch>>,
    pub tasks: Option<CollectionMutation<PlanTask, PlanTaskPatch>>,
    pub assumptions: Option<CollectionMutation<String, PlanAssumptionPatch>>,
}

impl PlanMutation {
    /// Report whether the request carries any semantic mutation.
    pub fn is_empty(&self) -> bool {
        let plan_empty = self.plan.as_ref().is_none_or(|plan| {
            plan.modify.title.is_none()
                && plan.modify.overview.is_none()
                && matches!(plan.modify.usage, PatchField::Missing)
        });
        plan_empty
            && self
                .entity_changes
                .as_ref()
                .is_none_or(CollectionMutation::is_empty)
            && self
                .dependencies
                .as_ref()
                .is_none_or(CollectionMutation::is_empty)
            && self.flows.as_ref().is_none_or(CollectionMutation::is_empty)
            && self.tasks.as_ref().is_none_or(CollectionMutation::is_empty)
            && self
                .assumptions
                .as_ref()
                .is_none_or(CollectionMutation::is_empty)
    }
}

/// Represents one optimistic atomic edit request.
#[derive(Clone, Debug, Serialize)]
pub struct PlanEditRequest {
    pub plan_id: String,
    pub expected_version: u64,
    #[serde(flatten)]
    pub mutation: PlanMutation,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct PlanEditRequestWire {
    plan_id: String,
    expected_version: u64,
    plan: Option<PlanFieldMutation>,
    entity_changes: Option<CollectionMutation<ProgramEntityChange, ProgramEntityPatch>>,
    dependencies: Option<CollectionMutation<PlanDependencyChange, PlanDependencyPatch>>,
    flows: Option<CollectionMutation<PlanFlow, PlanFlowPatch>>,
    tasks: Option<CollectionMutation<PlanTask, PlanTaskPatch>>,
    assumptions: Option<CollectionMutation<String, PlanAssumptionPatch>>,
}

impl<'de> Deserialize<'de> for PlanEditRequest {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let wire = PlanEditRequestWire::deserialize(deserializer)?;
        Ok(Self {
            plan_id: wire.plan_id,
            expected_version: wire.expected_version,
            mutation: PlanMutation {
                plan: wire.plan,
                entity_changes: wire.entity_changes,
                dependencies: wire.dependencies,
                flows: wire.flows,
                tasks: wire.tasks,
                assumptions: wire.assumptions,
            },
        })
    }
}

/// Reports the canonical document after one committed edit batch.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct PlanEditResult {
    pub plan_id: String,
    pub version: u64,
    pub document: PlanDocument,
}

/// Apply one typed mutation batch without exposing partial state.
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
        !request.mutation.is_empty(),
        "plan edit requires a mutation"
    );

    let mut updated = document.clone();
    apply_plan_mutation(&mut updated, request.mutation)?;
    updated.plan_id.clone_from(&document.plan_id);
    updated.version = document.version;
    updated.validate()?;
    if updated == *document {
        return Ok(PlanEditResult {
            plan_id: document.plan_id.clone(),
            version: document.version,
            document: document.clone(),
        });
    }
    updated.version = document.version.saturating_add(1);
    Ok(PlanEditResult {
        plan_id: updated.plan_id.clone(),
        version: updated.version,
        document: updated,
    })
}

/// Apply reusable plan mutations to an in-memory document.
pub fn apply_plan_mutation(document: &mut PlanDocument, mutation: PlanMutation) -> Result<()> {
    let previous_name_by_id = document
        .entity_changes
        .iter()
        .map(|entity| (entity.entity_id.clone(), entity.name.clone()))
        .collect::<HashMap<_, _>>();
    let mut mutation = mutation;
    normalize_added_identity(&mut mutation);
    if let Some(plan) = mutation.plan {
        apply_plan_field_patch(document, plan.modify);
    }
    if let Some(entity_changes) = mutation.entity_changes {
        apply_collection_mutation(
            &mut document.entity_changes,
            entity_changes,
            |entity| entity.name.as_str(),
            |patch| patch.entity_id.as_str(),
            apply_entity_patch,
            "program entity",
        )?;
    }
    if let Some(dependencies) = mutation.dependencies {
        apply_collection_mutation(
            &mut document.dependencies,
            dependencies,
            |dependency| dependency.name.as_str(),
            |patch| patch.dependency.as_str(),
            apply_dependency_patch,
            "dependency",
        )?;
    }
    if let Some(flows) = mutation.flows {
        apply_collection_mutation(
            &mut document.flows,
            flows,
            |flow| flow.title.as_str(),
            |patch| patch.flow_id.as_str(),
            apply_flow_patch,
            "flow",
        )?;
    }
    if let Some(tasks) = mutation.tasks {
        apply_collection_mutation(
            &mut document.tasks,
            tasks,
            |task| task.title.as_str(),
            |patch| patch.task_id.as_str(),
            apply_task_patch,
            "task",
        )?;
    }
    if let Some(assumptions) = mutation.assumptions {
        apply_collection_mutation(
            &mut document.assumptions,
            assumptions,
            String::as_str,
            |patch| patch.assumption.as_str(),
            apply_assumption_patch,
            "assumption",
        )?;
    }
    propagate_entity_rename(document, &previous_name_by_id);
    Ok(())
}

fn propagate_entity_rename(
    document: &mut PlanDocument,
    previous_name_by_id: &HashMap<String, String>,
) {
    let rename_by_previous_name = document
        .entity_changes
        .iter()
        .filter_map(|entity| {
            let previous_name = previous_name_by_id.get(&entity.entity_id)?;
            (previous_name != &entity.name).then(|| (previous_name.clone(), entity.name.clone()))
        })
        .collect::<HashMap<_, _>>();
    if rename_by_previous_name.is_empty() {
        return;
    }
    let rename = |value: &mut String| {
        if let Some(name) = rename_by_previous_name.get(value) {
            value.clone_from(name);
        }
    };
    for entity in &mut document.entity_changes {
        if let Some(EntityReference::PlannedEntity { entity }) = &mut entity.extends {
            rename(entity);
        }
        for reference in &mut entity.conforms_to {
            if let EntityReference::PlannedEntity { entity } = reference {
                rename(entity);
            }
        }
        if let Some(owner) = &mut entity.exclusive_owner_entity_id {
            rename(owner);
        }
    }
    for flow in &mut document.flows {
        for step in &mut flow.steps {
            if let EntityReference::PlannedEntity { entity } = &mut step.target {
                rename(entity);
            }
            for operation in &mut step.operations {
                if let EntityReference::PlannedEntity { entity } = &mut operation.target {
                    rename(entity);
                }
            }
        }
    }
    for task in &mut document.tasks {
        for file in &mut task.files {
            for subtask in &mut file.subtasks {
                if let Some(entity_ids) = subtask.owned_entity_ids_mut() {
                    for entity in entity_ids {
                        rename(entity);
                    }
                }
                if let PlanSubtask::Test(test) = subtask {
                    for entity in &mut test.covered_entity_ids {
                        rename(entity);
                    }
                }
            }
        }
    }
}

fn apply_plan_field_patch(document: &mut PlanDocument, patch: PlanFieldPatch) {
    if let Some(title) = patch.title {
        document.title = title;
    }
    if let Some(overview) = patch.overview {
        document.overview = overview;
    }
    patch.usage.apply(&mut document.usage);
}

fn apply_entity_patch(entity: &mut ProgramEntityChange, patch: ProgramEntityPatch) -> Result<()> {
    assign(&mut entity.action, patch.action);
    assign(&mut entity.kind, patch.kind);
    assign(&mut entity.name, patch.name);
    assign(&mut entity.description, patch.description);
    assign(&mut entity.path, patch.path);
    patch.extends.apply(&mut entity.extends);
    assign(&mut entity.conforms_to, patch.conforms_to);
    patch
        .exclusive_owner_entity_id
        .apply(&mut entity.exclusive_owner_entity_id);
    if let Some(members) = patch.members {
        apply_collection_mutation(
            &mut entity.members,
            members,
            |member| member.name.as_str(),
            |patch| patch.member_id.as_str(),
            apply_member_patch,
            "entity member",
        )?;
    }
    if let Some(variants) = patch.variants {
        apply_collection_mutation(
            &mut entity.variants,
            variants,
            |variant| variant.name.as_str(),
            |patch| patch.variant_id.as_str(),
            apply_variant_patch,
            "enum variant",
        )?;
    }
    Ok(())
}

fn apply_dependency_patch(
    dependency: &mut PlanDependencyChange,
    patch: PlanDependencyPatch,
) -> Result<()> {
    assign(&mut dependency.action, patch.action);
    assign(&mut dependency.name, patch.name);
    assign(&mut dependency.version, patch.version);
    assign(&mut dependency.manifest, patch.manifest);
    patch.license.apply(&mut dependency.license);
    assign(&mut dependency.justification, patch.justification);
    Ok(())
}

fn normalize_added_identity(mutation: &mut PlanMutation) {
    if let Some(entity_changes) = &mut mutation.entity_changes {
        for entity in &mut entity_changes.add {
            if entity.entity_id.is_empty() {
                entity.entity_id = format!("entity_{}", semantic_identity(&entity.name));
            }
            for member in &mut entity.members {
                normalize_member_identity(&entity.entity_id, member);
            }
            for variant in &mut entity.variants {
                normalize_variant_identity(&entity.entity_id, variant);
            }
        }
        for entity in &mut entity_changes.modify {
            let entity_identity = format!("entity_{}", semantic_identity(&entity.entity_id));
            if let Some(members) = &mut entity.members {
                for member in &mut members.add {
                    normalize_member_identity(&entity_identity, member);
                }
            }
            if let Some(variants) = &mut entity.variants {
                for variant in &mut variants.add {
                    normalize_variant_identity(&entity_identity, variant);
                }
                for variant in &mut variants.modify {
                    let variant_identity = format!(
                        "{}_variant_{}",
                        entity_identity,
                        semantic_identity(&variant.variant_id)
                    );
                    if let Some(fields) = &mut variant.fields {
                        for field in &mut fields.add {
                            normalize_field_identity(&variant_identity, field);
                        }
                    }
                }
            }
        }
    }
    if let Some(dependencies) = &mut mutation.dependencies {
        for dependency in &mut dependencies.add {
            if dependency.dependency_id.is_empty() {
                dependency.dependency_id =
                    format!("dependency_{}", semantic_identity(&dependency.name));
            }
        }
    }
    if let Some(flows) = &mut mutation.flows {
        for flow in &mut flows.add {
            if flow.flow_id.is_empty() {
                flow.flow_id = format!("flow_{}", semantic_identity(&flow.title));
            }
            for step in &mut flow.steps {
                normalize_step_identity(&flow.flow_id, step);
            }
        }
        for flow in &mut flows.modify {
            if let Some(steps) = &mut flow.steps {
                let flow_identity = format!("flow_{}", semantic_identity(&flow.flow_id));
                for step in &mut steps.add {
                    normalize_step_identity(&flow_identity, step);
                }
                for step in &mut steps.modify {
                    if let Some(operation_list) = &mut step.operations {
                        let step_identity = format!(
                            "{}_step_{}",
                            flow_identity,
                            semantic_identity(&step.step_id)
                        );
                        for operation in operation_list {
                            normalize_operation_identity(&step_identity, operation);
                        }
                    }
                }
            }
        }
    }
    if let Some(tasks) = &mut mutation.tasks {
        for task in &mut tasks.add {
            if task.task_id.is_empty() {
                task.task_id = format!("task_{}", semantic_identity(&task.title));
            }
            for file in &mut task.files {
                for subtask in &mut file.subtasks {
                    normalize_subtask_identity(&task.task_id, subtask);
                }
            }
        }
        for task in &mut tasks.modify {
            let task_identity = format!("task_{}", semantic_identity(&task.task_id));
            if let Some(files) = &mut task.files {
                for file in &mut files.add {
                    for subtask in &mut file.subtasks {
                        normalize_subtask_identity(&task_identity, subtask);
                    }
                }
                for file in &mut files.modify {
                    if let Some(subtasks) = &mut file.subtasks {
                        for subtask in &mut subtasks.add {
                            normalize_subtask_identity(&task_identity, subtask);
                        }
                    }
                }
            }
        }
    }
}

/// Restore Harness-owned identities after decoding a semantic model projection.
pub(crate) fn restore_internal_identity(document: &mut PlanDocument) {
    for entity in &mut document.entity_changes {
        if entity.entity_id.is_empty() {
            entity.entity_id = format!("entity_{}", semantic_identity(&entity.name));
        }
        for member in &mut entity.members {
            normalize_member_identity(&entity.entity_id, member);
        }
        for variant in &mut entity.variants {
            normalize_variant_identity(&entity.entity_id, variant);
        }
    }
    for dependency in &mut document.dependencies {
        if dependency.dependency_id.is_empty() {
            dependency.dependency_id =
                format!("dependency_{}", semantic_identity(&dependency.name));
        }
    }
    for flow in &mut document.flows {
        if flow.flow_id.is_empty() {
            flow.flow_id = format!("flow_{}", semantic_identity(&flow.title));
        }
        for step in &mut flow.steps {
            normalize_step_identity(&flow.flow_id, step);
        }
    }
    for task in &mut document.tasks {
        if task.task_id.is_empty() {
            task.task_id = format!("task_{}", semantic_identity(&task.title));
        }
        for file in &mut task.files {
            for subtask in &mut file.subtasks {
                normalize_subtask_identity(&task.task_id, subtask);
            }
        }
    }
}

fn normalize_member_identity(entity_identity: &str, member: &mut ProgramEntityMemberChange) {
    if member.member_id.is_empty() {
        member.member_id = format!(
            "{}_member_{}",
            entity_identity,
            semantic_identity(&member.name)
        );
    }
}

fn normalize_variant_identity(entity_identity: &str, variant: &mut EnumVariantChange) {
    if variant.variant_id.is_empty() {
        variant.variant_id = format!(
            "{}_variant_{}",
            entity_identity,
            semantic_identity(&variant.name)
        );
    }
    for field in &mut variant.fields {
        normalize_field_identity(&variant.variant_id, field);
    }
}

fn normalize_field_identity(variant_identity: &str, field: &mut EnumVariantFieldChange) {
    if field.field_id.is_empty() {
        field.field_id = format!(
            "{}_field_{}",
            variant_identity,
            semantic_identity(&field.name)
        );
    }
}

fn normalize_step_identity(flow_identity: &str, step: &mut PlanFlowStep) {
    if step.step_id.is_empty() {
        step.step_id = format!("{}_step_{}", flow_identity, semantic_identity(&step.action));
    }
    for operation in &mut step.operations {
        normalize_operation_identity(&step.step_id, operation);
    }
}

fn normalize_operation_identity(step_identity: &str, operation: &mut PlanFlowOperation) {
    if operation.operation_id.is_empty() {
        operation.operation_id = format!(
            "{}_operation_{}",
            step_identity,
            semantic_identity(&operation.action)
        );
    }
}

fn normalize_subtask_identity(task_identity: &str, subtask: &mut PlanSubtask) {
    if subtask.subtask_id().is_empty() {
        let identity_seed = match subtask {
            PlanSubtask::Test(test) => &test.name,
            PlanSubtask::Work(work) => &work.description,
        };
        *subtask.subtask_id_mut() = format!(
            "{}_subtask_{}",
            task_identity,
            semantic_identity(identity_seed)
        );
    }
}

fn semantic_identity(value: &str) -> String {
    let mut identity = String::new();
    let mut separator_pending = false;
    let mut previous_was_lowercase_or_digit = false;
    for character in value.chars() {
        if character.is_ascii_alphanumeric() {
            if (separator_pending
                || character.is_ascii_uppercase() && previous_was_lowercase_or_digit)
                && !identity.is_empty()
            {
                identity.push('_');
            }
            identity.extend(character.to_lowercase());
            separator_pending = false;
            previous_was_lowercase_or_digit =
                character.is_ascii_lowercase() || character.is_ascii_digit();
        } else {
            separator_pending = true;
            previous_was_lowercase_or_digit = false;
        }
    }
    identity
}

fn apply_member_patch(
    member: &mut ProgramEntityMemberChange,
    patch: ProgramEntityMemberPatch,
) -> Result<()> {
    assign(&mut member.action, patch.action);
    assign(&mut member.kind, patch.kind);
    assign(&mut member.name, patch.name);
    assign(&mut member.description, patch.description);
    patch.visibility.apply(&mut member.visibility);
    patch.type_name.apply(&mut member.type_name);
    assign(&mut member.parameters, patch.parameters);
    patch.return_type.apply(&mut member.return_type);
    Ok(())
}

fn apply_variant_patch(variant: &mut EnumVariantChange, patch: EnumVariantPatch) -> Result<()> {
    assign(&mut variant.action, patch.action);
    assign(&mut variant.name, patch.name);
    assign(&mut variant.description, patch.description);
    if let Some(fields) = patch.fields {
        apply_collection_mutation(
            &mut variant.fields,
            fields,
            |field| field.name.as_str(),
            |patch| patch.field_id.as_str(),
            apply_variant_field_patch,
            "variant payload field",
        )?;
    }
    Ok(())
}

fn apply_variant_field_patch(
    field: &mut EnumVariantFieldChange,
    patch: EnumVariantFieldPatch,
) -> Result<()> {
    assign(&mut field.action, patch.action);
    assign(&mut field.name, patch.name);
    assign(&mut field.type_name, patch.type_name);
    Ok(())
}

fn apply_flow_patch(flow: &mut PlanFlow, patch: PlanFlowPatch) -> Result<()> {
    assign(&mut flow.title, patch.title);
    assign(&mut flow.description, patch.description);
    if let Some(steps) = patch.steps {
        apply_collection_mutation(
            &mut flow.steps,
            steps,
            |step| step.action.as_str(),
            |patch| patch.step_id.as_str(),
            apply_flow_step_patch,
            "flow step",
        )?;
    }
    Ok(())
}

fn apply_flow_step_patch(step: &mut PlanFlowStep, patch: PlanFlowStepPatch) -> Result<()> {
    assign(&mut step.action, patch.action);
    assign(&mut step.target, patch.target);
    assign(&mut step.operations, patch.operations);
    patch.value_to_next.apply(&mut step.value_to_next);
    Ok(())
}

fn apply_task_patch(task: &mut PlanTask, patch: PlanTaskPatch) -> Result<()> {
    assign(&mut task.title, patch.title);
    assign(&mut task.description, patch.description);
    if let Some(files) = patch.files {
        apply_collection_mutation(
            &mut task.files,
            files,
            |file| file.path.as_str(),
            |patch| patch.path.as_str(),
            apply_file_patch,
            "task file",
        )?;
    }
    Ok(())
}

fn apply_file_patch(file: &mut PlanFile, patch: PlanFilePatch) -> Result<()> {
    assign(&mut file.action, patch.action);
    if let Some(subtasks) = patch.subtasks {
        apply_collection_mutation(
            &mut file.subtasks,
            subtasks,
            |subtask| match subtask {
                PlanSubtask::Test(test) => test.name.as_str(),
                PlanSubtask::Work(work) => work.description.as_str(),
            },
            PlanSubtaskPatch::subtask_id,
            apply_subtask_patch,
            "subtask",
        )?;
    }
    Ok(())
}

fn apply_subtask_patch(subtask: &mut PlanSubtask, patch: PlanSubtaskPatch) -> Result<()> {
    match (subtask, patch) {
        (PlanSubtask::Work(subtask), PlanSubtaskPatch::Work(patch)) => {
            assign(&mut subtask.action, patch.action);
            assign(&mut subtask.description, patch.description);
            assign(&mut subtask.entity_ids, patch.entity_ids);
            Ok(())
        }
        (PlanSubtask::Test(subtask), PlanSubtaskPatch::Test(patch)) => {
            assign(&mut subtask.action, patch.action);
            assign(&mut subtask.name, patch.name);
            assign(&mut subtask.category, patch.category);
            assign(&mut subtask.behavior, patch.behavior);
            assign(&mut subtask.covered_entity_ids, patch.covered_entity_ids);
            Ok(())
        }
        _ => anyhow::bail!("subtask patch cannot change between implementation and test roles"),
    }
}

fn apply_assumption_patch(assumption: &mut String, patch: PlanAssumptionPatch) -> Result<()> {
    *assumption = patch.text;
    Ok(())
}

fn assign<T>(target: &mut T, value: Option<T>) {
    if let Some(value) = value {
        *target = value;
    }
}

fn apply_collection_mutation<T, P>(
    values: &mut Vec<T>,
    mutation: CollectionMutation<T, P>,
    value_id: impl Fn(&T) -> &str,
    patch_id: impl Fn(&P) -> &str,
    apply_patch: impl Fn(&mut T, P) -> Result<()>,
    kind: &str,
) -> Result<()> {
    let mut target_set = HashSet::new();
    for value in &mutation.add {
        ensure_unique_target(&mut target_set, value_id(value), kind)?;
    }
    for patch in &mutation.modify {
        ensure_unique_target(&mut target_set, patch_id(patch), kind)?;
    }
    for id in &mutation.remove {
        ensure_unique_target(&mut target_set, id, kind)?;
    }

    for value in mutation.add {
        let id = value_id(&value).to_owned();
        anyhow::ensure!(
            values.iter().all(|candidate| value_id(candidate) != id),
            "{kind} {id} already exists"
        );
        values.push(value);
    }
    for patch in mutation.modify {
        let id = patch_id(&patch).to_owned();
        let value = values
            .iter_mut()
            .find(|candidate| value_id(candidate) == id)
            .with_context(|| format!("{kind} {id} not found"))?;
        apply_patch(value, patch)?;
    }
    for id in mutation.remove {
        let index = values
            .iter()
            .position(|candidate| value_id(candidate) == id)
            .with_context(|| format!("{kind} {id} not found"))?;
        values.remove(index);
    }
    Ok(())
}

fn ensure_unique_target(target_set: &mut HashSet<String>, id: &str, kind: &str) -> Result<()> {
    anyhow::ensure!(
        target_set.insert(id.to_owned()),
        "{kind} {id} appears in multiple mutation operations"
    );
    Ok(())
}

#[cfg(test)]
mod test {
    use super::*;

    fn empty_document() -> PlanDocument {
        PlanDocument {
            version: 4,
            plan_id: "plan".into(),
            title: "Plan".into(),
            prompt: "Plan the change.".into(),
            overview: "Initial".into(),
            usage: None,
            entity_changes: Vec::new(),
            dependencies: Vec::new(),
            flows: Vec::new(),
            tasks: Vec::new(),
            assumptions: Vec::new(),
        }
    }

    #[test]
    fn applies_atomic_typed_edits_and_increments_once() {
        let result = apply_plan_edit(
            &empty_document(),
            PlanEditRequest {
                plan_id: "plan".into(),
                expected_version: 4,
                mutation: PlanMutation {
                    plan: Some(PlanFieldMutation {
                        modify: PlanFieldPatch {
                            overview: Some("Changed".into()),
                            ..PlanFieldPatch::default()
                        },
                    }),
                    assumptions: Some(CollectionMutation {
                        add: vec!["Stable".into()],
                        ..CollectionMutation::default()
                    }),
                    ..PlanMutation::default()
                },
            },
        )
        .unwrap();
        assert_eq!(result.version, 5);
        assert_eq!(result.document.overview, "Changed");
        assert_eq!(result.document.assumptions.len(), 1);
    }

    #[test]
    fn generates_internal_identity_and_modifies_by_semantic_name() {
        let added = apply_plan_edit(
            &empty_document(),
            PlanEditRequest {
                plan_id: "plan".into(),
                expected_version: 4,
                mutation: PlanMutation {
                    entity_changes: Some(CollectionMutation {
                        add: vec![ProgramEntityChange {
                            entity_id: String::new(),
                            action: ChangeAction::Add,
                            kind: EntityKind::Resource,
                            name: "DraftCache".into(),
                            description: "Own pending drafts.".into(),
                            path: "src/draft_sync.rs".into(),
                            members: vec![ProgramEntityMemberChange {
                                member_id: String::new(),
                                action: ChangeAction::Add,
                                kind: MemberKind::Method,
                                name: "store".into(),
                                description: "Store one draft.".into(),
                                visibility: Some(Visibility::Public),
                                type_name: None,
                                parameters: Vec::new(),
                                return_type: None,
                            }],
                            variants: Vec::new(),
                            extends: None,
                            conforms_to: Vec::new(),
                            exclusive_owner_entity_id: None,
                        }],
                        ..CollectionMutation::default()
                    }),
                    ..PlanMutation::default()
                },
            },
        )
        .unwrap();

        assert_eq!(
            added.document.entity_changes[0].entity_id,
            "entity_draft_cache"
        );
        assert_eq!(
            added.document.entity_changes[0].members[0].member_id,
            "entity_draft_cache_member_store"
        );

        let modified = apply_plan_edit(
            &added.document,
            PlanEditRequest {
                plan_id: "plan".into(),
                expected_version: 5,
                mutation: PlanMutation {
                    entity_changes: Some(CollectionMutation {
                        modify: vec![ProgramEntityPatch {
                            entity_id: "DraftCache".into(),
                            action: None,
                            kind: None,
                            name: None,
                            description: Some("Own durable pending drafts.".into()),
                            path: None,
                            members: None,
                            variants: None,
                            extends: PatchField::Missing,
                            conforms_to: None,
                            exclusive_owner_entity_id: PatchField::Missing,
                        }],
                        ..CollectionMutation::default()
                    }),
                    ..PlanMutation::default()
                },
            },
        )
        .unwrap();

        assert_eq!(
            modified.document.entity_changes[0].description,
            "Own durable pending drafts."
        );
        assert_eq!(
            modified.document.entity_changes[0].entity_id,
            "entity_draft_cache"
        );
    }

    #[test]
    fn edits_plain_text_assumptions_by_exact_value() {
        let mut document = empty_document();
        document.assumptions = vec!["Keep".into(), "Replace".into(), "Remove".into()];

        let result = apply_plan_edit(
            &document,
            PlanEditRequest {
                plan_id: "plan".into(),
                expected_version: 4,
                mutation: PlanMutation {
                    assumptions: Some(CollectionMutation {
                        add: vec!["Append".into()],
                        modify: vec![PlanAssumptionPatch {
                            assumption: "Replace".into(),
                            text: "Replaced".into(),
                        }],
                        remove: vec!["Remove".into()],
                    }),
                    ..PlanMutation::default()
                },
            },
        )
        .unwrap();

        assert_eq!(result.document.assumptions, ["Keep", "Replaced", "Append"]);
    }

    #[test]
    fn edits_dependencies_by_semantic_name() {
        let added = apply_plan_edit(
            &empty_document(),
            PlanEditRequest {
                plan_id: "plan".into(),
                expected_version: 4,
                mutation: PlanMutation {
                    dependencies: Some(CollectionMutation {
                        add: vec![PlanDependencyChange {
                            dependency_id: String::new(),
                            action: ChangeAction::Add,
                            name: "tokio".into(),
                            version: "1".into(),
                            manifest: "Cargo.toml".into(),
                            license: Some("MIT".into()),
                            justification: "Run asynchronous work.".into(),
                        }],
                        ..CollectionMutation::default()
                    }),
                    ..PlanMutation::default()
                },
            },
        )
        .unwrap();

        assert_eq!(
            added.document.dependencies[0].dependency_id,
            "dependency_tokio"
        );

        let modified = apply_plan_edit(
            &added.document,
            PlanEditRequest {
                plan_id: "plan".into(),
                expected_version: 5,
                mutation: PlanMutation {
                    dependencies: Some(CollectionMutation {
                        modify: vec![PlanDependencyPatch {
                            dependency: "tokio".into(),
                            action: None,
                            name: None,
                            version: Some("1.1".into()),
                            manifest: None,
                            license: PatchField::Missing,
                            justification: Some("Run draft persistence.".into()),
                        }],
                        ..CollectionMutation::default()
                    }),
                    ..PlanMutation::default()
                },
            },
        )
        .unwrap();

        assert_eq!(modified.document.dependencies[0].version, "1.1");
        assert_eq!(
            modified.document.dependencies[0].justification,
            "Run draft persistence."
        );
    }

    #[test]
    fn modifies_one_nested_member_without_replacing_its_entity() {
        let mut document = super::super::test_fixture("plan", "Modify one member.");
        document.entity_changes[0]
            .members
            .push(ProgramEntityMemberChange {
                member_id: "validate".into(),
                action: ChangeAction::Add,
                kind: MemberKind::Method,
                name: "validate".into(),
                description: "Validate the plan.".into(),
                visibility: Some(Visibility::Public),
                type_name: None,
                parameters: Vec::new(),
                return_type: Some("Result<()>".into()),
            });

        let result = apply_plan_edit(
            &document,
            PlanEditRequest {
                plan_id: "plan".into(),
                expected_version: 1,
                mutation: PlanMutation {
                    entity_changes: Some(CollectionMutation {
                        modify: vec![ProgramEntityPatch {
                            entity_id: "PlanDocument".into(),
                            action: None,
                            kind: None,
                            name: None,
                            description: None,
                            path: None,
                            members: Some(CollectionMutation {
                                modify: vec![ProgramEntityMemberPatch {
                                    member_id: "validate".into(),
                                    action: None,
                                    kind: None,
                                    name: None,
                                    description: Some("Validate one canonical plan.".into()),
                                    visibility: PatchField::Missing,
                                    type_name: PatchField::Missing,
                                    parameters: None,
                                    return_type: PatchField::Missing,
                                }],
                                ..CollectionMutation::default()
                            }),
                            variants: None,
                            extends: PatchField::Missing,
                            conforms_to: None,
                            exclusive_owner_entity_id: PatchField::Missing,
                        }],
                        ..CollectionMutation::default()
                    }),
                    ..PlanMutation::default()
                },
            },
        )
        .unwrap();

        assert_eq!(
            result.document.entity_changes[0].members[0].description,
            "Validate one canonical plan."
        );
    }

    #[test]
    fn adds_modifies_and_removes_enum_variants_through_their_own_resource() {
        let mut document = super::super::test_fixture("plan", "Edit enum variants.");
        document.entity_changes[0].kind = EntityKind::Enum;

        let added = apply_plan_edit(
            &document,
            PlanEditRequest {
                plan_id: "plan".into(),
                expected_version: 1,
                mutation: PlanMutation {
                    entity_changes: Some(CollectionMutation {
                        modify: vec![ProgramEntityPatch {
                            entity_id: "PlanDocument".into(),
                            action: None,
                            kind: None,
                            name: None,
                            description: None,
                            path: None,
                            members: None,
                            variants: Some(CollectionMutation {
                                add: vec![EnumVariantChange {
                                    variant_id: String::new(),
                                    action: ChangeAction::Add,
                                    name: "Ready".into(),
                                    description: "Marks a ready plan.".into(),
                                    fields: vec![EnumVariantFieldChange {
                                        field_id: String::new(),
                                        action: ChangeAction::Add,
                                        name: "version".into(),
                                        type_name: "u64".into(),
                                    }],
                                }],
                                ..CollectionMutation::default()
                            }),
                            extends: PatchField::Missing,
                            conforms_to: None,
                            exclusive_owner_entity_id: PatchField::Missing,
                        }],
                        ..CollectionMutation::default()
                    }),
                    ..PlanMutation::default()
                },
            },
        )
        .unwrap();
        assert_eq!(
            added.document.entity_changes[0].variants[0].variant_id,
            "entity_plan_document_variant_ready"
        );

        let modified = apply_plan_edit(
            &added.document,
            PlanEditRequest {
                plan_id: "plan".into(),
                expected_version: 2,
                mutation: PlanMutation {
                    entity_changes: Some(CollectionMutation {
                        modify: vec![ProgramEntityPatch {
                            entity_id: "PlanDocument".into(),
                            action: None,
                            kind: None,
                            name: None,
                            description: None,
                            path: None,
                            members: None,
                            variants: Some(CollectionMutation {
                                modify: vec![EnumVariantPatch {
                                    variant_id: "Ready".into(),
                                    action: None,
                                    name: None,
                                    description: Some("Carries the ready plan version.".into()),
                                    fields: None,
                                }],
                                ..CollectionMutation::default()
                            }),
                            extends: PatchField::Missing,
                            conforms_to: None,
                            exclusive_owner_entity_id: PatchField::Missing,
                        }],
                        ..CollectionMutation::default()
                    }),
                    ..PlanMutation::default()
                },
            },
        )
        .unwrap();
        assert_eq!(
            modified.document.entity_changes[0].variants[0].description,
            "Carries the ready plan version."
        );

        let removed = apply_plan_edit(
            &modified.document,
            PlanEditRequest {
                plan_id: "plan".into(),
                expected_version: 3,
                mutation: PlanMutation {
                    entity_changes: Some(CollectionMutation {
                        modify: vec![ProgramEntityPatch {
                            entity_id: "PlanDocument".into(),
                            action: None,
                            kind: None,
                            name: None,
                            description: None,
                            path: None,
                            members: None,
                            variants: Some(CollectionMutation {
                                remove: vec!["Ready".into()],
                                ..CollectionMutation::default()
                            }),
                            extends: PatchField::Missing,
                            conforms_to: None,
                            exclusive_owner_entity_id: PatchField::Missing,
                        }],
                        ..CollectionMutation::default()
                    }),
                    ..PlanMutation::default()
                },
            },
        )
        .unwrap();
        assert!(removed.document.entity_changes[0].variants.is_empty());
    }

    #[test]
    fn propagates_entity_renames_through_semantic_references() {
        let mut document = super::super::test_fixture("plan", "Rename one entity.");
        super::super::attach_test_fixture(&mut document);
        document.flows[0].steps[0].target = EntityReference::PlannedEntity {
            entity: "PlanDocument".into(),
        };
        document.flows[0].steps[0]
            .operations
            .push(PlanFlowOperation {
                operation_id: "validate_plan".into(),
                action: "validate()".into(),
                target: EntityReference::PlannedEntity {
                    entity: "PlanDocument".into(),
                },
                result: Some("ValidatedPlan".into()),
            });
        *document.tasks[0].files[0].subtasks[0]
            .owned_entity_ids_mut()
            .unwrap() = vec!["PlanDocument".into()];
        let PlanSubtask::Test(test) = &mut document.tasks[0].files[0].subtasks[1] else {
            panic!("fixture must append a test subtask");
        };
        test.covered_entity_ids = vec!["PlanDocument".into()];

        let result = apply_plan_edit(
            &document,
            PlanEditRequest {
                plan_id: "plan".into(),
                expected_version: 1,
                mutation: PlanMutation {
                    entity_changes: Some(CollectionMutation {
                        modify: vec![ProgramEntityPatch {
                            entity_id: "PlanDocument".into(),
                            action: None,
                            kind: None,
                            name: Some("CanonicalPlan".into()),
                            description: None,
                            path: None,
                            members: None,
                            variants: None,
                            extends: PatchField::Missing,
                            conforms_to: None,
                            exclusive_owner_entity_id: PatchField::Missing,
                        }],
                        ..CollectionMutation::default()
                    }),
                    ..PlanMutation::default()
                },
            },
        )
        .unwrap();

        assert_eq!(
            result.document.flows[0].steps[0].target,
            EntityReference::PlannedEntity {
                entity: "CanonicalPlan".into()
            }
        );
        assert_eq!(
            result.document.flows[0].steps[0].operations[0].target,
            EntityReference::PlannedEntity {
                entity: "CanonicalPlan".into()
            }
        );
        assert_eq!(
            result.document.tasks[0].files[0].subtasks[0].owned_entity_ids(),
            ["CanonicalPlan"]
        );
        assert_eq!(
            result.document.tasks[0].files[0].subtasks[1].covered_entity_ids(),
            ["CanonicalPlan"]
        );
    }

    #[test]
    fn modifies_one_flat_test_subtask_without_replacing_its_file() {
        let mut document = super::super::test_fixture("plan", "Revise one test.");
        super::super::attach_test_fixture(&mut document);

        let result = apply_plan_edit(
            &document,
            PlanEditRequest {
                plan_id: "plan".into(),
                expected_version: 1,
                mutation: PlanMutation {
                    tasks: Some(CollectionMutation {
                        modify: vec![PlanTaskPatch {
                            task_id: "Create plan state".into(),
                            title: None,
                            description: None,
                            files: Some(CollectionMutation {
                                modify: vec![PlanFilePatch {
                                    path: "src/plan.rs".into(),
                                    action: None,
                                    subtasks: Some(CollectionMutation {
                                        modify: vec![PlanSubtaskPatch::Test(
                                            PlanTestSubtaskPatch {
                                                subtask_id: "validates_plans".into(),
                                                operation: TestSubtaskOperation::Test,
                                                action: None,
                                                name: None,
                                                category: Some(TestCategory::Integration),
                                                behavior: Some(
                                                    "Reject malformed plans through the broker."
                                                        .into(),
                                                ),
                                                covered_entity_ids: None,
                                            },
                                        )],
                                        ..CollectionMutation::default()
                                    }),
                                }],
                                ..CollectionMutation::default()
                            }),
                        }],
                        ..CollectionMutation::default()
                    }),
                    ..PlanMutation::default()
                },
            },
        )
        .unwrap();

        let PlanSubtask::Test(test) = &result.document.tasks[0].files[0].subtasks[1] else {
            panic!("fixture must retain the test role");
        };
        assert_eq!(test.category, TestCategory::Integration);
        assert_eq!(test.behavior, "Reject malformed plans through the broker.");
        assert_eq!(result.document.tasks[0].files[0].subtasks.len(), 2);
    }

    #[test]
    fn adds_and_replaces_nested_flow_operations_with_stable_identity() {
        let added = apply_plan_edit(
            &empty_document(),
            PlanEditRequest {
                plan_id: "plan".into(),
                expected_version: 4,
                mutation: PlanMutation {
                    flows: Some(CollectionMutation {
                        add: vec![PlanFlow {
                            flow_id: String::new(),
                            title: "Table inspection".into(),
                            description: "Read one registered table and produce its observable schema and row count. Keep independent DataFusion observations under one inspection owner.".into(),
                            steps: vec![PlanFlowStep {
                                step_id: String::new(),
                                action: "Read table observations".into(),
                                target: EntityReference::ExternalEntity {
                                    entity: "GeoParquetInspector".into(),
                                },
                                operations: vec![PlanFlowOperation {
                                    operation_id: String::new(),
                                    action: "schema()".into(),
                                    target: EntityReference::ExternalEntity {
                                        entity: "datafusion::SessionContext".into(),
                                    },
                                    result: Some("schema text".into()),
                                }],
                                value_to_next: None,
                            }],
                        }],
                        ..CollectionMutation::default()
                    }),
                    ..PlanMutation::default()
                },
            },
        )
        .unwrap();

        let step = &added.document.flows[0].steps[0];
        assert_eq!(
            step.operations[0].operation_id,
            "flow_table_inspection_step_read_table_observations_operation_schema"
        );

        let modified = apply_plan_edit(
            &added.document,
            PlanEditRequest {
                plan_id: "plan".into(),
                expected_version: 5,
                mutation: PlanMutation {
                    flows: Some(CollectionMutation {
                        modify: vec![PlanFlowPatch {
                            flow_id: "Table inspection".into(),
                            title: None,
                            description: None,
                            steps: Some(CollectionMutation {
                                modify: vec![PlanFlowStepPatch {
                                    step_id: "Read table observations".into(),
                                    action: None,
                                    target: None,
                                    operations: Some(vec![PlanFlowOperation {
                                        operation_id: String::new(),
                                        action: "count(*)".into(),
                                        target: EntityReference::ExternalEntity {
                                            entity: "datafusion::SessionContext".into(),
                                        },
                                        result: Some("u64".into()),
                                    }]),
                                    value_to_next: PatchField::Missing,
                                }],
                                ..CollectionMutation::default()
                            }),
                        }],
                        ..CollectionMutation::default()
                    }),
                    ..PlanMutation::default()
                },
            },
        )
        .unwrap();

        let operation_list = &modified.document.flows[0].steps[0].operations;
        assert_eq!(operation_list.len(), 1);
        assert_eq!(operation_list[0].action, "count(*)");
        assert_eq!(
            operation_list[0].operation_id,
            "flow_table_inspection_step_read_table_observations_operation_count"
        );
    }

    #[test]
    fn rejects_stale_versions_before_mutating() {
        let error = apply_plan_edit(
            &empty_document(),
            PlanEditRequest {
                plan_id: "plan".into(),
                expected_version: 3,
                mutation: PlanMutation {
                    plan: Some(PlanFieldMutation {
                        modify: PlanFieldPatch {
                            overview: Some("Changed".into()),
                            ..PlanFieldPatch::default()
                        },
                    }),
                    ..PlanMutation::default()
                },
            },
        )
        .unwrap_err();
        assert!(error.to_string().contains("version conflict"));
    }

    #[test]
    fn preserves_the_version_for_a_semantic_noop() {
        let document = empty_document();
        let result = apply_plan_edit(
            &document,
            PlanEditRequest {
                plan_id: "plan".into(),
                expected_version: 4,
                mutation: PlanMutation {
                    plan: Some(PlanFieldMutation {
                        modify: PlanFieldPatch {
                            overview: Some("Initial".into()),
                            ..PlanFieldPatch::default()
                        },
                    }),
                    ..PlanMutation::default()
                },
            },
        )
        .unwrap();

        assert_eq!(result.version, 4);
        assert_eq!(result.document, document);
    }
}
