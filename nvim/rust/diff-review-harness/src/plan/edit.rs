use std::collections::{HashMap, HashSet};

use anyhow::{Context, Result};
use schemars::{JsonSchema, generate::SchemaSettings};
use serde::{Deserialize, Deserializer, Serialize};
use serde_json::{Value, json};

use super::document::*;

/// Represents an omitted, cleared, or replaced nullable plan field.
#[derive(Clone, Debug, Default, Eq, PartialEq, Serialize)]
#[serde(untagged)]
pub enum PatchField<T> {
    #[default]
    Missing,
    Null,
    Value(T),
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

/// Stores ordered complete-resource values for one atomic plan edit.
#[derive(Clone, Debug, Default, Deserialize, JsonSchema, Serialize)]
#[serde(default, deny_unknown_fields)]
pub struct PlanResourceSet {
    pub entity_changes: Option<Vec<ProgramEntityChange>>,
    pub dependencies: Option<Vec<PlanDependencyChange>>,
    pub flows: Option<Vec<PlanFlow>>,
    pub tasks: Option<Vec<PlanTask>>,
}

impl PlanResourceSet {
    fn is_empty(&self) -> bool {
        self.entity_changes.as_ref().is_none_or(Vec::is_empty)
            && self.dependencies.as_ref().is_none_or(Vec::is_empty)
            && self.flows.as_ref().is_none_or(Vec::is_empty)
            && self.tasks.as_ref().is_none_or(Vec::is_empty)
    }
}

/// Represents one explicit semantic-key rename inside a plan collection.
#[derive(Clone, Debug, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct PlanSemanticRename {
    pub from: String,
    pub to: String,
}

/// Stores semantic-key renames applied before complete-resource sets and deletes.
#[derive(Clone, Debug, Default, Deserialize, JsonSchema, Serialize)]
#[serde(default, deny_unknown_fields)]
pub struct PlanResourceRename {
    pub entity_changes: Option<Vec<PlanSemanticRename>>,
    pub dependencies: Option<Vec<PlanSemanticRename>>,
    pub flows: Option<Vec<PlanSemanticRename>>,
    pub tasks: Option<Vec<PlanSemanticRename>>,
}

impl PlanResourceRename {
    fn is_empty(&self) -> bool {
        self.entity_changes.as_ref().is_none_or(Vec::is_empty)
            && self.dependencies.as_ref().is_none_or(Vec::is_empty)
            && self.flows.as_ref().is_none_or(Vec::is_empty)
            && self.tasks.as_ref().is_none_or(Vec::is_empty)
    }
}

/// Stores current semantic keys retracted by one atomic plan edit.
#[derive(Clone, Debug, Default, Deserialize, JsonSchema, Serialize)]
#[serde(default, deny_unknown_fields)]
pub struct PlanResourceDelete {
    pub entity_changes: Option<Vec<String>>,
    pub dependencies: Option<Vec<String>>,
    pub flows: Option<Vec<String>>,
    pub tasks: Option<Vec<String>>,
}

impl PlanResourceDelete {
    fn is_empty(&self) -> bool {
        self.entity_changes.as_ref().is_none_or(Vec::is_empty)
            && self.dependencies.as_ref().is_none_or(Vec::is_empty)
            && self.flows.as_ref().is_none_or(Vec::is_empty)
            && self.tasks.as_ref().is_none_or(Vec::is_empty)
    }
}

/// Represents one semantic PlanDocument patch violation at its model-authored path.
#[derive(Debug)]
pub struct PlanMutationError {
    pub path: String,
    pub code: String,
    pub message: String,
    pub expected_shape: Value,
    pub hint: String,
}

impl std::fmt::Display for PlanMutationError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str(&self.message)
    }
}

impl std::error::Error for PlanMutationError {}

/// Modifies scalar plan fields without replacing the canonical document.
#[derive(Clone, Debug, Default, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
#[serde(default, deny_unknown_fields)]
pub struct PlanFieldPatch {
    pub title: Option<String>,
    pub overview: Option<String>,
    #[schemars(with = "Option<PlanUsage>")]
    pub usage: PatchField<PlanUsage>,
}

impl PlanFieldPatch {
    fn is_empty(&self) -> bool {
        self.title.is_none() && self.overview.is_none() && matches!(self.usage, PatchField::Missing)
    }
}

/// Owns every scalar and complete-resource change supported by one plan edit.
#[derive(Clone, Debug, Default, Deserialize, JsonSchema, Serialize)]
#[serde(default, deny_unknown_fields)]
pub struct PlanMutation {
    pub plan: Option<PlanFieldPatch>,
    pub rename: Option<PlanResourceRename>,
    pub set: Option<PlanResourceSet>,
    pub delete: Option<PlanResourceDelete>,
    pub assumptions: Option<Vec<String>>,
}

impl PlanMutation {
    /// Report whether the request carries any semantic mutation.
    pub fn is_empty(&self) -> bool {
        self.plan.as_ref().is_none_or(PlanFieldPatch::is_empty)
            && self
                .rename
                .as_ref()
                .is_none_or(PlanResourceRename::is_empty)
            && self.set.as_ref().is_none_or(PlanResourceSet::is_empty)
            && self
                .delete
                .as_ref()
                .is_none_or(PlanResourceDelete::is_empty)
            && self.assumptions.is_none()
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

#[derive(Deserialize, JsonSchema)]
#[serde(deny_unknown_fields)]
struct PlanEditRequestWire {
    plan_id: String,
    #[schemars(range(min = 1))]
    expected_version: u64,
    plan: Option<PlanFieldPatch>,
    rename: Option<PlanResourceRename>,
    set: Option<PlanResourceSet>,
    delete: Option<PlanResourceDelete>,
    assumptions: Option<Vec<String>>,
}

/// Generate the model-facing PlanEdit schema from the same types Serde decodes.
pub(crate) fn plan_edit_request_schema() -> Value {
    let generator = SchemaSettings::draft07().for_deserialize().into_generator();
    let mut schema = serde_json::to_value(generator.into_root_schema_for::<PlanEditRequestWire>())
        .expect("PlanEditRequestWire schema serializes");
    schema["anyOf"] = json!([
        { "required": ["plan"] },
        { "required": ["rename"] },
        { "required": ["set"] },
        { "required": ["delete"] },
        { "required": ["assumptions"] }
    ]);
    schema
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
                rename: wire.rename,
                set: wire.set,
                delete: wire.delete,
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

/// Apply one optimistic atomic edit to a canonical plan.
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

/// Rename one newly added entity through its unique semantic name.
pub fn rename_added_entity(
    document: &PlanDocument,
    entity_name: &str,
    new_name: String,
) -> Result<PlanEditResult> {
    let entity = document
        .entity_changes
        .iter()
        .find(|entity| entity.name == entity_name)
        .with_context(|| format!("program entity `{entity_name}` does not exist"))?;
    anyhow::ensure!(
        entity.action == EntityChangeAction::Add,
        "only newly added plan entities can be renamed"
    );
    apply_plan_edit(
        document,
        PlanEditRequest {
            plan_id: document.plan_id.clone(),
            expected_version: document.version,
            mutation: PlanMutation {
                rename: Some(PlanResourceRename {
                    entity_changes: Some(vec![PlanSemanticRename {
                        from: entity.name.clone(),
                        to: new_name,
                    }]),
                    ..PlanResourceRename::default()
                }),
                ..PlanMutation::default()
            },
        },
    )
}

/// Apply one complete-resource mutation batch to an in-memory plan.
pub fn apply_plan_mutation(document: &mut PlanDocument, mutation: PlanMutation) -> Result<()> {
    let PlanMutation {
        plan,
        rename,
        set,
        delete,
        assumptions,
    } = mutation;
    let rename = rename.unwrap_or_default();
    let set = set.unwrap_or_default();
    let delete = delete.unwrap_or_default();
    let entity_rename_list = rename.entity_changes.clone().unwrap_or_default();
    let rename_by_previous_name = entity_rename_list
        .iter()
        .map(|rename| (rename.from.clone(), rename.to.clone()))
        .collect::<HashMap<_, _>>();

    if let Some(plan) = plan {
        apply_plan_field_patch(document, plan);
    }
    if rename.entity_changes.is_some()
        || set.entity_changes.is_some()
        || delete.entity_changes.is_some()
    {
        apply_resource_patch(
            &mut document.entity_changes,
            entity_rename_list,
            set.entity_changes.unwrap_or_default(),
            delete.entity_changes.unwrap_or_default(),
            "program entity",
            "entity_changes",
            "name",
            |entity| entity.name.as_str(),
            |entity, name| entity.name = name,
            prepare_entity,
        )?;
    }
    if rename.dependencies.is_some() || set.dependencies.is_some() || delete.dependencies.is_some()
    {
        apply_resource_patch(
            &mut document.dependencies,
            rename.dependencies.unwrap_or_default(),
            set.dependencies.unwrap_or_default(),
            delete.dependencies.unwrap_or_default(),
            "dependency",
            "dependencies",
            "name",
            |dependency| dependency.name.as_str(),
            |dependency, name| dependency.name = name,
            prepare_dependency,
        )?;
    }
    if rename.flows.is_some() || set.flows.is_some() || delete.flows.is_some() {
        apply_resource_patch(
            &mut document.flows,
            rename.flows.unwrap_or_default(),
            set.flows.unwrap_or_default(),
            delete.flows.unwrap_or_default(),
            "flow",
            "flows",
            "title",
            |flow| flow.title.as_str(),
            |flow, title| flow.title = title,
            prepare_flow,
        )?;
    }
    if rename.tasks.is_some() || set.tasks.is_some() || delete.tasks.is_some() {
        apply_resource_patch(
            &mut document.tasks,
            rename.tasks.unwrap_or_default(),
            set.tasks.unwrap_or_default(),
            delete.tasks.unwrap_or_default(),
            "task",
            "tasks",
            "title",
            |task| task.title.as_str(),
            |task, title| task.title = title,
            prepare_task,
        )?;
    }
    if let Some(assumptions) = assumptions {
        document.assumptions = assumptions;
    }

    propagate_entity_rename(document, &rename_by_previous_name);
    propagate_member_rename(document);
    Ok(())
}

fn apply_resource_patch<T>(
    value_list: &mut Vec<T>,
    rename_list: Vec<PlanSemanticRename>,
    set_list: Vec<T>,
    delete_list: Vec<String>,
    resource: &str,
    collection: &str,
    semantic_key_field: &str,
    semantic_key: impl Fn(&T) -> &str,
    rename_value: impl Fn(&mut T, String),
    prepare_value: impl Fn(Option<&T>, &mut T),
) -> Result<()> {
    let current_key_set = value_list
        .iter()
        .map(|value| semantic_key(value).to_owned())
        .collect::<HashSet<_>>();

    let mut rename_index_by_source = HashMap::new();
    let mut rename_destination_set = HashSet::new();
    for (index, rename) in rename_list.iter().enumerate() {
        if rename.from == rename.to {
            return Err(plan_mutation_error(
                format!("rename.{collection}[{index}].to"),
                "noop_rename",
                format!(
                    "{resource} rename source and destination are both `{}`",
                    rename.from
                ),
                json!({ "to": "<different semantic key>" }),
                "Choose a different destination key or omit this rename.",
            ));
        }
        if rename_index_by_source
            .insert(rename.from.clone(), index)
            .is_some()
        {
            return Err(plan_mutation_error(
                format!("rename.{collection}[{index}].from"),
                "duplicate_target",
                format!(
                    "{resource} key `{}` appears more than once in `rename.{collection}`",
                    rename.from
                ),
                json!({ "unique_from": rename.from }),
                "Keep exactly one rename entry for each current semantic key.",
            ));
        }
        if !current_key_set.contains(&rename.from) {
            return Err(plan_mutation_error(
                format!("rename.{collection}[{index}].from"),
                "missing_target",
                format!("{resource} `{}` does not exist", rename.from),
                json!({ "from": "<current semantic key>" }),
                "Read the active plan and rename from its current semantic key.",
            ));
        }
        if !rename_destination_set.insert(rename.to.clone()) {
            return Err(plan_mutation_error(
                format!("rename.{collection}[{index}].to"),
                "duplicate_destination",
                format!(
                    "{resource} key `{}` appears more than once as a rename destination",
                    rename.to
                ),
                json!({ "unique_to": rename.to }),
                "Choose one unique destination for each rename.",
            ));
        }
    }

    let mut final_key_source = HashMap::new();
    for value in value_list.iter() {
        let current_key = semantic_key(value);
        let (final_key, path) =
            if let Some(index) = rename_index_by_source.get(current_key).copied() {
                (
                    rename_list[index].to.as_str(),
                    format!("rename.{collection}[{index}].to"),
                )
            } else {
                (current_key, format!("<plan>.{collection}"))
            };
        ensure_unique_final_key(&mut final_key_source, final_key, path, resource, collection)?;
    }

    for value in value_list.iter_mut() {
        if let Some(index) = rename_index_by_source.get(semantic_key(value)).copied() {
            rename_value(value, rename_list[index].to.clone());
        }
    }
    let current_key_set = value_list
        .iter()
        .map(|value| semantic_key(value).to_owned())
        .collect::<HashSet<_>>();

    let mut set_index_by_key = HashMap::new();
    for (index, value) in set_list.iter().enumerate() {
        let key = semantic_key(value);
        if set_index_by_key.insert(key.to_owned(), index).is_some() {
            return Err(plan_mutation_error(
                format!("set.{collection}[{index}].{semantic_key_field}"),
                "duplicate_target",
                format!("{resource} key `{key}` appears more than once in `set.{collection}`"),
                json!({ (format!("unique_{semantic_key_field}")): key }),
                format!(
                    "Keep exactly one complete resource for each {semantic_key_field} in `set.{collection}`."
                ),
            ));
        }
    }

    let mut delete_key_set = HashSet::new();
    for (index, key) in delete_list.iter().enumerate() {
        if !delete_key_set.insert(key.clone()) {
            return Err(plan_mutation_error(
                format!("delete.{collection}[{index}]"),
                "duplicate_target",
                format!("{resource} key `{key}` appears more than once in `delete.{collection}`"),
                json!({ "unique_key": key }),
                "Keep exactly one delete entry for each current semantic key.",
            ));
        }
        if set_index_by_key.contains_key(key) {
            return Err(plan_mutation_error(
                format!("delete.{collection}[{index}]"),
                "overlapping_target",
                format!("{resource} key `{key}` appears in both set and delete"),
                json!({ "choose_one_of": ["set", "delete"] }),
                "Choose either set or delete for one current semantic key.",
            ));
        }
        if rename_destination_set.contains(key) {
            return Err(plan_mutation_error(
                format!("delete.{collection}[{index}]"),
                "overlapping_target",
                format!("{resource} key `{key}` appears in both rename and delete"),
                json!({ "choose_one_of": ["rename", "delete"] }),
                "Choose either rename or delete for one current semantic key.",
            ));
        }
        if !current_key_set.contains(key) {
            return Err(plan_mutation_error(
                format!("delete.{collection}[{index}]"),
                "missing_target",
                format!("{resource} `{key}` does not exist"),
                json!({ "key": "<current semantic key>" }),
                "Read the active plan and delete by its current semantic key.",
            ));
        }
    }

    let insertion_key_list = set_list
        .iter()
        .filter(|value| !current_key_set.contains(semantic_key(value)))
        .map(|value| semantic_key(value).to_owned())
        .collect::<Vec<_>>();
    let mut set_value_by_key = set_list
        .into_iter()
        .map(|value| (semantic_key(&value).to_owned(), value))
        .collect::<HashMap<_, _>>();
    let mut output = Vec::with_capacity(
        value_list
            .len()
            .saturating_sub(delete_key_set.len())
            .saturating_add(insertion_key_list.len()),
    );
    for existing in std::mem::take(value_list) {
        let current_key = semantic_key(&existing).to_owned();
        if delete_key_set.contains(&current_key) {
            continue;
        }
        if let Some(mut replacement) = set_value_by_key.remove(&current_key) {
            prepare_value(Some(&existing), &mut replacement);
            output.push(replacement);
        } else {
            output.push(existing);
        }
    }
    for key in insertion_key_list {
        let mut value = set_value_by_key
            .remove(&key)
            .expect("validated insertion key must retain one complete value");
        prepare_value(None, &mut value);
        output.push(value);
    }
    *value_list = output;
    Ok(())
}

fn ensure_unique_final_key(
    final_key_source: &mut HashMap<String, String>,
    final_key: &str,
    path: String,
    resource: &str,
    collection: &str,
) -> Result<()> {
    if let Some(previous_path) = final_key_source.insert(final_key.to_owned(), path.clone()) {
        let (violation_path, conflicting_path) =
            if path.starts_with("<plan>") && !previous_path.starts_with("<plan>") {
                (previous_path, path)
            } else {
                (path, previous_path)
            };
        return Err(plan_mutation_error(
            violation_path,
            "target_conflict",
            format!(
                "{resource} key `{final_key}` would appear more than once after applying this `{collection}` patch"
            ),
            json!({ "unique_destination_key": final_key }),
            format!(
                "Choose a unique destination key. The conflicting value comes from `{conflicting_path}`."
            ),
        ));
    }
    Ok(())
}

fn plan_mutation_error(
    path: String,
    code: &str,
    message: String,
    expected_shape: Value,
    hint: impl Into<String>,
) -> anyhow::Error {
    PlanMutationError {
        path,
        code: code.to_owned(),
        message,
        expected_shape,
        hint: hint.into(),
    }
    .into()
}

fn prepare_entity(_existing: Option<&ProgramEntityChange>, _entity: &mut ProgramEntityChange) {}

fn prepare_dependency(
    existing: Option<&PlanDependencyChange>,
    dependency: &mut PlanDependencyChange,
) {
    dependency.resolved_version = None;
    let _ = existing;
}

fn prepare_flow(_existing: Option<&PlanFlow>, _flow: &mut PlanFlow) {}

fn prepare_task(_existing: Option<&PlanTask>, _task: &mut PlanTask) {}

fn propagate_entity_rename(
    document: &mut PlanDocument,
    rename_by_previous_name: &HashMap<String, String>,
) {
    if rename_by_previous_name.is_empty() {
        return;
    }
    let rename = |value: &mut String| {
        if let Some(name) = rename_by_previous_name.get(value) {
            value.clone_from(name);
        }
    };
    replace_identifier_occurrences(&mut document.title, rename_by_previous_name);
    replace_identifier_occurrences(&mut document.overview, rename_by_previous_name);
    if let Some(usage) = &mut document.usage {
        replace_identifier_occurrences(&mut usage.command, rename_by_previous_name);
        replace_identifier_occurrences(&mut usage.expected_result, rename_by_previous_name);
    }
    for assumption in &mut document.assumptions {
        replace_identifier_occurrences(assumption, rename_by_previous_name);
    }
    for dependency in &mut document.dependencies {
        replace_identifier_occurrences(&mut dependency.justification, rename_by_previous_name);
    }
    for entity in &mut document.entity_changes {
        replace_identifier_occurrences(&mut entity.description, rename_by_previous_name);
        for member in &mut entity.members {
            if let Some(description) = &mut member.description {
                replace_identifier_occurrences(description, rename_by_previous_name);
            }
            if let Some(type_name) = &mut member.type_name {
                replace_identifier_occurrences(type_name, rename_by_previous_name);
            }
            for parameter in &mut member.parameters {
                replace_identifier_occurrences(&mut parameter.type_name, rename_by_previous_name);
            }
            if let Some(return_type) = &mut member.return_type {
                replace_identifier_occurrences(return_type, rename_by_previous_name);
            }
        }
        for variant in &mut entity.variants {
            if let Some(description) = &mut variant.description {
                replace_identifier_occurrences(description, rename_by_previous_name);
            }
            for field in &mut variant.fields {
                replace_identifier_occurrences(&mut field.type_name, rename_by_previous_name);
            }
        }
        if let Some(EntityReference::PlannedEntity { entity }) = &mut entity.extends {
            rename(entity);
        }
        for reference in &mut entity.conforms_to {
            if let EntityReference::PlannedEntity { entity } = reference {
                rename(entity);
            }
        }
    }
    for flow in &mut document.flows {
        replace_identifier_occurrences(&mut flow.title, rename_by_previous_name);
        replace_identifier_occurrences(&mut flow.description, rename_by_previous_name);
        for step in &mut flow.steps {
            propagate_flow_step_rename(step, rename_by_previous_name);
        }
    }
    for task in &mut document.tasks {
        replace_identifier_occurrences(&mut task.title, rename_by_previous_name);
        replace_identifier_occurrences(&mut task.description, rename_by_previous_name);
        for file in &mut task.files {
            for subtask in &mut file.subtasks {
                if let Some(entities) = subtask.owned_entities_mut() {
                    for entity in entities {
                        rename(entity);
                    }
                }
                match subtask {
                    PlanSubtask::Work(work) => {
                        replace_identifier_occurrences(
                            &mut work.description,
                            rename_by_previous_name,
                        );
                    }
                    PlanSubtask::Test(test) => {
                        replace_identifier_occurrences(&mut test.behavior, rename_by_previous_name);
                        for entity in &mut test.covers_entities {
                            rename(entity);
                        }
                    }
                }
            }
        }
    }
}

fn propagate_member_rename(document: &mut PlanDocument) {
    let rename_by_entity = document
        .entity_changes
        .iter()
        .filter_map(|entity| {
            let rename_list = entity
                .members
                .iter()
                .filter(|member| member.action == ChangeAction::Rename)
                .filter_map(|member| {
                    member
                        .renamed_from
                        .as_ref()
                        .map(|previous| (previous.clone(), member.name.clone()))
                })
                .collect::<HashMap<_, _>>();
            (!rename_list.is_empty()).then(|| (entity.name.clone(), rename_list))
        })
        .collect::<HashMap<_, _>>();
    if rename_by_entity.is_empty() {
        return;
    }
    for flow in &mut document.flows {
        propagate_member_rename_in_steps(&mut flow.steps, &rename_by_entity);
    }
}

fn propagate_member_rename_in_steps(
    step_list: &mut [PlanFlowStep],
    rename_by_entity: &HashMap<String, HashMap<String, String>>,
) {
    for step in step_list {
        for edge in &mut step.edges {
            if let EntityReference::PlannedEntity { entity } = &edge.target
                && let Some(rename_by_name) = rename_by_entity.get(entity)
            {
                match &mut edge.relation {
                    PlanFlowRelation::Call { callable }
                    | PlanFlowRelation::Read { callable }
                    | PlanFlowRelation::Write { callable } => {
                        if let Some(replacement) = rename_by_name.get(&callable.name) {
                            callable.name.clone_from(replacement);
                        }
                    }
                    _ => {}
                }
            }
            propagate_member_rename_in_steps(&mut edge.expansion, rename_by_entity);
        }
        for branch in &mut step.branches {
            propagate_member_rename_in_steps(&mut branch.steps, rename_by_entity);
        }
    }
}

fn propagate_flow_step_rename(
    step: &mut PlanFlowStep,
    rename_by_previous_name: &HashMap<String, String>,
) {
    replace_identifier_occurrences(&mut step.action, rename_by_previous_name);
    rename_planned_reference(&mut step.target, rename_by_previous_name);
    for edge in &mut step.edges {
        match &mut edge.relation {
            PlanFlowRelation::Call { callable }
            | PlanFlowRelation::Read { callable }
            | PlanFlowRelation::Write { callable } => {
                replace_identifier_occurrences(&mut callable.name, rename_by_previous_name);
            }
            PlanFlowRelation::Send { event } => {
                replace_identifier_occurrences(event, rename_by_previous_name);
            }
            PlanFlowRelation::Construct | PlanFlowRelation::Emit | PlanFlowRelation::Return => {}
        }
        if let Some(result) = &mut edge.result {
            match result {
                PlanFlowValue::Type { name } => {
                    replace_identifier_occurrences(name, rename_by_previous_name);
                }
                PlanFlowValue::Text { text } => {
                    replace_identifier_occurrences(text, rename_by_previous_name);
                }
            }
        }
        rename_planned_reference(&mut edge.target, rename_by_previous_name);
        for nested_step in &mut edge.expansion {
            propagate_flow_step_rename(nested_step, rename_by_previous_name);
        }
    }
    for branch in &mut step.branches {
        replace_identifier_occurrences(&mut branch.condition, rename_by_previous_name);
        for nested_step in &mut branch.steps {
            propagate_flow_step_rename(nested_step, rename_by_previous_name);
        }
    }
}

fn rename_planned_reference(
    reference: &mut EntityReference,
    rename_by_previous_name: &HashMap<String, String>,
) {
    let EntityReference::PlannedEntity { entity } = reference else {
        return;
    };
    if let Some(name) = rename_by_previous_name.get(entity) {
        entity.clone_from(name);
    }
}

fn replace_identifier_occurrences(value: &mut String, rename_by_name: &HashMap<String, String>) {
    if rename_by_name.is_empty() || value.is_empty() {
        return;
    }
    let original = std::mem::take(value);
    let mut rendered = String::with_capacity(original.len());
    let mut byte_offset = 0;
    while byte_offset < original.len() {
        let replacement = rename_by_name
            .iter()
            .filter(|(name, _)| original[byte_offset..].starts_with(name.as_str()))
            .filter(|(name, _)| {
                let previous = original[..byte_offset].chars().next_back();
                let next = original[byte_offset + name.len()..].chars().next();
                previous.is_none_or(|character| !is_identifier_character(character))
                    && next.is_none_or(|character| !is_identifier_character(character))
            })
            .max_by_key(|(name, _)| name.len());
        if let Some((name, replacement)) = replacement {
            rendered.push_str(replacement);
            byte_offset += name.len();
            continue;
        }
        let character = original[byte_offset..]
            .chars()
            .next()
            .expect("byte offset must remain on a character boundary");
        rendered.push(character);
        byte_offset += character.len_utf8();
    }
    *value = rendered;
}

fn is_identifier_character(character: char) -> bool {
    character.is_alphanumeric() || character == '_'
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

#[cfg(test)]
mod test {
    use super::*;

    fn empty_document() -> PlanDocument {
        PlanDocument {
            schema_version: PLAN_SCHEMA_VERSION,
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

    fn entity(name: &str) -> ProgramEntityChange {
        ProgramEntityChange {
            action: EntityChangeAction::Add,
            kind: EntityKind::Struct,
            renamed_from: None,
            name: name.into(),
            description: format!("Owns {name}."),
            path: "src/lib.rs".into(),
            members: Vec::new(),
            variants: Vec::new(),
            extends: None,
            conforms_to: Vec::new(),
        }
    }

    #[test]
    fn applies_scalar_and_complete_list_edits_atomically() {
        let result = apply_plan_edit(
            &empty_document(),
            PlanEditRequest {
                plan_id: "plan".into(),
                expected_version: 4,
                mutation: PlanMutation {
                    plan: Some(PlanFieldPatch {
                        overview: Some("Changed".into()),
                        ..PlanFieldPatch::default()
                    }),
                    assumptions: Some(vec!["Stable".into()]),
                    ..PlanMutation::default()
                },
            },
        )
        .unwrap();

        assert_eq!(result.version, 5);
        assert_eq!(result.document.overview, "Changed");
        assert_eq!(result.document.assumptions, vec!["Stable"]);
    }

    #[test]
    fn creates_replaces_and_deletes_complete_resources() {
        let created = apply_plan_edit(
            &empty_document(),
            PlanEditRequest {
                plan_id: "plan".into(),
                expected_version: 4,
                mutation: PlanMutation {
                    set: Some(PlanResourceSet {
                        entity_changes: Some(vec![entity("Inspector"), entity("Report")]),
                        ..PlanResourceSet::default()
                    }),
                    ..PlanMutation::default()
                },
            },
        )
        .unwrap();
        let mut replacement = entity("GeoParquetInspector");
        replacement.description = "Owns typed inspection.".into();

        let replaced = apply_plan_edit(
            &created.document,
            PlanEditRequest {
                plan_id: "plan".into(),
                expected_version: 5,
                mutation: PlanMutation {
                    rename: Some(PlanResourceRename {
                        entity_changes: Some(vec![PlanSemanticRename {
                            from: "Inspector".into(),
                            to: "GeoParquetInspector".into(),
                        }]),
                        ..PlanResourceRename::default()
                    }),
                    set: Some(PlanResourceSet {
                        entity_changes: Some(vec![replacement]),
                        ..PlanResourceSet::default()
                    }),
                    delete: Some(PlanResourceDelete {
                        entity_changes: Some(vec!["Report".into()]),
                        ..PlanResourceDelete::default()
                    }),
                    ..PlanMutation::default()
                },
            },
        )
        .unwrap();

        assert_eq!(replaced.document.entity_changes.len(), 1);
        assert_eq!(
            replaced.document.entity_changes[0].name,
            "GeoParquetInspector"
        );
        assert_eq!(
            replaced.document.entity_changes[0].name,
            "GeoParquetInspector"
        );
    }

    #[test]
    fn preserves_existing_positions_and_appends_insertions_in_request_order() {
        let mut document = empty_document();
        document.entity_changes = vec![entity("First"), entity("Second")];
        let mut replacement = entity("SecondRenamed");
        replacement.description = "Owns the renamed second value.".into();

        let result = apply_plan_edit(
            &document,
            PlanEditRequest {
                plan_id: "plan".into(),
                expected_version: 4,
                mutation: PlanMutation {
                    rename: Some(PlanResourceRename {
                        entity_changes: Some(vec![PlanSemanticRename {
                            from: "Second".into(),
                            to: "SecondRenamed".into(),
                        }]),
                        ..PlanResourceRename::default()
                    }),
                    set: Some(PlanResourceSet {
                        entity_changes: Some(vec![replacement, entity("Fourth"), entity("Third")]),
                        ..PlanResourceSet::default()
                    }),
                    ..PlanMutation::default()
                },
            },
        )
        .unwrap();

        let name_list = result
            .document
            .entity_changes
            .iter()
            .map(|entity| entity.name.as_str())
            .collect::<Vec<_>>();
        assert_eq!(name_list, ["First", "SecondRenamed", "Fourth", "Third"]);
        assert_eq!(result.document.entity_changes[1].name, "SecondRenamed");
    }

    #[test]
    fn rejects_overlapping_targets_without_mutating_the_document() {
        let mut document = empty_document();
        document.entity_changes.push(entity("Inspector"));
        let original = document.clone();

        let error = apply_plan_edit(
            &document,
            PlanEditRequest {
                plan_id: "plan".into(),
                expected_version: 4,
                mutation: PlanMutation {
                    set: Some(PlanResourceSet {
                        entity_changes: Some(vec![entity("Inspector")]),
                        ..PlanResourceSet::default()
                    }),
                    delete: Some(PlanResourceDelete {
                        entity_changes: Some(vec!["Inspector".into()]),
                        ..PlanResourceDelete::default()
                    }),
                    ..PlanMutation::default()
                },
            },
        )
        .unwrap_err();

        let mutation_error = error.downcast_ref::<PlanMutationError>().unwrap();
        assert_eq!(mutation_error.path, "delete.entity_changes[0]");
        assert_eq!(mutation_error.code, "overlapping_target");
        assert_eq!(document, original);
    }

    #[test]
    fn rejects_duplicate_set_keys_and_missing_delete_keys() {
        let duplicate_error = apply_plan_edit(
            &empty_document(),
            PlanEditRequest {
                plan_id: "plan".into(),
                expected_version: 4,
                mutation: PlanMutation {
                    set: Some(PlanResourceSet {
                        entity_changes: Some(vec![entity("Inspector"), entity("Inspector")]),
                        ..PlanResourceSet::default()
                    }),
                    ..PlanMutation::default()
                },
            },
        )
        .unwrap_err();
        let duplicate_error = duplicate_error.downcast_ref::<PlanMutationError>().unwrap();
        assert_eq!(duplicate_error.path, "set.entity_changes[1].name");
        assert_eq!(duplicate_error.code, "duplicate_target");

        let missing_error = apply_plan_edit(
            &empty_document(),
            PlanEditRequest {
                plan_id: "plan".into(),
                expected_version: 4,
                mutation: PlanMutation {
                    delete: Some(PlanResourceDelete {
                        flows: Some(vec!["Missing flow".into()]),
                        ..PlanResourceDelete::default()
                    }),
                    ..PlanMutation::default()
                },
            },
        )
        .unwrap_err();
        let missing_error = missing_error.downcast_ref::<PlanMutationError>().unwrap();
        assert_eq!(missing_error.path, "delete.flows[0]");
        assert_eq!(missing_error.code, "missing_target");
    }

    #[test]
    fn rejects_destination_collisions_at_the_authored_set_path() {
        let mut document = empty_document();
        document.entity_changes = vec![entity("Inspector"), entity("Report")];

        let error = apply_plan_edit(
            &document,
            PlanEditRequest {
                plan_id: "plan".into(),
                expected_version: 4,
                mutation: PlanMutation {
                    rename: Some(PlanResourceRename {
                        entity_changes: Some(vec![PlanSemanticRename {
                            from: "Inspector".into(),
                            to: "Report".into(),
                        }]),
                        ..PlanResourceRename::default()
                    }),
                    ..PlanMutation::default()
                },
            },
        )
        .unwrap_err();

        let mutation_error = error.downcast_ref::<PlanMutationError>().unwrap();
        assert_eq!(mutation_error.path, "rename.entity_changes[0].to");
        assert_eq!(mutation_error.code, "target_conflict");
    }

    #[test]
    fn rejects_a_rename_with_a_missing_source() {
        let error = apply_plan_edit(
            &empty_document(),
            PlanEditRequest {
                plan_id: "plan".into(),
                expected_version: 4,
                mutation: PlanMutation {
                    rename: Some(PlanResourceRename {
                        entity_changes: Some(vec![PlanSemanticRename {
                            from: "OldName".into(),
                            to: "NewName".into(),
                        }]),
                        ..PlanResourceRename::default()
                    }),
                    ..PlanMutation::default()
                },
            },
        )
        .unwrap_err();

        let mutation_error = error.downcast_ref::<PlanMutationError>().unwrap();
        assert_eq!(mutation_error.path, "rename.entity_changes[0].from");
        assert_eq!(mutation_error.code, "missing_target");
        assert!(mutation_error.hint.contains("current semantic key"));
    }

    #[test]
    fn complete_noop_replacement_preserves_the_version() {
        let mut document = empty_document();
        document.entity_changes.push(entity("Inspector"));
        let replacement = document.entity_changes[0].clone();

        let result = apply_plan_edit(
            &document,
            PlanEditRequest {
                plan_id: "plan".into(),
                expected_version: 4,
                mutation: PlanMutation {
                    set: Some(PlanResourceSet {
                        entity_changes: Some(vec![replacement]),
                        ..PlanResourceSet::default()
                    }),
                    ..PlanMutation::default()
                },
            },
        )
        .unwrap();

        assert_eq!(result.version, 4);
        assert_eq!(result.document, document);
    }

    #[test]
    fn member_renames_rewrite_matching_flow_calls_recursively() {
        let mut document = empty_document();
        let mut inspector = entity("Inspector");
        inspector.members.push(ProgramEntityMemberChange {
            action: ChangeAction::Add,
            renamed_from: None,
            kind: MemberKind::Method,
            name: "inspect".into(),
            description: Some("Inspect one input.".into()),
            visibility: Some(Visibility::Public),
            type_name: None,
            parameters: Vec::new(),
            return_type: Some("Report".into()),
        });
        document.entity_changes.push(inspector.clone());

        let call_step = |action: &str| PlanFlowStep {
            action: action.into(),
            target: EntityReference::PlannedEntity {
                entity: "Inspector".into(),
            },
            edges: vec![PlanFlowEdge {
                relation: PlanFlowRelation::Call {
                    callable: PlanCallable {
                        kind: PlanCallableKind::Method,
                        name: "inspect".into(),
                    },
                },
                target: EntityReference::PlannedEntity {
                    entity: "Inspector".into(),
                },
                expansion: Vec::new(),
                result: Some(PlanFlowValue::Type {
                    name: "Report".into(),
                }),
            }],
            branches: Vec::new(),
        };
        let mut root = call_step("Inspect root");
        root.edges[0].expansion.push(call_step("Inspect expansion"));
        root.branches.push(PlanFlowBranch {
            condition: "retry requested".into(),
            steps: vec![call_step("Inspect branch")],
        });
        document.flows.push(PlanFlow {
            title: "Inspection".into(),
            description: "Inspect one input.".into(),
            steps: vec![root],
        });

        inspector.members[0].action = ChangeAction::Rename;
        inspector.members[0].renamed_from = Some("inspect".into());
        inspector.members[0].name = "inspect_file".into();
        apply_plan_mutation(
            &mut document,
            PlanMutation {
                set: Some(PlanResourceSet {
                    entity_changes: Some(vec![inspector]),
                    ..PlanResourceSet::default()
                }),
                ..PlanMutation::default()
            },
        )
        .unwrap();

        assert_eq!(document.entity_changes[0].members[0].name, "inspect_file");
        let root = &document.flows[0].steps[0];
        let callable_name = |step: &PlanFlowStep| match &step.edges[0].relation {
            PlanFlowRelation::Call { callable } => callable.name.clone(),
            _ => unreachable!(),
        };
        assert_eq!(callable_name(root), "inspect_file");
        assert_eq!(callable_name(&root.edges[0].expansion[0]), "inspect_file");
        assert_eq!(callable_name(&root.branches[0].steps[0]), "inspect_file");
    }

    #[test]
    fn rejects_stale_versions_before_mutating() {
        let error = apply_plan_edit(
            &empty_document(),
            PlanEditRequest {
                plan_id: "plan".into(),
                expected_version: 3,
                mutation: PlanMutation {
                    assumptions: Some(Vec::new()),
                    ..PlanMutation::default()
                },
            },
        )
        .unwrap_err();

        assert!(error.to_string().contains("expected 3, current 4"));
    }

    #[test]
    fn decodes_ordered_complete_set_resources_without_generated_node_ids() {
        let request: PlanEditRequest = serde_json::from_value(serde_json::json!({
            "plan_id": "plan",
            "expected_version": 4,
            "set": {
                "entity_changes": [{
                    "action": "add",
                    "kind": "struct",
                    "name": "Inspector",
                    "description": "Owns inspection.",
                    "path": "src/lib.rs",
                    "members": [],
                    "variants": [],
                    "extends": null,
                    "conforms_to": []
                }]
            }
        }))
        .unwrap();

        let entry_list = request.mutation.set.unwrap().entity_changes.unwrap();
        assert_eq!(entry_list[0].name, "Inspector");
    }

    #[test]
    fn rejects_legacy_resource_operations() {
        let error = serde_json::from_value::<PlanEditRequest>(serde_json::json!({
            "plan_id": "plan",
            "expected_version": 4,
            "entity_changes": [{
                "operation": "create",
                "value": {
                    "action": "add",
                    "kind": "struct",
                    "name": "Inspector",
                    "description": "Owns inspection.",
                    "path": "src/lib.rs"
                }
            }]
        }))
        .unwrap_err();

        assert!(error.to_string().contains("unknown field `entity_changes`"));
    }
}
