use std::collections::{HashMap, HashSet};
use std::fmt;
use std::path::{Component, Path};

use anyhow::{Result, anyhow};

use super::{
    ChangeAction, EntityKind, EntityReference, ExternalEntityKind, PROVISIONAL_PLAN_TITLE,
    PlanCallable, PlanDocument, PlanFlowRelation, PlanFlowValue, PlanGraph, PlanSubtask,
    ProgramEntityChange,
};

/// Defines the validation boundary that rejected one canonical plan.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum PlanValidationPhase {
    Edit,
    Submission,
    Render,
}

/// Represents one actionable violation in a canonical plan.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PlanViolation {
    pub path: String,
    pub message: String,
}

/// Owns every violation found during one validation pass.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PlanValidationError {
    pub phase: PlanValidationPhase,
    pub violation: Vec<PlanViolation>,
}

impl fmt::Display for PlanValidationError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(
            formatter,
            "plan {:?} validation found {} violation(s)",
            self.phase,
            self.violation.len()
        )?;
        for violation in &self.violation {
            writeln!(formatter, "- {}: {}", violation.path, violation.message)?;
        }
        Ok(())
    }
}

impl std::error::Error for PlanValidationError {}

/// Validate invariants required after every typed edit.
pub fn validate_plan_edit(document: &PlanDocument) -> Result<()> {
    let mut validator = PlanValidator::new(document, PlanValidationPhase::Edit);
    validator.validate_base();
    validator.finish()
}

/// Validate the complete reviewer-facing and executable plan contract.
pub fn validate_plan_submission(document: &PlanDocument) -> Result<()> {
    let mut validator = PlanValidator::new(document, PlanValidationPhase::Submission);
    validator.validate_base();
    validator.validate_submission();
    validator.finish()
}

/// Validate invariants required to produce deterministic Markdown and navigation.
pub fn validate_plan_render(document: &PlanDocument) -> Result<()> {
    let mut validator = PlanValidator::new(document, PlanValidationPhase::Render);
    validator.validate_base();
    validator.validate_render();
    validator.finish()
}

struct PlanValidator<'a> {
    document: &'a PlanDocument,
    graph: PlanGraph<'a>,
    phase: PlanValidationPhase,
    violation: Vec<PlanViolation>,
}

impl<'a> PlanValidator<'a> {
    fn new(document: &'a PlanDocument, phase: PlanValidationPhase) -> Self {
        Self {
            document,
            graph: PlanGraph::new(document),
            phase,
            violation: Vec::new(),
        }
    }

    fn validate_base(&mut self) {
        self.required("plan_id", &self.document.plan_id);
        self.required("title", &self.document.title);
        self.required("overview", &self.document.overview);
        self.validate_global_ids();
        self.validate_entities();
        self.validate_dependencies();
        self.validate_flows();
        self.validate_tasks();
        self.validate_assumptions();
    }

    fn validate_entities(&mut self) {
        self.unique_id(
            "entity_changes",
            self.document
                .entity_changes
                .iter()
                .map(|entity| entity.entity_id.as_str()),
        );
        self.unique_id(
            "entity_changes.names",
            self.document
                .entity_changes
                .iter()
                .map(|entity| entity.name.as_str()),
        );
        for entity in &self.document.entity_changes {
            let path = format!("entity_changes.{}", entity.entity_id);
            self.semantic_id(&format!("{path}.entity_id"), &entity.entity_id);
            self.required(&format!("{path}.name"), &entity.name);
            self.required(&format!("{path}.description"), &entity.description);
            self.repository_path(&format!("{path}.path"), &entity.path);
            self.unique_id(
                &format!("{path}.members"),
                entity
                    .members
                    .iter()
                    .map(|member| member.member_id.as_str()),
            );
            self.unique_id(
                &format!("{path}.member_names"),
                entity.members.iter().map(|member| member.name.as_str()),
            );
            self.unique_id(
                &format!("{path}.variants"),
                entity
                    .variants
                    .iter()
                    .map(|variant| variant.variant_id.as_str()),
            );
            self.unique_id(
                &format!("{path}.variant_names"),
                entity.variants.iter().map(|variant| variant.name.as_str()),
            );
            if entity.kind != EntityKind::Enum && !entity.variants.is_empty() {
                self.push(
                    &format!("{path}.variants"),
                    "only enum entities can declare variants",
                );
            }
            self.validate_entity_action_tree(entity, &path);
            for member in &entity.members {
                let member_path = format!("{path}.members.{}", member.member_id);
                self.semantic_id(&format!("{member_path}.member_id"), &member.member_id);
                self.required(&format!("{member_path}.name"), &member.name);
                self.required(&format!("{member_path}.description"), &member.description);
            }
            for variant in &entity.variants {
                let variant_path = format!("{path}.variants.{}", variant.variant_id);
                self.semantic_id(&format!("{variant_path}.variant_id"), &variant.variant_id);
                self.required(&format!("{variant_path}.name"), &variant.name);
                self.required(&format!("{variant_path}.description"), &variant.description);
                self.unique_id(
                    &format!("{variant_path}.fields"),
                    variant.fields.iter().map(|field| field.field_id.as_str()),
                );
                for field in &variant.fields {
                    let field_path = format!("{variant_path}.fields.{}", field.field_id);
                    self.semantic_id(&format!("{field_path}.field_id"), &field.field_id);
                    self.required(&format!("{field_path}.name"), &field.name);
                    self.required(&format!("{field_path}.type"), &field.type_name);
                }
            }
            if let Some(reference) = &entity.extends {
                self.validate_type_reference(&format!("{path}.extends"), reference);
            }
            for (index, reference) in entity.conforms_to.iter().enumerate() {
                self.validate_type_reference(&format!("{path}.conforms_to[{index}]"), reference);
            }
        }
    }

    fn validate_dependencies(&mut self) {
        self.unique_id(
            "dependencies",
            self.document
                .dependencies
                .iter()
                .map(|dependency| dependency.dependency_id.as_str()),
        );
        let mut declaration_set = HashSet::new();
        for dependency in &self.document.dependencies {
            let path = format!("dependencies.{}", dependency.dependency_id);
            self.semantic_id(&format!("{path}.dependency_id"), &dependency.dependency_id);
            self.required(&format!("{path}.name"), &dependency.name);
            self.required(&format!("{path}.version"), &dependency.version);
            self.repository_path(&format!("{path}.manifest"), &dependency.manifest);
            self.required(&format!("{path}.justification"), &dependency.justification);
            if let Some(license) = &dependency.license {
                self.required(&format!("{path}.license"), license);
                if spdx::Expression::parse(license).is_err() {
                    self.push(
                        &format!("{path}.license"),
                        "must be a valid SPDX license expression or null",
                    );
                }
            }
            if !declaration_set.insert((dependency.manifest.as_str(), dependency.name.as_str())) {
                self.push(
                    &path,
                    "duplicates a dependency name within the same manifest",
                );
            }
        }
    }

    fn validate_entity_action_tree(&mut self, entity: &ProgramEntityChange, path: &str) {
        for member in &entity.members {
            if entity.action == ChangeAction::Add && member.action != ChangeAction::Add {
                self.push(
                    &format!("{path}.members.{}.action", member.member_id),
                    "an added entity can contain only added members",
                );
            }
            if entity.action == ChangeAction::Remove && member.action != ChangeAction::Remove {
                self.push(
                    &format!("{path}.members.{}.action", member.member_id),
                    "a removed entity can contain only removed members",
                );
            }
        }
        for variant in &entity.variants {
            if entity.action == ChangeAction::Add && variant.action != ChangeAction::Add {
                self.push(
                    &format!("{path}.variants.{}.action", variant.variant_id),
                    "an added enum can contain only added variants",
                );
            }
            if entity.action == ChangeAction::Remove && variant.action != ChangeAction::Remove {
                self.push(
                    &format!("{path}.variants.{}.action", variant.variant_id),
                    "a removed enum can contain only removed variants",
                );
            }
            for field in &variant.fields {
                if variant.action == ChangeAction::Add && field.action != ChangeAction::Add {
                    self.push(
                        &format!(
                            "{path}.variants.{}.fields.{}.action",
                            variant.variant_id, field.field_id
                        ),
                        "an added variant can contain only added fields",
                    );
                }
                if variant.action == ChangeAction::Remove && field.action != ChangeAction::Remove {
                    self.push(
                        &format!(
                            "{path}.variants.{}.fields.{}.action",
                            variant.variant_id, field.field_id
                        ),
                        "a removed variant can contain only removed fields",
                    );
                }
            }
        }
    }

    fn validate_flows(&mut self) {
        self.unique_id(
            "flows",
            self.document.flows.iter().map(|flow| flow.flow_id.as_str()),
        );
        for flow in &self.document.flows {
            let path = format!("flows.{}", flow.flow_id);
            self.semantic_id(&format!("{path}.flow_id"), &flow.flow_id);
            self.required(&format!("{path}.title"), &flow.title);
            self.required(&format!("{path}.description"), &flow.description);
            if sentence_count(&flow.description) != 2 {
                self.push(
                    &format!("{path}.description"),
                    "must contain exactly two sentences",
                );
            }
            self.unique_id(
                &format!("{path}.steps"),
                flow.steps.iter().map(|step| step.step_id.as_str()),
            );
            for step in &flow.steps {
                let step_path = format!("{path}.steps.{}", step.step_id);
                self.semantic_id(&format!("{step_path}.step_id"), &step.step_id);
                self.required(&format!("{step_path}.action"), &step.action);
                self.validate_reference(&format!("{step_path}.target"), &step.target);
                self.unique_id(
                    &format!("{step_path}.edges"),
                    step.edges.iter().map(|edge| edge.edge_id.as_str()),
                );
                for edge in &step.edges {
                    let edge_path = format!("{step_path}.edges.{}", edge.edge_id);
                    self.semantic_id(&format!("{edge_path}.edge_id"), &edge.edge_id);
                    self.validate_reference(&format!("{edge_path}.target"), &edge.target);
                    match &edge.relation {
                        PlanFlowRelation::Call { callable }
                        | PlanFlowRelation::Read { callable }
                        | PlanFlowRelation::Write { callable } => {
                            self.callable(&format!("{edge_path}.relation.callable"), callable);
                            self.validate_type_reference(
                                &format!("{edge_path}.target"),
                                &edge.target,
                            );
                        }
                        PlanFlowRelation::Send { event } => {
                            self.required(&format!("{edge_path}.relation.event"), event);
                        }
                        PlanFlowRelation::Construct => self
                            .validate_type_reference(&format!("{edge_path}.target"), &edge.target),
                        PlanFlowRelation::Emit | PlanFlowRelation::Return => {}
                    }
                    if let Some(result) = &edge.result {
                        match result {
                            PlanFlowValue::Type { name } => {
                                self.required(&format!("{edge_path}.result.name"), name);
                            }
                            PlanFlowValue::Text { text } => {
                                self.required(&format!("{edge_path}.result.text"), text);
                            }
                        }
                    }
                }
            }
        }
    }

    fn validate_tasks(&mut self) {
        self.unique_id(
            "tasks",
            self.document.tasks.iter().map(|task| task.task_id.as_str()),
        );
        let mut attached_entity = HashMap::<&str, String>::new();
        for task in &self.document.tasks {
            let path = format!("tasks.{}", task.task_id);
            self.semantic_id(&format!("{path}.task_id"), &task.task_id);
            self.required(&format!("{path}.title"), &task.title);
            self.required(&format!("{path}.description"), &task.description);
            let mut file_path_set = HashSet::new();
            for file in &task.files {
                let path_value = file.change.path();
                let file_path = format!("{path}.files.{path_value}");
                self.repository_path(&format!("{file_path}.path"), path_value);
                if let Some(source_path) = file.change.source_path() {
                    self.repository_path(&format!("{file_path}.from"), source_path);
                    if source_path == path_value {
                        self.push(&file_path, "rename source and destination must differ");
                    }
                }
                if !file_path_set.insert(path_value) {
                    self.push(&file_path, "duplicates a task file path");
                }
                self.unique_id(
                    &format!("{file_path}.subtasks"),
                    file.subtasks.iter().map(PlanSubtask::subtask_id),
                );
                for subtask in &file.subtasks {
                    let subtask_path = format!("{file_path}.subtasks.{}", subtask.subtask_id());
                    self.semantic_id(&format!("{subtask_path}.subtask_id"), subtask.subtask_id());
                    match subtask {
                        PlanSubtask::Work(subtask) => {
                            self.required(
                                &format!("{subtask_path}.description"),
                                &subtask.description,
                            );
                            let mut local_entity = HashSet::new();
                            for entity_id in &subtask.entity_ids {
                                if !local_entity.insert(entity_id.as_str()) {
                                    self.push(
                                        &format!("{subtask_path}.entities"),
                                        &format!("duplicates entity {entity_id}"),
                                    );
                                }
                                let Some(entity) = self.graph.entity(entity_id) else {
                                    self.push(
                                        &format!("{subtask_path}.entities"),
                                        &format!("references missing entity {entity_id}"),
                                    );
                                    continue;
                                };
                                if let Some(previous) = attached_entity
                                    .insert(entity.entity_id.as_str(), subtask_path.clone())
                                {
                                    self.push(
                                        &format!("{subtask_path}.entities"),
                                        &format!(
                                            "entity {entity_id} already belongs to {previous}"
                                        ),
                                    );
                                }
                                if entity.path != path_value {
                                    self.push(
                                        &format!("{subtask_path}.entities"),
                                        &format!(
                                            "entity {entity_id} belongs to {} rather than {}",
                                            entity.path, path_value
                                        ),
                                    );
                                }
                                if file.change.entity_action() == ChangeAction::Add
                                    && entity.action != ChangeAction::Add
                                {
                                    self.push(
                                        &format!("{file_path}.action"),
                                        "an added file can contain only added entities",
                                    );
                                }
                                if file.change.entity_action() == ChangeAction::Remove
                                    && entity.action != ChangeAction::Remove
                                {
                                    self.push(
                                        &format!("{file_path}.action"),
                                        "a removed file can contain only removed entities",
                                    );
                                }
                            }
                        }
                        PlanSubtask::Test(test) => {
                            self.required(&format!("{subtask_path}.name"), &test.name);
                            self.required(&format!("{subtask_path}.behavior"), &test.behavior);
                            let mut covered_entity = HashSet::new();
                            for entity_id in &test.covered_entity_ids {
                                if !covered_entity.insert(entity_id.as_str()) {
                                    self.push(
                                        &format!("{subtask_path}.covers_entities"),
                                        &format!("duplicates entity {entity_id}"),
                                    );
                                }
                                if self.graph.entity(entity_id).is_none() {
                                    self.push(
                                        &format!("{subtask_path}.covers_entities"),
                                        &format!("references missing entity {entity_id}"),
                                    );
                                }
                            }
                            if file.change.entity_action() == ChangeAction::Add
                                && test.action != ChangeAction::Add
                            {
                                self.push(
                                    &format!("{file_path}.action"),
                                    "an added file can contain only added tests",
                                );
                            }
                            if file.change.entity_action() == ChangeAction::Remove
                                && test.action != ChangeAction::Remove
                            {
                                self.push(
                                    &format!("{file_path}.action"),
                                    "a removed file can contain only removed tests",
                                );
                            }
                        }
                    }
                }
            }
        }
    }

    fn validate_assumptions(&mut self) {
        let mut assumption_set = HashSet::new();
        for (index, assumption) in self.document.assumptions.iter().enumerate() {
            let path = format!("assumptions[{index}]");
            self.required(&path, assumption);
            if !assumption_set.insert(assumption.as_str()) {
                self.push(&path, "duplicates an assumption");
            }
        }
    }

    fn validate_submission(&mut self) {
        if self.document.title == PROVISIONAL_PLAN_TITLE {
            self.push(
                "title",
                "must replace the provisional title before submission",
            );
        }
        if self.document.tasks.is_empty() {
            self.push("tasks", "requires at least one task");
        }
        for flow in &self.document.flows {
            if flow.steps.is_empty() {
                self.push(
                    &format!("flows.{}.steps", flow.flow_id),
                    "requires at least one acting step",
                );
            }
            for step in &flow.steps {
                if step.edges.is_empty() {
                    self.push(
                        &format!("flows.{}.steps.{}.edges", flow.flow_id, step.step_id),
                        "requires at least one typed runtime edge",
                    );
                }
            }
        }
        let attached_entity_id = self
            .document
            .tasks
            .iter()
            .flat_map(|task| &task.files)
            .flat_map(|file| &file.subtasks)
            .flat_map(PlanSubtask::owned_entity_ids)
            .filter_map(|entity| self.graph.entity(entity))
            .map(|entity| entity.entity_id.as_str())
            .collect::<HashSet<_>>();
        for dependency in &self.document.dependencies {
            let owner_count = self
                .document
                .tasks
                .iter()
                .flat_map(|task| &task.files)
                .filter(|file| file.change.path() == dependency.manifest)
                .count();
            if owner_count != 1 {
                self.push(
                    &format!("dependencies.{}.manifest", dependency.dependency_id),
                    "must match exactly one task file",
                );
            }
        }
        for entity in &self.document.entity_changes {
            if !attached_entity_id.contains(entity.entity_id.as_str()) {
                self.push(
                    &format!("entity_changes.{}", entity.entity_id),
                    "must belong to exactly one subtask before submission",
                );
            }
        }
        for task in &self.document.tasks {
            if task.files.is_empty() {
                self.push(
                    &format!("tasks.{}.files", task.task_id),
                    "requires at least one file",
                );
            }
            for file in &task.files {
                let file_path = file.change.path();
                if file.subtasks.is_empty() {
                    self.push(
                        &format!("tasks.{}.files.{file_path}.subtasks", task.task_id),
                        "requires at least one subtask",
                    );
                }
                for subtask in &file.subtasks {
                    if let PlanSubtask::Work(work) = subtask
                        && description_starts_with_action(work.action.label(), &work.description)
                    {
                        self.push(
                            &format!(
                                "tasks.{}.files.{}.subtasks.{}.description",
                                task.task_id, file_path, work.subtask_id
                            ),
                            &format!(
                                "must complement operation `{}` without repeating `{}` as its first word",
                                work.action.label().to_ascii_lowercase(),
                                work.action.label()
                            ),
                        );
                    }
                    let owns_dependency = self
                        .document
                        .dependencies
                        .iter()
                        .any(|dependency| dependency.manifest == file_path);
                    if matches!(subtask, PlanSubtask::Work(work) if work.entity_ids.is_empty())
                        && !owns_dependency
                    {
                        self.push(
                            &format!(
                                "tasks.{}.files.{}.subtasks.{}.entities",
                                task.task_id,
                                file_path,
                                subtask.subtask_id()
                            ),
                            "requires at least one entity",
                        );
                    }
                }
            }
        }
        for flow in &self.document.flows {
            if flow.steps.is_empty() {
                self.push(
                    &format!("flows.{}.steps", flow.flow_id),
                    "requires at least one flow step",
                );
            }
        }
    }

    fn validate_render(&mut self) {
        for entity in &self.document.entity_changes {
            if entity.name.trim().is_empty() {
                self.push(
                    &format!("entity_changes.{}", entity.entity_id),
                    "cannot render an unnamed entity",
                );
            }
        }
    }

    fn validate_reference(&mut self, path: &str, reference: &EntityReference) {
        match reference {
            EntityReference::PlannedEntity { entity } => {
                if self.graph.entity(entity).is_none() {
                    self.push(
                        path,
                        &format!("references unknown planned entity `{entity}`"),
                    );
                }
            }
            EntityReference::ExternalEntity {
                name, dependency, ..
            } => {
                self.required(&format!("{path}.name"), name);
                if let Some(dependency) = dependency {
                    self.required(&format!("{path}.dependency"), dependency);
                }
            }
        }
    }

    fn validate_type_reference(&mut self, path: &str, reference: &EntityReference) {
        self.validate_reference(path, reference);
        let is_type = match reference {
            EntityReference::PlannedEntity { entity } => self
                .graph
                .entity(entity)
                .is_some_and(|entity| entity_kind_is_type(entity.kind)),
            EntityReference::ExternalEntity { entity_kind, .. } => {
                *entity_kind == ExternalEntityKind::Type
            }
        };
        if !is_type {
            self.push(path, "must reference one type entity");
        }
    }

    fn validate_global_ids(&mut self) {
        let mut entry_list = Vec::new();
        for dependency in &self.document.dependencies {
            entry_list.push((
                dependency.dependency_id.as_str(),
                format!("dependencies.{}", dependency.dependency_id),
            ));
        }
        for entity in &self.document.entity_changes {
            entry_list.push((
                entity.entity_id.as_str(),
                format!("entity_changes.{}", entity.entity_id),
            ));
            for member in &entity.members {
                entry_list.push((
                    member.member_id.as_str(),
                    format!(
                        "entity_changes.{}.members.{}",
                        entity.entity_id, member.member_id
                    ),
                ));
            }
            for variant in &entity.variants {
                entry_list.push((
                    variant.variant_id.as_str(),
                    format!(
                        "entity_changes.{}.variants.{}",
                        entity.entity_id, variant.variant_id
                    ),
                ));
                for field in &variant.fields {
                    entry_list.push((
                        field.field_id.as_str(),
                        format!(
                            "entity_changes.{}.variants.{}.fields.{}",
                            entity.entity_id, variant.variant_id, field.field_id
                        ),
                    ));
                }
            }
        }
        for flow in &self.document.flows {
            entry_list.push((flow.flow_id.as_str(), format!("flows.{}", flow.flow_id)));
            for step in &flow.steps {
                entry_list.push((
                    step.step_id.as_str(),
                    format!("flows.{}.steps.{}", flow.flow_id, step.step_id),
                ));
                for edge in &step.edges {
                    entry_list.push((
                        edge.edge_id.as_str(),
                        format!(
                            "flows.{}.steps.{}.edges.{}",
                            flow.flow_id, step.step_id, edge.edge_id
                        ),
                    ));
                }
            }
        }
        for task in &self.document.tasks {
            entry_list.push((task.task_id.as_str(), format!("tasks.{}", task.task_id)));
            for file in &task.files {
                for subtask in &file.subtasks {
                    entry_list.push((
                        subtask.subtask_id(),
                        format!(
                            "tasks.{}.files.{}.subtasks.{}",
                            task.task_id,
                            file.change.path(),
                            subtask.subtask_id()
                        ),
                    ));
                }
            }
        }
        let mut owner_by_id = HashMap::new();
        for (id, path) in entry_list {
            if let Some(previous_path) = owner_by_id.insert(id, path.clone()) {
                self.push(
                    &path,
                    &format!("duplicates globally unique ID {id} already owned by {previous_path}"),
                );
            }
        }
    }

    fn semantic_id(&mut self, path: &str, value: &str) {
        if value.is_empty()
            || value.starts_with('_')
            || value.ends_with('_')
            || value.contains("__")
            || !value
                .bytes()
                .all(|byte| byte.is_ascii_lowercase() || byte.is_ascii_digit() || byte == b'_')
        {
            self.push(path, "must use stable snake_case");
        }
    }

    fn required(&mut self, path: &str, value: &str) {
        if value.trim().is_empty() {
            self.push(path, "cannot be empty");
        }
    }

    fn callable(&mut self, path: &str, callable: &PlanCallable) {
        self.required(&format!("{path}.name"), &callable.name);
        if !callable.name.bytes().enumerate().all(|(index, byte)| {
            byte == b'_' || byte.is_ascii_alphanumeric() && (index > 0 || !byte.is_ascii_digit())
        }) {
            self.push(
                &format!("{path}.name"),
                "must be one function or method identifier without parentheses",
            );
        }
    }

    fn repository_path(&mut self, field_path: &str, value: &str) {
        self.required(field_path, value);
        let path = Path::new(value);
        if path.is_absolute() {
            self.push(field_path, "must be repository-relative");
        }
        if !path
            .components()
            .all(|component| matches!(component, Component::Normal(_)))
        {
            self.push(field_path, "cannot escape the workspace");
        }
    }

    fn unique_id<'b>(&mut self, path: &str, values: impl Iterator<Item = &'b str>) {
        let mut value_set = HashSet::new();
        for value in values {
            if value.trim().is_empty() {
                self.push(path, "contains an empty identifier");
            } else if !value_set.insert(value) {
                self.push(path, &format!("duplicates identifier {value}"));
            }
        }
    }

    fn push(&mut self, path: &str, message: &str) {
        self.violation.push(PlanViolation {
            path: path.to_owned(),
            message: message.to_owned(),
        });
    }

    fn finish(self) -> Result<()> {
        if self.violation.is_empty() {
            Ok(())
        } else {
            Err(anyhow!(PlanValidationError {
                phase: self.phase,
                violation: self.violation,
            }))
        }
    }
}

fn description_starts_with_action(action: &str, description: &str) -> bool {
    let first_word = description
        .trim_start()
        .trim_start_matches(|character: char| !character.is_ascii_alphabetic())
        .chars()
        .take_while(|character| character.is_ascii_alphabetic())
        .collect::<String>();
    first_word.eq_ignore_ascii_case(action)
}

fn entity_kind_is_type(kind: EntityKind) -> bool {
    matches!(
        kind,
        EntityKind::Class
            | EntityKind::AbstractClass
            | EntityKind::Struct
            | EntityKind::Enum
            | EntityKind::Trait
            | EntityKind::Interface
            | EntityKind::Config
            | EntityKind::Resource
            | EntityKind::Cache
            | EntityKind::Adapter
    )
}

fn sentence_count(value: &str) -> usize {
    let character_list = value.chars().collect::<Vec<_>>();
    character_list
        .iter()
        .enumerate()
        .filter(|(index, character)| {
            matches!(character, '.' | '?' | '!')
                && character_list
                    .get(index + 1)
                    .is_none_or(|next| next.is_whitespace())
        })
        .count()
}

#[cfg(test)]
mod test {
    use super::super::{
        EntityReference, ExternalEntityKind, PlanCallable, PlanCallableKind, PlanFileChange,
        PlanFlowEdge, PlanFlowRelation, PlanFlowValue, attach_test_fixture, test_fixture,
    };

    #[test]
    fn rejects_a_file_rename_without_distinct_paths() {
        let mut document = test_fixture("plan", "Overview");
        document.tasks[0].files[0].change = PlanFileChange::Rename {
            from: "src/plan.rs".into(),
            to: "src/plan.rs".into(),
        };

        let error = document.validate().unwrap_err().to_string();
        assert!(error.contains("rename source and destination must differ"));
    }

    #[test]
    fn aggregates_independent_edit_violations() {
        let mut document = test_fixture("plan", "Validate.");
        document.entity_changes[0].entity_id = "Bad Id".into();
        document.entity_changes[0].path = "../escape.rs".into();

        let error = document.validate().unwrap_err().to_string();

        assert!(error.contains("stable snake_case"));
        assert!(error.contains("cannot escape the workspace"));
    }

    #[test]
    fn submission_rejects_unattached_entities() {
        let mut document = test_fixture("plan", "Validate.");
        document.tasks[0].files[0].subtasks[0]
            .owned_entity_ids_mut()
            .unwrap()
            .clear();

        let error = document.validate_for_submission().unwrap_err().to_string();

        assert!(error.contains("must belong to exactly one subtask"));
        assert!(error.contains("requires at least one entity"));
    }

    #[test]
    fn rejects_ids_reused_across_plan_object_types() {
        let mut document = test_fixture("plan", "Validate.");
        document.flows[0].flow_id = "plan_document".into();

        let error = document.validate().unwrap_err().to_string();

        assert!(error.contains("duplicates globally unique ID"));
    }

    #[test]
    fn planned_entity_references_must_resolve() {
        let mut document = test_fixture("plan", "Validate.");
        document.flows[0].steps[0].target =
            super::super::document::EntityReference::PlannedEntity {
                entity: "missing_entity".into(),
            };

        let error = document.validate().unwrap_err().to_string();

        assert!(error.contains("references unknown planned entity `missing_entity`"));
    }

    #[test]
    fn structured_flow_values_require_nonempty_content() {
        let mut document = test_fixture("plan", "Build structured planning.");
        document.flows[0].steps[0].edges[0].result = Some(PlanFlowValue::Type { name: " ".into() });

        let error = document.validate().unwrap_err();

        assert!(error.to_string().contains("result.name: cannot be empty"));
    }

    #[test]
    fn flow_descriptions_require_two_substantive_sentences() {
        let mut document = test_fixture("plan", "Build structured planning.");
        document.flows[0].description = "Connect intent to work.".into();

        let error = document.validate().unwrap_err();

        assert!(
            error
                .to_string()
                .contains("description: must contain exactly two sentences")
        );
    }

    #[test]
    fn typed_flow_edges_require_unique_identity_valid_targets_and_results() {
        let mut document = test_fixture("plan", "Validate.");
        document.flows[0].steps[0].edges = vec![
            PlanFlowEdge {
                edge_id: "read_schema".into(),
                relation: PlanFlowRelation::Read {
                    callable: PlanCallable {
                        kind: PlanCallableKind::Method,
                        name: "schema".into(),
                    },
                },
                target: EntityReference::PlannedEntity {
                    entity: "missing_entity".into(),
                },
                result: Some(PlanFlowValue::Text {
                    text: "schema text".into(),
                }),
            },
            PlanFlowEdge {
                edge_id: "read_schema".into(),
                relation: PlanFlowRelation::Call {
                    callable: PlanCallable {
                        kind: PlanCallableKind::Method,
                        name: "count".into(),
                    },
                },
                target: EntityReference::ExternalEntity {
                    entity_kind: ExternalEntityKind::Type,
                    name: "SessionContext".into(),
                    dependency: Some("datafusion".into()),
                },
                result: Some(PlanFlowValue::Text { text: " ".into() }),
            },
        ];

        let error = document.validate().unwrap_err().to_string();

        assert!(error.contains("duplicates identifier read_schema"));
        assert!(error.contains("references unknown planned entity `missing_entity`"));
        assert!(error.contains("result.text: cannot be empty"));
    }

    #[test]
    fn callable_edges_require_bare_callable_names_and_type_receivers() {
        let mut document = test_fixture("plan", "Validate typed receivers.");
        document.flows[0].steps[0].edges = vec![PlanFlowEdge {
            edge_id: "read_schema".into(),
            relation: PlanFlowRelation::Read {
                callable: PlanCallable {
                    kind: PlanCallableKind::Method,
                    name: "schema()".into(),
                },
            },
            target: EntityReference::ExternalEntity {
                entity_kind: ExternalEntityKind::Endpoint,
                name: "registered relation".into(),
                dependency: Some(" ".into()),
            },
            result: Some(PlanFlowValue::Type {
                name: "SchemaRef".into(),
            }),
        }];

        let error = document.validate().unwrap_err().to_string();
        assert!(error.contains("without parentheses"));
        assert!(error.contains("must reference one type entity"));
        assert!(error.contains("target.dependency: cannot be empty"));
    }

    #[test]
    fn submission_allows_no_tests_or_inferred_coverage() {
        let document = test_fixture("plan", "Keep tests optional.");

        document.validate_for_submission().unwrap();
    }

    #[test]
    fn submission_requires_a_model_authored_title() {
        let mut document = test_fixture("plan", "Name the plan.");
        document.title = super::PROVISIONAL_PLAN_TITLE.into();

        let error = document.validate_for_submission().unwrap_err().to_string();

        assert!(error.contains("must replace the provisional title"));
    }

    #[test]
    fn submission_rejects_a_description_that_repeats_its_operation() {
        let mut document = test_fixture("plan", "Keep structured operations singular.");
        let super::super::document::PlanSubtask::Work(subtask) =
            &mut document.tasks[0].files[0].subtasks[0]
        else {
            panic!("fixture must contain a work subtask");
        };
        subtask.description = "Create the canonical plan owner.".into();

        let error = document.validate_for_submission().unwrap_err().to_string();

        assert!(error.contains(
            "tasks.create_plan_state.files.src/plan.rs.subtasks.create_owner.description"
        ));
        assert!(error.contains(
            "must complement operation `create` without repeating `Create` as its first word"
        ));
    }

    #[test]
    fn submission_accepts_a_description_that_complements_its_operation() {
        let mut document = test_fixture("plan", "Keep structured operations singular.");
        let super::super::document::PlanSubtask::Work(subtask) =
            &mut document.tasks[0].files[0].subtasks[0]
        else {
            panic!("fixture must contain a work subtask");
        };
        subtask.description = "the canonical plan owner and its lifecycle.".into();

        document.validate_for_submission().unwrap();
    }

    #[test]
    fn supplied_test_entity_references_must_resolve() {
        let mut document = test_fixture("plan", "Validate optional test traceability.");
        attach_test_fixture(&mut document);
        let super::super::document::PlanSubtask::Test(test) =
            &mut document.tasks[0].files[0].subtasks[1]
        else {
            panic!("fixture must append a test subtask");
        };
        test.covered_entity_ids = vec!["missing_entity".into()];

        let error = document.validate().unwrap_err().to_string();

        assert!(error.contains("references missing entity missing_entity"));
    }

    #[test]
    fn only_enum_entities_can_declare_variants() {
        let mut document = test_fixture("plan", "Validate enum ownership.");
        document.entity_changes[0]
            .variants
            .push(super::super::document::EnumVariantChange {
                variant_id: "plan_document_variant_ready".into(),
                action: super::super::document::ChangeAction::Add,
                name: "Ready".into(),
                description: "Marks a ready plan.".into(),
                fields: Vec::new(),
            });

        let error = document.validate().unwrap_err().to_string();

        assert!(error.contains("only enum entities can declare variants"));
    }

    #[test]
    fn dependency_manifest_must_have_one_task_owner() {
        let mut document = test_fixture("plan", "Validate dependency ownership.");
        document
            .dependencies
            .push(super::super::document::PlanDependencyChange {
                dependency_id: "dependency_tokio".into(),
                action: super::super::document::ChangeAction::Add,
                name: "tokio".into(),
                version: "1".into(),
                manifest: "Cargo.toml".into(),
                license: Some("MIT".into()),
                justification: "Run asynchronous work.".into(),
            });

        let error = document.validate_for_submission().unwrap_err().to_string();
        assert!(error.contains("must match exactly one task file"));

        document.dependencies[0].manifest = "src/plan.rs".into();
        document.validate_for_submission().unwrap();
    }

    #[test]
    fn dependency_manifest_task_needs_no_program_entity() {
        let mut document = test_fixture("plan", "Validate dependency-only ownership.");
        document
            .dependencies
            .push(super::super::document::PlanDependencyChange {
                dependency_id: "dependency_tokio".into(),
                action: super::super::document::ChangeAction::Add,
                name: "tokio".into(),
                version: "1".into(),
                manifest: "Cargo.toml".into(),
                license: Some("MIT".into()),
                justification: "Run asynchronous work.".into(),
            });
        document.tasks[0]
            .files
            .push(super::super::document::PlanFile {
                change: super::super::document::PlanFileChange::Modify {
                    path: "Cargo.toml".into(),
                },
                subtasks: vec![super::super::document::PlanSubtask::Work(
                    super::super::document::PlanWorkSubtask {
                        subtask_id: "configure_runtime".into(),
                        action: super::super::document::SubtaskAction::Configure,
                        description: "the asynchronous runtime.".into(),
                        entity_ids: Vec::new(),
                    },
                )],
            });

        document.validate_for_submission().unwrap();
    }

    #[test]
    fn dependency_license_must_be_spdx_or_null() {
        let mut document = test_fixture("plan", "Validate dependency licenses.");
        document
            .dependencies
            .push(super::super::document::PlanDependencyChange {
                dependency_id: "dependency_tokio".into(),
                action: super::super::document::ChangeAction::Add,
                name: "tokio".into(),
                version: "1".into(),
                manifest: "src/plan.rs".into(),
                license: Some("probably permissive".into()),
                justification: "Run asynchronous work.".into(),
            });

        let error = document.validate().unwrap_err().to_string();
        assert!(error.contains("valid SPDX license expression or null"));

        document.dependencies[0].license = None;
        document.validate().unwrap();
    }
}
