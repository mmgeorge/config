use std::collections::{HashMap, HashSet};
use std::fmt;
use std::path::{Component, Path};

use anyhow::{Result, anyhow};
use serde::{Deserialize, Serialize};

use super::{
    ChangeAction, EntityChangeAction, EntityKind, EntityReference, PLAN_SCHEMA_VERSION,
    PROVISIONAL_PLAN_TITLE, PlanCallable, PlanDocument, PlanFlowBranch, PlanFlowEdge,
    PlanFlowRelation, PlanGraph, PlanSubtask, ProgramEntityChange, ReferencedEntityKind,
};

const RETURN_ONLY_EXPANSION_MESSAGE: &str = "Expansion only returns a value and does not describe work performed inside the parent relationship. Add a nested construct, call, read, write, send, or emit edge, add a meaningful branch, or remove the expansion and keep the return type on the parent edge.";

/// Defines the validation boundary that rejected one canonical plan.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum PlanValidationPhase {
    Edit,
    Submission,
    Render,
}

/// Represents one actionable violation in a canonical plan.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
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

/// Validate source anchors for unchanged repository constructs at submission time.
pub fn validate_workspace_references(document: &PlanDocument, workspace_root: &Path) -> Result<()> {
    let mut violation = Vec::new();
    let mut validate = |json_path: String, reference: &EntityReference| {
        let EntityReference::WorkspaceEntity {
            name, path, line, ..
        } = reference
        else {
            return;
        };
        let source_path = workspace_root.join(path);
        let source = match std::fs::read_to_string(&source_path) {
            Ok(source) => source,
            Err(error) => {
                violation.push(PlanViolation {
                    path: format!("{json_path}.path"),
                    message: format!("could not read workspace source `{path}`: {error}"),
                });
                return;
            }
        };
        let Some(source_line) = source.lines().nth(line.saturating_sub(1)) else {
            violation.push(PlanViolation {
                path: format!("{json_path}.line"),
                message: format!(
                    "line {line} is outside workspace source `{path}` with {} line(s)",
                    source.lines().count()
                ),
            });
            return;
        };
        if !source_line.contains(name) {
            violation.push(PlanViolation {
                path: format!("{json_path}.line"),
                message: format!("line {line} in `{path}` does not contain entity name `{name}`"),
            });
        }
    };
    for (entity_index, entity) in document.entity_changes.iter().enumerate() {
        if let Some(reference) = &entity.extends {
            validate(format!("entity_changes[{entity_index}].extends"), reference);
        }
        for (reference_index, reference) in entity.conforms_to.iter().enumerate() {
            validate(
                format!("entity_changes[{entity_index}].conforms_to[{reference_index}]"),
                reference,
            );
        }
    }
    for (flow_index, flow) in document.flows.iter().enumerate() {
        let flow_path = format!("flows[{flow_index}]");
        validate(format!("{flow_path}.source"), &flow.source);
        for (edge_index, edge) in flow.edges.iter().enumerate() {
            visit_workspace_edge(
                edge,
                &format!("{flow_path}.edges[{edge_index}]"),
                &mut validate,
            );
        }
    }
    if violation.is_empty() {
        Ok(())
    } else {
        Err(PlanValidationError {
            phase: PlanValidationPhase::Submission,
            violation,
        }
        .into())
    }
}

fn visit_workspace_edge(
    edge: &PlanFlowEdge,
    path: &str,
    validate: &mut impl FnMut(String, &EntityReference),
) {
    validate(format!("{path}.target"), &edge.target);
    for (edge_index, nested_edge) in edge.expansion.iter().enumerate() {
        visit_workspace_edge(
            nested_edge,
            &format!("{path}.expansion[{edge_index}]"),
            validate,
        );
    }
    for (branch_index, branch) in edge.branches.iter().enumerate() {
        for (edge_index, branch_edge) in branch.edges.iter().enumerate() {
            visit_workspace_edge(
                branch_edge,
                &format!("{path}.branches[{branch_index}].edges[{edge_index}]"),
                validate,
            );
        }
    }
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
        if self.document.schema_version != PLAN_SCHEMA_VERSION {
            self.push(
                "schema_version",
                &format!("must equal supported PlanDocument schema version {PLAN_SCHEMA_VERSION}"),
            );
        }
        self.required("plan_id", &self.document.plan_id);
        self.required("title", &self.document.title);
        self.prose("overview", &self.document.overview);
        self.validate_entities();
        self.validate_dependencies();
        self.validate_flows();
        self.validate_tasks();
        self.validate_assumptions();
    }

    fn validate_entities(&mut self) {
        self.unique_name(
            "entity_changes",
            self.document
                .entity_changes
                .iter()
                .map(|entity| entity.name.as_str()),
        );
        for (entity_index, entity) in self.document.entity_changes.iter().enumerate() {
            let path = format!("entity_changes[{entity_index}]");
            self.required(&format!("{path}.name"), &entity.name);
            match (&entity.action, &entity.renamed_from) {
                (EntityChangeAction::Rename, Some(renamed_from)) => {
                    self.required(&format!("{path}.renamed_from"), renamed_from);
                    if renamed_from == &entity.name {
                        self.push(
                            &format!("{path}.renamed_from"),
                            "must differ from the renamed entity name",
                        );
                    }
                }
                (EntityChangeAction::Rename, None) => self.push(
                    &format!("{path}.renamed_from"),
                    "is required when action is rename",
                ),
                (_, Some(_)) => self.push(
                    &format!("{path}.renamed_from"),
                    "is valid only when action is rename",
                ),
                (_, None) => {}
            }
            self.prose(&format!("{path}.description"), &entity.description);
            self.repository_path(&format!("{path}.path"), &entity.path);
            self.unique_name(
                &format!("{path}.members"),
                entity.members.iter().map(|member| member.name.as_str()),
            );
            self.unique_name(
                &format!("{path}.variants"),
                entity.variants.iter().map(|variant| variant.name.as_str()),
            );
            if entity.kind != EntityKind::Enum && !entity.variants.is_empty() {
                self.push(
                    &format!("{path}.variants"),
                    "only enum entities can declare variants",
                );
            }
            self.validate_entity_action_tree(entity, &path);
            for (member_index, member) in entity.members.iter().enumerate() {
                let member_path = format!("{path}.members[{member_index}]");
                self.required(&format!("{member_path}.name"), &member.name);
                self.validate_nested_rename(
                    &member_path,
                    member.action,
                    member.renamed_from.as_deref(),
                    &member.name,
                );
                if let Some(description) = &member.description {
                    self.prose(&format!("{member_path}.description"), description);
                }
                match member.kind {
                    super::MemberKind::Field
                    | super::MemberKind::Property
                    | super::MemberKind::Constant => {
                        if member.type_name.as_deref().is_none_or(str::is_empty) {
                            self.push(
                                &format!("{member_path}.type"),
                                "is required for field-like members",
                            );
                        }
                        if !member.parameters.is_empty() || member.return_type.is_some() {
                            self.push(
                                &member_path,
                                "field-like members cannot declare callable parameters or return types",
                            );
                        }
                        if matches!(
                            member.kind,
                            super::MemberKind::Field | super::MemberKind::Property
                        ) && member.visibility.is_none()
                        {
                            self.push(
                                &format!("{member_path}.visibility"),
                                "is required for entity-owned fields and properties",
                            );
                        }
                    }
                    super::MemberKind::Method | super::MemberKind::Function => {
                        if member.type_name.is_some() {
                            self.push(
                                &format!("{member_path}.type"),
                                "is not valid for callable members",
                            );
                        }
                    }
                }
            }
            for (variant_index, variant) in entity.variants.iter().enumerate() {
                let variant_path = format!("{path}.variants[{variant_index}]");
                self.required(&format!("{variant_path}.name"), &variant.name);
                self.validate_nested_rename(
                    &variant_path,
                    variant.action,
                    variant.renamed_from.as_deref(),
                    &variant.name,
                );
                if let Some(description) = &variant.description {
                    self.prose(&format!("{variant_path}.description"), description);
                }
                self.unique_name(
                    &format!("{variant_path}.fields"),
                    variant.fields.iter().map(|field| field.name.as_str()),
                );
                for (field_index, field) in variant.fields.iter().enumerate() {
                    let field_path = format!("{variant_path}.fields[{field_index}]");
                    self.required(&format!("{field_path}.name"), &field.name);
                    self.required(&format!("{field_path}.type"), &field.type_name);
                    self.validate_nested_rename(
                        &field_path,
                        field.action,
                        field.renamed_from.as_deref(),
                        &field.name,
                    );
                    if let Some(description) = &field.description {
                        self.prose(&format!("{field_path}.description"), description);
                    }
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
        self.unique_name(
            "dependencies",
            self.document
                .dependencies
                .iter()
                .map(|dependency| dependency.name.as_str()),
        );
        let mut declaration_set = HashSet::new();
        for (dependency_index, dependency) in self.document.dependencies.iter().enumerate() {
            let path = format!("dependencies[{dependency_index}]");
            if dependency.action == ChangeAction::Rename {
                self.push(
                    &format!("{path}.action"),
                    "cannot rename a dependency; modify its complete manifest declaration instead",
                );
            }
            self.required(&format!("{path}.name"), &dependency.name);
            self.required(&format!("{path}.version"), &dependency.version);
            self.repository_path(&format!("{path}.manifest"), &dependency.manifest);
            self.prose(&format!("{path}.justification"), &dependency.justification);
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
        for (member_index, member) in entity.members.iter().enumerate() {
            if entity.action == EntityChangeAction::Add && member.action != ChangeAction::Add {
                self.push(
                    &format!("{path}.members[{member_index}].action"),
                    "an added entity can contain only added members",
                );
            }
            if entity.action == EntityChangeAction::Remove && member.action != ChangeAction::Remove
            {
                self.push(
                    &format!("{path}.members[{member_index}].action"),
                    "a removed entity can contain only removed members",
                );
            }
        }
        for (variant_index, variant) in entity.variants.iter().enumerate() {
            if entity.action == EntityChangeAction::Add && variant.action != ChangeAction::Add {
                self.push(
                    &format!("{path}.variants[{variant_index}].action"),
                    "an added enum can contain only added variants",
                );
            }
            if entity.action == EntityChangeAction::Remove && variant.action != ChangeAction::Remove
            {
                self.push(
                    &format!("{path}.variants[{variant_index}].action"),
                    "a removed enum can contain only removed variants",
                );
            }
            for (field_index, field) in variant.fields.iter().enumerate() {
                if variant.action == ChangeAction::Add && field.action != ChangeAction::Add {
                    self.push(
                        &format!("{path}.variants[{variant_index}].fields[{field_index}].action"),
                        "an added variant can contain only added fields",
                    );
                }
                if variant.action == ChangeAction::Remove && field.action != ChangeAction::Remove {
                    self.push(
                        &format!("{path}.variants[{variant_index}].fields[{field_index}].action"),
                        "a removed variant can contain only removed fields",
                    );
                }
            }
        }
    }

    fn validate_flows(&mut self) {
        self.unique_name(
            "flows",
            self.document.flows.iter().map(|flow| flow.title.as_str()),
        );
        for (flow_index, flow) in self.document.flows.iter().enumerate() {
            let path = format!("flows[{flow_index}]");
            self.required(&format!("{path}.title"), &flow.title);
            self.prose(&format!("{path}.description"), &flow.description);
            self.validate_reference(&format!("{path}.source"), &flow.source);
            if flow.edges.is_empty() {
                self.push(&format!("{path}.edges"), "must contain at least one edge");
            }
            for (edge_index, edge) in flow.edges.iter().enumerate() {
                self.validate_flow_edge(edge, &format!("{path}.edges[{edge_index}]"));
            }
        }
    }

    fn validate_flow_branch(&mut self, branch: &PlanFlowBranch, branch_path: &str) {
        self.required(&format!("{branch_path}.condition"), &branch.condition);
        if branch.edges.is_empty() {
            self.push(
                &format!("{branch_path}.edges"),
                "must contain at least one edge",
            );
        }
        for (edge_index, edge) in branch.edges.iter().enumerate() {
            self.validate_flow_edge(edge, &format!("{branch_path}.edges[{edge_index}]"));
        }
    }

    fn validate_flow_branches(&mut self, branches: &[PlanFlowBranch], edge_path: &str) {
        let mut condition_set = HashSet::new();
        for (branch_index, branch) in branches.iter().enumerate() {
            let branch_path = format!("{edge_path}.branches[{branch_index}]");
            if !condition_set.insert(branch.condition.as_str()) {
                self.push(
                    &format!("{branch_path}.condition"),
                    &format!("duplicates branch condition {}", branch.condition),
                );
            }
            self.validate_flow_branch(branch, &branch_path);
        }
    }

    fn validate_flow_edge(&mut self, edge: &PlanFlowEdge, edge_path: &str) {
        self.validate_reference(&format!("{edge_path}.target"), &edge.target);
        match edge.relation {
            PlanFlowRelation::Call | PlanFlowRelation::Read | PlanFlowRelation::Write => {
                if let Some(callable) = &edge.callable {
                    self.callable(&format!("{edge_path}.callable"), callable);
                } else {
                    self.push(
                        &format!("{edge_path}.callable"),
                        "is required for callable relations",
                    );
                }
                self.validate_type_reference(&format!("{edge_path}.target"), &edge.target);
            }
            PlanFlowRelation::Send | PlanFlowRelation::Emit | PlanFlowRelation::Return => {
                if let Some(payload_type) = &edge.payload_type {
                    self.required(&format!("{edge_path}.payload_type"), payload_type);
                } else {
                    self.push(
                        &format!("{edge_path}.payload_type"),
                        "is required for payload relations",
                    );
                }
            }
            PlanFlowRelation::Construct => {
                self.validate_type_reference(&format!("{edge_path}.target"), &edge.target);
            }
        }
        if !matches!(
            edge.relation,
            PlanFlowRelation::Call | PlanFlowRelation::Read | PlanFlowRelation::Write
        ) && edge.return_type.is_some()
        {
            self.push(
                &format!("{edge_path}.return_type"),
                "belongs only to callable relations",
            );
        }
        if let Some(return_type) = &edge.return_type {
            self.required(
                &format!("{edge_path}.return_type.value_type"),
                &return_type.value_type,
            );
            if let Some(error_type) = &return_type.error_type {
                self.required(&format!("{edge_path}.return_type.error_type"), error_type);
            }
        }
        for (edge_index, nested_edge) in edge.expansion.iter().enumerate() {
            self.validate_flow_edge(nested_edge, &format!("{edge_path}.expansion[{edge_index}]"));
        }
        self.validate_flow_branches(&edge.branches, edge_path);
    }

    fn validate_tasks(&mut self) {
        self.unique_name(
            "tasks",
            self.document.tasks.iter().map(|task| task.title.as_str()),
        );
        let mut attached_entity = HashMap::<&str, String>::new();
        for (task_index, task) in self.document.tasks.iter().enumerate() {
            let path = format!("tasks[{task_index}]");
            self.required(&format!("{path}.title"), &task.title);
            self.prose(&format!("{path}.description"), &task.description);
            let mut file_path_set = HashSet::new();
            for (file_index, file) in task.files.iter().enumerate() {
                let path_value = file.change.path();
                let file_path = format!("{path}.files[{file_index}]");
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
                for (subtask_index, subtask) in file.subtasks.iter().enumerate() {
                    let subtask_path = format!("{file_path}.subtasks[{subtask_index}]");
                    match subtask {
                        PlanSubtask::Work(subtask) => {
                            self.prose(
                                &format!("{subtask_path}.description"),
                                &subtask.description,
                            );
                            let mut local_entity = HashSet::new();
                            for entity_name in &subtask.entities {
                                if !local_entity.insert(entity_name.as_str()) {
                                    self.push(
                                        &format!("{subtask_path}.entities"),
                                        &format!("duplicates entity {entity_name}"),
                                    );
                                }
                                let Some(entity) = self.graph.entity(entity_name) else {
                                    self.push(
                                        &format!("{subtask_path}.entities"),
                                        &format!("references missing entity {entity_name}"),
                                    );
                                    continue;
                                };
                                if let Some(previous) = attached_entity
                                    .insert(entity.name.as_str(), subtask_path.clone())
                                {
                                    self.push(
                                        &format!("{subtask_path}.entities"),
                                        &format!(
                                            "entity {entity_name} already belongs to {previous}"
                                        ),
                                    );
                                }
                                if entity.path != path_value {
                                    self.push(
                                        &format!("{subtask_path}.entities"),
                                        &format!(
                                            "entity {entity_name} belongs to {} rather than {}",
                                            entity.path, path_value
                                        ),
                                    );
                                }
                                if file.change.entity_action() == ChangeAction::Add
                                    && entity.action != EntityChangeAction::Add
                                {
                                    self.push(
                                        &format!("{file_path}.action"),
                                        "an added file can contain only added entities",
                                    );
                                }
                                if file.change.entity_action() == ChangeAction::Remove
                                    && entity.action != EntityChangeAction::Remove
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
                            self.validate_nested_rename(
                                &subtask_path,
                                test.action,
                                test.renamed_from.as_deref(),
                                &test.name,
                            );
                            self.prose(&format!("{subtask_path}.behavior"), &test.behavior);
                            let mut covered_entity = HashSet::new();
                            for entity_name in &test.covers_entities {
                                if !covered_entity.insert(entity_name.as_str()) {
                                    self.push(
                                        &format!("{subtask_path}.covers_entities"),
                                        &format!("duplicates entity {entity_name}"),
                                    );
                                }
                                if self.graph.entity(entity_name).is_none() {
                                    self.push(
                                        &format!("{subtask_path}.covers_entities"),
                                        &format!("references missing entity {entity_name}"),
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
            self.prose(&path, assumption);
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
        for (flow_index, flow) in self.document.flows.iter().enumerate() {
            if flow.edges.is_empty() {
                self.push(
                    &format!("flows[{flow_index}].edges"),
                    "requires at least one typed runtime edge",
                );
            }
            for (edge_index, edge) in flow.edges.iter().enumerate() {
                self.validate_submission_flow_edge(
                    edge,
                    &format!("flows[{flow_index}].edges[{edge_index}]"),
                );
            }
        }
        let attached_entity_name = self
            .document
            .tasks
            .iter()
            .flat_map(|task| &task.files)
            .flat_map(|file| &file.subtasks)
            .flat_map(PlanSubtask::owned_entities)
            .filter_map(|entity| self.graph.entity(entity))
            .map(|entity| entity.name.as_str())
            .collect::<HashSet<_>>();
        for (dependency_index, dependency) in self.document.dependencies.iter().enumerate() {
            let owner_count = self
                .document
                .tasks
                .iter()
                .flat_map(|task| &task.files)
                .filter(|file| file.change.path() == dependency.manifest)
                .count();
            if owner_count != 1 {
                self.push(
                    &format!("dependencies[{dependency_index}].manifest"),
                    "must match exactly one task file",
                );
            }
        }
        for (entity_index, entity) in self.document.entity_changes.iter().enumerate() {
            if !attached_entity_name.contains(entity.name.as_str()) {
                self.push(
                    &format!("entity_changes[{entity_index}]"),
                    "must belong to exactly one subtask before submission",
                );
            }
        }
        for (task_index, task) in self.document.tasks.iter().enumerate() {
            if task.files.is_empty() {
                self.push(
                    &format!("tasks[{task_index}].files"),
                    "requires at least one file",
                );
            }
            for (file_index, file) in task.files.iter().enumerate() {
                let file_path = file.change.path();
                if file.subtasks.is_empty() {
                    self.push(
                        &format!("tasks[{task_index}].files[{file_index}].subtasks"),
                        "requires at least one subtask",
                    );
                }
                for (subtask_index, subtask) in file.subtasks.iter().enumerate() {
                    let subtask_path = format!(
                        "tasks[{task_index}].files[{file_index}].subtasks[{subtask_index}]"
                    );
                    if let PlanSubtask::Work(work) = subtask
                        && description_starts_with_action(work.action.label(), &work.description)
                    {
                        self.push(
                            &format!("{subtask_path}.description"),
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
                    if matches!(subtask, PlanSubtask::Work(work) if work.entities.is_empty())
                        && !owns_dependency
                    {
                        self.push(
                            &format!("{subtask_path}.entities"),
                            "requires at least one entity",
                        );
                    }
                }
            }
        }
    }

    fn validate_submission_flow_edge(&mut self, edge: &PlanFlowEdge, edge_path: &str) {
        if !edge.expansion.is_empty() && !flow_expansion_has_material_work(&edge.expansion) {
            self.push(
                &format!("{edge_path}.expansion"),
                RETURN_ONLY_EXPANSION_MESSAGE,
            );
        }
        for (edge_index, nested_edge) in edge.expansion.iter().enumerate() {
            self.validate_submission_flow_edge(
                nested_edge,
                &format!("{edge_path}.expansion[{edge_index}]"),
            );
        }
        for (branch_index, branch) in edge.branches.iter().enumerate() {
            for (edge_index, branch_edge) in branch.edges.iter().enumerate() {
                self.validate_submission_flow_edge(
                    branch_edge,
                    &format!("{edge_path}.branches[{branch_index}].edges[{edge_index}]"),
                );
            }
        }
    }

    fn validate_render(&mut self) {
        for (entity_index, entity) in self.document.entity_changes.iter().enumerate() {
            if entity.name.trim().is_empty() {
                self.push(
                    &format!("entity_changes[{entity_index}]"),
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
            EntityReference::WorkspaceEntity {
                name,
                path: source_path,
                line,
                ..
            } => {
                self.required(&format!("{path}.name"), name);
                self.repository_path(&format!("{path}.path"), source_path);
                if *line == 0 {
                    self.push(&format!("{path}.line"), "must be greater than zero");
                }
                if self
                    .document
                    .entity_changes
                    .iter()
                    .any(|entity| entity.name == *name && entity.path == *source_path)
                {
                    self.push(
                        path,
                        "must use planned_entity for a construct changed by this plan",
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
            EntityReference::PlannedEntity { entity } => {
                let Some(entity) = self.graph.entity(entity) else {
                    return;
                };
                entity_kind_is_type(entity.kind)
            }
            EntityReference::WorkspaceEntity { entity_kind, .. }
            | EntityReference::ExternalEntity { entity_kind, .. } => {
                *entity_kind == ReferencedEntityKind::Type
            }
        };
        if !is_type {
            self.push(path, "must reference one type entity");
        }
    }

    fn required(&mut self, path: &str, value: &str) {
        if value.trim().is_empty() {
            self.push(path, "cannot be empty");
        }
    }

    fn validate_nested_rename(
        &mut self,
        path: &str,
        action: ChangeAction,
        renamed_from: Option<&str>,
        name: &str,
    ) {
        match (action, renamed_from) {
            (ChangeAction::Rename, Some(previous_name)) => {
                self.required(&format!("{path}.renamed_from"), previous_name);
                if previous_name == name {
                    self.push(
                        &format!("{path}.renamed_from"),
                        "must differ from the renamed declaration name",
                    );
                }
            }
            (ChangeAction::Rename, None) => self.push(
                &format!("{path}.renamed_from"),
                "is required when action is rename",
            ),
            (_, Some(_)) => self.push(
                &format!("{path}.renamed_from"),
                "is valid only when action is rename",
            ),
            (_, None) => {}
        }
    }

    fn prose(&mut self, path: &str, value: &str) {
        self.required(path, value);
        let normalized = value
            .trim()
            .trim_matches(['.', '!', '?'])
            .to_ascii_lowercase();
        if matches!(
            normalized.as_str(),
            "todo" | "tbd" | "placeholder" | "n/a" | "none"
        ) {
            self.push(path, "must describe substantive reviewer-visible intent");
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

    fn unique_name<'b>(&mut self, path: &str, values: impl Iterator<Item = &'b str>) {
        let mut value_set = HashSet::new();
        for value in values {
            if value.trim().is_empty() {
                self.push(path, "contains an empty name");
            } else if !value_set.insert(value) {
                self.push(path, &format!("duplicates name {value}"));
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

fn flow_expansion_has_material_work(edge_list: &[PlanFlowEdge]) -> bool {
    edge_list.iter().any(|edge| {
        !matches!(edge.relation, PlanFlowRelation::Return)
            || !edge.branches.is_empty()
            || flow_expansion_has_material_work(&edge.expansion)
    })
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

#[cfg(test)]
mod test {
    use super::super::{
        ChangeAction, EntityChangeAction, MemberKind, PlanCallable, PlanCallableKind,
        PlanFileChange, PlanFlowRelation, ProgramEntityMemberChange, Visibility,
        attach_test_fixture, test_fixture,
    };
    use super::validate_plan_edit;

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
    fn validates_entity_rename_semantics() {
        let mut document = test_fixture("plan", "Overview");
        document.entity_changes[0].action = EntityChangeAction::Rename;
        document.entity_changes[0].renamed_from = Some("LegacyPlanDocument".into());
        document.tasks[0].files[0].change = PlanFileChange::Modify {
            path: document.entity_changes[0].path.clone(),
        };
        assert!(document.validate().is_ok());

        document.entity_changes[0].renamed_from = None;
        let missing_error = document.validate().unwrap_err().to_string();
        assert!(missing_error.contains("is required when action is rename"));

        document.entity_changes[0].renamed_from = Some(document.entity_changes[0].name.clone());
        let identical_error = document.validate().unwrap_err().to_string();
        assert!(identical_error.contains("must differ from the renamed entity name"));

        document.entity_changes[0].action = EntityChangeAction::Modify;
        let unexpected_error = document.validate().unwrap_err().to_string();
        assert!(unexpected_error.contains("is valid only when action is rename"));
    }

    #[test]
    fn nested_renames_require_one_distinct_source_name() {
        let mut document = test_fixture("plan", "Rename one member.");
        document.entity_changes[0]
            .members
            .push(ProgramEntityMemberChange {
                action: ChangeAction::Rename,
                renamed_from: None,
                kind: MemberKind::Method,
                name: "render_plan".into(),
                description: Some("Render the canonical plan.".into()),
                visibility: Some(Visibility::Public),
                type_name: None,
                parameters: Vec::new(),
                return_type: Some("String".into()),
            });

        let missing = document.validate().unwrap_err().to_string();
        assert!(missing.contains("renamed_from: is required when action is rename"));

        document.entity_changes[0].members[0].action = ChangeAction::Modify;
        document.entity_changes[0].members[0].renamed_from = Some("render".into());
        let misplaced = document.validate().unwrap_err().to_string();
        assert!(misplaced.contains("renamed_from: is valid only when action is rename"));
    }

    #[test]
    fn aggregates_independent_edit_violations() {
        let mut document = test_fixture("plan", "TODO");
        document.entity_changes[0].path = "../escape.rs".into();

        let error = document.validate().unwrap_err().to_string();

        assert!(error.contains("substantive reviewer-visible intent"));
        assert!(error.contains("cannot escape the workspace"));
    }

    #[test]
    fn submission_rejects_unattached_entities() {
        let mut document = test_fixture("plan", "Validate.");
        document.tasks[0].files[0].subtasks[0]
            .owned_entities_mut()
            .unwrap()
            .clear();

        let error = document.validate_for_submission().unwrap_err().to_string();

        assert!(error.contains("must belong to exactly one subtask"));
        assert!(error.contains("requires at least one entity"));
    }

    #[test]
    fn flow_edges_require_their_relation_specific_types() {
        let mut document = test_fixture("plan", "Validate edge types.");
        document.flows[0].edges[0].relation = PlanFlowRelation::Call;
        document.flows[0].edges[0].payload_type = None;
        document.flows[0].edges[0].return_type = None;
        document.flows[0].edges[0].callable = None;
        document.flows[0].edges[0].target = crate::plan::EntityReference::PlannedEntity {
            entity: "PlanDocument".into(),
        };

        let error = validate_plan_edit(&document).unwrap_err().to_string();

        assert!(error.contains("flows[0].edges[0].callable"));
        document.flows[0].edges[0].callable = Some(PlanCallable {
            kind: PlanCallableKind::Method,
            name: "dispatch".into(),
        });
        document.flows[0].edges[0].return_type = Some(crate::plan::PlanFlowReturnType {
            value_type: "ExecutablePlan".into(),
            error_type: Some("PlanError".into()),
        });
        validate_plan_edit(&document).unwrap();
    }

    #[test]
    fn flow_descriptions_accept_concise_prose_and_reject_placeholders() {
        let mut document = test_fixture("plan", "Build structured planning.");
        document.flows[0].description = "Connect intent to work.".into();

        document.validate().unwrap();

        document.flows[0].description = "TBD".into();
        let error = document.validate().unwrap_err();

        assert!(
            error
                .to_string()
                .contains("description: must describe substantive reviewer-visible intent")
        );
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

        assert!(error.contains("tasks[0].files[0].subtasks[0].description"));
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
        test.covers_entities = vec!["missing_entity".into()];

        let error = document.validate().unwrap_err().to_string();

        assert!(error.contains("references missing entity missing_entity"));
    }

    #[test]
    fn only_enum_entities_can_declare_variants() {
        let mut document = test_fixture("plan", "Validate enum ownership.");
        document.entity_changes[0]
            .variants
            .push(super::super::document::EnumVariantChange {
                action: super::super::document::ChangeAction::Add,
                renamed_from: None,
                name: "Ready".into(),
                description: Some("Marks a ready plan.".into()),
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
                action: super::super::document::ChangeAction::Add,
                name: "tokio".into(),
                version: "1".into(),
                resolved_version: None,
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
                action: super::super::document::ChangeAction::Add,
                name: "tokio".into(),
                version: "1".into(),
                resolved_version: None,
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
                        action: super::super::document::SubtaskAction::Configure,
                        description: "the asynchronous runtime.".into(),
                        entities: Vec::new(),
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
                action: super::super::document::ChangeAction::Add,
                name: "tokio".into(),
                version: "1".into(),
                resolved_version: None,
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
