use super::document::*;
use super::{PlanGraph, validate_plan_render};
use anyhow::Result;
use serde::{Deserialize, Serialize};

/// Defines one reviewer-visible section in the rendered plan.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum PlanSection {
    Title,
    Overview,
    Usage,
    Diagrams,
    ObjectModel,
    Dependencies,
    Tasks,
    Tests,
    Assumptions,
}

/// Identifies the exact canonical object represented by one rendered line.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "target_type", rename_all = "snake_case")]
pub enum PlanReviewTarget {
    Section {
        section: PlanSection,
    },
    Entity {
        entity_id: String,
    },
    EntityMember {
        entity_id: String,
        member_id: String,
    },
    EnumVariant {
        entity_id: String,
        variant_id: String,
    },
    EnumVariantField {
        entity_id: String,
        variant_id: String,
        field_id: String,
    },
    Dependency {
        dependency_id: String,
    },
    DependencyManifest {
        manifest: String,
    },
    Flow {
        flow_id: String,
    },
    FlowStep {
        flow_id: String,
        step_id: String,
    },
    FlowValue {
        flow_id: String,
        step_id: String,
        value_kind: PlanFlowValueKind,
    },
    FlowOperation {
        flow_id: String,
        step_id: String,
        operation_id: String,
    },
    Task {
        task_id: String,
    },
    File {
        task_id: String,
        path: String,
    },
    Subtask {
        task_id: String,
        path: String,
        subtask_id: String,
    },
    Test {
        subtask_id: String,
        category: TestCategory,
    },
    Assumption {
        assumption_index: usize,
    },
}

/// Maps one rendered line back to its exact canonical JSON location and source boundary.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct PlanNavigationAnchor {
    pub line: u32,
    pub target: PlanReviewTarget,
    pub json_path: String,
    pub path: Option<String>,
    pub label: String,
}

/// Owns exact source-navigation anchors for one rendered plan revision.
#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub struct PlanNavigationIndex {
    pub anchor: Vec<PlanNavigationAnchor>,
}

impl PlanNavigationIndex {
    /// Resolve the semantic anchor attached to one reviewer-visible line.
    pub fn resolve_line(&self, line: u32) -> Option<&PlanNavigationAnchor> {
        self.anchor.iter().find(|anchor| anchor.line == line)
    }
}

/// Represents the deterministic human projection of one canonical plan.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct RenderedPlan {
    pub markdown: String,
    pub navigation: PlanNavigationIndex,
}

struct PlanRenderer {
    line: Vec<String>,
    navigation: PlanNavigationIndex,
}

const PLAN_LINE_WIDTH: usize = 100;
const DIAGRAM_CONCRETE_COLUMN: usize = 42;
const DIAGRAM_PATH_COLUMN_MAX: usize = 36;

#[derive(Clone)]
struct DiagramAnchor {
    target: PlanReviewTarget,
    json_path: String,
    path: Option<String>,
    label: String,
}

#[derive(Clone)]
struct DiagramCell {
    text: String,
    anchor: DiagramAnchor,
}

impl PlanRenderer {
    fn new() -> Self {
        Self {
            line: Vec::new(),
            navigation: PlanNavigationIndex::default(),
        }
    }

    fn push(
        &mut self,
        value: impl Into<String>,
        target: PlanReviewTarget,
        json_path: impl Into<String>,
        path: Option<&str>,
        label: impl Into<String>,
    ) {
        let value = value.into();
        self.line.push(value.clone());
        if value.is_empty() {
            return;
        }
        self.navigation.anchor.push(PlanNavigationAnchor {
            line: self.line.len() as u32,
            target,
            json_path: json_path.into(),
            path: path.map(str::to_owned),
            label: label.into(),
        });
    }

    fn blank(&mut self) {
        self.line.push(String::new());
    }

    fn push_wrapped(
        &mut self,
        first_prefix: &str,
        continuation_prefix: &str,
        content: &str,
        target: PlanReviewTarget,
        json_path: impl Into<String>,
        path: Option<&str>,
        label: impl Into<String>,
    ) {
        let json_path = json_path.into();
        let label = label.into();
        for line in wrap_prefixed(content, first_prefix, continuation_prefix, PLAN_LINE_WIDTH) {
            self.push(line, target.clone(), json_path.clone(), path, label.clone());
        }
    }

    fn section(&mut self, value: impl Into<String>, section: PlanSection, json_path: &'static str) {
        self.push(
            value,
            PlanReviewTarget::Section { section },
            json_path,
            None,
            section_label(section),
        );
    }

    fn finish(self) -> RenderedPlan {
        debug_assert_eq!(
            self.line.iter().filter(|line| !line.is_empty()).count(),
            self.navigation.anchor.len()
        );
        RenderedPlan {
            markdown: self.line.join("\n") + "\n",
            navigation: self.navigation,
        }
    }
}

fn wrap_prefixed(
    content: &str,
    first_prefix: &str,
    continuation_prefix: &str,
    width: usize,
) -> Vec<String> {
    let mut line_list = Vec::new();
    let mut current_line = first_prefix.to_owned();
    let mut has_content = false;

    for word in content.split_whitespace() {
        let separator_width = usize::from(has_content);
        if has_content
            && current_line.chars().count() + separator_width + word.chars().count() > width
        {
            line_list.push(current_line);
            current_line = continuation_prefix.to_owned();
            has_content = false;
        }
        if has_content {
            current_line.push(' ');
        }
        current_line.push_str(word);
        has_content = true;
    }

    if has_content || line_list.is_empty() {
        line_list.push(current_line);
    }
    line_list
}

/// Render one canonical plan into reviewer-readable Markdown and exact navigation metadata.
pub fn render_plan(document: &PlanDocument) -> Result<RenderedPlan> {
    validate_plan_render(document)?;
    let graph = PlanGraph::new(document);
    let mut renderer = PlanRenderer::new();
    renderer.section(
        format!("# {}", document.title),
        PlanSection::Title,
        "/title",
    );
    renderer.blank();
    renderer.section(&document.overview, PlanSection::Overview, "/overview");
    renderer.blank();
    render_usage(&mut renderer, document.usage.as_ref());
    render_assumptions(&mut renderer, document);
    renderer.blank();
    render_diagrams(&mut renderer, document, &graph);
    render_dependencies(&mut renderer, document);
    render_tasks(&mut renderer, document, &graph);
    render_tests(&mut renderer, document);
    Ok(renderer.finish())
}

/// Build one unified delta for a canonical plan artifact revision.
pub fn render_plan_delta(path: &str, before: &str, after: &str) -> String {
    let diff = similar::TextDiff::from_lines(before, after);
    let before_header = if before.is_empty() {
        "/dev/null".to_owned()
    } else {
        format!("a/{path}")
    };
    let mut output = format!("diff --git a/{path} b/{path}\n");
    output.push_str(
        &diff
            .unified_diff()
            .context_radius(3)
            .header(&before_header, &format!("b/{path}"))
            .to_string(),
    );
    output
}

fn render_usage(renderer: &mut PlanRenderer, usage: Option<&PlanUsage>) {
    renderer.section("# Usage", PlanSection::Usage, "/usage");
    renderer.blank();
    let Some(usage) = usage else {
        renderer.section("<Omitted>", PlanSection::Usage, "/usage");
        renderer.blank();
        return;
    };
    renderer.section("```text", PlanSection::Usage, "/usage/command");
    for line in usage.command.lines() {
        renderer.section(line, PlanSection::Usage, "/usage/command");
    }
    renderer.section("```", PlanSection::Usage, "/usage/command");
    renderer.blank();
    renderer.section(
        "## Expected Result",
        PlanSection::Usage,
        "/usage/expected_result",
    );
    renderer.blank();
    renderer.section("```text", PlanSection::Usage, "/usage/expected_result");
    for line in usage.expected_result.lines() {
        renderer.section(line, PlanSection::Usage, "/usage/expected_result");
    }
    renderer.section("```", PlanSection::Usage, "/usage/expected_result");
    renderer.blank();
}

fn render_diagrams(renderer: &mut PlanRenderer, document: &PlanDocument, graph: &PlanGraph<'_>) {
    renderer.section("# Diagrams", PlanSection::Diagrams, "/entity_changes");
    renderer.blank();
    render_object_model(renderer, document, graph);

    for (flow_index, flow) in document.flows.iter().enumerate() {
        let flow_path = format!("/flows/{flow_index}");
        let flow_target = PlanReviewTarget::Flow {
            flow_id: flow.flow_id.clone(),
        };
        renderer.push(
            format!("## Code flow: {}", flow.title),
            flow_target.clone(),
            &flow_path,
            None,
            format!("Flow: {}", flow.title),
        );
        renderer.blank();
        renderer.push(
            &flow.description,
            flow_target.clone(),
            format!("{flow_path}/description"),
            None,
            format!("Flow: {}", flow.title),
        );
        renderer.blank();
        renderer.push(
            "```text",
            flow_target.clone(),
            &flow_path,
            None,
            format!("Flow: {}", flow.title),
        );
        render_flow_rows(renderer, graph, flow, &flow_path);
        renderer.push(
            "```",
            flow_target,
            &flow_path,
            None,
            format!("Flow: {}", flow.title),
        );
        renderer.blank();
    }
}

fn render_flow_rows(
    renderer: &mut PlanRenderer,
    graph: &PlanGraph<'_>,
    flow: &PlanFlow,
    flow_path: &str,
) {
    let owner_column = flow
        .steps
        .iter()
        .flat_map(|step| {
            std::iter::once(marked_flow_action(&step.action, &step.target))
                .chain(step.operations.iter().map(flow_operation_content))
        })
        .map(|content| content.chars().count())
        .max()
        .unwrap_or_default()
        + 4;

    for (step_index, step) in flow.steps.iter().enumerate() {
        let step_path = format!("{flow_path}/steps/{step_index}");
        let step_target = PlanReviewTarget::FlowStep {
            flow_id: flow.flow_id.clone(),
            step_id: step.step_id.clone(),
        };
        push_owned_flow_line(
            renderer,
            marked_flow_action(&step.action, &step.target),
            format!("[{}]", flow_step_location(graph, &step.target)),
            owner_column,
            step_target.clone(),
            &step_path,
            format!("Flow step: {}", step.action),
        );
        for (operation_index, operation) in step.operations.iter().enumerate() {
            let operation_path = format!("{step_path}/operations/{operation_index}");
            push_owned_flow_line(
                renderer,
                flow_operation_content(operation),
                format!("[{}]", flow_step_location(graph, &operation.target)),
                owner_column,
                PlanReviewTarget::FlowOperation {
                    flow_id: flow.flow_id.clone(),
                    step_id: step.step_id.clone(),
                    operation_id: operation.operation_id.clone(),
                },
                &operation_path,
                format!("Flow operation: {}", operation.action),
            );
        }
        if let Some(value) = &step.value_to_next {
            let branch = if step_index + 1 < flow.steps.len() {
                "├─"
            } else {
                "└─"
            };
            renderer.push(
                format!("    {branch} {}", value.text()),
                PlanReviewTarget::FlowValue {
                    flow_id: flow.flow_id.clone(),
                    step_id: step.step_id.clone(),
                    value_kind: value.kind(),
                },
                format!("{step_path}/value_to_next"),
                None,
                format!("Flow step: {}", step.action),
            );
        }
        if step_index + 1 < flow.steps.len() {
            renderer.push(
                "    ▼",
                step_target,
                &step_path,
                None,
                format!("Flow step: {}", step.action),
            );
        }
    }
}

fn marked_flow_action(action: &str, target: &EntityReference) -> String {
    format!(
        "{}{action}",
        if matches!(target, EntityReference::PlannedEntity { .. }) {
            "*"
        } else {
            ""
        }
    )
}

fn flow_operation_content(operation: &PlanFlowOperation) -> String {
    let action = marked_flow_action(&operation.action, &operation.target);
    match operation.result.as_deref() {
        Some(result) => format!("    ├─ {action}: {result}"),
        None => format!("    ├─ {action}"),
    }
}

fn push_owned_flow_line(
    renderer: &mut PlanRenderer,
    content: String,
    owner: String,
    owner_column: usize,
    target: PlanReviewTarget,
    json_path: &str,
    label: String,
) {
    if owner_column + owner.chars().count() <= PLAN_LINE_WIDTH {
        let mut line = pad_diagram_column(&content, owner_column);
        line.push_str(&owner);
        renderer.push(line, target, json_path, None, label);
        return;
    }
    renderer.push(content, target.clone(), json_path, None, label.clone());
    renderer.push(format!("    {owner}"), target, json_path, None, label);
}

fn flow_step_location(graph: &PlanGraph<'_>, target: &EntityReference) -> String {
    match target {
        EntityReference::PlannedEntity { entity } => graph
            .entity_path(entity)
            .map(str::to_owned)
            .or_else(|| graph.entity_label(entity))
            .unwrap_or_else(|| "<missing entity>".into()),
        EntityReference::ExternalEntity { entity } => entity.clone(),
    }
}

fn render_object_model(
    renderer: &mut PlanRenderer,
    document: &PlanDocument,
    graph: &PlanGraph<'_>,
) {
    let has_contract = document
        .entity_changes
        .iter()
        .any(|entity| entity.exclusive_owner_entity_id.is_none() && is_contract_kind(entity.kind));
    if !has_contract {
        renderer.section("## Concrete", PlanSection::ObjectModel, "/entity_changes");
        renderer.blank();
        renderer.section("```text", PlanSection::ObjectModel, "/entity_changes");
        for cell in diagram_entity_group(
            document,
            graph,
            |kind| !is_contract_kind(kind),
            PLAN_LINE_WIDTH,
        ) {
            renderer.push(
                cell.text,
                cell.anchor.target,
                cell.anchor.json_path,
                cell.anchor.path.as_deref(),
                cell.anchor.label,
            );
        }
        renderer.section("```", PlanSection::ObjectModel, "/entity_changes");
        renderer.blank();
        return;
    }

    renderer.section("```text", PlanSection::ObjectModel, "/entity_changes");
    let contract_cell_list =
        diagram_entity_group(document, graph, is_contract_kind, DIAGRAM_CONCRETE_COLUMN);
    let concrete_cell_list = diagram_entity_group(
        document,
        graph,
        |kind| !is_contract_kind(kind),
        PLAN_LINE_WIDTH.saturating_sub(DIAGRAM_CONCRETE_COLUMN),
    );
    renderer.section(
        format!(
            "{}Concrete",
            pad_diagram_column("Contracts", DIAGRAM_CONCRETE_COLUMN)
        ),
        PlanSection::ObjectModel,
        "/entity_changes",
    );
    renderer.blank();
    let row_count = contract_cell_list.len().max(concrete_cell_list.len());
    for row_index in 0..row_count {
        let contract_cell = contract_cell_list.get(row_index);
        let concrete_cell = concrete_cell_list.get(row_index);
        let contract_text = contract_cell.map(|cell| cell.text.as_str()).unwrap_or("");
        let concrete_text = concrete_cell.map(|cell| cell.text.as_str()).unwrap_or("");
        let line = if concrete_text.is_empty() {
            contract_text.to_owned()
        } else {
            format!(
                "{}{}",
                pad_diagram_column(contract_text, DIAGRAM_CONCRETE_COLUMN),
                concrete_text
            )
        };
        let anchor = contract_cell
            .or(concrete_cell)
            .map(|cell| cell.anchor.clone())
            .unwrap_or_else(object_model_anchor);
        renderer.push(
            line,
            anchor.target,
            anchor.json_path,
            anchor.path.as_deref(),
            anchor.label,
        );
    }
    renderer.section("```", PlanSection::ObjectModel, "/entity_changes");
    renderer.blank();
}

fn diagram_entity_group(
    document: &PlanDocument,
    graph: &PlanGraph<'_>,
    include: impl Fn(EntityKind) -> bool,
    width: usize,
) -> Vec<DiagramCell> {
    let entity_list = document
        .entity_changes
        .iter()
        .enumerate()
        .filter(|(_, entity)| entity.exclusive_owner_entity_id.is_none() && include(entity.kind))
        .collect::<Vec<_>>();
    if entity_list.is_empty() {
        return vec![DiagramCell {
            text: "<none>".into(),
            anchor: object_model_anchor(),
        }];
    }
    let longest_path_width = entity_list
        .iter()
        .map(|(_, entity)| format!("[{}]", entity.path).chars().count())
        .max()
        .unwrap_or_default();
    let path_column = DIAGRAM_PATH_COLUMN_MAX.min(width.saturating_sub(longest_path_width));
    let mut cell_list = Vec::new();
    for (entity_index, entity) in entity_list {
        append_entity_cells(
            &mut cell_list,
            document,
            graph,
            entity_index,
            entity,
            0,
            width,
            path_column,
        );
        cell_list.push(DiagramCell {
            text: String::new(),
            anchor: object_model_anchor(),
        });
    }
    cell_list
}

fn append_entity_cells(
    cell_list: &mut Vec<DiagramCell>,
    document: &PlanDocument,
    graph: &PlanGraph<'_>,
    entity_index: usize,
    entity: &ProgramEntityChange,
    depth: usize,
    width: usize,
    path_column: usize,
) {
    let entity_path = format!("/entity_changes/{entity_index}");
    let indent = "  ".repeat(depth);
    let mut declaration = format!(
        "{}{} {}",
        declaration_marker(entity.action),
        entity_kind_label(entity.kind),
        entity.name
    );
    if let Some(reference) = &entity.extends {
        declaration.push_str(&format!(
            " extends {}",
            entity_reference_label(graph, reference)
        ));
    }
    if !entity.conforms_to.is_empty() {
        let contract_list = entity
            .conforms_to
            .iter()
            .map(|reference| entity_reference_label(graph, reference))
            .collect::<Vec<_>>()
            .join(", ");
        declaration.push_str(&format!(": {contract_list}"));
    }
    let declaration_width = indent.chars().count() + declaration.chars().count();
    let path_gap = path_column.saturating_sub(declaration_width).max(1);
    declaration.push_str(&format!("{}[{}]", " ".repeat(path_gap), entity.path));
    let entity_anchor = DiagramAnchor {
        target: PlanReviewTarget::Entity {
            entity_id: entity.entity_id.clone(),
        },
        json_path: entity_path.clone(),
        path: Some(entity.path.clone()),
        label: entity.name.clone(),
    };
    append_aligned_diagram_declaration(
        cell_list,
        &indent,
        &declaration,
        width,
        entity_anchor.clone(),
    );
    for (member_index, member) in entity.members.iter().enumerate() {
        append_diagram_text(
            cell_list,
            &format!("{indent}  "),
            &format!("{indent}  "),
            &format!(
                "{} {}",
                visibility_marker(member.visibility),
                member_signature(member)
            ),
            width,
            DiagramAnchor {
                target: PlanReviewTarget::EntityMember {
                    entity_id: entity.entity_id.clone(),
                    member_id: member.member_id.clone(),
                },
                json_path: format!("{entity_path}/members/{member_index}"),
                path: Some(entity.path.clone()),
                label: format!("{}::{}", entity.name, member.name),
            },
        );
    }
    for (variant_index, variant) in entity.variants.iter().enumerate() {
        let variant_path = format!("{entity_path}/variants/{variant_index}");
        append_diagram_text(
            cell_list,
            &format!("{indent}  "),
            &format!("{indent}  "),
            &variant.name,
            width,
            DiagramAnchor {
                target: PlanReviewTarget::EnumVariant {
                    entity_id: entity.entity_id.clone(),
                    variant_id: variant.variant_id.clone(),
                },
                json_path: variant_path.clone(),
                path: Some(entity.path.clone()),
                label: format!("{}::{}", entity.name, variant.name),
            },
        );
        for (field_index, field) in variant.fields.iter().enumerate() {
            append_diagram_text(
                cell_list,
                &format!("{indent}    "),
                &format!("{indent}    "),
                &format!("{}: {}", field.name, field.type_name),
                width,
                DiagramAnchor {
                    target: PlanReviewTarget::EnumVariantField {
                        entity_id: entity.entity_id.clone(),
                        variant_id: variant.variant_id.clone(),
                        field_id: field.field_id.clone(),
                    },
                    json_path: format!("{variant_path}/fields/{field_index}"),
                    path: Some(entity.path.clone()),
                    label: format!("{}::{}::{}", entity.name, variant.name, field.name),
                },
            );
        }
    }
    let child_list = document
        .entity_changes
        .iter()
        .enumerate()
        .filter(|(_, child)| {
            child.exclusive_owner_entity_id.as_deref() == Some(entity.entity_id.as_str())
                || child.exclusive_owner_entity_id.as_deref() == Some(entity.name.as_str())
        })
        .collect::<Vec<_>>();
    for (child_index, child) in child_list {
        append_entity_cells(
            cell_list,
            document,
            graph,
            child_index,
            child,
            depth.saturating_add(1),
            width,
            path_column,
        );
    }
}

fn append_aligned_diagram_declaration(
    cell_list: &mut Vec<DiagramCell>,
    indent: &str,
    declaration: &str,
    width: usize,
    anchor: DiagramAnchor,
) {
    let text = format!("{indent}{declaration}");
    if text.chars().count() <= width {
        cell_list.push(DiagramCell { text, anchor });
        return;
    }
    append_diagram_text(cell_list, indent, indent, declaration, width, anchor);
}

fn append_diagram_text(
    cell_list: &mut Vec<DiagramCell>,
    first_prefix: &str,
    continuation_prefix: &str,
    content: &str,
    width: usize,
    anchor: DiagramAnchor,
) {
    cell_list.extend(
        wrap_prefixed(content, first_prefix, continuation_prefix, width)
            .into_iter()
            .map(|text| DiagramCell {
                text,
                anchor: anchor.clone(),
            }),
    );
}

fn object_model_anchor() -> DiagramAnchor {
    DiagramAnchor {
        target: PlanReviewTarget::Section {
            section: PlanSection::ObjectModel,
        },
        json_path: "/entity_changes".into(),
        path: None,
        label: "Object model".into(),
    }
}

fn pad_diagram_column(value: &str, width: usize) -> String {
    let padding = width.saturating_sub(value.chars().count());
    format!("{value}{}", " ".repeat(padding))
}

fn is_contract_kind(kind: EntityKind) -> bool {
    matches!(
        kind,
        EntityKind::Trait | EntityKind::Interface | EntityKind::AbstractClass
    )
}

fn declaration_marker(action: ChangeAction) -> &'static str {
    match action {
        ChangeAction::Add | ChangeAction::Modify => "*",
        ChangeAction::Remove => "~",
    }
}

fn render_dependencies(renderer: &mut PlanRenderer, document: &PlanDocument) {
    renderer.section("# Dependencies", PlanSection::Dependencies, "/dependencies");
    renderer.blank();
    if document.dependencies.is_empty() {
        renderer.section("- <none>", PlanSection::Dependencies, "/dependencies");
        renderer.blank();
        return;
    }
    let mut manifest_list = Vec::new();
    for dependency in &document.dependencies {
        if !manifest_list.contains(&dependency.manifest.as_str()) {
            manifest_list.push(dependency.manifest.as_str());
        }
    }
    for (manifest_index, manifest) in manifest_list.iter().enumerate() {
        let dependency_list = document
            .dependencies
            .iter()
            .enumerate()
            .filter(|(_, dependency)| dependency.manifest == *manifest)
            .collect::<Vec<_>>();
        let first_dependency_index = dependency_list[0].0;
        renderer.push(
            format!("file {manifest}"),
            PlanReviewTarget::DependencyManifest {
                manifest: (*manifest).to_owned(),
            },
            format!("/dependencies/{first_dependency_index}/manifest"),
            Some(manifest),
            (*manifest).to_owned(),
        );
        for (dependency_position, (dependency_index, dependency)) in
            dependency_list.iter().enumerate()
        {
            let dependency_path = format!("/dependencies/{dependency_index}");
            let target = PlanReviewTarget::Dependency {
                dependency_id: dependency.dependency_id.clone(),
            };
            let is_last = dependency_position + 1 == dependency_list.len();
            renderer.push_wrapped(
                if is_last { "└─ " } else { "├─ " },
                if is_last { "   " } else { "│  " },
                &format!(
                    "{} {} ({}, {}) - {}",
                    action_label(dependency.action),
                    dependency.name,
                    dependency.version,
                    dependency.license.as_deref().unwrap_or("Unverified"),
                    dependency.justification
                ),
                target,
                dependency_path,
                Some(&dependency.manifest),
                dependency.name.clone(),
            );
        }
        if manifest_index + 1 < manifest_list.len() {
            renderer.blank();
        }
    }
    renderer.blank();
}

fn render_tasks(renderer: &mut PlanRenderer, document: &PlanDocument, graph: &PlanGraph<'_>) {
    renderer.section("# Tasks", PlanSection::Tasks, "/tasks");
    renderer.blank();
    for (task_index, task) in document.tasks.iter().enumerate() {
        let task_path = format!("/tasks/{task_index}");
        renderer.push_wrapped(
            &format!("{}. ", task_index + 1),
            "   ",
            &format!("**{}** {}", task.title, task.description),
            PlanReviewTarget::Task {
                task_id: task.task_id.clone(),
            },
            &task_path,
            None,
            format!("Task: {}", task.title),
        );
        renderer.blank();
        for (file_index, file) in task.files.iter().enumerate() {
            let file_path = format!("{task_path}/files/{file_index}");
            renderer.push(
                format!("   file {}", file.path),
                PlanReviewTarget::File {
                    task_id: task.task_id.clone(),
                    path: file.path.clone(),
                },
                &file_path,
                Some(&file.path),
                file.path.clone(),
            );
            for (subtask_index, subtask) in file.subtasks.iter().enumerate() {
                let subtask_path = format!("{file_path}/subtasks/{subtask_index}");
                let subtask_is_last = subtask_index + 1 == file.subtasks.len();
                let subtask_prefix = if subtask_is_last {
                    "   └─ "
                } else {
                    "   ├─ "
                };
                let subtask_continuation = if subtask_is_last {
                    "      "
                } else {
                    "   │  "
                };
                match subtask {
                    PlanSubtask::Work(subtask) => {
                        renderer.push_wrapped(
                            subtask_prefix,
                            subtask_continuation,
                            &format!("{} {}", subtask.action.label(), subtask.description),
                            PlanReviewTarget::Subtask {
                                task_id: task.task_id.clone(),
                                path: file.path.clone(),
                                subtask_id: subtask.subtask_id.clone(),
                            },
                            &subtask_path,
                            Some(&file.path),
                            format!("{}: {}", file.path, subtask.description),
                        );
                        for (entity_index, entity_id) in subtask.entity_ids.iter().enumerate() {
                            let Some(entity) = graph.entity(entity_id) else {
                                continue;
                            };
                            let entity_is_last = entity_index + 1 == subtask.entity_ids.len();
                            let entity_prefix = format!(
                                "{subtask_continuation}{}",
                                if entity_is_last { "└─ " } else { "├─ " }
                            );
                            let entity_continuation = format!(
                                "{subtask_continuation}{}",
                                if entity_is_last { "   " } else { "│  " }
                            );
                            renderer.push_wrapped(
                                &entity_prefix,
                                &entity_continuation,
                                &format!(
                                    "{} {} `{}` — {}",
                                    action_label(entity.action),
                                    entity_kind_label(entity.kind),
                                    entity.name,
                                    entity.description
                                ),
                                PlanReviewTarget::Entity {
                                    entity_id: entity.entity_id.clone(),
                                },
                                entity_json_path(document, &entity.entity_id),
                                Some(&entity.path),
                                format!("{} {}", action_label(entity.action), entity.name),
                            );
                        }
                    }
                    PlanSubtask::Test(test) => {
                        renderer.push_wrapped(
                            subtask_prefix,
                            subtask_continuation,
                            &format!(
                                "{} {} test `{}` — {}",
                                action_label(test.action),
                                test_category_label(test.category),
                                test.name,
                                test.behavior
                            ),
                            PlanReviewTarget::Test {
                                subtask_id: test.subtask_id.clone(),
                                category: test.category,
                            },
                            &subtask_path,
                            Some(&file.path),
                            test.name.clone(),
                        );
                    }
                }
            }
            if file_index + 1 < task.files.len() {
                renderer.blank();
            }
        }
        renderer.blank();
    }
}

fn render_tests(renderer: &mut PlanRenderer, document: &PlanDocument) {
    renderer.section("# Tests", PlanSection::Tests, "/tasks");
    renderer.blank();
    let mut file_group_list = Vec::new();
    for (task_index, task) in document.tasks.iter().enumerate() {
        for (file_index, file) in task.files.iter().enumerate() {
            let test_list = file
                .subtasks
                .iter()
                .enumerate()
                .filter_map(|(subtask_index, subtask)| {
                    subtask.test().map(|test| (subtask_index, test))
                })
                .collect::<Vec<_>>();
            if !test_list.is_empty() {
                file_group_list.push((task_index, file_index, task, file, test_list));
            }
        }
    }
    if file_group_list.is_empty() {
        renderer.section("<none>", PlanSection::Tests, "/tasks");
        renderer.blank();
        return;
    }
    let file_group_count = file_group_list.len();
    for (group_index, (task_index, file_index, task, file, test_list)) in
        file_group_list.into_iter().enumerate()
    {
        renderer.push(
            format!("file {}", file.path),
            PlanReviewTarget::File {
                task_id: task.task_id.clone(),
                path: file.path.clone(),
            },
            format!("/tasks/{task_index}/files/{file_index}"),
            Some(&file.path),
            file.path.clone(),
        );
        let test_count = test_list.len();
        for (test_index, (subtask_index, test)) in test_list.into_iter().enumerate() {
            let branch = if test_index + 1 == test_count {
                "└─"
            } else {
                "├─"
            };
            renderer.push(
                format!(
                    "{branch} {}Test {}",
                    test_category_label(test.category),
                    test.name
                ),
                PlanReviewTarget::Test {
                    subtask_id: test.subtask_id.clone(),
                    category: test.category,
                },
                format!("/tasks/{task_index}/files/{file_index}/subtasks/{subtask_index}"),
                Some(&file.path),
                test.name.clone(),
            );
        }
        if group_index + 1 < file_group_count {
            renderer.blank();
        }
    }
    renderer.blank();
}

fn test_category_label(category: TestCategory) -> &'static str {
    match category {
        TestCategory::Unit => "Unit",
        TestCategory::Integration => "Integration",
    }
}

fn render_assumptions(renderer: &mut PlanRenderer, document: &PlanDocument) {
    renderer.section("# Assumptions", PlanSection::Assumptions, "/assumptions");
    renderer.blank();
    if document.assumptions.is_empty() {
        renderer.section("- <none>", PlanSection::Assumptions, "/assumptions");
        return;
    }
    for (assumption_index, assumption) in document.assumptions.iter().enumerate() {
        renderer.push(
            format!("- {assumption}"),
            PlanReviewTarget::Assumption { assumption_index },
            format!("/assumptions/{assumption_index}"),
            None,
            format!("Assumption: {assumption}"),
        );
    }
}

fn section_label(section: PlanSection) -> &'static str {
    match section {
        PlanSection::Title => "Plan title",
        PlanSection::Overview => "Overview",
        PlanSection::Usage => "Usage",
        PlanSection::Diagrams => "Diagrams",
        PlanSection::ObjectModel => "Object model",
        PlanSection::Dependencies => "Dependencies",
        PlanSection::Tasks => "Tasks",
        PlanSection::Tests => "Tests",
        PlanSection::Assumptions => "Assumptions",
    }
}

fn entity_kind_label(kind: EntityKind) -> &'static str {
    match kind {
        EntityKind::Class => "class",
        EntityKind::AbstractClass => "abstract class",
        EntityKind::Struct => "struct",
        EntityKind::Enum => "enum",
        EntityKind::Trait => "trait",
        EntityKind::Interface => "interface",
        EntityKind::App => "app",
        EntityKind::Config => "config",
        EntityKind::Function => "fn",
        EntityKind::Method => "method",
        EntityKind::Constant => "constant",
        EntityKind::Field => "field",
        EntityKind::Resource => "Resource",
        EntityKind::Cache => "Cache",
        EntityKind::Adapter => "Adapter",
    }
}

fn visibility_marker(visibility: Option<Visibility>) -> &'static str {
    match visibility {
        Some(Visibility::Public) => "+",
        _ => "-",
    }
}

fn member_signature(member: &ProgramEntityMemberChange) -> String {
    if matches!(member.kind, MemberKind::Method | MemberKind::Function) {
        let parameter_list = member
            .parameters
            .iter()
            .map(|parameter| format!("{}: {}", parameter.name, parameter.type_name))
            .collect::<Vec<_>>()
            .join(", ");
        let return_type = member
            .return_type
            .as_deref()
            .map(|value| format!(": {value}"))
            .unwrap_or_default();
        format!("{}({parameter_list}){return_type}", member.name)
    } else {
        member
            .type_name
            .as_deref()
            .map(|value| format!("{}: {value}", member.name))
            .unwrap_or_else(|| member.name.clone())
    }
}

fn action_label(action: ChangeAction) -> &'static str {
    match action {
        ChangeAction::Add => "Add",
        ChangeAction::Modify => "Modify",
        ChangeAction::Remove => "Remove",
    }
}

fn entity_reference_label(graph: &PlanGraph<'_>, target: &EntityReference) -> String {
    match target {
        EntityReference::PlannedEntity { entity } => graph
            .entity_label(entity)
            .unwrap_or_else(|| "<missing entity>".into()),
        EntityReference::ExternalEntity { entity } => entity.clone(),
    }
}

fn entity_json_path(document: &PlanDocument, entity_id: &str) -> String {
    document
        .entity_changes
        .iter()
        .position(|entity| entity.entity_id == entity_id)
        .map(|index| format!("/entity_changes/{index}"))
        .unwrap_or_else(|| "/entity_changes".into())
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::plan::{attach_integration_test_fixture, attach_test_fixture, test_fixture};

    #[test]
    fn renders_single_concrete_column_below_a_markdown_heading() {
        let mut document = test_fixture("plan", "Overview");
        document.entity_changes[0].name = "GeoParquetInspector".into();
        document.entity_changes[0].path = "hello/src/inspection.rs".into();
        document.entity_changes[0].members = vec![ProgramEntityMemberChange {
            member_id: "inspect".into(),
            action: ChangeAction::Add,
            kind: MemberKind::Method,
            name: "inspect".into(),
            description: "Inspect one local file.".into(),
            visibility: Some(Visibility::Public),
            type_name: None,
            parameters: vec![FunctionParameter {
                name: "path".into(),
                type_name: "&Path".into(),
            }],
            return_type: Some("Result<InspectionReport, InspectionError>".into()),
        }];
        document.entity_changes.push(ProgramEntityChange {
            entity_id: "inspection_report".into(),
            action: ChangeAction::Add,
            kind: EntityKind::Struct,
            name: "InspectionReport".into(),
            description: "Owns one inspection result.".into(),
            path: "hello/src/inspection.rs".into(),
            members: Vec::new(),
            variants: Vec::new(),
            extends: None,
            conforms_to: Vec::new(),
            exclusive_owner_entity_id: None,
        });

        let graph = PlanGraph::new(&document);
        let mut renderer = PlanRenderer::new();
        render_object_model(&mut renderer, &document, &graph);
        let rendered = renderer.finish();
        let inspector_line = rendered
            .markdown
            .lines()
            .find(|line| line.starts_with("*struct GeoParquetInspector"))
            .expect("inspector declaration");
        let report_line = rendered
            .markdown
            .lines()
            .find(|line| line.starts_with("*struct InspectionReport"))
            .expect("report declaration");

        assert_eq!(inspector_line.find('['), Some(DIAGRAM_PATH_COLUMN_MAX));
        assert_eq!(report_line.find('['), Some(DIAGRAM_PATH_COLUMN_MAX));
        assert!(
            rendered
                .markdown
                .contains("  + inspect(path: &Path): Result<InspectionReport, InspectionError>\n")
        );
        assert!(!rendered.markdown.contains("\n  InspectionError>"));
        assert!(
            !rendered
                .markdown
                .contains("Contracts                                 Concrete")
        );
        assert!(
            !rendered
                .markdown
                .contains("<none>                                    ")
        );
        assert!(!rendered.markdown.contains("### Contracts"));
        assert_eq!(rendered.markdown.matches("## Concrete").count(), 1);
    }

    #[test]
    fn keeps_the_harness_owned_prompt_canonical_without_rendering_it() {
        let mut document = test_fixture("plan", "Overview");
        document.title = "Create datafusion CLI tool".into();
        document.prompt = "Build a GeoParquet inspector.\nKeep output deterministic.".into();

        let rendered = render_plan(&document).unwrap();
        let canonical = serde_json::to_value(&document).unwrap();

        assert!(rendered.markdown.starts_with(
            "# Create datafusion CLI tool\n\nOverview\n\n# Usage"
        ));
        assert!(!rendered.markdown.contains("# Overview"));
        assert_eq!(
            canonical.pointer("/prompt"),
            Some(&serde_json::json!(
                "Build a GeoParquet inspector.\nKeep output deterministic."
            ))
        );
        assert!(!rendered.markdown.contains("# Prompt"));
        assert!(!rendered.markdown.contains("Build a GeoParquet inspector."));
        assert!(
            rendered
                .navigation
                .anchor
                .iter()
                .all(|anchor| anchor.json_path != "/prompt")
        );
        assert!(!rendered.markdown.contains(
            "The object model establishes ownership before each affected flow crosses those boundaries."
        ));
        assert!(!rendered.markdown.contains("## Object model and ownership"));
        assert!(!rendered.markdown.contains(
            "Concrete types own the state and implementation without introducing a shared contract."
        ));
        assert!(!rendered.markdown.contains(
            "Contracts define shared behavior while concrete types own the state and implementation."
        ));
        let usage_index = rendered.markdown.find("# Usage").expect("usage section");
        let assumptions_index = rendered
            .markdown
            .find("# Assumptions")
            .expect("assumptions section");
        let diagrams_index = rendered
            .markdown
            .find("# Diagrams")
            .expect("diagrams section");
        assert!(usage_index < assumptions_index);
        assert!(assumptions_index < diagrams_index);
    }

    #[test]
    fn renders_expected_usage_output_under_a_level_two_heading() {
        let mut document = test_fixture("plan", "Overview");
        document.usage = Some(PlanUsage {
            command: "inspect sample.geoparquet".into(),
            expected_result: "row_count: 42\nexit status: 0".into(),
        });

        let rendered = render_plan(&document).unwrap();

        assert!(rendered.markdown.contains(
            "# Usage\n\n```text\ninspect sample.geoparquet\n```\n\n## Expected Result\n\n```text\nrow_count: 42\nexit status: 0\n```"
        ));
        assert!(!rendered.markdown.contains("Expected result:"));
    }

    #[test]
    fn renders_contracts_conformance_and_exclusive_children_in_fixed_columns() {
        let mut document = test_fixture("plan", "Overview");
        document.entity_changes[0] = ProgramEntityChange {
            entity_id: "plan_document".into(),
            action: ChangeAction::Modify,
            kind: EntityKind::Trait,
            name: "Backend".into(),
            description: "Defines execution behavior.".into(),
            path: "src/backend.rs".into(),
            members: vec![ProgramEntityMemberChange {
                member_id: "backend_run".into(),
                action: ChangeAction::Modify,
                kind: MemberKind::Method,
                name: "run".into(),
                description: "Run one request.".into(),
                visibility: Some(Visibility::Public),
                type_name: None,
                parameters: Vec::new(),
                return_type: Some("Result".into()),
            }],
            variants: Vec::new(),
            extends: None,
            conforms_to: Vec::new(),
            exclusive_owner_entity_id: None,
        };
        document.entity_changes.push(ProgramEntityChange {
            entity_id: "codex_backend".into(),
            action: ChangeAction::Add,
            kind: EntityKind::Struct,
            name: "CodexBackend".into(),
            description: "Runs requests through Codex.".into(),
            path: "src/codex.rs".into(),
            members: vec![ProgramEntityMemberChange {
                member_id: "codex_backend_client".into(),
                action: ChangeAction::Add,
                kind: MemberKind::Field,
                name: "client".into(),
                description: "Owns the provider client.".into(),
                visibility: Some(Visibility::Private),
                type_name: Some("Client".into()),
                parameters: Vec::new(),
                return_type: None,
            }],
            variants: Vec::new(),
            extends: None,
            conforms_to: vec![EntityReference::PlannedEntity {
                entity: "plan_document".into(),
            }],
            exclusive_owner_entity_id: None,
        });
        document.entity_changes.push(ProgramEntityChange {
            entity_id: "request_cache".into(),
            action: ChangeAction::Remove,
            kind: EntityKind::Cache,
            name: "RequestCache".into(),
            description: "Stores request state.".into(),
            path: "src/codex.rs".into(),
            members: Vec::new(),
            variants: Vec::new(),
            extends: None,
            conforms_to: Vec::new(),
            exclusive_owner_entity_id: Some("codex_backend".into()),
        });

        let graph = PlanGraph::new(&document);
        let mut renderer = PlanRenderer::new();
        render_object_model(&mut renderer, &document, &graph);
        let rendered = renderer.finish();

        let declaration_line = rendered
            .markdown
            .lines()
            .find(|line| line.starts_with("*trait Backend"))
            .expect("paired declarations");
        let backend_path_column = declaration_line.find("[src/backend.rs]").unwrap();
        let codex_path_column = declaration_line.find("[src/codex.rs]").unwrap();
        assert_eq!(backend_path_column, 26);
        assert_eq!(
            codex_path_column,
            DIAGRAM_CONCRETE_COLUMN + DIAGRAM_PATH_COLUMN_MAX
        );
        assert!(
            rendered
                .markdown
                .contains("  + run(): Result                           - client: Client")
        );
        let child_line = rendered
            .markdown
            .lines()
            .find(|line| line.contains("~Cache RequestCache"))
            .expect("exclusive child declaration");
        assert_eq!(
            child_line.find("[src/codex.rs]"),
            Some(DIAGRAM_CONCRETE_COLUMN + DIAGRAM_PATH_COLUMN_MAX)
        );
        assert!(!rendered.markdown.contains("\n  [src/backend.rs]"));
        assert!(!rendered.markdown.contains("\n  [src/codex.rs]"));
    }

    #[test]
    fn anchors_every_nonblank_line_to_a_json_path() {
        let rendered = render_plan(&test_fixture("plan", "Overview")).unwrap();
        let nonblank_line_count = rendered
            .markdown
            .lines()
            .filter(|line| !line.is_empty())
            .count();

        assert_eq!(rendered.navigation.anchor.len(), nonblank_line_count);
        assert!(
            rendered
                .navigation
                .anchor
                .iter()
                .all(|anchor| anchor.json_path.starts_with('/'))
        );
    }

    #[test]
    fn separates_task_heading_and_aligns_wrapped_tree_rows() {
        let mut document = test_fixture("plan", "Overview");
        let PlanSubtask::Work(subtask) = &mut document.tasks[0].files[0].subtasks[0] else {
            panic!("expected work subtask");
        };
        subtask.description = "the canonical plan owner with enough supporting detail to wrap onto a continuation row and preserve subtask-alignment-sentinel.".into();
        document.entity_changes[0].description = "Own canonical planning data with enough supporting detail to wrap onto a continuation row and preserve entity-alignment-sentinel.".into();
        attach_integration_test_fixture(&mut document);

        let rendered = render_plan(&document).unwrap();
        let task_markdown = rendered
            .markdown
            .split("# Tasks\n\n")
            .nth(1)
            .expect("tasks section")
            .split("# Tests")
            .next()
            .expect("task body");

        assert!(task_markdown.contains("Give planning one owner.\n\n   file src/plan.rs"));
        assert!(task_markdown.lines().any(
            |line| line.starts_with("      ") && line.contains("subtask-alignment-sentinel")
        ));
        assert!(
            task_markdown
                .lines()
                .any(|line| line.starts_with("         ")
                    && line.contains("entity-alignment-sentinel"))
        );
        let task_line_list = task_markdown.lines().collect::<Vec<_>>();
        let second_file_index = task_line_list
            .iter()
            .position(|line| *line == "   file tests/plan_submission.rs")
            .expect("second indented file group");
        assert_eq!(task_line_list[second_file_index - 1], "");
        assert!(
            task_markdown
                .lines()
                .all(|line| line.chars().count() <= PLAN_LINE_WIDTH)
        );
    }

    #[test]
    fn closes_final_task_tree_siblings() {
        let mut document = test_fixture("plan", "Overview");
        let mut second_entity = document.entity_changes[0].clone();
        second_entity.entity_id = "plan_renderer".into();
        second_entity.name = "PlanRenderer".into();
        second_entity.description = "Render canonical planning data.".into();
        document.entity_changes.push(second_entity);
        let PlanSubtask::Work(subtask) = &mut document.tasks[0].files[0].subtasks[0] else {
            panic!("expected work subtask");
        };
        subtask.entity_ids.push("plan_renderer".into());

        let rendered = render_plan(&document).unwrap();
        let task_markdown = rendered
            .markdown
            .split("# Tasks\n\n")
            .nth(1)
            .expect("tasks section")
            .split("# Tests")
            .next()
            .expect("task body");

        assert!(task_markdown.contains("   └─ Create Keep state durable."));
        assert!(task_markdown.contains("      ├─ Add struct `PlanDocument`"));
        assert!(task_markdown.contains("      └─ Add struct `PlanRenderer`"));
        assert!(!task_markdown.contains("      │  Add struct `PlanRenderer`"));
    }

    #[test]
    fn renders_flow_diagrams_inside_text_fences() {
        let rendered = render_plan(&test_fixture("plan", "Overview")).unwrap();

        assert!(
            rendered
                .markdown
                .contains(
                    "## Code flow: Execution\n\nStart from the accepted plan and produce executable \
                     work. Keep planning ownership distinct from execution state.\n\n```text",
                )
        );
    }

    #[test]
    fn renders_flow_projection_as_a_vertical_graph_with_aligned_owners() {
        let mut document = test_fixture("plan", "Overview");
        document.flows[0].title = "DataFusion table inspection".into();
        document.flows[0].steps = vec![
            PlanFlowStep {
                step_id: "register".into(),
                action: "Register local Parquet".into(),
                target: EntityReference::ExternalEntity {
                    entity: "datafusion::SessionContext".into(),
                },
                operations: Vec::new(),
                value_to_next: Some(PlanFlowValue::Text {
                    text: "registered table".into(),
                }),
            },
            PlanFlowStep {
                step_id: "observe".into(),
                action: "Read table observations".into(),
                target: EntityReference::PlannedEntity {
                    entity: "plan_document".into(),
                },
                operations: vec![
                    PlanFlowOperation {
                        operation_id: "schema".into(),
                        action: "schema()".into(),
                        target: EntityReference::ExternalEntity {
                            entity: "datafusion::SessionContext".into(),
                        },
                        result: Some("schema text".into()),
                    },
                    PlanFlowOperation {
                        operation_id: "count".into(),
                        action: "count(*)".into(),
                        target: EntityReference::ExternalEntity {
                            entity: "datafusion::SessionContext".into(),
                        },
                        result: Some("u64".into()),
                    },
                ],
                value_to_next: None,
            },
            PlanFlowStep {
                step_id: "assemble".into(),
                action: "Assemble inspection summary".into(),
                target: EntityReference::PlannedEntity {
                    entity: "plan_document".into(),
                },
                operations: Vec::new(),
                value_to_next: Some(PlanFlowValue::Type {
                    name: "InspectionSummary".into(),
                }),
            },
            PlanFlowStep {
                step_id: "render".into(),
                action: "Render inspection output".into(),
                target: EntityReference::PlannedEntity {
                    entity: "plan_document".into(),
                },
                operations: Vec::new(),
                value_to_next: Some(PlanFlowValue::Text {
                    text: "stdout".into(),
                }),
            },
        ];

        let rendered = render_plan(&document).unwrap();
        let diagram = rendered
            .markdown
            .split("## Code flow: DataFusion table inspection")
            .nth(1)
            .expect("flow section")
            .split("```text\n")
            .nth(1)
            .expect("flow fence")
            .split("\n```")
            .next()
            .expect("flow diagram");
        let diagram_line_list = diagram.lines().collect::<Vec<_>>();

        let expected_content_list = [
            "Register local Parquet",
            "    ├─ registered table",
            "    ▼",
            "*Read table observations",
            "    ├─ schema(): schema text",
            "    ├─ count(*): u64",
            "    ▼",
            "*Assemble inspection summary",
            "    ├─ InspectionSummary",
            "    ▼",
            "*Render inspection output",
            "    └─ stdout",
        ];
        assert_eq!(diagram_line_list.len(), expected_content_list.len());
        let mut owner_column_list = Vec::new();
        for (line, expected_content) in diagram_line_list.iter().zip(expected_content_list) {
            assert!(line.starts_with(expected_content));
            if let Some(owner_byte_index) = line.find('[') {
                owner_column_list.push(line[..owner_byte_index].chars().count());
            }
        }
        assert_eq!(owner_column_list, [32, 32, 32, 32, 32, 32]);
        assert!(diagram_line_list[0].ends_with("[datafusion::SessionContext]"));
        assert!(diagram_line_list[3].ends_with("[src/plan.rs]"));
        assert!(diagram_line_list[4].ends_with("[datafusion::SessionContext]"));
        assert!(diagram_line_list[5].ends_with("[datafusion::SessionContext]"));
        assert!(diagram_line_list[7].ends_with("[src/plan.rs]"));
        assert!(diagram_line_list[10].ends_with("[src/plan.rs]"));
        assert!(
            diagram_line_list
                .iter()
                .all(|line| line.chars().count() <= PLAN_LINE_WIDTH)
        );
        for action in [
            "Register local Parquet",
            "Read table observations",
            "schema()",
            "count(*)",
            "Assemble inspection summary",
            "Render inspection output",
        ] {
            assert_eq!(
                diagram.matches(action).count(),
                1,
                "{action} must render once"
            );
        }
        assert!(!diagram.contains("DataFusion table inspection"));

        let schema_operation_anchor = rendered
            .navigation
            .anchor
            .iter()
            .find(|anchor| {
                matches!(
                    &anchor.target,
                    PlanReviewTarget::FlowOperation {
                        operation_id,
                        ..
                    } if operation_id == "schema"
                )
            })
            .expect("schema operation anchor");
        assert_eq!(
            schema_operation_anchor.json_path,
            "/flows/0/steps/1/operations/0"
        );
        let typed_value_anchor = rendered
            .navigation
            .anchor
            .iter()
            .find(|anchor| anchor.json_path == "/flows/0/steps/2/value_to_next")
            .expect("typed flow value anchor");
        assert!(matches!(
            typed_value_anchor.target,
            PlanReviewTarget::FlowValue {
                value_kind: PlanFlowValueKind::Type,
                ..
            }
        ));
        let text_value_anchor = rendered
            .navigation
            .anchor
            .iter()
            .find(|anchor| anchor.json_path == "/flows/0/steps/0/value_to_next")
            .expect("text flow value anchor");
        assert!(matches!(
            text_value_anchor.target,
            PlanReviewTarget::FlowValue {
                value_kind: PlanFlowValueKind::Text,
                ..
            }
        ));
    }

    #[test]
    fn renders_even_a_short_flow_vertically() {
        let rendered = render_plan(&test_fixture("plan", "Overview")).unwrap();
        let diagram = rendered
            .markdown
            .split("## Code flow: Execution")
            .nth(1)
            .expect("flow section")
            .split("```text\n")
            .nth(1)
            .expect("flow fence")
            .split("\n```")
            .next()
            .expect("flow diagram");

        assert_eq!(diagram, "*Read plan    [src/plan.rs]");
    }

    #[test]
    fn moves_an_owner_below_content_when_the_right_column_cannot_fit() {
        let mut document = test_fixture("plan", "Overview");
        let action = "Read a deliberately wide observation that consumes nearly the complete diagram line width";
        document.flows[0].steps = vec![PlanFlowStep {
            step_id: "wide_observation".into(),
            action: action.into(),
            target: EntityReference::ExternalEntity {
                entity: "LongExternalOwner".into(),
            },
            operations: Vec::new(),
            value_to_next: None,
        }];

        let rendered = render_plan(&document).unwrap();
        let diagram = rendered
            .markdown
            .split("## Code flow: Execution")
            .nth(1)
            .expect("flow section")
            .split("```text\n")
            .nth(1)
            .expect("flow fence")
            .split("\n```")
            .next()
            .expect("flow diagram");

        assert_eq!(diagram, format!("{action}\n    [LongExternalOwner]"));
        assert!(
            diagram
                .lines()
                .all(|line| line.chars().count() <= PLAN_LINE_WIDTH)
        );
    }

    #[test]
    fn renders_flow_steps_in_array_position() {
        let mut document = test_fixture("plan", "Overview");
        let first_step = document.flows[0].steps[0].clone();
        document.flows[0].steps = vec![
            PlanFlowStep {
                step_id: "first".into(),
                action: "First action".into(),
                ..first_step.clone()
            },
            PlanFlowStep {
                step_id: "second".into(),
                action: "Second action".into(),
                ..first_step
            },
        ];

        let rendered = render_plan(&document).unwrap();
        let first_position = rendered.markdown.find("First action").unwrap();
        let second_position = rendered.markdown.find("Second action").unwrap();

        assert!(first_position < second_position);
    }

    #[test]
    fn renders_auditable_dependencies_before_tasks() {
        let mut document = test_fixture("plan", "Overview");
        document.dependencies.push(PlanDependencyChange {
            dependency_id: "dependency_tokio".into(),
            action: ChangeAction::Add,
            name: "tokio".into(),
            version: "1".into(),
            manifest: "Cargo.toml".into(),
            license: Some("MIT".into()),
            justification: "Run asynchronous work. The standard library provides no runtime.".into(),
        });
        document.dependencies.push(PlanDependencyChange {
            dependency_id: "dependency_serde".into(),
            action: ChangeAction::Modify,
            name: "serde".into(),
            version: "1.0".into(),
            manifest: "Cargo.toml".into(),
            license: Some("MIT OR Apache-2.0".into()),
            justification: "Decode canonical plan state. Manual decoding would duplicate schema rules.".into(),
        });

        let rendered = render_plan(&document).unwrap();
        let dependency_position = rendered.markdown.find("# Dependencies").unwrap();
        let task_position = rendered.markdown.find("# Tasks").unwrap();

        assert!(dependency_position < task_position);
        assert!(rendered.markdown.contains(
            "file Cargo.toml\n├─ Add tokio (1, MIT) - Run asynchronous work. The standard library provides no runtime.\n└─ Modify serde (1.0, MIT OR Apache-2.0) - Decode canonical plan state. Manual decoding would\n   duplicate schema rules."
        ));
        assert!(!rendered.markdown.contains("## Add"));
        assert!(rendered.navigation.anchor.iter().any(|anchor| {
            matches!(
                &anchor.target,
                PlanReviewTarget::DependencyManifest { manifest }
                    if manifest == "Cargo.toml"
            )
        }));
    }

    #[test]
    fn renders_unit_and_integration_test_subtasks_in_tasks_and_the_test_plan() {
        let mut document = test_fixture("plan", "Overview");
        attach_test_fixture(&mut document);
        attach_integration_test_fixture(&mut document);

        let rendered = render_plan(&document).unwrap();

        assert!(
            rendered
                .markdown
                .contains("   └─ Add Unit test `validates_plans` — Reject malformed plans.")
        );
        assert!(rendered.markdown.contains(
            "   └─ Add Integration test `submits_complete_plan` — Submit one complete plan through the real\n      broker boundary."
        ));
        let test_plan_markdown = rendered
            .markdown
            .split_once("# Tests")
            .expect("test plan section")
            .1;
        assert!(test_plan_markdown.contains(
            "file src/plan.rs\n└─ UnitTest validates_plans\n\nfile tests/plan_submission.rs\n└─ IntegrationTest submits_complete_plan"
        ));
        assert!(!test_plan_markdown.contains("## Unit tests"));
        assert!(!test_plan_markdown.contains("## Integration tests"));
        assert!(!test_plan_markdown.contains("Reject malformed plans."));
        assert!(
            rendered
                .navigation
                .anchor
                .iter()
                .filter(|anchor| matches!(anchor.target, PlanReviewTarget::Test { .. }))
                .all(|anchor| anchor.json_path.starts_with("/tasks/"))
        );
    }

    #[test]
    fn groups_test_plan_entries_by_file_with_closed_unicode_branches() {
        let mut document = test_fixture("plan", "Overview");
        attach_test_fixture(&mut document);
        let file = document.tasks[0]
            .files
            .iter_mut()
            .find(|file| file.path == "src/plan.rs")
            .expect("test file");
        let mut second_test = file.subtasks.last().expect("test subtask").clone();
        let PlanSubtask::Test(test) = &mut second_test else {
            panic!("expected test subtask");
        };
        test.subtask_id = "test_rejects_missing_input".into();
        test.name = "rejects_missing_input".into();
        test.behavior = "Reject missing input.".into();
        file.subtasks.push(second_test);

        let rendered = render_plan(&document).unwrap();
        let test_plan_markdown = rendered
            .markdown
            .split_once("# Tests")
            .expect("test plan section")
            .1;

        assert!(test_plan_markdown.contains(
            "file src/plan.rs\n├─ UnitTest validates_plans\n└─ UnitTest rejects_missing_input"
        ));
    }

    #[test]
    fn renders_enum_variants_and_fields_without_member_visibility() {
        let mut document = test_fixture("plan", "Overview");
        document.entity_changes[0].kind = EntityKind::Enum;
        document.entity_changes[0].variants.push(EnumVariantChange {
            variant_id: "plan_document_variant_ready".into(),
            action: ChangeAction::Add,
            name: "Ready".into(),
            description: "Carries the ready version.".into(),
            fields: vec![EnumVariantFieldChange {
                field_id: "plan_document_variant_ready_field_version".into(),
                action: ChangeAction::Add,
                name: "version".into(),
                type_name: "u64".into(),
            }],
        });

        let rendered = render_plan(&document).unwrap();

        let enum_line = rendered
            .markdown
            .lines()
            .find(|line| line.starts_with("*enum PlanDocument"))
            .expect("enum declaration");
        assert_eq!(
            enum_line.find("[src/plan.rs]"),
            Some(DIAGRAM_PATH_COLUMN_MAX)
        );
        assert!(rendered.markdown.contains("\n  Ready\n    version: u64"));
        assert!(!rendered.markdown.contains("**enum PlanDocument**"));
        assert!(!rendered.markdown.contains("(new)"));
        assert!(
            rendered
                .navigation
                .anchor
                .iter()
                .any(|anchor| matches!(anchor.target, PlanReviewTarget::EnumVariant { .. }))
        );
        assert!(
            rendered
                .navigation
                .anchor
                .iter()
                .any(|anchor| matches!(anchor.target, PlanReviewTarget::EnumVariantField { .. }))
        );
    }

    #[test]
    fn serializes_review_entities_with_an_explicit_target_type() {
        let value = serde_json::to_value(PlanReviewTarget::Entity {
            entity_id: "plan_document".into(),
        })
        .unwrap();

        assert_eq!(
            value.pointer("/target_type"),
            Some(&serde_json::json!("entity"))
        );
        assert_eq!(
            value.pointer("/entity_id"),
            Some(&serde_json::json!("plan_document"))
        );

        let operation_value = serde_json::to_value(PlanReviewTarget::FlowOperation {
            flow_id: "inspection".into(),
            step_id: "observe".into(),
            operation_id: "schema".into(),
        })
        .unwrap();
        assert_eq!(
            operation_value.pointer("/target_type"),
            Some(&serde_json::json!("flow_operation"))
        );
        assert_eq!(
            operation_value.pointer("/operation_id"),
            Some(&serde_json::json!("schema"))
        );
    }
}
