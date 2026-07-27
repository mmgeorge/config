use super::document::*;
use super::{PlanGraph, validate_plan_render};
use anyhow::Result;
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::path::Path;

/// Defines one reviewer-visible section in the rendered plan.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum PlanSection {
    Title,
    Overview,
    Usage,
    Diagrams,
    ObjectModel,
    Files,
    Dependencies,
    Tasks,
    Tests,
    Assumptions,
}

/// Identifies the ownership boundary behind one reviewer-visible entity reference.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum PlanReviewReferenceKind {
    PlannedEntity,
    WorkspaceEntity,
    ExternalEntity,
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
        reference_kind: PlanReviewReferenceKind,
        target_name: String,
        target_is_type: bool,
        workspace_path: Option<String>,
        workspace_line: Option<usize>,
    },
    FlowEdge {
        flow_id: String,
        step_id: String,
        edge_id: String,
        callable_kind: Option<PlanCallableKind>,
        callable_name: Option<String>,
        reference_kind: PlanReviewReferenceKind,
        target_name: String,
        target_is_type: bool,
        workspace_path: Option<String>,
        workspace_line: Option<usize>,
    },
    FlowEdgeResult {
        flow_id: String,
        step_id: String,
        edge_id: String,
        value_kind: PlanFlowValueKind,
    },
    FlowBranch {
        flow_id: String,
        step_id: String,
        branch_id: String,
        condition: String,
    },
    Task {
        task_id: String,
    },
    File {
        task_id: String,
        path: String,
    },
    FileDirectory {
        path: String,
    },
    FileTreeFile {
        path: String,
    },
    FileTreeEntity {
        entity_id: String,
        path: String,
    },
    FileTreeTest {
        subtask_id: String,
        path: String,
        category: TestCategory,
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
    render_plan_at(document, Path::new("."))
}

/// Render one plan against its repository root so file status reflects the worktree.
pub fn render_plan_at(document: &PlanDocument, workspace: &Path) -> Result<RenderedPlan> {
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
    render_files(&mut renderer, document, &graph, workspace);
    render_dependencies(&mut renderer, document);
    render_tasks(&mut renderer, document, &graph);
    render_tests(&mut renderer, document);
    Ok(renderer.finish())
}

struct RenderedFileEntry {
    label: String,
    path: String,
    json_path: String,
    symbol_list: Vec<RenderedFileSymbol>,
}

struct RenderedFileSymbol {
    label: String,
    target: PlanReviewTarget,
    json_path: String,
}

#[derive(Default)]
struct RenderedFileTree {
    directory: BTreeMap<String, RenderedFileTree>,
    file: BTreeMap<String, RenderedFileEntry>,
}

fn render_files(
    renderer: &mut PlanRenderer,
    document: &PlanDocument,
    graph: &PlanGraph<'_>,
    workspace: &Path,
) {
    renderer.section("# Files", PlanSection::Files, "/tasks");
    renderer.blank();

    let mut file_list = Vec::<RenderedFileEntry>::new();
    for (task_index, task) in document.tasks.iter().enumerate() {
        for (file_index, file) in task.files.iter().enumerate() {
            let path = file.change.path().to_owned();
            if !file_list.iter().any(|entry| entry.path == path) {
                let label = match &file.change {
                    PlanFileChange::Add { .. } => format!("(new) {}", file_name(&path)),
                    PlanFileChange::Modify { .. } => file_name(&path).to_owned(),
                    PlanFileChange::Remove { .. } => {
                        format!("(remove) {}", file_name(&path))
                    }
                    PlanFileChange::Rename { from, .. } => {
                        let source = if file_parent(from) == file_parent(&path) {
                            file_name(from)
                        } else {
                            from
                        };
                        format!("{source} → {}", file_name(&path))
                    }
                };
                file_list.push(RenderedFileEntry {
                    label,
                    path: path.clone(),
                    json_path: format!("/tasks/{task_index}/files/{file_index}"),
                    symbol_list: Vec::new(),
                });
            }
        }
    }
    for (dependency_index, dependency) in document.dependencies.iter().enumerate() {
        if !file_list
            .iter()
            .any(|entry| entry.path == dependency.manifest)
        {
            file_list.push(RenderedFileEntry {
                label: inferred_file_label(workspace, &dependency.manifest),
                path: dependency.manifest.clone(),
                json_path: format!("/dependencies/{dependency_index}/manifest"),
                symbol_list: Vec::new(),
            });
        }
    }
    for (entity_index, entity) in document.entity_changes.iter().enumerate() {
        if !file_list.iter().any(|entry| entry.path == entity.path) {
            file_list.push(RenderedFileEntry {
                label: inferred_file_label(workspace, &entity.path),
                path: entity.path.clone(),
                json_path: format!("/entity_changes/{entity_index}/path"),
                symbol_list: Vec::new(),
            });
        }
    }

    if file_list.is_empty() {
        renderer.section("<none>", PlanSection::Files, "/tasks");
        renderer.blank();
        return;
    }

    let entity_id_list = document
        .entity_changes
        .iter()
        .map(|entity| entity.entity_id.clone())
        .collect::<Vec<_>>();
    for entity in graph.entities_in_presentation_order(&entity_id_list) {
        let Some(entry) = file_list.iter_mut().find(|entry| entry.path == entity.path) else {
            continue;
        };
        entry.symbol_list.push(RenderedFileSymbol {
            label: file_entity_label(entity),
            target: PlanReviewTarget::FileTreeEntity {
                entity_id: entity.entity_id.clone(),
                path: entity.path.clone(),
            },
            json_path: entity_json_path(document, &entity.entity_id).to_owned(),
        });
    }
    for (task_index, task) in document.tasks.iter().enumerate() {
        for (file_index, file) in task.files.iter().enumerate() {
            let path = file.change.path();
            let Some(entry) = file_list.iter_mut().find(|entry| entry.path == path) else {
                continue;
            };
            for (subtask_index, subtask) in file.subtasks.iter().enumerate() {
                let PlanSubtask::Test(test) = subtask else {
                    continue;
                };
                entry.symbol_list.push(RenderedFileSymbol {
                    label: file_test_label(test),
                    target: PlanReviewTarget::FileTreeTest {
                        subtask_id: test.subtask_id.clone(),
                        path: path.to_owned(),
                        category: test.category,
                    },
                    json_path: format!(
                        "/tasks/{task_index}/files/{file_index}/subtasks/{subtask_index}"
                    ),
                });
            }
        }
    }

    let mut tree = RenderedFileTree::default();
    for entry in file_list {
        insert_rendered_file(&mut tree, entry);
    }
    render_file_tree(renderer, &tree, "", "", true);
    renderer.blank();
}

fn inferred_file_label(workspace: &Path, path: &str) -> String {
    if workspace.join(path).exists() {
        file_name(path).to_owned()
    } else {
        format!("(new) {}", file_name(path))
    }
}

fn file_entity_label(entity: &ProgramEntityChange) -> String {
    match entity.action {
        EntityChangeAction::Add => {
            format!("(new) {} {}", entity_kind_label(entity.kind), entity.name)
        }
        EntityChangeAction::Modify => {
            format!("{} {}", entity_kind_label(entity.kind), entity.name)
        }
        EntityChangeAction::Remove => {
            format!(
                "(remove) {} {}",
                entity_kind_label(entity.kind),
                entity.name
            )
        }
        EntityChangeAction::Rename => format!(
            "{} → {}",
            entity.renamed_from.as_deref().unwrap_or("<missing name>"),
            entity.name
        ),
    }
}

fn file_test_label(test: &PlanTestSubtask) -> String {
    let kind = format!("{}Test", test_category_label(test.category));
    match test.action {
        ChangeAction::Add => format!("(new) {kind} {}", test.name),
        ChangeAction::Modify => format!("{kind} {}", test.name),
        ChangeAction::Remove => format!("(remove) {kind} {}", test.name),
    }
}

fn normalized_path_part_list(path: &str) -> Vec<&str> {
    path.split(['/', '\\'])
        .filter(|part| !part.is_empty())
        .collect()
}

fn file_name(path: &str) -> &str {
    normalized_path_part_list(path)
        .last()
        .copied()
        .unwrap_or(path)
}

fn file_parent(path: &str) -> String {
    let mut part_list = normalized_path_part_list(path);
    part_list.pop();
    part_list.join("/")
}

fn insert_rendered_file(tree: &mut RenderedFileTree, entry: RenderedFileEntry) {
    let part_list = normalized_path_part_list(&entry.path);
    let Some((_, directory_part_list)) = part_list.split_last() else {
        return;
    };
    let mut owner = tree;
    for part in directory_part_list {
        owner = owner.directory.entry((*part).to_owned()).or_default();
    }
    owner.file.insert(file_name(&entry.path).to_owned(), entry);
}

fn render_file_tree(
    renderer: &mut PlanRenderer,
    tree: &RenderedFileTree,
    prefix: &str,
    directory_path: &str,
    omit_root_connector: bool,
) {
    let mut child_list = tree
        .directory
        .keys()
        .map(|name| (name.as_str(), true))
        .chain(tree.file.keys().map(|name| (name.as_str(), false)))
        .collect::<Vec<_>>();
    child_list.sort_by(|left, right| left.0.cmp(right.0));

    for (index, (name, is_directory)) in child_list.iter().enumerate() {
        let is_last = index + 1 == child_list.len();
        let branch = if omit_root_connector && prefix.is_empty() && child_list.len() == 1 {
            ""
        } else if is_last {
            "└─ "
        } else {
            "├─ "
        };
        let content = format!("{prefix}{branch}{name}");
        if *is_directory {
            let path = if directory_path.is_empty() {
                (*name).to_owned()
            } else {
                format!("{directory_path}/{name}")
            };
            renderer.push(
                content,
                PlanReviewTarget::FileDirectory { path: path.clone() },
                "/tasks",
                None,
                format!("File directory: {path}"),
            );
            let child_prefix = if branch.is_empty() {
                String::new()
            } else {
                format!("{prefix}{}", if is_last { "   " } else { "│  " })
            };
            render_file_tree(
                renderer,
                tree.directory.get(*name).expect("directory child"),
                &child_prefix,
                &path,
                false,
            );
        } else {
            let entry = tree.file.get(*name).expect("file child");
            renderer.push(
                format!("{prefix}{branch}{}", entry.label),
                PlanReviewTarget::FileTreeFile {
                    path: entry.path.clone(),
                },
                &entry.json_path,
                Some(&entry.path),
                entry.path.clone(),
            );
            let symbol_prefix = if branch.is_empty() {
                String::new()
            } else {
                format!("{prefix}{}", if is_last { "   " } else { "│  " })
            };
            for (symbol_index, symbol) in entry.symbol_list.iter().enumerate() {
                let symbol_is_last = symbol_index + 1 == entry.symbol_list.len();
                renderer.push(
                    format!(
                        "{symbol_prefix}{}{}",
                        if symbol_is_last { "└─ " } else { "├─ " },
                        symbol.label
                    ),
                    symbol.target.clone(),
                    &symbol.json_path,
                    Some(&entry.path),
                    symbol.label.clone(),
                );
            }
        }
    }
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
    let row_group_list = flow
        .steps
        .iter()
        .enumerate()
        .map(|(step_index, step)| {
            let mut row_list = Vec::new();
            collect_flow_step_rows(
                &mut row_list,
                graph,
                flow,
                step,
                &format!("{flow_path}/steps/{step_index}"),
                &[],
                None,
            );
            row_list
        })
        .collect::<Vec<_>>();
    let owner_column = row_group_list
        .iter()
        .flatten()
        .map(|row| row.content.chars().count())
        .max()
        .unwrap_or_default()
        + 4;

    for (group_index, row_group) in row_group_list.into_iter().enumerate() {
        for row in row_group {
            push_owned_flow_line(
                renderer,
                row.content,
                row.path,
                owner_column,
                row.target,
                &row.json_path,
                row.label,
            );
        }
        if group_index + 1 < flow.steps.len() {
            renderer.blank();
        }
    }
}

struct FlowRenderRow {
    content: String,
    path: Option<String>,
    target: PlanReviewTarget,
    json_path: String,
    label: String,
}

fn collect_flow_step_rows(
    row_list: &mut Vec<FlowRenderRow>,
    graph: &PlanGraph<'_>,
    flow: &PlanFlow,
    step: &PlanFlowStep,
    step_path: &str,
    ancestor_last_list: &[bool],
    is_last: Option<bool>,
) {
    row_list.push(FlowRenderRow {
        content: flow_tree_content(ancestor_last_list, is_last, &flow_step_content(graph, step)),
        path: flow_target_path(graph, &step.target),
        target: PlanReviewTarget::FlowStep {
            flow_id: flow.flow_id.clone(),
            step_id: step.step_id.clone(),
            reference_kind: entity_reference_kind(&step.target),
            target_name: entity_reference_label(graph, &step.target),
            target_is_type: entity_reference_is_type(graph, &step.target),
            workspace_path: entity_reference_workspace_path(&step.target),
            workspace_line: entity_reference_workspace_line(&step.target),
        },
        json_path: step_path.to_owned(),
        label: format!("Flow step: {}", step.action),
    });

    let mut child_ancestor_list = ancestor_last_list.to_vec();
    if let Some(is_last) = is_last {
        child_ancestor_list.push(is_last);
    }
    let child_count = step.edges.len() + step.branches.len();
    for (edge_index, edge) in step.edges.iter().enumerate() {
        collect_flow_edge_rows(
            row_list,
            graph,
            flow,
            step,
            edge,
            &format!("{step_path}/edges/{edge_index}"),
            &child_ancestor_list,
            edge_index + 1 == child_count,
        );
    }
    for (branch_index, branch) in step.branches.iter().enumerate() {
        collect_flow_branch_rows(
            row_list,
            graph,
            flow,
            step,
            branch,
            &format!("{step_path}/branches/{branch_index}"),
            &child_ancestor_list,
            step.edges.len() + branch_index + 1 == child_count,
        );
    }
}

fn collect_flow_edge_rows(
    row_list: &mut Vec<FlowRenderRow>,
    graph: &PlanGraph<'_>,
    flow: &PlanFlow,
    owner: &PlanFlowStep,
    edge: &PlanFlowEdge,
    edge_path: &str,
    ancestor_last_list: &[bool],
    is_last: bool,
) {
    row_list.push(FlowRenderRow {
        content: flow_tree_content(
            ancestor_last_list,
            Some(is_last),
            &flow_edge_content(graph, edge),
        ),
        path: flow_target_path(graph, &edge.target),
        target: PlanReviewTarget::FlowEdge {
            flow_id: flow.flow_id.clone(),
            step_id: owner.step_id.clone(),
            edge_id: edge.edge_id.clone(),
            callable_kind: flow_edge_callable(edge).map(|callable| callable.kind),
            callable_name: flow_edge_callable(edge).map(|callable| callable.name.clone()),
            reference_kind: entity_reference_kind(&edge.target),
            target_name: entity_reference_label(graph, &edge.target),
            target_is_type: entity_reference_is_type(graph, &edge.target),
            workspace_path: entity_reference_workspace_path(&edge.target),
            workspace_line: entity_reference_workspace_line(&edge.target),
        },
        json_path: edge_path.to_owned(),
        label: format!("Flow edge: {}", edge.relation.label()),
    });

    let mut child_ancestor_list = ancestor_last_list.to_vec();
    child_ancestor_list.push(is_last);
    let child_count = edge.expansion.len() + usize::from(edge.result.is_some());
    for (step_index, step) in edge.expansion.iter().enumerate() {
        collect_flow_step_rows(
            row_list,
            graph,
            flow,
            step,
            &format!("{edge_path}/expansion/{step_index}"),
            &child_ancestor_list,
            Some(step_index + 1 == child_count),
        );
    }
    if let Some(result) = &edge.result {
        row_list.push(FlowRenderRow {
            content: flow_tree_content(
                &child_ancestor_list,
                Some(true),
                &format!("→ {}", result.text()),
            ),
            path: None,
            target: PlanReviewTarget::FlowEdgeResult {
                flow_id: flow.flow_id.clone(),
                step_id: owner.step_id.clone(),
                edge_id: edge.edge_id.clone(),
                value_kind: result.kind(),
            },
            json_path: format!("{edge_path}/result"),
            label: format!("Flow edge result: {}", edge.relation.label()),
        });
    }
}

fn collect_flow_branch_rows(
    row_list: &mut Vec<FlowRenderRow>,
    graph: &PlanGraph<'_>,
    flow: &PlanFlow,
    owner: &PlanFlowStep,
    branch: &PlanFlowBranch,
    branch_path: &str,
    ancestor_last_list: &[bool],
    is_last: bool,
) {
    row_list.push(FlowRenderRow {
        content: flow_tree_content(
            ancestor_last_list,
            Some(is_last),
            &format!("when {}", branch.condition),
        ),
        path: None,
        target: PlanReviewTarget::FlowBranch {
            flow_id: flow.flow_id.clone(),
            step_id: owner.step_id.clone(),
            branch_id: branch.branch_id.clone(),
            condition: branch.condition.clone(),
        },
        json_path: branch_path.to_owned(),
        label: format!("Flow branch: {}", branch.condition),
    });

    let mut child_ancestor_list = ancestor_last_list.to_vec();
    child_ancestor_list.push(is_last);
    for (step_index, step) in branch.steps.iter().enumerate() {
        collect_flow_step_rows(
            row_list,
            graph,
            flow,
            step,
            &format!("{branch_path}/steps/{step_index}"),
            &child_ancestor_list,
            Some(step_index + 1 == branch.steps.len()),
        );
    }
}

fn flow_tree_content(ancestor_last_list: &[bool], is_last: Option<bool>, content: &str) -> String {
    let mut rendered = String::new();
    for ancestor_is_last in ancestor_last_list {
        rendered.push_str(if *ancestor_is_last { "   " } else { "│  " });
    }
    if let Some(is_last) = is_last {
        rendered.push_str(if is_last { "└─ " } else { "├─ " });
    }
    rendered.push_str(content);
    rendered
}

fn flow_step_content(graph: &PlanGraph<'_>, step: &PlanFlowStep) -> String {
    format!(
        "{}{} — {}",
        if matches!(step.target, EntityReference::PlannedEntity { .. }) {
            "*"
        } else {
            ""
        },
        entity_reference_label(graph, &step.target),
        step.action
    )
}

fn flow_edge_content(graph: &PlanGraph<'_>, edge: &PlanFlowEdge) -> String {
    let target = entity_reference_label(graph, &edge.target);
    match &edge.relation {
        PlanFlowRelation::Construct => format!("Construct {target}"),
        PlanFlowRelation::Call { callable } => {
            format!("Call {target}.{}()", callable.name)
        }
        PlanFlowRelation::Read { callable } => {
            format!("Read {target}.{}()", callable.name)
        }
        PlanFlowRelation::Write { callable } => {
            format!("Write {target}.{}()", callable.name)
        }
        PlanFlowRelation::Send { event } => format!("Send {event} to {target}"),
        PlanFlowRelation::Emit => format!("Emit to {target}"),
        PlanFlowRelation::Return => format!("Return to {target}"),
    }
}

fn flow_edge_callable(edge: &PlanFlowEdge) -> Option<&PlanCallable> {
    match &edge.relation {
        PlanFlowRelation::Call { callable }
        | PlanFlowRelation::Read { callable }
        | PlanFlowRelation::Write { callable } => Some(callable),
        _ => None,
    }
}

fn push_owned_flow_line(
    renderer: &mut PlanRenderer,
    content: String,
    path: Option<String>,
    owner_column: usize,
    target: PlanReviewTarget,
    json_path: &str,
    label: String,
) {
    let Some(path) = path else {
        renderer.push(content, target, json_path, None, label);
        return;
    };
    let owner = format!("[{path}]");
    if owner_column + owner.chars().count() <= PLAN_LINE_WIDTH {
        let mut line = pad_diagram_column(&content, owner_column);
        line.push_str(&owner);
        renderer.push(line, target, json_path, None, label);
        return;
    }
    renderer.push(content, target.clone(), json_path, None, label.clone());
    renderer.push(format!("    {owner}"), target, json_path, None, label);
}

fn flow_target_path(graph: &PlanGraph<'_>, target: &EntityReference) -> Option<String> {
    match target {
        EntityReference::PlannedEntity { entity } => Some(
            graph
                .entity_path(entity)
                .unwrap_or("<missing path>")
                .to_owned(),
        ),
        EntityReference::WorkspaceEntity { path, .. } => Some(path.clone()),
        EntityReference::ExternalEntity { .. } => None,
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
        .any(|entity| is_contract_kind(entity.kind));
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
    let mut entity_list = document
        .entity_changes
        .iter()
        .enumerate()
        .filter(|(_, entity)| {
            graph.presentation_parent(&entity.entity_id).is_none() && include(entity.kind)
        })
        .collect::<Vec<_>>();
    entity_list.sort_by_key(|(_, entity)| {
        graph
            .presentation_position(&entity.entity_id)
            .map(|position| position.preorder_rank())
            .unwrap_or(usize::MAX)
    });
    if entity_list.is_empty() {
        return vec![DiagramCell {
            text: "<none>".into(),
            anchor: object_model_anchor(),
        }];
    }
    let longest_path_width = document
        .entity_changes
        .iter()
        .filter(|entity| include(entity.kind))
        .map(|entity| format!("[{}]", entity.path).chars().count())
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
        declaration_marker(entity.action.base_action()),
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
    for child_index in graph.presentation_child_indices(&entity.entity_id) {
        let child = &document.entity_changes[child_index];
        cell_list.push(DiagramCell {
            text: String::new(),
            anchor: object_model_anchor(),
        });
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
            let path = file.change.path();
            renderer.push(
                format!("   file {path}"),
                PlanReviewTarget::File {
                    task_id: task.task_id.clone(),
                    path: path.to_owned(),
                },
                &file_path,
                Some(path),
                path,
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
                                path: path.to_owned(),
                                subtask_id: subtask.subtask_id.clone(),
                            },
                            &subtask_path,
                            Some(path),
                            format!("{path}: {}", subtask.description),
                        );
                        let entity_list = graph.entities_in_presentation_order(&subtask.entity_ids);
                        for (entity_index, entity) in entity_list.iter().enumerate() {
                            let entity_is_last = entity_index + 1 == entity_list.len();
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
                                    "{} — {}",
                                    entity_change_label(entity),
                                    entity.description
                                ),
                                PlanReviewTarget::Entity {
                                    entity_id: entity.entity_id.clone(),
                                },
                                entity_json_path(document, &entity.entity_id),
                                Some(&entity.path),
                                entity_change_label(entity),
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
                            Some(path),
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
        let path = file.change.path();
        renderer.push(
            format!("file {path}"),
            PlanReviewTarget::File {
                task_id: task.task_id.clone(),
                path: path.to_owned(),
            },
            format!("/tasks/{task_index}/files/{file_index}"),
            Some(path),
            path,
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
                Some(path),
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
        PlanSection::Files => "Files",
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

fn entity_change_label(entity: &ProgramEntityChange) -> String {
    match entity.action {
        EntityChangeAction::Add => {
            format!("Add {} {}", entity_kind_label(entity.kind), entity.name)
        }
        EntityChangeAction::Modify => {
            format!("Modify {} {}", entity_kind_label(entity.kind), entity.name)
        }
        EntityChangeAction::Remove => {
            format!("Remove {} {}", entity_kind_label(entity.kind), entity.name)
        }
        EntityChangeAction::Rename => format!(
            "Rename {} {} → {}",
            entity_kind_label(entity.kind),
            entity.renamed_from.as_deref().unwrap_or("<missing name>"),
            entity.name
        ),
    }
}

fn entity_reference_label(graph: &PlanGraph<'_>, target: &EntityReference) -> String {
    match target {
        EntityReference::PlannedEntity { entity } => graph
            .entity_label(entity)
            .unwrap_or_else(|| "<missing entity>".into()),
        EntityReference::WorkspaceEntity { name, .. }
        | EntityReference::ExternalEntity { name, .. } => name.clone(),
    }
}

fn entity_reference_kind(target: &EntityReference) -> PlanReviewReferenceKind {
    match target {
        EntityReference::PlannedEntity { .. } => PlanReviewReferenceKind::PlannedEntity,
        EntityReference::WorkspaceEntity { .. } => PlanReviewReferenceKind::WorkspaceEntity,
        EntityReference::ExternalEntity { .. } => PlanReviewReferenceKind::ExternalEntity,
    }
}

fn entity_reference_is_type(graph: &PlanGraph<'_>, target: &EntityReference) -> bool {
    match target {
        EntityReference::PlannedEntity { entity } => graph.entity(entity).is_some_and(|entity| {
            matches!(
                entity.kind,
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
        }),
        EntityReference::WorkspaceEntity { entity_kind, .. }
        | EntityReference::ExternalEntity { entity_kind, .. } => {
            *entity_kind == ReferencedEntityKind::Type
        }
    }
}

fn entity_reference_workspace_path(target: &EntityReference) -> Option<String> {
    match target {
        EntityReference::WorkspaceEntity { path, .. } => Some(path.clone()),
        EntityReference::PlannedEntity { .. } | EntityReference::ExternalEntity { .. } => None,
    }
}

fn entity_reference_workspace_line(target: &EntityReference) -> Option<usize> {
    match target {
        EntityReference::WorkspaceEntity { line, .. } => Some(*line),
        EntityReference::PlannedEntity { .. } | EntityReference::ExternalEntity { .. } => None,
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
    fn renders_plan_file_changes_as_a_status_aligned_tree() {
        let workspace = tempfile::tempdir().unwrap();
        let mut document = test_fixture("plan", "Overview");
        document.entity_changes[0].path = "hello/src/main.rs".into();
        document.entity_changes[0].action = EntityChangeAction::Rename;
        document.entity_changes[0].renamed_from = Some("LegacyPlanDocument".into());
        let mut inspector = document.entity_changes[0].clone();
        inspector.entity_id = "geo_parquet_inspector".into();
        inspector.action = EntityChangeAction::Add;
        inspector.renamed_from = None;
        inspector.kind = EntityKind::Struct;
        inspector.name = "GeoParquetInspector".into();
        inspector.path = "hello/src/inspection.rs".into();
        document.entity_changes.push(inspector);
        document.dependencies.clear();
        document.tasks[0].files[0].change = PlanFileChange::Modify {
            path: "hello/src/main.rs".into(),
        };
        document.tasks[0].files.push(PlanFile {
            change: PlanFileChange::Add {
                path: "hello/src/inspection.rs".into(),
            },
            subtasks: Vec::new(),
        });
        document.tasks[0].files.push(PlanFile {
            change: PlanFileChange::Remove {
                path: "hello/src/obsolete.rs".into(),
            },
            subtasks: Vec::new(),
        });
        document.tasks[0].files.push(PlanFile {
            change: PlanFileChange::Rename {
                from: "hello/tests/old_cli.rs".into(),
                to: "hello/tests/inspect_cli.rs".into(),
            },
            subtasks: Vec::new(),
        });

        let mut renderer = PlanRenderer::new();
        let graph = PlanGraph::new(&document);
        render_files(&mut renderer, &document, &graph, workspace.path());
        let markdown = renderer.finish().markdown;

        assert_eq!(
            markdown,
            concat!(
                "# Files\n",
                "\n",
                "hello\n",
                "├─ src\n",
                "│  ├─ (new) inspection.rs\n",
                "│  │  └─ (new) struct GeoParquetInspector\n",
                "│  ├─ main.rs\n",
                "│  │  └─ LegacyPlanDocument → PlanDocument\n",
                "│  └─ (remove) obsolete.rs\n",
                "└─ tests\n",
                "   └─ old_cli.rs → inspect_cli.rs\n",
                "\n",
            )
        );
    }

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
            action: EntityChangeAction::Add,
            kind: EntityKind::Struct,
            renamed_from: None,
            name: "InspectionReport".into(),
            description: "Owns one inspection result.".into(),
            path: "hello/src/report.rs".into(),
            members: Vec::new(),
            variants: Vec::new(),
            extends: None,
            conforms_to: Vec::new(),
        });
        document.entity_changes.push(ProgramEntityChange {
            entity_id: "inspection_error".into(),
            action: EntityChangeAction::Add,
            kind: EntityKind::Enum,
            renamed_from: None,
            name: "InspectionError".into(),
            description: "Classifies inspection failures.".into(),
            path: "hello/src/error.rs".into(),
            members: Vec::new(),
            variants: Vec::new(),
            extends: None,
            conforms_to: Vec::new(),
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
            .find(|line| line.starts_with("  *struct InspectionReport"))
            .expect("report declaration");
        let error_line = rendered
            .markdown
            .lines()
            .find(|line| line.starts_with("  *enum InspectionError"))
            .expect("error declaration");

        assert_eq!(inspector_line.find('['), Some(DIAGRAM_PATH_COLUMN_MAX));
        assert_eq!(report_line.find('['), Some(DIAGRAM_PATH_COLUMN_MAX));
        assert_eq!(error_line.find('['), Some(DIAGRAM_PATH_COLUMN_MAX));
        let inspector_position = rendered.markdown.find(inspector_line).unwrap();
        let report_position = rendered.markdown.find(report_line).unwrap();
        let error_position = rendered.markdown.find(error_line).unwrap();
        assert!(inspector_position < report_position);
        assert!(report_position < error_position);
        assert!(
            rendered
                .markdown
                .contains("  + inspect(path: &Path): Result<InspectionReport, InspectionError>\n")
        );
        assert!(
            rendered.markdown.contains(
                "  + inspect(path: &Path): Result<InspectionReport, InspectionError>\n\n  *struct InspectionReport"
            )
        );
        assert!(
            rendered
                .markdown
                .contains("[hello/src/report.rs]\n\n  *enum InspectionError")
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

        assert!(
            rendered
                .markdown
                .starts_with("# Create datafusion CLI tool\n\nOverview\n\n# Usage")
        );
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
    fn renders_contracts_conformance_and_derived_children_in_fixed_columns() {
        let mut document = test_fixture("plan", "Overview");
        document.entity_changes[0] = ProgramEntityChange {
            entity_id: "plan_document".into(),
            action: EntityChangeAction::Modify,
            kind: EntityKind::Trait,
            renamed_from: None,
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
        };
        document.entity_changes.push(ProgramEntityChange {
            entity_id: "codex_backend".into(),
            action: EntityChangeAction::Add,
            kind: EntityKind::Struct,
            renamed_from: None,
            name: "CodexBackend".into(),
            description: "Runs requests through Codex.".into(),
            path: "src/codex.rs".into(),
            members: vec![
                ProgramEntityMemberChange {
                    member_id: "codex_backend_client".into(),
                    action: ChangeAction::Add,
                    kind: MemberKind::Field,
                    name: "client".into(),
                    description: "Owns the provider client.".into(),
                    visibility: Some(Visibility::Private),
                    type_name: Some("Client".into()),
                    parameters: Vec::new(),
                    return_type: None,
                },
                ProgramEntityMemberChange {
                    member_id: "codex_backend_cache".into(),
                    action: ChangeAction::Add,
                    kind: MemberKind::Field,
                    name: "cache".into(),
                    description: "Owns request state.".into(),
                    visibility: Some(Visibility::Private),
                    type_name: Some("RequestCache".into()),
                    parameters: Vec::new(),
                    return_type: None,
                },
            ],
            variants: Vec::new(),
            extends: None,
            conforms_to: vec![EntityReference::PlannedEntity {
                entity: "plan_document".into(),
            }],
        });
        document.entity_changes.push(ProgramEntityChange {
            entity_id: "request_cache".into(),
            action: EntityChangeAction::Remove,
            kind: EntityKind::Cache,
            renamed_from: None,
            name: "RequestCache".into(),
            description: "Stores request state.".into(),
            path: "src/codex.rs".into(),
            members: Vec::new(),
            variants: Vec::new(),
            extends: None,
            conforms_to: Vec::new(),
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
            .expect("derived child declaration");
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
        assert!(
            task_markdown
                .lines()
                .any(|line| line.starts_with("      ")
                    && line.contains("subtask-alignment-sentinel"))
        );
        assert!(task_markdown.lines().any(
            |line| line.starts_with("         ") && line.contains("entity-alignment-sentinel")
        ));
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
    fn orders_task_entities_by_dependency_and_closes_final_siblings() {
        let mut document = test_fixture("plan", "Overview");
        let mut second_entity = document.entity_changes[0].clone();
        second_entity.entity_id = "plan_renderer".into();
        second_entity.name = "PlanRenderer".into();
        second_entity.description = "Render canonical planning data.".into();
        second_entity.members = vec![ProgramEntityMemberChange {
            member_id: "plan_renderer_document".into(),
            action: ChangeAction::Add,
            kind: MemberKind::Field,
            name: "document".into(),
            description: "Retains the canonical plan.".into(),
            visibility: Some(Visibility::Private),
            type_name: Some("PlanDocument".into()),
            parameters: Vec::new(),
            return_type: None,
        }];
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
        assert!(task_markdown.contains("      ├─ Add struct PlanRenderer"));
        assert!(task_markdown.contains("      └─ Add struct PlanDocument"));
        assert!(!task_markdown.contains("      │  Add struct PlanDocument"));
        assert!(
            task_markdown.find("PlanRenderer").unwrap()
                < task_markdown.find("PlanDocument").unwrap()
        );
    }

    #[test]
    fn orders_each_subtask_without_requiring_the_dependency_parent() {
        let mut document = test_fixture("plan", "Overview");
        let mut report = document.entity_changes[0].clone();
        report.entity_id = "inspection_report".into();
        report.name = "InspectionReport".into();
        report.description = "Own one inspection result.".into();
        let mut error = document.entity_changes[0].clone();
        error.entity_id = "inspection_error".into();
        error.kind = EntityKind::Enum;
        error.name = "InspectionError".into();
        error.description = "Classify inspection failures.".into();
        let mut inspector = document.entity_changes[0].clone();
        inspector.entity_id = "geo_parquet_inspector".into();
        inspector.name = "GeoParquetInspector".into();
        inspector.description = "Coordinate one inspection.".into();
        inspector.members = vec![ProgramEntityMemberChange {
            member_id: "geo_parquet_inspector_inspect".into(),
            action: ChangeAction::Add,
            kind: MemberKind::Method,
            name: "inspect".into(),
            description: "Inspect one local file.".into(),
            visibility: Some(Visibility::Public),
            type_name: None,
            parameters: Vec::new(),
            return_type: Some("Result<InspectionReport, InspectionError>".into()),
        }];
        document.entity_changes = vec![report, error, inspector];
        document.tasks[0].files[0].subtasks = vec![
            PlanSubtask::Work(PlanWorkSubtask {
                subtask_id: "create_inspector".into(),
                action: SubtaskAction::Create,
                description: "Create the inspector boundary.".into(),
                entity_ids: vec!["geo_parquet_inspector".into()],
            }),
            PlanSubtask::Work(PlanWorkSubtask {
                subtask_id: "create_results".into(),
                action: SubtaskAction::Create,
                description: "Create the result boundaries.".into(),
                entity_ids: vec!["inspection_error".into(), "inspection_report".into()],
            }),
        ];

        let graph = PlanGraph::new(&document);
        let mut renderer = PlanRenderer::new();
        render_tasks(&mut renderer, &document, &graph);
        let task_markdown = renderer.finish().markdown;
        let result_subtask = task_markdown
            .split("Create the result boundaries.")
            .nth(1)
            .expect("result subtask");

        assert!(
            result_subtask.find("InspectionReport").unwrap()
                < result_subtask.find("InspectionError").unwrap()
        );
        assert!(!result_subtask.contains("GeoParquetInspector"));
    }

    #[test]
    fn renders_flow_diagrams_inside_text_fences() {
        let rendered = render_plan(&test_fixture("plan", "Overview")).unwrap();

        assert!(rendered.markdown.contains(
            "## Code flow: Execution\n\nStart from the accepted plan and produce executable \
                     work. Keep planning ownership distinct from execution state.\n\n```text",
        ));
    }

    #[test]
    fn renders_flow_projection_as_typed_runtime_edges_with_aligned_owners() {
        let mut document = test_fixture("plan", "Overview");
        document.flows[0].title = "DataFusion table inspection".into();
        document.flows[0].steps = vec![
            PlanFlowStep {
                step_id: "register".into(),
                action: "Register local Parquet".into(),
                target: EntityReference::ExternalEntity {
                    entity_kind: ReferencedEntityKind::Type,
                    name: "SessionContext".into(),
                    dependency: Some("datafusion".into()),
                },
                edges: vec![PlanFlowEdge {
                    edge_id: "register_table".into(),
                    relation: PlanFlowRelation::Write {
                        callable: PlanCallable {
                            kind: PlanCallableKind::Method,
                            name: "register_table".into(),
                        },
                    },
                    target: EntityReference::ExternalEntity {
                        entity_kind: ReferencedEntityKind::Type,
                        name: "SessionContext".into(),
                        dependency: Some("datafusion".into()),
                    },
                    expansion: Vec::new(),
                    result: Some(PlanFlowValue::Text {
                        text: "registered table".into(),
                    }),
                }],
                branches: Vec::new(),
            },
            PlanFlowStep {
                step_id: "observe".into(),
                action: "Read table observations".into(),
                target: EntityReference::PlannedEntity {
                    entity: "plan_document".into(),
                },
                edges: vec![
                    PlanFlowEdge {
                        edge_id: "schema".into(),
                        relation: PlanFlowRelation::Read {
                            callable: PlanCallable {
                                kind: PlanCallableKind::Method,
                                name: "schema".into(),
                            },
                        },
                        target: EntityReference::ExternalEntity {
                            entity_kind: ReferencedEntityKind::Type,
                            name: "SessionContext".into(),
                            dependency: Some("datafusion".into()),
                        },
                        expansion: Vec::new(),
                        result: Some(PlanFlowValue::Text {
                            text: "schema text".into(),
                        }),
                    },
                    PlanFlowEdge {
                        edge_id: "count".into(),
                        relation: PlanFlowRelation::Call {
                            callable: PlanCallable {
                                kind: PlanCallableKind::Method,
                                name: "count".into(),
                            },
                        },
                        target: EntityReference::ExternalEntity {
                            entity_kind: ReferencedEntityKind::Type,
                            name: "DataFrame".into(),
                            dependency: Some("datafusion".into()),
                        },
                        expansion: Vec::new(),
                        result: Some(PlanFlowValue::Type { name: "u64".into() }),
                    },
                ],
                branches: Vec::new(),
            },
            PlanFlowStep {
                step_id: "assemble".into(),
                action: "Assemble inspection summary".into(),
                target: EntityReference::PlannedEntity {
                    entity: "plan_document".into(),
                },
                edges: vec![PlanFlowEdge {
                    edge_id: "return_summary".into(),
                    relation: PlanFlowRelation::Return,
                    target: EntityReference::ExternalEntity {
                        entity_kind: ReferencedEntityKind::Endpoint,
                        name: "CLI command".into(),
                        dependency: None,
                    },
                    expansion: Vec::new(),
                    result: Some(PlanFlowValue::Type {
                        name: "InspectionSummary".into(),
                    }),
                }],
                branches: Vec::new(),
            },
            PlanFlowStep {
                step_id: "render".into(),
                action: "Render inspection output".into(),
                target: EntityReference::PlannedEntity {
                    entity: "plan_document".into(),
                },
                edges: vec![PlanFlowEdge {
                    edge_id: "emit_stdout".into(),
                    relation: PlanFlowRelation::Emit,
                    target: EntityReference::ExternalEntity {
                        entity_kind: ReferencedEntityKind::Endpoint,
                        name: "terminal stdout".into(),
                        dependency: None,
                    },
                    expansion: Vec::new(),
                    result: Some(PlanFlowValue::Text {
                        text: "stdout".into(),
                    }),
                }],
                branches: Vec::new(),
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
            "SessionContext — Register local Parquet",
            "└─ Write SessionContext.register_table()",
            "   └─ → registered table",
            "",
            "*PlanDocument — Read table observations",
            "├─ Read SessionContext.schema()",
            "│  └─ → schema text",
            "└─ Call DataFrame.count()",
            "   └─ → u64",
            "",
            "*PlanDocument — Assemble inspection summary",
            "└─ Return to CLI command",
            "   └─ → InspectionSummary",
            "",
            "*PlanDocument — Render inspection output",
            "└─ Emit to terminal stdout",
            "   └─ → stdout",
        ];
        assert_eq!(diagram_line_list.len(), expected_content_list.len());
        let mut owner_column_list = Vec::new();
        for (line, expected_content) in diagram_line_list.iter().zip(expected_content_list) {
            assert!(line.starts_with(expected_content));
            if let Some(owner_byte_index) = line.find('[') {
                owner_column_list.push(line[..owner_byte_index].chars().count());
            }
        }
        assert_eq!(owner_column_list.len(), 3);
        assert!(diagram_line_list[4].ends_with("[src/plan.rs]"));
        assert!(diagram_line_list[10].ends_with("[src/plan.rs]"));
        assert!(diagram_line_list[14].ends_with("[src/plan.rs]"));
        assert!(!diagram.contains("[SessionContext]"));
        assert!(!diagram.contains("[DataFrame]"));
        assert!(!diagram.contains("[CLI command]"));
        assert!(!diagram.contains("[terminal stdout]"));
        assert!(
            diagram_line_list
                .iter()
                .all(|line| line.chars().count() <= PLAN_LINE_WIDTH)
        );
        for action in [
            "Register local Parquet",
            "Read table observations",
            "SessionContext.schema()",
            "DataFrame.count()",
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

        let schema_edge_anchor = rendered
            .navigation
            .anchor
            .iter()
            .find(|anchor| {
                matches!(
                    &anchor.target,
                    PlanReviewTarget::FlowEdge {
                        edge_id,
                        ..
                    } if edge_id == "schema"
                )
            })
            .expect("schema edge anchor");
        assert_eq!(schema_edge_anchor.json_path, "/flows/0/steps/1/edges/0");
        assert!(matches!(
            &schema_edge_anchor.target,
            PlanReviewTarget::FlowEdge {
                callable_kind: Some(PlanCallableKind::Method),
                callable_name: Some(callable_name),
                target_name,
                target_is_type: true,
                ..
            } if callable_name == "schema" && target_name == "SessionContext"
        ));
        let typed_value_anchor = rendered
            .navigation
            .anchor
            .iter()
            .find(|anchor| anchor.json_path == "/flows/0/steps/2/edges/0/result")
            .expect("typed flow value anchor");
        assert!(matches!(
            typed_value_anchor.target,
            PlanReviewTarget::FlowEdgeResult {
                value_kind: PlanFlowValueKind::Type,
                ..
            }
        ));
        let text_value_anchor = rendered
            .navigation
            .anchor
            .iter()
            .find(|anchor| anchor.json_path == "/flows/0/steps/0/edges/0/result")
            .expect("text flow value anchor");
        assert!(matches!(
            text_value_anchor.target,
            PlanReviewTarget::FlowEdgeResult {
                value_kind: PlanFlowValueKind::Text,
                ..
            }
        ));
    }

    #[test]
    fn renders_nested_expansions_and_labeled_branches_with_exact_anchors() {
        let mut document = test_fixture("plan", "Overview");
        document.flows[0].title = "Inspection routing".into();
        let terminal_target = || EntityReference::ExternalEntity {
            entity_kind: ReferencedEntityKind::Endpoint,
            name: "terminal".into(),
            dependency: None,
        };
        let terminal_step = |step_id: &str, action: &str, result: &str| PlanFlowStep {
            step_id: step_id.into(),
            action: action.into(),
            target: EntityReference::PlannedEntity {
                entity: "plan_document".into(),
            },
            edges: vec![PlanFlowEdge {
                edge_id: format!("{step_id}_emit"),
                relation: PlanFlowRelation::Emit,
                target: terminal_target(),
                expansion: Vec::new(),
                result: Some(PlanFlowValue::Text {
                    text: result.into(),
                }),
            }],
            branches: Vec::new(),
        };
        document.flows[0].steps = vec![PlanFlowStep {
            step_id: "route".into(),
            action: "Route inspection".into(),
            target: EntityReference::PlannedEntity {
                entity: "plan_document".into(),
            },
            edges: vec![PlanFlowEdge {
                edge_id: "inspect".into(),
                relation: PlanFlowRelation::Call {
                    callable: PlanCallable {
                        kind: PlanCallableKind::Method,
                        name: "validate".into(),
                    },
                },
                target: EntityReference::PlannedEntity {
                    entity: "plan_document".into(),
                },
                expansion: vec![PlanFlowStep {
                    step_id: "read_metadata".into(),
                    action: "Read metadata".into(),
                    target: EntityReference::PlannedEntity {
                        entity: "plan_document".into(),
                    },
                    edges: vec![PlanFlowEdge {
                        edge_id: "read_schema".into(),
                        relation: PlanFlowRelation::Read {
                            callable: PlanCallable {
                                kind: PlanCallableKind::Method,
                                name: "schema".into(),
                            },
                        },
                        target: EntityReference::ExternalEntity {
                            entity_kind: ReferencedEntityKind::Type,
                            name: "SessionContext".into(),
                            dependency: Some("datafusion".into()),
                        },
                        expansion: Vec::new(),
                        result: Some(PlanFlowValue::Type {
                            name: "SchemaRef".into(),
                        }),
                    }],
                    branches: Vec::new(),
                }],
                result: Some(PlanFlowValue::Type {
                    name: "InspectionReport".into(),
                }),
            }],
            branches: vec![
                PlanFlowBranch {
                    branch_id: "success".into(),
                    condition: "success".into(),
                    steps: vec![terminal_step("render_report", "Render report", "stdout")],
                },
                PlanFlowBranch {
                    branch_id: "failure".into(),
                    condition: "failure".into(),
                    steps: vec![terminal_step("emit_failure", "Emit failure", "stderr")],
                },
            ],
        }];

        let rendered = render_plan(&document).unwrap();
        let diagram = rendered
            .markdown
            .split("## Code flow: Inspection routing")
            .nth(1)
            .expect("flow section")
            .split("```text\n")
            .nth(1)
            .expect("flow fence")
            .split("\n```")
            .next()
            .expect("flow diagram");
        let content_list = diagram
            .lines()
            .map(|line| {
                line.find('[')
                    .map_or(line, |owner_start| line[..owner_start].trim_end())
            })
            .collect::<Vec<_>>();

        assert_eq!(
            content_list,
            vec![
                "*PlanDocument — Route inspection",
                "├─ Call PlanDocument.validate()",
                "│  ├─ *PlanDocument — Read metadata",
                "│  │  └─ Read SessionContext.schema()",
                "│  │     └─ → SchemaRef",
                "│  └─ → InspectionReport",
                "├─ when success",
                "│  └─ *PlanDocument — Render report",
                "│     └─ Emit to terminal",
                "│        └─ → stdout",
                "└─ when failure",
                "   └─ *PlanDocument — Emit failure",
                "      └─ Emit to terminal",
                "         └─ → stderr",
            ]
        );
        assert!(rendered.navigation.anchor.iter().any(|anchor| {
            anchor.json_path == "/flows/0/steps/0/edges/0/expansion/0"
                && matches!(
                    anchor.target,
                    PlanReviewTarget::FlowStep {
                        target_is_type: true,
                        ..
                    }
                )
        }));
        assert!(rendered.navigation.anchor.iter().any(|anchor| {
            anchor.json_path == "/flows/0/steps/0/branches/0"
                && matches!(
                    &anchor.target,
                    PlanReviewTarget::FlowBranch {
                        condition,
                        ..
                    } if condition == "success"
                )
        }));
    }

    #[test]
    fn renders_workspace_entities_with_repository_navigation_anchors() {
        let mut document = test_fixture("plan", "Overview");
        let step = &mut document.flows[0].steps[0];
        step.target = EntityReference::WorkspaceEntity {
            entity_kind: ReferencedEntityKind::Type,
            name: "PlanValidator".into(),
            path: "src/plan/validation.rs".into(),
            line: 76,
        };
        let step_id = step.step_id.clone();

        let rendered = render_plan(&document).unwrap();
        let anchor = rendered
            .navigation
            .anchor
            .iter()
            .find(|anchor| {
                matches!(
                    &anchor.target,
                    PlanReviewTarget::FlowStep { step_id: candidate, .. } if candidate == &step_id
                )
            })
            .expect("workspace flow step anchor");

        assert!(rendered.markdown.contains("[src/plan/validation.rs]"));
        assert!(matches!(
            &anchor.target,
            PlanReviewTarget::FlowStep {
                target_name,
                workspace_path: Some(path),
                workspace_line: Some(76),
                ..
            } if target_name == "PlanValidator" && path == "src/plan/validation.rs"
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

        assert!(diagram.contains("*PlanDocument — Read plan"));
        assert!(diagram.contains("└─ Return to execution scheduler"));
        assert!(diagram.contains("   └─ → ExecutablePlan"));
    }

    #[test]
    fn moves_an_owner_below_content_when_the_right_column_cannot_fit() {
        let mut document = test_fixture("plan", "Overview");
        let action = "Read a deliberately wide observation that consumes the diagram line width";
        document.flows[0].steps = vec![PlanFlowStep {
            step_id: "wide_observation".into(),
            action: action.into(),
            target: EntityReference::PlannedEntity {
                entity: "plan_document".into(),
            },
            edges: vec![PlanFlowEdge {
                edge_id: "emit_observation".into(),
                relation: PlanFlowRelation::Emit,
                target: EntityReference::ExternalEntity {
                    entity_kind: ReferencedEntityKind::Endpoint,
                    name: "terminal".into(),
                    dependency: None,
                },
                expansion: Vec::new(),
                result: None,
            }],
            branches: Vec::new(),
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

        assert_eq!(
            diagram,
            format!("*PlanDocument — {action}\n    [src/plan.rs]\n└─ Emit to terminal")
        );
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
                edges: first_step
                    .edges
                    .iter()
                    .cloned()
                    .map(|mut edge| {
                        edge.edge_id = format!("second_{}", edge.edge_id);
                        edge
                    })
                    .collect(),
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
            resolved_version: None,
            manifest: "Cargo.toml".into(),
            license: Some("MIT".into()),
            justification: "Run asynchronous work. The standard library provides no runtime."
                .into(),
        });
        document.dependencies.push(PlanDependencyChange {
            dependency_id: "dependency_serde".into(),
            action: ChangeAction::Modify,
            name: "serde".into(),
            version: "1.0".into(),
            resolved_version: None,
            manifest: "Cargo.toml".into(),
            license: Some("MIT OR Apache-2.0".into()),
            justification:
                "Decode canonical plan state. Manual decoding would duplicate schema rules.".into(),
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
            .find(|file| file.change.path() == "src/plan.rs")
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
        let diagram = rendered
            .markdown
            .split("# Files")
            .next()
            .expect("diagram projection");
        assert!(!diagram.contains("(new)"));
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

        let edge_value = serde_json::to_value(PlanReviewTarget::FlowEdge {
            flow_id: "inspection".into(),
            step_id: "observe".into(),
            edge_id: "schema".into(),
            callable_kind: Some(PlanCallableKind::Method),
            callable_name: Some("schema".into()),
            reference_kind: PlanReviewReferenceKind::ExternalEntity,
            target_name: "SessionContext".into(),
            target_is_type: true,
            workspace_path: None,
            workspace_line: None,
        })
        .unwrap();
        assert_eq!(
            edge_value.pointer("/target_type"),
            Some(&serde_json::json!("flow_edge"))
        );
        assert_eq!(
            edge_value.pointer("/edge_id"),
            Some(&serde_json::json!("schema"))
        );
        assert_eq!(
            edge_value.pointer("/callable_kind"),
            Some(&serde_json::json!("method"))
        );
        assert_eq!(
            edge_value.pointer("/target_is_type"),
            Some(&serde_json::json!(true))
        );
        assert_eq!(
            edge_value.pointer("/reference_kind"),
            Some(&serde_json::json!("external_entity"))
        );
    }
}
