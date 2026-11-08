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
    Objective,
    Requirements,
    Background,
    Decisions,
    Design,
    Flows,
    Overview,
    Verification,
    AutomatedVerification,
    ManualVerification,
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
    InlineSymbol {
        name: String,
    },
    Check {
        id: String,
    },
    TestCase {
        path: String,
        name: String,
    },
    Section {
        section: PlanSection,
    },
    Entity {
        name: String,
    },
    EntityMember {
        entity: String,
        member: String,
    },
    EnumVariant {
        entity: String,
        variant: String,
    },
    EnumVariantField {
        entity: String,
        variant: String,
        field: String,
    },
    Dependency {
        name: String,
    },
    DependencyManifest {
        manifest: String,
    },
    Flow {
        title: String,
    },
    FlowStep {
        reference_kind: PlanReviewReferenceKind,
        target_name: String,
        target_is_type: bool,
        workspace_path: Option<String>,
        workspace_line: Option<usize>,
    },
    FlowEdge {
        callable_kind: Option<PlanCallableKind>,
        callable_name: Option<String>,
        reference_kind: PlanReviewReferenceKind,
        target_name: String,
        target_is_type: bool,
        workspace_path: Option<String>,
        workspace_line: Option<usize>,
    },
    FlowEdgeResult {
        type_name: String,
    },
    FlowBranch {
        condition: String,
    },
    Stage {
        id: String,
        title: String,
    },
    TaskDetail,
    Task {
        title: String,
    },
    File {
        path: String,
    },
    Declaration {
        path: String,
        side: String,
        line: u32,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        column: Option<u32>,
    },
    Call {
        #[serde(default, skip_serializing_if = "super::CallKind::is_call")]
        kind: super::CallKind,
        path: String,
        side: String,
        owner: String,
        name: String,
    },
    Behavior {
        path: String,
        side: String,
        owner: String,
        offset: u32,
    },
    FileDirectory {
        path: String,
    },
    FileTreeFile {
        path: String,
    },
    FileTreeEntity {
        name: String,
        path: String,
    },
    FileTreeTest {
        name: String,
        action: ChangeAction,
        path: String,
        category: TestCategory,
    },
    Subtask {
        path: String,
    },
    Test {
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
    pub plan_id: String,
    pub plan_version: u64,
    pub anchor: Vec<PlanNavigationAnchor>,
}

impl PlanNavigationIndex {
    /// Resolve the semantic anchor attached to one reviewer-visible line.
    pub fn resolve_line(&self, line: u32) -> Option<&PlanNavigationAnchor> {
        self.anchor.iter().rev().find(|anchor| anchor.line == line)
    }
}

/// Represents the deterministic human projection of one canonical plan.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct RenderedPlan {
    pub markdown: String,
    pub navigation: PlanNavigationIndex,
}

struct PlanRenderer {
    line: Vec<PlanLine>,
    navigation: PlanNavigationIndex,
}

enum PlanLine {
    Blank,
    Content {
        text: String,
        anchor: Box<PlanNavigationAnchor>,
    },
}

const PLAN_LINE_WIDTH: usize = 100;
const DIAGRAM_CONCRETE_COLUMN: usize = 42;
const DIAGRAM_PATH_COLUMN_MAX: usize = 36;

#[derive(Clone, Serialize)]
struct DiagramAnchor {
    target: PlanReviewTarget,
    json_path: String,
    path: Option<String>,
    label: String,
}

#[derive(Clone, Serialize)]
struct DiagramCell {
    text: String,
    anchor: DiagramAnchor,
}

impl PlanRenderer {
    fn new(document: &PlanDocument) -> Self {
        Self {
            line: Vec::new(),
            navigation: PlanNavigationIndex {
                plan_id: document.plan_id.clone(),
                plan_version: document.version,
                anchor: Vec::new(),
            },
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
        let mut anchor = PlanNavigationAnchor {
            line: 0,
            target,
            json_path: json_path.into(),
            path: path.map(str::to_owned),
            label: label.into(),
        };
        for row in value.split('\n') {
            if row.is_empty() {
                self.line.push(PlanLine::Blank);
            } else {
                anchor.line = self.line.len() as u32 + 1;
                self.line.push(PlanLine::Content {
                    text: row.to_owned(),
                    anchor: (anchor.clone()).into(),
                });
            }
        }
    }

    fn blank(&mut self) {
        self.line.push(PlanLine::Blank);
    }

    fn push_wrapped(
        &mut self,
        prefix: [&str; 2],
        content: &str,
        target: PlanReviewTarget,
        json_path: impl Into<String>,
        path: Option<&str>,
        label: impl Into<String>,
    ) {
        let [first_prefix, continuation_prefix] = prefix;
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

    fn finish(self) -> Result<RenderedPlan> {
        let mut markdown = String::new();
        let mut navigation = self.navigation;
        for line in self.line {
            if let PlanLine::Content { text, anchor } = line {
                markdown.push_str(&text);
                navigation.anchor.push(*anchor);
            }
            markdown.push('\n');
        }
        if markdown.is_empty() {
            markdown.push('\n');
        }
        Ok(RenderedPlan {
            markdown,
            navigation,
        })
    }
}

fn wrap_prefixed<'source>(
    content: &'source str,
    first_prefix: &'source str,
    continuation_prefix: &'source str,
    width: usize,
) -> impl Iterator<Item = String> + 'source {
    let mut words = content.split_whitespace().peekable();
    let mut first = true;
    std::iter::from_fn(move || {
        if !first && words.peek().is_none() {
            return None;
        }
        let mut line = if first {
            first_prefix
        } else {
            continuation_prefix
        }
        .to_owned();
        first = false;
        let mut columns = line.chars().count();
        let mut has_content = false;
        while let Some(word) =
            words.next_if(|word| !has_content || columns + 1 + word.chars().count() <= width)
        {
            let word_columns = word.chars().count();
            let separator = usize::from(has_content);
            if has_content {
                line.push(' ');
            }
            line.push_str(word);
            columns += separator + word_columns;
            has_content = true;
        }
        Some(line)
    })
}

/// Render one canonical plan into reviewer-readable Markdown and exact navigation metadata.
pub fn render_plan(document: &PlanDocument) -> Result<RenderedPlan> {
    render_plan_at(document, Path::new("."))
}

/// Render one plan against its repository root so file status reflects the worktree.
pub fn render_plan_at(document: &PlanDocument, workspace: &Path) -> Result<RenderedPlan> {
    if document.design.is_some() {
        document.validate()?;
        return super::design_review::render(document);
    }
    validate_plan_render(document)?;
    let graph = PlanGraph::new(document);
    let mut renderer = PlanRenderer::new(document);
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
    renderer.finish()
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
    for task_entry in document.task_entries() {
        let task = task_entry.task();
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
                    json_path: format!("{}/files/{file_index}", task_entry.path()),
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

    let entity_name_list = document
        .entity_changes
        .iter()
        .map(|entity| entity.name.clone())
        .collect::<Vec<_>>();
    for entity in graph.entities_in_presentation_order(&entity_name_list) {
        let Some(entry) = file_list.iter_mut().find(|entry| entry.path == entity.path) else {
            continue;
        };
        entry.symbol_list.push(RenderedFileSymbol {
            label: file_entity_label(entity),
            target: PlanReviewTarget::FileTreeEntity {
                name: entity.name.clone(),
                path: entity.path.clone(),
            },
            json_path: entity_json_path(document, &entity.name),
        });
    }
    for task_entry in document.task_entries() {
        let task = task_entry.task();
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
                        name: test.name.clone(),
                        action: test.action,
                        path: path.to_owned(),
                        category: test.category,
                    },
                    json_path: format!(
                        "{}/files/{file_index}/subtasks/{subtask_index}",
                        task_entry.path()
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
        ChangeAction::Rename => format!(
            "(rename {} → {}) {kind}",
            test.renamed_from.as_deref().unwrap_or("?"),
            test.name
        ),
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
    enum Child<'tree> {
        Directory(&'tree RenderedFileTree),
        File(&'tree RenderedFileEntry),
    }
    let mut child_list = tree
        .directory
        .iter()
        .map(|(name, child)| (name.as_str(), Child::Directory(child)))
        .chain(
            tree.file
                .iter()
                .map(|(name, child)| (name.as_str(), Child::File(child))),
        )
        .collect::<Vec<_>>();
    child_list.sort_by(|left, right| left.0.cmp(right.0));

    for (index, (name, child)) in child_list.iter().enumerate() {
        let is_last = index + 1 == child_list.len();
        let branch = if omit_root_connector && prefix.is_empty() && child_list.len() == 1 {
            ""
        } else if is_last {
            "└─ "
        } else {
            "├─ "
        };
        let content = format!("{prefix}{branch}{name}");
        match child {
            Child::Directory(directory) => {
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
                render_file_tree(renderer, directory, &child_prefix, &path, false);
            }
            Child::File(entry) => {
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
            title: flow.title.clone(),
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
    let mut row_list = Vec::new();
    for (edge_index, edge) in flow.edges.iter().enumerate() {
        collect_flow_edge_rows(
            &mut row_list,
            graph,
            edge,
            &format!("{flow_path}/edges/{edge_index}"),
            &[],
            edge_index + 1 == flow.edges.len(),
        );
    }
    let preferred_owner_column = row_list
        .iter()
        .map(|row| row.content.chars().count())
        .max()
        .unwrap_or_default()
        + 4;
    let widest_owner = row_list
        .iter()
        .filter_map(|row| row.path.as_ref())
        .map(|path| path.chars().count() + 2)
        .max()
        .unwrap_or_default();
    let owner_column = preferred_owner_column.min(PLAN_LINE_WIDTH.saturating_sub(widest_owner));

    for row in row_list {
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
}

#[derive(Serialize)]
struct FlowRenderRow {
    content: String,
    path: Option<String>,
    target: PlanReviewTarget,
    json_path: String,
    label: String,
}

fn collect_flow_edge_rows(
    row_list: &mut Vec<FlowRenderRow>,
    graph: &PlanGraph<'_>,
    edge: &PlanFlowEdge,
    edge_path: &str,
    ancestor_last_list: &[bool],
    is_last: bool,
) {
    enum FlowEntry<'source> {
        Edge(&'source PlanFlowEdge),
        Branch(&'source PlanFlowBranch),
    }
    let mut pending = vec![(
        FlowEntry::Edge(edge),
        edge_path.to_owned(),
        ancestor_last_list.to_vec(),
        is_last,
    )];
    while let Some((entry, entry_path, ancestor_last_list, is_last)) = pending.pop() {
        let mut child_ancestor_list = ancestor_last_list.clone();
        child_ancestor_list.push(is_last);
        match entry {
            FlowEntry::Edge(edge) => {
                row_list.push(FlowRenderRow {
                    content: flow_tree_content(
                        &ancestor_last_list,
                        Some(is_last),
                        &flow_edge_content(graph, edge),
                    ),
                    path: flow_target_path(graph, &edge.target),
                    target: PlanReviewTarget::FlowEdge {
                        callable_kind: edge.callable().map(|callable| callable.kind),
                        callable_name: edge.callable().map(|callable| callable.name.clone()),
                        reference_kind: entity_reference_kind(&edge.target),
                        target_name: entity_reference_label(graph, &edge.target),
                        target_is_type: entity_reference_is_type(graph, &edge.target),
                        workspace_path: entity_reference_workspace_path(&edge.target),
                        workspace_line: entity_reference_workspace_line(&edge.target),
                    },
                    json_path: entry_path.clone(),
                    label: format!("Flow edge: {}", flow_relation_label(&edge.relation())),
                });

                let child_count = edge.expansion.len() + edge.branches.len();
                for (branch_index, branch) in edge.branches.iter().enumerate().rev() {
                    pending.push((
                        FlowEntry::Branch(branch),
                        format!("{entry_path}/branches/{branch_index}"),
                        child_ancestor_list.clone(),
                        edge.expansion.len() + branch_index + 1 == child_count,
                    ));
                }
                for (step_index, step) in edge.expansion.iter().enumerate().rev() {
                    pending.push((
                        FlowEntry::Edge(step),
                        format!("{entry_path}/expansion/{step_index}"),
                        child_ancestor_list.clone(),
                        step_index + 1 == child_count,
                    ));
                }
            }
            FlowEntry::Branch(branch) => {
                row_list.push(FlowRenderRow {
                    content: flow_tree_content(
                        &ancestor_last_list,
                        Some(is_last),
                        &format!("when {}", branch.condition),
                    ),
                    path: None,
                    target: PlanReviewTarget::FlowBranch {
                        condition: branch.condition.clone(),
                    },
                    json_path: entry_path.clone(),
                    label: format!("Flow branch: {}", branch.condition),
                });

                for (edge_index, edge) in branch.edges.iter().enumerate().rev() {
                    pending.push((
                        FlowEntry::Edge(edge),
                        format!("{entry_path}/edges/{edge_index}"),
                        child_ancestor_list.clone(),
                        edge_index + 1 == branch.edges.len(),
                    ));
                }
            }
        }
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

fn flow_edge_content(graph: &PlanGraph<'_>, edge: &PlanFlowEdge) -> String {
    let target = entity_reference_label(graph, &edge.target);
    let content = match &edge.operation {
        super::PlanFlowOperation::Construct => format!("Construct {target}"),
        super::PlanFlowOperation::Call(call) => format!("Call {target}.{}()", call.callable.name),
        super::PlanFlowOperation::Read(call) => format!("Read {target}.{}()", call.callable.name),
        super::PlanFlowOperation::Write(call) => format!("Write {target}.{}()", call.callable.name),
        super::PlanFlowOperation::Send(payload) => format!("Send {payload} to {target}"),
        super::PlanFlowOperation::Emit(_) => format!("Emit to {target}"),
        super::PlanFlowOperation::Return(_) => format!("Return to {target}"),
    };
    if let Some(return_type) = edge.return_type() {
        format!("{content} → {}", return_type.value_type)
    } else {
        content
    }
}

const fn flow_relation_label(relation: &PlanFlowRelation) -> &'static str {
    match relation {
        PlanFlowRelation::Construct => "Construct",
        PlanFlowRelation::Call => "Call",
        PlanFlowRelation::Read => "Read",
        PlanFlowRelation::Write => "Write",
        PlanFlowRelation::Send => "Send",
        PlanFlowRelation::Emit => "Emit",
        PlanFlowRelation::Return => "Return",
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
    if content.chars().count() < owner_column
        && owner_column + owner.chars().count() <= PLAN_LINE_WIDTH
    {
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
        let cells = diagram_entity_group(
            document,
            graph,
            |kind| !is_contract_kind(kind),
            PLAN_LINE_WIDTH,
        );
        for cell in cells {
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
            graph.presentation_parent(&entity.name).is_none() && include(entity.kind)
        })
        .collect::<Vec<_>>();
    entity_list.sort_by_key(|(_, entity)| {
        graph
            .presentation_position(&entity.name)
            .map(|position| position.preorder_rank())
            .unwrap_or(usize::MAX)
    });
    if entity_list.is_empty() {
        let cells = vec![DiagramCell {
            text: "<none>".into(),
            anchor: object_model_anchor(),
        }];
        return cells;
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
    for (entity_index, _) in entity_list {
        append_entity_cells(
            &mut cell_list,
            document,
            graph,
            entity_index,
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
    depth: usize,
    width: usize,
    path_column: usize,
) {
    let mut pending = vec![(entity_index, depth)];
    while let Some((entity_index, current_depth)) = pending.pop() {
        if current_depth > depth {
            cell_list.push(DiagramCell {
                text: String::new(),
                anchor: object_model_anchor(),
            });
        }
        let depth = current_depth;
        let entity = &document.entity_changes[entity_index];
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
                name: entity.name.clone(),
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
            append_diagram_member(
                cell_list,
                &format!("{indent}  "),
                member,
                width,
                DiagramAnchor {
                    target: PlanReviewTarget::EntityMember {
                        entity: entity.name.clone(),
                        member: member.name.clone(),
                    },
                    json_path: format!("{entity_path}/members/{member_index}"),
                    path: Some(entity.path.clone()),
                    label: format!("{}::{}", entity.name, member.name),
                },
            );
        }
        for (variant_index, variant) in entity.variants.iter().enumerate() {
            let variant_path = format!("{entity_path}/variants/{variant_index}");
            let variant_declaration = variant.name.clone();
            append_diagram_text(
                cell_list,
                &format!("{indent}  "),
                &format!("{indent}  "),
                &variant_declaration,
                width,
                DiagramAnchor {
                    target: PlanReviewTarget::EnumVariant {
                        entity: entity.name.clone(),
                        variant: variant.name.clone(),
                    },
                    json_path: variant_path.clone(),
                    path: Some(entity.path.clone()),
                    label: format!("{}::{}", entity.name, variant.name),
                },
            );
            for (field_index, field) in variant.fields.iter().enumerate() {
                let field_declaration = format!("{}: {}", field.name, field.type_name);
                append_diagram_text(
                    cell_list,
                    &format!("{indent}    "),
                    &format!("{indent}    "),
                    &field_declaration,
                    width,
                    DiagramAnchor {
                        target: PlanReviewTarget::EnumVariantField {
                            entity: entity.name.clone(),
                            variant: variant.name.clone(),
                            field: field.name.clone(),
                        },
                        json_path: format!("{variant_path}/fields/{field_index}"),
                        path: Some(entity.path.clone()),
                        label: format!("{}::{}::{}", entity.name, variant.name, field.name),
                    },
                );
            }
        }
        pending.extend(
            graph
                .presentation_child_indices(&entity.name)
                .collect::<Vec<_>>()
                .into_iter()
                .rev()
                .map(|child_index| (child_index, depth + 1)),
        );
    }
}

fn append_diagram_member(
    cell_list: &mut Vec<DiagramCell>,
    indent: &str,
    member: &ProgramEntityMemberChange,
    width: usize,
    anchor: DiagramAnchor,
) {
    let declaration = format!(
        "{} {}",
        visibility_marker(member.visibility),
        member_signature(member)
    );
    if !matches!(member.kind, MemberKind::Method | MemberKind::Function)
        || member.parameters.is_empty()
        || indent.chars().count() + declaration.chars().count() <= width
    {
        append_diagram_text(cell_list, indent, indent, &declaration, width, anchor);
        return;
    }
    cell_list.push(DiagramCell {
        text: format!(
            "{indent}{} {}(",
            visibility_marker(member.visibility),
            member.name
        ),
        anchor: anchor.clone(),
    });
    cell_list.extend(member.parameters.iter().map(|parameter| DiagramCell {
        text: format!("{indent}    {}: {},", parameter.name, parameter.type_name),
        anchor: anchor.clone(),
    }));
    let return_type = member
        .return_type
        .as_deref()
        .map(|value| format!(": {value}"))
        .unwrap_or_default();
    cell_list.push(DiagramCell {
        text: format!("{indent}  ){return_type}"),
        anchor,
    });
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
        wrap_prefixed(content, first_prefix, continuation_prefix, width).map(|text| DiagramCell {
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
        ChangeAction::Add | ChangeAction::Modify | ChangeAction::Rename => "*",
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
                name: dependency.name.clone(),
            };
            let is_last = dependency_position + 1 == dependency_list.len();
            renderer.push_wrapped(
                [
                    if is_last { "└─ " } else { "├─ " },
                    if is_last { "   " } else { "│  " },
                ],
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
    renderer.section("# Tasks", PlanSection::Tasks, "/stages");
    renderer.blank();
    for (stage_index, stage) in document.stages.iter().enumerate() {
        renderer.push_wrapped(
            [&format!("{}. ", stage_index + 1), "   "],
            &stage.title,
            PlanReviewTarget::Stage {
                id: stage.id.clone(),
                title: stage.title.clone(),
            },
            format!("/stages/{stage_index}"),
            None,
            stage.title.clone(),
        );
        for (task_index, task) in stage.tasks.iter().enumerate() {
            let task_path = format!("/stages/{stage_index}/tasks/{task_index}");
            let branch = if task_index + 1 == stage.tasks.len() {
                "└─"
            } else {
                "├─"
            };
            let continuation = if task_index + 1 == stage.tasks.len() {
                "   "
            } else {
                "│  "
            };
            let label = document.task_label(&task.id).unwrap();
            let heading = format!("   {branch} {label}. ");
            let content_prefix =
                format!("   {continuation}{}", " ".repeat(label.chars().count() + 2));
            renderer.push_wrapped(
                [&heading, &content_prefix],
                &task.title,
                PlanReviewTarget::Task {
                    title: task.title.clone(),
                },
                &task_path,
                None,
                format!("Task: {}", task.title),
            );
            renderer.push_wrapped(
                [&content_prefix, &content_prefix],
                &task.description,
                PlanReviewTarget::TaskDetail,
                format!("{task_path}/description"),
                None,
                task.title.clone(),
            );
            for (file_index, file) in task.files.iter().enumerate() {
                let file_path = format!("{task_path}/files/{file_index}");
                let path = file.change.path();
                renderer.push(
                    format!(
                        "{content_prefix}{} file {path}",
                        if file_index + 1 == task.files.len() {
                            "└─"
                        } else {
                            "├─"
                        }
                    ),
                    PlanReviewTarget::File {
                        path: path.to_owned(),
                    },
                    &file_path,
                    Some(path),
                    path,
                );
                for (subtask_index, subtask) in file.subtasks.iter().enumerate() {
                    let subtask_path = format!("{file_path}/subtasks/{subtask_index}");
                    let subtask_is_last = subtask_index + 1 == file.subtasks.len();
                    let file_continuation = if file_index + 1 == task.files.len() {
                        "   "
                    } else {
                        "│  "
                    };
                    let subtask_prefix = format!(
                        "{content_prefix}{file_continuation}{} ",
                        if subtask_is_last { "└─" } else { "├─" }
                    );
                    let subtask_continuation = format!(
                        "{content_prefix}{file_continuation}{}",
                        if subtask_is_last { "   " } else { "│  " }
                    );
                    match subtask {
                        PlanSubtask::Work(subtask) => {
                            renderer.push_wrapped(
                                [&subtask_prefix, &subtask_continuation],
                                &format!("{} {}", subtask.action.label(), subtask.description),
                                PlanReviewTarget::Subtask {
                                    path: path.to_owned(),
                                },
                                &subtask_path,
                                Some(path),
                                format!("{path}: {}", subtask.description),
                            );
                            let entity_list =
                                graph.entities_in_presentation_order(&subtask.entities);
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
                                    [&entity_prefix, &entity_continuation],
                                    &format!(
                                        "{} — {}",
                                        entity_change_label(entity),
                                        entity.description
                                    ),
                                    PlanReviewTarget::Entity {
                                        name: entity.name.clone(),
                                    },
                                    entity_json_path(document, &entity.name),
                                    Some(&entity.path),
                                    entity_change_label(entity),
                                );
                            }
                        }
                        PlanSubtask::Test(test) => {
                            renderer.push_wrapped(
                                [&subtask_prefix, &subtask_continuation],
                                &format!(
                                    "{} {}Test `{}` — {}",
                                    action_label(test.action),
                                    test_category_label(test.category),
                                    test.name,
                                    test.behavior
                                ),
                                PlanReviewTarget::Test {
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
        }
        renderer.blank();
    }
}

fn render_tests(renderer: &mut PlanRenderer, document: &PlanDocument) {
    renderer.section("# Tests", PlanSection::Tests, "/tasks");
    renderer.blank();
    let mut file_group_list = Vec::new();
    for task_entry in document.task_entries() {
        let task = task_entry.task();
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
                file_group_list.push((task_entry.path().to_owned(), file_index, file, test_list));
            }
        }
    }
    if file_group_list.is_empty() {
        renderer.section("<none>", PlanSection::Tests, "/tasks");
        renderer.blank();
        return;
    }
    let file_group_count = file_group_list.len();
    for (group_index, (task_path, file_index, file, test_list)) in
        file_group_list.into_iter().enumerate()
    {
        let path = file.change.path();
        renderer.push(
            format!("file {path}"),
            PlanReviewTarget::File {
                path: path.to_owned(),
            },
            format!("{task_path}/files/{file_index}"),
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
                    category: test.category,
                },
                format!("{}/files/{file_index}/subtasks/{subtask_index}", task_path),
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
        PlanSection::Objective => "Objective",
        PlanSection::Requirements => "Requirements",
        PlanSection::Background => "Background",
        PlanSection::Decisions => "Decisions",
        PlanSection::Design => "Design",
        PlanSection::Flows => "Flows",
        PlanSection::Overview => "Overview",
        PlanSection::Verification => "Verification",
        PlanSection::AutomatedVerification => "Checks",
        PlanSection::ManualVerification => "Verification",
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
        EntityKind::Constant => "constant",
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
        ChangeAction::Rename => "Rename",
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

fn entity_json_path(document: &PlanDocument, entity_name: &str) -> String {
    document
        .entity_changes
        .iter()
        .position(|entity| entity.name == entity_name)
        .map(|index| format!("/entity_changes/{index}"))
        .unwrap_or_else(|| "/entity_changes".into())
}

#[cfg(test)]
mod test {

    use super::*;
    use crate::plan::test_fixture;

    #[test]
    fn anchors_every_nonblank_line_to_a_json_path() {
        let overview = "Overview\n\n```rust\nfn example() {}\n```\nUnicode: 日本語\n";
        let rendered = render_plan(&test_fixture("plan", overview)).unwrap();
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
        assert!(rendered.markdown.contains(overview));
        let source_row: Vec<_> = rendered.markdown.split('\n').collect();
        let anchored_row: Vec<_> = rendered
            .navigation
            .anchor
            .iter()
            .map(|anchor| anchor.line as usize)
            .collect();
        let nonblank_row: Vec<_> = source_row
            .iter()
            .enumerate()
            .filter_map(|(index, row)| (!row.is_empty()).then_some(index + 1))
            .collect();
        assert_eq!(anchored_row, nonblank_row);
        for anchor in &rendered.navigation.anchor {
            if matches!(anchor.target, PlanReviewTarget::Task { .. }) {
                assert!(source_row[anchor.line as usize - 1].starts_with("   └─ 1a. "));
            }
        }
    }
}
