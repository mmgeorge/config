use super::document::*;
use anyhow::Result;
use serde::{Deserialize, Serialize};

/// Maps one rendered line range back to its canonical plan node and source boundary.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct PlanNavigationTarget {
    pub first_line: u32,
    pub last_line: u32,
    pub kind: String,
    pub id: String,
    pub path: Option<String>,
    pub task_id: Option<String>,
    pub subtask_id: Option<String>,
    pub definition_id: Option<String>,
    pub member_id: Option<String>,
}

/// Owns stable source-navigation targets for one rendered plan revision.
#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub struct PlanNavigationIndex {
    pub target: Vec<PlanNavigationTarget>,
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

impl PlanRenderer {
    fn new() -> Self {
        Self {
            line: Vec::new(),
            navigation: PlanNavigationIndex::default(),
        }
    }

    fn push(&mut self, value: impl Into<String>) {
        self.line.push(value.into());
    }

    fn mark(
        &mut self,
        first_line: usize,
        kind: &str,
        id: &str,
        path: Option<&str>,
        task_id: Option<&str>,
        subtask_id: Option<&str>,
        definition_id: Option<&str>,
        member_id: Option<&str>,
    ) {
        self.navigation.target.push(PlanNavigationTarget {
            first_line: first_line as u32,
            last_line: self.line.len() as u32,
            kind: kind.into(),
            id: id.into(),
            path: path.map(str::to_owned),
            task_id: task_id.map(str::to_owned),
            subtask_id: subtask_id.map(str::to_owned),
            definition_id: definition_id.map(str::to_owned),
            member_id: member_id.map(str::to_owned),
        });
    }

    fn finish(self) -> RenderedPlan {
        RenderedPlan {
            markdown: self.line.join("\n") + "\n",
            navigation: self.navigation,
        }
    }
}

/// Render one canonical plan into reviewer-readable Markdown and navigation metadata.
pub fn render_plan(document: &PlanDocument) -> Result<RenderedPlan> {
    document.validate()?;
    let mut renderer = PlanRenderer::new();
    renderer.push(format!("# {}", document.title));
    renderer.push("");
    renderer.push("# Overview");
    renderer.push("");
    renderer.push(&document.overview);
    renderer.push("");
    render_usage(&mut renderer, document.usage.as_ref());
    render_diagrams(&mut renderer, document);
    render_tasks(&mut renderer, document);
    render_validation(&mut renderer);
    render_test_plan(&mut renderer, &document.test_plan);
    render_assumptions(&mut renderer, &document.assumptions);
    Ok(renderer.finish())
}

fn render_usage(renderer: &mut PlanRenderer, usage: Option<&PlanUsage>) {
    renderer.push("# Usage");
    renderer.push("");
    match usage {
        Some(usage) if usage.kind != PlanUsageKind::Omitted => {
            renderer.push("```text");
            renderer.push(&usage.input);
            renderer.push("```");
            renderer.push("");
            renderer.push("Expected result:");
            renderer.push("");
            renderer.push("```text");
            renderer.push(&usage.expected_result);
            renderer.push("```");
        }
        _ => renderer.push("<Omitted>"),
    }
    renderer.push("");
}

fn render_diagrams(renderer: &mut PlanRenderer, document: &PlanDocument) {
    renderer.push("# Diagrams");
    renderer.push("");
    renderer.push(
        "The object model establishes ownership before the affected flows cross those boundaries.",
    );
    renderer.push("");
    renderer.push("## Object model and ownership");
    renderer.push("");
    renderer.push("```text");
    renderer.push("Contracts                                 Concrete");
    renderer.push("");
    let mut definition_list = document.definitions.iter().collect::<Vec<_>>();
    definition_list.sort_by_key(|definition| {
        (
            definition_column(definition.kind),
            definition.name.to_lowercase(),
        )
    });
    if !definition_list
        .iter()
        .any(|definition| definition_column(definition.kind) == 0)
    {
        renderer.push("<none>");
    }
    for definition in definition_list {
        let first_line = renderer.line.len() + 1;
        let indent = if definition.exclusive_parent_id.is_some() {
            "  "
        } else {
            ""
        };
        let declaration = definition_declaration(definition);
        if definition_column(definition.kind) == 0 {
            renderer.push(format!("{indent}{declaration}"));
        } else {
            renderer.push(format!(
                "                                          {indent}{declaration}"
            ));
        }
        renderer.push(format!("{indent}  [{}]", definition.path));
        let mut member_list = definition.members.iter().collect::<Vec<_>>();
        member_list.sort_by_key(|member| (member.visibility, member.name.to_lowercase()));
        for member in member_list {
            renderer.push(format!(
                "{indent}  {} {}",
                visibility_marker(member.visibility),
                member_signature(member)
            ));
        }
        let mut variant_list = definition.variants.iter().collect::<Vec<_>>();
        variant_list.sort_by_key(|variant| variant.name.to_lowercase());
        for variant in variant_list {
            renderer.push(format!("{indent}  {}", variant.name));
            for field in &variant.fields {
                renderer.push(format!("{indent}    + {}: {}", field.name, field.type_name));
            }
        }
        renderer.push("");
        renderer.mark(
            first_line,
            "definition",
            &definition.id,
            Some(&definition.path),
            None,
            None,
            Some(&definition.id),
            None,
        );
    }
    renderer.push("```");
    renderer.push("");

    let mut flow_list = document.flows.iter().collect::<Vec<_>>();
    flow_list.sort_by_key(|flow| flow.order);
    for flow in flow_list {
        let first_line = renderer.line.len() + 1;
        renderer.push(format!("## Code flow: {}", flow.title));
        renderer.push("");
        renderer.push(&flow.rationale);
        renderer.push("");
        renderer.push("```text");
        let mut step_list = flow.steps.iter().collect::<Vec<_>>();
        step_list.sort_by_key(|step| step.order);
        for (index, step) in step_list.iter().enumerate() {
            let arrow = step
                .value_to_next
                .as_deref()
                .map(|value| format!(" ─{value}▶"))
                .unwrap_or_default();
            renderer.push(format!(
                "{}  {}{}",
                if index == 0 { "Flow" } else { "    " },
                step.action,
                arrow
            ));
            renderer.push(format!("      [{}]", step.location));
        }
        renderer.push("```");
        renderer.push("");
        renderer.mark(first_line, "flow", &flow.id, None, None, None, None, None);
    }
}

fn render_tasks(renderer: &mut PlanRenderer, document: &PlanDocument) {
    renderer.push("# Tasks");
    renderer.push("");
    let mut task_list = document.tasks.iter().collect::<Vec<_>>();
    task_list.sort_by_key(|task| task.order);
    for (task_index, task) in task_list.iter().enumerate() {
        let first_line = renderer.line.len() + 1;
        renderer.push(format!(
            "{}. **{}** {}",
            task_index + 1,
            task.title,
            task.rationale
        ));
        renderer.push("");
        for file in &task.files {
            renderer.push(format!("file {}", file.path));
            let mut subtask_list = file.subtasks.iter().collect::<Vec<_>>();
            subtask_list.sort_by_key(|subtask| subtask.order);
            for subtask in subtask_list {
                let subtask_first = renderer.line.len() + 1;
                renderer.push(format!("└─ {} {}", subtask.title, subtask.detail));
                for edit in &subtask.code_edits {
                    renderer.push(format!(
                        "   ├─ {} {} `{}` — {}",
                        action_label(edit.action),
                        code_kind_label(edit.kind),
                        edit.target,
                        edit.description
                    ));
                }
                renderer.mark(
                    subtask_first,
                    "subtask",
                    &subtask.id,
                    Some(&file.path),
                    Some(&task.id),
                    Some(&subtask.id),
                    None,
                    None,
                );
            }
            renderer.push("");
        }
        renderer.mark(
            first_line,
            "task",
            &task.id,
            None,
            Some(&task.id),
            None,
            None,
            None,
        );
    }
}

fn render_validation(renderer: &mut PlanRenderer) {
    renderer.push("# Modularity, testability, and plan validation");
    renderer.push("");
    renderer.push("Every changed construct maps to a task, every task owns concrete source boundaries, and each major flow links back to the object model and verification plan.");
    renderer.push("");
}

fn render_test_plan(renderer: &mut PlanRenderer, test_plan: &PlanTestPlan) {
    renderer.push("# Test plan");
    renderer.push("");
    renderer.push("## Unit tests");
    renderer.push("");
    for test in &test_plan.unit {
        renderer.push(format!("- **{}**: {}", test.title, test.behavior));
    }
    if test_plan.unit.is_empty() {
        renderer.push("- <none>");
    }
    renderer.push("");
    renderer.push("## Integration tests");
    renderer.push("");
    for test in &test_plan.integration {
        renderer.push(format!("- **{}**: {}", test.title, test.behavior));
    }
    if test_plan.integration.is_empty() {
        renderer.push("- <none>");
    }
    renderer.push("");
}

fn render_assumptions(renderer: &mut PlanRenderer, assumption_list: &[PlanAssumption]) {
    renderer.push("# Assumptions");
    renderer.push("");
    for assumption in assumption_list {
        renderer.push(format!("- {}", assumption.text));
    }
    if assumption_list.is_empty() {
        renderer.push("- <none>");
    }
}

fn definition_column(kind: DefinitionKind) -> u8 {
    match kind {
        DefinitionKind::Trait | DefinitionKind::Interface | DefinitionKind::AbstractClass => 0,
        _ => 1,
    }
}

fn definition_declaration(definition: &TypeDefinition) -> String {
    let marker = match definition.action {
        PlanAction::Remove => "~",
        _ => "*",
    };
    let kind = match definition.kind {
        DefinitionKind::Trait => "trait",
        DefinitionKind::Interface => "interface",
        DefinitionKind::AbstractClass => "abstract class",
        DefinitionKind::Class => "class",
        DefinitionKind::Struct => "struct",
        DefinitionKind::Enum => "enum",
        DefinitionKind::Config => "config",
        DefinitionKind::Resource => "resource",
        DefinitionKind::TypeAlias => "type",
    };
    let mut result = format!("{marker}{kind} {}", definition.name);
    if let Some(parent) = definition.extends.as_deref() {
        result.push_str(&format!(" extends {parent}"));
    }
    if !definition.conforms_to.is_empty() {
        result.push_str(&format!(": {}", definition.conforms_to.join(", ")));
    }
    result
}

fn visibility_marker(visibility: Visibility) -> &'static str {
    match visibility {
        Visibility::Public => "+",
        _ => "-",
    }
}

fn member_signature(member: &TypeMember) -> String {
    if matches!(member.kind, MemberKind::Method | MemberKind::Function) {
        let parameters = member
            .parameters
            .iter()
            .map(|parameter| format!("{}: {}", parameter.name, parameter.type_name))
            .collect::<Vec<_>>()
            .join(", ");
        let result = member
            .return_type
            .as_deref()
            .map(|value| format!(": {value}"))
            .unwrap_or_default();
        format!("{}({parameters}){result}", member.name)
    } else {
        member
            .type_name
            .as_deref()
            .map(|value| format!("{}: {value}", member.name))
            .unwrap_or_else(|| member.name.clone())
    }
}

fn action_label(action: PlanAction) -> &'static str {
    match action {
        PlanAction::Add => "Add",
        PlanAction::Modify => "Modify",
        PlanAction::Remove => "Remove",
    }
}

fn code_kind_label(kind: CodeKind) -> &'static str {
    match kind {
        CodeKind::Class => "class",
        CodeKind::Struct => "struct",
        CodeKind::Enum => "enum",
        CodeKind::Trait => "trait",
        CodeKind::Interface => "interface",
        CodeKind::Test => "test",
        CodeKind::App => "app",
        CodeKind::Config => "config",
        CodeKind::Function => "fn",
        CodeKind::Method => "method",
        CodeKind::Constant => "constant",
        CodeKind::Field => "field",
        CodeKind::Resource => "Resource",
        CodeKind::Cache => "Cache",
        CodeKind::Adapter => "Adapter",
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn renders_required_sections_deterministically() {
        let document = PlanDocument {
            version: 1,
            plan_id: "plan".into(),
            title: "Plan".into(),
            overview: "Overview".into(),
            usage: None,
            definitions: Vec::new(),
            flows: Vec::new(),
            tasks: Vec::new(),
            test_plan: PlanTestPlan::default(),
            assumptions: Vec::new(),
        };
        let first = render_plan(&document).unwrap();
        let second = render_plan(&document).unwrap();
        assert_eq!(first, second);
        assert!(first.markdown.contains("# Overview"));
        assert!(first.markdown.contains("# Test plan"));
    }
}
