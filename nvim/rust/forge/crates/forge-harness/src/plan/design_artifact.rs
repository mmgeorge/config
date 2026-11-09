//! Holds the plan's authored Markdown artifacts and parses them into `DesignDocument` at submission.
//!
//! An artifact edit stores the agent's text verbatim, so every submission diagnostic names a line
//! the agent actually wrote. Submission is the only path from artifact text to `DesignDocument`.
//! An artifact without stored text renders from the document, which keeps Harness-side writers
//! consistent: the review check toggle and test removal change the document and drop the stored
//! text, and the next read renders the result in the same grammar the agent edits.

use std::collections::{BTreeMap, BTreeSet, HashSet};

use anyhow::{Context, Result, bail};

use super::design::DeclarationDesign;
use super::design_document::{DesignDecision, DesignDocument};
use super::design_tests::{DesignTestCase, DesignTestChange, DesignTestFile};
use crate::declaration::{DeclarationDiagnostic, DeclarationDiagnosticKind};

/// Reserves the artifact namespace so it can never collide with a workspace path.
pub(crate) const ARTIFACT_PREFIX: &str = "plan:";

type ArtifactSection<'text> = (usize, Vec<(usize, &'text str)>);
type ArtifactFlowBlock<'text> = (usize, &'text str, Vec<(usize, &'text str)>);

/// Names one Harness-owned plan artifact addressed through the plan file tools.
#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub(crate) enum PlanArtifact {
    /// Holds the title and the prose sections of the overview.
    Overview,
    /// Holds runtime flows, each a short description and a Mermaid diagram.
    Flows,
    /// Holds the planned test inventory grouped by file.
    Tests,
    /// Lists configured checks with the plan's selection as checkboxes.
    Checks,
    /// Lists execution findings that Resolve must address; read-only.
    Findings,
    /// Lists the tool results a Verify report may cite; read-only.
    Evidence,
    /// Holds the Verify report, which the agent writes only in Verify.
    Verification,
}

impl PlanArtifact {
    /// Artifacts the planning agent writes, in presentation order.
    pub(crate) const AUTHORED: [Self; 4] = [Self::Overview, Self::Flows, Self::Tests, Self::Checks];

    /// Every artifact, in presentation order.
    const ALL: [Self; 7] = [
        Self::Overview,
        Self::Flows,
        Self::Tests,
        Self::Checks,
        Self::Findings,
        Self::Evidence,
        Self::Verification,
    ];

    /// Return the reserved path that addresses this artifact.
    pub(crate) fn path(self) -> &'static str {
        match self {
            Self::Overview => "plan:overview.md",
            Self::Flows => "plan:flows.md",
            Self::Tests => "plan:tests.md",
            Self::Checks => "plan:checks.md",
            Self::Findings => "plan:findings.md",
            Self::Evidence => "plan:evidence.md",
            Self::Verification => "plan:verification.md",
        }
    }

    /// Resolve a reserved path, rejecting unknown names inside the artifact namespace.
    pub(crate) fn from_path(path: &str) -> Result<Option<Self>> {
        if !path.starts_with(ARTIFACT_PREFIX) {
            return Ok(None);
        }
        Self::ALL
            .into_iter()
            .find(|artifact| artifact.path() == path)
            .map(Some)
            .with_context(|| {
                format!(
                    "unknown plan artifact {path}; artifacts are {}",
                    Self::ALL.map(Self::path).join(", ")
                )
            })
    }

    /// Report whether planning edits may change this artifact.
    pub(crate) fn authored(self) -> bool {
        Self::AUTHORED.contains(&self)
    }
}

/// Build one diagnostic at the start of an artifact line.
pub(crate) fn artifact_diagnostic(
    path: &str,
    line: usize,
    kind: DeclarationDiagnosticKind,
    reference: &str,
    reason: String,
) -> DeclarationDiagnostic {
    DeclarationDiagnostic {
        path: path.to_owned(),
        line: u32::try_from(line).unwrap_or(u32::MAX),
        column: 0,
        reference: reference.to_owned(),
        error: true,
        reason,
        kind,
    }
}

/// Collects the diagnostics of one artifact so parsers can continue past each problem.
struct ArtifactReport<'report> {
    path: &'static str,
    diagnostic: &'report mut Vec<DeclarationDiagnostic>,
}

impl ArtifactReport<'_> {
    fn syntax(&mut self, line: usize, reference: &str, reason: String) {
        self.diagnostic.push(artifact_diagnostic(
            self.path,
            line,
            DeclarationDiagnosticKind::ArtifactSyntax,
            reference,
            reason,
        ));
    }

    fn content(&mut self, line: usize, reference: &str, reason: String) {
        self.diagnostic.push(artifact_diagnostic(
            self.path,
            line,
            DeclarationDiagnosticKind::ArtifactContent,
            reference,
            reason,
        ));
    }

    fn check(&mut self, line: usize, reference: &str, reason: String) {
        self.diagnostic.push(artifact_diagnostic(
            self.path,
            line,
            DeclarationDiagnosticKind::PlanCheck,
            reference,
            reason,
        ));
    }
}

/// Get an authored artifact's current text: the agent's stored text, or the document's
/// rendering when the agent has not written it.
pub(crate) fn authored_text(
    artifact: PlanArtifact,
    title: &str,
    design: &DeclarationDesign,
) -> Result<String> {
    if let Some(text) = design.artifact_text.get(artifact.path()) {
        return Ok(text.clone());
    }
    render(artifact, title, &design.document)
}

/// Render an authored artifact from the document in the grammar the agent edits.
fn render(artifact: PlanArtifact, title: &str, document: &DesignDocument) -> Result<String> {
    Ok(match artifact {
        PlanArtifact::Overview => overview_text(title, document),
        PlanArtifact::Flows => document.flows.clone(),
        PlanArtifact::Tests => tests_text(&document.tests),
        PlanArtifact::Checks => checks_text(document),
        PlanArtifact::Findings | PlanArtifact::Evidence | PlanArtifact::Verification => {
            bail!("{} is generated from execution state", artifact.path())
        }
    })
}

/// Store an edited artifact verbatim, keeping the plan title in step with the overview heading.
///
/// Text identical to the document's rendering is dropped instead of stored, so an edit that
/// restores the rendering leaves no trace.
pub(crate) fn store_authored(
    artifact: PlanArtifact,
    text: String,
    title: &mut String,
    design: &mut DeclarationDesign,
) -> Result<()> {
    if artifact == PlanArtifact::Overview
        && let Some(heading) = text
            .lines()
            .find_map(|line| line.strip_prefix("# "))
            .map(str::trim)
            .filter(|heading| !heading.is_empty())
    {
        *title = heading.to_owned();
    }
    if render(artifact, title, &design.document)? == text {
        design.artifact_text.remove(artifact.path());
    } else {
        design
            .artifact_text
            .insert(artifact.path().to_owned(), text);
    }
    Ok(())
}

/// Drop an artifact's stored text so it renders from the document again.
pub(crate) fn revert_authored(artifact: PlanArtifact, design: &mut DeclarationDesign) {
    design.artifact_text.remove(artifact.path());
}

/// Name the authored artifacts whose text or document content differs between two designs.
pub(crate) fn changed_artifacts(
    before_title: &str,
    before: &DeclarationDesign,
    after_title: &str,
    after: &DeclarationDesign,
) -> Vec<PlanArtifact> {
    let (old, new) = (&before.document, &after.document);
    let document_changed = |artifact: PlanArtifact| match artifact {
        PlanArtifact::Overview => {
            before_title != after_title
                || old.objective != new.objective
                || old.usage != new.usage
                || old.requirements != new.requirements
                || old.background != new.background
                || old.decisions != new.decisions
                || old.design != new.design
                || old.manual_checks != new.manual_checks
        }
        PlanArtifact::Flows => old.flows != new.flows,
        PlanArtifact::Tests => old.tests != new.tests,
        PlanArtifact::Checks => {
            old.checks != new.checks || old.disabled_checks != new.disabled_checks
        }
        _ => false,
    };
    PlanArtifact::AUTHORED
        .into_iter()
        .filter(|artifact| {
            before.artifact_text.get(artifact.path()) != after.artifact_text.get(artifact.path())
                || document_changed(*artifact)
        })
        .collect()
}

/// Holds the result of parsing every authored artifact for one submission.
///
/// The plan title needs no field here: every overview edit already copies its `# Title` line
/// into the plan, and a missing title is one of the diagnostics.
pub(crate) struct ParsedArtifacts {
    /// Document assembled from the artifacts, complete only when `diagnostic` has no errors.
    pub(crate) document: DesignDocument,
    /// Every problem found, each at the artifact line that caused it.
    pub(crate) diagnostic: Vec<DeclarationDiagnostic>,
}

/// Parse every authored artifact into a document, reporting each problem at its line.
///
/// A clean parse produces a document that `DesignDocument::validate_for_submission` accepts,
/// because these checks cover every rule it enforces.
pub(crate) fn parse_authored(
    title: &str,
    design: &DeclarationDesign,
    config: &crate::checks::CheckConfig,
) -> Result<ParsedArtifacts> {
    let text = |artifact: PlanArtifact| -> Result<String> {
        match design.artifact_text.get(artifact.path()) {
            Some(text) => Ok(text.clone()),
            None => render(artifact, title, &design.document),
        }
    };
    let mut diagnostic = Vec::new();
    let mut document = DesignDocument::default();
    parse_overview(
        &text(PlanArtifact::Overview)?,
        &mut document,
        true,
        &mut ArtifactReport {
            path: PlanArtifact::Overview.path(),
            diagnostic: &mut diagnostic,
        },
    );
    document.flows = parse_flows(
        &text(PlanArtifact::Flows)?,
        &mut ArtifactReport {
            path: PlanArtifact::Flows.path(),
            diagnostic: &mut diagnostic,
        },
    );
    document.tests = parse_tests(
        &text(PlanArtifact::Tests)?,
        &mut ArtifactReport {
            path: PlanArtifact::Tests.path(),
            diagnostic: &mut diagnostic,
        },
    );
    document.checks = parse_checks(
        &text(PlanArtifact::Checks)?,
        Some(config),
        &mut ArtifactReport {
            path: PlanArtifact::Checks.path(),
            diagnostic: &mut diagnostic,
        },
    );
    document.disabled_checks = design
        .document
        .disabled_checks
        .iter()
        .filter(|id| !document.checks.contains(id))
        .cloned()
        .collect();
    Ok(ParsedArtifacts {
        document,
        diagnostic,
    })
}

/// Parse the authored artifacts without the check configuration or submission rules, for
/// summaries of a draft that may not be valid yet.
pub(crate) fn preview_authored(design: &DeclarationDesign) -> DesignDocument {
    let mut ignored = Vec::new();
    let mut document = design.document.clone();
    let parse = |artifact: PlanArtifact| {
        design
            .artifact_text
            .get(artifact.path())
            .map(|text| (text.clone(), artifact.path()))
    };
    if let Some((text, path)) = parse(PlanArtifact::Overview) {
        let mut overview = DesignDocument::default();
        parse_overview(
            &text,
            &mut overview,
            false,
            &mut ArtifactReport {
                path,
                diagnostic: &mut ignored,
            },
        );
        overview.flows = std::mem::take(&mut document.flows);
        overview.tests = std::mem::take(&mut document.tests);
        overview.checks = std::mem::take(&mut document.checks);
        overview.disabled_checks = std::mem::take(&mut document.disabled_checks);
        document = overview;
    }
    if let Some((text, path)) = parse(PlanArtifact::Flows) {
        document.flows = parse_flows(
            &text,
            &mut ArtifactReport {
                path,
                diagnostic: &mut ignored,
            },
        );
    }
    if let Some((text, path)) = parse(PlanArtifact::Tests) {
        document.tests = parse_tests(
            &text,
            &mut ArtifactReport {
                path,
                diagnostic: &mut ignored,
            },
        );
    }
    if let Some((text, path)) = parse(PlanArtifact::Checks) {
        document.checks = parse_checks(
            &text,
            None,
            &mut ArtifactReport {
                path,
                diagnostic: &mut ignored,
            },
        );
    }
    document
}

/// Present the authored artifacts to a provider prompt as the text the agent edits.
///
/// Implementation prompts omit `## Manual checks` and `plan:checks.md`, which belong to Verify
/// and to Harness's Check phase.
pub(crate) fn model_artifacts(
    title: &str,
    design: &DeclarationDesign,
    include_verification: bool,
) -> Result<BTreeMap<&'static str, String>> {
    let mut artifacts = BTreeMap::new();
    for artifact in PlanArtifact::AUTHORED {
        if artifact == PlanArtifact::Checks && !include_verification {
            continue;
        }
        let text = match design.artifact_text.get(artifact.path()) {
            Some(text) => text.clone(),
            None => render(artifact, title, &design.document)?,
        };
        let text = if artifact == PlanArtifact::Overview && !include_verification {
            without_section(&text, MANUAL_CHECKS)
        } else {
            text
        };
        artifacts.insert(artifact.path(), text);
    }
    Ok(artifacts)
}

/// Remove one `##` section and its body, leaving every other line untouched.
fn without_section(text: &str, heading: &str) -> String {
    let mut kept = Vec::new();
    let mut skipping = false;
    for line in text.lines() {
        if let Some(name) = line.strip_prefix("## ") {
            skipping = name.trim().eq_ignore_ascii_case(heading);
        }
        if !skipping {
            kept.push(line);
        }
    }
    let mut result = kept.join("\n");
    result.push('\n');
    result
}

/// Names the overview section that lists the manual checks Verify performs.
const MANUAL_CHECKS: &str = "Manual checks";

const OVERVIEW_SECTIONS: [&str; 7] = [
    "Objective",
    "Usage",
    "Requirements",
    "Background",
    "Decisions",
    "Design",
    MANUAL_CHECKS,
];

/// Render the title and every overview section, including empty ones, in a fixed order.
fn overview_text(title: &str, document: &DesignDocument) -> String {
    let mut text = format!("# {title}\n");
    for (heading, body) in [
        ("Objective", document.objective.clone()),
        ("Usage", document.usage.clone().unwrap_or_default()),
        ("Requirements", list_text(&document.requirements)),
        ("Background", document.background.clone()),
        (
            "Decisions",
            list_text(
                &document
                    .decisions
                    .iter()
                    .map(|decision| format!("**{}** {}", decision.decision, decision.rationale))
                    .collect::<Vec<_>>(),
            ),
        ),
        ("Design", document.design.clone()),
        (MANUAL_CHECKS, list_text(&document.manual_checks)),
    ] {
        text.push_str(&format!("\n## {heading}\n"));
        let body = body.trim_matches('\n');
        if !body.trim().is_empty() {
            text.push_str(body);
            text.push('\n');
        }
    }
    text
}

/// Render list items as `- item`, indenting continuation lines by two spaces.
fn list_text(items: &[String]) -> String {
    items
        .iter()
        .map(|item| format!("- {}", item.replace('\n', "\n  ")))
        .collect::<Vec<_>>()
        .join("\n")
}

/// Names the part of the overview a line belongs to while parsing.
#[derive(Clone, Copy)]
enum OverviewBody {
    Preamble,
    Skipped,
    Section(&'static str),
}

/// Parse the overview into the document's prose fields and return its title.
///
/// `submission` adds the rules that only a submitted plan must satisfy, such as required
/// sections. The title is `None` when the overview has no usable `# Title` line.
fn parse_overview(
    text: &str,
    document: &mut DesignDocument,
    submission: bool,
    report: &mut ArtifactReport<'_>,
) -> Option<String> {
    let mut title = None;
    let mut body = OverviewBody::Preamble;
    let mut sections: BTreeMap<&'static str, ArtifactSection<'_>> = BTreeMap::new();
    let mut fenced = false;
    for (index, line) in text.lines().enumerate() {
        let number = index + 1;
        if line.contains('\0') {
            report.syntax(number, "NUL", "the line contains a NUL byte".into());
            continue;
        }
        if !fenced {
            if let Some(heading) = line.strip_prefix("## ") {
                let heading = heading.trim();
                body = match OVERVIEW_SECTIONS
                    .into_iter()
                    .find(|section| section.eq_ignore_ascii_case(heading))
                {
                    Some(section) if sections.contains_key(section) => {
                        report.syntax(
                            number,
                            &format!("## {section}"),
                            "the section appears twice; merge it into the first one".into(),
                        );
                        OverviewBody::Skipped
                    }
                    Some(section) => {
                        sections.insert(section, (number, Vec::new()));
                        OverviewBody::Section(section)
                    }
                    None => {
                        report.syntax(
                            number,
                            &format!("## {heading}"),
                            format!(
                                "unknown section; sections are {}",
                                OVERVIEW_SECTIONS.join(", ")
                            ),
                        );
                        OverviewBody::Skipped
                    }
                };
                continue;
            }
            if let Some(heading) = line.strip_prefix("# ") {
                if title.is_none() && matches!(body, OverviewBody::Preamble) {
                    title = Some(heading.trim().to_owned());
                } else {
                    report.syntax(
                        number,
                        &format!("# {}", heading.trim()),
                        "only the first line may be a `# Title` heading; use `##` for sections"
                            .into(),
                    );
                }
                continue;
            }
        }
        if line.trim_start().starts_with("```") {
            fenced = !fenced;
        }
        match body {
            OverviewBody::Preamble => {
                if !line.trim().is_empty() {
                    report.syntax(
                        number,
                        line.trim(),
                        "text must follow `# Title` and a `## Section` heading".into(),
                    );
                }
            }
            OverviewBody::Skipped => {}
            OverviewBody::Section(section) => {
                if let Some((_, lines)) = sections.get_mut(section) {
                    lines.push((number, line));
                }
            }
        }
    }
    let title = title.filter(|title| !title.is_empty());
    if title.is_none() {
        report.syntax(
            1,
            "# Title",
            "the overview must start with a nonempty `# Title` line".into(),
        );
    }
    for (section, (_, lines)) in &sections {
        match *section {
            "Objective" => document.objective = prose(lines),
            "Usage" => document.usage = Some(prose(lines)).filter(|usage| !usage.trim().is_empty()),
            "Requirements" => {
                document.requirements = list_items(section, lines, report)
                    .into_iter()
                    .map(|(_, item)| item)
                    .collect();
            }
            "Background" => document.background = prose(lines),
            "Decisions" => {
                document.decisions = list_items(section, lines, report)
                    .into_iter()
                    .filter_map(|(number, item)| decision(number, &item, report))
                    .collect();
            }
            "Design" => document.design = prose(lines),
            MANUAL_CHECKS => {
                document.manual_checks = list_items(section, lines, report)
                    .into_iter()
                    .map(|(_, item)| item)
                    .collect();
            }
            _ => {}
        }
    }
    if submission {
        for (section, text) in [
            ("Objective", &document.objective),
            ("Background", &document.background),
            ("Design", &document.design),
        ] {
            if text.trim().is_empty() {
                let line = sections.get(section).map_or(1, |(line, _)| *line);
                report.content(
                    line,
                    &format!("## {section}"),
                    "the section is required and must not be empty".into(),
                );
            }
        }
    }
    title
}

/// Join section lines verbatim after removing blank lines at either edge.
fn prose(lines: &[(usize, &str)]) -> String {
    let start = lines
        .iter()
        .position(|(_, line)| !line.trim().is_empty())
        .unwrap_or(lines.len());
    let end = lines
        .iter()
        .rposition(|(_, line)| !line.trim().is_empty())
        .map_or(start, |index| index + 1);
    lines[start..end]
        .iter()
        .map(|(_, line)| *line)
        .collect::<Vec<_>>()
        .join("\n")
}

/// Parse `- item` lines with two-space continuation lines, keeping each item's first line.
fn list_items(
    section: &str,
    lines: &[(usize, &str)],
    report: &mut ArtifactReport<'_>,
) -> Vec<(usize, String)> {
    let mut items: Vec<(usize, String)> = Vec::new();
    for (number, line) in lines {
        if line.trim().is_empty() {
            continue;
        }
        if let Some(item) = line
            .strip_prefix("- ")
            .or_else(|| (*line == "-").then_some(""))
        {
            items.push((*number, item.trim_end().to_owned()));
        } else if let (Some(continuation), Some((_, item))) =
            (line.strip_prefix("  "), items.last_mut())
        {
            item.push('\n');
            item.push_str(continuation.trim_end());
        } else {
            report.syntax(
                *number,
                line.trim(),
                format!(
                    "`## {section}` items start with `- `, and continuation lines are indented by \
                     two spaces"
                ),
            );
        }
    }
    items.retain(|(number, item)| {
        let empty = item.trim().is_empty();
        if empty {
            report.content(
                *number,
                "-",
                format!("`## {section}` items must not be empty"),
            );
        }
        !empty
    });
    items
}

/// Split a `**Decision.** Rationale.` item into its choice and justification.
fn decision(number: usize, item: &str, report: &mut ArtifactReport<'_>) -> Option<DesignDecision> {
    let parsed = item
        .strip_prefix("**")
        .and_then(|rest| rest.split_once("**"));
    match parsed {
        Some((decision, rationale))
            if !decision.trim().is_empty() && !rationale.trim().is_empty() =>
        {
            Some(DesignDecision {
                decision: decision.trim().into(),
                rationale: rationale.trim().into(),
            })
        }
        _ => {
            report.syntax(
                number,
                item.lines().next().unwrap_or_default(),
                "decisions use `- **Decision.** Rationale.` with both parts present".into(),
            );
            None
        }
    }
}

/// Check every `## Title` flow block and return the flow Markdown unchanged.
fn parse_flows(text: &str, report: &mut ArtifactReport<'_>) -> String {
    let mut blocks: Vec<ArtifactFlowBlock<'_>> = Vec::new();
    let mut fenced = false;
    for (index, line) in text.lines().enumerate() {
        let number = index + 1;
        if !fenced && let Some(title) = line.strip_prefix("## ") {
            blocks.push((number, title.trim(), Vec::new()));
            continue;
        }
        if line.starts_with("```") {
            fenced = !fenced;
        }
        if let Some((_, _, lines)) = blocks.last_mut() {
            lines.push((number, line));
        } else if !line.trim().is_empty() {
            report.syntax(
                number,
                line.trim(),
                "each flow starts with a `## Title` heading".into(),
            );
        }
    }
    let mut titles = HashSet::new();
    for (number, title, lines) in blocks {
        check_flow(number, title, &lines, report);
        if !title.is_empty()
            && !titles.insert(title.split_whitespace().collect::<Vec<_>>().join(" "))
        {
            report.content(
                number,
                &format!("## {title}"),
                "another flow has the same title".into(),
            );
        }
    }
    text.to_owned()
}

/// Check one flow block: a description, then exactly one ```` ```mermaid ```` block whose first
/// line names an allowed diagram type.
fn check_flow(
    heading: usize,
    title: &str,
    lines: &[(usize, &str)],
    report: &mut ArtifactReport<'_>,
) {
    let reference = format!("## {title}");
    if title.is_empty() {
        report.content(heading, "##", "a flow heading needs a title".into());
    }
    let mut remaining = lines.iter().peekable();
    let description =
        std::iter::from_fn(|| remaining.next_if(|(_, line)| !line.starts_with("```")))
            .copied()
            .collect::<Vec<_>>();
    if prose(&description).trim().is_empty() {
        report.content(
            heading,
            &reference,
            "the flow needs a one-to-two-line description before its mermaid block".into(),
        );
    }
    let Some((fence, opening)) = remaining.next() else {
        report.content(
            heading,
            &reference,
            "the flow needs a ```mermaid block after its description".into(),
        );
        return;
    };
    if opening.trim_end() != "```mermaid" {
        report.syntax(
            *fence,
            opening.trim(),
            "flow diagrams are fenced as ```mermaid".into(),
        );
    }
    let mut first_line = None;
    let mut closed = false;
    for (number, line) in remaining.by_ref() {
        if line.trim_end() == "```" {
            closed = true;
            break;
        }
        first_line.get_or_insert((*number, *line));
    }
    if !closed {
        report.syntax(
            *fence,
            opening.trim(),
            "the mermaid block needs a closing ``` line".into(),
        );
    }
    for (number, line) in remaining.filter(|(_, line)| !line.trim().is_empty()) {
        report.syntax(
            *number,
            line.trim(),
            "a flow holds one description and one mermaid block; start another flow with `## Title`"
                .into(),
        );
    }
    let (number, line) = first_line.unwrap_or((*fence, ""));
    if !super::design_flows::diagram_allowed(line) {
        report.content(
            number,
            line.trim(),
            format!(
                "the first line of a flow's mermaid block must start with {}",
                super::design_flows::allowed_keywords()
            ),
        );
    }
}

/// Render the planned tests as one `## file` section per file with one item per test.
fn tests_text(files: &[DesignTestFile]) -> String {
    let blocks = files
        .iter()
        .map(|file| {
            let mut lines = vec![format!("## {}", file.file)];
            for case in &file.cases {
                let target = case
                    .target
                    .as_ref()
                    .map(|target| format!(" [target: {target}]"))
                    .unwrap_or_default();
                lines.push(format!(
                    "- {} `{}`{target}: {}",
                    change_word(case.change),
                    case.name,
                    case.description.replace('\n', "\n  ")
                ));
            }
            lines.join("\n")
        })
        .collect::<Vec<_>>();
    if blocks.is_empty() {
        String::new()
    } else {
        format!("{}\n", blocks.join("\n\n"))
    }
}

fn change_word(change: DesignTestChange) -> &'static str {
    match change {
        DesignTestChange::New => "new",
        DesignTestChange::Modified => "modified",
        DesignTestChange::Removed => "removed",
        DesignTestChange::Reused => "reused",
    }
}

/// Parse one `- change \`name\` [target: t]: description` item.
fn test_case(item: &str) -> Option<DesignTestCase> {
    let (change, rest) = item.split_once(' ')?;
    let change = match change {
        "new" => DesignTestChange::New,
        "modified" => DesignTestChange::Modified,
        "removed" => DesignTestChange::Removed,
        "reused" => DesignTestChange::Reused,
        _ => return None,
    };
    let (name, rest) = rest.trim_start().strip_prefix('`')?.split_once('`')?;
    let mut rest = rest.trim_start();
    let mut target = None;
    if let Some(tagged) = rest.strip_prefix("[target:") {
        let (value, after) = tagged.split_once(']')?;
        target = Some(value.trim().to_owned());
        rest = after.trim_start();
    }
    Some(DesignTestCase {
        name: name.into(),
        target,
        change,
        description: rest.strip_prefix(':')?.trim().into(),
    })
}

/// Parse `## file` sections of test items, reporting malformed items and invalid inventories.
fn parse_tests(text: &str, report: &mut ArtifactReport<'_>) -> Vec<DesignTestFile> {
    const SHAPE: &str = "tests are `- new|modified|removed|reused `name` [target: runner]: \
                         scenario`, with an optional target";
    let mut files: Vec<(usize, DesignTestFile, Vec<usize>)> = Vec::new();
    for (index, line) in text.lines().enumerate() {
        let number = index + 1;
        if line.trim().is_empty() {
            continue;
        }
        if let Some(file) = line.strip_prefix("## ") {
            files.push((
                number,
                DesignTestFile {
                    file: file.trim().into(),
                    cases: Vec::new(),
                },
                Vec::new(),
            ));
            continue;
        }
        let Some((_, file, lines)) = files.last_mut() else {
            report.syntax(
                number,
                line.trim(),
                "tests are grouped under a `## path/to/file` heading".into(),
            );
            continue;
        };
        if let Some(item) = line.strip_prefix("- ") {
            match test_case(item) {
                Some(case) => {
                    file.cases.push(case);
                    lines.push(number);
                }
                None => report.syntax(number, item.trim(), SHAPE.into()),
            }
        } else if let (Some(continuation), Some(case)) =
            (line.strip_prefix("  "), file.cases.last_mut())
        {
            case.description.push('\n');
            case.description.push_str(continuation.trim_end());
        } else {
            report.syntax(number, line.trim(), SHAPE.into());
        }
    }
    let mut paths = BTreeSet::new();
    for (heading, file, lines) in &files {
        let reference = format!("## {}", file.file);
        if let Err(error) = super::design::validate_relative_path(&file.file) {
            report.content(*heading, &reference, error.to_string());
        } else if file.file.chars().any(char::is_control) {
            report.content(*heading, &reference, "the path must be printable".into());
        }
        if !paths.insert(file.file.clone()) {
            report.content(
                *heading,
                &reference,
                "another section lists the same file; merge them".into(),
            );
        }
        if file.cases.is_empty() {
            report.content(*heading, &reference, "the file lists no tests".into());
        }
        let mut names = BTreeSet::new();
        for (case, line) in file.cases.iter().zip(lines) {
            let reference = format!("`{}`", case.name);
            if case.name.is_empty()
                || case.name.trim() != case.name
                || case.name.chars().any(char::is_control)
            {
                report.content(
                    *line,
                    &reference,
                    "the test needs a nonempty, unpadded name".into(),
                );
            }
            if !names.insert(case.name.clone()) {
                report.content(*line, &reference, "the file lists this test twice".into());
            }
            if case
                .target
                .as_ref()
                .is_some_and(|target| target.is_empty() || target.chars().any(char::is_control))
            {
                report.content(*line, &reference, "the target must be a runner name".into());
            }
            if case.description.trim().is_empty() {
                report.content(
                    *line,
                    &reference,
                    "the test needs a scenario and expected result".into(),
                );
            }
        }
    }
    files.into_iter().map(|(_, file, _)| file).collect()
}

/// Render the checks the plan adds, one ID per line.
fn checks_text(document: &DesignDocument) -> String {
    document.checks.iter().map(|id| format!("{id}\n")).collect()
}

/// Parse one check ID per line into the checks the plan adds to the automatic ones.
///
/// A line's presence selects its check. A leading `- ` is accepted. With a configuration, each ID
/// must name an enabled check, and Harness adds every unlisted prerequisite after the listed IDs,
/// so the agent never lists prerequisites itself. Listing an automatic check is harmless.
fn parse_checks(
    text: &str,
    config: Option<&crate::checks::CheckConfig>,
    report: &mut ArtifactReport<'_>,
) -> Vec<String> {
    let mut listed: Vec<(usize, String)> = Vec::new();
    for (index, line) in text.lines().enumerate() {
        let number = index + 1;
        let id = line.trim();
        let id = id.strip_prefix("- ").map_or(id, str::trim);
        if id.is_empty() {
            continue;
        }
        if id.contains(char::is_whitespace) || id.starts_with('[') {
            report.syntax(
                number,
                id,
                "list one check ID per line, such as `unit`".into(),
            );
            continue;
        }
        if listed.iter().any(|(_, listed)| listed == id) {
            report.syntax(number, id, "the check is listed twice".into());
            continue;
        }
        listed.push((number, id.to_owned()));
    }
    let mut selected = listed.iter().map(|(_, id)| id.clone()).collect::<Vec<_>>();
    let Some(config) = config else {
        return selected;
    };
    let definition = |id: &str| {
        config
            .definitions
            .iter()
            .find(|definition| definition.id == id)
    };
    for (number, id) in &listed {
        match definition(id) {
            None => report.check(
                *number,
                id,
                format!(
                    "no configured check has this ID; configured checks are {}",
                    config
                        .definitions
                        .iter()
                        .filter(|definition| definition.enabled)
                        .map(|definition| definition.id.as_str())
                        .collect::<Vec<_>>()
                        .join(", ")
                ),
            ),
            Some(definition) if !definition.enabled => report.check(
                *number,
                id,
                "the check is disabled in the check configuration".into(),
            ),
            Some(_) => {}
        }
    }
    // Follow prerequisites transitively; an index walk visits each appended ID once.
    let mut position = 0;
    while position < selected.len() {
        if let Some(definition) = definition(&selected[position]) {
            for prerequisite in &definition.depends_on {
                if !selected.contains(prerequisite) {
                    selected.push(prerequisite.clone());
                }
            }
        }
        position += 1;
    }
    selected
}

#[cfg(test)]
mod test {
    use super::*;

    fn sample_document() -> DesignDocument {
        DesignDocument {
            objective: "Expand the prototype.".into(),
            usage: Some("```sh\ncargo run\n```".into()),
            requirements: vec!["Use four systems.".into(), "Keep it\nsmall.".into()],
            background: "A minimal Bevy app.".into(),
            decisions: vec![DesignDecision {
                decision: "Keep one file.".into(),
                rationale: "The prototype is small.".into(),
            }],
            design: "Plain prose.\n\n```text\n## Not a section inside a fence\n```".into(),
            flows: String::new(),
            manual_checks: vec!["Run the game.".into()],
            tests: Vec::new(),
            checks: Vec::new(),
            disabled_checks: Vec::new(),
        }
    }

    fn report(diagnostic: &mut Vec<DeclarationDiagnostic>) -> ArtifactReport<'_> {
        ArtifactReport {
            path: "plan:test.md",
            diagnostic,
        }
    }

    fn lines(diagnostic: &[DeclarationDiagnostic]) -> Vec<(u32, &str)> {
        diagnostic
            .iter()
            .map(|diagnostic| (diagnostic.line, diagnostic.kind.code()))
            .collect()
    }

    /// A rendered overview parses back to the same document without diagnostics.
    #[test]
    fn overview_round_trips() {
        let document = sample_document();
        let text = overview_text("Neon Salvager", &document);
        assert!(text.contains("## Manual checks\n- Run the game."));
        let mut diagnostic = Vec::new();
        let mut parsed = DesignDocument::default();
        let title = parse_overview(&text, &mut parsed, true, &mut report(&mut diagnostic));
        assert_eq!(diagnostic, Vec::new());
        assert_eq!(title.as_deref(), Some("Neon Salvager"));
        assert_eq!(parsed, document);
    }

    /// Every overview problem is reported at its line in one pass.
    #[test]
    fn overview_reports_every_problem_at_its_line() {
        let text = "# Title\n\n## Goals\nx\n\n## Decisions\n- Keep one file.\n\n## Objective\n\n";
        let mut diagnostic = Vec::new();
        parse_overview(
            text,
            &mut DesignDocument::default(),
            true,
            &mut report(&mut diagnostic),
        );
        assert_eq!(
            lines(&diagnostic),
            vec![
                (3, "invalid_plan_artifact"),
                (7, "invalid_plan_artifact"),
                (9, "invalid_plan_content"),
                (1, "invalid_plan_content"),
                (1, "invalid_plan_content"),
            ]
        );
        let mut diagnostic = Vec::new();
        assert_eq!(
            parse_overview(
                "## Objective\nx\n",
                &mut DesignDocument::default(),
                false,
                &mut report(&mut diagnostic),
            ),
            None
        );
        assert_eq!(lines(&diagnostic), vec![(1, "invalid_plan_artifact")]);
    }

    /// Flow Markdown is kept exactly as written, with no diagnostics for a well-formed flow.
    #[test]
    fn flows_keep_their_authored_markdown() {
        let text = "## Submit a plan\n\nRuns when the agent calls `plan_submit`.\nA rejection changes nothing.\n\n```mermaid\nsequenceDiagram\n    Agent->>Runtime: plan_submit(version)\n    Runtime-->>Agent: submitted\n```\n";
        let mut diagnostic = Vec::new();
        assert_eq!(parse_flows(text, &mut report(&mut diagnostic)), text);
        assert_eq!(diagnostic, Vec::new());
        assert_eq!(super::super::design_flows::count(text), 1);
    }

    /// Flow parsing reports trailing text, unknown diagram types, misfenced blocks, and missing
    /// descriptions or diagrams, each at its line.
    #[test]
    fn flows_report_each_malformed_block_at_its_line() {
        let mut diagnostic = Vec::new();
        parse_flows(
            "## Flow\n\nRuns once.\n\n```mermaid\ngraph TD\n  a --> b\n```\ntrailing\n\n## Other\n\n```text\nflowchart TD\n```\n\n## Bare\n\nOnly prose.\n",
            &mut report(&mut diagnostic),
        );
        assert_eq!(
            lines(&diagnostic),
            vec![
                (9, "invalid_plan_artifact"),
                (6, "invalid_plan_content"),
                (11, "invalid_plan_content"),
                (13, "invalid_plan_artifact"),
                (17, "invalid_plan_content"),
            ]
        );

        let mut diagnostic = Vec::new();
        parse_flows(
            "## Open\n\nRuns.\n\n```mermaid\nflowchart TD\n",
            &mut report(&mut diagnostic),
        );
        assert_eq!(lines(&diagnostic), vec![(5, "invalid_plan_artifact")]);
    }

    /// Test inventories round-trip, and malformed or duplicate items are reported at their line.
    #[test]
    fn tests_round_trip_and_report_bad_items() {
        let files = vec![DesignTestFile {
            file: "src/main.rs".into(),
            cases: vec![
                DesignTestCase {
                    name: "waves_spawn".into(),
                    target: None,
                    change: DesignTestChange::New,
                    description: "Timers spawn waves.\nEnemies pursue.".into(),
                },
                DesignTestCase {
                    name: "arena_bounds".into(),
                    target: Some("integration".into()),
                    change: DesignTestChange::Reused,
                    description: "Clamp is unchanged.".into(),
                },
            ],
        }];
        let text = tests_text(&files);
        assert!(
            text.contains("- reused `arena_bounds` [target: integration]: Clamp is unchanged.")
        );
        let mut diagnostic = Vec::new();
        assert_eq!(parse_tests(&text, &mut report(&mut diagnostic)), files);
        assert_eq!(diagnostic, Vec::new());
        let mut diagnostic = Vec::new();
        parse_tests(
            "- new `orphan`: no file\n## src/a.rs\n- added `x`: y\n- new `x`: y\n- new `x`: z\n",
            &mut report(&mut diagnostic),
        );
        assert_eq!(
            lines(&diagnostic),
            vec![
                (1, "invalid_plan_artifact"),
                (3, "invalid_plan_artifact"),
                (5, "invalid_plan_content"),
            ]
        );
    }

    /// Check selection reports unknown IDs and missing prerequisites without adding them.
    #[test]
    fn checks_report_unknown_ids_and_missing_prerequisites() {
        use crate::checks::{
            CheckCategory, CheckConfig, CheckDefinition, CheckOperation, CheckPurpose, CheckScope,
            CheckTrigger,
        };
        let definition = |id: &str, depends_on: &[&str]| CheckDefinition {
            auto: false,
            id: id.into(),
            name: id.into(),
            category: CheckCategory::Test,
            purpose: CheckPurpose::Verify,
            enabled: true,
            area: "Project".into(),
            trigger: CheckTrigger::AnyChange,
            scope: CheckScope::All,
            directory: ".".into(),
            all: CheckOperation {
                input: false,
                args: None,
                inputs: None,
                command: "true".into(),
            },
            files: None,
            cases: None,
            depends_on: depends_on.iter().map(|id| (*id).to_owned()).collect(),
        };
        let config = CheckConfig {
            version: 2,
            definitions: vec![definition("build", &[]), definition("unit", &["build"])],
        };
        let mut diagnostic = Vec::new();
        let selected = parse_checks(
            "unit\n- unit-tests\n\nnot a check\n- [x] build\n",
            Some(&config),
            &mut report(&mut diagnostic),
        );
        assert_eq!(
            selected,
            vec![
                "unit".to_owned(),
                "unit-tests".to_owned(),
                "build".to_owned()
            ],
            "the unlisted prerequisite follows the listed checks"
        );
        assert_eq!(
            lines(&diagnostic),
            vec![
                (4, "invalid_plan_artifact"),
                (5, "invalid_plan_artifact"),
                (2, "invalid_plan_check"),
            ]
        );
        assert!(
            diagnostic[2]
                .reason
                .contains("configured checks are build, unit")
        );
    }

    /// Reserved paths resolve exactly, and unknown names inside the namespace are rejected.
    #[test]
    fn artifact_paths_are_reserved() {
        assert_eq!(
            PlanArtifact::from_path("plan:flows.md").unwrap(),
            Some(PlanArtifact::Flows)
        );
        assert_eq!(
            PlanArtifact::from_path("plan:evidence.md").unwrap(),
            Some(PlanArtifact::Evidence)
        );
        assert_eq!(PlanArtifact::from_path("src/plan.rs").unwrap(), None);
        assert!(PlanArtifact::from_path("plan:notes.md").is_err());
    }
}
