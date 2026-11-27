//! Holds authored plan artifacts and derives execution inputs at submission.
//!
//! An artifact edit stores the agent's text verbatim, so every submission diagnostic names a line
//! the agent actually wrote. Submission retains the overview as Markdown and extracts Verify actions.
//! An artifact without stored text renders from the document, which keeps Harness-side writers
//! consistent: the review check toggle and test removal change the document and drop the stored
//! text, and the next read renders the result in the same grammar the agent edits.

use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::ops::Range;

use anyhow::{Context, Result, bail};
use forge_diff::syntax::{DeclarationOverview, ReviewSymbol};
use pulldown_cmark::{Event, HeadingLevel, Parser, Tag};
use serde_json::value::RawValue;

use super::design::DeclarationDesign;
use super::design_document::{DesignDocument, DesignWalkthroughComment};
use super::design_tests::{DesignTestCase, DesignTestChange, DesignTestFile};
use crate::declaration::{DeclarationDiagnostic, DeclarationDiagnosticKind};

/// Reserves the artifact namespace so it can never collide with a workspace path.
pub(crate) const ARTIFACT_PREFIX: &str = "plan:";

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
    /// Holds the ordered inline comments for declaration review.
    Walkthrough,
    /// Lists execution findings that Resolve must address; read-only.
    Findings,
    /// Lists the tool results a Verify report may cite; read-only.
    Evidence,
    /// Holds the Verify report, which the agent writes only in Verify.
    Verification,
}

impl PlanArtifact {
    /// Artifacts the planning agent writes, in presentation order.
    pub(crate) const AUTHORED: [Self; 5] = [
        Self::Overview,
        Self::Flows,
        Self::Tests,
        Self::Checks,
        Self::Walkthrough,
    ];

    /// Every artifact, in presentation order.
    const ALL: [Self; 8] = [
        Self::Overview,
        Self::Flows,
        Self::Tests,
        Self::Checks,
        Self::Walkthrough,
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
            Self::Walkthrough => "plan:walkthrough.json",
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
        PlanArtifact::Overview => document
            .overview_markdown
            .clone()
            .unwrap_or_else(|| legacy_overview_text(title, document)),
        PlanArtifact::Flows => document.flows.clone(),
        PlanArtifact::Tests => tests_text(&document.tests),
        PlanArtifact::Checks => checks_text(document),
        PlanArtifact::Walkthrough => serde_json::to_string_pretty(&document.walkthrough)?,
        PlanArtifact::Findings | PlanArtifact::Evidence | PlanArtifact::Verification => {
            bail!("{} is generated from execution state", artifact.path())
        }
    })
}

/// Convert CRLF and bare CR line endings to LF.
///
/// Authored text enters a plan only through this conversion, because every parser and projection
/// splits rows on LF: a retained CR would make the stored text and its rendered rows disagree on
/// each row's byte length.
pub(crate) fn lf_line_endings(text: &str) -> String {
    text.replace("\r\n", "\n").replace('\r', "\n")
}

/// Store an edited artifact verbatim apart from LF line endings, keeping the plan title in step
/// with the overview heading.
///
/// Text identical to the document's rendering is dropped instead of stored, so an edit that
/// restores the rendering leaves no trace.
pub(crate) fn store_authored(
    artifact: PlanArtifact,
    text: String,
    title: &mut String,
    design: &mut DeclarationDesign,
) -> Result<()> {
    let text = lf_line_endings(&text);
    if artifact == PlanArtifact::Overview
        && let Some(heading) = overview_title(&text)
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
                || old.overview_markdown != new.overview_markdown
        }
        PlanArtifact::Flows => old.flows != new.flows,
        PlanArtifact::Tests => old.tests != new.tests,
        PlanArtifact::Checks => {
            old.checks != new.checks
                || old.disabled_checks != new.disabled_checks
                || old.test_scopes != new.test_scopes
        }
        PlanArtifact::Walkthrough => old.walkthrough != new.walkthrough,
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
pub(crate) struct ParsedArtifacts {
    /// Document assembled from the artifacts, complete only when `diagnostic` has no errors.
    pub(crate) document: DesignDocument,
    /// Every problem found, each at the artifact line that caused it.
    pub(crate) diagnostic: Vec<DeclarationDiagnostic>,
}

/// Parse every authored artifact into a document, reporting each problem at its line.
pub(crate) fn parse_authored(
    title: &str,
    design: &DeclarationDesign,
    config: &crate::checks::CheckConfig,
) -> Result<ParsedArtifacts> {
    let text = |artifact| authored_text(artifact, title, design);
    let mut diagnostic = Vec::new();
    let overview = text(PlanArtifact::Overview)?;
    if overview.trim().is_empty() {
        ArtifactReport {
            path: PlanArtifact::Overview.path(),
            diagnostic: &mut diagnostic,
        }
        .content(
            1,
            "",
            "the overview must contain non-whitespace text".into(),
        );
    }
    let mut document = if design
        .artifact_text
        .contains_key(PlanArtifact::Overview.path())
    {
        DesignDocument {
            manual_checks: manual_checks(&overview),
            overview_markdown: Some(overview),
            ..DesignDocument::default()
        }
    } else {
        design.document.clone()
    };
    document.flows = parse_flows(
        &text(PlanArtifact::Flows)?,
        &mut ArtifactReport {
            path: PlanArtifact::Flows.path(),
            diagnostic: &mut diagnostic,
        },
    );
    document.tests = parse_tests(
        &text(PlanArtifact::Tests)?,
        Some(config),
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
    let mut walkthrough_report = ArtifactReport {
        path: PlanArtifact::Walkthrough.path(),
        diagnostic: &mut diagnostic,
    };
    document.walkthrough = validate_walkthrough(
        design,
        parse_walkthrough(&text(PlanArtifact::Walkthrough)?, &mut walkthrough_report),
        &mut walkthrough_report,
    )?;
    document.disabled_checks = design
        .document
        .disabled_checks
        .iter()
        .filter(|id| !document.checks.contains(id))
        .cloned()
        .collect();
    document.test_scopes = design.document.test_scopes.clone();
    for id in document.test_scopes.keys() {
        if !config.definitions.iter().any(|check| {
            &check.id == id
                && check.category == crate::checks::CheckCategory::Test
                && matches!(check.runner, crate::checks::CheckRunner::Adapter(_))
        }) {
            diagnostic.push(artifact_diagnostic(
                PlanArtifact::Checks.path(),
                1,
                DeclarationDiagnosticKind::PlanCheck,
                id,
                format!("test scope requires an existing test adapter check: {id}"),
            ));
        }
    }
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
    if let Some(text) = design.artifact_text.get(PlanArtifact::Overview.path()) {
        document.manual_checks = manual_checks(text);
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
            None,
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
    if let Some((text, path)) = parse(PlanArtifact::Walkthrough) {
        document.walkthrough = parse_walkthrough(
            &text,
            &mut ArtifactReport {
                path,
                diagnostic: &mut ignored,
            },
        )
        .into_iter()
        .map(|(_, comment)| comment)
        .collect();
    }
    document
}

/// Keep each entry's source position even when objects span lines or share a line.
fn parse_walkthrough(
    text: &str,
    report: &mut ArtifactReport<'_>,
) -> Vec<(usize, DesignWalkthroughComment)> {
    if text.trim().is_empty() {
        return Vec::new();
    }
    let entries = match serde_json::from_str::<Vec<&RawValue>>(text) {
        Ok(entries) => entries,
        Err(error) => {
            walkthrough_json_error(report, &error, 1, 0);
            return Vec::new();
        }
    };
    let (mut offset, mut line, mut column) = (0, 1, 0);
    let mut comments = Vec::new();
    for entry in entries {
        let entry = entry.get();
        let start = entry.as_ptr() as usize - text.as_ptr() as usize;
        let preceding = &text[offset..start];
        line += preceding.bytes().filter(|byte| *byte == b'\n').count();
        column = preceding
            .rsplit_once('\n')
            .map_or(column + preceding.len(), |(_, tail)| tail.len());
        offset = start;
        if !entry.starts_with('{') {
            report.syntax(line, "", "walkthrough entry must be a JSON object".into());
            continue;
        }
        match serde_json::from_str(entry) {
            Ok(comment) => comments.push((line, comment)),
            Err(error) => walkthrough_json_error(report, &error, line, column),
        }
    }
    comments
}

fn walkthrough_json_error(
    report: &mut ArtifactReport<'_>,
    error: &serde_json::Error,
    first_line: usize,
    first_column: usize,
) {
    let line = first_line + error.line().saturating_sub(1);
    let column = error.column() + if error.line() <= 1 { first_column } else { 0 };
    let message = error.to_string();
    let location = format!(" at line {} column {}", error.line(), error.column());
    let message = message.strip_suffix(&location).unwrap_or(&message);
    report.syntax(
        line,
        "",
        format!("invalid walkthrough JSON at line {line} column {column}: {message}"),
    );
    report.diagnostic.last_mut().unwrap().column =
        u32::try_from(column.saturating_sub(1)).unwrap_or(u32::MAX);
}

fn validate_walkthrough(
    design: &DeclarationDesign,
    entries: Vec<(usize, DesignWalkthroughComment)>,
    report: &mut ArtifactReport<'_>,
) -> Result<Vec<DesignWalkthroughComment>> {
    if entries.is_empty() {
        return Ok(Vec::new());
    }
    let parsed = design.parsed()?;
    let changed: HashSet<_> = design
        .changed_paths()
        .into_iter()
        .map(|path| design.moved.get(&path).cloned().unwrap_or(path))
        .collect();
    let mut inventory = HashMap::new();
    let mut anchors = HashSet::new();
    let mut comments = Vec::new();
    for (line, mut entry) in entries {
        super::resolution_deadline::ResolutionDeadline::check()?;
        if entry
            .title
            .as_deref()
            .is_some_and(|title| title.contains('\0'))
        {
            report.content(line, &entry.path, "title must contain no NUL bytes".into());
        }
        if entry.comment.trim().is_empty() || entry.comment.contains('\0') {
            report.content(
                line,
                &entry.path,
                "comment must contain text and no NUL bytes".into(),
            );
        }
        if !changed.contains(&entry.path) {
            report.content(
                line,
                &entry.path,
                "path must identify a changed file at its review path".into(),
            );
            continue;
        }
        let symbols = inventory.entry(entry.path.clone()).or_insert_with(|| {
            let source = if design.proposed_source.contains_key(&entry.path) {
                parsed.proposed.get(&entry.path).map(String::as_str)
            } else {
                design
                    .baseline
                    .get(&entry.path)
                    .map(|file| file.text.as_str())
            };
            source
                .and_then(|source| DeclarationOverview::review_symbols(&entry.path, source))
                .unwrap_or_default()
        });
        let Some(symbol) = resolve_walkthrough_symbol(symbols, &entry.symbol) else {
            let candidates = symbols
                .iter()
                .map(|symbol| symbol.name.as_str())
                .collect::<BTreeSet<_>>()
                .into_iter()
                .collect::<Vec<_>>()
                .join(", ");
            let candidates = if candidates.is_empty() {
                "(none)"
            } else {
                &candidates
            };
            report.content(
                line,
                &entry.symbol,
                format!(
                    "unknown or ambiguous symbol in {}. Valid symbols: {candidates}",
                    entry.path
                ),
            );
            continue;
        };
        entry.symbol.clone_from(&symbol.name);
        if !anchors.insert((entry.path.clone(), entry.symbol.clone())) {
            report.content(line, &entry.path, "duplicate walkthrough anchor".into());
        }
        comments.push(entry);
    }
    Ok(comments)
}

/// Resolve exact qualified names before trying an unambiguous short name.
pub(super) fn resolve_walkthrough_symbol<'a>(
    symbols: &'a [ReviewSymbol],
    name: &str,
) -> Option<&'a ReviewSymbol> {
    let mut exact = symbols.iter().filter(|symbol| symbol.name == name);
    if let Some(symbol) = exact.next() {
        return exact.next().is_none().then_some(symbol);
    }
    let mut matches = symbols.iter().filter(|symbol| {
        let short = symbol.name.rsplit([':', '.']).next().unwrap_or_default();
        let short = short
            .rsplit_once('#')
            .filter(|(_, ordinal)| {
                !ordinal.is_empty() && ordinal.bytes().all(|byte| byte.is_ascii_digit())
            })
            .map_or(short, |(name, _)| name);
        short == name
    });
    let symbol = matches.next()?;
    matches.next().is_none().then_some(symbol)
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
        let text = authored_text(artifact, title, design)?;
        let text = if artifact == PlanArtifact::Overview && !include_verification {
            without_manual_checks(text)
        } else {
            text
        };
        artifacts.insert(artifact.path(), text);
    }
    Ok(artifacts)
}

/// Get the first authored top-level `# Title` heading.
pub(super) fn overview_title(text: &str) -> Option<&str> {
    overview_headings(text)
        .filter(|(level, _)| *level == HeadingLevel::H1)
        .filter_map(|(_, range)| text[range].lines().next()?.strip_prefix("# "))
        .map(str::trim)
        .find(|heading| !heading.is_empty())
}

/// Names the overview section that lists the manual checks Verify performs.
const MANUAL_CHECKS: &str = "Manual checks";

/// Recreate the overview artifact of a revision that predates stored overview Markdown.
fn legacy_overview_text(title: &str, document: &DesignDocument) -> String {
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
        let body = body.trim_matches('\n');
        if matches!(heading, "Requirements" | "Decisions") && body.trim().is_empty() {
            continue;
        }
        text.push_str(&format!("\n## {heading}\n"));
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

/// Locate top-level headings with the Markdown parser so code and quotes stay narrative.
fn overview_headings(text: &str) -> impl Iterator<Item = (HeadingLevel, Range<usize>)> + '_ {
    let mut depth = 0usize;
    Parser::new(text)
        .into_offset_iter()
        .filter_map(move |(event, range)| match event {
            Event::Start(tag) => {
                let top_level = depth == 0;
                depth += 1;
                match tag {
                    Tag::Heading { level, .. } if top_level => Some((level, range)),
                    _ => None,
                }
            }
            Event::End(_) => {
                depth -= 1;
                None
            }
            _ => None,
        })
}

/// Locate each Manual checks heading and its body up to the next peer or parent heading.
fn manual_check_sections(text: &str) -> Vec<Range<usize>> {
    let mut sections = Vec::new();
    let mut start = None;
    for (level, heading) in overview_headings(text) {
        if !matches!(level, HeadingLevel::H1 | HeadingLevel::H2) {
            continue;
        }
        if let Some(previous) = start.take() {
            sections.push(previous..heading.start);
        }
        if level == HeadingLevel::H2
            && text[heading.clone()]
                .lines()
                .next()
                .and_then(|line| line.trim_start().strip_prefix("## "))
                .is_some_and(|name| name.trim().eq_ignore_ascii_case(MANUAL_CHECKS))
        {
            start = Some(heading.start);
        }
    }
    if let Some(start) = start {
        sections.push(start..text.len());
    }
    sections
}

/// Extract top-level dash-list items as Verify actions, preserving indented continuations.
pub(super) fn manual_checks(text: &str) -> Vec<String> {
    let mut checks = Vec::new();
    for section in manual_check_sections(text) {
        let mut depth = 0usize;
        let section = &text[section];
        for (event, range) in Parser::new(section).into_offset_iter() {
            match event {
                Event::Start(tag) => {
                    if depth == 1
                        && matches!(tag, Tag::Item)
                        && let Some(item) = section[range].strip_prefix("- ")
                    {
                        let mut lines = item.lines();
                        let mut action = lines.next().unwrap_or_default().to_owned();
                        for line in lines.filter_map(|line| line.strip_prefix("  ")) {
                            action.push('\n');
                            action.push_str(line);
                        }
                        if !action.trim().is_empty() {
                            checks.push(action);
                        }
                    }
                    depth += 1;
                }
                Event::End(_) => depth -= 1,
                _ => {}
            }
        }
    }
    checks
}

/// Remove Verify instructions while preserving every other byte of the overview.
fn without_manual_checks(mut text: String) -> String {
    for section in manual_check_sections(&text).into_iter().rev() {
        text.replace_range(section, "");
    }
    text
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
///
/// With a configuration, each target must name a configured test check by its ID or label.
fn parse_tests(
    text: &str,
    config: Option<&crate::checks::CheckConfig>,
    report: &mut ArtifactReport<'_>,
) -> Vec<DesignTestFile> {
    const SHAPE: &str = "tests are `- new|modified|removed|reused `name` [target: check]: \
                         scenario`, where the optional target is a test check's ID or label";
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
                report.content(
                    *line,
                    &reference,
                    "the target must name a test check".into(),
                );
            } else if let (Some(config), Some(target)) = (config, case.target.as_deref())
                && let Err(reason) = config.test_target(target)
            {
                report.check(*line, &reference, reason);
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

/// Parse the checks requested by the plan and include their declared prerequisites.
///
/// A line's presence selects its check. A leading `- ` is accepted. With a configuration, each ID
/// must name an enabled check, and Harness adds every unlisted prerequisite after the listed IDs,
/// so the agent can name just the checks it wants to run.
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

    #[test]
    fn walkthrough_keeps_order_and_resolves_moved_and_deleted_declarations() -> Result<()> {
        let mut design = DeclarationDesign::scratch(BTreeMap::from([(
            "src/worker.rs".into(),
            "struct Worker;\nimpl Worker { fn start(&self); }\n".into(),
        )]))?;
        for (path, text) in [
            ("old.rs", "struct Worker;\n"),
            ("deleted.rs", "enum Old { Gone }\n"),
        ] {
            design.baseline.insert(
                path.into(),
                super::super::DeclarationFile {
                    text: text.into(),
                    editable_text: text.into(),
                    source_digest: String::new(),
                },
            );
        }
        design.moved.insert("old.rs".into(), "src/worker.rs".into());
        let input = r#"[
  {"path":"src/worker.rs","symbol":"Worker","comment":"Register the new worker."},
  {"path":"deleted.rs","symbol":"Old","comment":"Retire the old job type."},
  {"path":"src/worker.rs","symbol":"start","title":"Acquire the lease","comment":"Acquire the `lease` first."}
]"#;
        let mut diagnostic = Vec::new();
        let comments = validate_walkthrough(
            &design,
            parse_walkthrough(input, &mut report(&mut diagnostic)),
            &mut report(&mut diagnostic),
        )?;
        assert!(diagnostic.is_empty());
        assert_eq!(comments[0].symbol, "Worker");
        assert_eq!(comments[0].title, None);
        assert_eq!(comments[1].symbol, "Old");
        assert_eq!(comments[2].symbol, "Worker::start");
        assert_eq!(comments[2].title.as_deref(), Some("Acquire the lease"));
        design.document.walkthrough = comments;
        let rendered = authored_text(PlanArtifact::Walkthrough, "", &design)?;
        assert_eq!(
            rendered,
            serde_json::to_string_pretty(&design.document.walkthrough)?
        );
        assert_eq!(
            serde_json::from_str::<Vec<DesignWalkthroughComment>>(&rendered)?,
            design.document.walkthrough
        );
        Ok(())
    }

    #[test]
    fn walkthrough_reports_json_locations_and_invalid_anchors() -> Result<()> {
        let proposed = "struct First;\nimpl First { fn start(&self); }\nstruct Second;\nimpl Second { fn start(&self); }\n";
        let unchanged = "struct Unchanged;\n";
        let mut design = DeclarationDesign::scratch(BTreeMap::from([
            ("worker.rs".into(), proposed.into()),
            ("same.rs".into(), unchanged.into()),
        ]))?;
        design.baseline.insert(
            "same.rs".into(),
            super::super::DeclarationFile {
                text: unchanged.into(),
                editable_text: unchanged.into(),
                source_digest: String::new(),
            },
        );
        let mut diagnostic = Vec::new();
        let invalid = r#"[
  {"path":"worker.rs","symbol":"start","comment":"Ambiguous."},
  {"path":"worker.rs","symbol":"run","comment":"Missing."},
  {"path":"worker.rs","symbol":"First::start","comment":"Valid."},
  {"path":"worker.rs","symbol":"First::start","comment":"Duplicate."},
  {"path":"same.rs","symbol":"Unchanged","comment":"Unchanged."},
  {"path":"missing.rs","symbol":"Missing","comment":"Missing."},
  {"path":"worker.rs","symbol":"First","comment":"  "},
  {"path":"worker.rs","symbol":"Second","comment":"\u0000"},
  {"path":"worker.rs","symbol":"First","comment":"Unknown field.","caption":"No"},
  ["worker.rs", null, "Array entry"],
  {"path":"worker.rs","comment":"Wrong type.","symbol":null},
  {"path":"worker.rs","symbol":"Second::start","title":"\u0000","comment":"Invalid title."},
  {"path":"worker.rs","comment":"Missing symbol."},
  {"path":"worker.rs","symbol":"","comment":"Empty symbol."}
]"#;
        let valid_json = parse_walkthrough(invalid, &mut report(&mut diagnostic));
        assert_eq!(
            diagnostic.iter().map(|item| item.line).collect::<Vec<_>>(),
            [10, 11, 12, 14]
        );
        assert!(diagnostic[0].reason.contains("unknown field `caption`"));
        assert!(diagnostic[0].column > 2);
        assert!(diagnostic[3].reason.contains("missing field `symbol`"));
        diagnostic.clear();
        validate_walkthrough(&design, valid_json, &mut report(&mut diagnostic))?;
        assert_eq!(
            diagnostic.iter().map(|item| item.line).collect::<Vec<_>>(),
            [2, 3, 5, 6, 7, 8, 9, 13, 15]
        );
        assert_eq!(diagnostic[7].reason, "title must contain no NUL bytes");
        for item in &diagnostic[..2] {
            assert!(item.reason.contains("First::start"));
            assert!(item.reason.contains("Second::start"));
        }
        diagnostic.clear();
        let broken = "[\n  { \"path\": ? }\n]";
        let error = serde_json::from_str::<Vec<&RawValue>>(broken).unwrap_err();
        assert!(parse_walkthrough(broken, &mut report(&mut diagnostic)).is_empty());
        assert_eq!(diagnostic[0].line, error.line() as u32);
        assert_eq!(diagnostic[0].column + 1, error.column() as u32);
        assert!(parse_walkthrough(" \n\t", &mut report(&mut diagnostic)).is_empty());
        assert_eq!(diagnostic.len(), 1);
        Ok(())
    }

    /// Keep headings and list items in examples out of the Verify checklist.
    #[test]
    fn overview_extracts_only_authored_manual_actions() {
        let example = "~~~markdown\n# Example title\n## Manual checks\n- Sample item.\n~~~\n\n";
        let narrative = "# Plan title\n\nA free-form plan.\n";
        let checks = "## Manual checks\n- Open the view.\n  Confirm the title.\n\n```text\n- Sample item.\n```\n\n";
        let following = "## Notes\nKeep this section.";
        let text = format!("{example}{narrative}{checks}{following}");
        assert_eq!(overview_title(&text), Some("Plan title"));
        assert_eq!(manual_checks(&text), ["Open the view.\nConfirm the title."]);
        assert_eq!(
            without_manual_checks(text),
            format!("{example}{narrative}{following}")
        );
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
        assert_eq!(
            parse_tests(&text, None, &mut report(&mut diagnostic)),
            files
        );
        assert_eq!(diagnostic, Vec::new());
        let mut diagnostic = Vec::new();
        parse_tests(
            "- new `orphan`: no file\n## src/a.rs\n- added `x`: y\n- new `x`: y\n- new `x`: z\n",
            None,
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
            CheckCategory, CheckConfig, CheckDefinition, CheckOperation, CheckScope, CheckTrigger,
        };
        let definition = |id: &str, depends_on: &[&str]| CheckDefinition {
            auto: false,
            id: id.into(),
            name: id.into(),
            category: CheckCategory::Custom,
            enabled: true,
            area: "Project".into(),
            trigger: CheckTrigger::AnyChange,
            scope: CheckScope::All,
            directory: ".".into(),
            runner: crate::checks::CheckRunner::Command(CheckOperation {
                prepare: false,
                args: None,
                inputs: None,
                command: "true".into(),
            }),
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

    /// An artifact written with CRLF or bare CR line endings is stored with LF, so the parsed
    /// document and every projection of it agree on the bytes of each row.
    #[test]
    fn stored_artifacts_use_lf_line_endings() {
        let mut design = DeclarationDesign::default();
        let mut title = String::new();
        store_authored(
            PlanArtifact::Flows,
            "## Flow\r\nRuns once.\r\n\r\n```mermaid\rflowchart TD\r\n  a --> b\r\n```\r\n".into(),
            &mut title,
            &mut design,
        )
        .unwrap();
        assert_eq!(
            design.artifact_text[PlanArtifact::Flows.path()],
            "## Flow\nRuns once.\n\n```mermaid\nflowchart TD\n  a --> b\n```\n"
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

    /// A test target names a configured test check by ID or label; submission refuses any other
    /// target at its line and lists the valid targets.
    #[test]
    fn submission_refuses_a_test_target_that_names_no_test_check() -> Result<()> {
        let config: crate::checks::CheckConfig = serde_json::from_value(serde_json::json!({
            "test":{"action":{"label":"Vitest","category":"test",
                "adapter":{"script":{"command":["unused"]}}}},
            "lint":{"action":{"label":"ESLint","category":"lint",
                "adapter":{"script":{"command":["unused"]}}}}
        }))?;
        let target_diagnostics = |target: &str| -> Result<Vec<DeclarationDiagnostic>> {
            let mut design = DeclarationDesign::default();
            design.artifact_text.insert(
                PlanArtifact::Tests.path().into(),
                format!(
                    "## tests/money.test.ts
- new `splits evenly` [target: {target}]: scenario
"
                ),
            );
            Ok(parse_authored("Money", &design, &config)?
                .diagnostic
                .into_iter()
                .filter(|diagnostic| diagnostic.path == PlanArtifact::Tests.path())
                .collect())
        };
        for valid in ["test", "Vitest", "vitest"] {
            assert_eq!(
                target_diagnostics(valid)?,
                Vec::new(),
                "{valid} was refused"
            );
        }
        for invalid in ["Jest", "ESLint"] {
            let diagnostic = target_diagnostics(invalid)?;
            assert_eq!(
                lines(&diagnostic),
                vec![(2, "invalid_plan_check")],
                "{invalid}"
            );
            assert!(
                diagnostic[0].reason.contains("`test` (Vitest)"),
                "{}",
                diagnostic[0].reason
            );
        }
        Ok(())
    }
}
