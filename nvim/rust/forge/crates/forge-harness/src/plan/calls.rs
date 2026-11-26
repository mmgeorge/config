use std::collections::{BTreeMap, VecDeque};
use std::sync::{Arc, Mutex, OnceLock};

use crate::declaration::DeclarationDiagnosticKind;
use anyhow::{Context, Result, ensure};
use forge_diff::syntax::{
    AlgorithmReference, DeclarationCalls, DeclarationOverview, DeclarationPosition,
    DeclarationPresentation, SyntaxError, algorithm_diff_reference, algorithm_diff_references,
    algorithm_diff_source_for_highlighting, classify_algorithm_diff_line, validate_algorithm,
    validate_algorithm_diff,
};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

const PRESENTATION_CACHE_BYTES: usize = 8 * 1024 * 1024;
const PRESENTATION_CACHE_ENTRIES: usize = 64;
static PRESENTATION_CACHE: OnceLock<Mutex<VecDeque<CachedPresentation>>> = OnceLock::new();

/// The semantic operation a navigation or search target performs on its declaration.
#[derive(
    Clone, Copy, Debug, Default, Deserialize, Eq, JsonSchema, Ord, PartialEq, PartialOrd, Serialize,
)]
#[serde(rename_all = "snake_case")]
pub enum CallKind {
    /// Invokes a callable.
    #[default]
    Call,
    /// Reads, writes, or takes a reference to a named property.
    Property,
    /// Takes a callable as a value, including callback registration.
    Callback,
    /// Reads a named value.
    Value,
    /// Constructs a named type directly or invokes a factory returning an owned named type.
    Construct,
}

impl CallKind {
    /// Keep the default kind out of serialized navigation targets.
    pub(crate) fn is_call(&self) -> bool {
        *self == Self::Call
    }

    /// Classify the occurrence in a reference picker and navigation identity.
    pub(crate) fn label(self) -> &'static str {
        match self {
            Self::Call => "call",
            Self::Property => "property",
            Self::Callback => "callback",
            Self::Value => "value",
            Self::Construct => "construct",
        }
    }
}

/// Saved behavioral intent belonging to one callable.
#[derive(Clone, Debug, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
pub struct FunctionBody {
    /// Callable identity within its saved declaration file.
    pub owner: String,
    /// Authored algorithm for a new callable or behavior change for an existing callable.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub behavior: Option<FunctionBehavior>,
}

#[derive(Clone, Debug, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
#[serde(tag = "kind", content = "text", rename_all = "snake_case")]
/// Mutually exclusive behavioral sections preserved in editable and rendered plans.
pub enum FunctionBehavior {
    /// Complete academic pseudocode for a newly introduced callable.
    Algorithm(String),
    /// Intended behavioral delta for an existing callable.
    Change(String),
}

impl FunctionBehavior {
    /// Editable section heading for this behavior category.
    pub(crate) fn label(&self) -> &'static str {
        match self {
            Self::Algorithm(_) => "Algorithm",
            Self::Change(_) => "Change",
        }
    }

    /// Authored content, including nested pseudocode indentation.
    pub(crate) fn text(&self) -> &str {
        match self {
            Self::Algorithm(text) | Self::Change(text) => text,
        }
    }

    /// Get the first visible token's byte column in an authored behavior row.
    pub(super) fn statement_column(&self, row: &str) -> usize {
        let payload = match self {
            Self::Algorithm(_) => row,
            Self::Change(_) => classify_algorithm_diff_line(row).payload,
        };
        row.len() - payload.trim_start().len()
    }

    /// Enumerate authored identifier segments using the behavior's source coordinates.
    pub(super) fn references(&self) -> Result<Vec<AlgorithmReference>, SyntaxError> {
        match self {
            Self::Algorithm(text) => forge_diff::syntax::algorithm_references(text),
            Self::Change(text) => algorithm_diff_references(text),
        }
    }

    /// Get mutable access to the authored content for a token edit.
    pub(super) fn text_mut(&mut self) -> &mut String {
        match self {
            Self::Algorithm(text) | Self::Change(text) => text,
        }
    }
}

/// Position of a rendered row within its authored behavior block.
pub(super) struct BehaviorRow {
    /// Identity of the callable that owns this row.
    pub owner: String,
    /// Zero for the heading and one-based within the authored body.
    pub offset: u32,
    /// Behavior heading text.
    pub category: &'static str,
    /// Byte column where the authored row starts in the rendered source.
    pub column: usize,
}

impl BehaviorRow {
    /// Get a Change body row at its authored prefix column.
    pub(super) fn change_line<'row>(&self, text: &'row str) -> Option<&'row str> {
        if self.category == "Change" && self.offset > 0 {
            text.get(self.column..)
        } else {
            None
        }
    }
}

/// Maps display rows to saved declarations and behavior blocks.
pub(crate) struct CallPresentation {
    visibility: [OnceLock<forge_diff::syntax::DeclarationVisibility>; 2],
    folding: OnceLock<Vec<forge_diff::syntax::DeclarationFold>>,
    callable: OnceLock<Vec<forge_diff::syntax::DeclarationCallable>>,
    /// Signature-only text used by declaration analysis before synthetic bodies are rendered.
    pub plain: String,
    pub declaration: DeclarationPresentation,
    pub(super) behavior_row: BTreeMap<usize, BehaviorRow>,
    /// Rendered body boundaries keyed by their final signature row.
    pub body: BTreeMap<usize, CallBody>,
    pub plain_row: Vec<usize>,
    /// Original declaration rows mapped to their rendered rows, excluding synthetic body rows.
    pub declaration_row: BTreeMap<usize, usize>,
}

impl CallPresentation {
    /// Reuses parsed callable identities and attached source boundaries during comparison.
    pub(crate) fn callables(
        &self,
        path: &str,
    ) -> Result<&[forge_diff::syntax::DeclarationCallable]> {
        if let Some(callable) = self.callable.get() {
            return Ok(callable);
        }
        let callable = DeclarationCalls::extract(path, &self.plain, true)
            .map_err(|error| anyhow::anyhow!("{error:?}"))?;
        Ok(self.callable.get_or_init(|| callable))
    }

    /// Isolate pseudocode from native declarations while preserving authored byte columns.
    pub(crate) fn algorithm_source(&self) -> Option<String> {
        self.behavior_row
            .values()
            .any(|row| row.offset > 0)
            .then(|| {
                let mut source = String::new();
                self.declaration
                    .text
                    .lines()
                    .enumerate()
                    .for_each(|(row, text)| {
                        if let Some(behavior) = self.behavior_row.get(&row) {
                            if behavior.category == "Algorithm" {
                                source.push_str(text);
                            } else if let Some(change) = behavior
                                .change_line(text)
                                .and_then(algorithm_diff_source_for_highlighting)
                            {
                                source.push_str(&text[..behavior.column]);
                                source.push_str(&change);
                            }
                        }
                        source.push('\n');
                    });
                source
            })
    }
}

impl CallPresentation {
    /// Reuses container analysis for the same immutable declaration presentation.
    pub(crate) fn folds(&self, path: &str) -> Result<&[forge_diff::syntax::DeclarationFold]> {
        if let Some(folding) = self.folding.get() {
            return Ok(folding);
        }
        let folding = forge_diff::syntax::DeclarationFolding::analyze(path, &self.plain)
            .map_err(|error| anyhow::anyhow!("{error:?}"))?;
        Ok(self.folding.get_or_init(|| folding))
    }
}

/// A rendered function body, from its final signature row through its closing delimiter.
pub(crate) struct CallBody {
    /// First signature row included as context when the behavior changes.
    pub heading: usize,
    /// Closing delimiter row included in this body.
    pub end: usize,
    /// Final declaration line before the saved review format inserts a body.
    pub declaration: String,
}

struct CachedPresentation {
    key: String,
    bytes: usize,
    value: Arc<CallPresentation>,
}

/// Return an editable file view in declaration and stored call order.
pub(crate) fn combined(path: &str, text: &str, calls: &[FunctionBody]) -> Result<String> {
    Ok(insert(path, text, calls, false)?.declaration.text)
}

/// A signature binding or declaration name selected in a behavior block.
pub(crate) enum AlgorithmTarget {
    Binding {
        name: String,
        position: forge_diff::syntax::DeclarationPosition,
    },
    /// Qualified declaration name and the lookup it needs; the resolver may not find it.
    Reference { name: String, kind: CallKind },
}

/// Resolve a behavior identifier using signature bindings before declaration names.
pub(crate) fn algorithm_reference(
    path: &str,
    declarations: &str,
    body: &FunctionBody,
    offset: u32,
    row: &str,
    column: usize,
) -> Result<Option<AlgorithmTarget>> {
    let Some(behavior) = &body.behavior else {
        return Ok(None);
    };
    let Some(source) = offset
        .checked_sub(1)
        .and_then(|offset| behavior.text().lines().nth(offset as usize))
    else {
        return Ok(None);
    };
    let statement_column = behavior.statement_column(source);
    let statement = &source[statement_column..];
    let displayed = row.trim_start();
    let Some(offset_in_statement) = column
        .checked_sub(row.len() - displayed.len())
        .filter(|column| *column < statement.len() && displayed.starts_with(statement))
    else {
        return Ok(None);
    };
    let column = statement_column + offset_in_statement + 1;
    let reference = match behavior {
        FunctionBehavior::Algorithm(algorithm) => {
            forge_diff::syntax::algorithm_reference(algorithm, offset as usize, column)
        }
        FunctionBehavior::Change(change) => {
            algorithm_diff_reference(change, offset as usize, column)
        }
    };
    let Some(reference) = reference.map_err(|error| anyhow::anyhow!("{error:?}"))? else {
        return Ok(None);
    };
    let function = DeclarationCalls::extract(path, declarations, true)
        .map_err(|error| anyhow::anyhow!("{error:?}"))?
        .into_iter()
        .find(|function| function.owner == body.owner)
        .context("Behavior callable signature is unavailable")?;
    Ok(resolve_algorithm_reference(path, &function, reference))
}

/// Name the declaration an Algorithm identifier refers to, without validating it.
///
/// Algorithms mix repository declarations with abstract operations such as
/// `keyboard_direction(input)`, so resolution is best effort: a signature binding resolves to its
/// parameter, a qualified or capitalized name or a member of a typed binding yields a qualified
/// name for the declaration resolver, and anything else, including every unqualified lowercase
/// call, has no target.
pub(crate) fn resolve_algorithm_reference(
    path: &str,
    function: &forge_diff::syntax::DeclarationCallable,
    reference: forge_diff::syntax::AlgorithmReference,
) -> Option<AlgorithmTarget> {
    if reference.first
        && let Some(position) = function.binding_position.get(&reference.segment)
    {
        return Some(AlgorithmTarget::Binding {
            name: reference.segment,
            position: *position,
        });
    }
    let separator = if path.ends_with(".rs") { "::" } else { "." };
    let capitalized = |name: &str| name.chars().next().is_some_and(char::is_uppercase);
    if !reference.last {
        return capitalized(&reference.segment).then(|| AlgorithmTarget::Reference {
            name: reference.prefix.replace('.', separator),
            kind: CallKind::Value,
        });
    }
    let member = reference.segment;
    let kind = match (reference.invocation, capitalized(&member)) {
        (true, true) => CallKind::Construct,
        (true, false) => CallKind::Call,
        (false, _) => CallKind::Property,
    };
    let receiver = reference
        .name
        .rsplit_once('.')
        .map(|(receiver, _)| receiver);
    if reference.name.contains("::") || receiver.is_some_and(capitalized) {
        return Some(AlgorithmTarget::Reference {
            name: reference.name.replace('.', separator),
            kind,
        });
    }
    let Some(receiver) = receiver else {
        return (reference.invocation && capitalized(&member))
            .then_some(AlgorithmTarget::Reference { name: member, kind });
    };
    let declared_type = function.binding.get(receiver)?.as_deref()?;
    let types = declared_type
        .trim()
        .trim_start_matches(':')
        .split(|character: char| !character.is_alphanumeric() && !"_:$.'".contains(character))
        .filter(|part| {
            !part.is_empty()
                && !part.starts_with('\'')
                && !matches!(*part, "mut" | "const" | "dyn" | "impl")
        })
        .collect::<std::collections::BTreeSet<_>>();
    let [receiver_type] = types.into_iter().collect::<Vec<_>>()[..] else {
        return None;
    };
    Some(AlgorithmTarget::Reference {
        name: format!("{receiver_type}{separator}{member}"),
        kind,
    })
}

/// Render function summaries and behavior blocks for review.
pub(crate) fn present(
    path: &str,
    text: &str,
    calls: &[FunctionBody],
) -> Result<Arc<CallPresentation>> {
    let key = super::digest(&serde_json::to_vec(&(path, text, calls))?);
    let cache = PRESENTATION_CACHE.get_or_init(Default::default);
    {
        let mut entries = cache
            .lock()
            .map_err(|_| anyhow::anyhow!("call presentation cache lock poisoned"))?;
        if let Some(index) = entries.iter().position(|entry| entry.key == key)
            && let Some(entry) = entries.remove(index)
        {
            let result = Arc::clone(&entry.value);
            entries.push_back(entry);
            return Ok(result);
        }
    }
    let presentation =
        DeclarationOverview::present(path, text).map_err(|error| anyhow::anyhow!("{error:?}"))?;
    let mut result = insert(path, &presentation.text, calls, true)?;
    for position in result.declaration.source.iter_mut().flatten() {
        *position = presentation
            .source
            .get(position.line as usize - 1)
            .copied()
            .flatten()
            .unwrap_or(*position);
    }
    let bytes = result.plain.capacity() * 3
        + result.declaration.text.capacity()
        + result.plain.lines().count() * 256
        + result.declaration.source.capacity() * std::mem::size_of::<Option<DeclarationPosition>>()
        + result.plain_row.capacity() * std::mem::size_of::<usize>()
        + result.declaration_row.len() * 64
        + result
            .behavior_row
            .values()
            .map(|row| row.owner.capacity() + 104)
            .sum::<usize>()
        + result
            .body
            .values()
            .map(|body| body.declaration.capacity() + 128)
            .sum::<usize>();
    let result = Arc::new(result);
    if bytes <= PRESENTATION_CACHE_BYTES {
        let mut entries = cache
            .lock()
            .map_err(|_| anyhow::anyhow!("call presentation cache lock poisoned"))?;
        while entries.len() >= PRESENTATION_CACHE_ENTRIES
            || entries.iter().map(|entry| entry.bytes).sum::<usize>() + bytes
                > PRESENTATION_CACHE_BYTES
        {
            entries.pop_front();
        }
        entries.push_back(CachedPresentation {
            key,
            bytes,
            value: Arc::clone(&result),
        });
    }
    Ok(result)
}

pub(crate) fn insert(
    path: &str,
    text: &str,
    calls: &[FunctionBody],
    review: bool,
) -> Result<CallPresentation> {
    if calls.is_empty() {
        return Ok(CallPresentation {
            visibility: Default::default(),
            folding: Default::default(),
            callable: Default::default(),
            plain: text.into(),
            declaration: DeclarationPresentation {
                text: text.into(),
                source: text
                    .lines()
                    .enumerate()
                    .map(|(row, _)| {
                        Some(DeclarationPosition {
                            line: row as u32 + 1,
                            column: 0,
                        })
                    })
                    .collect(),
            },
            behavior_row: BTreeMap::new(),
            body: BTreeMap::new(),
            plain_row: (0..text.lines().count()).collect(),
            declaration_row: (0..text.lines().count()).map(|row| (row, row)).collect(),
        });
    }
    let functions = DeclarationCalls::extract(path, text, true)
        .map_err(|error| anyhow::anyhow!("{error:?}"))?;
    let by_owner = calls
        .iter()
        .map(|call| (call.owner.as_str(), call))
        .collect::<BTreeMap<_, _>>();
    let mut at = BTreeMap::<usize, Vec<_>>::new();
    for function in functions {
        if let Some(call) = by_owner.get(function.owner.as_str()) {
            at.entry(function.end_line as usize)
                .or_default()
                .push((function, call));
        }
    }
    let mut output = String::new();
    let mut source = Vec::new();
    let mut behavior_row = BTreeMap::new();
    let mut body = BTreeMap::new();
    let mut declaration_row = BTreeMap::new();
    let lua = path.ends_with(".lua");
    for (index, line) in text.lines().enumerate() {
        declaration_row.insert(index, source.len());
        let function_body = review && at.contains_key(&(index + 1));
        output.push_str(if function_body && !lua {
            line.trim_end().trim_end_matches(';')
        } else {
            line
        });
        if function_body && !lua {
            output.push_str(" {");
        }
        output.push('\n');
        source.push(DeclarationPosition {
            line: index as u32 + 1,
            column: 0,
        });
        if let Some(functions) = at.get(&(index + 1)) {
            for (function, calls) in functions {
                let indent = &line[..line.len() - line.trim_start().len()];
                let opening = source.len() - 1;
                let heading = declaration_row[&(function.line as usize - 1)];
                let body_indent = if review { "  " } else { "" };
                if let Some(behavior) = &calls.behavior {
                    for (offset, summary) in std::iter::once(behavior.label())
                        .chain(behavior.text().lines())
                        .enumerate()
                    {
                        let summary_indent = if offset == 0 { "" } else { "  " };
                        behavior_row.insert(
                            source.len(),
                            BehaviorRow {
                                owner: function.owner.clone(),
                                offset: offset as u32,
                                category: behavior.label(),
                                column: if summary.is_empty() {
                                    0
                                } else {
                                    indent.len() + body_indent.len() + summary_indent.len()
                                },
                            },
                        );
                        if summary.is_empty() {
                            output.push('\n');
                        } else {
                            output.push_str(&format!(
                                "{indent}{body_indent}{summary_indent}{summary}\n"
                            ));
                        }
                        source.push(DeclarationPosition {
                            line: function.line,
                            column: function.column,
                        });
                    }
                }
                if review {
                    body.insert(
                        opening,
                        CallBody {
                            heading,
                            end: source.len(),
                            declaration: line.into(),
                        },
                    );
                    output.push_str(&format!("{indent}{}\n", if lua { "end" } else { "}" }));
                    source.push(DeclarationPosition {
                        line: function.line,
                        column: function.column,
                    });
                }
            }
        }
    }
    let plain_row = source
        .iter()
        .map(|position| position.line as usize - 1)
        .collect();
    Ok(CallPresentation {
        visibility: Default::default(),
        folding: Default::default(),
        callable: Default::default(),
        plain: text.into(),
        declaration: DeclarationPresentation {
            text: output,
            source: source.into_iter().map(Some).collect(),
        },
        behavior_row,
        body,
        plain_row,
        declaration_row,
    })
}

/// Apply declaration visibility to synthetic call rows through their owning signature.
pub(crate) fn visibility(
    path: &str,
    presentation: &CallPresentation,
    public_only: bool,
) -> Result<forge_diff::syntax::DeclarationVisibility> {
    let cached = &presentation.visibility[usize::from(public_only)];
    if let Some(visibility) = cached.get() {
        return Ok(forge_diff::syntax::DeclarationVisibility {
            rows: visibility.rows.clone(),
            replacement: visibility.replacement.clone(),
        });
    }
    let visibility =
        forge_diff::syntax::DeclarationVisibility::analyze(path, &presentation.plain, public_only)
            .map_err(|error| anyhow::anyhow!("{error:?}"))?;
    let result = forge_diff::syntax::DeclarationVisibility {
        rows: presentation
            .plain_row
            .iter()
            .map(|row| visibility.rows.get(*row).copied().unwrap_or(false))
            .collect(),
        replacement: visibility
            .replacement
            .iter()
            .filter_map(|(plain, text)| {
                presentation
                    .declaration_row
                    .get(plain)
                    .map(|row| (*row, text.clone()))
            })
            .collect(),
    };
    let _ = cached.set(forge_diff::syntax::DeclarationVisibility {
        rows: result.rows.clone(),
        replacement: result.replacement.clone(),
    });
    Ok(result)
}

/// Check every behavior section at submission and return all violations together, so one
/// rejection carries every correction the planning model must make.
/// Bookkeeping rules apply only to repository declarations: fields and constructions of
/// dependency, standard-library, and prelude types need no reference section entries.
pub(super) fn validate_behaviors(
    design: &super::DeclarationDesign,
) -> Result<Vec<crate::declaration::DeclarationDiagnostic>> {
    let mut diagnostics = Vec::new();
    let parsed = design.parsed()?;
    for (path, text) in &parsed.proposed {
        let proposed = DeclarationCalls::extract(path, text, true)
            .map_err(|error| anyhow::anyhow!("{error:?}"))?;
        for function in proposed {
            let mut callable = CallableDiagnostics {
                path,
                function: &function,
                diagnostics: &mut diagnostics,
            };
            let body = parsed
                .proposed_calls
                .get(path)
                .into_iter()
                .flatten()
                .find(|body| body.owner == function.owner);
            let Some(behavior) = body.and_then(|body| body.behavior.as_ref()) else {
                continue;
            };
            if behavior.text().trim().is_empty() {
                let reason = match behavior {
                    FunctionBehavior::Algorithm(_) => format!(
                        "new callable {} requires a nonempty Algorithm block",
                        function.owner
                    ),
                    FunctionBehavior::Change(_) => format!(
                        "Change for {} requires a nonempty Algorithm diff",
                        function.owner
                    ),
                };
                callable.push(
                    DeclarationDiagnosticKind::BehaviorBlock,
                    &function.owner,
                    reason,
                );
                continue;
            }
            let validation = match behavior {
                FunctionBehavior::Algorithm(algorithm) => validate_algorithm(algorithm),
                FunctionBehavior::Change(change) => validate_algorithm_diff(change),
            };
            match validation {
                Ok(_) => {}
                Err(forge_diff::syntax::AlgorithmValidationError::Invalid(violations)) => {
                    for violation in violations {
                        callable.push(
                            DeclarationDiagnosticKind::AlgorithmSyntax,
                            &function.owner,
                            match behavior {
                                FunctionBehavior::Algorithm(_) => violation.to_string(),
                                FunctionBehavior::Change(_) => format!(
                                    "Change line {}, column {}: {}",
                                    violation.line, violation.column, violation.message
                                ),
                            },
                        );
                    }
                }
                Err(error) => anyhow::bail!("{path}: {}: {error}", function.owner),
            }
        }
    }
    Ok(diagnostics)
}

/// Collects one callable's behavior diagnostics at its signature.
struct CallableDiagnostics<'context> {
    path: &'context str,
    function: &'context forge_diff::syntax::DeclarationCallable,
    diagnostics: &'context mut Vec<crate::declaration::DeclarationDiagnostic>,
}

impl CallableDiagnostics<'_> {
    fn push(&mut self, kind: DeclarationDiagnosticKind, reference: &str, reason: String) {
        self.diagnostics
            .push(crate::declaration::DeclarationDiagnostic {
                path: self.path.to_owned(),
                line: self.function.line,
                column: self.function.column,
                reference: if reference == self.function.owner {
                    reference.to_owned()
                } else {
                    format!("{}: {reference}", self.function.owner)
                },
                error: true,
                reason,
                kind,
            });
    }
}

/// A semantic-structure problem at one line of a proposed file's editable text.
///
/// `plan_edit` stores text that fails with this error as written, and `plan_submit` reports the
/// line, so a structure mistake never discards the rest of an edit.
#[derive(Debug)]
pub(crate) struct DeclarationStructureError {
    /// One-based line in the editable text that the agent patches.
    pub line: usize,
    pub message: String,
}

impl std::fmt::Display for DeclarationStructureError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(formatter, "line {}: {}", self.line, self.message)
    }
}

impl std::error::Error for DeclarationStructureError {}

/// Build a structure error for the zero-based row `index`.
fn structure_error(index: usize, message: impl Into<String>) -> anyhow::Error {
    DeclarationStructureError {
        line: index + 1,
        message: message.into(),
    }
    .into()
}

fn is_section_heading(line: &str) -> bool {
    matches!(
        line.trim(),
        "Algorithm" | "Change" | "Calls" | "Constructs" | "Accesses"
    )
}

/// Return the exclusive end row of the section whose heading is row `heading`.
///
/// Presentation indents every body, but the agent may write one flush with its heading, so the
/// extent cannot depend on indentation alone. A body indented past its heading ends at the first
/// nonblank row that is not; behavior bodies may contain blank rows, and reference lists end at
/// one. A flush body ends at the first blank row or section heading. Trailing blank rows belong
/// to the surrounding declarations.
fn section_end(lines: &[&str], heading: usize, blank_rows_continue: bool) -> usize {
    let indent_of = |line: &str| line.len() - line.trim_start().len();
    let heading_indent = indent_of(lines[heading]);
    let indented = lines[heading + 1..]
        .iter()
        .find(|line| !line.trim().is_empty())
        .is_some_and(|line| indent_of(line) > heading_indent);
    let mut index = heading + 1;
    while index < lines.len() {
        let line = lines[index];
        let ends = if line.trim().is_empty() {
            !(indented && blank_rows_continue)
        } else if indented {
            indent_of(line) <= heading_indent
        } else {
            is_section_heading(line)
        };
        if ends {
            break;
        }
        index += 1;
    }
    while index > heading + 1 && lines[index - 1].trim().is_empty() {
        index -= 1;
    }
    index
}

fn protected_declaration_lines(
    path: &str,
    text: &str,
    lines: &[&str],
) -> Result<std::collections::BTreeSet<usize>> {
    let regions = DeclarationCalls::protected_regions(path, text)
        .map_err(|error| anyhow::anyhow!("{error:?}"))?;
    if !lines
        .iter()
        .any(|line| matches!(line.trim(), "Algorithm" | "Change"))
    {
        return Ok(regions.into_iter().flatten().collect());
    }
    let mut sections = Vec::new();
    let mut index = 0;
    while index < lines.len() {
        if is_section_heading(lines[index]) {
            let start = index;
            index = section_end(lines, index, true);
            sections.push(start..index);
        } else {
            index += 1;
        }
    }
    let source_regions = regions
        .into_iter()
        .filter(|region| {
            let preceding = sections.partition_point(|section| section.start < region.start);
            !preceding
                .checked_sub(1)
                .is_some_and(|index| sections[index].contains(&region.start))
        })
        .collect::<Vec<_>>();
    let mut masked = vec![false; lines.len()];
    for section in sections {
        let preceding = source_regions.partition_point(|region| region.start <= section.start);
        if !preceding
            .checked_sub(1)
            .is_some_and(|index| source_regions[index].contains(&section.start))
        {
            masked[section].fill(true);
        }
    }
    let mut declarations = String::with_capacity(text.len());
    for (row, line) in lines.iter().enumerate() {
        if masked[row] {
            declarations.extend(std::iter::repeat_n(' ', line.len()));
        } else {
            declarations.push_str(line);
        }
        declarations.push('\n');
    }
    Ok(DeclarationCalls::protected_regions(path, &declarations)
        .map_err(|error| anyhow::anyhow!("{error:?}"))?
        .into_iter()
        .flatten()
        .collect())
}

/// Split function summaries and references from signatures, retaining unchanged source evidence.
pub(crate) fn parse(path: &str, text: &str) -> Result<(String, Vec<FunctionBody>)> {
    if forge_diff::syntax::ConfigurationFormat::for_path(path).is_some() {
        return Ok((
            DeclarationOverview::parse(path, text).map_err(|error| anyhow::anyhow!("{error:?}"))?,
            Vec::new(),
        ));
    }
    ensure!(!text.contains('\0'), "combined declaration contains NUL");
    let lines = text.lines().collect::<Vec<_>>();
    let protected = protected_declaration_lines(path, text, &lines)?;
    let mut declaration = String::new();
    let mut block = Vec::new();
    let mut original_line = Vec::new();
    let mut index = 0;
    while index < lines.len() {
        let line = lines[index];
        if is_section_heading(line) && !protected.contains(&index) {
            let heading = index;
            if original_line.is_empty() {
                return Err(structure_error(
                    heading,
                    format!("{} must follow a callable declaration", line.trim()),
                ));
            }
            if !matches!(line.trim(), "Algorithm" | "Change") {
                return Err(structure_error(
                    heading,
                    format!(
                        "{} sections are not written in plans; describe what the callable uses in its Algorithm or Change",
                        line.trim()
                    ),
                ));
            }
            let end = section_end(&lines, heading, true);
            let start = heading + 1;
            index = end;
            let summary_indent = lines[start..end]
                .iter()
                .filter(|value| !value.trim().is_empty())
                .map(|value| value.len() - value.trim_start().len())
                .min()
                .ok_or_else(|| {
                    structure_error(
                        heading,
                        format!(
                            "{} requires nonempty content on the lines directly below it",
                            line.trim()
                        ),
                    )
                })?;
            let content = lines[start..end]
                .iter()
                .map(|value| {
                    if value.trim().is_empty() {
                        ""
                    } else {
                        &value[summary_indent..]
                    }
                })
                .collect::<Vec<_>>()
                .join("\n");
            let behavior = if line.trim() == "Algorithm" {
                FunctionBehavior::Algorithm(content)
            } else {
                FunctionBehavior::Change(content)
            };
            block.push((original_line.len(), heading, behavior));
        } else {
            declaration.push_str(line);
            declaration.push('\n');
            original_line.push(index);
            index += 1;
        }
    }
    let declaration = DeclarationOverview::parse(path, &declaration)
        .map_err(|error| anyhow::anyhow!("{error:?}"))?;
    let functions = DeclarationCalls::extract(path, &declaration, true)
        .map_err(|error| anyhow::anyhow!("{error:?}"))?;
    let mut behaviors = BTreeMap::<usize, FunctionBehavior>::new();
    let by_line = functions
        .iter()
        .map(|function| (function.end_line as usize, function))
        .collect::<BTreeMap<_, _>>();
    for (line, heading, behavior) in block {
        let function = by_line.get(&line).ok_or_else(|| {
            structure_error(
                heading,
                format!(
                    "{} must directly follow a callable signature",
                    lines[heading].trim()
                ),
            )
        })?;
        if behaviors.insert(line, behavior).is_some() {
            return Err(structure_error(
                heading,
                format!(
                    "{} already has a behavior block; a callable has one Algorithm (new) or one Change (existing), never both",
                    function.owner
                ),
            ));
        }
    }
    let output = functions
        .iter()
        .filter_map(|function| {
            behaviors
                .remove(&(function.end_line as usize))
                .map(|behavior| FunctionBody {
                    owner: function.owner.clone(),
                    behavior: Some(behavior),
                })
        })
        .collect();
    Ok((declaration, output))
}

#[cfg(test)]
mod tests {
    use super::{FunctionBehavior, FunctionBody, validate_behaviors};
    use crate::declaration::DeclarationDiagnosticKind;
    use crate::plan::DeclarationDesign;

    const DECLARATIONS: &str = "use bevy::prelude::*;

#[derive(Component)]
struct Velocity(Vec2);

#[derive(Component)]
struct Player {
  speed: f32,
}

#[derive(Resource)]
struct RunSession {
  score: u32,
}

/// Spawn the player entity.
fn spawn_player(commands: Commands);

/// Move the player and record the score.
fn move_player(player: &Player, transform: &mut Transform, session: &RunSession);

/// Report the score.
fn report_score(session: &RunSession);
";

    fn body(owner: &str, algorithm: &str) -> FunctionBody {
        FunctionBody {
            owner: owner.into(),
            behavior: Some(FunctionBehavior::Algorithm(algorithm.into())),
        }
    }

    #[test]
    fn presentations_hold_only_behavior_blocks() {
        let body = body("report_score", "record(session.score)");
        for review in [true, false] {
            let presentation = super::insert(
                "src/main.rs",
                DECLARATIONS,
                std::slice::from_ref(&body),
                review,
            )
            .expect("presentation");
            let text = &presentation.declaration.text;
            let rows = text.lines().map(str::trim).collect::<Vec<_>>();
            assert!(
                rows.windows(2)
                    .any(|pair| pair == ["Algorithm", "record(session.score)"]),
                "{text}"
            );
            for heading in ["Calls", "Constructs", "Accesses"] {
                assert!(
                    !text.lines().any(|row| row.trim() == heading),
                    "{heading} in {text}"
                );
            }
        }
    }

    /// Validate provided Algorithms without inferring classification from baseline file identity.
    #[test]
    fn behavior_classification_belongs_to_the_authored_block() {
        let mut design = DeclarationDesign::scratch(std::collections::BTreeMap::from([(
            "renamed.rs".into(),
            "pub fn existing();\npub fn modified();\npub fn unchanged();\n".into(),
        )]))
        .unwrap();
        design.baseline.insert(
            "original.rs".into(),
            crate::plan::DeclarationFile::new(
                "original.rs",
                "pub fn existing();\n".into(),
                "original-source".into(),
                &[],
            )
            .unwrap(),
        );
        design.parsed_mut().unwrap().proposed_calls.insert(
            "renamed.rs".into(),
            vec![
                body("existing", "return none"),
                FunctionBody {
                    owner: "modified".into(),
                    behavior: Some(FunctionBehavior::Change(
                        "- return old\n+ return new".into(),
                    )),
                },
            ],
        );
        assert!(validate_behaviors(&design).unwrap().is_empty());
        design
            .parsed_mut()
            .unwrap()
            .proposed_calls
            .get_mut("renamed.rs")
            .unwrap()[1]
            .behavior = Some(FunctionBehavior::Change(String::new()));
        assert!(
            validate_behaviors(&design)
                .unwrap()
                .iter()
                .any(
                    |diagnostic| diagnostic.kind == DeclarationDiagnosticKind::BehaviorBlock
                        && diagnostic.reference == "modified"
                )
        );
        design
            .parsed_mut()
            .unwrap()
            .proposed_calls
            .get_mut("renamed.rs")
            .unwrap()[1]
            .behavior = Some(FunctionBehavior::Change(
            "Preserve the caller contract.".into(),
        ));
        design
            .parsed_mut()
            .unwrap()
            .proposed_calls
            .get_mut("renamed.rs")
            .unwrap()[0] = body("existing", "prose is not a statement");
        let diagnostics = validate_behaviors(&design).unwrap();
        assert!(!diagnostics.is_empty());
        assert!(
            diagnostics
                .iter()
                .all(|diagnostic| diagnostic.kind == DeclarationDiagnosticKind::AlgorithmSyntax)
        );
        assert!(diagnostics.iter().any(|diagnostic| {
            diagnostic.reference == "modified"
                && diagnostic.reason.contains("Change line 1, column 1")
        }));
    }

    #[test]
    fn change_syntax_preserves_authored_columns_and_omits_legacy_prose() {
        let bodies = [
            FunctionBody {
                owner: "S::save".into(),
                behavior: Some(FunctionBehavior::Change(
                    "...\n- if old\n+ if new\n    ... # existing body\n  return value".into(),
                )),
            },
            FunctionBody {
                owner: "S::load".into(),
                behavior: Some(FunctionBehavior::Change("Keep the old value.".into())),
            },
        ];
        let presentation = super::insert(
            "src/lib.rs",
            "impl S {\n  fn save(&self, value: u32);\n  fn load(&self);\n}\n",
            &bodies,
            true,
        )
        .unwrap();
        let syntax = presentation.algorithm_source().unwrap();
        assert_eq!(
            syntax
                .lines()
                .filter(|line| !line.is_empty())
                .collect::<Vec<_>>(),
            ["        if old", "        if new", "        return value"]
        );
        assert_eq!(
            syntax.lines().count(),
            presentation.declaration.text.lines().count()
        );
    }

    #[test]
    fn parsed_change_preserves_the_diff_column_after_a_leading_elision() {
        let authored = concat!(
            "fn save(value: u32);\n",
            "Change\n",
            "  ...\n",
            "  - if old\n",
            "  + if new\n",
            "      record(value)\n",
        );
        let (_, bodies) = super::parse("src/lib.rs", authored).unwrap();
        let behavior = bodies[0].behavior.as_ref().unwrap();
        assert_eq!(
            behavior.text(),
            "...\n- if old\n+ if new\n    record(value)"
        );
        assert!(super::validate_algorithm_diff(behavior.text()).is_ok());
    }

    #[test]
    fn change_navigation_maps_displayed_columns_to_a_signature_binding() {
        let body = FunctionBody {
            owner: "save".into(),
            behavior: Some(FunctionBehavior::Change(
                "- return value\n+ return replace(value)".into(),
            )),
        };
        for (offset, row) in [(1, "    return value"), (2, "    return replace(value)")] {
            let target = super::algorithm_reference(
                "src/lib.rs",
                "fn save(value: u32);\n",
                &body,
                offset,
                row,
                row.find("value").unwrap(),
            )
            .unwrap()
            .unwrap();
            assert!(
                matches!(target, super::AlgorithmTarget::Binding { name, .. } if name == "value")
            );
        }
    }

    #[test]
    /// Submission checks Algorithm syntax only; fields and constructions an Algorithm names need
    /// no reference entries.
    fn behavior_validation_reports_algorithm_syntax_without_reference_rules() {
        let mut design = DeclarationDesign::scratch(std::collections::BTreeMap::from([(
            "src/main.rs".into(),
            DECLARATIONS.into(),
        )]))
        .expect("design");
        design.parsed_mut().expect("parsed").proposed_calls.insert(
            "src/main.rs".into(),
            vec![
                body(
                    "spawn_player",
                    "velocity ← Velocity((0, 0))\nplayer ← Player(speed = 5)\nlabel ← Text2d(\"score\")\nspawn(commands, [velocity, player, label])",
                ),
                body(
                    "move_player",
                    "transform.translation ← transform.translation + player.speed\ntotal <- session.score\nif total mod 2 = 0\n  record(total)",
                ),
                body(
                    "report_score",
                    "process candidate now\nlimit ← ← 2",
                ),
            ],
        );
        let mut reported = validate_behaviors(&design)
            .expect("validation")
            .into_iter()
            .map(|diagnostic| (diagnostic.kind, diagnostic.reference))
            .collect::<Vec<_>>();
        reported.sort();
        assert_eq!(
            reported,
            vec![
                (
                    DeclarationDiagnosticKind::AlgorithmSyntax,
                    "report_score".to_owned()
                ),
                (
                    DeclarationDiagnosticKind::AlgorithmSyntax,
                    "report_score".to_owned()
                ),
            ]
        );
    }
}
