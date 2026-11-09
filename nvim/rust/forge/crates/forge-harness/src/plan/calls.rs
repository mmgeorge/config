use std::collections::{BTreeMap, VecDeque};
use std::sync::{Arc, Mutex, OnceLock};

use crate::declaration::DeclarationDiagnosticKind;
use anyhow::{Context, Result, ensure};
use forge_diff::syntax::{
    DeclarationCalls, DeclarationOverview, DeclarationPosition, DeclarationPresentation,
};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

const PRESENTATION_CACHE_BYTES: usize = 8 * 1024 * 1024;
const PRESENTATION_CACHE_ENTRIES: usize = 64;
static PRESENTATION_CACHE: OnceLock<Mutex<VecDeque<CachedPresentation>>> = OnceLock::new();

/// The semantic operation of one saved function-body reference.
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

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub(crate) enum ReferenceCategory {
    Calls,
    Accesses,
    Constructs,
}

impl ReferenceCategory {
    fn default_kind(self) -> CallKind {
        match self {
            Self::Calls => CallKind::Call,
            Self::Accesses => CallKind::Property,
            Self::Constructs => CallKind::Construct,
        }
    }
}

impl CallKind {
    /// Group semantic references into editable body sections.
    pub(crate) fn category(self) -> ReferenceCategory {
        match self {
            Self::Call | Self::Callback => ReferenceCategory::Calls,
            Self::Property | Self::Value => ReferenceCategory::Accesses,
            Self::Construct => ReferenceCategory::Constructs,
        }
    }
    /// Preserve canonical bytes for older invocation-only plan snapshots.
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

/// One function-body occurrence, retaining its operation and source order independently of display sorting.
#[derive(Clone, Debug, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
pub struct CallSite {
    /// Older saved plans default to invocation references.
    #[serde(default, skip_serializing_if = "CallKind::is_call")]
    pub kind: CallKind,
    /// Target name without arguments, qualified only when type evidence exists.
    pub name: String,
    /// Original source evidence, absent for a newly authored occurrence.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub source: Option<CallPosition>,
    /// Retains opaque local binding evidence without changing the displayed target name.
    #[serde(default, skip_serializing_if = "std::ops::Not::not")]
    pub unresolved: bool,
}

/// An immutable source position for an extracted call.
#[derive(Clone, Debug, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
pub struct CallPosition {
    /// One-based line in the captured source.
    pub line: u32,
    /// Zero-based byte column in the captured source.
    pub column: u32,
    /// Exact captured expression, separate from the inferred target name.
    #[serde(default, skip_serializing_if = "String::is_empty")]
    pub expression: String,
}

/// Saved behavioral intent and ordered references belonging to one callable.
#[derive(Clone, Debug, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
pub struct FunctionBody {
    /// Presence of each authored category. None retains unknown historical evidence.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub evidence: Option<ReferenceEvidence>,
    /// Callable identity within its saved declaration file.
    pub owner: String,
    /// Authored algorithm for a new callable or behavior change for an existing callable.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub behavior: Option<FunctionBehavior>,
    /// Ordered occurrences, including repeated targets. Absence means references are unavailable.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub call: Option<Vec<CallSite>>,
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
}

/// Distinguishes omitted reference categories from explicitly empty blocks.
#[derive(Clone, Copy, Debug, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
pub struct ReferenceEvidence {
    /// Whether invocations and callbacks were inspected or authored.
    pub calls: bool,
    /// Whether properties and named values were inspected or authored.
    pub accesses: bool,
    /// Whether direct constructions and factory calls were inspected or authored.
    pub constructs: bool,
}

impl ReferenceEvidence {
    /// Tests whether the category has known evidence, including an empty list.
    pub(crate) fn contains(self, category: ReferenceCategory) -> bool {
        match category {
            ReferenceCategory::Calls => self.calls,
            ReferenceCategory::Accesses => self.accesses,
            ReferenceCategory::Constructs => self.constructs,
        }
    }
}

/// One reference-section entry and the byte columns its text occupies on a rendered row.
#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct CallRowTarget {
    pub owner: String,
    pub name: String,
    pub kind: CallKind,
    pub start: usize,
    pub end: usize,
}

/// Maps display rows to saved declarations and call identities.
pub(crate) struct CallPresentation {
    visibility: [OnceLock<forge_diff::syntax::DeclarationVisibility>; 2],
    folding: OnceLock<Vec<forge_diff::syntax::DeclarationFold>>,
    callable: OnceLock<Vec<forge_diff::syntax::DeclarationCallable>>,
    /// Signature-only text used by declaration analysis before synthetic bodies are rendered.
    pub plain: String,
    pub declaration: DeclarationPresentation,
    /// Reference-section entries on each rendered row. Review rows group Accesses entries
    /// by declaring type, so one row can carry several entries at distinct columns.
    pub call_row: BTreeMap<usize, Vec<CallRowTarget>>,
    pub behavior_row: BTreeMap<usize, (String, u32, &'static str)>,
    /// Behavior and reference sections keyed by heading row, with their exclusive end row and
    /// heading name.
    pub section: BTreeMap<usize, (usize, &'static str)>,
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

    /// Preserve displayed coordinates while isolating Algorithm rows from native declarations.
    pub(crate) fn algorithm_source(&self) -> Option<String> {
        self.behavior_row
            .values()
            .any(|(_, _, category)| *category == "Algorithm")
            .then(|| {
                self.declaration
                    .text
                    .lines()
                    .enumerate()
                    .map(|(row, text)| {
                        if self
                            .behavior_row
                            .get(&row)
                            .is_some_and(|(_, _, category)| *category == "Algorithm")
                        {
                            format!("{text}\n")
                        } else {
                            "\n".into()
                        }
                    })
                    .collect()
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
    /// Saved callable identity used to retain fold intent across rendering.
    pub owner: String,
    /// First signature row that can toggle this body.
    pub heading: usize,
    /// Closing delimiter row included in this body.
    pub end: usize,
    /// Completes the visible signature when its body is collapsed.
    pub collapsed_suffix: &'static str,
}

struct CachedPresentation {
    key: String,
    bytes: usize,
    value: Arc<CallPresentation>,
}

pub(crate) fn from_extracted(
    functions: Vec<forge_diff::syntax::DeclarationCallable>,
) -> Vec<FunctionBody> {
    functions
        .into_iter()
        .map(|function| FunctionBody {
            evidence: Some(ReferenceEvidence {
                calls: true,
                accesses: true,
                constructs: true,
            }),
            owner: function.owner,
            behavior: None,
            call: Some(
                function
                    .call
                    .into_iter()
                    .map(|call| CallSite {
                        kind: match call.kind {
                            forge_diff::syntax::DeclarationCallKind::Call => CallKind::Call,
                            forge_diff::syntax::DeclarationCallKind::Property => CallKind::Property,
                            forge_diff::syntax::DeclarationCallKind::Callback => CallKind::Callback,
                            forge_diff::syntax::DeclarationCallKind::Value => CallKind::Value,
                            forge_diff::syntax::DeclarationCallKind::Construct => {
                                CallKind::Construct
                            }
                        },
                        name: call.name,
                        unresolved: call.unresolved,
                        source: Some(CallPosition {
                            line: call.line,
                            column: call.column,
                            expression: call.expression,
                        }),
                    })
                    .collect(),
            ),
        })
        .collect()
}

/// Refine non-invoking references using captured declaration identities without source discovery.
pub(crate) fn classify(
    path: &str,
    functions: &mut [FunctionBody],
    resolver: &mut crate::declaration::DeclarationResolver,
) {
    let mut classification = BTreeMap::new();
    for function in functions {
        for reference in function.call.iter_mut().flatten() {
            let invocation = reference.kind == CallKind::Call;
            if reference.unresolved
                || !(matches!(reference.kind, CallKind::Value | CallKind::Property) || invocation)
            {
                continue;
            }
            let key = (
                function.owner.clone(),
                reference.name.clone(),
                reference.kind,
                reference.source.is_some(),
            );
            let kind = classification.entry(key).or_insert_with(|| {
                let target = resolver.value(path, &function.owner, &reference.name);
                match resolver.role(&target) {
                    Some(
                        forge_diff::syntax::DeclarationRole::Type
                        | forge_diff::syntax::DeclarationRole::Variant,
                    ) if path.ends_with(".rs") && reference.source.is_some() => CallKind::Construct,
                    Some(forge_diff::syntax::DeclarationRole::Callable)
                        if invocation
                            && reference.source.is_some()
                            && resolver.factory(&target) =>
                    {
                        CallKind::Construct
                    }
                    _ if invocation => CallKind::Call,
                    Some(forge_diff::syntax::DeclarationRole::Callable) => CallKind::Callback,
                    Some(
                        forge_diff::syntax::DeclarationRole::Value
                        | forge_diff::syntax::DeclarationRole::Type
                        | forge_diff::syntax::DeclarationRole::Module,
                    ) => CallKind::Value,
                    _ => reference.kind,
                }
            });
            reference.kind = *kind;
            if *kind == CallKind::Construct
                && let Some(evidence) = &mut function.evidence
            {
                evidence.constructs = true;
            }
        }
    }
}

/// Return an editable file view in declaration and stored call order.
pub(crate) fn combined(path: &str, text: &str, calls: &[FunctionBody]) -> Result<String> {
    Ok(insert(path, text, calls, false, false)?.declaration.text)
}

/// A signature binding or declaration reference selected in an Algorithm.
pub(crate) enum AlgorithmTarget {
    Binding {
        name: String,
        position: forge_diff::syntax::DeclarationPosition,
    },
    Reference(CallSite),
}

/// Resolve a parsed Algorithm identifier using signature bindings before reference metadata.
pub(crate) fn algorithm_reference(
    path: &str,
    declarations: &str,
    body: &FunctionBody,
    offset: u32,
    row: &str,
    column: usize,
) -> Result<Option<AlgorithmTarget>> {
    let Some(FunctionBehavior::Algorithm(algorithm)) = &body.behavior else {
        return Ok(None);
    };
    let Some(source) = offset
        .checked_sub(1)
        .and_then(|offset| algorithm.lines().nth(offset as usize))
    else {
        return Ok(None);
    };
    let Some(indent) = row
        .len()
        .checked_sub(source.len())
        .filter(|_| row.ends_with(source))
    else {
        return Ok(None);
    };
    let Some(column) = column.checked_sub(indent).map(|column| column + 1) else {
        return Ok(None);
    };
    let Some(reference) =
        forge_diff::syntax::algorithm_reference(algorithm, offset as usize, column)
            .map_err(|error| anyhow::anyhow!("{error:?}"))?
    else {
        return Ok(None);
    };
    let function = DeclarationCalls::extract(path, declarations, true)
        .map_err(|error| anyhow::anyhow!("{error:?}"))?
        .into_iter()
        .find(|function| function.owner == body.owner)
        .context("Algorithm callable signature is unavailable")?;
    resolve_algorithm_reference(path, &function, body, reference)
}

/// Whether the Algorithm invocation at a review column names a `Calls` or `Constructs` entry.
/// Abstract operations such as `delta(time)` describe behavior without naming plan declarations,
/// so review highlighting must not present them as calls into the plan.
pub(crate) fn declared_algorithm_call(
    path: &str,
    function: &forge_diff::syntax::DeclarationCallable,
    body: &FunctionBody,
    offset: u32,
    row: &str,
    column: usize,
) -> bool {
    let Some(FunctionBehavior::Algorithm(algorithm)) = &body.behavior else {
        return false;
    };
    let Some(source) = offset
        .checked_sub(1)
        .and_then(|offset| algorithm.lines().nth(offset as usize))
    else {
        return false;
    };
    let Some(indent) = row
        .len()
        .checked_sub(source.len())
        .filter(|_| row.ends_with(source))
    else {
        return false;
    };
    let Some(column) = column.checked_sub(indent).map(|column| column + 1) else {
        return false;
    };
    let Ok(Some(reference)) =
        forge_diff::syntax::algorithm_reference(algorithm, offset as usize, column)
    else {
        return false;
    };
    let Ok(Some(AlgorithmTarget::Reference(target))) =
        resolve_algorithm_reference(path, function, body, reference)
    else {
        return false;
    };
    body.call.iter().flatten().any(|call| {
        matches!(
            call.kind,
            CallKind::Call | CallKind::Callback | CallKind::Construct
        ) && call.name.replace("::", ".") == target.name.replace("::", ".")
    })
}

/// Resolve a parsed segment consistently for navigation, validation, and rename.
pub(crate) fn resolve_algorithm_reference(
    path: &str,
    function: &forge_diff::syntax::DeclarationCallable,
    body: &FunctionBody,
    reference: forge_diff::syntax::AlgorithmReference,
) -> Result<Option<AlgorithmTarget>> {
    if reference.first
        && let Some(position) = function.binding_position.get(&reference.segment)
    {
        return Ok(Some(AlgorithmTarget::Binding {
            name: reference.segment,
            position: *position,
        }));
    }
    if !reference.last {
        return Ok(reference
            .segment
            .chars()
            .next()
            .is_some_and(char::is_uppercase)
            .then(|| {
                AlgorithmTarget::Reference(CallSite {
                    name: reference
                        .prefix
                        .replace('.', if path.ends_with(".rs") { "::" } else { "." }),
                    kind: CallKind::Value,
                    source: None,
                    unresolved: false,
                })
            }));
    }
    let member = reference.segment;
    let separator = if path.ends_with(".rs") { "::" } else { "." };
    let receiver = reference
        .name
        .rsplit_once('.')
        .map(|(receiver, _)| receiver);
    let declared_type = receiver
        .and_then(|receiver| function.binding.get(receiver))
        .and_then(|binding| binding.as_deref());
    let types = declared_type
        .into_iter()
        .flat_map(|declared| {
            declared
                .trim()
                .trim_start_matches(':')
                .split(|character: char| {
                    !character.is_alphanumeric() && !"_:$.'".contains(character)
                })
        })
        .filter(|part| {
            !part.is_empty()
                && !part.starts_with('\'')
                && !matches!(*part, "mut" | "const" | "dyn" | "impl")
        })
        .collect::<std::collections::BTreeSet<_>>()
        .into_iter()
        .collect::<Vec<_>>();
    let qualified = reference.name.contains("::")
        || receiver.is_some_and(|receiver| receiver.chars().next().is_some_and(char::is_uppercase));
    let eligible = |call: &&CallSite| {
        if reference.invocation {
            matches!(
                call.kind,
                CallKind::Call | CallKind::Callback | CallKind::Construct
            )
        } else {
            matches!(call.kind, CallKind::Property | CallKind::Value)
        }
    };
    let mut candidates = body
        .call
        .iter()
        .flatten()
        .filter(eligible)
        .filter(|call| {
            if qualified {
                call.name.replace("::", ".") == reference.name.replace("::", ".")
            } else {
                call.name.rsplit([':', '.']).next() == Some(member.as_str())
            }
        })
        .map(|call| ((call.name.clone(), call.kind), call.clone()))
        .collect::<std::collections::BTreeMap<_, _>>();
    if !types.is_empty() {
        let typed = candidates
            .iter()
            .filter(|((name, _), _)| {
                name.rsplit_once("::")
                    .or_else(|| name.rsplit_once('.'))
                    .is_some_and(|(container, _)| {
                        types.iter().any(|declared| {
                            container == *declared
                                || (!declared.contains([':', '.'])
                                    && container.rsplit([':', '.']).next() == Some(*declared))
                        })
                    })
            })
            .map(|(key, call)| (key.clone(), call.clone()))
            .collect::<std::collections::BTreeMap<_, _>>();
        if !typed.is_empty() {
            candidates = typed;
        } else if let [receiver_type] = types.as_slice() {
            let name = format!("{receiver_type}{separator}{member}");
            let kind = if reference.invocation {
                CallKind::Call
            } else {
                CallKind::Property
            };
            return Ok(Some(AlgorithmTarget::Reference(CallSite {
                name,
                kind,
                source: None,
                unresolved: false,
            })));
        }
    }
    ensure!(
        candidates.len() <= 1,
        "Algorithm reference `{}` is ambiguous: {}",
        reference.name,
        candidates
            .keys()
            .map(|(name, _)| name.as_str())
            .collect::<Vec<_>>()
            .join(", ")
    );
    if let Some((_, call)) = candidates.pop_first() {
        return Ok(Some(AlgorithmTarget::Reference(call)));
    }
    if !reference.invocation {
        return Ok(qualified.then(|| {
            AlgorithmTarget::Reference(CallSite {
                name: reference.name.replace('.', separator),
                kind: CallKind::Property,
                source: None,
                unresolved: false,
            })
        }));
    }
    if receiver.is_none() && !qualified && !member.chars().next().is_some_and(char::is_uppercase) {
        return Ok(None);
    }
    let name = if receiver.is_some() && !qualified {
        let [receiver_type] = types.as_slice() else {
            anyhow::bail!("Algorithm receiver has no unique signature type or Calls target");
        };
        format!("{receiver_type}{separator}{member}")
    } else {
        reference.name
    };
    let kind = if member.chars().next().is_some_and(char::is_uppercase) {
        CallKind::Construct
    } else {
        CallKind::Call
    };
    Ok(Some(AlgorithmTarget::Reference(CallSite {
        name,
        kind,
        source: None,
        unresolved: false,
    })))
}

/// Render function summaries and unique references while preserving saved occurrence order.
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
    const SORT_FUNCTION_USES: bool = false;
    let mut result = insert(path, &presentation.text, calls, true, SORT_FUNCTION_USES)?;
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
            .call_row
            .values()
            .flatten()
            .map(|target| target.owner.capacity() + target.name.capacity() + 128)
            .sum::<usize>()
        + result
            .behavior_row
            .values()
            .map(|(owner, _, _)| owner.capacity() + 96)
            .sum::<usize>()
        + result
            .body
            .values()
            .map(|body| body.owner.capacity() + 128)
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

/// One rendered reference row and the entries it shows, each with its byte span in `text`.
struct ReferenceRow {
    text: String,
    entry: Vec<(String, CallKind, usize, usize)>,
}

/// Group Accesses entries into one `Type { member, ... }` row per declaring type, in order of
/// first appearance. A bare type entry folds into its type's row; a type with no members keeps
/// a row of its own. The type is the leading path through its first capitalized segment, so
/// `Transform::translation::xy` shows as `translation::xy` within `Transform`.
fn grouped_access_rows(targets: &[(&str, CallKind)]) -> Vec<ReferenceRow> {
    let mut group = Vec::<(
        String,
        Option<(String, CallKind)>,
        Vec<(String, String, CallKind)>,
    )>::new();
    for (name, kind) in targets {
        let separator = if name.contains("::") { "::" } else { "." };
        let segments = name.split(separator).collect::<Vec<_>>();
        let key_length = segments
            .iter()
            .position(|segment| segment.chars().next().is_some_and(char::is_uppercase))
            .map_or(1, |index| index + 1)
            .min(segments.len());
        let key = segments[..key_length].join(separator);
        let member = segments[key_length..].join(separator);
        let index = match group.iter().position(|(existing, _, _)| *existing == key) {
            Some(index) => index,
            None => {
                group.push((key.clone(), None, Vec::new()));
                group.len() - 1
            }
        };
        if member.is_empty() {
            group[index].1 = Some((name.to_string(), *kind));
        } else {
            group[index].2.push((member, name.to_string(), *kind));
        }
    }
    group
        .into_iter()
        .map(|(key, bare, members)| {
            let mut text = key.clone();
            let mut entry = bare
                .map(|(name, kind)| vec![(name, kind, 0, key.len())])
                .unwrap_or_default();
            if !members.is_empty() {
                text.push_str(" { ");
                for (index, (member, name, kind)) in members.into_iter().enumerate() {
                    if index > 0 {
                        text.push_str(", ");
                    }
                    entry.push((name, kind, text.len(), text.len() + member.len()));
                    text.push_str(&member);
                }
                text.push_str(" }");
            }
            ReferenceRow { text, entry }
        })
        .collect()
}

pub(crate) fn insert(
    path: &str,
    text: &str,
    calls: &[FunctionBody],
    review: bool,
    sort: bool,
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
            call_row: BTreeMap::new(),
            behavior_row: BTreeMap::new(),
            section: BTreeMap::new(),
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
    let mut call_row = BTreeMap::new();
    let mut behavior_row = BTreeMap::new();
    let mut section = BTreeMap::new();
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
                    let behavior_heading = source.len();
                    for (offset, summary) in std::iter::once(behavior.label())
                        .chain(behavior.text().lines())
                        .enumerate()
                    {
                        behavior_row.insert(
                            source.len(),
                            (function.owner.clone(), offset as u32, behavior.label()),
                        );
                        let summary_indent = if offset == 0 { "" } else { "  " };
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
                    section.insert(behavior_heading, (source.len(), behavior.label()));
                }
                for kind in [
                    ReferenceCategory::Calls,
                    ReferenceCategory::Constructs,
                    ReferenceCategory::Accesses,
                ] {
                    if calls
                        .evidence
                        .is_some_and(|evidence| !evidence.contains(kind))
                    {
                        continue;
                    }
                    let Some(occurrences) = &calls.call else {
                        continue;
                    };
                    let mut targets = calls
                        .call
                        .iter()
                        .flatten()
                        .filter(|call| call.kind.category() == kind)
                        .map(|call| (call.name.as_str(), call.kind))
                        .collect::<Vec<_>>();
                    if targets.is_empty()
                        && calls.evidence.is_none()
                        && !(kind == ReferenceCategory::Calls && occurrences.is_empty())
                    {
                        continue;
                    }
                    if review {
                        let mut seen = std::collections::HashSet::with_capacity(targets.len());
                        targets.retain(|(name, _)| seen.insert(*name));
                        if sort {
                            targets.sort_unstable();
                        }
                    }
                    let heading = match kind {
                        ReferenceCategory::Calls => "Calls",
                        ReferenceCategory::Accesses => "Accesses",
                        ReferenceCategory::Constructs => "Constructs",
                    };
                    output.push_str(&format!("{indent}{body_indent}{heading}\n"));
                    let reference_heading = source.len();
                    source.push(DeclarationPosition {
                        line: function.line,
                        column: function.column,
                    });
                    let rows = if review && kind == ReferenceCategory::Accesses {
                        grouped_access_rows(&targets)
                    } else {
                        targets
                            .iter()
                            .map(|(name, kind)| ReferenceRow {
                                text: (*name).to_owned(),
                                entry: vec![(name.to_string(), *kind, 0, name.len())],
                            })
                            .collect()
                    };
                    for row in rows {
                        let prefix = format!("{indent}{body_indent}  ");
                        call_row.insert(
                            source.len(),
                            row.entry
                                .into_iter()
                                .map(|(name, kind, start, end)| CallRowTarget {
                                    owner: function.owner.clone(),
                                    name,
                                    kind,
                                    start: prefix.len() + start,
                                    end: prefix.len() + end,
                                })
                                .collect(),
                        );
                        output.push_str(&format!("{prefix}{}\n", row.text));
                        source.push(DeclarationPosition {
                            line: function.line,
                            column: function.column,
                        });
                    }
                    section.insert(reference_heading, (source.len(), heading));
                }
                if review {
                    body.insert(
                        opening,
                        CallBody {
                            owner: function.owner.clone(),
                            heading,
                            end: source.len(),
                            collapsed_suffix: if lua { " ... end" } else { "...}" },
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
        call_row,
        behavior_row,
        section,
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

enum BodySection {
    Behavior(FunctionBehavior),
    /// Reference entries with the zero-based editable row each was written on.
    References(ReferenceCategory, Vec<(String, CallKind, usize)>),
}

/// Check every behavior section at submission and return all violations together, so one
/// rejection carries every correction the planning model must make.
/// Bookkeeping rules apply only to repository declarations: fields and constructions of
/// dependency, standard-library, and prelude types need no reference section entries.
pub(super) fn validate_behaviors(
    design: &super::DeclarationDesign,
    resolver: &mut crate::declaration::DeclarationResolver,
) -> Result<Vec<crate::declaration::DeclarationDiagnostic>> {
    let mut diagnostics = Vec::new();
    let parsed = design.parsed()?;
    for (path, text) in &parsed.proposed {
        let original_path = design
            .moved
            .iter()
            .find(|(_, destination)| *destination == path)
            .map_or(path, |(original, _)| original);
        let original = design
            .baseline
            .get(original_path)
            .map(|file| DeclarationCalls::extract(original_path, &file.text, true))
            .transpose()
            .map_err(|error| anyhow::anyhow!("{error:?}"))?
            .unwrap_or_default();
        let original_owner = original
            .iter()
            .map(|function| function.owner.as_str())
            .collect::<std::collections::BTreeSet<_>>();
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
            let behavior = body.and_then(|body| body.behavior.as_ref());
            if original_owner.contains(function.owner.as_str()) {
                if matches!(behavior, Some(FunctionBehavior::Algorithm(_))) {
                    callable.push(
                        DeclarationDiagnosticKind::BehaviorBlock,
                        &function.owner,
                        format!(
                            "existing callable {} uses Change, not Algorithm",
                            function.owner
                        ),
                    );
                }
                continue;
            }
            let (Some(body), Some(FunctionBehavior::Algorithm(algorithm))) = (body, behavior)
            else {
                callable.push(
                    DeclarationDiagnosticKind::BehaviorBlock,
                    &function.owner,
                    format!(
                        "new callable {} requires a nonempty Algorithm block",
                        function.owner
                    ),
                );
                continue;
            };
            if algorithm.trim().is_empty() {
                callable.push(
                    DeclarationDiagnosticKind::BehaviorBlock,
                    &function.owner,
                    format!(
                        "new callable {} requires a nonempty Algorithm block",
                        function.owner
                    ),
                );
                continue;
            }
            let invocations = match forge_diff::syntax::validate_algorithm(algorithm) {
                Ok(invocations) => invocations,
                Err(forge_diff::syntax::AlgorithmValidationError::Invalid(violations)) => {
                    for violation in violations {
                        callable.push(
                            DeclarationDiagnosticKind::AlgorithmSyntax,
                            &function.owner,
                            violation.to_string(),
                        );
                    }
                    continue;
                }
                Err(error) => anyhow::bail!("{path}: {}: {error}", function.owner),
            };
            let references = forge_diff::syntax::algorithm_references(algorithm)
                .map_err(|error| anyhow::anyhow!("{error:?}"))?;
            let mut missing_access = Vec::<(String, Vec<usize>)>::new();
            for reference in references.into_iter().filter(|reference| {
                reference.last
                    && !reference.first
                    && !reference.invocation
                    && !reference.construction_field
            }) {
                let (line, column) = (reference.line, reference.column);
                let target = match resolve_algorithm_reference(path, &function, body, reference) {
                    Ok(Some(AlgorithmTarget::Reference(target))) => target,
                    Ok(_) => continue,
                    Err(error) => {
                        callable.push(
                            DeclarationDiagnosticKind::AlgorithmReference,
                            &function.owner,
                            format!("Algorithm line {line}, column {column}: {error}"),
                        );
                        continue;
                    }
                };
                let Some(required) = required_access(resolver, path, &function.owner, &target.name)
                else {
                    continue;
                };
                let recorded = body.call.iter().flatten().any(|call| {
                    matches!(call.kind, CallKind::Property | CallKind::Value)
                        && covers_access(&call.name, &required)
                });
                if !recorded {
                    match missing_access
                        .iter_mut()
                        .find(|(field, _)| *field == required)
                    {
                        Some((_, lines)) if lines.contains(&line) => {}
                        Some((_, lines)) => lines.push(line),
                        None => missing_access.push((required, vec![line])),
                    }
                }
            }
            for (field, lines) in missing_access {
                let lines = lines
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(", ");
                callable.push(
                    DeclarationDiagnosticKind::AlgorithmAccess,
                    &field,
                    format!("field {field} used on Algorithm line {lines} requires a matching Accesses target"),
                );
            }
            for construction in body
                .call
                .iter()
                .flatten()
                .filter(|call| call.kind == CallKind::Construct && !call.unresolved)
            {
                let resolution = resolver.construct(path, &function.owner, &construction.name);
                if !resolver.repository_declaration(&resolution) {
                    continue;
                }
                let matching = invocations
                    .iter()
                    .filter(|invocation| {
                        invocation.name.replace("::", ".") == construction.name.replace("::", ".")
                    })
                    .collect::<Vec<_>>();
                if matching.is_empty() {
                    callable.push(
                        DeclarationDiagnosticKind::AlgorithmConstruction,
                        &construction.name,
                        format!(
                            "Algorithm must explicitly construct {name} using {name}(field = value), {name}(value) for a tuple type, or {name}() for a marker",
                            name = construction.name
                        ),
                    );
                    continue;
                }
                if resolver.field_shape(&resolution) == Some(forge_diff::syntax::FieldShape::Tuple)
                {
                    continue;
                }
                for invocation in matching
                    .into_iter()
                    .filter(|invocation| invocation.has_positional_argument)
                {
                    callable.push(
                        DeclarationDiagnosticKind::AlgorithmConstruction,
                        &construction.name,
                        format!(
                            "Algorithm line {}, column {}: construction {} requires named arguments (field = value)",
                            invocation.line, invocation.column, construction.name
                        ),
                    );
                }
            }
        }
    }
    Ok(diagnostics)
}

/// Collects one callable's diagnostics at its signature, the closest file position an
/// Algorithm-relative location has.
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

/// Select the field path an Algorithm reference must record in Accesses, or None when the
/// reference leaves repository declarations before reaching a member.
/// `Velocity::vector::x` requires `Velocity::vector` because `x` belongs to an external vector
/// type, and `Transform::translation` requires nothing because `Transform` is external.
/// A repository type with an unresolvable member still requires its first member, so the
/// declaration resolver reports the misspelled entry.
fn required_access(
    resolver: &mut crate::declaration::DeclarationResolver,
    path: &str,
    owner: &str,
    name: &str,
) -> Option<String> {
    let separator = if path.ends_with(".rs") { "::" } else { "." };
    let segments = name.split(separator).collect::<Vec<_>>();
    let root = resolver.value(path, owner, segments.first()?);
    if !resolver.repository_declaration(&root) {
        return matches!(
            root,
            crate::declaration::DeclarationResolution::Invalid { .. }
                | crate::declaration::DeclarationResolution::Ambiguous { .. }
        )
        .then(|| name.to_owned());
    }
    let mut required = None;
    for end in 2..=segments.len() {
        let prefix = segments[..end].join(separator);
        let mut resolution = resolver.property(path, owner, &prefix);
        if !resolver.repository_declaration(&resolution) {
            resolution = resolver.value(path, owner, &prefix);
        }
        if resolver.repository_declaration(&resolution) {
            required = Some(prefix);
            continue;
        }
        if required.is_none() {
            required = Some(prefix);
        }
        break;
    }
    required
}

/// An Accesses entry covers a required field when it names that field or a member within it.
fn covers_access(recorded: &str, required: &str) -> bool {
    let recorded = recorded.replace("::", ".");
    let required = required.replace("::", ".");
    recorded == required
        || recorded
            .strip_prefix(&required)
            .is_some_and(|rest| rest.starts_with('.'))
}

#[derive(Default)]
struct FunctionSection {
    behavior: Option<FunctionBehavior>,
    reference: BTreeMap<ReferenceCategory, Vec<(String, CallKind, usize)>>,
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
pub(crate) fn parse(
    path: &str,
    text: &str,
    previous: &[FunctionBody],
) -> Result<(String, Vec<FunctionBody>)> {
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
            let kind = match line.trim() {
                "Accesses" => ReferenceCategory::Accesses,
                "Constructs" => ReferenceCategory::Constructs,
                _ => ReferenceCategory::Calls,
            };
            let heading = index;
            if original_line.is_empty() {
                return Err(structure_error(
                    heading,
                    format!("{} must follow a callable declaration", line.trim()),
                ));
            }
            let behavior_section = matches!(line.trim(), "Algorithm" | "Change");
            let end = section_end(&lines, heading, behavior_section);
            let start = heading + 1;
            index = end;
            if behavior_section {
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
                block.push((
                    original_line.len(),
                    heading,
                    BodySection::Behavior(behavior),
                ));
                continue;
            }
            let names = (start..end)
                .map(|row| (lines[row].trim().to_owned(), kind.default_kind(), row))
                .collect();
            block.push((
                original_line.len(),
                heading,
                BodySection::References(kind, names),
            ));
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
    let owner_order = functions
        .iter()
        .enumerate()
        .map(|(index, function)| (function.owner.as_str(), index))
        .collect::<BTreeMap<_, _>>();
    let mut sections = BTreeMap::<usize, FunctionSection>::new();
    let by_line = functions
        .iter()
        .map(|function| (function.end_line as usize, function))
        .collect::<BTreeMap<_, _>>();
    let by_owner = previous
        .iter()
        .map(|calls| (calls.owner.as_str(), calls))
        .collect::<BTreeMap<_, _>>();
    for (line, heading, section) in block {
        let function = by_line.get(&line).ok_or_else(|| {
            structure_error(
                heading,
                format!(
                    "{} must directly follow a callable signature",
                    lines[heading].trim()
                ),
            )
        })?;
        let entry = sections.entry(line).or_default();
        match section {
            BodySection::Behavior(behavior) => {
                if entry.behavior.is_some() {
                    return Err(structure_error(
                        heading,
                        format!(
                            "{} already has a behavior block; a callable has one Algorithm (new) or one Change (existing), never both",
                            function.owner
                        ),
                    ));
                }
                entry.behavior = Some(behavior);
            }
            BodySection::References(kind, names) => {
                if entry.reference.contains_key(&kind) {
                    return Err(structure_error(
                        heading,
                        format!(
                            "{} already has a {} section",
                            function.owner,
                            lines[heading].trim()
                        ),
                    ));
                }
                entry.reference.insert(kind, names);
            }
        }
    }
    let mut output = Vec::new();
    for (line, sections) in sections {
        let function = by_line[&line];
        let references_available = !sections.reference.is_empty();
        let evidence = references_available.then_some(ReferenceEvidence {
            calls: sections.reference.contains_key(&ReferenceCategory::Calls),
            accesses: sections
                .reference
                .contains_key(&ReferenceCategory::Accesses),
            constructs: sections
                .reference
                .contains_key(&ReferenceCategory::Constructs),
        });
        let mut category = sections
            .reference
            .into_iter()
            .map(|(kind, names)| (kind, std::collections::VecDeque::from(names)))
            .collect::<BTreeMap<_, _>>();
        let mut names = Vec::new();
        for previous in by_owner
            .get(function.owner.as_str())
            .into_iter()
            .flat_map(|calls| calls.call.iter().flatten())
        {
            if let Some(entry) = category
                .get_mut(&previous.kind.category())
                .and_then(std::collections::VecDeque::pop_front)
            {
                names.push(entry);
            }
        }
        for remaining in category.into_values() {
            names.extend(remaining);
        }
        let mut remaining =
            BTreeMap::<(String, ReferenceCategory), std::collections::VecDeque<CallSite>>::new();
        for call in by_owner
            .get(function.owner.as_str())
            .into_iter()
            .flat_map(|calls| calls.call.iter().flatten())
        {
            remaining
                .entry((call.name.clone(), call.kind.category()))
                .or_default()
                .push_back(call.clone());
        }
        let call = names
            .into_iter()
            .map(|(name, kind, row)| -> Result<CallSite> {
                let unresolved = function
                    .binding
                    .keys()
                    .any(|binding| name == *binding || name.starts_with(&format!("{binding}.")));
                let captured = remaining
                    .get_mut(&(name.clone(), kind.category()))
                    .and_then(std::collections::VecDeque::pop_front);
                if !(captured.as_ref().is_some_and(|call| call.source.is_some())
                    || (!name.is_empty()
                        && name.chars().all(|character| {
                            character.is_alphanumeric()
                                || if matches!(
                                    DeclarationOverview::filetype(path),
                                    "typescript" | "tsx"
                                ) {
                                    "_:.<>#$"
                                } else {
                                    "_:.<>#"
                                }
                                .contains(character)
                        })))
                {
                    return Err(structure_error(
                        row,
                        format!(
                            "`{name}`: reference entries are bare qualified names such as App::add_systems, without receivers, calls, or arguments"
                        ),
                    ));
                }
                let mut call = captured.unwrap_or(CallSite {
                    kind,
                    name,
                    source: None,
                    unresolved,
                });
                call.unresolved |= unresolved;
                Ok(call)
            })
            .collect::<Result<Vec<_>>>()?;
        output.push(FunctionBody {
            evidence,
            owner: function.owner.clone(),
            behavior: sections.behavior,
            call: references_available.then_some(call),
        });
    }
    output.sort_by_key(|calls| {
        owner_order
            .get(calls.owner.as_str())
            .copied()
            .unwrap_or(usize::MAX)
    });
    Ok((declaration, output))
}

#[cfg(test)]
mod tests {
    use super::{CallKind, CallSite, FunctionBehavior, FunctionBody, validate_behaviors};
    use crate::declaration::{DeclarationDiagnosticKind, DeclarationResolver};
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

    fn body(owner: &str, algorithm: &str, construct: &[&str]) -> FunctionBody {
        FunctionBody {
            evidence: None,
            owner: owner.into(),
            behavior: Some(FunctionBehavior::Algorithm(algorithm.into())),
            call: Some(
                construct
                    .iter()
                    .map(|name| CallSite {
                        kind: CallKind::Construct,
                        name: (*name).into(),
                        source: None,
                        unresolved: false,
                    })
                    .collect(),
            ),
        }
    }

    #[test]
    fn review_groups_accesses_by_type_with_a_column_per_entry() {
        let mut body = body("report_score", "record(session.score)", &[]);
        body.call = Some(
            [
                ("RunPhase", CallKind::Value),
                ("RunSession::phase", CallKind::Property),
                ("RunSession::score", CallKind::Property),
                ("RunPhase::Active", CallKind::Value),
                ("Transform::translation::xy", CallKind::Property),
            ]
            .into_iter()
            .map(|(name, kind)| CallSite {
                kind,
                name: name.into(),
                source: None,
                unresolved: false,
            })
            .collect(),
        );
        let review = super::insert("src/main.rs", DECLARATIONS, &[body.clone()], true, false)
            .expect("review presentation");
        let rows = review.declaration.text.lines().collect::<Vec<_>>();
        let accesses = rows
            .iter()
            .position(|row| row.trim() == "Accesses")
            .expect("Accesses heading");
        assert_eq!(
            rows[accesses + 1..accesses + 4]
                .iter()
                .map(|row| row.trim())
                .collect::<Vec<_>>(),
            [
                "RunPhase { Active }",
                "RunSession { phase, score }",
                "Transform { translation::xy }",
            ]
        );
        assert_eq!(
            review.section.get(&accesses),
            Some(&(accesses + 4, "Accesses"))
        );
        let session = &review.call_row[&(accesses + 2)];
        assert_eq!(
            session
                .iter()
                .map(|target| (
                    target.name.as_str(),
                    &rows[accesses + 2][target.start..target.end]
                ))
                .collect::<Vec<_>>(),
            [
                ("RunSession::phase", "phase"),
                ("RunSession::score", "score")
            ]
        );
        let phase = &review.call_row[&(accesses + 1)];
        assert_eq!(
            phase
                .iter()
                .map(|target| (
                    target.name.as_str(),
                    &rows[accesses + 1][target.start..target.end]
                ))
                .collect::<Vec<_>>(),
            [("RunPhase", "RunPhase"), ("RunPhase::Active", "Active")]
        );
        let editable = super::insert("src/main.rs", DECLARATIONS, &[body], false, false)
            .expect("editable presentation");
        assert!(editable.declaration.text.contains(
            "  RunSession::phase
"
        ));
    }

    #[test]
    fn only_reference_section_calls_are_declared_algorithm_calls() {
        let function =
            forge_diff::syntax::DeclarationCalls::extract("src/main.rs", DECLARATIONS, true)
                .expect("declarations")
                .into_iter()
                .find(|function| function.owner == "report_score")
                .expect("callable");
        let mut report = body(
            "report_score",
            "publish(session.score)
total <- delta(time)",
            &[],
        );
        report.call.as_mut().expect("references").push(CallSite {
            kind: CallKind::Call,
            name: "publish".into(),
            source: None,
            unresolved: false,
        });
        assert!(super::declared_algorithm_call(
            "src/main.rs",
            &function,
            &report,
            1,
            "    publish(session.score)",
            4,
        ));
        assert!(!super::declared_algorithm_call(
            "src/main.rs",
            &function,
            &report,
            2,
            "    total <- delta(time)",
            13,
        ));
    }

    #[test]
    fn one_pass_reports_every_repository_violation_and_exempts_external_types() {
        let workspace = tempfile::tempdir().expect("workspace");
        std::fs::write(
            workspace.path().join("Cargo.toml"),
            "[package]\nname = \"arena\"\nversion = \"0.1.0\"\nedition = \"2024\"\n",
        )
        .expect("manifest");
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
                    &["Velocity", "Player", "Text2d"],
                ),
                body(
                    "move_player",
                    "transform.translation ← transform.translation + player.speed\ntotal <- session.score\nif total mod 2 = 0\n  record(total)",
                    &[],
                ),
                body(
                    "report_score",
                    "process candidate now\nlimit ← ← 2",
                    &[],
                ),
            ],
        );
        let mut resolver =
            DeclarationResolver::planned(workspace.path(), &design, false).expect("resolver");
        let mut reported = validate_behaviors(&design, &mut resolver)
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
                (
                    DeclarationDiagnosticKind::AlgorithmAccess,
                    "move_player: Player::speed".to_owned()
                ),
                (
                    DeclarationDiagnosticKind::AlgorithmAccess,
                    "move_player: RunSession::score".to_owned()
                ),
            ]
        );
    }
}
