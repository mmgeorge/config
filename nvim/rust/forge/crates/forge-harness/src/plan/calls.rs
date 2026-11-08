use std::collections::{BTreeMap, VecDeque};
use std::sync::{Arc, Mutex, OnceLock};

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

/// Maps display rows to saved declarations and call identities.
pub(crate) struct CallPresentation {
    visibility: [OnceLock<forge_diff::syntax::DeclarationVisibility>; 2],
    folding: OnceLock<Vec<forge_diff::syntax::DeclarationFold>>,
    callable: OnceLock<Vec<forge_diff::syntax::DeclarationCallable>>,
    /// Signature-only text used by declaration analysis before synthetic bodies are rendered.
    pub plain: String,
    pub declaration: DeclarationPresentation,
    pub call_row: BTreeMap<usize, (String, String, CallKind)>,
    pub behavior_row: BTreeMap<usize, (String, u32, &'static str)>,
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

/// Extract source call evidence without capturing unrelated files.
#[cfg(test)]
pub(crate) fn extract(path: &str, source: &str) -> Result<Vec<FunctionBody>> {
    Ok(from_extracted(
        DeclarationCalls::extract(path, source, false)
            .map_err(|error| anyhow::anyhow!("{error:?}"))?,
    ))
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
            .map(|(owner, name, _)| owner.capacity() + name.capacity() + 128)
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
                    source.push(DeclarationPosition {
                        line: function.line,
                        column: function.column,
                    });
                    for (name, semantic_kind) in targets {
                        call_row.insert(
                            source.len(),
                            (function.owner.clone(), name.to_owned(), semantic_kind),
                        );
                        output.push_str(&format!("{indent}{body_indent}  {name}\n"));
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
    References(ReferenceCategory, Vec<(String, CallKind)>),
}

/// Require algorithms for new callables at submission while keeping draft edits incomplete.
pub(super) fn validate_behaviors(design: &super::DeclarationDesign) -> Result<()> {
    for (path, text) in &design.proposed {
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
            let body = design
                .proposed_calls
                .get(path)
                .into_iter()
                .flatten()
                .find(|body| body.owner == function.owner);
            let behavior = body.and_then(|body| body.behavior.as_ref());
            if original_owner.contains(function.owner.as_str()) {
                ensure!(
                    !matches!(behavior, Some(FunctionBehavior::Algorithm(_))),
                    "{path}: existing callable {} uses Change, not Algorithm",
                    function.owner
                );
            } else {
                ensure!(
                    matches!(behavior, Some(FunctionBehavior::Algorithm(text)) if !text.trim().is_empty()),
                    "{path}: new callable {} requires a nonempty Algorithm block",
                    function.owner
                );
                if let (Some(body), Some(FunctionBehavior::Algorithm(text))) = (body, behavior) {
                    let invocations =
                        forge_diff::syntax::validate_algorithm(text).map_err(|error| {
                            anyhow::anyhow!("{path}: {}: {error:?}", function.owner)
                        })?;
                    for reference in forge_diff::syntax::algorithm_references(text)
                        .map_err(|error| anyhow::anyhow!("{error:?}"))?
                        .into_iter()
                        .filter(|reference| {
                            reference.last
                                && !reference.first
                                && !reference.invocation
                                && !reference.construction_field
                        })
                    {
                        let line = reference.line;
                        let column = reference.column;
                        let Some(AlgorithmTarget::Reference(target)) =
                            resolve_algorithm_reference(path, &function, body, reference)?
                        else {
                            continue;
                        };
                        ensure!(
                            body.call
                                .iter()
                                .flatten()
                                .any(|call| call.name.replace("::", ".")
                                    == target.name.replace("::", ".")
                                    && matches!(call.kind, CallKind::Property | CallKind::Value)),
                            "{path}: {}: Algorithm line {line}, column {column}: field {} requires a matching Accesses target",
                            function.owner,
                            target.name
                        );
                    }
                    for construction in body
                        .call
                        .iter()
                        .flatten()
                        .filter(|call| call.kind == CallKind::Construct)
                    {
                        let matching = invocations
                            .iter()
                            .filter(|invocation| invocation.name == construction.name)
                            .collect::<Vec<_>>();
                        ensure!(
                            !matching.is_empty(),
                            "{path}: {}: Algorithm must explicitly construct {} using {}(field = value) or {}() for a marker",
                            function.owner,
                            construction.name,
                            construction.name,
                            construction.name
                        );
                        for invocation in matching {
                            ensure!(
                                !invocation.has_positional_argument,
                                "{path}: {}: Algorithm line {}, column {}: construction {} requires named arguments (field = value)",
                                function.owner,
                                invocation.line,
                                invocation.column,
                                construction.name
                            );
                        }
                    }
                }
            }
        }
    }
    Ok(())
}

#[derive(Default)]
struct FunctionSection {
    behavior: Option<FunctionBehavior>,
    reference: BTreeMap<ReferenceCategory, Vec<(String, CallKind)>>,
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
        let line = lines[index];
        if matches!(
            line.trim(),
            "Algorithm" | "Change" | "Calls" | "Constructs" | "Accesses"
        ) {
            let start = index;
            let indent = line.len() - line.trim_start().len();
            index += 1;
            while index < lines.len()
                && (lines[index].trim().is_empty()
                    || lines[index].len() - lines[index].trim_start().len() > indent)
            {
                index += 1;
            }
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
        if matches!(
            line.trim(),
            "Algorithm" | "Change" | "Calls" | "Constructs" | "Accesses"
        ) && !protected.contains(&index)
        {
            let kind = match line.trim() {
                "Accesses" => ReferenceCategory::Accesses,
                "Constructs" => ReferenceCategory::Constructs,
                _ => ReferenceCategory::Calls,
            };
            let indent = line.len() - line.trim_start().len();
            let preceding = original_line
                .last()
                .copied()
                .context("Function uses must follow a callable declaration")?;
            index += 1;
            if matches!(line.trim(), "Algorithm" | "Change") {
                let start = index;
                while index < lines.len() {
                    let value = lines[index];
                    if !value.trim().is_empty() && value.len() - value.trim_start().len() <= indent
                    {
                        break;
                    }
                    index += 1;
                }
                let mut end = index;
                while end > start && lines[end - 1].trim().is_empty() {
                    end -= 1;
                }
                index = end;
                let summary_indent = lines[start..end]
                    .iter()
                    .filter(|value| !value.trim().is_empty())
                    .map(|value| value.len() - value.trim_start().len())
                    .min()
                    .with_context(|| {
                        format!("{} requires indented, nonempty content", line.trim())
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
                    preceding,
                    BodySection::Behavior(behavior),
                ));
                continue;
            }
            let mut names = Vec::new();
            while index < lines.len() {
                let value = lines[index];
                if value.trim().is_empty() || value.len() - value.trim_start().len() <= indent {
                    break;
                }
                let name = value.trim();
                names.push((name.to_owned(), kind.default_kind()));
                index += 1;
            }
            block.push((
                original_line.len(),
                preceding,
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
    for (line, _, section) in block {
        let function = by_line
            .get(&line)
            .context("Function body sections must immediately follow a callable signature")?;
        let entry = sections.entry(line).or_default();
        match section {
            BodySection::Behavior(behavior) => {
                ensure!(
                    entry.behavior.is_none(),
                    "only one Algorithm or Change block is allowed for {}",
                    function.owner
                );
                entry.behavior = Some(behavior);
            }
            BodySection::References(kind, names) => {
                ensure!(
                    !entry.reference.contains_key(&kind),
                    "duplicate {:?} block for {}",
                    kind,
                    function.owner
                );
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
            .map(|(name, kind)| -> Result<CallSite> {
                let unresolved = function
                    .binding
                    .keys()
                    .any(|binding| name == *binding || name.starts_with(&format!("{binding}.")));
                let captured = remaining
                    .get_mut(&(name.clone(), kind.category()))
                    .and_then(std::collections::VecDeque::pop_front);
                ensure!(
                    captured.as_ref().is_some_and(|call| call.source.is_some())
                        || (!name.is_empty()
                            && name.chars().all(|character| character.is_alphanumeric()
                                || if matches!(
                                    DeclarationOverview::filetype(path),
                                    "typescript" | "tsx"
                                ) {
                                    "_:.<>#$"
                                } else {
                                    "_:.<>#"
                                }
                                .contains(character))),
                    "Function uses contain only a qualified target name, without arguments: {name}"
                );
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
    use super::*;

    #[test]
    fn combined_round_trip_keeps_call_order_and_evidence() {
        for (path, source) in [
            ("main.rs", "fn run() { z(); a(); z(); }"),
            ("main.ts", "function run() { z(); a(); z(); }"),
            ("main.tsx", "function run() { z(); a(); z(); }"),
            ("main.lua", "function run()\n z()\n a()\n z()\nend\n"),
            ("arrow.ts", "const run = () => { z(); a(); z(); };"),
            (
                "assigned.lua",
                "local M = {}\nM.run = function()\n z()\n a()\n z()\nend\nreturn M\n",
            ),
        ] {
            let declaration = DeclarationOverview::extract(path, source).unwrap();
            let calls = extract(path, source).unwrap();
            let combined = combined(path, &declaration, &calls).unwrap();
            let (parsed, parsed_calls) = parse(path, &combined, &calls).unwrap();
            assert_eq!(parsed, declaration, "{path}: {combined}");
            assert_eq!(parsed_calls, calls, "{path}: {combined}");
            let display = present(path, &declaration, &calls).unwrap();
            assert_eq!(
                display
                    .call_row
                    .values()
                    .map(|(_, name, _)| name.as_str())
                    .collect::<Vec<_>>(),
                ["z", "a"],
                "{path}"
            );
            let sorted = insert(path, &display.plain, &calls, true, true).unwrap();
            assert_eq!(
                sorted
                    .call_row
                    .values()
                    .map(|(_, name, _)| name.as_str())
                    .collect::<Vec<_>>(),
                ["a", "z"]
            );
        }
    }
}
