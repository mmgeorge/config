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
    fn algorithm_typed_fields_require_matching_accesses() {
        for (expression, access, valid) in [
            (
                "Record Projectile.previous_position",
                Some("Projectile::previous_position"),
                true,
            ),
            ("Record Projectile.previous_position", None, false),
            (
                "Record Projectile.missing",
                Some("Projectile::previous_position"),
                false,
            ),
            ("Advance the position", None, true),
        ] {
            let mut design = super::super::DeclarationDesign::default();
            design.proposed.insert(
                "main.rs".into(),
                "pub struct Projectile { pub previous_position: u32 }\npub fn run();\n".into(),
            );
            design.proposed_calls.insert(
                "main.rs".into(),
                vec![FunctionBody {
                    owner: "run".into(),
                    evidence: None,
                    behavior: Some(FunctionBehavior::Algorithm(expression.into())),
                    call: Some(
                        access
                            .into_iter()
                            .map(|name| CallSite {
                                name: name.into(),
                                kind: CallKind::Property,
                                source: None,
                                unresolved: false,
                            })
                            .collect(),
                    ),
                }],
            );
            let result = validate_behaviors(&design);
            assert_eq!(result.is_ok(), valid, "{expression}: {result:?}");
            if !valid {
                assert!(
                    result
                        .unwrap_err()
                        .to_string()
                        .contains("matching Accesses")
                );
            }
        }
    }

    #[test]
    fn algorithm_navigation_prefers_signature_then_reference_evidence() {
        let declarations = "pub fn run(abilities: &mut PlayerAbilities, wrapped: ResMut<PlayerAbilities>, keys: Keys);";
        let mut body = FunctionBody {
            owner: "run".into(),
            evidence: None,
            behavior: Some(FunctionBehavior::Algorithm(
                "abilities.start_dash(keys)".into(),
            )),
            call: Some(vec![
                CallSite {
                    name: "Other::start_dash".into(),
                    kind: CallKind::Call,
                    source: None,
                    unresolved: false,
                },
                CallSite {
                    name: "PlayerAbilities::start_dash".into(),
                    kind: CallKind::Call,
                    source: None,
                    unresolved: false,
                },
                CallSite {
                    name: "PlayerAbilities::remaining".into(),
                    kind: CallKind::Property,
                    source: None,
                    unresolved: false,
                },
                CallSite {
                    name: "movement_direction".into(),
                    kind: CallKind::Call,
                    source: None,
                    unresolved: false,
                },
            ]),
        };
        let select = |body: &FunctionBody, source: &str, token: &str| {
            let mut body = body.clone();
            body.behavior = Some(FunctionBehavior::Algorithm(source.into()));
            algorithm_reference(
                "main.rs",
                declarations,
                &body,
                1,
                &format!("    {source}"),
                4 + source.find(token).unwrap(),
            )
        };
        for token in ["abilities", "keys"] {
            let AlgorithmTarget::Binding { name, position } =
                select(&body, "abilities.start_dash(keys)", token)
                    .unwrap()
                    .unwrap()
            else {
                panic!("expected parameter");
            };
            assert_eq!(name, token);
            assert_eq!(
                &declarations[position.column as usize..position.column as usize + name.len()],
                name
            );
        }
        for (source, token, expected, kind) in [
            (
                "Record Projectile.previous_position",
                "Projectile",
                "Projectile",
                CallKind::Value,
            ),
            (
                "abilities.start_dash(keys)",
                "start_dash",
                "PlayerAbilities::start_dash",
                CallKind::Call,
            ),
            (
                "wrapped.start_dash(keys)",
                "start_dash",
                "PlayerAbilities::start_dash",
                CallKind::Call,
            ),
            (
                "abilities.remaining",
                "remaining",
                "PlayerAbilities::remaining",
                CallKind::Property,
            ),
            (
                "movement_direction(keys)",
                "movement_direction",
                "movement_direction",
                CallKind::Call,
            ),
            ("start_dash(keys)", "start_dash", "", CallKind::Call),
        ] {
            let result = select(&body, source, token);
            if expected.is_empty() {
                assert!(result.err().unwrap().to_string().contains("ambiguous"));
                continue;
            }
            let AlgorithmTarget::Reference(reference) = result.unwrap().unwrap() else {
                panic!("expected declaration");
            };
            assert_eq!((reference.name.as_str(), reference.kind), (expected, kind));
        }
        body.call
            .as_mut()
            .unwrap()
            .retain(|reference| reference.name != "PlayerAbilities::start_dash");
        let AlgorithmTarget::Reference(reference) =
            select(&body, "abilities.start_dash(keys)", "start_dash")
                .unwrap()
                .unwrap()
        else {
            panic!("expected typed declaration");
        };
        assert_eq!(
            reference.name, "PlayerAbilities::start_dash",
            "unrelated Calls must not override a known receiver type"
        );
        let AlgorithmTarget::Reference(reference) =
            select(&body, "unknown.start_dash(keys)", "start_dash")
                .unwrap()
                .unwrap()
        else {
            panic!("expected reference evidence");
        };
        assert_eq!(reference.name, "Other::start_dash");
        assert!(
            select(&body, "Decrease cooldown", "Decrease")
                .unwrap()
                .is_none()
        );
        assert!(
            select(&body, "return \"abilities\"", "abilities")
                .unwrap()
                .is_none()
        );
    }

    #[test]
    fn algorithm_receiver_navigation_uses_typescript_and_lua_signature_types() {
        for (path, declarations) in [
            ("main.ts", "export function run(client: Client): void;"),
            (
                "main.lua",
                "---@param client Client\nfunction run(client)\nend",
            ),
        ] {
            let source = "client.send()";
            let body = FunctionBody {
                owner: "run".into(),
                evidence: None,
                behavior: Some(FunctionBehavior::Algorithm(source.into())),
                call: Some(vec![CallSite {
                    name: "Other.send".into(),
                    kind: CallKind::Call,
                    source: None,
                    unresolved: false,
                }]),
            };
            let AlgorithmTarget::Reference(reference) =
                algorithm_reference(path, declarations, &body, 1, source, 7)
                    .unwrap()
                    .unwrap()
            else {
                panic!("expected typed reference for {path}");
            };
            assert_eq!(reference.name, "Client.send", "{path}");
        }
    }

    #[test]
    fn algorithm_sections_preserve_nested_pseudocode_and_reference_boundaries() {
        for (path, source) in [
            ("main.rs", "pub fn run() {}\n"),
            ("main.ts", "export function run() {}\n"),
            ("main.tsx", "export function run() {}\n"),
            ("main.lua", "function run()\nend\n"),
        ] {
            let declaration = DeclarationOverview::extract(path, source).unwrap();
            let algorithm = "for each item in items:\n  if item = none: continue\n  result ← inspect(item)\n  if result contains \" or /* or [[:\n    return result";
            let authored = format!(
                "{declaration}Algorithm\n  {}\nCalls\n  inspect\n",
                algorithm.replace('\n', "\n  ")
            );
            let (parsed, bodies) = parse(path, &authored, &[]).unwrap();
            assert_eq!(
                bodies[0].behavior,
                Some(FunctionBehavior::Algorithm(algorithm.into()))
            );
            assert_eq!(bodies[0].call.as_ref().unwrap()[0].name, "inspect");
            assert_eq!(combined(path, &parsed, &bodies).unwrap(), authored);
            let restored: Vec<FunctionBody> =
                serde_json::from_slice(&serde_json::to_vec(&bodies).unwrap()).unwrap();
            let presentation = present(path, &parsed, &restored).unwrap();
            assert!(presentation.declaration.text.contains("Algorithm\n"));
            assert!(
                presentation
                    .behavior_row
                    .values()
                    .all(|(_, _, category)| *category == "Algorithm")
            );
            assert!(
                presentation
                    .body
                    .values()
                    .all(|body| !body.collapsed_suffix.contains("[changed]"))
            );
        }
    }

    #[test]
    fn behavior_sections_reject_empty_duplicate_and_mixed_categories() {
        for invalid in [
            "Algorithm\n  return none\n",
            "pub struct Data;\nAlgorithm\n  return none\n",
            "pub fn run();\nAlgorithm\n",
            "pub fn run();\nAlgorithm\n  \nCalls\n",
            "pub fn run();\nAlgorithm\n  return none\nAlgorithm\n  return none\n",
            "pub fn run();\nAlgorithm\n  return none\nChange\n  Update the result.\n",
            "pub fn run();\nChange\n  Update the result.\nAlgorithm\n  return none\n",
        ] {
            assert!(parse("main.rs", invalid, &[]).is_err(), "{invalid}");
        }
        let documented = "/*\nAlgorithm\n  return none\n*/\npub fn run();\n";
        assert_eq!(
            parse("main.rs", documented, &[]).unwrap(),
            (documented.into(), vec![])
        );
    }

    #[test]
    fn submission_requires_algorithms_only_for_new_callable_identities() {
        let mut design = crate::plan::DeclarationDesign::default();
        design.baseline.insert(
            "before.rs".into(),
            crate::plan::DeclarationFile {
                text: "pub fn existing();\n".into(),
                source_digest: String::new(),
            },
        );
        design.moved.insert("before.rs".into(), "after.rs".into());
        let proposed = "pub fn existing();\npub fn added();\n";
        design.proposed.insert("after.rs".into(), proposed.into());
        assert!(
            validate_behaviors(&design)
                .unwrap_err()
                .to_string()
                .contains("added requires a nonempty Algorithm")
        );
        for category in ["Change", "Algorithm"] {
            let (text, bodies) = parse(
                "after.rs",
                &format!("{proposed}{category}\n  return none\n"),
                &[],
            )
            .unwrap();
            design.proposed.insert("after.rs".into(), text);
            design.proposed_calls.insert("after.rs".into(), bodies);
            assert_eq!(validate_behaviors(&design).is_ok(), category == "Algorithm");
        }
        let (text, bodies) = parse("after.rs", "pub fn existing();\nAlgorithm\n  return none\npub fn added();\nAlgorithm\n  return none\n", &[]).unwrap();
        design.proposed.insert("after.rs".into(), text);
        design.proposed_calls.insert("after.rs".into(), bodies);
        assert!(
            validate_behaviors(&design)
                .unwrap_err()
                .to_string()
                .contains("existing callable existing uses Change")
        );
    }

    #[test]
    fn algorithm_drafts_remain_editable_but_submission_reports_structure_errors() {
        let (text, bodies) = parse(
            "main.rs",
            "pub fn run();\nAlgorithm\n  if ready\n  return value\n",
            &[],
        )
        .unwrap();
        let mut design = crate::plan::DeclarationDesign::default();
        design.proposed.insert("main.rs".into(), text);
        design.proposed_calls.insert("main.rs".into(), bodies);
        let error = validate_behaviors(&design).unwrap_err().to_string();
        assert!(error.contains("main.rs: run") && error.contains("Algorithm line 1"));
        design.proposed_calls.get_mut("main.rs").unwrap()[0].behavior = Some(
            FunctionBehavior::Algorithm("if ready\n  return value\nreturn none".into()),
        );
        validate_behaviors(&design).unwrap();
    }

    #[test]
    fn algorithm_construction_requires_explicit_matching_names_and_field_values() {
        for (algorithm, accepted) in [
            (
                "queue a Projectile with damage 1\nmarker ← CombatEntity()",
                false,
            ),
            (
                "description ← \"Projectile(damage = 1)\"\nmarker ← CombatEntity()",
                false,
            ),
            ("# Projectile(damage = 1)\nmarker ← CombatEntity()", false),
            (
                "projectile ← Other::Projectile(damage = 1)\nmarker ← CombatEntity()",
                false,
            ),
            ("projectile ← Projectile(1)\nmarker ← CombatEntity()", false),
            ("projectile ← Projectile(damage = 1)", false),
            (
                "projectile ← Projectile(damage = 1)\nother ← Projectile(2)\nmarker ← CombatEntity()",
                false,
            ),
            (
                "projectile ← Projectile(damage = 1)\nmarker ← CombatEntity()",
                true,
            ),
            (
                "queue spawn(Projectile(damage = inspect(value)), CombatEntity())",
                true,
            ),
        ] {
            let mut design = crate::plan::DeclarationDesign::default();
            design
                .proposed
                .insert("main.rs".into(), "pub fn launch();\n".into());
            design.proposed_calls.insert(
                "main.rs".into(),
                vec![FunctionBody {
                    owner: "launch".into(),
                    evidence: None,
                    behavior: Some(FunctionBehavior::Algorithm(algorithm.into())),
                    call: Some(
                        ["Projectile", "CombatEntity"]
                            .into_iter()
                            .map(|name| CallSite {
                                kind: CallKind::Construct,
                                name: name.into(),
                                source: None,
                                unresolved: false,
                            })
                            .collect(),
                    ),
                }],
            );
            let result = validate_behaviors(&design);
            assert_eq!(result.is_ok(), accepted, "{algorithm}: {result:?}");
        }
    }

    #[test]
    fn inline_receiver_types_round_trip_without_becoming_reference_names() {
        let source = r#"export class BufferStore {
  private current: {
    writer: Writer;
    metric: Metric | null;
  };
  private pending: { writer: Writer; metric: Metric }[];
  read(options: { writer: Writer; metric: Metric | null }): number {
    options.writer.read();
    this.current.writer.read();
    this.pending.push(this.current);
    return this.current.writer.count;
  }
  create(): { writer: Writer; metric: Metric | null } {
    return this.current;
  }
  run(): void {
    this.create().writer.read();
  }
}"#;
        let path = "buffer.ts";
        let (declaration, extracted) =
            DeclarationOverview::extract_with_calls(path, source).unwrap();
        let declaration = DeclarationOverview::format_with_width(path, &declaration, 80).unwrap();
        let captured = from_extracted(extracted);
        let combined = combined(path, &declaration, &captured).unwrap();
        let (_, parsed) = parse(path, &combined, &captured).unwrap();
        assert_eq!(parsed, captured);
        for reference in captured
            .iter()
            .flat_map(|function| function.call.iter().flatten())
        {
            assert!(
                !reference.name.contains(['{', '}', ';', '\n']),
                "{}",
                reference.name
            );
        }
    }

    #[test]
    fn captured_receiver_expressions_round_trip_without_an_lsp() {
        let path = "main.ts";
        let source = "export class RenderState { getStage(): unknown { return undefined; } run(options: unknown) { options.$view.scale; this.getStage()?.labelManager.symbolFader; options.$view.scale; } }";
        let (declaration, extracted) =
            DeclarationOverview::extract_with_calls(path, source).unwrap();
        let declaration = DeclarationOverview::format_with_width(path, &declaration, 80).unwrap();
        let captured = from_extracted(extracted);
        let combined = combined(path, &declaration, &captured).unwrap();
        let (_, parsed) = parse(path, &combined, &captured).unwrap();
        assert_eq!(parsed, captured);
        let expressions = parsed
            .iter()
            .flat_map(|function| function.call.iter().flatten())
            .filter_map(|call| {
                call.source
                    .as_ref()
                    .map(|source| source.expression.as_str())
            })
            .collect::<Vec<_>>();
        assert_eq!(
            expressions
                .iter()
                .filter(|expression| **expression == "options.$view.scale")
                .count(),
            2
        );
        assert!(expressions.contains(&"this.getStage()?.labelManager.symbolFader"));
        for call in parsed
            .iter()
            .flat_map(|function| function.call.iter().flatten())
        {
            let position = call.source.as_ref().unwrap();
            let token = &source.lines().nth(position.line as usize - 1).unwrap()
                [position.column as usize..];
            let member = position
                .expression
                .rsplit('.')
                .next()
                .unwrap()
                .trim_start_matches('?');
            assert!(
                token.starts_with(member),
                "{}: {token}",
                position.expression
            );
        }
        assert!(
            parse(path, &combined, &[]).is_err(),
            "new authored expressions bypassed qualified-target validation"
        );
        let authored = "export function run(): void;\n  Accesses\n    Options.$view\n";
        assert!(parse(path, authored, &[]).is_ok());
        assert!(parse(path, &authored.replace("Options.$view", "fn(value)"), &[]).is_err());
    }

    #[test]
    fn presentation_cache_tracks_declarations_and_reference_metadata() {
        let text = "pub fn cache_probe();\n";
        let body = vec![FunctionBody {
            evidence: None,
            owner: "cache_probe".into(),
            behavior: Some(crate::plan::FunctionBehavior::Change(
                "Keep the original behavior.".into(),
            )),
            call: None,
        }];
        let original = present("cache_probe.rs", text, &body).unwrap();
        assert!(Arc::ptr_eq(
            &original,
            &present("cache_probe.rs", text, &body).unwrap()
        ));
        let mut changed = body.clone();
        changed[0].behavior = Some(crate::plan::FunctionBehavior::Change(
            "Use the changed behavior.".into(),
        ));
        let revised = present("cache_probe.rs", text, &changed).unwrap();
        assert!(!Arc::ptr_eq(&original, &revised));
        assert!(
            revised
                .declaration
                .text
                .contains("Use the changed behavior.")
        );
        let renamed = present("cache_probe.rs", "pub fn renamed_probe();\n", &[]).unwrap();
        assert!(!Arc::ptr_eq(&original, &renamed));
        assert!(renamed.declaration.text.contains("renamed_probe"));
    }

    #[test]
    fn constructs_round_trip_separately_from_calls_and_accesses() {
        let source = "struct Weapon { cooldown: f32 } pub fn setup() { register(); let first = Weapon { cooldown: 0.0 }; let second = Weapon { cooldown: 1.0 }; }";
        let declaration = DeclarationOverview::extract("lib.rs", source).unwrap();
        let captured = extract("lib.rs", source).unwrap();
        let editable = combined("lib.rs", &declaration, &captured).unwrap();
        assert!(
            editable.contains("Constructs\n  Weapon\n  Weapon\n"),
            "{editable}"
        );
        assert!(
            editable.contains("Accesses\n  Weapon::cooldown\n  Weapon::cooldown\n"),
            "{editable}"
        );
        assert_eq!(parse("lib.rs", &editable, &captured).unwrap().1, captured);
        let display = present("lib.rs", &declaration, &captured).unwrap();
        assert_eq!(
            display
                .call_row
                .values()
                .filter(|(_, name, kind)| name == "Weapon" && *kind == CallKind::Construct)
                .count(),
            1
        );
        let (_, empty) = parse("lib.rs", "pub fn setup();\nConstructs\n", &[]).unwrap();
        assert_eq!(
            empty[0].evidence,
            Some(ReferenceEvidence {
                calls: false,
                constructs: true,
                accesses: false
            })
        );
        assert!(parse("lib.rs", "pub fn setup();\nConstructs\nConstructs\n", &[]).is_err());
        assert!(parse("lib.rs", "pub fn setup();\nConstructs\n  Weapon()\n", &[]).is_err());
    }

    #[test]
    fn reference_evidence_distinguishes_omitted_and_empty_categories() {
        for (text, calls, accesses) in [
            ("Calls\n", true, false),
            ("Accesses\n", false, true),
            ("Calls\nAccesses\n", true, true),
        ] {
            let (declarations, body) =
                parse("lib.rs", &format!("pub fn run();\n{text}"), &[]).unwrap();
            assert_eq!(
                body[0].evidence,
                Some(ReferenceEvidence {
                    calls,
                    accesses,
                    constructs: false
                })
            );
            let saved = serde_json::to_string(&body).unwrap();
            let loaded: Vec<FunctionBody> = serde_json::from_str(&saved).unwrap();
            assert_eq!(
                parse(
                    "lib.rs",
                    &combined("lib.rs", &declarations, &loaded).unwrap(),
                    &[]
                )
                .unwrap()
                .1,
                body
            );
        }
    }

    #[test]
    fn change_sections_round_trip_without_inventing_references() {
        for (path, source) in [
            ("main.rs", "pub fn run() { send(); }\n"),
            ("main.ts", "export function run() { send(); }\n"),
            ("main.tsx", "export function run() { send(); }\n"),
            ("main.lua", "function run()\n send()\nend\n"),
        ] {
            let declaration = DeclarationOverview::extract(path, source).unwrap();
            let authored = format!(
                "{}Change\n  Stop retrying authentication failures.\n\n  Record the final attempt.\n",
                declaration
            );
            let (parsed, body) = parse(path, &authored, &[]).unwrap();
            assert_eq!(parsed, declaration);
            assert_eq!(
                body[0]
                    .behavior
                    .as_ref()
                    .map(crate::plan::FunctionBehavior::text),
                Some("Stop retrying authentication failures.\n\nRecord the final attempt.")
            );
            assert_eq!(body[0].call, None);
            assert_eq!(combined(path, &parsed, &body).unwrap(), authored);
            let saved = serde_json::to_value(&body).unwrap();
            assert!(saved[0].get("call").is_none());
            assert_eq!(
                serde_json::from_value::<Vec<FunctionBody>>(saved).unwrap(),
                body
            );
            let review = present(path, &parsed, &body).unwrap();
            assert!(review.call_row.is_empty());
            assert_eq!(review.behavior_row.len(), 4);
            assert!(
                review
                    .body
                    .values()
                    .all(|body| !body.collapsed_suffix.contains("[changed]"))
            );
            assert!(!review.declaration.text.contains("Calls"));
            let mut captured = extract(path, source).unwrap();
            captured[0].behavior = body[0].behavior.clone();
            assert_eq!(
                parse(
                    path,
                    &combined(path, &parsed, &captured).unwrap(),
                    &captured
                )
                .unwrap()
                .1,
                captured
            );
        }
    }

    #[test]
    fn change_prose_does_not_affect_later_reference_sections() {
        for (path, source) in [
            ("run.rs", "pub fn run();\npub fn next();\n"),
            (
                "run.ts",
                "export function run();\nexport function next();\n",
            ),
            ("run.lua", "function run()\nfunction next()\n"),
        ] {
            let declaration = DeclarationOverview::parse(path, source).unwrap();
            let lines = declaration.lines().collect::<Vec<_>>();
            for prose in [
                "Handle a \" token in malformed input.",
                "Handle a [[ token in malformed input.",
                "Reject /* delimiters in invalid input.",
            ] {
                let text = format!(
                    "{}\nChange\n  {prose}\nCalls\n  send\n{}\nChange\n  Report a \" token and */ delimiter in the result.\nCalls\n  record\n",
                    lines[0], lines[1]
                );
                let (_, body) = parse(path, &text, &[])
                    .unwrap_or_else(|error| panic!("{path}: {prose}: {error}"));
                assert_eq!(body.len(), 2);
                assert_eq!(body[1].call.as_ref().unwrap()[0].name, "record");
            }
        }
    }

    #[test]
    fn change_sections_require_one_nonempty_summary_owned_by_a_function() {
        for invalid in [
            "Change\n  Explain the change.\n",
            "pub struct Data;\nChange\n  Explain the change.\n",
            "pub fn run();\nChange\n",
            "pub fn run();\nChange\n  \nCalls\n",
            "pub fn run();\nChange\n  First.\nChange\n  Second.\n",
        ] {
            assert!(parse("main.rs", invalid, &[]).is_err(), "{invalid}");
        }
        let text = "/*\nChange\n  Documentation example.\n*/\npub fn run();\n";
        assert_eq!(parse("main.rs", text, &[]).unwrap(), (text.into(), vec![]));
        let (declaration, body) = parse(
            "main.rs",
            "pub fn run();\nChange\n  Preserve ordering.\nCalls\nAccesses\n",
            &[],
        )
        .unwrap();
        assert_eq!(body[0].call, Some(vec![]));
        let editable = combined("main.rs", &declaration, &body).unwrap();
        assert!(editable.contains("Calls\n"));
        assert_eq!(
            parse("main.rs", "pub fn run();\nCalls\n", &body).unwrap().1[0].behavior,
            None
        );
    }

    #[test]
    fn display_sorting_is_opt_in_for_both_categories_and_preserves_occurrences() {
        let path = "main.rs";
        let source = "fn run(client: Client) { z(); client.z; a(); client.a; z(); client.z; }";
        let declaration = DeclarationOverview::extract(path, source).unwrap();
        let calls = extract(path, source).unwrap();
        let original = calls.clone();
        let default = present(path, &declaration, &calls).unwrap();
        assert_eq!(
            default
                .call_row
                .values()
                .map(|(_, name, _)| name.as_str())
                .collect::<Vec<_>>(),
            ["z", "a", "Client::z", "Client::a"]
        );
        let sorted = insert(path, &default.plain, &calls, true, true).unwrap();
        assert_eq!(
            sorted
                .call_row
                .values()
                .map(|(_, name, _)| name.as_str())
                .collect::<Vec<_>>(),
            ["a", "z", "Client::a", "Client::z"]
        );
        assert_eq!(calls, original);
        assert_eq!(calls[0].call.as_ref().unwrap().len(), 6);
        assert_eq!(
            parse(path, &combined(path, &declaration, &calls).unwrap(), &calls)
                .unwrap()
                .1,
            original
        );
    }

    #[test]
    fn review_bodies_wrap_calls_and_accesses_without_changing_editable_declarations() {
        for (path, source, closing) in [
            (
                "plugin.rs",
                "struct ArenaPlugin; impl ArenaPlugin { /// Validates configuration.\n pub fn new(config: ArenaConfig) -> Self { config.validate(); config.enabled; Self } }",
                "}",
            ),
            (
                "client.ts",
                "class Client { update(config: Config) { config.validate(); config.enabled; } }",
                "}",
            ),
            (
                "main.lua",
                "function update(config)\n config.validate()\n local enabled = config.enabled\nend\n",
                "end",
            ),
        ] {
            let declaration = DeclarationOverview::extract(path, source).unwrap();
            let calls = extract(path, source).unwrap();
            let display = present(path, &declaration, &calls).unwrap();
            let rows = display.declaration.text.lines().collect::<Vec<_>>();
            let (opening, body) = display.body.iter().next().unwrap();
            assert!(body.heading <= *opening);
            assert_eq!(rows[body.end].trim(), closing);
            assert!(rows[*opening + 1].trim() == "Calls");
            assert!(
                display
                    .call_row
                    .iter()
                    .all(|(row, _)| *row > *opening && *row < body.end)
            );
            if closing == "}" {
                assert!(rows[*opening].ends_with(" {"));
                assert!(!rows[*opening].contains("; {"));
                assert!(format!("{}{}", rows[*opening], body.collapsed_suffix).ends_with(" {...}"));
            }
            let visibility = visibility(path, &display, false).unwrap();
            assert_eq!(visibility.rows.len(), rows.len());
            let editable = combined(path, &declaration, &calls).unwrap();
            assert_eq!(
                parse(path, &editable, &calls).unwrap(),
                (declaration, calls)
            );
        }
    }

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

    #[test]
    fn grouped_categories_preserve_interleaved_occurrences_and_kind_identity() {
        for (path, source, target) in [
            (
                "main.rs",
                "fn run(client: Client) { client.count; z(); client.count(); client.count; a(); }",
                "Client::count",
            ),
            (
                "main.ts",
                "function run(client: Client) { client.count; z(); client.count(); client.count; a(); }",
                "Client.count",
            ),
            (
                "main.lua",
                "function run(client)\n local first = client.count\n z()\n client.count()\n local second = client.count\n a()\nend\n",
                "client.count",
            ),
        ] {
            let declaration = DeclarationOverview::extract(path, source).unwrap();
            let calls = extract(path, source).unwrap();
            let editable = combined(path, &declaration, &calls).unwrap();
            assert!(editable.find("Calls\n").unwrap() < editable.find("Accesses\n").unwrap());
            assert!(!editable.contains("property ") && !editable.contains("call "));
            let (parsed, restored) = parse(path, &editable, &calls).unwrap();
            assert_eq!(parsed, declaration);
            assert_eq!(
                restored, calls,
                "{path}: categorization reordered saved occurrences"
            );
            let display = present(path, &declaration, &calls).unwrap();
            assert_eq!(
                display
                    .declaration
                    .text
                    .lines()
                    .map(str::trim)
                    .filter(|line| matches!(*line, "Calls" | "Accesses"))
                    .collect::<Vec<_>>(),
                ["Calls", "Accesses"]
            );
            assert_eq!(
                display
                    .call_row
                    .values()
                    .filter(|(_, name, _)| name == target)
                    .map(|(_, _, kind)| *kind)
                    .collect::<Vec<_>>(),
                [CallKind::Call, CallKind::Property]
            );
            let edited = editable.replace(&format!("Accesses\n  {target}"), "Accesses\n  changed");
            let (_, edited_calls) = parse(path, &edited, &calls).unwrap();
            assert_eq!(
                edited_calls[0]
                    .call
                    .iter()
                    .flatten()
                    .map(|call| call.kind)
                    .collect::<Vec<_>>(),
                calls[0]
                    .call
                    .iter()
                    .flatten()
                    .map(|call| call.kind)
                    .collect::<Vec<_>>()
            );
        }
        assert!(parse("main.rs", "fn run();\nCalls\nCalls\n", &[]).is_err());
        assert!(parse("main.rs", "fn run();\nAccesses\nAccesses\n", &[]).is_err());
        assert!(parse("main.rs", "struct Data;\nAccesses\n  Data::field\n", &[]).is_err());
    }

    #[test]
    fn malformed_calls_reject_arguments_and_unattached_blocks() {
        assert!(parse("main.rs", "fn run();\nCalls\n  send(value)\n", &[]).is_err());
        assert!(parse("main.rs", "struct Data;\nCalls\n  send\n", &[]).is_err());
    }

    #[test]
    fn authored_parameter_calls_do_not_resolve_same_named_imports() {
        let (_, calls) = parse(
            "run.ts",
            "import { send } from './client';\nfunction run(send: () => void);\nCalls\n  send\n",
            &[],
        )
        .unwrap();
        assert!(calls[0].call.as_ref().unwrap()[0].unresolved);
        let previous = vec![FunctionBody {
            evidence: None,
            behavior: None,
            owner: "run".into(),
            call: Some(vec![CallSite {
                kind: crate::plan::CallKind::Call,
                name: "send".into(),
                source: None,
                unresolved: false,
            }]),
        }];
        let (_, calls) = parse(
            "run.ts",
            "function run(send: () => void);\nCalls\n  send\n",
            &previous,
        )
        .unwrap();
        assert!(calls[0].call.as_ref().unwrap()[0].unresolved);
    }
}
