use std::collections::{BTreeMap, BTreeSet};

use anyhow::{Context, Result, ensure};
use forge_diff::syntax::{
    DeclarationCalls, DeclarationOverview, DeclarationPosition, DeclarationPresentation,
};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

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
}

impl CallKind {
    /// Preserve canonical bytes for older invocation-only plan snapshots.
    pub(crate) fn is_call(&self) -> bool {
        *self == Self::Call
    }

    /// Classify the occurrence in a reference picker and navigation identity.
    pub(crate) fn label(self) -> &'static str {
        match self {
            Self::Call => "call",
            Self::Property => "property",
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
}

/// An ordered sequence of invocations and property accesses belonging to a named callable.
#[derive(Clone, Debug, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
pub struct FunctionCalls {
    /// Callable identity within its saved declaration file.
    pub owner: String,
    /// Ordered occurrences, including repeated targets.
    pub call: Vec<CallSite>,
}

/// Maps display rows to saved declarations and call identities.
pub(crate) struct CallPresentation {
    /// Signature-only text used by declaration analysis before synthetic bodies are rendered.
    pub plain: String,
    pub declaration: DeclarationPresentation,
    pub call_row: BTreeMap<usize, (String, String, CallKind)>,
    /// Rendered body boundaries keyed by their final signature row.
    pub body: BTreeMap<usize, CallBody>,
    pub plain_row: Vec<usize>,
    /// Original declaration rows mapped to their rendered rows, excluding synthetic body rows.
    pub declaration_row: BTreeMap<usize, usize>,
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

/// Extract source call evidence without capturing unrelated files.
#[cfg(test)]
pub(crate) fn extract(path: &str, source: &str) -> Result<Vec<FunctionCalls>> {
    Ok(from_extracted(
        DeclarationCalls::extract(path, source, false)
            .map_err(|error| anyhow::anyhow!("{error:?}"))?,
    ))
}

pub(crate) fn from_extracted(
    functions: Vec<forge_diff::syntax::DeclarationCallable>,
) -> Vec<FunctionCalls> {
    functions
        .into_iter()
        .map(|function| FunctionCalls {
            owner: function.owner,
            call: function
                .call
                .into_iter()
                .map(|call| CallSite {
                    kind: match call.kind {
                        forge_diff::syntax::DeclarationCallKind::Call => CallKind::Call,
                        forge_diff::syntax::DeclarationCallKind::Property => CallKind::Property,
                    },
                    name: call.name,
                    unresolved: call.unresolved,
                    source: Some(CallPosition {
                        line: call.line,
                        column: call.column,
                    }),
                })
                .collect(),
        })
        .collect()
}

/// Return an editable file view in declaration and stored call order.
pub(crate) fn combined(path: &str, text: &str, calls: &[FunctionCalls]) -> Result<String> {
    Ok(insert(path, text, calls, false, false)?.declaration.text)
}

/// Render alphabetical unique targets while preserving the saved occurrence sequence.
pub(crate) fn present(path: &str, text: &str, calls: &[FunctionCalls]) -> Result<CallPresentation> {
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
    Ok(result)
}

pub(crate) fn insert(
    path: &str,
    text: &str,
    calls: &[FunctionCalls],
    review: bool,
    sort: bool,
) -> Result<CallPresentation> {
    if calls.is_empty() {
        return Ok(CallPresentation {
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
        source.push(Some(DeclarationPosition {
            line: index as u32 + 1,
            column: 0,
        }));
        if let Some(functions) = at.get(&(index + 1)) {
            for (function, calls) in functions {
                let indent = &line[..line.len() - line.trim_start().len()];
                let opening = source.len() - 1;
                let heading = declaration_row[&(function.line as usize - 1)];
                let body_indent = if review { "  " } else { "" };
                for kind in [CallKind::Call, CallKind::Property] {
                    let mut targets = calls
                        .call
                        .iter()
                        .filter(|call| call.kind == kind)
                        .map(|call| call.name.as_str())
                        .collect::<Vec<_>>();
                    if targets.is_empty() && !(kind == CallKind::Call && calls.call.is_empty()) {
                        continue;
                    }
                    if review {
                        let mut seen = std::collections::HashSet::with_capacity(targets.len());
                        targets.retain(|name| seen.insert(*name));
                        if sort {
                            targets.sort_unstable();
                        }
                    }
                    let heading = match kind {
                        CallKind::Call => "Calls",
                        CallKind::Property => "Accesses",
                    };
                    output.push_str(&format!("{indent}{body_indent}{heading}\n"));
                    source.push(Some(DeclarationPosition {
                        line: function.line,
                        column: function.column,
                    }));
                    for name in targets {
                        call_row.insert(
                            source.len(),
                            (function.owner.clone(), name.to_owned(), kind),
                        );
                        output.push_str(&format!("{indent}{body_indent}  {name}\n"));
                        source.push(Some(DeclarationPosition {
                            line: function.line,
                            column: function.column,
                        }));
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
                    source.push(Some(DeclarationPosition {
                        line: function.line,
                        column: function.column,
                    }));
                }
            }
        }
    }
    let plain_row = source
        .iter()
        .map(|position| position.unwrap().line as usize - 1)
        .collect();
    Ok(CallPresentation {
        plain: text.into(),
        declaration: DeclarationPresentation {
            text: output,
            source,
        },
        call_row,
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
    let visibility =
        forge_diff::syntax::DeclarationVisibility::analyze(path, &presentation.plain, public_only)
            .map_err(|error| anyhow::anyhow!("{error:?}"))?;
    Ok(forge_diff::syntax::DeclarationVisibility {
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
    })
}

/// Split combined patch text, retaining evidence for unchanged call occurrences.
pub(crate) fn parse(
    path: &str,
    text: &str,
    previous: &[FunctionCalls],
) -> Result<(String, Vec<FunctionCalls>)> {
    if forge_diff::syntax::ConfigurationFormat::for_path(path).is_some() {
        return Ok((
            DeclarationOverview::parse(path, text).map_err(|error| anyhow::anyhow!("{error:?}"))?,
            Vec::new(),
        ));
    }
    ensure!(
        text.len() <= 1024 * 1024 && !text.contains('\0'),
        "combined declaration exceeds 1 MiB or contains NUL"
    );
    let lines = text.lines().collect::<Vec<_>>();
    let protected = DeclarationCalls::protected_lines(path, text)
        .map_err(|error| anyhow::anyhow!("{error:?}"))?;
    let mut declaration = String::new();
    let mut block = Vec::new();
    let mut original_line = Vec::new();
    let mut index = 0;
    let mut occurrence_count = 0;
    while index < lines.len() {
        let line = lines[index];
        if matches!(line.trim(), "Calls" | "Accesses") && !protected.contains(&index) {
            let kind = if line.trim() == "Accesses" {
                CallKind::Property
            } else {
                CallKind::Call
            };
            let indent = line.len() - line.trim_start().len();
            let preceding = original_line
                .last()
                .copied()
                .context("Function uses must follow a callable declaration")?;
            index += 1;
            let mut names = Vec::new();
            while index < lines.len() {
                let value = lines[index];
                if value.trim().is_empty() || value.len() - value.trim_start().len() <= indent {
                    break;
                }
                let name = value.trim();
                ensure!(
                    !name.is_empty()
                        && name
                            .chars()
                            .all(|character| character.is_alphanumeric()
                                || "_:.<>#".contains(character)),
                    "Function uses contain only a qualified target name, without arguments: {name}"
                );
                names.push((name.to_owned(), kind));
                occurrence_count += 1;
                ensure!(
                    occurrence_count <= 65536,
                    "Function uses exceed 65536 occurrences"
                );
                index += 1;
            }
            block.push((original_line.len(), preceding, kind, names));
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
    let mut sections = BTreeMap::<usize, BTreeMap<CallKind, Vec<(String, CallKind)>>>::new();
    let mut seen = BTreeSet::new();
    let by_line = functions
        .iter()
        .map(|function| (function.end_line as usize, function))
        .collect::<BTreeMap<_, _>>();
    let by_owner = previous
        .iter()
        .map(|calls| (calls.owner.as_str(), calls))
        .collect::<BTreeMap<_, _>>();
    for (line, _, kind, names) in block {
        let function = by_line
            .get(&line)
            .context("Function uses must immediately follow a callable signature")?;
        ensure!(
            seen.insert((function.owner.clone(), kind)),
            "duplicate {:?} block for {}",
            kind,
            function.owner
        );
        sections.entry(line).or_default().insert(kind, names);
    }
    let mut output = Vec::new();
    for (line, sections) in sections {
        let function = by_line[&line];
        let mut category = sections
            .into_iter()
            .map(|(kind, names)| (kind, std::collections::VecDeque::from(names)))
            .collect::<BTreeMap<_, _>>();
        let mut names = Vec::new();
        for previous in by_owner
            .get(function.owner.as_str())
            .into_iter()
            .flat_map(|calls| &calls.call)
        {
            if let Some(entry) = category
                .get_mut(&previous.kind)
                .and_then(std::collections::VecDeque::pop_front)
            {
                names.push(entry);
            }
        }
        for remaining in category.into_values() {
            names.extend(remaining);
        }
        let mut remaining =
            BTreeMap::<(String, CallKind), std::collections::VecDeque<CallSite>>::new();
        for call in by_owner
            .get(function.owner.as_str())
            .into_iter()
            .flat_map(|calls| &calls.call)
        {
            remaining
                .entry((call.name.clone(), call.kind))
                .or_default()
                .push_back(call.clone());
        }
        let call = names
            .into_iter()
            .map(|(name, kind)| {
                let unresolved = function
                    .binding
                    .iter()
                    .any(|binding| name == *binding || name.starts_with(&format!("{binding}.")));
                let mut call = remaining
                    .get_mut(&(name.clone(), kind))
                    .and_then(std::collections::VecDeque::pop_front)
                    .unwrap_or_else(|| CallSite {
                        kind,
                        name,
                        source: None,
                        unresolved,
                    });
                call.unresolved |= unresolved;
                call
            })
            .collect();
        output.push(FunctionCalls {
            owner: function.owner.clone(),
            call,
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
    fn display_sorting_is_opt_in_for_both_categories_and_preserves_occurrences() {
        let path = "main.rs";
        let source = "fn run(client: Client) { z(); client.z; a(); client.a; z(); client.z; }";
        let declaration = DeclarationOverview::extract(path, source).unwrap();
        let calls = extract(path, source).unwrap();
        let original = calls.clone();
        let default = present(path, &declaration, &calls).unwrap();
        assert_eq!(default.call_row.values().map(|(_, name, _)| name.as_str()).collect::<Vec<_>>(), ["z", "a", "Client::z", "Client::a"]);
        let sorted = insert(path, &default.plain, &calls, true, true).unwrap();
        assert_eq!(sorted.call_row.values().map(|(_, name, _)| name.as_str()).collect::<Vec<_>>(), ["a", "z", "Client::a", "Client::z"]);
        assert_eq!(calls, original);
        assert_eq!(calls[0].call.len(), 6);
        assert_eq!(parse(path, &combined(path, &declaration, &calls).unwrap(), &calls).unwrap().1, original);
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
            assert_eq!(sorted.call_row.values().map(|(_, name, _)| name.as_str()).collect::<Vec<_>>(), ["a", "z"]);
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
                    .map(|call| call.kind)
                    .collect::<Vec<_>>(),
                calls[0]
                    .call
                    .iter()
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
        assert!(calls[0].call[0].unresolved);
        let previous = vec![FunctionCalls {
            owner: "run".into(),
            call: vec![CallSite {
                kind: crate::plan::CallKind::Call,
                name: "send".into(),
                source: None,
                unresolved: false,
            }],
        }];
        let (_, calls) = parse(
            "run.ts",
            "function run(send: () => void);\nCalls\n  send\n",
            &previous,
        )
        .unwrap();
        assert!(calls[0].call[0].unresolved);
    }
}
