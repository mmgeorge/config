use std::collections::{BTreeMap, BTreeSet};

use anyhow::{Context, Result, ensure};
use forge_diff::syntax::{
    DeclarationCalls, DeclarationOverview, DeclarationPosition, DeclarationPresentation,
};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

/// One call occurrence, retaining source order independently of display sorting.
#[derive(Clone, Debug, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
pub struct CallSite {
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

/// An ordered call list belonging to a named callable.
#[derive(Clone, Debug, Deserialize, Eq, JsonSchema, PartialEq, Serialize)]
pub struct FunctionCalls {
    /// Callable identity within its saved declaration file.
    pub owner: String,
    /// Ordered occurrences, including repeated targets.
    pub call: Vec<CallSite>,
}

/// Maps display rows to saved declarations and call identities.
pub(crate) struct CallPresentation {
    pub declaration: DeclarationPresentation,
    pub call_row: BTreeMap<usize, (String, String)>,
    pub owner_row: BTreeMap<usize, String>,
    pub plain_row: Vec<usize>,
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
    Ok(insert(path, text, calls, false)?.declaration.text)
}

/// Render alphabetical unique targets while preserving the saved occurrence sequence.
pub(crate) fn present(path: &str, text: &str, calls: &[FunctionCalls]) -> Result<CallPresentation> {
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
    Ok(result)
}

pub(crate) fn insert(
    path: &str,
    text: &str,
    calls: &[FunctionCalls],
    sorted: bool,
) -> Result<CallPresentation> {
    if calls.is_empty() {
        return Ok(CallPresentation {
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
            owner_row: BTreeMap::new(),
            plain_row: (0..text.lines().count()).collect(),
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
    let mut owner_row = BTreeMap::new();
    for (index, line) in text.lines().enumerate() {
        output.push_str(line);
        output.push('\n');
        source.push(Some(DeclarationPosition {
            line: index as u32 + 1,
            column: 0,
        }));
        if let Some(functions) = at.get(&(index + 1)) {
            for (function, calls) in functions {
                let indent = &line[..line.len() - line.trim_start().len()];
                owner_row.insert(source.len(), function.owner.clone());
                output.push_str(&format!("{indent}Calls\n"));
                source.push(Some(DeclarationPosition {
                    line: function.line,
                    column: function.column,
                }));
                let names = if sorted {
                    calls
                        .call
                        .iter()
                        .map(|call| call.name.as_str())
                        .collect::<BTreeSet<_>>()
                        .into_iter()
                        .collect::<Vec<_>>()
                } else {
                    calls.call.iter().map(|call| call.name.as_str()).collect()
                };
                for name in names {
                    call_row.insert(source.len(), (function.owner.clone(), name.to_owned()));
                    output.push_str(&format!("{indent}  {name}\n"));
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
        declaration: DeclarationPresentation {
            text: output,
            source,
        },
        call_row,
        owner_row,
        plain_row,
    })
}

/// Apply declaration visibility to synthetic call rows through their owning signature.
pub(crate) fn visibility(
    path: &str,
    presentation: &CallPresentation,
    public_only: bool,
) -> Result<forge_diff::syntax::DeclarationVisibility> {
    let (plain, _) = parse(path, &presentation.declaration.text, &[])?;
    let visibility = forge_diff::syntax::DeclarationVisibility::analyze(path, &plain, public_only)
        .map_err(|error| anyhow::anyhow!("{error:?}"))?;
    Ok(forge_diff::syntax::DeclarationVisibility {
        rows: presentation
            .plain_row
            .iter()
            .map(|row| visibility.rows.get(*row).copied().unwrap_or(false))
            .collect(),
        replacement: presentation
            .plain_row
            .iter()
            .enumerate()
            .filter_map(|(row, plain)| {
                if presentation.call_row.contains_key(&row)
                    || presentation.owner_row.contains_key(&row)
                {
                    return None;
                }
                visibility
                    .replacement
                    .get(plain)
                    .map(|text| (row, text.clone()))
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
        if line.trim() == "Calls" && !protected.contains(&index) {
            let indent = line.len() - line.trim_start().len();
            let preceding = original_line
                .last()
                .copied()
                .context("Calls must follow a callable declaration")?;
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
                    "Calls entries contain only a qualified target name, without arguments: {name}"
                );
                names.push(name.to_owned());
                occurrence_count += 1;
                ensure!(occurrence_count <= 65536, "Calls exceed 65536 occurrences");
                index += 1;
            }
            block.push((original_line.len(), preceding, names));
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
    let mut output = Vec::new();
    let mut seen = BTreeSet::new();
    let by_line = functions
        .iter()
        .map(|function| (function.end_line as usize, function))
        .collect::<BTreeMap<_, _>>();
    let by_owner = previous
        .iter()
        .map(|calls| (calls.owner.as_str(), calls))
        .collect::<BTreeMap<_, _>>();
    for (line, _, names) in block {
        let function = by_line
            .get(&line)
            .context("Calls must immediately follow a callable signature")?;
        ensure!(
            seen.insert(function.owner.clone()),
            "duplicate Calls block for {}",
            function.owner
        );
        let mut remaining = BTreeMap::<String, std::collections::VecDeque<CallSite>>::new();
        for call in by_owner
            .get(function.owner.as_str())
            .into_iter()
            .flat_map(|calls| &calls.call)
        {
            remaining
                .entry(call.name.clone())
                .or_default()
                .push_back(call.clone());
        }
        let call = names
            .into_iter()
            .map(|name| {
                let unresolved = function
                    .binding
                    .iter()
                    .any(|binding| name == *binding || name.starts_with(&format!("{binding}.")));
                let mut call = remaining
                    .get_mut(&name)
                    .and_then(std::collections::VecDeque::pop_front)
                    .unwrap_or_else(|| CallSite {
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
                    .map(|(_, name)| name.as_str())
                    .collect::<Vec<_>>(),
                ["a", "z"],
                "{path}"
            );
        }
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
