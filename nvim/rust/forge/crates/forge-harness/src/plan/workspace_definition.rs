use anyhow::Result;
use forge_diff::syntax::{DeclarationIndex, DeclarationPosition};
use serde::{Deserialize, Serialize};

/// Immutable real-source evidence for one on-demand editor definition request.
#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub struct WorkspaceDefinition {
    /// Absolute existing source path. No generated document is created.
    pub path: String,
    /// SHA-256 of the exact captured file bytes.
    pub digest: String,
    /// One-based source line.
    pub line: u32,
    /// Zero-based byte column of the referenced identifier.
    pub column: u32,
    /// Neovim filetype used to select an already-running client.
    pub filetype: String,
}

/// A definition location normalized by Neovim into UTF-8 byte coordinates.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct DefinitionLocation {
    /// Absolute file path returned by the project language server.
    pub path: String,
    /// One-based source line.
    pub line: u32,
    /// Zero-based byte column.
    pub column: u32,
    /// A modified editor target must open as source rather than map to the saved plan.
    #[serde(default)]
    pub modified: bool,
}

/// Map the same indexed declaration token between saved and real source syntax.
pub(super) fn map_position(
    path: &str,
    from: &str,
    to: &str,
    position: DeclarationPosition,
) -> Result<Option<DeclarationPosition>> {
    let source =
        DeclarationIndex::extract(path, from).map_err(|error| anyhow::anyhow!("{error:?}"))?;
    let target =
        DeclarationIndex::extract(path, to).map_err(|error| anyhow::anyhow!("{error:?}"))?;
    for symbol in &source.symbol {
        if symbol.position.line == position.line
            && symbol.position.column <= position.column
            && position.column < symbol.position.column + symbol.name.len() as u32
        {
            let matches = target
                .symbol
                .iter()
                .filter(|candidate| {
                    candidate.name == symbol.name
                        && candidate.scope == symbol.scope
                        && candidate.role == symbol.role
                })
                .collect::<Vec<_>>();
            return Ok((matches.len() == 1).then(|| DeclarationPosition {
                line: matches[0].position.line,
                column: matches[0].position.column + position.column - symbol.position.column,
            }));
        }
    }
    for reference in &source.reference {
        if reference.position.line == position.line
            && reference.position.column <= position.column
            && position.column < reference.position.column + reference.length as u32
        {
            for import in source.import.iter().filter(|import| {
                reference.scope.starts_with(&import.scope)
                    && (import.alias.as_ref() == reference.path.first()
                        || import.alias.is_none() && import.path.last() == reference.path.first())
            }) {
                if !target.import.iter().any(|candidate| {
                    candidate.path == import.path
                        && candidate.source == import.source
                        && candidate.alias == import.alias
                        && candidate.scope == import.scope
                }) {
                    return Ok(None);
                }
            }
            let matches = target
                .reference
                .iter()
                .filter(|candidate| {
                    candidate.path == reference.path
                        && candidate.scope == reference.scope
                        && candidate.value_namespace == reference.value_namespace
                })
                .collect::<Vec<_>>();
            return Ok((matches.len() == 1).then(|| DeclarationPosition {
                line: matches[0].position.line,
                column: matches[0].position.column + position.column - reference.position.column,
            }));
        }
    }
    for import in &source.import {
        if import.position.line == position.line && import.position.column == position.column {
            let matches = target
                .import
                .iter()
                .filter(|candidate| {
                    candidate.path == import.path
                        && candidate.scope == import.scope
                        && candidate.alias == import.alias
                        && candidate.source == import.source
                })
                .collect::<Vec<_>>();
            return Ok((matches.len() == 1).then(|| matches[0].position));
        }
    }
    Ok(None)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn changed_imports_do_not_reuse_workspace_reference_evidence() {
        let source =
            "import { Value } from 'existing';\nexport function run(value: Value): void {}\n";
        let saved = "import { Value } from 'proposed';\nexport function run(value: Value): void;\n";
        let position = DeclarationPosition {
            line: 2,
            column: 27,
        };
        assert!(
            map_position("run.ts", source, saved, position)
                .unwrap()
                .is_none()
        );
        let saved = saved.replace("proposed", "existing");
        assert_eq!(
            map_position("run.ts", source, &saved, position).unwrap(),
            Some(position)
        );
    }
}
