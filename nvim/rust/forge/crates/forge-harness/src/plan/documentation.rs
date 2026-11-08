use std::collections::BTreeSet;

use anyhow::Result;
use forge_diff::syntax::{DeclarationOverview, DeclarationRole};

use super::{DeclarationDesign, references::PlanReferenceIndex};
use crate::declaration::{DeclarationDiagnostic, DeclarationDiagnosticKind};

/// Require a documentation comment on every declaration the plan introduces, including enum
/// variants and fields, so reviewers read each new name's role where it is declared.
/// Captured source keeps its existing documentation; parameters, bindings, modules, and test or
/// trait-contract entries are exempt because their role is stated by their owner.
pub(super) fn diagnostics(
    design: &DeclarationDesign,
    index: &PlanReferenceIndex,
) -> Result<Vec<DeclarationDiagnostic>> {
    let mut diagnostic = Vec::new();
    for (path, declaration) in &index.declaration {
        let Some(marker) = documentation_marker(path) else {
            continue;
        };
        let Some(text) = design.proposed.get(path) else {
            continue;
        };
        let lines = text.lines().collect::<Vec<_>>();
        let documented_end = DeclarationOverview::comments(path, text)
            .map_err(|error| anyhow::anyhow!("extract plan comments from {path}: {error:?}"))?
            .into_iter()
            .filter(|comment| marker.iter().any(|marker| comment.text.starts_with(marker)))
            .map(|comment| {
                let span = comment.text.trim_end_matches(['\r', '\n']).lines().count();
                comment.position.line + span.saturating_sub(1) as u32
            })
            .collect::<BTreeSet<_>>();
        for symbol in &declaration.symbol {
            if symbol.parameter
                || symbol.external_entry
                || matches!(
                    symbol.role,
                    DeclarationRole::Binding | DeclarationRole::Module
                )
                || !index.introduced.contains(&(
                    path.clone(),
                    symbol.position.line,
                    symbol.position.column,
                ))
            {
                continue;
            }
            if preceding_line(&lines, symbol.position.line)
                .is_some_and(|line| documented_end.contains(&line))
            {
                continue;
            }
            let owner = symbol
                .scope
                .iter()
                .chain(std::iter::once(&symbol.name))
                .cloned()
                .collect::<Vec<_>>()
                .join("::");
            diagnostic.push(DeclarationDiagnostic {
                path: path.clone(),
                line: symbol.position.line,
                column: symbol.position.column,
                reference: owner,
                error: true,
                reason: format!(
                    "New declaration has no documentation comment. Add a `{}` comment directly above it that states its role.",
                    marker[0]
                ),
                kind: DeclarationDiagnosticKind::MissingDocumentation,
            });
        }
    }
    Ok(diagnostic)
}

/// Documentation comment openings for languages whose declarations plans document.
fn documentation_marker(path: &str) -> Option<&'static [&'static str]> {
    let extension = std::path::Path::new(path).extension()?.to_str()?;
    match extension {
        "rs" => Some(&["///", "/**"]),
        "ts" | "tsx" | "mts" | "cts" => Some(&["/**"]),
        _ => None,
    }
}

/// Find the one-based line a documentation comment must end on: the nearest line above the
/// declaration that is not an attribute or decorator.
fn preceding_line(lines: &[&str], declaration_line: u32) -> Option<u32> {
    let mut line = declaration_line.checked_sub(1)?;
    while line > 0 {
        let text = lines.get(line as usize - 1)?.trim_start();
        if !(text.starts_with("#[") || text.starts_with('@')) {
            return Some(line);
        }
        line -= 1;
    }
    None
}

#[cfg(test)]
mod tests {
    use super::diagnostics;
    use crate::plan::{DeclarationDesign, references::PlanReferenceIndex};

    #[test]
    fn every_introduced_type_variant_and_field_needs_documentation() {
        let workspace = tempfile::tempdir().expect("workspace");
        std::fs::write(
            workspace.path().join("Cargo.toml"),
            "[package]\nname = \"arena\"\nversion = \"0.1.0\"\nedition = \"2024\"\n",
        )
        .expect("manifest");
        let mut design = DeclarationDesign::default();
        design.proposed.insert(
            "src/main.rs".into(),
            "// Closed gameplay state.
enum RunPhase {
  /// The player is alive.
  Active,
  Defeated,
}

/// Movement tuning for the player.
#[derive(Component)]
struct Player {
  /// Units per second.
  speed: f32,
  health: u32,
}
"
            .into(),
        );
        let index = PlanReferenceIndex::design(&design, workspace.path()).expect("index");
        let mut reported = diagnostics(&design, &index)
            .expect("documentation")
            .into_iter()
            .map(|diagnostic| diagnostic.reference)
            .collect::<Vec<_>>();
        reported.sort();
        assert_eq!(
            reported,
            ["Player::health", "RunPhase", "RunPhase::Defeated"]
        );
    }
}
