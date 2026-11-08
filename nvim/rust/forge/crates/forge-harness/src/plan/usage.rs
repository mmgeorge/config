use std::collections::{BTreeMap, BTreeSet};
use std::path::Path;

use anyhow::Result;
use forge_diff::syntax::DeclarationRole;

use super::{DeclarationDesign, references::PlanReferenceIndex};
use crate::declaration::{DeclarationDiagnostic, DeclarationValidationError};

/// Reject new internal declarations without resolved incoming uses at submission time.
pub(super) fn validate(design: &DeclarationDesign, workspace: &Path) -> Result<()> {
    let index = PlanReferenceIndex::design(design, workspace)?;
    if index.introduced.is_empty() {
        return Ok(());
    }
    let external = crate::declaration::exposure::symbols(workspace, design)?;
    let mut incoming = BTreeMap::<String, BTreeSet<(String, String, String)>>::new();
    for reference in &index.occurrence {
        if matches!(reference.kind.as_str(), "import" | "owner") {
            continue;
        }
        incoming
            .entry(reference.symbol.clone())
            .or_default()
            .insert((
                reference.path.clone(),
                normalized(&reference.owner),
                reference.kind.clone(),
            ));
    }
    let mut used = BTreeSet::new();
    let mut used_container = BTreeSet::new();
    for (path, declaration) in &index.declaration {
        for symbol in &declaration.symbol {
            let position = (path.clone(), symbol.position.line, symbol.position.column);
            let owner = normalized(
                &symbol
                    .scope
                    .iter()
                    .chain(std::iter::once(&symbol.name))
                    .cloned()
                    .collect::<Vec<_>>()
                    .join("::"),
            );
            let incoming = index
                .definition
                .get(&position)
                .and_then(|(identity, _)| incoming.get(identity))
                .is_some_and(|uses| {
                    uses.iter().any(|(caller_path, caller, kind)| {
                        !(caller_path == path && caller == &owner)
                            && (symbol.role != DeclarationRole::Callable
                                || matches!(
                                    kind.as_str(),
                                    "call" | "callback" | "value" | "property" | "construct"
                                ))
                    })
                });
            if incoming {
                used.insert(position.clone());
            }
            if incoming || external.contains(&position) {
                for depth in 1..=symbol.scope.len() {
                    used_container.insert((path.clone(), symbol.scope[..depth].to_vec()));
                }
            }
        }
    }
    let mut module_owner = BTreeMap::<String, Vec<(String, u32, u32)>>::new();
    for (position, source) in &index.module_source {
        if let Ok(path) = source.strip_prefix(workspace) {
            module_owner
                .entry(path.to_string_lossy().replace('\\', "/"))
                .or_default()
                .push(position.clone());
        }
    }
    let mut used_file = used
        .iter()
        .chain(&external)
        .map(|position| position.0.clone())
        .collect::<BTreeSet<_>>();
    let mut pending = std::collections::VecDeque::from_iter(used_file.iter().cloned());
    while let Some(path) = pending.pop_front() {
        for position in module_owner.get(&path).into_iter().flatten() {
            used.insert(position.clone());
            if used_file.insert(position.0.clone()) {
                pending.push_back(position.0.clone());
            }
        }
    }
    let mut diagnostic = Vec::new();
    for (path, declaration) in &index.declaration {
        for symbol in &declaration.symbol {
            let line = symbol.position.line;
            let column = symbol.position.column;
            let position = (path.clone(), line, column);
            if !index.introduced.contains(&position)
                || external.contains(&position)
                || used.contains(&position)
            {
                continue;
            }
            let scope = symbol
                .scope
                .iter()
                .chain(std::iter::once(&symbol.name))
                .cloned()
                .collect::<Vec<_>>();
            if symbol.role == DeclarationRole::Module
                && used_container.contains(&(path.clone(), scope.clone()))
            {
                continue;
            }
            let owner = normalized(&scope.join("::"));
            let (role, repair) = match symbol.role {
                DeclarationRole::Callable => (
                    "function",
                    "Add its invocation or callback registration to the caller's Calls section.",
                ),
                DeclarationRole::Property => (
                    "property",
                    "Add its read, write, construction, or destructuring use to the user's Accesses section.",
                ),
                DeclarationRole::Type => (
                    "type",
                    "Add its signature reference or construction to the caller's Constructs section.",
                ),
                DeclarationRole::Value => (
                    "value",
                    "Add a resolved type reference or a named use in Calls, Constructs, or Accesses.",
                ),
                DeclarationRole::Module => (
                    "module",
                    "Add a resolved type reference or a named use in Calls, Constructs, or Accesses.",
                ),
                DeclarationRole::Variant => (
                    "variant",
                    "Add a resolved type reference or a named use in Calls, Constructs, or Accesses.",
                ),
                DeclarationRole::Binding => continue,
            };
            diagnostic.push(DeclarationDiagnostic {
                path: path.clone(),
                line,
                column,
                reference: owner,
                error: true,
                reason: format!("New internal {role} has no recorded incoming use. {repair}"),
            });
        }
    }
    if diagnostic.is_empty() {
        Ok(())
    } else {
        Err(DeclarationValidationError { diagnostic }.into())
    }
}

fn normalized(owner: &str) -> String {
    owner
        .split("::")
        .filter(|scope| !scope.starts_with("@impl:"))
        .flat_map(|scope| scope.split(['.', ':']))
        .filter(|scope| !scope.is_empty())
        .collect::<Vec<_>>()
        .join("::")
}
