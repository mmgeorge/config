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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::plan::calls;
    use forge_diff::syntax::DeclarationOverview;

    fn design(path: &str, source: &str) -> DeclarationDesign {
        let mut design = DeclarationDesign::default();
        design.proposed.insert(
            path.into(),
            DeclarationOverview::extract(path, source).unwrap(),
        );
        design
            .proposed_calls
            .insert(path.into(), calls::extract(path, source).unwrap());
        design
    }

    #[test]
    fn rejects_unrecorded_registration_and_accepts_corrective_calls() {
        let workspace = tempfile::tempdir().unwrap();
        let mut design = design(
            "lib.rs",
            "pub fn install() {}\nfn spawn_collectibles() {}\nfn resolve_collection() {}\n",
        );
        let failure = validate(&design, workspace.path()).unwrap_err();
        let error = failure
            .downcast_ref::<DeclarationValidationError>()
            .unwrap();
        assert_eq!(error.diagnostic.len(), 2);
        assert!(
            error
                .diagnostic
                .iter()
                .all(|error| error.path == "lib.rs" && error.reason.contains("Calls"))
        );
        let combined = calls::combined(
            "lib.rs",
            &design.proposed["lib.rs"],
            &design.proposed_calls["lib.rs"],
        )
        .unwrap();
        let fixed = combined.replacen(
            "Calls\n",
            "Calls\n  spawn_collectibles\n  resolve_collection\n",
            1,
        );
        let (declaration, body) =
            calls::parse("lib.rs", &fixed, &design.proposed_calls["lib.rs"]).unwrap();
        design.proposed.insert("lib.rs".into(), declaration);
        design.proposed_calls.insert("lib.rs".into(), body);
        validate(&design, workspace.path()).unwrap();
    }

    #[test]
    fn self_recursion_and_imports_do_not_count_but_mutual_calls_do() {
        let workspace = tempfile::tempdir().unwrap();
        let recursive = design("lib.rs", "fn unused() { unused(); }\n");
        assert!(validate(&recursive, workspace.path()).is_err());
        let mutual = design(
            "lib.rs",
            "fn first() { second(); }\nfn second() { first(); }\n",
        );
        validate(&mutual, workspace.path()).unwrap();
        let mut imported = design(
            "lib.rs",
            "mod internal { pub fn unused() {} }\nuse internal::unused;\n",
        );
        assert!(
            validate(&imported, workspace.path())
                .unwrap_err()
                .to_string()
                .contains("unused")
        );
        imported.proposed_calls.clear();
        assert!(validate(&imported, workspace.path()).is_err());
    }

    #[test]
    fn public_api_exposure_respects_modules_reexports_and_member_visibility() {
        let workspace = tempfile::tempdir().unwrap();
        let exposed = design(
            "lib.rs",
            "mod hidden { pub struct Api; impl Api { pub fn new() -> Self { Self } } }\npub use hidden::Api;\n",
        );
        validate(&exposed, workspace.path()).unwrap();
        let exported_value = design(
            "lib.rs",
            "mod hidden { pub fn operation() {} pub const LIMIT: u32 = 3; }\npub use hidden::{operation, LIMIT};\n",
        );
        validate(&exported_value, workspace.path()).unwrap();
        let restricted = design(
            "lib.rs",
            "pub(crate) fn internal() {}\nmod hidden { pub fn unreachable() {} }\npub struct Api { private: u32, pub exposed: u32 }\nimpl Api { fn helper() {} pub fn supported() {} }\n",
        );
        let failure = validate(&restricted, workspace.path())
            .unwrap_err()
            .to_string();
        assert!(
            failure.contains("internal")
                && failure.contains("unreachable")
                && failure.contains("private")
                && failure.contains("helper"),
            "{failure}"
        );
        assert!(
            !failure.contains("exposed") && !failure.contains("supported"),
            "{failure}"
        );
    }

    #[test]
    fn tests_entrypoints_and_trait_contracts_are_automatic_exemptions() {
        let workspace = tempfile::tempdir().unwrap();
        let entry = design(
            "main.rs",
            "fn main() {}\n#[test]\nfn validates() {}\n#[tokio::test]\nasync fn async_validates() {}\nstruct Worker; trait Work { fn work(&self); } impl Work for Worker { fn work(&self) {} }\n",
        );
        let failure = validate(&entry, workspace.path()).unwrap_err().to_string();
        assert!(
            !failure.contains("main:")
                && !failure.contains("validates")
                && !failure.contains("New internal function"),
            "{failure}"
        );
        let misleading = design("lib.rs", "fn main() {}\n#[cfg(test)] fn helper() {}\n");
        let failure = validate(&misleading, workspace.path())
            .unwrap_err()
            .to_string();
        assert!(
            failure.contains("main") && failure.contains("helper"),
            "{failure}"
        );
    }

    #[test]
    fn typescript_exports_do_not_expose_private_members() {
        let workspace = tempfile::tempdir().unwrap();
        let design = design(
            "api.ts",
            "export class Api { public call(): void {} private helper(): void {} private count: number = 0; }\nfunction unused(): void {}\n",
        );
        let failure = validate(&design, workspace.path()).unwrap_err().to_string();
        assert!(
            failure.contains("helper") && failure.contains("count") && failure.contains("unused"),
            "{failure}"
        );
        assert!(!failure.contains("Api::call"), "{failure}");
    }

    #[tokio::test]
    async fn rejects_missing_uses_before_dependency_acquisition() {
        let workspace = tempfile::tempdir().unwrap();
        crate::checks::configure_fixture(workspace.path());
        let design = design("lib.rs", "fn internal() {}\n");
        let error = design.validated(workspace.path()).await.unwrap_err();
        assert!(error.downcast_ref::<DeclarationValidationError>().is_some());
    }
}
