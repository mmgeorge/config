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
                                    "call" | "callback" | "value" | "property"
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
                || symbol.role == DeclarationRole::Binding
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
            let repair = match symbol.role {
                DeclarationRole::Callable => {
                    "Add its invocation or callback registration to the caller's Calls section."
                }
                DeclarationRole::Property => {
                    "Add its read, write, construction, or destructuring use to the user's Accesses section."
                }
                _ => "Add a resolved type reference or a named use in Calls or Accesses.",
            };
            let role = match symbol.role {
                DeclarationRole::Callable => "function",
                DeclarationRole::Type => "type",
                DeclarationRole::Property => "property",
                DeclarationRole::Value => "value",
                DeclarationRole::Module => "module",
                DeclarationRole::Variant => "variant",
                DeclarationRole::Binding => unreachable!(),
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
    use crate::plan::{DeclarationFile, calls};
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
    fn callbacks_and_repeated_calls_preserve_real_incoming_uses() {
        let workspace = tempfile::tempdir().unwrap();
        let design = design(
            "lib.rs",
            "pub fn install() { register(worker); worker(); worker(); }\nfn worker() {}\n",
        );
        assert!(
            design.proposed_calls["lib.rs"][0]
                .call
                .as_ref()
                .unwrap()
                .iter()
                .any(|call| call.name == "worker" && call.kind == crate::plan::CallKind::Callback)
        );
        validate(&design, workspace.path()).unwrap();
        let combined = calls::combined(
            "lib.rs",
            &design.proposed["lib.rs"],
            &design.proposed_calls["lib.rs"],
        )
        .unwrap();
        assert_eq!(
            calls::parse("lib.rs", &combined, &design.proposed_calls["lib.rs"])
                .unwrap()
                .1,
            design.proposed_calls["lib.rs"]
        );
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
    fn types_properties_and_named_values_need_structured_uses() {
        let workspace = tempfile::tempdir().unwrap();
        let used = design(
            "lib.rs",
            "struct Item { count: u32 }\nconst LIMIT: u32 = 3;\npub fn run() { let item = Item { count: LIMIT }; sink(item.count); }\n",
        );
        validate(&used, workspace.path()).unwrap();
        let mut unused = used.clone();
        unused.proposed_calls.clear();
        let failure = validate(&unused, workspace.path()).unwrap_err().to_string();
        assert!(
            failure.contains("Item") && failure.contains("count") && failure.contains("LIMIT"),
            "{failure}"
        );
    }

    #[test]
    fn authored_accesses_resolve_named_values_from_the_complete_atomic_patch() {
        let workspace = tempfile::tempdir().unwrap();
        let design = DeclarationDesign::default().patch(workspace.path(), &BTreeMap::new(), "*** Begin Patch\n*** Add File: src/lib.rs\n+mod values;\n+use values::LIMIT;\n+pub fn run();\n+Accesses\n+  LIMIT\n*** Add File: src/values.rs\n+pub(crate) const LIMIT: u32;\n*** End Patch").unwrap();
        assert_eq!(
            design.proposed_calls["src/lib.rs"][0]
                .call
                .as_ref()
                .unwrap()[0]
                .kind,
            crate::plan::CallKind::Value
        );
        validate(&design, workspace.path()).unwrap();
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
    fn public_wildcard_reexports_expose_only_public_members() {
        let workspace = tempfile::tempdir().unwrap();
        let exposed = design(
            "lib.rs",
            "mod hidden { pub struct Api; impl Api { pub fn new() -> Self { Self } } }\npub use hidden::*;\n",
        );
        validate(&exposed, workspace.path()).unwrap();
        let restricted = design(
            "lib.rs",
            "mod hidden { pub struct Api { private: u32 } }\npub use hidden::*;\n",
        );
        assert!(
            validate(&restricted, workspace.path())
                .unwrap_err()
                .to_string()
                .contains("private")
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
    fn moved_baselines_and_signature_changes_do_not_introduce_existing_symbols() {
        let workspace = tempfile::tempdir().unwrap();
        let mut design = design("new.rs", "fn existing(value: u32) {}\n");
        design.baseline.insert(
            "old.rs".into(),
            DeclarationFile {
                text: "fn existing();\n".into(),
                source_digest: "saved".into(),
            },
        );
        design.moved.insert("old.rs".into(), "new.rs".into());
        validate(&design, workspace.path()).unwrap();
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

    #[test]
    fn lua_local_functions_require_uses_and_returned_module_functions_are_public() {
        let workspace = tempfile::tempdir().unwrap();
        let design = design(
            "api.lua",
            "local M = {}\nfunction M.public() end\nlocal function helper() end\nreturn M\n",
        );
        let failure = validate(&design, workspace.path()).unwrap_err().to_string();
        assert!(
            failure.contains("helper") && !failure.contains("public"),
            "{failure}"
        );
    }

    #[test]
    fn typescript_method_callbacks_establish_private_callable_uses() {
        let workspace = tempfile::tempdir().unwrap();
        let design = design(
            "api.ts",
            "export class Api { public install(): void { register(this.worker); } private worker(): void {} }\n",
        );
        validate(&design, workspace.path()).unwrap();
    }

    #[test]
    fn workspace_callers_are_not_scanned_for_new_plan_symbols() {
        let workspace = tempfile::tempdir().unwrap();
        std::fs::write(
            workspace.path().join("caller.rs"),
            "fn elsewhere() { planned(); }",
        )
        .unwrap();
        let design = design("lib.rs", "fn planned() {}\n");
        assert!(
            validate(&design, workspace.path())
                .unwrap_err()
                .to_string()
                .contains("planned")
        );
    }

    #[test]
    fn public_exposure_reads_only_owning_module_metadata_and_respects_private_boundaries() {
        let workspace = tempfile::tempdir().unwrap();
        std::fs::create_dir(workspace.path().join("src")).unwrap();
        std::fs::write(
            workspace.path().join("Cargo.toml"),
            "[package]\nname = 'metadata'\nversion = '0.1.0'\n",
        )
        .unwrap();
        std::fs::write(
            workspace.path().join("src/lib.rs"),
            "pub mod service;\nmod unrelated;\n",
        )
        .unwrap();
        std::fs::write(
            workspace.path().join("src/unrelated.rs"),
            "invalid syntax {",
        )
        .unwrap();
        let design = design("src/service.rs", "pub fn exported() {}\n");
        validate(&design, workspace.path()).unwrap();
        assert_eq!(design.proposed.len(), 1);
        std::fs::write(
            workspace.path().join("src/lib.rs"),
            "mod service;\nmod unrelated;\n",
        )
        .unwrap();
        assert!(
            validate(&design, workspace.path())
                .unwrap_err()
                .to_string()
                .contains("exported")
        );
    }

    #[test]
    fn lua_named_values_and_properties_share_the_resolved_use_index() {
        let workspace = tempfile::tempdir().unwrap();
        let used = design(
            "api.lua",
            "local LIMIT = 3\nlocal M = {}\nM.count = 0\nfunction M.run() print(LIMIT, M.count) end\nreturn M\n",
        );
        validate(&used, workspace.path()).unwrap();
        let mut unused = used.clone();
        unused.proposed_calls.clear();
        assert!(
            validate(&unused, workspace.path())
                .unwrap_err()
                .to_string()
                .contains("LIMIT")
        );
    }

    #[tokio::test]
    async fn rejects_missing_uses_before_dependency_acquisition() {
        let workspace = tempfile::tempdir().unwrap();
        let design = design("lib.rs", "fn internal() {}\n");
        let error = design.validated(workspace.path()).await.unwrap_err();
        assert!(error.downcast_ref::<DeclarationValidationError>().is_some());
    }

    #[test]
    fn removing_calls_rejects_cached_submission_without_rewriting_the_draft() {
        let workspace = tempfile::tempdir().unwrap();
        let store =
            crate::plan::PlanFileStore::new(workspace.path().join("data"), workspace.path());
        let mut document = crate::plan::document::test_fixture("uses", "Connect internal behavior");
        let mut design = design("lib.rs", "pub fn install() { worker(); }\nfn worker() {}\n");
        design.document.objective = "Connect internal behavior".into();
        design.document.background = "The fixture contains the declarations under review.".into();
        design.document.requirements = vec!["Preserve the declared behavior and ownership.".into()];
        design.document.design = "Register the internal operation".into();
        design.validation = Some(crate::declaration::DeclarationValidation {
            fingerprint: crate::declaration::fingerprint(&design),
            checked: 0,
            diagnostic: Vec::new(),
        });
        design
            .proposed_calls
            .get_mut("lib.rs")
            .unwrap()
            .iter_mut()
            .find(|function| function.owner == "install")
            .unwrap()
            .call = Some(Vec::new());
        document.design = Some(design);
        store
            .write_working_document("session", "uses", &document)
            .unwrap();
        let path = store.plan_dir("session", "uses");
        let before = std::fs::read(path.join("working.json")).unwrap();
        let error = store
            .submit_document_revision("session", "uses", 1, document.version)
            .unwrap_err();
        assert!(error.to_string().contains("worker"));
        assert_eq!(std::fs::read(path.join("working.json")).unwrap(), before);
        assert!(!path.join("revisions/submitted-0001.json").exists());
        let design = document.design.as_mut().unwrap();
        design
            .proposed_calls
            .get_mut("lib.rs")
            .unwrap()
            .iter_mut()
            .find(|function| function.owner == "install")
            .unwrap()
            .call = Some(vec![crate::plan::CallSite {
            kind: crate::plan::CallKind::Call,
            name: "worker".into(),
            source: None,
            unresolved: false,
        }]);
        store
            .write_working_document("session", "uses", &document)
            .unwrap();
        store
            .submit_document_revision("session", "uses", 1, document.version)
            .unwrap();
        assert!(path.join("revisions/submitted-0001.json").exists());
    }
}
